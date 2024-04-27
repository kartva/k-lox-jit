use std::{collections::{HashMap, VecDeque}, fmt::Display, marker::PhantomPinned};

use crate::
    vm::{ByteCode, Op}
;

use dynasmrt::{aarch64, dynasm, AssemblyOffset, DynamicLabel, DynasmApi, DynasmLabelApi, ExecutableBuffer};
use log::debug;

#[derive(Debug)]
pub enum CompileError {
    OutOfBoundsConst(usize),
}

macro_rules! mdynasm {
    ($ops:ident $($t:tt)*) => {
        dynasm!($ops
            ; .arch aarch64
            ; .alias fp, x29
            ; .alias lr, x30
            $($t)*
        )
    }
}

pub struct CompiledBlockCache {
    bc: ByteCode,
    // add exec metadata to ExecutableBuffer for specialized compilation
    // switch to LRU cache
    cache: HashMap<usize, (ExecutableBuffer, AssemblyOffset)>,
    _safety: PhantomPinned
}

/// # Safety
/// cbc must be a valid pointer and not be moved
#[no_mangle]
#[inline(never)]
pub unsafe extern "C" fn call_fn(cbc: *mut CompiledBlockCache, function_idx: u32, argv: *const i64, _argc: u32) -> i64 {
    let cbc_ref = unsafe { &mut *cbc };
    let compiled = if let Some(cached_compiled) = cbc_ref.cache.get(&(function_idx as usize)) {
        // check cache if requested block has already been compiled
        cached_compiled
    } else {
        cbc_ref.cache.insert(function_idx as usize, unsafe {CompiledBlockCache::compile(cbc, function_idx)});
        cbc_ref.cache.get(&(function_idx as usize)).unwrap()
    };

    // Safety: the ExecutableBuffer is guaranteed to be valid
    let (exec_buf, offset) = compiled;
    let f: extern "C" fn(*const i64) -> i64 = unsafe { std::mem::transmute(exec_buf.ptr(*offset)) };

    let ret = f(argv);
    debug!("return value of function_idx {function_idx}: {ret}");
    ret
}

#[derive(Debug, Clone, Copy)]
enum Loc {
    Reg(u32),
    Stack(u32),
}

#[derive(Debug, Clone, Copy)]
enum LocTy {
    Origin,
    Ref,
}

#[derive(Clone, Copy)]
struct StackEntry {
    ty: LocTy,
    loc: Loc,
}

impl Display for StackEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            LocTy::Origin => write!(f, ""),
            LocTy::Ref => write!(f, "ref "),
        }?;

        match self.loc {
            Loc::Reg(reg) => write!(f, "X{}", reg),
            Loc::Stack(idx) => write!(f, "asm_stack[{}]", idx),
        }
    }
}

impl std::fmt::Debug for StackEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

struct MemLocationAllocator {
    free_regs: VecDeque<u32>,
    used_regs: VecDeque<u32>,
    /// Tracks the number of elements that exist on the stack.
    asm_stack_size: u32,
    /// VM stack entries, where each entry is either a register or a stack location.
    vm_stack: Vec<StackEntry>,
}

struct MemTransaction<'a> {
    in_use_regs: VecDeque<u32>,
    allocator: &'a mut MemLocationAllocator,
}

impl Drop for MemTransaction<'_> {
    fn drop(&mut self) {
        self.allocator.used_regs.extend(self.in_use_regs.drain(..));
    }
}

impl MemLocationAllocator {
    const DEFAULT_REGS: &'static [u32] = &[2, 3, 4, 5, 6, 7];

    fn new() -> Self {
        Self::new_with_regs(Self::DEFAULT_REGS.to_vec())
    }

    fn new_with_regs(free_regs: Vec<u32>) -> Self {
        MemLocationAllocator {
            free_regs: VecDeque::from(free_regs),
            used_regs: VecDeque::new(),
            asm_stack_size: 0,
            vm_stack: Vec::new(),
        }
    }

    fn start_txn(&mut self) -> MemTransaction<'_> {
        MemTransaction {
            in_use_regs: VecDeque::new(),
            allocator: self,
        }
    }

    fn stack_offset_from_base(stack_idx: u32) -> u32 {
        ((stack_idx + 1) * 16) as _
    }

    fn top_asm_stack_offset_from_base(&self) -> u32 {
        Self::stack_offset_from_base(self.asm_stack_size)
    }

    fn load_from_stack_to_reg(&self, ops: &mut aarch64::Assembler, stack_idx: u32, reg: u32) {
        mdynasm!(ops
            ; sub x1, fp, #Self::stack_offset_from_base(stack_idx)
            ; ldr X(reg), [x1]
        );
    }

    fn store_from_reg_to_stack(&self, ops: &mut aarch64::Assembler, stack_idx: u32, reg: u32) {
        mdynasm!(ops
            ; sub x1, fp, #Self::stack_offset_from_base(stack_idx)
            ; str X(reg), [x1]
        );
    }

    fn alloc_asm_stack_entry(&mut self) -> u32 {
        self.asm_stack_size += 1;
        self.asm_stack_size
    }

    /// Clones the entry at vm_stack_idx to the top of the VM stack.
    fn clone_entry_to_top(&mut self, vm_stack_idx: usize) -> StackEntry {
        let top_entry = StackEntry { ty: LocTy::Ref, loc: self.vm_stack[vm_stack_idx].loc };
        
        debug!("vm_stack[{}] <= vm_stack[{vm_stack_idx}]", self.vm_stack.len());
        self.vm_stack.push(top_entry);
        top_entry
    }
}

enum WriteBackResult {
    None,
    AsmStack(u32),
}

impl MemTransaction<'_> {
    fn free_reg(&mut self, reg: u32) {
        self.allocator.free_regs.push_front(reg);
        self.allocator.used_regs.retain(|&r| r != reg);
        self.in_use_regs.retain(|&r| r != reg);
    }

    /// Demotes register to stack locations.
    /// Scans the VM stack for all references to the register and replaces them with a newly allocated stack location.
    /// The registers are then freed.
    fn demote_reg_to_stack(&mut self, ops: &mut aarch64::Assembler, reg: u32) {
        let stack_entry = self.allocator.alloc_asm_stack_entry();
        self.allocator.store_from_reg_to_stack(ops, stack_entry, reg);

        debug!("demote X{reg} => asm_stack[{stack_entry}]");

        for (i, st) in self.allocator.vm_stack.iter_mut().enumerate() {
            match st {
                StackEntry { ty, loc: Loc::Reg(r) } if *r == reg => {
                    let new_st = StackEntry { ty: *ty, loc: Loc::Stack(stack_entry) };
                    debug!("vm_stack[{i}] = {st} => {new_st}");
                    *st = new_st;
                },
                _ => (),
            }
        }

        self.free_reg(reg);
    }

    fn demote_all_regs_to_stack(&mut self, ops: &mut aarch64::Assembler) {
        while let Some(reg) = self.in_use_regs.pop_back() {
            self.demote_reg_to_stack(ops, reg);
        }
        while let Some(reg) = self.allocator.used_regs.pop_back() {
            self.demote_reg_to_stack(ops, reg);
        }
    }

    pub fn alloc_reg(&mut self, ops: &mut aarch64::Assembler) -> u32 {
        let new_reg = if let Some(reg) = self.allocator.free_regs.pop_back() {
            reg
        } else {
            let reg = *self.allocator.used_regs.back().expect("no free registers available");
            // used regs needs to be pushed to stack
            self.demote_reg_to_stack(ops, reg);
            reg
        };
        self.in_use_regs.push_back(new_reg);
        new_reg
    }

    pub fn load_entry_to_reg(&mut self, ops: &mut aarch64::Assembler, ent: StackEntry) -> u32 {
        match ent {
            StackEntry { ty: _, loc: Loc::Reg(reg) } => reg,
            StackEntry { ty: _, loc: Loc::Stack(idx) } => {
                let reg = self.alloc_reg(ops);
                self.allocator.load_from_stack_to_reg(ops, idx, reg);
                
                reg
            }
        }
    }

    /// Pops the top-most entry from the VM stack. If the entry is a register, it is freed.
    fn pop_top_entry(&mut self) {
        let st = self.allocator.vm_stack.pop().unwrap();
        debug!("vm_stack[{}] pop: {st}", self.allocator.vm_stack.len());
        match st {
            StackEntry { ty: LocTy::Origin, loc: Loc::Reg(reg) } => {
                self.free_reg(reg)
            },
            StackEntry { ty: LocTy::Origin, loc: Loc::Stack(_) } => {
                self.allocator.asm_stack_size -= 1;
            },
            StackEntry { ty: _, loc: _ } => (),
        };
    }

    /// Pops N entries from the VM stack and returns the registers they were promoted to.
    /// Any newly allocated registers are then freed.
    /// Since this operations frees the registers, future calls to allocate registers may return the same registers.
    fn pop_vm_stack_to_regs<const N: usize>(&mut self, ops: &mut aarch64::Assembler) -> [u32; N] {
        assert!(self.allocator.vm_stack.len() >= N, "not enough entries in vm stack");

        let entries: [_; N] = self.allocator.vm_stack.split_off(self.allocator.vm_stack.len() - N).try_into().unwrap();
        
        let mut regs = [0; N];
        for (i, ent) in entries.iter().enumerate() {
            regs[i] = self.load_entry_to_reg(ops, *ent);
        }

        debug!("vm_stack[{}..{}] pop: {entries:?} -> X{regs:?}", self.allocator.vm_stack.len(), self.allocator.vm_stack.len() + N);

        for (entry, reg) in entries.iter().zip(regs.iter()) {
            // A register is allocated in all cases except for an entry which is a ref to a register.
            if matches!(entry, StackEntry { ty: LocTy::Origin, loc: Loc::Reg(_) } | StackEntry { ty: _, loc: Loc::Stack(_) }) {
                self.free_reg(*reg);
            }
        }

        regs
    }

    pub fn push_reg_to_vm_stack(&mut self, reg: u32) {
        let stack_entry = StackEntry { ty: LocTy::Origin, loc: Loc::Reg(reg) };
        debug!("vm_stack[{}] <- X{reg}", self.allocator.vm_stack.len());
        self.allocator.vm_stack.push(stack_entry);
    }

    /// Pushes a register to the VM stack, and returns the register.
    pub fn alloc_reg_on_vm_stack(&mut self, ops: &mut aarch64::Assembler) -> u32 {
        let reg = self.alloc_reg(ops);
        self.push_reg_to_vm_stack(reg);
        reg
    }

    /// Given a VM stack index, returns a register .
    /// If the register has to be allocated, it is immediately freed.
    /// FIXME: better design to return RegisterEntry which contains whether the register is temporary or not.
    /// and then at transaction drop, free the temporary registers.
    pub fn obtain_mutable_reg_entry(&mut self, ops: &mut aarch64::Assembler, stack_idx: usize) -> (u32, WriteBackResult) {
        match self.allocator.vm_stack[stack_idx] {
            StackEntry { ty: LocTy::Origin, loc: Loc::Reg(reg) } => (reg, WriteBackResult::None),
            ent @ StackEntry { ty: LocTy::Ref, loc: Loc::Reg(reg) } => {
                let new_reg = self.alloc_reg(ops);
                self.allocator.vm_stack[stack_idx] = StackEntry { ty: LocTy::Origin, loc: Loc::Reg(new_reg) };

                debug!("vm_stack[{stack_idx}] = {ent} => {}", self.allocator.vm_stack[stack_idx]);

                mdynasm!(ops
                    ; mov X(new_reg), X(reg)
                );
                (new_reg, WriteBackResult::None)
            },
            // If the entry is a stack location, we need to promote it to a register.
            // If the entry is a ref to a stack location, it can be promoted to a register with no issues.
            // However, if the entry is the origin of a stack location, we need to search through the VM stack
            // for all references to this stack location and promote the bottom-most ref to the new origin.
            ent @ StackEntry { ty, loc: Loc::Stack(asm_idx) } => {
                let new_reg = self.load_entry_to_reg(ops, ent);

                debug!("vm_stack[{stack_idx}] = {ent} => {}", self.allocator.vm_stack[stack_idx]);

                if let LocTy::Origin = ty {
                    // search through vm_stack for refs to this stack location
                    // and promote the bottom-most ref to an origin
                    for (i, st) in self.allocator.vm_stack.iter_mut().enumerate() {
                        if let StackEntry { ty: LocTy::Ref, loc: Loc::Stack(ref_idx) } = st {
                            if asm_idx == *ref_idx {
                                debug!("vm_stack[{i}] = {st} => {ent}");
                                *st = ent;
                                break;
                            }
                        }
                    }
                }

                debug!("vm_stack[{stack_idx}] = {ent} => {}", self.allocator.vm_stack[stack_idx]);

                (new_reg, WriteBackResult::AsmStack(asm_idx))
            },
        }
    }

    pub fn write_back_mutable_entry(&mut self, ops: &mut aarch64::Assembler, (reg, write_back): (u32, WriteBackResult)) {
        if let WriteBackResult::AsmStack(stack_idx) = write_back {
            self.allocator.store_from_reg_to_stack(ops, stack_idx, reg);
            self.free_reg(reg);
        }
    }
}

impl CompiledBlockCache {
    pub fn new(bc: ByteCode) -> Box<CompiledBlockCache> {
        Box::new(CompiledBlockCache {
            bc,
            cache: HashMap::new(),
            _safety: PhantomPinned,
        })
    }

    /// # Safety
    /// cbc must be a valid pointer and not be moved
    pub unsafe fn compile(cbc: *const Self, idx: u32) -> (ExecutableBuffer, AssemblyOffset) {
        let cbc_ref = unsafe { &*cbc };
        debug!("jitting function idx {}", idx);
        let chunk = &cbc_ref.bc.chunks[idx as usize];

        let mut ops = aarch64::Assembler::new().unwrap();
        let mut malloc = MemLocationAllocator::new();

        mdynasm!(
            ops
            // call function call_fn
            ; .align 8
            ; ->call_fn:
            ; .qword call_fn as _
            ; .align 8
            ; ->cbc_ptr:
            ; .qword cbc as _
            ; .align 8
        );

        let offset = ops.offset();

        // Prologue: push stack frame
        mdynasm!(ops
            ; nop
            // rustc may omit frame pointers for builds
            // if saved fp value seems off, enable frame pointers in config.toml
            ; stp fp, lr, [sp, #-16]!
            ; mov fp, sp    // frame pointer points to location of previous frame pointer
                            // frame pointer acts as base pointer for current function
        );

        // ARGS: x0: argv
        // push args to stack
        // x0 should contains pointer to args (in reverse order)
        if chunk.in_arg > 0 {
            mdynasm!(ops
                ; sub sp, sp, #(16 * (chunk.in_arg)) // allocate space for args
                ; mov x2, sp // save current stack pointer
            );
            for _ in 0..chunk.in_arg {
                mdynasm!(ops
                    ; ldr x1, [x0], #16
                    ; str x1, [sp], #16
                );
            }
            // restore stack pointer
            mdynasm!(ops
                ; mov sp, x2
            );
        }

        for i in 1..=chunk.in_arg {
            malloc.vm_stack.push(StackEntry { ty: LocTy::Origin, loc: Loc::Stack(i) });
        }
        malloc.asm_stack_size = chunk.in_arg as _;

        let mut labels: HashMap<usize, DynamicLabel> = HashMap::new();
        for op in chunk.code.iter() {
            if let Op::JumpLabel { label_id } = op {
                let label = ops.new_dynamic_label();
                labels.insert(*label_id, label);
            }
        }

        for op in &chunk.code {
            debug!("--- op: {:?} ---", op);
            match op {
                Op::Constant { val } => {
                    let mut txn = malloc.start_txn();

                    let num = *val;
                    let entry = txn.alloc_reg_on_vm_stack(&mut ops);

                    debug!("X{entry} <- {num}");

                    mdynasm!(ops
                        ; mov X(entry), #num as _
                    );
                }
                bin_op_with_res @ (Op::Add | Op::Sub | Op::Mul | Op::Div | Op::LessThan | Op::LessThanEq | Op::GreaterThan | Op::GreaterThanEq) => {
                    let mut txn = malloc.start_txn();
                    let [r1, r2] = txn.pop_vm_stack_to_regs(&mut ops);
                    let res_r = txn.alloc_reg_on_vm_stack(&mut ops);

                    debug!("X{res_r} = X{r1} {bin_op_with_res:?} X{r2}");

                    match bin_op_with_res {
                        Op::Add => {
                            mdynasm!(ops
                                ; add X(res_r), X(r1), X(r2)
                            );
                        }
                        Op::Sub => {
                            mdynasm!(ops
                                ; sub X(res_r), X(r1), X(r2)
                            );
                        }
                        Op::Mul => {
                            mdynasm!(ops
                                ; mul X(res_r), X(r1), X(r2)
                            );
                        }
                        Op::Div => {
                            mdynasm!(ops
                                ; sdiv X(res_r), X(r1), X(r2)
                            );
                        },
                        Op::LessThan => {
                            mdynasm!(ops
                                ; cmp X(r1), X(r2)
                                ; mov X(res_r), #0
                                ; cset X(res_r), lt
                            );
                        },
                        Op::GreaterThan => {
                            mdynasm!(ops
                                ; cmp X(r1), X(r2)
                                ; mov X(res_r), #0
                                ; cset X(res_r), gt
                            );
                        },
                        Op::LessThanEq => {
                            mdynasm!(ops
                                ; cmp X(r1), X(r2)
                                ; mov X(res_r), #0
                                ; cset X(res_r), le
                            );
                        },
                        Op::GreaterThanEq => {
                            mdynasm!(ops
                                ; cmp X(r1), X(r2)
                                ; mov X(res_r), #0
                                ; cset X(res_r), ge
                            );
                        },
                        _ => unreachable!(),
                    }
                },
                Op::LoadVar { stack_idx } => {
                    malloc.clone_entry_to_top(*stack_idx);
                },
                Op::SetVar { stack_idx } => {
                    let mut txn = malloc.start_txn();

                    if *stack_idx != txn.allocator.vm_stack.len() - 1 {
                        let val_entry = txn.load_entry_to_reg(&mut ops, *txn.allocator.vm_stack.last().unwrap());
                        let (to_set_entry, writeback) = txn.obtain_mutable_reg_entry(&mut ops, *stack_idx);

                        debug!("X{to_set_entry} <- X{val_entry}");
                        mdynasm!(ops
                            ; mov X(to_set_entry), X(val_entry)
                        );
                        txn.write_back_mutable_entry(&mut ops, (to_set_entry, writeback));
                    }
                },
                Op::Pop { count } => {
                    let mut txn = malloc.start_txn();
                    for _ in 0..*count {
                        txn.pop_top_entry();
                    }
                },
                Op::JumpLabel { label_id} => {
                    let label = *labels.get(label_id).unwrap();
                    mdynasm!(ops
                        ; =>label
                    );
                },
                Op::Jump { label_id } => {
                    let label = *labels.get(label_id).unwrap();
                    mdynasm!(ops
                        ; b =>label
                    );
                },
                Op::JumpIfZero { label_id } => {
                    let label = *labels.get(label_id).unwrap();
                    let [reg] = malloc.start_txn().pop_vm_stack_to_regs(&mut ops);
                    mdynasm!(ops
                        ; cbz X(reg), =>label
                    );
                },
                Op::Call { fn_idx: idx, word_argc } => {
                    let mut txn = malloc.start_txn();
                    txn.demote_all_regs_to_stack(&mut ops);

                    mdynasm!(ops
                        // load arguments to registers
                        ; add sp, fp, #txn.allocator.top_asm_stack_offset_from_base()
                        ; ldr x0, ->cbc_ptr
                        ; mov w1, *idx as u64
                        ; mov x2, sp // argv is current stack pointer
                        ; mov x3, *word_argc as u64
                        ; ldr x4, ->call_fn
                        ; blr x4 // call function
                    );
                    // pop arguments from stack
                    for _ in 0..*word_argc {
                        txn.pop_top_entry();
                    }
                    // push return value to stack
                    txn.push_reg_to_vm_stack(0);
                },
                Op::Return => {
                    let [reg] = malloc.start_txn().pop_vm_stack_to_regs(&mut ops);
                    mdynasm!(ops
                        ; mov x0, X(reg)
                        ; mov sp, fp // restore stack pointer
                        ; ldp fp, lr, [sp], #16 // restore frame pointer and link register
                        ; ret
                    );
                }
            }
        }

        let buf = ops.finalize().unwrap();
        (buf, offset)
    }
}

#[cfg(test)]
mod jit_tests {
    use std::ptr::null;

    use crate::vm::ByteCodeChunk;
    use super::*;

    #[test]
    fn jit_vars() {
        let chunk = ByteCodeChunk {
            code: vec![
                Op::Constant { val: 2 },
                Op::Constant { val: 3 },
                Op::Add,
                Op::Return,
            ],
            in_arg: 0,
            out_args: 0,
        };
        let bytecode = ByteCode { chunks: vec![chunk] };
        let mut cbc = CompiledBlockCache::new(bytecode);
        let ret = unsafe { call_fn(&mut *cbc, 0, null(), 0) };
        assert_eq!(ret, 5);
    }
}
