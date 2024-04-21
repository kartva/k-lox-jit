use std::collections::{HashMap, VecDeque};

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
enum StackEntry {
    Origin(Loc),
    Ref(Loc),
}

struct MemLocationAllocator {
    free_regs: VecDeque<u32>,
    used_regs: VecDeque<u32>,
    /// Tracks the number of elements that exist on the stack.
    asm_stack_size: u32,
    /// VM stack entries, where each entry is either a register or a stack location.
    vm_stack: Vec<StackEntry>,
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

    fn stack_offset_from_base(&self, stack_idx: u32) -> u32 {
        (stack_idx * 16) as _
    }

    fn load_from_stack_to_reg(&self, ops: &mut aarch64::Assembler, stack_idx: u32, reg: u32) {
        mdynasm!(ops
            ; sub x1, fp, #self.stack_offset_from_base(stack_idx)
            ; ldr X(reg), [x1]
        );
    }

    fn store_from_reg_to_stack(&self, ops: &mut aarch64::Assembler, stack_idx: u32, reg: u32) {
        mdynasm!(ops
            ; sub x1, fp, #self.stack_offset_from_base(stack_idx)
            ; str X(reg), [x1]
        );
    }

    fn alloc_asm_stack_entry(&mut self) -> u32 {
        self.asm_stack_size += 1;
        self.asm_stack_size
    }

    fn try_alloc_reg(&mut self) -> Option<u32> {
        self.free_regs.pop_back().map(|reg| {
            self.used_regs.push_front(reg);
            reg
        })
    }

    /// Allocates a new memory location, either a register or a stack location.
    fn alloc_memory_loc(&mut self) -> StackEntry {
        self.try_alloc_reg().map_or_else(|| StackEntry::Origin(Loc::Stack(self.alloc_asm_stack_entry())), |reg| StackEntry::Origin(Loc::Reg(reg)))
    }

    /// Demotes registers to stack locations.
    /// For each register, scans the VM stack for all references to the register and replaces them with a newly allocated stack location.
    /// The registers are then freed.
    fn demote_regs_to_stack(&mut self, ops: &mut aarch64::Assembler, regs: &[u32]) {
        for &reg in regs {
            let stack_entry = self.alloc_asm_stack_entry();
            self.store_from_reg_to_stack(ops, stack_entry, reg);

            debug!("demote X{reg} => asm_stack[{stack_entry}]");

            for (i, st) in self.vm_stack.iter_mut().enumerate() {
                match st {
                    StackEntry::Origin(Loc::Reg(r)) if *r == reg => {
                        debug!("vm_stack[{i}] = X{reg} => asm_stack[{stack_entry}]");
                        *st = StackEntry::Origin(Loc::Stack(stack_entry))
                    },
                    StackEntry::Ref(Loc::Reg(r)) if *r == reg => {
                        debug!("vm_stack[{i}] = ref X{reg} => ref asm_stack[{stack_entry}]");
                        *st = StackEntry::Ref(Loc::Stack(stack_entry))
                    },
                    _ => (),
                }
            }
        }

        for &reg in regs {
            self.free_reg(reg);
        }
    }

    /// Allocates registers by (potentially) spilling existing register values to stack,
    /// and returns the registers.
    fn alloc_n_regs<const N: usize>(&mut self, ops: &mut aarch64::Assembler) -> [u32; N] {
        let regs = self.alloc_regs(ops, N, None);
        regs.try_into().unwrap()
    }

    /// Allocates registers by (potentially) spilling existing register values to stack,
    /// and returns the registers.
    /// 
    /// Each call to `alloc_regs` may invalidate the registers returned by previous calls.
    fn alloc_regs(&mut self, ops: &mut aarch64::Assembler, n: usize, preserve_regs: Option<&[u32]>) -> Vec<u32> {
        let free_regs = self.free_regs.len();

        if free_regs < n {
            let regs_to_demote = n - free_regs;
            let regs_to_demote = match preserve_regs {
                Some(preserve_regs) => self.used_regs.iter().filter(|&r| !preserve_regs.contains(&r)).take(regs_to_demote).copied().collect::<Vec<_>>(),
                None => self.used_regs.iter().take(regs_to_demote).copied().collect::<Vec<_>>()
            };
            self.demote_regs_to_stack(ops, &regs_to_demote);
        }

        (0..n).map(|_| self.try_alloc_reg().expect("regalloc succeeds because we demoted enough registers")).collect()
    }

    /// Clones the entry at vm_stack_idx to the top of the VM stack.
    fn clone_entry_to_top(&mut self, vm_stack_idx: usize) -> StackEntry {
        let top_entry = match self.vm_stack[vm_stack_idx] {
            StackEntry::Origin(o) => StackEntry::Ref(o),
            st @ StackEntry::Ref(_) => st,
        };
        debug!("vm_stack[{}] <= vm_stack[{vm_stack_idx}]", self.vm_stack.len());
        self.vm_stack.push(top_entry);
        top_entry
    }

    /// Pops N entries from the VM stack and returns the registers they were promoted to.
    /// The registers are then freed.
    fn pop_vm_stack_to_regs<const N: usize>(&mut self, ops: &mut aarch64::Assembler) -> [u32; N] {
        assert!(self.vm_stack.len() >= N, "not enough entries in vm stack");
        assert!(self.used_regs.len() + self.free_regs.len() >= N, "not enough registers available");

        let entries = self.vm_stack.split_off(self.vm_stack.len() - N).try_into().unwrap();
        let msg = format!("vm_stack[{}..{}] pop: {entries:?}", self.vm_stack.len(), self.vm_stack.len() + N);
        let regs = self.promote_entries_to_regs(ops, &entries);
        debug!("{msg} -> X{regs:?}");

        for (entry, reg) in entries.iter().zip(regs.iter()) {
            if matches!(entry, StackEntry::Origin(_)) {
                self.free_reg(*reg);
            }
        }

        regs
    }

    /// Pops the top-most entry from the VM stack. If the entry is a register, it is freed.
    fn pop_top_entry(&mut self) {
        let st = self.vm_stack.pop().unwrap();
        match st {
            StackEntry::Origin(Loc::Reg(reg)) => {
                self.free_reg(reg)
            },
            StackEntry::Origin(Loc::Stack(_)) => {
                self.asm_stack_size -= 1;
            },
            StackEntry::Ref(_) => (),
        };
    }

    /// Promotes an entry in the VM stack to a register.
    /// If the entry is already a register, the register is returned. Otherwise, promotes the entry to a register
    /// by scanning the VM stack for all references to the entry and replaces them with a newly allocated register.
    /// Returns the allocated register.
    /// 
    /// Each call to `promote_entries_to_regs` may invalidate the registers returned by previous calls.
    fn promote_entries_to_regs<const N: usize>(&mut self, ops: &mut aarch64::Assembler, locs: &[StackEntry; N]) -> [u32; N] {
        let mut regs = [0u32; N];

        let allocs_required = locs.iter().filter(|&loc| matches!(loc, StackEntry::Origin(Loc::Stack(_)) | StackEntry::Ref(Loc::Stack(_)))).count();
        let regs_to_preserve = locs.iter().filter_map(|&loc| match loc {
            StackEntry::Origin(Loc::Reg(reg)) | StackEntry::Ref(Loc::Reg(reg)) => Some(reg),
            _ => None,
        }).collect::<Vec<_>>();

        let mut alloced_regs = self.alloc_regs(ops, allocs_required, Some(&regs_to_preserve));

        for (i, loc) in locs.into_iter().enumerate() {
            regs[i] = match loc {
                StackEntry::Origin(Loc::Reg(reg)) | StackEntry::Ref(Loc::Reg(reg)) => *reg,
                StackEntry::Origin(Loc::Stack(idx)) | StackEntry::Ref(Loc::Stack(idx)) => {
                    let reg = alloced_regs.pop().unwrap();
                    let idx = *idx;

                    debug!("promote asm_stack[{idx}] => X{reg}");
                    self.load_from_stack_to_reg(ops, idx, reg);
                    for st in self.vm_stack.iter_mut() {
                        match st {
                            StackEntry::Origin(Loc::Stack(i)) if *i == idx => {
                                debug!("vm_stack[{i}] = asm_stack[{idx}] => X{reg}");
                                *st = StackEntry::Origin(Loc::Reg(reg));
                            },
                            StackEntry::Ref(Loc::Stack(i)) if *i == idx => {
                                debug!("vm_stack[{i}] = ref asm_stack[{idx}] => ref X{reg}");
                                *st = StackEntry::Ref(Loc::Reg(reg));
                            },
                            _ => (),
                        }
                    }
                    reg
                }
            }
        }
        regs
    }

    fn free_reg(&mut self, reg: u32) {
        self.free_regs.push_front(reg);
        self.used_regs.retain(|&r| r != reg);
    }

    pub fn push_reg_to_vm_stack(&mut self, reg: u32) {
        let stack_entry = StackEntry::Origin(Loc::Reg(reg));
        debug!("vm_stack[{}] <- X{reg}", self.vm_stack.len());
        self.vm_stack.push(stack_entry);
    }

    fn obtain_mutable_reg_entry(&mut self, ops: &mut aarch64::Assembler, stack_idx: usize) -> u32 {
        match self.vm_stack[stack_idx] {
            StackEntry::Origin(Loc::Reg(reg)) => reg,
            StackEntry::Origin(Loc::Stack(idx)) | StackEntry::Ref(Loc::Stack(idx)) => {
                let [new_reg] = self.alloc_n_regs(ops);
                self.vm_stack[stack_idx] = StackEntry::Origin(Loc::Reg(new_reg));
                self.load_from_stack_to_reg(ops, idx, new_reg);
                new_reg
            },
            StackEntry::Ref(Loc::Reg(reg)) => {
                let [new_reg] = self.alloc_n_regs(ops);
                self.vm_stack[stack_idx] = StackEntry::Origin(Loc::Reg(new_reg));
                mdynasm!(ops
                    ; mov X(new_reg), X(reg)
                );
                new_reg
            }        
        }
    }

    fn demote_all_regs_to_stack(&mut self, ops: &mut aarch64::Assembler) {
        while let Some(reg) = self.used_regs.back().copied() {
            self.demote_regs_to_stack(ops, &[reg]);
        }
    }
}

impl CompiledBlockCache {
    pub fn new(bc: ByteCode) -> Box<CompiledBlockCache> {
        Box::new(CompiledBlockCache {
            bc,
            cache: HashMap::new()
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
            malloc.vm_stack.push(StackEntry::Origin(Loc::Stack(i)));
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
                    let num = *val;
                    let [entry] = malloc.alloc_n_regs(&mut ops);
                    malloc.push_reg_to_vm_stack(entry);

                    debug!("X{entry} <- {num}");

                    mdynasm!(ops
                        ; mov X(entry), #num as _
                    );
                }
                bin_op_with_res @ (Op::Add | Op::Sub | Op::Mul | Op::Div | Op::LessThan | Op::LessThanEq | Op::GreaterThan | Op::GreaterThanEq) => {
                    let [r1, r2] = malloc.pop_vm_stack_to_regs(&mut ops);
                    let [res_r] = malloc.alloc_n_regs(&mut ops);

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
                    malloc.push_reg_to_vm_stack(res_r);
                },
                Op::LoadVar { stack_idx } => {
                    malloc.clone_entry_to_top(*stack_idx as usize);
                },
                Op::SetVar { stack_idx } => {
                    let [val] = malloc.promote_entries_to_regs(&mut ops, &[*malloc.vm_stack.last().unwrap()]);
                    let reg = malloc.obtain_mutable_reg_entry(&mut ops, *stack_idx as usize);
                    mdynasm!(ops
                        ; mov X(reg), X(val)
                    );
                },
                Op::Pop { count } => {
                    for _ in 0..*count {
                        malloc.pop_top_entry();
                    }
                },
                Op::JumpLabel { label_id} => {
                    let label = *labels.get(label_id).unwrap();
                    malloc.demote_all_regs_to_stack(&mut ops);
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
                    let [reg] = malloc.pop_vm_stack_to_regs(&mut ops);
                    mdynasm!(ops
                        ; cbz X(reg), =>label
                    );
                },
                Op::Call { fn_idx: idx, word_argc } => {
                    malloc.demote_all_regs_to_stack(&mut ops);
                    mdynasm!(ops
                        // load arguments to registers
                        ; add sp, fp, #malloc.stack_offset_from_base(malloc.asm_stack_size)
                        ; ldr x0, ->cbc_ptr
                        ; mov w1, *idx as u64
                        ; mov x2, sp // argv is current stack pointer
                        ; mov x3, *word_argc as u64
                        ; ldr x4, ->call_fn
                        ; blr x4 // call function
                    );
                    // pop arguments from stack
                    for _ in 0..*word_argc {
                        malloc.pop_top_entry();
                    }
                    // push return value to stack
                    malloc.push_reg_to_vm_stack(0);
                },
                Op::Return => {
                    let [reg] = malloc.pop_vm_stack_to_regs(&mut ops);
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
