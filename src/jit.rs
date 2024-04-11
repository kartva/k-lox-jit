use std::collections::HashMap;

use crate::
    vm::{ByteCode, ByteCodeChunk, Op}
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

/// Loads arg1 and arg2 into x0 and x1, and stores the result in x0.
macro_rules! two_arg_one_ret {
    ($ops:ident $($t:tt)*) => {
        mdynasm!($ops
            // load value from the address stored in sp,
            // stores in dest, then increment sp by 16
			; ldr x0, [sp], #16
			; ldr x1, [sp], #16
            $($t)*
            // store the result in the address stored in sp
            // then decrement sp by 16
            ; mov x1, #0 // store dummy value
			; stp x0, x1, [sp, #-16]!
        )
    }
}

/// Top-most stack value is stored in reg_range.start.
/// Bottom-most stack value is stored in reg_range.end.
fn load_stack_into_regs(ops: &mut aarch64::Assembler, reg_range: Range<u32>) {
    for i in reg_range {
        mdynasm!(ops
            ; ldr X(i), [sp], #16
        );
    }
}

/// Spills registers to stack, such that
/// the top-most stack value contains value in reg_range.start.
/// Eg. if reg_range is 0..3, then stack will have values in the order:
/// [sp, #32]: X2
/// [sp, #16]: X1
/// [sp, #0]:  X0 <- top of stack
fn spill_regs_to_stack(ops: &mut aarch64::Assembler, regs: Range<u32>) {
    for i in regs.rev() {
        mdynasm!(ops
            ; str X(i), [sp, #-16]!
        );
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
pub unsafe extern "C" fn call_fn(cbc: *mut CompiledBlockCache, idx: u32, argv: *const i64, _argc: u32) -> i64 {
    let cbc_ref = unsafe { &mut *cbc };
    let compiled = if let Some(cached_compiled) = cbc_ref.cache.get(&(idx as usize)) {
        // check cache if requested block has already been compiled
        cached_compiled
    } else {
        cbc_ref.cache.insert(idx as usize, unsafe {CompiledBlockCache::compile(cbc, idx)});
        cbc_ref.cache.get(&(idx as usize)).unwrap()
    };

    // Safety: the ExecutableBuffer is guaranteed to be valid
    let (exec_buf, offset) = compiled;
    let f: extern "C" fn(*const i64) -> i64 = unsafe { std::mem::transmute(exec_buf.ptr(*offset)) };
    f(argv)
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
    pub unsafe fn compile(cbc: *mut Self, idx: u32) -> (ExecutableBuffer, AssemblyOffset) {
        let cbc_ref = unsafe { &*cbc };
        debug!("jitting function idx {}", idx);
        let chunk = &cbc_ref.bc.chunks[idx as usize];

        let mut ops = aarch64::Assembler::new().unwrap();

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
        let mut labels: HashMap<usize, DynamicLabel> = HashMap::new();
        for op in chunk.code.iter() {
            if let Op::JumpLabel { label_id } = op {
                let label = ops.new_dynamic_label();
                labels.insert(*label_id, label);
            }
        }
        for op in &chunk.code {
            match op {
                Op::Constant { val } => {
                    let num = *val;
                    mdynasm!(ops
                        ; mov x0, num as u64
                        ; str x0, [sp, #-16]!
                    );
                }
                Op::Add => {
                    two_arg_one_ret!(ops
                        ; add x0, x0, x1
                    );
                }
                Op::Sub => {
                    two_arg_one_ret!(ops
                        ; sub x0, x1, x0
                    );
                }
                Op::Mul => {
                    two_arg_one_ret!(ops
                        ; mul x0, x0, x1
                    );
                }
                Op::Div => {
                    two_arg_one_ret!(ops
                        ; sdiv x0, x0, x1 // rounds towards zero
                    );
                },
                Op::LoadVar { idx } => {
                    mdynasm!(ops
                        ; sub x0, fp, #((idx + 1) * 16) // calculate addr
                        // attempted to do this using a negative offset for ldr
                        // did not work
                        ; ldr x0, [x0] // load from address
                        ; str x0, [sp, #-16]! // store on stack
                    );
                },
                Op::SetVar { idx } => {
                    mdynasm!(ops
                        ; sub x1, fp, #((idx + 1) * 16) // calculate addr
                        ; ldr x0, [sp] // load new value from stack
                        ; str x0, [x1] // update on stack
                    );
                }
                Op::Pop => {
                    mdynasm!(ops
                        ; add sp, sp, #16
                    );
                },
                Op::LessThan => {
                    two_arg_one_ret!(ops
                        ; cmp x0, x1
                        ; cset x0, ge // are inverted since cset sets to 1 if true
                    );
                },
                Op::GreaterThan => {
                    two_arg_one_ret!(ops
                        ; cmp x0, x1
                        ; cset x0, le
                    );
                },
                Op::LessThanEq => {
                    two_arg_one_ret!(ops
                        ; cmp x0, x1
                        ; cset x0, gt
                    );
                },
                Op::GreaterThanEq => {
                    two_arg_one_ret!(ops
                        ; cmp x0, x1
                        ; cset x0, lt
                    );
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
                    mdynasm!(ops
                        ; ldr x0, [sp], #16
                        ; cbz x0, =>label
                    );
                },
                Op::Call { idx, word_argc } => {
                    mdynasm!(ops
                        // load arguments to registers
                        ; ldr x0, ->cbc_ptr
                        ; mov w1, *idx as u64
                        ; mov x2, sp // argv is current stack pointer
                        ; mov x3, *word_argc as u64
                        ; ldr x4, ->call_fn
                        ; blr x4 // call function

                        ; add sp, sp, #(word_argc * 16) // pop args
                        ; str x0, [sp, #-16]! // store return value on stack
                    );
                },
                Op::Return => {
                    mdynasm!(ops
                        ; ldr x0, [sp], #16
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

    use super::*;

    #[test]
    fn jit_vars() {
        let chunk = ByteCodeChunk {
            code: vec![
                Op::Constant { val: 2 },
                Op::Constant { val: 3 },
                Op::LoadVar { idx: 0 }, 
                Op::LoadVar { idx: 1 },
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
