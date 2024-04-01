use core::slice;
use std::{collections::HashMap, io::{self, Write}};

use crate::
    vm::{ByteCodeChunk, Op}
;
use dynasmrt::{aarch64, dynasm, DynamicLabel, DynasmApi, ExecutableBuffer, DynasmLabelApi};
use log::debug;

#[derive(Debug)]
pub enum CompileError {
    OutOfBoundsConst(usize),
}

extern "C" fn print(buf: *const u8, len: u64) {
    let buf = unsafe { slice::from_raw_parts(buf, len as usize) };
    let s = std::str::from_utf8(buf).unwrap();
    io::stdout().write_all(s.as_bytes()).unwrap();
    println!("print was called with buf {:?} and len {}", buf, len);
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
			; str x0, [sp, #-16]!
        )
    }
}

pub struct JIT {
    _exec_buf: ExecutableBuffer,
	func: *const u8,
}

impl JIT {
    pub fn compile(chunk: ByteCodeChunk) -> Result<JIT, CompileError> {
        debug!("Starting JIT");
        let mut ops = aarch64::Assembler::new().unwrap();
        let offset = ops.offset();

        // Prologue: push stack frame
        mdynasm!(ops
            ; nop
            // rustc may omit frame pointers for builds
            // if saved fp value seems off, enable frame pointers in config.toml
            ; stp fp, lr, [sp, #-16]!
            ; add fp, sp, #16 // frame pointer points to location of previous frame pointer
                              // frame pointer acts as base pointer for current function
        );

        let mut labels: HashMap<usize, DynamicLabel> = HashMap::new();
        for op in chunk.code.iter() {
            if let Op::JumpLabel { label_id } = op {
                let label = ops.new_dynamic_label();
                labels.insert(*label_id, label);
            }
        }
        for op in chunk.code {
            match op {
                Op::Constant { val } => {
                    let num = val;
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
                        ; sub x0, x0, x1
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
                        ; sub x0, x29, #((idx + 1) * 16) // calculate addr
                        // attempted to do this using a negative offset for ldr
                        // did not work
                        ; ldr x0, [x0] // load from address
                        ; str x0, [sp, #-16]! // store on stack
                    );
                },
                Op::SetVar { idx } => {
                    mdynasm!(ops
                        ; sub x1, x29, #((idx + 1) * 16) // calculate addr
                        ; ldr x0, [sp] // load new value from stack
                        ; str x0, [x1] // update on stack
                    );
                }
                Op::Pop => {
                    mdynasm!(ops
                        ; add sp, sp, #16
                    );
                },
                Op::JumpLabel { label_id} => {
                    let label = *labels.get(&label_id).unwrap();
                    mdynasm!(ops
                        ; =>label
                    );
                },
                Op::JumpIfNotZero { label_id } => {
                    let label = *labels.get(&label_id).unwrap();
                    mdynasm!(ops
                        ; ldr x0, [sp], #16
                        ; cbnz x0, =>label
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
        Ok(JIT { func: buf.ptr(offset), _exec_buf: buf })
    }

	pub fn run(&self) -> i64 {
		// Safety: the ExecutableBuffer is guaranteed to be valid
		let f: extern "C" fn() -> i64 = unsafe { std::mem::transmute(self.func) };
		f()
	}
}

#[cfg(test)]
mod jit_tests {
    use super::*;

    #[test]
    fn jit_arith() {
        let chunk = ByteCodeChunk {
            code: vec![
                Op::Constant { val: 2 },
                Op::Constant { val: 2 },
                Op::Add,
				Op::Constant { val: 5 },
				Op::Mul,
                Op::Return,
            ],
            in_arg: vec![],
            out_args: vec![],
        };
        let jit = JIT::compile(chunk).unwrap();
        let ret = jit.run();
        assert_eq!(ret, (2 + 2) * 5);
    }

    #[test]
    fn jit_vars() {
        let chunk = ByteCodeChunk {
            code: vec![
                Op::Constant { val: 2 },
                Op::Constant { val: 3 },
                Op::LoadVar { idx: 1 }, 
                Op::LoadVar {idx: 2},
                Op::Add,
                Op::Return,
            ],
            in_arg: vec![],
            out_args: vec![],
        };
        let jit = JIT::compile(chunk).unwrap();
        let ret = jit.run();
        assert_eq!(ret, 2);
    }
}
