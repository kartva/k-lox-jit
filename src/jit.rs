use core::slice;
use std::io::{self, Write};

use crate::{
    value::Value,
    vm::{ByteCodeChunk, Op, VMError, VM},
};
use dynasmrt::{aarch64, dynasm, AssemblyOffset, DynasmApi, DynasmLabelApi, ExecutableBuffer};
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
            $($t)*
        )
    }
}

/// Loads arg1 and arg2 into x0 and x1, and stores the result in x0.
macro_rules! two_arg_one_ret {
    ($ops:ident $($t:tt)*) => {
        mdynasm!($ops
			; ldr x0, [sp], #16
			; ldr x1, [sp], #16
            $($t)*
			; str x0, [sp, #-16]!
        )
    }
}

pub struct JIT {
    exec_buf: ExecutableBuffer,
	func: *const u8,
}

impl JIT {
    pub fn compile(chunk: ByteCodeChunk) -> Result<JIT, CompileError> {
        debug!("Starting JIT");
        let mut ops = aarch64::Assembler::new().unwrap();
        let consts = chunk.consts;

        let offset = ops.offset();
        for op in chunk.code {
            match op {
                Op::Constant { idx } => {
                    let constant = consts.get(idx).ok_or(CompileError::OutOfBoundsConst(idx))?;
                    let num = constant.0;
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
                Op::Return => {
                    mdynasm!(ops
                        ; ldr x0, [sp], #16
                        ; ret
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
                }
            }
        }

        let buf = ops.finalize().unwrap();
        Ok(JIT { func: buf.ptr(offset), exec_buf: buf })
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
    fn test_jit() {
        let chunk = ByteCodeChunk {
            consts: vec![Value(2), Value(2), Value(5)],
            code: vec![
                Op::Constant { idx: 0 },
                Op::Constant { idx: 1 },
                Op::Add,
				Op::Constant { idx: 2 },
				Op::Mul,
                Op::Return,
            ],
        };
        let jit = JIT::compile(chunk).unwrap();
        let ret = jit.run();
        assert_eq!(ret, (2 + 2) * 5);
    }
}
