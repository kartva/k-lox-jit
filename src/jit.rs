use core::slice;
use std::io::{self, Write};

use log::debug;
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi, aarch64};
use crate::{value::Value, vm::{ByteCodeChunk, Op, VMError, VM}};

pub struct JIT {
	chunk: ByteCodeChunk,
}

#[derive(Debug)]
pub enum CompileError {
	OutOfBoundsConst(usize)
}

extern "C" fn print(buf: *const u8, len: u64) {
	let buf = unsafe { slice::from_raw_parts(buf, len as usize) };
	let s = std::str::from_utf8(buf).unwrap();
    io::stdout()
        .write_all(s.as_bytes())
        .unwrap();
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

impl JIT {
	pub fn new(chunk: ByteCodeChunk) -> Self {
		JIT {
			chunk,
		}
	}

	pub fn compile_and_run(self) -> Result<i64, CompileError> {
		debug!("Starting JIT");
		let mut ops = aarch64::Assembler::new().unwrap();
		let consts = self.chunk.consts;
		
		let offset = ops.offset();
		for op in self.chunk.code {
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
					mdynasm!(ops
						; ldr x0, [sp], #16
						; ldr x1, [sp], #16
						; add x0, x0, x1
						; str x0, [sp, #-16]!
					);
				}
				Op::Return => {
					mdynasm!(ops
						; ldr x0, [sp], #16
						; ret
					);
				}
			}
		}

		let buf = ops.finalize().unwrap();
		let compiled: unsafe extern "C" fn() -> i64 = unsafe { std::mem::transmute(buf.ptr(offset)) };

		// Safety: the ExecutableBuffer is alive and valid.
		Ok(unsafe { compiled() })
	}
}

#[cfg(test)]
mod jit_tests {
	use super::*;

	#[test]
	fn test_jit() {
		let chunk = ByteCodeChunk {
			consts: vec![
				Value(2),
				Value(2)
			],
			code: vec![
				Op::Constant { idx: 0 },
				Op::Constant { idx: 1 },
				Op::Add,
				Op::Return
			]
		};
		let jit = JIT::new(chunk);
		let ret = jit.compile_and_run().unwrap();
		assert_eq!(ret, 4);
	}
}