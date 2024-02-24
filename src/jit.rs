use core::slice;
use std::io::{self, Write};

use log::debug;
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi, aarch64};
use crate::{value::Value, vm::{ByteCodeChunk, Op, VMError, VM}};

pub struct JIT {
	chunk: ByteCodeChunk,
	stack: Vec<Value>
}

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

impl JIT {
	pub fn new(chunk: ByteCodeChunk) -> Self {
		JIT {
			chunk,
			stack: Vec::new()
		}
	}

	pub fn compile(self) -> Result<(), CompileError> {
		debug!("Starting JIT");
		let mut ops = aarch64::Assembler::new().unwrap();
		let consts = self.chunk.consts;
		
		let string = "Hello, world!\n";

		// for op in self.chunk.code {
		// 	match op {
		// 		Op::Constant { idx } => {
		// 			let constant = consts.get(idx).ok_or(CompileError::OutOfBoundsConst(idx))?;
		// 			let num = constant.0;
		// 			mdynasm!(ops
		// 				; mov x0, num as u64
		// 				; str x0, [sp, #-16]!
		// 			);
		// 		},
		// 		Op::Return => {
		// 			mdynasm!(ops
		// 				; ldr x0, [sp], #16
		// 				; ret
		// 			);
		// 		},
		// 		Op::Add => {
		// 			mdynasm!(ops
		// 				; ldr x0, [sp], #16
		// 				; ldr x1, [sp], #16
		// 				; add x0, x0, x1
		// 				; str x0, [sp, #-16]!
		// 			);
		// 		}
		// 	}
		// }

		dynasm!(
			ops
			; .arch aarch64
			; ->hello:
			; .bytes string.as_bytes()
			; .align 4 // LDR requires 4-byte alignment
			; ->print:
			; .qword print as _
			; .align 4
		);

		let hello = ops.offset();
		dynasm!(ops
			; .arch aarch64
			; adr x0, -> hello            // Message.
			; mov x1, string.len() as u64 // Length.
			; ldr x2, -> print            // Address of the `print` function.
			; br x2                       // Call the function (actually tail call).
		);

		let buf = ops.finalize().unwrap();
		let hello_fn: extern "C" fn() -> () = unsafe { std::mem::transmute(buf.ptr(hello)) };

		hello_fn();

		println!("it worked!");
		Ok(())
	}
}

#[cfg(test)]
mod jit_tests {
	use super::*;

	#[test]
	fn test_jit() {
		let chunk = ByteCodeChunk {
			consts: vec![],
			code: vec![]
		};
		let jit = JIT::new(chunk);
		assert!(jit.compile().is_ok());
	}
}