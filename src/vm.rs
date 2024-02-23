use std::fmt;

struct Constant (i32);

impl fmt::Debug for Constant {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Debug)]
enum OpCode {
	Constant = 0,
	Return
}

#[derive(Debug)]
struct ByteCodeChunk {
	consts: Vec<Constant>,
	code: Vec<u8>
}

struct VM {
	chunk: ByteCodeChunk,
	ip: usize
}

enum VMError {
	UnknownOpCode(u8),
	UnexpectedEof,
}

impl VM {
	fn new(chunk: ByteCodeChunk) -> Self {
		VM {
			chunk,
			ip: 0
		}
	}

	fn run(&mut self) -> Result<(), VMError> {
		while let Some(op) = self.next_byte() {
			match unsafe { std::mem::transmute::<u8, OpCode>(op) } {
				OpCode::Constant => {
					let constant = self.read_constant()?;
					println!("{:?}", constant);
				},
				OpCode::Return => {
					return Ok(());
				}
			}
		}
		Err(VMError::UnexpectedEof)
	}

	fn next_byte(&mut self) -> Option<u8> {
		let byte = self.chunk.code.get(self.ip)?;
		self.ip += 1;
		Some(*byte)
	}

	fn read_constant(&mut self) -> Result<&Constant, VMError> {
		let idx = self.next_byte().ok_or(VMError::UnexpectedEof)? as usize;
		Ok(&self.chunk.consts[idx])
	}
}