use std::fmt;
use log::{debug, trace};

use crate::value::Value;

#[derive(Debug)]
pub enum Op {
	Constant { idx: usize },
	Add,
	Sub,
	Mul,
	Div,
	Return
}

#[derive(Debug)]
pub struct ByteCodeChunk {
	pub consts: Vec<Value>,
	pub code: Vec<Op>
}

pub struct VM {
	chunk: ByteCodeChunk,
	ip: usize,
	stack: Vec<Value>
}

pub enum VMError {
	OutOfBoundsIP(usize),
	OutOfBoundsConst(usize),
}

impl VM {
	pub fn new(chunk: ByteCodeChunk) -> Self {
		VM {
			chunk,
			ip: 0,
			stack: Vec::new()
		}
	}

	pub fn run(mut self) -> Result<(), VMError> {
		debug!("Starting VM");
		loop {
			let op = self.chunk.code.get(self.ip).ok_or(VMError::OutOfBoundsIP(self.ip))?;
			trace!("{:?}", op);

			let mut pop_stack = || self.stack.pop().unwrap();

			match op {
				Op::Constant { idx } => {
					let constant = self.chunk.consts.get(*idx).ok_or(VMError::OutOfBoundsConst(*idx))?;
					self.stack.push(*constant);
					println!("{:?}", constant);
				},
				Op::Return => {
					return Ok(());
				},
				Op::Add => {
					let (a, b) = (pop_stack(), pop_stack());
					self.stack.push(a + b);
				}
				Op::Sub => {
					let (a, b) = (pop_stack(), pop_stack());
					self.stack.push(a - b);
				}
				Op::Mul => {
					let (a, b) = (pop_stack(), pop_stack());
					self.stack.push(a * b);
				}
				Op::Div => {
					let (a, b) = (pop_stack(), pop_stack());
					self.stack.push(a / b);
				}
			}
			self.ip += 1;
		}
	}
}