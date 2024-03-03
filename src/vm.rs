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
	Pop,
	/// idx is zero-indexed
	LoadVar { idx: u32 },
	SetVar { idx: u32 },
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
}