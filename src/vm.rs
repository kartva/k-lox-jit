#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Op {
	Constant { val: i64 },
	Add,
	Sub,
	Mul,
	Div,
	Pop,
	LessThan,
	GreaterThan,
	LessThanEq,
	GreaterThanEq,
	/// idx is zero-indexed
	LoadVar { idx: u32 },
	SetVar { idx: u32 },
	JumpLabel { label_id: usize },
	JumpIfNotZero { label_id: usize },
	Call { idx: u32, word_argc: u32 },
	Return
}

#[derive(Debug)]
pub enum Var {
	Stack(usize),
	Reg(usize)
}

#[derive(Debug)]
pub struct ByteCode {
	pub chunks: Vec<ByteCodeChunk>
}

#[derive(Debug, Default)]
pub struct ByteCodeChunk {
	pub in_arg: u32,
	pub code: Vec<Op>,
	pub out_args: u32
}

impl ByteCodeChunk {
	pub fn push(&mut self, op: Op) {
		self.code.push(op);
	}
}