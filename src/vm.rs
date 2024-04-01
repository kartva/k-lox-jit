#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Op {
	Constant { val: i64 },
	Add,
	Sub,
	Mul,
	Div,
	Pop,
	/// idx is zero-indexed
	LoadVar { idx: u32 },
	SetVar { idx: u32 },
	JumpLabel { label_id: usize },
	JumpIfNotZero { label_id: usize },
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
	pub in_arg: Vec<Var>,
	pub code: Vec<Op>,
	pub out_args: Vec<Var>
}