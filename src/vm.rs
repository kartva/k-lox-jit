#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Op {
	Constant { val: i64 },
	Add,
	Sub,
	Mul,
	Div,
	Pop { count: u32 },
	LessThan,
	GreaterThan,
	LessThanEq,
	GreaterThanEq,
	/// idx is zero-indexed
	LoadVar { stack_idx: usize },
	SetVar { stack_idx: usize },
	JumpLabel { label_id: usize },
	Jump { label_id: usize },
	JumpIfZero { label_id: usize },
	Call { fn_idx: u32, word_argc: u32 },
	CallExternal { ext_call: ExternalCall },
	Return
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ExternalCall {
	Print
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