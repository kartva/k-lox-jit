use std::{collections::HashMap, mem::{replace, take}};

use crate::{parse::Expr, vm::{ByteCode, ByteCodeChunk, Op}};

#[derive(Debug, Clone, Copy)]
struct StackIdx(u32);

struct MemoryManager {
	greatest_allocated: u32
}

impl MemoryManager {
	fn new() -> Self {
		MemoryManager {
			greatest_allocated: 0
		}
	}

	fn alloc(&mut self) -> StackIdx {
		self.greatest_allocated += 1;
		StackIdx(self.greatest_allocated)
	}
}

#[derive(Debug, Default)]
struct BlockCtx {
	bc: ByteCodeChunk,
	parent_ctx: Option<Box<BlockCtx>>,
	vars: HashMap<String, StackIdx>
}

impl BlockCtx {
	fn new() -> Self {
		BlockCtx {
			parent_ctx: None,
			bc: ByteCodeChunk {
				in_arg: Vec::new(),
				code: Vec::new(),
				out_args: Vec::new()
			},
			vars: HashMap::new()
		}
	}

	fn alloc(&mut self, name: String, ssa: &mut MemoryManager) -> StackIdx {
		let reg = ssa.alloc();
		self.vars.insert(name, reg);
		reg
	}

	fn get_mut(&mut self, name: &str) -> Option<&mut StackIdx> {
		self.vars.get_mut(name).or_else(|| self.parent_ctx.as_mut().and_then(|ctx| ctx.get_mut(name)))
	}

	fn get(&self, name: &str) -> Option<&StackIdx> {
		self.vars.get(name).or_else(|| self.parent_ctx.as_ref().and_then(|ctx| ctx.get(name)))
	}

	fn mutate_var(&mut self, name: &str, mut ssa: MemoryManager) {
		if let Some(existing_reg) = self.get_mut(name) {
			*existing_reg = ssa.alloc();
		}
	}

	fn create_inner_block_ctx(&mut self) {
		self.parent_ctx = Some(Box::new(take(self)));
	}

	fn finish_block(&mut self) -> ByteCodeChunk {
		let parent_ctx = take(&mut self.parent_ctx);
		let prev = replace(self, *parent_ctx.unwrap());
		prev.bc
	}
}

fn emit_expr<'a>(e: &Expr, bc: &mut BlockCtx, ssa: &mut MemoryManager) {
	match e {
		Expr::Num(n) => {
			bc.bc.code.push(Op::Constant { val: *n });
		},
		Expr::Add(lhs, rhs) => {
			emit_expr(lhs, bc, ssa);
			emit_expr(rhs, bc, ssa);
			bc.bc.code.push(Op::Add);
		},
		Expr::Sub(lhs, rhs) => {
			emit_expr(lhs, bc, ssa);
			emit_expr(rhs, bc, ssa);
			bc.bc.code.push(Op::Sub);
		},
		Expr::Mul(lhs, rhs) => {
			emit_expr(lhs, bc, ssa);
			emit_expr(rhs, bc, ssa);
			bc.bc.code.push(Op::Mul);
		},
		Expr::Div(lhs, rhs) => {
			emit_expr(lhs, bc, ssa);
			emit_expr(rhs, bc, ssa);
			bc.bc.code.push(Op::Div);
		},
		Expr::Var(name) => {
			let reg = bc.get(name.as_str()).expect("Variable not found");
			bc.bc.code.push(Op::LoadVar { idx: reg.0 });
		},
		_ => unimplemented!()
	}
}

fn emit_let(e: Expr, ctx: &mut BlockCtx, ssa: &mut MemoryManager) {
	if let Expr::Let { name, rhs } = e {
		emit_expr(&rhs, ctx, ssa);
		ctx.alloc(name, ssa); // emit_expr pushes result on stack
	} else {
		emit_expr(&e, ctx, ssa);
		ctx.bc.code.push(Op::Pop);
	}
}

fn emit_fn(e: Expr, ctx: &mut BlockCtx, mm: &mut MemoryManager, blocks: &mut Vec<ByteCodeChunk>) {
	if let Expr::Fn { name, args, mut body } = e {
		ctx.create_inner_block_ctx();

		for arg in args {
			ctx.alloc(arg, mm);
		}

		let ret = body.pop().expect("Expected return statement");
		for stmt in body.into_iter() {
			emit_let(stmt, ctx, mm);
		}

		emit_expr(&ret, ctx, mm);
		ctx.bc.code.push(Op::Return);

		let finished_block = ctx.finish_block();
		blocks.push(finished_block);
	} else {
		panic!("Expected function expression, found {e:?}");
	}
}

pub fn codegen(e: Vec<Expr>) -> ByteCode {
	let mut ssa = MemoryManager::new();
	let mut ctx = BlockCtx::new();
	let mut blocks = Vec::new();
	for f in e {
		emit_fn(f, &mut ctx, &mut ssa, &mut blocks);
	}

	ByteCode {
		chunks: blocks
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::parse::parse_text;

	#[test]
	fn test_codegen() {
		let e = parse_text("fn add x y { let x = x + y; x }").unwrap();
		let bc = codegen(e);

		assert_eq!(bc.chunks.len(), 1);
		eprintln!("{:?}", bc.chunks[0].code);
		assert_eq!(bc.chunks[0].code.len(), 6);
		assert_eq!(bc.chunks[0].code[0], Op::LoadVar { idx: 1 });
		assert_eq!(bc.chunks[0].code[1], Op::LoadVar { idx: 2 });
		assert_eq!(bc.chunks[0].code[2], Op::Add);
		assert_eq!(bc.chunks[0].code[3], Op::SetVar { idx: 1 });
		assert_eq!(bc.chunks[0].code[4], Op::LoadVar { idx: 1 });
		assert_eq!(bc.chunks[0].code[5], Op::Return);
	}
}