use std::collections::HashMap;

use crate::{parse::Expr, vm::{ByteCode, ByteCodeChunk, Op}};

#[derive(Debug, Clone, Copy)]
struct StackIdx(u32);

#[derive(Debug, Clone, Copy)]
struct FuncIdx(u32);

#[derive(Debug, Default)]
struct Scope {
	vars: HashMap<String, StackIdx>,
	fns: HashMap<String, FuncIdx>
}

impl Scope {
	fn new() -> Self {
		Default::default()
	}

	fn alloc(&mut self, name: String) -> StackIdx {
		let reg = StackIdx(self.vars.len() as u32);
		self.vars.insert(name, reg);
		reg
	}
}

#[derive(Debug, Default)]
struct CodegenCtx {
	chunks: Vec<ByteCodeChunk>,
	scopes: Vec<Scope>,
}

impl CodegenCtx {
	fn new() -> Self {
		Default::default()
	}

	fn scope(&mut self) -> &mut Scope {
		self.scopes.last_mut().unwrap()
	}

	fn block(&mut self) -> &mut ByteCodeChunk {
		self.chunks.last_mut().unwrap()
	}

	fn alloc(&mut self, name: String) -> StackIdx {
		self.scope().alloc(name)
	}

	/// Function code blocks are allocated on a global level
	fn alloc_fn(&mut self, name: String) -> FuncIdx {
		let idx = FuncIdx(self.chunks.len() as u32);
		self.scope().fns.insert(name, idx);
		idx
	}

	fn get_fn(&self, name: &str) -> Option<FuncIdx> {
		for scope in self.scopes.iter().rev() {
			if let Some(reg) = scope.fns.get(name) {
				return Some(*reg);
			}
		}
		None
	}

	fn get(&self, name: &str) -> Option<StackIdx> {
		for scope in self.scopes.iter().rev() {
			if let Some(reg) = scope.vars.get(name) {
				return Some(*reg);
			}
		}
		None
	}

	fn start_new_scope(&mut self) {
		self.scopes.push(Scope::new());
	}

	fn start_new_block(&mut self) {
		self.chunks.push(ByteCodeChunk::default());
	}

	fn finish_scope(&mut self) {
		self.scopes.pop().unwrap();
	}

	fn finish(self) -> ByteCode {
		ByteCode {
			chunks: self.chunks
		}
	}
}

fn emit_expr(e: &Expr, cx: &mut CodegenCtx) {
	match e {
		Expr::Num(n) => {
			cx.block().push(Op::Constant { val: *n });
		},
		Expr::Add(lhs, rhs) => {
			emit_expr(lhs, cx);
			emit_expr(rhs, cx);
			cx.block().push(Op::Add);
		},
		Expr::Sub(lhs, rhs) => {
			emit_expr(lhs, cx);
			emit_expr(rhs, cx);
			cx.block().push(Op::Sub);
		},
		Expr::Mul(lhs, rhs) => {
			emit_expr(lhs, cx);
			emit_expr(rhs, cx);
			cx.block().push(Op::Mul);
		},
		Expr::Div(lhs, rhs) => {
			emit_expr(lhs, cx);
			emit_expr(rhs, cx);
			cx.block().push(Op::Div);
		},
		Expr::Var(name) => {
			let reg = cx.get(name.as_str()).expect("Variable not found");
			cx.block().push(Op::LoadVar { idx: reg.0 });
		},
		Expr::LessThan(lhs, rhs) => {
			emit_expr(lhs, cx);
			emit_expr(rhs, cx);
			cx.block().push(Op::LessThan);
		},
		Expr::GreaterThan(lhs, rhs) => {
			emit_expr(lhs, cx);
			emit_expr(rhs, cx);
			cx.block().push(Op::GreaterThan);
		},
		Expr::LessThanEq(lhs, rhs) => {
			emit_expr(lhs, cx);
			emit_expr(rhs, cx);
			cx.block().push(Op::LessThanEq);
		},
		Expr::GreaterThenEq(lhs, rhs) => {
			emit_expr(lhs, cx);
			emit_expr(rhs, cx);
			cx.block().push(Op::GreaterThanEq);
		},
		Expr::Call(name, args) => {
			for arg in args {
				emit_expr(arg, cx);
			}
			let call = Op::Call { idx: cx.get_fn(name).unwrap().0, word_argc: args.len() as u32 };
			cx.block().push(call);
		},
		_ => unimplemented!()
	}
}

fn emit_stmt(e: Expr, ctx: &mut CodegenCtx) {
	match e {
		Expr::VarDecl { name, rhs } => {
			if let Some(rhs) = rhs {
				emit_expr(&rhs, ctx);
			} else {
				// placeholder nil value
				ctx.block().push(Op::Constant { val: 0 });
			}
			ctx.alloc(name); // emit_expr pushes result on stack
		},
		Expr::Set { name, rhs } => {
			emit_expr(&rhs, ctx);
			let reg = ctx.get(name.as_str()).expect("Variable not found");
			ctx.block().push(Op::SetVar { idx: reg.0 });
			ctx.block().push(Op::Pop);
		},
		_ => {
			emit_expr(&e, ctx);
			ctx.block().push(Op::Pop);
		}
	}
}

fn emit_fn(e: Expr, ctx: &mut CodegenCtx) {
	if let Expr::Fn { name, args, mut body } = e {
		let fn_idx = ctx.alloc_fn(name.clone());
		ctx.start_new_block();
		ctx.start_new_scope();

		for arg in args {
			ctx.alloc(arg);
		}

		let ret = body.pop().expect("Expected return statement");
		for stmt in body.into_iter() {
			emit_stmt(stmt, ctx);
		}

		emit_expr(&ret, ctx);
		ctx.block().push(Op::Return);
		ctx.finish_scope();
	} else {
		panic!("Expected function expression, found {e:?}");
	}
}

pub fn codegen(e: Vec<Expr>) -> ByteCode {
	let mut ctx = CodegenCtx::new();

	ctx.start_new_scope();
	for f in e {
		emit_fn(f, &mut ctx);
	}

	ctx.finish_scope();
	ctx.finish()
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::parse::parse_text;

	#[test]
	fn test_codegen() {
		let e = parse_text("fn add (x, y) { var z = x + y; z }").unwrap();
		let bc = codegen(e);

		assert_eq!(bc.chunks.len(), 1);
		eprintln!("{:?}", bc.chunks[0].code);
		assert_eq!(bc.chunks[0].code.len(), 5);
		assert_eq!(bc.chunks[0].code[0], Op::LoadVar { idx: 0 });
		assert_eq!(bc.chunks[0].code[1], Op::LoadVar { idx: 1 });
		assert_eq!(bc.chunks[0].code[2], Op::Add);
		assert_eq!(bc.chunks[0].code[3], Op::LoadVar { idx: 2 });
		assert_eq!(bc.chunks[0].code[4], Op::Return);
	}
}