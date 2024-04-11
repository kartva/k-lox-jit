use std::{collections::HashMap, ops::Range};

use crate::{parse::{ExprTy, Spanned}, vm::{ByteCode, ByteCodeChunk, Op}};
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

#[derive(Debug, Clone, Copy)]
struct StackIdx(u32);

#[derive(Debug, Clone, Copy)]
struct FuncIdx(u32);

#[derive(Debug, Default)]
enum ScopeTy {
	#[default]
	None,
	While { end_label: usize},
}

#[derive(Debug, Default)]
struct Scope {
	ty: ScopeTy,
	start_idx: u32,
	vars: HashMap<String, StackIdx>,
	fns: HashMap<String, FuncIdx>
}

impl Scope {
	fn new(parent_scope: Option<&Scope>) -> Self {
		Scope::new_with_type(parent_scope, ScopeTy::None)
	}

	fn new_with_type(parent_scope: Option<&Scope>, ty: ScopeTy) -> Self {
		Scope {
			ty,
			start_idx: parent_scope.map(|p| p.start_idx + p.vars.len() as u32).unwrap_or(0),
			..Default::default()
		}
	}

	fn alloc(&mut self, name: String) -> StackIdx {
		let reg = StackIdx(self.start_idx + self.vars.len() as u32);
		self.vars.insert(name, reg);
		reg
	}
}

#[derive(Debug, Default)]
struct CodegenCtx {
	chunks: Vec<ByteCodeChunk>,
	scopes: Vec<Scope>,
	max_labels: usize,
	err_report: Vec<ariadne::Report<'static, Range<usize>>>
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

	fn alloc_label(&mut self) -> usize {
		let new_label = self.max_labels;
		self.max_labels += 1;
		new_label
	}

	/// Function block indices are allocated on a global level
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
		self.scopes.push(Scope::new(self.scopes.last()));
	}

	fn start_new_scope_with_type(&mut self, ty: ScopeTy) {
		self.scopes.push(Scope::new_with_type(self.scopes.last(), ty));
	}

	fn start_new_block(&mut self) {
		self.chunks.push(ByteCodeChunk::default());
	}

	/// Finish the current scope and pop all variables
	/// and push the result of the scope (topmost stack value) to the stack
	fn finish_scope(&mut self) {
		if !self.scope().vars.is_empty() {
			let result_set = Op::SetVar { stack_idx: self.scope().start_idx };
			self.block().push(result_set);
		
			// pop used variables
			for _ in 0..self.scope().vars.len() {
				self.block().push(Op::Pop);
			}
		}
	
		self.scopes.pop().unwrap();
	}

	fn finish(self) -> Result<ByteCode, Vec<ariadne::Report<'static, Range<usize>>>> {
		if !self.err_report.is_empty() {
			Err(self.err_report)
		} else {
			Ok(ByteCode {
				chunks: self.chunks
			})
		}
	}

	fn register_new_report(&mut self, report: ariadne::Report<'static, Range<usize>>) {
		self.err_report.push(report);		
	}
}

fn emit_expr(Spanned(e, span): &Spanned, ctx: &mut CodegenCtx) {
	match e {
		ExprTy::Num(n) => {
			ctx.block().push(Op::Constant { val: *n });
		},
		ExprTy::Add(lhs, rhs) => {
			emit_expr(lhs, ctx);
			emit_expr(rhs, ctx);
			ctx.block().push(Op::Add);
		},
		ExprTy::Sub(lhs, rhs) => {
			emit_expr(lhs, ctx);
			emit_expr(rhs, ctx);
			ctx.block().push(Op::Sub);
		},
		ExprTy::Mul(lhs, rhs) => {
			emit_expr(lhs, ctx);
			emit_expr(rhs, ctx);
			ctx.block().push(Op::Mul);
		},
		ExprTy::Div(lhs, rhs) => {
			emit_expr(lhs, ctx);
			emit_expr(rhs, ctx);
			ctx.block().push(Op::Div);
		},
		ExprTy::Var(name) => {
			match ctx.get(name.as_str()) {
				Some(reg) => ctx.block().push(Op::LoadVar { stack_idx: reg.0 }),
				None => {
					ctx.register_new_report(Report::build(ReportKind::Error, (), span.start)
						.with_message(format!("Undeclared variable {}", name))
						.with_label(
							Label::new(span.clone())
								.with_message("Undeclared variable")
								.with_color(Color::Red))
						.finish());
				}
			}
		},
		ExprTy::LessThan(lhs, rhs) => {
			emit_expr(lhs, ctx);
			emit_expr(rhs, ctx);
			ctx.block().push(Op::LessThan);
		},
		ExprTy::GreaterThan(lhs, rhs) => {
			emit_expr(lhs, ctx);
			emit_expr(rhs, ctx);
			ctx.block().push(Op::GreaterThan);
		},
		ExprTy::LessThanEq(lhs, rhs) => {
			emit_expr(lhs, ctx);
			emit_expr(rhs, ctx);
			ctx.block().push(Op::LessThanEq);
		},
		ExprTy::GreaterThanEq(lhs, rhs) => {
			emit_expr(lhs, ctx);
			emit_expr(rhs, ctx);
			ctx.block().push(Op::GreaterThanEq);
		},
		ExprTy::Call(name, args) => {
			for arg in args {
				emit_expr(arg, ctx);
			}

			match ctx.get_fn(name) {
				Some(fn_idx) => {
					ctx.block().push(Op::Call { fn_idx: fn_idx.0, word_argc: args.len() as u32 });
				},
				None => {
					ctx.register_new_report(Report::build(ReportKind::Error, (), span.start)
						.with_message(format!("Undeclared function {}", name))
						.with_label(
							Label::new(span.clone())
								.with_message("Undeclared function")
								.with_color(Color::Red))
						.finish());
				}
			}
		},
		ExprTy::If { cond, then, r#else } => {
			emit_expr(cond, ctx);
			let if_lbl = ctx.alloc_label();
			ctx.block().push(Op::JumpIfZero { label_id: if_lbl });

			ctx.start_new_scope();
			emit_stmt_block(then.as_slice(), ctx);
			ctx.finish_scope();

			if let Some(r#else) = r#else {
				let else_lbl = ctx.alloc_label();
				ctx.block().push(Op::Jump { label_id: else_lbl });
				ctx.block().push(Op::JumpLabel { label_id: if_lbl });

				ctx.start_new_scope();
				emit_stmt_block(r#else.as_slice(), ctx);
				ctx.finish_scope();

				ctx.block().push(Op::JumpLabel { label_id: else_lbl });
			} else {
				ctx.block().push(Op::JumpLabel { label_id: if_lbl });
			}
		},
		_ => {
			eprintln!("Unimplemented: {e:?}");
			unimplemented!()
		}
	}
}

fn emit_stmt(node: &Spanned, ctx: &mut CodegenCtx) {
	let Spanned(e, span) = node;
	match e {
		ExprTy::VarDecl { name, rhs } => {
			if let Some(rhs) = rhs {
				emit_expr(rhs, ctx);
			} else {
				// placeholder nil value
				ctx.block().push(Op::Constant { val: 0 });
			}
			ctx.alloc(name.clone()); // emit_expr pushes result on stack
		},
		ExprTy::Set { name, rhs } => {
			emit_expr(rhs, ctx);
			let reg = ctx.get(name.as_str()).expect("Variable not found");
			ctx.block().push(Op::SetVar { stack_idx: reg.0 });
			ctx.block().push(Op::Pop);
		},
		ExprTy::While { cond, body } => {
			let start_label = ctx.alloc_label();
			let end_label = ctx.alloc_label();

			ctx.block().push(Op::JumpLabel { label_id: start_label });
			emit_expr(cond, ctx);
			ctx.block().push(Op::JumpIfZero { label_id: end_label });

			ctx.start_new_scope_with_type(ScopeTy::While { end_label });
			emit_stmt_block(body.as_slice(), ctx);
			ctx.finish_scope();

			ctx.block().push(Op::Jump { label_id: start_label });
			ctx.block().push(Op::JumpLabel { label_id: end_label });
		},
		ExprTy::Break => {
			let while_scope = ctx.scopes.iter().rev().find(|s| matches!(s.ty, ScopeTy::While { .. }));
			match while_scope {
				Some (Scope {ty: ScopeTy::While { end_label }, .. }) => {
					let jmp = Op::Jump { label_id: *end_label };
					ctx.block().push(jmp);
				},
				_ => {
					ctx.register_new_report(Report::build(ReportKind::Error, (), span.start)
						.with_message("Break statement outside of loop")
						.with_label(Label::new(span.clone())
							.with_message("Break statement outside of loop")
							.with_color(Color::Red))
						.finish());
				}
			}
		},
		ExprTy::Return { expr } => {
			emit_expr(expr, ctx);
			ctx.block().push(Op::Return);
		},
		_ => {
			emit_expr(node, ctx);
			ctx.block().push(Op::Pop);
		}
	}
}

fn emit_stmt_block(body: &[Spanned], ctx: &mut CodegenCtx) {
	for stmt in body {
		emit_stmt(stmt, ctx);
	}
}

fn emit_fn(Spanned(e, span): Spanned, ctx: &mut CodegenCtx) {
	if let ExprTy::Fn { name, args, body } = e {
		let fn_idx = ctx.alloc_fn(name.clone());
		ctx.start_new_block();
		ctx.start_new_scope();

		ctx.block().in_arg = args.len() as u32;
		for arg in args {
			ctx.alloc(arg);
		}

		emit_stmt_block(body.as_slice(), ctx);

		ctx.finish_scope();

		// push null value in case return hasn't happened already.
		ctx.block().push(Op::Constant { val: 0 });
		ctx.block().push(Op::Return);
	} else {
		let error_report = Report::build(ReportKind::Error, (), span.start)
			.with_message(format!("Expected function expression, found {:?}", e))
			.with_label(Label::new(span.clone()).with_message("Expected function expression"))
			.finish();
		ctx.register_new_report(error_report);
	}
}

pub fn codegen(e: Vec<Spanned>) -> Result<ByteCode, Vec<Report<'static, Range<usize>>>> {
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
		let e = parse_text("fn add (x, y) { var z = x + y; z }");
		let bc = codegen(e).unwrap();

		assert_eq!(bc.chunks.len(), 1);
		eprintln!("{:?}", bc.chunks[0].code); 
		assert_eq!(bc.chunks[0].code, [
			Op::LoadVar { stack_idx: 0 },
			Op::LoadVar { stack_idx: 1 },
			Op::Add,
			Op::LoadVar { stack_idx: 2 },
			Op::SetVar { stack_idx: 0 }, 
			Op::Pop, // pop z
			Op::Pop, // pop y
			Op::Pop, // pop x
			Op::Return
		]);
	}

	#[test]
	fn test_if_codegen() {
		let e = parse_text("fn test (x) { if (x) { x } else { 3 } }");
		let bc = codegen(e).unwrap();

		assert_eq!(bc.chunks.len(), 1);
		assert_eq!(bc.chunks[0].code, [
			Op::LoadVar { stack_idx: 0 },
			Op::JumpIfZero { label_id: 0 }, // eval condition
			Op::LoadVar { stack_idx: 0 }, 		// if true (1), continue down if branch
			Op::Jump { label_id: 1 }, 		// jump to end of if-else
			Op::JumpLabel { label_id: 0 },  // if false (0), jump to else branch
			Op::Constant { val: 3 }, 	    // else branch
			Op::JumpLabel { label_id: 1 },  // end of if-else
			Op::SetVar { stack_idx: 0 },	// store result of if-else (by overwriting x's place on stack)
			Op::Pop,						// pop upper values
			Op::Return
		]);
	}
}