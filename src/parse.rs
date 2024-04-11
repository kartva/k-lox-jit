#[derive(Debug, PartialEq, Eq)]
pub enum ExprTy {
    Neg(Box<Spanned>),
    Num(i64),
    Var(String),
    Mul(Box<Spanned>, Box<Spanned>),
    Div(Box<Spanned>, Box<Spanned>),
    Add(Box<Spanned>, Box<Spanned>),
    Sub(Box<Spanned>, Box<Spanned>),
    LessThan(Box<Spanned>, Box<Spanned>),
    GreaterThan(Box<Spanned>, Box<Spanned>),
    LessThanEq(Box<Spanned>, Box<Spanned>),
    GreaterThenEq(Box<Spanned>, Box<Spanned>),
    Call(String, Vec<Spanned>),
    If {
        cond: Box<Spanned>,
        then: Vec<Spanned>,
        r#else: Option<Vec<Spanned>>,
    },
    VarDecl {
        name: String,
        rhs: Option<Box<Spanned>>,
    },
    Set {
        name: String,
        rhs: Box<Spanned>,
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Vec<Spanned>,
    },
}

use std::ops::Range;

#[derive(Debug)]
pub struct Spanned<T = ExprTy>(pub T, pub Range<usize>);

impl From<ExprTy> for Spanned {
    fn from(s: ExprTy) -> Self {
        Spanned(s, 0..0)
    }
}

impl PartialEq for Spanned {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Spanned {}

fn create_span_from_pair(lhs: Spanned, op: fn(Box<Spanned>, Box<Spanned>) -> ExprTy, rhs: Spanned) -> Spanned {
    let span = lhs.1.start..rhs.1.end;
    Spanned(op(Box::new(lhs), Box::new(rhs)), span)
}

use chumsky::{combinator::Repeated, prelude::*, Span};

use crate::parse_err::format_errors;
fn parse() -> impl Parser<char, Vec<Spanned>, Error = Simple<char>> {
    let ident = text::ident().padded();

    let delim = |c| just(c).padded();

    let mut stmt_block = Recursive::<_, _, Simple<char>>::declare();
    let expr = recursive(|expr| {
        let int = text::int(10)
            .map_with_span(|s: String, span| Spanned(ExprTy::Num(s.parse().unwrap()), span))
            .padded();

        let atom = int
            .or(expr.clone().delimited_by(delim('('), delim(')')))
            .or(ident.debug("var").map_with_span(|atom, span| Spanned(ExprTy::Var(atom), span)));

        let call = ident
            .debug("call_func_name")
            .then(
                expr.clone()
                    .padded()
                    .separated_by(delim(','))
                    .delimited_by(delim('('), delim(')'))
                    .debug("call_args"),
            )
            .map_with_span(|(name, args), span| Spanned(ExprTy::Call(name, args), span));

        let r#if = text::keyword("if")
            .padded()
            .debug("if")
            .ignore_then(expr.clone().padded().delimited_by(delim('('), delim(')')))
            .debug("if_cond")
            .then(stmt_block.clone().padded())
            .debug("if_then")
            .then(
                text::keyword("else")
                    .padded()
                    .ignore_then(stmt_block.clone().padded())
                    .debug("if_else")
                    .or_not(),
            )
            .map_with_span(|((cond, then), r#else), span| Spanned(ExprTy::If {
                cond: Box::new(cond),
                then,
                r#else,
            }, span));

        let op = |c| just(c).padded();

        let unary = op("-").map_with_span(|_, span: Range<usize>| span)
            .repeated()
            .then(r#if.or(call).or(atom))
            .foldr(|neg: Range<usize>, rhs: Spanned| {
                let span = neg.start..rhs.1.end;
                Spanned(ExprTy::Neg(Box::new(rhs)), span)
            });

        let product = unary
            .clone()
            .then(
                op("*")
                    .to(ExprTy::Mul as fn(Box<Spanned>, Box<Spanned>) -> ExprTy)
                    .or(op("/").to(ExprTy::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| create_span_from_pair(lhs, op, rhs));

        let sum = product
            .clone()
            .then(
                op("+")
                    .to(ExprTy::Add as fn(Box<Spanned>, Box<Spanned>) -> ExprTy)
                    .or(op("-").to(ExprTy::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| create_span_from_pair(lhs, op, rhs));

        let cmp = sum
            .clone()
            .then(
                op("<")
                    .to(ExprTy::LessThan as fn(_, _) -> _)
                    .or(op(">").to(ExprTy::GreaterThan as fn(_, _) -> _))
                    .or(op("<=").to(ExprTy::LessThanEq as fn(_, _) -> _))
                    .or(op(">=").to(ExprTy::GreaterThenEq as fn(_, _) -> _))
                    .then(sum)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| create_span_from_pair(lhs, op, rhs));

        cmp
    })
    .debug("expr");

    let var_decl = text::keyword("var")
        .padded()
        .ignore_then(ident.debug("var_name"))
        .then(
            delim('=')
                .ignore_then(expr.clone().debug("var_rhs"))
                .or_not(),
        )
        .map_with_span(|(name, rhs), span| Spanned(ExprTy::VarDecl {
            name,
            rhs: rhs.map(Box::new),
        }, span))
        .debug("var_decl");

    let var_set = ident
        .debug("var_name")
        .then_ignore(delim('='))
        .then(expr.clone().debug("var_rhs"))
        .map_with_span(|(name, rhs), span| Spanned(ExprTy::Set {
            name,
            rhs: Box::new(rhs),
        }, span))
        .debug("var_set");

    stmt_block.define((var_decl).or(var_set).or(expr.clone()).separated_by(delim(';'))
            .delimited_by(delim('{'), delim('}'))
            .debug("{stmt_block}"));

    let r#fn = text::keyword("fn")
        .padded()
        .ignore_then(ident.debug("fn_name"))
        .then(
            ident
                .separated_by(delim(','))
                .delimited_by(delim('('), delim(')'))
                .padded()
                .debug("arg_names"),
        )
        .then(stmt_block.clone().padded())
        .map_with_span(|((name, args), body), span| Spanned(ExprTy::Fn { name, args, body }, span))
        .debug("fn");

    let fns = Repeated::at_least(r#fn.padded().repeated(), 1);

    fns.then_ignore(end())
}

pub fn parse_text(src: &str) -> Vec<Spanned> {
    parse().parse(src).unwrap_or_else(|errs| panic!("{}", format_errors(src, errs))) 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function() {
        let src = "fn add (x, y) { var x = x + y; x }";
        let (expr, err) = parse().parse_recovery_verbose(src);
        if !err.is_empty() {
            panic!("{:?}", err);
        }
        assert_eq!(
            expr,
            Some(vec![ExprTy::Fn {
                name: "add".to_string(),
                args: vec!["x".to_string(), "y".to_string()],
                body: vec![
                    ExprTy::VarDecl {
                        name: "x".to_string(),
                        rhs: Some(Box::new(ExprTy::Add(
                            Box::new(ExprTy::Var("x".to_string()).into()),
                            Box::new(ExprTy::Var("y".to_string()).into())
                        ).into())),
                    }.into(),
                    ExprTy::Var("x".to_string()).into()
                ],
            }.into()])
        );
    }

    #[test]
    fn test_functions_call() {
        let src = "fn fst (x, y) { x }
                   fn sec (x, y) { fst (y, x) }";
        let (expr, err) = parse().parse_recovery_verbose(src);
        if !err.is_empty() {
            panic!("{:?}", err);
        }
        assert_eq!(
            expr,
            Some(vec![
                ExprTy::Fn {
                    name: "fst".to_string(),
                    args: vec!["x".to_string(), "y".to_string()],
                    body: vec!(ExprTy::Var("x".to_string()).into()),
                }.into(),
                ExprTy::Fn {
                    name: "sec".to_string(),
                    args: vec!["x".to_string(), "y".to_string()],
                    body: vec!(ExprTy::Call(
                        "fst".to_string(),
                        vec!(ExprTy::Var("y".to_string()).into(), ExprTy::Var("x".to_string()).into())
                    ).into()),
                }.into(),
            ])
        );
    }

    #[test]
    fn test_if_stmts() {
        let src = "fn abs (x) { if (x < 0) { -x } else { x } }";
        let (expr, err) = parse().parse_recovery_verbose(src);
        if !err.is_empty() {
            panic!("{:?}", err);
        }
        assert_eq!(
            expr,
            Some(vec![ExprTy::Fn {
                name: "abs".to_string(),
                args: vec!["x".to_string()],
                body: vec!(ExprTy::If {
                    cond: Box::new(ExprTy::LessThan(
                        Box::new(ExprTy::Var("x".to_string()).into()),
                        Box::new(ExprTy::Num(0).into())
                    ).into()),
                    then: vec!(ExprTy::Neg(Box::new(ExprTy::Var("x".to_string()).into())).into()),
                    r#else: Some(vec!(ExprTy::Var("x".to_string()).into())),
                }.into()),
            }.into()])
        );
    }
}
