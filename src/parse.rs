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
    GreaterThanEq(Box<Spanned>, Box<Spanned>),
    Call(String, Vec<Spanned>),
    Return {
        expr: Box<Spanned>,
    },
    If {
        cond: Box<Spanned>,
        then: Vec<Spanned>,
        r#else: Option<Vec<Spanned>>,
    },
    While {
        cond: Box<Spanned>,
        body: Vec<Spanned>,
    },
    Break,
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

impl From<ExprTy> for Box<Spanned> {
    fn from(s: ExprTy) -> Self {
        Box::new(Spanned(s, 0..0))
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

use chumsky::{combinator::Repeated, prelude::*};

use crate::error::format_parse_errors;
fn parse() -> impl Parser<char, Vec<Spanned>, Error = Simple<char>> {
    let ident = text::ident().padded().labelled("ident");

    let delim = |c| just(c).padded();

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

        let op = |c| just(c).padded();

        let unary = op("-").map_with_span(|_, span: Range<usize>| span)
            .repeated()
            .then(call.or(atom))
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

        sum
            .clone()
            .then(
                op("<=").to(ExprTy::LessThanEq as fn(_, _) -> _)
                    .or(op(">=").to(ExprTy::GreaterThanEq as fn(_, _) -> _))
                    .or(op(">").to(ExprTy::GreaterThan as fn(_, _) -> _))
                    .or(op("<").to(ExprTy::LessThan as fn(_, _) -> _))
                    .then(sum)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| create_span_from_pair(lhs, op, rhs))
    })
    .debug("expr").labelled("expr");

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
        .debug("var_decl").labelled("var_decl");

    let var_set = ident
        .debug("var_name")
        .then_ignore(delim('='))
        .then(expr.clone().debug("var_rhs"))
        .map_with_span(|(name, rhs), span| Spanned(ExprTy::Set {
            name,
            rhs: Box::new(rhs),
        }, span))
        .debug("var_set").labelled("var_set");

    let r#return = text::keyword("return")
        .padded()
        .ignore_then(expr.clone().padded())
        .debug("return")
        .map_with_span(|expr, span| Spanned(ExprTy::Return {
            expr: Box::new(expr),
        }, span));

    let stmt_block = recursive(|stmt_block| {
        let r#if = text::keyword("if")
            .padded()
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

        let r#while = text::keyword("while")
            .padded()
            .ignore_then(expr.clone().padded().delimited_by(delim('('), delim(')')))
            .debug("while_cond")
            .then(stmt_block.clone().padded())
            .debug("while_body")
            .map_with_span(|(cond, body), span| Spanned(ExprTy::While {
                cond: Box::new(cond),
                body,
            }, span));

        Repeated::at_least(
            r#return.or(r#if).or(r#while).or(var_decl).or(var_set).or(expr.clone())
                .then_ignore(delim(';'))
                .repeated(),
                1
            )
        .delimited_by(delim('{'), delim('}'))
        .debug("{stmt_block}")
    });

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
    parse().parse(src).unwrap_or_else(|errs| panic!("{}", format_parse_errors(src, errs))) 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function() {
        let src = "fn add (x, y) { var x = x + y; return x; }";
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
                    ExprTy::Return {
                        expr: Box::new(ExprTy::Var("x".to_string()).into())
                    }.into()
                ],
            }.into()])
        );
    }

    #[test]
    fn test_functions_call() {
        let src = "fn fst (x, y) { return x; }
                   fn sec (x, y) { return fst (y, x); }";
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
                    body: vec![ExprTy::Return { expr: ExprTy::Var("x".to_string()).into() }.into()],
                }.into(),
                ExprTy::Fn {
                    name: "sec".to_string(),
                    args: vec!["x".to_string(), "y".to_string()],
                    body: vec![
                        ExprTy::Return {
                            expr: ExprTy::Call(
                                "fst".to_string(),
                                vec![ExprTy::Var("y".to_string()).into(), ExprTy::Var("x".to_string()).into()]
                            ).into()
                        }.into()
                    ],
                }.into(),
            ])
        );
    }

    #[test]
    fn test_if_stmts() {
        let src = "fn abs (x) { if (x < 0) { return -x; } else { return x; }; }";
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
                    cond: ExprTy::LessThan(
                        ExprTy::Var("x".to_string()).into(),
                        ExprTy::Num(0).into()
                    ).into(),
                    then: vec![ExprTy::Return { expr: ExprTy::Neg(ExprTy::Var("x".to_string()).into()).into() }.into()],
                    r#else: Some(vec![ExprTy::Return { expr:ExprTy::Var("x".to_string()).into() }.into() ]),
                }.into()),
            }.into()])
        );
    }

    #[test]
    fn test_while_stmts() {
        let src = "fn fact (n) { var acc = 1; var i = 1; while (i <= n) { acc = acc * i; i = i + 1; }; return acc; }";
        let (expr, err) = parse().parse_recovery_verbose(src);
        if !err.is_empty() {
            panic!("{:?}", err);
        }
        assert_eq!(
            expr,
            Some(vec![ExprTy::Fn {
                name: "fact".to_string(),
                args: vec!["n".to_string()],
                body: vec![
                    ExprTy::VarDecl {
                        name: "acc".to_string(),
                        rhs: Some(ExprTy::Num(1).into()),
                    }.into(),
                    ExprTy::VarDecl {
                        name: "i".to_string(),
                        rhs: Some(ExprTy::Num(1).into()),
                    }.into(),
                    ExprTy::While {
                        cond: ExprTy::LessThanEq(
                            ExprTy::Var("i".to_string()).into(),
                            ExprTy::Var("n".to_string()).into()
                        ).into(),
                        body: vec![
                            ExprTy::Set {
                                name: "acc".to_string(),
                                rhs: Box::new(ExprTy::Mul(
                                    ExprTy::Var("acc".to_string()).into(),
                                    ExprTy::Var("i".to_string()).into()
                                ).into()),
                            }.into(),
                            ExprTy::Set {
                                name: "i".to_string(),
                                rhs: Box::new(ExprTy::Add(
                                    ExprTy::Var("i".to_string()).into(),
                                    ExprTy::Num(1).into()
                                ).into()),
                            }.into(),
                        ],
                    }.into(),
                    ExprTy::Return { expr: ExprTy::Var("acc".to_string()).into() }.into(),
                ],
            }.into()])
        )
    }
}
