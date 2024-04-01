#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Neg(Box<Expr>),
    Num(i64),
    Var(String),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Let {
        name: String,
        rhs: Box<Expr>,
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Vec<Expr>,
    },
}

use chumsky::{combinator::Repeated, prelude::*};
fn parse() -> impl Parser<char, Vec<Expr>, Error = Simple<char>> {
    let ident = text::ident().padded();

    let expr = recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Num(s.parse().unwrap()))
            .padded();

        let atom = int
            .or(expr.clone().delimited_by(just('('), just(')')))
            .or(ident.debug("var").map(Expr::Var));

        let call = ident
            .debug("call_func_name")
            .then(Repeated::at_least(
                expr.clone().padded().debug("call_arg").repeated(),
                1,
            ))
            .map(|(name, args)| Expr::Call(name, args));

        let op = |c| just(c).padded();

        let unary = op('-')
            .repeated()
            .then(atom.or(call))
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                op('*')
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(op('/').to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                op('+')
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        sum
    })
    .debug("expr");

    let r#let = text::keyword("let")
        .padded()
        .ignore_then(ident.debug("let_name"))
        .then_ignore(just('='))
        .then(expr.clone().debug("let_rhs"))
        .map(|(name, rhs)| Expr::Let {
            name,
            rhs: Box::new(rhs),
        })
        .debug("let");

    let stmt_block = (r#let.or(expr.clone()))
        .separated_by(just(';').padded())
        .debug("stmt_block");

    let r#fn = text::keyword("fn")
        .ignore_then(ident.debug("fn_name"))
        .then(ident.repeated().debug("arg_name"))
        .then_ignore(just('{'))
        .then(stmt_block.clone())
        .then_ignore(just('}'))
        .map(|((name, args), body)| Expr::Fn { name, args, body })
        .debug("fn");

    let fns = Repeated::at_least(r#fn.padded().repeated(), 1);

    fns.then_ignore(end())
}

pub fn parse_text(src: &str) -> Result<Vec<Expr>, Vec<Simple<char>>> {
    parse().parse(src)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_function() {
        use super::*;
        let src = "fn add x y { let x = x + y; x }";
        let (expr, err) = parse().parse_recovery_verbose(src);
        if !err.is_empty() {
            panic!("{:?}", err);
        }
        assert_eq!(
            expr,
            Some(vec![Expr::Fn {
                name: "add".to_string(),
                args: vec!["x".to_string(), "y".to_string()],
                body: vec!(
                    Expr::Let {
                        name: "x".to_string(),
                        rhs: Box::new(Expr::Add(
                            Box::new(Expr::Var("x".to_string())),
                            Box::new(Expr::Var("y".to_string()))
                        )),
                    },
                    Expr::Var("x".to_string())
                ),
            }])
        );
    }
}
