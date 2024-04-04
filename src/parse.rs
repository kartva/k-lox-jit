#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Neg(Box<Expr>),
    Num(i64),
    Var(String),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    LessThanEq(Box<Expr>, Box<Expr>),
    GreaterThenEq(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    VarDecl {
        name: String,
        rhs: Option<Box<Expr>>,
    },
    Set {
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
            .then(
                expr.clone()
                    .padded()
                    .separated_by(just(','))
                    .delimited_by(just('('), just(')'))
                    .debug("call_args"),
            )
            .map(|(name, args)| Expr::Call(name, args));

        let op = |c| just(c).padded();

        let unary = op("-")
            .repeated()
            .then(call.or(atom))
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                op("*")
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(op("/").to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                op("+")
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(op("-").to(Expr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let cmp = sum
            .clone()
            .then(
                op("<")
                    .to(Expr::LessThan as fn(_, _) -> _)
                    .or(op(">").to(Expr::GreaterThan as fn(_, _) -> _))
                    .or(op("<=").to(Expr::LessThanEq as fn(_, _) -> _))
                    .or(op(">=").to(Expr::GreaterThenEq as fn(_, _) -> _))
                    .then(sum)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        cmp
    })
    .debug("expr");

    let var_decl = text::keyword("var")
        .padded()
        .ignore_then(ident.debug("var_name"))
        .then(
            just('=')
                .ignore_then(expr.clone().debug("var_rhs"))
                .or_not(),
        )
        .map(|(name, rhs)| Expr::VarDecl {
            name,
            rhs: rhs.map(Box::new),
        })
        .debug("var_decl");

    let var_set = ident
        .debug("var_name")
        .then_ignore(just('='))
        .then(expr.clone().debug("var_rhs"))
        .map(|(name, rhs)| Expr::Set {
            name,
            rhs: Box::new(rhs),
        })
        .debug("var_set");

    let stmt_block = (var_decl.or(var_set).or(expr.clone()))
        .separated_by(just(';'))
        .delimited_by(just('{'), just('}'))
        .debug("{stmt_block}");

    let r#fn = text::keyword("fn")
        .ignore_then(ident.debug("fn_name"))
        .then(ident
            .separated_by(just(','))
            .delimited_by(just('('), just(')'))
            .padded()
            .debug("arg_names")
        )
        .then(stmt_block.clone().padded())
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
    use super::*;

    #[test]
    fn test_function() {
        let src = "fn add x y { var x = x + y; x }";
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
                    Expr::VarDecl {
                        name: "x".to_string(),
                        rhs: Some(Box::new(Expr::Add(
                            Box::new(Expr::Var("x".to_string())),
                            Box::new(Expr::Var("y".to_string()))
                        ))),
                    },
                    Expr::Var("x".to_string())
                ),
            }])
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
                Expr::Fn {
                    name: "fst".to_string(),
                    args: vec!["x".to_string(), "y".to_string()],
                    body: vec!(Expr::Var("x".to_string())),
                },
                Expr::Fn {
                    name: "sec".to_string(),
                    args: vec!["x".to_string(), "y".to_string()],
                    body: vec!(Expr::Call(
                        "fst".to_string(),
                        vec!(Expr::Var("y".to_string()), Expr::Var("x".to_string()))
                    )),
                },
            ])
        );
    }
}
