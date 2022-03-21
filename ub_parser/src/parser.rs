use std::{ops::Range, path::PathBuf};

use chumsky::{prelude::*, Stream};

use crate::{
    ast::{BinOp, BinOpKind, Call, Expr, File, Literal, Stmt},
    lexer::Token,
};

type Error<'src> = Simple<Token<'src>>;
type Span = Range<usize>;

fn ident_parser<'src>() -> impl Parser<Token<'src>, &'src str, Error = Error<'src>> + Clone {
    filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("identifier")
}

fn expr_parser<'src>() -> impl Parser<Token<'src>, Expr, Error = Error<'src>> + Clone {
    recursive(|expr| {
        let literal = filter_map(|span, token| match token {
            Token::String(str) => Ok(Expr::Literal(Literal::String(
                str[1..str.len() - 2].to_owned(),
                span,
            ))),
            // todo lol unwrap
            Token::Integer(int) => Ok(Expr::Literal(Literal::Integer(int.parse().unwrap(), span))),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        })
        .labelled("literal");

        // A list of expressions
        let items = expr
            .clone()
            .chain(just(Token::Comma).ignore_then(expr.clone()).repeated())
            .then_ignore(just(Token::Comma).or_not())
            .or_not()
            .map(|item| item.unwrap_or_else(Vec::new));

        let array = items
            .clone()
            .delimited_by(just(Token::BracketO), just(Token::BracketC))
            .map(Expr::Array);

        let atom = literal
            .or(ident_parser().map(|str| Expr::Name(str.to_owned())))
            .or(array)
            .or(expr
                .clone()
                .delimited_by(just(Token::ParenO), just(Token::ParenC)));

        let call = atom
            .then(
                items
                    .delimited_by(just(Token::ParenO), just(Token::ParenC))
                    .repeated(),
            )
            .foldl(|callee, args| {
                Expr::Call(Call {
                    callee: Box::new(callee),
                    args,
                })
            });

        let op = just(Token::Asterisk)
            .to(BinOpKind::Mul)
            .or(just(Token::Slash).to(BinOpKind::Div));

        let product = call
            .clone()
            .then(op.then(call).repeated())
            .foldl(|a, (kind, b)| {
                Expr::BinOp(BinOp {
                    kind,
                    lhs: Box::new(a),
                    rhs: Box::new(b),
                    span: 0..0, // lol todo
                })
            });

        // Sum ops (add and subtract) have equal precedence
        let op = just(Token::Plus)
            .to(BinOpKind::Add)
            .or(just(Token::Minus).to(BinOpKind::Sub));
        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (kind, b)| {
                Expr::BinOp(BinOp {
                    kind,
                    lhs: Box::new(a),
                    rhs: Box::new(b),
                    span: 0..0, // lol todo
                })
            });

        // Comparison ops (equal, not-equal) have equal precedence
        let op = just(Token::EqEq)
            .to(BinOpKind::Eq)
            .or(just(Token::BangEq).to(BinOpKind::Neq));
        let compare = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (kind, b)| {
                Expr::BinOp(BinOp {
                    kind,
                    lhs: Box::new(a),
                    rhs: Box::new(b),
                    span: 0..0, // lol todo
                })
            });

        compare
    })
}

fn file_parser<'src>(
    file_name: PathBuf,
) -> impl Parser<Token<'src>, File, Error = Error<'src>> + Clone {
    expr_parser()
        .map(move |expr| File {
            name: file_name.clone(),
            items: vec![Stmt::Expr(expr)],
        })
        .then_ignore(just(Token::Semi))
}

pub fn parse<'src, I>(lexer: I, len: usize, file_name: PathBuf) -> (Option<File>, Vec<Error<'src>>)
where
    I: 'src,
    I: Iterator<Item = (Token<'src>, Span)>,
{
    file_parser(file_name).parse_recovery(Stream::from_iter(len..len + 1, lexer))
}

#[cfg(test)]
mod tests {
    use std::{fmt::Debug, path::PathBuf};

    use logos::Logos;

    use crate::lexer::Token;

    fn parse(src: &str) -> impl Debug + '_ {
        let lexer = Token::lexer(src);
        let len = lexer.source().len();

        super::parse(
            lexer.spanned(),
            len,
            PathBuf::from(module_path!().replace("::", "__")),
        )
    }

    #[test]
    fn addition() {
        let r = parse("1 + 4;");
        insta::assert_debug_snapshot!(r);
    }

    #[test]
    fn expression() {
        let r = parse("(4 / hallo()) + 5;");
        insta::assert_debug_snapshot!(r)
    }
}
