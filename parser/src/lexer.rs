use std::fmt::{Debug, Display, Formatter};

use logos::Logos;

#[derive(Logos, Debug, Clone, Hash, PartialEq, Eq)]
pub enum Token<'a> {
    #[regex("//[^\n]*", logos::skip)]
    Comment,

    // punctuation
    #[token("{")]
    BraceO,
    #[token("}")]
    BraceC,
    #[token("[")]
    BracketO,
    #[token("]")]
    BracketC,
    #[token("(")]
    ParenO,
    #[token(")")]
    ParenC,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("=")]
    Eq,
    #[token("==")]
    EqEq,
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEq,
    #[token(">")]
    Greater,
    #[token("<")]
    Less,
    #[token(">=")]
    GreaterEq,
    #[token("<=")]
    LessEq,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("|")]
    Or,
    #[token("&")]
    And,
    #[token("||")]
    OrOr,
    #[token("&&")]
    AndAnd,
    #[token("^")]
    Caret,
    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,

    // keywords
    #[token("struct")]
    Struct,
    #[token("fn")]
    Fn,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("loop")]
    Loop,

    #[regex(r"[a-zA-Z_]\w*")]
    Ident(&'a str),

    #[regex(r##""[^"]*""##)]
    String(&'a str),

    #[regex(r"\d+")]
    Integer(&'a str),

    #[error]
    #[regex(r"[ \t\r\n]+", logos::skip)]
    Error,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Comment => f.write_str("comment"),
            Token::BraceO => f.write_str("{"),
            Token::BraceC => f.write_str("}"),
            Token::BracketO => f.write_str("["),
            Token::BracketC => f.write_str("]"),
            Token::ParenO => f.write_str("("),
            Token::ParenC => f.write_str(")"),
            Token::Dot => f.write_str("."),
            Token::Comma => f.write_str(","),
            Token::Semi => f.write_str(";"),
            Token::Eq => f.write_str("="),
            Token::EqEq => f.write_str("=="),
            Token::Bang => f.write_str("!"),
            Token::BangEq => f.write_str("!="),
            Token::Greater => f.write_str(">"),
            Token::Less => f.write_str("<"),
            Token::GreaterEq => f.write_str(">="),
            Token::LessEq => f.write_str("<="),
            Token::Asterisk => f.write_str("*"),
            Token::Slash => f.write_str("/"),
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Or => f.write_str("|"),
            Token::And => f.write_str("&"),
            Token::OrOr => f.write_str("||"),
            Token::AndAnd => f.write_str("&&"),
            Token::Caret => f.write_str("^"),
            Token::Arrow => f.write_str("->"),
            Token::Colon => f.write_str(":"),
            Token::Struct => f.write_str("struct"),
            Token::Fn => f.write_str("fn"),
            Token::If => f.write_str("if"),
            Token::Else => f.write_str("else"),
            Token::While => f.write_str("while"),
            Token::Loop => f.write_str("loop"),
            Token::Ident(ident) => write!(f, "identifier `{ident}`"),
            Token::String(str) => write!(f, "\"{str}\""),
            Token::Integer(int) => write!(f, "{int}"),
            Token::Error => f.write_str("error"),
        }
    }
}

pub fn lex<'src>(code: &'src str) -> logos::Lexer<'_, Token<'src>> {
    Token::lexer(code)
}

#[cfg(test)]
mod tests {
    use crate::lexer::Token;

    fn lex_test(str: &str) -> Vec<Token<'_>> {
        let lexer = super::lex(str);
        lexer.collect()
    }

    #[test]
    fn punctuation() {
        let tokens = lex_test("{} [] () .,; = == != >= <= < > + - * / | || & && ^ -> :");
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn whitespace() {
        let tokens = lex_test(
            ". 
          \r\n \t .",
        );
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn idents() {
        let tokens = lex_test("hello w_world b235_");
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn literals() {
        let tokens = lex_test(r##""hello friend" 5 "morning" 3263475"##);
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn keywords() {
        let tokens = lex_test("struct fn . if else while loop;");
        insta::assert_debug_snapshot!(tokens);
    }
}
