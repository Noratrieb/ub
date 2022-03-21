use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
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

    // keywords
    #[token("struct")]
    Struct,
    #[token("fn")]
    Fn,

    #[error]
    #[regex(r"[ \t\n\r\f]+")]
    Error,
}

pub fn lex(code: &str) -> logos::Lexer<'_, Token> {
    Token::lexer(code)
}
