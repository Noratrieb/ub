use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
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
        let tokens = lex_test("{} [] () .,; = == != >= <= < > + - * / | || & && ^");
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
