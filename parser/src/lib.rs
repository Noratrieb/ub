#![warn(rust_2018_idioms)]
#![allow(dead_code)]

use std::path::PathBuf;

use logos::Logos;

use crate::lexer::Token;

mod ast;
mod lexer;
mod parser;

pub fn parse(_str: &str, _file_name: PathBuf) -> Result<ast::File, ()> {
    todo!()
}

pub fn test() {
    let lexer = Token::lexer(
        "fn foo() {
    1 + 5;
        struct test {}
    }",
    );
    let len = lexer.source().len();

    let r = parser::parse(lexer.spanned(), len, "test_file".into());

    println!("{r:#?}");
}
