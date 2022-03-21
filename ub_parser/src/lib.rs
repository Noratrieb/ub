#![warn(rust_2018_idioms)]
#![allow(dead_code)]

use std::path::PathBuf;

mod ast;
mod lexer;
mod span;

pub fn parse(_str: &str, _file_name: PathBuf) -> Result<ast::File, ()> {
    todo!()
}
