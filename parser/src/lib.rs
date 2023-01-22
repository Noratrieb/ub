#![warn(rust_2018_idioms)]
#![allow(dead_code)]

use std::path::PathBuf;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use logos::Logos;

use crate::lexer::Token;

mod ast;
mod lexer;
mod parser;
mod pretty;

pub fn parse(_str: &str, _file_name: PathBuf) -> Result<ast::File, ()> {
    todo!()
}

pub fn test() {
    let src = "
fn main(uwu: u64, owo: ptr WOW) -> ptr u64 {
    let uwu = &1;
    let owo = yeet(1+2*3, AA);

    if 1 { 
        10;
    } else {}

    if 1 { if 1 { if 1 {} } }
}
";

    let lexer = Token::lexer(src);
    let len = lexer.source().len();
    let state = parser::ParserState::default();

    let (file, errors) = parser::parse(lexer.spanned(), &state, len, "test_file".into());

    if let Some(file) = file {
        println!("{}", pretty::pretty_print_ast(&file));
    }

    errors
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            report.finish().print(Source::from(&src)).unwrap();
        });
}
