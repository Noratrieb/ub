#![warn(rust_2018_idioms)]
#![allow(dead_code)]

use std::path::PathBuf;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use parser::Error;

mod ast;
mod lexer;
mod parser;
mod pretty;

#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,
    #[return_ref]
    pub file_name: PathBuf,
}

#[salsa::jar(db = Db)]
pub struct Jar(SourceProgram, Diagnostics, crate::parser::parse);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[salsa::accumulator]
pub struct Diagnostics(Error);

#[derive(Default)]
#[salsa::db(crate::Jar)]
pub(crate) struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {}

// aaa

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

fn aa() {}
";

    let db = Database::default();
    let source_program = SourceProgram::new(&db, src.to_string(), "uwu.ub".into());

    let file = parser::parse(&db, source_program);

    if let Some(file) = file {
        println!("{}", pretty::pretty_print_ast(&file));
    }

    let errs = parser::parse::accumulated::<Diagnostics>(&db, source_program);

    report_errors(src, errs);
}

fn report_errors(src: &str, errors: Vec<parser::Error>) {
    errors
        .into_iter()
        .map(|e| e.0.map(|c| c.to_string()))
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
