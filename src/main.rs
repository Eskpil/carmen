mod ast;
mod cil;
mod codegen;
mod errors;
mod lexer;
mod parser;
mod unescape;

use std::fs;
use std::io::Write;
use ast::statements::Statement;
use codegen::Context;
use lexer::{Lexer, Span};
use parser::Parser;
use crate::cil::TypeChecker;

fn main() {
    let mut parser = Parser::new(
        include_str!("../tests/print.cn").to_owned(),
        "tests/print.cn".to_owned(),
    );

    let mut children = Vec::<Statement>::new();

    loop {
        if parser.ended {
            break;
        }

        match parser.parse_statement() {
            Ok(statement) => children.push(statement),
            Err(err) => {
                if err.level == errors::Level::Ignore {
                    continue;
                } else {
                    err.report();
                }
            }
        }
    }

    let typechecker = TypeChecker::new();

    let program = Statement::Program(Span::default(), children);
    program.print(0);

    let typechecked_program = typechecker.generate_cil(&program);

    let mut context = Context::new();
    context.generate(&typechecked_program);

    let mut file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .open("a.out")
        .expect("failed to open file");

    file.write_all(&*context.build()).expect("could not write code to file")
}
