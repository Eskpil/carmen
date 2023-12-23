mod ast;
mod cil;
// mod codegen;
mod errors;
mod lexer;
mod parser;
mod unescape;

mod codegen;

use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use ast::statements::Statement;
use lexer::{Lexer, Span};
use parser::Parser;
use crate::cil::Pipeline;

struct Compiler {
    pub modules: Vec<ast::Module>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            modules: vec![],
        }
    }

    pub fn load_directory(&mut self, dir: String) {
        let paths = fs::read_dir(dir).unwrap();

        for path in paths {
            let path = path.expect("something wrong");
            if  path.path().extension().unwrap() != OsStr::new("cn") {
                continue
            }

            let filetype = path.file_type().expect("could not get filetype");
            // TODO: Allow directories somehow
            if !filetype.is_file() {
                continue
            }

            let filename_without_extension = path.path().file_stem().unwrap().to_str().unwrap().to_string();
            self.modules.push(self.load_file(filename_without_extension, path.path()));
        }
    }

    fn load_file(&self, module: String, path: PathBuf) -> ast::Module {
        let mut parser = Parser::new(
            fs::read_to_string(path.to_str().unwrap().to_string()).expect("could not read file"),
            path.to_str().unwrap().to_string(),
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

        ast::Module {
            name: module,
            statements: children,
        }
    }
}

fn main() {
    let mut compiler = Compiler::new();

    compiler.load_directory("./examples/hello_world".to_owned());

    let mut pipeline = Pipeline::new();
    pipeline.load(compiler.modules);

    let mut gen = codegen::Context::new();
    gen.generate(&pipeline.run());

    let mut file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .open("a.out")
        .expect("failed to open file");

    file.write_all(&*gen.build()).expect("could not write code to file")
}
