use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use crate::ast;
use crate::ast::BinaryOp;

pub mod typecheck;
pub mod compressed;
mod common;

pub struct Pipeline {
    modules: Vec<ast::Module>,
}

impl Pipeline {
    pub fn new() -> Self {
        Pipeline {
            modules: vec![],
        }
    }

    pub fn load(&mut self, modules: Vec<ast::Module>) {
        self.modules.extend(modules);
    }

    pub fn run(&mut self) -> compressed::compressed_ast::Program {
        let mut typechecker = typecheck::TypeChecker::new();
        let typechecked_program = typechecker.typecheck_modules(self.modules.clone());

        let mut compressor = compressed::Compressor::new();
        let compressed_program = compressor.compress_program(&typechecked_program);
        compressed_program
    }
}
