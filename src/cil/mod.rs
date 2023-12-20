use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use crate::ast;
use crate::ast::BinaryOp;

pub mod typecheck;
pub mod simplified;
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

    pub fn run(&mut self) -> simplified::Program {
        let mut typechecker = typecheck::TypeChecker::new();
        let typechecked_program = typechecker.typecheck_modules(self.modules.clone());

        let mut simplifier = simplified::Simplifier::new();
        let simplified_program = simplifier.simplify(&typechecked_program);
        simplified_program
    }
}
