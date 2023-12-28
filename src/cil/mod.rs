use crate::ast;

pub mod common;
pub mod compressed;
pub mod typecheck;

pub struct Pipeline {
    modules: Vec<ast::Module>,
}

impl Pipeline {
    pub fn new() -> Self {
        Pipeline { modules: vec![] }
    }

    pub fn load(&mut self, modules: Vec<ast::Module>) {
        self.modules.extend(modules);
    }

    pub fn run(&mut self) -> compressed::compressed_ast::Program {
        let mut typechecker = typecheck::TypeChecker::new();
        let typechecked_program = typechecker.typecheck_modules(self.modules.clone());

        let mut compressor = compressed::Compressor::new();
        compressor.compress_program(&typechecked_program)
    }
}
