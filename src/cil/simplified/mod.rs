use crate::cil::typecheck;

pub struct Program {}

pub struct Simplifier {}

impl Simplifier {
    pub fn new() -> Simplifier {
        Simplifier {}
    }

    pub fn simplify(&mut self, typecheck_program: &typecheck::Program) -> Program {
        Program {}
    }
}