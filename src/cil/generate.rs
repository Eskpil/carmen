use crate::ast::{self, definitions::ExplicitType};

use super::{Block, Declaration, DeclareVariableStatement, ExpressionStatement, FunctionDeclaration, FunctionDefinition, Program, ReturnStatement, Statement, Type, TypeChecker, Variable};

/*

    three stages

    1. resolve imports
    2. typecheck type declarations (structs, enums, functions)
    3. typecheck function definitions.

    TODO: Name the stages.
 */

fn convert_type(explicit_type: &ExplicitType) -> Type {
    let typ =match explicit_type {
        ExplicitType::Name(name) => {
            match name.as_str() {
                "usize" => Type::Usize,
                "u8" => Type::U8,
                other => todo!("implement explicit type: {other}")
            }
        }
        ExplicitType::Pointer(to) => {
            Type::Pointer(Box::new(convert_type(&*to)))
        }
        other => todo!("implement type: {:?}", other)
    };

    typ
}

impl TypeChecker {
    pub fn generate_function_declaration(&self, stmt: &ast::statements::Statement) -> FunctionDeclaration {
        match stmt {
            ast::statements::Statement::Function(_, name, parameters, body, return_type, external) => {
                let mut accepts: Vec<Type> = vec![];
                let mut returns: Vec<Type> = vec![];

                for param in parameters.iter() {
                    accepts.push(convert_type(&param.defined_type));
                }

                if return_type.clone() != ExplicitType::Empty {
                    returns.push(Type::Usize);
                }

                return FunctionDeclaration {
                    name: name.clone(),
                    signature: super::Signature { accepts, returns },
                };
            }
            _ => todo!("do this better"),
        }
    }

    pub fn generate_block(&mut self, stmts: Vec<ast::statements::Statement>) -> Block {
        let mut block = Block {
            statements: vec![],
            parameters: vec![],
        };

        for stmt in stmts.iter() {
            let typechecked_stmt = match stmt {
                ast::statements::Statement::Return(_, expr) => {
                    let expr = self.typecheck_expression(expr);

                    if let Some(scope) = self.current_scope() {
                        if Some(self.expression_returns(&expr)) != scope.expected_return_type {
                            todo!("implement type errors")
                        }
                    } else {
                        unreachable!("no scope?!?")
                    }

                    Statement::Return(ReturnStatement{expr})
                }
                ast::statements::Statement::Declaration(_, name, expr) => {
                    let expr = self.typecheck_expression(expr);

                    let variable = Variable {
                        name: name.clone(),
                        typ:  Type::Usize,
                        id: self.get_variable_id(),
                    };

                    self.push_variable(variable.clone());
                    Statement::DeclareVariable(DeclareVariableStatement{ variable, expr })
                }
                ast::statements::Statement::Expression(_, expr) => {
                    let expr = self.typecheck_expression(expr);
                    Statement::Expression(ExpressionStatement(expr))
                }
                s => todo!("implement: {:?}", s)
            };

            block.statements.push(typechecked_stmt);
        }

        block
    }

    pub fn generate_function_definition(&mut self, stmt: &ast::statements::Statement) -> Option<FunctionDefinition> {
        match stmt {
            ast::statements::Statement::Function(_, name, parameters, body, return_type, _) => {
                if 0 >= body.len() {
                    return None;
                }

                let mut scope = self.new_scope();
                scope.expected_return_type = Some(convert_type(return_type));

                for param in parameters {
                    let id = self.get_variable_id();

                    scope.variables.push(Variable {
                        name: param.name.clone(),
                        typ: convert_type(&param.defined_type),
                        id,
                    })
                }
                let parameters = scope.variables.clone();

                self.insert_scope(scope);
                let mut block = self.generate_block(body.clone());
                block.parameters = parameters;

                return Some(FunctionDefinition {
                    name: name.clone(),
                    block,
                });
            }
            _ => todo!("do this better"),
        }
    }

    // TODO: Implement resolve_imports
    pub fn resolve_imports(&self) {}

    pub fn typecheck_declarations(&mut self, program: &ast::statements::Statement) {
        match program {
            ast::statements::Statement::Program(_, stmts) => {
                for function_stmt in stmts.iter() {
                    self.program.declarations.push(Declaration::Function(self.generate_function_declaration(function_stmt)));
                }
            }
            _ => todo!("implement"),
        }
    }

    pub fn typecheck_definitions(&mut self, program: &ast::statements::Statement) {
        match program {
            ast::statements::Statement::Program(_, stmts) => {
                for function_stmt in stmts.iter() {
                    let definition = self.generate_function_definition(function_stmt);
                    if let Some(definition) = definition {
                        self.program.definitions.push(definition);
                    }
                }
            }
            _ => todo!("implement"),
        }
    }

    pub fn generate_cil(mut self, stmt: &ast::statements::Statement) -> Program {
        self.resolve_imports();
        self.typecheck_declarations(stmt);
        self.typecheck_definitions(stmt);

        return self.program
    }
}
