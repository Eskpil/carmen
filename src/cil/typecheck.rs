use std::ops::Index;
use crate::ast;
use crate::cil::{BinaryExpression, CallExpression, DataDeclaration, Declaration, Expression, LiteralExpression, TypeChecker, UseDataExpression, VariableLookupExpression};

impl TypeChecker {
    pub fn typecheck_expression(&mut self, expr: &ast::expressions::Expression) -> Expression {
        match expr {
            ast::expressions::Expression::Literal(_, val) => {
                Expression::Literal(LiteralExpression(*val))
            }
            ast::expressions::Expression::StringLiteral(_, string) => {
                let name = "123_123_data_weee";

                let data_decl = DataDeclaration {
                    name: name.to_owned(),
                    data: string.clone().into_bytes(),
                };
                self.program.declarations.push(Declaration::Data(data_decl));

                Expression::UseData(UseDataExpression { name: name.to_owned() } )
            }
            ast::expressions::Expression::Binary(_, op, lhs, rhs) => {
                let typechecked_lhs =  self.typecheck_expression(lhs);
                let typechecked_rhs = self.typecheck_expression(rhs);

                if self.expression_returns(&typechecked_lhs) != self.expression_returns(&typechecked_rhs) {
                    todo!("throw type error");
                }

                Expression::Binary(BinaryExpression{
                    op: op.clone(),
                    lhs: Box::new(typechecked_lhs),
                    rhs: Box::new(typechecked_rhs),
                })
            }
            ast::expressions::Expression::Identifier(_, name) => {
                if let Some(variable) = self.lookup_variable(name.clone()) {
                    Expression::VariableLookup(VariableLookupExpression{variable })
                } else {
                    todo!("throw variable not found error")
                }
            }
            ast::expressions::Expression::Call(_, name, ast_arguments) => {
                let declaration = self.find_function_declaration(name.clone());
                if declaration.is_none() {
                    // TODO: Check if it is a anonymous function declared in scope or throw error.
                    todo!("throw function not found error")
                }

                let declaration = declaration.unwrap();

                let mut arguments = Vec::<Box<Expression>>::new();

                for i in 0..ast_arguments.len() {
                    let argument = ast_arguments.index(i);
                    let signature_accept = &declaration.signature.accepts[i];
                    let typechecked_argument = self.typecheck_expression(&argument.value);
                    if *signature_accept != self.expression_returns(&typechecked_argument) {
                        println!("func: {name} sig: {:?} arg: {:?}", signature_accept, self.expression_returns(&typechecked_argument));
                        todo!("throw type error");
                    }
                    arguments.push(Box::new(typechecked_argument));
                }

                Expression::Call(CallExpression {
                    name: name.clone(),
                    arguments,
                })
            }
            expr => todo!("implement: {:?}", expr),
        }
    }
}