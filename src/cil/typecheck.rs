use crate::ast;
use crate::cil::{BinaryExpression, CallExpression, Expression, LiteralExpression, TypeChecker, VariableLookupExpression};

impl TypeChecker {
    pub fn typecheck_expression(&mut self, expr: &ast::expressions::Expression) -> Expression {
        match expr {
            ast::expressions::Expression::Literal(_, val) => {
                Expression::Literal(LiteralExpression(*val))
            }
            ast::expressions::Expression::Binary(_, op, lhs, rhs) => {
                let typechecked_lhs =  self.typecheck_expression(lhs);
                let typechecked_rhs = self.typecheck_expression(rhs);

                if typechecked_lhs.returns() != typechecked_rhs.returns() {
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
                    Expression::VariableLookup(VariableLookupExpression{id: variable.id})
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

                let mut i = 0;
                for argument in ast_arguments.iter() {
                    let signature_accept = declaration.signature.accepts[i];
                    i += 1;
                    let typechecked_argument = self.typecheck_expression(&argument.value);

                    if signature_accept != typechecked_argument.returns() {
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