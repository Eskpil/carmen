use crate::ast;
use crate::ast::expressions::LookupExpression;
use crate::ast::BinaryOp;
use crate::cil::common::Endianness;
use crate::cil::typecheck::type_id::{aliases, TypeError, TypeId, TypePool};
use crate::lexer::Span;

#[derive(Debug, Clone)]
pub enum ComputeError {
    ConstantVariableNotFound(Span, String),
    ExpressionNotConstant(Span, String),
    Type(Span, TypeError),
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct ResolvedModuleName {
    pub module: String,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Value {
    pub inner: u64,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: ResolvedModuleName,
    pub typ: TypeId,
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: Vec<Variable>,
}

pub type ComputeResult<T> = Result<T, ComputeError>;

#[derive(Debug, Clone)]
pub struct Computer<'a> {
    scope: &'a Scope,
    current_module_name: String,
}

impl Value {
    pub fn get_type_id(&self) -> &TypeId {
        &self.type_id
    }

    // TODO: Use endianness, doesn't really matter right now since we are x86-64 only.
    pub fn encode(&self, _: Endianness, size: usize) -> Vec<u8> {
        assert_eq!(self.type_id.size(), size);

        let mut bytes = Vec::from(self.inner.to_le_bytes());
        bytes.shrink_to(size);
        bytes
    }

    pub fn mul(&self, rhs: &Value) -> Value {
        Value {
            type_id: self.get_type_id().clone(),
            inner: self.inner * rhs.inner,
        }
    }

    pub fn div(&self, rhs: &Value) -> Value {
        Value {
            type_id: self.get_type_id().clone(),
            inner: self.inner / rhs.inner,
        }
    }

    pub fn add(&self, rhs: &Value) -> Value {
        Value {
            type_id: self.get_type_id().clone(),
            inner: self.inner + rhs.inner,
        }
    }

    pub fn sub(&self, rhs: &Value) -> Value {
        Value {
            type_id: self.get_type_id().clone(),
            inner: self.inner - rhs.inner,
        }
    }

    pub fn mod_(&self, rhs: &Value) -> Value {
        Value {
            type_id: self.get_type_id().clone(),
            inner: self.inner % rhs.inner,
        }
    }

    // These are tricky since they return boolean, we probably need to bring in the type pool.
    pub fn less(&self, _: &Value) -> Value {
        todo!("less");
    }

    pub fn greater(&self, _: &Value) -> Value {
        todo!("greater");
    }

    pub fn less_eq(&self, _: &Value) -> Value {
        todo!("less_eq");
    }

    pub fn greater_eq(&self, _: &Value) -> Value {
        todo!("greater_eq");
    }

    pub fn eq(&self, _: &Value) -> Value {
        todo!("eq");
    }

    pub fn ne(&self, _: &Value) -> Value {
        todo!("ne");
    }
}

impl Variable {
    pub fn set_module(&self, module: String) -> Self {
        let name = ResolvedModuleName {
            module,
            name: self.name.name.clone(),
        };

        Self {
            name,
            value: self.value.clone(),
            typ: self.typ.clone(),
        }
    }
}

impl Scope {
    pub fn merge(&mut self, other: &Scope) {
        self.variables.extend(other.variables.clone());
    }

    pub fn has(&self, name: &ResolvedModuleName) -> bool {
        let mut found = false;
        for var in &self.variables {
            if &var.name == name {
                found = true;
                break;
            }
        }

        found
    }

    pub fn get(&self, name: &ResolvedModuleName) -> Option<&Variable> {
        self.variables.iter().find(|v| v.name == *name)
    }

    pub fn set_module(&mut self, module: String) {
        self.variables = self
            .variables
            .iter()
            .map(|var| var.set_module(module.clone()))
            .collect();
    }
}

fn lookup_to_resolved_module_name(
    lookup: &LookupExpression,
    current_module_name: String,
) -> ResolvedModuleName {
    let module ;
    let name ;
    if lookup.child.is_some() {
        module = lookup.name.clone();
        name = lookup.child.clone().unwrap().name;
    } else {
        module = current_module_name;
        name = lookup.name.clone();
    }

    ResolvedModuleName { module, name }
}

impl<'a> Computer<'a> {
    pub fn new(scope: &'a Scope, current_module_name: String) -> Self {
        Self {
            scope,
            current_module_name,
        }
    }

    pub fn compute_expression(
        &mut self,
        expr: &ast::expressions::Expression,
        explicit_type: Option<TypeId>,
        type_pool: &TypePool,
    ) -> ComputeResult<Value> {
        match expr {
            ast::expressions::Expression::Lookup(lookup) => {
                let name = lookup_to_resolved_module_name(lookup, self.current_module_name.clone());
                if !self.scope.has(&name) {
                    Err(ComputeError::ConstantVariableNotFound(
                        expr.span(),
                        "Constant variable not found".to_owned(),
                    ))
                } else {
                    Ok(self.scope.get(&name).unwrap().value.clone())
                }
            }
            ast::expressions::Expression::Literal(lit) => {
                let mut type_id = type_pool.find_alias(aliases::USIZE).expect("no usize?");

                if explicit_type.is_some() {
                    type_id = explicit_type.unwrap().clone();
                }
                // TODO: Bounds check.

                Ok(Value {
                    type_id,
                    inner: lit.val,
                })
            }
            ast::expressions::Expression::Binary(bin) => {
                let lhs = self.compute_expression(&bin.lhs, None, type_pool)?;
                let rhs = self.compute_expression(&bin.rhs, None, type_pool)?;

                // TODO: Bounds check;
                // TODO: Type check;

                let res = match bin.op {
                    BinaryOp::Mul => lhs.mul(&rhs),
                    BinaryOp::Div => lhs.div(&rhs),

                    BinaryOp::Add => lhs.add(&rhs),
                    BinaryOp::Sub => lhs.sub(&rhs),

                    BinaryOp::Mod => lhs.mod_(&rhs),
                    BinaryOp::Less => lhs.less(&rhs),
                    BinaryOp::Greater => lhs.greater(&rhs),
                    BinaryOp::LessEquals => lhs.less_eq(&rhs),
                    BinaryOp::GreaterEquals => lhs.greater_eq(&rhs),
                    BinaryOp::Equals => lhs.eq(&rhs),
                    BinaryOp::NotEquals => lhs.ne(&rhs),
                };

                Ok(res)
            }
            _ => Err(ComputeError::ExpressionNotConstant(
                expr.span(),
                "expression is not constant".to_owned(),
            )),
        }
    }
}
