use cranelift_object::object::SectionKind::Debug;
use crate::cil::common::Tag;
use crate::cil::typecheck::type_id::{aliases, Primitive, TypePool};
use crate::cil::typecheck::typechecked_ast::{Block, CallExpression, Declaration, Expression, ExpressionStatement, FunctionDeclaration, FunctionDefinition, LiteralExpression, Module, ModuleName, ReturnStatement, Signature, Statement};
use crate::cil::typecheck::TypeChecker;

pub struct Runtime {
    module: Module,
    with_libc: bool,
}

impl Runtime {
    pub fn new(id: u32, with_libc: bool) -> Self {
        Self {
            module: Module::new(id, "__builtin_rt".to_owned()),
            with_libc,
        }
    }

    pub fn generate_main_declaration(&mut self, typechecker: &TypeChecker) {
        let signature = Signature {
            returns: typechecker.type_pool.find_primitive(&Primitive::U32).expect("could not find U32 primitive"),
            accepts: vec![],
        };

        let decl = FunctionDeclaration {
            name: ModuleName {
                id: self.module.id,
                name: "main".to_owned(),
            },
            tags: vec![Tag::NoMangle, Tag::External],
            signature,
        };

        self.module.declarations.push(Declaration::Function(decl));
    }

    pub fn generate_main_definition(&mut self, typechecker: &TypeChecker) {
        let mut block = Block {
            statements: vec![],
            parameters: vec![],
        };

        {
            let module = typechecker.module_by_name("main".to_owned());
            let decl = module.get_function_declaration("main".to_owned());
            if let None = decl {
                todo!("main module missing main function");
            }
            let decl = decl.unwrap();

            let call = Expression::Call(CallExpression {
                name: decl.name,
                arguments: vec![],
            });
            block.statements.push(Statement::Expression(ExpressionStatement(call)));
        }

        {
            let literal = Expression::Literal(LiteralExpression {
                typ: typechecker.type_pool.find_primitive(&Primitive::U32).expect("no u32?"),
                value: 0,
            });

            block.statements.push(Statement::Return(ReturnStatement {
                expr: literal,
            }))
        }

        let def = FunctionDefinition {
            name: ModuleName {
                id: self.module.id,
                name: "main".to_owned(),
            },
            block,
        };

        self.module.definitions.push(def);
    }

    pub fn generate_main(&mut self, typechecker: &TypeChecker) {
        self.generate_main_declaration(typechecker);
        self.generate_main_definition(typechecker);
    }

    pub fn finalize(mut self, typechecker: &TypeChecker) -> Module {
        self.generate_main(typechecker);

        self.module
    }
}