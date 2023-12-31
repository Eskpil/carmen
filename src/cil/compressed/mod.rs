pub mod compressed_ast;

use crate::ast::BinaryOp;
use crate::cil::common::Tag;
use crate::cil::compressed::compressed_ast::{Block, DeclareVariableStatement, DefineVariableStatement, ExpressionStatement, IfStatement, Integer, LoopStatement, MemoryReadExpression, MemoryWriteExpression};
use crate::cil::typecheck::type_id::Type;
use crate::cil::typecheck::type_id::{Alias, Primitive};
use crate::cil::typecheck::{
    self,
    type_id::{Tag as TypeTag, TypeId},
    typechecked_ast,
    typechecked_ast::ModuleName,
    ModuleId,
};
use rand::distributions::{Alphanumeric, DistString};
use std::collections::HashMap;

pub struct Compressor {
    program: compressed_ast::Program,
}

impl Compressor {
    pub fn new() -> Compressor {
        Compressor {
            program: compressed_ast::Program {
                declarations: vec![],
                definitions: vec![],
            },
        }
    }

    pub fn mangle_name(&mut self, name: &ModuleName) -> String {
        format!(
            "{}_{}_{}",
            Alphanumeric.sample_string(&mut rand::thread_rng(), 12),
            name.id,
            name.name
        )
    }

    pub fn find_function_decl_by_module_name(
        &mut self,
        name: &ModuleName,
    ) -> compressed_ast::FunctionDeclaration {
        let mut res = None;
        for decl in &self.program.declarations {
            if let compressed_ast::Declaration::Function(func) = decl {
                if func.module_name != *name {
                    continue;
                }

                res = Some(func.clone());
            }
        }

        res.unwrap()
    }

    pub fn find_data_decl_by_module_name(
        &mut self,
        name: &ModuleName,
    ) -> compressed_ast::DataDeclaration {
        let mut res = None;
        for decl in &self.program.declarations {
            if let compressed_ast::Declaration::Data(data) = decl {
                if data.module_name != *name {
                    continue;
                }

                res = Some(data.clone());
            }
        }

        res.unwrap()
    }
    pub fn compress_data_declaration(
        &mut self,
        typechecked_data: &typechecked_ast::DataDeclaration,
    ) -> compressed_ast::DataDeclaration {
        let mut mangled_name = self.mangle_name(&typechecked_data.name);
        if typechecked_data.tags.contains(&Tag::NoMangle) {
            mangled_name = typechecked_data.name.name.clone();
        }

        compressed_ast::DataDeclaration {
            name: mangled_name,
            data: typechecked_data.data.clone(),
            size: typechecked_data.size,
            module_name: typechecked_data.name.clone(),
        }
    }

    pub fn transform_primitive(&mut self, primitive: &Primitive) -> compressed_ast::Type {
        let (signed, byte_size) = match primitive {
            Primitive::U8 => (false, primitive.size()),
            Primitive::U16 => (false, primitive.size()),
            Primitive::U32 => (false, primitive.size()),
            Primitive::U64 => (false, primitive.size()),
            Primitive::I8 => (true, primitive.size()),
            Primitive::I16 => (true, primitive.size()),
            Primitive::I32 => (true, primitive.size()),
            Primitive::I64 => (true, primitive.size()),
            Primitive::Bool => (false, primitive.size()),
            Primitive::Void => todo!("handle void"),
            Primitive::Unknown => unreachable!("wtf?"),
        };

        compressed_ast::Type::Integer(Integer { signed, byte_size })
    }

    pub fn transform_alias(&mut self, alias: &Alias) -> compressed_ast::Type {
        self.transform_type_id(&alias.to)
    }

    pub fn transform_type_id(&mut self, type_id: &TypeId) -> compressed_ast::Type {
        match type_id.tag {
            TypeTag::Primitive => self.transform_primitive(&type_id.to_primitive()),
            TypeTag::Alias => self.transform_alias(&type_id.to_alias()),
            TypeTag::Pointer => self.transform_primitive(&Primitive::U64),
            TypeTag::Slice => {
                // it is actually just a pointer.
                self.transform_primitive(&Primitive::U64)
            }
        }
    }

    pub fn transform_signature(
        &mut self,
        signature: &typechecked_ast::Signature,
    ) -> compressed_ast::Signature {
        let mut returns = None;
        if !signature.returns.is_void() {
            returns = Some(self.transform_type_id(&signature.returns));
        }
        let mut accepts = vec![];
        for accept in &signature.accepts {
            accepts.push(self.transform_type_id(accept));
        }

        compressed_ast::Signature { accepts, returns }
    }

    pub fn compress_function_declaration(
        &mut self,
        typechecked_function: &typechecked_ast::FunctionDeclaration,
    ) -> compressed_ast::FunctionDeclaration {
        let mut mangled_name = self.mangle_name(&typechecked_function.name);
        if typechecked_function.tags.contains(&Tag::NoMangle) {
            mangled_name = typechecked_function.name.name.clone();
        }

        let signature = self.transform_signature(&typechecked_function.signature);

        compressed_ast::FunctionDeclaration {
            name: mangled_name,
            module_name: typechecked_function.name.clone(),
            signature,
            tags: typechecked_function.tags.clone(),
        }
    }

    pub fn compress_global_variable_declaration(
        &mut self,
        typechecked_global_variable: &typechecked_ast::GlobalVariableDeclaration,
    ) -> compressed_ast::GlobalVariableDeclaration {
        compressed_ast::GlobalVariableDeclaration {
            variable_id: typechecked_global_variable.variable.id,
            typ: self.transform_type_id(&typechecked_global_variable.variable.typ),
            expr: self.compress_expression(&typechecked_global_variable.expr),
        }
    }

    pub fn compress_declaration(&mut self, typechecked_declaration: &typechecked_ast::Declaration) {
        let decl = match typechecked_declaration {
            typechecked_ast::Declaration::Function(function) => {
                compressed_ast::Declaration::Function(self.compress_function_declaration(function))
            }
            typechecked_ast::Declaration::Data(data) => {
                compressed_ast::Declaration::Data(self.compress_data_declaration(data))
            }
            typechecked_ast::Declaration::GlobalVariable(global) => {
                compressed_ast::Declaration::GlobalVariable(
                    self.compress_global_variable_declaration(global),
                )
            }
        };

        self.program.declarations.push(decl);
    }

    pub fn compress_literal_expression(
        &mut self,
        typechecked_literal: &typechecked_ast::LiteralExpression,
    ) -> compressed_ast::Expression {
        compressed_ast::Expression::Literal(compressed_ast::LiteralExpression {
            typ: self.transform_type_id(&typechecked_literal.typ),
            value: typechecked_literal.value,
        })
    }

    pub fn compress_binary_expression(
        &mut self,
        typechecked_binary: &typechecked_ast::BinaryExpression,
    ) -> compressed_ast::Expression {
        compressed_ast::Expression::Binary(compressed_ast::BinaryExpression {
            op: typechecked_binary.op,
            rhs: Box::new(self.compress_expression(&typechecked_binary.rhs)),
            lhs: Box::new(self.compress_expression(&typechecked_binary.lhs)),
        })
    }

    pub fn compress_call_expression(
        &mut self,
        typechecked_call: &typechecked_ast::CallExpression,
    ) -> compressed_ast::Expression {
        let decl = self.find_function_decl_by_module_name(&typechecked_call.name);
        let mut arguments = vec![];
        for arg in &typechecked_call.arguments {
            arguments.push(self.compress_expression(arg));
        }

        compressed_ast::Expression::Call(compressed_ast::CallExpression {
            name: decl.name,
            arguments,
        })
    }

    pub fn compress_variable_lookup_expression(
        &mut self,
        typechecked_lookup: &typechecked_ast::VariableLookupExpression,
    ) -> compressed_ast::Expression {
        compressed_ast::Expression::VariableLookup(compressed_ast::VariableLookupExpression {
            id: typechecked_lookup.variable.id,
        })
    }

    pub fn compress_use_data_expression(
        &mut self,
        typechecked_use: &typechecked_ast::UseDataExpression,
    ) -> compressed_ast::Expression {
        let decl = self.find_data_decl_by_module_name(&typechecked_use.name);

        compressed_ast::Expression::UseData(compressed_ast::UseDataExpression { name: decl.name })
    }

    pub fn compress_bool_expression(
        &mut self,
        typechecked_bool: &typechecked_ast::BooleanExpression,
    ) -> compressed_ast::Expression {
        let mut value = 1;
        if !typechecked_bool.value {
            value = 0;
        }

        compressed_ast::Expression::Literal(compressed_ast::LiteralExpression {
            typ: compressed_ast::Type::Integer(Integer {
                signed: false,
                byte_size: 1,
            }),
            value,
        })
    }

    pub fn compress_substrate_expression(
        &mut self,
        typechecked_substrate: &typechecked_ast::SubstrateExpression,
    ) -> compressed_ast::Expression {
        compressed_ast::Expression::Binary(compressed_ast::BinaryExpression {
            op: BinaryOp::Add,
            lhs: Box::new(self.compress_expression(&typechecked_substrate.on)),
            rhs: Box::new(self.compress_expression(&typechecked_substrate.offset)),
        })
    }

    pub fn compress_read_expression(
        &mut self,
        typechecked_read: &typechecked_ast::ReadExpression,
    ) -> compressed_ast::Expression {
        compressed_ast::Expression::MemoryRead(MemoryReadExpression {
            from: Box::new(self.compress_expression(&typechecked_read.from)),
        })
    }

    pub fn compress_write_expression(
        &mut self,
        typechecked_write: &typechecked_ast::WriteExpression,
    ) -> compressed_ast::Expression {
        compressed_ast::Expression::MemoryWrite(MemoryWriteExpression {
            to: Box::new(self.compress_expression(&typechecked_write.to)),
            value: Box::new(self.compress_expression(&typechecked_write.value)),
        })
    }

    pub fn compress_expression(
        &mut self,
        typechecked_expression: &typechecked_ast::Expression,
    ) -> compressed_ast::Expression {
        match typechecked_expression {
            typechecked_ast::Expression::Literal(lit) => self.compress_literal_expression(lit),
            typechecked_ast::Expression::Binary(bin) => self.compress_binary_expression(bin),
            typechecked_ast::Expression::Call(call) => self.compress_call_expression(call),
            typechecked_ast::Expression::VariableLookup(var) => {
                self.compress_variable_lookup_expression(var)
            }
            typechecked_ast::Expression::UseData(data) => self.compress_use_data_expression(data),
            typechecked_ast::Expression::Bool(bo) => self.compress_bool_expression(bo),
            typechecked_ast::Expression::Substrate(sub) => self.compress_substrate_expression(sub),

            typechecked_ast::Expression::Read(read) => self.compress_read_expression(read),
            typechecked_ast::Expression::Write(write) => self.compress_write_expression(write),
        }
    }

    pub fn compress_return_statement(
        &mut self,
        typechecked_return: &typechecked_ast::ReturnStatement,
    ) -> compressed_ast::Statement {
        let expr = self.compress_expression(&typechecked_return.expr);
        compressed_ast::Statement::Return(compressed_ast::ReturnStatement { expr })
    }

    pub fn compress_declare_variable_statement(
        &mut self,
        typechecked_decl: &typechecked_ast::DeclareVariableStatement,
    ) -> compressed_ast::Statement {
        compressed_ast::Statement::DeclareVariable(DeclareVariableStatement {
            id: typechecked_decl.variable.id,
            typ: self.transform_type_id(&typechecked_decl.variable.typ),
        })
    }

    pub fn compress_define_variable_statement(
        &mut self,
        typechecked_def: &typechecked_ast::DefineVariableStatement,
    ) -> compressed_ast::Statement {
        let expr = self.compress_expression(&typechecked_def.expr);
        compressed_ast::Statement::DefineVariable(DefineVariableStatement {
            id: typechecked_def.variable.id,
            expr,
        })
    }

    pub fn compress_expression_statement(
        &mut self,
        typechecked_expr: &typechecked_ast::ExpressionStatement,
    ) -> compressed_ast::Statement {
        let expr = self.compress_expression(&typechecked_expr.0);
        compressed_ast::Statement::Expression(ExpressionStatement { expr })
    }

    pub fn compress_while_statement(
        &mut self,
        typechecked_while: &typechecked_ast::WhileStatement,
    ) -> compressed_ast::Statement {
        let cond = self.compress_expression(&typechecked_while.cond);
        let block = self.compress_block(&typechecked_while.block);

        compressed_ast::Statement::Loop(LoopStatement { cond, block })
    }

    pub fn compress_if_statement(
        &mut self,
        typechecked_if: &typechecked_ast::IfStatement,
    ) -> compressed_ast::Statement {
        let cond = self.compress_expression(&typechecked_if.condition);
        let if_block = self.compress_block(&typechecked_if.if_block);
        let mut else_block = None;
        if let Some(block) = &typechecked_if.else_block {
            else_block = Some(self.compress_block(block));
        }

        compressed_ast::Statement::If(IfStatement {
            cond,
            if_block,
            else_block,
        })
    }

    pub fn compress_statement(
        &mut self,
        typechecked_statement: &typechecked_ast::Statement,
    ) -> compressed_ast::Statement {
        match typechecked_statement {
            typechecked_ast::Statement::Return(ret) => self.compress_return_statement(ret),
            typechecked_ast::Statement::DeclareVariable(dec) => {
                self.compress_declare_variable_statement(dec)
            }
            typechecked_ast::Statement::DefineVariable(def) => {
                self.compress_define_variable_statement(def)
            }
            typechecked_ast::Statement::Expression(expr) => {
                self.compress_expression_statement(expr)
            }
            typechecked_ast::Statement::While(w) => self.compress_while_statement(w),
            typechecked_ast::Statement::If(i) => self.compress_if_statement(i),
        }
    }

    pub fn compress_block(&mut self, typechecked_block: &typechecked_ast::Block) -> Block {
        let mut block = Block {
            parameters: vec![],
            body: vec![],
        };

        for param in &typechecked_block.parameters {
            block
                .parameters
                .push((param.id, self.transform_type_id(&param.typ)));
        }

        for stmt in &typechecked_block.statements {
            block.body.push(self.compress_statement(stmt));
        }

        block
    }

    pub fn compress_function_definition(
        &mut self,
        typechecked_function: &typechecked_ast::FunctionDefinition,
    ) -> compressed_ast::FunctionDefinition {
        let decl = self.find_function_decl_by_module_name(&typechecked_function.name);

        compressed_ast::FunctionDefinition {
            for_declaration: decl.name,
            block: self.compress_block(&typechecked_function.block),
        }
    }

    pub fn compress_definition(
        &mut self,
        typechecked_definition: &typechecked_ast::FunctionDefinition,
    ) {
        let definition = self.compress_function_definition(typechecked_definition);
        self.program.definitions.push(definition);
    }

    pub fn compress_module_declarations(&mut self, typechecked_module: &typechecked_ast::Module) {
        for typechecked_declaration in typechecked_module.declarations.iter() {
            self.compress_declaration(typechecked_declaration);
        }
    }
    pub fn compress_module_definitions(&mut self, typechecked_module: &typechecked_ast::Module) {
        for typechecked_definition in typechecked_module.definitions.iter() {
            self.compress_definition(typechecked_definition)
        }
    }

    pub fn compress_modules(
        &mut self,
        typechecked_modules: &HashMap<ModuleId, typechecked_ast::Module>,
    ) {
        for (_, typechecked_module) in typechecked_modules.iter() {
            self.compress_module_declarations(typechecked_module);
        }

        for (_, typechecked_module) in typechecked_modules.iter() {
            self.compress_module_definitions(typechecked_module);
        }
    }

    pub fn compress_program(
        &mut self,
        typechecked_program: &typecheck::Program,
    ) -> compressed_ast::Program {
        self.compress_modules(&typechecked_program.modules);
        self.program.clone()
    }
}
