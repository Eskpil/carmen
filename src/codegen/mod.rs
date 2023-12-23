use cranelift_codegen::ir::{types::*, ExtFuncData, ExternalName, Function, UserExternalName, UserFuncName, Value};
use cranelift_codegen::ir::{AbiParam, InstBuilder, Signature};
use cranelift_codegen::isa::{lookup};
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataDescription, DataId, default_libcall_names, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::{Architecture, BinaryFormat, Environment, OperatingSystem, Triple, Vendor};

use std::collections::HashMap;

use crate::ast::BinaryOp;
use crate::cil::compressed::compressed_ast;

#[derive(Clone)]
struct FuncIdEntry {
    id: FuncId,
    sig: Signature,
}

pub struct Context {
    ctx: cranelift_codegen::Context,

    module: ObjectModule,

    func_id_cache: HashMap<String, FuncIdEntry>,
    data_id_cache: HashMap<String, DataId>,
    variables: HashMap<u32, Variable>,

    block_has_returned: bool,
}

fn convert_int(int: &compressed_ast::Integer) -> Type {
    // TODO: Doesn't matter if integer is signed or not???+!???
    match int.byte_size {
        1 => I8,
        2 => I16,
        4 => I32,
        8 => I64,

        _ => unreachable!(),
    }
}

fn convert_type(compressed_type: &compressed_ast::Type) -> Type {
    match compressed_type {
        compressed_ast::Type::Integer(int) => convert_int(int),
    }
}

// TODO: Do actual signature conversion.
fn convert_signature(sig: &mut Signature, cil_sig: &compressed_ast::Signature) {
    for param in cil_sig.accepts.iter() {
        sig.params.push(AbiParam::new(convert_type(param)));
    }

    sig.returns.push(AbiParam::new(convert_type(&cil_sig.returns)));
}

impl Context {
    pub fn new() -> Context {
        // Set up a Cranelift context, a compilation setting, and a module.
        let triple = Triple {
            architecture: Architecture::X86_64,
            vendor: Vendor::Unknown,
            operating_system: OperatingSystem::Linux,
            binary_format: BinaryFormat::Elf,
            environment: Environment::Gnu,
        };

        println!("using triple: {triple}");

        let isa_builder = lookup(triple).expect("failed to create isa builder");
        let b = settings::builder();
        let f = settings::Flags::new(b);
        let object_builder = ObjectBuilder::new(
            isa_builder
                .finish(f)
                .expect("could not finish building isa"),
            "example".to_owned(),
            default_libcall_names(),
        )
        .expect("failed to create object builder");

        let module = ObjectModule::new(object_builder);

        Context {
            ctx: module.make_context(),
            module,

            func_id_cache: HashMap::new(),
            data_id_cache: HashMap::new(),

            variables: HashMap::new(),

            block_has_returned: false,
        }
    }


    pub fn generate_function_declaration(&mut self, declaration: &compressed_ast::FunctionDeclaration) {
        let mut sig = self.module.make_signature();
        convert_signature(&mut sig, &declaration.signature);

        let func_id = self.module.declare_function(&declaration.name, Linkage::Export, &sig).expect("could not declare function");

        println!("function: {}@{:?}", declaration.name, sig);

        self.func_id_cache.insert(declaration.name.clone(), FuncIdEntry {
            id: func_id,
            sig,
        });
    }

    pub fn generate_data_declaration(&mut self, declaration: &compressed_ast::DataDeclaration) {
        let data_id = self.module.declare_data(&declaration.name, Linkage::Export, true, false).expect("could not declare data");

        let mut data_description = DataDescription::new();
        let slice = declaration.data.clone().into_boxed_slice();
        data_description.define(slice);

        self.module.define_data(data_id, &data_description).expect("could not define data");

        self.data_id_cache.insert(declaration.name.clone(), data_id);
    }

    pub fn generate_declarations(&mut self, declarations: &[compressed_ast::Declaration]) {
        for declaration in declarations.iter() {
            match declaration {
                compressed_ast::Declaration::Function(function) => {
                    self.generate_function_declaration(function);
                }
                compressed_ast::Declaration::Data(data) => {
                    self.generate_data_declaration(data);
                }
            }
        }
    }

    pub fn generate_definitions(&mut self, definitions: &[compressed_ast::FunctionDefinition]) {
        for definition in definitions.iter() {
            self.generate_function_definition(definition);
        }
    }

    pub fn generate_expression(&mut self, expr: &compressed_ast::Expression, builder: &mut FunctionBuilder) -> Vec<Value> {
        match expr {
            compressed_ast::Expression::Literal(lit) => {
                vec![builder.ins().iconst(convert_type(&lit.typ), lit.value as i64)]
            }
            compressed_ast::Expression::Binary(bin) => {
                let lhs = self.generate_expression(&bin.lhs, builder);
                let rhs = self.generate_expression(&bin.rhs, builder);

                assert!(!lhs.is_empty());
                assert!(!rhs.is_empty());

                match bin.op {
                    BinaryOp::Add => {
                        vec![builder.ins().iadd(lhs[0], rhs[0])]
                    }
                    BinaryOp::Sub => {
                        vec![builder.ins().isub(lhs[0], rhs[0])]
                    }
                    BinaryOp::Mul => {
                        vec![builder.ins().imul(lhs[0], rhs[0])]
                    }
                    b => todo!("implement: {}", b.to_string())
                }
            }
            compressed_ast::Expression::VariableLookup(vl) => {
                vec![builder.use_var(Variable::from_u32(vl.id))]
            }
            compressed_ast::Expression::Call(call) => {
                let cache = self.func_id_cache.clone();
                let func_entry = cache.get(&*call.name).expect("could not find func");

                let func_external_name = builder
                    .func
                    .declare_imported_user_function(UserExternalName::new(0, func_entry.id.as_u32()));

                let func_sig_ref = builder.import_signature(func_entry.sig.clone());
                let func_ref = builder.import_function(ExtFuncData {
                    name: ExternalName::User(func_external_name),
                    signature: func_sig_ref,
                    colocated: true,
                });

                let mut arguments = Vec::<Value>::new();

                for arg in &call.arguments {
                    let expr = self.generate_expression(arg, builder);
                    assert!(!expr.is_empty());
                    arguments.push(expr[0]);
                }

                let result = builder.ins().call(func_ref, &arguments);
                builder.inst_results(result).to_vec()
            }
            compressed_ast::Expression::UseData(data) => {
                let cache = self.data_id_cache.clone();
                let data_id = cache.get(&*data.name).expect("could not find data id");

                let global_value = self.module.declare_data_in_func(*data_id, builder.func);
                let val = builder.ins().global_value(I64, global_value);
                vec![val]
            }
        }
    }

    pub fn generate_statement(&mut self, stmt: &compressed_ast::Statement, builder: &mut FunctionBuilder) {
        match stmt {
            compressed_ast::Statement::Return(ret) => {
                let val = self.generate_expression(&ret.expr, builder);
                builder.ins().return_(&val);
                self.block_has_returned = true;
            }
            compressed_ast::Statement::DeclareVariable(decl) => {
                let var  =Variable::from_u32(decl.id);
                builder.declare_var(var, convert_type(&decl.typ));
                self.variables.insert(decl.id, var);
            }
            compressed_ast::Statement::DefineVariable(def) => {
                let cache = self.variables.clone();
                let var = cache.get(&def.id).expect("no variable?!?");

                let val = self.generate_expression(&def.expr, builder);
                assert_eq!(val.len(), 1);
                builder.def_var(*var, val[0]);
            }
            compressed_ast::Statement::Expression(expr) => {
                self.generate_expression(&expr.expr, builder);
            }
        }
    }

    pub fn generate_block(&mut self, block: &compressed_ast::Block, builder: &mut FunctionBuilder) {
        let ir_block = builder.create_block();
        let mut has_switched_block = false;

        if !block.parameters.is_empty() {
            for (id, typ) in block.parameters.clone() {
                let block_param = builder.append_block_param(ir_block, convert_type(&typ));

                if !has_switched_block {
                    builder.switch_to_block(ir_block);
                    has_switched_block = true
                }

                let var = Variable::from_u32(id);
                builder.declare_var(var, convert_type(&typ));
                builder.def_var(var, block_param);

                self.variables.insert(id, var);
            }
        }

        if !has_switched_block {
            builder.switch_to_block(ir_block);
        }

        for stmt in block.body.clone() {
            self.generate_statement(&stmt, builder);
        }

        if !self.block_has_returned {
            builder.ins().return_(&[]);
        }

        self.block_has_returned = false;
    }

    pub fn generate_function_definition(&mut self, definition: &compressed_ast::FunctionDefinition) {
        // inefficient, i do not care. Borrow checker got me tired.
        let cache = self.func_id_cache.clone();
        let entry = cache.get(&*definition.for_declaration).expect("could not find entry");

        println!("generating definition for {}", definition.for_declaration);

        let mut func = Function::with_name_signature(UserFuncName::user(0, entry.id.as_u32()), entry.sig.clone());

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_context);

        self.generate_block(&definition.block, &mut builder);

        builder.seal_all_blocks();
        builder.finalize();
//
        self.ctx.func = func;
        println!("{}", self.ctx.func.display());

        self.module
            .define_function(entry.id, &mut self.ctx)
            .expect("failed to define function");

        // Clear the context for the next function.
        self.ctx.clear();
    }

    pub fn generate(&mut self, program: &compressed_ast::Program) {
        self.generate_declarations(&program.declarations);
        self.generate_definitions(&program.definitions);
    }

    pub fn build(self) -> Vec<u8> {
        let product = self.module.finish();
        product.emit().expect("could not emit data")
    }
}
