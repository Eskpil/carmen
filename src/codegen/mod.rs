use cranelift_codegen::ir::{types::*, ExtFuncData, ExternalName, Function, UserExternalName, UserFuncName, Value, Block, InstBuilderBase};
use cranelift_codegen::ir::{AbiParam, InstBuilder, Signature};
use cranelift_codegen::isa::{lookup};
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataDescription, DataId, default_libcall_names, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::{Architecture, BinaryFormat, Environment, OperatingSystem, Triple, Vendor};

use std::collections::HashMap;
use cranelift_codegen::ir::condcodes::IntCC;

use crate::ast::BinaryOp;
use crate::cil::common::{Tag, Tags};
use crate::cil::compressed::compressed_ast;

#[derive(Clone)]
struct FuncIdEntry {
    id: FuncId,
    sig: Signature,
    colocated: bool,
}

pub struct Context {
    ctx: cranelift_codegen::Context,

    module: ObjectModule,

    func_id_cache: HashMap<String, FuncIdEntry>,
    data_id_cache: HashMap<String, DataId>,
    variables: HashMap<u32, Variable>,

    function_has_returned: bool,
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

    if let Some(returns) = &cil_sig.returns {
        sig.returns.push(AbiParam::new(convert_type(returns)));
    }
}

impl Context {
    pub fn new() -> Context {
        // Set up a Cranelift context, a compilation setting, and a module.
        let triple = Triple {
            architecture: Architecture::X86_64,
            vendor: Vendor::Amd,
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

            function_has_returned: false,
        }
    }

    pub fn determine_function_linkage(tags: &Tags) -> Linkage {
        if tags.contains(&Tag::Imported) {
            return Linkage::Import;
        }

        if tags.contains(&Tag::External) {
            return Linkage::Export;
        }

        Linkage::Local
    }

    pub fn generate_function_declaration(&mut self, declaration: &compressed_ast::FunctionDeclaration) {
        let mut sig = self.module.make_signature();
        convert_signature(&mut sig, &declaration.signature);

        let linkage = Context::determine_function_linkage(&declaration.tags);
        let func_id = self.module.declare_function(&declaration.name, linkage, &sig).expect("could not declare function");

        println!("function: {}@{:?}", declaration.name, linkage);

        let mut colocated = true;
        if linkage == Linkage::Import {
            colocated = false;
        }

        self.func_id_cache.insert(declaration.name.clone(), FuncIdEntry {
            id: func_id,
            sig,
            colocated,
        });
    }

    pub fn generate_data_declaration(&mut self, declaration: &compressed_ast::DataDeclaration) {
        let data_id = self.module.declare_data(&declaration.name, Linkage::Local, true, false).expect("could not declare data");

        let mut data_description = DataDescription::new();
        let slice = declaration.data.clone().into_boxed_slice();
        data_description.define(slice);

        self.module.define_data(data_id, &data_description).expect("could not define data");

        println!("declaring data: {}@{data_id}", declaration.name);

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

    pub fn generate_binary_expression(&mut self, expr: &compressed_ast::BinaryExpression, builder: &mut FunctionBuilder) -> Vec<Value> {
        let lhs = self.generate_expression(&expr.lhs, builder);
        let rhs = self.generate_expression(&expr.rhs, builder);

        assert!(!lhs.is_empty());
        assert!(!rhs.is_empty());

        match expr.op {
            BinaryOp::Add => {
                vec![builder.ins().iadd(lhs[0], rhs[0])]
            }
            BinaryOp::Sub => {
                vec![builder.ins().isub(lhs[0], rhs[0])]
            }
            BinaryOp::Mul => {
                vec![builder.ins().imul(lhs[0], rhs[0])]
            }
            b => unreachable!("op: {} is not suitable for expression", b)
        }
    }

    pub fn generate_binary_condition(&mut self, expr: &compressed_ast::BinaryExpression, builder: &mut FunctionBuilder) -> Vec<Value> {
        let lhs = self.generate_expression(&expr.lhs, builder);
        let rhs = self.generate_expression(&expr.rhs, builder);

        assert!(!lhs.is_empty());
        assert!(!rhs.is_empty());

        // TODO: Differentiate signed and unsigned additions.
        let op = match expr.op  {
            BinaryOp::Greater => {
                IntCC::UnsignedGreaterThan
            }
            BinaryOp::GreaterEquals => {
                IntCC::UnsignedGreaterThanOrEqual
            }

            BinaryOp::Less => {
                IntCC::UnsignedLessThan
            }
            BinaryOp::LessEquals => {
                IntCC::UnsignedLessThanOrEqual
            }

            BinaryOp::Equals => {
                IntCC::Equal
            }
            BinaryOp::NotEquals => {
                IntCC::NotEqual
            }
            b => unreachable!("op: {} is not suitable for condition", b)
        };

        vec![builder.ins().icmp(op, lhs[0], rhs[0])]
    }

    pub fn generate_expression(&mut self, expr: &compressed_ast::Expression, builder: &mut FunctionBuilder) -> Vec<Value> {
        match expr {
            compressed_ast::Expression::Literal(lit) => {
                vec![builder.ins().iconst(convert_type(&lit.typ), lit.value as i64)]
            }
            compressed_ast::Expression::Binary(bin) => {
                if bin.op.returns_bool() {
                    self.generate_binary_condition(bin, builder)
                } else {
                    self.generate_binary_expression(bin, builder)
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

    pub fn generate_statement(&mut self, stmt: &compressed_ast::Statement, parent_block: Block, builder: &mut FunctionBuilder) {
        match stmt {
            compressed_ast::Statement::Return(ret) => {
                let val = self.generate_expression(&ret.expr, builder);
                builder.ins().return_(&val);
                self.function_has_returned = true;
            }
            compressed_ast::Statement::DeclareVariable(decl) => {
                let var = Variable::from_u32(decl.id);
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
            compressed_ast::Statement::Loop(lo) => {
                let cond_block = builder.create_block();
                let body_block = builder.create_block();
                let end_block = builder.create_block();

                // If the while statement is the first statement in a block cranelift will complain that we can not
                // switch to another block before filling the previous. Cranelift did not tolerate a nop so we introduce
                // a redundant jump to the condition block. This should be solved some other way. This could be solved
                // by determining if the parent_block has been filled. If not, just use the parent block for the
                // condition.
                builder.ins().jump(cond_block, &[]);

                builder.switch_to_block(cond_block);
                {
                    let res = self.generate_expression(&lo.cond, builder);
                    assert_eq!(1, res.len());
                    let res = res[0];

                    builder.ins().brif(res, body_block, &[], end_block, &[]);
                }

                builder.switch_to_block(body_block);
                {
                    self.fill_block_without_parameters(&lo.block, body_block, builder);
                    builder.ins().jump(cond_block, &[]);
                }


                builder.switch_to_block(end_block);
                builder.seal_block(end_block);

                builder.seal_block(cond_block);
                builder.seal_block(body_block);
            }
        }
    }

    pub fn fill_block_without_parameters(&mut self, block: &compressed_ast::Block, parent_block: Block, builder: &mut FunctionBuilder) {
        for stmt in &block.body {
            self.generate_statement(stmt, parent_block, builder);
        }
    }

    pub fn generate_block(&mut self, block: &compressed_ast::Block, builder: &mut FunctionBuilder) {
        let ir_block = builder.create_block();

        if !block.parameters.is_empty() {
            builder.append_block_params_for_function_params(ir_block);
        }

        builder.switch_to_block(ir_block);

        let mut i = 0;
        while block.parameters.len() > i {
            let (id, typ) = block.parameters.get(i).unwrap();
            let block_param = builder.block_params(ir_block)[i];
            let var = Variable::from_u32(*id);
            builder.declare_var(var, convert_type(typ));
            builder.def_var(var, block_param);
            self.variables.insert(*id, var);

            i += 1;
        }

        for stmt in &block.body {
            self.generate_statement(stmt, ir_block, builder);
        }

        builder.seal_block(ir_block);
    }
    pub fn generate_function_definition(&mut self, definition: &compressed_ast::FunctionDefinition) {
        // inefficient, i do not care. Borrow checker got me tired.
        let cache = self.func_id_cache.clone();
        let entry = cache.get(&*definition.for_declaration).expect("could not find entry");

        let mut func = Function::with_name_signature(UserFuncName::user(0, entry.id.as_u32()), entry.sig.clone());

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_context);

        let _ = self.generate_block(&definition.block, &mut builder);

        if !self.function_has_returned {
            builder.ins().return_(&[]);
        }

        self.function_has_returned = false;

        builder.finalize();

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
