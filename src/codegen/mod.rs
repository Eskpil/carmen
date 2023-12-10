use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::{types::*, ExtFuncData, ExternalName, Function, KnownSymbol, LibCall, UserExternalName, UserFuncName, Value};
use cranelift_codegen::ir::{AbiParam, InstBuilder, Signature};
use cranelift_codegen::isa::{lookup, CallConv};
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::{Architecture, BinaryFormat, Environment, OperatingSystem, Riscv64Architecture, Triple, Vendor};

use std::collections::HashMap;
use std::env::var;
use std::ops::Index;
use cranelift_module::FuncOrDataId::Func;

use crate::{cil};
#[derive(Clone)]
struct FuncIdEntry {
    id: FuncId,
    sig: Signature,
}

pub struct Context {
    ctx: cranelift_codegen::Context,

    module: ObjectModule,

    func_id_cache: HashMap<String, FuncIdEntry>,
    variables: HashMap<u32, Variable>,
}

fn convert_type(cil_type: &cil::Type) -> Type {
    I64
}

// TODO: Do actual signature conversion.
fn convert_signature(cil_sig: &cil::Signature) -> Signature {
    let mut sig = Signature::new(CallConv::SystemV);

    for param in cil_sig.accepts.iter() {
        sig.params.push(AbiParam::new(convert_type(param)));
    }

    for ret in cil_sig.returns.iter() {
        sig.returns.push(AbiParam::new(convert_type(ret)));
    }

    return sig;
}

fn libcall_name(name: LibCall) -> String {
    println!("getting name for: {name}");
    return String::from("default");
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
            Box::new(libcall_name),
        )
        .expect("failed to create object builder");

        let module = ObjectModule::new(object_builder);

        Context {
            ctx: module.make_context(),
            module,

            func_id_cache: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn generate_function_declaration(&mut self, declaration: &cil::FunctionDeclaration) {
        let sig = convert_signature(&declaration.signature);

        println!("declaring function: {}", declaration.name);

        let func_id = self
            .module
            .declare_function(&declaration.name, Linkage::Export, &sig)
            .expect("could not declare function");

        self.func_id_cache.insert(declaration.name.clone(), FuncIdEntry {
            id: func_id,
            sig,
        });
    }

    pub fn generate_declarations(&mut self, declarations: Vec<cil::Declaration>) {
        for declaration in declarations.iter() {
            match declaration {
                cil::Declaration::Function(function) => {
                    self.generate_function_declaration(function);
                }
            }
        }
    }

    pub fn get_func_id(&mut self, name: String) -> &mut FuncIdEntry {
        self.func_id_cache.get_mut(&*name).expect("no func id")
    }

    pub fn generate_expression(&mut self, expr: &cil::Expression, builder: &mut FunctionBuilder) -> Value {
        match expr {
            cil::Expression::Literal(lit) => {
                builder.ins().iconst(I64, lit.0 as i64)
            }
            cil::Expression::Binary(bin) => {
                let lhs = self.generate_expression(&*bin.lhs, builder);
                let rhs = self.generate_expression(&*bin.rhs, builder);

                builder.ins().iadd(lhs, rhs)
            }
            cil::Expression::VariableLookup(vl) => {
                builder.use_var(Variable::from_u32(vl.id))
            }
            cil::Expression::Call(call) => {
                let cache = self.func_id_cache.clone();
                let func_entry = cache.get(&*call.name).expect("could not find func");

                let func_external_name = builder
                    .func
                    .declare_imported_user_function(UserExternalName::new(0, func_entry.id.clone().as_u32()));

                let func_sig_ref = builder.import_signature(func_entry.sig.clone());
                let func_ref = builder.import_function(ExtFuncData {
                    name: ExternalName::User(func_external_name),
                    signature: func_sig_ref,
                    colocated: true,
                });

                let mut arguments = Vec::<Value>::new();

                for arg in &call.arguments {
                    arguments.push(self.generate_expression(&*arg, builder));
                }

                let result = builder.ins().call(func_ref, &arguments);
                builder.inst_results(result).get(0).expect("no results?!?").clone()
            }
            x => todo!("implement: {:?}", x)
        }
    }

    pub fn generate_statement(&mut self, stmt: &cil::Statement, builder: &mut FunctionBuilder) {
        match stmt {
            cil::Statement::Return(ret) => {
                let val = self.generate_expression(&ret.expr, builder);
                builder.ins().return_(&[val]);
            }
            x => todo!("implement: {:?}", x)
        }
    }

    pub fn generate_block(&mut self, block: &cil::Block, builder: &mut FunctionBuilder) {
        let ir_block = builder.create_block();
        let mut has_switched_block = false;

        if block.parameters.len() > 0 {
            for param in block.parameters.clone() {
                let block_param = builder.append_block_param(ir_block, convert_type(&param.typ));

                if !has_switched_block {
                    builder.switch_to_block(ir_block);
                    has_switched_block = true
                }

                let var = Variable::from_u32(param.id);
                builder.declare_var(var, convert_type(&param.typ));
                builder.def_var(var, block_param);

                self.variables.insert(param.id, var);
            }
        }

        if !has_switched_block {
            builder.switch_to_block(ir_block);
            has_switched_block = true
        }

        builder.seal_block(ir_block);

        for stmt in block.statements.clone() {
            self.generate_statement(&stmt, builder);
        }
    }

    pub fn generate_function_definition(&mut self, definition: &cil::FunctionDefinition) {
        // inefficient, i do not care. Borrow checker got me tired.
        let cache = self.func_id_cache.clone();
        let entry = cache.get(&*definition.name).expect("could not find entry");

        let mut func = Function::with_name_signature(UserFuncName::user(0, 0), entry.sig.clone());

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_context);

        self.generate_block(&definition.block, &mut builder);

        builder.seal_all_blocks();
        builder.finalize();

        self.ctx.func = func;

        println!("{}", self.ctx.func.display());

        self.module
            .define_function(entry.id, &mut self.ctx)
            .expect("failed to define function");

        // Clear the context for the next function.
        self.ctx.clear();
    }

    pub fn generate_definitions(&mut self, definitions: Vec<cil::FunctionDefinition>) {
        for definition in definitions.iter() {
            self.generate_function_definition(definition);
        }
    }

    pub fn generate(&mut self, program: &cil::Program) {
        self.generate_declarations(program.declarations.clone());
        self.generate_definitions(program.definitions.clone());
    }

    pub fn build(self) -> Vec<u8> {
        let product = self.module.finish();
        product.emit().expect("could not emit data")
    }
}
