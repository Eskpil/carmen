use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::{
    types::*, ExtFuncData, ExternalName, Function, KnownSymbol, LibCall, UserExternalName,
    UserFuncName,
};
use cranelift_codegen::ir::{AbiParam, InstBuilder, Signature};
use cranelift_codegen::isa::{lookup, CallConv};
use cranelift_codegen::settings;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::{Architecture, BinaryFormat, Environment, OperatingSystem, Triple, Vendor};

use std::fs;
use std::io::Write; // bring trait into scope

fn libcall_name(name: LibCall) -> String {
    println!("getting name for: {name}");
    return String::from("default");
}

fn perform_main() {
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
    .expect("failed to create object builer");

    let mut module = ObjectModule::new(object_builder);
    let mut context = module.make_context();

    // Generate code for the `add` function.
    generate_add_function(&mut context, &mut module);
    println!("made add function");

    // Generate code for the `main` function.
    generate_main_function(&mut context, &mut module);
    println!("made main function");

    let product = module.finish();

    let code = product.emit().expect("failed to emit code");

    let mut file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .open("a.out")
        .expect("failed to open file");

    file.write_all(&code).expect("could not write code to file")
}

fn generate_add_function(context: &mut Context, module: &mut ObjectModule) {
    let mut sig = Signature::new(CallConv::SystemV);

    sig.returns.push(AbiParam::new(I64));
    sig.params.push(AbiParam::new(I64));
    sig.params.push(AbiParam::new(I64));
    context.func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
    // Create the add function.
    let mut builder_context = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);

    {
        let block = builder.create_block();

        let a = builder.append_block_param(block, I64);
        let b = builder.append_block_param(block, I64);

        builder.switch_to_block(block);

        let result = builder.ins().iadd(a, b);

        // Set the result as the return value.
        builder.ins().return_(&[result]);
    }

    // Finalize the add function.
    builder.seal_all_blocks();
    builder.finalize();

    // Name the function and add it to the module.
    let add_func_id = module
        .declare_function("add", Linkage::Export, &context.func.signature)
        .expect("could not declare function");
    module
        .define_function(add_func_id, context)
        .expect("failed to define function");

    // Clear the context for the next function.
    context.clear();
}

fn generate_main_function(context: &mut Context, module: &mut ObjectModule) {
    let mut main_func_sig = Signature::new(CallConv::SystemV);
    main_func_sig.returns.push(AbiParam::new(I64));

    context.func = Function::with_name_signature(UserFuncName::user(0, 1), main_func_sig);

    let mut builder_context = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);

    let mut add_func_sig = Signature::new(CallConv::SystemV);
    add_func_sig.returns.push(AbiParam::new(I64));
    add_func_sig.params.push(AbiParam::new(I64));
    add_func_sig.params.push(AbiParam::new(I64));

    let add_func_external_name = builder
        .func
        .declare_imported_user_function(UserExternalName::new(0, 0));

    let add_func_sig_ref = builder.import_signature(add_func_sig);
    let add_func_ref = builder.import_function(ExtFuncData {
        name: ExternalName::User(add_func_external_name),
        signature: add_func_sig_ref,
        colocated: true,
    });

    //let add_func_ref = module.declare_func_in_func(add_func_id, builder.func);

    {
        let block = builder.create_block();

        builder.switch_to_block(block);
        builder.seal_block(block);

        let arg1 = builder.ins().iconst(I64, 10);
        let arg2 = builder.ins().iconst(I64, 20);

        let _ = builder.ins().call(add_func_ref, &[arg1, arg2]);

        let out = builder.ins().iconst(I64, 69);
        builder.ins().return_(&[out]);
    }

    builder.finalize();

    let main_func_id = module
        .declare_function("main", Linkage::Export, &context.func.signature)
        .expect("could not declare function");
    module
        .define_function(main_func_id, context)
        .expect("could not define function");

    // Clear the context for the next function.
    context.clear();
}
