use std::collections::HashMap;
use std::sync::Once;
use birl::parser::{Command, CommandKind, FunctionParameter, TypeKind};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::{AddressSpace, OptimizationLevel};
use inkwell::execution_engine::ExecutionEngine;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{BasicTypeEnum, FloatType, FunctionType, IntType, PointerType, StructType};
use inkwell::values::StructValue;
use crate::ast::Module;

/// The lock controlling whether the native LLVM target has been initialized.
///
/// It must only be done once, hence, why this static variable exists. Whenever
/// we're building a module we check whether state has been initialized and do
/// it if it hasn't.
static INIT_NATIVE: Once = Once::new();

pub struct BuiltModule<'a> {
	context: &'a Context,
	module: inkwell::module::Module<'a>,
	jit: ExecutionEngine<'a>,
}
impl<'a> BuiltModule<'a> {
	pub fn build(context: &'a Context, module: &Module) -> Result<Self, CompilationError> {
		/* Initialize the native target. */
		INIT_NATIVE.call_once(|| {
			Target::initialize_native(&InitializationConfig::default())
				.expect("Failed to initialize native target");
		});

		/* Set up the module and its JIT execution engine. */
		let module = context.create_module("");
		let jit = module
			.create_jit_execution_engine(OptimizationLevel::Default)
			.unwrap();

		/* Set up the basic data types as used in the runtime. */
		let types = Types::new(context, &jit);
	}

	pub fn run(&mut self) {

	}
}

struct Types<'a> {
	integer: IntType<'a>,
	number: FloatType<'a>,
	size: IntType<'a>,
	text_ptr: PointerType<'a>,
	text: StructType<'a>,
	list_node_ptr: PointerType<'a>,
	list_node: StructType<'a>,
	object: StructType<'a>,
	object_ptr: PointerType<'a>,
}
impl<'a> Types<'a> {
	pub fn new(context: &'a Context, jit: &ExecutionEngine) -> Self {
		let integer = context.i64_type();
		let number = context.f64_type();
		let size = context.ptr_sized_int_type(
			jit.get_target_data(),
			None);

		let text_ptr = context.i8_type().ptr_type(AddressSpace::Global);
		let text = context.struct_type(
			&[
				BasicTypeEnum::IntType(size),
				BasicTypeEnum::PointerType(text_ptr),
			],
			false);

		let object = context.struct_type(
			&[
				BasicTypeEnum::IntType(context.i32_type()),
				BasicTypeEnum::IntType(size),
				BasicTypeEnum::IntType(size),
			],
			false);
		let object_ptr = object.ptr_type(AddressSpace::Global);

		let list_node_ptr = context.i8_type().ptr_type(AddressSpace::Global);
		let list_node = context.struct_type(
			&[
				BasicTypeEnum::PointerType(list_node_ptr),
				BasicTypeEnum::StructType(object),
			],
			false);

		Self {
			integer,
			number,
			size,
			text_ptr,
			text,
			list_node_ptr,
			list_node,
			object,
			object_ptr
		}
	}
}

pub enum CompilationError {

}



struct Compiler<'a> {
	context: &'a Context,
	parameters: &'a [FunctionParameter],
	commands: &'a [Command],
	vars: HashMap<String, StructValue<'a>>
}
impl Compiler {
	/// Builds
	pub fn build<A>(mut self) {
		let context = Context::create();
		let mut builder = context.create_builder();

		self.visit(&context, &mut builder, commands.as_ref());
	}

	fn visit(
		&mut self,
		context: &Context,
		builder: &mut Builder,
		node: &[Command]) -> Result<(), CompilationError> {

		for command in node {
			match command.kind {
				CommandKind::Return => {}
				CommandKind::Quit => {}
				CommandKind::Print => {}
				CommandKind::PrintLn => {}
				CommandKind::PrintDebug => {}
				CommandKind::Declare => {}
				CommandKind::Set => {}
				CommandKind::Compare => {}
				CommandKind::EndSubScope => {}
				CommandKind::ExecuteIfEqual => {}
				CommandKind::ExecuteIfNotEqual => {}
				CommandKind::ExecuteIfEqualOrLess => {}
				CommandKind::ExecuteIfLess => {}
				CommandKind::ExecuteIfEqualOrGreater => {}
				CommandKind::ExecuteIfGreater => {}
				CommandKind::Call => {}
				CommandKind::GetStringInput => {}
				CommandKind::GetNumberInput => {}
				CommandKind::GetIntegerInput => {}
				CommandKind::ConvertToNum => {}
				CommandKind::ConvertToInt => {}
				CommandKind::IntoString => {}
				CommandKind::ExecuteWhileEqual => {}
				CommandKind::ExecuteWhileNotEqual => {}
				CommandKind::ExecuteWhileEqualOrLess => {}
				CommandKind::ExecuteWhileLess => {}
				CommandKind::ExecuteWhileGreater => {}
				CommandKind::ExecuteWhileEqualOrGreater => {}
				CommandKind::RangeLoop => {}
				CommandKind::MakeNewList => {}
				CommandKind::QueryListSize => {}
				CommandKind::AddListElement => {}
				CommandKind::RemoveListElement => {}
				CommandKind::IndexList => {}
				CommandKind::BreakScope => {}
				CommandKind::SkipNextIteration => {}
			}
		};
	}
}

struct Variable {
	kind: TypeKind,
}
