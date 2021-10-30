use std::borrow::Borrow;
use std::collections::HashMap;
use birl::parser::{Command, CommandArgument, FunctionParameter, ParserResult};

/// A bundle for a module.
///
/// This structure holds an AST-like structure for a module.
pub struct Module {
	global: GlobalFunction,
	named: HashMap<String, NamedFunction>,
}
impl Module {
	/// Parses the given source code into a module, if possible.
	pub fn parse<A>(source: A) -> Result<Self, ParseError>
		where A: AsRef<str> {

		/* Start by building a complete list of statements from the source.*/
		let source = source.as_ref();
		let statements = source.lines()
			.enumerate()
			.filter_map(|(i, line)| {
				birl::parser::parse_line(line)
					.map(|line| match line {
						ParserResult::Nothing => None,
						other => Some(other),
					})
					.map_err(|msg| (i + 1, msg))
					.transpose()
			})
			.collect::<Result<Vec<_>, _>>()
			.map_err(|(line, what)| ParseError::InvalidStatement {
				line,
				what
			})?;

		/* Separate the module into functions. */
		let mut global = GlobalFunction { commands: Default::default() };
		let mut named = HashMap::new();

		for statement in statements {
			let mut scope = None;
			match statement {
				ParserResult::FunctionStart(function) => {
					let scope = scope.replace((
						function.name.clone(),
						NamedFunction {
							parameters: function.arguments,
							commands: vec![]
						}));
					if let Some((name, _)) = scope {
						return Err(ParseError::NestedFunction {
							parent: name,
							nested: function.name
						})
					}
				},
				ParserResult::FunctionEnd => match scope.take() {
					Some((name, function)) =>
						if let Some(_) = named.insert(name.clone(), function) {
							return Err(ParseError::RedefinedFunction { name })
						}
					None =>
						return Err(ParseError::GlobalFunctionTerminator)
				},
				ParserResult::Command(command) =>
					if let Some((_, function)) = &mut scope {
						function.commands.push(command)
					} else {
						global.commands.push(command)
					},
				ParserResult::Nothing =>
					/* We filter out empty lines during the previous stage. */
					unreachable!()
			}
		}

		Ok(Self {
			global,
			named
		})
	}

	/// Returns a reference to the value associated with the global function.
	pub fn global(&self) -> &GlobalFunction {
		&self.global
	}
}

/// The definition of the global function in a module.
#[derive(Debug, Clone)]
pub struct GlobalFunction {
	commands: Vec<Command>,
}
impl GlobalFunction {
	/// All of the commands executed by this function.
	pub fn commands(&self) -> &[Command] {
		&self.commands[..]
	}
}

/// The definition of a named function in a module.
#[derive(Debug, Clone)]
pub(crate) struct NamedFunction {
	parameters: Vec<FunctionParameter>,
	commands: Vec<Command>,
}
impl NamedFunction {
	/// All of the commands executed by this function.
	pub fn commands(&self) -> &[Command] {
		&self.commands[..]
	}

	/// All of the parameters expected to be handed to this function.
	pub fn parameters(&self) -> &[FunctionParameter] {
		&self.parameters[..]
	}
}

/// A map keyed on uniquely defined function names.
///
struct FunctionMap<T> {
	global: GlobalFunction,
	named: HashMap<String, NamedFunction>,
}
impl<T> FunctionMap<T> {
	/// Creates a new function map, associating the global function with the
	/// default value for its type.
	pub fn new() -> Self
		where T: Default {
		Self {
			global: Default::default(),
			named: Default::default()
		}
	}

	/// Returns a reference to the value associated with the global function.
	pub fn global(&self) -> &T {
		&self.global
	}

	/// Returns a mutable reference to the value associated with the global function.
	pub fn global_mut(&mut self) -> &mut T {
		&mut self.global
	}

	/// Returns a reference to the value associated with the given named
	/// function, if there is any.
	pub fn named<K>(&self, name: &K) -> Option<&T>
		where String: Borrow<K>,
			  K: Ord + Eq + ?Sized {

		self.named.get(name)
	}

	/// Returns a mutable reference to the value associated with the given named
	/// function, if there is any.
	pub fn named_mut<K>(&mut self, name: &K) -> Option<&mut T>
		where String: Borrow<K>,
			  K: Ord + Eq + ?Sized {

		self.named.get_mut(name)
	}

}

/// Errors that may occur during parsing.
#[derive(Debug, thiserror::Error)]
pub enum ParseError {
	#[error("invalid statement on line {}: {}", line, what)]
	InvalidStatement {
		line: usize,
		what: String
	},
	#[error("tried to nest a function ({}) inside another function ({})", nested, parent)]
	NestedFunction {
		parent: String,
		nested: String,
	},
	#[error("tried to redefine the function with name {}", name)]
	RedefinedFunction {
		name: String
	},
	#[error("tried to finish a function while in global scope")]
	GlobalFunctionTerminator,
}
