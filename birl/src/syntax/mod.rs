/// Source code tokenizer and token stream implementation.
mod lexer;

/// Token parser and AST generation functionality.
mod parser;

use std::rc::Rc;

type NodeRef<'a> = Rc<Node<'a>>;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Node<'a> {
	line: usize,
	column: usize,
	kind: NodeType<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum NodeType<'a> {
	Assign {
		binding: NodeRef<'a>,
		value: NodeRef<'a>
	},
	Binding {
		name: &'a str,
	},
	Block(Vec<NodeRef<'a>>),
	If {
		condition: NodeRef<'a>,
		then: NodeRef<'a>,
		other: NodeRef<'a>,
	},
	Return {
		value: NodeRef<'a>
	},
	Literal {
		value: &'a str,
	}
}

pub struct Syntax {

}
impl Syntax {

}

#[derive(Debug)]
pub struct SyntaxError<'a> {
	node: Node<'a>,
	cause: Cause,
}

#[derive(Debug)]
enum Cause {

}
