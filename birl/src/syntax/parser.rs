use crate::syntax::{Node, SyntaxError};

/// Tries to parse a source file into a root node.
pub(super) fn parse(source: &str) -> Result<Node, SyntaxError> {
	todo!()
}

/// Type encoding the result of a parse operation.
type ParseResult<'a> = Result<(Node<'a>, &'a str), SyntaxError<'a>>;

/// Parse an expression of any type at the start of the given slice.
fn next_expression(source: &str) -> ParseResult {
	todo!()
}

/// Parse a block expression at the start of the given slice.
fn next_block(source: &str) -> ParseResult {
	todo!()
}
