use std::ops::Range;
use std::str::FromStr;
use crate::scanner::{Scanner, ScanOp, StringDriver};

/// A reference to a token in the source code.
pub enum Token<'a> {
	/// This token is an identifier.
	Identifier(Identifier<'a>),
	/// This token is a quoted string literal.
	Literal(Literal<'a>),
	/// This token is a punctuation mark.
	Punctuation(Punctuation<'a>),
}

/// A reference to an identifier token in the source code.
pub struct Identifier<'a> {
	/// The structure holding the data in this token.
	internal: InternalToken<'a>,
}
impl<'a> Identifier<'a> {
	/// The string of characters that define this identifier.
	pub fn as_str(&self) -> &str {
		match &self.internal {
			InternalToken::Source(string) => *string,
			InternalToken::Semicolon => ";"
		}
	}

	/// Tries to parse the data in this token into the given type.
	pub fn parse<T>(&self) -> Result<T, T::Err>
		where T: FromStr {

		self.as_str().parse()
	}
}

/// A reference to a punctuation mark token in the source code.
pub struct Punctuation<'a> {
	/// The structure holding the data in this token.
	internal: InternalToken<'a>,
}
impl<'a> Punctuation<'a> {
	/// The string of characters that define this identifier.
	pub fn as_str(&self) -> &str {
		match &self.internal {
			InternalToken::Source(string) => *string,
			InternalToken::Semicolon => ";"
		}
	}

	/// Tries to parse the data in this token into the given type.
	pub fn parse<T>(&self) -> Result<T, T::Err>
		where T: FromStr {

		self.as_str().parse()
	}
}

/// A reference to a literal token in the source code.
pub enum Literal<'a> {
	/// This is a quoted string literal.
	String(&'a str),
	/// This is an integer literal.
	Integer(i64)
}

/// The actual data held in a [Token] structure.
///
/// This structure lets us implement special tokens that don't necessarily point
/// to anything and keep them as implementation details, as far as consumers of
/// the public [Token] structure are concerned.
///
/// # BIRL and Automatic Semicolon Insertion
/// Given the nature of the language, in which whitespace can have meaning in
/// the syntactical meaning, it can be very useful sometimes to insert tokens
/// into the stream which may not necessarily be found in the source file. This
/// is often done in order to aid the parser when a stream would otherwise be
/// ambiguous due to removal of whitespace.
///
/// An example of a well-formed expression that can become ambiguous in an
/// unmodified token stream is `<Statement A>  <Statement B>`. The expression,
/// because of the two spaces, is defined in the spec as expressing two separate
/// statements. If we don't add a special separator token, a consumer handling
/// the the token stream will have no way to tell these are separate statements.
///
/// [Token]: Token
enum InternalToken<'a> {
	/// Any token originating in the source file.
	Source(&'a str),
	/// This is an automatic semicolon.
	///
	/// In every aspect, this token should be treated as a standard semicolon,
	/// except that it doesn't point to a specific semicolon in the source file
	/// and, instead, has been inserted into the stream by the tokenizer in
	/// order to clarify what would be an otherwise ambiguous sequence.
	Semicolon,
}

/// The tokenizer.
///
/// This structure defines an iterator that yields a stream of tokens
/// originating in the source file.
pub struct TokenStream<'a> {
	/// The scanner over the input string.
	scanner: Scanner<StringDriver<'a>>,
	/// Whether the tokenizer has determined the next statement breaking word
	/// should be indicated in the stream by the addition of an implicit semi-
	/// colon.
	///
	/// When this internal flag is marked, the first call to the next function
	/// must always immediately return a new semicolon and set this flag to
	/// false.
	requires_implicit_semicolon: bool,
}
impl<'a> Iterator for TokenStream<'a> {
	type Item = Result<Token<'a>, LexError>;
	fn next(&mut self) -> Option<Self::Item> {
		if self.requires_implicit_semicolon {
			self.requires_implicit_semicolon = false;
			return Some(Ok(Token::Punctuation(Punctuation {
				internal: InternalToken::Semicolon
			})))
		}

		self.scanner.scan(|candidate| {
			ScanOp::Continue
		})?;

		Some(Token {
			internal: InternalToken::Source(&self.source[..])
		})
	}
}

fn scan_last_

/// The type for errors that may happen during tokenization.
pub enum LexError {

}

