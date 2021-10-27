use std::str::FromStr;
use scanner::{Scan, Scanner, ScanOp, StrDriver};

/// A reference to a token in the source code.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Token<'a> {
	/// This token is an identifier.
	Identifier(Identifier<'a>),
	/// This token is a quoted string literal.
	Literal(Literal<'a>),
	/// This token is a punctuation mark.
	Punctuation(Punctuation<'a>),
}

/// A reference to an identifier token in the source code.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

/// A reference to whitespace on the file.
///
/// This type is only used internally to distinguish between breaking and
/// non-breaking whitespace, both allowed by the language specification.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Whitespace {
	/// Whether this is breaking whitespace or not.
	breaking: bool
}

/// The tokenizer.
///
/// This structure defines an iterator that yields a stream of tokens
/// originating in the source file.
pub struct TokenStream<'a> {
	/// The scanner over the input string.
	scanner: Scanner<StrDriver<'a>>,
	/// Whether the tokenizer has determined the next statement breaking word
	/// should be indicated in the stream by the addition of an implicit semi-
	/// colon.
	///
	/// When this internal flag is marked, the first call to the next function
	/// must always immediately return a new semicolon and set this flag to
	/// false.
	requires_implicit_semicolon: bool,
}
impl<'a> TokenStream<'a> {
	/// Creates a new token stream lexing the given source string.
	pub fn new(source: &'a str) -> Self {
		Self {
			scanner: Scanner::from_str(source),
			requires_implicit_semicolon: false
		}


	}
}
impl<'a> Iterator for TokenStream<'a> {
	type Item = Result<Token<'a>, LexError>;
	fn next(&mut self) -> Option<Self::Item> {

		let breaking_whitespace = match self.scanner.ref_scan(accept_whitespace) {
			Scan::Accepted(whitespace) => whitespace.consume().breaking,
			Scan::Dropped(()) => false,
			Scan::Rejected(()) => false,
			Scan::Unfruitful => unreachable!()
		};

		if self.requires_implicit_semicolon && breaking_whitespace {
			self.requires_implicit_semicolon = false;
			return Some(Ok(Token::Punctuation(Punctuation {
				internal: InternalToken::Semicolon
			})))
		}

		match self.scanner.ref_scan(accept_token) {
			Scan::Accepted(accepted) => {
				let token = accepted.consume();

				/* Determine whether the next breaking whitespace has to be
				 * automatically inserted or not. */
				let insert_break = if let Token::Punctuation(punct) = &token {
					if punct.as_str() == ";" {
						true
					} else {
						false
					}
				} else {
					false
				};
				self.requires_implicit_semicolon = insert_break;

				Some(Ok(token))
			},
			Scan::Rejected(what) => Some(Err(what)),
			Scan::Unfruitful => None,
			Scan::Dropped(_) => panic!(),
		}
	}
}

/// Accepts whitespace and determines whether it is breaking or not.
fn accept_whitespace(string: &str, candidate: Option<&char>) -> ScanOp<Whitespace, (), ()> {
	let mut whitespace = Whitespace { breaking: false };

	eprintln!("accept_whitespace: {}, {:?}", string, candidate);

	match candidate {
		Some(&'\n') => {
			whitespace.breaking = true;
			ScanOp::Continue
		},
		Some(char) if char.is_ascii_whitespace() =>
			ScanOp::Continue,
		Some(_) | None =>
			if string.is_empty() {
				ScanOp::Drop(())
			} else {
				ScanOp::Accept(whitespace)
			}
	}
}

/// Accepts any token at the start of the given slice.
fn accept_token<'a>(string: &'a str, candidate: Option<&char>) -> ScanOp<Token<'a>, (), LexError> {
	let pat = (
		accept_identifier(string, candidate),
		accept_literal(string, candidate),
		accept_punctuation(string, candidate));

	match pat {
		(ScanOp::Accept(t), _, _) => ScanOp::Accept(Token::Identifier(t)),
		(_, ScanOp::Accept(t), _) => ScanOp::Accept(Token::Literal(t)),
		(_, _, ScanOp::Accept(t)) => ScanOp::Accept(Token::Punctuation(t)),
		(ScanOp::Continue, _, _) => ScanOp::Continue,
		(_, ScanOp::Continue, _) => ScanOp::Continue,
		(_, _, ScanOp::Continue) => ScanOp::Continue,
		(ScanOp::Reject(what), _, _) =>
			ScanOp::Reject(what),
		(_, ScanOp::Reject(what), _) =>
			ScanOp::Reject(what),
		(_, _, ScanOp::Reject(what)) =>
			ScanOp::Reject(what),
		(ScanOp::Drop(_), ScanOp::Drop(_), ScanOp::Drop(_)) =>
			ScanOp::Reject(LexError::ExpectedToken),
	}
}

/// Accepts a literal value at the start of the given slice.
fn accept_literal<'a>(string: &'a str, candidate: Option<&char>) -> ScanOp<Literal<'a>, (), LexError> {
	let pat = (
		accept_integer_dec(string, candidate),
		accept_string(string, candidate));

	match pat {
		(ScanOp::Accept(t), _) => ScanOp::Accept(Literal::Integer(t)),
		(_, ScanOp::Accept(t)) => ScanOp::Accept(Literal::String(t)),
		(ScanOp::Continue, _) => ScanOp::Continue,
		(_, ScanOp::Continue) => ScanOp::Continue,
		(ScanOp::Reject(what), _) =>
			ScanOp::Reject(what),
		(_, ScanOp::Reject(what)) =>
			ScanOp::Reject(what),
		(ScanOp::Drop(_), ScanOp::Drop(_)) =>
			ScanOp::Drop(()),
	}
}

/// Accepts a decimal integer literal at the start of the given slice.
fn accept_integer_dec(string: &str, candidate: Option<&char>) -> ScanOp<i64, (), LexError> {
	if candidate.map(char::is_ascii_digit).unwrap_or(false) {
		ScanOp::Continue
	} else {
		match string.parse() {
			Ok(integer) => ScanOp::Accept(integer),
			Err(_) => ScanOp::Drop(())
		}
	}
}

/// Accepts an escaped string literal at the start of the given slice.
fn accept_string<'a>(string: &'a str, candidate: Option<&char>) -> ScanOp<&'a str, (), LexError> {
	eprintln!("accept_string: {}, {:?}", string, candidate);

	let s = string.is_empty() && candidate.map(|c| *c == '"').unwrap_or(false);
	let l = string.starts_with('"') || s ;
	let r = string.len() > 1 && string.ends_with('"') && !string.ends_with("\\\"");

	if l && r {
		ScanOp::Accept(&string[1..string.len() - 1])
	} else if l && candidate.is_some() {
		ScanOp::Continue
	} else {
		ScanOp::Drop(())
	}
}

/// Accepts an instance of punctuation at the start of the given slice.
fn accept_punctuation<'a>(string: &'a str, candidate: Option<&char>) -> ScanOp<Punctuation<'a>, (), LexError> {
	match string {
		"&" | "|" | ":" | "," | "(" | ")" | ";" | "+" | "-" | "*" | "/" =>
			ScanOp::Accept(Punctuation {
				internal: InternalToken::Source(string)
			}),
		_ => match candidate {
			Some(candidate) => match candidate {
				'&' | '|' | ':' | ',' | '(' | ')' | ';' | '+' | '-' | '*' | '/' =>
					ScanOp::Continue,
				_ => ScanOp::Drop(())
			},
			_ => ScanOp::Drop(())
		}
	}
}

/// Accepts an instance of an identifier at the start of the given slice.
fn accept_identifier<'a>(string: &'a str, candidate: Option<&char>) -> ScanOp<Identifier<'a>, (), LexError> {
	eprintln!("accept_identifier: {}, {:?}", string, candidate);

	if string.is_empty() && candidate.map(char::is_ascii_digit).unwrap_or(false) {
		ScanOp::Drop(())
	} else {
		let ident_valid = |c: char|
			   c.is_ascii_alphanumeric()
			|| c == '_';
		let candidate = match candidate {
			Some(candidate) => *candidate,
			None => return if string.is_empty() {
				ScanOp::Accept(Identifier {
					internal: InternalToken::Source(string)
				})
			} else {
				ScanOp::Drop(())
			}
		};

		if ident_valid(candidate) {
			ScanOp::Continue
		} else if !string.is_empty() {
			ScanOp::Accept(Identifier {
				internal: InternalToken::Source(string)
			})
		} else {
			ScanOp::Drop(())
		}
	}
}

/// The type for errors that may happen during acceptance of tokens.
#[derive(Debug)]
pub enum LexError {
	/// Expected any token, but didn't match with any valid pattern.
	ExpectedToken,
}

#[cfg(test)]
mod tests {
	use scanner::{Accepted, Scan, Scanner};
	use crate::syntax::lexer::{Identifier, InternalToken, Punctuation, Token, TokenStream};

	/// Initializes a token stream from the source data.
	fn stream() -> TokenStream<'static> {
		TokenStream::new(SOURCE)
	}

	/// Initializes a token stream from the source data.
	fn stream_fn() -> impl FnMut() -> Option<Token<'static>> + 'static {
		let mut stream = TokenStream::new(SOURCE);
		move || stream.next().transpose().unwrap()
	}

	/// Creates an identifier.
	fn ident(data: &str) -> Token {
		Token::Identifier(Identifier { internal: InternalToken::Source(data) })
	}

	/// Creates a punctuation mark.
	fn punct(data: &str) -> Token {
		Token::Punctuation(Punctuation { internal: InternalToken::Source(data) })
	}

	/// Accepts the first token in the source.
	#[test]
	fn first_token() {
		let mut next = stream_fn();
		assert_eq!(next(), Some(ident("JAULA")))
	}

	/// Accepts the first statement in the source.
	#[test]
	fn first_statement() {
		let mut next = stream_fn();
		assert_eq!(next(), Some(ident("JAULA")));
		assert_eq!(next(), Some(ident("FIBONACCI")));
		assert_eq!(next(), Some(punct("(")));
		assert_eq!(next(), Some(ident("NUMERO")));
		assert_eq!(next(), Some(punct(":")));
		assert_eq!(next(), Some(ident("BATATA")));
		assert_eq!(next(), Some(ident("DOCE")));
		assert_eq!(next(), Some(punct(")")));
		assert_eq!(next(), Some(punct(";")));
	}

	/// Accepts an escaped quoted string.
	#[test]
	fn accept_string() {
		let mut scanner = Scanner::from_str(r#""A\"\"B\\C""#);
		let string = scanner
			.ref_scan(super::accept_string)
			.unwrap()
			.consume();

		assert_eq!(string, r#"A\"\"B\\C"#);
	}

	/// Source code for producing the Fibonacci sequence.
	///
	/// This is the piece of source code used throughout our unit tests in this
	/// module, as it provides us with most of the things we will see throughout
	/// our dealings with BIRL source code.
	const SOURCE: &'static str = r#"
		JAULA FIBONACCI(NUMERO: BATATA DOCE)
			E ELE QUE A GENTE QUER: NUMERO, 1
			MENOR OU E MEMO:
				BIRL: NUMERO
			FIM
			VEM: RESULTADO, 0
			E HORA DO: FIBONACCI, NUMERO - 1
			BORA: RESULTADO, TREZE
			E HORA DO: FIBONACCI, NUMERO - 2
			BIRL: RESULTADO + TREZE
		SAINDO DA JAULA

		JAULA PRINTA_FIBONACCI(TOTAL: BATATA DOCE, VEZES: BATATA DOCE)
			E ELE QUE A GENTE QUER: TOTAL, VEZES
			E ELE MEMO:
				BIRL
			FIM
			E HORA DO: FIBONACCI, TOTAL
			CE QUER VER ISSO: TREZE
			E HORA DO: PRINTA_FIBONACCI, TOTAL + 1, VEZES
		SAINDO DA JAULA

		JAULA SHOW
			VEM: VEZES, 13
			E HORA DO: PRINTA_FIBONACCI, 0, VEZES
		SAINDO DA JAULA
	"#;
}