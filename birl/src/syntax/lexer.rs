use std::ops::Range;
use std::str::FromStr;

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
#[derive(Debug)]
pub struct TokenStream<'a> {
	/// The string we are taking our tokens from.
	source: &'a str,

	/// The offset to the start of the current token.
	///
	/// This value should only be modified by means of the [`scan()`]'s return
	/// type's [`consume()`] function. This ensures not only that this value
	/// will stay at a character boundary at all times, it ensures the token
	/// may only change through acceptation or rejection of possible values.
	///
	/// [`scan()`]: TokenStream::scan
	/// [`commit()`]: Scan::consume
	token: usize,
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

		/* Consume all whitespace in the buffer. */
		self.grow_token(|_, candidate| candidate.is_ascii_whitespace());

		if let "\"" = self.grow_token(|_, candidate| candidate == '"')? {

		}

		Some(Token {
			internal: InternalToken::Source(&self.source[..])
		})
	}
}
impl<'a> TokenStream<'a> {
	fn next
}
impl<'a> TokenStream<'a> {
	/// Scans over the token stream, starting at the current position of the
	/// cursor, and applies the given functor at each step to try and produce
	/// a value.
	///
	/// The semantics of this operation are very close to those of a parser
	/// combinator, except that, instead of having the functor itself drive the
	/// cursor, it has this function do it, while consulting the functor at each
	/// steep to whether the input is complete.
	fn scan<T, E>(
		&mut self,
		mut cond: impl FnMut(&'a str, char) -> ScanOp<T, E>)
		-> Result<Scan<T>, E> {

		let mut base = self.token;
		let next_char_len = || {
			let mut offset = 1usize;

			while !self.source.is_char_boundary(base.checked_add(offset).unwrap()) {
				offset += 1;
			}

			offset
		};

		let mut extension = 0usize;
		loop {
			if base >= self.source.len() {
				/* We can't grow our token further, seeing as we're past the end
				 * of the input string. We should bail out right away. */
				return extension
			}

			/* Fetch the character immediately following the end of the token.
			 *
			 * We do this by slicing the string in such a way that the first and
			 * only character inside the new slice is the character we want.
			 * Then, we instantiate a character iterator over it and pull the
			 * character out.
			 *
			 * This is, admittedly, a very roundabout way of doing this, as
			 * opposed to decoding the byte slice we already know comprise of
			 * the character. But it turns out the standard library exposes no
			 * way of decoding a UTF-8 byte slice into a character. And so,
			 * rather tha implement a UTF-8 to UTF-32 conversion procedure, we
			 * have elected to just use the .chars() iterator.
			 */
			let char_len = next_char_len();
			let char = &self.source[base..base + char_len];
			let char = char.chars().next().unwrap();

			let slice = &self.source[self.token.clone()];
			if (cond)(slice, char) {
				/* The character has been accepted. Extend the token range so
				 * that the token will include it, and move on to processing the
				 * next candidate for inclusion. */
				base += char_len;
			} else {
				/* The character has been rejected and so we can't process any
				 * characters past this one and have reached the end of this
				 * extension process. */
				break
			}
		}

		extension
	}
}

/// Character operations to be applied during character scans.
///
/// Throughout a character scan operation, it is often desirable to have more
/// than a binary acceptation or undiagnosed rejection of characters. It may be
/// also be desirable for a scanning operator to give a motive for the rejection
/// of a character, as well as for it ot skip characters at the head or tail
/// end of a token.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum ScanOp<T, E> {
	/// Accept the input with the given value.
	Accept(T),
	/// Reject the input as a while with the given error.
	Reject(E),
	/// Append this character to the token and try again.
	Continue,
}

/// The result of a scanning operation over a token stream.
#[derive(Debug)]
struct Scan<'a, T> {
	/// The underlying stream instance.
	stream: &'a mut TokenStream<'a>,
	/// The length of the accepted character string.
	len: usize,
	/// The data value generated by the acceptation of the string.
	value: T,
}
impl<'a, T> Scan<'a, T> {
	/// The accepted character string produced by the scan operation.
	pub fn string(&self) -> &str {
		let range = self.stream.token..self.stream.token + self.len;
		&self.stream.source[range]
	}

	/// Peeks the value produced by the scanning operation.
	pub fn peek(&self) -> &T {
		&self.value
	}

	/// Discards the character string produced by this scan operation.
	pub fn rollback(self) {}

	/// Consumes the selected character string scanner over by this operation
	/// and readies the scanner to take on the next token. This function also
	/// returns the value produced by the scanning operation.
	pub fn consume(self) -> T {
		self.stream.token += self.len;
		self.value
	}
}

/// The type for errors that may happen during tokenization.
pub enum LexError {

}

