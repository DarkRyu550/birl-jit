use crate::{Driver, RefDriver};

/// A scan driver iterating over characters in string slices.
pub struct StrDriver<'a> {
	/// The source string we pull tokens off of.
	source: &'a str,
	/// An iterator over its characters.
	chars: std::iter::Peekable<std::str::Chars<'a>>,
	/// The candidate character.
	candidate: Option<char>,
	/// The offset to the start of the current token.
	token: usize,
	/// The current length of the token plus lookahead.
	len: usize,
	/// The current length of the token.
	acc: usize,
}
impl<'a> StrDriver<'a> {
	pub(crate) fn new(source: &'a str) -> Self {
		let mut chars = source.chars().peekable();
		let candidate = chars.peek().cloned();

		Self {
			source,
			chars,
			candidate,
			token: 0,
			len: candidate.map(|char| {
				let mut buffer = [0; 6];
				char.encode_utf8(&mut buffer[..]).len()
			}).unwrap_or(0),
			acc: 0
		}
	}

	fn to_next(&self, len: usize) -> usize {
		let mut offset = 1usize;

		let check = self.token + len;
		if check >= self.source.len() { return 0 }

		let check = check.checked_add(offset).unwrap();
		while !self.source.is_char_boundary(check) {
			offset += 1;
		}

		offset
	}
}
impl<'a> Driver for StrDriver<'a> {
	type Accumulator = str;
	type Candidate = char;

	fn accumulated(&self) -> &Self::Accumulator {
		&self.source[self.token..self.token + self.acc]
	}
	fn candidate(&self) -> Option<&Self::Candidate> {
		self.candidate.as_ref()
	}
	fn pull(&mut self) -> bool {
		if self.acc >= self.source.len() {
			/* We can't produce any more data. */
			return false
		}

		self.len += self.to_next(self.len);
		self.acc += self.to_next(self.acc);

		let _ = self.chars.next();
		self.candidate = self.chars.peek().cloned();

		true
	}
	fn consume(&mut self) {
		self.token += self.acc;

		self.len = self.candidate.map(|char| {
			let mut buffer = [0; 6];
			char.encode_utf8(&mut buffer[..]).len()
		}).unwrap_or(0);
		self.acc = 0;
	}
	fn rollback(&mut self) {
		self.acc = 0;
		self.len = 0;
	}
}
impl<'a> RefDriver<'a> for StrDriver<'a> {
	fn ref_accumulated(&self) -> &'a Self::Accumulator {
		eprintln!("{}..{}", self.token, self.token + self.acc);
		&self.source[self.token..self.token + self.acc]
	}
}
