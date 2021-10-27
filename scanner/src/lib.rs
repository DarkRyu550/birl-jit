/// Driver trait and standard driver implementations.
mod drivers;
pub use drivers::*;

/// The Scanner.
///
/// A scanner is a structure responsible for scanning over some input data and
/// producing output data. It can be through of as a driver for a parser
/// combinator-like parsing solution, except for powerful in the sense that it
/// can easily handle things like iterators, in addition to the standard slice
/// data types combinators usually operate on, with no need for a distinction
/// between streaming and complete parsing.
///
/// This does come with a tradeoff, however. Where a standard parser combinator
/// can let the combinators themselves drive the consumption of data, this style
/// of parsing cannot. All consumption of data is driven by this structure. This
/// may incur increased runtime cost, depending on how complex your parse
/// functor is.
///
/// # Scans and Reference Scans
/// This structure implements two flavors of input stream scanning, referred to
/// by "scanning" and "reference scanning", that differ on how lenient they are
/// when it comes to what the scanning functor may do with the reference to the
/// accumulator at any given call.
///
/// During regular scans, the reference to the accumulator is only guaranteed to
/// be valid during the functor call. This leads to the restriction that
/// functors driving regular scans may only accept with values whose lifetime is
/// not bound to that of the accumulator.
///
/// In reference scans, the reference to the accumulator has its lifetime
/// extended to match that of the driver and, thus that of the scanner
/// structure. This means that, when accepting a value, the functor driving a
/// reference scan may, in addition to all types that may be accepted by the
/// functor driving a regular scan, accept types whose lifetimes depend on that
/// of the reference to the accumulator.
///
/// Naturally, not every driver can guarantee this relatively lax property
/// applies to its references and so, when running scanners with drivers that
/// are dot not, reference scanning is unavailable.
///
/// Regular scanning is provided by means of the [`scan()`] function, while
/// reference scanning is provided by means of the [`ref_scan()`] function.
///
/// [`ref_scan()`]: Scanner::ref_scan
/// [`scan()`]: Scanner::scan
#[derive(Debug)]
pub struct Scanner<D>(D);
impl<'a> Scanner<StrDriver<'a>> {
	/// Creates a new scanner over the given source string slice.
	pub fn from_str(source: &'a str) -> Self {
		Self(StrDriver::new(source))
	}
}
impl<D> Scanner<D>
	where D: Driver {

	/// Scans over the token stream, starting at the current position of the
	/// cursor, and applies the given functor at each step to try and produce
	/// a value.
	///
	/// The semantics of this operation are very close to those of a parser
	/// combinator, except that, instead of having the functor itself drive the
	/// cursor, it has this function do it, while consulting the functor at each
	/// steep to whether the input is complete.
	///
	/// This is the regular scanning variant of this function. For the reference
	/// scanning variant, see the [`ref_scan()`] function.
	///
	/// [`ref_scan()`]: Scanner::ref_scan
	pub fn scan<T, S, E>(
		&mut self,
		mut functor: impl FnMut(&D::Accumulator, Option<&D::Candidate>) -> ScanOp<T, S, E>)
		-> Scan<D, T, S, E> {

		loop {
			match (functor)(self.0.accumulated(), self.0.candidate()) {
				ScanOp::Continue => {
					/* The function has not made a decision yet, try again. */
					if !self.0.pull() {
						/* We can't grow our token further, seeing as we're past
						 * the end of the input string. We should bail out right
						 * away. */
						return Scan::Unfruitful
					}
				},
				ScanOp::Accept(value) =>
					/* The current accumulated string has been accepted. */
					return Scan::Accepted(Accepted {
						stream: self,
						value
					}),
				ScanOp::Drop(what) =>
					/* The current accumulated string has dropped out. */
					return Scan::Dropped(what),
				ScanOp::Reject(what) =>
					/* The current accumulated string has been rejected. */
					return Scan::Rejected(what),
			}
		}
	}
}
impl<'a, D> Scanner<D>
	where D: RefDriver<'a> {

	/// Scans over the token stream, starting at the current position of the
	/// cursor, and applies the given functor at each step to try and produce
	/// a value.
	///
	/// The semantics of this operation are very close to those of a parser
	/// combinator, except that, instead of having the functor itself drive the
	/// cursor, it has this function do it, while consulting the functor at each
	/// steep to whether the input is complete.
	///
	/// This is the reference scanning variant of this function. For the regular
	/// scanning one, see the [`scan()`] function.
	///
	/// [`scan()`]: Scanner::scan
	pub fn ref_scan<T, S, E>(
		&mut self,
		mut functor: impl FnMut(&'a D::Accumulator, Option<&D::Candidate>) -> ScanOp<T, S, E>)
		-> Scan<D, T, S, E>
		where <D as Driver>::Accumulator: 'a {

		loop {
			match (functor)(self.0.ref_accumulated(), self.0.candidate()) {
				ScanOp::Continue => {
					/* The function has not made a decision yet, try again. */
					if !self.0.pull() {
						/* We can't grow our token further, seeing as we're past
						 * the end of the input string. We should bail out right
						 * away. */
						return Scan::Unfruitful
					}
				},
				ScanOp::Accept(value) =>
					/* The current accumulated string has been accepted. */
					return Scan::Accepted(Accepted {
						stream: self,
						value
					}),
				ScanOp::Drop(what) =>
					/* The current accumulated string has dropped out. */
					return Scan::Dropped(what),
				ScanOp::Reject(what) =>
					/* The current accumulated string has been rejected. */
					return Scan::Rejected(what),
			}
		}
	}
}

/// Candidate operations to be applied during scans.
///
/// Throughout a token scan operation, it is often desirable to have more
/// than a binary acceptation or undiagnosed rejection of items. It may be
/// also be desirable for a scanning operator to give a motive for the rejection
/// of an item, as well as for it to to ask for more items before deciding.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ScanOp<T, D, E> {
	/// Accept the input with the given value.
	Accept(T),
	/// Reject thi input with the given error while also indicating that, if the
	/// scan can continue through other path, it should.
	Drop(D),
	/// Reject the input with the given error while also indicating that the
	/// scan should stop regardless of whether it could have continued through
	/// another path.
	Reject(E),
	/// Append the candidate item to the accumulated value and try again.
	Continue,
}

/// The results of a scanning operation over a token stream.
#[derive(Debug)]
pub enum Scan<'a, D, T, S, E> {
	/// The scan was successful and the functor has accepted a string.
	Accepted(Accepted<'a, D, T>),
	/// The scan was unsuccessful, dropping from its top level path.
	Dropped(S),
	/// The scan was unsuccessful, erring out from its top level path.
	Rejected(E),
	/// The scan was unsuccessful, having gotten to the end of the stream before
	/// reaching any conclusion as to what to do with the data it has seen.
	Unfruitful
}
impl<'a, D, T, S, E> Scan<'a, D, T, S, E> {
	/// Unwraps this scan structure and tries to produce an accepted value.
	pub fn unwrap(self) -> Accepted<'a, D, T> {
		match self {
			Scan::Accepted(accepted) => accepted,
			_ => panic!("called `unwrap()` on a non-Accepted value")
		}
	}
}

/// The result of an accepted scanning operation over a token stream.
#[derive(Debug)]
pub struct Accepted<'a, D, T> {
	/// The underlying stream instance.
	stream: &'a mut Scanner<D>,
	/// The data value generated by the acceptation of the string.
	value: T,
}
impl<'a, D, T> Accepted<'a, D, T>
	where D: Driver {

	/// The accumulated string of items that was accepted.
	///
	/// This is the regular scanning variant of this function. For the reference
	/// scanning one, see the [`ref_string()`] function.
	///
	/// [`ref_string()`]: Scan::ref_string
	pub fn string(&self) -> &D::Accumulator {
		self.stream.0.accumulated()
	}

	/// Peeks the value produced by the scanning operation.
	pub fn peek(&self) -> &T {
		&self.value
	}

	/// Discards the character string produced by this scan operation.
	pub fn rollback(self) {
		self.stream.0.rollback();
	}

	/// Consumes the selected character string scanner over by this operation
	/// and readies the scanner to take on the next token. This function also
	/// returns the value produced by the scanning operation.
	pub fn consume(self) -> T {
		self.stream.0.consume();
		self.value
	}
}
impl<'a, 'b, D, T> Accepted<'a, D, T>
	where D: RefDriver<'b> {

	/// The accumulated string of items that was accepted.
	///
	/// This is the reference scanning variant of this function, and thus, it's
	/// only of limited availability. For the regular scanning one, see the
	/// [`string()`] function.
	///
	/// [`string()`]: Scan::string
	pub fn ref_string(&self) -> &'b D::Accumulator {
		self.stream.0.ref_accumulated()
	}
}
