/// Implementation of string-based scan drivers.
mod string;
pub use string::*;

/// The interface expected of a scan driver.
pub trait Driver {
	/// The type that accumulates candidates until a parse decision is made.
	type Accumulator: ?Sized;
	/// The type that is a candidate for accumulation.
	type Candidate: ?Sized;

	/// The string of items currently in the accumulator.
	fn accumulated(&self) -> &Self::Accumulator;
	/// A reference to the next candidate.
	fn candidate(&self) -> Option<&Self::Candidate>;
	/// Pull the next candidate in the stream into the accumulator. Returns true
	/// if there is another item in the data source, false otherwise.
	fn pull(&mut self) -> bool;
	/// Consume everything in the accumulator.
	fn consume(&mut self);
	/// Rollback to the state before the last call to `consume()`.
	fn rollback(&mut self);
}

/// The interface for a scan driver that, in addition to performing all of
/// the operations expected of a scan driver, can produce accumulator pointers
/// that live for longer than the borrow to the driver itself.
pub trait RefDriver<'a>: Driver {
	/// The string of items currently in the accumulator.
	///
	/// This variant differs from the standard [`accumulated()`] function in
	/// that it guarantees a longer lifetime to the accumulator reference. This
	/// allows for longer lived reference scans to be performed.
	fn ref_accumulated(&self) -> &'a Self::Accumulator;
}
