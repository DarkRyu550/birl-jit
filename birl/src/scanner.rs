
pub struct Scanner<D> {
	driver: D,
}
impl<D> Scanner<D>
	where D: Driver {

	fn scan(
		&mut self,
		mut functor: impl FnMut(&D::Partial, D::Candidate) -> ScanOp) {

	}
}

pub trait Driver {
	type Partial;
	type Owned;
	type Candidate;

	fn partial(&self) -> &Self::Partial;
	fn consume(&mut self) -> Self::Owned;
}

pub struct StringScan<'a>(&'a str);
impl<'a> Driver for StringScan<'a> {
	type Partial = str;
	type Candidate = char;

	fn partial(&self) -> &Self::Partial {
		todo!()
	}
	fn consume(&mut self) -> {
		todo!()
	}
}

pub struct IterScan<T, I>(I)
