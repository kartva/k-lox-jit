use std::ops;

#[derive(Debug, Clone, Copy)]
pub struct Value (pub i64);

impl ops::Add for Value {
	type Output = Self;

	fn add(self, rhs: Self) -> Self {
		Value(self.0 + rhs.0)
	}
}

impl ops::Sub for Value {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self {
		Value(self.0 - rhs.0)
	}
}

impl ops::Mul for Value {
	type Output = Self;

	fn mul(self, rhs: Self) -> Self {
		Value(self.0 * rhs.0)
	}
}

impl ops::Div for Value {
	type Output = Self;

	fn div(self, rhs: Self) -> Self {
		Value(self.0 / rhs.0)
	}
}