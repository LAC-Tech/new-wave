use std::ops::{Add, Sub};

struct TopLevel {
	ds: Vec<u64>
}

impl TopLevel {
	fn create() -> Self { 
		Self { ds: vec![] }
	}

	fn bin_op<Op: Fn(u64, u64) -> u64>(&mut self, op: Op) 
	-> Result<(), String> {
		match (self.ds.pop(), self.ds.pop()) {
			(Some(x), Some(y)) => Ok(self.ds.push(op(x, y))),
			_ => Err("stack underflow".to_string())
		}
	}

	fn exec_token(&mut self, token: &str) -> Result<(), String> {
		match token {
			"+" => self.bin_op(u64::add),
			"_" => self.bin_op(u64::sub),
			t => match t.parse::<u64>() {
				Ok(n) => Ok(self.ds.push(n)),
				Err(_) => Err(format!("{} is not a number", t))
			}
		}
	}

	fn exec_tokens<'a>(&mut self, mut tokens:impl std::iter::Iterator<Item = &'a str>)
	-> Result<(), String> {
		tokens.try_for_each(|t| self.exec_token(t))
	}

	fn eval(s: &str) -> String {
		s
			.split_whitespace()
			.try_for_each(|t| self.exec_token(t))
			.map(|_| "")
	}
}

fn main() {

}
