use std::io::{stdin, Write};
use std::ops::{Add, Sub, Div, Mul};
use std::fmt;

type Word = f64;

struct TopLevel {
	ds: Vec<Word>
}

impl TopLevel {
	fn new() -> Self { 
		Self { ds: vec![] }
	}

	fn bin_op<Op: Fn(Word, Word) -> Word>(
		&mut self, 
		op: Op
	) -> Result<(), String> {
		// TODO: benchmark if it's quicker to pop once and modify TOS directly
		match (self.ds.pop(), self.ds.pop()) {
			(Some(y), Some(x)) => Ok(self.ds.push(op(x, y))),
			_ => Err("stack underflow\n".to_string())
		}
	}

	fn exec_token(&mut self, token: &str) -> Result<(), String> {
		match token {
			"+" => self.bin_op(Word::add),
			"-" => self.bin_op(Word::sub),
			"/" => self.bin_op(Word::div),
			"*" => self.bin_op(Word::mul),
			t => t.parse::<Word>()
				.map(|n| self.ds.push(n))
				.map_err(|_| format!("{} is not a number\n", t))
		}
	}

	fn eval(&mut self, input: &str) -> String {
		input
			.split_whitespace()
			.try_for_each(|t| self.exec_token(t))
			.map_or_else(|err| err, |_| self.to_string())

	}
}

impl std::fmt::Display for TopLevel {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		if self.ds.is_empty() {
			return write!(f, "");
		}

		let values = self.ds.iter()
			.map(|d| d.to_string())
			.collect::<Vec<String>>()
			.join(" ");

		let types = vec!["num"; self.ds.len()].join(" ");

		write!(f, "{} : {}\n", values, types)
	}
}

fn main() -> Result<(), std::io::Error> {
	let mut top_level = TopLevel::new();
	let mut input = String::new();

	loop {
		print!("> ");
		std::io::stdout().flush()?;
		stdin().read_line(&mut input)?;
		print!("{}", top_level.eval(&input));
		input.clear();
	}
}



#[cfg(test)]
mod errors {
    use crate::TopLevel;

    #[test]
    fn stack_underflow() {
        assert_eq!(TopLevel::new().eval("1 +"), "stack underflow\n");
    }
}

#[cfg(test)]
mod type_inference {
	use crate::TopLevel;

    #[test]
    fn two_constants() {
    	assert_eq!(TopLevel::new().eval("42 30"), "42 30 : num num\n");
    }
}

#[cfg(test)]
mod sicp_examples {
	use crate::TopLevel;

	#[test]
	fn eval_empty_input() {
		assert_eq!(TopLevel::new().eval(""), "");
	}

	#[test]
	fn primitive_expression() {
		assert_eq!(TopLevel::new().eval("486"), "486 : num\n");
	}

	#[test]
	fn add_ints() {
		assert_eq!(TopLevel::new().eval("137 349 +"), "486 : num\n");
	}

	#[test]
	fn subtract_ints() {
		assert_eq!(TopLevel::new().eval("1000 334 -"), "666 : num\n");
	}

	#[test]
	fn divide_ints() {
		assert_eq!(TopLevel::new().eval("10 5 /"), "2 : num\n");	
	}

	#[test]
	fn add_real_to_int() {
		assert_eq!(TopLevel::new().eval("2.7 10 +"), "12.7 : num\n");		
	}

	#[test]
	fn add_multiple_ints() {
		assert_eq!(TopLevel::new().eval("21 35 + 12 + 7 +"), "75 : num\n");
	}

	#[test]
	fn multiply_multiple_ints() {
		assert_eq!(TopLevel::new().eval("25 4 * 12 *"), "1200 : num\n");
	}

	#[test]
	fn nested_combinations() {
		assert_eq!(TopLevel::new().eval("3 5 * 10 6 - +"), "19 : num\n");
	}

	#[test]
	fn relatively_simple_expressions() {
		assert_eq!(
			TopLevel::new().eval("3 2 4 * 3 5 + + * 10 7 - 6 + +"), 
			"57 : num\n"
		);
	}
}
