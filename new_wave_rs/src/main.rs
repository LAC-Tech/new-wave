use std::io::{stdin, Write};
use std::ops::{AddAssign, SubAssign, MulAssign, DivAssign};
use std::fmt;

/**
 * NEW WAVE
 * A Concatenative language bytecode interpreter.
 */

/** TODO: union type, including integers. Try and keep it 64 bits. */
type Operand = f64;

/** I'm hoping I can fit this all in 8 bits. */
#[repr(u8)]
enum OpCode {
	// Push,
	
	Add, Sub, Mul, Div
}

struct Chunk {
	operands: [Operand; 15], 	// 15 * 8 bytes = 120 bytes
	op_codes: [OpCode; 8],		// + 8 bytes = 128 bytes
}

struct VM {
	ds: Vec<Operand>
}

impl VM {
	fn new() -> Self {
		Self { ds: vec![] }
	}

	fn bin_op<Op: for<'r> Fn(&'r mut Operand, Operand) -> ()>(
		&mut self, 	op: Op
	) -> Result<(), String> {
		match (self.ds.pop(), self.ds.last_mut()) {
			(Some(x), Some(tos)) => Ok(op(tos, x)),
			_ => Err("stack underflow\n".to_string())
		}
	}

	fn exec(&mut self, chunk: &Chunk) -> Result<(), String> {
		let mut op_codes = chunk.op_codes.iter();

		while let Some(op_code) = op_codes.next() {
			match op_code {
				OpCode::Add => self.bin_op(Operand::add_assign)?,
				OpCode::Sub => self.bin_op(Operand::sub_assign)?,
				OpCode::Div => self.bin_op(Operand::div_assign)?,
				OpCode::Mul => self.bin_op(Operand::mul_assign)?,
			}
		}

		Ok(())
	}
}

struct TopLevel {
	ds: Vec<Operand>
}

impl TopLevel {
	fn new() -> Self { 
		Self { ds: vec![] }
	}

	fn bin_op<Op: for<'r> Fn(&'r mut Operand, Operand) -> ()>(
		&mut self, 	
	op: Op
	) -> Result<(), String> {
		match (self.ds.pop(), self.ds.last_mut()) {
			(Some(x), Some(tos)) => Ok(op(tos, x)), //Ok(self.ds.push(op(x, y))),
			_ => Err("stack underflow\n".to_string())
		}
	}

	fn exec_token(&mut self, token: &str) -> Result<(), String> {
		match token {
			"+" => self.bin_op(Operand::add_assign),
			"-" => self.bin_op(Operand::sub_assign),
			"/" => self.bin_op(Operand::div_assign),
			"*" => self.bin_op(Operand::mul_assign),
			t => t.parse::<Operand>()
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
