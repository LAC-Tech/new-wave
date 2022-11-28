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
#[derive(Clone, Copy)]
enum OpCode {
	Push,
	
	Add, Sub, Mul, Div,

	End
}

struct Chunk {
	operands: [Operand; 14], 	// 14 * 8 bytes = 112 bytes
	op_codes: [OpCode; 16],		// + 16 bytes = 128 bytes
}

impl Chunk {
	fn new() -> Self { 
		Self {
			operands: [f64::NAN; 14], 
			op_codes: [OpCode::End; 16]
		}
	}
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
		let mut operands = chunk.operands.iter();

		while let Some(op_code) = op_codes.next() {
			match op_code {
				OpCode::Push => match operands.next() {
					Some(operand) => self.ds.push(*operand),
					None => return Err("expected operand".to_string())
				},
				OpCode::Add => self.bin_op(Operand::add_assign)?,
				OpCode::Sub => self.bin_op(Operand::sub_assign)?,
				OpCode::Div => self.bin_op(Operand::div_assign)?,
				OpCode::Mul => self.bin_op(Operand::mul_assign)?,
				OpCode::End => break
			}
		}

		Ok(())
	}
}

impl std::fmt::Display for VM {
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

struct TopLevel {
	vm: VM
}

impl TopLevel {
	fn new() -> Self { 
		Self { vm: VM::new() }
	}

	// TODO: assumes everything fits in one chunk
	fn parse(input: &str) -> Result<Chunk, String> {
		let mut result = Chunk::new();

		let mut op_code_index = 0;
		let mut operand_index = 0;

		let mut push_op_code = |op_code: OpCode| {
			result.op_codes[op_code_index] = op_code;
			op_code_index += 1;
		};

		let mut push_operand = |operand: Operand| {
			result.operands[operand_index] = operand;
			operand_index += 1;
		};

		for token in input.split_whitespace() {
			match token {
				"+" => push_op_code(OpCode::Add),
				"-" => push_op_code(OpCode::Sub),
				"*" => push_op_code(OpCode::Mul),
				"/" => push_op_code(OpCode::Div),
				literal => {
					if let Ok(n) = literal.parse::<Operand>() {
						push_op_code(OpCode::Push);
						push_operand(n);
					} else {
						return Err(format!("{} is not a number\n", literal));
					}
				}
			}
		}

		Ok(result)
	}

	fn eval(&mut self, input: &str) -> String {
		Self::parse(input)
			.and_then(|chunk| self.vm.exec(&chunk))
			.map_or_else(|err| err, |_| self.vm.to_string())
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
