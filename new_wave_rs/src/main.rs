use std::collections::HashMap;
use std::io::{stdin, Write};
use std::ops::{AddAssign, SubAssign, MulAssign, DivAssign};
use std::fmt;

/**
 * NEW WAVE
 * A Concatenative language bytecode interpreter.
 */

/** TODO: union type, including integers. Try and keep it 64 bits. */
type Operand = f64;
type OpCode = u64;

type Instruction = fn(
	&mut VM,
	ip: &mut std::vec::IntoIter<OpCode>
) -> Result<(), String>;

struct Primitive (OpCode, Instruction);

const PUSH: Primitive = Primitive(0x0, |vm, ip| {
	ip.next()
		.map(|literal| vm.ds.push(f64::from_bits(literal as u64)))
		.ok_or("stack underflow".to_string())
});

const ADD: Primitive =
	Primitive(0x10, |vm, _| VM::bin_op(vm, Operand::add_assign));
const SUB: Primitive =
	Primitive(0x11, |vm, _| VM::bin_op(vm, Operand::sub_assign));
const MUL: Primitive = 
	Primitive(0x12, |vm, _| VM::bin_op(vm, Operand::mul_assign));
const DIV: Primitive =
	Primitive(0x13, |vm, _| VM::bin_op(vm, Operand::div_assign));

struct VM {
	ds: Vec<Operand>,
	memory: Vec<Instruction>
}

impl VM {
	fn new() -> Self {
		let ds = vec![];
		let mut memory: Vec<Instruction> = vec![|_, _| Ok (()); 16];

		for p in &[PUSH, ADD, SUB, MUL, DIV] {
			memory.insert(p.0 as usize, p.1);
		}

		Self { ds, memory }
	}

	fn bin_op<Op: for<'r> Fn(&'r mut Operand, Operand) -> ()>(
		&mut self, 	op: Op
	) -> Result<(), String> {
		match (self.ds.pop(), self.ds.last_mut()) {
			(Some(x), Some(tos)) => Ok(op(tos, x)),
			_ => Err("stack underflow\n".to_string())
		}
	}

	fn exec(&mut self, addresses: Vec<OpCode>) -> Result<(), String> {
		let mut as_iter = addresses.into_iter();

		while let Some(address) = as_iter.next() {
			self.memory[address as usize](self, &mut as_iter)?;
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
	vm: VM,
	env: HashMap<String, OpCode>
}

impl TopLevel {
	fn new() -> Self {
		let vm = VM::new();
		let env = HashMap::from([
			("+", ADD),
			("-", SUB),
			("*", MUL),
			("/", DIV),
		].map(|(name, primitive)| (name.to_string(), primitive.0)));

		Self { vm, env }
	}

	// TODO: assumes everything fits in one chunk
	fn parse(&self, input: &str) -> Result<Vec<OpCode>, String> {
		let mut result: Vec<OpCode> = vec![];

		for token in input.split_whitespace() {
			match self.env.get(token) {
				Some(&word) => result.push(word),
				None => {
					if let Ok(n) = token.parse::<Operand>() {
						result.extend_from_slice(&[PUSH.0, f64::to_bits(n)]);
					} else {
						return Err(format!("{} is not a number\n", token));
					}
				}
			}
		}

		Ok(result)
	}

	fn eval(&mut self, input: &str) -> String {
		self.parse(input)
			.and_then(|iv| self.vm.exec(iv))
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
    fn floating() {
    	assert_eq!(f64::from_bits(f64::to_bits(12.7)), 12.7);
    }

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
