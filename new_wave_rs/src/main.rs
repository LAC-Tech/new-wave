use std::collections::HashMap;
use std::io::{stdin, Write};
use std::fmt;

/**
 * NEW WAVE
 * A Concatenative language interpreter, using subroutine threading.
 */

type Operand = f64;
// cast to usize as needed - cleaner than assuming 64 bit usize
type OpCode = u64; 

type Subroutine = fn(
	&mut VM,
	ip: &mut std::slice::Iter<OpCode>
) -> Result<(), String>;

struct Instruction {
	op_code: OpCode, 
	subr: Subroutine
}

macro_rules! bin_op {
    ($op:tt) => (|vm, _| {
    	match (vm.ds.pop(), vm.ds.last_mut()) {
			(Some(x), Some(tos)) => Ok((*tos $op x)),
			_ => Err("stack underflow\n".to_string())
		}
    })
}

macro_rules! isa {
	( $(($i:ident, $op_code:literal, $subr:expr)),*) => {
		$(
			const $i: Instruction = Instruction { 
				op_code: $op_code, 
				subr: $subr
			};
		)*
	}
}

isa!(
	(PUSH, 0x0, |vm, ip| {
		ip.next()
			.map(|&literal| vm.ds.push(f64::from_bits(literal as u64)))
			.ok_or("stack underflow".to_string())
	}),
	(ADD, 0x10, bin_op!(+=)),
	(SUB, 0x11, bin_op!(-=)),
	(MUL, 0x12, bin_op!(*=)),
	(DIV, 0x13, bin_op!(/=)));

struct VM {
	ds: Vec<Operand>,
	memory: Vec<Subroutine>
}

impl VM {
	fn new() -> Self {
		let ds = vec![];
		let mut memory: Vec<Subroutine> = vec![|_, _| Ok (()); 16];

		for p in &[PUSH, ADD, SUB, MUL, DIV] {
			memory.insert(p.op_code as usize, p.subr);
		}

		Self { ds, memory }
	}

	fn exec(&mut self, addresses: &[OpCode]) -> Result<(), String> {
		let mut as_iter = addresses.into_iter();

		while let Some(&address) = as_iter.next() {
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
	env: HashMap<String, OpCode>,
}

impl TopLevel {
	fn new() -> Self {
		let vm = VM::new();
		let env = HashMap::from([
			("+", ADD),
			("-", SUB),
			("*", MUL),
			("/", DIV),
		].map(|(name, primitive)| (name.to_string(), primitive.op_code)));

		Self { vm, env }
	}

	fn parse_token(&self, op_code_buf: &mut Vec<OpCode>, token: &str) -> Result<(), String> {
		match self.env.get(token) {
			Some(&word) => Ok(op_code_buf.push(word)),
			None => {
				if let Ok(n) = token.parse::<Operand>() {
					let op_codes = &[PUSH.op_code, f64::to_bits(n)];
					op_code_buf.extend_from_slice(op_codes);
					Ok(())
				} else {
					Err(format!("{} is not a number\n", token))
				}				
			}
		}
	}

	fn eval(&mut self, input: &str) -> String {
		let mut result: Vec<OpCode> = vec![];

		input.split_whitespace()
			.try_for_each(|token| self.parse_token(&mut result, token))
			.and_then(|_| self.vm.exec(&result))
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
