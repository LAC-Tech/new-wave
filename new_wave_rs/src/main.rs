
use std::collections::HashMap;
use std::io::{stdin, Write};
use std::fmt;

/**
 * NEW WAVE
 * A Concatenative language interpreter, using subroutine threading.
 */

type Operand = f64;
type Instr = u64;

const PUSH:	Instr	= 0xffffffffffffff_f8;
const DUP:	Instr	= 0xffffffffffffff_f9;
const ADD: 	Instr	= 0xffffffffffffff_fa;
const SUB: 	Instr	= 0xffffffffffffff_fb;
const MUL: 	Instr	= 0xffffffffffffff_fc;
const DIV: 	Instr	= 0xffffffffffffff_fd;
const RET: 	Instr	= 0xffffffffffffff_fe;
const HALT: Instr	= 0xffffffffffffff_ff;

struct Frame<'a> {
	pc: usize,
	instructions: &'a[u64]
}

impl<'a> Frame<'a> {
	fn new(instructions: &'a[Instr]) -> Self {
		Self { pc: 0, instructions }
	}

	fn next(&mut self) -> Instr {
		let res = self.instructions[self.pc];
		self.pc += 1;
		res
	}

	fn next_operand(&mut self) -> Operand {
		f64::from_bits(self.next())
	}
}

struct VM {
	ds: Vec<Operand>,
	memory: Vec<u64>
}

impl VM {
	fn new() -> Self {
		Self { ds: vec![], memory: vec![] }
	}

	fn write<I: IntoIterator<Item = Instr>>(&mut self, intrs: I) -> Instr {
		let result = self.memory.len();
		self.memory.extend(intrs);
		result as Instr
	}

	fn exec(&mut self, instructions: &[u64]) -> Result<(), String> {
		let mut ip = Frame::new(instructions);
		let mut rs: Vec<Frame> = vec![];

		macro_rules! bin_op {
		    ($op:tt) => {
		    	match (self.ds.pop(), self.ds.last_mut()) {
					(Some(x), Some(tos)) => Ok((*tos $op x)),
					_ => Err("stack underflow\n".to_string())
				}
			}
		}

		loop {
			let op_code = ip.next();

			match op_code {
				PUSH => {
					self.ds.push(ip.next_operand());
					Ok(())
				},
				DUP => self.ds.last().cloned()
					.map(|operand| self.ds.push(operand))
					.ok_or_else(|| "stack underflow\n".to_string()),
				ADD => bin_op!(+=),
				SUB => bin_op!(-=),
				MUL => bin_op!(*=),
				DIV => bin_op!(/=),

				RET => {
					rs.pop()
						.map(|old_ip| ip = old_ip)
						.ok_or_else(|| "empty return stack".to_string())
				},

				HALT => break,

				// Lower numbers are implicitly calls
				addr => {
					rs.push(ip);
					ip = Frame::new(&self.memory[addr as usize..]);

					Ok(())
				}
			}?;
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

		writeln!(f, "{} : {}", values, types)
	}
}

type Env = HashMap<String, Instr>;

struct Interpreter {
	vm: VM,
	env: Env,
}

impl Interpreter {
	fn new() -> Self {
		let env = HashMap::from([
			("dup".to_string(), DUP),
			("+".to_string(), ADD),
			("-".to_string(), SUB),
			("*".to_string(), MUL),
			("/".to_string(), DIV)
		]);

		Self { vm: VM::new(), env }
	}

	fn inner_eval(&mut self, input: &str) -> Result<(), String> {


		let mut exec_buf = vec![];
		let mut def_buf = vec![];
		let mut instr_buf: &mut Vec<Instr> = &mut exec_buf;

		let mut name = "";

		let mut lexemes = input.split_whitespace();

		while let Some(lexeme) = lexemes.next() {
			if lexeme == ":" {
				lexemes.next().map(|new_name| {
					name = new_name
				});
				instr_buf = &mut def_buf;
			} else if lexeme == ";" {
				instr_buf.push(RET);
				let addr = self.vm.write(instr_buf.drain(..));
				self.env.insert(name.to_string(), addr);

				instr_buf = &mut exec_buf;
			} else if let Some(&instr) = self.env.get(lexeme) {
				instr_buf.push(instr);
			} else if let Ok(literal) = lexeme.parse::<Operand>() {
				instr_buf.extend([PUSH, f64::to_bits(literal)]);
			} else {
				return Err("lex error".to_string())
			}
		}
	
		instr_buf.push(HALT);
		self.vm.exec(&instr_buf)
	}

	fn eval(&mut self, input: &str) -> String {
		self.inner_eval(input)
			.map_or_else(|err| err,  |_| self.vm.to_string())
	}
}

fn main() -> Result<(), std::io::Error> {
	let mut top_level = Interpreter::new();
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
    use crate::Interpreter;

    #[test]
    fn stack_underflow() {
        assert_eq!(Interpreter::new().eval("1 +"), "stack underflow\n");
    }
}

#[cfg(test)]
mod type_inference {
	use crate::Interpreter;

    #[test]
    fn two_constants() {
    	assert_eq!(Interpreter::new().eval("42 30"), "42 30 : num num\n");
    }
}

#[cfg(test)]
mod sicp_examples {
	use crate::Interpreter;

	#[test]
	fn eval_empty_input() {
		assert_eq!(Interpreter::new().eval(""), "");
	}

	#[test]
	fn primitive_expression() {
		assert_eq!(Interpreter::new().eval("486"), "486 : num\n");
	}

	#[test]
	fn add_ints() {
		assert_eq!(Interpreter::new().eval("137 349 +"), "486 : num\n");
	}

	#[test]
	fn subtract_ints() {
		assert_eq!(Interpreter::new().eval("1000 334 -"), "666 : num\n");
	}

	#[test]
	fn divide_ints() {
		assert_eq!(Interpreter::new().eval("10 5 /"), "2 : num\n");	
	}

	#[test]
	fn add_real_to_int() {
		assert_eq!(Interpreter::new().eval("2.7 10 +"), "12.7 : num\n");		
	}

	#[test]
	fn add_multiple_ints() {
		assert_eq!(Interpreter::new().eval("21 35 + 12 + 7 +"), "75 : num\n");
	}

	#[test]
	fn multiply_multiple_ints() {
		assert_eq!(Interpreter::new().eval("25 4 * 12 *"), "1200 : num\n");
	}

	#[test]
	fn nested_combinations() {
		assert_eq!(Interpreter::new().eval("3 5 * 10 6 - +"), "19 : num\n");
	}

	#[test]
	fn relatively_simple_expressions() {
		assert_eq!(
			Interpreter::new().eval("3 2 4 * 3 5 + + * 10 7 - 6 + +"), 
			"57 : num\n"
		);
	}
}
