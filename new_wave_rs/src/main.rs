use std::collections::HashMap;
use std::io::{stdin, Write};
use std::fmt;

/**
 * NEW WAVE
 * A Concatenative language interpreter, using subroutine threading.
 */

type Operand = f64;
type Instr = u64;

const PUSH:	Instr	= 0xffffffffffffff_9f;
const ADD: 	Instr	= 0xffffffffffffff_af;
const SUB: 	Instr	= 0xffffffffffffff_bf;
const MUL: 	Instr	= 0xffffffffffffff_cf;
const DIV: 	Instr	= 0xffffffffffffff_df;
const RET: 	Instr	= 0xffffffffffffff_ef;
const HALT: Instr	= 0xffffffffffffff_ff;

struct Frame<'a> {
	pc: usize,
	instructions: &'a[u64]
}

impl<'a> Frame<'a> {
	fn new(instructions: &'a[Instr]) -> Self {
		Self { pc: 0, instructions }
	}

	fn next_index(&mut self) -> usize {
		let res = self.instructions[self.pc] as usize;
		self.pc += 1;
		res
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
				}
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

enum ExecToken {
	Subroutine(Instr),
	Literal(Operand),
	// CompTime(&'a str) // immediate in forth, parsing word in factor
}

// TODO: useless? fold back into eval?
struct InstrBuf {
	buf: Vec<Instr>,
	sentinel: Instr
}

impl InstrBuf {
	fn new(sentinel: Instr) -> Self {
		Self {buf: vec![], sentinel }
	}

	fn push(&mut self, token: ExecToken) {
		match token {
			ExecToken::Subroutine(op_code) => {
				self.buf.push(op_code);
			},
			ExecToken::Literal(literal) => {	
				self.buf.push(PUSH);
				self.buf.push(f64::to_bits(literal));
			}
		}
	}

	// TODO: apply optimisations here
	fn finish(&mut self) -> &[Instr] {
		self.buf.push(self.sentinel);
		&self.buf
	}
}

struct Env(HashMap<String, Instr>);

impl Env {
	fn new() -> Self {
		let table = HashMap::from([
			("+".to_string(), ADD),
			("-".to_string(), SUB),
			("*".to_string(), MUL),
			("/".to_string(), DIV)
		]);

		Self(table)
	}

	fn parse(&self, lexeme: &str) -> Result<ExecToken, String> {
		if let Some(&word) = self.0.get(lexeme) {
			Ok(ExecToken::Subroutine(word))
		} else if let Ok(n) = lexeme.parse::<Operand>() {
			Ok(ExecToken::Literal(n))
		} else {
			Err(format!("{} is not a number\n", lexeme))
		}
	}
}

struct Interpreter {
	vm: VM,
	env: Env,
}

impl Interpreter {
	fn new() -> Self {
		Self { vm: VM::new(), env: Env::new() }
	}

	fn parse(&self, input: &str) -> Result<Vec<ExecToken>, String> {
		let lexemes = input.split_whitespace();
		lexemes.map(|lexeme| self.env.parse(lexeme)).collect()
	}

	fn eval(&mut self, input: &str) -> String {
		let mut buf = InstrBuf::new(HALT);

		self.parse(input)
			.map(|tokens| tokens.into_iter().for_each(|token| buf.push(token)))
			.and_then(|_| {
				self.vm.exec(buf.finish())?;
				Ok(())
			})
			.map_or_else(|err| err, |_| self.vm.to_string())
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
