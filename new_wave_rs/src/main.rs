
use std::collections::HashMap;
use std::io::{stdin, Write};
use std::fmt;

/**
 * NEW WAVE
 * A Concatenative language interpreter.
 */

type Operand = f64;
type Instr = u64;

// Grows downard - leaves lower numbers for user defined functions.
const PUSH:	Instr	= 0xffffffffffffff_ff;
const DUP:	Instr	= 0xffffffffffffff_fe;
const ADD: 	Instr	= 0xffffffffffffff_fd;
const SUB: 	Instr	= 0xffffffffffffff_fc;
const MUL: 	Instr	= 0xffffffffffffff_fb;
const DIV: 	Instr	= 0xffffffffffffff_fa;
const RET: 	Instr	= 0xffffffffffffff_f9;
const HALT: Instr	= 0xffffffffffffff_f8;

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

	fn exec(&mut self, instructions: &[u64]) {
		let mut ip = Frame::new(instructions);
		let mut rs: Vec<Frame> = vec![];

		macro_rules! bin_op {
		    ($op:tt) => {
		    	*self.ds.last_mut().unwrap() $op self.ds.pop().unwrap()
			}
		}

		/* No sanity checking is done here, it's the job of the interpreter to provide correct instructions */
		loop {
			let op_code = ip.next();

			match op_code {
				PUSH => self.ds.push(ip.next_operand()),
				DUP => self.ds.push(self.ds.last().cloned().unwrap()),
				ADD => bin_op!(+=),
				SUB => bin_op!(-=),
				MUL => bin_op!(*=),
				DIV => bin_op!(/=),
				RET => ip = rs.pop().unwrap(),
				HALT => break,

				// Lower numbers are implicitly calls
				addr => {
					rs.push(ip);
					ip = Frame::new(&self.memory[addr as usize..]);
				}
			};
		}
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

#[derive(Clone, Copy, Debug, PartialEq)]
struct TypeSig(u8, u8);

impl TypeSig {
	fn compose(self, other: Self) -> Self {
		let diff = self.1 as i16 - other.0 as i16;

		if diff == 0 {
			TypeSig(self.0, other.1)
		} else if 0 > diff {
			TypeSig(self.0 + diff.abs() as u8, other.1)
		} else {
			TypeSig(self.0, other.1 + diff as u8)
		}
	}
}

impl std::fmt::Display for TypeSig {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f, 
			"{} -> {}", 
			vec!["num"; self.0 as usize].join(" "), 
			vec!["num"; self.1 as usize].join(" "))
	}
}

#[derive(Clone, Copy)]
struct Word {
	instr: Instr,
	type_sig: TypeSig
}

struct WordBuf {
	instrs: Vec<Instr>,
	type_sig: TypeSig
}

impl WordBuf {
	fn new() -> Self {
		Self { instrs: vec![], type_sig: TypeSig(0, 0) }
	}

	fn push(&mut self, word: Word) {
		self.instrs.push(word.instr);
		self.type_sig = self.type_sig.compose(word.type_sig);
	}

	fn push_literal(&mut self, operand: Operand) {
		self.instrs.extend([PUSH, f64::to_bits(operand)]);
		self.type_sig = self.type_sig.compose(TypeSig(0, 1));
	}

	fn write_to_memory(&mut self, vm: &mut VM) -> Word {
		let type_sig = self.type_sig.clone(); // TODO: is `clone` needed? 
		self.instrs.push(RET);
		self.type_sig = TypeSig(0, 0);
		let instr = vm.write(self.instrs.drain(..));
		Word { instr, type_sig }
	}

	fn execute_at_runtime(&mut self, vm: &mut VM) -> Result<(), String> {
		if self.type_sig.0 > vm.ds.len() as u8 {
			Err("stack underflow\n".to_string())
		} else {
			self.instrs.push(HALT);
			vm.exec(&self.instrs);
			Ok(())
		}
	}
}

impl Word {
	fn new(instr: Instr, type_sig: TypeSig) -> Self {
		Self { instr, type_sig }
	}
}

type Env = HashMap<String, Word>;

struct Interpreter {
	vm: VM,
	env: Env,
}

impl Interpreter {
	fn new() -> Self {
		let env = HashMap::from([
			("dup", DUP, (1, 2)),
			("+", 	ADD, (2, 1)),
			("-", 	SUB, (2, 1)),
			("*", 	MUL, (2, 1)),
			("/", 	DIV, (2, 1))
		].map(|(id, instr, (inputs, outputs))| {
			(id.to_string(), Word::new(instr, TypeSig(inputs, outputs)))
		}));

		Self { vm: VM::new(), env }
	}

	fn inner_eval(&mut self, input: &str) -> Result<(), String> {
		let mut exec_buf = WordBuf::new();
		let mut def_buf = WordBuf::new();
		let mut word_buf: &mut WordBuf = &mut exec_buf;

		let mut name = "";
		// let mut def_output_buffer = vec![];


		let mut lexemes = input.split_whitespace();

		while let Some(lexeme) = lexemes.next() {
			if lexeme == ":" {
				lexemes.next().map(|new_name| {
					name = new_name
				});
				word_buf = &mut def_buf;
			} else if lexeme == ";" {
				let word = word_buf.write_to_memory(&mut self.vm);
				self.env.insert(name.to_string(), word);
				word_buf = &mut exec_buf;
			} else if let Some(&word) = self.env.get(lexeme) {
				word_buf.push(word);
			} else if let Ok(operand) = lexeme.parse::<Operand>() {
				word_buf.push_literal(operand);
			} else {
				return Err("lex error".to_string())
			}
		}
	
		word_buf.execute_at_runtime(&mut self.vm)
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
	use crate::{Interpreter, TypeSig};

    #[test]
    fn two_constants() {
    	assert_eq!(Interpreter::new().eval("42 30"), "42 30 : num num\n");
    }

    #[test]
    fn dup_mul() {
    	assert_eq!(TypeSig(1, 2).compose(TypeSig(2, 1)), TypeSig(1, 1))
    }

    #[test]
    fn dup_dup() {
    	assert_eq!(TypeSig(1, 2).compose(TypeSig(1, 2)), TypeSig(1, 3))
    }

    #[test]
    fn mul_mul() {
    	assert_eq!(TypeSig(2, 1).compose(TypeSig(2, 1)), TypeSig(3, 1))
    }

    #[test]
    fn swap_div() {
    	assert_eq!(TypeSig(2, 2).compose(TypeSig(2, 1)), TypeSig(2, 1))
    }

    #[test]

    fn dup_dup_mul_mul() {
    	let actual = TypeSig(1, 2)
    		.compose(TypeSig(1, 2))
    		.compose(TypeSig(2, 1))
    		.compose(TypeSig(2, 1));

    	assert_eq!(actual, TypeSig(1, 1))
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
