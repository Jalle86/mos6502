mod pass1;
mod pass2;

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Cursor, Read, Write};

type AsmResult<T> = Result<T, AsmError>;
type Lines = Vec<Line>;

#[derive(Debug, PartialEq)]
enum AsmError {
	InvalidNumberFormat,
	InvalidBinaryNumber,
	InvalidHexadecimalNumber,
	InvalidOctalNumber,
	InvalidDecimalNumber,
	InvalidLabelName,
	LabelAlreadyExists,
	InvalidAssignment,
	InvalidOpcode,
	InvalidAddrMode,
	InvalidInstruction,
	InvalidPragma,
	IOError,
	UndefinedLabel,
	WordOverflow,
	ByteOverflow,
	BufferWriteOverflow,
	BufferWriteError,
}

#[derive(Debug, PartialEq, Clone)]
enum Operation {
	ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI, CLV, CMP, CPX,
	CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP, JSR, LDA, LDY, LDX, LSR, NOP, ORA, PHA, PHP, PLA,
	PLP, ROL, ROR, RTI, RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,
}

#[derive(Debug, PartialEq, Clone)]
enum AddrMode {
	Absolute(Operand),
	AbsoluteX(Operand),
	AbsoluteY(Operand),
	
	Accumulator,

	Immediate(Operand),

	Implied,
	
	Indirect(Operand),
	IndirectX(Operand),
	IndirectY(Operand),

	Relative(Operand),

	ZeroPage(Operand),
	ZeroPageX(Operand),
	ZeroPageY(Operand),
}

#[derive(Debug)]
struct ParsedData {
	symtab:	SymTab,
	lines:	Lines,
}

#[derive(Debug, PartialEq)]
struct Instruction {
	operation: Operation,
	addr_mode: AddrMode,
}

#[derive(Debug, PartialEq)]
enum Identifier {
	Number(usize),
	PC
}

#[derive(Debug, PartialEq, Clone)]
enum Operand {
	Label(String),
	Value(usize),
}

#[derive(Debug, PartialEq)]
enum Pragma {
	Byte(Operand),
	Word(Operand),
	End,
	LocationCounter(usize),
}

#[derive(Debug, PartialEq)]
enum Line {
	Pragma(Pragma),
	Instruction(Instruction),
}

#[derive(Debug)]
struct SymTab {
	tab:				HashMap<String, usize>,
	location_counter:	usize,
}

impl ParsedData {
	fn new() -> ParsedData {
		ParsedData {
			symtab: SymTab::new(),
			lines: Lines::new(),
		}
	}
}

impl SymTab {
	fn new() -> SymTab {
		SymTab {
			tab: HashMap::new(),
			location_counter: 0,
		}
	}

	fn get(&self, key: &str) -> Option<&usize> {
		self.tab.get(key)
	}

	fn insert(&mut self, s: String, n: usize) {
		self.tab.insert(s, n);
	}

	fn insert_at_counter(&mut self, s: String) {
		self.tab.insert(s, self.location_counter);
	}

	fn contains(&mut self, s: &str) -> bool {
		self.tab.contains_key(s)
	}

	fn set_counter(&mut self, n: usize) {
		self.location_counter = n;
	}

	fn increment_counter(&mut self, n: usize) {
		self.location_counter += n;
	}

	fn add_label(&mut self, label: String, value: usize) -> AsmResult<()> {
		if label == "*" {
			self.location_counter = value;
			Ok(())
		}
		else if !self.contains(&label) {
			self.insert(label, value);
			Ok(())
		}
		else {
			Err(AsmError::LabelAlreadyExists)
		}
	}
}

pub fn assemble(source_file: &str, output_file: &str) {
	let mut source = File::open(source_file).expect("File not found");
	let mut contents = String::new();
	source.read_to_string(&mut contents).expect("something went wrong reading the file");

	let parsed_data = match pass1::pass1(BufReader::new(source)) {
		Ok(pd) => pd,
		Err(_) => panic!(),
	};

	let buf : &mut [u8] = &mut [0; 65536];
	let mut cursor = Cursor::new(buf);

	match pass2::pass2(&mut cursor, parsed_data) {
		Ok(_) => println!("ok :D"),
		Err(_) => panic!(),
	};

	File::create(output_file)
		.expect("Could not create file")
		.write(cursor.get_ref())
		.expect("Could not write to file");
}