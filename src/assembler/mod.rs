mod pass1;
mod pass2;
mod error_handler;

use std::collections::HashMap;
use std::io::{BufRead, Cursor};

type AsmResult<T> = Result<T, AsmError>;
type DetailResult<T> = Result<T, (AsmError, usize)>;

const MEM_SIZE: usize = 65536;

#[derive(Debug, PartialEq)]
enum AsmError {
	InvalidNumberFormat,
	InvalidBinaryNumber,
	InvalidHexadecimalNumber,
	InvalidOctalNumber,
	InvalidDecimalNumber,
	InvalidLabelName,
	InvalidLabelNameOpcode,
	LabelAlreadyExists,
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
	data:	Vec<(usize, Data)>,
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
enum Data {
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
			data: Vec::new(),
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

impl Instruction {
	fn new(operation: Operation, addr_mode: AddrMode) -> Instruction {
		Instruction {
			operation: operation,
			addr_mode: addr_mode,
		}
	}
}

pub fn assemble<R: BufRead>(source: R) -> Result<Box<[u8]>, String> {
	let parsed_data = match pass1::pass1(source) {
		Ok(pd) => pd,
		Err((e, n)) => return Err(error_handler::write_error(e, n)),
	};

	let buf : Box<[u8]> = Box::new([0; MEM_SIZE]);
	let mut cursor = Cursor::new(buf);

	match pass2::pass2(&mut cursor, parsed_data) {
		Ok(()) => Ok(cursor.into_inner()),
		Err((e, n)) => return Err(error_handler::write_error(e, n)),
	}
}