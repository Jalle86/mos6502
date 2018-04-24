extern crate regex;

use std::collections::HashMap;
use self::regex::*;
use std::io::{BufRead, Write};

type AsmResult<T> = Result<T, AsmError>;
type Lines = Vec<Line>;
type Bytes = Vec<u8>;

static LABEL_REGEX: &'static str = r"^(?P<label>([A-Z][A-Z0-9]*)|\*)$";
static NUM_REGEX: &'static str = r"^P<num>[\$%O]?[0-9A-F]+";

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

#[derive(Debug, PartialEq)]
enum Operation {
	ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI, CLV, CMP, CPX,
	CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP, JSR, LDA, LDY, LDX, LSR, NOP, ORA, PHA, PHP, PLA,
	PLP, ROL, ROR, RTI, RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,
}

#[derive(Debug, PartialEq)]
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
struct LabelAssignment {
	lvalue: String,
	rvalue: Identifier,
}

#[derive(Debug, PartialEq)]
enum Identifier {
	Number(usize),
	PC
}

#[derive(Debug, PartialEq)]
enum Operand {
	Label(String),
	Value(usize),
}

#[derive(Debug, PartialEq)]
enum Pragma {
	Byte(Operand),
	Word(Operand),
	End,
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

fn pass2<W: Write>(writer: &mut W, parsed_data: ParsedData) -> AsmResult<()> {
	for line in parsed_data.lines {
		write_line(writer, line, &parsed_data.symtab)?;
	}

	Ok(())
}

fn write_line<W: Write>(writer: &mut W, line: Line, symtab: &SymTab) -> AsmResult<()> {
	match line {
		Line::Instruction(instruction)	=> write_instruction(writer, instruction, symtab),
		Line::Pragma(pragma)			=> write_pragma(writer, pragma, symtab),
	}
}

fn write_pragma<W: Write>(writer: &mut W, pragma: Pragma, symtab: &SymTab) -> AsmResult<()> {
	match pragma {
		Pragma::Byte(op) => write_byte(writer, expect_byte(op, symtab)?),
		Pragma::Word(op) => write_word(writer, expect_word(op, symtab)?),
		_ => panic!(),
	}
}

fn write_instruction<W: Write>(writer: &mut W, instruction: Instruction, symtab: &SymTab)
	-> AsmResult<()> {
	
	write_opcode(writer, &instruction)?;
	write_addr_mode(writer, instruction.addr_mode, symtab)?;
	Ok(())
}

fn write_opcode<W: Write>(writer: &mut W, instruction: &Instruction) -> AsmResult<()> {
	write_byte(writer, optab(&instruction)?)?;
	Ok(())
}

fn optab(instruction: &Instruction) -> AsmResult<u8> {
	use self::Operation::*;
	use self::AddrMode::*;

	match (*instruction.operation, *instruction.addr_mode) {
		(ADC, Immediate(_))	=> Ok(0x69),	(ADC, ZeroPage(_))	=> Ok(0x65),
		(ADC, ZeroPageX(_))	=> Ok(0x75),	(ADC, Absolute(_))	=> Ok(0x6D),
		(ADC, AbsoluteX(_))	=> Ok(0x7D),	(ADC, AbsoluteY(_))	=> Ok(0x79),
		(ADC, IndirectX(_))	=> Ok(0x61),	(ADC, IndirectY(_))	=> Ok(0x71),

		(AND, Immediate(_))	=> Ok(0x29),	(AND, ZeroPage(_))	=> Ok(0x25),
		(AND, ZeroPageX(_))	=> Ok(0x35),	(AND, Absolute(_))	=> Ok(0x2D),
		(AND, AbsoluteX(_))	=> Ok(0x3D),	(AND, AbsoluteY(_))	=> Ok(0x39),
		(AND, IndirectX(_))	=> Ok(0x21),	(AND, IndirectY(_))	=> Ok(0x31),

		(ASL, Accumulator)	=> Ok(0x0A),	(ASL, ZeroPage(_))	=> Ok(0x06),
		(ASL, ZeroPageX(_))	=> Ok(0x16),	(ASL, Absolute(_))	=> Ok(0x0E),
		(ASL, AbsoluteX(_))	=> Ok(0x1E),
		
		(BCC, Relative(_))	=> Ok(0x90),

		(BCS, Relative(_))	=> Ok(0xB0),

		(BEQ, Relative(_))	=> Ok(0xF0),

		(BIT, ZeroPage(_))	=> Ok(0x24),	(BCS, Absolute(_)) => Ok(0x2C),

		(BMI, Relative(_))	=> Ok(0x30),

		(BNE, Relative(_))	=> Ok(0xD0),

		(BPL, Relative(_))	=> Ok(0x10),

		(BRK, Implied)		=> Ok(0x00),

		(BVC, Relative(_))	=> Ok(0xD0),

		(BVS, Relative(_))	=> Ok(0x70),

		(CLC, Implied)		=> Ok(0x18),

		(CLD, Implied)		=> Ok(0xD8),

		(CLI, Implied)		=> Ok(0x58),

		(CLV, Implied)		=> Ok(0xB8),

		(CMP, Immediate(_)) => Ok(0xC9),	(CMP, ZeroPage(_))	=> Ok(0xC5),
		(CMP, ZeroPageX(_)) => Ok(0xD5),	(CMP, Absolute(_))	=> Ok(0xCD),
		(CMP, AbsoluteX(_)) => Ok(0xDD),	(CMP, AbsoluteY(_))	=> Ok(0xD9),
		(CMP, IndirectX(_)) => Ok(0xC1),	(CMP, IndirectY(_))	=> Ok(0xD1),

		(CPX, Immediate(_)) => Ok(0xE0),	(CPX, ZeroPage(_))	=> Ok(0xE4),
		(CPX, Absolute(_))	=> Ok(0xEC),

		(CPY, Immediate(_)) => Ok(0xC0),	(CPY, ZeroPage(_))	=> Ok(0xC4),
		(CPY, Absolute(_))	=> Ok(0xCC),

		(DEC, ZeroPage(_))	=> Ok(0xC6),	(DEC, ZeroPageX(_))	=> Ok(0xD6),
		(DEC, Absolute(_))	=> Ok(0xCE),	(DEC, AbsoluteX(_))	=> Ok(0xDE),

		(DEX, Implied)		=> Ok(0xCA),

		(DEY, Implied)		=> Ok(0x88),

		(EOR, Immediate(_))	=> Ok(0x49),	(EOR, ZeroPage(_))	=> Ok(0x45),
		(EOR, ZeroPageX(_))	=> Ok(0x55),	(EOR, Absolute(_))	=> Ok(0x4D),
		(EOR, AbsoluteX(_))	=> Ok(0x5D),	(EOR, AbsoluteY(_))	=> Ok(0x59),
		(EOR, IndirectX(_))	=> Ok(0x41),	(EOR, IndirectY(_))	=> Ok(0x51),

		(INC, ZeroPage(_))	=> Ok(0xE6),	(INC, ZeroPageX(_))	=> Ok(0xF6),
		(INC, Absolute(_))	=> Ok(0xEE),	(INC, AbsoluteX(_))	=> Ok(0xFE),

		(INX, Implied)		=> Ok(0xE8),

		(INY, Implied)		=> Ok(0xC8),

		(JMP, Absolute(_))	=> Ok(0x4C),	(JMP, Indirect(_))	=> Ok(0x6C),

		(JSR, Absolute(_))	=> Ok(0x20),

		(LDA, Immediate(_))	=> Ok(0xA9),	(LDA, ZeroPage(_))	=> Ok(0xA5),
		(LDA, ZeroPageX(_))	=> Ok(0xB5),	(LDA, Absolute(_))	=> Ok(0xAD),
		(LDA, AbsoluteX(_))	=> Ok(0xBD),	(LDA, AbsoluteY(_))	=> Ok(0xB9),
		(LDA, IndirectX(_))	=> Ok(0xA1),	(LDA, IndirectY(_))	=> Ok(0xB1),

		(LDX, Immediate(_))	=> Ok(0xA2),	(LDX, ZeroPage(_))	=> Ok(0xA6),
		(LDX, ZeroPageY(_))	=> Ok(0xB6),	(LDX, Absolute(_))	=> Ok(0xAE),
		(LDX, AbsoluteY(_))	=> Ok(0xBE),

		(LDY, Immediate(_))	=> Ok(0xA0),	(LDY, ZeroPage(_))	=> Ok(0xA4),
		(LDY, ZeroPageY(_))	=> Ok(0xB4),	(LDY, Absolute(_))	=> Ok(0xAC),
		(LDY, AbsoluteY(_))	=> Ok(0xBC),

		(LSR, Accumulator)	=> Ok(0x4A),	(LSR, ZeroPage(_))	=> Ok(0x46),
		(LSR, ZeroPageX(_))	=> Ok(0x56),	(LSR, Absolute(_))	=> Ok(0x4E),
		(LSR, AbsoluteX(_))	=> Ok(0x5E),

		(NOP, Implied)		=> Ok(0xEA),

		(ORA, Immediate(_)) => Ok(0x09),	(ORA, ZeroPage(_))	=> Ok(0x05),
		(ORA, ZeroPageX(_)) => Ok(0x15),	(ORA, Absolute(_))	=> Ok(0x0D),
		(ORA, AbsoluteX(_)) => Ok(0x1D),	(ORA, AbsoluteY(_))	=> Ok(0x19),
		(ORA, IndirectX(_)) => Ok(0x01),	(ORA, IndirectY(_))	=> Ok(0x11),

		(PHA, Implied)		=> Ok(0x48),

		(PHP, Implied)		=> Ok(0x08),

		(PLA, Implied)		=> Ok(0x68),

		(PLP, Implied)		=> Ok(0x28),

		(ROL, Accumulator)	=> Ok(0x2A),	(ROL, ZeroPage(_))	=> Ok(0x26),
		(ROL, ZeroPageX(_))	=> Ok(0x36),	(ROL, Absolute(_))	=> Ok(0x2E),
		(ROL, AbsoluteX(_))	=> Ok(0x3E),

		(ROR, Accumulator)	=> Ok(0x6A),	(ROR, ZeroPage(_))	=> Ok(0x66),
		(ROR, ZeroPageX(_))	=> Ok(0x76),	(ROR, Absolute(_))	=> Ok(0x6E),
		(ROR, AbsoluteX(_))	=> Ok(0x7E),

		(RTI, Implied)		=> Ok(0x40),

		(RTS, Implied)		=> Ok(0x60),

		(SBC, Immediate(_)) => Ok(0xE9),	(SBC, ZeroPage(_))	=> Ok(0xE5),
		(SBC, ZeroPageX(_)) => Ok(0xF5),	(SBC, Absolute(_))	=> Ok(0xED),
		(SBC, AbsoluteX(_)) => Ok(0xFD),	(SBC, AbsoluteY(_))	=> Ok(0xF9),
		(SBC, IndirectX(_)) => Ok(0xE1),	(SBC, IndirectY(_))	=> Ok(0xF1),

		(SEC, Implied)		=> Ok(0x38),

		(SED, Implied)		=> Ok(0xF8),

		(SEI, Implied)		=> Ok(0x78),

		(STA, ZeroPage(_))	=> Ok(0x85),	(STA, ZeroPageX(_))	=> Ok(0x95),
		(STA, Absolute(_))	=> Ok(0x8D),	(STA, AbsoluteX(_))	=> Ok(0x9D),
		(STA, AbsoluteY(_)) => Ok(0x99),	(STA, IndirectX(_))	=> Ok(0x81),
		(STA, IndirectY(_)) => Ok(0x91),

		(STX, ZeroPage(_))	=> Ok(0x86),	(STX, ZeroPageY(_))	=> Ok(0x96),
		(STX, Absolute(_))	=> Ok(0x8E),

		(STY, ZeroPage(_))	=> Ok(0x84),	(STY, ZeroPageY(_))	=> Ok(0x94),
		(STY, Absolute(_))	=> Ok(0x8C),

		(TAX, Implied)		=> Ok(0xAA),

		(TAY, Implied)		=> Ok(0xA8),

		(TSX, Implied)		=> Ok(0xBA),

		(TXA, Implied)		=> Ok(0x8A),

		(TXS, Implied)		=> Ok(0x9A),

		(TYA, Implied)		=> Ok(0x98),

		_ => Err(AsmError::InvalidAddrMode),
	}
}

fn write_addr_mode<W: Write>(writer: &mut W, addr_mode: AddrMode, symtab: &SymTab)
	-> AsmResult<()> {
	use self::AddrMode::*;

	let write_op_word = |w, o, s| write_word(w, expect_word(o, s)?);
	let write_op_byte = |w, o, s| write_word(w, expect_word(o, s)?);

	match addr_mode {
		Absolute(op)	| AbsoluteX(op)	| AbsoluteY(op)
			=> write_op_word(writer, op, symtab),

		Immediate(op)	| Indirect(op)	| IndirectX(op)	| Relative(op)	| ZeroPage(op)	|
		ZeroPageX(op)	| ZeroPageY(op)
			=> write_op_byte(writer, op, symtab),

		_ => Ok(()),
	}
}

fn write_byte<W: Write>(writer: &mut W, byte: u8) -> AsmResult<()> {
	let bytes_written = writer.write(&[byte]);
	match bytes_written {
		Ok(n) if n < 1 => Err(AsmError::BufferWriteOverflow),
		Ok(_) => Ok(()),
		Err(_) => Err(AsmError::BufferWriteError),
	}
}

fn write_word<W: Write>(writer: &mut W, word: u16) -> AsmResult<()> {
	let bytes : [u8; 2] = [ word as u8, (word >> 8) as u8 ];
	let bytes_written = writer.write(&bytes);

	match bytes_written {
		Ok(n) if n < 2 => Err(AsmError::BufferWriteOverflow),
		Ok(_) => Ok(()),
		Err(_) => Err(AsmError::BufferWriteError),
	}
}

fn expect_byte(op: Operand, symtab: &SymTab) -> AsmResult<u8> {
	use std::u8;

	let value = evaluate_operand(op, symtab)?;
	if value <= u8::MAX as usize {
		Ok(value as u8)
	}
	else {
		Err(AsmError::ByteOverflow)
	}
}

fn expect_word(op: Operand, symtab: &SymTab) -> AsmResult<u16> {
	use std::u16;

	let value = evaluate_operand(op, symtab)?;
	if value <= u16::MAX as usize {
		Ok(value as u16)
	}
	else {
		Err(AsmError::WordOverflow)
	}
}

fn evaluate_operand(op: Operand, symtab: &SymTab) -> AsmResult<usize> {
	match op {
		Operand::Value(n) => Ok(n),
		Operand::Label(label) => Ok(
			*symtab
			.get(&label)
			.ok_or(AsmError::UndefinedLabel)?),
	}
}

fn pass1<R: BufRead>(mut reader: R) -> AsmResult<ParsedData> {
	let mut parsed_data = ParsedData::new();

	loop {
		let line = match read_line(&mut reader)? {
			None => break,
			Some(line) => line,
		};

		if line.is_empty() {
			continue;
		}

		let (label, rest) = split_at_label(line);
		add_label(label, &mut parsed_data.symtab)?;

		if rest.is_empty() {
			continue;
		}
		else if rest.contains("=") {
			add_label_from_assignment(&rest, &mut parsed_data.symtab)?;
		}
		else {
			let stop = parse_codedata(rest, &mut parsed_data)?;
			if stop {
				break;
			}
		}
	}

	Ok(parsed_data)
}

fn read_line<R: BufRead>(reader: &mut R) -> AsmResult<Option<String>> {
		let mut line = String::new();
		let bytes = reader
		.read_line(&mut line)
		.map_err(|_| AsmError::IOError)?;

		if bytes == 0 {
			Ok(None) // EOF reached
		}
		else {
			Ok(Some(format_line(line)))
		}
}

fn add_label(label: Option<String>, symtab: &mut SymTab) -> AsmResult<()> {
	if let Some(label_text) = label {
		let pc = symtab.location_counter;
		symtab.add_label(label_text, pc)?;
	}

	Ok(())
}

fn add_label_from_assignment(s: &str, symtab: &mut SymTab) -> AsmResult<()> {
	let assignment = parse_label_assignment(s)?;
	let identifier = evaluate_identifier(assignment.rvalue, symtab);
	symtab.add_label(assignment.lvalue, identifier)?;

	Ok(())
}

fn parse_codedata(s: String, parsed_data: &mut ParsedData) -> AsmResult<bool> {
	let line = parse_line(s)?;

	if let Line::Pragma(Pragma::End) = line {
		return Ok(true);
	}

	let increment = evalute_increment(&line);
	parsed_data.lines.push(line);
	parsed_data.symtab.increment_counter(increment);

	Ok(false)
}

fn format_line(line: String) -> String {
	line.to_uppercase()
		.trim()
		.replace(r"\s+"," ")
		.chars()
		.take_while(|c| *c != ';')
		.collect::<String>()
}

fn evaluate_identifier(identifier: Identifier, symtab: &SymTab) -> usize {
	match identifier {
		Identifier::Number(n) => n,
		Identifier::PC => symtab.location_counter,
	}
}

fn evalute_increment(line: &Line) -> usize {
	match *line {
		Line::Instruction(ref instr) => instruction_length(&instr.addr_mode),
		Line::Pragma(Pragma::Byte(_)) => 1,
		Line::Pragma(Pragma::Word(_)) => 2,
		Line::Pragma(Pragma::End) => panic!(),
	}
}

fn parse_line(line: String) -> AsmResult<Line>{
	let mut chars = line.chars().peekable();

	match chars.peek() {
		Some(&'.') => {
			chars.next();
			let line = parse_pragma(&chars.collect::<String>())?;
			Ok(Line::Pragma(line))
		}
		Some(_) => {
			let line = parse_instruction_line(chars.collect::<String>())?;
			Ok(Line::Instruction(line))
		}
		None => panic!(),
	}
}

fn parse_pragma(s: &str) -> AsmResult<Pragma> {
	let (pragma, value) = split_at_first(s, ' ');

	match pragma.as_str() {
		"END" => Ok(Pragma::End),
		"BYTE" => parse_byte(value.trim()),
		"WORD" => parse_word(value.trim()),
		_ => Err(AsmError::InvalidPragma),
	}
}

fn parse_byte(s: &str) -> AsmResult<Pragma> {
	let op = parse_operand(s)?;
	Ok(Pragma::Byte(op))
}

fn parse_word(s: &str) -> AsmResult<Pragma> {
	let op = parse_operand(s)?;
	Ok(Pragma::Word(op))
}

fn parse_instruction_line(s: String) -> AsmResult<Instruction> {
	let (operation_text, addr_mode_text) = split_at_first(&s, ' ');

	let operation = parse_opcode(&operation_text)?;
	let addr_mode = parse_addr_mode(&addr_mode_text.trim())?;

	Ok(Instruction { operation: operation, addr_mode: addr_mode })
}

fn parse_opcode(s: &str) -> AsmResult<Operation> {
	use self::Operation::*;

	match s {
		"ADC" => Ok(ADC),	"AND" => Ok(AND),	"ASL" => Ok(ASL),	"BCC" => Ok(BCC),
		"BCS" => Ok(BCS),	"BEQ" => Ok(BEQ),	"BIT" => Ok(BIT),	"BMI" => Ok(BMI),
		"BNE" => Ok(BNE),	"BPL" => Ok(BPL),	"BRK" => Ok(BRK),	"BVC" => Ok(BVC),
		"BVS" => Ok(BVS),	"CLC" => Ok(CLC),	"CLD" => Ok(CLD),	"CLI" => Ok(CLI),
		"CLV" => Ok(CLV),	"CMP" => Ok(CMP),	"CPX" => Ok(CPX),	"CPY" => Ok(CPY),
		"DEC" => Ok(DEC),	"DEX" => Ok(DEX),	"DEY" => Ok(DEY),	"EOR" => Ok(EOR),
		"INC" => Ok(INC),	"INX" => Ok(INX),	"INY" => Ok(INY),	"JMP" => Ok(JMP),
		"JSR" => Ok(JSR),	"LDA" => Ok(LDA),	"LDX" => Ok(LDX),	"LDY" => Ok(LDY),
		"LSR" => Ok(LSR),	"NOP" => Ok(NOP),	"ORA" => Ok(ORA),	"PHA" => Ok(PHA),
		"PHP" => Ok(PHP),	"PLP" => Ok(PLP),	"ROL" => Ok(ROL),	"ROR" => Ok(ROR),
		"RTI" => Ok(RTI),	"RTS" => Ok(RTS),	"SBC" => Ok(SBC),	"SEC" => Ok(SEC),
		"SED" => Ok(SED),	"SEI" => Ok(SEI),	"STA" => Ok(STA),	"STX" => Ok(STX),
		"STY" => Ok(STY),	"TAX" => Ok(TAX),	"TAY" => Ok(TAY),	"TSX" => Ok(TSX),
		"TXA" => Ok(TXA),	"TXS" => Ok(TXS),	"TYA" => Ok(TYA),

		_ => Err(AsmError::InvalidOpcode),
	}
}

fn parse_addr_mode(s: &str) -> AsmResult<AddrMode> {
	let mut chars = s.chars();
	let len = s.chars().take(2).count(); //only need the first 2 chars to count

	match len {
		0 => Ok(AddrMode::Implied),
		1 => {
			if chars.next().unwrap() == 'A' {
				Ok(AddrMode::Accumulator)
			}
			else {
				Err(AsmError::InvalidAddrMode)
			}
		}
		_ => match chars.next().unwrap() {
			'#' => parse_immediate(&mut chars.collect()),
			'*' => parse_zeropage_addressing(&mut chars.collect()),
			'(' => parse_indirect_addressing(&mut chars.collect()),
			_ 	=> parse_absolute_addressing(&mut String::from(s)),
		}
	}
}

fn parse_immediate(s: &mut String) -> AsmResult<AddrMode> {
	let op = parse_operand(s)?;
	Ok(AddrMode::Immediate(op))
}

fn parse_zeropage_addressing(s: &mut String) -> AsmResult<AddrMode> {
	let (body, rest) = split_at_first(s, ',');

	let op = parse_operand(&body)?;

	match rest.as_str() {
		"" => Ok(AddrMode::ZeroPage(op)),
		",X" => Ok(AddrMode::ZeroPageX(op)),
		",Y" => Ok(AddrMode::ZeroPageY(op)),
		_ => Err(AsmError::InvalidLabelName),
	}

}

fn parse_indirect_addressing(s: &String) -> AsmResult<AddrMode> {
	let mut chars = s.chars();
	let mut body = String::new();
	let mut rest = String::new();
	while let Some(ss) = chars.next() {
		match ss {
			',' | ')' => {
				rest.push(ss);
				break;
			},
			c => body.push(c),
		}
	}

	rest.push_str(&chars.collect::<String>());

	let op = parse_operand(&body)?;
	match rest.as_str() {
		",X)" => Ok(AddrMode::IndirectX(op)),
		"),Y" => Ok(AddrMode::IndirectY(op)),
		")" => Ok(AddrMode::Indirect(op)),
		_ => Err(AsmError::InvalidLabelName),
	}

}

fn parse_absolute_addressing(s: &String) -> AsmResult<AddrMode> {
	let (body, rest) = split_at_first(s, ',');

	let op = parse_operand(&body)?;

	match rest.as_str() {
		"" => Ok(AddrMode::Absolute(op)),
		",X" => Ok(AddrMode::AbsoluteX(op)),
		",Y" => Ok(AddrMode::AbsoluteY(op)),
		_ => panic!(),
	}
}

fn instruction_length(addr_mode: &AddrMode) -> usize {
	use self::AddrMode::*;

	match *addr_mode {
		Accumulator	| Implied => 1,

		Immediate(_) | Indirect(_) | IndirectX(_) | IndirectY(_) | Relative(_) | ZeroPage(_) |
		ZeroPageX(_) | ZeroPageY(_) => 2,

		Absolute(_) | AbsoluteX(_) | AbsoluteY(_) => 3,
	}
}

fn parse_label_assignment(s: &str) -> AsmResult<LabelAssignment> {
	let (lvalue, mut rvalue) = split_at_first(s, '=');

	rvalue.remove(0); // remove '=' character

	Ok(LabelAssignment {
		lvalue: parse_label(lvalue.trim())?,
		rvalue: parse_identifier(rvalue.trim())?,
	})
}

fn parse_identifier(s: &str) -> AsmResult<Identifier> {
	match s {
		"*" => Ok(Identifier::PC),
		_ => Ok(Identifier::Number(parse_number(s)?)),
	}
}

fn parse_operand(s: &str) -> AsmResult<Operand> {
	match parse_number(s) {
		Ok(n) => Ok(Operand::Value(n)),
		Err(AsmError::InvalidNumberFormat) =>
			parse_label(s).map(|label| Operand::Label(label)),
		Err(x) => Err(x),
	}
}

fn parse_label(s: &str) -> AsmResult<String> {
	let regex = Regex::new(LABEL_REGEX).unwrap();
	let label = regex
		.captures(s)
		.and_then(|c| c.name("label"))
		.map(|m| m.as_str().to_string())
		.and_then(filter_opcode)
		.ok_or(AsmError::InvalidLabelName)?;

		// no keywords
		if parse_opcode(&label).is_ok() {
			Err(AsmError::InvalidLabelName)
		}
		else {
			Ok(label)
		}
}

fn filter_opcode(s: String) -> Option<String> {
	match s.as_str() {
		"ADC" => None,
		_ => Some(s),
	}
}

fn parse_number(s: &str) -> AsmResult<usize> {
	// remember to throw error if string happens to be empty
	match s.chars().next().ok_or(AsmError::InvalidNumberFormat)? {
		'$' => parse_hex(&s[1..]),
		'%' => parse_binary(&s[1..]),
		'O' => parse_octal(&s[1..]),
		'0'...'9' => parse_decimal(s),
		_ => Err(AsmError::InvalidNumberFormat),
	}
}

fn parse_hex(s: &str) -> AsmResult<usize> {
	match usize::from_str_radix(s, 16) {
		Ok(n) => Ok(n),
		_ => Err(AsmError::InvalidHexadecimalNumber),
	}
}

fn parse_binary(s: &str) -> AsmResult<usize> {
	match usize::from_str_radix(s, 2) {
		Ok(n) => Ok(n),
		_ => Err(AsmError::InvalidBinaryNumber),
	}
}

fn parse_octal(s: &str) -> AsmResult<usize> {
	match usize::from_str_radix(s, 8) {
		Ok(n) => Ok(n),
		_ => Err(AsmError::InvalidOctalNumber),
	}
}

fn parse_decimal(s: &str) -> AsmResult<usize> {
	match usize::from_str_radix(s, 10) {
		Ok(n) => Ok(n),
		_ => Err(AsmError::InvalidDecimalNumber),
	}
}

fn split_at_first(s: &str, delim: char) -> (String, String) {
	let offset = s.find(delim).unwrap_or(s.len());
	let mut split_right = String::from(s);
	let split_left = split_right.drain(..offset).collect();

	(split_left, split_right)
}


fn split_at_label(mut line: String) -> (Option<String>, String) {
	match line.find(':') {
		Some(n) => {
			let label = line.drain(..n).collect();
			line = line
				.chars()
				.skip_while(|c| !c.is_alphanumeric())
				.collect();
			(Some(label), line)
		},
		None => (None, line),
	}
}

#[test]
fn test_parse_num() {
	assert_eq!(parse_number("$1B23").unwrap(), 0x1B23);
	assert_eq!(parse_number("1902").unwrap(), 1902);
	assert_eq!(parse_number("O3752").unwrap(), 2026);
	assert_eq!(parse_number("%00111010").unwrap(), 0x3A);
	assert_eq!(parse_number("L33T").unwrap_err(), AsmError::InvalidNumberFormat);
}

#[test]
fn test_parse_label() {
	assert_eq!(parse_label("*").unwrap(), "*"); 
	assert_eq!(parse_label("TEST").unwrap(), "TEST");
	assert_eq!(parse_label("0PX").unwrap_err(), AsmError::InvalidLabelName);
	assert!(parse_label("ADC").is_err());
}

#[test]
fn test_parse_label_assignment() {
	let label = LabelAssignment {
		lvalue: String::from("HEJ"),
		rvalue: Identifier::Number(0x1234),
	};
	
	assert_eq!(parse_label_assignment("HEJ = $1234").unwrap(), label);
	assert_eq!(parse_label_assignment("HEJ =$1234").unwrap(), label);
	assert_eq!(parse_label_assignment("HEJ= $1234").unwrap(), label);
	assert_eq!(parse_label_assignment("HEJ=$1234").unwrap(), label);
	assert!(parse_label_assignment("HEJ=").is_err());
	assert!(parse_label_assignment("=$1234").is_err());
}

#[test]
fn test_implied() {
	assert_addr_mode("", AddrMode::Implied);
}

#[test]
fn test_accumulator() {
	assert_addr_mode("A", AddrMode::Accumulator);
}

#[test]
fn test_zeropage() {
	assert_addr_mode("*$1234", AddrMode::ZeroPage(Operand::Value(0x1234)));
}

#[test]
fn test_zeropage_x() {
	assert_addr_mode("*$248F,X", AddrMode::ZeroPageX(Operand::Value(0x248F)));
}

#[test]
fn test_zeropage_y() {
	assert_addr_mode("*$248F,Y", AddrMode::ZeroPageY(Operand::Value(0x248F)));
}

#[test]
fn test_indirect() {
	assert_addr_mode("($248F)", AddrMode::Indirect(Operand::Value(0x248F)));
}

#[test]
fn test_indirect_x() {
	assert_addr_mode("($248F,X)", AddrMode::IndirectX(Operand::Value(0x248F)));
}

#[test]
fn test_indirect_y() {
	assert_addr_mode("($248F),Y", AddrMode::IndirectY(Operand::Value(0x248F)));
}

#[test]
fn test_absolute() {
	assert_addr_mode("TJOLAHOPP", AddrMode::Absolute(Operand::Label(String::from("TJOLAHOPP"))));
}

#[test]
fn test_absolute_x() {
	assert_addr_mode("TJOLAHOPP,X", AddrMode::AbsoluteX(Operand::Label(String::from("TJOLAHOPP"))));
}

#[test]
fn test_absolute_y() {
	assert_addr_mode("TJOLAHOPP,Y", AddrMode::AbsoluteY(Operand::Label(String::from("TJOLAHOPP"))));
}

#[test]
fn test_pragma_end() {
	assert_eq!(parse_pragma("END").unwrap(), Pragma::End);
}

#[test]
fn test_pragma_byte() {
	assert_eq!(parse_pragma("BYTE $1234").unwrap(), Pragma::Byte(Operand::Value(0x1234)));
}

#[test]
fn test_pragma_word() {
	assert_eq!(parse_pragma("WORD $1234").unwrap(), Pragma::Word(Operand::Value(0x1234)));
}

#[test]
fn test_instruction() {
	assert_eq!(parse_instruction_line(String::from("ADC #$1234")).unwrap(),
		Instruction {
			operation: Operation::ADC,
			addr_mode: AddrMode::Immediate(Operand::Value(0x1234))
		});
}

#[test]
fn test_source() {
	use std::io::Cursor;

	let source = 
		"*=$c000\n
		LDX #0\n
		Label1: TXA\n
		STA $0400,X\n
		LDA #1\n
		STA $D800,X\n
		Label2: INX\n
		BNE Label1\n
		RTS\n
		.END";
	
	let cursor = Cursor::new(source);

	let parsed_data = pass1(cursor);
	
	assert!(parsed_data.is_ok());
}

#[test]
fn test_source_fail() {
	use std::io::Cursor;

	let source = 
		"*=$c000\n
		LDX #0\n
		Label1: TXA\n
		STA $0400,X\n
		LDA #1\n
		STA $D800,X\n
		Label1: INX\n
		BNE Label1\n
		RTS\n
		.END";
	
	let cursor = Cursor::new(source);

	let parsed_data = pass1(cursor);
	
	assert!(parsed_data.is_err());
}

fn assert_addr_mode(s: &str, addr_mode: AddrMode) {
	assert_eq!(parse_addr_mode(&String::from(s)).unwrap(), addr_mode);
}