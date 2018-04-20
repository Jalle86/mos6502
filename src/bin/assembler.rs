extern crate regex;

use std::collections::HashMap;
use self::regex::*;
use std::io::BufRead;

type AsmResult<T> = Result<T, AsmError>;
type Lines = Vec<Line>;

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
		if !self.contains(&label) {
			self.insert(label, value);
			Ok(())
		}
		else {
			Err(AsmError::LabelAlreadyExists)
		}
	}
}

/*fn optab_adc(addr_mode: AddrMode, symtab: SymTab) -> AsmResult<usize> {

	match addr_mode {
		AddrMode::Immediate(_)	=> Ok(0x69),
		AddrMode::ZeroPage(_)	=> Ok(0x65),
		AddrMode::ZeroPageX(_)	=> Ok(0x75),
		AddrMode::Absolute(_)	=> Ok(0x6D),
		AddrMode::AbsoluteX(_)	=> Ok(0x7D),
		AddrMode::AbsoluteY(_)	=> Ok(0x79),
		AddrMode::IndirectX(_)	=> Ok(0x61),
		AddrMode::IndirectY(_)	=> Ok(0x71),
		_ 						=> Err(AsmError::InvalidAddrMode),
	}
}

fn parse_addr_mode(op: Operation, s: &str) -> AsmResult<AddrMode> {
	match op {
		Operation::ADC => parse_addr_mode_adc(s)
	}
}*/

fn main() {
	use std::io::Cursor;

	let source = 
		"*=$c000\n
LDX #0\n
Label1: TXA\n0
STA $0400,X\n
LDA #1\n
STA $D800,X\n
INX BNE\n
Label2: RTS\n
.END";
	
	let cursor = Cursor::new(source);
	pass1(cursor);
}

fn pass1<R>(mut reader: R) -> AsmResult<ParsedData>
	where R: BufRead {
	let mut parsed_data = ParsedData::new();

	loop {
		let mut line = String::new();
		let bytes = reader
		.read_line(&mut line)
		.map_err(|_| AsmError::IOError)?;
		if bytes == 0 {
			break;
		}

		line = format_line(line);

		if line.chars().count() == 0 {
			continue;
		}

		let (label, rest) = split_at_label(line);
		if let Some(label_text) = label {
			let pc = parsed_data.symtab.location_counter;
			parsed_data.symtab.insert(label_text, pc);
		}

		if rest.is_empty() {
			continue;
		}

		if rest.contains("=") {
			let assignment = parse_label_assignment(&rest)?;
			let identifier = evaluate_identifier(assignment.rvalue, &parsed_data.symtab);
			parsed_data.symtab.insert(assignment.lvalue, identifier);
		}
		else {
			let line = parse_line(rest)?;

			if let Line::Pragma(Pragma::End) = line {
				return Ok(parsed_data);
			}

			let increment = evalute_increment(&line);
			parsed_data.lines.push(line);
			parsed_data.symtab.increment_counter(increment);
		}
	}

	Ok(parsed_data)
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
		if parse_opcode(label).is_ok() {
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

fn add_label(label: String, symtab: &mut SymTab) -> AsmResult<()> {
	if !symtab.contains(&label) {
		symtab.insert_at_counter(label);
		Ok(())
	}
	else {
		Err(AsmError::LabelAlreadyExists)
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
		INX BNE\n
		Label2: RTS\n
		.END";
	
	let cursor = Cursor::new(source);

	let parsed_data = pass1(cursor).unwrap();
	
	assert!(parsed_data.symtab.get("LABEL2").is_some());
}

fn assert_addr_mode(s: &str, addr_mode: AddrMode) {
	assert_eq!(parse_addr_mode(&String::from(s)).unwrap(), addr_mode);
}