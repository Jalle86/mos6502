extern crate regex;

use std::collections::HashMap;
use self::regex::*;

type SymTab = HashMap<String, usize>; 
type AsmResult<T> = Result<T, AsmError>;
type Label = (String, usize);

static LABEL_REGEX : &'static str = r"^(?P<label>([A-Z][A-Z0-9]*)|\*)$";
static NUM_REGEX : &'static str = r"^P<num>[\$%O]?[0-9A-F]+";

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
}

#[derive(Debug)]
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

struct Instruction {
	operation: Operation,
	addr_mode: AddrMode,
}

fn format_line(line: String) -> String {
	line.to_uppercase()
		.trim()
		.replace(r"\s+"," ")
}

fn add_label(label: String, symtab: &mut SymTab, locctr: usize)	-> AsmResult<()> {
	if !symtab.contains_key(&label) {
		symtab.insert(label, locctr);
		Ok(())
	}
	else {
		Err(AsmError::LabelAlreadyExists)
	}
}

fn splice_label(mut line: String) -> (Option<String>, String) {
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

fn locctr_start(line: &String) -> AsmResult<usize> {
	let regex = Regex::new(r"^\*=\$(?P<addr>[\dA-F]{4})$").unwrap();
	match regex.captures(line) {
		Some(cap) => {
			let s = cap
				.name("addr")
				.expect("Not a valid hexadecimal number")
				.as_str()
				.to_string();
			parse_hex(&s)
		}
		None => Ok(0),
	}
}

fn split_at_first(s: &str, delim: char) -> (String, String) {
	let offset = s.find(delim).unwrap_or(s.len());
	let mut split_right = String::from(s);
	let split_left = split_right.drain(..offset).collect();

	(split_left, split_right)
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
}

fn parse_addr_mode_adc(s: &str) -> AsmResult<AddrMode> {
	let lab_num = || format!(r"{}|{}", LABEL_REGEX, NUM_REGEX);
	let set = RegexSet::new(&[
		lab_num(),
		format!(r"#{}", lab_num()),
		format!(r"{},X", lab_num()),
		format!(r"{},Y", lab_num()),
		format!(r"\({},X\)", lab_num()),
		format!(r"\({}\),Y", lab_num()),
	]).unwrap();

	let matches: Vec<_> = set.matches(s).into_iter().collect();
	if matches.len() > 0 {
		Ok(matches[0])
	}
	else {
		Err(AsmError::InvalidAddrMode)
	}
}*/

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
			'*' => parse_zeropage_addressing(&mut chars.collect()),
			'(' => parse_indirect_addressing(&mut chars.collect()),
			_ 	=> parse_absolute_addressing(&mut String::from(s)),
		}
	}
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

fn parse_opcode(s :&str) -> AsmResult<Operation> {
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

fn parse_assignment(s: &str) -> AsmResult<Label> {
	let regex = Regex::new(r"^(.+)\s?=\s?(.+)$").unwrap();
	let cap = regex.captures(s).ok_or(AsmError::InvalidAssignment)?;
	let id = parse_label(cap.get(1).unwrap().as_str().trim())?;
	let val = parse_number(cap.get(2).unwrap().as_str().trim())?;

	Ok((id, val))
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
	regex
		.captures(s)
		.and_then(|c| c.name("label"))
		.map(|m| m.as_str().to_string())
		.and_then(filter_opcode)
		.ok_or(AsmError::InvalidLabelName)
}

fn filter_opcode(s: String) -> Option<String> {
	match s.as_str() {
		"ADC" => None,
		_ => Some(s),
	}
}

fn parse_number(s: &str) -> AsmResult<usize> {
	match s.chars().next().unwrap() {
		'$' => parse_hex(&s[1..]),
		'%' => parse_binary(&s[1..]),
		'O' => parse_octal(&s[1..]),
		'0'...'9' => parse_decimal(s),
		_ => Err(AsmError::InvalidNumberFormat),
	}
}

fn parse_hex(s : &str) -> AsmResult<usize> {
	match usize::from_str_radix(s, 16) {
		Ok(n) => Ok(n),
		_ => Err(AsmError::InvalidHexadecimalNumber),
	}
}

fn parse_binary(s : &str) -> AsmResult<usize> {
	match usize::from_str_radix(s, 2) {
		Ok(n) => Ok(n),
		_ => Err(AsmError::InvalidBinaryNumber),
	}
}

fn parse_octal(s : &str) -> AsmResult<usize> {
	match usize::from_str_radix(s, 8) {
		Ok(n) => Ok(n),
		_ => Err(AsmError::InvalidOctalNumber),
	}
}

fn parse_decimal(s : &str) -> AsmResult<usize> {
	match usize::from_str_radix(s, 10) {
		Ok(n) => Ok(n),
		_ => Err(AsmError::InvalidDecimalNumber),
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
fn test_parse_assignment() {
	assert_eq!(parse_assignment("HEJ = $1234").unwrap(), ("HEJ".to_string(), 0x1234));
	assert_eq!(parse_assignment("HEJ =$1234").unwrap(), ("HEJ".to_string(), 0x1234));
	assert_eq!(parse_assignment("HEJ= $1234").unwrap(), ("HEJ".to_string(), 0x1234));
	assert_eq!(parse_assignment("HEJ=$1234").unwrap(), ("HEJ".to_string(), 0x1234));
	assert!(parse_assignment("HEJ=").is_err());
	assert!(parse_assignment("=$1234").is_err());
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

fn assert_addr_mode(s: &str, addr_mode: AddrMode) {
	assert_eq!(parse_addr_mode(&String::from(s)).unwrap(), addr_mode);
}