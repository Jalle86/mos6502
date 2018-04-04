extern crate regex;

use std::collections::HashMap;
use self::regex::*;

type SymTab = HashMap<String, usize>;
type AsmResult<T> = Result<T, AsmError>;
type Label = (String, usize);

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
}

enum Operation {
	ADC,
}

#[derive(Debug)]
enum Operand {
	Label(String),
	Value(usize),
}

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

struct Instruction {
	operation: Operation,
	addr_mode: AddrMode,
	length: usize,
}

fn format_line(line: String) -> String {
	line.to_uppercase()
		.trim()
		.replace(r"\s+"," ")
}

fn add_label(label: String, symtab: &mut SymTab, locctr: usize)
	-> AsmResult<()> {
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
			line = line .chars()
						.skip_while(|c| !c.is_alphanumeric())
						.collect();
			(Some(label), line)
		},
		None => (None, line),
	}
}

fn locctr_start(line: &String) -> AsmResult<usize> {
	let regex = Regex::new(r"^\*=\$(?P<addr>[\dA-F]{4})").unwrap();
	match regex.captures(line) {
		Some(cap) => {
			let s = cap.name("addr")
				.expect("Not a valid hexadecimal number")
				.as_str()
				.to_string();
			parse_hex(&s)
		}
		None => Ok(0),
	}
}

fn parse_assignment(s: &str) -> AsmResult<Label> {
	let regex = Regex::new(r"^(.+)\s?=\s?(.+)$").unwrap();
	let cap = regex.captures(s).ok_or(AsmError::InvalidAssignment)?;
	let id = parse_label(cap.get(1).unwrap().as_str())?;
	let val = parse_number(cap.get(2).unwrap().as_str())?;

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
	let regex = Regex::new(r"^(?P<label>([A-Z][A-Z0-9]*)|\*)$").unwrap();
	regex	.captures(s)
			.and_then(|c| c.name("label"))
			.map(|m| m.as_str().to_string())
			.ok_or(AsmError::InvalidLabelName)
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
	//assert_eq!(parse_label("*").unwrap(), "*"); 
	assert_eq!(parse_label("TEST").unwrap(), "TEST");
	assert_eq!(parse_label("0PX").unwrap_err(), AsmError::InvalidLabelName);
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