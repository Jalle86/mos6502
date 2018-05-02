use super::super::*;
use super::*;

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
	assert_eq!(parse_instruction(String::from("ADC #$1234")).unwrap(),
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

#[test]
fn test_source_pc() {
	use std::io::Cursor;

	let source = "*=$2000";
	let cursor = Cursor::new(source);
	let parsed_data = pass1(cursor).unwrap();

	assert_eq!(parsed_data.symtab.location_counter, 0x2000);
}

fn assert_addr_mode(s: &str, addr_mode: AddrMode) {
	assert_eq!(parse_addr_mode(&String::from(s)).unwrap(), addr_mode);
}