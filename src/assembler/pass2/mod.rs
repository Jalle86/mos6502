use super::*;
use std::io::{Write, Seek, SeekFrom};

pub(super) fn pass2<W: Write + Seek>(writer: &mut W, parsed_data: ParsedData) -> AsmResult<()> {
	for line in parsed_data.lines {
		write_line(writer, line, &parsed_data.symtab)?;
	}

	Ok(())
}

fn write_line<W: Write + Seek>(writer: &mut W, line: Line, symtab: &SymTab) -> AsmResult<()> {
	match line {
		Line::Instruction(instruction)	=> write_instruction(writer, instruction, symtab),
		Line::Pragma(pragma)			=> write_pragma(writer, pragma, symtab),
	}
}

fn write_pragma<W: Write + Seek>(writer: &mut W, pragma: Pragma, symtab: &SymTab) -> AsmResult<()> {
	match pragma {
		Pragma::Byte(op) => write_byte(writer, expect_byte(op, symtab)?),
		Pragma::Word(op) => write_word(writer, expect_word(op, symtab)?),
		Pragma::LocationCounter(n) => set_location_counter(writer, n),
		_ => panic!(),
	}
}

fn set_location_counter<W: Write + Seek>(writer: &mut W, n: usize) -> AsmResult<()> {
	writer.seek(SeekFrom::Start(n as u64)).map_err(|_| AsmError::IOError)?;
	Ok(())
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

	match (instruction.operation.clone(), instruction.addr_mode.clone()) {
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
	let write_op_byte = |w, o, s| write_byte(w, expect_byte(o, s)?);

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
		Ok(n) if n != 1 => Err(AsmError::BufferWriteOverflow),
		Ok(_) => Ok(()),
		Err(_) => Err(AsmError::BufferWriteError),
	}
}

fn write_word<W: Write>(writer: &mut W, word: u16) -> AsmResult<()> {
	let bytes : [u8; 2] = [ word as u8, (word >> 8) as u8 ];
	let bytes_written = writer.write(&bytes);

	match bytes_written {
		Ok(n) if n != 2 => Err(AsmError::BufferWriteOverflow),
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