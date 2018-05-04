mod test;

extern crate regex;

use super::*;
use self::regex::*;
use std::io::BufRead;

static LABEL_REGEX: &'static str = r"^(?P<label>([A-Z][A-Z0-9]*)|\*)$";
static NUM_REGEX: &'static str = r"^P<num>[\$%O]?[0-9A-F]+";

#[derive(Debug, PartialEq)]
struct LabelAssignment {
	lvalue: String,
	rvalue: Identifier,
}

pub(super) fn pass1<R: BufRead>(mut reader: R) -> DetailResult<ParsedData> {
	let mut parsed_data = ParsedData::new();
	let mut line_number = 0;

	loop {
		line_number += 1;

		let line = match read_line(&mut reader) {
			Err(e) => return Err(e, line_number),
			Ok(None) => break,
			Ok(Some(line)) => line,
		};

		if line.is_empty() {
			continue;
		}

		match parse_line(&mut reader, line, parsed_data, line_number) {
			Ok(pd) => parsed_data = pd,
			Err(e) => Err(e, line_number)
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

fn format_line(line: String) -> String {
	let regex = Regex::new(r"\s+").unwrap();
	let line = regex.replace_all(&line.trim(), " ");

	let wtf = line.to_uppercase()
		.chars()
		.take_while(|c| *c != ';')
		.collect::<String>();

	wtf
}

fn parse_line<R: BufRead>(reader: &mut R, line: String, mut parsed_data: ParsedData,
line_number: usize) -> AsmResult<ParsedData> {
	let (label, rest) = split_at_label(line);
	add_label(label, &mut parsed_data.symtab)?;

	if rest.is_empty() {
		return parsed_data;
	}
	else if rest.chars()
		.skip_while(|c| *c == ' ')
		.collect::<String>()
		.starts_with("*=")
		{
		let (_, mut pc) = split_at_first(&rest, '=');
		pc.remove(0); //remove '=' character
		let pc_evaluated = parse_number(&pc)?;
		parsed_data.symtab.location_counter = pc_evaluated;
		parsed_data.data.push((line_number,
			Data::Pragma(Pragma::LocationCounter(pc_evaluated))));
	}
	else if rest.contains("=") {
		add_label_from_assignment(&rest, &mut parsed_data.symtab)?;
	}
	else {
		let stop = insert_data(rest, &mut parsed_data, line_number)?;
		if stop {
			let buf = reader.fill_buf().unwrap();
			reader.consume(buf.len());
		}
	}

	Ok(parsed_data)
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

fn insert_data(s: String, parsed_data: &mut ParsedData, line_number: usize) -> AsmResult<bool> {
	let data = parse_data(s)?;

	if let Data::Pragma(Pragma::End) = data {
		return Ok(true);
	}

	let increment = evalute_increment(&data);
	parsed_data.data.push((line_number, data));
	parsed_data.symtab.increment_counter(increment);

	Ok(false)
}



fn evaluate_identifier(identifier: Identifier, symtab: &SymTab) -> usize {
	match identifier {
		Identifier::Number(n) => n,
		Identifier::PC => symtab.location_counter,
	}
}

fn evalute_increment(data: &Data) -> usize {
	match *data {
		Data::Instruction(ref instr) => instruction_length(&instr.addr_mode),
		Data::Pragma(Pragma::Byte(_)) => 1,
		Data::Pragma(Pragma::Word(_)) => 2,
		Data::Pragma(Pragma::LocationCounter(_)) => 0,
		Data::Pragma(Pragma::End) => panic!(),
	}
}

fn parse_data(data: String) -> AsmResult<Data>{
	let mut chars = data.chars().peekable();

	match chars.peek() {
		Some(&'.') => {
			chars.next();
			let pragma = parse_pragma(&chars.collect::<String>())?;
			Ok(Data::Pragma(pragma))
		}
		Some(_) => {
			let instruction = parse_instruction(chars.collect::<String>())?;
			Ok(Data::Instruction(instruction))
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

fn parse_instruction(s: String) -> AsmResult<Instruction> {
	let (operation_text, addr_mode_text) = split_at_first(&s, ' ');

	let operation = parse_opcode(&operation_text)?;
	let addr_mode = parse_addr_mode(&addr_mode_text.trim())?;

	Ok(apply_relative_addressing(operation, addr_mode))
}

// Due to absolute and relative addressing being parsed identically it would seem I have had a bit
// of a tumble. For the appropriate operations the addressing mode should therefore be corrected
fn apply_relative_addressing(operation: Operation, addr_mode: AddrMode)
-> Instruction {
	use self::Operation::*;

	match &operation {
		&BCC | &BCS | &BEQ | &BMI | &BNE | &BPL | &BVC | &BVS => {
			if let AddrMode::Absolute(n) = addr_mode {
				Instruction::new(operation, AddrMode::Relative(n))
			}
			else {
				Instruction::new(operation, addr_mode)
			}
		},
		_ => Instruction::new(operation, addr_mode),
	}
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
		"TXA" => Ok(TXA),	"TXS" => Ok(TXS),	"TYA" => Ok(TYA),	"PLA" => Ok(PLA),

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
			Err(AsmError::InvalidLabelNameOpcode)
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