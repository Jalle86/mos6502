use super::*;

pub fn write_error(error: AsmError, line: usize) -> String {
    format!("Error on line {}: {}.", line.to_string(), error_to_string(error))
}

fn error_to_string(error: AsmError) -> &'static str {
    use super::AsmError::*;

    match error {
	    InvalidNumberFormat => "Not a valid number",
        InvalidBinaryNumber => "Not a valid binary number",
        InvalidHexadecimalNumber => "Not a valid hexadecimal number",
        InvalidOctalNumber => "Not a valid octal number",
        InvalidDecimalNumber => "Not a valid decimal number",
        InvalidLabelName => "Not a valid label name",
        InvalidLabelNameOpcode => "Label name is an opcode",
        LabelAlreadyExists => "Label already defined",
        InvalidOpcode => "Not a valid opcode",
        InvalidAddrMode => "Not a valid addressing mode",
        InvalidInstruction => "Not a valid instruction",
        InvalidPragma => "Not a valid pragma",
        IOError => "Error while reading from file",
        UndefinedLabel => "Label has not been defined",
        WordOverflow => "Overflowing 16-bit value",
        ByteOverflow => "Overflowing 8-bit value",
        BufferWriteOverflow => "Attempt to write outside allocated memory",
        BufferWriteError => "Error while writing",
    }
}