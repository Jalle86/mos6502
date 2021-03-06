use self::AddrMode::*;

macro_rules! addr_mode_panic {
	($instr:expr, $addr_mode:expr) =>
	(panic!(format!("{} invalid addressing mode: {:?}", $instr, $addr_mode)));
}

type FlagArray = [bool; 7];

#[allow(dead_code)]
pub struct Mos6502 {
    pub mem     : [u8; 65536],
    pub acc     : u8,
    pub x       : u8,
    pub y       : u8,
    pub flags   : FlagArray,
    pub sp      : u8,
    pub pc      : u16,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
enum AddrMode
{
	Absolute(u16),
	AbsoluteX(u16),
	AbsoluteY(u16),
	
	Accumulator,
	Immediate(u8),
	Implied,
	
	Indirect(u16),
	IndirectX(u8),
	IndirectY(u16),
	Relative(i8),
	ZeroPage(u8),
	ZeroPageX(u8),
	ZeroPageY(u8),
}

#[allow(dead_code)]
enum Flag {
    C = 0, //carry
    Z = 1, //zero
    I = 2, //interrupt disable
    D = 3, //decimal mode
    B = 4, //break
    V = 5, //overflow
    N = 6, //negative
}

#[allow(dead_code)]
impl Flag {
    fn set(flag_reg: &mut FlagArray, flag: Flag, val : bool) {
        flag_reg[flag as usize] = val;
    }

    fn get(flag_reg: &FlagArray, flag: Flag) -> bool {
        flag_reg[flag as usize]
    }

    fn to_flag_array(flag_bits: u8) -> FlagArray {
    	let mut flag_reg: FlagArray = [true; 7];
    	Flag::set(&mut flag_reg, Flag::C, flag_bits & 0x01 != 0);
    	Flag::set(&mut flag_reg, Flag::Z, flag_bits & 0x02 != 0);
    	Flag::set(&mut flag_reg, Flag::I, flag_bits & 0x04 != 0);
    	Flag::set(&mut flag_reg, Flag::D, flag_bits & 0x08 != 0);
    	Flag::set(&mut flag_reg, Flag::B, flag_bits & 0x10 != 0);
    	Flag::set(&mut flag_reg, Flag::V, flag_bits & 0x40 != 0);
    	Flag::set(&mut flag_reg, Flag::N, flag_bits & 0x80 != 0);

    	flag_reg
    }

    fn from_flag_array(flag_reg: FlagArray) -> u8 {
    	let mut byte = 1 << 5;
    	byte |= (Flag::get(&flag_reg, Flag::C) as u8) << 0;
    	byte |= (Flag::get(&flag_reg, Flag::Z) as u8) << 1;
    	byte |= (Flag::get(&flag_reg, Flag::I) as u8) << 2;
    	byte |= (Flag::get(&flag_reg, Flag::D) as u8) << 3;
    	byte |= (Flag::get(&flag_reg, Flag::B) as u8) << 4;
    	byte |= (Flag::get(&flag_reg, Flag::V) as u8) << 6;
    	byte |= (Flag::get(&flag_reg, Flag::N) as u8) << 7;
    	byte
    }
}

#[allow(dead_code)]
fn is_negative(n: u8) -> bool {
	n & 0x80 != 0
}

#[allow(dead_code)]
fn is_overflowing(n: u8, m: u8) -> bool {
	is_negative(n) ^ is_negative(m)
}

#[allow(dead_code)]
impl Mos6502 {
	pub fn new() -> Mos6502 {
		Self::new_with_memory([0; 65536])
	}

	pub fn new_with_memory(mem: [u8; 65536]) -> Mos6502 {
		Mos6502 {
			mem: mem,
			acc: 0,
			x: 0,
			y: 0,
			flags: [false; 7],
			sp: 0xFF,
			pc: 0,
		}
	}

	pub fn reset(&mut self) {
		self.pc = self.read16(0xFFFC);
	}

	pub fn step(&mut self) {
		let pc = self.pc;
		let addr_mode = self.get_addr_mode(pc);

		let op = self.mem[pc as usize];
		let instr = self.get_instruction(op);
		self.pc += self.get_instr_len(op);

		instr(self, addr_mode);
	}

	fn get_flag(&self, flag: Flag) -> bool {
		Flag::get(&self.flags, flag)
	}

	fn set_flag(&mut self, flag: Flag, b: bool) {
		Flag::set(&mut self.flags, flag, b);
	}

	fn set_flag_array(&self, flags :u8) -> [bool; 7] {
		Flag::to_flag_array(flags)
	}
	
	fn get_instruction(&self, opcode: u8) -> fn(&mut Mos6502, AddrMode) {
		match opcode {
			0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => Mos6502::adc,

			0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => Mos6502::and,

			0x0A | 0x06 | 0x16 | 0x0E | 0x1E => Mos6502::asl,

			0x24 | 0x2C => Mos6502::bit,

			0x00 => Mos6502::brk,
			0x10 => Mos6502::bpl,
			0x30 => Mos6502::bmi,
			0x50 => Mos6502::bvc,
			0x70 => Mos6502::bvs,
			0x90 => Mos6502::bcc,
			0xB0 => Mos6502::bcs,
			0xD0 => Mos6502::bne,
			0xF0 => Mos6502::beq,

			0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => Mos6502::cmp,

			0xE0 | 0xE4 | 0xEC => Mos6502::cpx,

			0xC0 | 0xC4 | 0xCC => Mos6502::cpy,

			0xC6 | 0xD6 | 0xCE | 0xDE => Mos6502::dec,

			0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => Mos6502::eor,

			0x18 => Mos6502::clc,
			0x38 => Mos6502::sec,
			0x58 => Mos6502::cli,
			0x78 => Mos6502::sei,
			0xB8 => Mos6502::clv,
			//0xD8 => Mos6502::cld,
			//0xF8 => Mos6502::sed,

			0xE6 | 0xF6 | 0xEE | 0xFE => Mos6502::inc,

			0x4C | 0x6C => Mos6502::jmp,

			0x20 => Mos6502::jsr,

			0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0x89 | 0xA1 | 0xB1 => Mos6502::lda,

			0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => Mos6502::ldx,

			0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => Mos6502::ldy,

			0x4A | 0x46 | 0x56 | 0x4E | 0x5E => Mos6502::lsr,

			0xEA => Mos6502::nop,

			0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => Mos6502::ora,

			0xAA => Mos6502::tax,
			0x8A => Mos6502::txa,
			0xCA => Mos6502::dex,
			0xEB => Mos6502::inx,
			0xA8 => Mos6502::tay,
			0x98 => Mos6502::tya,
			0x88 => Mos6502::dey,
			0xC8 => Mos6502::iny,

			0x2A | 0x26 | 0x36 | 0x2E | 0x3E => Mos6502::rol,

			0x6A | 0x66 | 0x76 | 0x6E | 0x7E => Mos6502::ror,

			0x40 => Mos6502::rti,
			0x60 => Mos6502::rts,

			0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => Mos6502::sbc,

			0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => Mos6502::sta,

			0x9A => Mos6502::txs,
			0xBA => Mos6502::tsx,
			0x48 => Mos6502::pha,
			0x68 => Mos6502::pla,
			0x08 => Mos6502::php,
			0x28 => Mos6502::plp,

			0x86 | 0x96 | 0x8E => Mos6502::stx,

			0x84 | 0x94 | 0x8C => Mos6502::sty,

			_ => panic!(format!("Invalid opcode: {}", opcode)),
		}
	}

	fn get_addr_mode(&self, pc: u16) -> AddrMode {
		let pc_size = pc as usize;
		let opcode = self.mem[pc_size];
		let byte = self.mem[pc_size + 1];
		let byte2 = self.mem[pc_size + 2];

		let addr_16bit = || self.read16(byte as u16 + byte2 as u16);

		let zero_page_x = || ZeroPageX(byte.wrapping_add(self.x));
		let zero_page_y = || ZeroPageX(byte.wrapping_add(self.y));
		let zero_page = || ZeroPage(byte);
		let immediate = || Immediate(byte);
		let absolute = || Absolute(addr_16bit());
		let absolute_x = || AbsoluteX(addr_16bit() + self.x as u16);
		let absolute_y = || AbsoluteX(addr_16bit() + self.y as u16);
		let indirect = || Indirect(self.mem[addr_16bit() as usize] as u16);
		let indirect_x = || IndirectX(self.mem[byte.wrapping_add(self.x) as usize]);
		let indirect_y = || IndirectY(self.mem[byte as usize] as u16 + self.y as u16);
		let relative = || Relative(byte as i8);

		match opcode {
			0x69 | 0x29 | 0xC9 | 0xE0 | 0xC0 | 0x49 |
			0xA9 | 0xA2 | 0xA0 | 0x09 |	0xE9
			=>	immediate(),

			0x25 | 0x06 | 0x24 | 0xC5 | 0xE4 | 0xC4 |
			0xC6 | 0x45 | 0xE6 | 0xA5 | 0xA6 | 0xA4 |
			0x46 | 0x44 | 0x26 | 0x66 | 0xE5 | 0x85 |
			0x86 | 0x84
			=> zero_page(),

			0x75 | 0x35 | 0x16 | 0xD5 | 0xD6 | 0x55 |
			0xF6 | 0xB5 | 0xB4 | 0x56 | 0x15 | 0x36 |
			0x76 | 0xF5 | 0x95 | 0x94
			=> zero_page_x(),

			0xB6 | 0x96
			=> zero_page_y(),

			0x6D | 0x2D | 0x0E | 0xCD | 0xEC | 0xCC |
			0xCE | 0x4D | 0xEE | 0x4C | 0x20 | 0xAD |
			0xAE | 0xAC | 0x4E | 0x0D | 0x2E | 0x6E |
			0xED | 0x8D | 0x8E | 0x8C
			=> absolute(),

			0x7D | 0x3D | 0x1E | 0xDD | 0xDE | 0x5D |
			0xFE | 0xBD | 0xBC | 0x5E | 0x1D | 0x3E |
			0x7E | 0xFD | 0x9D
			=> absolute_x(),

			0x79 | 0x39 | 0xD9 | 0x59 | 0xB9 | 0xBE |
			0x19 | 0xF9 | 0x99
			=> absolute_y(),

			0x6C
			=> indirect(),

			0x61 | 0x21 | 0xC1 | 0x41 | 0xA1 | 0x01 |
			0xE1 | 0x81
			=> indirect_x(),

			0x71 | 0x31 | 0xD1 | 0x51 | 0xB1 | 0x11 |
			0xF1 | 0x91
			=> indirect_y(),

			0x0A | 0x4A | 0x2A | 0x6A
			=> Accumulator,

			0x00 | 0x18 | 0x38 | 0x58 | 0x78 | 0xB8 |
			0xD8 | 0xF8 | 0xEA | 0xAA | 0x8A | 0xCA |
			0xE8 | 0xA8 | 0x98 | 0x88 | 0xC8 | 0x40 |
			0x60 | 0x9A | 0xBA | 0x48 | 0x68 | 0x08 |
			0x28
			=> Implied,

			0x10 | 0x30 | 0x50 | 0x70 | 0x90 | 0xB0 |
			0xD0 | 0xF0
			=> relative(),

			_ => panic!(format!("Invalid addressing mode opcode: {}", opcode)),
		}

	}

	fn get_instr_len(&mut self, opcode: u8) -> u16 {
		match opcode {
			0x0A | 0x00 | 0x4A | 0x2A | 0x6A | 0x40 |
			0x60 => 1,

			0x69 | 0x65 | 0x75 | 0x61 | 0x71 | 0x29 |
			0x25 | 0x35 | 0x21 | 0x31 | 0x06 | 0x16 |
			0x24 | 0xC9 | 0xC5 | 0xD5 | 0xC1 | 0xD1 |
			0xC0 | 0xC4 | 0xC6 | 0xD6 | 0x49 | 0x45 |
			0x55 | 0x41 | 0x51 | 0xA9 | 0xA5 | 0xA1 |
			0xB1 | 0xA2 | 0xA6 | 0xB6 | 0xA0 | 0xA4 |
			0xB4 | 0x46 | 0x56 | 0x09 | 0x05 | 0x15 |
			0x01 | 0x11 | 0x26 | 0x36 | 0x66 | 0x76 |
			0x85 | 0x95 | 0x81 | 0x91 | 0x9A | 0xBA |
			0x86 | 0x96 | 0x84 | 0x94 => 2,

			0x6D | 0x7D | 0x79 | 0x2D | 0x3D | 0x39 |
			0x2C | 0xCD | 0xDD | 0xD9 | 0xCC | 0x4D |
			0x5D | 0x59 | 0x4C | 0x6C | 0x20 | 0xAD |
			0xBD | 0xB9 | 0xAC | 0xBC | 0x0D | 0x1D |
			0x19 | 0x8D | 0x9D | 0x99 | 0x48 | 0x08 |
			0x8C => 3,

			0x68 | 0x28 => 4,

			_ => panic!(format!("Invalid opcode in instr_len: {}", opcode)),
		}
	}

	fn adc(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("ADC", x),
		};

		let (acc, carry) = self.acc.overflowing_add(op);

		Flag::set(&mut self.flags, Flag::N, is_negative(acc));
		Flag::set(&mut self.flags, Flag::V, is_overflowing(acc, self.acc));
		Flag::set(&mut self.flags, Flag::Z, acc == 0);
		Flag::set(&mut self.flags, Flag::C, carry);

		self.acc = acc;
	}

	fn and(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("AND", x),
		};

		Flag::set(&mut self.flags, Flag::Z, self.acc == 0);
		Flag::set(&mut self.flags, Flag::N, is_negative(self.acc));

		self.acc &= op;
	}

	fn asl(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Accumulator => &mut self.acc,
			ZeroPage(n) | ZeroPageX(n) => &mut self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) => &mut self.mem[n as usize],
			x => addr_mode_panic!("ASL", x),
		};

		let val = *op << 1;
		Flag::set(&mut self.flags, Flag::N, is_negative(val));
		Flag::set(&mut self.flags, Flag::Z, val == 0);
		Flag::set(&mut self.flags, Flag::C, *op & 0x80 != 0);
	}

	fn bit(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			ZeroPage(n) => self.mem[n as usize],
			Absolute(n) => self.mem[n as usize],
			x => addr_mode_panic!("BIT", x),
		};

		let result = self.acc & op;
		Flag::set(&mut self.flags, Flag::N, is_negative(op));
		Flag::set(&mut self.flags, Flag::V, is_overflowing(op, result));
		Flag::set(&mut self.flags, Flag::Z, result == 0);
	}

	fn branch (&mut self, addr_mode: AddrMode, flag: Flag, v: bool) {
		if let Relative(n) = addr_mode {
			if self.get_flag(flag) == v {
				self.pc = self.pc.overflowing_add(n as u16).0;
			}
		}
		else {
			match (flag, v) {
			(Flag::N, false) => addr_mode_panic!("BPL", addr_mode),
			(Flag::N, true) => addr_mode_panic!("BMI", addr_mode),
			(Flag::V, false) => addr_mode_panic!("BVC", addr_mode),
			(Flag::V, true) => addr_mode_panic!("BVS", addr_mode),
			(Flag::C, false) => addr_mode_panic!("BCC", addr_mode),
			(Flag::C, true) => addr_mode_panic!("BCS", addr_mode),
			(Flag::Z, false) => addr_mode_panic!("BNE", addr_mode),
			(Flag::Z, true) => addr_mode_panic!("BEQ", addr_mode),
			_ => panic!(addr_mode),
			};
		}
	}

	fn bpl(&mut self, addr_mode: AddrMode) {
		self.branch(addr_mode, Flag::N, false);
	}

	fn bmi(&mut self, addr_mode: AddrMode) {
		self.branch(addr_mode, Flag::N, true);
	}

	fn bvc(&mut self, addr_mode: AddrMode) {
		self.branch(addr_mode, Flag::V, false);
	}

	fn bvs(&mut self, addr_mode: AddrMode) {
		self.branch(addr_mode, Flag::V, true);
	}

	fn bcc(&mut self, addr_mode: AddrMode) {
		self.branch(addr_mode, Flag::C, false);
	}
	
	fn bcs(&mut self, addr_mode: AddrMode) {
		self.branch(addr_mode, Flag::C, true);
	}

	fn bne(&mut self, addr_mode: AddrMode) {
		self.branch(addr_mode, Flag::Z, false);
	}
	
	fn beq(&mut self, addr_mode: AddrMode) {
		self.branch(addr_mode, Flag::Z, true);
	}

	fn brk(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				self.set_flag(Flag::B, true);
				let pc = self.pc;
				self.push16(pc);
				let flags = Flag::from_flag_array(self.flags);
				self.push(flags);
				self.pc = self.read16(0xFFFE);
				}
			x => addr_mode_panic!("BRK", x),
		}
	}

	fn compare(&mut self, addr_mode: AddrMode, val: u8) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("CMP", x),
		};

		Flag::set(&mut self.flags, Flag::C, val > op);
		Flag::set(&mut self.flags, Flag::Z, val == op);
		Flag::set(&mut self.flags, Flag::N, is_negative(val));
	}

	fn cmp(&mut self, addr_mode: AddrMode) {
		let val = self.acc;
		self.compare(addr_mode, val);
	}

	fn cpx(&mut self, addr_mode: AddrMode) {
		let val = self.x;
		self.compare(addr_mode, val);
	}

	fn cpy(&mut self, addr_mode: AddrMode) {
		let val = self.y;
		self.compare(addr_mode, val);
	}

	fn dec(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			ZeroPage(n) | ZeroPageX(n) =>
				&mut self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) =>
				&mut self.mem[n as usize],
			x => addr_mode_panic!("DEC", x),
		};

		*op = op.overflowing_sub(1).0;
		Flag::set(&mut self.flags, Flag::N, is_negative(*op));
		Flag::set(&mut self.flags, Flag::Z, *op == 0);
	}

	fn eor(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("EOR", x),
		};

		self.acc ^= op;
		Flag::set(&mut self.flags, Flag::N, is_negative(self.acc));
		Flag::set(&mut self.flags, Flag::Z, self.acc == 0);
	}

	fn flag_instr(&mut self, addr_mode: AddrMode, flag: Flag, v: bool, op: &str) {
		match addr_mode {
			Implied => self.set_flag(flag, v),
			_ => addr_mode_panic!(op, addr_mode),
		}
	}

	fn clc(&mut self, addr_mode: AddrMode) {
		self.flag_instr(addr_mode, Flag::C, false, "CLC");
	}

	fn sec(&mut self, addr_mode: AddrMode) {
		self.flag_instr(addr_mode, Flag::C, true, "SEC");
	}

	fn cli(&mut self, addr_mode: AddrMode) {
		self.flag_instr(addr_mode, Flag::I, false, "CLI");
	}

	fn sei(&mut self, addr_mode: AddrMode) {
		self.flag_instr(addr_mode, Flag::I, true, "SEI");
	}

	fn clv(&mut self, addr_mode: AddrMode) {
		self.flag_instr(addr_mode, Flag::V, false, "CLV");
	}

	fn inc(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			ZeroPage(n) | ZeroPageX(n) =>
				&mut self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) =>
				&mut self.mem[n as usize],
			x => addr_mode_panic!("DEC", x),
		};

		*op = op.overflowing_add(1).0;
		Flag::set(&mut self.flags, Flag::N, is_negative(*op));
		Flag::set(&mut self.flags, Flag::Z, *op == 0);
	}

	fn jmp_addr(&self, addr: u16) -> u16 {
		let high = self.mem[(addr as u8).wrapping_add(1) as usize];
		self.mem[addr as usize] as u16 + (high as u16) << 4
	}

	fn jmp(&mut self, addr_mode: AddrMode) {
		self.pc = match addr_mode {
			Absolute(n) => n,
			Indirect(n) => self.jmp_addr(n),
			x => addr_mode_panic!("JMP", x),
		}
	}

	fn jsr(&mut self, addr_mode: AddrMode) {
		let addr = self.pc - 1;
		self.push16(addr);
		match addr_mode {
			Absolute(n) => self.pc = n,
			x => addr_mode_panic!("JSR", x),
		}
	}

	fn lda(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("ADC", x),
		};

		self.acc = op;

		Flag::set(&mut self.flags, Flag::N, is_negative(self.acc));
		Flag::set(&mut self.flags, Flag::Z, self.acc == 0);
	}

	fn ldx(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("ADC", x),
		};

		self.x = op;

		Flag::set(&mut self.flags, Flag::N, is_negative(self.x));
		Flag::set(&mut self.flags, Flag::Z, self.x == 0);
	}

	fn ldy(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("ADC", x),
		};

		self.y = op;
		Flag::set(&mut self.flags, Flag::N, is_negative(self.y));
		Flag::set(&mut self.flags, Flag::Z, self.y == 0);
	}

	fn lsr(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Accumulator => &mut self.acc,
			ZeroPage(n) | ZeroPageX(n) => &mut self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) => &mut self.mem[n as usize],
			x => addr_mode_panic!("LSR", x),
		};

		let val = *op >> 1;
		Flag::set(&mut self.flags, Flag::N, is_negative(val));
		Flag::set(&mut self.flags, Flag::Z, val == 0);
		Flag::set(&mut self.flags, Flag::C, *op & 0x01 != 0);
	}

	fn nop(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => (),
			x => addr_mode_panic!("NOP", x),
		}
	}

	fn ora(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("ORA", x),
		};

		self.acc |= op;
		Flag::set(&mut self.flags, Flag::N, is_negative(self.acc));
		Flag::set(&mut self.flags, Flag::Z, self.acc == 0);
	}

	fn dex(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => self.x = self.x.overflowing_sub(1).0,
			x => addr_mode_panic!("DEX", x),
		}

		Flag::set(&mut self.flags, Flag::N, is_negative(self.x));
		Flag::set(&mut self.flags, Flag::Z, self.x == 0);
	}

	fn inx(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => self.x = self.x.overflowing_add(1).0,
			x => addr_mode_panic!("INX", x),
		}

		Flag::set(&mut self.flags, Flag::N, is_negative(self.x));
		Flag::set(&mut self.flags, Flag::Z, self.x == 0);
	}

	fn dey(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => self.y = self.y.overflowing_sub(1).0,
			x => addr_mode_panic!("DEY", x),
		}

		Flag::set(&mut self.flags, Flag::N, is_negative(self.y));
		Flag::set(&mut self.flags, Flag::Z, self.y == 0);
	}

	fn iny(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => self.y = self.y.overflowing_add(1).0,
			x => addr_mode_panic!("INY", x),
		}

		Flag::set(&mut self.flags, Flag::N, is_negative(self.y));
		Flag::set(&mut self.flags, Flag::Z, self.y == 0);
	}

	fn tax(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => self.x = self.acc,
			x => addr_mode_panic!("TAX", x),
		}

		Flag::set(&mut self.flags, Flag::N, is_negative(self.x));
		Flag::set(&mut self.flags, Flag::Z, self.x == 0);
	}

	fn txa(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => self.acc = self.x,
			x => addr_mode_panic!("TXA", x),
		}

		Flag::set(&mut self.flags, Flag::N, is_negative(self.acc));
		Flag::set(&mut self.flags, Flag::Z, self.acc == 0);
	}

	fn tay(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => self.y = self.acc,
			x => addr_mode_panic!("TAY", x),
		}

		Flag::set(&mut self.flags, Flag::N, is_negative(self.y));
		Flag::set(&mut self.flags, Flag::Z, self.y == 0);
	}

	fn tya(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => self.acc = self.y,
			x => addr_mode_panic!("TYA", x),
		}

		Flag::set(&mut self.flags, Flag::N, is_negative(self.acc));
		Flag::set(&mut self.flags, Flag::Z, self.acc == 0);
	}

	fn rol(&mut self, addr_mode: AddrMode) {
		let carry = self.get_flag(Flag::C) as u8;
		let op = match addr_mode {
			Accumulator => &mut self.acc,
			ZeroPage(n) | ZeroPageX(n) => &mut self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) => &mut self.mem[n as usize],
			x => addr_mode_panic!("ROL", x),
		};
		let op_old = *op;
		*op = (*op << 1) | carry;

		Flag::set(&mut self.flags, Flag::N, is_negative(*op));
		Flag::set(&mut self.flags, Flag::Z, *op == 0);
		Flag::set(&mut self.flags, Flag::C, op_old & 0x80 != 0);
	}

	fn ror(&mut self, addr_mode: AddrMode) {
		let carry = (self.get_flag(Flag::C) as u8) << 7;
		let op = match addr_mode {
			Accumulator => &mut self.acc,
			ZeroPage(n) | ZeroPageX(n) => &mut self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) => &mut self.mem[n as usize],
			x => addr_mode_panic!("ROL", x),
		};
		let op_old = *op;
		*op = (*op >> 1) | carry;

		Flag::set(&mut self.flags, Flag::N, is_negative(*op));
		Flag::set(&mut self.flags, Flag::Z, *op == 0);
		Flag::set(&mut self.flags, Flag::C, op_old & 0x01 != 0);
	}

	fn rti(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				self.flags = Flag::to_flag_array(self.pop());
				self.pc = self.pop16();
			},
			x => addr_mode_panic!("RTI", x),
		}
	}

	fn rts(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				let addr = self.pop16() + 1;
				self.pc = addr;
			},
			x => addr_mode_panic!("RTS", x),
		}
	}

	fn sbc(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Immediate(n) => n,
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n) | Indirect(n) =>
				self.mem[n as usize],
			x => addr_mode_panic!("SBC", x),
		};

		let (acc, carry) = self.acc.overflowing_sub(op);

		Flag::set(&mut self.flags, Flag::N, is_negative(acc));
		Flag::set(&mut self.flags, Flag::V, is_overflowing(acc, self.acc));
		Flag::set(&mut self.flags, Flag::Z, acc == 0);
		Flag::set(&mut self.flags, Flag::C, carry);

		self.acc = acc;
	}

	fn sta(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			ZeroPage(n) | ZeroPageX(n) | IndirectX(n) =>
				&mut self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) | AbsoluteY(n)
						| Indirect(n) | IndirectY(n) =>
				&mut self.mem[n as usize],
			x => addr_mode_panic!("STA", x),
		};

		*op = self.acc;
	}

	fn txs(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				self.sp = self.x;
			},
			x => addr_mode_panic!("TXS", x),
		}
	}

	fn tsx(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				self.x = self.sp;
			},
			x => addr_mode_panic!("TSX", x),
		}
	}

	fn pha(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				let acc = self.acc;
				self.push(acc);
			},
			x => addr_mode_panic!("PHA", x),
		}
	}

	fn pla(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				let acc = self.acc;
				self.push(acc);
			},
			x => addr_mode_panic!("PLA", x),
		}
	}

	fn php(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				let flags = Flag::from_flag_array(self.flags);
				self.push(flags);
			},
			x => addr_mode_panic!("PHP", x),
		}

	}

	fn plp(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Implied => {
				self.flags = Flag::to_flag_array(self.pop());
			},
			x => addr_mode_panic!("PLP", x),
		}

	}

	fn stx(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			ZeroPage(n) | ZeroPageY(n) => self.mem[n as usize] = self.x,
			Absolute(n) => self.mem[n as usize] = self.x,
			x => addr_mode_panic!("STX", x),
		}
	}

	fn sty(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			ZeroPage(n) | ZeroPageY(n) => self.mem[n as usize] = self.y,
			Absolute(n) => self.mem[n as usize] = self.y,
			x => addr_mode_panic!("STY", x),
		}
	}

	fn push(&mut self, val: u8) {
		self.mem[0x0100 + self.sp as usize] = u8::wrapping_sub(val, 1);
		self.sp = self.sp.overflowing_sub(1).0;
	}

	fn pop(&mut self) -> u8 {
		let ret = self.mem[0x0100 + self.sp as usize];
		self.sp = self.sp.overflowing_add(1).0;
		ret
	}

	fn push16(&mut self, val: u16) {
		self.push((val & 0xFF) as u8);
		self.push(((val >> 4) & 0xFF) as u8);
	}

	fn pop16(&mut self) -> u16 {
		self.pop() as u16 + (self.pop() as u16) << 4
	}

	fn read16(&self, addr: u16) -> u16 {
		self.mem[addr as usize] as u16 +
					((self.mem[addr as usize + 1] as u16) << 8)
	}

	fn write16(&mut self, addr: u16, val: u16) {
		self.mem[addr as usize] = (val & 0xFF) as u8;
		self.mem[addr as usize + 1] = ((val >> 8) & 0xFF) as u8;
	}
}

#[test]
fn test_adc_overflow() {
	let mut m = Mos6502::new();
	m.acc = 100;
	m.adc(Immediate(200));
	assert_eq!(m.acc, 44);
}

#[test]
fn test_adc_flags() {
	let mut m = Mos6502::new();

	m.acc = 100;
	m.adc(Immediate(156));
	assert_eq!(m.get_flag(Flag::Z), true);
	assert_eq!(m.get_flag(Flag::C), true);

	m = Mos6502::new();
	m.acc = 0x7F;
	m.adc(Immediate(0x10));
	assert_eq!(m.get_flag(Flag::N), true);
	assert_eq!(m.get_flag(Flag::V), true);	
}

#[test]
fn test_and() {
	let mut m = Mos6502::new();
	m.acc = 0xC3;
	m.and(Immediate(0x12));
	assert_eq!(m.acc, 0x02);
}

#[test]
fn test_and_flags() {
	let mut m = Mos6502::new();
	m.acc = 0xF3;
	m.and(Immediate(0x92));
	assert_eq!(m.get_flag(Flag::N), true);
	assert_eq!(m.get_flag(Flag::Z), false);
}

#[test]
fn test_dec_overflow() {
	let mut m = Mos6502::new();
	m.dec(Absolute(0));
	assert_eq!(m.mem[0], 255);
}