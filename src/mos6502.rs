use self::AddrMode::*;

macro_rules! addr_mode_panic {
	($instr:expr, $addr_mode:expr) =>
	(panic!(format!("{} invalid addressing mode: {:?}", $instr, $addr_mode)));
}

#[allow(dead_code)]
struct Mos6502 {
    mem     : [u8; 65536],
    acc     : u8,
    x       : u8,
    y       : u8,
    flags   : [bool; 7],
    sp      : u8,
    pc      : u16,
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
    C = 0,
    Z = 1,
    I = 2,
    D = 3,
    B = 4,
    V = 5,
    N = 6,
}

#[allow(dead_code)]
impl Flag {
    fn set(flag_reg: &mut [bool], flag: Flag, val : bool) {
        flag_reg[flag as usize] = val;
    }

    fn get(flag_reg: &[bool], flag: Flag) -> bool {
        flag_reg[flag as usize]
    }
}

#[allow(dead_code)]
impl Mos6502 {
	fn new() -> Mos6502 {
		Mos6502 {
			mem: [0; 65536],
			acc: 0,
			x: 0,
			y: 0,
			flags: [false; 7],
			sp: 0,
			pc: 0,
		}
	}

	fn get_flag(&self, flag: Flag) -> bool {
		Flag::get(&self.flags, flag)
	}

	fn set_flag(&mut self, flag: Flag, v: bool) {
		Flag::set(&mut self.flags, flag, v);
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

		self.acc = self.acc.overflowing_add(op).0;
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

		self.acc &= op;
	}

	fn asl(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			Accumulator => &mut self.acc,
			ZeroPage(n) | ZeroPageX(n) => &mut self.mem[n as usize],
			Absolute(n) | AbsoluteX(n) => &mut self.mem[n as usize],
			x => addr_mode_panic!("ASL", x),
		};

		*op << 1;
	}

	fn bit(&mut self, addr_mode: AddrMode) {
		let op = match addr_mode {
			ZeroPage(n) => self.mem[n as usize],
			Absolute(n) => self.mem[n as usize],
			x => addr_mode_panic!("BIT", x),
		};

		let result = self.acc & op;
		Flag::set(&mut self.flags, Flag::Z, result == 0);
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
		// TODO: S Flag
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

	fn bpl(&mut self, addr_mode: AddrMode) {
		match addr_mode {
			Relative(n) => if !self.get_flag(Flag::N) {
				self.pc.overflowing_add(n as u16);
			},
			x => addr_mode_panic!("BPL", x),
		};
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
fn test_and() {
	let mut m = Mos6502::new();
	m.acc = 0xC3;
	m.and(Immediate(0x12));
	assert_eq!(m.acc, 0x02);
}

#[test]
fn test_dec_overflow() {
	let mut m = Mos6502::new();
	m.dec(Absolute(0));
	assert_eq!(m.mem[0], 255);
}