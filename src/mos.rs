struct Mos6502 {
    mem     : [u8; 65536],
    acc     : u8,
    x       : u8,
    y       : u8,
    flags   : [bool; 7],
    sp      : u8,
    pc      : u16,
}

enum Flag {
    C = 0,
    Z = 1,
    I = 2,
    D = 3,
    B = 4,
    V = 5,
    N = 6,
}

impl Flag {
    fn set(flag_reg: &mut [bool], flag: Flag, val : bool) {
        flag_reg[flag as usize] = val;
    }

    fn get(flag_reg: &mut [bool], flag: Flag) -> bool {
        flag_reg[flag as usize]
    }
}