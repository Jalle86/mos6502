extern crate mos6502;

use std::io::*;
use mos6502::emu6502::*;

fn main() {
    print!("> ");
    stdout().flush().unwrap();

    let mut input = String::new();

    match stdin().read_line(&mut input) {
        Ok(_) => println!("{}", input),
        Err(_) => panic!(),
    }
}

fn read_char(c: char) -> Option<char> {
    match c {
        '\n' => None,
        c => Some(c),
    }
}

fn print_state(mos: Mos6502) {
    println!("A=${}\tX=${}\tY=${}", mos.acc, mos.x, mos.y);
    println!("PC=${}\tSP=${}", mos.pc, mos.sp);
}