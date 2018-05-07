extern crate mos6502;

use std::env;
use std::io::*;
use mos6502::emu6502::*;
use std::fs::File;

enum DbgError {
    InsufficientArguments,
    NotANumber,
}

type DbgResult = Result<(), DbgError>;

fn main() {
    let args: Vec<String> = env::args().collect();
    let bin_path = &args[1];
    let mut bin_file = File::open(bin_path).expect("File not found");
    let mut mem : [u8; 65536] = [0; 65536];
    bin_file.write_all(&mut mem);

    let mos = Mos6502::new_with_memory(mem);

    loop {
        print!("> ");
        stdout().flush().unwrap();

        evaluate_input(&read_input());

        print_state(&mos);
    }
}

fn read_input() -> String {
    let mut input = String::new();
    match stdin().read_line(&mut input) {
        Ok(_) => input,
        Err(_) => panic!(),
    }
}

fn evaluate_input(input: &str) -> DbgResult {
    let split_whitespace = input.split_whitespace();

    if let Some(cmd) = split_whitespace.next() {
        match cmd {
            "step" => step(split_whitespace),
        }
    }

    Ok(())
}

fn step(iter: SplitWhitespace) -> DbgResult {
    if let Some(num) = iter.next() {
        let n = num.parse::<usize>().map_err(|_| DbgError::NotANumber)?;
    }
}

fn read_char(c: char) -> Option<char> {
    match c {
        '\n' => None,
        c => Some(c),
    }
}

fn print_state(mos: &Mos6502) {
    println!("A=${}\tX=${}\tY=${}", mos.acc, mos.x, mos.y);
    println!("PC=${}\tSP=${}", mos.pc, mos.sp);
}