extern crate mos6502;

use std::env;
use std::str::SplitWhitespace;
use std::io::{stdin, stdout, Read, Write};
use mos6502::emu6502::*;
use std::fs::File;

enum DbgError {
    InsufficientArguments,
    NotANumber,
}

type DbgResult = Result<(), DbgError>;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 1 {
        panic!();
    }
    let bin_path = &args[1];
    let mut bin_file = File::open(bin_path).expect("File not found");
    let mut mem : [u8; 65536] = [0; 65536];
    println!("{} bytes", bin_file.read(&mut mem).expect("WTF"));

    let mut mos = Mos6502::new_with_memory(mem);

    loop {
        print!("> ");
        stdout().flush().unwrap();

        if let Err(_) = evaluate_input(&read_input(), &mut mos) {
            panic!("MUERTE");
        };

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

fn evaluate_input(input: &str, mos: &mut Mos6502) -> DbgResult {
    let mut split_whitespace = input.split_whitespace();

    if let Some(cmd) = split_whitespace.next() {
        return match cmd {
            "step" => step(&mut split_whitespace, mos),
            "reset" => {
                mos.reset();
                Ok(())
            },
            "quit" => std::process::exit(0),
            _ => panic!(),
        }
    }

    Ok(())
}

fn step(iter: &mut SplitWhitespace, mos: &mut Mos6502) -> DbgResult {
    if let Some(num) = iter.next() {
        println!("{}", num);
        let n = num.parse::<usize>().map_err(|_| DbgError::NotANumber)?;
        for _ in 0..n {
            mos.step();
        }
    }
    else {
        mos.step();
    }

    Ok(())
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