extern crate mos6502;

use std::env;
use mos6502::assembler;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("No input file specified.");
    }
    else if args.len() < 3 {
        panic!("No output file specified.")
    }
    else {
        assembler::assemble(&args[1], &args[2]);
    }
}