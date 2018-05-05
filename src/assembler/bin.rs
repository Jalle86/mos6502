extern crate mos6502;

use std::env;
use std::fs::File;
use std::io::{Cursor, Read, Write};
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
        let input_path = &args[1];
        let output_path = &args[2];

        let mut input_file = File::open(input_path).expect("File not found");
	    let mut contents = String::new();
	    input_file.read_to_string(&mut contents).expect("something went wrong reading the file");
	    let cursor = Cursor::new(contents);

        let output = match assembler::assemble(cursor) {
            Ok(buf) => buf,
            Err(e) => {
				println!("{}", e);
				return;
			},
        };

        print_hex_dump(remove_trailing_zeroes(&output), 20);

        File::create(output_path)
		    .expect("Could not create file")
		    .write(&output)
		    .expect("Could not write to file");
    }
}


fn print_hex_dump(memory: &[u8], bytes_per_column: usize) {
	let mut column = 0;

	for byte in memory {
		column += 1;
		
		if column == bytes_per_column {
			println!("{:02X}", byte);
			column = 0;
		}
		else {
			print!("{:02X} ", byte);
		}
	}

	if column != 0 {
		println!("");
	}
}

fn remove_trailing_zeroes(buf: &[u8]) -> &[u8] {
	for (i, byte) in buf.iter().enumerate().rev() {
		if *byte != 0 {
			return &buf[0..i+1];
		}
	};

	&buf[..0]
}