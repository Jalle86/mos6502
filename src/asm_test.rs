use std::fs::File;
use std::io::{Cursor, Read};
use std::env;
use assembler;

#[test]
fn test_first() {
    assert_source("first");
}

#[test]
fn test_addrmode() {
    assert_source("addrmode");
}

#[test]
fn test_addrmode2() {
    assert_source("addrmode2");
}

#[test]
fn test_addrmode3() {
    assert_source("addrmode3");
}

#[test]
fn test_addrmode4() {
    assert_source("addrmode4");
}

#[test]
fn test_branch() {
    assert_source("branch");
}

#[test]
fn test_instr() {
    assert_source("instr");
}

#[test]
fn test_jsr() {
    assert_source("jsr");
}

#[test]
fn test_jump() {
    assert_source("jump");
}

#[test]
fn test_loop() {
    assert_source("loop");
}

fn cwd() -> String {
    format!("{}/test_sources/", env::current_dir().unwrap().display())
}

fn assert_source(s: &str) {
    assert!(test_source(s))
}

fn test_source(path: &str) -> bool {
    let cwd = cwd();
    test_source_extension(format!("{}{}.asm", cwd, path), format!("{}{}.hex", cwd, path))
}

fn test_source_extension(source_path: String, hex_path: String) -> bool {
    let contents = read_file(source_path);
	let cursor = Cursor::new(contents);

    let output = match assembler::assemble(cursor) {
        Ok(buf) => buf.into_vec(),
        Err(e) => panic!("{}", e),
    };

    let hex = format_hex_string(read_file(hex_path));

    for (i, h) in hex.into_iter().enumerate() {
        print!("{} {} ", h, output[i]);
        if h != output[i] {
            return false;
        }
    }
    
    true
}

fn read_file(path: String) -> String {
    let mut file = File::open(path).expect("File not found");
    let mut contents = String::new();
	file.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}

fn format_hex_string(hex: String) -> Vec<u8> {
    let mut v: Vec<u8> = Vec::new();
    {
        let pm = |p| match p {
            Ok(b) => v.push(b),
            Err(_) => panic!(),
        };

        hex
            .split_whitespace()
            .map(|c| u8::from_str_radix(c, 16))
            .for_each(pm);
    }

    v
}