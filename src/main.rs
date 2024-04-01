use std::io::Read;
use std::path::PathBuf;
use std::fs::File;
use chumsky::debug;
use clap::Parser;

use log::debug;
use lox_jit::codegen::codegen;
use lox_jit::jit::JIT;
use lox_jit::parse::parse_text;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: PathBuf
}

fn main() {
    let args = Args::parse();

    let mut buf = String::new();
    
    File::open(args.file).unwrap().read_to_string(&mut buf).unwrap();

    let parsed = parse_text(&buf).unwrap();
    debug!("{:#?}", parsed);
    let mut bytecode = codegen(parsed);
    debug!("{:#?}", bytecode);
    let jit = JIT::compile(bytecode.chunks.pop().unwrap()).unwrap();
    let ret = jit.run();
    println!("{}", ret);
}
