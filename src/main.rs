use std::io::Read;
use std::path::PathBuf;
use std::fs::File;
use std::ptr::null;
use clap::Parser;

use log::debug;
use lox_jit::codegen::codegen;
use lox_jit::jit::CompiledBlockCache;
use lox_jit::parse::parse_text;
use simple_logger::SimpleLogger;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: PathBuf
}

fn main() {
    SimpleLogger::new().init().unwrap();
    let args = Args::parse();

    let mut buf = String::new();
    
    File::open(args.file).unwrap().read_to_string(&mut buf).unwrap();

    let parsed = parse_text(&buf).unwrap();
    debug!("{:#?}", parsed);
    let bytecode = codegen(parsed);
    debug!("{:#?}", bytecode);
    let mut cbc = CompiledBlockCache::new(bytecode);
    let ret = unsafe { CompiledBlockCache::call_fn(&mut *cbc, 0, null(), 0) };
    println!("{}", ret);
}
