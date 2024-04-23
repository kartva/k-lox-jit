use std::io::Read;
use std::path::PathBuf;
use std::fs::File;
use std::ptr::null;
use clap::Parser;

use log::debug;
use lox_jit::codegen::codegen;
use lox_jit::error;
use lox_jit::jit::{call_fn, CompiledBlockCache};
use lox_jit::parse::parse_text;
use simple_logger::SimpleLogger;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: PathBuf
}

fn run_last_fn (src: &str) -> i64 {
    let parsed = parse_text(src);
    debug!("{:#?}", parsed);
    let bytecode = codegen(parsed).unwrap_or_else(|errs| {
        let err = error::format_codegen_errors(src, errs);
        panic!("Error during codegen: {}", err);
    });

    let main_idx = (bytecode.chunks.len() - 1) as u32;
    assert!(bytecode.chunks[main_idx as usize].in_arg == 0);
    debug!("{:#?}", bytecode);
    let mut cbc = CompiledBlockCache::new(bytecode);
    unsafe { call_fn(&mut *cbc, main_idx, null(), 0) }    
}

fn main() {
    SimpleLogger::new().without_timestamps().init().unwrap();
    let args = Args::parse();

    let mut buf = String::new();
    
    File::open(args.file).unwrap().read_to_string(&mut buf).unwrap();

    let ret = run_last_fn(&buf);
    println!("{}", ret);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_main() {
        assert_eq!(run_last_fn("fn main() { 42 }"), 42);
    }

    #[test]
    fn test_simple_if() {
        assert_eq!(run_last_fn("fn main() { var x = 1; if (x) { x } else { 3 } }"), 1);
    }
}
