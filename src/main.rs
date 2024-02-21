use std::io::Read;
use std::path::PathBuf;
use std::fs::File;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: PathBuf
}

fn main() {
    println!("Hello, world!");
    let args = Args::parse();

    let mut buf = String::new();
    File::open(args.file).unwrap().read_to_string(&mut buf).unwrap();
}
