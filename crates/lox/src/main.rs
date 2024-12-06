use std::{env, fs};

use lox_syntax::parse;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.len() != 1 {
        eprintln!("Usage: lox [path]");
        return;
    }

    let path = args.first().unwrap();
    let data = fs::read_to_string(path).unwrap();

    println!("{:?}", parse(&data));
}
