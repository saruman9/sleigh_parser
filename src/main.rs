use std::{env::args, fs::read_to_string};

use sleigh_parser::parser::lexer::Tokenizer;

fn main() {
    let input = read_to_string(args().nth(1).unwrap()).unwrap();
    let tokens = Tokenizer::new(&input);
    for token in tokens {
        println!("{:?}", token);
    }
}
