use std::{env::args, fs::read_to_string};

use sleigh_parser::parser::lexer::Tokenizer;

fn main() {
    env_logger::builder()
        .target(env_logger::Target::Stdout)
        .init();

    let file_path = args().nth(1).unwrap();

    let mut tokenizer = Tokenizer::new(&file_path);

    for token in tokenizer.tokenize(read_to_string(file_path).unwrap()) {
        if token.is_err() {
            println!("{:?}", token);
        }
    }
}
