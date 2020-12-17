use std::env::args;

use sleigh_parser::parser::lexer::Parser;

fn main() {
    env_logger::builder()
        .target(env_logger::Target::Stdout)
        .init();

    let file_path = args().nth(1).unwrap();

    let parser = Parser::new(file_path);
}
