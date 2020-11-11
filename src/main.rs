use std::{collections::HashMap, env::args};

use sleigh_parser::parser::lexer::Tokenizer;
use sleigh_preprocessor::SleighPreprocessor;

fn main() {
    env_logger::builder()
        .target(env_logger::Target::Stdout)
        .init();

    let mut writer = String::new();
    let mut definitions = HashMap::new();
    let file_path = args().nth(1).unwrap();
    let mut sleigh_preprocessor = SleighPreprocessor::new(definitions, &file_path);

    definitions = match sleigh_preprocessor.process(&mut writer) {
        Ok(def) => def,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };
    println!("{:?}", definitions);

    let tokens = Tokenizer::new(&writer);
    for token in tokens {
        println!("{:?}", token);
    }
}
