use std::{collections::HashMap, env::args};

use sleigh_parser::parser::{grammar::SpecParser, lexer::Tokenizer};
use sleigh_preprocessor::SleighPreprocessor;

fn main() {
    env_logger::builder()
        .target(env_logger::Target::Stdout)
        .init();

    let mut writer = String::new();
    let definitions = HashMap::new();
    let file_path = args().nth(1).unwrap();
    let mut sleigh_preprocessor = SleighPreprocessor::new(definitions, &file_path, false);

    match sleigh_preprocessor.process(&mut writer) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }

    let tokens = Tokenizer::new(&writer);
    let spec = SpecParser::new().parse(&writer, tokens);
    println!("{:#?}", spec);
}
