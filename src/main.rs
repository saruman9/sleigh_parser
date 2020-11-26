use std::collections::HashMap;

use pest::Parser;
use sleigh_preprocessor::SleighPreprocessor;

use sleigh_parser::{Rule, SleighParser};

fn main() {
    let mut writer = String::new();
    let mut preprocessor =
        SleighPreprocessor::new(HashMap::new(), std::env::args().nth(1).unwrap(), true);
    if let Err(e) = preprocessor.process(&mut writer) {
        eprintln!("{}", e);
        std::process::exit(1);
    }

    println!("{:#?}", preprocessor.definitions());
    println!("{:#?}", preprocessor.locations());

    match SleighParser::parse(Rule::sleigh, &writer) {
        Ok(parser) => {
            println!("{:#?}", parser.tokens());
        }
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
