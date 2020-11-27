use std::collections::HashMap;

use sleigh_preprocessor::SleighPreprocessor;

use sleigh_parser::SleighParser;

fn main() {
    let mut writer = String::new();
    let mut preprocessor =
        SleighPreprocessor::new(HashMap::new(), std::env::args().nth(1).unwrap(), true);
    if let Err(e) = preprocessor.process(&mut writer) {
        eprintln!("{}", e);
        std::process::exit(1);
    }

    let parser = SleighParser::new(
        preprocessor.take_definitions(),
        preprocessor.take_locations(),
    );

    println!("{:#?}", parser.definitions());
    println!("{:#?}", parser.locations());

    match parser.parse(&writer) {
        Ok(parser) => {
            println!("{:#?}", parser.tokens());
        }
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
