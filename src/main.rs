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

    match parser.parse(&writer) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
