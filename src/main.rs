use std::{collections::HashMap, env::args};

use sleigh_parser::parser::{grammar::SpecParser, lexer::Tokenizer};
use sleigh_preprocessor::SleighPreprocessor;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use lalrpop_util::ParseError;

fn main() {
    env_logger::builder()
        .target(env_logger::Target::Stdout)
        .init();

    let term_writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();

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
    match SpecParser::new().parse(&writer, tokens) {
        Ok(spec) => println!("{:#?}", spec),
        Err(e) => match e {
            ParseError::UnrecognizedToken { token, expected } => {
                let start = &token.0;
                let end = &token.2;
                let source = writer
                    .lines()
                    .skip(start.global_lineno() - start.local_lineno() + 1)
                    .take(start.local_lineno())
                    .collect::<Vec<&str>>()
                    .join("\n");
                let file = SimpleFile::new(start.filename(), &source);
                let diagnostic = Diagnostic::error()
                    .with_message("parse error")
                    .with_labels(vec![Label::primary(
                        (),
                        start.global_pos()..end.global_pos(),
                    )])
                    .with_notes(vec![format!("expected: {:?}", expected)]);
                println!("{}", &source);
                term::emit(&mut term_writer.lock(), &config, &file, &diagnostic).unwrap();
            }
            ParseError::User { error } => {
                eprintln!("{:?}", error);
            }
            _ => {}
        },
    }
}
