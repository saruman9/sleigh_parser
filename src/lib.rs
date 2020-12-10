use std::ops::Range;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use pest::{
    error::{Error, LineColLocation},
    iterators::Pairs,
    Parser,
};
use pest_derive::Parser;
use sleigh_preprocessor::{location::Location, Definitions};

#[derive(Parser)]
#[grammar = "sleigh.pest"]
pub struct SleighParser {
    definitions: Definitions,
    locations: Vec<Location>,
}

impl<'i> SleighParser {
    pub fn new(definitions: Definitions, locations: Vec<Location>) -> Self {
        Self {
            definitions,
            locations,
        }
    }

    pub fn definitions(&self) -> &Definitions {
        &self.definitions
    }

    pub fn locations(&self) -> &[Location] {
        &self.locations
    }

    pub fn parse(&'i self, input: &'i str) -> Result<Pairs<Rule>, Error<Rule>> {
        match <Self as Parser<Rule>>::parse(Rule::sleigh, input) {
            Ok(pairs) => Ok(pairs),
            Err(e) => {
                let mut new_line_col_loc = NewLineColLocation::from(&e.line_col);
                let line_num = new_line_col_loc.line_num();
                let location = match self
                    .locations
                    .binary_search_by(|x| x.global_line_num().cmp(&line_num))
                {
                    Ok(i) => &self.locations[i],
                    Err(i) => &self.locations[i - 1],
                };
                new_line_col_loc = new_line_col_loc.update(location.global_line_num());

                let source = input
                    .lines()
                    .skip(location.global_line_num() - 1)
                    .collect::<Vec<&str>>()
                    .join("\n");
                let file = SimpleFile::new(format!("{}", location.filepath().display()), &source);
                let mut pos_line = 0;
                for line in source.lines().take(new_line_col_loc.line_num() - 1) {
                    pos_line += line.len() + 1;
                }
                pos_line -= 1;
                let diagnostic = Diagnostic::error()
                    .with_message("parse error")
                    .with_labels(vec![Label::primary(
                        (),
                        new_line_col_loc.get_range(pos_line),
                    )])
                    .with_notes(vec![format!("expected: {:?}", e.variant)]);
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = term::Config::default();
                term::emit(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
                std::process::exit(1);
            }
        }
    }
}

#[derive(Debug)]
enum NewLineColLocation {
    Pos((usize, usize)),
    Span((usize, usize), (usize, usize)),
}

impl NewLineColLocation {
    fn line_num(&self) -> usize {
        match self {
            Self::Pos((line, _)) => *line,
            Self::Span((line, _), (_, _)) => *line,
        }
    }

    fn get_range(&self, pos_line: usize) -> Range<usize> {
        match self {
            Self::Pos((_, col)) => (pos_line + col..pos_line + col),
            Self::Span((_, col0), (_, col1)) => (pos_line + col0..pos_line + col1),
        }
    }

    fn update(self, line_num: usize) -> Self {
        match self {
            Self::Pos((line, col)) => Self::Pos((line - line_num + 1, col)),
            Self::Span((line0, col0), (line1, col1)) => {
                Self::Span((line0 - line_num, col0), (line1 - line_num + 1, col1))
            }
        }
    }
}

impl From<&LineColLocation> for NewLineColLocation {
    fn from(line_col_location: &LineColLocation) -> Self {
        match line_col_location {
            LineColLocation::Pos((line, col)) => Self::Pos((*line, *col)),
            LineColLocation::Span((line0, col0), (line1, col1)) => {
                Self::Span((*line0, *col0), (*line1, *col1))
            }
        }
    }
}
