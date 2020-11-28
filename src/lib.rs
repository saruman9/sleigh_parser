use pest::{
    error::{Error, LineColLocation, ErrorVariant},
    iterators::Pairs,
    Parser, Position,
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
                let new_line_col_loc = NewLineColLocation::from(e.line_col);
                let line_number = new_line_col_loc.line_num();
                let location = match self
                    .locations
                    .binary_search_by(|x| x.global_line_num().cmp(&line_number))
                {
                    Ok(i) => &self.locations[i],
                    Err(i) => &self.locations[i - 1],
                };
                let new_input = input
                    .lines()
                    .skip(location.global_line_num() - location.local_line_num())
                    .take(location.local_line_num())
                    .collect::<Vec<&str>>()
                    .join("\n");
                let pos = Position::new(&new_input, 3).unwrap();
                let e = Error::new_from_pos(e.variant, pos)
                    .with_path(location.path().to_str().unwrap());
                dbg!(location);
                println!("{}", dbg!(e));
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
            NewLineColLocation::Pos((line, _)) => *line,
            NewLineColLocation::Span((line, _), (_, _)) => *line,
        }
    }
}

impl From<LineColLocation> for NewLineColLocation {
    fn from(line_col_location: LineColLocation) -> Self {
        match line_col_location {
            LineColLocation::Pos((line, col)) => Self::Pos((line, col)),
            LineColLocation::Span((line0, col0), (line1, col1)) => {
                Self::Span((line0, col0), (line1, col1))
            }
        }
    }
}
