use pest::{error::Error, iterators::Pairs, Parser};
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
        <Self as Parser<Rule>>::parse(Rule::sleigh, input)
    }
}
