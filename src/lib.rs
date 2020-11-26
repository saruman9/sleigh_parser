use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "sleigh.pest"]
pub struct SleighParser;
