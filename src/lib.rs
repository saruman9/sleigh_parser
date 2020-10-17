use pest_derive::Parser;

pub mod boolean_expression;

#[derive(Parser)]
#[grammar = "sleigh.pest"]
pub struct SleighParser;

#[cfg(test)]
mod tests {
    use crate::boolean_expression::parse_boolean_expression;
    use std::collections::HashMap;
    #[test]
    fn first() {
        let input = r#"(SH_VERSION == "2") || (SH_VERSION == "2A")"#;
        let mut definitions = HashMap::new();
        definitions.insert("SH_VERSION".to_string(), "2".to_string());
        dbg!(parse_boolean_expression(input, &definitions).unwrap());
    }

    #[test]
    fn second() {
        let input = "(defined(dsPIC24F) || defined(dsPIC33E)) && defined(dsPIC33C)";
        let mut definitions = HashMap::new();
        definitions.insert("dsPIC24F".to_string(), Default::default());
        dbg!(parse_boolean_expression(input, &definitions).unwrap());
    }
}
