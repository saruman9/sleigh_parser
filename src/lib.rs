use pest_derive::Parser;

pub mod boolean_expression;

#[derive(Parser)]
#[grammar = "sleigh.pest"]
pub struct SleighParser;

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::boolean_expression::parse_boolean_expression;

    #[test]
    fn first() {
        let input = r#"(SH_VERSION == "2") || (SH_VERSION == "2A")"#;
        let mut definitions = HashMap::new();
        definitions.insert("SH_VERSION".to_string(), "2".to_string());
        assert_eq!(parse_boolean_expression(input, &definitions).unwrap(), true);
    }

    #[test]
    fn second() {
        let input = "(defined(dsPIC24F) || defined(dsPIC33E)) && defined(dsPIC33C)";
        let mut definitions = HashMap::new();
        definitions.insert("dsPIC24F".to_string(), Default::default());
        assert_eq!(
            parse_boolean_expression(input, &definitions).unwrap(),
            false
        );
    }

    #[test]
    #[should_panic]
    fn third() {
        let input = r#"(defined(test) || defined(ti)) && PROCESSOR == "PIC_16""#;
        let definitions = HashMap::new();
        assert_eq!(
            parse_boolean_expression(input, &definitions).unwrap(),
            false
        );
    }
}
