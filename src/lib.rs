use pest_derive::Parser;

pub mod boolean_expression;
pub mod errors;

#[derive(Parser)]
#[grammar = "sleigh.pest"]
pub struct SleighParser;

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::boolean_expression::parse_boolean_expression;
    use crate::errors::Error;

    #[test]
    fn eq_or() {
        let input = r#"SH_VERSION == "2" || SH_VERSION == "2A""#;
        let mut definitions = HashMap::new();
        definitions.insert("SH_VERSION".to_string(), "2".to_string());
        assert_eq!(parse_boolean_expression(input, &definitions).unwrap(), true);
    }

    #[test]
    fn paren() {
        let input = "(defined(dsPIC24F) || defined(dsPIC33E)) && defined(dsPIC33C)";
        let mut definitions = HashMap::new();
        definitions.insert("dsPIC24F".to_string(), Default::default());
        assert_eq!(
            parse_boolean_expression(input, &definitions).unwrap(),
            false
        );
    }

    #[test]
    fn not_defined() {
        let input = r#"(defined(test) || defined(ti)) && PROCESSOR == "PIC_16""#;
        assert_eq!(
            parse_boolean_expression(input, &Default::default())
                .err()
                .unwrap(),
            Error::NotDefined("PROCESSOR".to_string())
        );
    }

    #[test]
    fn definitions() {
        let input = r#"DATA_ENDIAN == "little""#;
        let mut definitions = HashMap::new();
        definitions.insert("DATA_ENDIAN".to_string(), "little".to_string());
        assert_eq!(parse_boolean_expression(input, &definitions).unwrap(), true);
    }
}
