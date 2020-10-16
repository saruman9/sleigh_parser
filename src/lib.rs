use pest_derive::Parser;

pub mod boolean_expression;

#[derive(Parser)]
#[grammar = "sleigh.pest"]
pub struct SleighParser;

#[cfg(test)]
mod tests {
    use crate::boolean_expression::{parse_boolean_expression, parse_expr_paren};
    use std::collections::HashMap;
    #[test]
    fn boolean_expression() {
        let input = r#"(SH_VERSION == "2") || (SH_VERSION == "2A")"#;
        let mut definitions = HashMap::new();
        definitions.insert("SH_VERSION".to_string(), "2".to_string());
        parse_boolean_expression(input, &definitions).unwrap();
    }

    // #[test]
    // fn expr_paren() {
    //     let input = "(TEST)";
    //     parse_expr_paren(input, &Default::default()).unwrap();
    // }
}
