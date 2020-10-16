use std::collections::HashMap;
use std::error::Error;

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "boolean_expression.pest"]
pub struct BooleanExpressionParser;

type Definitions = HashMap<String, String>;

pub fn parse_boolean_expression(
    input: &str,
    definitions: &Definitions,
) -> Result<bool, Box<dyn Error>> {
    let mut expr =
        BooleanExpressionParser::parse(Rule::boolean_expression, input).or(Err("Parser"))?;
    parse_expr(expr.next().unwrap(), definitions)
}

pub fn parse_expr(pair: Pair<Rule>, definitions: &Definitions) -> Result<bool, Box<dyn Error>> {
    dbg!(&pair.as_rule());
    let mut inner = pair.into_inner();
    let mut l = parse_boolean_clause(inner.next().unwrap(), definitions)?;
    let mut op = "";
    for pair in inner {
        match pair.as_rule() {
            Rule::bool_op => op = pair.as_str(),
            Rule::boolean_clause => {
                let r = parse_boolean_clause(pair, definitions)?;
                l = parse_bool_op(l, op, r);
            }
            Rule::EOI => return Ok(l),
            _ => unreachable!(pair),
        };
    }
    Ok(l)
}

pub fn parse_boolean_clause(
    pair: Pair<Rule>,
    definitions: &Definitions,
) -> Result<bool, Box<dyn Error>> {
    dbg!(&pair.as_rule());
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::expr_not => parse_expr_not(pair, definitions),
        Rule::expr_paren => parse_expr_paren(pair, definitions),
        Rule::expr_eq => parse_expr_eq(pair, definitions),
        Rule::expr_defined => parse_expr_defined(pair, definitions),
        _ => unreachable!(pair),
    }
}

pub fn parse_expr_not(pair: Pair<Rule>, definitions: &Definitions) -> Result<bool, Box<dyn Error>> {
    dbg!(&pair.as_rule());
    parse_expr_paren(pair.into_inner().next().unwrap(), definitions).map(|b| !b)
}

pub fn parse_expr_paren(
    pair: Pair<Rule>,
    definitions: &Definitions,
) -> Result<bool, Box<dyn Error>> {
    dbg!(&pair.as_rule());
    parse_expr(pair.into_inner().next().unwrap(), definitions)
}

pub fn parse_expr_eq(pair: Pair<Rule>, definitions: &Definitions) -> Result<bool, Box<dyn Error>> {
    dbg!(&pair.as_rule());
    let mut inner = pair.into_inner();
    let l = parse_expr_term(inner.next().unwrap(), definitions)?;
    let comp_op = inner.next().unwrap();
    let r = parse_expr_term(inner.next().unwrap(), definitions)?;
    match comp_op.as_str() {
        "!=" => Ok(l != r),
        "==" => Ok(l == r),
        _ => unreachable!(comp_op),
    }
}

pub fn parse_expr_defined(
    pair: Pair<Rule>,
    definitions: &Definitions,
) -> Result<bool, Box<dyn Error>> {
    dbg!(&pair.as_rule());
    unimplemented!()
}

pub fn parse_expr_term(
    pair: Pair<Rule>,
    definitions: &Definitions,
) -> Result<String, Box<dyn Error>> {
    let pair = pair.into_inner().next().unwrap();
    dbg!(pair.as_rule());
    match pair.as_rule() {
        Rule::IDENTIFIER => {
            let key = pair.as_str();
            Ok(dbg!(definitions.get(key).cloned().unwrap_or_default()))
        }
        Rule::STRING => Ok(dbg!(pair.as_str().to_string())),
        _ => unreachable!(pair),
    }
}

pub fn parse_bool_op(l: bool, op: &str, r: bool) -> bool {
    dbg!(l, op, r);
    match op {
        "||" => l || r,
        "^^" => l ^ r,
        "&&" => l && r,
        _ => unreachable!(op),
    }
}
