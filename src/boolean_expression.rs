use std::collections::HashMap;

use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "boolean_expression.pest"]
pub struct BooleanExpressionParser;

type Definitions = HashMap<String, String>;

pub fn parse_boolean_expression(input: &str, definitions: &Definitions) -> Result<bool, String> {
    let mut expr =
        BooleanExpressionParser::parse(Rule::boolean_expression, input).or(Err("Parser"))?;
    Ok(parse_expr(expr.next().unwrap(), definitions))
}

pub fn parse_expr(pair: Pair<Rule>, definitions: &Definitions) -> bool {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::OR_OP, Assoc::Left),
        Operator::new(Rule::XOR_OP, Assoc::Left),
        Operator::new(Rule::AND_OP, Assoc::Left),
    ]);
    consume(pair, &climber, definitions)
}

fn consume(pair: Pair<Rule>, climber: &PrecClimber<Rule>, definitions: &Definitions) -> bool {
    let primary = |pair| consume(pair, climber, definitions);
    let infix = |l: bool, op: Pair<Rule>, r: bool| match op.as_rule() {
        Rule::OR_OP => l || r,
        Rule::XOR_OP => l ^ r,
        Rule::AND_OP => l && r,
        _ => unreachable!(op),
    };

    match pair.as_rule() {
        Rule::expr => climber.climb(pair.into_inner(), primary, infix),
        Rule::boolean_clause => parse_boolean_clause(pair, definitions),
        _ => unreachable!(pair),
    }
}

pub fn parse_boolean_clause(pair: Pair<Rule>, definitions: &Definitions) -> bool {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::expr_not => parse_expr_not(pair, definitions),
        Rule::expr_paren => parse_expr_paren(pair, definitions),
        Rule::expr_eq => parse_expr_eq(pair, definitions),
        Rule::expr_defined => parse_expr_defined(pair, definitions),
        _ => unreachable!(pair),
    }
}

pub fn parse_expr_not(pair: Pair<Rule>, definitions: &Definitions) -> bool {
    !parse_expr_paren(pair.into_inner().next().unwrap(), definitions)
}

pub fn parse_expr_paren(pair: Pair<Rule>, definitions: &Definitions) -> bool {
    parse_expr(pair.into_inner().next().unwrap(), definitions)
}

pub fn parse_expr_eq(pair: Pair<Rule>, definitions: &Definitions) -> bool {
    let mut inner = pair.into_inner();
    let l = parse_expr_term(inner.next().unwrap(), definitions);
    let comp_op = inner.next().unwrap();
    let r = parse_expr_term(inner.next().unwrap(), definitions);
    match comp_op.as_str() {
        "!=" => l != r,
        "==" => l == r,
        _ => unreachable!(comp_op),
    }
}

pub fn parse_expr_defined(pair: Pair<Rule>, definitions: &Definitions) -> bool {
    let identifier_pair = pair.into_inner();
    definitions.contains_key(identifier_pair.as_str())
}

pub fn parse_expr_term(pair: Pair<Rule>, definitions: &Definitions) -> String {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::IDENTIFIER => {
            let key = pair.as_str();
            definitions.get(key).cloned().unwrap()
        }
        Rule::STRING => pair.as_str().to_string(),
        _ => unreachable!(pair),
    }
}
