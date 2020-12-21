use std::ops;

use pest::{
    iterators::Pair,
    prec_climber::{Assoc, Operator, PrecClimber},
    Parser,
};
use pest_derive::Parser;

use super::{
    lexer::{Definitions, LexicalError, LexicalResult},
    location::Location,
};

#[derive(Parser)]
#[grammar = "./parser/boolean_expression.pest"]
pub struct BooleanExpressionParser;

pub fn parse_boolean_expression(
    input: &str,
    definitions: &Definitions,
    start: &Location,
    end: &Location,
) -> LexicalResult<bool> {
    let mut expr = BooleanExpressionParser::parse(Rule::boolean_expression, input)
        .map_err(|e| LexicalError::new(format!("{}", e), start, end))?;
    parse_expr(expr.next().unwrap(), definitions, start, end)
}

fn parse_expr(
    pair: Pair<Rule>,
    definitions: &Definitions,
    start: &Location,
    end: &Location,
) -> LexicalResult<bool> {
    debug_assert_eq!(pair.as_rule(), Rule::expr);
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::OR_OP, Assoc::Left),
        Operator::new(Rule::XOR_OP, Assoc::Left),
        Operator::new(Rule::AND_OP, Assoc::Left),
    ]);
    consume(pair, &climber, definitions, start, end)
}

fn consume(
    pair: Pair<Rule>,
    climber: &PrecClimber<Rule>,
    definitions: &Definitions,
    start: &Location,
    end: &Location,
) -> LexicalResult<bool> {
    let primary = |pair| consume(pair, climber, definitions, start, end);
    let infix = |l: LexicalResult<bool>, op: Pair<Rule>, r: LexicalResult<bool>| {
        let l = l?;
        let r = r?;
        let result = match op.as_rule() {
            Rule::OR_OP => l || r,
            Rule::XOR_OP => l ^ r,
            Rule::AND_OP => l && r,
            _ => unreachable!(op),
        };
        Ok(result)
    };

    match pair.as_rule() {
        Rule::expr => climber.climb(pair.into_inner(), primary, infix),
        Rule::boolean_clause => parse_boolean_clause(pair, definitions, start, end),
        _ => unreachable!(pair),
    }
}

fn parse_boolean_clause(
    pair: Pair<Rule>,
    definitions: &Definitions,
    start: &Location,
    end: &Location,
) -> LexicalResult<bool> {
    debug_assert_eq!(pair.as_rule(), Rule::boolean_clause);
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::expr_not => parse_expr_not(pair, definitions, start, end),
        Rule::expr_paren => parse_expr_paren(pair, definitions, start, end),
        Rule::expr_eq => parse_expr_eq(pair, definitions, start, end),
        Rule::expr_defined => parse_expr_defined(pair, definitions),
        _ => unreachable!(pair),
    }
}

fn parse_expr_not(
    pair: Pair<Rule>,
    definitions: &Definitions,
    start: &Location,
    end: &Location,
) -> LexicalResult<bool> {
    debug_assert_eq!(pair.as_rule(), Rule::expr_not);
    parse_expr_paren(pair.into_inner().next().unwrap(), definitions, start, end).map(ops::Not::not)
}

fn parse_expr_paren(
    pair: Pair<Rule>,
    definitions: &Definitions,
    start: &Location,
    end: &Location,
) -> LexicalResult<bool> {
    debug_assert_eq!(pair.as_rule(), Rule::expr_paren);
    parse_expr(pair.into_inner().next().unwrap(), definitions, start, end)
}

fn parse_expr_eq(
    pair: Pair<Rule>,
    definitions: &Definitions,
    start: &Location,
    end: &Location,
) -> LexicalResult<bool> {
    debug_assert_eq!(pair.as_rule(), Rule::expr_eq);
    let mut pairs = pair.into_inner();
    let l = parse_expr_term(pairs.next().unwrap(), definitions, start, end)?;
    let comp_op = pairs.next().unwrap();
    let r = parse_expr_term(pairs.next().unwrap(), definitions, start, end)?;
    let result = match comp_op.as_str() {
        "!=" => l != r,
        "==" => l == r,
        _ => unreachable!(comp_op),
    };
    Ok(result)
}

fn parse_expr_defined(pair: Pair<Rule>, definitions: &Definitions) -> LexicalResult<bool> {
    debug_assert_eq!(pair.as_rule(), Rule::expr_defined);
    let pair = pair.into_inner();
    Ok(definitions.contains_key(pair.as_str()))
}

fn parse_expr_term(
    pair: Pair<Rule>,
    definitions: &Definitions,
    start: &Location,
    end: &Location,
) -> LexicalResult<String> {
    debug_assert_eq!(pair.as_rule(), Rule::expr_term);
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::IDENTIFIER => {
            let key = pair.as_str();
            Ok(definitions
                .get(key)
                .cloned()
                .ok_or_else(|| {
                    LexicalError::new(format!(r#"unknown variable: "{}""#, key), start, end)
                })?
                .1)
        }
        Rule::STRING => Ok(pair.as_str().to_string()),
        _ => unreachable!(pair),
    }
}
