use std::collections::HashMap;

use peg;

type Definitions = HashMap<String, String>;

fn main() {
    let mut definitions = HashMap::new();
    definitions.insert("dsPIC24F".to_string(), Default::default());
    definitions.insert("BIT_OPS".to_string(), "PCODEOPS".to_string());
    dbg!(boolean_expression::boolean_clause(
        r#"defined(dsPIC24F) || defined(dsPIC33E) || defined(dsPIC33C)"#,
        &definitions,
    ))
    .unwrap();
    dbg!(boolean_expression::boolean_clause(
        r#"BIT_OPS == "PCODEOPS""#,
        &definitions,
    ))
    .unwrap();
}

peg::parser! {
    grammar boolean_expression(definitions: &'input Definitions) for str {
        rule _() =
            [' ' | '\t' | '\r' | '\n']*

        rule alpha() -> &'input str =
            c:$(['A'..='Z' | 'a'..='z']) { c }
        rule digit() -> &'input str =
            d:$(['0'..='9']) { d }
        rule identifier() -> &'input str =
            id:$((alpha() / ['_'] / digit())+) { id }

        rule hexdigit() -> &'input str =
            d:$(['0'..='9' | 'a'..='f' | 'A'..='F']) { d }
        rule unicode_escape() -> &'input str =
            "\\" "u" d:$(hexdigit()*<4>) { d }
        rule octal_escape() -> &'input str =
            "\\" d:$(['0'..='3']* ['0'..='7']*<1,2>) { d }
        rule escape() -> &'input str =
            e:$(
                "\\" ("b" / "t" / "n" / "f" / "r" / "\"" / "\"" / "\\") /
                unicode_escape() /
                octal_escape()
            ) { e }
        rule qstring() -> String =
            "\"" e:$(escape() / !("\\" / "\"") [_])* "\"" { e.concat() }

        rule expr_term() -> String =
            _ f:(id:identifier() {
                definitions.get(id).unwrap().to_string() }
               / qs:qstring() { qs }
            ) _ {
                f
            }
        rule expr_eq() -> bool =
            l:expr_term() "==" r: expr_term() { l == r } /
            l:expr_term() "!=" r: expr_term() { l != r }

        rule defined() -> bool =
            _ "defined" "(" id:identifier() ")" _ {
                definitions.contains_key(id)
            }

        pub rule boolean_clause() -> bool = precedence! {
            lhs:(@) "||" rhs:@ { lhs || rhs }
            --
            lhs:(@) "^^" rhs:@ { lhs ^ rhs }
            --
            lhs:(@) "&&" rhs:@ { lhs && rhs }
            --
                    "!"  rhs:@ { !rhs }
            --
            "(" b:boolean_clause() ")" { b }
            d:(expr_eq() / defined()) { d }
        }
    }

}
