use logos::Logos;

use sleigh_parser::parser::lexer;

const TEST: &str = r#"
@define VEX_NONE "vexMode=1 & vexVVVV=0"
"#;

fn main() {
    let lex = lexer::Token::lexer(TEST);
    for token in lex.spanned() {
        println!("{:?}", token);
    }
}
