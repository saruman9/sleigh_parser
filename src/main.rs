use sleigh_parser::parser::lexer::Tokenizer;

const TEST: &str = r###"
"###;

fn main() {
    let tokens = Tokenizer::new(TEST);
    for token in tokens {
        println!("{:?}", token);
    }
}
