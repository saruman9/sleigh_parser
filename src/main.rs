use sleigh_parser::parser::lexer::Tokenizer;

const TEST: &str = r###"
@include "ia.sinc"
@include "avx.sinc"
@include "avx_manual.sinc"
@include "avx2.sinc"
@include "avx2_manual.sinc"
@include "adx.sinc"
@include "clwb.sinc"
@include "pclmulqdq.sinc"
@include "mpx.sinc"
@include "lzcnt.sinc"
@include "bmi1.sinc"
@include "bmi2.sinc"
@include "sha.sinc"
@include "smx.sinc"
@include "cet.sinc"
@include "rdrand.sinc"
@include "rdseed.sinc"
"###;

fn main() {
    let tokens = Tokenizer::new(TEST);
    for token in tokens {
        println!("{:?}", token);
    }
}
