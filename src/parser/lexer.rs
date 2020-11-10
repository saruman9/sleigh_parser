use log::trace;
use logos::{Lexer, Logos, Span};

use super::location::Location;

#[derive(Logos, Debug, Clone)]
#[logos(subpattern hex = r"[0-9a-fA-F]")]
#[logos(extras = Location)]
pub enum Token<'input> {
    #[error]
    UnexpectedToken,

    // WhiteSpaces
    #[regex(r"[ \t]+", logos::skip)]
    #[regex(r"\r?\n?", |lex| {
        lex.extras.inc_lineno();
        logos::Skip
    })]
    #[regex(r"#[^\n\r]*\r?\n?", |lex|{
        lex.extras.inc_lineno();
    })]
    LineComment,
    // FIXME: Report error
    #[token("//")]
    CppComment,

    // Preprocessor
    #[regex(r"\x08[^\n\x08]*\x08", |lex| {
        let split: Vec<&str> = lex
            .slice()
            .trim_matches('\x08')
            .split("###")
            .collect();
        if split.len() == 2 {
            lex.extras
                .set_location(split[0], split[1].parse().unwrap(), lex.span());
            trace!("{}, {:?}", lex.slice(), lex.extras);
        }
        logos::Skip
    })]
    PPPosition,

    // Reserved words and keywords
    #[token("with")]
    With,
    #[token("alignment")]
    Alignment,
    #[token("attach")]
    Attach,
    #[token("big")]
    Big,
    #[token("bitrange")]
    Bitrange,
    #[token("build")]
    Build,
    #[token("call")]
    Call,
    #[token("context")]
    Context,
    #[token("crossbuild")]
    Crossbuild,
    #[token("dec")]
    Dec,
    #[token("default")]
    Default,
    #[token("define")]
    Define,
    #[token("endian")]
    Endian,
    #[token("export")]
    Export,
    #[token("goto")]
    GoTo,
    #[token("hex")]
    Hex,
    #[token("little")]
    Little,
    #[token("local")]
    Local,
    #[token("macro")]
    Macro,
    #[token("names")]
    Names,
    #[token("noflow")]
    NoFlow,
    #[token("offset")]
    Offset,
    #[token("pcodeop")]
    PCodeOp,
    #[token("return")]
    Return,
    #[token("signed")]
    Signed,
    #[token("size")]
    Size,
    #[token("space")]
    Space,
    #[token("token")]
    Token,
    #[token("type")]
    Type,
    #[token("unimpl")]
    Unimpl,
    #[token("values")]
    Values,
    #[token("variables")]
    Variables,
    #[token("wordsize")]
    Wordsize,

    // Grouping, block, and sectioning symbols
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    // Miscellaneous
    #[token("...")]
    Ellipsis,
    #[token("_")]
    Underscore,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("!")]
    Exclaim,
    #[token("~")]
    Tilde,
    #[token(";")]
    Semi,

    // Operators:
    #[token("=")]
    Assign,

    // Comparisons
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Great,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreatEqual,

    // Boolean and bitwise logic operations
    #[token("||")]
    BoolOr,
    #[token("^^")]
    BoolXor,
    #[token("&&")]
    BoolAnd,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("&")]
    Ampersand,

    // Shifting operations
    #[token("<<")]
    Left,
    #[token(">>")]
    Right,

    // Arithmetic operations
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,

    // Explicitly named boolean operations
    #[token("$or")]
    SpecOr,
    #[token("$and")]
    SpecAnd,
    #[token("$xor")]
    SpecXor,

    // IDs, Literals
    #[regex(r"[A-Za-z_.][A-Za-z_.0-9]*", |lex| lex.slice())]
    Identifier(&'input str),
    #[token("\"")]
    StartQString,
    QString(String),
    #[regex(r"[0-9]+")]
    DecInt(&'input str),
    #[regex(r"0x(?&hex)+")]
    HexInt(&'input str),
    #[regex(r"0b[01]+")]
    BinInt(&'input str),
}

#[derive(Logos, Debug, Clone)]
#[logos(subpattern hex = r"[0-9a-fA-F]")]
#[logos(extras = Location)]
pub enum QString<'input> {
    #[error]
    Error,
    #[regex(r#"[^\\"]+"#)]
    String(&'input str),
    #[regex(r#"\\[btnfr"'\\]"#)]
    EscapeCharacter(&'input str),
    // TODO: [issue #126] {n,m} repetition range is currently unsupported.
    #[regex(r"\\u(?&hex)(?&hex)(?&hex)(?&hex)")]
    UnicodeEscape,
    // TODO: [issue #126] {n,m} repetition range is currently unsupported.
    #[regex(r"\\[0-3]?[0-7][0-7]?")]
    OctalEscape,
    #[token("\"")]
    EndString,
}

pub struct Tokenizer<'input> {
    lex: Lexer<'input, Token<'input>>,
}

impl<'input> Tokenizer<'input> {
    pub fn new(input: &'input str) -> Self {
        let lex = Token::lexer(input);
        Self { lex }
    }
}

#[derive(Debug)]
pub struct LexicalError {
    error: String,
    filename: String,
    span: Span,
    lineno: usize,
}
impl LexicalError {
    fn new(error: impl ToString, span: Span, location: &Location) -> Self {
        Self {
            error: error.to_string(),
            filename: location.filename().to_string(),
            span: Span {
                start: span.start - location.span().start,
                end: span.end - location.span().end,
            },
            lineno: location.lineno(),
        }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<(usize, Token<'input>, usize), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lex.next()?;
        let span = self.lex.span();
        match token {
            Token::UnexpectedToken => {
                let error = format!("Unknown token: {}", self.lex.slice());
                Some(Err(LexicalError::new(error, span, &self.lex.extras)))
            }
            Token::StartQString => {
                let mut result = String::new();
                let mut lex = self.lex.to_owned().morph();
                loop {
                    match lex.next() {
                        Some(QString::String(_)) => result += lex.slice(),
                        Some(QString::EscapeCharacter(_)) => {
                            match lex.slice().chars().nth(1).unwrap() {
                                'b' => result.push('\u{0008}'),
                                't' => result.push('\t'),
                                'n' => result.push('\n'),
                                'f' => result.push('\u{000c}'),
                                'r' => result.push('\r'),
                                '"' => result.push('"'),
                                '\'' => result.push('\''),
                                '\\' => result.push('\\'),
                                c => {
                                    return Some(Err(LexicalError::new(
                                        format!("Unknown escape character: {}", c),
                                        lex.span(),
                                        &lex.extras,
                                    )))
                                }
                            }
                        }
                        Some(QString::UnicodeEscape) => {
                            let slice = lex.slice();
                            let hex = &slice[2..slice.len()];
                            result.push(
                                u32::from_str_radix(hex, 16)
                                    .map(|c| std::char::from_u32(c).unwrap())
                                    .unwrap(),
                            );
                        }
                        Some(QString::OctalEscape) => {
                            let slice = lex.slice();
                            let oct = &slice[1..slice.len()];
                            result.push(
                                u32::from_str_radix(oct, 8)
                                    .map(|c| std::char::from_u32(c).unwrap())
                                    .unwrap(),
                            );
                        }
                        Some(QString::EndString) => break,
                        Some(QString::Error) => {
                            return Some(Err(LexicalError::new(
                                format!("Unexpected string: {}", lex.slice()),
                                lex.span(),
                                &lex.extras,
                            )))
                        }
                        None => {
                            return Some(Err(LexicalError::new(
                                "Unclosed string",
                                span.start..lex.span().end,
                                &lex.extras,
                            )))
                        }
                    }
                }
                self.lex = lex.morph::<Token>();
                Some(Ok((
                    span.start,
                    Token::QString(result),
                    self.lex.span().end,
                )))
            }
            _ => Some(Ok((span.start, token, span.end))),
        }
    }
}
