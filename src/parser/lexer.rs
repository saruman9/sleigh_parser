use std::collections::HashMap;

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
        lex.extras.inc_global_lineno();
        lex.extras.inc_local_lineno();
        logos::Skip
    })]
    #[regex(r"#[^\n\r]*\r?\n?", |lex|{
        lex.extras.inc_global_lineno();
        lex.extras.inc_local_lineno();
        logos::Skip
    })]
    LineComment,
    #[token("//")]
    CPPComment,

    // Preprocessor
    #[regex(r"\x08[^\n\x08]*\x08")]
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
    locations: HashMap<String, (usize, usize)>,
    current_location: Location,
}

impl<'input> Tokenizer<'input> {
    pub fn new(input: &'input str) -> Self {
        let lex = Token::lexer(input);
        Self {
            lex,
            locations: Default::default(),
            current_location: Default::default(),
        }
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
                start: span.start - location.global_pos(),
                end: span.end - location.global_pos(),
            },
            lineno: location.local_lineno(),
        }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<(Location, Token<'input>, Location), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let token = self.lex.next()?;
            let span = self.lex.span();
            let location = &self.current_location;
            match token {
                Token::UnexpectedToken => {
                    return Some(Err(LexicalError::new(
                        format!("Unknown token: {}", self.lex.slice()),
                        span,
                        location,
                    )))
                }
                Token::CPPComment => {
                    return Some(Err(LexicalError::new(
                        "C++ commentaries are not allowed",
                        span,
                        location,
                    )))
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
                    return Some(Ok((
                        self.lex.extras.clone(),
                        Token::QString(result),
                        self.lex.extras.clone(),
                    )));
                }
                Token::PPPosition => {
                    let split: Vec<&str> =
                        self.lex.slice().trim_matches('\x08').split("###").collect();
                    if split.len() == 2 {
                        let new_filename = split[0];
                        let current_filename = location.filename();
                        if current_filename.is_empty() {
                            self.locations
                                .insert(new_filename.to_string(), (span.end + 1, 1));
                            self.current_location
                                .set_location(new_filename, span.end + 1, 1);
                            trace!("{:#?} {:#?}", self.locations, self.current_location,);
                            continue;
                        }
                        let value = self.locations.get_mut(current_filename).unwrap();
                        value.1 += span.end + 1 - value.0;
                        value.0 = span.end + 1;

                        let pos = self
                            .locations
                            .entry(new_filename.to_string())
                            .or_insert((span.end + 1, 1));
                        self.current_location
                            .set_location(new_filename, span.end + 1, pos.1);

                        trace!("{:#?} {:#?}", self.locations, self.current_location,);
                    }
                }
                _ => {
                    let mut start = location.clone();
                    start.set_global_pos(self.lex.span().start);
                    start.set_local_pos(
                        location.local_pos() + self.lex.span().start - location.global_pos(),
                    );
                    let mut end = location.clone();
                    end.set_global_pos(self.lex.span().end);
                    end.set_local_pos(start.local_pos() + self.lex.slice().len());
                    trace!("{:#?}", start);
                    trace!("{:?}", token);
                    trace!("{:#?}", end);
                    return Some(Ok((start, token, end)));
                }
            }
        }
    }
}
