use std::{fs::read_to_string, path::PathBuf};

use log::{info, trace};
use logos::{Lexer, Logos, Span};

use super::location::Location;

#[derive(Logos, Debug, Clone)]
#[logos(subpattern hex = r"[0-9a-fA-F]")]
pub enum Token {
    #[error]
    UnexpectedToken,

    // WhiteSpaces
    #[regex(r"[ \t]+")]
    #[regex(r"\r?\n?")]
    #[regex(r"#[^\n\r]*\r?\n?")]
    LineComment,
    #[token("//")]
    CPPComment,

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
    #[regex(r"[A-Za-z_.][A-Za-z_.0-9]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[token("\"")]
    StartQString,
    QString(String),
    #[regex(r"[0-9]+", |lex| lex.slice().to_string())]
    DecInt(String),
    #[regex(r"0x(?&hex)+", |lex| lex.slice().to_string())]
    HexInt(String),
    #[regex(r"0b[01]+", |lex| lex.slice().to_string())]
    BinInt(String),

    // Preprocessor
    #[regex(r#"@define\s+([0-9A-Z_a-z]+)\s+"[^"]*"\s*"#)]
    PreprocDefine,
}

#[derive(Logos, Debug, Clone)]
#[logos(subpattern hex = r"[0-9a-fA-F]")]
pub enum QString {
    #[error]
    Error,
    #[regex(r#"[^\\"]+"#, |lex| lex.slice().to_string())]
    String(String),
    #[regex(r#"\\[btnfr"'\\]"#)]
    EscapeCharacter,
    // TODO: [issue #126] {n,m} repetition range is currently unsupported.
    #[regex(r"\\u(?&hex)(?&hex)(?&hex)(?&hex)")]
    UnicodeEscape,
    // TODO: [issue #126] {n,m} repetition range is currently unsupported.
    #[regex(r"\\[0-3]?[0-7][0-7]?")]
    OctalEscape,
    #[token("\"")]
    EndString,
}

pub struct Parser {}

impl Parser {
    pub fn new(slaspec_path: impl Into<PathBuf>) -> Self {
        let file_path = slaspec_path.into();
        let input = read_to_string(&file_path).unwrap();
        let tokenizer = Tokenizer::new(&input, &file_path);
        for token in tokenizer {
            println!("{:#?}", token);
        }
        Self {}
    }
}

pub struct Tokenizer<'input> {
    lex: Lexer<'input, Token>,
    location: Location,
}

impl<'input> Tokenizer<'input> {
    pub fn new(input: &'input str, file_path: impl Into<PathBuf>) -> Self {
        let file_path = file_path.into();
        let location = Location::new(file_path.display().to_string(), 1);
        let lex = Token::lexer(&input);
        Self { lex, location }
    }
}

#[derive(Debug)]
pub struct LexicalError {
    error: String,
    start: Location,
    end: Location,
}
impl LexicalError {
    fn new(error: impl ToString, start: Location, end: Location) -> Self {
        Self {
            error: error.to_string(),
            start,
            end,
        }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<(Location, Token, Location), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let token = self.lex.next()?;
            let span = self.lex.span();
            match token {
                Token::UnexpectedToken => {
                    let start = self.location.clone();
                    self.location.add_pos(self.lex.slice().len());
                    let end = self.location.clone();
                    return Some(Err(LexicalError::new(
                        format!("Unknown token: {}", self.lex.slice()),
                        start,
                        end,
                    )));
                }
                Token::CPPComment => {
                    let start = self.location.clone();
                    self.location.add_pos(self.lex.slice().len());
                    let end = self.location.clone();
                    return Some(Err(LexicalError::new(
                        "C++ commentaries are not allowed",
                        start,
                        end,
                    )));
                }
                Token::StartQString => {
                    let mut result = String::new();
                    let mut lex = self.lex.to_owned().morph();
                    loop {
                        match lex.next() {
                            Some(QString::String(_)) => result += lex.slice(),
                            Some(QString::EscapeCharacter) => {
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
                                        let start = self.location.clone();
                                        self.location.add_pos(lex.slice().len());
                                        let end = self.location.clone();
                                        return Some(Err(LexicalError::new(
                                            format!("Unknown escape character: {}", c),
                                            start,
                                            end,
                                        )));
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
                                let start = self.location.clone();
                                self.location.add_pos(lex.slice().len());
                                let end = self.location.clone();
                                return Some(Err(LexicalError::new(
                                    format!("Unexpected string: {}", lex.slice()),
                                    start,
                                    end,
                                )));
                            }
                            None => {
                                let start = self.location.clone();
                                self.location.add_pos(lex.slice().len());
                                let end = self.location.clone();
                                return Some(Err(LexicalError::new("Unclosed string", start, end)));
                            }
                        }
                    }
                    self.lex = lex.morph::<Token>();
                    return Some(Ok((
                        self.location.clone(),
                        Token::QString(result),
                        self.location.clone(),
                    )));
                }
                Token::LineComment => {
                    self.location.add_pos(self.lex.slice().len());
                    continue;
                }
                // Token::PreprocDefine => {}
                _ => {
                    let start = self.location.clone();
                    self.location.add_pos(self.lex.slice().len());
                    let end = self.location.clone();
                    return Some(Ok((start, token, end)));
                }
            }
        }
    }
}
