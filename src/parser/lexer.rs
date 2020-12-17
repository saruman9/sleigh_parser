use std::{fs::read_to_string, path::PathBuf};

use log::{debug, trace};
use logos::Logos;
use regex::Regex;

use super::location::Location;

lazy_static::lazy_static! {
    static ref INCLUDE_RE: Regex = Regex::new(r#"@include\s+"(.*)""#).unwrap();
    static ref DEFINE1_RE: Regex = Regex::new(r#"^\s*@define\s+([0-9A-Z_a-z]+)\s+"(.*)"\s*$"#).unwrap();
    static ref DEFINE2_RE: Regex = Regex::new(r"^\s*@define\s+([0-9A-Z_a-z]+)\s+(\S+)\s*$").unwrap();
    static ref DEFINE3_RE: Regex = Regex::new(r"^\s*@define\s+([0-9A-Z_a-z]+)\s*$").unwrap();
    static ref UNDEF_RE: Regex = Regex::new(r"^\s*@undef\s+([0-9A-Z_a-z]+)\s*$").unwrap();
    static ref IFDEF_RE: Regex = Regex::new(r"^\s*@ifdef\s+([0-9A-Z_a-z]+)\s*$").unwrap();
    static ref IFNDEF_RE: Regex = Regex::new(r"^\s*@ifndef\s+([0-9A-Z_a-z]+)\s*$").unwrap();
    static ref IF_RE: Regex = Regex::new(r"^\s*@if\s+(.*)").unwrap();
    static ref ELIF_RE: Regex = Regex::new(r"^\s*@elif\s+(.*)").unwrap();
}

#[derive(Logos, Debug, Clone)]
#[logos(subpattern hex = r"[0-9a-fA-F]")]
#[logos(subpattern w = r"[\t\v\f ]")]
pub enum Token {
    #[error]
    UnexpectedToken,

    // Whitespaces
    #[regex(r"[ \t]+")]
    #[regex(r"\r?\n?")]
    Whitespace,
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
    #[regex(r#"@define(?&w)+[0-9A-Z_a-z]+(?&w)+"[^"\n\r]*""#, |lex| lex.slice().to_string())]
    PreprocDefineQString(String),
    #[regex(r"@define(?&w)+[0-9A-Z_a-z]+(?&w)+[\S]+", |lex| lex.slice().to_string())]
    PreprocDefineString(String),
    #[regex(r"@define(?&w)+[0-9A-Z_a-z]+", |lex| lex.slice().to_string())]
    PreprocDefine(String),
    #[regex(r#"@include(?&w)+"[^"\n\r]*""#)]
    PreprocInclude,
}

#[derive(Logos, Debug, Clone)]
#[logos(subpattern hex = r"[0-9a-fA-F]")]
pub enum QString {
    #[error]
    Error,
    #[regex(r#"[^\\"]+"#)]
    String,
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

pub struct Tokenizer {
    location: Location,
    input: String,
}

impl Tokenizer {
    pub fn new(slaspec_path: impl Into<PathBuf>) -> Self {
        let file_path = slaspec_path.into();
        let input = read_to_string(&file_path).unwrap();
        let location = Location::new(file_path.display().to_string(), 1);
        Self { location, input }
    }

    pub fn tokenize(&mut self) -> Vec<Result<(Location, Token, Location), LexicalError>> {
        let mut tokens = Vec::new();
        let mut lexer = Token::lexer(&self.input);
        loop {
            let token = match lexer.next() {
                Some(t) => {
                    trace!("{:?}", t);
                    t
                }
                None => return tokens,
            };
            match token {
                Token::UnexpectedToken => {
                    let start = self.location.clone();
                    self.location.add_pos(lexer.slice().len());
                    let end = self.location.clone();
                    let err = Err(LexicalError::new(
                        format!("Unknown token: {}", lexer.slice()),
                        start,
                        end,
                    ));
                    debug!("{:?}", err);
                    tokens.push(err);
                }
                Token::CPPComment => {
                    let start = self.location.clone();
                    self.location.add_pos(lexer.slice().len());
                    let end = self.location.clone();
                    let err = Err(LexicalError::new(
                        "C++ commentaries are not allowed",
                        start,
                        end,
                    ));
                    debug!("{:?}", err);
                    tokens.push(err);
                }
                Token::StartQString => {
                    let mut result = String::new();
                    let mut lexer_qstr = lexer.to_owned().morph::<QString>();
                    let start = self.location.clone();
                    self.location.add_pos(1);
                    loop {
                        let token = match lexer_qstr.next() {
                            Some(t) => {
                                trace!("{:?}", t);
                                t
                            }
                            None => {
                                self.location.add_pos(lexer_qstr.slice().len());
                                let err = Err(LexicalError::new(
                                    "Unclosed string",
                                    start.clone(),
                                    self.location.clone(),
                                ));
                                debug!("{:?}", err);
                                tokens.push(err);
                                break;
                            }
                        };
                        match token {
                            QString::String => {
                                let slice = lexer_qstr.slice();
                                self.location.add_pos(slice.len());
                                result += slice;
                            }
                            QString::EscapeCharacter => {
                                self.location.add_pos(lexer_qstr.slice().len());

                                match lexer_qstr.slice().chars().nth(1).unwrap() {
                                    'b' => result.push('\u{0008}'),
                                    't' => result.push('\t'),
                                    'n' => result.push('\n'),
                                    'f' => result.push('\u{000c}'),
                                    'r' => result.push('\r'),
                                    '"' => result.push('"'),
                                    '\'' => result.push('\''),
                                    '\\' => result.push('\\'),
                                    c => {
                                        let err = Err(LexicalError::new(
                                            format!("Unknown escape character: {}", c),
                                            start.clone(),
                                            self.location.clone(),
                                        ));
                                        debug!("{:?}", err);
                                        tokens.push(err);
                                        break;
                                    }
                                }
                            }
                            QString::UnicodeEscape => {
                                let slice = lexer_qstr.slice();
                                self.location.add_pos(slice.len());
                                let hex = &slice[2..slice.len()];
                                result.push(
                                    u32::from_str_radix(hex, 16)
                                        .map(|c| std::char::from_u32(c).unwrap())
                                        .unwrap(),
                                );
                            }
                            QString::OctalEscape => {
                                let slice = lexer_qstr.slice();
                                self.location.add_pos(slice.len());
                                let oct = &slice[1..slice.len()];
                                result.push(
                                    u32::from_str_radix(oct, 8)
                                        .map(|c| std::char::from_u32(c).unwrap())
                                        .unwrap(),
                                );
                            }
                            QString::EndString => {
                                self.location.add_pos(1);
                                break;
                            }
                            QString::Error => {
                                self.location.add_pos(lexer_qstr.slice().len());
                                let err = Err(LexicalError::new(
                                    format!("Unexpected string: {}", lexer_qstr.slice()),
                                    start.clone(),
                                    self.location.clone(),
                                ));
                                debug!("{:?}", err);
                                tokens.push(err);
                                break;
                            }
                        }
                    }
                    lexer = lexer_qstr.morph::<Token>();
                    let ok = Ok((start, Token::QString(result), self.location.clone()));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                }
                Token::LineComment | Token::Whitespace => {
                    self.location.add_pos(lexer.slice().len());
                    continue;
                }
                // Token::PreprocDefineQString(_) => {
                //     let start = self.location.clone();
                //     self.location.add_pos(self.lexer().slice().len());
                //     let end = self.location.clone();
                //     return Some(Ok((start, token, end)));
                // }
                // Token::PreprocDefineString(_) => {
                //     let start = self.location.clone();
                //     self.location.add_pos(self.lexer().slice().len());
                //     let end = self.location.clone();
                //     return Some(Ok((start, token, end)));
                // }
                // Token::PreprocDefine(_) => {
                //     let start = self.location.clone();
                //     self.location.add_pos(self.lexer().slice().len());
                //     let end = self.location.clone();
                //     return Some(Ok((start, token, end)));
                // }
                Token::PreprocInclude => {
                    let slice = lexer.slice();
                    let start = self.location.clone();
                    self.location.add_pos(slice.len());

                    let m = INCLUDE_RE.captures(&slice).unwrap();
                    let mut include_file_path = PathBuf::from(m.get(1).unwrap().as_str());
                    if include_file_path.is_relative() {
                        include_file_path = PathBuf::from(self.location.path())
                            .parent()
                            .unwrap()
                            .join(include_file_path);
                    }
                    if !include_file_path.exists() {
                        let err = Err(LexicalError::new(
                            format!(
                                "included file \"{}\" does not exist",
                                include_file_path.display()
                            ),
                            start.clone(),
                            self.location.clone(),
                        ));
                        debug!("{:?}", err);
                        tokens.push(err);
                    }
                    let ok = (start, token, self.location.clone());
                    debug!("{:?}, '{}'", ok, include_file_path.display());
                    let mut tokenizer = Tokenizer::new(include_file_path);
                    tokens.extend(tokenizer.tokenize());
                }
                _ => {
                    let start = self.location.clone();
                    self.location.add_pos(lexer.slice().len());
                    let end = self.location.clone();
                    let ok = Ok((start, token, end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                }
            }
        }
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
