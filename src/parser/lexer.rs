use std::{collections::HashMap, fs::read_to_string, path::PathBuf};

use log::{debug, trace};
use logos::Logos;
use regex::Regex;

use super::{conditional_helper::ConditionalHelper, location::Location};

// TODO: Parse String to Tokens and add Location
pub type Definitions = HashMap<
    String,
    (
        Vec<Result<(Location, Token, Location), LexicalError>>,
        String,
    ),
>;

lazy_static::lazy_static! {
    static ref EXPANSION_RE: Regex = Regex::new(r"\$\(([0-9A-Z_a-z]+)\)").unwrap();
    static ref INCLUDE_RE: Regex = Regex::new(r#"@include\s+"(.*)""#).unwrap();
    static ref DEFINE_QSTRING: Regex = Regex::new(r#"@define\s+([0-9A-Z_a-z]+)\s+"(.*)"\s*"#).unwrap();
    static ref DEFINE_STRING: Regex = Regex::new(r"@define\s+([0-9A-Z_a-z]+)\s+(\S+)\s*").unwrap();
    static ref DEFINE: Regex = Regex::new(r"@define\s+([0-9A-Z_a-z]+)\s*").unwrap();
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
    #[regex(r#"@define(?&w)+[0-9A-Z_a-z]+(?&w)+"[^"\n\r]*""#)]
    PreprocDefineQString,
    #[regex(r"@define(?&w)+[0-9A-Z_a-z]+(?&w)+[\S]+")]
    PreprocDefineString,
    #[regex(r"@define(?&w)+[0-9A-Z_a-z]+")]
    PreprocDefine,
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

#[derive(Debug)]
pub struct Tokenizer {
    definitions: Option<Definitions>,
    location: Location,
    ifstack: Vec<ConditionalHelper>,
    input: String,
}

impl Tokenizer {
    pub fn new(slaspec_path: impl Into<PathBuf>) -> Self {
        let file_path = slaspec_path.into();
        Self {
            definitions: Some(HashMap::new()),
            location: Location::new(file_path.display().to_string(), 1),
            ifstack: Vec::new(),
            input: read_to_string(&file_path).unwrap(),
        }
    }

    pub fn from_string<S>(name: S, input: S, location: Location) -> Self
    where
        S: Into<String>,
    {
        Self {
            definitions: Some(HashMap::new()),
            location: location.with_definition(name, 1),
            ifstack: Vec::new(),
            input: input.into(),
        }
    }

    pub fn with_definitions(mut self, definitions: &Definitions) -> Self {
        debug!("definitions: {:?}", definitions);
        self.definitions = Some(definitions.clone());
        self
    }

    pub fn definitions(&self) -> &Definitions {
        self.definitions.as_ref().unwrap()
    }

    fn handle_definitions_in_include<S: Into<String>>(
        &self,
        input: S,
        start: Location,
        end: Location,
    ) -> Result<String, LexicalError> {
        let mut input = input.into();
        let mut output = String::new();
        while let Some(m) = EXPANSION_RE.captures(&input) {
            // TODO: Locate the expression with `Location`
            let expansion_match = m.get(0).unwrap();
            let expansion = expansion_match.as_str();
            trace!("found expansion: {}", expansion);
            let variable = m.get(1).unwrap().as_str();
            let start = start.clone();
            let end = end.clone();
            let definiton = self
                .definitions
                .as_ref()
                .unwrap()
                .get(variable)
                .ok_or_else(move || {
                    LexicalError::new(format!("unknown variable: {}", variable), start, end)
                })?;
            output.push_str(input.get(0..expansion_match.start()).unwrap());
            output.push_str(&definiton.1);
            input = input.get(expansion_match.end()..).unwrap().to_string();
        }
        output.push_str(&input);
        Ok(output)
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
                                lexer = lexer_qstr.morph::<Token>();
                                let ok = Ok((start, Token::QString(result), self.location.clone()));
                                debug!("{:?}", ok);
                                tokens.push(ok);
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
                }
                Token::LineComment | Token::Whitespace => {
                    self.location.add_pos(lexer.slice().len());
                    continue;
                }
                Token::PreprocInclude => {
                    let slice = lexer.slice();
                    let start = self.location.clone();
                    self.location.add_pos(slice.len());
                    let end = self.location.clone();

                    let m = INCLUDE_RE.captures(&slice).unwrap();
                    let include_file_str = match self.handle_definitions_in_include(
                        m.get(1).unwrap().as_str(),
                        start.clone(),
                        end.clone(),
                    ) {
                        Ok(s) => s,
                        Err(e) => {
                            let err = Err(e);
                            debug!("{:?}", err);
                            tokens.push(err);
                            continue;
                        }
                    };
                    let mut include_file_path = PathBuf::from(include_file_str);
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
                            end,
                        ));
                        debug!("{:?}", err);
                        tokens.push(err);
                        continue;
                    }
                    let ok = (start, token, end);
                    debug!("include file: '{}', {:?}", include_file_path.display(), ok);
                    let mut tokenizer =
                        Tokenizer::new(include_file_path).with_definitions(self.definitions());
                    tokens.extend(tokenizer.tokenize());
                }
                Token::PreprocDefineQString => {
                    let mut start = self.location.clone();
                    self.location.add_pos(lexer.slice().len());

                    let slice = lexer.slice();
                    let m = DEFINE_QSTRING.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str().to_string();
                    let value_match = m.get(2).unwrap();
                    start.add_pos(value_match.start());
                    let value = value_match.as_str().to_string();
                    let mut tokenizer = Tokenizer::from_string(key.clone(), value.clone(), start)
                        .with_definitions(self.definitions());
                    let tokens = tokenizer.tokenize();
                    debug!(
                        "@define key: '{}' value: '{}' tokens: '{:?}'",
                        &key, &value, &tokens
                    );
                    self.definitions
                        .as_mut()
                        .unwrap()
                        .insert(key, (tokens, value));
                }
                Token::PreprocDefineString => {
                    let mut start = self.location.clone();
                    self.location.add_pos(lexer.slice().len());

                    let slice = lexer.slice();
                    let m = DEFINE_STRING.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str().to_string();
                    let value_match = m.get(2).unwrap();
                    start.add_pos(value_match.start());
                    let value = value_match.as_str().to_string();
                    let mut tokenizer = Tokenizer::from_string(key.clone(), value.clone(), start)
                        .with_definitions(self.definitions());
                    let tokens = tokenizer.tokenize();
                    debug!(
                        "@define key: '{}' value: '{}' tokens: '{:?}'",
                        &key, &value, &tokens
                    );
                    self.definitions
                        .as_mut()
                        .unwrap()
                        .insert(key, (tokens, value));
                }
                Token::PreprocDefine => {
                    self.location.add_pos(lexer.slice().len());

                    let slice = lexer.slice();
                    let m = DEFINE.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str().to_string();
                    debug!("@define key: '{}'", key);
                    self.definitions
                        .as_mut()
                        .unwrap()
                        .insert(key, (Vec::new(), String::new()));
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

#[derive(Debug, Clone)]
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
