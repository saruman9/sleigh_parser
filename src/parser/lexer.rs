use std::{collections::HashMap, fs::read_to_string, path::PathBuf};

use log::{debug, trace};
use logos::Logos;
use regex::Regex;

use super::{
    boolean_expression::parse_boolean_expression, conditional_helper::ConditionalHelper,
    location::Location,
};

pub type LexicalResult<T> = Result<T, LexicalError>;
pub type SpannedToken<Token, Location> = LexicalResult<(Location, Token, Location)>;
// TODO: Parse String to Tokens and add Location
pub type Definitions = HashMap<String, (Vec<SpannedToken<Token, Location>>, String)>;

lazy_static::lazy_static! {
    static ref EXPANSION_RE: Regex = Regex::new(r"\$\(([0-9A-Z_a-z]+)\)").unwrap();
    static ref INCLUDE_RE: Regex = Regex::new(r#"@include\s+"(.*)""#).unwrap();
    static ref DEFINE_QSTRING_RE: Regex = Regex::new(r#"@define\s+([0-9A-Z_a-z]+)\s+"(.*)"\s*"#).unwrap();
    static ref DEFINE_STRING_RE: Regex = Regex::new(r"@define\s+([0-9A-Z_a-z]+)\s+(\S+)\s*").unwrap();
    static ref DEFINE_RE: Regex = Regex::new(r"@define\s+([0-9A-Z_a-z]+)\s*").unwrap();
    static ref UNDEF_RE: Regex = Regex::new(r"@undef\s+([0-9A-Z_a-z]+)\s*").unwrap();
    static ref IFDEF_RE: Regex = Regex::new(r"@ifdef\s+([0-9A-Z_a-z]+)\s*").unwrap();
    static ref IFNDEF_RE: Regex = Regex::new(r"@ifndef\s+([0-9A-Z_a-z]+)\s*").unwrap();
    static ref IF_RE: Regex = Regex::new(r"@if\s+(.*)").unwrap();
    static ref ELIF_RE: Regex = Regex::new(r"@elif\s+(.*)").unwrap();
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
    #[regex(r"@undef(?&w)+[0-9A-Z_a-z]+")]
    PreprocUndef,
    #[regex(r"@ifdef(?&w)+[0-9A-Z_a-z]+")]
    PreprocIfDef,
    #[regex(r"@ifndef(?&w)+[0-9A-Z_a-z]+")]
    PreprocIfNDef,
    #[regex(r"@if(?&w)+[^\n\r#]*")]
    PreprocIf,
    #[regex(r"@elif(?&w)+[^\n\r#]*")]
    PreprocElIf,
    #[token("@endif")]
    PreprocEndIf,
    #[token("@else")]
    PreprocElse,
    #[regex(r"\$\(([0-9A-Z_a-z]+)\)")]
    PreprocExpansion,
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
pub struct Lexer {
    definitions: Option<Definitions>,
    ifstack: Vec<ConditionalHelper>,
    location: Location,
}

impl Lexer {
    pub fn new(slaspec_path: impl Into<PathBuf>) -> Self {
        let slaspec_path = slaspec_path.into();
        debug!(r#"new tokenizer "{}""#, slaspec_path.display());
        Self {
            definitions: Some(Definitions::new()),
            ifstack: vec![ConditionalHelper::new(false, false, false, true)],
            location: Location::new(slaspec_path),
        }
    }

    pub fn with_definitions(mut self, definitions: &Definitions) -> Self {
        debug!("with definitions: {:?}", definitions);
        self.definitions = Some(definitions.clone());
        self
    }

    fn from_definition(name: impl Into<String>, location: &Location) -> Self {
        let name = name.into();
        debug!(r#"new tokenizer from definition "{}""#, name);
        Self {
            definitions: Some(Definitions::new()),
            ifstack: vec![ConditionalHelper::new(false, false, false, true)],
            location: location.clone().with_definition(name),
        }
    }

    pub fn definitions(&self) -> &Definitions {
        self.definitions.as_ref().unwrap()
    }

    pub fn mut_definitions(&mut self) -> &mut Definitions {
        self.definitions.as_mut().unwrap()
    }

    fn take_definitions(&mut self) -> Definitions {
        self.definitions.take().unwrap()
    }

    pub fn contains_definition(&self, key: impl AsRef<str>) -> bool {
        self.definitions
            .as_ref()
            .unwrap()
            .contains_key(key.as_ref())
    }

    fn add_pos(&mut self, diff: usize) -> (Location, Location) {
        let start = self.location.clone();
        self.location.add_pos(diff);
        (start, self.location.clone())
    }

    fn handle_expression(
        &mut self,
        expression: impl AsRef<str>,
        start: &Location,
        end: &Location,
    ) -> LexicalResult<()> {
        let expression = expression.as_ref();
        if self.is_handled() {
            self.set_copy(false);
            debug!("already handled");
        } else if !self.parse_expression(expression, start, end)? {
            self.set_copy(false);
            debug!("expression \"{}\" is FALSE", expression);
        } else {
            self.set_copy(true);
            self.set_handled(true);
            debug!("expression \"{}\" is true", expression);
        }
        Ok(())
    }

    fn parse_expression(
        &self,
        expression: impl AsRef<str>,
        start: &Location,
        end: &Location,
    ) -> LexicalResult<bool> {
        let expression = expression.as_ref();
        parse_boolean_expression(expression, self.definitions(), start, end)
    }

    fn handle_expansion_in_include(
        &self,
        input: impl Into<String>,
        start: &Location,
        end: &Location,
    ) -> LexicalResult<String> {
        let mut input = input.into();
        let mut output = String::new();
        while let Some(m) = EXPANSION_RE.captures(&input) {
            let expansion_match = m.get(0).unwrap();
            let expansion = expansion_match.as_str();
            trace!(r#"found expansion: "{}""#, expansion);
            let variable = m.get(1).unwrap().as_str();
            let definiton = self.definitions().get(variable).ok_or_else(|| {
                LexicalError::new(format!(r#"unknown variable: "{}""#, variable), start, end)
            })?;
            output.push_str(input.get(0..expansion_match.start()).unwrap());
            output.push_str(&definiton.1);
            input = input.get(expansion_match.end()..).unwrap().to_string();
        }
        output.push_str(&input);
        Ok(output)
    }

    fn handle_expansion(
        &self,
        input: impl AsRef<str>,
        start: &Location,
        end: &Location,
    ) -> Vec<SpannedToken<Token, Location>> {
        let mut output = Vec::new();
        let variable = EXPANSION_RE
            .captures(input.as_ref())
            .unwrap()
            .get(1)
            .unwrap()
            .as_str();
        let definiton = match self.definitions().get(variable) {
            Some(d) => d,
            None => {
                output.push(Err(LexicalError::new(
                    format!(r#"unknown variable: "{}""#, variable),
                    start,
                    end,
                )));
                return output;
            }
        };
        definiton.0.clone()
    }

    pub fn tokenize(&mut self, input: impl AsRef<str>) -> Vec<SpannedToken<Token, Location>> {
        let mut tokens = Vec::new();
        let mut lexer = Token::lexer(input.as_ref());
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
                    let (start, end) = self.add_pos(lexer.slice().len());
                    let err = Err(LexicalError::new(
                        format!("Unknown token: {}", lexer.slice()),
                        &start,
                        &end,
                    ));
                    debug!("{:?}", err);
                    tokens.push(err);
                }
                Token::CPPComment => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    let err = Err(LexicalError::new(
                        "C++ commentaries are not allowed",
                        &start,
                        &end,
                    ));
                    debug!("{:?}", err);
                    tokens.push(err);
                }
                Token::StartQString => {
                    let mut result = String::new();
                    let mut lexer_qstr = lexer.to_owned().morph::<QString>();
                    let (start, _) = self.add_pos(1);
                    loop {
                        let token = match lexer_qstr.next() {
                            Some(t) => {
                                trace!("{:?}", t);
                                t
                            }
                            None => {
                                let (_, end) = self.add_pos(lexer_qstr.slice().len());
                                let err = Err(LexicalError::new("Unclosed string", &start, &end));
                                debug!("{:?}", err);
                                tokens.push(err);
                                break;
                            }
                        };
                        match token {
                            QString::String => {
                                let slice = lexer_qstr.slice();
                                self.add_pos(slice.len());
                                result += slice;
                            }
                            QString::EscapeCharacter => {
                                let slice = lexer_qstr.slice();
                                let (_, end) = self.add_pos(slice.len());

                                match slice.chars().nth(1).unwrap() {
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
                                            &start,
                                            &end,
                                        ));
                                        debug!("{:?}", err);
                                        tokens.push(err);
                                        break;
                                    }
                                }
                            }
                            QString::UnicodeEscape => {
                                let slice = lexer_qstr.slice();
                                self.add_pos(slice.len());
                                let hex = &slice[2..slice.len()];
                                result.push(
                                    u32::from_str_radix(hex, 16)
                                        .map(|c| std::char::from_u32(c).unwrap())
                                        .unwrap(),
                                );
                            }
                            QString::OctalEscape => {
                                let slice = lexer_qstr.slice();
                                self.add_pos(slice.len());
                                let oct = &slice[1..slice.len()];
                                result.push(
                                    u32::from_str_radix(oct, 8)
                                        .map(|c| std::char::from_u32(c).unwrap())
                                        .unwrap(),
                                );
                            }
                            QString::EndString => {
                                let (_, end) = self.add_pos(1);
                                lexer = lexer_qstr.morph::<Token>();
                                if !self.is_copy() {
                                    break;
                                }
                                let ok = Ok((start, Token::QString(result), end));
                                debug!("{:?}", ok);
                                tokens.push(ok);
                                break;
                            }
                            QString::Error => {
                                let slice = lexer_qstr.slice();
                                let (_, end) = self.add_pos(slice.len());
                                let err = Err(LexicalError::new(
                                    format!("Unexpected string: {}", slice),
                                    &start,
                                    &end,
                                ));
                                debug!("{:?}", err);
                                tokens.push(err);
                                break;
                            }
                        }
                    }
                }
                Token::LineComment | Token::Whitespace => {
                    self.add_pos(lexer.slice().len());
                    continue;
                }
                Token::PreprocInclude => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let input = INCLUDE_RE
                        .captures(&slice)
                        .unwrap()
                        .get(1)
                        .unwrap()
                        .as_str();
                    let include_file_str =
                        match self.handle_expansion_in_include(input, &start, &end) {
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
                        include_file_path = self
                            .location
                            .path()
                            .to_path_buf()
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
                            &start,
                            &end,
                        ));
                        debug!("{:?}", err);
                        tokens.push(err);
                        continue;
                    }
                    let ok = (start, token, end);
                    debug!("include file: '{}', {:?}", include_file_path.display(), ok);
                    let mut tokenizer =
                        Lexer::new(&include_file_path).with_definitions(self.definitions());
                    tokens.extend(tokenizer.tokenize(read_to_string(&include_file_path).unwrap()));
                    self.definitions = Some(tokenizer.take_definitions());
                }
                Token::PreprocDefineQString => {
                    let slice = lexer.slice();
                    let (mut start, _) = self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let m = DEFINE_QSTRING_RE.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str();
                    let value_match = m.get(2).unwrap();
                    start.add_pos(value_match.start());
                    let value = value_match.as_str();
                    self.define(key, Some(value), &start);
                }
                Token::PreprocDefineString => {
                    let slice = lexer.slice();
                    let (mut start, _) = self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let m = DEFINE_STRING_RE.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str();
                    let value_match = m.get(2).unwrap();
                    start.add_pos(value_match.start());
                    let value = value_match.as_str();
                    self.define(key, Some(value), &start);
                }
                Token::PreprocDefine => {
                    let slice = lexer.slice();
                    let (start, _) = self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let m = DEFINE_RE.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str().to_string();
                    self.define(key, None, &start);
                }
                Token::PreprocUndef => {
                    let slice = lexer.slice();
                    self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let key = UNDEF_RE.captures(slice).unwrap().get(1).unwrap().as_str();
                    self.undefine(key);
                }
                Token::PreprocIfDef => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    self.enter_if();

                    let key = IFDEF_RE.captures(slice).unwrap().get(1).unwrap().as_str();
                    if self.contains_definition(key) {
                        self.set_handled(true);
                        debug!(
                            r#"@ifdef "{}": yes (start: {:?}, end: {:?})"#,
                            key, &start, &end
                        );
                    } else {
                        self.set_copy(false);
                        debug!(
                            r#"@ifdef "{}": NO (start: {:?}, end: {:?})"#,
                            key, &start, &end
                        );
                    }
                }
                Token::PreprocIfNDef => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    self.enter_if();

                    let key = IFNDEF_RE.captures(slice).unwrap().get(1).unwrap().as_str();
                    if self.contains_definition(key) {
                        self.set_copy(false);
                        debug!(
                            r#"@ifndef "{}": NO (start: {:?}, end: {:?})"#,
                            key, &start, &end
                        );
                    } else {
                        self.set_handled(true);
                        debug!(
                            r#"@ifndef "{}": yes (start: {:?}, end: {:?})"#,
                            key, &start, &end
                        );
                    }
                }
                Token::PreprocIf => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    self.enter_if();

                    let m = IF_RE.captures(slice).unwrap().get(1).unwrap().as_str();
                    debug!(r#"@if... "{}" (start: {:?}, end: {:?})"#, m, &start, &end);
                    if let Err(e) = self.handle_expression(m, &start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                    }
                }
                Token::PreprocElIf => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    if let Err(e) = self.enter_elif(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                    }

                    let m = ELIF_RE.captures(slice).unwrap().get(1).unwrap().as_str();
                    debug!(r#"@elif... "{}" (start: {:?}, end: {:?})"#, m, &start, &end);
                    if let Err(e) = self.handle_expression(m, &start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                    }
                }
                Token::PreprocEndIf => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    debug!(r"@endif (start: {:?}, end: {:?})", &start, &end);
                    if let Err(e) = self.leave_if(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                    }
                }
                Token::PreprocElse => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    debug!(r"@else (start: {:?}, end: {:?})", &start, &end);
                    if let Err(e) = self.enter_else(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                        continue;
                    }
                    self.set_copy(!self.is_handled());
                }
                Token::PreprocExpansion => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let expansion_tokens = self.handle_expansion(slice, &start, &end);
                    debug!("expansion (start: {:?}, end: {:?})", &start, &end);
                    debug!("tokens: {:?}", expansion_tokens);
                    tokens.extend(expansion_tokens);
                }
                _ => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let ok = Ok((start, token, end));
                    debug!("{:?}", &ok);
                    tokens.push(ok);
                }
            }
        }
    }

    fn define<S>(&mut self, key: S, value: Option<S>, start: &Location)
    where
        S: Into<String>,
    {
        let key = key.into();
        if value.is_none() {
            debug!(r#"@define key: "{}""#, &key,);
            self.mut_definitions()
                .insert(key, (Vec::new(), String::new()));
            return;
        }
        let value = value.unwrap().into();

        let mut tokenizer =
            Lexer::from_definition(&key, &start).with_definitions(self.definitions());
        let tokens = tokenizer.tokenize(&value);
        debug!(
            r#"@define key: "{}" value: "{}" tokens: "{:?}""#,
            &key, &value, &tokens
        );
        self.mut_definitions().insert(key, (tokens, value));
    }

    fn undefine(&mut self, key: impl Into<String>) {
        let key = key.into();
        trace!(r#"@undef "{}""#, key);
        self.mut_definitions().remove(&key);
    }

    fn enter_if(&mut self) {
        self.ifstack
            .push(ConditionalHelper::new(true, false, false, self.is_copy()));
    }

    fn enter_elif(&mut self, start: &Location, end: &Location) -> LexicalResult<()> {
        if !self.is_in_if() {
            return Err(LexicalError::new(
                "elif outside of IF* directive",
                start,
                end,
            ));
        }
        if self.is_saw_else() {
            return Err(LexicalError::new("already saw else directive", start, end));
        }
        Ok(())
    }

    fn leave_if(&mut self, start: &Location, end: &Location) -> LexicalResult<()> {
        if !self.is_in_if() {
            return Err(LexicalError::new("not in IF* directive", start, end));
        }
        self.ifstack.pop();
        Ok(())
    }

    fn enter_else(&mut self, start: &Location, end: &Location) -> LexicalResult<()> {
        if !self.is_in_if() {
            return Err(LexicalError::new(
                "else outside of IF* directive",
                start,
                end,
            ));
        }
        if self.is_saw_else() {
            return Err(LexicalError::new("duplicate else directive", start, end));
        }
        self.set_saw_else(true);
        Ok(())
    }

    // Functions for checking/setting the ifstack. The ifstack always must be not empty.

    fn is_in_if(&self) -> bool {
        self.ifstack.last().unwrap().in_if()
    }

    fn set_saw_else(&mut self, is_saw_else: bool) {
        self.ifstack.last_mut().unwrap().set_saw_else(is_saw_else)
    }

    fn is_saw_else(&self) -> bool {
        self.ifstack.last().unwrap().saw_else()
    }

    fn set_copy(&mut self, is_copy: bool) {
        self.ifstack.last_mut().unwrap().set_copy(is_copy);
    }

    fn is_copy(&self) -> bool {
        self.ifstack.iter().all(|x| x.copy())
    }

    fn set_handled(&mut self, is_handled: bool) {
        self.ifstack.last_mut().unwrap().set_handled(is_handled);
    }

    fn is_handled(&self) -> bool {
        self.ifstack.last().unwrap().handled()
    }
}

#[derive(Debug, Clone)]
pub struct LexicalError {
    error: String,
    start: Location,
    end: Location,
}

impl LexicalError {
    pub(crate) fn new(error: impl ToString, start: &Location, end: &Location) -> Self {
        Self {
            error: error.to_string(),
            start: start.clone(),
            end: end.clone(),
        }
    }
}
