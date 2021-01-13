use std::{collections::HashMap, fs::read_to_string, path::PathBuf};

use log::{debug, trace};
use logos::{self, Logos};
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
    PatternBlock,
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
    DefineBlock,
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
    PrintBlock,
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

    // Print block
    CharPrint(char),
    IsPrint,
    SymbolStringPrint(String),
    WhitespacePrint,

    // Define block
    LParenDef,
    RParenDef,
    CommaDef,
    AssignDef,
    ColonDef,
    LBracketDef,
    RBracketDef,
    SemiDef,
    SpaceDef,
    TypeDef,
    RamSpaceDef,
    DefaultDef,
    RegisterSpaceDef,
    TokenDef,
    ContextDef,
    BitrangeDef,
    SignedDef,
    NoflowDef,
    HexDef,
    DecDef,
    EndianDef,
    AlignmentDef,
    BigDef,
    LittleDef,
    SizeDef,
    WordsizeDef,
    OffsetDef,
    NamesDef,
    ValuesDef,
    VariablesDef,
    PcodeopDef,

    // Pattern block
    LBracePat,
    UnimplPat,
    GlobalsetPat,
    RightPat,
    LeftPat,
    NotEqualPat,
    LessEqualPat,
    GreatEqualPat,
    SpecAndPat,
    SpecOrPat,
    SpecXorPat,
    EllipsisPat,
    LBracketPat,
    RBracketPat,
    AmpersandPat,
    AndPat,
    PipePat,
    OrPat,
    CaretPat,
    AssignPat,
    LParenPat,
    RParenPat,
    CommaPat,
    ColonPat,
    SemiPat,
    PlusPat,
    MinusPat,
    AsteriskPat,
    SlashPat,
    TildePat,
    LessPat,
    GreatPat,
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

#[derive(Logos, Debug, Clone)]
#[logos(subpattern w = r"[\t\v\f ]")]
pub enum PrintBlock {
    #[error]
    Error,

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

    // Main
    #[regex(r#"[~!@#$%&*()\-=+\[\]{}|;:<>?,/0-9]"#, |lex| lex.slice().chars().next().unwrap())]
    Char(char),
    #[token("^")]
    Caret,
    #[token("is")]
    Is,
    #[regex(r"[a-zA-Z_.][a-zA-Z0-9_.]*", |lex| lex.slice().to_string())]
    SymbolString(String),
    #[regex(r#""([^"]|"")*""#, |lex| {
        let slice = lex.slice();
        slice.get(1..slice.len() - 1).unwrap().to_string()
    })]
    QString(String),
    #[regex(r"[\r \t\v]+")]
    Whitespace,
    #[regex(r"\n")]
    Newline,
}

#[derive(Logos, Debug, Clone)]
#[logos(subpattern w = r"[\t\v\f ]")]
pub enum DefineBlock {
    #[error]
    Error,

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

    // Main
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token("=")]
    Assign,
    #[token(":")]
    Colon,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(";")]
    Semi,
    #[token("space")]
    Space,
    #[token("type")]
    Type,
    #[token("ram_space")]
    RamSpace,
    #[token("default")]
    Default,
    #[token("register_space")]
    RegisterSpace,
    #[token("token")]
    Token,
    #[token("context")]
    Context,
    #[token("bitrange")]
    Bitrange,
    #[token("signed")]
    Signed,
    #[token("noflow")]
    Noflow,
    #[token("hex")]
    Hex,
    #[token("dec")]
    Dec,
    #[token("endian")]
    Endian,
    #[token("alignment")]
    Alignment,
    #[token("big")]
    Big,
    #[token("little")]
    Little,
    #[token("size")]
    Size,
    #[token("wordsize")]
    Wordsize,
    #[token("offset")]
    Offset,
    #[token("names")]
    Names,
    #[token("values")]
    Values,
    #[token("variables")]
    Variables,
    #[token("pcodeop")]
    Pcodeop,
    #[regex(r"#[^\n\r]*")]
    Comment,
    #[regex(r"[a-zA-Z_.][a-zA-Z0-9_.]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r"[0-9]|[1-9][0-9]+", |lex| lex.slice().to_string())]
    DecInt(String),
    #[regex(r"0x[0-9a-fA-F]+", |lex| lex.slice().to_string())]
    HexInt(String),
    #[regex(r"0b[01]+", |lex| lex.slice().to_string())]
    BinInt(String),
    #[regex(r#""([^"]|"")*""#, |lex| {
        let slice = lex.slice();
        slice.get(1..slice.len() - 1).unwrap().to_string()
    })]
    QString(String),
    #[regex(r"[\r \t\v]+")]
    Whitespace,
    #[regex(r"\n")]
    Newline,
}

#[derive(Logos, Debug, Clone)]
#[logos(subpattern w = r"[\t\v\f ]")]
pub enum PatternBlock {
    #[error]
    Error,

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

    // Main
    #[token("{")]
    LBrace,
    #[token("unimpl")]
    Unimpl,
    #[token("globalset")]
    Globalset,
    #[token(">>")]
    Right,
    #[token("<<")]
    Left,
    #[token("!=")]
    NotEqual,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreatEqual,
    #[token("$and")]
    SpecAnd,
    #[token("$or")]
    SpecOr,
    #[token("$xor")]
    SpecXor,
    #[token("...")]
    Ellipsis,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("=")]
    Assign,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("~")]
    Tilde,
    #[token("<")]
    Less,
    #[token(">")]
    Great,
    #[regex(r"#[^\n\r]*")]
    Comment,
    #[regex(r"[a-zA-Z_.][a-zA-Z0-9_.]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r"[0-9]|[1-9][0-9]+", |lex| lex.slice().to_string())]
    DecInt(String),
    #[regex(r"0x[0-9a-fA-F]+", |lex| lex.slice().to_string())]
    HexInt(String),
    #[regex(r"0b[01]+", |lex| lex.slice().to_string())]
    BinInt(String),
    #[regex(r"[\r \t\v]+")]
    Whitespace,
    #[regex(r"\n")]
    Newline,
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
                // Errors
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
                // Whitespace
                Token::LineComment | Token::Whitespace => {
                    self.add_pos(lexer.slice().len());
                    continue;
                }
                // Preprocessor
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
                // Additional blocks
                Token::PrintBlock => {
                    lexer = self.handle_print_block(lexer, &mut tokens);
                }
                Token::DefineBlock => {
                    lexer = self.handle_define_block(lexer, &mut tokens);
                }
                Token::StartQString => {
                    lexer = self.handle_qstring_block(lexer, &mut tokens);
                }
                // Main
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

    fn handle_print_block<'a>(
        &mut self,
        lexer: logos::Lexer<'a, Token>,
        tokens: &mut Vec<SpannedToken<Token, Location>>,
    ) -> logos::Lexer<'a, Token> {
        let (start, end) = self.add_pos(lexer.slice().len());
        if !self.is_copy() {
            return lexer;
        }
        let mut lexer = lexer.to_owned().morph::<PrintBlock>();
        let ok = Ok((start.clone(), Token::PrintBlock, end));
        debug!("{:?}", ok);
        tokens.push(ok);
        loop {
            let token = match lexer.next() {
                Some(t) => {
                    trace!("{:?}", t);
                    t
                }
                None => {
                    let (_, end) = self.add_pos(lexer.slice().len());
                    let err = Err(LexicalError::new("Unclosed print block", &start, &end));
                    debug!("{:?}", err);
                    tokens.push(err);
                    return lexer.morph::<Token>();
                }
            };
            match token {
                // Error
                PrintBlock::Error => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    let err = Err(LexicalError::new(
                        format!(r#"Unexpected token in Print block: "{}""#, slice),
                        &start,
                        &end,
                    ));
                    debug!("{:?}", err);
                    tokens.push(err);
                    return lexer.morph::<Token>();
                }
                // Preprocessor
                PrintBlock::PreprocInclude => {
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
                PrintBlock::PreprocDefineQString => {
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
                PrintBlock::PreprocDefineString => {
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
                PrintBlock::PreprocDefine => {
                    let slice = lexer.slice();
                    let (start, _) = self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let m = DEFINE_RE.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str().to_string();
                    self.define(key, None, &start);
                }
                PrintBlock::PreprocUndef => {
                    let slice = lexer.slice();
                    self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let key = UNDEF_RE.captures(slice).unwrap().get(1).unwrap().as_str();
                    self.undefine(key);
                }
                PrintBlock::PreprocIfDef => {
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
                PrintBlock::PreprocIfNDef => {
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
                PrintBlock::PreprocIf => {
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
                PrintBlock::PreprocElIf => {
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
                PrintBlock::PreprocEndIf => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    debug!(r"@endif (start: {:?}, end: {:?})", &start, &end);
                    if let Err(e) = self.leave_if(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                    }
                }
                PrintBlock::PreprocElse => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    debug!(r"@else (start: {:?}, end: {:?})", &start, &end);
                    if let Err(e) = self.enter_else(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                        continue;
                    }
                    self.set_copy(!self.is_handled());
                }
                PrintBlock::PreprocExpansion => {
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
                // Main
                PrintBlock::Is => {
                    return self.handle_pattern_block(lexer.morph::<Token>(), tokens);
                }
                _ => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let ok = Ok((start, token.into(), end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                }
            }
        }
    }

    fn handle_pattern_block<'a>(
        &mut self,
        lexer: logos::Lexer<'a, Token>,
        tokens: &mut Vec<SpannedToken<Token, Location>>,
    ) -> logos::Lexer<'a, Token> {
        let (start, end) = self.add_pos(lexer.slice().len());
        if !self.is_copy() {
            return lexer;
        }
        let mut lexer = lexer.to_owned().morph::<PatternBlock>();
        let ok = Ok((start.clone(), Token::PatternBlock, end));
        debug!("{:?}", ok);
        tokens.push(ok);

        let mut is_action = false;
        loop {
            let token = match lexer.next() {
                Some(t) => {
                    trace!("{:?}", t);
                    t
                }
                None => {
                    let (_, end) = self.add_pos(lexer.slice().len());
                    let err = Err(LexicalError::new("Unclosed Pattern block", &start, &end));
                    debug!("{:?}", err);
                    tokens.push(err);
                    return lexer.morph::<Token>();
                }
            };
            match token {
                // Error
                PatternBlock::Error => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    let err = Err(LexicalError::new(
                        format!(r#"Unexpected token in Pattern block: "{}""#, slice),
                        &start,
                        &end,
                    ));
                    debug!("{:?}", err);
                    tokens.push(err);
                    return lexer.morph::<Token>();
                }
                // Preprocessor
                PatternBlock::PreprocInclude => {
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
                PatternBlock::PreprocDefineQString => {
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
                PatternBlock::PreprocDefineString => {
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
                PatternBlock::PreprocDefine => {
                    let slice = lexer.slice();
                    let (start, _) = self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let m = DEFINE_RE.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str().to_string();
                    self.define(key, None, &start);
                }
                PatternBlock::PreprocUndef => {
                    let slice = lexer.slice();
                    self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let key = UNDEF_RE.captures(slice).unwrap().get(1).unwrap().as_str();
                    self.undefine(key);
                }
                PatternBlock::PreprocIfDef => {
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
                PatternBlock::PreprocIfNDef => {
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
                PatternBlock::PreprocIf => {
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
                PatternBlock::PreprocElIf => {
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
                PatternBlock::PreprocEndIf => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    debug!(r"@endif (start: {:?}, end: {:?})", &start, &end);
                    if let Err(e) = self.leave_if(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                    }
                }
                PatternBlock::PreprocElse => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    debug!(r"@else (start: {:?}, end: {:?})", &start, &end);
                    if let Err(e) = self.enter_else(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                        continue;
                    }
                    self.set_copy(!self.is_handled());
                }
                PatternBlock::PreprocExpansion => {
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
                // Main
                PatternBlock::Comment | PatternBlock::Whitespace | PatternBlock::Newline => {
                    self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                }
                PatternBlock::LBrace | PatternBlock::Unimpl => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let ok = Ok((start, Token::LBracePat, end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                    return lexer.morph::<Token>();
                }
                PatternBlock::LBracket => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let ok = Ok((start, Token::LBracketPat, end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                    is_action = true;
                }
                PatternBlock::RBracket => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let ok = Ok((start, Token::LBracketPat, end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                    is_action = false;
                }
                PatternBlock::Ampersand => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let token = if is_action {
                        Token::AndPat
                    } else {
                        Token::AmpersandPat
                    };
                    let ok = Ok((start, token, end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                }
                PatternBlock::Pipe => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let token = if is_action {
                        Token::OrPat
                    } else {
                        Token::PipePat
                    };
                    let ok = Ok((start, token, end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                }
                _ => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let ok = Ok((start, token.into(), end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                }
            }
        }
    }

    fn handle_qstring_block<'a>(
        &mut self,
        lexer: logos::Lexer<'a, Token>,
        tokens: &mut Vec<SpannedToken<Token, Location>>,
    ) -> logos::Lexer<'a, Token> {
        let (start, _) = self.add_pos(1);
        if !self.is_copy() {
            return lexer;
        }
        let mut lexer = lexer.to_owned().morph::<QString>();
        let mut result = String::new();
        loop {
            let token = match lexer.next() {
                Some(t) => {
                    trace!("{:?}", t);
                    t
                }
                None => {
                    let (_, end) = self.add_pos(lexer.slice().len());
                    let err = Err(LexicalError::new("Unclosed string", &start, &end));
                    debug!("{:?}", err);
                    tokens.push(err);
                    return lexer.morph::<Token>();
                }
            };
            match token {
                QString::String => {
                    let slice = lexer.slice();
                    self.add_pos(slice.len());
                    result += slice;
                }
                QString::EscapeCharacter => {
                    let slice = lexer.slice();
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
                            return lexer.morph::<Token>();
                        }
                    }
                }
                QString::UnicodeEscape => {
                    let slice = lexer.slice();
                    self.add_pos(slice.len());
                    let hex = &slice[2..slice.len()];
                    result.push(
                        u32::from_str_radix(hex, 16)
                            .map(|c| std::char::from_u32(c).unwrap())
                            .unwrap(),
                    );
                }
                QString::OctalEscape => {
                    let slice = lexer.slice();
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
                    if !self.is_copy() {
                        return lexer.morph::<Token>();
                    }
                    let ok = Ok((start, Token::QString(result), end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                    return lexer.morph::<Token>();
                }
                QString::Error => {
                    let slice = lexer.slice();
                    let (_, end) = self.add_pos(slice.len());
                    let err = Err(LexicalError::new(
                        format!("Unexpected string: {}", slice),
                        &start,
                        &end,
                    ));
                    debug!("{:?}", err);
                    tokens.push(err);
                    return lexer.morph::<Token>();
                }
            }
        }
    }

    fn handle_define_block<'a>(
        &mut self,
        lexer: logos::Lexer<'a, Token>,
        tokens: &mut Vec<SpannedToken<Token, Location>>,
    ) -> logos::Lexer<'a, Token> {
        let (start, end) = self.add_pos(lexer.slice().len());
        if !self.is_copy() {
            return lexer;
        }
        let mut lexer = lexer.to_owned().morph::<DefineBlock>();
        let ok = Ok((start.clone(), Token::DefineBlock, end));
        debug!("{:?}", ok);
        tokens.push(ok);
        loop {
            let token = match lexer.next() {
                Some(t) => {
                    trace!("{:?}", t);
                    t
                }
                None => {
                    let (_, end) = self.add_pos(lexer.slice().len());
                    let err = Err(LexicalError::new("Unclosed define block", &start, &end));
                    debug!("{:?}", err);
                    tokens.push(err);
                    return lexer.morph::<Token>();
                }
            };
            match token {
                // Error
                DefineBlock::Error => {
                    let slice = lexer.slice();
                    let (start, end) = self.add_pos(slice.len());
                    let err = Err(LexicalError::new(
                        format!(r#"Unexpected token in Define block: "{}""#, slice),
                        &start,
                        &end,
                    ));
                    debug!("{:?}", err);
                    tokens.push(err);
                    return lexer.morph::<Token>();
                }
                // Preprocessor
                DefineBlock::PreprocInclude => {
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
                DefineBlock::PreprocDefineQString => {
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
                DefineBlock::PreprocDefineString => {
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
                DefineBlock::PreprocDefine => {
                    let slice = lexer.slice();
                    let (start, _) = self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let m = DEFINE_RE.captures(slice).unwrap();
                    let key = m.get(1).unwrap().as_str().to_string();
                    self.define(key, None, &start);
                }
                DefineBlock::PreprocUndef => {
                    let slice = lexer.slice();
                    self.add_pos(slice.len());
                    if !self.is_copy() {
                        continue;
                    }

                    let key = UNDEF_RE.captures(slice).unwrap().get(1).unwrap().as_str();
                    self.undefine(key);
                }
                DefineBlock::PreprocIfDef => {
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
                DefineBlock::PreprocIfNDef => {
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
                DefineBlock::PreprocIf => {
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
                DefineBlock::PreprocElIf => {
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
                DefineBlock::PreprocEndIf => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    debug!(r"@endif (start: {:?}, end: {:?})", &start, &end);
                    if let Err(e) = self.leave_if(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                    }
                }
                DefineBlock::PreprocElse => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    debug!(r"@else (start: {:?}, end: {:?})", &start, &end);
                    if let Err(e) = self.enter_else(&start, &end) {
                        debug!("{:?}", e);
                        tokens.push(Err(e));
                        continue;
                    }
                    self.set_copy(!self.is_handled());
                }
                DefineBlock::PreprocExpansion => {
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
                // Main
                DefineBlock::Comment | DefineBlock::Whitespace | DefineBlock::Newline => {
                    self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                }
                DefineBlock::Semi => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let ok = Ok((start, Token::SemiDef, end));
                    debug!("{:?}", ok);
                    tokens.push(ok);
                    return lexer.morph::<Token>();
                }
                _ => {
                    let (start, end) = self.add_pos(lexer.slice().len());
                    if !self.is_copy() {
                        continue;
                    }
                    let ok = Ok((start, token.into(), end));
                    debug!("{:?}", ok);
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

impl From<PrintBlock> for Token {
    fn from(token: PrintBlock) -> Self {
        match token {
            PrintBlock::Char(ch) => Token::CharPrint(ch),
            PrintBlock::Caret => Token::Caret,
            PrintBlock::SymbolString(symbol) => Token::SymbolStringPrint(symbol),
            PrintBlock::QString(string) => Token::QString(string),
            PrintBlock::Whitespace | PrintBlock::Newline => Token::WhitespacePrint,
            _ => unreachable!(),
        }
    }
}

impl From<DefineBlock> for Token {
    fn from(token: DefineBlock) -> Self {
        match token {
            DefineBlock::LParen => Token::LParenDef,
            DefineBlock::RParen => Token::RParenDef,
            DefineBlock::Comma => Token::CommaDef,
            DefineBlock::Assign => Token::AssignDef,
            DefineBlock::Colon => Token::ColonDef,
            DefineBlock::LBracket => Token::LBracketDef,
            DefineBlock::RBracket => Token::RBracketDef,
            DefineBlock::Space => Token::SpaceDef,
            DefineBlock::Type => Token::TypeDef,
            DefineBlock::RamSpace => Token::RamSpaceDef,
            DefineBlock::Default => Token::DefaultDef,
            DefineBlock::RegisterSpace => Token::RegisterSpaceDef,
            DefineBlock::Token => Token::TokenDef,
            DefineBlock::Context => Token::ContextDef,
            DefineBlock::Bitrange => Token::BitrangeDef,
            DefineBlock::Signed => Token::SignedDef,
            DefineBlock::Noflow => Token::NoflowDef,
            DefineBlock::Hex => Token::HexDef,
            DefineBlock::Dec => Token::DecDef,
            DefineBlock::Endian => Token::EndianDef,
            DefineBlock::Alignment => Token::AlignmentDef,
            DefineBlock::Big => Token::BigDef,
            DefineBlock::Little => Token::LittleDef,
            DefineBlock::Size => Token::SizeDef,
            DefineBlock::Wordsize => Token::WordsizeDef,
            DefineBlock::Offset => Token::OffsetDef,
            DefineBlock::Names => Token::NamesDef,
            DefineBlock::Values => Token::ValuesDef,
            DefineBlock::Variables => Token::VariablesDef,
            DefineBlock::Pcodeop => Token::PcodeopDef,
            DefineBlock::Identifier(string) => Token::Identifier(string),
            DefineBlock::DecInt(string) => Token::DecInt(string),
            DefineBlock::HexInt(string) => Token::HexInt(string),
            DefineBlock::BinInt(string) => Token::BinInt(string),
            DefineBlock::QString(string) => Token::QString(string),
            _ => unreachable!(),
        }
    }
}

impl From<PatternBlock> for Token {
    fn from(token: PatternBlock) -> Self {
        match token {
            PatternBlock::Globalset => Token::GlobalsetPat,
            PatternBlock::Right => Token::RightPat,
            PatternBlock::Left => Token::LeftPat,
            PatternBlock::NotEqual => Token::NotEqualPat,
            PatternBlock::LessEqual => Token::LessEqualPat,
            PatternBlock::GreatEqual => Token::GreatEqualPat,
            PatternBlock::SpecAnd => Token::SpecAndPat,
            PatternBlock::SpecOr => Token::SpecOrPat,
            PatternBlock::SpecXor => Token::SpecXorPat,
            PatternBlock::Ellipsis => Token::EllipsisPat,
            PatternBlock::Caret => Token::CaretPat,
            PatternBlock::Assign => Token::AssignPat,
            PatternBlock::LParen => Token::LParenPat,
            PatternBlock::RParen => Token::RParenPat,
            PatternBlock::Comma => Token::CommaPat,
            PatternBlock::Colon => Token::ColonPat,
            PatternBlock::Semi => Token::SemiPat,
            PatternBlock::Plus => Token::PlusPat,
            PatternBlock::Minus => Token::MinusPat,
            PatternBlock::Asterisk => Token::AsteriskPat,
            PatternBlock::Slash => Token::SlashPat,
            PatternBlock::Tilde => Token::TildePat,
            PatternBlock::Less => Token::LessPat,
            PatternBlock::Great => Token::GreatPat,
            PatternBlock::Identifier(string) => Token::Identifier(string),
            PatternBlock::DecInt(string) => Token::DecInt(string),
            PatternBlock::HexInt(string) => Token::HexInt(string),
            PatternBlock::BinInt(string) => Token::BinInt(string),
            _ => unreachable!(),
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
    pub(crate) fn new(error: impl ToString, start: &Location, end: &Location) -> Self {
        Self {
            error: error.to_string(),
            start: start.clone(),
            end: end.clone(),
        }
    }
}
