use logos::{Lexer, Logos};
use regex::Regex;

lazy_static::lazy_static! {
    static ref INCLUDE_RE: Regex = Regex::new(r#"^\s*@include\s+"(.*)"\s*$"#).unwrap();
    static ref DEFINE1_RE: Regex = Regex::new(r#"^\s*@define\s+([0-9A-Z_a-z]+)\s+"(.*)"\s*$"#).unwrap();
    static ref DEFINE2_RE: Regex = Regex::new(r"^\s*@define\s+([0-9A-Z_a-z]+)\s+(\S+)\s*$").unwrap();
    static ref DEFINE3_RE: Regex = Regex::new(r"^\s*@define\s+([0-9A-Z_a-z]+)\s*$").unwrap();
    static ref UNDEF_RE: Regex = Regex::new(r"^\s*@undef\s+([0-9A-Z_a-z]+)\s*$").unwrap();
    static ref IFDEF_RE: Regex = Regex::new(r"^\s*@ifdef\s+([0-9A-Z_a-z]+)\s*$").unwrap();
    static ref IFNDEF_RE: Regex = Regex::new(r"^\s*@ifndef\s+([0-9A-Z_a-z]+)\s*$").unwrap();
    static ref IF_RE: Regex = Regex::new(r"^\s*@if\s+(.*)").unwrap();
    static ref ELIF_RE: Regex = Regex::new(r"^\s*@elif\s+(.*)").unwrap();
    static ref ENDIF_RE: Regex = Regex::new(r"^\s*@endif\s*$").unwrap();
    static ref ELSE_RE: Regex = Regex::new(r"^\s*@else\s*$").unwrap();
    static ref EXPANSION_RE: Regex = Regex::new(r"\$\(([0-9A-Z_a-z]+)\)").unwrap();
}

#[derive(Logos, Debug, Clone)]
#[logos(subpattern hex = r"[0-9a-fA-F]")]
#[logos(subpattern pre_id = r"[0-9A-Za-z_]+")]
pub enum Token<'input> {
    #[error]
    UnexpectedToken,

    // WhiteSpaces
    #[regex(r"[ \t\r\n]+", logos::skip)]
    #[regex(r"#[^\n\r]*\r?\n?")]
    LineComment,
    // FIXME: Report error
    #[token("//")]
    CppComment,

    // Preprocessor
    // TODO: [issue #143] Expose captured groups for regex match.
    #[regex(r#"@include\s+"([^"]*)""#)]
    IncludePreproc(&'input str),
    #[regex(r#"@define\s+(?&pre_id)\s+"([^"]*)""#)]
    Define1Preproc(&'input str),
    #[regex(r"@define\s+(?&pre_id)\s+(\S+)")]
    Define2Preproc(&'input str),
    #[regex(r"@define\s+(?&pre_id)")]
    Define3Preproc(&'input str),
    #[regex(r"@undef\s+(?&pre_id)")]
    UnDefPreproc(&'input str),
    #[regex(r"@ifdef\s+(?&pre_id)")]
    IfDefPreproc(&'input str),
    #[regex(r"@ifndef\s+(?&pre_id)")]
    IfNDefPreproc(&'input str),
    #[regex(r"@if\s+(.*)")]
    IfPreproc(&'input str),
    #[regex(r"@elif\s+(.*)")]
    ElIfPreproc(&'input str),
    #[regex(r"@endif")]
    EndIfPreproc(&'input str),
    #[regex(r"@else")]
    ElsePreproc(&'input str),
    #[regex(r"\$\((?&pre_id)\)")]
    ExpansionPreproc(&'input str),

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

pub(crate) type Span = std::ops::Range<usize>;
#[derive(Debug)]
pub struct LexicalError {
    error: String,
    span: Span,
}
impl LexicalError {
    fn new(error: impl ToString, span: Span) -> Self {
        Self {
            error: error.to_string(),
            span,
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
                Some(Err(LexicalError::new(error, span)))
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
                            )))
                        }
                        None => {
                            return Some(Err(LexicalError::new(
                                "Unclosed string",
                                span.start..lex.span().end,
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
            Token::IncludePreproc(include_preproc) => {
                let file = INCLUDE_RE
                    .captures(include_preproc)
                    .unwrap()
                    .get(1)
                    .unwrap()
                    .as_str();
                Some(Ok((span.start, Token::IncludePreproc(file), span.end)))
            }
            _ => Some(Ok((span.start, token, span.end))),
        }
    }
}
