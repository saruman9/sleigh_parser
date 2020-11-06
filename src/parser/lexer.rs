use logos::Logos;

#[derive(Logos, Debug, Clone)]
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
    #[regex(r#"@include\s+"(.*)""#)]
    IncludePreproc(&'input str),
    #[regex(r#"@define\s+([0-9A-Za-z_]+)\s+"(.*)""#)]
    Define1Preproc(&'input str),
    #[regex(r"@define\s+([0-9A-Za-z_]+)\s+(\S+)")]
    Define2Preproc(&'input str),
    #[regex(r"@define\s+([0-9A-Za-z_]+)")]
    Define3Preproc(&'input str),
    #[regex(r"@undef\s+([0-9A-Z_a-z]+)")]
    UnDefPreproc(&'input str),
    #[regex(r"@ifdef\s+([0-9A-Za-z_]+)")]
    IfDefPreproc(&'input str),
    #[regex(r"@ifndef\s+([0-9A-Za-z_]+)")]
    IfNDefPreproc(&'input str),
    #[regex(r"@if\s+(.*)")]
    IfPreproc(&'input str),
    #[regex(r"@elif\s+(.*)")]
    ElIfPreproc(&'input str),
    #[regex(r"@endif")]
    EndIfPreproc(&'input str),
    #[regex(r"@else")]
    ElsePreproc(&'input str),
    #[regex(r"\$\(([0-9A-Za-z_]+)\)")]
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
    StartString,
    QString(&'input str),
    #[regex(r"[0-9]+")]
    DecInt(&'input str),
    #[regex(r"0x[:xdigit:]+")]
    HexInt(&'input str),
    #[regex(r"0b[01]+")]
    BinInt(&'input str)
}

#[derive(Logos, Debug, Clone)]
pub enum QString {
    #[error]
    Error,
    #[regex(r#"[^\\"]+"#)]
    String,
    #[regex(r#"\\[btnfr"'\\]"#)]
    EscapeCharacter,
    // TODO: [issue #126] {n,m} repetition range is currently unsupported.
    #[regex(r"\\u[:xdigit:][:xdigit:][:xdigit:][:xdigit:]")]
    UnicodeEscape,
    // TODO: [issue #126] {n,m} repetition range is currently unsupported.
    #[regex(r"\\[0-3]?[0-7][0-7]?")]
    OctalEscape,
    #[token("\"")]
    EndString,
}

pub(crate) type Span = std::ops::Range<usize>;

pub struct LexicalError {
    err: String,
    span: Span,
}
