use indexmap::IndexMap;
use regex::Regex;
use std::sync::LazyLock;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(EnumIter, Debug, Hash, Copy, Clone, PartialEq, Eq)]
pub(crate) enum TokenType {
    KwDef,
    KwVar,
    KwIf,
    KwElse,
    KwWhile,

    TypeInt,
    TypeFloat,
    TypeBool,

    BooleanTrue,
    BooleanFalse,

    Lparen,
    Rparen,
    Lbrac,
    Rbrac,
    Comma,
    Semicolon,

    Integer,
    Float,
    Identifier,

    Plus,
    Minus,
    Mul,
    Div,
    Assign,

    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Neq,

    Not,
}

static PATTERN_MAP: LazyLock<IndexMap<&'static str, TokenType>> = LazyLock::new(|| {
    let mut map = IndexMap::new();
    for variant in TokenType::iter() {
        let pattern = match variant {
            TokenType::KwDef => "def",
            TokenType::KwVar => "var",
            TokenType::KwIf => "if",
            TokenType::KwElse => "else",
            TokenType::KwWhile => "while",
            TokenType::TypeFloat => r"float",
            TokenType::TypeBool => r"bool",
            TokenType::TypeInt => r"int",
            TokenType::BooleanTrue => r"true",
            TokenType::BooleanFalse => r"false",
            TokenType::Lparen => r"\(",
            TokenType::Rparen => r"\)",
            TokenType::Lbrac => r"\{",
            TokenType::Rbrac => r"\}",
            TokenType::Integer => r"\d+",
            TokenType::Float => r"\d+.\d+",
            TokenType::Identifier => r"[a-zA-Z_][a-zA-Z_0-9]*",
            TokenType::Plus => r"\+",
            TokenType::Minus => "-",
            TokenType::Mul => r"\*",
            TokenType::Div => "/",
            TokenType::Le => "<=",
            TokenType::Ge => ">=",
            TokenType::Lt => "<",
            TokenType::Gt => ">",
            TokenType::Assign => "=",
            TokenType::Eq => "==",
            TokenType::Neq => "!=",
            TokenType::Not => "!",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
        };

        map.insert(pattern, variant);
    }

    map
});

#[derive(Debug, Clone)]
pub struct Token {
    pub r#type: TokenType,
    pub start: usize,
    pub end: usize,
    pub content: String,
}

pub fn lexer(input: &str) -> Result<Vec<Token>, (Vec<Token>, String)> {
    let mut offset = 0;
    let mut tokens = Vec::new();

    while offset < input.len() {
        if let Some((off, _)) = input
            .chars()
            .enumerate()
            .find(|(off, ip)| *off >= offset && !ip.is_whitespace())
        {
            offset = off;
        } else {
            return Ok(tokens); // Nothing except whitespaces found. Return lexed this far.
        }

        let mut found = false;

        for (pattern_str, &token_type) in PATTERN_MAP.iter() {
            let pattern = Regex::new(pattern_str).expect("Invalid regex pattern");
            if let Some(cap) = pattern.find(&input[offset..]) {
                if cap.start() == 0 {
                    let start = offset;
                    let end = offset + cap.end();
                    let content = &input[start..end];

                    tokens.push(Token {
                        r#type: token_type,
                        start,
                        end,
                        content: content.to_string(),
                    });

                    offset = end;
                    found = true;
                    break;
                }
            }
        }

        if !found {
            return Err((tokens, (&input[offset..]).to_string()));
        }
    }

    Ok(tokens)
}
