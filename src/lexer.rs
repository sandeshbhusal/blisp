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
    KwWhile,

    TypeInt,
    TypeFloat,
    TypeBool,

    Lparen,
    Rparen,
    Lbrac,
    Rbrac,

    Integer,
    Float,
    Identifier,

    Plus,
    Minus,
    Mul,
    Div,

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
            TokenType::KwWhile => "while",
            TokenType::TypeFloat => r"Float",
            TokenType::TypeBool => r"Bool",
            TokenType::TypeInt => r"Int",
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
            TokenType::Eq => "=",
            TokenType::Neq => "!=",
            TokenType::Not => "!",
        };

        map.insert(pattern, variant);
    }

    map
});

#[derive(Debug)]
pub(crate) struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) content: String,
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

#[cfg(test)]
mod lexer_tests {
    use super::lexer;

    #[test]
    fn skip_whitespace() {
        let matched = lexer("  def  ");
        assert!(matched.is_ok_and(|k| k.len() == 1));
    }

    #[test]
    fn check_all() {
        let check = r#"
            def some_function() {
                const a
                const b
                if a b {}
            }
        "#;

        assert!(lexer(check).is_ok());
    }

    #[test]
    fn check_fibo() {
        let check = r#"
            (def fibo (var x Int) (
                (if (<= x 0) (1) (
                    (+ (fibo (- x 1)) (fibo (-x 2))
                ))
            )<)
        "#;

        assert!(lexer(check).is_ok());
    }

    #[test]
    fn check_types() {
        let check = r#"
            def some_function() {
                const a Int
                const b Float
                const c Bool
            }
        "#;

        assert!(lexer(check).is_ok());
    }
}
