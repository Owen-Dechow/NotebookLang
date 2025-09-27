use crate::lexer::TokenType;

pub const OPERATORS: phf::Map<&str, TokenType> = phf::phf_map! {
    "?" => TokenType::CanExcept,
    "=" => TokenType::Assign,
    "(" => TokenType::OpenParen,
    ")" => TokenType::CloseParen,
    "[" => TokenType::OpenBracket,
    "]" => TokenType::CloseBracket,
    "{" => TokenType::OpenMap,
    "}" => TokenType::CloseMap,
    "," => TokenType::Comma,
    "+" => TokenType::Add,
    "-" => TokenType::Subtract,
    "/" => TokenType::Divide,
    "*" => TokenType::Multiply,
    "->" => TokenType::Map,
    ":" => TokenType::Peek,
    "^" => TokenType::Exponentiate,
    "%" => TokenType::Modulate,
    "." => TokenType::Compose,
    ">" => TokenType::GreaterThan,
    "<" => TokenType::LessThan,
    "==" => TokenType::Equals,
    "~~" => TokenType::Similar,
    ">=" => TokenType::GreaterThanOrEqual,
    "<=" => TokenType::LessThanOrEqual,
    "!" => TokenType::Factorial,
    "!=" => TokenType::NotEqual,
    "!~" => TokenType::NotSimilar,
};

pub const KEY_WORDS: phf::Map<&str, TokenType> = phf::phf_map! {
    "def" => TokenType::Def,
    "let" => TokenType::Let,
    "static" => TokenType::Static,
    "impl" => TokenType::Impl,
    "capture" => TokenType::Capture,
    "B" => TokenType::Bool,
    "Z" => TokenType::Int,
    "R" => TokenType::Real,
    "N" => TokenType::Natural,
    "undefined" => TokenType::Undefined,
    "true" => TokenType::True,
    "false" => TokenType::False,
};

pub const STRING_QUOTES: [char; 3] = ['"', '\'', '`'];
pub const LINE_COMMENT: char = '#';
pub const VALID_VARIABLE: [char; 2] = ['_', '@'];
