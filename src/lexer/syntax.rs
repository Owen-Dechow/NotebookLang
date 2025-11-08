use crate::lexer::tokens::TokenType;

pub(super) const OPERATORS: phf::Map<&str, TokenType> = phf::phf_map! {
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
    "$" => TokenType::DenoteBlock,
};

pub(super) const KEY_WORDS: phf::Map<&str, TokenType> = phf::phf_map! {
    "def" => TokenType::Def,
    "let" => TokenType::Let,
    "static" => TokenType::Static,
    "impl" => TokenType::Impl,
    "capture" => TokenType::Capture,
    "raise" => TokenType::Raise,
    "B" => TokenType::Bool,
    "Z" => TokenType::Int,
    "R" => TokenType::Real,
    "N" => TokenType::Natural,
    "S" => TokenType::Str,
    "undefined" => TokenType::Undefined,
    "true" => TokenType::True,
    "open" => TokenType::Open,
    "false" => TokenType::False,
};

pub(super) const STRING_QUOTES: [char; 3] = ['"', '\'', '`'];
pub(super) const LINE_COMMENT: char = '#';
pub(super) const VALID_VARIABLE: [char; 2] = ['_', '@'];
