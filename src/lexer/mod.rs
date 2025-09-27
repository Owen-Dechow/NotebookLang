mod char_stream;
mod syntax;
pub mod token_stream;

use crate::{lexer::char_stream::CharStream, location::Location};

#[derive(Debug, Clone)]
pub enum TokenType {
    RealLit,
    IntLit,
    Capture,
    Impl,
    Static,
    Let,
    Def,
    False,
    True,
    Undefined,
    Natural,
    Real,
    Int,
    Bool,
    Identity,
    IncompleteToken,
    Str,
    Comment,
    CloseParen,
    OpenBracket,
    Multiply,
    Subtract,
    Factorial,
    CanExcept,
    Comma,
    Map,
    GreaterThan,
    Divide,
    LessThan,
    Peek,
    NotEqual,
    OpenMap,
    Exponentiate,
    NotSimilar,
    CloseBracket,
    OpenParen,
    Similar,
    Assign,
    CloseMap,
    GreaterThanOrEqual,
    Equals,
    Compose,
    Modulate,
    LessThanOrEqual,
    Add,
}

#[derive(Debug)]
pub struct Token {
    r#type: TokenType,
    value: String,
    location: Location,
}

impl Token {
    pub fn new(t: TokenType, v: String, l: Location) -> Self {
        Self {
            r#type: t,
            value: v,
            location: l,
        }
    }

    pub fn incomplete<T: AsRef<str>>(v: T, cs: &CharStream) -> Self {
        Self {
            r#type: TokenType::IncompleteToken,
            location: cs.build_loc(v.as_ref()),
            value: v.as_ref().to_string(),
        }
    }
}
