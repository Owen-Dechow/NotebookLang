use crate::{lexer::char_stream::CharStream, location::Location};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenType {
    RealLit,
    Raise,
    IntLit,
    Open,
    StrLit,
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
    DenoteBlock,
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) value: String,
    pub(crate) location: Location,
}

impl Token {
    pub(crate) fn is_type(&self, t: &TokenType) -> bool {
        self.r#type == *t
    }
}

impl Token {
    pub(super) fn new(t: TokenType, v: String, l: Location) -> Self {
        Self {
            r#type: t,
            value: v,
            location: l,
        }
    }

    pub(super) fn incomplete<T: AsRef<str>>(v: T, cs: &CharStream) -> Self {
        Self {
            r#type: TokenType::IncompleteToken,
            location: cs.build_loc(v.as_ref()),
            value: v.as_ref().to_string(),
        }
    }
}
