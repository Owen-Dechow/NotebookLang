use crate::{
    fli,
    lexer::{Token, TokenStream, TokenType},
    parser::{syntax_error::SyntaxError, take_sig_of_type},
};

#[derive(Debug)]
pub(crate) enum AbstractType {
    List(Box<AbstractType>),
    Set(Box<AbstractType>),
    Map(Box<AbstractType>, Box<AbstractType>),
    Base(Token),
}

pub(super) fn parse_type(ts: &mut TokenStream) -> Result<AbstractType, SyntaxError> {
    match ts.take_sig() {
        Some(t) => match t.r#type {
            TokenType::OpenMap => {
                let inner1 = parse_type(ts)?;
                match ts.take_sig() {
                    Some(t) if t.is_type(&TokenType::Map) => {
                        let inner2 = parse_type(ts)?;
                        let _close = take_sig_of_type(ts, &TokenType::CloseMap);
                        Ok(AbstractType::Map(inner1.into(), inner2.into()))
                    }
                    Some(t) if t.is_type(&TokenType::CloseMap) => {
                        Ok(AbstractType::Set(inner1.into()))
                    }
                    Some(t) => Err(SyntaxError::InvalidToken(t, TokenType::CloseMap, fli!())),
                    None => Err(SyntaxError::ExpectedToken(TokenType::CloseMap, fli!())),
                }
            }
            TokenType::OpenBracket => {
                let inner = parse_type(ts)?;
                let _close = take_sig_of_type(ts, &TokenType::CloseBracket);
                Ok(AbstractType::List(inner.into()))
            }
            TokenType::Identity
            | TokenType::Int
            | TokenType::Bool
            | TokenType::Real
            | TokenType::Natural
            | TokenType::Str => Ok(AbstractType::Base(t)),
            _ => Err(SyntaxError::InvalidToken(t, TokenType::Identity, fli!())),
        },
        None => Err(SyntaxError::ExpectedToken(TokenType::Identity, fli!())),
    }
}
