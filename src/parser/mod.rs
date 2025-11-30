mod abstract_expression;
mod abstract_syntax_tree;
mod abstract_type;
mod syntax_error;

#[macro_export]
macro_rules! fli {
    () => {
        concat!(file!(), ":", line!())
    };
}

use crate::{
    lexer::{Token, TokenStream, TokenType},
    parser::{abstract_syntax_tree::AbstractSyntaxTree, syntax_error::SyntaxError},
};

pub(crate) fn parse_token_stream(ts: &mut TokenStream) -> AbstractSyntaxTree {
    return AbstractSyntaxTree::from_token_stream(ts);
}

fn parse_identity(ts: &mut TokenStream) -> Result<Token, SyntaxError> {
    match ts.take_sig() {
        Some(t) if t.is_type(&TokenType::Identity) => Ok(t),
        Some(t) => return Err(SyntaxError::InvalidToken(t, TokenType::Identity, fli!())),
        None => return Err(SyntaxError::ExpectedToken(TokenType::Identity, fli!())),
    }
}

fn take_sig_of_type(ts: &mut TokenStream, t: &TokenType) -> Result<Token, SyntaxError> {
    match ts.take_sig() {
        Some(token) if token.is_type(t) => Ok(token),
        Some(token) => Err(SyntaxError::InvalidToken(token, t.clone(), fli!())),
        None => Err(SyntaxError::ExpectedToken(t.clone(), fli!())),
    }
}

fn absorb_comma_or_allow(ts: &mut TokenStream, allow: &TokenType) -> Result<(), SyntaxError> {
    match ts.take_sig() {
        Some(t) if t.is_type(&TokenType::Comma) => Ok(()),
        Some(t) if t.is_type(allow) => {
            ts.retake();
            Ok(())
        }
        Some(t) => Err(SyntaxError::InvalidToken(t, allow.clone(), fli!())),
        None => Err(SyntaxError::ExpectedToken(allow.clone(), fli!())),
    }
}

fn take_next_if_type(ts: &mut TokenStream, r#type: &TokenType) -> Result<bool, SyntaxError> {
    match ts.take_sig() {
        Some(t) if t.is_type(r#type) => Ok(true),
        Some(_) => {
            ts.retake();
            Ok(false)
        }
        None => Err(SyntaxError::ExpectedToken(r#type.clone(), fli!())),
    }
}
