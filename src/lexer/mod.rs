pub(crate) use crate::lexer::{
    token_stream::TokenStream,
    tokens::{Token, TokenType},
};
use std::{io::Result, path::Path};

mod char_stream;
mod syntax;
mod token_stream;
mod tokens;

pub(crate) fn lex_file<T: AsRef<Path>>(file: T) -> Result<TokenStream> {
    let content = std::fs::read_to_string(&file)?;
    return Ok(TokenStream::from_str(content, file));
}
