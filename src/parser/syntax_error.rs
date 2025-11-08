use crate::lexer::{Token, TokenType};

const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const RED: &str = "\x1b[31m";
const RESET: &str = "\x1b[0m";

#[derive(Debug)]
pub(crate) enum SyntaxError {
    InvalidToken(Token, TokenType, &'static str),
    ExpectedToken(TokenType, &'static str),
}

impl SyntaxError {
    pub(crate) fn print_error(&self) {
        match self {
            SyntaxError::InvalidToken(token, token_type, cline) => {
                println!(
                    r#"{}({}) Expected token "{:?}" found "{:?}"{}"#,
                    YELLOW, cline, token_type, token.r#type, RESET
                );

                let range = &token.location.file_range;
                let file = std::fs::read_to_string(&token.location.file).unwrap();

                let start = range.start.saturating_sub(50);
                let end = usize::min(range.end + 50, file.len());

                let before = &file[start..range.start];
                let error = &file[range.clone()];
                let after = &file[range.end..end];

                println!(
                    "{}{}{}{}{}{}{}",
                    GREEN, before, RED, error, GREEN, after, RESET
                );
            }
            SyntaxError::ExpectedToken(token_type, cline) => {
                println!(
                    "{}({}) Expected token {:?}{}",
                    YELLOW, cline, token_type, RESET
                );
            }
        }
    }
}
