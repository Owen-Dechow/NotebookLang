use crate::lexer::{char_stream::CharStream, syntax, Token, TokenType};
use std::path::Path;

#[derive(Debug)]
pub struct TokenStream {
    tokens: Vec<Token>,
}

impl TokenStream {
    pub fn from_file<T: AsRef<Path>>(file: &T) -> Result<Self, std::io::Error> {
        let content = std::fs::read_to_string(file)?;
        let mut cs = CharStream::new(&content, file);
        let mut tokens = Vec::new();

        while let Some(c) = cs.take_sig() {
            if c.is_digit(10) {
                tokens.push(parse_digit(&mut cs));
            } else if c.is_alphabetic() || syntax::VALID_VARIABLE.contains(&c) {
                tokens.push(parse_alpha(&mut cs));
            } else if syntax::STRING_QUOTES.contains(&c) {
                tokens.push(parse_string(&mut cs));
            } else if c == syntax::LINE_COMMENT {
                tokens.push(parse_comment(&mut cs));
            } else {
                tokens.push(parse_operator(&mut cs));
            }
        }

        return Ok(Self { tokens });
    }
}

fn is_operator_component<T: AsRef<str>>(val: T) -> bool {
    for op in syntax::OPERATORS.keys() {
        if op.starts_with(val.as_ref()) {
            return true;
        }
    }

    return false;
}

fn parse_operator(cs: &mut CharStream<'_>) -> Token {
    let mut value = String::new();

    while let Some(c) = cs.take() {
        let mut new_val = value.clone();
        new_val.push(c);

        if is_operator_component(&new_val) {
            value = new_val;
        } else {
            cs.retake();
            break;
        }
    }

    let loc = cs.build_loc(&value);
    let r#type = syntax::OPERATORS.get(&value);
    return Token::new(
        r#type.unwrap_or(&TokenType::IncompleteToken).clone(),
        value,
        loc,
    );
}

fn parse_comment(cs: &mut CharStream<'_>) -> Token {
    let comment_start = match cs.take() {
        Some(c) => c,
        None => return Token::incomplete("", cs),
    };

    let mut value = String::from(comment_start);

    while let Some(c) = cs.take() {
        if c == '\n' {
            break;
        }

        value.push(c);
    }

    let loc = cs.build_loc(&value);
    return Token::new(TokenType::Comment, value, loc);
}

fn parse_string(cs: &mut CharStream<'_>) -> Token {
    let quote_char = match cs.take() {
        Some(c) => c,
        None => return Token::incomplete("", cs),
    };

    let mut value = String::from(quote_char);

    loop {
        let c = match cs.take() {
            Some(c) => c,
            None => return Token::incomplete(value, cs),
        };

        value.push(c);

        if c == quote_char {
            break;
        }
    }

    let loc = cs.build_loc(&value);
    return Token::new(TokenType::Str, value, loc);
}

fn parse_digit(cs: &mut CharStream) -> Token {
    let mut has_decimal = false;
    let mut value = String::new();

    while let Some(digit) = cs.take() {
        if digit.is_digit(10) {
            value.push(digit);
        } else if digit == '.' && !has_decimal {
            value.push('.');
            has_decimal = true;
        } else {
            cs.retake();
            break;
        }
    }

    let loc = cs.build_loc(&value);

    return Token::new(
        if has_decimal {
            TokenType::RealLit
        } else {
            TokenType::IntLit
        },
        value,
        loc,
    );
}

fn parse_alpha(cs: &mut CharStream) -> Token {
    let mut value = String::new();

    while let Some(letter) = cs.take() {
        if letter.is_alphanumeric() || syntax::VALID_VARIABLE.contains(&letter) {
            value.push(letter);
        } else {
            cs.retake();
            break;
        }
    }

    let loc = cs.build_loc(&value);
    let r#type = syntax::KEY_WORDS.get(&value);
    return Token::new(r#type.unwrap_or(&TokenType::Identity).clone(), value, loc);
}
