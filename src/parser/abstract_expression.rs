use std::{collections::HashMap, fmt::Display};

use crate::{
    fli,
    lexer::{Token, TokenStream, TokenType},
    parser::{
        absorb_comma_or_allow,
        abstract_syntax_tree::{parse_variable_reference, AbstractVariableReference},
        abstract_type::{parse_type, AbstractType},
        parse_identity,
        syntax_error::SyntaxError,
        take_next_if_type, take_sig_of_type,
    },
};

#[derive(Debug)]
pub(crate) enum AbstractStatement {
    Expression(AbstractExpression),
    Declaration(AbstractDeclaration),
    Update(AbstractUpdate),
}

#[derive(Debug)]
pub(crate) struct AbstractBlock {
    statements: Vec<AbstractStatement>,
    r#return: Option<Box<AbstractExpression>>,
}

#[derive(Debug)]
pub(crate) struct AbstractDeclaration {
    open: bool,
    identity: Token,
    r#type: Option<AbstractType>,
    expression: AbstractExpression,
}

#[derive(Debug)]
pub(crate) struct AbstractUpdate {
    var: AbstractVariableReference,
    expression: AbstractExpression,
    method: Option<Token>,
}

#[derive(Debug)]
pub(crate) struct AbstractMap {
    pub(super) mappings: Vec<(AbstractExpression, AbstractExpression)>,
}

#[derive(Debug)]
pub(crate) struct AbstractSet {
    pub(super) items: Vec<AbstractExpression>,
}

#[derive(Debug)]
pub(crate) struct AbstractList {
    pub(super) items: Vec<AbstractExpression>,
}

#[derive(Debug)]
pub(crate) struct AbstractCapture {
    captures: Vec<(AbstractVariableReference, Option<Token>, AbstractExpression)>,
    expression: Box<AbstractExpression>,
}

#[derive(Debug)]
pub(crate) struct AbstractCall {
    func: Box<AbstractExpression>,
    args: AbstractCallArgs,
}

#[derive(Debug)]
pub(crate) enum AbstractCallArgs {
    ArgList(Vec<AbstractExpression>),
    NamedArgs(Vec<(Token, AbstractExpression)>),
}

impl AbstractCallArgs {
    fn sorted(self) -> Self {
        match self {
            AbstractCallArgs::ArgList(args) => {
                AbstractCallArgs::ArgList(args.into_iter().map(|arg| arg.sorted()).collect())
            }
            AbstractCallArgs::NamedArgs(items) => AbstractCallArgs::NamedArgs(
                items.into_iter().map(|(k, v)| (k, v.sorted())).collect(),
            ),
        }
    }
}

#[derive(Debug)]
pub(crate) enum AbstractExpression {
    IntLiteral(Token),
    RealLiteral(Token),
    StrLiteral(Token),
    BoolLiteral(Token),
    Undefined(Token),
    UnaryOperation(Token, Box<AbstractExpression>),
    BinaryOperation(Box<AbstractExpression>, Token, Box<AbstractExpression>),
    Block(AbstractBlock),
    Map(AbstractMap),
    Set(AbstractSet),
    EmptySet,
    List(AbstractList),
    Capture(AbstractCapture),
    Call(AbstractCall),
    VariableReference(AbstractVariableReference),
    Raise(Box<AbstractExpression>),
    Group(Box<AbstractExpression>),
}

impl AbstractExpression {
    fn sort_binary_expression(
        left: AbstractExpression,
        token: Token,
        right: AbstractExpression,
    ) -> AbstractExpression {
        let precedence = HashMap::from([
            (TokenType::Equals, 1),
            (TokenType::NotEqual, 1),
            (TokenType::Similar, 1),
            (TokenType::NotSimilar, 1),
            (TokenType::GreaterThan, 2),
            (TokenType::GreaterThanOrEqual, 2),
            (TokenType::LessThanOrEqual, 2),
            (TokenType::LessThan, 2),
            (TokenType::Add, 3),
            (TokenType::Subtract, 3),
            (TokenType::Multiply, 4),
            (TokenType::Divide, 4),
            (TokenType::Modulate, 5),
            (TokenType::Exponentiate, 5),
            (TokenType::Compose, 6),
            (TokenType::Peek, 6),
        ]);

        let sorted_left = left.sorted();
        let sorted_right = right.sorted();
        let prec = precedence[&token.r#type];

        if let AbstractExpression::BinaryOperation(_, ref ltoken, _) = sorted_left {
            let l_prec = precedence[&ltoken.r#type];
            if l_prec < prec {
                if let AbstractExpression::BinaryOperation(lleft, ltoken, lright) = sorted_left {
                    let new_right =
                        AbstractExpression::BinaryOperation(lright, token, sorted_right.into());
                    return AbstractExpression::BinaryOperation(
                        lleft,
                        ltoken,
                        new_right.sorted().into(),
                    );
                }
            }
        }

        if let AbstractExpression::BinaryOperation(_, ref rtoken, _) = sorted_right {
            let r_prec = precedence[&rtoken.r#type];
            if r_prec <= prec {
                if let AbstractExpression::BinaryOperation(rleft, rtoken, rright) = sorted_right {
                    let new_left =
                        AbstractExpression::BinaryOperation(sorted_left.into(), token, rleft);
                    return AbstractExpression::BinaryOperation(
                        new_left.sorted().into(),
                        rtoken,
                        rright,
                    );
                }
            }
        }

        return AbstractExpression::BinaryOperation(sorted_left.into(), token, sorted_right.into());
    }

    fn sort_unary_expression(token: Token, right: AbstractExpression) -> AbstractExpression {
        let sorted_right = right.sorted();

        if let AbstractExpression::BinaryOperation(bleft, btoken, bright) = sorted_right {
            let left = AbstractExpression::UnaryOperation(token, bleft);
            return AbstractExpression::BinaryOperation(
                left.sorted().into(),
                btoken,
                bright.sorted().into(),
            )
            .sorted();
        }

        return AbstractExpression::UnaryOperation(token, sorted_right.into());
    }

    fn sort_raise_expression(exp: AbstractExpression) -> AbstractExpression {
        let sorted_exp = exp.sorted();

        if let AbstractExpression::BinaryOperation(bleft, btoken, bright) = sorted_exp {
            let left = AbstractExpression::Raise(bleft);
            return AbstractExpression::BinaryOperation(
                left.sorted().into(),
                btoken,
                bright.sorted().into(),
            )
            .sorted();
        }

        return AbstractExpression::Raise(sorted_exp.into());
    }

    fn sorted(self) -> Self {
        return match self {
            AbstractExpression::IntLiteral(..)
            | AbstractExpression::RealLiteral(..)
            | AbstractExpression::StrLiteral(..)
            | AbstractExpression::Undefined(..)
            | AbstractExpression::Block(..)
            | AbstractExpression::EmptySet
            | AbstractExpression::VariableReference(..)
            | AbstractExpression::BoolLiteral(..) => self,
            AbstractExpression::Group(abstract_expression) => {
                AbstractExpression::Group(abstract_expression.sorted().into())
            }
            AbstractExpression::Map(map) => AbstractExpression::Map(AbstractMap {
                mappings: map
                    .mappings
                    .into_iter()
                    .map(|(k, v)| (k.sorted(), v.sorted()))
                    .collect(),
            }),
            AbstractExpression::Set(set) => AbstractExpression::Set(AbstractSet {
                items: set.items.into_iter().map(|v| v.sorted()).collect(),
            }),
            AbstractExpression::List(list) => AbstractExpression::List(AbstractList {
                items: list.items.into_iter().map(|v| v.sorted()).collect(),
            }),
            AbstractExpression::UnaryOperation(token, right) => {
                Self::sort_unary_expression(token, *right)
            }
            AbstractExpression::BinaryOperation(left, token, right) => {
                Self::sort_binary_expression(*left, token, *right)
            }
            AbstractExpression::Capture(capture) => AbstractExpression::Capture(AbstractCapture {
                captures: capture
                    .captures
                    .into_iter()
                    .map(|(k, e, v)| (k, e, v.sorted()))
                    .collect(),
                expression: capture.expression.sorted().into(),
            }),
            AbstractExpression::Call(call) => AbstractExpression::Call(AbstractCall {
                func: call.func.sorted().into(),
                args: call.args.sorted(),
            }),
            AbstractExpression::Raise(exp) => Self::sort_raise_expression(*exp),
        };
    }
}

impl Display for AbstractExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AbstractExpression::RealLiteral(token)
                | AbstractExpression::StrLiteral(token)
                | AbstractExpression::BoolLiteral(token)
                | AbstractExpression::Undefined(token)
                | AbstractExpression::IntLiteral(token) => token.value.clone(),
                AbstractExpression::UnaryOperation(token, abstract_expression) => {
                    format!("{}({})", token.value, abstract_expression)
                }
                AbstractExpression::BinaryOperation(
                    abstract_expression,
                    token,
                    abstract_expression1,
                ) => format!(
                    "({} {} {})",
                    abstract_expression, token.value, abstract_expression1
                ),
                AbstractExpression::Block(..) => "(...)".into(),
                AbstractExpression::Map(..) => "{... -> ...}".into(),
                AbstractExpression::Set(..) => "{...}".into(),
                AbstractExpression::EmptySet => "{}".into(),
                AbstractExpression::List(..) => "[...]".into(),
                AbstractExpression::Capture(..) => "Capture...".into(),
                AbstractExpression::Call(..) => "Call...".into(),
                AbstractExpression::VariableReference(..) => "var...".into(),
                AbstractExpression::Raise(exp) => format!("raise({})", exp),
                AbstractExpression::Group(abstract_expression) => {
                    format!("({})", abstract_expression)
                }
            }
        )
    }
}

fn parse_block(ts: &mut TokenStream) -> Result<AbstractBlock, SyntaxError> {
    let _denote_block = ts.etake_type(&TokenType::DenoteBlock);
    let _open_paren = take_sig_of_type(ts, &TokenType::OpenParen);
    let mut statements = Vec::new();

    loop {
        let token = match ts.take_sig() {
            Some(token) => token,
            None => return Err(SyntaxError::ExpectedToken(TokenType::CloseParen, fli!())),
        };

        if token.is_type(&TokenType::CloseParen) {
            break;
        } else if token.is_type(&TokenType::Let) {
            ts.retake();
            statements.push(AbstractStatement::Declaration(parse_let_statement(ts)?));
        } else {
            ts.retake();
            let ts_point = ts.point();

            let var = match parse_variable_reference(ts) {
                Ok(var) => match ts.take_sig() {
                    Some(assign) => match assign.r#type {
                        TokenType::Multiply
                        | TokenType::Subtract
                        | TokenType::Divide
                        | TokenType::Exponentiate
                        | TokenType::Modulate
                        | TokenType::Add => match ts.take_sig() {
                            Some(t) if t.is_type(&TokenType::Assign) => Some((var, Some(assign))),
                            _ => None,
                        },
                        TokenType::Assign => Some((var, None)),
                        _ => None,
                    },
                    None => None,
                },
                Err(_) => None,
            };

            if let Some((var, method)) = var {
                statements.push(AbstractStatement::Update(AbstractUpdate {
                    var,
                    expression: parse_expression_unordered(ts)?,
                    method,
                }));
            } else {
                ts_point.restore(ts);
                statements.push(AbstractStatement::Expression(parse_expression_unordered(
                    ts,
                )?));
            }
        }
    }

    let ts_point = ts.point();
    let r#return = match parse_expression_unordered(ts) {
        Ok(r#return) => Some(r#return.into()),
        Err(_) => {
            ts_point.restore(ts);
            None
        }
    };

    return Ok(AbstractBlock {
        statements,
        r#return,
    });
}

fn parse_let_statement(ts: &mut TokenStream) -> Result<AbstractDeclaration, SyntaxError> {
    let _let = ts.etake_type(&TokenType::Let);
    let open = ts.take_sig().is_some_and(|t| t.is_type(&TokenType::Open));
    if !open {
        ts.retake();
    }

    let r#type = parse_type(ts)?;
    let (identity, r#type) = match parse_identity(ts) {
        Ok(identity) => (identity, Some(r#type)),
        Err(_) => match r#type {
            AbstractType::Base(identity) => {
                ts.retake();
                (identity, None)
            }
            _ => return Err(SyntaxError::ExpectedToken(TokenType::Identity, fli!())),
        },
    };

    let _assign = take_sig_of_type(ts, &TokenType::Assign)?;
    let expression = parse_expression_unordered(ts)?;

    return Ok(AbstractDeclaration {
        open,
        identity,
        r#type,
        expression,
    });
}

fn parse_list(ts: &mut TokenStream) -> Result<AbstractList, SyntaxError> {
    let _open_bracket = ts.etake_type(&TokenType::OpenBracket);
    let mut items = Vec::new();

    loop {
        if take_next_if_type(ts, &TokenType::CloseBracket)? {
            break;
        }

        items.push(parse_expression_unordered(ts)?);
        absorb_comma_or_allow(ts, &TokenType::CloseBracket)?
    }

    return Ok(AbstractList { items });
}

enum MapOrSet {
    Map(AbstractMap),
    Set(AbstractSet),
    Empty,
}

fn parse_capture(ts: &mut TokenStream) -> Result<AbstractCapture, SyntaxError> {
    let _capture = ts.etake_type(&TokenType::Capture);
    let _open_map = take_sig_of_type(ts, &TokenType::OpenMap)?;
    let mut captures = Vec::new();

    loop {
        if take_next_if_type(ts, &TokenType::CloseMap)? {
            break;
        }

        let err = parse_variable_reference(ts)?;
        let var = match ts.take_sig() {
            Some(t) if t.is_type(&TokenType::Map) => None,
            Some(t) if t.is_type(&TokenType::Assign) => {
                let name = parse_identity(ts)?;
                let _map = take_sig_of_type(ts, &TokenType::Map);
                Some(name)
            }
            Some(t) => return Err(SyntaxError::InvalidToken(t, TokenType::Map, fli!())),
            None => return Err(SyntaxError::ExpectedToken(TokenType::Map, fli!())),
        };

        let expression = parse_expression_unordered(ts)?;
        captures.push((err, var, expression));

        absorb_comma_or_allow(ts, &TokenType::CloseMap)?;
    }

    let expression = parse_expression_unordered(ts)?;

    return Ok(AbstractCapture {
        captures,
        expression: expression.into(),
    });
}

fn parse_map_or_set(ts: &mut TokenStream) -> Result<MapOrSet, SyntaxError> {
    let _open_map = ts.etake_type(&TokenType::OpenMap);
    let mut setmap = MapOrSet::Empty;

    loop {
        if take_next_if_type(ts, &TokenType::CloseMap)? {
            break;
        }

        let item1 = parse_expression_unordered(ts)?;
        let next = match ts.take_sig() {
            Some(next) => next,
            None => return Err(SyntaxError::ExpectedToken(TokenType::CloseMap, fli!())),
        };

        match next.r#type {
            TokenType::CloseMap | TokenType::Comma => {
                match setmap {
                    MapOrSet::Set(ref mut set) => set.items.push(item1),
                    MapOrSet::Empty => setmap = MapOrSet::Set(AbstractSet { items: vec![item1] }),
                    MapOrSet::Map(_) => {
                        return Err(SyntaxError::InvalidToken(next, TokenType::Map, fli!()))
                    }
                }
                ts.retake();
            }
            TokenType::Map => match setmap {
                MapOrSet::Map(ref mut map) => {
                    let value = parse_expression_unordered(ts)?;
                    let mapping = (item1, value);
                    map.mappings.push(mapping);
                }
                MapOrSet::Empty => {
                    let value = parse_expression_unordered(ts)?;
                    setmap = MapOrSet::Map(AbstractMap {
                        mappings: vec![(item1, value)],
                    })
                }
                MapOrSet::Set(_) => {
                    return Err(SyntaxError::InvalidToken(next, TokenType::CloseMap, fli!()))
                }
            },
            _ => {
                return Err(SyntaxError::InvalidToken(
                    next,
                    match setmap {
                        MapOrSet::Map(_) => TokenType::Map,
                        MapOrSet::Set(_) | MapOrSet::Empty => TokenType::CloseMap,
                    },
                    fli!(),
                ))
            }
        };

        absorb_comma_or_allow(ts, &TokenType::CloseMap)?;
    }

    return Ok(setmap);
}

fn parse_binary_expression(
    ts: &mut TokenStream,
    left: AbstractExpression,
) -> Result<AbstractExpression, SyntaxError> {
    let operon = ts.etake();
    let right = parse_expression_unordered(ts)?;
    return Ok(AbstractExpression::BinaryOperation(
        left.into(),
        operon,
        right.into(),
    ));
}

fn parse_call(ts: &mut TokenStream, func: AbstractExpression) -> Result<AbstractCall, SyntaxError> {
    let _open_paren = ts.etake_type(&TokenType::OpenParen);
    let mut args = {
        let ts_point = ts.point();
        let c = match parse_identity(ts) {
            Ok(_) => match take_sig_of_type(ts, &TokenType::Assign) {
                Ok(_) => AbstractCallArgs::NamedArgs(Vec::new()),
                Err(_) => AbstractCallArgs::ArgList(Vec::new()),
            },
            Err(_) => AbstractCallArgs::ArgList(Vec::new()),
        };
        ts_point.restore(ts);
        c
    };

    loop {
        if take_next_if_type(ts, &TokenType::CloseParen)? {
            break;
        }

        match args {
            AbstractCallArgs::ArgList(ref mut args) => {
                let arg = parse_expression_unordered(ts)?;
                args.push(arg);
            }
            AbstractCallArgs::NamedArgs(ref mut args) => {
                let name = parse_identity(ts)?;
                let _assign = take_sig_of_type(ts, &TokenType::Assign)?;
                let val = parse_expression_unordered(ts)?;

                args.push((name, val));
            }
        }

        absorb_comma_or_allow(ts, &TokenType::CloseParen)?;
    }

    return Ok(AbstractCall {
        func: func.into(),
        args,
    });
}

fn parse_expression_unordered(ts: &mut TokenStream) -> Result<AbstractExpression, SyntaxError> {
    let mut exp = None;
    loop {
        let token = match ts.take_sig() {
            Some(token) => token,
            None => match exp {
                Some(_) => break,
                None => {
                    return Err(SyntaxError::ExpectedToken(
                        TokenType::IncompleteToken,
                        fli!(),
                    ))
                }
            },
        };

        let newexp = match exp {
            Some(_) => match token.r#type {
                TokenType::OpenParen => {
                    ts.retake();
                    AbstractExpression::Call(parse_call(ts, exp.expect("EXP MUST EXIST HERE"))?)
                }

                TokenType::Multiply
                | TokenType::Subtract
                | TokenType::GreaterThan
                | TokenType::Divide
                | TokenType::LessThan
                | TokenType::Peek
                | TokenType::NotEqual
                | TokenType::NotSimilar
                | TokenType::Similar
                | TokenType::GreaterThanOrEqual
                | TokenType::Equals
                | TokenType::Compose
                | TokenType::Modulate
                | TokenType::LessThanOrEqual
                | TokenType::Add
                | TokenType::Exponentiate => {
                    ts.retake();
                    parse_binary_expression(ts, exp.expect("EXP MUST EXIST HERE"))?
                }
                _ => {
                    ts.retake();
                    break;
                }
            },
            None => match token.r#type {
                TokenType::RealLit => AbstractExpression::RealLiteral(token),
                TokenType::IntLit => AbstractExpression::IntLiteral(token),
                TokenType::StrLit => AbstractExpression::StrLiteral(token),
                TokenType::True | TokenType::False => AbstractExpression::BoolLiteral(token),
                TokenType::Undefined => AbstractExpression::Undefined(token),
                TokenType::Factorial | TokenType::Subtract => {
                    AbstractExpression::UnaryOperation(token, parse_expression(ts)?.into())
                }
                TokenType::Capture => {
                    ts.retake();
                    AbstractExpression::Capture(parse_capture(ts)?)
                }
                TokenType::OpenParen => {
                    let exp = parse_expression_unordered(ts)?;
                    let _close_paren = take_sig_of_type(ts, &TokenType::CloseParen);
                    AbstractExpression::Group(exp.into())
                }
                TokenType::Raise => {
                    AbstractExpression::Raise(parse_expression_unordered(ts)?.into())
                }
                TokenType::DenoteBlock => {
                    ts.retake();
                    AbstractExpression::Block(parse_block(ts)?)
                }
                TokenType::Identity => {
                    ts.retake();
                    AbstractExpression::VariableReference(parse_variable_reference(ts)?)
                }
                TokenType::OpenBracket => {
                    ts.retake();
                    AbstractExpression::List(parse_list(ts)?)
                }
                TokenType::OpenMap => {
                    ts.retake();
                    let map_or_list = parse_map_or_set(ts)?;
                    match map_or_list {
                        MapOrSet::Map(map) => AbstractExpression::Map(map),
                        MapOrSet::Set(set) => AbstractExpression::Set(set),
                        MapOrSet::Empty => AbstractExpression::EmptySet,
                    }
                }
                _ => {
                    return Err(SyntaxError::InvalidToken(
                        token,
                        TokenType::IncompleteToken,
                        fli!(),
                    ));
                }
            },
        };

        exp = Some(newexp);
    }

    return match exp {
        Some(exp) => Ok(exp),
        None => Err(match ts.take_sig() {
            Some(token) => SyntaxError::InvalidToken(token, TokenType::IncompleteToken, fli!()),
            None => SyntaxError::ExpectedToken(TokenType::IncompleteToken, fli!()),
        }),
    };
}

pub(super) fn parse_expression(ts: &mut TokenStream) -> Result<AbstractExpression, SyntaxError> {
    let exp = parse_expression_unordered(ts)?;
    let sorted_exp = exp.sorted();
    println!("{}", sorted_exp);
    return Ok(sorted_exp);
}
