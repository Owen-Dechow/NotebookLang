use crate::{
    fli,
    lexer::{Token, TokenStream, TokenType},
    parser::{
        absorb_comma_or_allow,
        abstract_expression::{parse_expression, AbstractExpression},
        abstract_type::{parse_type, AbstractType},
        parse_identity,
        syntax_error::SyntaxError,
        take_next_if_type, take_sig_of_type,
    },
};

#[derive(Debug)]
pub(crate) struct AbstractArgument {
    name: Token,
    r#type: AbstractType,
}

#[derive(Debug)]
pub(crate) enum AbstractErrorDeclaration {
    All,
    Explicit(AbstractVariableReference),
}

#[derive(Debug)]
pub(crate) struct AbstractFunction {
    name: Token,
    r#type: Option<AbstractType>,
    r#static: bool,
    args: Vec<AbstractArgument>,
    expression: AbstractExpression,
    errors: Vec<AbstractErrorDeclaration>,
}

#[derive(Debug)]
pub(crate) struct AbstractConst {
    name: Token,
    r#static: bool,
    r#type: Option<AbstractType>,
    expression: AbstractExpression,
}

#[derive(Debug)]
pub(crate) struct AbstractStructure {
    name: Token,
    functions: Vec<AbstractFunction>,
    constants: Vec<AbstractConst>,
    impl_funcs: Vec<AbstractFunction>,
    impl_consts: Vec<AbstractConst>,
}

#[derive(Debug)]
pub(crate) struct AbstractVariableReference {
    pub(super) identities: Vec<Token>,
}

enum ASTComponent {
    Func(AbstractFunction),
    ImplFunc(AbstractFunction),
    Const(AbstractConst),
    ImplConst(AbstractConst),
    ImplDefConst(AbstractConst),
    ImplDefFunc(AbstractConst),
}

#[derive(Debug)]
pub(crate) struct AbstractSyntaxTree {
    structures: Vec<AbstractStructure>,
    constants: Vec<AbstractConst>,
    functions: Vec<AbstractFunction>,
    syntax_errors: Vec<SyntaxError>,
}

impl AbstractSyntaxTree {
    pub(super) fn from_token_stream<'a>(ts: &'a mut TokenStream) -> Self {
        let mut structures = Vec::new();
        let mut errors = Vec::new();
        let mut functions = Vec::new();
        let mut constants = Vec::new();
        while let Some(t) = ts.take_sig() {
            ts.retake();

            match t.r#type {
                TokenType::Def => match parse_structure(ts) {
                    Ok(structure) => {
                        let structure = structure;
                        structures.push(structure);
                    }
                    Err(err) => {
                        errors.push(err);
                    }
                },
                TokenType::Let => match parse_expression_definition(ts, false) {
                    Ok(ok) => match ok {
                        ASTComponent::Func(abstract_function) => functions.push(abstract_function),
                        ASTComponent::Const(abstract_const) => constants.push(abstract_const),
                        ASTComponent::ImplDefConst(abstract_const) => todo!(),
                        ASTComponent::ImplDefFunc(abstract_const) => todo!(),
                        _ => unreachable!(),
                    },
                    Err(err) => {
                        errors.push(err);
                    }
                },
                _ => {
                    let err = SyntaxError::InvalidToken(t, TokenType::IncompleteToken, fli!());
                    err.print_error();
                    errors.push(err);
                }
            }
        }

        return Self {
            structures,
            constants,
            functions,
            syntax_errors: errors,
        };
    }
}

pub(super) fn parse_variable_reference(
    ts: &mut TokenStream,
) -> Result<AbstractVariableReference, SyntaxError> {
    let mut identities = vec![parse_identity(ts)?];

    loop {
        if ts.take_sig().is_some_and(|t| t.is_type(&TokenType::Peek)) {
            identities.push(parse_identity(ts)?);
        } else {
            ts.retake();
            break;
        }
    }

    return Ok(AbstractVariableReference { identities });
}

fn parse_argument_definitions(
    ts: &mut TokenStream,
) -> Result<(Vec<AbstractArgument>, Vec<AbstractErrorDeclaration>), SyntaxError> {
    let mut args = Vec::new();
    let _open_paren = take_sig_of_type(ts, &TokenType::OpenParen)?;

    loop {
        if take_next_if_type(ts, &TokenType::CloseParen)? {
            break;
        }

        let r#type = parse_type(ts)?;
        let name = parse_identity(ts)?;

        args.push(AbstractArgument { name, r#type });
        absorb_comma_or_allow(ts, &TokenType::CloseParen)?;
    }

    let mut error_types = Vec::new();
    loop {
        if ts
            .take_sig()
            .is_some_and(|t| t.is_type(&TokenType::CanExcept))
        {
            let error = match ts.take_sig() {
                Some(t) if t.is_type(&TokenType::Identity) => {
                    ts.retake();
                    AbstractErrorDeclaration::Explicit(parse_variable_reference(ts)?)
                }
                Some(t) if t.is_type(&TokenType::Multiply) => AbstractErrorDeclaration::All,
                Some(t) => return Err(SyntaxError::InvalidToken(t, TokenType::Identity, fli!())),
                None => return Err(SyntaxError::ExpectedToken(TokenType::Identity, fli!())),
            };

            error_types.push(error);
        } else {
            ts.retake();
            break;
        }
    }

    return Ok((args, error_types));
}

fn parse_expression_definition(
    ts: &mut TokenStream,
    in_struct: bool,
) -> Result<ASTComponent, SyntaxError> {
    let leading_token = ts.etake();
    let is_impl = match leading_token.r#type {
        TokenType::Let => false,
        TokenType::Impl => true,
        _ => {
            return Err(SyntaxError::InvalidToken(
                leading_token,
                TokenType::Let,
                fli!(),
            ))
        }
    };

    let r#static = match in_struct {
        true => {
            let is_static = ts.take_sig().is_some_and(|t| t.is_type(&TokenType::Static));
            if !is_static {
                ts.retake();
            }
            is_static
        }
        false => true,
    };

    let point = ts.point();
    let possible_type = parse_type(ts)?;

    let (r#type, name) = match parse_identity(ts) {
        Ok(name) => (Some(possible_type), name),
        Err(_) => {
            point.restore(ts);
            (None, parse_identity(ts)?)
        }
    };

    let args = match ts.take_sig() {
        Some(t) if t.is_type(&TokenType::OpenParen) => {
            ts.retake();
            Some(parse_argument_definitions(ts)?)
        }
        _ => {
            ts.retake();
            None
        }
    };

    if is_impl && !in_struct {
        return todo!();
    }

    let _assign = take_sig_of_type(ts, &TokenType::Assign);
    let expression = parse_expression(ts)?;

    return Ok(match args {
        Some((args, errors)) => {
            let func = AbstractFunction {
                name,
                r#type,
                args,
                errors,
                r#static,
                expression,
            };
            match is_impl {
                true => ASTComponent::ImplFunc(func),
                false => ASTComponent::Func(func),
            }
        }
        None => {
            let constant = AbstractConst {
                name,
                r#type,
                r#static,
                expression,
            };
            match is_impl {
                true => ASTComponent::ImplConst(constant),
                false => ASTComponent::Const(constant),
            }
        }
    });
}

fn parse_structure(ts: &mut TokenStream) -> Result<AbstractStructure, SyntaxError> {
    let _def = ts.etake_type(&TokenType::Def);
    let name = parse_identity(ts)?;
    let _open_struct = take_sig_of_type(ts, &TokenType::OpenParen)?;

    let mut functions = Vec::new();
    let mut constants = Vec::new();
    let mut impl_funcs = Vec::new();
    let mut impl_consts = Vec::new();

    loop {
        match ts.take_sig() {
            Some(t) if t.is_type(&TokenType::CloseParen) => break,
            Some(_) => {
                ts.retake();
                match parse_expression_definition(ts, true)? {
                    ASTComponent::Func(abstract_function) => functions.push(abstract_function),
                    ASTComponent::ImplFunc(abstract_function) => impl_funcs.push(abstract_function),
                    ASTComponent::Const(abstract_const) => constants.push(abstract_const),
                    ASTComponent::ImplConst(abstract_const) => impl_consts.push(abstract_const),
                    _ => unreachable!(),
                }
            }
            None => return Err(SyntaxError::ExpectedToken(TokenType::CloseParen, fli!())),
        }
    }

    return Ok(AbstractStructure {
        name,
        functions,
        constants,
        impl_funcs,
        impl_consts,
    });
}
