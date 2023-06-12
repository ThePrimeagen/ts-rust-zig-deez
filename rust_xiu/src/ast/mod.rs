mod error;
#[cfg(test)]
mod util;

use crate::{
    lex::Token,
    parse::{self, Parse, Parser, Precedence},
};
use std::{cell::RefCell, rc::Rc};

pub use error::{Error, Result};

pub type SharedRef<T> = Rc<RefCell<T>>;

pub struct AST {
    pub statements: Vec<Statement>,
}

impl<'a> Parse<'a> for AST {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<AST> {
        let mut statements = vec![];
        while !parser.current_token_is(Token::Eof) {
            statements.push(Statement::parse(parser, None)?);

            parser.step()?;
        }

        Ok(AST { statements })
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Let),
    Return(Return),
    Block(Block),
    Expression(Expression),
}

impl<'a> Parse<'a> for Statement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        let parser_expression_statement = |parser: &mut Parser| {
            let expression = Self::Expression(Expression::parse(parser, Some(Precedence::Lowest))?);

            match &parser.next_token {
                Token::Semicolon => {
                    parser.step()?;

                    Ok(expression)
                }
                token => unexpected_token_error(&token),
            }
        };

        match &parser.current_token {
            Token::Let => Ok(Self::Let(Let::parse(parser, None)?)),
            Token::Return => Ok(Self::Return(Return::parse(parser, None)?)),
            Token::LSquirly => Ok(Self::Block(Block::parse(parser, None)?)),
            _ => parser_expression_statement(parser),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Let {
    pub name: Identifier,
    pub value: Expression,
}

impl<'a> Parse<'a> for Let {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        let name = Identifier::parse(parser, None)?;

        parser.step()?;
        if parser.next_token != Token::Assign {
            return unexpected_token_error(&parser.next_token);
        }

        let value = Expression::parse(parser, Some(Precedence::Lowest))?;

        Ok(Self { name, value })
    }
}

#[derive(Debug, PartialEq)]
pub struct Return {
    pub value: Expression,
}

impl<'a> Parse<'a> for Return {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        parser.step()?;
        Ok(Self {
            value: Expression::parse(parser, Some(Precedence::Lowest))?,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Block(Vec<Statement>);

impl<'a> Parse<'a> for Block {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        let mut statements = vec![];
        while parser.next_token != Token::RSquirly {
            statements.push(Statement::parse(parser, None)?);
        }

        parser.step()?;

        Ok(Self(statements))
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Primitive(Primitive),
    PrefixOperator(PrefixOperator),
    InfixOperator(PrefixOperator),
    Conditional(Conditional),
    FunctionDefinition(FunctionDefinition),
    FunciontCall(FunctionCall),
}

impl<'a> Parse<'a> for Expression {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        match &parser.current_token {
            Token::True | Token::False | Token::Int(_) => {
                Ok(Self::Primitive(Primitive::parse(parser, None)?))
            }
            token => {
                eprintln!(
                    "fuck we found a {token:?} followed by a {:?}",
                    parser.next_token
                );

                panic!()
            }
        }

        // todo!("expression parser")
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String,
}

impl<'a> Parse<'a> for Identifier {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        match &parser.next_token {
            Token::Ident(value) => Ok(Self {
                value: value.to_owned(),
            }),
            token => unexpected_token_error(&token),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Primitive {
    Int(i64),
    Bool(bool),
}

impl<'a> Parse<'a> for Primitive {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        let primitive = match &parser.current_token {
            Token::Int(value) => Self::Int(value.parse()?),
            Token::True => Self::Bool(true),
            Token::False => Self::Bool(false),
            token => return unexpected_token_error(&token),
        };

        parser.step()?;

        Ok(primitive)
    }
}

#[derive(Debug, PartialEq)]
pub enum PrefixOperatorKind {
    Not,
    Negative,
}

#[derive(Debug, PartialEq)]
pub struct PrefixOperator {
    pub kind: PrefixOperatorKind,
    pub rest: Box<Expression>,
}

impl<'a> Parse<'a> for PrefixOperator {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub enum InfixOperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreateThanOrEqual,
    LessThanOrEqual,
}

#[derive(Debug, PartialEq)]
pub struct InfixOperator {
    pub kind: InfixOperatorKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

impl<'a> Parse<'a> for InfixOperator {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct Conditional {
    pub condition: Box<Expression>,
    pub resolution: Block,
    pub alternative: Option<Block>,
}

impl<'a> Parse<'a> for Conditional {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        if parser.next_token != Token::Lparen {
            return unexpected_token_error(&parser.next_token);
        }

        parser.step()?;
        parser.step()?;
        let condition = Box::new(Expression::parse(parser, Some(Precedence::Lowest))?);

        if parser.current_token != Token::Rparen {
            return unexpected_token_error(&parser.next_token);
        }

        let try_parse_block = |parser: &mut Parser| {
            if parser.next_token != Token::LSquirly {
                return unexpected_token_error(&parser.next_token);
            }

            parser.step()?;
            parser.step()?;

            Block::parse(parser, None)
        };

        let resolution = try_parse_block(parser)?;

        parser.step()?;
        let alternative = match &parser.current_token {
            Token::Else => Some(try_parse_block(parser)?),
            _ => None,
        };

        Ok(Conditional {
            condition,
            resolution,
            alternative,
        })
    }
}

#[test]
fn if_statement() -> parse::Result<()> {
    let input = r#"
        if (true) {
            return true;
        } else {
            return false;
        }
    "#;

    macro_rules! bool {
        ($value:expr) => {
            Expression::Primitive(Primitive::Bool($value))
        };
    }

    macro_rules! stmt {
        ($value:expr) => {
            Block(vec![Statement::Return(Return {
                value: bool!($value),
            })])
        };
    }

    let expected = Conditional {
        condition: Box::new(bool!(true)),
        resolution: stmt!(true),
        alternative: Some(stmt!(false)),
    };

    let mut lexer = crate::lex::Lexer::new(input.to_owned());
    let mut parser = Parser::new(&mut lexer)?;
    let actual = Conditional::parse(&mut parser, None)?;

    assert_eq!(expected, actual);

    Ok(())
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    pub parameters: Vec<Identifier>,
    pub body: Block,
}

impl<'a> Parse<'a> for FunctionDefinition {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl<'a> Parse<'a> for FunctionCall {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> parse::Result<Self> {
        todo!()
    }
}

fn unexpected_token_error<T>(token: &Token) -> parse::Result<T> {
    Err(parse::Error::UnexpectedToken(token.to_owned()))
}
