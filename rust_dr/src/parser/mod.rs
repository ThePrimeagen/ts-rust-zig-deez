use crate::{
    ast::{Expression, Identifier, LetStatement, Literal, Program, Statement, ReturnStatement},
    lexer::lexer_nom::{Lexer, Token},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().unwrap();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.cur_token != Token::Eof {
            if let Some(statement) = self.parse_statement(self.cur_token.clone()) {
                program.statements.push(statement);
            }
            self.next_token();
        }

        Some(program)
    }

    fn parse_statement(&mut self, cur_token: Token) -> Option<Statement> {
        match cur_token {
            // Handle other kinds of tokens
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        self.next_token(); // Expect the next token to be an identifier
        let ident = if let Token::Ident(name) = &self.cur_token {
            Identifier {
                name: name.to_string(),
            }
        } else {
            // Error: expected identifier
            self.errors.push(format!(
                "expected next token to be IDENT, got {:?}",
                self.cur_token
            ));
            return None;
        };

        self.next_token(); // Expect the next token to be an assignment
        if self.cur_token != Token::Assign {
            // Error: expected assignment
            self.errors.push(format!(
                "expected next token to be '=', got {:?}",
                self.cur_token
            ));
            return None;
        }

        self.next_token(); // Expect the next token to be an expression
        let expr = match &self.cur_token {
            Token::Int(value) => Expression::Literal(Literal {
                value: value.to_string(),
            }),
            Token::Ident(name) => Expression::Identifier(Identifier {
                name: name.to_string(),
            }),
            // Note: this will fail if the token is not a literal or identifier.
            // If your language allows more complex expressions on the right-hand side,
            // you should call a more comprehensive parse_expression function here.
            _ => {
                // Error: expected expression
                self.errors.push(format!(
                    "expected next token to be INT or IDENT, got {:?}",
                    self.cur_token
                ));
                return None;
            }
        };

        Some(Statement::Let(LetStatement { value: expr, ident }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token(); // Expect the next token to be an expression
    
        let expr = match &self.cur_token {
            Token::Int(value) => Expression::Literal(Literal {
                value: value.to_string(),
            }),
            Token::Ident(name) => Expression::Identifier(Identifier {
                name: name.to_string(),
            }),
            // Note: this will fail if the token is not a literal or identifier.
            // If your language allows more complex expressions on the right-hand side,
            // you should call a more comprehensive parse_expression function here.
            _ => {
                // Error: expected expression
                self.errors.push(format!("expected next token to be INT or IDENT, got {:?}", self.cur_token));
                return None;
            }
        };
    
        Some(Statement::Return(ReturnStatement { value: expr }))
    }
    
}

#[test]
fn test_let_statements() {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    assert!(program.is_some(), "ParseProgram() returned None");

    // Check for parsing errors
    assert!(p.errors.is_empty(), "Parser has errors: {:?}", p.errors);

    let program = program.unwrap();
    assert_eq!(
        program.statements.len(),
        3,
        "program.Statements does not contain 3 statements. got={}",
        program.statements.len()
    );

    let tests = vec![("x", "5"), ("y", "10"), ("foobar", "838383")];

    for (i, (ident_name, literal_value)) in tests.iter().enumerate() {
        match &program.statements[i] {
            Statement::Let(let_stmt) => {
                assert_eq!(let_stmt.ident.name, *ident_name);

                // Check the value of the let statement
                match &let_stmt.value {
                    Expression::Literal(literal) => {
                        assert_eq!(literal.value, *literal_value);
                    }
                    _ => panic!("Expected Literal"),
                }
            },
            _ => panic!("Expected LetStatement"),
        }
    }
}

#[test]
fn test_parser_errors() {
    let input = "let x 5;";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let _program = p.parse_program();

    let program = p.parse_program();
    assert!(program.is_some(), "ParseProgram() returned None");

    // Check for parsing errors
    assert!(!p.errors.is_empty(), "Parser has no errors");
    
    assert_eq!(
        p.errors.len(),
        1,
        "parser has wrong number of errors. got={}",
        p.errors.len()
    );
}
