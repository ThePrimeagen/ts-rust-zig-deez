use super::{
    error::{Error, Result},
    Parser,
};
use crate::{
    ast::{Let, Statement},
    let_statement,
    lexer::Token,
    Lexer,
};

#[test]
fn let_statement() -> Result<()> {
    let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;"#;

    let mut lexer = Lexer::new(input.to_owned());
    let mut parser = Parser::new(&mut lexer)?;

    let ast = parser.parse()?;
    let ast_len = ast.statements.len();
    if ast_len != 3 {
        return Err(Error::IncorrectStatementCount {
            expected: 3,
            actual: ast_len,
        });
    }

    [let_statement!("x", "5", Token::Int("5".to_owned()))]
        // [crate::ast::Let {
        //     token: Token::Let,
        //     name: "x".to_owned(),
        //     value: "5".to_owned(),
        // }] // expected
        .into_iter()
        .enumerate()
        .map(|(i, identifier)| {
            let statement = ast.statements[i].borrow();
            let temp: Option<&Let> = statement.as_any().downcast_ref::<Let>();
            assert_eq!(Some(&identifier), temp);

            Ok(())
        })
        .collect::<Result<()>>()

    // Ok(())

    // todo!()
}

fn compare_let_statement(statement: Box<dyn Statement>, name: String) -> bool {
    todo!()
}
