// use super::{
//     error::{Error, Result},
//     Parse, Parser,
// };
// use crate::{ast::AST, bool, int, let_stmt, Lexer};

// #[test]
// fn let_statement() -> Result<()> {
//     let input = r#"
// let a = 5;
// let b = -10;
// let c = true;
// let d = false;
// "#;

//     let mut lexer = Lexer::new(input.to_owned());
//     let mut parser = Parser::new(&mut lexer)?;

//     let ast = AST::parse(&mut parser)?;
//     let ast_len = ast.statements.len();
//     if ast_len != 3 {
//         return Err(Error::IncorrectStatementCount {
//             expected: 3,
//             actual: ast_len,
//         });
//     }

//     ast.statements
//         .iter()
//         .zip([
//             let_stmt!("a" => int!(5)),
//             let_stmt!("b" => int!(-10)),
//             let_stmt!("c" => bool!(true)),
//             let_stmt!("d" => bool!(false)),
//         ])
//         .for_each(|(expected, actual)| {
//             assert_eq!(expected, &actual);
//         });

//     Ok(())
// }
