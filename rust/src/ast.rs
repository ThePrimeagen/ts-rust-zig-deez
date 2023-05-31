mod error;

use std::{cell::RefCell, rc::Rc};

pub type SharedRef<T> = Rc<RefCell<T>>;

// pub trait AsAny {
//     fn as_any(&self) -> &dyn Any;
// }

// pub trait Node: Debug {
//     fn token_literal(&self) -> String;

//     fn as_any(&self) -> &dyn Any;
// }

// pub trait Statement: Node {}

// pub trait Expression: Node {}

pub struct AST {
    statements: Vec<Statement>,
}

pub enum Statement {
    Let(Let),
    Return(Return),
    Block(Block),
    Expression(Expression),
}

pub struct Let {
    name: Identifier,
    value: Expression,
}

pub struct Return {
    value: Expression,
}

pub struct Block(Vec<Statement>);

pub enum Expression {
    Identifier(Identifier),
    Primitive(Primitive),
    PrefixOperator(PrefixOperator),
    InfixOperator(PrefixOperator),
    Conditional(Conditional),
    FunctionDefinition(FunctionDefinition),
    FunciontCall(FunctionCall),
}

pub struct Identifier {
    value: String,
}

pub enum Primitive {
    Int(i64),
    Bool(bool),
}

pub struct PrefixOperator {
    kind: PrefixOperatorKind,
    rest: Box<Expression>,
}

pub enum PrefixOperatorKind {
    Not,
    Negative,
}

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

pub struct InfixOperator {
    kind: InfixOperatorKind,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

pub struct Conditional {
    condition: Box<Expression>,
    resolution: Block,
    alternative: Block,
}

pub struct FunctionDefinition {
    parameters: Vec<Identifier>,
    body: Block,
}

pub struct FunctionCall {
    function: Box<Expression>,
    arguments: Vec<Expression>,
}

// fn temp() {
//     let temp: SharedRef<dyn Expression> = Rc::new(RefCell::new(Atom {
//         token: Token::Int("10".to_owned()),
//         value: "10".to_owned(),
//     }));

//     temp.borrow().as_any().downcast_ref::<Atom>();
// }

// // trait Downcast {
// //     fn downcast()
// // }

// // impl AsAny for dyn Expression {
// //     fn as_any(&self) -> &dyn Any {
// //         &self
// //     }
// // }

// // pub trait FromUpcast<'a>
// // where
// //     Self: Sized + 'static,
// // {
// //     fn from_upcast<Source: AsAny>(value: &'a Source) -> Result<&'a Self> {
// //         value
// //             .as_any()
// //             .downcast_ref::<Self>()
// //             .ok_or(Error::DowncastFailed)
// //     }
// // }

// // impl<'a, T: AsAny + 'static> FromUpcast<'a> for Box<dyn Expression> {
// //     fn from_upcast<T>(value: &'a T) -> Result<&'a Self> {
// //         value
// //             .as_any()
// //             .downcast_ref::<Self>()
// //             .ok_or(Error::DowncastFailed)
// //     }
// // }

// // impl PartialEq for Box<dyn Expression> {
// //     fn eq(&self, other: &Self) -> bool {
// //         let lhs = match self.as_any().downcast_ref::<T>() {
// //             Ok(lhs) => lhs,
// //             Err(err) => return false,
// //         };

// //         match other.as_any().downcast_ref::<T>() {
// //             Ok(rhs) => lhs == rhs,
// //             Err(err) => false,
// //         }
// //     }
// // }

// // impl<T: PartialEq> PartialEq for Box<dyn Expression> {
// //     fn eq(&self, other: &Self) -> bool {
// //         let lhs = match self.as_any().downcast_ref::<T>() {
// //             Ok(lhs) => lhs,
// //             Err(err) => return false,
// //         };

// //         match other.as_any().downcast_ref::<T>() {
// //             Ok(rhs) => lhs == rhs,
// //             Err(err) => false,
// //         }
// //     }
// // }

// #[derive(Debug)]
// pub struct AST {
//     pub statements: Vec<SharedRef<dyn Statement>>,
// }

// impl Node for AST {
//     fn token_literal(&self) -> String {
//         self.statements.get(0).map_or("".to_owned(), |statement| {
//             statement.as_ref().borrow().token_literal()
//         })
//     }

//     fn as_any(&self) -> &dyn Any {
//         self
//     }
// }

// #[derive(Debug, PartialEq)]
// pub struct Atom {
//     pub token: Token,
//     pub value: String,
// }

// // impl<'a> Atom {
// //     pub fn from_upcast(value: &'a Box<dyn Expression>) -> Result<&'a Self> {
// //         value
// //             .as_any()
// //             .downcast_ref::<Self>()
// //             .ok_or(Error::DowncastFailed)
// //     }
// // }

// impl Node for Atom {
//     fn token_literal(&self) -> String {
//         format!("{:?}", self.token)
//     }

//     fn as_any(&self) -> &dyn Any {
//         self
//     }
// }

// // impl AsAny for Atom {
// // fn as_any<'a>(&'a self) -> &'a dyn Any {
// //     todo!()
// // }
// // }

// impl Expression for Atom {}

// #[derive(Debug, PartialEq)]
// pub struct Identifier {
//     pub value: String,
// }

// // impl Node for Identifier {
// //     fn token_literal(&self) -> String {
// //         format!("{:?}", self.token)
// //     }

// //     fn as_any(&self) -> &dyn Any {
// //         self
// //     }
// // }

// // impl AsAny for Identifier {
// // fn as_any(&self) -> &dyn Any {
// //     todo!()
// // }
// // }

// impl Expression for Identifier {}

// #[derive(Debug)]
// pub struct Let {
//     pub token: Token,
//     pub name: Identifier,
//     pub value: Box<dyn Expression>,
// }

// // impl PartialEq for Box<dyn Expression> {
// //     fn eq(&self, other: &Self) -> bool {
// //         self == other
// //     }
// // }

// impl Node for Let {
//     fn token_literal(&self) -> String {
//         format!("{:?}", self.token)
//     }

//     fn as_any(&self) -> &dyn Any {
//         self
//     }
// }

// // impl AsAny for Let {
// // fn as_any(&self) -> &dyn Any {
// //     self
// // }
// // }

// // impl FromUpcast

// // impl Statement for Let {}

// // impl<'a> From<SharedRef<dyn Statement>> for Let<'a> {
// //     fn from(value: SharedRef<dyn Statement>) -> Self {
// //         let let_statement: Self = unsafe { std::mem::transmute(value.borrow()) };
// //         // unsafe { std::mem::transmute::<Ref<dyn Statement>, Ref<Self>>(value.borrow()) };
// //         // Self { token: (), ident: (), value: () }
// //         todo!()
// //     }
// // }
// //

// #[macro_export]
// macro_rules! atom {
//     ($tt:expr, $value:expr) => {
//         $crate::ast::Atom {
//             token: $tt,
//             value: $value.to_owned(),
//         }
//     };
// }

// #[macro_export]
// macro_rules! identifier {
//     ($value:expr) => {
//         $crate::ast::Identifier {
//             token: Token::Ident($value.to_owned()),
//             value: $value.to_owned(),
//         }
//     };
// }

// #[macro_export]
// macro_rules! let_statement {
//     ($name:expr, $value:expr, $tt:expr) => {
//         $crate::ast::Let {
//             token: Token::Let,
//             name: $crate::identifier!($name),
//             value: Box::new($crate::atom!($tt, $value)),
//         }
//     };
// }
