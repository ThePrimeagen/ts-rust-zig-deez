#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    // other kinds of statements go here
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    // other kinds of expressions go here
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct LetStatement {
    pub ident: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct Literal {
    pub value: String,
}
