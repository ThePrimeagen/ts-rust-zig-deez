#[macro_export]
macro_rules! let_stmt {
    ($name:expr => $value:expr) => {
        $crate::ast::Statement::Let($crate::ast::Let {
            name: $crate::ident!($name),
            value: $value,
        })
    };
}

#[macro_export]
macro_rules! ident {
    ($name:expr) => {
        $crate::ast::Identifier {
            value: $name.to_owned(),
        }
    };
}

#[macro_export]
macro_rules! int {
($value:expr) => {
    $crate::primitive!(Int => $value)
};
}

#[macro_export]
macro_rules! bool {
($value:expr) => {
    $crate::primitive!(Bool => $value)
};
}

#[macro_export]
macro_rules! primitive {
    ($kind:tt => $value:expr) => {
        $crate::ast::Expression::Primitive($crate::ast::Primitive::$kind($value))
    };
}
