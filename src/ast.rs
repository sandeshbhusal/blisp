#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Operator {
    Plus,
    Minus,
    Times,
    Divide,
    Assign,

    Le,
    Ge,
    Lt,
    Gt,
    Eq,
    Ne,

    Not,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeName {
    Int,
    Float,
    Bool,
    Unit,
    UDT(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct Decl {
    pub ident: String,
    pub typename: TypeName,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Value {
    Integer(i32),
    Float(f32),
    Boolean(bool),
    Nil,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expression {
    Binary {
        op: Operator,
        operands: Vec<Expression>,
    },
    FuncCall {
        ident: String,
        args: Vec<Expression>,
    },
    Boolean(String),
    Integer(String),
    Float(String),
    Identifier(String),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Param {
    pub(crate) ident: String,
    pub(crate) typename: TypeName,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Statement {
    Func {
        ident: String,
        declarations: Vec<Param>,
        rtype: TypeName,
        body: Box<Statement>,
    },

    Assignment {
        ident: String,
        value: Expression
    },

    VarDecl {
        ident: String,
        typename: TypeName,
    },

    While {
        condition: Expression,
        block: Box<Statement>,
    },

    If {
        condition: Expression,
        true_branch: Box<Statement>,
        false_branch: Option<Box<Statement>>,
    },

    Block {
        statements: Vec<Statement>
    },

    Return {
        expression: Expression
    },

    Expr(Expression),
    Empty,
}
