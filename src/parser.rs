use std::{collections::VecDeque, iter::Peekable};

use crate::lexer::TokenType;

use super::lexer::Token;
use anyhow::{Context, Result};

#[derive(Debug, Clone, Eq, PartialEq)]
enum Operator {
    Plus,
    Minus,
    Times,
    Divide,

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
pub struct Decl {
    pub ident: String,
    pub typename: TypeName,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Value {
    Integer(i32),
    Float(f32),
    Function,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expression {
    Block(Vec<Expression>),
    If {
        check: Box<Expression>,
        true_branch: Box<Expression>,
        false_branch: Box<Expression>,
    },
    While {
        check: Box<Expression>,
        block: Box<Expression>,
    },
    Binary {
        op: Operator,
        operands: Vec<Expression>,
    },
    FuncDecl {
        ident: String,
        declarations: Vec<Decl>,
        body: Box<Expression>,
        rtype: TypeName,
    },
    FuncCall {
        ident: String,
        args: Vec<Expression>,
    },
    Boolean(bool),
    Integer(i32),
    Float(f32),
}

struct Parser {
    token_stream: VecDeque<Token>,
    program: Vec<Expression>,
}

impl Parser {
    fn new<T: Iterator<Item = Token>>(stream: T) -> Self {
        return Self {
            token_stream: stream.into_iter().collect::<VecDeque<_>>(),
            program: vec![],
        };
    }

    fn expect(&mut self, token_type: TokenType) -> Result<Token> {
        if let Some(tok) = self.token_stream.pop_front() {
            if tok.r#type == token_type {
                return Ok(tok);
            } else {
                anyhow::bail!(
                    "Found token of type {:?} when {:?} was expected",
                    tok.r#type,
                    token_type
                );
            }
        } else {
            anyhow::bail!("Nothing left to parse when {:?} was expected", token_type);
        }
    }

    pub fn parse(&mut self) -> Result<Expression> {
        if let Some(token) = self.token_stream.front() {
            let token_type = token.r#type;
            match token_type {
                TokenType::Integer => return self.parse_integer(),
                TokenType::Float => return self.parse_float(),
                TokenType::Lparen => return self.parse_block(),
                TokenType::KwIf => return self.parse_if_statement(),
                TokenType::KwWhile => return self.parse_while_statement(),
                TokenType::BooleanTrue => return self.parse_bool_true(),
                TokenType::BooleanFalse => return self.parse_bool_false(),
                TokenType::Plus
                | TokenType::Eq
                | TokenType::Ge
                | TokenType::Gt
                | TokenType::Le
                | TokenType::Lt
                | TokenType::Div
                | TokenType::Mul
                | TokenType::Minus
                | TokenType::Neq => return self.parse_expr(),
                TokenType::Rparen => anyhow::bail!("Unexpected RParen found at {}", token.start),
                TokenType::KwDef => return self.parse_function_decl(),
                TokenType::Identifier => return self.parse_function_call(),

                _ => anyhow::bail!("Unexpected token {:?}", token_type),
            }
        } else {
            return Ok(Expression::Block(vec![]));
        }
    }

    fn parse_bool_true(&mut self) -> Result<Expression> {
        self.expect(TokenType::BooleanTrue)?;
        return Ok(Expression::Boolean(true));
    }

    fn parse_bool_false(&mut self) -> Result<Expression> {
        self.expect(TokenType::BooleanFalse)?;
        return Ok(Expression::Boolean(false));
    }

    fn parse_type(&mut self) -> Result<TypeName> {
        if let Some(token) = self.token_stream.pop_front() {
            match token.r#type {
                TokenType::TypeInt => Ok(TypeName::Int),
                TokenType::TypeFloat => Ok(TypeName::Float),
                TokenType::TypeBool => Ok(TypeName::Bool),
                TokenType::Identifier => Ok(TypeName::UDT(token.content)),
                _ => anyhow::bail!(
                    "Expected type declaration, found {:?} as {:?}",
                    token.r#type,
                    token
                ),
            }
        } else {
            anyhow::bail!("Expected type declaration, found end of input")
        }
    }

    pub fn parse_function_decl(&mut self) -> Result<Expression> {
        self.expect(TokenType::KwDef)?;

        // Get function name
        let name = self
            .token_stream
            .pop_front()
            .context("Expected function name")?
            .content;

        // Parse parameter declarations
        self.expect(TokenType::Lparen)?;
        let mut declarations = Vec::new();

        while let Some(token) = self.token_stream.front() {
            if token.r#type == TokenType::Rparen {
                break;
            }
            declarations.push(self.parse_decl()?);
        }

        self.expect(TokenType::Rparen)?;

        // Parse return type
        let return_type = self.parse_type()?;

        // Parse function body
        let body = Box::new(self.parse()?);

        Ok(Expression::FuncDecl {
            ident: name,
            declarations,
            rtype: return_type,
            body,
        })
    }

    pub fn parse_function_call(&mut self) -> Result<Expression> {
        let name = self
            .token_stream
            .pop_front()
            .context("Expected function name")?
            .content;

        let mut args = Vec::new();

        while let Some(token) = self.token_stream.front() {
            if token.r#type == TokenType::Rparen {
                break;
            }
            args.push(self.parse()?);
        }

        Ok(Expression::FuncCall { ident: name, args })
    }

    pub fn parse_expr(&mut self) -> Result<Expression> {
        let op = self.token_stream.pop_front().unwrap();
        let mut operands = vec![];

        while let Some(token) = self.token_stream.front() {
            if token.r#type == TokenType::Rparen {
                break;
            }
            operands.push(self.parse()?);
        }

        let oper = match op.r#type {
            TokenType::Plus => Operator::Plus,
            TokenType::Minus => Operator::Minus,
            TokenType::Mul => Operator::Times,
            TokenType::Div => Operator::Divide,
            TokenType::Lt => Operator::Lt,
            TokenType::Gt => Operator::Gt,
            TokenType::Le => Operator::Le,
            TokenType::Ge => Operator::Ge,
            TokenType::Eq => Operator::Eq,
            TokenType::Neq => Operator::Ne,
            TokenType::Not => Operator::Not,
            _ => anyhow::bail!("Unknown operator {:?} at {:?}", op, op.start),
        };

        Ok(Expression::Binary { op: oper, operands })
    }

    pub fn parse_decl(&mut self) -> Result<Decl> {
        self.expect(TokenType::KwVar)?;

        // Get the variable name
        let ident = self
            .expect(TokenType::Identifier)
            .context("Error parsing decl identifier")?;

        // Get the type
        let typename = self.parse_type().context("Error parsing decl type")?;

        Ok(Decl {
            ident: ident.content,
            typename,
        })
    }

    pub fn parse_while_statement(&mut self) -> Result<Expression> {
        self.expect(TokenType::KwWhile)?;

        let condblock = Box::new(
            self.parse()
                .context("Missing condition for while statement")?,
        );
        let bodyblock = Box::new(self.parse().context("Missing body for while statement")?);

        Ok(Expression::While {
            check: condblock,
            block: bodyblock,
        })
    }

    pub fn parse_if_statement(&mut self) -> Result<Expression> {
        self.expect(TokenType::KwIf)?;

        let condblock = self.parse().context("Missing condition for if statement")?;
        let then_block = self
            .parse()
            .context("Missing 'then' block for if statement")?;
        let else_block = self
            .parse()
            .context("Missing 'else' block for if statement")?;

        Ok(Expression::If {
            check: Box::new(condblock),
            true_branch: Box::new(then_block),
            false_branch: Box::new(else_block),
        })
    }

    pub fn parse_block(&mut self) -> Result<Expression> {
        self.expect(TokenType::Lparen)?;

        let mut block = vec![];
        while let Some(tok) = self.token_stream.front() {
            if tok.r#type == TokenType::Rparen {
                self.expect(TokenType::Rparen)?;
                break;
            }
            block.push(self.parse()?);
        }

        Ok(Expression::Block(block))
    }

    pub fn parse_integer(&mut self) -> Result<Expression> {
        let token = self.expect(TokenType::Integer)?;
        Ok(Expression::Integer(token.content.parse::<i32>()?))
    }

    pub fn parse_float(&mut self) -> Result<Expression> {
        let token = self.expect(TokenType::Float)?;
        Ok(Expression::Float(token.content.parse::<f32>()?))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer;

    use super::*;

    #[test]
    fn test_parse_integer() {
        let mut parser = Parser::new(
            vec![Token {
                r#type: TokenType::Integer,
                start: 0,
                end: 3,
                content: "123".to_string(),
            }]
            .into_iter(),
        );
        let expr = parser.parse().unwrap();
        assert_eq!(expr, Expression::Integer(123));
    }

    #[test]
    fn test_parse_float() {
        let mut parser = Parser::new(
            vec![Token {
                r#type: TokenType::Float,
                start: 0,
                end: 6,
                content: "123.456".to_string(),
            }]
            .into_iter(),
        );
        let expr = parser.parse().unwrap();
        assert_eq!(expr, Expression::Float(123.456));
    }

    #[test]
    fn test_parse_block() {
        let mut parser = Parser::new(
            vec![
                Token {
                    r#type: TokenType::Lparen,
                    start: 0,
                    end: 1,
                    content: "(".to_string(),
                },
                Token {
                    r#type: TokenType::Integer,
                    start: 0,
                    end: 3,
                    content: "123".to_string(),
                },
                Token {
                    r#type: TokenType::Integer,
                    start: 4,
                    end: 7,
                    content: "456".to_string(),
                },
                Token {
                    r#type: TokenType::Rparen,
                    start: 8,
                    end: 9,
                    content: ")".to_string(),
                },
            ]
            .into_iter(),
        );
        let expr = parser.parse().unwrap();
        assert_eq!(
            expr,
            Expression::Block(vec![Expression::Integer(123), Expression::Integer(456)])
        );
    }

    #[test]
    fn print_program_ast() {
        let fiboprogram = r#"
            (
                (def fibo (var x Int)
                    Int
                    (if (== x 0)
                        0
                        (if (== x 1)
                            1
                            (+ (fibo (- x 1)) (fibo (- x 2)))
                        )
                    )
                )
                (fibo 10)
            "#;

        let lexer = lexer(fiboprogram).unwrap();
        let mut parser = Parser::new(lexer.into_iter());
        let ast = parser.parse().unwrap();
        println!("{:?}", ast);
    }
}
