use std::collections::VecDeque;
use std::env::consts::ARCH;

use crate::ast::Operator;
pub(crate) use crate::ast::TypeName;
pub(crate) use crate::ast::{Expression, Param, Statement};
use crate::lexer::TokenType;
use anyhow::Context;
pub(crate) use anyhow::Result;

use super::lexer::Token;

enum ParserError {
    UnexpectedToken,
}

#[derive(Default, Clone)]
pub(crate) struct Parser {
    token_stream: VecDeque<Token>,
    program: Vec<Statement>,
}

fn get_operator_precedence(operator: TokenType) -> (i32, i32) {
    match operator {
        TokenType::Plus => (1, 0),
        TokenType::Minus => (1, 0),
        TokenType::Mul => (2, 0),
        TokenType::Div => (2, 0),
        TokenType::Assign => (3, 0),
        TokenType::Lt => (1, 0),
        TokenType::Gt => (1, 0),
        TokenType::Le => (1, 0),
        TokenType::Ge => (1, 0),
        TokenType::Eq => (1, 0),
        TokenType::Neq => (1, 0),
        TokenType::Not => (1, 0),

        _ => panic!("Unknown operator! {:?}", operator),
    }
}

impl TryFrom<TokenType> for Operator {
    type Error = anyhow::Error;

    fn try_from(value: TokenType) -> Result<Self> {
        let corresp = match value {
            TokenType::Plus => Operator::Plus,
            TokenType::Minus => Operator::Minus,
            TokenType::Mul => Operator::Times,
            TokenType::Div => Operator::Divide,
            TokenType::Assign => Operator::Assign,
            TokenType::Lt => Operator::Lt,
            TokenType::Gt => Operator::Gt,
            TokenType::Le => Operator::Le,
            TokenType::Ge => Operator::Ge,
            TokenType::Eq => Operator::Eq,
            TokenType::Neq => Operator::Ne,
            TokenType::Not => Operator::Not,

            _ => panic!("Unknown operator! {:?}", value),
        };

        Ok(corresp)
    }
}

impl Parser {
    pub fn new<T: Iterator<Item = Token>>(iter: T) -> Self {
        Self {
            token_stream: iter.collect::<VecDeque<Token>>(),
            program: vec![],
        }
    }

    fn expect(&mut self, expected: TokenType) -> Result<Token> {
        if let Some(top) = self.token_stream.front() {
            if top.r#type != expected {
                anyhow::bail!(
                    "Expected token of type {:?} but got {:?} instead.",
                    expected,
                    top.r#type
                );
            } else {
                // SAFETY: Safe to unwrap - we check for existence before.
                return Ok(self.token_stream.pop_front().unwrap());
            }
        } else {
            anyhow::bail!(
                "Expected token {:?} but nothing remains to be parsed.",
                expected
            );
        }
    }

    fn peek_top(&mut self) -> Option<Token> {
        self.token_stream.front().map(|t| t.clone())
    }

    fn advance(&mut self) {
        self.token_stream.pop_front();
    }

    fn parse_program(&mut self) -> Result<Statement> {
        todo!()
    }

    fn parse_func_call(&mut self) -> Result<Expression> {
        let funcname = self
            .expect(TokenType::Identifier)
            .context("Error parsing function name in function call")?;
        let params = self
            .parse_arg_list()
            .context("Error parsing params in function call")?;
        return Ok(Expression::FuncCall {
            ident: funcname.content,
            args: params,
        });
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Expression>> {
        self.expect(TokenType::Lparen)
            .context("Expected '(' at start of arg list")?;
        let mut exprlist = vec![];
        while let Some(tok) = self.peek_top() {
            if tok.r#type == TokenType::Rparen {
                break;
            }

            let expr = self.parse_expr().context("Error parsing expression")?;
            exprlist.push(expr);

            if self.expect(TokenType::Comma).is_err() {
                self.expect(TokenType::Rparen)
                    .context("Expected ')' at end - arg list not closed.")?;
                return Ok(exprlist);
            }
        }

        return Ok(exprlist);
    }

    fn parse_func_decl(&mut self) -> Result<Statement> {
        self.expect(TokenType::KwDef)
            .context("Expected 'def' to define a function.")?;

        let identifier = self
            .expect(TokenType::Identifier)
            .context("Error parsing function name")?;
        let paramlist = self
            .parse_param_list()
            .context("Error parsing param list")?;
        let rtype = self
            .parse_type()
            .context("Error parsing function return type")?;
        let body = Box::new(self.parse_block().context("Error parsing function body")?);

        return Ok(Statement::Func {
            ident: identifier.content,
            declarations: paramlist,
            rtype,
            body,
        });
    }

    fn parse_type(&mut self) -> Result<TypeName> {
        let rval = Ok(match self.peek_top().map(|tok| tok.r#type) {
            Some(TokenType::TypeInt) => TypeName::Int,
            Some(TokenType::TypeBool) => TypeName::Bool,
            Some(TokenType::TypeFloat) => TypeName::Float,
            other => anyhow::bail!(
                "Expected one of 'int', 'float', or 'bool', found {:?} instead",
                other
            ),
        });

        self.advance();
        rval
    }

    fn parse_param_list(&mut self) -> Result<Vec<Param>> {
        self.expect(TokenType::Lparen)
            .context("Expected param list")?;

        let mut decls = vec![];

        while let Some(top) = self.peek_top() {
            if top.r#type == TokenType::Rparen {
                break;
            }

            let typename = self.parse_type()?;
            let ident = self.expect(TokenType::Identifier)?;

            decls.push(Param {
                ident: ident.content,
                typename,
            });

            if self.expect(TokenType::Comma).is_err() {
                // Early return
                if self.expect(TokenType::Rparen).is_ok() {
                    return Ok(decls);
                }
            }
        }

        self.expect(TokenType::Rparen);
        return Ok(decls);
    }

    fn parse_block(&mut self) -> Result<Statement> {
        self.expect(TokenType::Lbrac)?;
        let mut statements = vec![];
        while let Some(top) = self.peek_top() {
            if top.r#type == TokenType::Rbrac {
                break;
            }
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        self.expect(TokenType::Rbrac)?;
        return Ok(Statement::Block { statements });
    }

    fn parse_while(&mut self) -> Result<Statement> {
        self.expect(TokenType::KwWhile)?;
        let expr = self.parse_expr()?;
        let block = Box::new(self.parse_block()?);

        return Ok(Statement::While {
            condition: expr,
            block,
        });
    }

    fn parse_var_decl(&mut self) -> Result<Statement> {
        self.expect(TokenType::KwVar)?;
        let ident = self.expect(TokenType::Identifier)?;
        let typename = self.parse_type()?;

        return Ok(Statement::VarDecl {
            ident: ident.content,
            typename,
        });
    }

    fn parse_var_decl_assign(&mut self) -> Result<Statement> {
        self.expect(TokenType::KwVar)?;
        let ident = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Assign)?;
        let expr = self.parse_expr()?;

        return Ok(Statement::Assignment {
            ident: ident.content,
            value: expr,
        });
    }

    fn parse_assignment(&mut self) -> Result<Statement> {
        let ident = self
            .expect(TokenType::Identifier)
            .context("Expected an identifier")?;
        let value = self
            .parse_expr()
            .context("Expected an expression on the RHS of assignment")?;

        return Ok(Statement::Assignment {
            ident: ident.content,
            value,
        });
    }

    fn parse_if(&mut self) -> Result<Statement> {
        self.expect(TokenType::KwIf);
        let expr = self.parse_expr()?;
        let trueblock = Box::new(self.parse_block()?);

        let falseblock = if let Ok(_) = self.expect(TokenType::KwElse) {
            Some(Box::new(self.parse_block()?))
        } else {
            None
        };

        return Ok(Statement::If {
            condition: expr,
            true_branch: trueblock,
            false_branch: falseblock,
        });
    }

    fn parse_int(&mut self) -> Result<Expression> {
        let inttoken = self.expect(TokenType::Integer)?;
        return Ok(Expression::Integer(inttoken.content));
    }

    fn parse_float(&mut self) -> Result<Expression> {
        let floattoken = self.expect(TokenType::TypeFloat)?;
        return Ok(Expression::Float(floattoken.content));
    }

    fn parse_bool(&mut self) -> Result<Expression> {
        let booltoken = self.expect(TokenType::TypeBool)?;
        return Ok(Expression::Boolean(booltoken.content));
    }

    fn parse_prefix(&mut self) -> Result<Expression> {
        let token = self
            .peek_top()
            .context("Expected some prefix, but nothing was found")?;

        return match token.r#type {
            TokenType::Integer => Ok(self.parse_int()?),
            TokenType::Float => Ok(self.parse_float()?),
            TokenType::BooleanTrue => Ok(self.parse_bool()?),
            TokenType::BooleanFalse => Ok(self.parse_bool()?),
            TokenType::Lparen => {
                self.advance(); // Eat the '('
                let expr = self.parse_expr().context("Invalid expression in parantheses")?;
                self.expect(TokenType::Rparen)
                    .context("Improperly closed paranthese in expression")?;
                Ok(expr)
            }
            TokenType::Identifier => {
                // Check if next token is a LParen, else, return an identifier.
                let ident = self.expect(TokenType::Identifier).context(
                    "Internal parser bug: if this breaks it means parser impl is unsound.",
                )?;

                match self.peek_top().map(|tok| tok.r#type) {
                    Some(TokenType::Lparen) => {
                        self.token_stream.push_front(ident);
                        return Ok(self.parse_func_call()?);
                    }
                    _ => {
                        return Ok(Expression::Identifier(ident.content));
                    }
                }
            }
            _ => anyhow::bail!("{:?} is not a valid expression prefix.", token),
        };
    }

    fn pratt_parser(&mut self, min_precedence: i32) -> Result<Expression> {
        let mut left = self.parse_prefix()?;

        while let Some(operator) = self.get_operator() {
            let (prec, _) = get_operator_precedence(operator.r#type);
            if prec < min_precedence {
                // The current operator has lower precedence than the minimum, so we stop parsing
                self.token_stream.push_front(operator);
                break;
            }

            // Call pratt_parser recursively with the new minimum precedence
            let right = self.pratt_parser(prec + 1)?;

            left = Expression::Binary {
                op: operator.r#type.try_into()?,
                operands: vec![left, right],
            };
        }

        Ok(left)
    }

    fn get_operator(&mut self) -> Option<Token> {
        match self.peek_top().map(|tok| tok.r#type) {
            Some(TokenType::Lt)
            | Some(TokenType::Le)
            | Some(TokenType::Gt)
            | Some(TokenType::Ge)
            | Some(TokenType::Eq)
            | Some(TokenType::Plus)
            | Some(TokenType::Minus)
            | Some(TokenType::Mul)
            | Some(TokenType::Div) => {
                return Some(
                    self.token_stream
                        .pop_front()
                        .context("This should not happen")
                        .ok()?,
                );
            }

            Some(other) => None,
            None => None,
        }
    }

    fn parse_expr(&mut self) -> Result<Expression> {
        return Ok(self
            .pratt_parser(-1)
            .context("Failed to parse expression")?);
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let rval = self.parse_if().or(self.parse_while().or(self
            .parse_var_decl()
            .or(self.parse_var_decl_assign().or(self.parse_func_decl()))));

        self.expect(TokenType::Semicolon)
            .context("Statement must be terminated with a semicolon.")?;

        rval
    }
}

#[cfg(test)]
mod parser_tests {
    use crate::{
        ast::Expression,
        lexer::{lexer, Token},
    };

    use super::Parser;

    fn get_lexer(input: &str) -> Vec<Token> {
        lexer(input).unwrap()
    }

    #[test]
    fn test_parse_funcdecl() {
        let stmt = r#"
            def some_function(int x, int y) bool {
                var x int
            }
        "#;

        let mut parser = Parser::new(get_lexer(stmt).into_iter());
        let funcdecl = parser.parse_func_decl().unwrap();
        println!("{:?}", funcdecl);
    }

    #[test]
    fn test_parse_prefix() {
        let stmt = "1 + 2";
        let mut parser = Parser::new(get_lexer(stmt).into_iter());
        let prefix = parser.parse_prefix().unwrap();
        assert!(prefix == Expression::Integer("1".into()));

        let stmt = "x1 + x2";
        let mut parser = Parser::new(get_lexer(stmt).into_iter());
        let prefix = parser.parse_prefix().unwrap();
        assert!(prefix == Expression::Identifier("x1".into()));
    }

    #[test]
    fn test_parse_expression() {
        let expr = "(a + b) / (c + d)";
        let mut parser = Parser::new(get_lexer(expr).into_iter());
        let expr = parser.parse_expr().unwrap();
        dbg!(expr);
    }
}
