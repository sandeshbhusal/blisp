use super::parser::Expression;
use crate::parser::Value;

pub fn eval(input: Expression) -> Value {
    match input {
        Expression::Block(exprs) => {
            let mut rval = Value::Integer(0);
            for expr in exprs {
                rval = eval(expr);
            }
            return rval;
        }

        Expression::If {
            check,
            true_branch,
            false_branch,
        } => todo!(),
        Expression::While { check, block } => todo!(),
        Expression::Binary { op, operands } => todo!(),
        Expression::FuncDecl {
            ident,
            declarations,
            body,
            rtype,
        } => todo!(),
        Expression::FuncCall { ident, args } => todo!(),
        Expression::Integer(_) => todo!(),
        Expression::Float(_) => todo!(),
    }
}
