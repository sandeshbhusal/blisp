use super::parser::Expression;
use crate::parser::Value;

pub fn eval(input: &Expression) -> Value {
    match input {
        Expression::Block(exprs) => {
            let mut rval = Value::Nil;
            for expr  in  exprs {
                rval = eval(expr);
            }

            return rval;
        },

        Expression::If {
            check,
            true_branch,
            false_branch,
        } => {
            // Evaluate the check.
            let checkout = eval(check);
            return if matches!(checkout, Value::Boolean(true)) {
                eval(true_branch)
            } else {
                eval(false_branch)
            };
        },

        Expression::While { check, block } => {
            todo!()
        },

        Expression::Binary { op, operands } => todo!(),
        Expression::FuncDecl {
            ident,
            declarations,
            body,
            rtype,
        } => todo!(),

        Expression::FuncCall { ident, args } => todo!(),

        Expression::Boolean(bv) => return Value::Boolean(*bv),
        Expression::Integer(iv) => return Value::Integer(*iv),
        Expression::Float(fv) => return Value::Float(*fv),
    }
}
