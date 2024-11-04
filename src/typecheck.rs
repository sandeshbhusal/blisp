use crate::parser::{Decl, Expression, TypeName};
use std::collections::HashMap;

struct TypeChecker {
    ast: Expression,
    functions: HashMap<String, TypeName>,
    function_params: HashMap<String, Vec<TypeName>>,
}

impl TypeChecker {
    /// Typechecks an expression and returns its type.
    pub fn typecheck(&mut self, expr: &Expression) -> Result<TypeName, String> {
        match expr {
            Expression::Block(exprs) => {
                // Return the type of the last expression in the block, or Unit if empty.
                exprs
                    .iter()
                    .last()
                    .map(|last_expr| self.typecheck(last_expr))
                    .unwrap_or(Ok(TypeName::Unit))
            }

            Expression::If {
                check,
                true_branch,
                false_branch,
            } => {
                // Ensure the condition is a boolean and both branches have matching types.
                if self.typecheck(check)? != TypeName::Bool {
                    return Err("Condition in 'if' expression must be a boolean.".into());
                }
                let true_type = self.typecheck(true_branch)?;
                let false_type = self.typecheck(false_branch)?;
                if true_type == false_type {
                    Ok(true_type)
                } else {
                    Err("Mismatched types in 'if' branches.".into())
                }
            }

            Expression::While { check, block } => {
                // Ensure the condition is a boolean; return the type of the block.
                if self.typecheck(check)? != TypeName::Bool {
                    return Err("Condition in 'while' expression must be a boolean.".into());
                }
                self.typecheck(block)
            }

            Expression::Binary { op: _op, operands } => {
                // Ensure all operands have the same type.
                if operands.is_empty() {
                    return Ok(TypeName::Unit);
                }
                let first_type = self.typecheck(&operands[0])?;
                for operand in operands.iter() {
                    if self.typecheck(operand)? != first_type {
                        return Err("Mismatched operand types in binary expression.".into());
                    }
                }
                Ok(first_type)
            }

            Expression::FuncDecl {
                ident,
                declarations,
                body: _,
                rtype,
            } => {
                // Register function type and parameters.
                self.functions.insert(ident.clone(), rtype.clone());
                let param_types: Vec<_> = declarations
                    .iter()
                    .map(|decl| self.typecheck_decl(decl))
                    .collect();
                self.function_params.insert(ident.clone(), param_types);
                Ok(rtype.clone())
            }

            Expression::FuncCall { ident, args } => {
                // Check for function existence and match argument types to parameters.
                let param_types = self
                    .function_params
                    .get(ident)
                    .ok_or_else(|| format!("Function '{}' not found.", ident))?;
                if param_types.len() != args.len() {
                    return Err(format!(
                        "Function '{}' called with incorrect number of arguments.",
                        ident
                    ));
                }
                for (param_type, arg) in param_types.clone().iter().zip(args) {
                    if self.typecheck(arg)? != *param_type {
                        return Err(format!("Argument type mismatch in call to '{}'.", ident));
                    }
                }

                // Return the function's declared return type.
                self.functions
                    .get(ident)
                    .cloned()
                    .ok_or_else(|| format!("Return type for '{}' not found.", ident))
            }

            Expression::Boolean(_) => Ok(TypeName::Bool),
            Expression::Integer(_) => Ok(TypeName::Int),
            Expression::Float(_) => Ok(TypeName::Float),
        }
    }

    pub fn typecheck_decl(&self, decl: &Decl) -> TypeName {
        decl.typename.clone()
    }
}
