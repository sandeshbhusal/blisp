use std::collections::HashMap;

use crate::ast::{Expression, Statement, TypeName};

#[derive(Default, Debug)]
struct Scope {
    globals: HashMap<String, TypeName>, // Store the globals' types.
    locals : HashMap<String, TypeName>, // Store the locals' types.
    func_rtypes: HashMap<String, TypeName>, // Store functions' return types.
    func_params: HashMap<String, Vec<TypeName>>, // Store functions' params types.
}

#[derive(Debug, Default)]
struct TypeChecker {
    scope: Vec<Scope>
}

impl TypeChecker {
    fn new() -> Self {
        return Default::default()
    }

    fn expr(&self, expression: &Expression) -> TypeName {
        todo!()
    }

    fn stmt(&self, statement: &Statement) -> TypeName {
        todo!()
    }
}
