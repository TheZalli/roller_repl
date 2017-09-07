use std::collections::BTreeMap;

use ast::{Value, IdType};

#[derive(Debug)]
pub struct RollerNamespace {
    variables: BTreeMap<IdType, Value>,
}

impl RollerNamespace {
    pub fn new() -> Self {
        RollerNamespace {
            variables: BTreeMap::new(),
        }
    }
}
