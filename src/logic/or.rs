use crate::logic::Statement;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct OrStatement {
    operands: Vec<Box<dyn Statement>>,
}

impl OrStatement {
    pub fn new(operands: Vec<Box<dyn Statement>>) -> Self {
        Self { operands }
    }
}

impl Display for OrStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({})",
            self.operands
                .iter()
                .map(|operand| format!("{}", operand))
                .collect::<Vec<String>>()
                .join(" ∨ ")
        )
    }
}

impl Statement for OrStatement {}
