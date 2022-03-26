use crate::logic::Statement;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct NotStatement {
    operand: Box<dyn Statement>,
}

impl NotStatement {
    pub fn new(operand: Box<dyn Statement>) -> Self {
        Self { operand }
    }
}

impl Display for NotStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "¬{}", self.operand)
    }
}

impl Statement for NotStatement {}
