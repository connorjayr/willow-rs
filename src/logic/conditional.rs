use crate::logic::Statement;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct ConditionalStatement {
    lhs: Box<dyn Statement>,
    rhs: Box<dyn Statement>,
}

impl ConditionalStatement {
    pub fn new(lhs: Box<dyn Statement>, rhs: Box<dyn Statement>) -> Self {
        Self { lhs, rhs }
    }
}

impl Display for ConditionalStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} → {}", self.lhs, self.rhs)
    }
}

impl Statement for ConditionalStatement {}
