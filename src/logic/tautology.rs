use crate::logic::Statement;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, Debug, Default)]
pub struct Tautology;

impl Tautology {
    pub fn new() -> Self {
        Self
    }
}

impl Display for Tautology {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "⊤")
    }
}

impl Statement for Tautology {}
