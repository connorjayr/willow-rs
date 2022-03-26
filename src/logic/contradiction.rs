use crate::logic::Statement;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, Debug, Default)]
pub struct Contradiction;

impl Contradiction {
    pub fn new() -> Self {
        Self
    }
}

impl Display for Contradiction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "⊥")
    }
}

impl Statement for Contradiction {}
