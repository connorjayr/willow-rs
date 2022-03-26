use crate::logic::Statement;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct Formula {
    name: String,
    args: Vec<Box<dyn Statement>>,
}

impl Formula {
    pub fn new(name: String, args: Vec<Box<dyn Statement>>) -> Self {
        Self { name, args }
    }

    pub fn with_name(name: String) -> Self {
        Self::new(name, Vec::new())
    }

    pub fn is_predicate(&self) -> bool {
        match self.name.chars().next() {
            Some(c) => c.is_uppercase(),
            None => false,
        }
    }
}

impl Display for Formula {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.args.len() {
            0 => write!(f, "{}", self.name),
            _ => {
                let args = self
                    .args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{}({})", self.name, args)
            }
        }
    }
}

impl Statement for Formula {}
