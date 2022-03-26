use crate::logic::Statement;
use std::fmt::{self, Display, Formatter};

/// A logical statement that combines two or more other statements with the
/// conjunction logical connective.
///
/// # Examples
///
/// ```
/// # use willow::logic::{AndStatement, Formula, Statement};
/// let operands: Vec<Box<dyn Statement>> = vec![
///     Box::new(Formula::with_name("A".to_string())),
///     Box::new(Formula::with_name("B".to_string())),
/// ];
/// let statement = AndStatement::new(operands);
/// assert_eq!(format!("{}", statement), "(A ∧ B)".to_string());
/// ```
#[derive(Debug)]
pub struct AndStatement {
    operands: Vec<Box<dyn Statement>>,
}

impl AndStatement {
    pub fn new(operands: Vec<Box<dyn Statement>>) -> Self {
        Self { operands }
    }
}

impl Display for AndStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({})",
            self.operands
                .iter()
                .map(|operand| format!("{}", operand))
                .collect::<Vec<String>>()
                .join(" ∧ ")
        )
    }
}

impl Statement for AndStatement {}
