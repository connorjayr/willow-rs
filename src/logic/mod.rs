use std::fmt::{Debug, Display};

mod and;
mod biconditional;
mod conditional;
mod contradiction;
mod formula;
mod not;
mod or;
mod tautology;

pub use and::AndStatement;
pub use biconditional::BiconditionalStatement;
pub use conditional::ConditionalStatement;
pub use contradiction::Contradiction;
pub use formula::Formula;
pub use not::NotStatement;
pub use or::OrStatement;
pub use tautology::Tautology;

pub trait Statement: Debug + Display {}
