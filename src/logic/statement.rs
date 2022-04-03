use crate::logic::Object;
use std::fmt::Display;

#[derive(Debug)]
pub enum Statement {
    Atom {
        predicate: String,
        args: Vec<Object>,
    },
    Biconditional {
        lhs: Box<Statement>,
        rhs: Box<Statement>,
    },
    Conditional {
        lhs: Box<Statement>,
        rhs: Box<Statement>,
    },
    Conjunction {
        operands: Vec<Box<Statement>>,
    },
    Contradiction,
    Disjunction {
        operands: Vec<Box<Statement>>,
    },
    Existential {
        variables: Vec<Object>,
        proposition: Box<Statement>,
    },
    Negation {
        operand: Box<Statement>,
    },
    Tautology,
    Universal {
        variables: Vec<Object>,
        proposition: Box<Statement>,
    },
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Atom { predicate, args } => match args.len() {
                0 => write!(f, "{}", predicate),
                _ => {
                    let args = args
                        .iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<String>>()
                        .join(",");
                    write!(f, "{}({})", predicate, args)
                }
            },
            Statement::And { operands } => write!(
                f,
                "({})",
                operands
                    .iter()
                    .map(|operand| format!("{}", operand))
                    .collect::<Vec<String>>()
                    .join(" ∧ ")
            ),
            Statement::Biconditional { lhs, rhs } => write!(f, "({} ⟷ {})", lhs, rhs),
            Statement::Conditional { lhs, rhs } => write!(f, "({} → {})", lhs, rhs),
            Statement::Contradiction => write!(f, "⊥"),
            Statement::Existential {
                variables,
                proposition,
            } => {
                let variables = variables
                    .iter()
                    .map(|variable| format!("{}", variable))
                    .collect::<Vec<String>>()
                    .join(",");

                write!(f, "(∃{} {})", variables, proposition)
            }
            Statement::Negation { operand } => write!(f, "¬{}", operand),
            Statement::Or { operands } => write!(
                f,
                "({})",
                operands
                    .iter()
                    .map(|operand| format!("{}", operand))
                    .collect::<Vec<String>>()
                    .join(" ∨ ")
            ),
            Statement::Tautology => write!(f, "⊤"),
            Statement::Universal {
                variables,
                proposition,
            } => {
                let variables = variables
                    .iter()
                    .map(|variable| format!("{}", variable))
                    .collect::<Vec<String>>()
                    .join(",");

                write!(f, "(∀{} {})", variables, proposition)
            }
        }
    }
}
