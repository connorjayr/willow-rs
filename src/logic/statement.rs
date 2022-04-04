use crate::logic::Term;
use std::fmt::{self, Display, Formatter};

/// A logical statement in [first-order logic](https://en.wikipedia.org/wiki/First-order_logic).
#[derive(Debug)]
pub enum Statement<'a> {
    Atom {
        predicate: &'a str,
        args: Vec<Term<'a>>,
    },
    Biconditional(Box<Statement<'a>>, Box<Statement<'a>>),
    Conditional(Box<Statement<'a>>, Box<Statement<'a>>),
    Conjunction(Vec<Statement<'a>>),
    Contradiction,
    Disjunction(Vec<Statement<'a>>),
    Existential {
        vars: Vec<&'a str>,
        formula: Box<Statement<'a>>,
    },
    Negation(Box<Statement<'a>>),
    Tautology,
    Universal {
        vars: Vec<&'a str>,
        formula: Box<Statement<'a>>,
    },
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
            Statement::Biconditional(left, right) => write!(f, "({} ⟷ {})", left, right),
            Statement::Conditional(left, right) => write!(f, "({} → {})", left, right),
            Statement::Conjunction(operands) => write!(
                f,
                "({})",
                operands
                    .iter()
                    .map(|operand| format!("{}", operand))
                    .collect::<Vec<String>>()
                    .join(" ∧ ")
            ),
            Statement::Contradiction => write!(f, "⊥"),
            Statement::Disjunction(operands) => write!(
                f,
                "({})",
                operands
                    .iter()
                    .map(|operand| format!("{}", operand))
                    .collect::<Vec<String>>()
                    .join(" ∨ ")
            ),
            Statement::Existential { vars, formula } => {
                let vars = vars
                    .iter()
                    .map(|var| var.to_string())
                    .collect::<Vec<String>>()
                    .join(",");

                write!(f, "(∃{} {})", vars, formula)
            }
            Statement::Negation(operand) => write!(f, "¬{}", operand),
            Statement::Tautology => write!(f, "⊤"),
            Statement::Universal { vars, formula } => {
                let vars = vars
                    .iter()
                    .map(|var| var.to_string())
                    .collect::<Vec<String>>()
                    .join(",");

                write!(f, "(∀{} {})", vars, formula)
            }
        }
    }
}
