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

// For implementations, we can use the shorthand
use Statement::*;

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Atom { predicate, args } => match args.len() {
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
            Biconditional(left, right) => write!(f, "({} ⟷ {})", left, right),
            Conditional(left, right) => write!(f, "({} → {})", left, right),
            Conjunction(operands) => write!(
                f,
                "({})",
                operands
                    .iter()
                    .map(|operand| format!("{}", operand))
                    .collect::<Vec<String>>()
                    .join(" ∧ ")
            ),
            Contradiction => write!(f, "⊥"),
            Disjunction(operands) => write!(
                f,
                "({})",
                operands
                    .iter()
                    .map(|operand| format!("{}", operand))
                    .collect::<Vec<String>>()
                    .join(" ∨ ")
            ),
            Existential { vars, formula } => {
                let vars = vars
                    .iter()
                    .map(|var| var.to_string())
                    .collect::<Vec<String>>()
                    .join(",");

                write!(f, "(∃{} {})", vars, formula)
            }
            Negation(operand) => write!(f, "¬{}", operand),
            Tautology => write!(f, "⊤"),
            Universal { vars, formula } => {
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

impl Statement<'_> {
    /// Returns true if this statement is a literal.
    ///
    /// # Examples
    ///
    /// An atom is a literal:
    /// ```
    /// use willow::logic::Statement::*;
    ///
    /// let p = Atom { predicate: "P", args: Vec::new() };
    /// assert!(p.is_literal());
    /// ```
    ///
    /// A negation of an atom is also a literal:
    /// ```
    /// use willow::logic::Statement::*;
    ///
    /// let p = Atom { predicate: "P", args: Vec::new() };
    /// let not_p = Negation(Box::new(p));
    /// assert!(not_p.is_literal());
    /// ```
    ///
    /// Any other statements are not literals:
    /// ```
    /// use willow::logic::Statement::*;
    ///
    /// let p = Atom { predicate: "P", args: Vec::new() };
    /// let q = Atom { predicate: "Q", args: Vec::new() };
    /// let p_and_q = Conjunction(vec![p, q]);
    /// assert!(!p_and_q.is_literal());
    /// ```
    pub fn is_literal(&self) -> bool {
        match self {
            Atom { .. } => true,
            Negation(operand) => matches!(**operand, Atom { .. }),
            _ => false,
        }
    }
}
