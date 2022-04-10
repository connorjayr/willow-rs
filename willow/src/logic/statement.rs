use crate::{logic::Term, parser};
use nom::Finish;
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
    str::FromStr,
};

/// A logical statement in [first-order logic](https://en.wikipedia.org/wiki/First-order_logic).
#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Atom {
        predicate: String,
        args: Vec<Term>,
    },
    Biconditional(Box<Statement>, Box<Statement>),
    Conditional(Box<Statement>, Box<Statement>),
    Conjunction(Vec<Statement>),
    Contradiction,
    Disjunction(Vec<Statement>),
    Existential {
        var: String,
        formula: Box<Statement>,
    },
    Negation(Box<Statement>),
    Tautology,
    Universal {
        var: String,
        formula: Box<Statement>,
    },
}
use Statement::*;

impl Display for Statement {
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
            Existential { var, formula } => {
                write!(f, "(∃{} {})", var, formula)
            }
            Negation(operand) => write!(f, "¬{}", operand),
            Tautology => write!(f, "⊤"),
            Universal { var, formula } => {
                write!(f, "(∀{} {})", var, formula)
            }
        }
    }
}

impl FromStr for Statement {
    type Err = nom::error::Error<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parser::iff_expr(s).finish() {
            Err(err) => Err(Self::Err::new(err.input.to_string(), err.code)),
            Ok((leftover_text, statement)) => match leftover_text.len() {
                0 => Ok(*statement),
                _ => Err(Self::Err::new(
                    leftover_text.to_string(),
                    nom::error::ErrorKind::Fail,
                )),
            },
        }
    }
}

impl Statement {
    /// Returns a set of all constants that are in this statement.
    ///
    /// A constant is an expression that represents an element in the universe of discourse.
    /// `bound_vars` contains all variables that are bound to elements in the universe of discourse
    /// through the use of logical quantifiers.
    ///
    /// # Examples
    ///
    /// Functions whose arguments are constants are also constants:
    /// ```
    /// use willow::logic::{Statement, Term};
    ///
    /// let statement: Statement = "IsSquare(x) implies (Equals(width(x),height(x)) and not IsCircle(x))"
    ///     .parse()
    ///     .unwrap();
    ///
    /// let constants = [
    ///     Term::var("x"),
    ///     Term::new("width", vec![Term::var("x")]),
    ///     Term::new("height", vec![Term::var("x")]),
    /// ];
    ///
    /// assert_eq!(statement.get_constants(&[]), constants.iter().collect());
    /// ```
    ///
    /// Variables that are bound by logical quantifiers are not constants:
    /// ```
    /// use willow::logic::Statement;
    ///
    /// let statement: Statement = "forall x not exists y <(x, y)"
    ///     .parse()
    ///     .unwrap();
    ///
    /// assert!(statement.get_constants(&[]).is_empty());
    /// ```
    pub fn get_constants(&self, bound_vars: &[&str]) -> HashSet<&Term> {
        match self {
            Atom { args, .. } => args
                .iter()
                .flat_map(|arg| arg.get_constants(bound_vars))
                .collect(),
            Biconditional(left, right) | Conditional(left, right) => [left, right]
                .into_iter()
                .flat_map(|arg| arg.get_constants(bound_vars))
                .collect(),
            Contradiction | Tautology => HashSet::new(),
            Conjunction(operands) | Disjunction(operands) => operands
                .iter()
                .flat_map(|arg| arg.get_constants(bound_vars))
                .collect(),
            Existential { var, formula } | Universal { var, formula } => {
                let bound_vars = [bound_vars, &[var]].concat();
                formula.get_constants(&bound_vars)
            }
            Negation(operand) => operand.get_constants(bound_vars),
        }
    }

    /// Returns true if this statement is a literal.
    ///
    /// # Examples
    ///
    /// An atom is a literal:
    /// ```
    /// use willow::logic::Statement::*;
    ///
    /// let p = Atom { predicate: "P".to_string(), args: Vec::new() };
    /// assert!(p.is_literal());
    /// ```
    ///
    /// A negation of an atom is also a literal:
    /// ```
    /// use willow::logic::Statement::*;
    ///
    /// let p = Atom { predicate: "P".to_string(), args: Vec::new() };
    /// let not_p = Negation(Box::new(p));
    /// assert!(not_p.is_literal());
    /// ```
    ///
    /// Any other statements are not literals:
    /// ```
    /// use willow::logic::Statement::*;
    ///
    /// let p = Atom { predicate: "P".to_string(), args: Vec::new() };
    /// let q = Atom { predicate: "Q".to_string(), args: Vec::new() };
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
