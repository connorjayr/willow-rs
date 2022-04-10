use crate::logic::Term;
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
};

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
        var: &'a str,
        formula: Box<Statement<'a>>,
    },
    Negation(Box<Statement<'a>>),
    Tautology,
    Universal {
        var: &'a str,
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

impl Statement<'_> {
    /// Gets the set of constants used in this Statement.
    ///
    /// Gathers the set of all constants within the Statement. If any variables from the vars
    /// argument appear within a Statement, it is not considered a constant since its value may
    /// still vary.
    ///
    /// # Examples
    ///
    /// ```
    /// use willow::logic::{Statement::*, Term};
    /// use std::collections::HashSet;
    ///
    /// let basic_statement = Conditional(
    ///     Box::new(Atom {
    ///         predicate: "IsSquare",
    ///         args: vec![Term::var("x")],
    ///     }),
    ///     Box::new(Conjunction(vec![
    ///         Atom {
    ///             predicate: "Equals",
    ///             args: vec![
    ///                 Term::new("width", vec![Term::var("x")]),
    ///                 Term::new("height", vec![Term::var("x")]),
    ///             ],
    ///         },
    ///         Negation(Box::new(Atom {
    ///             predicate: "IsCircle",
    ///             args: vec![Term::var("x")],
    ///         })),
    ///     ])),
    /// );
    ///
    /// let constant_array = [
    ///     Term::var("x"),
    ///     Term::new("width", vec![Term::var("x")]),
    ///     Term::new("height", vec![Term::var("x")]),
    /// ];
    /// let mut constants = HashSet::new();
    /// constants.extend(constant_array.iter());
    ///
    /// assert_eq!(basic_statement.get_constants(&[]), constants);
    /// ```
    ///
    /// ```
    /// use willow::logic::{Statement::*, Term};
    /// use std::collections::HashSet;
    ///
    /// let complex_statement = Universal {
    ///     var: "x",
    ///     formula: Box::new(Negation(Box::new(Existential {
    ///         var: "y",
    ///         formula: Box::new(Atom {
    ///             predicate: "<",
    ///             args: vec![Term::var("x"), Term::var("y")],
    ///         }),
    ///     }))),
    /// };
    ///
    ///
    /// let constants = HashSet::new();
    ///
    /// assert_eq!(complex_statement.get_constants(&[]), constants);
    /// ```
    pub fn get_constants(&self, vars: &[&str]) -> HashSet<&Term> {
        let mut constants = HashSet::new();

        match self {
            Atom { predicate: _, args } => {
                constants.extend(args.iter().flat_map(|arg| arg.get_constants(vars)))
            }
            Biconditional(left, right) => {
                constants.extend([left, right].iter().flat_map(|arg| arg.get_constants(vars)))
            }
            Conditional(left, right) => {
                constants.extend([left, right].iter().flat_map(|arg| arg.get_constants(vars)))
            }
            Contradiction => (),
            Conjunction(operands) => {
                constants.extend(operands.iter().flat_map(|arg| arg.get_constants(vars)))
            }
            Disjunction(operands) => {
                constants.extend(operands.iter().flat_map(|arg| arg.get_constants(vars)))
            }
            Existential { var, formula } => {
                let extended_vars = [vars, &[var]].concat();
                constants.extend(formula.get_constants(extended_vars.as_slice()));
            }
            Negation(operand) => constants.extend(operand.get_constants(vars)),
            Tautology => (),
            Universal { var, formula } => {
                let extended_vars = [vars, &[var]].concat();
                constants.extend(formula.get_constants(extended_vars.as_slice()));
            }
        }

        constants
    }

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
