use crate::logic::term::{parser, Substitution, Term, UnificationError};
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

    pub fn unify_with<'a>(
        &'a self,
        other: &'a Self,
    ) -> Result<Substitution<'a>, UnificationError<'a>> {
        return self.unify_with_helper(other, &mut Vec::new(), Substitution::new());
    }

    fn unify_with_helper<'a>(
        &'a self,
        other: &'a Self,
        vars: &mut Vec<&'a str>,
        mut assignment: Substitution<'a>,
    ) -> Result<Substitution<'a>, UnificationError<'a>> {
        match (self, other) {
            (
                Atom {
                    predicate: predicate1,
                    args: args1,
                },
                Atom {
                    predicate: predicate2,
                    args: args2,
                },
            ) => {
                if predicate1 != predicate2 {
                    return Err(UnificationError::NameMismatch(predicate1, predicate2));
                }

                if args1.len() != args2.len() {
                    return Err(UnificationError::ArityMismatch(args1, args2));
                }

                for (arg1, arg2) in args1.iter().zip(args2) {
                    assignment = arg1.unify_with(arg2, vars, assignment)?;
                }

                Ok(assignment)
            }
            (Biconditional(left1, right1), Biconditional(left2, right2))
            | (Conditional(left1, right1), Conditional(left2, right2)) => {
                assignment = left1.unify_with_helper(left2, vars, assignment)?;
                assignment = right1.unify_with_helper(right2, vars, assignment)?;
                Ok(assignment)
            }
            (Conjunction(operands1), Conjunction(operands2))
            | (Disjunction(operands1), Disjunction(operands2)) => {
                if operands1.len() != operands2.len() {
                    return Err(UnificationError::ArityMismatch(self, other));
                }

                for (operand1, operand2) in operands1.iter().zip(operands2) {
                    assignment = operand1.unify_with_helper(operand2, vars, assignment)?;
                }

                Ok(assignment)
            }
            (Contradiction, Contradiction) | (Tautology, Tautology) => Ok(assignment),
            (
                Existential {
                    var: var1,
                    formula: formula1,
                },
                Existential {
                    var: var2,
                    formula: formula2,
                },
            )
            | (
                Universal {
                    var: var1,
                    formula: formula1,
                },
                Universal {
                    var: var2,
                    formula: formula2,
                },
            ) => {
                if var1 != var2 {
                    // TODO: make a new error type for mismatching variables...?
                    todo!()
                }

                vars.push(var1);
                assignment = formula1.unify_with_helper(formula2, vars, assignment)?;
                vars.pop();

                Ok(assignment)
            }
            (Negation(formula1), Negation(formula2)) => {
                formula1.unify_with_helper(formula2, vars, assignment)
            }
            _ => Err(UnificationError::TypeMismatch(self, other)),
        }
    }
}
