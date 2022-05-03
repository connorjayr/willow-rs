use crate::logic::Statement;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
    iter,
};

/// An expression that is associated with an element of the universe of discourse under a variable
/// assignment.
///
/// The set of terms is inductively defined [here](https://en.wikipedia.org/wiki/First-order_logic#Terms).
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Term {
    name: String,
    args: Vec<Term>,
}

impl Term {
    /// Constructs a new [Term].
    ///
    /// # Examples
    ///
    /// ```
    /// use willow::logic::Term;
    ///
    /// let f = Term::new("f", vec![Term::var("x"), Term::var("y")]);
    /// assert_eq!("f(x,y)", f.to_string());
    /// ```
    pub fn new(name: impl Into<String>, args: Vec<Term>) -> Self {
        Self {
            name: name.into(),
            args,
        }
    }

    /// Constructs a new variable.
    ///
    /// A variable is a nullary function symbol; i.e., a function with no arguments.
    ///
    /// # Examples
    ///
    /// ```
    /// use willow::logic::Term;
    ///
    /// let var = Term::var("x");
    /// assert_eq!("x", var.to_string());
    /// ```
    pub fn var(name: impl Into<String>) -> Self {
        Self::new(name, Vec::new())
    }

    /// Returns the arity of this term.
    ///
    /// # Examples
    ///
    /// The arity of a function is equal to its number of arguments:
    /// ```
    /// use willow::logic::Term;
    ///
    /// let f = Term::new("f", vec![Term::var("x")]);
    /// assert_eq!(f.arity(), 1);
    /// ```
    ///
    /// The arity of a variable is zero:
    /// ```
    /// use willow::logic::Term;
    ///
    /// let var = Term::var("x");
    /// assert_eq!(var.arity(), 0);
    /// ```
    pub fn arity(&self) -> usize {
        self.args.len()
    }

    /// Checks if this term can unify with another [Term].
    ///
    /// Two terms are unifiable if all constants match with the same name and arity, and each
    /// variable unifies with at least one corresponding constant. Constants are treated as
    /// variables in this Term if they appear within the `vars` argument.
    ///
    /// # Examples
    ///
    /// ```
    /// use willow::logic::{Substitution, Term};
    ///
    /// let term_1 = Term::new("f", vec![Term::var("x"), Term::var("y")]);
    /// let term_2 = Term::new("f", vec![Term::var("x"), Term::var("z")]);
    ///
    /// let mut assignment = Substitution::new();
    /// let z = Term::var("z");
    /// assignment.insert("y", &z);
    ///
    /// assert_eq!(
    ///     term_1.unify_with(&term_2, &["y"], Substitution::new()).unwrap(),
    ///     assignment
    /// );
    /// ```
    ///
    /// ```
    /// use willow::logic::{Substitution, Term};
    ///
    /// let term1 = Term::var("x");
    /// let term2 = Term::new("f", vec![Term::var("x"), Term::var("z")]);
    ///
    /// let mut assignment = Substitution::new();
    /// assignment.insert("x", &term2);
    ///
    /// assert_eq!(
    ///     term1.unify_with(&term2, &["x"], Substitution::new()).unwrap(),
    ///     assignment
    /// );
    /// ```
    pub fn unify_with<'a>(
        &'a self,
        other: &'a Self,
        vars: &[&str],
        mut assignment: Substitution<'a>,
    ) -> Result<Substitution<'a>, UnificationError<'a>> {
        if self.arity() == 0 && vars.contains(&self.name.as_str()) {
            // `self` is a nullary function which is a variable, so we can assign a value to it
            let var = self.name.as_str();
            // Try to assign var to the value
            return match assignment.get(var) {
                // Variable is not yet assigned => assign the corresponding value to it
                None => {
                    assignment.insert(var, other);
                    Ok(assignment)
                }
                // Variable is already assigned to this value => consistent assignment so no-op
                Some(value) if *value == other => Ok(assignment),
                // Variable has a conflicting assignment => inconsistent assignment so return error
                Some(old) => Err(UnificationError::ConflictingAssignment {
                    var,
                    old,
                    new: other,
                }),
            };
        }
        // self is a function symbol or doesn't vary

        // must exactly match `other` in name...
        if self.arity() != other.arity() {
            return Err(UnificationError::ArityMismatch(self, other));
        }
        // ... and in arity
        if self.name != other.name {
            return Err(UnificationError::NameMismatch(self, other));
        }

        // Unify each argument
        for (a, b) in self.args.iter().zip(other.args.iter()) {
            assignment = a.unify_with(b, vars, assignment)?;
        }

        Ok(assignment)
    }

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
    /// use willow::logic::Term;
    /// use std::collections::HashSet;
    ///
    /// let term = Term::new(
    ///     "f",
    ///     vec![
    ///         Term::new("g", vec![Term::var("x"), Term::var("y")]),
    ///         Term::new("h", vec![Term::var("z")]),
    ///     ],
    /// );
    ///
    /// let constants = [
    ///     Term::new(
    ///         "f",
    ///         vec![
    ///             Term::new("g", vec![Term::var("x"), Term::var("y")]),
    ///             Term::new("h", vec![Term::var("z")]),
    ///         ],
    ///     ),
    ///     Term::new("g", vec![Term::var("x"), Term::var("y")]),
    ///     Term::new("h", vec![Term::var("z")]),
    ///     Term::var("x"),
    ///     Term::var("y"),
    ///     Term::var("z"),
    /// ];
    ///
    /// assert_eq!(term.get_constants(&[]), constants.iter().collect());
    /// ```
    ///
    /// If any arguments are bound variables (instead of constants), then the entire term is not a
    /// constant:
    /// ```
    /// use willow::logic::Term;
    ///
    /// let term = Term::new(
    ///     "f",
    ///     vec![
    ///         Term::new("g", vec![Term::var("x"), Term::var("y")]),
    ///         Term::new("h", vec![Term::var("z"), Term::var("x")]),
    ///     ],
    /// );
    ///
    /// let constants = [Term::var("y"), Term::var("z")];
    ///
    /// assert_eq!(term.get_constants(&["x"]), constants.iter().collect());
    /// ```
    pub fn get_constants(&self, bound_vars: &[&str]) -> HashSet<&Term> {
        let mut constants = HashSet::new();

        if self.arity() == 0 {
            // If this term is a free variable, then it is a constant
            if !bound_vars.contains(&self.name.as_str()) {
                constants.insert(self);
            }
        } else {
            // If this term is a function, then add the constants from each argument
            constants.extend(
                self.args
                    .iter()
                    .flat_map(|arg| arg.get_constants(bound_vars)),
            );

            // If all of the arguments are constants, then the entire term is also a constant
            if self.args.iter().all(|arg| constants.contains(arg)) {
                constants.insert(self);
            }
        }

        constants
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.args.len() {
            0 => write!(f, "{}", self.name),
            _ => {
                let args = self
                    .args
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{}({})", self.name, args)
            }
        }
    }
}

/// A mapping from variables to terms.
///
/// The process of unification creates an instance of [Substitution]. See
/// [this page](https://en.wikipedia.org/wiki/Substitution_(logic)) for more details.
pub type Substitution<'a> = HashMap<&'a str, &'a Term>;

/// Errors that occur during unification of two statements.
#[derive(Debug, thiserror::Error)]
pub enum UnificationError<'a> {
    /// An error that occurs when a term cannot be unified with another term because they have
    /// different arities; e.g., if we try to unify a nullary function with a unary function
    #[error(
        "cannot unify {}-ary function {} with {}-ary function {}",
        .0.arity(),
        .0.name,
        .1.arity(),
        .1.name
    )]
    ArityMismatch(&'a Term, &'a Term),
    #[error("")]
    ConflictingAssignment {
        var: &'a str,
        old: &'a Term,
        new: &'a Term,
    },
    /// An error that occurs when a term cannot be unified with another term because they have
    /// different names (also referred to as "function symbols").
    #[error("cannot unify symbol {0} to symbol {1}")]
    NameMismatch(&'a Term, &'a Term),
    /// An error that occurs when a statement cannot be unified with another statement because they
    /// have differing types (e.g. one is a Conjunction and one is a Disjunction)
    #[error("cannot unify statement {} to {} due to differing types", .0.to_string(), .1.to_string())]
    TypeMismatch(&'a Statement, &'a Statement),
}
