use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
};

/// An expression that is associated with an element of the universe of discourse under a variable
/// assignment.
///
/// The set of terms is inductively defined [here](https://en.wikipedia.org/wiki/First-order_logic#Terms).
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Term<'a> {
    name: &'a str,
    args: Vec<Term<'a>>,
}

impl<'a> Term<'a> {
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
    pub fn new(name: &'a str, args: Vec<Term<'a>>) -> Self {
        Self { name, args }
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
    pub fn var(name: &'a str) -> Self {
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

    /// Checks if this Term can unify with another Term.
    ///
    /// Two Terms are unifiable if all constants match with the same name and arity, and each
    /// variable unifies with at least one corresponding constant. Constants are treated as
    /// variables in this Term if they appear within the `vars` argument.
    ///
    /// # Examples
    ///
    /// ```
    /// use willow::logic::{Substitution, Term};
    ///
    /// let term1 = Term::new("f", vec![Term::var("x"), Term::var("y")]);
    /// let term2 = Term::new("f", vec![Term::var("x"), Term::var("z")]);
    ///
    /// let mut assignment = Substitution::new();
    /// let z = Term::var("z");
    /// assignment.insert("y", &z);
    ///
    /// assert_eq!(
    ///     term1.unify_with(&term2, &["y"], Substitution::new()).unwrap(),
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
    pub fn unify_with(
        &'a self,
        other: &'a Self,
        quantified_vars: &[&str],
        mut assignment: Substitution<'a>,
    ) -> Result<Substitution<'a>, UnificationError<'a>> {
        if self.args.is_empty() && quantified_vars.contains(&self.name) {
            // The current position in `self` is a variable
            let var = self.name;
            // Try to assign var to the value
            return match assignment.get(var) {
                // Variable is not yet assigned => assign it
                None => {
                    assignment.insert(var, other);
                    Ok(assignment)
                }
                // Variable is already assigned to this value => do nothing
                Some(value) if *value == other => Ok(assignment),
                // Variable has a conflicting assignment => return an error
                Some(old) => Err(UnificationError::ConflictingAssignment {
                    var,
                    old,
                    new: other,
                }),
            };
        }
        // self is either a constant or a function symbol, so it must exactly match other in name
        // and arity
        if self.args.len() != other.args.len() {
            return Err(UnificationError::ArityMismatch(self, other));
        }

        if self.name != other.name {
            return Err(UnificationError::NameMismatch(self, other));
        }

        // Unify each argument
        for (a, b) in self.args.iter().zip(other.args.iter()) {
            assignment = a.unify_with(b, quantified_vars, assignment)?;
        }

        Ok(assignment)
    }

    /// Gets the set of constants used in this Term.
    ///
    /// Note that
    ///
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
    /// let constant_array = [
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
    /// let mut constants = HashSet::new();
    /// constants.extend(constant_array.iter());
    ///
    /// assert_eq!(term.get_constants(&[]), constants);
    ///
    /// ```
    pub fn get_constants(&self, vars: &[&str]) -> HashSet<&Term> {
        let mut constants = HashSet::new();

        if self.args.is_empty() {
            if !vars.contains(&self.name) {
                constants.insert(self);
            }
        } else {
            // This term is a function of subterms
            /*
            Basic imperative approach

            let mut num_const_args: usize = 0;
            for arg in self.args.iter() {
                let sub_constants = arg.get_constants(vars);

                if sub_constants.contains(&arg) {
                    num_const_args += 1;
                }
                constants.extend(sub_constants);
            }
            */

            // Functional approach

            // Add the constants from each of the arguments to the constants set
            constants.extend(self.args.iter().flat_map(|arg| arg.get_constants(vars)));

            // Count how many args are constants (appear in the constants set)
            let num_const_args = self
                .args
                .iter()
                .filter(|arg| constants.contains(arg))
                .count();

            // If this term is a function symbol made of constant arguments, then it is a constant
            if num_const_args == self.args.len() {
                constants.insert(self);
            }
        }

        constants
    }
}

impl Display for Term<'_> {
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
pub type Substitution<'a> = HashMap<&'a str, &'a Term<'a>>;

/// Errors that occur during unification of two terms.
#[derive(Debug, thiserror::Error)]
pub enum UnificationError<'a> {
    #[error("")]
    ConflictingAssignment {
        var: &'a str,
        old: &'a Term<'a>,
        new: &'a Term<'a>,
    },
    /// An error that occurs when a term cannot be unified with another term because they have
    /// different names (also referred to as "function symbols").
    #[error("cannot unify symbol {0} to symbol {1}")]
    NameMismatch(&'a Term<'a>, &'a Term<'a>),
    /// An error that occurs when a term cannot be unified with another term because they have
    /// different arities; e.g., if we try to unify a variable with a function.
    #[error(
        "cannot unify function {} with arity {} to function {} with arity {}",
        .0.name,
        .0.arity(),
        .1.name,
        .1.arity()
    )]
    ArityMismatch(&'a Term<'a>, &'a Term<'a>),
}
