use std::{
    collections::HashMap,
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

    /// Checks if the two terms can unify. Two terms are unifiable if they have the same structure
    /// and all constants within the terms match. The first term can be optionally quantified via
    /// the `quantified_vars` argument. Any variable in the `quantified_vars` argument acts as a
    /// wildcard during the unification process. However, a variable can not be unified twice during
    /// unification.
    ///
    /// # Examples
    ///
    /// ```
    /// use willow::logic::Term;
    ///
    /// let term1 = Term::new("f", vec![Term::var("x"), Term::var("y")]);
    /// let term2 = Term::new("f", vec![Term::var("x"), Term::var("z")]);
    ///
    /// let mut assignment = Assignment::new();
    /// assignment.insert("y", term2.args.get(1).unwrap());
    ///
    /// assert_eq!(
    ///     Term::get_assignment(&term1, &term2, &vec!["y"], Assignment::new()).unwrap(),
    ///     assignment
    /// );
    /// ```
    pub fn unify_with(
        &'a self,
        other: &'a Self,
        quantified_vars: &[&str],
        mut assignment: Substitution<'a>,
    ) -> Result<Substitution<'a>, UnificationError<'a>> {
        todo!()
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
