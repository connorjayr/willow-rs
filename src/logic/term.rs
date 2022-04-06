use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display, Formatter},
};

/// A term is an expression that is associated with an element of the universe of discourse under
/// a variable assignment.
///
/// The set of terms is inductively defined [here](https://en.wikipedia.org/wiki/First-order_logic#Terms).
#[derive(Debug, PartialEq, Eq)]
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
    /// let term = Term::new("f", vec![Term::var("x"), Term::var("y")]);
    /// assert_eq!("f(x,y)", format!("{}", term));
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
    /// assert_eq!("x", format!("{}", var));
    /// ```
    pub fn var(name: &'a str) -> Self {
        Self::new(name, Vec::new())
    }

    /// Checks if this term can unify with the other given term. See `get_assignment` for more
    /// details.
    ///
    /// # Examples
    ///
    /// ```
    /// use willow::logic::{Assignment, Term};
    ///
    /// let term1 = Term::new("f", vec![Term::var("x"), Term::var("y")]);
    /// let term2 = Term::new("f", vec![Term::var("x"), Term::var("z")]);
    ///
    /// assert!(term1.unifies(&term2, &vec!["y"]));
    /// ```
    pub fn unifies(&self, other: &Self, quantified_vars: &Vec<&str>) -> bool {
        let assignment = Assignment::new();
        match Term::get_assignment(self, other, quantified_vars, assignment) {
            Ok(_) => true,
            Err(_) => false,
        }
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
    /// use willow::logic::{Assignment, Term};
    ///
    /// let term1 = Term::new("f", vec![Term::var("x"), Term::var("y")]);
    /// let term2 = Term::new("f", vec![Term::var("x"), Term::var("z")]);
    ///
    /// let mut assignment = Assignment::new();
    /// let z = Term::var("z");
    /// assignment.insert("y", &z);
    ///
    /// assert_eq!(
    ///     Term::get_assignment(&term1, &term2, &vec!["y"], Assignment::new()).unwrap(),
    ///     assignment
    /// );
    /// ```
    ///
    /// ```
    /// use willow::logic::{Assignment, Term};
    ///
    /// let term1 = Term::var("x");
    /// let term2 = Term::new("f", vec![Term::var("x"), Term::var("z")]);
    ///
    /// let mut assignment = Assignment::new();
    /// assignment.insert("x", &term2);
    ///
    /// assert_eq!(
    ///     Term::get_assignment(&term1, &term2, &vec!["x"], Assignment::new()).unwrap(),
    ///     assignment
    /// );
    /// ```
    pub fn get_assignment(
        first: &'a Self,
        second: &'a Self,
        quantified_vars: &Vec<&str>,
        mut assignment: Assignment<'a>,
    ) -> Result<Assignment<'a>, UnifyError<'a>> {
        if first.args.len() == 0 && quantified_vars.contains(&first.name) {
            // We are at a variable in `first`
            let var = first.name;
            // Try to assign var to the value
            return match assignment.get(var) {
                // Variable is not yet assigned => assign it
                None => {
                    assignment.insert(var, second);
                    Ok(assignment)
                }
                // Variable is already assigned to this value => do nothing
                Some(value) if *value == second => Ok(assignment),
                // Variable has a conflicting assignment => return an error
                Some(old) => Err(UnifyError::ConflictingAssignment {
                    var,
                    old,
                    new: second,
                }),
            };
        }
        // first is either a constant or a function symbol, so it must exactly match second in name
        // and arity
        if first.args.len() != second.args.len() {
            return Err(UnifyError::ArityMismatch(first, second));
        }

        if first.name != second.name {
            return Err(UnifyError::NameMismatch(first, second));
        }

        // Unify each argument
        for (a, b) in first.args.iter().zip(second.args.iter()) {
            assignment = Term::get_assignment(a, b, quantified_vars, assignment)?;
        }

        Ok(assignment)
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
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{}({})", self.name, args)
            }
        }
    }
}

/// An assignment of variables in logic is a mapping from variables to constants. We represent this
/// as a map from variable names to Term objects.
pub type Assignment<'a> = HashMap<&'a str, &'a Term<'a>>;

#[derive(Debug)]
pub enum UnifyError<'a> {
    ConflictingAssignment {
        var: &'a str,
        old: &'a Term<'a>,
        new: &'a Term<'a>,
    },
    NameMismatch(&'a Term<'a>, &'a Term<'a>),
    ArityMismatch(&'a Term<'a>, &'a Term<'a>),
}

impl Display for UnifyError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            UnifyError::ConflictingAssignment { var, old, new } => write!(
                f,
                "Could not assign {} to {}; already assigned to {}",
                var, old, new
            ),
            UnifyError::ArityMismatch(first, second) => {
                write!(
                    f,
                    "{} and {} are constants with different arity.",
                    first, second
                )
            }
            UnifyError::NameMismatch(first, second) => {
                write!(
                    f,
                    "{} and {} are constants with different names.",
                    first, second
                )
            }
        }
    }
}

impl Error for UnifyError<'_> {}
