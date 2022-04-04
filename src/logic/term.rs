use std::fmt::{self, Display, Formatter};

/// A term is an expression that is associated with an element of the universe of discourse under
/// a variable assignment.
///
/// The set of terms is inductively defined [here](https://en.wikipedia.org/wiki/First-order_logic#Terms).
#[derive(Debug)]
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
    /// let term = Term::new("f", vec![Term::var("x"), Term::var("y")]);
    /// assert_eq!("f(x,y)", format!("{}", term));
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
    /// assert_eq!("x", format!("{}", var));
    /// ```
    pub fn var(name: impl Into<String>) -> Self {
        Self::new(name, Vec::new())
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
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{}({})", self.name, args)
            }
        }
    }
}
