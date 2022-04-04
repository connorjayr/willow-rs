use std::fmt::{self, Display, Formatter};

/// A term is an expression that is associated with an element of the universe of discourse under
/// a variable assignment.
///
/// The set of terms is inductively defined [here](https://en.wikipedia.org/wiki/First-order_logic#Terms).
#[derive(Debug)]
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
