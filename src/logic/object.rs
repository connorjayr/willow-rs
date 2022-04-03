use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct Object {
    name: String,
    args: Vec<Object>,
}

impl Object {
    pub fn new(name: String, args: Vec<Object>) -> Self {
        Self { name, args }
    }

    pub fn with_name(name: String) -> Self {
        Self::new(name, Vec::new())
    }
}

impl Display for Object {
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
