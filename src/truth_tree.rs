use crate::logic::Statement;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::sync::{Arc, Mutex, Weak};

#[derive(Debug)]
enum TreeError {
    NodeAlreadyExists { id: usize },
}

impl Display for TreeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TreeError::NodeAlreadyExists { id } => {
                write!(f, "A node with id {} already exists", id)
            }
        }
    }
}

impl Error for TreeError {}

struct Node {
    id: usize,
    tree: Weak<Mutex<TruthTree>>,
    // statement: Option<Statement<'_>>,
    premise: bool,
    parent: Option<usize>,
    children: Vec<usize>,
}

impl Node {
    pub fn new(id: usize, tree: Weak<Mutex<TruthTree>>) -> Self {
        Self {
            id,
            tree,
            // statement: None,
            premise: false,
            parent: None,
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, child_id: usize) -> Result<(), TreeError> {
        let mut child = Node::new(child_id, Weak::clone(&self.tree));
        child.parent = Some(self.id);

        {
            let tree = self.tree.upgrade().unwrap();
            let mut tree = tree.lock().unwrap();
            match tree.nodes.entry(child_id) {
                Entry::Occupied(_) => return Err(TreeError::NodeAlreadyExists { id: child_id }),
                Entry::Vacant(entry) => entry.insert(child),
            };
        }

        self.children.push(child_id);
        Ok(())
    }
}

pub struct TruthTree {
    nodes: HashMap<usize, Node>,
    root: usize,
}

impl TruthTree {
    pub fn new() -> Arc<Mutex<Self>> {
        let root_id = 0;
        let tree = Self {
            nodes: HashMap::new(),
            root: root_id,
        };
        let tree = Arc::new(Mutex::new(tree));

        let root = Node::new(root_id, Arc::downgrade(&tree));
        {
            let mut tree = tree.lock().unwrap();
            tree.nodes.insert(root_id, root);
        }

        tree
    }
}
