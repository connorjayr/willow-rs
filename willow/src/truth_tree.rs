use std::collections::HashMap;
use std::sync::{Arc, Mutex, Weak};

#[derive(Debug, thiserror::Error)]
pub enum TreeError {
    #[error("no node exists with id {id}")]
    NodeDoesNotExist { id: usize },
}

#[derive(Debug)]
pub struct Node {
    /// A weak reference to the tree that contains this node.
    ///
    /// The reference must be weak in order to prevent a reference cycle and possible memory leak.
    tree: Weak<Mutex<TruthTree>>,
    /// A unique, numerical identifier for this node.
    id: usize,
    /// The parent of this node.
    ///
    /// If this node is the root of a tree, then it has no parent.
    parent: Option<usize>,
    /// The children of this node.
    children: Vec<usize>,
    // statement: ???,
    /// Indicates if this node is a premise; i.e., it is assumed to be valid in a truth tree.
    premise: bool,
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

    pub fn id(&self) -> usize {
        self.id
    }
}

#[derive(Debug)]
pub struct TruthTree {
    nodes: HashMap<usize, Node>,
    root: usize,
    next_node_id: usize,
}

impl TruthTree {
    pub fn new() -> Arc<Mutex<Self>> {
        let root_id = 0;
        let tree = Self {
            nodes: HashMap::new(),
            root: root_id,
            next_node_id: root_id + 1,
        };
        let tree = Arc::new(Mutex::new(tree));

        let root = Node::new(root_id, Arc::downgrade(&tree));
        {
            let mut tree = tree.lock().unwrap();
            tree.nodes.insert(root_id, root);
        }

        tree
    }

    fn add_node(&mut self) -> &mut Node {
        let id = self.next_node_id;
        self.next_node_id += 1;

        let node = Node::new(id, Weak::clone(&self.root().tree));

        if self.nodes.insert(id, node).is_some() {
            panic!("node id {} is not unique", id);
        }

        self.nodes.get_mut(&id).unwrap()
    }

    pub fn add_child(&mut self, parent_id: usize) -> Result<(), TreeError> {
        let mut child = self.add_node();
        let child_id = child.id;
        child.parent = Some(parent_id);

        let parent = match self.nodes.get_mut(&parent_id) {
            Some(parent) => parent,
            None => return Err(TreeError::NodeDoesNotExist { id: parent_id }),
        };
        parent.children.push(child_id);

        Ok(())
    }

    /// Returns a reference to the root node of this tree.
    pub fn root(&self) -> &Node {
        self.nodes.get(&self.root).unwrap()
    }

    pub fn size(&self) -> usize {
        self.nodes.len()
    }
}
