use crate::logic::Statement;
use std::collections::HashMap;
use std::str::FromStr;
use std::sync::{Arc, Mutex};

type NodeId = usize;

#[derive(Debug, thiserror::Error)]
pub enum TreeError {
    #[error("no node exists with id {id}")]
    NodeDoesNotExist { id: NodeId },
}

#[derive(Debug)]
pub struct Node {
    /// A unique, numerical identifier for this node.
    id: NodeId,
    /// The parent of this node.
    ///
    /// If this node is the root of a tree, then it has no parent.
    parent: Option<NodeId>,
    /// The children of this node.
    children: Vec<NodeId>,
    /// The logical statement for this node.
    statement: Result<Statement, <Statement as FromStr>::Err>,
    /// Indicates if this node is a premise; i.e., it is assumed to be valid in a truth tree.
    premise: bool,
}

impl Node {
    pub fn new(id: NodeId) -> Self {
        Self {
            id,
            statement: "".parse(),
            premise: false,
            parent: None,
            children: Vec::new(),
        }
    }

    pub fn id(&self) -> NodeId {
        self.id
    }
}

#[derive(Debug)]
pub struct TruthTree {
    nodes: HashMap<NodeId, Node>,
    root: NodeId,
    next_node_id: NodeId,
}

impl TruthTree {
    /// Constructs a new [TruthTree].
    ///
    /// Truth trees must have at least one node, so this constructs a new tree with an empty root
    /// node.
    pub fn new() -> Arc<Mutex<Self>> {
        let root_id = 0;
        let tree = Self {
            nodes: HashMap::new(),
            root: root_id,
            next_node_id: root_id + 1,
        };
        let tree = Arc::new(Mutex::new(tree));

        let root = Node::new(root_id);
        {
            let mut tree = tree.lock().unwrap();
            tree.nodes.insert(root_id, root);
        }

        tree
    }

    /// Adds an empty node to this truth tree.
    ///
    /// This function does not link the new node to any other nodes in the tree, so it is the
    /// responsibility of the caller to do so.
    fn add_node(&mut self) -> &mut Node {
        let id = self.generate_node_id();

        let node = Node::new(id);

        if self.nodes.insert(id, node).is_some() {
            // We hold `&mut self`, which guarantees that no other mutable references could claim
            // `self.next_node_id` before we use it. The only way that this could panic is if
            // integer overflow occurs.
            panic!("node id {} is not unique", id);
        }

        self.nodes.get_mut(&id).unwrap()
    }

    /// Gets the next available node id and marks it as taken.
    fn generate_node_id(&mut self) -> NodeId {
        let new_id = self.next_node_id;
        self.next_node_id += 1;
        new_id
    }

    /// Adds an empty child to a node in this truth tree.
    pub fn add_child(&mut self, parent_id: NodeId) -> Result<(), TreeError> {
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

    /// Determines whether this node is valid; i.e. this node's statement is a logical consequence
    /// of some other node's statement in the truth tree.
    pub fn is_node_logically_valid(&self, node_id: NodeId) -> Result<(), TreeError> {
        todo!()
    }

    /// Determines whether this node's statement is fully decomposed in the branches it affects.
    pub fn is_node_decomposed(&self, node_id: NodeId) -> Result<(), TreeError> {
        todo!()
    }

    /// Returns a reference to the root node of this tree.
    pub fn root(&self) -> &Node {
        self.nodes.get(&self.root).unwrap()
    }

    /// Returns the number of nodes in this tree.
    pub fn size(&self) -> usize {
        self.nodes.len()
    }
}
