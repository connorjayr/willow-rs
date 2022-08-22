use crate::logic::Statement;
use std::collections::HashMap;
use std::str::FromStr;

type NodeId = usize;

#[derive(Debug, thiserror::Error)]
pub enum TreeError {
    #[error("no node exists with id {id}")]
    NodeDoesNotExist { id: NodeId },
}

#[derive(Debug, PartialEq)]
pub struct Node {
    /// A unique, numerical identifier for this node.
    id: NodeId,
    /// The parent of this node.
    ///
    /// If this node is the root of a tree, then it has no parent.
    parent: Option<NodeId>,
    /// The children of this node.
    children: Vec<NodeId>,
    /// The logical statement in this node.
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

    pub fn children(&self) -> &[usize] {
        &self.children
    }
}

#[derive(Debug)]
pub struct TruthTree {
    /// Maps the ID of a node to the node itself.
    nodes: HashMap<NodeId, Node>,
    // The ID of the root node.
    root: NodeId,
    // The next highest unused ID.
    next_node_id: NodeId,
}

impl PartialEq for TruthTree {
    fn eq(&self, other: &Self) -> bool {
        // `self.next_node_id` is just a counter used to efficiently add nodes, so it does not
        // affect equality
        self.nodes == other.nodes && self.root == other.root
    }
}

impl TruthTree {
    /// Constructs a new [TruthTree].
    ///
    /// Truth trees must have at least one node, so this constructs a new tree with an empty root
    /// node.
    pub fn new() -> Self {
        let root_id = 0;
        let root = Node::new(root_id);

        let mut nodes = HashMap::new();
        nodes.insert(root_id, root);

        Self {
            nodes,
            root: root_id,
            next_node_id: root_id + 1,
        }
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
        if !self.nodes.contains_key(&parent_id) {
            return Err(TreeError::NodeDoesNotExist { id: parent_id });
        }

        let mut child = self.add_node();
        let child_id = child.id;
        child.parent = Some(parent_id);

        let parent = self.nodes.get_mut(&parent_id).unwrap();
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

    /// Returns a reference to the node with the given id, if it exists.
    pub fn get_node(&self, id: usize) -> Option<&Node> {
        self.nodes.get(&id)
    }

    /// Returns a mutable reference to the node with the given id, if it exists.
    pub fn get_node_mut(&mut self, id: usize) -> Option<&mut Node> {
        self.nodes.get_mut(&id)
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

impl Default for TruthTree {
    fn default() -> Self {
        Self::new()
    }
}
