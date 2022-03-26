use crate::logic::Statement;
use std::rc::Rc;
use std::sync::Weak;

struct Node {
    statement: Box<dyn Statement>,
    parent: Weak<Node>,
    children: Vec<Rc<Node>>,
}

#[derive(Default)]
pub struct TruthTree {
    root: Option<Node>,
}
