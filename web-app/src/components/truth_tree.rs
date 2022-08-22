use crate::components::TruthTreeNode;
use yew::prelude::*;

pub struct TruthTree {
    tree: willow::truth_tree::TruthTree,
}

impl TruthTree {
    fn render_branch(&self, mut id: usize) -> Html {
        let mut branch = Vec::new();
        loop {
            let node = self.tree.get_node(id).unwrap();
            branch.push(html! {<TruthTreeNode key={id} id={id} />});
            if node.children().len() == 1 {
                id = node.children()[0];
            } else {
                break;
            }
        }
        branch.into_iter().collect()
    }
}

impl Component for TruthTree {
    type Message = ();
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            tree: willow::truth_tree::TruthTree::new(),
        }
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        self.render_branch(self.tree.root().id())
    }
}
