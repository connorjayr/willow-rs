use std::sync::{Arc, Mutex};
use willow::truth_tree::TruthTree;
use yew::prelude::*;

struct Model {
    truth_tree: Arc<Mutex<TruthTree>>,
}

enum Msg {
    AddChild,
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            truth_tree: TruthTree::new(),
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::AddChild => {
                let mut truth_tree = self.truth_tree.lock().unwrap();
                let root_id = truth_tree.root().id();
                for _ in 0..1_000 {
                    truth_tree.add_child(root_id).unwrap();
                }
            }
        };
        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <div>
                <p onclick={ctx.link().callback(|_| Msg::AddChild)}>{format!("{:?}", self.truth_tree.lock().unwrap().size())}</p>
            </div>
        }
    }
}

fn main() {
    yew::start_app::<Model>();
}
