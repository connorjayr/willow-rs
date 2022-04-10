use std::sync::{Arc, Mutex};
use web_sys::HtmlInputElement;
use willow::truth_tree::TruthTree;
use yew::prelude::*;

struct Model {
    truth_tree: Arc<Mutex<TruthTree>>,
}

enum Msg {
    AddChild,
    UpdateStatement(String),
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
            Msg::UpdateStatement(text) => {
                log::info!("{}", text);
            }
        };
        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let on_input = ctx.link().callback(|e: InputEvent| {
            Msg::UpdateStatement(e.target_unchecked_into::<HtmlInputElement>().value())
        });
        html! {
            <div>
                <input type="text" oninput={on_input}/>
            </div>
        }
    }
}

fn main() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::start_app::<Model>();
}
