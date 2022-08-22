use yew::prelude::*;

pub struct TruthTreeNode;

#[derive(PartialEq, Properties)]
pub struct Props {
    pub id: usize,
}

impl Component for TruthTreeNode {
    type Message = ();
    type Properties = Props;

    fn create(_ctx: &yew::Context<Self>) -> Self {
        Self
    }

    fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        html! {
            <input type="text" value={ctx.props().id.to_string()}/>
        }
    }
}
