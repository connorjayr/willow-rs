use yew::prelude::*;

struct Model;

impl Component for Model {
    type Message = ();
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Model
    }

    fn update(&mut self, _ctx: &Context<Self>, _msg: Self::Message) -> bool {
        true
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            <div>
                <p>{"Hello world"}</p>
            </div>
        }
    }
}

fn main() {
    yew::start_app::<Model>();
}
