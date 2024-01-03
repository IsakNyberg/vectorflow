use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::{Event, HtmlInputElement, InputEvent};
use yew::prelude::*;

#[derive(Clone, PartialEq, Properties)]
pub struct Props {
    pub value: String,
    pub on_change: Callback<String>,
}

fn get_value_from_input_event(e: InputEvent) -> String {
    let event: Event = e.dyn_into().unwrap_throw();
    let event_target = event.target().unwrap_throw();
    let target: HtmlInputElement = event_target.dyn_into().unwrap_throw();
    web_sys::console::log_1(&target.value().into());
    target.value()
}

fn get_value_from_textarea_event(e: InputEvent) -> String {
    let target: web_sys::HtmlTextAreaElement = e.target_unchecked_into();
    target.value()
}

#[function_component(FunctionInput)]
pub fn function_input(props: &Props) -> Html {
    let Props { value, on_change } = props.clone();

    let oninput = Callback::from(move |input_event: InputEvent| {
        // on_change.emit(get_value_from_input_event(input_event));
        on_change.emit(get_value_from_textarea_event(input_event));
    });

    let style = format!("height: 80px; width: 500px; rows=5; overflow: hidden;");

    html! {
        <textarea
            placeholder="(x, y)"
            value={value}
            oninput={oninput}
            style={style}
        />
    }
    // html! {
    //     <textarea type="text" {value} {oninput} style={style}/>
    // }
}