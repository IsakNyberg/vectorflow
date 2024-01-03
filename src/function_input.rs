use web_sys::InputEvent;
use yew::prelude::*;

#[derive(Clone, PartialEq, Properties)]
pub struct Props {
    pub value: String,
    pub on_change: Callback<String>,
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
            spellcheck="false"
        />
    }
}