use wasm_bindgen::{prelude::{Closure, wasm_bindgen}, JsCast, JsValue};
use web_sys::window;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement};
use yew::prelude::*;
use log::info;


mod function_input;
use function_input::FunctionInput;

mod particle;
use particle::Particle;

mod parser;
use parser::{interpret_field_function, pretty_print};

pub enum Msg {
    Init,
    Render,
    UpdateFunc(String),
}

const TARGET_FPS: f64 = 32.0;  // never set this higher than 60
const FPS_UPDATE_PERIOD: f64 = 500.0;
const STARTING_NUM_PARTICLES: usize = 10_000;
const BACKGROUND_COLOUR: &str = "#000";
const FOREGROUND_COLOUR: &str = "#1ce";
const DEFAULT_FUNCTION: &str = "(
200*abs(cos(len(x, y)/25))
,
200*sin(len(x, y)/25)
)";
struct Config {
    width: usize,
    height: usize,
    avg_lifetime: i32,
    fg_colour: JsValue,
    bg_colour: JsValue,
    func: Box<dyn Fn((f64, f64)) -> (f64, f64)>,
    target_fps: f64,
}

struct AnimationCanvas {
    canvas: NodeRef,
    particles: Vec<Particle>,
    callback: Closure<dyn FnMut()>,
    config: Config,

    func_string: String,
    func_error_message: String,

    time_delta: f64,
    average_fps: f64,
    frame_count: usize,
}

impl Component for AnimationCanvas {
    type Message = Msg;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let func = interpret_field_function(&DEFAULT_FUNCTION.to_string()).unwrap();
        

        ctx.link().send_future(async {Msg::Init});
        let comp_ctx = ctx.link().clone();
        let callback = Closure::wrap(Box::new(move || comp_ctx.send_message(Msg::Render)) as Box<dyn FnMut()>);
        let config = Config {
            width: window().unwrap().inner_width().unwrap().as_f64().unwrap() as usize + 100,
            height: window().unwrap().inner_height().unwrap().as_f64().unwrap() as usize + 100,
            avg_lifetime: 200,
            fg_colour: JsValue::from(FOREGROUND_COLOUR),
            bg_colour: JsValue::from(BACKGROUND_COLOUR),
            target_fps: TARGET_FPS,
            func: func,
        };
        Self {
            canvas: NodeRef::default(),
            particles: vec![],
            callback: callback,
            config: config,

            func_string: DEFAULT_FUNCTION.to_string(),
            func_error_message: "".to_string(),

            time_delta: js_sys::Date::now(),
            average_fps: TARGET_FPS,
            frame_count: 0,
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Init => {
                let canvas: HtmlCanvasElement = self.canvas.cast().unwrap();
                canvas.set_width(self.config.width.try_into().unwrap());
                canvas.set_height(self.config.height.try_into().unwrap());
                self.particles = Vec::with_capacity(STARTING_NUM_PARTICLES);
                let w = self.config.width as i32;
                let h = self.config.height as i32;
                for _ in 0..STARTING_NUM_PARTICLES {
                    self.particles.push(Particle::new(
                        (-w, w),
                        (-h, h),
                        self.config.avg_lifetime,
                    ));
                }

                ctx.link().send_message(Msg::Render);
                true
            }
            Msg::Render => {
                let delta = 1000.0 / self.average_fps;
                
                self.update_particles(delta);
                self.render();
                self.frame_count += 1;

                let time = js_sys::Date::now();
                if time - self.time_delta > FPS_UPDATE_PERIOD {
                    self.time_delta = time;
                    self.average_fps = 1000.0 * self.frame_count as f64 / FPS_UPDATE_PERIOD;
                    self.frame_count = 0;

                    let fps_ratio = (self.average_fps / self.config.target_fps).max(0.90).min(1.10);
                    let target_num_particles = (self.particles.len() as f64 * fps_ratio) as usize;
                    info!("FPS: {}   {}", self.average_fps as i32, target_num_particles);
                    
                    let w = self.config.width as i32;
                    let h = self.config.height as i32;
                    self.particles.resize(
                        target_num_particles,
                        Particle::new(
                            (-w, w),
                            (-h, h),
                            self.config.avg_lifetime,
                        ),
                    );
                    true
                } else {
                    false
                }
            }
            Msg::UpdateFunc(func_string) => {
                self.func_string = func_string.clone();
                match interpret_field_function(&func_string) {
                    Ok(f) => {
                        self.config.func = f;
                        self.func_error_message = "".to_string();
                        info!("{}", pretty_print(self.func_string.to_string())); 
                    },
                    Err(e) => {
                        info!("{}", e);
                        self.func_error_message = e;
                    }
                }
                
                false
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let on_change = ctx.link().callback(Msg::UpdateFunc);

        let func_error_message_html = if self.func_error_message.len() > 0 {
            html! {
                <div class="error-message">{self.func_error_message.clone()}</div>
            }
        } else {
            html! {}
        };

        html! {
            <div>
                <div style="position: absolute; color: #1ce;"> 
                    <FunctionInput {on_change} value={self.func_string.clone()} />
                    {func_error_message_html}
                    <div> {"FPS: "} {self.average_fps as usize } {"    Particles: "} {self.particles.len()} </div>
                </div>
                <canvas
                    id="canvas"
                    class="canvas"
                    ref={self.canvas.clone()}>
                </canvas>
            </div>
        }
    }
}

impl AnimationCanvas {
    fn update_particles(&mut self, delta: f64) {
        for particle in self.particles.iter_mut() {
            if !particle.update(&self.config.func, delta) { 
                particle.respawn();
            }
        }
    }

    fn render(&mut self) {
        let canvas: HtmlCanvasElement = self.canvas.cast().unwrap();
        let ctx: CanvasRenderingContext2d = canvas.get_context("2d").unwrap().unwrap().unchecked_into();

        // put a black square over canvas to fade old particles
        ctx.set_global_alpha(0.01);  // lower values make the trails longer
        ctx.set_fill_style(&self.config.bg_colour);
        ctx.fill_rect(0.0, 0.0, self.config.width as f64, self.config.height as f64);

        // render all updated particles
        ctx.set_global_alpha(1.0);
        ctx.set_fill_style(&self.config.fg_colour);
        for particle in self.particles.iter_mut() {
            // shift the particle's position so that the origin is in the center of the canvas
            let x = particle.pos.0 + (self.config.width as f64 / 2.0);
            let y = (self.config.height as f64 / 2.0) - particle.pos.1;
            ctx.fill_rect(x, y, 1.0, 1.0);
        }

        window()
            .unwrap()
            .request_animation_frame(self.callback.as_ref().unchecked_ref())
            .unwrap();
    }
}

#[function_component(App)]
fn app_body() -> Html {
    html! {
        <>
            <AnimationCanvas/>
        </>
    }
}


#[wasm_bindgen(start)]
pub fn main() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::Renderer::<App>::new().render();
}
