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

const TARGET_FPS: f64 = 64.0;
const FPS_HISTORY_SIZE: usize = TARGET_FPS as usize;
const STARTING_NUM_PARTICLES: usize = 5_000;
const BACKGROUND_COLOUR: &str = "#000";
const FOREGROUND_COLOUR: &str = "#1ce";

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

    last_render_time: f64,
    fps_history: Vec<f64>,
    average_fps: f64,
    frame_count: usize,
}

impl Component for AnimationCanvas {
    type Message = Msg;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let func_string = "(
x*cos(400/sqrt((x*x+y*y))) - y*sin(400/sqrt((x*x+y*y)))
,
x*sin(400/sqrt((x*x+y*y))) + y*cos(400/sqrt((x*x+y*y)))
)";
        let func = match interpret_field_function(&func_string.to_string()) {
            Ok(f) => f,
            Err(e) => {
                info!("{}", e);
                info!("Using default function");
                Box::new(move |(x, y)| (x, y))
            }
        };
        

        ctx.link().send_future(async {Msg::Init});
        let comp_ctx = ctx.link().clone();
        let callback = Closure::wrap(Box::new(move || comp_ctx.send_message(Msg::Render)) as Box<dyn FnMut()>);
        let config = Config {
            width: window().unwrap().inner_width().unwrap().as_f64().unwrap() as usize,
            height: window().unwrap().inner_height().unwrap().as_f64().unwrap() as usize,
            avg_lifetime: 100,
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

            func_string: func_string.to_string(),

            last_render_time: js_sys::Date::now(),
            fps_history: vec![TARGET_FPS; FPS_HISTORY_SIZE],
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
                let t0 = js_sys::Date::now();
                let delta = t0 - self.last_render_time;
                
                self.update_particles(delta);
                self.render();

                let t1 = js_sys::Date::now();
                self.last_render_time = t1;
                let fps = 1000.0 / (t1 - t0);
                self.fps_history[self.frame_count % FPS_HISTORY_SIZE] = fps;
                self.frame_count += 1;

                if self.frame_count % FPS_HISTORY_SIZE == 0 {
                    self.average_fps = self.fps_history.iter().sum::<f64>() / self.fps_history.len() as f64;
                    let max_ratio: f64 = 1.01;
                    let min_ratio: f64 = 0.99;
                    let fps_ratio = min_ratio.max(max_ratio.min(self.average_fps / self.config.target_fps));
                    let target_num_particles = (self.particles.len() as f64 * fps_ratio) as usize;
                    info!("FPS: {}    {}", self.average_fps as i32, target_num_particles);
                    

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
                        info!("{}", pretty_print(self.func_string.to_string())); 
                    },
                    Err(e) => {
                        info!("{}", e);
                        info!("Using default function");
                    }
                }
                
                false
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let on_change = ctx.link().callback(Msg::UpdateFunc);

        html! {
            <div>
                <div style="position: absolute; color: white;"> 
                    <FunctionInput {on_change} value={self.func_string.clone()} />
                    {"FPS: "} {self.average_fps as usize } {"    Particles: "} {self.particles.len()}
                </div>
                <canvas
                    id="canvas"
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
