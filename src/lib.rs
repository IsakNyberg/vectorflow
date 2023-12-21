
use std::vec;

use wasm_bindgen::{prelude::{Closure, wasm_bindgen}, JsCast, JsValue};
use web_sys::window;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement};
use yew::prelude::*;
use log::info;


mod particle;

use particle::Particle;

pub enum Msg {
    Init,
    Render,
}

struct Config {
    width: usize,
    height: usize,
    num_particles: usize,
    avg_lifetime: i32,
    colour: String,
    lambda: Box<dyn Fn((f64, f64)) -> (f64, f64)>,
    target_fps: f64,
}

struct AnimationCanvas {
    canvas: NodeRef,
    particles: Vec<Particle>,
    callback: Closure<dyn FnMut()>,
    config: Config,
    last_time_rendered: f64,
    fps_history: Vec<f64>,
    frame_count: usize,
}

impl Component for AnimationCanvas {
    type Message = Msg;
    type Properties = ();
    fn create(ctx: &Context<Self>) -> Self {
        ctx.link().send_future(async {Msg::Init});
        let comp_ctx = ctx.link().clone();
        let callback = Closure::wrap(Box::new(move || comp_ctx.send_message(Msg::Render)) as Box<dyn FnMut()>);
        let config = Config {
            width: window().unwrap().inner_width().unwrap().as_f64().unwrap() as usize,
            height: window().unwrap().inner_height().unwrap().as_f64().unwrap() as usize,
            num_particles: 17000,
            avg_lifetime: 100,
            colour: String::from("#1ce"),
            target_fps: 30.0,
            lambda: Box::new(|(x, y)| {
                let theta = 400.0 / (x * x + y * y).sqrt();
                let x = x * theta.cos() - y * theta.sin();
                let y = x * theta.sin() + y * theta.cos();
                (x, y)
            }),
        };
        Self {
            last_time_rendered: js_sys::Date::now(),
            canvas: NodeRef::default(),
            particles: vec![],
            callback,
            config,
            fps_history: vec![60.0; 100],
            frame_count: 0,

        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Init => {
                let canvas: HtmlCanvasElement = self.canvas.cast().unwrap();
                canvas.set_width(self.config.width.try_into().unwrap());
                canvas.set_height(self.config.height.try_into().unwrap());
                self.particles = Vec::with_capacity(self.config.num_particles);
                for _ in 0..self.config.num_particles {
                    self.particles.push(Particle::new(
                        (self.config.width as i32 / -2, self.config.width as i32 / 2),
                        (self.config.height as i32 / -2, self.config.height as i32 / 2),
                        self.config.avg_lifetime,
                    ));
                }

                ctx.link().send_message(Msg::Render);
                true
            }
            Msg::Render => {
                let t0 = js_sys::Date::now();
                let delta = t0 - self.last_time_rendered;
                self.render(delta);

                if self.frame_count % 25 == 0 {    
                    let avg_fps = self.fps_history.iter().sum::<f64>() / self.fps_history.len() as f64;
                    if avg_fps > self.config.target_fps * 1.1 {
                        self.config.num_particles = (self.config.num_particles as f64*1.05) as usize;
                        info!(" +FPS: {}    {}", avg_fps as i32, self.config.num_particles);

                    } else if avg_fps < self.config.target_fps * 0.9 {
                        self.config.num_particles = (self.config.num_particles as f64*0.95) as usize;
                        info!("-FPS: {}    {}", avg_fps as i32, self.config.num_particles);
                    } else {
                        info!(" FPS: {}    {}", avg_fps as i32, self.config.num_particles);
                    }
                    
                    while self.particles.len() < self.config.num_particles {
                        self.particles.push(Particle::new(
                            (self.config.width as i32 / -2, self.config.width as i32 / 2),
                            (self.config.height as i32 / -2, self.config.height as i32 / 2),
                            self.config.avg_lifetime,
                        ));
                    }
                    while self.particles.len() > self.config.num_particles {
                        self.particles.pop();
                    }
                }


                let t1 = js_sys::Date::now();
                self.last_time_rendered = t1;
                let fps = 1000.0 / (t1 - t0);
                self.fps_history[self.frame_count % 100] = fps;
                self.frame_count += 1;

                false
            }
        }
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            <div>
                <canvas
                    id="canvas"
                    ref={self.canvas.clone()}>
                </canvas>
            </div>
        }
    }
}

impl AnimationCanvas {
    fn render(&mut self, delta: f64) {
        let canvas: HtmlCanvasElement = self.canvas.cast().unwrap();
        let ctx: CanvasRenderingContext2d = canvas.get_context("2d").unwrap().unwrap().unchecked_into();
        ctx.set_global_alpha(0.01); // lower values make the trails longer
        ctx.set_fill_style(&JsValue::from("#000"));
        ctx.fill_rect(0.0, 0.0, canvas.width().into(), canvas.height().into());
        ctx.set_global_alpha(1.0);
        let particles = &mut self.particles;

        particles.iter_mut().for_each(|particle: &mut Particle| {
            if particle.update(&self.config.lambda, delta) {
                render_particle(&self.config, particle, &ctx);
            } else {
                particle.respawn();
            }
        });


        window()
            .unwrap()
            .request_animation_frame(self.callback.as_ref().unchecked_ref())
            .unwrap();
    }

}
fn render_particle(config: &Config, particle: &Particle, ctx: &CanvasRenderingContext2d) {
    ctx.begin_path();
    let js_rgb = config.colour.as_str();
    ctx.set_fill_style(&JsValue::from_str(js_rgb.as_ref()));
    let x = particle.pos.0 + config.width as f64 / 2.0;
    let y = particle.pos.1 + config.height as f64 / 2.0;
    ctx.fill_rect(x, y, 1.0, 1.0);

    ctx.fill();
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
fn main() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::start_app::<App>();
}
