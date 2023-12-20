
use rand::{thread_rng, Rng};

pub struct Particle {
    pub pos: (f64, f64),
    pub velocity: (f64, f64),
    pub space: (f64, f64),
    pub size: f64,
    pub lifetime: i32,
    pub tick: i32,
}

impl Particle {
    pub fn new(width: usize, height: usize, lifetime: i32, size: f64) -> Self {
        let mut randy = thread_rng();
        let space = (width as f64, height as f64);
        let y = randy.gen_range(0f64..height as f64);
        let x = randy.gen_range(0f64..width as f64);
        let size = size;
        let lifetime = randy.gen_range(0..lifetime*2);
        Self {
            pos: (x, y),
            velocity: (0.0, 0.0),
            space,
            size,
            lifetime,
            tick: 0,
        }
    }

    pub fn update(&mut self, lambda: &dyn Fn((f64, f64)) -> (f64, f64), delta: f64) -> bool {
        self.tick += 1;
        let acceleration = lambda(self.pos);
        self.velocity.0 += acceleration.0 * delta / 1000.0;
        self.velocity.1 += acceleration.1 * delta / 1000.0;

        self.pos.0 += self.velocity.0;
        self.pos.1 += self.velocity.1;

        self.tick < self.lifetime
        && self.pos.0 < self.space.0
        && self.pos.0 > 0.0
        && self.pos.1 < self.space.1
        && self.pos.1 > 0.0
    }

    pub fn respawn(&mut self) -> () {
        let mut randy = thread_rng();
        let y = randy.gen_range(0f64..self.space.1);
        let x = randy.gen_range(0f64..self.space.0);
        self.pos = (x, y);
        self.velocity = (0.0, 0.0);
        self.tick = 0;
    }
}
