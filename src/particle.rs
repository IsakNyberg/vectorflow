
use rand::{thread_rng, Rng};

#[derive(Clone, Copy)]
pub struct Particle {
    pub pos: (f64, f64),
    pub velocity: (f64, f64),
    pub space: ((i32, i32), (i32, i32)),
    pub lifetime: i32,
    pub tick: i32,
}

impl Particle {
    pub fn new(domain: (i32, i32), range: (i32, i32), lifetime: i32) -> Self {
        let mut randy = thread_rng();
        let space = (domain, range);
        let y = randy.gen_range(domain.0..domain.1) as f64;
        let x = randy.gen_range(range.0..range.1) as f64;
        let lifetime = randy.gen_range(0..lifetime*2);
        Self {
            pos: (x, y),
            velocity: (0.0, 0.0),
            space,
            lifetime,
            tick: 0,
        }
    }

    pub fn update(&mut self, lambda: &dyn Fn((f64, f64)) -> (f64, f64), delta: f64) -> bool {
        self.tick += 1;
        self.velocity = lambda(self.pos);

        self.pos.0 += self.velocity.0 * delta / 1000.0;
        self.pos.1 += self.velocity.1 * delta / 1000.0;

        self.tick < self.lifetime
        && (self.pos.0 as i32) > self.space.0.0
        && (self.pos.0 as i32) < self.space.0.1
        && (self.pos.1 as i32) > self.space.1.0
        && (self.pos.1 as i32) < self.space.1.1
    }

    pub fn respawn(&mut self) -> () {
        let mut randy = thread_rng();
        let y = randy.gen_range(self.space.0.0..self.space.0.1) as f64;
        let x = randy.gen_range(self.space.1.0..self.space.1.1) as f64;
        self.pos = (x, y);
        self.velocity = (0.0, 0.0);
        self.tick = 0;
    }
}
