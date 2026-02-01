use agb::{
    display::{GraphicsFrame, object::Object},
    fixnum::Vector2D,
    rng,
};
use tapir_script::TapirScript;

use crate::{AnimationEvent, Fix2D, entities::EntityData, sprites};

#[derive(TapirScript)]
#[tapir("tapir/particle.tapir", trigger_type = AnimationEvent)]
pub struct Particle {
    position: Fix2D,
    lifetime: i32,
}

impl Particle {
    pub fn new(position: Vector2D<i32>) -> Self {
        Self {
            position: position.into(),
            lifetime: rng::next_i32().rem_euclid(30) + 30,
        }
    }

    fn rng(&self) -> i32 {
        rng::next_i32()
    }
}

impl EntityData for Particle {
    fn show(&self, frame: &mut GraphicsFrame, screen_shake: Vector2D<i32>) {
        Object::new(sprites::COLLECT_PARTICLE.sprite(0))
            .set_pos(self.position.round() + screen_shake)
            .show(frame);
    }
}
