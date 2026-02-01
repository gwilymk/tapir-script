use agb::{
    display::{GraphicsFrame, object::Object},
    fixnum::Vector2D,
};
use tapir_script::TapirScript;

use crate::{AnimationEvent, Fix2D, entities::EntityData, sprites};

#[derive(TapirScript)]
#[tapir("tapir/coin_to_health.tapir", trigger_type = AnimationEvent)]
pub struct CoinToHealth {
    position: Fix2D,
    target: Vector2D<i32>,
    should_show: bool,
    frame: i32,
}

impl CoinToHealth {
    pub fn new(pos: Fix2D, target: Vector2D<i32>) -> Self {
        Self {
            position: pos,
            target,
            should_show: true,
            frame: 0,
        }
    }
}

impl EntityData for CoinToHealth {
    fn z(&self) -> i32 {
        1
    }

    fn show(&self, frame: &mut GraphicsFrame, _screen_shake: Vector2D<i32>) {
        if self.should_show {
            Object::new(sprites::COIN.animation_sprite(self.frame as usize))
                .set_pos(self.position.round())
                .show(frame);
        }
    }
}
