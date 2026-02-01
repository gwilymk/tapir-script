use agb::{
    display::{GraphicsFrame, object::Object},
    fixnum::{Rect, Vector2D, vec2},
    rng,
};
use tapir_script::{Script, TapirScript};

use crate::{AnimationEvent, Fix2D, entities::EntityData, sprites};

#[derive(TapirScript)]
#[tapir("tapir/enemy.tapir", trigger_type = AnimationEvent)]
pub struct Enemy {
    position: Fix2D,
    animation_frame: i32,
}

impl Enemy {
    pub fn new(pos: Fix2D) -> Self {
        Self {
            position: pos,
            animation_frame: 0,
        }
    }

    fn bounding_rect(&self) -> Rect<i32> {
        Rect::new(self.position.round() + vec2(4, 4), vec2(8, 8))
    }

    fn rng(&self) -> i32 {
        rng::next_i32()
    }
}

impl EntityData for Enemy {
    fn update(script: &mut Script<Self>, player_rect: Rect<i32>) {
        if script.properties.bounding_rect().touches(player_rect) {
            script.on_hit();
        }
    }

    fn show(&self, frame: &mut GraphicsFrame, camera: Vector2D<i32>) {
        Object::new(sprites::ENEMY.sprite(self.animation_frame as usize))
            .set_pos(self.position.round() + camera)
            .show(frame);
    }
}
