use agb::{
    display::{GraphicsFrame, object::Object},
    fixnum::{Rect, Vector2D, vec2},
};
use tapir_script::{Script, TapirScript};

use crate::{AnimationEvent, Fix2D, entities::EntityData, sprites};

#[derive(TapirScript)]
#[tapir("tapir/collectable.tapir", trigger_type = AnimationEvent)]
pub struct Collectable {
    position: Fix2D,
    anim_frame: i32,
    should_show: bool,
}

impl Collectable {
    pub fn new(pos: Fix2D) -> Self {
        Self {
            position: pos,
            anim_frame: 0,
            should_show: true,
        }
    }

    fn bounding_rect(&self) -> Rect<i32> {
        Rect::new(self.position.round(), vec2(16, 16))
    }
}

impl EntityData for Collectable {
    fn update(script: &mut Script<Self>, player_rect: Rect<i32>) {
        if script.properties.bounding_rect().touches(player_rect) {
            script.on_collide_with_player();
        }
    }

    fn show(&self, frame: &mut GraphicsFrame, screen_shake: Vector2D<i32>) {
        if self.should_show {
            Object::new(sprites::COIN.animation_sprite(self.anim_frame as usize))
                .set_pos(self.position.round() + screen_shake)
                .show(frame);
        }
    }
}
