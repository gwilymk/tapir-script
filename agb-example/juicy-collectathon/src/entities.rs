use crate::{Fix2D, enum_dispatch, sprites};

use agb::{
    display::{GraphicsFrame, object::Object},
    fixnum::{Rect, Vector2D, vec2},
    rng,
};
use alloc::vec::Vec;
use tapir_script::{Script, TapirScript};

use crate::AnimationEvent;

enum_dispatch!(
    pub trait EntityTrait {
        fn update(&mut self, player_rect: Rect<i32>) -> Vec<AnimationEvent>;
        fn show(&self, frame: &mut GraphicsFrame, camera: Vector2D<i32>);
        fn is_collectable(&self) -> bool {
            false
        }
    }

    pub enum Entity {
        Collectable(Collectable),
        Particle(Particle),
    }
);

pub struct Collectable {
    collectable_animation: Script<CollectableAnimation>,
}

impl Collectable {
    pub fn new(pos: Fix2D) -> Self {
        Self {
            collectable_animation: CollectableAnimation {
                position: pos,
                anim_frame: 0,
                should_show: true,
            }
            .script(),
        }
    }

    fn bounding_rect(&self) -> Rect<i32> {
        Rect::new(
            self.collectable_animation.properties.position.round(),
            vec2(16, 16),
        )
    }
}

impl EntityTrait for Collectable {
    fn update(&mut self, player_rect: Rect<i32>) -> Vec<AnimationEvent> {
        if self.bounding_rect().touches(player_rect) {
            self.collectable_animation.on_collide_with_player();
        }

        self.collectable_animation.run()
    }

    fn show(&self, frame: &mut GraphicsFrame, screen_shake: Vector2D<i32>) {
        let properties = &self.collectable_animation.properties;

        if properties.should_show {
            Object::new(sprites::COIN.animation_sprite(properties.anim_frame as usize))
                .set_pos(properties.position.round() + screen_shake)
                .show(frame);
        }
    }

    fn is_collectable(&self) -> bool {
        true
    }
}

#[derive(TapirScript)]
#[tapir("tapir/collectable.tapir", trigger_type = AnimationEvent)]
struct CollectableAnimation {
    position: Fix2D,
    anim_frame: i32,
    should_show: bool,
}

pub struct Particle {
    particle_animation: Script<ParticleAnimation>,
}

#[derive(TapirScript)]
#[tapir("tapir/particle.tapir", trigger_type = AnimationEvent)]
struct ParticleAnimation {
    position: Fix2D,
    lifetime: i32,
}

impl ParticleAnimation {
    fn rng(&self) -> i32 {
        rng::next_i32()
    }
}

impl Particle {
    pub fn new(position: Vector2D<i32>) -> Self {
        Self {
            particle_animation: ParticleAnimation {
                position: position.into(),
                lifetime: rng::next_i32().rem_euclid(30) + 30,
            }
            .script(),
        }
    }
}

impl EntityTrait for Particle {
    fn update(&mut self, _player_pos: Rect<i32>) -> Vec<AnimationEvent> {
        self.particle_animation.run()
    }

    fn show(&self, frame: &mut GraphicsFrame, screen_shake: Vector2D<i32>) {
        Object::new(sprites::COLLECT_PARTICLE.sprite(0))
            .set_pos(self.particle_animation.properties.position.round() + screen_shake)
            .show(frame);
    }
}
