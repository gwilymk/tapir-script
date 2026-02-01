// Games made using `agb` are no_std which means you don't have access to the standard
// rust library. This is because the game boy advance doesn't have an operating
// system, so most of the content of the standard library doesn't apply.
#![no_std]
// `agb` defines its own `main` function, so you must declare your game's main function
// using the #[agb::entry] proc macro. Failing to do so will cause failure in linking
// which won't be a particularly clear error message.
#![no_main]
// This is required to allow writing tests
#![cfg_attr(test, feature(custom_test_frameworks))]
#![cfg_attr(test, reexport_test_harness_main = "test_main")]
#![cfg_attr(test, test_runner(agb::test_runner::test_runner))]

pub(crate) mod declarative_enum_dispatch;
mod entities;

use agb::{
    display::{GraphicsFrame, HEIGHT, Rgb15, WIDTH, object::Object, tiled::VRAM_MANAGER},
    fixnum::{Rect, Vector2D, num, vec2},
    include_aseprite,
    input::ButtonController,
    rng,
};
use alloc::vec::Vec;
use tapir_script::{Fix, Script, TapirScript};

use crate::entities::{Collectable, Enemy, Entity, EntityTrait, Particle};

type Fix2D = Vector2D<Fix>;

// By default no_std crates don't get alloc, so you won't be able to use things like Vec
// until you declare the extern crate. `agb` provides an allocator so it will all work
extern crate alloc;

include_aseprite!(mod sprites, "gfx/sprites.aseprite");

// The main function must take 1 arguments and never returns, and must be marked with
// the #[agb::entry] macro.
#[agb::entry]
fn entry(gba: agb::Gba) -> ! {
    main(gba);
}

fn main(mut gba: agb::Gba) -> ! {
    VRAM_MANAGER.set_background_palette_colour(0, 0, Rgb15::WHITE);

    let mut player = Player::new();
    let mut input = ButtonController::new();

    let mut gfx = gba.graphics.get();

    let mut entities: Vec<Entity> = Vec::new();
    entities.push(Enemy::new(vec2(num!(30.), num!(50.))).into());

    let mut screen_shaker = ScreenShaker { amount: vec2(0, 0) }.script();

    loop {
        screen_shaker.run();

        input.update();

        let collectable_count = entities
            .iter()
            .filter(|e| matches!(e, Entity::Collectable(_)))
            .count();
        if rng::next_i32().rem_euclid(60) < 5 - collectable_count as i32 {
            entities.push(
                Collectable::new(vec2(
                    rng::next_i32().rem_euclid(WIDTH - 16).into(),
                    rng::next_i32().rem_euclid(HEIGHT - 16).into(),
                ))
                .into(),
            );
        }

        player.update(&input);
        let player_rect = player.bounding_rect();

        let mut new_entities: Vec<Entity> = Vec::new();
        entities.retain_mut(|entity| {
            let mut keep = true;
            for event in entity.update(player_rect) {
                match event {
                    AnimationEvent::DestroySelf => keep = false,
                    AnimationEvent::SpawnParticle(x, y, _kind) => {
                        new_entities.push(Particle::new(vec2(x, y)).into());
                    }
                    AnimationEvent::IncreaseScore => {}
                    AnimationEvent::ScreenShake => {
                        screen_shaker.on_shake();
                    }
                    AnimationEvent::HurtPlayer => {
                        player.player_animation.on_hurt();
                    }
                }
            }
            keep
        });
        entities.append(&mut new_entities);

        let mut frame = gfx.frame();
        player.show(&mut frame, screen_shaker.properties.amount);

        for entity in &entities {
            entity.show(&mut frame, screen_shaker.properties.amount);
        }

        frame.commit();
    }
}

pub enum AnimationEvent {
    DestroySelf,
    SpawnParticle(i32, i32, i32),
    IncreaseScore,
    ScreenShake,
    HurtPlayer,
}

struct Player {
    player_animation: Script<PlayerAnimation>,
}

impl Player {
    pub fn new() -> Self {
        Self {
            player_animation: PlayerAnimation {
                anim_frame: 0,
                position: vec2(num!(100), num!(100)),
            }
            .script(),
        }
    }

    pub fn update(&mut self, input: &ButtonController) {
        let vector = input.vector();
        self.player_animation.on_input(vector.x, vector.y);
        self.player_animation.run();
    }

    pub fn bounding_rect(&self) -> Rect<i32> {
        Rect::new(
            self.player_animation.properties.position.round(),
            vec2(16, 16),
        )
    }

    pub fn show(&self, frame: &mut GraphicsFrame, screen_shake: Vector2D<i32>) {
        let properties = &self.player_animation.properties;

        Object::new(sprites::PLAYER.animation_sprite(properties.anim_frame as usize))
            .set_pos(properties.position.round() + screen_shake)
            .show(frame);
    }
}

#[derive(TapirScript, Debug)]
#[tapir("tapir/player.tapir", trigger_type = AnimationEvent)]
struct PlayerAnimation {
    anim_frame: i32,
    position: Fix2D,
}

#[derive(TapirScript)]
#[tapir("tapir/screen_shaker.tapir")]
struct ScreenShaker {
    amount: Vector2D<i32>,
}

impl ScreenShaker {
    fn rng(&self) -> i32 {
        rng::next_i32()
    }
}
