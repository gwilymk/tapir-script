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

    let mut player_health = 10;
    let mut score = 0;

    let mut player = Player::new();
    let mut health_bar = HealthBar::new(player_health);
    let mut input = ButtonController::new();

    let mut gfx = gba.graphics.get();

    let mut entities: Vec<Entity> = Vec::new();

    let mut screen_shaker = ScreenShaker { amount: vec2(0, 0) }.script();

    loop {
        screen_shaker.run();

        input.update();

        let player_pos = player.player_animation.properties.position;

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

        // Spawn enemies randomly, away from the player
        let enemy_count = entities
            .iter()
            .filter(|e| matches!(e, Entity::Enemy(_)))
            .count();
        if rng::next_i32().rem_euclid(120) < 3 - enemy_count as i32 {
            // Generate a random spawn position
            let spawn_pos: Fix2D = vec2(
                rng::next_i32().rem_euclid(WIDTH - 16).into(),
                rng::next_i32().rem_euclid(HEIGHT - 16).into(),
            );
            let diff = spawn_pos - player_pos;
            if diff.magnitude_squared() > num!(50 * 50) {
                entities.push(Enemy::new(spawn_pos).into());
            }
        }

        for event in player.update(&input) {
            if matches!(event, AnimationEvent::PlayerDamaged) {
                player_health -= 1;
            }
        }
        let player_rect = player.bounding_rect();

        let mut new_entities: Vec<Entity> = Vec::new();
        entities.retain_mut(|entity| {
            let mut keep = true;
            for event in entity.update(player_rect) {
                match event {
                    AnimationEvent::DestroySelf => keep = false,
                    AnimationEvent::SpawnParticle(x, y, kind) => {
                        new_entities.push(Particle::new(vec2(x, y), kind).into());
                    }
                    AnimationEvent::IncreaseScore => {
                        score += 1;
                        if score % 10 == 0 && player_health < 10 {
                            player_health += 1;
                        }
                    }
                    AnimationEvent::ScreenShake(intensity) => {
                        screen_shaker.on_shake(intensity);
                    }
                    AnimationEvent::HurtPlayer => {
                        player.player_animation.on_hurt();
                    }
                    AnimationEvent::PlayerDamaged => {}
                }
            }
            keep
        });
        entities.append(&mut new_entities);
        health_bar.update(player_health);

        let mut frame = gfx.frame();
        let screen_shake = screen_shaker.properties.amount;

        health_bar.show(&mut frame, screen_shake);
        player.show(&mut frame, screen_shake);

        for entity in &entities {
            entity.show(&mut frame, screen_shake);
        }

        frame.commit();
    }
}

pub enum AnimationEvent {
    DestroySelf,
    SpawnParticle(i32, i32, i32),
    IncreaseScore,
    ScreenShake(i32),
    HurtPlayer,
    PlayerDamaged,
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

    pub fn update(&mut self, input: &ButtonController) -> Vec<AnimationEvent> {
        let vector = input.vector();
        self.player_animation.on_input(vector.x, vector.y);
        self.player_animation.run()
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

struct HealthBar {
    animation: Script<HealthBarAnimation>,
}

#[derive(TapirScript)]
#[tapir("tapir/health.tapir")]
struct HealthBarAnimation {
    health: i32,
    health_frame: i32,
    health_offset: i32,
}

impl HealthBar {
    pub fn new(player_heath: i32) -> Self {
        Self {
            animation: HealthBarAnimation {
                health: player_heath,
                health_frame: 0,
                health_offset: 0,
            }
            .script(),
        }
    }

    pub fn update(&mut self, player_heath: i32) {
        self.animation.properties.health = player_heath;
        self.animation.run();
    }

    pub fn show(&self, frame: &mut GraphicsFrame, _screen_shake: Vector2D<i32>) {
        let properties = &self.animation.properties;
        let display_health = properties.health - properties.health_offset;

        for i in 0..display_health {
            Object::new(sprites::HEALTH.sprite(0))
                .set_pos(vec2(WIDTH / 2 + i * 7, 1))
                .show(frame);
        }

        if properties.health_frame != 0 {
            Object::new(sprites::HEALTH.sprite(properties.health_frame as usize))
                .set_pos(vec2(WIDTH / 2 + display_health * 7, 1))
                .show(frame);
        }
    }
}
