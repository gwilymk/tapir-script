use crate::enum_dispatch;

use agb::{
    display::GraphicsFrame,
    fixnum::{Rect, Vector2D},
};
use alloc::vec::Vec;
use tapir_script::{Script, TapirScript};

use crate::AnimationEvent;

mod coin_to_health;
mod collectable;
mod enemy;
mod particle;

pub use coin_to_health::CoinToHealth;
pub use collectable::{Collectable, CollectableEvents};
pub use enemy::Enemy;
pub use particle::Particle;

enum_dispatch!(
    pub trait EntityTrait {
        fn z(&self) -> i32;
        fn update(&mut self, player_rect: Rect<i32>) -> Vec<AnimationEvent>;
        fn show(&self, frame: &mut GraphicsFrame, camera: Vector2D<i32>);
    }

    pub enum Entity {
        Collectable(Script<Collectable>),
        CoinToHealth(Script<CoinToHealth>),
        Particle(Script<Particle>),
        Enemy(Script<Enemy>),
    }
);

pub trait EntityData: TapirScript<EventType = AnimationEvent> + Sized {
    fn z(&self) -> i32 {
        0
    }
    fn update(_script: &mut Script<Self>, _player_rect: Rect<i32>) {}
    fn show(&self, frame: &mut GraphicsFrame, camera: Vector2D<i32>);
}

impl<T> EntityTrait for Script<T>
where
    T: EntityData,
{
    fn z(&self) -> i32 {
        self.properties.z()
    }

    fn update(&mut self, player_rect: Rect<i32>) -> Vec<AnimationEvent> {
        T::update(self, player_rect);
        self.run()
    }

    fn show(&self, frame: &mut GraphicsFrame, camera: Vector2D<i32>) {
        self.properties.show(frame, camera);
    }
}

impl<T> From<T> for Entity
where
    Script<T>: EntityTrait,
    Entity: From<Script<T>>,
    T: EntityData,
{
    fn from(value: T) -> Self {
        Entity::from(value.script())
    }
}
