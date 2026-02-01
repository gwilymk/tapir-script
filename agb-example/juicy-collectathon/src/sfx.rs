use agb::{
    fixnum::num,
    include_wav,
    sound::mixer::{Mixer, SoundChannel, SoundData},
};
use tapir_script::Fix;

static PICKUP: SoundData = include_wav!("sfx/pickup.wav");
static HEALTH_UP: SoundData = include_wav!("sfx/health_up.wav");
static ENEMY_DIE: SoundData = include_wav!("sfx/enemy_die.wav");
static ENEMY_DYING: SoundData = include_wav!("sfx/enemy_dying.wav");
static HIT: SoundData = include_wav!("sfx/hit.wav");
static COLLECTABLE_EXPLODE: SoundData = include_wav!("sfx/collectable_explode.wav");
static ENEMY_CHANGE_DIRECTION: SoundData = include_wav!("sfx/enemy_change_direction.wav");

pub fn play_sfx(mixer: &mut Mixer, sf_id: i32, pitch: Fix) {
    let mut channel = match sf_id {
        0 => SoundChannel::new(PICKUP),
        1 => SoundChannel::new(HEALTH_UP),
        2 => SoundChannel::new(ENEMY_DIE),
        3 => SoundChannel::new(ENEMY_DYING),
        4 => SoundChannel::new(HIT),
        5 => SoundChannel::new(COLLECTABLE_EXPLODE),
        6 => SoundChannel::new(ENEMY_CHANGE_DIRECTION),
        _ => return,
    };

    channel.playback(pitch.try_change_base().unwrap_or(num!(1)));
    mixer.play_sound(channel);
}
