/// The keymap module contains the logic for storing and accessing keyboard
/// state.  It maps SDL events to "virtual" chip8 key presses based on a
/// keymap, and exposes mechanisms for storing key press state.
use lazy_static::lazy_static;
use num_derive::{FromPrimitive, ToPrimitive};
use sdl2::keyboard::Keycode as SdlKeycode;
use std::collections::HashMap;

/// Represents the keys of a native Chip8 keyboard
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy, FromPrimitive, ToPrimitive)]
pub enum Chip8Key {
    K0 = 0,
    K1,
    K2,
    K3,
    K4,
    K5,
    K6,
    K7,
    K8,
    K9,
    KA,
    KB,
    KC,
    KD,
    KE,
    KF,
}

lazy_static! {
    /// Maps SDL key codes to virutal chip8 keys.
    /// See: http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#keyboard
    static ref KEYMAP: HashMap<SdlKeycode, Chip8Key> = {
        let mut m = HashMap::with_capacity(0xF);
        m.insert(SdlKeycode::Num1, Chip8Key::K1);
        m.insert(SdlKeycode::Num2, Chip8Key::K2);
        m.insert(SdlKeycode::Num3, Chip8Key::K3);
        m.insert(SdlKeycode::Num4, Chip8Key::KC);

        m.insert(SdlKeycode::Q, Chip8Key::K4);
        m.insert(SdlKeycode::W, Chip8Key::K5);
        m.insert(SdlKeycode::E, Chip8Key::K6);
        m.insert(SdlKeycode::R, Chip8Key::KD);

        m.insert(SdlKeycode::A, Chip8Key::K7);
        m.insert(SdlKeycode::S, Chip8Key::K8);
        m.insert(SdlKeycode::D, Chip8Key::K9);
        m.insert(SdlKeycode::F, Chip8Key::KE);

        m.insert(SdlKeycode::Z, Chip8Key::KA);
        m.insert(SdlKeycode::X, Chip8Key::K0);
        m.insert(SdlKeycode::C, Chip8Key::KB);
        m.insert(SdlKeycode::V, Chip8Key::KF);
        m
    };
}

/// Given an SDL keycode, returns a chip8 keycode (if it exists) from the default keymap
pub fn chip8_value_for_sdl_keycode(sdl_keycode: SdlKeycode) -> Option<Chip8Key> {
    return KEYMAP.get(&sdl_keycode).map(|val| *val);
}

#[derive(Debug)]
pub struct KeyboardState {
    map: HashMap<Chip8Key, bool>,
}

impl KeyboardState {
    pub fn new() -> Self {
        Self {
            map: HashMap::with_capacity(0xF),
        }
    }

    pub fn on_keyup(&mut self, keycode: Chip8Key) {
        self.map.insert(keycode, false);
    }

    pub fn on_keydown(&mut self, keycode: Chip8Key) {
        self.map.insert(keycode, true);
    }

    pub fn is_key_pressed(&self, keycode: Chip8Key) -> bool {
        return self.map.get(&keycode).is_some();
    }
}
