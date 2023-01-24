use color_eyre::{eyre::eyre, Result};
use sdl2::event as sdl_event;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::WindowCanvas;

use crate::keymap::chip8_value_for_sdl_keycode;
use crate::{
    keymap::Chip8Key,
    screenbuffer::{ScreenBuffer, COLS, ROWS},
};

// Window configuration:
const PX_SCALING: u32 = 20;
const WINDOW_HEIGHT: u32 = ROWS * PX_SCALING;
const WINDOW_WIDTH: u32 = COLS * PX_SCALING;
const WINDOW_TITLE: &'static str = "CHIP8 Emulator";

// Color configuration
const COLOR_FOREGROUND: Color = Color::RGB(255, 255, 255);
const COLOR_BACKGROUND: Color = Color::RGB(0, 0, 0);

/// A subset of SDL events that we care about handling
#[derive(Debug)]
pub enum Chip8Event {
    KeyUp(Chip8Key),
    KeyDown(Chip8Key),
    Quit,
}

pub struct GraphicsContext {
    event_pump: sdl2::EventPump,
    canvas: WindowCanvas,
}

impl GraphicsContext {
    pub fn new() -> Result<Self> {
        let sdl_context = sdl2::init().map_err(|e| eyre!("Could not initialize SDL: {}", e))?;
        let video_subsystem = sdl_context
            .video()
            .map_err(|e| eyre!("Could not init video subsystem: {}", e))?;

        let window = video_subsystem
            .window(WINDOW_TITLE, WINDOW_WIDTH, WINDOW_HEIGHT)
            .position_centered()
            .build()?;

        let mut canvas = window
            .into_canvas()
            .build()
            .map_err(|e| eyre!("Could not initialize canvas: {}", e))?;

        let event_pump = sdl_context
            .event_pump()
            .map_err(|e| eyre!("could not create SDL event pump: {}", e))?;

        canvas.set_draw_color(COLOR_BACKGROUND);
        canvas.clear();
        canvas.present();

        Ok(Self { canvas, event_pump })
    }

    /// Draws a screenbuffer onto the current graphics context.
    pub fn draw(&mut self, screenbuffer: &ScreenBuffer) -> Result<()> {
        for row in 0..ROWS {
            for col in 0..COLS {
                let pixel = screenbuffer.read_pixel(col as usize, row as usize);

                let foreground_color = match pixel {
                    true => COLOR_FOREGROUND,
                    false => COLOR_BACKGROUND,
                };

                let x = (col * PX_SCALING) as i32;
                let y = (row * PX_SCALING) as i32;
                let rect = Rect::new(x, y, PX_SCALING, PX_SCALING);

                self.canvas.set_draw_color(foreground_color);
                self.canvas
                    .fill_rect(rect)
                    .map_err(|e| eyre!("could not draw rect {:?}: {}", rect, e))?;
                self.canvas.set_draw_color(COLOR_BACKGROUND);
                self.canvas
                    .draw_rect(rect)
                    .map_err(|e| eyre!("could not draw rect {:?}: {}", rect, e))?;
            }
        }

        self.canvas.present();
        Ok(())
    }

    pub fn event_iter(&mut self) -> Result<GraphicsEventIterator> {
        Ok(GraphicsEventIterator::new(&mut self.event_pump))
    }

    /// Pause execution of the program until a keydown event occurs
    pub fn wait_for_key(&mut self) -> Chip8Key {
        loop {
            let event = self.event_pump.wait_event();
            match event {
                sdl_event::Event::KeyDown { keycode, .. } => {
                    if let Some(chip_8_key) =
                        keycode.and_then(|sdl_key| chip8_value_for_sdl_keycode(sdl_key))
                    {
                        return chip_8_key;
                    }
                }
                _ => {}
            }
        }
    }
}

/// Iterates over SDL events & trims them down to a subset that we care to
/// handle in the game loop
pub struct GraphicsEventIterator<'a> {
    event_pump: &'a mut sdl2::EventPump,
}

impl<'a> GraphicsEventIterator<'a> {
    fn new(event_pump: &'a mut sdl2::EventPump) -> Self {
        Self { event_pump }
    }
}

impl<'a> Iterator for GraphicsEventIterator<'a> {
    type Item = Chip8Event;

    fn next(&mut self) -> Option<Self::Item> {
        match self.event_pump.poll_event() {
            Some(sdl_event::Event::Quit { .. }) => Some(Chip8Event::Quit),

            Some(sdl_event::Event::KeyDown { keycode, .. }) => keycode
                .and_then(|sdl_keycode| chip8_value_for_sdl_keycode(sdl_keycode))
                .and_then(|chip_8_key| Some(Chip8Event::KeyDown(chip_8_key))),

            Some(sdl_event::Event::KeyUp { keycode, .. }) => keycode
                .and_then(|sdl_keycode| chip8_value_for_sdl_keycode(sdl_keycode))
                .and_then(|chip_8_key| Some(Chip8Event::KeyUp(chip_8_key))),
            _ => None,
        }
    }
}
