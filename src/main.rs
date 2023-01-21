use std::collections::HashMap;
use std::path::Path;
use std::time::{Duration, Instant};

use color_eyre::eyre::eyre;
use screenbuffer::ScreenBuffer;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::WindowCanvas;

use crate::vm::ExecutionResult;

pub mod screenbuffer;
pub mod vm;

// Window configuration:
const PX_SCALING: u32 = 20;
const WINDOW_HEIGHT: u32 = screenbuffer::ROWS * PX_SCALING;
const WINDOW_WIDTH: u32 = screenbuffer::COLS * PX_SCALING;
const WINDOW_TITLE: &'static str = "CHIP8 Emulator";

// Color configuration
const COLOR_FOREGROUND: Color = Color::RGB(255, 255, 255);
const COLOR_BACKGROUND: Color = Color::RGB(0, 0, 0);

// Timing config:
const CLOCK_SPEED: u64 = 540;
const FRAME_RATE: u64 = 60;
const FRAME_DURATION: Duration = Duration::from_millis(1000 / FRAME_RATE);

fn load_program_from_file<P: AsRef<Path>>(path: P) -> color_eyre::Result<Vec<u8>> {
    let program = std::fs::read(path)?;
    Ok(program)
}

fn main() -> color_eyre::Result<()> {
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

    canvas.set_draw_color(COLOR_BACKGROUND);
    canvas.clear();
    canvas.present();

    let program = load_program_from_file(String::from("./games/IBM.ch8"))?;
    let mut vm = vm::VM::new(program.as_slice());

    let mut event_pump = sdl_context
        .event_pump()
        .map_err(|e| eyre!("Could not create event pump: {}", e))?;

    // TODO: write a real key map
    let mut keymap: HashMap<u8, Keycode> = HashMap::with_capacity(16);
    keymap.insert(0, Keycode::Q);

    let mut frame_start = Instant::now();
    'running: loop {
        // Process all the events for the current frame:
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
                // TODO: handle keydown and keyup events so we can set the
                // appropriate values in the VM
                _ => {}
            }
        }

        // Render a frame, passing in the number of instructions that should be
        // executed per frame:
        let exe_result = vm.render_frame(CLOCK_SPEED / FRAME_RATE)?;

        // FIXME: this is the ugliest fucking shit I have ever seen in my life
        // please don't write code this way
        match exe_result {
            ExecutionResult::WaitForKey(wait_for_key) => {
                'waiting_for_key: loop {
                    for event in event_pump.poll_iter() {
                        match event {
                            Event::Quit { .. } => break 'running,
                            Event::KeyDown { keycode, .. } => {
                                if let Some(key) = keycode {
                                    // TODO: handle errors correctly
                                    if key == *keymap.get(&wait_for_key).unwrap_or(&Keycode::Q) {
                                        // TODO: set the key in the VM
                                        break 'waiting_for_key;
                                    }
                                    continue 'waiting_for_key;
                                }
                            }
                            _ => {}
                        }
                    }
                }

                continue 'running;
            }
            _ => {
                draw_to_buffer(vm.screenbuffer(), &mut canvas)?;
                canvas.present();
            }
        }

        // Sleep for however long is left in the frame:
        let frame_end = Instant::now();
        let elapsed = frame_end.duration_since(frame_start);
        // dbg!("{:?} {:?}", elapsed, FRAME_DURATION);
        if elapsed < FRAME_DURATION {
            std::thread::sleep(FRAME_DURATION - elapsed);
        }
        frame_start = Instant::now();
    }

    Ok(())
}

fn draw_to_buffer(
    screenbuffer: &ScreenBuffer,
    canvas: &mut WindowCanvas,
) -> color_eyre::Result<()> {
    for row in 0..screenbuffer::ROWS {
        for col in 0..screenbuffer::COLS {
            let pixel = screenbuffer.read_pixel(col as usize, row as usize);

            let foreground_color = match pixel {
                true => COLOR_FOREGROUND,
                false => COLOR_BACKGROUND,
            };

            let x = (col * PX_SCALING) as i32;
            let y = (row * PX_SCALING) as i32;
            let rect = Rect::new(x, y, PX_SCALING, PX_SCALING);

            canvas.set_draw_color(foreground_color);
            canvas
                .fill_rect(rect)
                .map_err(|e| eyre!("could not draw rect {:?}: {}", rect, e))?;
            canvas.set_draw_color(COLOR_BACKGROUND);
            canvas
                .draw_rect(rect)
                .map_err(|e| eyre!("could not draw rect {:?}: {}", rect, e))?;
        }
    }

    Ok(())
}
