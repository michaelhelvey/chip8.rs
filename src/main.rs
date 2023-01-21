use std::path::Path;
use std::time::{Duration, Instant};

use color_eyre::eyre::eyre;
use screenbuffer::ScreenBuffer;
use sdl2::event::Event;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::WindowCanvas;
use vm::VM;

pub mod screenbuffer;
pub mod vm;

const ROWS: u32 = 32;
const COLS: u32 = 64;

// Window configuration:
const PX_SCALING: u32 = 20;
const WINDOW_HEIGHT: u32 = ROWS * PX_SCALING;
const WINDOW_WIDTH: u32 = 64 * PX_SCALING;
const WINDOW_TITLE: &'static str = "CHIP8 Emulator";

// Color configuration
const COLOR_FOREGROUND: Color = Color::RGB(255, 255, 255);
const COLOR_BACKGROUND: Color = Color::RGB(0, 0, 50);

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
    let vm = VM::acquire();
    vm.load_program(program.as_slice());

    let screenbuffer = ScreenBuffer::acquire();

    let mut event_pump = sdl_context
        .event_pump()
        .map_err(|e| eyre!("Could not create event pump: {}", e))?;

    let mut frame_start = Instant::now();
    'running: loop {
        // Process all the events for the current frame:
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
                // TODO: pass some subset of keyboard events into a virtual
                // machine to set registers
                _ => {}
            }
        }

        // Render a frame, passing in the number of instructions that should be
        // executed per frame:
        vm.render_frame(CLOCK_SPEED / FRAME_RATE);
        draw_to_buffer(screenbuffer, &mut canvas)?;
        canvas.present();

        // Sleep for however long is left in the frame:
        let frame_end = Instant::now();
        let elapsed = frame_end.duration_since(frame_start);
        dbg!("elapsed: {:?}, duration: {:?}", elapsed, FRAME_DURATION);
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
    for row in 0..ROWS {
        for col in 0..COLS {
            let pixel = screenbuffer.buffer[(row * col) as usize];

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
