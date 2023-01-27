use std::path::Path;
use std::time::{Duration, Instant};

use graphics::{Chip8Event, GraphicsContext};
use vm::ExecutionResult;

pub mod graphics;
pub mod keymap;
pub mod screenbuffer;
pub mod vm;

// Timing config:
const CLOCK_SPEED: u64 = 540;
const FRAME_RATE: u64 = 60;
const FRAME_DURATION: Duration = Duration::from_millis(1000 / FRAME_RATE);

fn load_program_from_file<P: AsRef<Path>>(path: P) -> color_eyre::Result<Vec<u8>> {
    let program = std::fs::read(path)?;
    Ok(program)
}

fn exit_with_help() {
    let help = "\
    Usage: chip8 <program>
    ";
    eprintln!("{}", help);
    std::process::exit(1);
}

fn main() -> color_eyre::Result<()> {
    // Load program from cmd line arguments specifying file
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        exit_with_help();
    }

    let program = load_program_from_file(&args[1])?;

    // Initialize application state
    let mut vm = vm::VM::new(program.as_slice());
    let mut graphics_context = GraphicsContext::new()?;
    let mut frame_start = Instant::now();

    // Separating instructions per frame from instructions to execute on a given
    // iteration of the game loop allows us to maintain a fairly constant clock
    // speed no matter when the VM calls `draw`
    let instructions_per_frame = CLOCK_SPEED / FRAME_RATE;
    let mut instructions_to_execute = instructions_per_frame;
    'running: loop {
        // Process all the events for the current frame:
        for event in graphics_context.event_iter()? {
            match event {
                Chip8Event::Quit => break 'running,
                Chip8Event::KeyDown(keycode) => vm.on_keydown(keycode),
                Chip8Event::KeyUp(keycode) => vm.on_keyup(keycode),
            }
        }

        // TODO:  handle decrementing the sound & delay timers
        let (exe_result, exe_count) = vm.render_frame(instructions_to_execute)?;
        match exe_result {
            ExecutionResult::WaitForKey(register) => {
                let keycode = graphics_context.wait_for_key();
                vm.on_keydown(keycode);
                vm.wait_for_key_complete(register, keycode);
                instructions_to_execute = instructions_per_frame;
            }
            ExecutionResult::Draw => {
                graphics_context.draw(vm.screenbuffer())?;
                let instructions_remaining = instructions_to_execute - exe_count;
                if instructions_remaining > 0 {
                    instructions_to_execute = instructions_remaining;
                } else {
                    instructions_to_execute = instructions_per_frame;
                }
            }
            ExecutionResult::Done => {
                instructions_to_execute = instructions_per_frame;
                sleep(&mut frame_start);
            }
        }
    }

    Ok(())
}

fn sleep(frame_start: &mut Instant) {
    // Sleep for however long is left in the frame:
    let frame_end = Instant::now();
    let elapsed = frame_end.duration_since(*frame_start);
    if elapsed < FRAME_DURATION {
        std::thread::sleep(FRAME_DURATION - elapsed);
    }

    *frame_start = Instant::now()
}
