#![allow(dead_code)]

pub struct VM {
    registers: [u8; 16],
    index_register: u16,
    delay_timer: u8,
    sound_timer: u8,
    program_counter: u16,
    stack_pointer: u8,
    // Stack of return addresses for subroutines...not really a traditional
    // "stack" from modern asm programming.
    stack: [u16; 16],
    memory: [u8; 0xFFF],
}

const SPRITE_COUNT: u8 = 16;
const SPRITE_BYTES_SIZE: usize = 5 * SPRITE_COUNT as usize;

const FONT_SPRITES: [u8; SPRITE_BYTES_SIZE] = [
    0x90, 0x90, 0xF0, 0x10, 0x10, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

// If the Chip8 emulator can have mutable static memory, so can I, silly Rust
// compiler!
static mut VM_INSTANCE: VM = VM::new(false);

impl VM {
    const fn new(eti_660_mode: bool) -> Self {
        // The ETI 660 computer was unique in starting programs at 0x600 in
        // memory compared to everything else, which started programs at 0x200
        let program_counter_start = match eti_660_mode {
            true => 0x600,
            false => 0x200,
        };

        let mut instance = Self {
            registers: [0; 16],
            index_register: 0,
            delay_timer: 0,
            sound_timer: 0,
            program_counter: program_counter_start,
            stack_pointer: 0,
            stack: [0; 16],
            memory: [0; 0xFFF],
        };

        // This is the best way that I could find to safely set bytes into
        // memory in a const function.  Rust still seems to have a signficant
        // number of constraints around what's allowed in const functions...e.g.
        // a for loop is not allowed.
        let mut idx = 0;
        loop {
            instance.memory[idx] = FONT_SPRITES[idx];
            idx += 1;

            if idx == FONT_SPRITES.len() {
                break;
            }
        }

        instance
    }

    /// Retrieves the statically allocated VM singleton.  There is no way to
    /// destroy and re-create a VM in the lifetime of the application, but a
    /// program can be loaded dynamically via `load_program`.
    pub fn acquire() -> &'static mut Self {
        // This is a single threaded program, shut the fuck up, Rust.
        unsafe {
            &mut VM_INSTANCE
        }
    }

    /// Loads a program into the memory of the VM starting at the current
    /// program counter.  Does not reset the state of VM or perform any other
    /// logic, simply writes the bytes of the program into memory.  This
    /// function should only be called once per program execution.  Attempting
    /// to call it multiple times is undefined behavior.  Attempting to call it
    /// after changing the program counter will result in garbage instructions
    /// in the memory of the VM.  Attempting to load a program larger than
    /// available memory of the VM will panic.
    pub fn load_program(&mut self, program: &[u8]) {
        let available_memory = self.memory.len() - self.program_counter as usize;
        if program.len() > available_memory {
            panic!(
                "VM#load_program: attempted to load program with size {} at memory address {:x}.  Available memory after program counter: {:x} bytes", 
                program.len(), 
                self.program_counter, 
                available_memory
            );
        }
        for (idx, byte) in program.iter().enumerate() {
            self.memory[self.program_counter as usize + idx] = *byte;
        }
    }

    /// Execute enough instructions to render a single frame.  This function
    /// should be called at the game's framerate (probably 60hz, as this is the
    /// specified rate for decrementing the timer & delay registers).  The VM is
    /// not responsible for doing this, as the VM doesn't know how to draw
    /// anything.
    ///
    /// Example:
    ///     cpu clock speed     = 540hz
    ///     frame rate          = 60hz
    ///
    /// In this model, a call to render_frame(9) will:
    ///     1) Read and execute 9 (540/60) instructions from the program,
    ///     including writing to a screenbuffer if that's one of the
    ///     instructions.
    ///     2) Decrement the delay & sound timer registers if necessary.
    ///
    /// The caller is responsible for updating the screen and sleeping for an
    /// appropriate amount of time using whatever mechanism it chooses.
    pub fn render_frame(&mut self, instruction_count: u64) {}
}
