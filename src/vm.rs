#![allow(dead_code)]

use color_eyre::eyre::eyre;
use crate::screenbuffer::{ScreenBuffer, ROWS, COLS};

struct VMState {
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
static mut VM_INSTANCE: VMState = VMState::new(false);

impl VMState {
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
        // a for loop is not allowed.  Maybe I'm just a noob but I couldn't find
        // a good function for this in std::mem as (for example) &mut references
        // are not allowed in const functions either.
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
        // SAFTEY: this program is simple and single-threaded, so correct usage
        // of static mutable memory is easy to verify.
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
}

pub enum ExecutionResult {
    WaitForKey(u8),
    None,
}

/// Owns the virtual machine state and screenbuffer.  Since both of those
/// objects are simply static locations in memory, from an encapsulation
/// standpoint I find it useful to not expose those buffers to the rest of the
/// program, and simply expose all operations through this public VM struct.
pub struct VM {
    screenbuffer: &'static mut ScreenBuffer,
    state: &'static mut VMState,
}

impl VM {
    pub fn new(program: &[u8]) -> Self {
        let instance = Self {
            screenbuffer: ScreenBuffer::acquire(),
            state: VMState::acquire(),
        };

        instance.state.load_program(program);
        instance
    }

    pub fn screenbuffer(&self) -> &ScreenBuffer {
        return &self.screenbuffer;
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
    /// 
    /// Note that if the VM requires certain information from the runtime, such
    /// as a keypress, then it may return early with the requirements in the
    /// ExecutionResult.  The caller is responsible for handling this
    /// information in a way that maintains a consistent framerate and
    /// responsiveness.
    pub fn render_frame(&mut self, instruction_count: u64) -> color_eyre::Result<ExecutionResult> {
        for _ in 0..instruction_count {
            let pc = self.state.program_counter as usize;
            if pc + 2 > self.state.memory.len() {
                panic!("Invalid progam: could not read two bytes from program counter (pc = {:#04X})", pc);
            }

            let bytes = &self.state.memory[pc..pc + 2];
            let instruction = Instruction::parse((bytes[0], bytes[1]))?;

            self.execute_instruction(instruction);
        }

        Ok(ExecutionResult::None)
    }

    fn increment_program_counter(&mut self, amount: u16) {
        match self.state.program_counter + amount {
            0xFFF => {
                self.state.program_counter = 0x200;
            },
            new_value => {
                self.state.program_counter = new_value
            }
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) {
        println!("Executing {:?} (pc = {:#04X})", instruction, self.state.program_counter);
        match instruction {
            Instruction::CLS(_) => {
                self.screenbuffer.clear();
                self.increment_program_counter(2);
            }
            Instruction::JMP(raw) => {
                let addr = raw.as_u16() & 0x0FFF;
                self.state.program_counter = addr;
                // We do not need to increment the program counter because this
                // instruction manually sets the program counter
            },
            Instruction::LDIMM(raw) => {
                let register = raw.nibble_at(1);
                self.state.registers[register as usize] = raw.1;
                self.increment_program_counter(2);
            },
            Instruction::ADDIMM(raw) => {
                let register = raw.nibble_at(1) as usize;
                let immediate = raw.1;
                let current_value = self.state.registers[register];
                self.state.registers[register] = current_value + immediate;
                self.increment_program_counter(2);
            },
            Instruction::SEIMM(raw) => {
                let register = raw.nibble_at(1);
                let immediate = raw.1;

                if self.state.registers[register as usize] == immediate {
                    println!("we should skip the instruction");
                    // skip next instruction
                    self.increment_program_counter(4);
                } else {
                    self.increment_program_counter(2);
                }
            },
            Instruction::LDIMMI(raw) => {
                let immediate = raw.as_u16() & 0x0FFF;
                self.state.index_register = immediate;
                self.increment_program_counter(2);
            },
            Instruction::DRW(raw) => {
                let starting_addr = self.state.index_register as usize;
                let sprite_len = raw.nibble_at(3) as usize;

                // Coordinates (wrapped around if larger than screen):
                let vx = self.state.registers[raw.nibble_at(1) as usize];
                let vy = self.state.registers[raw.nibble_at(2) as usize];
                self.state.registers[0xF] = 0;

                // Sprite data, represented as an array of bytes. Each byte is a
                // row of 8 pixels, each pixel is a column.
                let sprite_data = &self.state.memory[starting_addr..starting_addr + sprite_len];

                // For each row (byte) in the sprite:
                for (row, sprite_byte) in sprite_data.iter().enumerate() {
                    // For each column (pixel) from left to right inside the byte:
                    for position in 0..8 {
                        let pixel_value = (sprite_byte & (1 << (7 - position))) > 0;

                        // Render the pixel as an offset from the initial start
                        // (vx, vy), wrapping when that value would be larger
                        // than the screen:
                        let x = (vx + position) as usize % COLS as usize;
                        let y = vy as usize + row % ROWS as usize;
                        if self.screenbuffer.write_pixel(pixel_value, x, y) {
                            self.state.registers[0xF] = 1;
                        }
                    }
                }

                self.increment_program_counter(2);
            }
            _ => println!("    -> Unhandled")
        }
    }
}

#[derive(Clone, Copy)]
struct RawInstruction(u8, u8);

impl RawInstruction {
    fn nibble_at(&self, idx: usize) -> u8 {
        match idx {
            1 => self.0 & 0x0F,
            2 => (self.1 & 0xF0) >> 4,
            3 => self.1 & 0x0F,
            idx => panic!("index out of range {}", idx)
        }
    }

    fn as_u16(&self) -> u16 {
        u16::from_be_bytes([self.0, self.1])
    }
}

impl std::fmt::Display for RawInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:02X}{:02X}", self.0, self.1)
    }
}

impl std::fmt::Debug for RawInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RawInstruction(0x{:02X}{:02X})", self.0, self.1)
    }
}

/// Represents a 2 byte chip8 instruction.  Where an instruction code could be
/// ambiguous, the suffice "IMM" is used to denote an operation with an
/// immediate value, while "REG" is used to denote an operation between two
/// registers.  Finally, "R" is used as a prefix to denote a operation that is
/// the reverse of another operation (e.g. Vy - Vx instead of Vx - Vy)
#[derive(Debug)]
enum Instruction {
    /// 0NNN - Jump to a machine code routine at NNN
    SYS(RawInstruction),
    /// 00E0 - Clear the display
    CLS(RawInstruction),
    /// 00EE - Return from a subroutine
    RET(RawInstruction),
    /// 1NNN - Jump to location NNN
    JMP(RawInstruction),
    /// 2NNN - Call subroute at NNN
    CALL(RawInstruction),
    /// 3xkk - Skip next instruction if Vx = kk
    SEIMM(RawInstruction),
    /// 4xkk - Skip next instruction Vx != kk
    SNEIMM(RawInstruction),
    /// 5xy0 - Skip next instruction if Vx = Vy
    SEREG(RawInstruction),
    /// 6xkk - Load the value kk into register Vx
    LDIMM(RawInstruction),
    /// 7xkk - Set Vx = Vx + kk
    ADDIMM(RawInstruction),
    /// 8xy0 - Set Vx = Vy
    LDREG(RawInstruction),
    /// 8xy1  - Set Vx = Vx OR Vy
    ORREG(RawInstruction),
    /// 8xy2 - Set Vx = Vx AND Vy
    ANDREG(RawInstruction),
    /// 8xy3 - Set Vx = Vx XOR Vy
    XORREG(RawInstruction),
    /// 8xy4 - Set Vx = Vx + Vy, set VF = carry
    ADDREG(RawInstruction),
    /// 8xy5 - Set Vx = Vx - Vy, Set VF = NOT borrow
    SUBREG(RawInstruction),
    /// 8xy6 - Set Vx = Vx SHR 1, VF = whether Vx least sig. bit = 1 
    SHR(RawInstruction),
    /// 8xy7 - Set Vx = Vy - Vx, set VF = NOT borrow
    RSUBREG(RawInstruction),
    /// 8xyE - Set Vx = Vx SHL 1, set VF = whether Vx most sig. bit = 1
    SHL(RawInstruction),
    /// 9xy0 - Skip next instruction if Vx != Vy
    SNEREG(RawInstruction),
    /// ANNN - Set register I equal to NNN
    LDIMMI(RawInstruction),
    /// BNNN - Set program counter to NNN + V0
    JPV0(RawInstruction),
    /// Cxkk - Set a random 8-bit number AND'd with kk into register Vx
    RND(RawInstruction),
    /// Dxyn - Display n-byte sprite starting at I, set VF = collision
    DRW(RawInstruction),
    /// Ex9E - Skip next instruction if key with value of Vx is pressed
    SKP(RawInstruction),
    /// ExA1 - Skip next instruction if key with value of Vx is NOT pressed.
    SKNP(RawInstruction),
    /// Fx07 - Set Vx to delay timer value
    LDDELAY(RawInstruction),
    /// Fx0A - Wait for key press, store the value of key in Vx
    LDKEY(RawInstruction),
    /// Fx15 - Set delay timer to value of Vx
    SETDELAY(RawInstruction),
    /// Fx18 - Set sounter timer = value in Vx
    LDSOUND(RawInstruction),
    /// Fx1E - Set I = Vx + I
    ADDINDEX(RawInstruction),
    /// Fx29 - Set I to the value of the sprite corresponding to Vx
    LDSPRITE(RawInstruction),
    /// Fx33 - Loads hundreds, tens, 1s place of decimal value of Fx into I, I + 1, I + 2
    LDIDECIMAL(RawInstruction),
    /// Fx55 - Store registers V0 through Vx into memory starting at I
    STOREREG(RawInstruction),
    /// Fx65 - Read memory contents into registers V0 through Vx starting at I
    LDMEM(RawInstruction)
}

impl Instruction {
    fn parse(bytes: (u8, u8)) -> color_eyre::Result<Self> {
        let ri = RawInstruction(bytes.0, bytes.1);

        match (bytes.0 & 0xF0) >> 4 {
            0 => match bytes {
                (0x00, 0xE0) => Ok(Instruction::CLS(ri)),
                (0x00, 0xEE) => Ok(Instruction::RET(ri)),
                _ => Ok(Instruction::SYS(ri)),
            }
            1 => Ok(Instruction::JMP(ri)),
            2 => Ok(Instruction::CALL(ri)),
            3 => Ok(Instruction::SEIMM(ri)),
            4 => Ok(Instruction::SNEIMM(ri)),
            5 => Ok(Instruction::SEREG(ri)),
            6 => Ok(Instruction::LDIMM(ri)),
            7 => Ok(Instruction::ADDIMM(ri)),
            8 => match ri.nibble_at(3) {
                0 => Ok(Instruction::LDREG(ri)),
                1 => Ok(Instruction::ORREG(ri)),
                2 => Ok(Instruction::ANDREG(ri)),
                3 => Ok(Instruction::XORREG(ri)),
                4 => Ok(Instruction::ADDREG(ri)),
                5 => Ok(Instruction::SUBREG(ri)),
                6 => Ok(Instruction::SHR(ri)),
                7 => Ok(Instruction::RSUBREG(ri)),
                0xE => Ok(Instruction::SHL(ri)),
                _ => Err(eyre!("Unknown 0x8xxx opcode 0x{:02X}{:02X}", bytes.0, bytes.1))
            },
            9 => Ok(Instruction::SNEREG(ri)),
            0xA => Ok(Instruction::LDIMMI(ri)),
            0xB => Ok(Instruction::JPV0(ri)),
            0xC => Ok(Instruction::RND(ri)),
            0xD => Ok(Instruction::DRW(ri)),
            0xE => match bytes.1 {
                0x9E => Ok(Instruction::SKP(ri)),
                0xA1 => Ok(Instruction::SKNP(ri)),
                _ => Err(eyre!("Unknown 0xEx opcode 0x{:02X}{:02X}", bytes.0, bytes.1))
            },
            0xF => match bytes.1 {
                0x07 => Ok(Instruction::LDDELAY(ri)),
                0x0A => Ok(Instruction::LDKEY(ri)),
                0x15 => Ok(Instruction::SETDELAY(ri)),
                0x18 => Ok(Instruction::LDSOUND(ri)),
                0x1E => Ok(Instruction::ADDINDEX(ri)),
                0x29 => Ok(Instruction::LDSPRITE(ri)),
                0x33 => Ok(Instruction::LDIDECIMAL(ri)),
                0x55 => Ok(Instruction::STOREREG(ri)),
                0x65 => Ok(Instruction::LDMEM(ri)),
                _ => Err(eyre!("Unknown 0xFx opcode 0x{:02X}{:02X}", bytes.0, bytes.1)),
            },
            seq => Err(eyre!("Unknown bytes sequence {:#04X}", seq))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test] 
    fn test_raw_instruction_parse() {
        let instruction = RawInstruction(0x1F, 0x23);
        assert_eq!(instruction.nibble_at(1), 15);
        assert_eq!(instruction.nibble_at(2), 2);
        assert_eq!(instruction.nibble_at(3), 3);
    }

    #[test]
    fn test_raw_instruction_display() {
        let instruction = RawInstruction(0x1F, 0x23);
        assert_eq!(format!("{}", instruction), "0x1F23");
    }

    #[test]
    fn test_raw_instruction_u16_byte_order() {
        // 00001011 00100010
        let instruction = RawInstruction(11, 34);
        assert_eq!(instruction.as_u16(), 2850);
        assert_eq!(instruction.as_u16() & 0x0FFF, 2850);
    }
}