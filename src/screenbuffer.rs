pub const ROWS: u32 = 32;
pub const COLS: u32 = 64;

const BUFFER_SIZE: usize = 64 * 32;
const FRONT_BUFFER: [bool; BUFFER_SIZE] = [false; BUFFER_SIZE];

static mut SCREEN_BUFFER_INSTANCE: ScreenBuffer = ScreenBuffer::new();

pub struct ScreenBuffer {
    // TODO: provide a better way to iterate over pixels?
    pub buffer: [bool; BUFFER_SIZE],
}

impl ScreenBuffer {
    const fn new() -> Self {
        Self {
            buffer: FRONT_BUFFER,
        }
    }

    pub fn acquire() -> &'static mut Self {
        unsafe { &mut SCREEN_BUFFER_INSTANCE }
    }

    /// Sets all buffer values to false using a cheap memset.
    pub fn clear(&mut self) {
        // SAFTEY: we know that `0` is a valid value for `false` (e.g. `boolean`)
        unsafe {
            self.buffer = std::mem::zeroed();
        }
    }

    /// Writes a pixel at a given coordinate location.  If this operation causes
    /// a pixel to be erased, returns true.  Otherwise returns false.
    pub fn write_pixel(&mut self, value: bool, x: usize, y: usize) -> bool {
        let index = (COLS as usize * y) + x;
        let previous_value = self.buffer[index];
        self.buffer[index] = value;

        return match previous_value {
            true if value == false => true,
            _ => false,
        };
    }

    pub fn read_pixel(&self, x: usize, y: usize) -> bool {
        self.buffer[(COLS as usize * y) + x]
    }
}
