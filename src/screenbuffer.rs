const BUFFER_SIZE: usize = 64 * 32;
const FRONT_BUFFER: [bool; BUFFER_SIZE] = [true; BUFFER_SIZE];

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

    pub fn clear(&mut self) {
        unsafe {
            self.buffer = std::mem::zeroed();
        }
    }
}
