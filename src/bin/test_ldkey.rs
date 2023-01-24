/// Tests the LDKEY instruction with a simple program that waits for a key, then
/// displays the key value as a sprite in an infinite loop.

pub fn main() -> color_eyre::Result<()> {
    // Using a cringe Vec becaused based static array length inference is not
    // present in the stable compiler yet, and I can't be bothered to type that
    // shit out :(
    let program: Vec<u8> = vec![
        0x00, 0xE0, // clear
        0xF0, 0x0A, // wait for key press, store value in V0
        0xF0, 0x29, // set I to the value of the sprite in V0
        0x61, 0x1C, // Set X coord
        0x62, 0x0C, // Set Y coord
        0x00, 0xE0, // clear
        0xD1, 0x25, // display sprite at I
        0x12, 0x02, // loop back to waiting for a key press
    ];
    std::fs::write("./test_program.ch8", program.as_slice())?;

    println!("Complete");
    Ok(())
}
