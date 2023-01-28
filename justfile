default: 
    cargo run --bin chip8 ./games/chip8-test-suite.ch8
    # cargo run --bin chip8 ./games/PONG

write_ldkey: 
    cargo run --bin test_ldkey
