# NES Emulator in Rust

nes_rs is a cycle accurate NES emulator written in the Rust language. The goal is to 
provide a platform to learn Rust and experiment with various Crates.  The emulator itself
is a cycle accurate implementation of the RP2A03 (6502 core) and PPU chips on the Nintendo 
Entertainment System.  

To build the project, install the Rust environment using RustUp: https://rustup.rs/ 

Then build the project as follows:

```
cargo build
cargo run "Super Mario Bros.nes"
```

# Status

Currently, the emulation for the RP2A03 is complete and some graphics support is available. 
The following features are working:
- Load ROM files and palettes
- CPU emulation with trace support
- Basic display of background (nametables) graphics

Todo:
- Support sprites
- Audio support
- Controller support
- Support mappers

# Debugging
To enable debug logging, the Env logger options can be used as follows:

```
RUST_LOG=nes_rs=debug cargo run "Super Mario Bros. (World).nes"
or
RUST_LOG=nes_rs=debug ./nes_rs "Super Mario Bros. (World).nes"
```

