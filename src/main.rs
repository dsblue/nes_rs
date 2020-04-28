#[macro_use]
extern crate log;

use std::env;
use std::path::Path;
use std::thread;
use std::time::Duration;

mod nes_2a03;
mod nes_mem;
mod nes_rom;

use nes_2a03::Cpu6502;
use nes_mem::MemoryMap;
use nes_rom::Rom;

fn usage(app: &String) {
    println!("Usage: {} filename", app);
}

fn main() {
    env_logger::init();

    let args: Vec<String> = env::args().collect();

    info!("Passed arguments: {:?}", args);

    if args.len() != 2 {
        usage(&args[0]);
        return;
    }

    // Load the contents of the ROM
    let path = Path::new(&args[1]);

    let rom = Rom::from_file(&path).unwrap();

    info!("Loaded ROM file: {}", rom);

    let mm = MemoryMap::new(&rom);

    let mut cpu = Cpu6502::new(mm);

    cpu.reset();

    for _ in 1..100 {
        cpu.tick();
        thread::sleep(Duration::from_millis(10));
    }
}
