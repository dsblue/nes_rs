#[macro_use]
extern crate log;

use std::env;
use std::path::Path;
use std::thread;
use std::time::Duration;
use std::collections::VecDeque;

mod nes_2a03;
mod nes_ppu;
mod nes_mem;
mod nes_rom;

use nes_2a03::Cpu6502;
use nes_ppu::Ppu2c02;
use nes_mem::MemoryMap;
use nes_rom::Rom;

use nes_ppu::Event;


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

    run(rom);

}

fn run(rom: Rom) {
    let mut mm = MemoryMap::new(&rom);

    let mut ppu = Ppu2c02::new();

    mm.add_region(0x100, 0x200, &mut ppu);

    let mut cpu = Cpu6502::new(&mut mm);

    cpu.power_on_reset();
    ppu.power_on_reset();

    let mut events: VecDeque<Event> = VecDeque::new();

    events.push_front(Event::Reset);

    loop {
        
        cpu.tick(&mut events);
        ppu.tick(&mut events);

        // Handle any generated events
        while let Some(e) = events.pop_back() {
            match e {
                Event::Reset => {
                    cpu.reset();
                    ppu.reset();
                }
                Event::Nmi => cpu.nmi(),
            }
        }

        thread::sleep(Duration::from_micros(1));
    }
}
