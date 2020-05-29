#[macro_use]
extern crate log;
//use log::error;
use pixels::{wgpu::Surface, Error, Pixels, SurfaceTexture};
use std::collections::VecDeque;
use std::env;
use std::path::Path;
use std::thread;
use std::time::Duration;
use winit::dpi::LogicalSize;
//use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::EventLoop;
//use winit::event_loop::{ControlFlow};
use winit::window::WindowBuilder;

mod nes_2a03;
mod nes_mem;
mod nes_ppu;
mod nes_rom;

use nes_2a03::Cpu6502;
use nes_mem::MemoryMap;
use nes_ppu::Ppu2c02;
use nes_rom::Rom;

const WIDTH: u32 = 320;
const HEIGHT: u32 = 240;

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

    run(rom).unwrap();
}

fn run(rom: Rom) -> Result<(), Error> {
    let event_loop = EventLoop::new();
    let window = {
        let size = LogicalSize::new(4.0 * WIDTH as f64, 4.0 * HEIGHT as f64);
        WindowBuilder::new()
            .with_title("NES RS")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };
    //let mut hidpi_factor = window.scale_factor();

    let mut pixels = {
        let surface = Surface::create(&window);
        let surface_texture = SurfaceTexture::new(WIDTH, HEIGHT, surface);
        Pixels::new(WIDTH, HEIGHT, surface_texture)?
    };

    pixels.resize(4 * WIDTH, 4 * HEIGHT);
    window.request_redraw();

    let mut mm = MemoryMap::new(&rom);
    let mut events: VecDeque<nes_ppu::Event> = VecDeque::new();
    let mut cpu = Cpu6502::new();
    let mut ppu = Ppu2c02::new();

    cpu.power_on_reset(&mut mm);
    ppu.power_on_reset();
    events.push_front(nes_ppu::Event::Reset);

    loop {
        cpu.tick(&mut mm, &mut events);
        ppu.tick(&mut mm, &mut events);
        ppu.tick(&mut mm, &mut events);
        ppu.tick(&mut mm, &mut events);

        if mm.ppu.nmi {
            mm.ppu.nmi = false;
            cpu.nmi();
        }

        // Handle any generated events
        while let Some(e) = events.pop_back() {
            match e {
                nes_ppu::Event::Reset => {
                    cpu.reset(&mut mm);
                    ppu.reset();
                }
                nes_ppu::Event::VBlank => {
                    let f = pixels.get_frame();

                    f.clone_from_slice(ppu.current_frame());

                    pixels.render().unwrap();
                }
            }
        }

        thread::sleep(Duration::from_micros(1));
    }
}
