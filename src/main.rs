#[macro_use]
extern crate log;
extern crate clap;
use clap::arg;
use clap::Command;
use pixels::{Pixels, SurfaceTexture};
use std::collections::VecDeque;
use std::path::Path;
use std::thread;
use winit::dpi::LogicalSize;
use winit::event::{Event, WindowEvent};
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;
use winit::window::WindowBuilder;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::sync::Mutex;

mod error;
mod prelude;
mod mem;
mod ppu;
mod rom;
mod rp2a03;

use mem::MemoryMap;
use ppu::Ppu2c02;
use rom::Rom;
use rp2a03::Cpu6502;

const WIDTH: u32 = 256;
const HEIGHT: u32 = 240;

fn main() {
    env_logger::init();

    let matches = Command::new("NES Rust")
        .version("0.1")
        .about("A simple NES emulator written in Rust")
        .author("Nathan Palmer")
        .args(&[
            arg!(-p --palette [FILE] "Load a custom palette file"),
            arg!(-d --debug "Enable debug console"),
            arg!(<ROM> "ROM file to load"),
        ])
        .get_matches();

    // Load the contents of the ROM
    if let Some(rom_path) = matches.get_one::<String>("ROM") {

        let path = Path::new(rom_path);
        match Rom::from_file(&path) {
            Ok(rom) => {
                info!("Loaded ROM file: {}", rom);
                run(rom);
            },
            Err(error) => {
                println!("{}", error.to_string());
            }
        }
    }

}

fn run(rom: Rom) {
    let event_loop = EventLoop::new().unwrap();

    let window = {
        let size = LogicalSize::new(WIDTH as f64, HEIGHT as f64);
        WindowBuilder::new()
            .with_title("NES RS Emulator Main PPU Window")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };

    let mut pixels = {
        let surface_texture = SurfaceTexture::new(WIDTH, HEIGHT, &window);
        Pixels::new(WIDTH, HEIGHT, surface_texture)
    }.unwrap();

    let running = Arc::new(AtomicBool::new(true));
    let r = running.clone();

    let frame = Arc::new(Mutex::new(ppu::FrameBuffer::default()));

    ctrlc::set_handler(move || {
        r.store(false, Ordering::SeqCst);
    })
    .expect("Error installing CTRL-C handler");

    let _event_loop_proxy = event_loop.create_proxy();

    {
        let is_running = running.clone();
        let frame = Arc::clone(&frame);

        thread::spawn(move || {
            let mut cpu = Cpu6502::new();
            let mut ppu = Ppu2c02::new(rom.chr_rom.to_owned());
            ppu.set_framebuffer(frame);

            let mut mm = MemoryMap::new(&rom, ppu);

            let mut events: VecDeque<ppu::Event> = VecDeque::new();
            events.push_front(ppu::Event::Reset);

            cpu.power_on_reset(&mut mm);

            loop {
                for _ in 0..10_000 {
                    cpu.tick(&mut mm, &mut events);
                    mm.tick_ppu(&mut events);
                    mm.tick_ppu(&mut events);
                    mm.tick_ppu(&mut events);

                    // Handle any generated events
                    while let Some(e) = events.pop_back() {
                        match e {
                            ppu::Event::Reset => {
                                cpu.reset(&mut mm);
                            }
                            ppu::Event::Nmi => {
                                info!("NMI-");
                                cpu.nmi();
                            }
                            ppu::Event::VBlank => (),
                        }
                    }

                    if !is_running.load(Ordering::SeqCst) {
                        println!("CTRL^C, Stopping Emulation");
                        return;
                    }
                }
                thread::sleep(std::time::Duration::from_millis(10));
            }
        });
    }

    event_loop.set_control_flow(ControlFlow::Poll);

    let _ = event_loop.run(move |event, elwt| {
        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                println!("The close button was pressed; stopping");
                elwt.exit();
            },
            Event::WindowEvent { 
                event: WindowEvent::Resized(size),
                ..
            } => {
                pixels.resize_surface(size.width, size.height).unwrap();
            },
            Event::AboutToWait => {
                {
                    let frame = frame.lock().unwrap();
                    frame.draw(pixels.frame_mut());
                }
                window.request_redraw();
            },
            Event::WindowEvent {
                event: WindowEvent::RedrawRequested,
                ..                
            } => {
                pixels.render().unwrap();
            }
            _ => ()
        }
    });
}
