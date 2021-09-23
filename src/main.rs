#[macro_use]
extern crate log;
extern crate clap;
use clap::App;
use pixels::{Error, Pixels, SurfaceTexture};
//use read_input::prelude::*;
use std::collections::VecDeque;
use std::path::Path;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use winit::dpi::{LogicalSize, PhysicalSize};
use winit::event::{Event, VirtualKeyCode, WindowEvent};
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;
use winit::window::WindowBuilder;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::sync::Mutex;

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

    let matches = App::new("NES Rust")
        .version("0.1")
        .about("A simple NES emulator written in Rust")
        .author("Nathan Palmer")
        .args_from_usage(
            "-p,--palette=[FILE]    'Load a custom palette file'
                          -d                     'Enable debug console'
                          <ROM>                  'ROM file to load'",
        )
        .get_matches();

    // Load the contents of the ROM
    let path = Path::new(matches.value_of("ROM").unwrap());

    let rom = Rom::from_file(&path).unwrap();

    info!("Loaded ROM file: {}", rom);

    run(rom).unwrap();
}

fn run(rom: Rom) -> Result<(), Error> {
    let event_loop = EventLoop::new();
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
        Pixels::new(WIDTH, HEIGHT, surface_texture)?
    };

    let scale = 2;
    window.set_inner_size(PhysicalSize::new(
        window.inner_size().width * scale, 
        window.inner_size().height * scale));

    let running = Arc::new(AtomicBool::new(true));
    let r = running.clone();

    let frame = Arc::new(Mutex::new(ppu::FrameBuffer::default()));

    ctrlc::set_handler(move || {
        r.store(false, Ordering::SeqCst);
    })
    .expect("Error installing CTRL-C handler");

    let _event_loop_proxy = event_loop.create_proxy();

    {
        let frame = Arc::clone(&frame);

        thread::spawn(move || {
            let mut mm = MemoryMap::new(&rom);
            let mut cpu = Cpu6502::new();
            let mut ppu = Ppu2c02::new();

            ppu.set_framebuffer(frame);

            let mut events: VecDeque<ppu::Event> = VecDeque::new();
            events.push_front(ppu::Event::Reset);

            cpu.power_on_reset(&mut mm);
            ppu.power_on_reset();

            loop {
                for _ in 0..10_000 {
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
                            ppu::Event::Reset => {
                                cpu.reset(&mut mm);
                                ppu.reset();
                            }
                            ppu::Event::VBlank => {}
                        }
                    }
                }
                thread::sleep(std::time::Duration::from_millis(10));
            }
        });
    }

    event_loop.run(move |event, _, control_flow| {
        //println!("{:?}", event);

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                println!("The close button was pressed; stopping");
                *control_flow = ControlFlow::Exit
            }
            Event::WindowEvent {
                event: WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(VirtualKeyCode::Escape) = input.virtual_keycode {
                    *control_flow = ControlFlow::Exit;
                }
            }
            Event::RedrawEventsCleared => {
                *control_flow =
                    ControlFlow::WaitUntil(Instant::now() + Duration::from_millis(33));

                {
                    let frame = frame.lock().unwrap();
                    frame.draw(pixels.get_frame());
                }

                pixels.render().unwrap();
            }
            Event::RedrawRequested(_) => {
                // May use this later
            }
            _ => (),
        }
    });
}
