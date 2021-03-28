#[macro_use]
extern crate log;
use pixels::{Error, Pixels, SurfaceTexture};
use std::collections::VecDeque;
use std::env;
use std::path::Path;
//use std::thread;
//use std::time::Duration;
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode, WindowEvent};
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;
use winit::window::WindowBuilder;

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
        // Make a scaled up window for testing
        // TODO: Support HIDPI
        let size = LogicalSize::new(4.0 * WIDTH as f64, 4.0 * HEIGHT as f64);
        WindowBuilder::new()
            .with_title("NES RS")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };
    let _hidpi_factor = window.scale_factor();

    let mut pixels = {
        let surface_texture = SurfaceTexture::new(WIDTH, HEIGHT, &window);
        Pixels::new(WIDTH, HEIGHT, surface_texture)?
    };

    let mut mm = MemoryMap::new(&rom);
    let mut events: VecDeque<ppu::Event> = VecDeque::new();
    let mut cpu = Cpu6502::new();
    let mut ppu = Ppu2c02::new();

    cpu.power_on_reset(&mut mm);
    ppu.power_on_reset();
    events.push_front(ppu::Event::Reset);

    event_loop.run(move |event, _, control_flow| {
        // ControlFlow::Poll continuously runs the event loop, even if the OS hasn't
        // dispatched any events. This is ideal for games and similar applications.
        *control_flow = ControlFlow::Poll;
        // ControlFlow::Wait pauses the event loop if no events are available to process.
        // This is ideal for non-game applications that only update in response to user
        // input, and uses significantly less power/CPU time than ControlFlow::Poll.
        //*control_flow = ControlFlow::Wait;

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
                ppu::Event::VBlank => {
                    let f = pixels.get_frame();

                    f.clone_from_slice(ppu.current_frame());

                    pixels.render().unwrap();
                }
            }
        }

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                println!("The close button was pressed; stopping");
                *control_flow = ControlFlow::Exit
            }
            Event::WindowEvent {
                event: WindowEvent::Resized(size),
                ..
            } => {
                pixels.resize(size.width, size.height);
            }
            Event::WindowEvent {
                event: WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(VirtualKeyCode::Escape) = input.virtual_keycode {
                    *control_flow = ControlFlow::Exit;
                }
            }
            Event::MainEventsCleared => {
                // Application update code.
                // Queue a RedrawRequested event.
                //
                // You only need to call this if you've determined that you need to redraw, in
                // applications which do not always need to. Applications that redraw continuously
                // can just render here instead.
                window.request_redraw();
            }
            Event::RedrawRequested(_) => {
                // Redraw the application.
                //
                // It's preferable for applications that do not render continuously to render in
                // this event rather than in MainEventsCleared, since rendering in here allows
                // the program to gracefully handle redraws requested by the OS.
            }
            _ => (),
        }
    });
}
