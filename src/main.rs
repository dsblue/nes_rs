#[macro_use]
extern crate log;
extern crate clap;
use clap::arg;
use clap::Command;
use pixels::{Pixels, SurfaceTexture};
use ppu::FrameBuffer;
use std::collections::VecDeque;
use std::path::Path;
use std::thread;
use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::WindowEvent;
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;
use winit::window::Window;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::sync::Mutex;

mod error;
mod mem;
mod ppu;
mod prelude;
mod rom;
mod rp2a03;

use mem::MemoryMap;
use ppu::Ppu2c02;
use rom::Rom;
use rp2a03::Cpu6502;

const WIDTH: u32 = 256;
const HEIGHT: u32 = 240;

#[derive(Default)]
struct App {
    window: Option<Window>,
    pixels: Option<Pixels>,

    frame: Arc<Mutex<FrameBuffer>>,
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        self.window = Some({
            let size = LogicalSize::new(WIDTH as f64, HEIGHT as f64);
            event_loop
                .create_window(
                    Window::default_attributes()
                        .with_title("NES RS Emulator Main PPU Window")
                        .with_inner_size(size)
                        .with_min_inner_size(size),
                )
                .unwrap()
        });

        if let Some(window) = &self.window {
            self.pixels = Some(
                {
                    let surface_texture = SurfaceTexture::new(WIDTH, HEIGHT, &window);
                    Pixels::new(WIDTH, HEIGHT, surface_texture)
                }
                .unwrap(),
            );
        }
    }

    fn about_to_wait(&mut self, _event_loop: &winit::event_loop::ActiveEventLoop) {
        if let Some(window) = self.window.as_mut() {
            if let Some(pixel) = self.pixels.as_mut() {
                let frame = self.frame.lock().unwrap();
                window.pre_present_notify();
                frame.draw(pixel.frame_mut());
                window.request_redraw();
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        _window_id: winit::window::WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                println!("The close button was pressed; stopping");
                event_loop.exit();
            }
            WindowEvent::Resized(size) => {
                if let Some(pixels) = self.pixels.as_mut() {
                    pixels.resize_surface(size.width, size.height).unwrap();
                }
            }
            WindowEvent::RedrawRequested => {
                if let Some(pixels) = self.pixels.as_mut() {
                    let frame = self.frame.lock().unwrap();
                    frame.draw(pixels.frame_mut());
                    pixels.render().unwrap();
                }
            }
            _ => (),
        }
    }
}

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
            }
            Err(error) => {
                println!("{}", error.to_string());
            }
        }
    }
}

fn run(rom: Rom) {
    let event_loop = EventLoop::new().unwrap();

    let running = Arc::new(AtomicBool::new(true));
    let r = running.clone();

    let mut app = App::default();

    app.frame = Arc::new(Mutex::new(ppu::FrameBuffer::default()));

    ctrlc::set_handler(move || {
        r.store(false, Ordering::SeqCst);
    })
    .expect("Error installing CTRL-C handler");

    let _event_loop_proxy = event_loop.create_proxy();

    {
        let is_running = running.clone();
        let frame = Arc::clone(&app.frame);

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

    event_loop.run_app(&mut app).unwrap();
}
