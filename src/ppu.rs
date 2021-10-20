/**
 * PPU Model
 *
 * The PPU is memory mapped to the following registers:
 *
 *
 * Power on behavior:
 *
 * Reset behavior:
 *
 *
 */
use rgb::ComponentSlice;
use rgb::RGBA8;
use std::collections::VecDeque;
use std::default::Default;
use std::fs::File;
use std::io::Read;
use std::sync::{Arc, Mutex};

const SCANLINES_PER_FRAME: usize = 262;
const CYCLES_PER_SCANLINE: usize = 341;

const VISIBLE_WIDTH: usize = 256;
const VISIBLE_HIGHT: usize = 240;

const PPUCTRL_V: u8 = 0x80;
const _PPUCTRL_P: u8 = 0x40;
const _PPUCTRL_H: u8 = 0x20;
const PPUCTRL_B: u8 = 0x10;
const _PPUCTRL_S: u8 = 0x08;
const PPUCTRL_I: u8 = 0x04;

const PPUSTATUS_VBLANK: u8 = 0x80;

#[derive(Debug)]
pub enum Event {
    Reset,
    VBlank,
    Nmi,
}

// #[derive(Debug, Copy, Clone)]
// pub enum PpuRegisters {
//     PpuCtrl,   // write
//     PpuStatus, // read
//     PpuMask,   // write
//     PpuScroll, // write x2
//     PpuAddr,   // write x2
//     PpuData,   // read/write
//     OamAddr,   // write
//     OamData,   // read/write
//     OamDma,    // write
// }

#[derive(Debug)]
enum PpuState {
    WarmUp,
    _VBlank,
}

impl Default for PpuState {
    fn default() -> PpuState {
        PpuState::WarmUp
    }
}

pub struct FrameBuffer {
    data: [u8; (4 * VISIBLE_HIGHT * VISIBLE_WIDTH)],
}

impl FrameBuffer {
    pub fn draw(&self, f: &mut [u8]) {
        f.copy_from_slice(&self.data);
    }
}

impl std::fmt::Debug for FrameBuffer {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ok(())
    }
}

impl std::default::Default for FrameBuffer {
    fn default() -> Self {
        FrameBuffer {
            data: [0u8; (4 * VISIBLE_HIGHT * VISIBLE_WIDTH)],
        }
    }
}

struct Nametable {
    data: [u8; 0x400],
}

impl std::fmt::Debug for Nametable {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ok(())
    }
}

impl std::default::Default for Nametable {
    fn default() -> Self {
        Nametable { data: [0u8; 0x400] }
    }
}

type Palette = [u8; 0x20];

type Oam = [u8; 256];

pub struct Ppu2c02 {
    reg_ppuctrl: u8,
    reg_ppumask: u8,
    reg_ppustatus: u8,
    reg_oamaddr: u8,
    reg_oamdata: u8,
    reg_ppudata: u8,
    reg_oamdma: u8,

    scanline: usize,
    cycle: usize,

    scroll_x: u8,
    scroll_y: u8,

    ppu_addr: u16,
    ppu_addr_temp: u16,

    got_ppuscroll: bool,
    got_ppuaddr: bool,

    count: u64,

    chr_rom: Vec<u8>,
    nametables: [Nametable; 4],
    palette: Palette,

    oam: Oam,

    frame_buffer: Arc<Mutex<FrameBuffer>>,
    ntsc: Vec<RGBA8>,
    //    pub nmi: bool,
}

impl std::fmt::Debug for Ppu2c02 {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ok(())
    }
}

impl std::default::Default for Ppu2c02 {
    fn default() -> Self {
        Ppu2c02 {
            reg_ppuctrl: 0,
            reg_ppumask: 0,
            reg_ppustatus: 0,
            reg_oamaddr: 0,
            reg_oamdata: 0,
            reg_ppudata: 0,
            reg_oamdma: 0,
            scanline: 0,
            ppu_addr: 0,
            ppu_addr_temp: 0,
            got_ppuaddr: false,
            got_ppuscroll: false,
            cycle: 0,
            scroll_x: 0,
            scroll_y: 0,
            count: 0,
            chr_rom: Vec::new(),
            nametables: [
                Nametable::default(),
                Nametable::default(),
                Nametable::default(),
                Nametable::default(),
            ],
            palette: Palette::default(),
            oam: [0u8; 256],
            frame_buffer: Arc::default(),
            ntsc: Vec::with_capacity(64),
            //            nmi: false,
        }
    }
}

impl Ppu2c02 {
    fn write_u8(&mut self, addr: usize, val: u8) {
        match addr {
            0x0000..=0x0fff => self.chr_rom[addr & 0x1fff] = val,
            0x1000..=0x1fff => self.chr_rom[addr & 0x1fff] = val,
            0x2000..=0x23ff => self.nametables[0].data[(addr & 0x3ff) as usize] = val,
            0x2400..=0x27ff => self.nametables[1].data[(addr & 0x3ff) as usize] = val,
            0x2800..=0x2bff => self.nametables[2].data[(addr & 0x3ff) as usize] = val,
            0x2c00..=0x2fff => self.nametables[3].data[(addr & 0x3ff) as usize] = val,
            0x3000..=0x33ff => self.nametables[0].data[(addr & 0x3ff) as usize] = val,
            0x3400..=0x37ff => self.nametables[1].data[(addr & 0x3ff) as usize] = val,
            0x3800..=0x3bff => self.nametables[2].data[(addr & 0x3ff) as usize] = val,
            0x3c00..=0x3eff => self.nametables[3].data[(addr & 0x3ff) as usize] = val,
            0x3f00..=0x3fff => {
                if (0b11 & addr) == 0 {
                    self.palette[0] = val;
                } else {
                    self.palette[0x1f & addr] = val;
                }
            }
            _ => error!("PPU Memory write out-if-range: {:04x}", addr),
        }
    }

    fn read_u8(&self, addr: usize) -> u8 {
        match addr {
            0x0000..=0x0fff => self.chr_rom[addr & 0x1fff],
            0x1000..=0x1fff => self.chr_rom[addr & 0x1fff],
            0x2000..=0x23ff => self.nametables[0].data[(addr & 0x3ff) as usize],
            0x2400..=0x27ff => self.nametables[1].data[(addr & 0x3ff) as usize],
            0x2800..=0x2bff => self.nametables[2].data[(addr & 0x3ff) as usize],
            0x2c00..=0x2fff => self.nametables[3].data[(addr & 0x3ff) as usize],
            0x3000..=0x33ff => self.nametables[0].data[(addr & 0x3ff) as usize],
            0x3400..=0x37ff => self.nametables[1].data[(addr & 0x3ff) as usize],
            0x3800..=0x3bff => self.nametables[2].data[(addr & 0x3ff) as usize],
            0x3c00..=0x3eff => self.nametables[3].data[(addr & 0x3ff) as usize],
            0x3f00..=0x3fff => {
                if (0b11 & addr) == 0 {
                    self.palette[0]
                } else {
                    self.palette[0x1f & addr]
                }
            }
            _ => {
                error!("PPU Memory write out-if-range: {:04x}", addr);
                0xff
            }
        }
    }

    pub fn write_reg(&mut self, offset: u8, val: u8) {
        info!("write_reg PPU: {:02x} = {:02x}", offset, val);
        match offset {
            0x00 => self.reg_ppuctrl = val,
            0x01 => self.reg_ppumask = val,
            0x02 => (),
            0x03 => self.reg_oamaddr = val,
            0x04 => {
                self.reg_oamdata = val;
                self.oam[self.reg_oamaddr as usize] = val;
                self.reg_oamaddr = self.reg_oamaddr.wrapping_add(1);
            }
            0x05 => {
                if self.got_ppuscroll {
                    self.scroll_y = val;
                } else {
                    self.scroll_x = val;
                }
                self.got_ppuscroll = !self.got_ppuscroll;
            }
            0x06 => {
                if self.got_ppuaddr {
                    self.ppu_addr = self.ppu_addr_temp | val as u16;
                } else {
                    // It could be that this should be stored in a temporary var
                    // until the full address is ready, but I think not.
                    self.ppu_addr_temp = (0x3f & val as u16) << 8;
                }
                self.got_ppuaddr = !self.got_ppuaddr;
            }
            0x07 => {
                self.reg_ppudata = val;
                info!("{:02x} -> {:04x}", val, self.ppu_addr);
                self.write_u8(self.ppu_addr as usize, val);
                if (self.reg_ppuctrl & PPUCTRL_I) == 0 {
                    self.ppu_addr = self.ppu_addr.wrapping_add(1) & 0x3fff;
                } else {
                    self.ppu_addr = self.ppu_addr.wrapping_add(32) & 0x3fff;
                }
            }
            _ => {
                error!("Attempt to write to an invalid PPU register {:02x}", offset);
            }
        }
    }

    pub fn write_oamdma(&mut self, val: u8) {
        info!("write_oamdma PPU: OAMDMA = {:02x}", val);
        self.reg_oamdma = val;
    }

    pub fn peek_reg(&mut self, offset: u8) -> u8 {
        match offset {
            0x00 => self.reg_ppuctrl,
            0x01 => self.reg_ppumask,
            0x02 => self.reg_ppustatus,
            0x03 => self.reg_oamaddr,
            0x04 => self.reg_oamdata,
            0x05 => 0xff,
            0x06 => 0xff,
            0x07 => self.reg_ppudata,
            _ => {
                error!(
                    "Attempt to read from an invalid PPU register {:02x}",
                    offset
                );
                0xff
            }
        }
    }

    pub fn read_reg(&mut self, offset: u8) -> u8 {
        //info!("read_reg PPU: {:02x}", offset);
        match offset {
            0x02 => {
                self.got_ppuscroll = false; // reset address latch
                self.got_ppuaddr = false; // reset address latch
                self.reg_ppustatus &= !PPUSTATUS_VBLANK; // Clear VBLANK
                self.reg_ppustatus
            }
            0x04 => self.reg_oamdata,
            0x07 => {
                if (self.reg_ppuctrl & PPUCTRL_I) == 0 {
                    self.ppu_addr = self.ppu_addr.wrapping_add(1) & 0x3fff;
                } else {
                    self.ppu_addr = self.ppu_addr.wrapping_add(32) & 0x3fff;
                }
                self.reg_ppudata = self.read_u8(self.ppu_addr as usize);
                self.reg_ppudata
            }
            _ => {
                error!(
                    "Attempt to read from an invalid PPU register {:02x}",
                    offset
                );
                0xff
            }
        }
    }
}

impl Ppu2c02 {
    pub fn new(chr_rom: Vec<u8>) -> Ppu2c02 {
        let mut p = Ppu2c02::default();

        let mut file = match File::open("default.pal") {
            Err(file) => panic!("No palette file {}", file),
            Ok(file) => file,
        };

        let mut buf = Vec::new();

        if let Err(_) = file.read_to_end(&mut buf) {
            panic!("Could not read palette data")
        }

        for (_, c) in buf.chunks_exact(3).enumerate() {
            p.ntsc.push(RGBA8 {
                r: c[0],
                g: c[1],
                b: c[2],
                a: 0xff,
            });
        }
        p.chr_rom = chr_rom;

        p
    }

    pub fn set_framebuffer(&mut self, frame_buffer: Arc<Mutex<FrameBuffer>>) {
        self.frame_buffer = frame_buffer;
    }

    pub fn _reset(&mut self) {
        info!("Reset PPU");
        self.count = 0;
    }

    pub fn _power_on_reset(&mut self) {
        info!("Power Cycle PPU");
        self.count = 0;
    }

    pub fn tick(&mut self, e: &mut VecDeque<Event>) {
        let mut render = false;

        match self.scanline {
            0 => {
                if self.cycle == 0 {
                    info!("PPU: Starting scanline {}", self.scanline);
                }
            }
            1..=239 => {}
            240 => {}
            241 => {
                if self.cycle == 1 {
                    self.reg_ppustatus = self.reg_ppustatus | PPUSTATUS_VBLANK;
                    if (self.reg_ppuctrl & PPUCTRL_V) == PPUCTRL_V {
                        //self.nmi = true;
                        info!("NMI...");

                        e.push_back(Event::Nmi);
                    }

                    render = true;

                    e.push_back(Event::VBlank);
                }
            }
            242..=260 => {}
            261 => {
                if self.cycle == 1 {
                    self.reg_ppustatus = self.reg_ppustatus & !PPUSTATUS_VBLANK;
                }
            }
            _ => (),
        }

        if render {
            let mut frame = self.frame_buffer.lock().unwrap();

            let nt = (self.reg_ppuctrl & 0b11) as usize;
            let pattern = 0x1000 * ((self.reg_ppuctrl & PPUCTRL_B) == PPUCTRL_B) as usize;

            for (i, pixel) in frame.data.chunks_exact_mut(4).enumerate() {
                let row = i / VISIBLE_WIDTH;
                let col = i % VISIBLE_WIDTH;

                let name_i = (col / 8) + (32 * (row / 8));
                let name = self.nametables[nt].data[name_i] as usize;
                let line = row & 0x7;
                let bp1 = self.read_u8(pattern + (name << 4) + 0 + line) as usize;
                let bp2 = self.read_u8(pattern + (name << 4) + 8 + line) as usize;
                let index = (bp1 >> (0x7 - (0x7 & i)) & 1) + ((bp2 >> (0x7 - (0x7 & i)) & 1) << 1);

                let attrib = self.nametables[nt].data[0x3c0 + (col / 32) + (row / 32) * 8];
                let quad = match (row & 0x10, col & 0x10) {
                    (0, 0) => 0,
                    (0, 0x10) => 2,
                    (0x10, 0) => 4,
                    (0x10, 0x10) => 6,
                    _ => 0,
                };
                let attrib = (((attrib >> quad) & 0b11) << 2) as usize;
                let index = attrib | index as usize;
                let index = if (0b11 & index) == 0 {
                    self.palette[0]
                } else {
                    self.palette[0x1f & index]
                } as usize;
                let rgba = self.ntsc[index];

                pixel.copy_from_slice(rgba.as_slice());
            }

            if false {
                print!("Frame: {}\n", self.count);
                let nt = (self.reg_ppuctrl & 0b11) as usize;
                for row in 0..8 {
                    print!("|");
                    for col in 0..8 {
                        print!(
                            "{}{}|",
                            self.nametables[nt].data[0x3c0 + row * 8 + col] >> 0 & 0x3,
                            self.nametables[nt].data[0x3c0 + row * 8 + col] >> 2 & 0x3,
                        );
                    }
                    print!("\n|");
                    for col in 0..8 {
                        print!(
                            "{}{}|",
                            self.nametables[nt].data[0x3c0 + row * 8 + col] >> 4 & 0x3,
                            self.nametables[nt].data[0x3c0 + row * 8 + col] >> 6 & 0x3,
                        );
                    }
                    print!("\n");
                }
            }
        }

        if self.cycle == CYCLES_PER_SCANLINE {
            self.cycle = 0;
            self.scanline += 1;
            if self.scanline == SCANLINES_PER_FRAME {
                self.scanline = 0;
            }
        } else {
            self.cycle += 1;
        }

        self.count += 1;
    }

    pub fn _draw(&self, f: &mut [u8]) {
        let frame = self.frame_buffer.lock().unwrap();
        f.copy_from_slice(&frame.data);
    }
}

impl std::fmt::Display for Ppu2c02 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "count: {:08x}\n", self.count)?;
        write!(f, "== Current PPU State ===========================\n")?;
        Ok(())
    }
}
