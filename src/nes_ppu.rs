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
use crate::nes_mem::MemoryMap;
use rgb::ComponentSlice;
use rgb::RGBA8;
use std::collections::VecDeque;
use std::default::Default;
use std::fs::File;
use std::io;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;

const SCANLINES_PER_FRAME: usize = 262;
const CYCLES_PER_SCANLINE: usize = 341;

const VISIBLE_WIDTH: usize = 256;
const VISIBLE_HIGHT: usize = 240;

const PPUCTRL_V: u8 = 0x80;
const PPUCTRL_P: u8 = 0x40;
const PPUCTRL_H: u8 = 0x20;
const PPUCTRL_B: u8 = 0x10;
const PPUCTRL_S: u8 = 0x08;
const PPUCTRL_I: u8 = 0x04;

const PPUSTATUS_VBLANK: u8 = 0x80;

static PALETTE: [(u8, u8, u8); 2] = [(0, 0, 0), (0, 0, 0)];

#[derive(Debug)]
pub enum Event {
    Reset,
    VBlank,
}

#[derive(Debug, Copy, Clone)]
pub enum PpuRegisters {
    PpuCtrl,   // write
    PpuStatus, // read
    PpuMask,   // write
    PpuScroll, // write x2
    PpuAddr,   // write x2
    PpuData,   // read/write
    OamAddr,   // write
    OamData,   // read/write
    OamDma,    // write
}

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

#[derive(Debug, Default)]
pub struct Ppu2c02Interface {
    pub nmi: bool,

    read_op: Option<(PpuRegisters, u8)>,
    write_op: Option<(PpuRegisters, u8)>,

    // Cache values for reads
    ppustatus: u8,
    ppuaddr: u8,
    ppudata: u8,
    oamdata: u8,
    oamdma: u8,
}

impl Ppu2c02Interface {
    pub fn new() -> Ppu2c02Interface {
        Ppu2c02Interface::default()
    }

    pub fn update_cache(&mut self, reg: PpuRegisters, val: u8) {
        match reg {
            PpuRegisters::PpuStatus => self.ppustatus = val,
            PpuRegisters::PpuAddr => self.ppuaddr = val,
            PpuRegisters::PpuData => self.ppudata = val,
            PpuRegisters::OamData => self.oamdata = val,
            PpuRegisters::OamDma => self.oamdma = val,
            _ => (),
        }
    }

    pub fn ppu_pop_read_op(&mut self) -> Option<(PpuRegisters, u8)> {
        if let Some(op) = self.read_op {
            self.read_op = None;
            Some(op)
        } else {
            None
        }
    }

    pub fn ppu_pop_write_op(&mut self) -> Option<(PpuRegisters, u8)> {
        if let Some(op) = self.write_op {
            self.write_op = None;
            Some(op)
        } else {
            None
        }
    }

    pub fn cpu_write(&mut self, offset: u8, val: u8) {
        match offset {
            0x00 => self.write_op = Some((PpuRegisters::PpuCtrl, val)),
            0x01 => self.write_op = Some((PpuRegisters::PpuMask, val)),
            0x03 => self.write_op = Some((PpuRegisters::OamAddr, val)),
            0x04 => self.write_op = Some((PpuRegisters::OamData, val)),
            0x05 => self.write_op = Some((PpuRegisters::PpuScroll, val)),
            0x06 => self.write_op = Some((PpuRegisters::PpuAddr, val)),
            0x07 => self.write_op = Some((PpuRegisters::PpuData, val)),
            0x14 => self.write_op = Some((PpuRegisters::OamDma, val)),
            _ => {
                self.write_op = None;
                error!("Attempt to write to an invalid PPU register {:02x}", offset);
            }
        }
    }

    pub fn cpu_read(&mut self, offset: u8) -> u8 {
        match offset {
            0x02 => {
                self.read_op = Some((PpuRegisters::PpuStatus, self.ppustatus));
                self.ppustatus
            }
            0x04 => {
                self.read_op = Some((PpuRegisters::OamData, self.oamdata));
                self.oamdata
            }
            0x07 => {
                self.read_op = Some((PpuRegisters::PpuData, self.ppudata));
                self.ppudata
            }
            _ => {
                self.write_op = None;
                error!(
                    "Attempt to read from an invalid PPU register {:02x}",
                    offset
                );
                0
            }
        }
    }
}

struct Frame {
    data: [u8; (4 * VISIBLE_HIGHT * VISIBLE_WIDTH)],
}

impl std::fmt::Debug for Frame {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ok(())
    }
}

impl std::default::Default for Frame {
    fn default() -> Self {
        Frame {
            data: [0u8; (4 * VISIBLE_HIGHT * VISIBLE_WIDTH)],
        }
    }
}

struct Nametable {
    _data: [u8; 0x400],
}

impl std::fmt::Debug for Nametable {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ok(())
    }
}

impl std::default::Default for Nametable {
    fn default() -> Self {
        Nametable {
            _data: [0u8; 0x400],
        }
    }
}

//#[derive(Debug, Default)]
//struct Palette {
//    data: [u8; 0x1f],
//}

type Palette = [u8; 0x20];

//impl std::fmt::Debug for Palette {
//    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
//        Ok(())
//    }
//}

//impl std::default::Default for Palette {
//    fn default() -> Self {
//        Palette { _data: [0u8; 0x20] }
//    }
//}

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

    got_ppuscroll: bool,
    got_ppuaddr: bool,

    state: PpuState,
    count: u64,

    nametables: [Nametable; 4],
    palette: Palette,

    oam: Oam,

    frame: Frame,
    ntsc: Vec<RGBA8>,
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
            got_ppuaddr: false,
            got_ppuscroll: false,
            cycle: 0,
            scroll_x: 0,
            scroll_y: 0,
            state: PpuState::default(),
            count: 0,
            nametables: [
                Nametable::default(),
                Nametable::default(),
                Nametable::default(),
                Nametable::default(),
            ],
            palette: Palette::default(),
            oam: [0u8; 256],
            frame: Frame::default(),
            ntsc: Vec::with_capacity(256),
        }
    }
}

impl Ppu2c02 {
    pub fn new() -> Ppu2c02 {
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
        p
    }

    pub fn reset(&mut self) {
        info!("Reset PPU");
        self.count = 0;
    }

    pub fn power_on_reset(&mut self) {
        info!("Power Cycle PPU");
        self.count = 0;
    }

    pub fn current_frame(&self) -> &[u8] {
        &self.frame.data
    }

    fn write_u8(&mut self, mm: &mut MemoryMap, addr: usize, val: u8) {
        match addr {
            0x0000..=0x0fff => mm.ppu_write_u8(addr, val),
            0x1000..=0x1fff => mm.ppu_write_u8(addr, val),
            0x2000..=0x23ff => self.nametables[0]._data[(addr & 0x3ff) as usize] = val,
            0x2400..=0x27ff => self.nametables[1]._data[(addr & 0x3ff) as usize] = val,
            0x2800..=0x2bff => self.nametables[2]._data[(addr & 0x3ff) as usize] = val,
            0x2c00..=0x2fff => self.nametables[3]._data[(addr & 0x3ff) as usize] = val,
            0x3000..=0x33ff => self.nametables[0]._data[(addr & 0x3ff) as usize] = val,
            0x3400..=0x37ff => self.nametables[1]._data[(addr & 0x3ff) as usize] = val,
            0x3800..=0x3bff => self.nametables[2]._data[(addr & 0x3ff) as usize] = val,
            0x3c00..=0x3eff => self.nametables[3]._data[(addr & 0x3ff) as usize] = val,
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

    fn read_u8(&self, mm: &MemoryMap, addr: usize) -> u8 {
        match addr {
            0x0000..=0x0fff => mm.ppu_read_u8(addr),
            0x1000..=0x1fff => mm.ppu_read_u8(addr),
            0x2000..=0x23ff => self.nametables[0]._data[(addr & 0x3ff) as usize],
            0x2400..=0x27ff => self.nametables[1]._data[(addr & 0x3ff) as usize],
            0x2800..=0x2bff => self.nametables[2]._data[(addr & 0x3ff) as usize],
            0x2c00..=0x2fff => self.nametables[3]._data[(addr & 0x3ff) as usize],
            0x3000..=0x33ff => self.nametables[0]._data[(addr & 0x3ff) as usize],
            0x3400..=0x37ff => self.nametables[1]._data[(addr & 0x3ff) as usize],
            0x3800..=0x3bff => self.nametables[2]._data[(addr & 0x3ff) as usize],
            0x3c00..=0x3eff => self.nametables[3]._data[(addr & 0x3ff) as usize],
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

    pub fn tick(&mut self, mm: &mut MemoryMap, e: &mut VecDeque<Event>) {
        // Handle register writes placed on the bus from the CPU
        if let Some(op) = mm.ppu.ppu_pop_write_op() {
            //info!("Write {:?}: {:02x}", op.0, op.1);
            match op {
                (PpuRegisters::PpuCtrl, v) => {
                    self.reg_ppuctrl = v;
                }
                (PpuRegisters::PpuMask, v) => {
                    self.reg_ppumask = v;
                }
                (PpuRegisters::PpuScroll, v) => {
                    if self.got_ppuscroll {
                        self.scroll_y = v;
                    } else {
                        self.scroll_x = v;
                    }
                    self.got_ppuscroll = !self.got_ppuscroll;
                }
                (PpuRegisters::PpuAddr, v) => {
                    if self.got_ppuaddr {
                        self.ppu_addr |= v as u16;
                    } else {
                        // It could be that this should be stored in a temporary var
                        // until the full address is ready, but I think not.
                        self.ppu_addr = (0x3f & v as u16) << 8;
                    }
                    self.got_ppuaddr = !self.got_ppuaddr;

                    let t = self.read_u8(mm, self.ppu_addr as usize);
                    mm.ppu.update_cache(PpuRegisters::PpuData, t);
                }
                (PpuRegisters::PpuData, v) => {
                    self.reg_ppudata = v;
                    info!("{:02x} -> {:04x}", v, self.ppu_addr);
                    self.write_u8(mm, self.ppu_addr as usize, v);
                    if (self.reg_ppuctrl & PPUCTRL_I) == 0 {
                        self.ppu_addr = self.ppu_addr.wrapping_add(1) & 0x3fff;
                    } else {
                        self.ppu_addr = self.ppu_addr.wrapping_add(32) & 0x3fff;
                    }
                    let t = self.read_u8(mm, self.ppu_addr as usize);
                    mm.ppu.update_cache(PpuRegisters::PpuData, t);
                }
                (PpuRegisters::OamAddr, v) => {
                    self.reg_oamaddr = v;
                }
                (PpuRegisters::OamData, v) => {
                    self.reg_oamdata = v;
                    self.oam[self.reg_oamaddr as usize] = v;
                    self.reg_oamaddr = self.reg_oamaddr.wrapping_add(1);
                    mm.ppu
                        .update_cache(PpuRegisters::OamData, self.oam[self.reg_oamaddr as usize]);
                }
                (PpuRegisters::OamDma, v) => {
                    self.reg_oamdma = v;
                }
                _ => error!("Invalid Write attempt to {:?}: {:02x}", op.0, op.1),
            }
        }

        // Handle register reads placed on the bus from the CPU
        if let Some(op) = mm.ppu.ppu_pop_read_op() {
            //info!("Read {:?}: {:02x}", op.0, op.1);
            match op {
                (PpuRegisters::PpuStatus, _) => {
                    self.got_ppuscroll = false; // reset address latch
                    self.got_ppuaddr = false; // reset address latch
                    self.reg_ppustatus &= !PPUCTRL_V; // Clear V
                    mm.ppu
                        .update_cache(PpuRegisters::PpuStatus, self.reg_ppustatus);
                }
                (PpuRegisters::PpuData, _) => {
                    if (self.reg_ppuctrl & PPUCTRL_I) == 0 {
                        self.ppu_addr = self.ppu_addr.wrapping_add(1) & 0x3fff;
                    } else {
                        self.ppu_addr = self.ppu_addr.wrapping_add(32) & 0x3fff;
                    }
                    let t = self.read_u8(mm, self.ppu_addr as usize);
                    mm.ppu.update_cache(PpuRegisters::PpuData, t);
                }
                (PpuRegisters::OamData, _) => {}
                _ => error!("Invalid Read attempt to {:?}: {:02x}", op.0, op.1),
            }
        }

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
                    mm.ppu
                        .update_cache(PpuRegisters::PpuStatus, self.reg_ppustatus);
                    if (self.reg_ppuctrl & PPUCTRL_V) == PPUCTRL_V {
                        mm.ppu.nmi = true;
                    }

                    render = true;

                    e.push_back(Event::VBlank);
                }
            }
            242..=260 => {}
            261 => {
                if self.cycle == 1 {
                    self.reg_ppustatus = self.reg_ppustatus & !PPUSTATUS_VBLANK;
                    mm.ppu
                        .update_cache(PpuRegisters::PpuStatus, self.reg_ppustatus);
                }
            }
            _ => (),
        }

        if render {
            let nt = (self.reg_ppuctrl & 0b11) as usize;
            let pattern = 0x1000 * ((self.reg_ppuctrl & PPUCTRL_B) == PPUCTRL_B) as usize;

            for (i, pixel) in self.frame.data.chunks_exact_mut(4).enumerate() {
                let row = i / VISIBLE_WIDTH;
                let col = i % VISIBLE_WIDTH;

                //let table = (self.reg_ppuctrl & 0x3) as usize;
                //let table = 0;
                //for row in 0..30 {
                //for col in 0..32 {
                //print!("{:02x} ", self.nametables[table]._data[row * 32 + col]);
                //}
                //print!("\n");
                //}

                let name_i = (col / 8) + (32 * (row / 8));
                let name = self.nametables[nt]._data[name_i] as usize;
                let line = row & 0x7;
                let bp1 = mm.ppu_read_u8(pattern + (name << 4) + 0 + line) as usize;
                let bp2 = mm.ppu_read_u8(pattern + (name << 4) + 8 + line) as usize;
                let index = (bp1 >> (0x7 - (0x7 & i)) & 1) + ((bp2 >> (0x7 - (0x7 & i)) & 1) << 1);

                let attrib = self.nametables[nt]._data[0x3c0 + (col / 32) + (row / 32) * 8];
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

                //let rgba = self.ntsc[index * 0x8];

                //let rgba = [self.nametables[1]._data[i & 0x3ff], 0x48, 0xe8, 0xff];
                //let t = mm.ppu_read_u8(0x1000 + i);
                //let rgba = [t, 0x48, 0xe8, 0xff];

                pixel.copy_from_slice(rgba.as_slice());
            }
        }

        if self.scanline == SCANLINES_PER_FRAME {
            self.scanline = 0;
        } else {
            self.scanline += 1;
        }

        if self.cycle == CYCLES_PER_SCANLINE {
            self.cycle = 0;
        } else {
            self.cycle += 1;
        }

        self.count += 1;
    }
}

impl std::fmt::Display for Ppu2c02 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "count: {:08x}\n", self.count)?;
        write!(f, "== Current PPU State ===========================\n")?;
        Ok(())
    }
}
