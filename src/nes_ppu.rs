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
use std::collections::VecDeque;
use std::default::Default;

const SCANLINES_PER_FRAME: u32 = 262;
const CYCLES_PER_SCANLINE: u32 = 341;

const PPUCTRL_V: u8 = 0x80;
const _PPUCTRL_P: u8 = 0x40;
const _PPUCTRL_H: u8 = 0x20;
const _PPUCTRL_B: u8 = 0x10;
const _PPUCTRL_S: u8 = 0x08;
const _PPUCTRL_I: u8 = 0x04;

const PPUSTATUS_VBLANK: u8 = 0x80;

#[derive(Debug)]
pub enum Event {
    Reset,
}

#[derive(Debug, Copy, Clone)]
pub enum PpuRegisters {
    Ctrl,
    Mask,
    Status,
    OamAddr,
    OamData,
    Scroll,
    Addr,
    Data,
    OamDma,
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

    op: Option<(PpuRegisters, u8)>,

    // Cache values for reads
    status: u8,
    oamdata: u8,
    addr: u8,
    data: u8,
    oamdma: u8,
}

impl Ppu2c02Interface {
    pub fn new() -> Ppu2c02Interface {
        Ppu2c02Interface::default()
    }

    pub fn ppu_write(&mut self, reg: PpuRegisters, val: u8) {
        match reg {
            PpuRegisters::Status => self.status = val,
            PpuRegisters::OamData => self.oamdata = val,
            PpuRegisters::Addr => self.addr = val,
            PpuRegisters::Data => self.data = val,
            PpuRegisters::OamDma => self.oamdma = val,
            _ => (),
        }
    }

    pub fn ppu_pop_write_op(&mut self) -> Option<(PpuRegisters, u8)> {
        if let Some(op) = self.op {
            self.op = None;
            Some(op)
        } else {
            None
        }
    }

    pub fn cpu_write(&mut self, offset: u8, val: u8) {
        match offset {
            0x00 => self.op = Some((PpuRegisters::Ctrl, val)),
            0x01 => self.op = Some((PpuRegisters::Mask, val)),
            0x02 => self.op = Some((PpuRegisters::Status, val)),
            0x03 => self.op = Some((PpuRegisters::OamAddr, val)),
            0x04 => self.op = Some((PpuRegisters::OamData, val)),
            0x05 => self.op = Some((PpuRegisters::Scroll, val)),
            0x06 => self.op = Some((PpuRegisters::Addr, val)),
            0x07 => self.op = Some((PpuRegisters::Data, val)),
            0x14 => self.op = Some((PpuRegisters::OamDma, val)),
            _ => {
                self.op = None;
                error!("Attempt to write to an invalid PPU register");
            }
        }
    }

    pub fn cpu_read(&self, offset: u8) -> u8 {
        match offset {
            0x02 => self.status,
            0x04 => self.oamdata,
            0x06 => self.addr,
            0x07 => self.data,
            0x14 => self.oamdma,
            _ => {
                error!("Attempt to read from an invalid PPU register");
                0
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct Ppu2c02 {
    reg_ppuctrl: u8,
    reg_ppumask: u8,
    reg_ppustatus: u8,
    reg_oamaddr: u8,
    reg_oamdata: u8,
    reg_ppuscroll: u8,
    reg_ppuaddr: u8,
    reg_ppudata: u8,
    reg_oamdma: u8,

    scanline: u32,
    cycle: u32,

    state: PpuState,
    count: u64,
}

impl Ppu2c02 {
    pub fn new() -> Ppu2c02 {
        Ppu2c02::default()
    }

    pub fn reset(&mut self) {
        info!("Reset PPU");
        self.count = 0;
    }

    pub fn power_on_reset(&mut self) {
        info!("Power Cycle PPU");
        self.count = 0;
    }

    pub fn tick(&mut self, mm: &mut MemoryMap, _e: &mut VecDeque<Event>) {
        // Handle register writes placed on the bus from the CPU
        if let Some(op) = mm.ppu.ppu_pop_write_op() {
            match op {
                (PpuRegisters::Ctrl, v) => {
                    self.reg_ppuctrl = v;
                }
                (PpuRegisters::Status, v) => {
                    self.reg_ppustatus = v;
                    mm.ppu.ppu_write(op.0, v);
                }
                (PpuRegisters::Mask, v) => {
                    self.reg_ppumask = v;
                }
                (PpuRegisters::OamAddr, v) => {
                    self.reg_oamaddr = v;
                }
                (PpuRegisters::OamData, v) => {
                    self.reg_oamdata = v;
                    mm.ppu.ppu_write(op.0, v);
                }
                (PpuRegisters::Scroll, v) => {
                    self.reg_ppuscroll = v;
                }
                (PpuRegisters::Addr, v) => {
                    self.reg_ppuaddr = v;
                    mm.ppu.ppu_write(op.0, v);
                }
                (PpuRegisters::Data, v) => {
                    self.reg_ppudata = v;
                    mm.ppu.ppu_write(op.0, v);
                }
                (PpuRegisters::OamDma, v) => {
                    self.reg_oamdma = v;
                    mm.ppu.ppu_write(op.0, v);
                }
            }
        }

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
                    mm.ppu.ppu_write(PpuRegisters::Status, self.reg_ppustatus);
                    if (self.reg_ppuctrl & PPUCTRL_V) == PPUCTRL_V {
                        mm.ppu.nmi = true;
                    }
                }
            }
            242..=260 => {}
            261 => {
                if self.cycle == 1 {
                    self.reg_ppustatus = self.reg_ppustatus & !PPUSTATUS_VBLANK;
                    mm.ppu.ppu_write(PpuRegisters::Status, self.reg_ppustatus);
                }
            }
            _ => (),
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

    fn read_u8(&self, mm: &mut MemoryMap) -> u8 {
        0
    }
}

impl std::fmt::Display for Ppu2c02 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "count: {:08x}\n", self.count)?;
        write!(f, "== Current PPU State ===========================\n")?;
        Ok(())
    }
}
