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

#[derive(Debug)]
pub enum Event {
    Nmi,
    Reset,
}

#[derive(Debug)]
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
    VBlank,
}

#[derive(Debug)]
pub struct Ppu2c02Interface {
    nmi: bool,

    op: Option<(PpuRegisters, u8)>,

    // Cache values for reads
    //ctrl: u8,
    //mask: u8,
    status: u8,
    //oamaddr: u8,
    oamdata: u8,
    //scroll: u8,
    addr: u8,
    data: u8,
    oamdma: u8,
}

impl Ppu2c02Interface {
    pub fn new() -> Ppu2c02Interface {
        Ppu2c02Interface {
            nmi: false,
            op: None,
            status: 0,
            oamdata: 0,
            addr: 0,
            data: 0,
            oamdma: 0,
        }
    }

    pub fn is_nmi(&self) -> bool {
        self.nmi
    }

    pub fn set_nmi(&mut self, nmi: bool) {
        self.nmi = nmi;
    }

    pub fn set_reg(&mut self, offset: u8, val: u8) {
        match offset {
            0x02 => self.status = val,
            0x04 => self.oamdata = val,
            0x06 => self.addr = val,
            0x07 => self.data = val,
            0x14 => self.oamdma = val,
            _ => {
                error!("Attempt to set an invalid PPU register");
            }
        }
    }

    pub fn reg_write(&mut self, offset: u8, val: u8) {
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

    pub fn reg_read(&self, offset: u8) -> u8 {
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

    fn pop_op(&mut self) -> Option<(PpuRegisters, u8)> {
        None
        //let mut reg: PpuRegisters;
        //if let Some(i) = self.op {
        //    reg = i.0;
        //}
        //match self.op {
        //    Some(ref i) => Some(i),
        //    _ => None,
        //}
        //Some(self.op.unwrap())
    }
}

#[derive(Debug)]
pub struct Ppu2c02 {
    state: PpuState,
    count: u64,
}

impl Ppu2c02 {
    pub fn new() -> Ppu2c02 {
        Ppu2c02 {
            state: PpuState::WarmUp,
            count: 0,
        }
    }

    pub fn reset(&mut self) {
        info!("Reset PPU");
        self.count = 0;
    }

    pub fn power_on_reset(&mut self) {
        info!("Power Cycle PPU");
        self.count = 0;
    }

    pub fn tick(&mut self, mm: &mut MemoryMap, e: &mut VecDeque<Event>) {
        // Handle register writes placed on the bus from the CPU
        if let Some(op) = mm.ppu.pop_op() {
            match op {
                (PpuRegisters::Ctrl, _v) => {}
                (PpuRegisters::Mask, _v) => {}
                (PpuRegisters::OamAddr, _v) => {}
                (PpuRegisters::OamData, _v) => {}
                (PpuRegisters::Scroll, _v) => {}
                (PpuRegisters::Addr, _v) => {}
                (PpuRegisters::Data, _v) => {}
                (PpuRegisters::OamDma, _v) => {}
                (_, _) => {}
            }
        }

        self.state = match self.state {
            PpuState::WarmUp => {
                if self.count == 29658 {
                    PpuState::VBlank
                } else {
                    PpuState::WarmUp
                }
            }
            PpuState::VBlank => {
                if self.count == 90000 {
                    e.push_front(Event::Nmi);
                }
                PpuState::VBlank
            }
        };

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
