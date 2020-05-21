use crate::nes_mem::MemoryMapInterface;
use std::collections::VecDeque;

#[derive(Debug)]
pub enum Event {
    Nmi,
    Reset,
}

#[derive(Debug)]
enum PpuState {
    WarmUp,
    VBlank,
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

    pub fn tick(&mut self, e: &mut VecDeque<Event>) {
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

impl MemoryMapInterface for Ppu2c02 {
    fn read_u8(&self, address: usize) -> u8 {
        let offset = address & 0b111;
        let value = match offset {
            0x00 => 0,
            0x01 => 0,
            0x02 => 0xff,
            0x03 => 0,
            0x04 => 0,
            0x05 => 0,
            0x06 => 0,
            0x07 => 0,
            0x14 => 0,
            _ => 0,
        };

        trace!("PPU Read 0x{:04x}: 0x{:02x}", offset, value);

        value
    }

    fn read_u16(&self, _offset: usize) -> u16 {
        0
    }

    fn write_u8(&mut self, offset: usize, val: u8) {
        trace!("PPU Write 0x{:04x}: 0x{:02x}", offset, val);
    }

    fn write_u16(&mut self, _offset: usize, _val: u16) {}
}

impl std::fmt::Display for Ppu2c02 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "count: {:08x}\n", self.count)?;
        write!(f, "== Current PPU State ===========================\n")?;
        Ok(())
    }
}
