/**
 *
 */

use std::collections::VecDeque;
use crate::nes_mem::MemoryMapInterface;

#[derive(Debug)]
pub enum Event {
    Nmi,
    Reset,
}

#[derive(Debug)]
pub struct Ppu2c02 {
    count: u64,
}


impl Ppu2c02 {
    pub fn new() -> Ppu2c02 {
        Ppu2c02 {
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
        if self.count < 29658 {

        } else {
            if self.count == 90000 {
                e.push_front(Event::Nmi);
            }
        }
        
        self.count += 1;
    }
}

impl MemoryMapInterface for Ppu2c02 {
    fn read_u8(&self, offset: usize) -> u8 {
        0
    }

    fn read_u16(&self, offset: usize) -> u16 {
        0
    }

    fn write_u8(&mut self, offset: usize, val: u8) {
        self.reset();
    }

    fn write_u16(&mut self, offset: usize, val: u16) {
        self.reset();
    }
}

impl std::fmt::Display for Ppu2c02 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "count: {:08x}\n", self.count)?;
        write!(f, "== Current PPU State ===========================\n")?;
        Ok(())
    }
}
