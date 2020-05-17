/**
 *
 */

use std::collections::VecDeque;

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

impl std::fmt::Display for Ppu2c02 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "count: {:08x}\n", self.count)?;
        write!(f, "== Current PPU State ===========================\n")?;
        Ok(())
    }
}
