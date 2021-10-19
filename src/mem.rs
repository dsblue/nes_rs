use crate::ppu::Ppu2c02;
use crate::Rom;
use std::fmt;
use std::collections::VecDeque;
use crate::ppu::Event;

pub trait MemoryMapInterface {
    fn read_u8(&self, address: usize) -> u8;
    fn write_u8(&mut self, address: usize, value: u8);
}

impl fmt::Debug for dyn MemoryMapInterface {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "MemoryMapInterface")
    }
}

#[derive(Debug)]
enum RegionType {
    Rom { data: Vec<u8> },
}

impl MemoryMapInterface for RegionType {
    fn read_u8(&self, offset: usize) -> u8 {
        match self {
            RegionType::Rom { data } => data[offset],
        }
    }

    fn write_u8(&mut self, _offset: usize, _val: u8) {
        match self {
            RegionType::Rom { data: _ } => {} 
        }
    }
}

struct Region {
    start: usize,
    size: usize,
    region: Box<dyn MemoryMapInterface>,
}

impl<'a> fmt::Debug for Region {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub struct MemoryMap {
    pub ppu: Ppu2c02,
    prg_regions: Vec<Region>,
    chr_regions: Vec<Region>,
}

impl<'a> fmt::Debug for MemoryMap {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl<'a> MemoryMap {
    pub fn new(rom: &Rom, ppu: Ppu2c02) -> MemoryMap {
        let mut mm = MemoryMap {
            ppu: ppu,
            prg_regions: Vec::new(),
            chr_regions: Vec::new(),
        };

        // Build the rest of the memory map based on the mapper value
        match rom.mapper {
            0 => {
                // NROM
                match rom.prg_rom.len() {
                    0x4000 => {
                        mm.prg_regions.push(Region {
                            start: 0x8000,
                            size: 0x4000,
                            region: Box::new(RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            }),
                        });
                        mm.prg_regions.push(Region {
                            start: 0xc000,
                            size: 0x4000,
                            region: Box::new(RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            }),
                        });
                    }
                    0x8000 => {
                        mm.prg_regions.push(Region {
                            start: 0x8000,
                            size: 0x8000,
                            region: Box::new(RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            }),
                        });
                    }
                    _ => println!("Unsupported PRG size {}", rom.prg_rom.len()),
                }

                mm.chr_regions.push(Region {
                    start: 0x0000,
                    size: 0x2000,
                    region: Box::new(RegionType::Rom {
                        data: rom.chr_rom.to_vec(),
                    }),
                });
            }
            4 => {
                // MMC3 Mapper
            }
            _ => {
                println!("Unsupported mapper {}", rom.mapper);
            }
        }

        mm
    }

    pub fn tick_ppu(&mut self, e: &mut VecDeque<Event>) {
        self.ppu.tick(e);
    }

    pub fn cpu_peek_u8(&mut self, address: usize) -> u8 {
        match address {
            0x2000..=0x3fff => {
                // PPU Registers
                self.ppu.peek_reg((address & 0b111) as u8) 
            }
            _ => {
                error!("Memory address not peek-able: {:04x}", address);     
                0
            }
        }
    }

    pub fn cpu_read_u8(&mut self, address: usize) -> u8 {
        match address {
            0x2000..=0x3fff => {
                // PPU Registers
                self.ppu.read_reg((address & 0b111) as u8) 
            }
            0x4000..=0x401f => {
                // IO Registers
                0xff
            }
            _ => {
                for r in &self.prg_regions {
                    if address >= r.start && address < r.start + r.size {
                        return r.region.read_u8(address - r.start);
                    }
                }
                error!("Memory address not mapped: {:04x}", address);     
                return 0;           
            }
        }
    }

    pub fn cpu_write_u8(&mut self, address: usize, val: u8) {
        match address {
            0x2000..=0x3fff => {
                // PPU Registers
                self.ppu.write_reg((address & 0b111) as u8, val);
            }
            0x4000..=0x401f => {
                // IO Registers
                if address == 0x4014 {
                    self.ppu.write_oamdma(val);
                }
            }
            _ => {
                for r in &mut self.prg_regions {
                    if address >= r.start && address < r.start + r.size {
                        return r.region.write_u8(address - r.start, val);
                    }
                }
                error!("Memory address not mapped: 0x{:04x}: {:02x}", address, val);
            }
        }
    }

    #[cfg(test)]
    pub fn new_stub() -> Self {
        MemoryMap {
            prg_regions: Vec::new(),
            chr_regions: Vec::new(),
            // cpu: Cpu6502::new(),
            ppu: Ppu2c02::new(Vec::new()),
        }
    }
}
