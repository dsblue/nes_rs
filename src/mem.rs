use crate::ppu::Ppu2c02Interface;
//use crate::Event;
use crate::Rom;
//use std::collections::VecDeque;
use std::fmt;

pub trait MemoryMapInterface {
    fn read_u8(&self, address: usize) -> u8;
    fn read_u16(&self, offset: usize) -> u16;
    fn write_u8(&mut self, address: usize, value: u8);
    fn write_u16(&mut self, offset: usize, val: u16);
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
            //RegionType::Ram { data } => data[offset],
        }
    }

    fn read_u16(&self, offset: usize) -> u16 {
        match self {
            RegionType::Rom { data } => (data[offset] as u16 | (data[offset + 1] as u16) << 8),
            //RegionType::Ram { data } => (data[offset] as u16 | (data[offset + 1] as u16) << 8),
        }
    }

    fn write_u8(&mut self, _offset: usize, _val: u8) {
        match self {
            RegionType::Rom { data: _ } => {} //RegionType::Ram { data } => {
                                              //   data[offset] = val;
                                              //}
        }
    }

    fn write_u16(&mut self, _offset: usize, _val: u16) {
        match self {
            RegionType::Rom { data: _ } => {} //RegionType::Ram { data } => {
                                              //    data[offset] = (val >> 8) as u8;
                                              //    data[offset + 1] = (val & 0xff) as u8;
                                              //}
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
    prg_regions: Vec<Region>,
    chr_regions: Vec<Region>,
    pub ppu: Ppu2c02Interface,
}

impl<'a> fmt::Debug for MemoryMap {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl<'a> MemoryMap {
    pub fn new(rom: &Rom) -> MemoryMap {
        let mut mm = MemoryMap {
            prg_regions: Vec::new(),
            chr_regions: Vec::new(),
            ppu: Ppu2c02Interface::new(),
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

    pub fn cpu_read_u8(&self, address: usize) -> u8 {
        for r in &self.prg_regions {
            if address >= r.start && address < r.start + r.size {
                return r.region.read_u8(address - r.start);
            }
        }
        error!("Memory address not mapped: {:04x}", address);
        0
    }

    pub fn cpu_write_u8(&mut self, address: usize, val: u8) {
        for r in &mut self.prg_regions {
            if address >= r.start && address < r.start + r.size {
                return r.region.write_u8(address - r.start, val);
            }
        }
        error!("Memory address not mapped: 0x{:04x}: {:02x}", address, val);
    }

    pub fn ppu_read_u8(&self, address: usize) -> u8 {
        for r in &self.chr_regions {
            if address >= r.start && address < r.start + r.size {
                return r.region.read_u8(address - r.start);
            }
        }
        error!("PPU Memory address not mapped: {:04x}", address);
        0
    }

    pub fn ppu_write_u8(&mut self, address: usize, val: u8) {
        for r in &mut self.chr_regions {
            if address >= r.start && address < r.start + r.size {
                return r.region.write_u8(address - r.start, val);
            }
        }
        error!("Memory address not mapped: 0x{:04x}: {:02x}", address, val);
    }
}
