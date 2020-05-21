use crate::Rom;
use std::fmt;
use std::rc::Rc;

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

pub fn read_u8() -> u8 {
    0
}

#[derive(Debug)]
enum RegionType {
    Rom { data: Vec<u8> },
    Ram { data: Vec<u8> },
}

impl MemoryMapInterface for RegionType {
    fn read_u8(&self, offset: usize) -> u8 {

        match self {
            RegionType::Rom { data } => data[offset],
            RegionType::Ram { data } => data[offset],
        }
    }

    fn read_u16(&self, offset: usize) -> u16 {
        match self {
            RegionType::Rom { data } => (data[offset] as u16 | (data[offset + 1] as u16) << 8),
            RegionType::Ram { data } => (data[offset] as u16 | (data[offset + 1] as u16) << 8),
        }
    }

    fn write_u8(&mut self, offset: usize, val: u8) {
        match self {
            RegionType::Rom { data: _ } => {}
            RegionType::Ram { data } => {
                data[offset] = val;
            }
        }
    }

    fn write_u16(&mut self, offset: usize, val: u16) {
        match self {
            RegionType::Rom { data: _ } => {}
            RegionType::Ram { data } => {
                data[offset] = (val >> 8) as u8;
                data[offset + 1] = (val & 0xff) as u8;
            }
        }
    }
}

struct Region<'a> {
    start: usize,
    end: usize,
    region: &'a mut (dyn MemoryMapInterface),
}

impl<'a> fmt::Debug for Region<'a> {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub struct MemoryMap<'a> {
    internal_ram: [u8; 2 * 1024],

    regions: Vec<Region<'a>>,
}

impl<'a> fmt::Debug for MemoryMap<'a> {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Fill in later
        Ok(())
    }
}

impl<'a> MemoryMap<'a> {
    pub fn new(rom: &Rom) -> MemoryMap {
        let mut mm = MemoryMap {
            internal_ram: [0u8; 2 * 1024],
            regions: Vec::new(),
        };
/*
        // Build the memory map based on the mapper value
        match rom.mapper {
            0 => {
                // NROM
                match rom.prg_rom.len() {
                    0x4000 => {
                        mm.regions.push(Region {
                            start: 0x8000,
                            end: 0xbfff,
                            region: Rc::new(RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            }).clone().as_ref()
                        });
                        mm.regions.push(Region {
                            start: 0xc000,
                            end: 0xffff,
                            region: Rc::new(RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            }).clone().as_ref(),
                        });
                    }
                    0x8000 => {
                        mm.regions.push(Region {
                            start: 0x8000,
                            end: 0xffff,
                            region: Rc::new(RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            }).clone().as_ref(),
                        });
                    }
                    _ => println!("Unsupported PRG size {}", rom.prg_rom.len()),
                }

                mm.add_ram(0x6000, 0x2000);
            }
            4 => {
                // MMC3 Mapper
            }
            _ => {
                println!("Unsupported mapper {}", rom.mapper);
            }
        }
*/
        mm
    }

    fn add_ram(&mut self, addr: usize, size: usize) {
        /*
        self.regions.push(Region {
            start: addr,
            end: addr + size,
            region: Box::new(RegionType::Ram {
                data: Vec::new(),
            }).as_ref(),
        });
        */
    }

    pub fn add_region(&mut self, addr: usize, size: usize, interface: &'a mut impl MemoryMapInterface) {
        self.regions.push(Region {
            start: addr,
            end: addr + size,
            region: interface,
        });
    }

    fn _read_u8<T: MemoryMapInterface> (&self, t: T) -> u8 {
        t.read_u8(0)
    }

    pub fn read_u8(&self, address: usize) -> u8 {
        match address {
            0x0000..=0x1fff => {
                // 2KB internal RAM mirrored x 4
                self.internal_ram[0x07ff & address]
            }
            0x2000..=0x3fff => {
                // PPU Registers
                ppu_read_u8(address & 0b111)
            }
            0x4000..=0x401f => {
                // NES APU and IO registers
                error!(
                    "APU Not implemented: Read APU:0x{:04x}",
                    (address - 0x4000)
                );
                (address - 0x4000) as u8
            }
            /*
            0x4020..=0xffff => {
                // Cartridge space
                Some(0xff)
            }*/
            _ => {
                for r in &self.regions {
                    if address >= r.start && address < r.end {
                        return r.region.read_u8(address - r.start);
                    }
                }
                error!("Memory address not mapped: {:04x}", address);
                0
            }
        }
    }

    pub fn read_u16(&self, address: usize) -> u16 {
        match address {
            0x4020..=0xffff => {
                for r in &self.regions {
                    if address >= r.start && address < r.end {
                        return r.region.read_u16(address - r.start);
                    }
                }
                error!("Memory address not mapped: {:04x}", address);
                0
            }
            _ => {
                error!("Memory address not mapped: {:04x}", address);
                0
            }
        }
    }

    pub fn write_u8(&mut self, address: usize, val: u8) {
        match address {
            0x0000..=0x1fff => {
                // 2KB internal RAM mirrored x 4
                self.internal_ram[0x07ff & address] = val;
            }
            0x2000..=0x3fff => {
                // PPU Registers
                error!(
                    "PPU Not implemented: Write PPU:0x{:04x}: 0x{:02x}",
                    (address & 0b111),
                    val
                );
            }
            0x4000..=0x401f => {
                // NES APU and IO registers
                error!(
                    "APU Not implemented: Write APU:0x{:04x}: 0x{:02x}",
                    (address - 0x4000),
                    val
                );
            }
            0x4020..=0xffff => {
                for r in &mut self.regions {
                    if address >= r.start && address < r.end {
                        return r.region.write_u8(address - r.start, val);
                    }
                }
                error!("Memory address not mapped: 0x{:04x}: {:02x}", address, val);
            }
            _ => {
                error!("Memory address not mapped: 0x{:04x}: {:02x}", address, val);
            }
        }
    }

    pub fn _write_u16(&mut self, address: usize, val: u16) {
        match address {
            0x0000..=0xffff => {
                for r in &mut self.regions {
                    if address >= r.start && address < r.end {
                        return r.region.write_u16(address - r.start, val);
                    }
                }
                error!("Memory address not mapped: {:04x}", address);
            }
            _ => {
                error!("Memory address not mapped: {:04x}", address);
            }
        }
    }
}

fn ppu_read_u8(addr: usize) -> u8 {

    let value = match addr {
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

    trace!("PPU Read 0x{:04x}: 0x{:02x}", addr, value);

    value
}
