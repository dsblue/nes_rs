use crate::Rom;
use std::fmt;

#[derive(Debug)]
struct Region {
    start: usize,
    end: usize,
    region: RegionType,
}

#[derive(Debug)]
enum RegionType {
    Rom { data: Vec<u8> },
    Ram { data: Vec<u8> },
}

impl RegionType {
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

    fn _write_u16(&mut self, offset: usize, val: u16) {
        match self {
            RegionType::Rom { data: _ } => {}
            RegionType::Ram { data } => {
                data[offset] = (val >> 8) as u8;
                data[offset + 1] = (val & 0xff) as u8;
            }
        }
    }
}

pub struct MemoryMap {
    internal_ram: [u8; 2 * 1024],

    regions: Vec<Region>,
}

impl fmt::Debug for MemoryMap {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Fill in later
        Ok(())
    }
}

impl MemoryMap {
    pub fn new(rom: &Rom) -> MemoryMap {
        let mut mm = MemoryMap {
            internal_ram: [0u8; 2 * 1024],
            regions: Vec::new(),
        };

        // Build the memory map based on the mapper value
        match rom.mapper {
            0 => {
                // NROM
                match rom.prg_rom.len() {
                    0x4000 => {
                        mm.regions.push(Region {
                            start: 0x8000,
                            end: 0xbfff,
                            region: RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            },
                        });
                        mm.regions.push(Region {
                            start: 0xc000,
                            end: 0xffff,
                            region: RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            },
                        });
                    }
                    0x8000 => {
                        mm.regions.push(Region {
                            start: 0x8000,
                            end: 0xffff,
                            region: RegionType::Rom {
                                data: rom.prg_rom.to_vec(),
                            },
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

        mm
    }

    fn add_ram(&mut self, addr: usize, size: usize) {
        self.regions.push(Region {
            start: addr,
            end: addr + size,
            region: RegionType::Ram {
                data: vec![0; size],
            },
        });
    }

    fn _load_region(&mut self, addr: usize, mem: Vec<u8>) -> Result<(), ()> {
        self.regions.push(Region {
            start: addr,
            end: addr + mem.len(),
            region: RegionType::Ram { data: mem },
        });
        Ok(())
    }

    pub fn read_u8(&self, address: usize) -> u8 {
        match address {
            0x0000..=0x1fff => {
                // 2KB internal RAM mirrored x 4
                self.internal_ram[0x07ff & address]
            }
            0x2000..=0x3fff => {
                // PPU Registers
                (address & 0b111) as u8
            }
            0x4000..=0x401f => {
                // NES APU and IO registers
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
                error!("PPU Not implemented: Write 0x{:04x}: 0x{:02x}", (address & 0b111), val);
            }
            0x4000..=0x401f => {
                // NES APU and IO registers
                error!("APU Not implemented: Write 0x{:04x}: 0x{:02x}", (address - 0x4000), val);
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
                        return r.region._write_u16(address - r.start, val);
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
