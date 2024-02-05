use crate::ppu::Event;
use crate::ppu::Ppu2c02;
use crate::Rom;
use std::collections::VecDeque;
use std::fmt;

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

    cpu_ram: [u8; 2 * 0x400],
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

            cpu_ram: [0u8; 2 * 1024],

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

    pub fn cpu_read_u8(&mut self, address: usize, peek: bool) -> u8 {
        match address {
            0x0000..=0x1fff => {
                // 2KB internal RAM mirrored x 4
                self.cpu_ram[0x07ff & address]
            }
            0x2000..=0x3fff => {
                // PPU Registers
                if peek {
                    self.ppu.peek_reg((address & 0b111) as u8)
                } else {
                    self.ppu.read_reg((address & 0b111) as u8)
                }
            }
            0x4000..=0x401f => {
                // NES APU and IO registers
                warn!("APU Not implemented: Read APU:0x{:04x}", (address - 0x4000));
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
            0x0000..=0x1fff => {
                // 2KB internal RAM mirrored x 4
                self.cpu_ram[0x07ff & address] = val;
            }
            0x2000..=0x3fff => {
                // PPU Registers
                self.ppu.write_reg((address & 0b111) as u8, val);
            }
            0x4000..=0x401f => {
                // IO Registers
                match address {
                    0x4014 => {
                        info!("write_oamdma PPU: OAMDMA = {:02x}", val);
                        for i in 0..256 {
                            let val = self.cpu_read_u8(i + (val as usize * 0x100) as usize, true);
                            self.ppu.write_oamdma(i, val);
                        };
                    }
                    _ => {
                        // NES APU and IO registers
                        warn!(
                            "APU Not implemented: Write APU:0x{:04x}: 0x{:02x}",
                            (address - 0x4000),
                            val
                        )
                    }
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
    pub fn _set_internal_ram(&mut self, m: Vec<(usize, u8)>) {
        for (addr, val) in m {
            self.cpu_ram[addr] = val;
        }
    }

    #[cfg(test)]
    pub fn _write_internal_ram(&mut self, addr: usize, mem: &[u8]) {
        self.cpu_ram[addr..addr + mem.len()].clone_from_slice(mem);
    }

    #[cfg(test)]
    pub fn new_stub() -> Self {
        MemoryMap {
            prg_regions: Vec::new(),
            chr_regions: Vec::new(),
            cpu_ram: [0u8; 2 * 1024],
            ppu: Ppu2c02::new(Vec::new()),
        }
    }
}
