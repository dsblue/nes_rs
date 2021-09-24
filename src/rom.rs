use byteorder::BigEndian;
use byteorder::ReadBytesExt;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;

pub const FLAG6_MIRRORING: u8 = 0b00000001;
pub const FLAG6_BATTRAM: u8 = 0b00000010;
pub const FLAG6_TRAINER: u8 = 0b00000100;
pub const FLAG6_VRAM: u8 = 0b00001000;

#[derive(Debug)]
pub struct Rom {
    prg_size: usize,
    chr_size: usize,
    vert_mirror: bool,
    has_prg_ram: bool,
    has_trainer: bool,
    has_vram: bool,
    pub mapper: u8,
    is_pal: bool,

    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
}

impl Rom {
    pub fn from_file(path: &Path) -> io::Result<Rom> {
        let mut file = match File::open(path) {
            Err(file) => panic!("No file {}", file),
            Ok(file) => file,
        };

        // Parse the ROM file
        if 0x4e45531a != file.read_u32::<BigEndian>().unwrap() {
            panic!("File not in iNES format");
        }

        let prg_size = file.read_u8().unwrap() as usize * 16384;
        let chr_size = file.read_u8().unwrap() as usize * 8192;
        let flags_6: u8 = file.read_u8().unwrap();
        let flags_7: u8 = file.read_u8().unwrap();
        let _flags_8: u8 = file.read_u8().unwrap();
        let _flags_9: u8 = file.read_u8().unwrap();
        let flags_10: u8 = file.read_u8().unwrap();

        file.seek(SeekFrom::Current(5))?;

        if (0b00001100 & flags_7) == 0b00001000 {
            panic!("NES 2.0 format not supported");
        }

        let mapper = (0xf0 & flags_6) >> 4 | (flags_7 & 0xf0);

        // Read the trainer if it exists
        if 0b100 & flags_6 != 0 {
            let mut buffer = [0u8; 512];
            file.read(&mut buffer).unwrap();
        }

        let mut prg_rom = vec![0u8; prg_size];
        file.read_exact(&mut prg_rom).unwrap();

        let mut chr_rom = vec![0u8; chr_size];
        file.read_exact(&mut chr_rom).unwrap();

        Ok(Rom {
            prg_size,
            chr_size,
            vert_mirror: FLAG6_MIRRORING & flags_6 != 0,
            has_prg_ram: FLAG6_BATTRAM & flags_6 != 0,
            has_trainer: FLAG6_TRAINER & flags_6 != 0,
            has_vram: FLAG6_VRAM & flags_6 != 0,
            mapper,
            is_pal: 0b11 & flags_10 == 2,
            prg_rom,
            chr_rom,
        })
    }
}

impl std::default::Default for Rom {
    fn default() -> Self {
        Rom {
            prg_size: 0,
            chr_size: 0,
            vert_mirror: false,
            has_prg_ram: false,
            has_trainer: false,
            has_vram: false,
            mapper: 0,
            is_pal: false,
            prg_rom: Vec::new(),
            chr_rom: Vec::new(),
        }
    }
}

impl fmt::Display for Rom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mirror = if self.vert_mirror {
            "VERT Mirror"
        } else {
            "HORIZONTAL Mirror"
        };
        write!(
            f,
            "Program Size:   {:8}\n\
            Character Size: {:8}\n \
            Mirror:         {:}\n \
            Has PRG RAM:    {:?}\n \
            Mapper:         {}\n",
            self.prg_size, self.chr_size, mirror, self.has_prg_ram, self.mapper
        )
    }
}
