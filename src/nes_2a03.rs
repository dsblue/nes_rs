/**
 *
 * 6502 Instruction behavior:
 * http://obelisk.me.uk/6502/reference.html
 *
 * Cycle timing:
 * http://nesdev.com/6502_cpu.txt
 *
 * Used Opcode names from: http://www.oxyron.de/html/opcodes02.html
 */
use std::collections::VecDeque;

use crate::nes_ppu::Event;
use crate::MemoryMap;

const N: u8 = 0b1000_0000; // Negitive
const V: u8 = 0b0100_0000; // Overflow
const D: u8 = 0b0000_1000; // Decimal
const I: u8 = 0b0000_0100; // Interrupt Disable
const Z: u8 = 0b0000_0010; // Zero
const C: u8 = 0b0000_0001; // Carry

// Usage: update_status!(reg_p, (reg == 0), Z);
macro_rules! update_status {
    ($p:expr, $is_set:expr, $flags:expr) => {
        if ($is_set) {
            $p |= ($flags);
        } else {
            $p &= !($flags);
        }
    };
}

// TODO: This can surely be simplified...
macro_rules! stat_v {
    ($p:expr, $reg1:expr, $reg2:expr) => {
        if ($reg1 & 0x80) == 0x80 && ($reg2 & 0x80) == 0x80 {
            if $reg1.wrapping_add($reg2) & 0x80 == 0 {
                $p |= V;
            } else {
                $p &= !V;
            }
        } else if ($reg1 & 0x80) == 0 && ($reg2 & 0x80) == 0 {
            if $reg1.wrapping_add($reg2) & 0x80 == 0x80 {
                $p |= V;
            } else {
                $p &= !V;
            }
        } else {
            $p &= !V;
        }
    };
}

macro_rules! stat_nz {
    ($p:expr, $reg:expr) => {
        if $reg == 0 {
            $p |= Z;
            $p &= !N;
        } else {
            $p &= !Z;
            if ($reg & 0x80) == 0x80 {
                $p |= N;
            } else {
                $p &= !N;
            }
        }
    };
}

#[derive(Debug, Copy, Clone)]
enum AddressMode {
    Imp,
    Imm,
    Zp,
    Zpx,
    Zpy,
    Izx,
    Izy,
    Abs,
    Abx,
    Aby,
    Ind,
}

impl AddressMode {
    fn display(&self, o: u16) -> String {
        use AddressMode::*;

        match *self {
            Imp => format!(""),
            Imm => format!("#${:02x}", o as u8),
            Zp => format!("${:02x}", o as u8),
            Zpx => format!("${:02x},X", o as u8),
            Zpy => format!("${:02x},Y", o as u8),
            Abs => format!("${:04x}", o as u16),
            Abx => format!("${:04x},X", o as u16),
            Aby => format!("${:04x},Y", o as u16),
            Ind => format!("(${:02x})", o as u8),
            Izx => format!("(${:02x},X)", o as u8),
            Izy => format!("(${:02x}),Y", o as u8),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Instruction {
    Adc(AddressMode),
    And(AddressMode),
    Asl(AddressMode),
    Bcc,
    Bcs,
    Beq,
    Bit(AddressMode),
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp(AddressMode),
    Cpx(AddressMode),
    Cpy(AddressMode),
    Dec(AddressMode),
    Dex,
    Dey,
    Eor(AddressMode),
    Inc(AddressMode),
    Inx,
    Iny,
    Jmp(AddressMode),
    Jsr,
    Lda(AddressMode),
    Ldx(AddressMode),
    Ldy(AddressMode),
    Lsr(AddressMode),
    Nop(AddressMode),
    Ora(AddressMode),
    Pha,
    Php,
    Pla,
    Plp,
    Rol(AddressMode),
    Ror(AddressMode),
    Rti,
    Rts,
    Sbc(AddressMode),
    Sec,
    Sed,
    Sei,
    Sta(AddressMode),
    Stx(AddressMode),
    Sty(AddressMode),
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,

    // Invalid Opcodes
    Alr,
    Arr,
    Anc,
    Axs,
    Dcp(AddressMode),
    Isc(AddressMode),
    Kil,
    Las,
    Lax(AddressMode),
    Rla(AddressMode),
    Rra(AddressMode),
    Sax(AddressMode),
    Ahx(AddressMode), // Also known as SHA
    Shx(AddressMode),
    Shy(AddressMode),
    Slo(AddressMode),
    Sre(AddressMode),
    Tas,
    Xaa,

    _Invalid,
    _Unimplemented,
}

impl Instruction {
    fn info(&self, o: u16) -> (&str, String) {
        use AddressMode::*;
        use Instruction::*;

        match *self {
            Adc(m) => ("adc", m.display(o)),
            And(m) => ("and", m.display(o)),
            Asl(m) => ("asl", m.display(o)),
            Bcc => ("bcc", Imm.display(o)),
            Bcs => ("bcs", Imm.display(o)),
            Beq => ("beq", Imm.display(o)),
            Bit(m) => ("bit", m.display(o)),
            Bmi => ("bmi", Imm.display(o)),
            Bne => ("bne", Imm.display(o)),
            Bpl => ("bpl", Imm.display(o)),
            Brk => ("brk", "".to_string()),
            Bvc => ("bvc", Imm.display(o)),
            Bvs => ("bvs", Imm.display(o)),
            Clc => ("clc", "".to_string()),
            Cld => ("cld", "".to_string()),
            Cli => ("cli", "".to_string()),
            Clv => ("clv", "".to_string()),
            Cmp(m) => ("cmp", m.display(o)),
            Cpx(m) => ("cpx", m.display(o)),
            Cpy(m) => ("cpy", m.display(o)),
            Dec(m) => ("dec", m.display(o)),
            Dex => ("dex", "".to_string()),
            Dey => ("dey", "".to_string()),
            Eor(m) => ("eor", m.display(o)),
            Inc(m) => ("inc", m.display(o)),
            Inx => ("inx", "".to_string()),
            Iny => ("iny", "".to_string()),
            Jmp(_m) => ("jmp", Imm.display(o)),
            Jsr => ("jsr", Imm.display(o)),
            Lda(m) => ("lda", m.display(o)),
            Ldx(m) => ("ldx", m.display(o)),
            Ldy(m) => ("ldy", m.display(o)),
            Lsr(m) => ("lsr", m.display(o)),
            Nop(m) => ("nop", m.display(o)),
            Ora(m) => ("ora", m.display(o)),
            Pha => ("pha", "".to_string()),
            Php => ("php", "".to_string()),
            Pla => ("pla", "".to_string()),
            Plp => ("plp", "".to_string()),
            Rol(m) => ("rol", m.display(o)),
            Ror(m) => ("ror", m.display(o)),
            Rti => ("rti", "".to_string()),
            Rts => ("rts", "".to_string()),
            Sbc(m) => ("sbc", m.display(o)),
            Sec => ("sec", "".to_string()),
            Sed => ("sed", "".to_string()),
            Sei => ("sei", "".to_string()),
            Sta(m) => ("sta", m.display(o)),
            Stx(m) => ("stx", m.display(o)),
            Sty(m) => ("sty", m.display(o)),
            Tax => ("tax", "".to_string()),
            Tay => ("tay", "".to_string()),
            Tsx => ("tsx", "".to_string()),
            Txa => ("txa", "".to_string()),
            Txs => ("txs", "".to_string()),
            Tya => ("tya", "".to_string()),

            _ => ("--", "--".to_string()),
        }
    }
}

//#[derive(Debug)]
pub struct Cpu6502 {
    reg_a: u8,   // Accumulator
    reg_x: u8,   // Index X
    reg_y: u8,   // Index Y
    reg_pc: u16, // Program counter
    reg_s: u8,   // Stack pointer
    reg_p: u8,   // Status

    count: u64,

    inst: Instruction,

    nmi_pending: bool,
    irq_pending: bool,

    addr: u16,
    addr_prev: u16,
    ptr: u8,
    value: u8,
    cycle: u8,

    internal_ram: [u8; 2 * 1024],
}

impl Cpu6502 {
    pub fn new() -> Cpu6502 {
        Cpu6502 {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            reg_pc: 0,
            reg_p: 0,
            reg_s: 0,

            count: 0,

            inst: Instruction::Brk,

            nmi_pending: false,
            irq_pending: false,

            addr: 0,
            addr_prev: 0,
            ptr: 0,
            value: 0,
            cycle: 1,

            internal_ram: [0u8; 2 * 1024],
        }
    }

    pub fn reset(&mut self, mm: &MemoryMap) {
        info!("Reset CPU");

        self.reg_p |= I;

        self.reg_s = self.reg_s.wrapping_sub(3);

        // Load the reset vector
        self.reg_pc = (self.read_u8(mm, 0xfffd) as u16) << 8 | self.read_u8(mm, 0xfffc) as u16;
    }

    pub fn power_on_reset(&mut self, mm: &MemoryMap) {
        info!("Power Cycle CPU");

        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.reg_p = 0x34;

        self.reg_s = 0xfd;

        // Load the reset vector
        self.reg_pc = (self.read_u8(mm, 0xfffd) as u16) << 8 | self.read_u8(mm, 0xfffc) as u16;
    }

    pub fn _irq(&mut self) {
        info!("IRQ");
        if (self.reg_p & I) == 0 {
            self.irq_pending = true;
        }
    }

    pub fn nmi(&mut self) {
        info!("NMI");
        self.nmi_pending = true;
    }

    pub fn tick(&mut self, mm: &mut MemoryMap, _e: &mut VecDeque<Event>) {
        use Instruction::*;

        match self.cycle {
            1 => {
                // Check for interrupts
                if self.irq_pending {
                    self.irq_pending = false;
                    self.reg_pc =
                        (self.read_u8(mm, 0xffff) as u16) << 8 | self.read_u8(mm, 0xfffe) as u16;
                }

                if self.nmi_pending {
                    self.nmi_pending = false;
                    self.reg_pc =
                        (self.read_u8(mm, 0xfffb) as u16) << 8 | self.read_u8(mm, 0xfffa) as u16;
                }

                // Fetch decode
                let op = self.read_u8(mm, self.reg_pc as usize);
                self.inst = self.decode_op(op);

                // Disassembly info for debug
                let next_mem = (self.read_u8(mm, self.reg_pc as usize + 2) as u16) << 8
                    | self.read_u8(mm, self.reg_pc as usize + 1) as u16;

                let (name, operand) = self.inst.info(next_mem);

                println!(
                    "{:>8}  {:04x}: {} {:<8}\t{} A:{:02x} X:{:02x} Y:{:02x} SP:{:04x} => {}",
                    self.count,
                    self.reg_pc,
                    name.to_ascii_uppercase(),
                    operand,
                    self.status_as_string(),
                    self.reg_a,
                    self.reg_x,
                    self.reg_y,
                    self.reg_s as u16 + 0x0100,
                    self.stack_as_string(mm),
                );

                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle += 1;
            }
            _ => match self.inst {
                Clc => self.ex_clc(),
                Cld => self.ex_cld(),
                Cli => self.ex_cli(),

                Sec => self.ex_sec(),
                Sed => self.ex_sed(),
                Sei => self.ex_sei(),

                Ahx(m) => self.ex_ahx(mm, m),
                Shx(m) => self.ex_shx(mm, m),
                Shy(m) => self.ex_shy(mm, m),
                Sax(m) => self.ex_sax(mm, m),

                Sta(m) => self.ex_sta(mm, m),
                Stx(m) => self.ex_stx(mm, m),
                Sty(m) => self.ex_sty(mm, m),

                Lda(m) => self.ex_lda(mm, m),
                Ldx(m) => self.ex_ldx(mm, m),
                Ldy(m) => self.ex_ldy(mm, m),

                Tax => self.ex_tax(),
                Tay => self.ex_tay(),
                Tsx => self.ex_tsx(),
                Txa => self.ex_txa(),
                Txs => self.ex_txs(),
                Tya => self.ex_tya(),

                Dec(m) => self.ex_dec(mm, m),
                Dex => self.ex_dex(),
                Dey => self.ex_dey(),
                Inc(m) => self.ex_inc(mm, m),
                Inx => self.ex_inx(),
                Iny => self.ex_iny(),

                Eor(m) => self.ex_eor(mm, m),
                Ora(m) => self.ex_ora(mm, m),
                And(m) => self.ex_and(mm, m),
                Adc(m) => self.ex_adc(mm, m),
                Sbc(m) => self.ex_sbc(mm, m),
                Cmp(m) => self.ex_cmp(mm, m),
                Cpx(m) => self.ex_cpx(mm, m),
                Cpy(m) => self.ex_cpy(mm, m),
                Bit(m) => self.ex_bit(mm, m),
                Lax(m) => self.ex_lax(mm, m),
                Nop(m) => self.ex_nop(mm, m),

                Lsr(m) => self.ex_lsr(mm, m),

                Bcc => self.ex_bcc(mm),
                Bcs => self.ex_bcs(mm),
                Beq => self.ex_beq(mm),
                Bmi => self.ex_bmi(mm),
                Bne => self.ex_bne(mm),
                Bpl => self.ex_bpl(mm),
                Bvc => self.ex_bvc(mm),
                Bvs => self.ex_bvs(mm),

                Jmp(m) => self.ex_jmp(mm, m),
                Jsr => self.ex_jsr(mm),
                Rts => self.ex_rts(mm),

                _ => {
                    panic!("Unknown instruction: {:?}", self.inst);
                }
            },
        }

        self.count += 1;
    }

    fn read_u8(&self, mm: &MemoryMap, addr: usize) -> u8 {
        match addr {
            0x0000..=0x1fff => {
                // 2KB internal RAM mirrored x 4
                self.internal_ram[0x07ff & addr]
            }
            0x2000..=0x3fff => {
                // PPU Registers
                mm.ppu.reg_read((addr & 0b111) as u8)
            }
            0x4000..=0x401f => {
                // NES APU and IO registers
                error!("APU Not implemented: Read APU:0x{:04x}", (addr - 0x4000));
                (addr - 0x4000) as u8
            }
            _ => mm.cpu_read_u8(addr),
        }
    }

    fn write_u8(&mut self, mm: &mut MemoryMap, addr: usize, val: u8) {
        match addr {
            0x0000..=0x1fff => {
                // 2KB internal RAM mirrored x 4
                self.internal_ram[0x07ff & addr] = val;
            }
            0x2000..=0x3fff => {
                // PPU Registers
                mm.ppu.reg_write((addr & 0b111) as u8, val);
            }
            0x4000..=0x401f => {
                // NES APU and IO registers
                error!(
                    "APU Not implemented: Write APU:0x{:04x}: 0x{:02x}",
                    (addr - 0x4000),
                    val
                );
            }
            _ => mm.cpu_write_u8(addr, val),
        }
    }

    fn decode_op(&mut self, op: u8) -> Instruction {
        use AddressMode::*;
        use Instruction::*;

        match op {
            0x00 => Brk,
            0x01 => Ora(Izx),
            0x02 => Kil,
            0x03 => Slo(Izx),
            0x04 => Nop(Zp),
            0x05 => Ora(Zp),
            0x06 => Asl(Zp),
            0x07 => Slo(Zp),
            0x08 => Php,
            0x09 => Ora(Imm),
            0x0a => Asl(Imp),
            0x0b => Anc,
            0x0c => Nop(Abs),
            0x0d => Ora(Abs),
            0x0e => Asl(Abs),
            0x0f => Slo(Abs),
            0x10 => Bpl,
            0x11 => Ora(Izy),
            0x12 => Kil,
            0x13 => Slo(Izy),
            0x14 => Nop(Zpx),
            0x15 => Ora(Zpx),
            0x16 => Asl(Zpx),
            0x17 => Slo(Zpx),
            0x18 => Clc,
            0x19 => Ora(Aby),
            0x1a => Nop(Imp),
            0x1b => Slo(Aby),
            0x1c => Nop(Abx),
            0x1d => Ora(Abx),
            0x1e => Asl(Abx),
            0x1f => Slo(Abx),
            0x20 => Jsr,
            0x21 => And(Izx),
            0x22 => Kil,
            0x23 => Rla(Izx),
            0x24 => Bit(Zp),
            0x25 => And(Zp),
            0x26 => Rol(Zp),
            0x27 => Rla(Zp),
            0x28 => Plp,
            0x29 => And(Imm),
            0x2a => Rol(Imp),
            0x2b => Anc,
            0x2c => Bit(Abs),
            0x2d => And(Abs),
            0x2e => Rol(Abs),
            0x2f => Rla(Abs),
            0x30 => Bmi,
            0x31 => And(Izy),
            0x32 => Kil,
            0x33 => Rla(Izy),
            0x34 => Nop(Zpx),
            0x35 => And(Zpx),
            0x36 => Rol(Zpx),
            0x37 => Rla(Zpx),
            0x38 => Sec,
            0x39 => And(Aby),
            0x3a => Nop(Imp),
            0x3b => Rla(Aby),
            0x3c => Nop(Abx),
            0x3d => And(Abx),
            0x3e => Rol(Abx),
            0x3f => Rla(Abx),
            0x40 => Rti,
            0x41 => Eor(Izx),
            0x42 => Kil,
            0x43 => Sre(Izx),
            0x44 => Nop(Zp),
            0x45 => Eor(Zp),
            0x46 => Lsr(Zp),
            0x47 => Sre(Zp),
            0x48 => Pha,
            0x49 => Eor(Imm),
            0x4a => Lsr(Imp),
            0x4b => Alr,
            0x4c => Jmp(Abs),
            0x4d => Eor(Abs),
            0x4e => Lsr(Abs),
            0x4f => Sre(Abs),
            0x50 => Bvc,
            0x51 => Eor(Izy),
            0x52 => Kil,
            0x53 => Sre(Izy),
            0x54 => Nop(Zpx),
            0x55 => Eor(Zpx),
            0x56 => Lsr(Zpx),
            0x57 => Sre(Zpx),
            0x58 => Cli,
            0x59 => Eor(Aby),
            0x5a => Nop(Imp),
            0x5b => Sre(Aby),
            0x5c => Nop(Abx),
            0x5d => Eor(Abx),
            0x5e => Lsr(Abx),
            0x5f => Sre(Abx),
            0x60 => Rts,
            0x61 => Adc(Izx),
            0x62 => Kil,
            0x63 => Rra(Izx),
            0x64 => Nop(Zp),
            0x65 => Adc(Zp),
            0x66 => Ror(Zp),
            0x67 => Rra(Zp),
            0x68 => Pla,
            0x69 => Adc(Imm),
            0x6a => Ror(Imp),
            0x6b => Arr,
            0x6c => Jmp(Ind),
            0x6d => Adc(Abs),
            0x6e => Ror(Abs),
            0x6f => Rra(Abs),
            0x70 => Bvs,
            0x71 => Adc(Izy),
            0x72 => Kil,
            0x73 => Rra(Izy),
            0x74 => Nop(Zpx),
            0x75 => Adc(Zpx),
            0x76 => Ror(Zpx),
            0x77 => Rra(Zpx),
            0x78 => Sei,
            0x79 => Adc(Aby),
            0x7a => Nop(Imp),
            0x7b => Rra(Aby),
            0x7c => Nop(Abx),
            0x7d => Adc(Abx),
            0x7e => Ror(Abx),
            0x7f => Rra(Abx),
            0x80 => Nop(Imp),
            0x81 => Sta(Izx),
            0x82 => Nop(Imm),
            0x83 => Sax(Izx),
            0x84 => Sty(Zp),
            0x85 => Sta(Zp),
            0x86 => Stx(Zp),
            0x87 => Sax(Zp),
            0x88 => Dey,
            0x89 => Nop(Imm),
            0x8a => Txa,
            0x8b => Xaa,
            0x8c => Sty(Abs),
            0x8d => Sta(Abs),
            0x8e => Stx(Abs),
            0x8f => Sax(Abs),
            0x90 => Bcc,
            0x91 => Sta(Izy),
            0x92 => Kil,
            0x93 => Ahx(Izy), // AKA: Sha
            0x94 => Sty(Zpx),
            0x95 => Sta(Zpx),
            0x96 => Stx(Zpy),
            0x97 => Sax(Zpy),
            0x98 => Tya,
            0x99 => Sta(Aby),
            0x9a => Txs,
            0x9b => Tas,
            0x9c => Shy(Abx),
            0x9d => Sta(Abx),
            0x9e => Shx(Abx),
            0x9f => Ahx(Abx), // AKA: Sha
            0xa0 => Ldy(Imm),
            0xa1 => Lda(Izx),
            0xa2 => Ldx(Imm),
            0xa3 => Lax(Izx),
            0xa4 => Ldy(Zp),
            0xa5 => Lda(Zp),
            0xa6 => Ldx(Zp),
            0xa7 => Lax(Zp),
            0xa8 => Tay,
            0xa9 => Lda(Imm),
            0xaa => Tax,
            0xab => Lax(Imm),
            0xac => Ldy(Abs),
            0xad => Lda(Abs),
            0xae => Ldx(Abs),
            0xaf => Lax(Abs),
            0xb0 => Bcs,
            0xb1 => Lda(Izy),
            0xb2 => Kil,
            0xb3 => Lax(Izy),
            0xb4 => Ldy(Zpx),
            0xb5 => Lda(Zpx),
            0xb6 => Ldx(Zpy),
            0xb7 => Lax(Zpy),
            0xb8 => Clv,
            0xb9 => Lda(Aby),
            0xba => Tsx,
            0xbb => Las,
            0xbc => Ldy(Abx),
            0xbd => Lda(Abx),
            0xbe => Ldx(Aby),
            0xbf => Lax(Aby),
            0xc0 => Cpy(Imm),
            0xc1 => Cmp(Izx),
            0xc2 => Nop(Imm),
            0xc3 => Dcp(Izx),
            0xc4 => Cpy(Zp),
            0xc5 => Cmp(Zp),
            0xc6 => Dec(Zp),
            0xc7 => Dcp(Zp),
            0xc8 => Iny,
            0xc9 => Cmp(Imm),
            0xca => Dex,
            0xcb => Axs,
            0xcc => Cpy(Abs),
            0xcd => Cmp(Abs),
            0xce => Dec(Abs),
            0xcf => Dcp(Abs),
            0xd0 => Bne,
            0xd1 => Cmp(Izy),
            0xd2 => Kil,
            0xd3 => Dcp(Izy),
            0xd4 => Nop(Zpx),
            0xd5 => Cmp(Zpx),
            0xd6 => Dec(Zpx),
            0xd7 => Dcp(Zpx),
            0xd8 => Cld,
            0xd9 => Cmp(Aby),
            0xda => Nop(Imp),
            0xdb => Dcp(Aby),
            0xdc => Nop(Abx),
            0xdd => Cmp(Abx),
            0xde => Dec(Abx),
            0xdf => Dcp(Abx),
            0xe0 => Cpx(Imm),
            0xe1 => Sbc(Izx),
            0xe2 => Nop(Imm),
            0xe3 => Isc(Izx),
            0xe4 => Cpx(Zp),
            0xe5 => Sbc(Zp),
            0xe6 => Inc(Zp),
            0xe7 => Isc(Zp),
            0xe8 => Inx,
            0xe9 => Sbc(Imm),
            0xea => Nop(Imp),
            0xeb => Sbc(Imm),
            0xec => Cpx(Abs),
            0xed => Sbc(Abs),
            0xee => Inc(Abs),
            0xef => Isc(Abs),
            0xf0 => Beq,
            0xf1 => Sbc(Izy),
            0xf2 => Kil,
            0xf3 => Isc(Izy),
            0xf4 => Nop(Zpx),
            0xf5 => Sbc(Zpx),
            0xf6 => Inc(Zpx),
            0xf7 => Isc(Zpx),
            0xf8 => Sed,
            0xf9 => Sbc(Aby),
            0xfa => Nop(Imp),
            0xfb => Isc(Aby),
            0xfc => Nop(Abx),
            0xfd => Sbc(Abx),
            0xfe => Inc(Abx),
            0xff => Isc(Abx),
        }
    }

    fn stack_as_string(&self, mm: &mut MemoryMap) -> String {
        format!(
            "[{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}]",
            self.read_u8(mm, self.reg_s as usize + 0x101),
            self.read_u8(mm, self.reg_s as usize + 0x102),
            self.read_u8(mm, self.reg_s as usize + 0x103),
            self.read_u8(mm, self.reg_s as usize + 0x104),
            self.read_u8(mm, self.reg_s as usize + 0x105),
            self.read_u8(mm, self.reg_s as usize + 0x106),
            self.read_u8(mm, self.reg_s as usize + 0x107),
            self.read_u8(mm, self.reg_s as usize + 0x108),
        )
    }

    fn status_as_string(&self) -> String {
        format!(
            "[{}{}..{}{}{}{}]",
            if self.reg_p & N == N { "N" } else { "n" },
            if self.reg_p & V == V { "V" } else { "v" },
            if self.reg_p & D == D { "D" } else { "d" },
            if self.reg_p & I == I { "I" } else { "i" },
            if self.reg_p & Z == Z { "Z" } else { "z" },
            if self.reg_p & C == C { "C" } else { "c" },
        )
    }

    // Handle branch instructions that use relative addressing
    fn handle_branch(&mut self, mm: &mut MemoryMap) {
        match self.cycle {
            2 => {
                // Fetch operand, increment PC
                self.addr_prev = self.reg_pc.wrapping_sub(1); // Save the current PC
                self.value = self.read_u8(mm, self.reg_pc as usize);
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle += 1;
            }
            3 => {
                let value = ((self.value as i8) as i16) as u16;
                self.addr = self.reg_pc.wrapping_add(value);
                self.cycle += 1;
            }
            4 => {
                // If branch is taken, this cycle executes
                if (self.addr_prev & 0xff00) == (self.addr & 0xff00) {
                    self.cycle = 1;
                } else {
                    self.cycle += 1;
                }
            }
            5 => {
                // If branch crosses pages, this cycle executes
                self.cycle = 1;
            }
            _ => (),
        }
    }

    fn handle_read_modify_write(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        let pc = self.reg_pc as usize;
        let addr = self.addr as usize;

        match m {
            AddressMode::Imp => {
                self.value = self.reg_a;
                self.cycle = 1;
            }
            AddressMode::Zp => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.value = self.read_u8(mm, addr);
                    self.cycle += 1;
                }
                4 => {
                    self.cycle += 1;
                }
                5 => self.cycle = 1,
                _ => (),
            },
            AddressMode::Zpx => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.read_u8(mm, addr) + self.reg_x) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.read_u8(mm, addr & 0xff);
                    self.cycle += 1;
                }
                5 => {
                    self.cycle += 1;
                }
                6 => {
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Abs => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.read_u8(mm, addr);
                    self.cycle += 1;
                }
                5 => {
                    self.cycle += 1;
                }
                6 => {
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Abx => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = self.addr.wrapping_add(self.reg_x as u16);
                    self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.read_u8(mm, addr);
                    self.cycle += 1;
                }
                5 => {
                    self.cycle += 1;
                }
                6 => {
                    self.cycle += 1;
                }
                7 => {
                    self.cycle = 1;
                }
                _ => (),
            },
            _ => panic!("Invalid mode: {:?}", m),
        }
    }

    fn handle_read(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        let pc = self.reg_pc as usize;
        let addr = self.addr as usize;
        let ptr = self.ptr as usize;

        match m {
            AddressMode::Imp => {
                self.value = self.read_u8(mm, pc);
                self.cycle = 1;
            }
            AddressMode::Imm => {
                self.value = self.read_u8(mm, pc);
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle = 1;
            }
            AddressMode::Abs => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.read_u8(mm, addr);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Abx => {
                match self.cycle {
                    2 => {
                        self.addr = self.read_u8(mm, pc) as u16;
                        self.reg_pc = self.reg_pc.wrapping_add(1);
                        self.cycle += 1;
                    }
                    3 => {
                        self.addr = self.addr + self.reg_x as u16;
                        self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                        self.reg_pc = self.reg_pc.wrapping_add(1);
                        self.cycle += 1;
                    }
                    4 => {
                        self.value = self.read_u8(mm, addr);
                        self.cycle += 1; // TODO: Handle the 4 cycle case
                    }
                    5 => {
                        self.value = self.read_u8(mm, addr);
                        self.cycle = 1;
                    }
                    _ => (),
                }
            }
            AddressMode::Aby => {
                match self.cycle {
                    2 => {
                        self.addr = self.read_u8(mm, pc) as u16;
                        self.reg_pc = self.reg_pc.wrapping_add(1);
                        self.cycle += 1;
                    }
                    3 => {
                        self.addr = self.addr + self.reg_y as u16;
                        self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                        self.reg_pc = self.reg_pc.wrapping_add(1);
                        self.cycle += 1;
                    }
                    4 => {
                        self.value = self.read_u8(mm, addr);
                        self.cycle += 1; // TODO: Handle the 4 cycle case
                    }
                    5 => {
                        self.value = self.read_u8(mm, addr);
                        self.cycle = 1;
                    }
                    _ => (),
                }
            }
            AddressMode::Izx => match self.cycle {
                2 => {
                    self.ptr = self.read_u8(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.read_u8(mm, ptr);
                    self.ptr = self.ptr.wrapping_add(self.reg_x);
                    self.cycle += 1;
                }
                4 => {
                    self.addr = self.read_u8(mm, ptr) as u16;
                    self.cycle += 1;
                }
                5 => {
                    // This MAY overflow, but ignore for now
                    self.addr |= (self.read_u8(mm, ptr + 1) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.value = self.read_u8(mm, addr);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izy => match self.cycle {
                2 => {
                    self.ptr = self.read_u8(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.read_u8(mm, ptr);
                    self.ptr = self.ptr.wrapping_add(self.reg_y);
                    self.cycle += 1;
                }
                4 => {
                    self.addr = self.read_u8(mm, ptr) as u16;
                    self.cycle += 1;
                }
                5 => {
                    // This MAY overflow, but ignore for now
                    self.addr |= (self.read_u8(mm, ptr + 1) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.value = self.read_u8(mm, addr);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zp => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.value = self.read_u8(mm, addr);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpx => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.read_u8(mm, addr).wrapping_add(self.reg_x)) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.read_u8(mm, addr & 0xff);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpy => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.read_u8(mm, addr).wrapping_add(self.reg_y)) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.read_u8(mm, addr & 0xff);
                    self.cycle = 1;
                }
                _ => (),
            },
            _ => panic!("Invalid mode: {:?}", m),
        }
    }

    fn handle_write(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        let pc = self.reg_pc as usize;
        let addr = self.addr as usize;
        let ptr = self.ptr as usize;
        let value = self.value;

        match m {
            AddressMode::Abs => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.write_u8(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Abx => {
                match self.cycle {
                    2 => {
                        self.addr = self.read_u8(mm, pc) as u16;
                        self.reg_pc = self.reg_pc.wrapping_add(1);
                        self.cycle += 1;
                    }
                    3 => {
                        self.addr = self.addr.wrapping_add(self.reg_x as u16);
                        self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                        self.reg_pc = self.reg_pc.wrapping_add(1);
                        self.cycle += 1;
                    }
                    4 => {
                        self.value = self.read_u8(mm, addr); // Value is ignored
                        self.cycle += 1;
                    }
                    5 => {
                        self.write_u8(mm, addr, value);
                        self.cycle = 1;
                    }
                    _ => (),
                }
            }
            AddressMode::Aby => {
                match self.cycle {
                    2 => {
                        self.addr = self.read_u8(mm, pc) as u16;
                        self.reg_pc = self.reg_pc.wrapping_add(1);
                        self.cycle += 1;
                    }
                    3 => {
                        self.addr = self.addr.wrapping_add(self.reg_y as u16);
                        self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                        self.reg_pc = self.reg_pc.wrapping_add(1);
                        self.cycle += 1;
                    }
                    4 => {
                        self.value = self.read_u8(mm, addr); // Value is ignored
                        self.cycle += 1;
                    }
                    5 => {
                        self.write_u8(mm, addr, value);
                        self.cycle = 1;
                    }
                    _ => (),
                }
            }
            AddressMode::Izx => match self.cycle {
                2 => {
                    self.ptr = self.read_u8(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.read_u8(mm, ptr);
                    self.cycle += 1;
                }
                4 => {
                    self.addr = self.read_u8(mm, ptr) as u16;
                    self.cycle += 1;
                }
                5 => {
                    self.addr |= (self.read_u8(mm, ptr + 1) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.write_u8(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izy => match self.cycle {
                2 => {
                    self.ptr = self.read_u8(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.read_u8(mm, ptr);
                    self.cycle += 1;
                }
                4 => {
                    self.addr = self.read_u8(mm, ptr) as u16;
                    self.cycle += 1;
                }
                5 => {
                    self.addr |= (self.read_u8(mm, ptr + 1) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.write_u8(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zp => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.write_u8(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpx => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.read_u8(mm, addr).wrapping_add(self.reg_x)) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.write_u8(mm, addr & 0xff, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpy => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.read_u8(mm, addr).wrapping_add(self.reg_y)) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.write_u8(mm, addr & 0xff, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            _ => panic!("Invalid mode: {:?}", m),
        }
    }

    // Start specific Instruction handlers

    fn ex_clc(&mut self) {
        self.reg_p &= !C;
        self.cycle = 1;
    }

    fn ex_cld(&mut self) {
        self.reg_p &= !D;
        self.cycle = 1;
    }

    fn ex_cli(&mut self) {
        self.reg_p &= !I;
        self.cycle = 1;
    }

    fn ex_sec(&mut self) {
        self.reg_p |= C;
        self.cycle = 1;
    }

    fn ex_sed(&mut self) {
        self.reg_p |= D;
        self.cycle = 1;
    }

    fn ex_sei(&mut self) {
        self.reg_p |= I;
        self.cycle = 1;
    }

    fn ex_bcc(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & C) == 0 {
                self.reg_pc = self.addr;
            } else {
                self.cycle = 1;
            }
        }
    }

    fn ex_bcs(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & C) == C {
                self.reg_pc = self.addr;
            } else {
                self.cycle = 1;
            }
        }
    }

    fn ex_beq(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & Z) == Z {
                self.reg_pc = self.addr;
            } else {
                self.cycle = 1;
            }
        }
    }

    fn ex_bmi(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & N) == N {
                self.reg_pc = self.addr;
            } else {
                self.cycle = 1;
            }
        }
    }

    fn ex_bne(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & Z) == 0 {
                self.reg_pc = self.addr;
            } else {
                self.cycle = 1;
            }
        }
    }

    fn ex_bpl(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & N) == 0 {
                self.reg_pc = self.addr;
            } else {
                self.cycle = 1;
            }
        }
    }

    fn ex_bvc(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & Z) == 0 {
                self.reg_pc = self.addr;
            } else {
                self.cycle = 1;
            }
        }
    }

    fn ex_bvs(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & V) == V {
                self.reg_pc = self.addr;
            } else {
                self.cycle = 1;
            }
        }
    }

    // RTS Return from subroutine
    fn ex_rts(&mut self, mm: &mut MemoryMap) {
        let s = self.reg_s as usize + 0x100;

        match self.cycle {
            2 => {
                self.cycle += 1;
            }
            3 => {
                self.reg_s = self.reg_s.wrapping_add(1);
                self.cycle += 1;
            }
            4 => {
                self.reg_pc = self.read_u8(mm, s) as u16;
                self.reg_s = self.reg_s.wrapping_add(1);
                self.cycle += 1;
            }
            5 => {
                self.reg_pc |= (self.read_u8(mm, s) as u16) << 8;
                self.cycle += 1;
            }
            6 => {
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle = 1;
            }
            _ => (),
        }
    }

    // JSR Jump to subroutine
    fn ex_jsr(&mut self, mm: &mut MemoryMap) {
        let pc = self.reg_pc as usize;
        let addr_prev = self.addr_prev as usize;
        let s = self.reg_s as usize + 0x100;

        match self.cycle {
            2 => {
                self.addr = self.read_u8(mm, pc) as u16;
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle += 1;
            }
            3 => {
                self.addr_prev = self.reg_pc;
                self.cycle += 1;
            }
            4 => {
                self.write_u8(mm, s, (addr_prev >> 8 & 0xff) as u8);
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle += 1;
            }
            5 => {
                self.write_u8(mm, s, (addr_prev & 0xff) as u8);
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle += 1;
            }
            6 => {
                self.addr |= (self.read_u8(mm, pc) as u16) << 8;
                self.reg_pc = self.addr;
                self.cycle = 1;
            }
            _ => (),
        }
    }

    // JMP Jump to address
    fn ex_jmp(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        let pc = self.reg_pc as usize;
        let ptr = self.ptr as usize;
        let addr = self.addr as usize;

        match m {
            AddressMode::Abs => match self.cycle {
                2 => {
                    self.addr = self.read_u8(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.reg_pc = ((self.read_u8(mm, pc) as u16) << 8) | self.addr;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Ind => match self.cycle {
                2 => {
                    self.ptr = self.read_u8(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.read_u8(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.reg_pc = self.read_u8(mm, addr | ptr) as u16;
                    self.ptr = self.ptr.wrapping_add(1);
                    self.cycle += 1;
                }
                5 => {
                    self.reg_pc |= (self.read_u8(mm, addr | ptr) as u16) << 8;
                    self.cycle = 1;
                }
                _ => (),
            },
            _ => panic!("Invalid mode: {:?}", m),
        }
    }

    // DEC Decrement memory
    fn ex_dec(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value.wrapping_sub(1);

            self.write_u8(mm, self.addr as usize, t);

            // Update N and Z flags
            stat_nz!(self.reg_p, t);
        }
    }

    // DEX Decrement X
    fn ex_dex(&mut self) {
        self.reg_x = self.reg_x.wrapping_sub(1);
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_x);
    }

    // DEY Decrement Y
    fn ex_dey(&mut self) {
        self.reg_y = self.reg_y.wrapping_sub(1);
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_y);
    }

    // INC Increment memory
    fn ex_inc(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value.wrapping_add(1);

            self.write_u8(mm, self.addr as usize, t);

            // Update N and Z flags
            stat_nz!(self.reg_p, t);
        }
    }

    // LSR Logical Shift Right
    fn ex_lsr(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value >> 1;

            if let AddressMode::Imp = m {
                self.reg_a = t;
            } else {
                self.write_u8(mm, self.addr as usize, t);
            }

            // Update C flag
            update_status!(self.reg_p, (self.value & 1) == 1, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, t);
        }
    }

    // INX Increment X
    fn ex_inx(&mut self) {
        self.reg_x = self.reg_x.wrapping_add(1);
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_x);
    }

    // INY Increment Y
    fn ex_iny(&mut self) {
        self.reg_y = self.reg_y.wrapping_add(1);
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_y);
    }

    // TAX Transfer A to X
    fn ex_tax(&mut self) {
        self.reg_x = self.reg_a;
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_x);
    }

    // TAY Transfer A to Y
    fn ex_tay(&mut self) {
        self.reg_y = self.reg_a;
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_y);
    }

    // TSX Transfer S to X
    fn ex_tsx(&mut self) {
        self.reg_x = self.reg_s;
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_x);
    }

    // TXA Transfer X to A
    fn ex_txa(&mut self) {
        self.reg_a = self.reg_x;
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_a);
    }

    // TXS Transfer X to stack pointer
    fn ex_txs(&mut self) {
        self.reg_s = self.reg_x;
        self.cycle = 1;
    }

    // TYA Transfer Y to A
    fn ex_tya(&mut self) {
        self.reg_a = self.reg_y;
        self.cycle = 1;

        // Update N and Z flags
        stat_nz!(self.reg_p, self.reg_a);
    }

    // LDA Load to A
    fn ex_lda(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_a = self.value;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // LDX Load to X
    fn ex_ldx(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_x = self.value;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_x);
        }
    }

    // LDY Load to Y
    fn ex_ldy(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_y = self.value;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_y);
        }
    }

    // EOR A = A ^ M
    fn ex_eor(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_a = self.value ^ self.reg_a;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // AND A = A & M
    fn ex_and(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_a = self.value & self.reg_a;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // ORA A = A | M
    fn ex_ora(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_a = self.value | self.reg_a;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // ADC Add with carry A = A + M + C
    fn ex_adc(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            let t = self.reg_a as u16 + self.value as u16 + (self.reg_p & C == C) as u16;

            self.reg_a = (t & 0xff) as u8;

            // Update C flag
            update_status!(self.reg_p, (t & 0x100) == 0x100, C);

            // Update V flag
            stat_v!(self.reg_p, self.reg_a, self.value);

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    fn ex_sbc(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            let t = self.reg_a as u16 - self.value as u16 - 1 + (self.reg_p & C == C) as u16;

            self.reg_a = (t & 0xff) as u8;

            // Update C flag
            update_status!(self.reg_p, (t & 0x100) == 0x100, C);

            // Update V flag
            stat_v!(self.reg_p, self.reg_a, self.value);

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    fn ex_cmp(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            // Update C flag
            update_status!(self.reg_p, self.reg_a >= self.value, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a.wrapping_sub(self.value));
        }
    }

    fn ex_cpx(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            // Update C flag
            update_status!(self.reg_p, self.reg_x >= self.value, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_x.wrapping_sub(self.value));
        }
    }

    fn ex_cpy(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            // Update C flag
            update_status!(self.reg_p, self.reg_y >= self.value, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_y.wrapping_sub(self.value));
        }
    }

    fn ex_bit(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            let t = self.value & self.reg_a;

            update_status!(self.reg_p, (t == 0), Z);

            update_status!(self.reg_p, (self.value & 0x40) == 0x40, V);

            update_status!(self.reg_p, (self.value & 0x80) == 0x80, N);
        }
    }

    fn ex_nop(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);
    }

    fn ex_lax(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        info!("Unusual instrction {:?}", self.inst);
        error!("Incomplete");
    }

    // AHX Store A & X & (ADDR_HI + 1) to memory
    fn ex_ahx(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_a & self.reg_x & ((self.addr >> 8) as u8).wrapping_add(1);
        self.handle_write(mm, m);
    }

    // SHX Store X & (ADDR_HI + 1) to memory
    fn ex_shx(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_x & ((self.addr >> 8) as u8).wrapping_add(1);
        self.handle_write(mm, m);
    }

    // SHY Store Y & (ADDR_HI + 1) to memory
    fn ex_shy(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_y & ((self.addr >> 8) as u8).wrapping_add(1);
        self.handle_write(mm, m);
    }

    // SAX Store A & X to memory
    fn ex_sax(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_a & self.reg_x;
        self.handle_write(mm, m);
    }

    // STA Store A to memory
    fn ex_sta(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.value = self.reg_a;
        self.handle_write(mm, m);
    }

    // STX Store X to memory
    fn ex_stx(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.value = self.reg_x;
        self.handle_write(mm, m);
    }

    // STY Store Y to memory
    fn ex_sty(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.value = self.reg_y;
        self.handle_write(mm, m);
    }
}

impl std::fmt::Display for Cpu6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "count: {:08x}\n", self.count)?;
        write!(f, "== Current CPU State ===========================\n")?;
        write!(
            f,
            "A: {:02x}\nX: {:02x} Y: {:02x}\n",
            self.reg_a, self.reg_x, self.reg_y
        )?;
        write!(f, "STACK: {:04x}\n", self.reg_s as u16 + 0x100)?;
        write!(f, "PC:   {:04x}\nSTATUS: {:02x}\n", self.reg_pc, self.reg_p)?;
        write!(f, "================================================")?;
        Ok(())
    }
}
