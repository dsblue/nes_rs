/**
 *
 * NES CPU / APU Emulation Model
 *
 * This model attempts to provide a cycle accurate emulation of the 2A03 CPU
 * used in the NA NES.  The chip includes a 6502 CPU core, APU for sound
 * generation and some IO ports for controller interaction.
 *
 *
 * 6502 Instruction behavior:
 * http://obelisk.me.uk/6502/reference.html
 *
 * Cycle timing:
 * http://nesdev.com/6502_cpu.txt
 *
 * Used Opcode names from: http://www.oxyron.de/html/opcodes02.html
 */
use crate::prelude::*;

use std::collections::VecDeque;
use std::string::String;

use crate::ppu::Event;
use crate::MemoryMap;

const N: u8 = 0b1000_0000; // Negitive
const V: u8 = 0b0100_0000; // Overflow
const B: u8 = 0b0001_0000; // Break instruction
const D: u8 = 0b0000_1000; // Decimal
const I: u8 = 0b0000_0100; // Interrupt Disable
const Z: u8 = 0b0000_0010; // Zero
const C: u8 = 0b0000_0001; // Carry

const IRQ_VECTOR: usize = 0xfffe;
const NMI_VECTOR: usize = 0xfffa;
const RESET_VECTOR: usize = 0xfffc;

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
    ($p:expr, $reg1:expr, $reg2:expr, $result:expr) => {
        if ($reg1 & 0x80) == 0x80 && ($reg2 & 0x80) == 0x80 {
            if $result & 0x80 == 0 {
                $p |= V;
            } else {
                $p &= !V;
            }
        } else if ($reg1 & 0x80) == 0 && ($reg2 & 0x80) == 0 {
            if $result & 0x80 == 0x80 {
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

macro_rules! read_u8 {
    ($mm:expr, $addr:expr) => {
        $mm.cpu_read_u8($addr, false)
    };
}

macro_rules! peek_u8 {
    ($mm:expr, $addr:expr) => {
        $mm.cpu_read_u8($addr, true)
    };
}

macro_rules! write_u8 {
    ($mm:expr, $addr:expr, $val:expr) => {
        $mm.cpu_write_u8($addr, $val)
    };
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum AddressMode {
    Imp, // Implicit
    Acc, // Accumulator
    Imm, // Immediate
    Adr, // Absolute jump (JSR, JMP)
    Rel, // Relative
    Zp,  // Zero Page
    Zpx, // Zero Page, X indexed
    Zpy, // Zero Page, Y indexed
    Izx, // Indirect, X indexed (aka INX)
    Izy, // Indirect, Y indexed (aka INY)
    Abs, // Absolute
    Abx, // Absolute, X
    Aby, // Absolute, Y
    Ind, // Indirect    (JMP only)
    Err, // Error (Halts)
}

impl AddressMode {
    fn display(&self, o: (u8, u16, u16)) -> String {
        use AddressMode::*;

        match *self {
            Imp => f!(""),
            Acc => f!("A"),
            Imm => f!("#${:02x}", o.0),
            Adr => f!("${:04x}", o.1),
            Rel => f!("${:04x}", o.2),
            Zp => f!("${:02x}", o.0),
            Zpx => f!("${:02x},X", o.0),
            Zpy => f!("${:02x},Y", o.0),
            Abs => f!("${:04x}", o.1),
            Abx => f!("${:04x},X", o.1),
            Aby => f!("${:04x},Y", o.1),
            Ind => f!("(${:04x})", o.1),
            Izx => f!("(${:02x},X)", o.0),
            Izy => f!("(${:02x}),Y", o.0),
            Err => f!(""),
        }
    }

    fn size(&self) -> u8 {
        use AddressMode::*;

        match *self {
            Imp => 1,
            Acc => 1,
            Imm => 2,
            Adr => 3,
            Abs => 3,
            Ind => 3,
            Rel => 2,
            Abx => 3,
            Aby => 3,
            Zp => 2,
            Zpx => 2,
            Zpy => 2,
            Izx => 2,
            Izy => 2,
            Err => 1,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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
    Jsr(AddressMode),
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
    Ahx(AddressMode), // Also known as SHA
    Alr,
    Anc,
    Arr,
    Axs,
    Dcp(AddressMode),
    Isb(AddressMode), // Also known as ISC
    Kil(AddressMode),
    Las,
    Lax(AddressMode),
    Rla(AddressMode),
    Rra(AddressMode),
    Sax(AddressMode),
    Shx(AddressMode),
    Shy(AddressMode),
    Slo(AddressMode),
    Sre(AddressMode),
    Tas,
    Xaa,

    // Interrupts (Not really instructions)
    Nmi,
    Irq,
}

impl Instruction {
    fn from_op(op: u8) -> Instruction {
        use AddressMode::*;
        use Instruction::*;

        match op {
            0x00 => Brk,
            0x01 => Ora(Izx),
            0x02 => Kil(Err),
            0x03 => Slo(Izx),
            0x04 => Nop(Zp),
            0x05 => Ora(Zp),
            0x06 => Asl(Zp),
            0x07 => Slo(Zp),
            0x08 => Php,
            0x09 => Ora(Imm),
            0x0a => Asl(Acc),
            0x0b => Anc,
            0x0c => Nop(Abs),
            0x0d => Ora(Abs),
            0x0e => Asl(Abs),
            0x0f => Slo(Abs),
            0x10 => Bpl,
            0x11 => Ora(Izy),
            0x12 => Kil(Err),
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
            0x20 => Jsr(Adr),
            0x21 => And(Izx),
            0x22 => Kil(Err),
            0x23 => Rla(Izx),
            0x24 => Bit(Zp),
            0x25 => And(Zp),
            0x26 => Rol(Zp),
            0x27 => Rla(Zp),
            0x28 => Plp,
            0x29 => And(Imm),
            0x2a => Rol(Acc),
            0x2b => Anc,
            0x2c => Bit(Abs),
            0x2d => And(Abs),
            0x2e => Rol(Abs),
            0x2f => Rla(Abs),
            0x30 => Bmi,
            0x31 => And(Izy),
            0x32 => Kil(Err),
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
            0x42 => Kil(Err),
            0x43 => Sre(Izx),
            0x44 => Nop(Zp),
            0x45 => Eor(Zp),
            0x46 => Lsr(Zp),
            0x47 => Sre(Zp),
            0x48 => Pha,
            0x49 => Eor(Imm),
            0x4a => Lsr(Acc),
            0x4b => Alr,
            0x4c => Jmp(Adr),
            0x4d => Eor(Abs),
            0x4e => Lsr(Abs),
            0x4f => Sre(Abs),
            0x50 => Bvc,
            0x51 => Eor(Izy),
            0x52 => Kil(Err),
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
            0x62 => Kil(Err),
            0x63 => Rra(Izx),
            0x64 => Nop(Zp),
            0x65 => Adc(Zp),
            0x66 => Ror(Zp),
            0x67 => Rra(Zp),
            0x68 => Pla,
            0x69 => Adc(Imm),
            0x6a => Ror(Acc),
            0x6b => Arr,
            0x6c => Jmp(Ind),
            0x6d => Adc(Abs),
            0x6e => Ror(Abs),
            0x6f => Rra(Abs),
            0x70 => Bvs,
            0x71 => Adc(Izy),
            0x72 => Kil(Err),
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
            0x80 => Nop(Imm),
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
            0x92 => Kil(Err),
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
            0xb2 => Kil(Err),
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
            0xd2 => Kil(Err),
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
            0xe3 => Isb(Izx),
            0xe4 => Cpx(Zp),
            0xe5 => Sbc(Zp),
            0xe6 => Inc(Zp),
            0xe7 => Isb(Zp),
            0xe8 => Inx,
            0xe9 => Sbc(Imm),
            0xea => Nop(Imp),
            0xeb => Sbc(Imm),
            0xec => Cpx(Abs),
            0xed => Sbc(Abs),
            0xee => Inc(Abs),
            0xef => Isb(Abs),
            0xf0 => Beq,
            0xf1 => Sbc(Izy),
            0xf2 => Kil(Err),
            0xf3 => Isb(Izy),
            0xf4 => Nop(Zpx),
            0xf5 => Sbc(Zpx),
            0xf6 => Inc(Zpx),
            0xf7 => Isb(Zpx),
            0xf8 => Sed,
            0xf9 => Sbc(Aby),
            0xfa => Nop(Imp),
            0xfb => Isb(Aby),
            0xfc => Nop(Abx),
            0xfd => Sbc(Abx),
            0xfe => Inc(Abx),
            0xff => Isb(Abx),
        }
    }

    fn info(&self, o: (u8, u16, u16)) -> (&str, String, u8) {
        use AddressMode::*;
        use Instruction::*;

        match *self {
            Adc(m) => ("adc", m.display(o), m.size()),
            And(m) => ("and", m.display(o), m.size()),
            Asl(m) => ("asl", m.display(o), m.size()),
            Bcc => ("bcc", Rel.display(o), 2),
            Bcs => ("bcs", Rel.display(o), 2),
            Beq => ("beq", Rel.display(o), 2),
            Bit(m) => ("bit", m.display(o), m.size()),
            Bmi => ("bmi", Rel.display(o), 2),
            Bne => ("bne", Rel.display(o), 2),
            Bpl => ("bpl", Rel.display(o), 2),
            Brk => ("brk", "".to_string(), 1),
            Bvc => ("bvc", Rel.display(o), 2),
            Bvs => ("bvs", Rel.display(o), 2),
            Clc => ("clc", "".to_string(), 1),
            Cld => ("cld", "".to_string(), 1),
            Cli => ("cli", "".to_string(), 1),
            Clv => ("clv", "".to_string(), 1),
            Cmp(m) => ("cmp", m.display(o), m.size()),
            Cpx(m) => ("cpx", m.display(o), m.size()),
            Cpy(m) => ("cpy", m.display(o), m.size()),
            Dec(m) => ("dec", m.display(o), m.size()),
            Dex => ("dex", "".to_string(), 1),
            Dey => ("dey", "".to_string(), 1),
            Eor(m) => ("eor", m.display(o), m.size()),
            Inc(m) => ("inc", m.display(o), m.size()),
            Inx => ("inx", "".to_string(), 1),
            Iny => ("iny", "".to_string(), 1),
            Jmp(m) => ("jmp", m.display(o), m.size()),
            Jsr(m) => ("jsr", m.display(o), m.size()),
            Lda(m) => ("lda", m.display(o), m.size()),
            Ldx(m) => ("ldx", m.display(o), m.size()),
            Ldy(m) => ("ldy", m.display(o), m.size()),
            Lsr(m) => ("lsr", m.display(o), m.size()),
            Nop(m) => ("nop", m.display(o), m.size()),
            Ora(m) => ("ora", m.display(o), m.size()),
            Pha => ("pha", "".to_string(), 1),
            Php => ("php", "".to_string(), 1),
            Pla => ("pla", "".to_string(), 1),
            Plp => ("plp", "".to_string(), 1),
            Rol(m) => ("rol", m.display(o), m.size()),
            Ror(m) => ("ror", m.display(o), m.size()),
            Rti => ("rti", "".to_string(), 1),
            Rts => ("rts", "".to_string(), 1),
            Sbc(m) => ("sbc", m.display(o), m.size()),
            Sec => ("sec", "".to_string(), 1),
            Sed => ("sed", "".to_string(), 1),
            Sei => ("sei", "".to_string(), 1),
            Sta(m) => ("sta", m.display(o), m.size()),
            Stx(m) => ("stx", m.display(o), m.size()),
            Sty(m) => ("sty", m.display(o), m.size()),
            Tax => ("tax", "".to_string(), 1),
            Tay => ("tay", "".to_string(), 1),
            Tsx => ("tsx", "".to_string(), 1),
            Txa => ("txa", "".to_string(), 1),
            Txs => ("txs", "".to_string(), 1),
            Tya => ("tya", "".to_string(), 1),

            Ahx(m) => ("ahx", m.display(o), m.size()),
            Alr => ("alr", "".to_string(), 1),
            Anc => ("anc", "".to_string(), 1),
            Arr => ("arr", "".to_string(), 1),
            Axs => ("axs", "".to_string(), 1),
            Dcp(m) => ("*dcp", m.display(o), m.size()),
            Isb(m) => ("*isb", m.display(o), m.size()),
            Kil(m) => ("kil", m.display(o), m.size()),
            Las => ("las", "".to_string(), 1),
            Lax(m) => ("*lax", m.display(o), m.size()),
            Rla(m) => ("rla", m.display(o), m.size()),
            Rra(m) => ("rra", m.display(o), m.size()),
            Sax(m) => ("*sax", m.display(o), m.size()),
            Shx(m) => ("shx", m.display(o), m.size()),
            Shy(m) => ("shy", m.display(o), m.size()),
            Slo(m) => ("*slo", m.display(o), m.size()),
            Sre(m) => ("*sre", m.display(o), m.size()),
            Tas => ("tas", "".to_string(), 1),
            Xaa => ("xaa", "".to_string(), 1),

            Nmi => ("nmi", "".to_string(), 1),
            Irq => ("irq", "".to_string(), 1),
        }
    }
}

pub struct Cpu6502 {
    reg_a: u8,   // Accumulator
    reg_x: u8,   // Index X
    reg_y: u8,   // Index Y
    reg_pc: u16, // Program counter
    reg_s: u8,   // Stack pointer
    reg_p: u8,   // Status

    inst_count: u64,
    cycle_count: u64,

    inst: Instruction,

    nmi_pending: bool,
    irq_level: bool,

    addr: u16,
    addr_prev: u16,
    ptr: u8,
    value: u8,
    cycle: u8,

    trace_on: bool,
    prev_state: (u16, Instruction, u8, u8, u8, u8, u8),
    debugu8: u8,
    debugu16: u16,
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

            inst_count: 0,
            cycle_count: 0,

            inst: Instruction::Brk,

            nmi_pending: false,
            irq_level: true,

            addr: 0,
            addr_prev: 0,
            ptr: 0,
            value: 0,
            cycle: 1,

            trace_on: false,
            prev_state: (0, Instruction::Brk, 0, 0, 0, 0, 0),
            debugu8: 0,
            debugu16: 0,
        }
    }

    pub fn reset(&mut self, mm: &mut MemoryMap) {
        info!("Reset CPU");

        self.reg_p |= I;

        self.reg_s = self.reg_s.wrapping_sub(3);

        // Load the reset vector
        self.reg_pc =
            (peek_u8!(mm, RESET_VECTOR + 1) as u16) << 8 | peek_u8!(mm, RESET_VECTOR) as u16;
    }

    pub fn power_on_reset(&mut self, mm: &mut MemoryMap) {
        info!("Power Cycle CPU");

        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.reg_p = 0x34;

        // To match nestest, OK?
        self.reg_p = 0b0010_0100;

        self.reg_s = 0xfd;

        // Load the reset vector
        self.reg_pc =
            (peek_u8!(mm, RESET_VECTOR + 1) as u16) << 8 | peek_u8!(mm, RESET_VECTOR) as u16;
    }

    pub fn _irq(&mut self, level: bool) {
        self.irq_level = level;
    }

    pub fn nmi(&mut self) {
        self.nmi_pending = true;
    }

    pub fn tick(&mut self, mm: &mut MemoryMap, _e: &mut VecDeque<Event>) {
        use Instruction::*;

        match self.cycle {
            0 => {
                panic!("Cycle 0 is reserved");
            }
            1 => {
                if self.trace_on && self.inst_count != 0 {
                    let c = self.disassemble_nestest(mm);
                    println!("{:>8}:  {}", self.inst_count, c);
                }

                // Check for interrupts
                if self.nmi_pending {
                    info!("NMI");
                    self.nmi_pending = false;
                    self.inst = Instruction::Nmi;
                } else if !self.irq_level && (self.reg_p & I == 0) {
                    info!("IRQ");
                    self.inst = Instruction::Irq;
                } else {
                    // Fetch decode
                    let op = read_u8!(mm, self.reg_pc as usize);
                    self.inst = Instruction::from_op(op);
                }

                self.prev_state = (
                    self.reg_pc,
                    self.inst,
                    self.reg_a,
                    self.reg_x,
                    self.reg_y,
                    self.reg_p,
                    self.reg_s,
                );
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle += 1;
                self.inst_count += 1;
            }
            _ => match self.inst {
                Adc(m) => self.ex_adc(mm, m),
                And(m) => self.ex_and(mm, m),
                Asl(m) => self.ex_asl(mm, m),
                Bcc => self.ex_bcc(mm),
                Bcs => self.ex_bcs(mm),
                Beq => self.ex_beq(mm),
                Bit(m) => self.ex_bit(mm, m),
                Bmi => self.ex_bmi(mm),
                Bne => self.ex_bne(mm),
                Bpl => self.ex_bpl(mm),
                Brk => self.ex_brk(mm),
                Bvc => self.ex_bvc(mm),
                Bvs => self.ex_bvs(mm),
                Clc => self.ex_clc(),
                Cld => self.ex_cld(),
                Cli => self.ex_cli(),
                Clv => self.ex_clv(),
                Cmp(m) => self.ex_cmp(mm, m),
                Cpx(m) => self.ex_cpx(mm, m),
                Cpy(m) => self.ex_cpy(mm, m),
                Dec(m) => self.ex_dec(mm, m),
                Dex => self.ex_dex(),
                Dey => self.ex_dey(),
                Eor(m) => self.ex_eor(mm, m),
                Inc(m) => self.ex_inc(mm, m),
                Inx => self.ex_inx(),
                Iny => self.ex_iny(),
                Jmp(m) => self.ex_jmp(mm, m),
                Jsr(_) => self.ex_jsr(mm),
                Lda(m) => self.ex_lda(mm, m),
                Ldx(m) => self.ex_ldx(mm, m),
                Ldy(m) => self.ex_ldy(mm, m),
                Lsr(m) => self.ex_lsr(mm, m),
                Nop(m) => self.ex_nop(mm, m),
                Ora(m) => self.ex_ora(mm, m),
                Pha => self.ex_pha(mm),
                Php => self.ex_php(mm),
                Pla => self.ex_pla(mm),
                Plp => self.ex_plp(mm),
                Rol(m) => self.ex_rol(mm, m),
                Ror(m) => self.ex_ror(mm, m),
                Rti => self.ex_rti(mm),
                Rts => self.ex_rts(mm),
                Sbc(m) => self.ex_sbc(mm, m),
                Sec => self.ex_sec(),
                Sed => self.ex_sed(),
                Sei => self.ex_sei(),
                Sta(m) => self.ex_sta(mm, m),
                Stx(m) => self.ex_stx(mm, m),
                Sty(m) => self.ex_sty(mm, m),
                Tax => self.ex_tax(),
                Tay => self.ex_tay(),
                Tsx => self.ex_tsx(),
                Txa => self.ex_txa(),
                Txs => self.ex_txs(),
                Tya => self.ex_tya(),

                // Unusual Instructions
                Ahx(m) => self.ex_ahx(mm, m),
                Alr => panic!("Unimplemented Opcode {:?}", self.inst),
                Anc => panic!("Unimplemented Opcode {:?}", self.inst),
                Arr => panic!("Unimplemented Opcode {:?}", self.inst),
                Axs => panic!("Unimplemented Opcode {:?}", self.inst),
                Dcp(m) => self.ex_dcp(mm, m),
                Isb(m) => self.ex_isb(mm, m),
                Kil(_) => panic!("Unimplemented Opcode {:?}", self.inst),
                Las => panic!("Unimplemented Opcode {:?}", self.inst),
                Lax(m) => self.ex_lax(mm, m),
                Rla(m) => self.ex_rla(mm, m),
                Rra(m) => self.ex_rra(mm, m),
                Sax(m) => self.ex_sax(mm, m),
                Shx(m) => self.ex_shx(mm, m),
                Shy(m) => self.ex_shy(mm, m),
                Slo(m) => self.ex_slo(mm, m),
                Sre(m) => self.ex_sre(mm, m),
                Tas => panic!("Unimplemented Opcode {:?}", self.inst),
                Xaa => panic!("Unimplemented Opcode {:?}", self.inst),

                // Meta Instructions
                Nmi => self.ex_nmi(mm),
                Irq => self.ex_irq(mm),
            },
        }

        self.cycle_count += 1;
    }

    #[allow(dead_code)]
    fn disassemble(mm: &mut MemoryMap, address: usize, num: usize) -> String {
        let mut s = String::new();
        let mut address = address;

        for _ in 0..num {
            // Disassembly info for debug
            let op = peek_u8!(mm, address);
            let inst = Instruction::from_op(op);
            let context = (
                peek_u8!(mm, address + 1),
                peek_u8!(mm, address + 1) as u16 | (peek_u8!(mm, address + 2) as u16) << 8,
                0,
            );

            let (name, operand, size) = inst.info(context);

            if size == 0 {
                s = f!("{}Unknown inst: (0x{:02x}) {}, break\n", s, op, name);
                break;
            } else {
                s = f!(
                    "{}{:04x}: {} {:<8}\n",
                    s,
                    address,
                    name.to_ascii_uppercase(),
                    operand,
                );
            }
            address += size as usize;
        }
        s
    }

    #[allow(dead_code)]
    fn disassemble_current(&mut self, mm: &mut MemoryMap) -> String {
        // Disassembly info for debug
        let _b0 = peek_u8!(mm, self.reg_pc as usize);
        let b1 = peek_u8!(mm, self.reg_pc as usize + 1);
        let b2 = peek_u8!(mm, self.reg_pc as usize + 2);

        let imm_addr = (b2 as u16) << 8 | b1 as u16;
        let cal_addr = self.addr;
        let (name, operand, _) = self.inst.info((b1, imm_addr, cal_addr));
        f!(
            "{:04x}: {} {:<8}\t{} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:04x} => {}",
            self.reg_pc,
            name.to_ascii_uppercase(),
            operand,
            self.status_as_string(),
            self.reg_a,
            self.reg_x,
            self.reg_y,
            self.reg_p,
            self.reg_s as u16 + 0x0100,
            self.stack_as_string(mm),
        )
    }

    #[allow(dead_code)]
    fn disassemble_nestest(&mut self, mm: &mut MemoryMap) -> String {
        let (pc, inst, a, x, y, p, s) = self.prev_state;

        // Disassembly info for debug
        let b0 = peek_u8!(mm, pc as usize);
        let b1 = peek_u8!(mm, pc as usize + 1);
        let b2 = peek_u8!(mm, pc as usize + 2);
        let imm_addr = (b2 as u16) << 8 | b1 as u16;
        let cal_addr = self.addr;
        let (name, operand, size) = inst.info((b1, imm_addr, cal_addr));
        f!(
            "{:04X}  {}{:>5} {:27} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            pc,
            match size {
                1 => f!("{:02X}      ", b0),
                2 => f!("{:02X} {:02X}   ", b0, b1),
                3 => f!("{:02X} {:02X} {:02X}", b0, b1, b2),
                _ => panic!("Bad Instruction Size"),
            },
            name.to_ascii_uppercase(),
            match inst {
                Instruction::Adc(m)
                | Instruction::Asl(m)
                | Instruction::And(m)
                | Instruction::Bit(m)
                | Instruction::Cmp(m)
                | Instruction::Cpx(m)
                | Instruction::Cpy(m)
                | Instruction::Dcp(m)
                | Instruction::Dec(m)
                | Instruction::Eor(m)
                | Instruction::Inc(m)
                | Instruction::Isb(m)
                | Instruction::Jmp(m)
                | Instruction::Lax(m)
                | Instruction::Lda(m)
                | Instruction::Ldx(m)
                | Instruction::Ldy(m)
                | Instruction::Lsr(m)
                | Instruction::Nop(m)
                | Instruction::Ora(m)
                | Instruction::Rla(m)
                | Instruction::Rol(m)
                | Instruction::Ror(m)
                | Instruction::Rra(m)
                | Instruction::Sax(m)
                | Instruction::Sbc(m)
                | Instruction::Slo(m)
                | Instruction::Sre(m)
                | Instruction::Sta(m)
                | Instruction::Stx(m)
                | Instruction::Sty(m) => match m {
                    AddressMode::Abs | AddressMode::Zp =>
                        operand.to_ascii_uppercase()
                            + &f!(" = {:02X}", self.debugu8).to_owned(),
                    AddressMode::Zpx | AddressMode::Zpy =>
                        operand.to_ascii_uppercase()
                            + &f!(
                                " @ {:02X} = {:02X}",
                                self.addr.to_owned(),
                                self.debugu8.to_owned(),
                            ),
                    AddressMode::Abx | AddressMode::Aby =>
                        operand.to_ascii_uppercase()
                            + &f!(
                                " @ {:04X} = {:02X}",
                                self.addr.to_owned(),
                                self.debugu8.to_owned(),
                            ),
                    AddressMode::Izx =>
                        operand.to_ascii_uppercase()
                            + &f!(
                                " @ {:02X} = {:04X} = {:02X}",
                                self.ptr.wrapping_sub(1).to_owned(),
                                self.addr.to_owned(),
                                self.debugu8.to_owned()
                            ),
                    AddressMode::Izy =>
                        operand.to_ascii_uppercase()
                            + &f!(
                                " = {:04X} @ {:04X} = {:02X}",
                                self.debugu16.to_owned(),
                                self.addr.to_owned(),
                                self.debugu8.to_owned()
                            ),
                    AddressMode::Ind =>
                        operand.to_ascii_uppercase()
                            + &f!(" = {:04X}", self.debugu16.to_owned()),
                    _ => operand.to_ascii_uppercase(),
                },
                _ => operand.to_ascii_uppercase(),
            },
            a,
            x,
            y,
            p,
            s
        )
    }

    fn stack_as_string(&self, mm: &mut MemoryMap) -> String {
        f!(
            "[{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}]",
            peek_u8!(mm, self.reg_s as usize + 0x101),
            peek_u8!(mm, self.reg_s as usize + 0x102),
            peek_u8!(mm, self.reg_s as usize + 0x103),
            peek_u8!(mm, self.reg_s as usize + 0x104),
            peek_u8!(mm, self.reg_s as usize + 0x105),
            peek_u8!(mm, self.reg_s as usize + 0x106),
            peek_u8!(mm, self.reg_s as usize + 0x107),
            peek_u8!(mm, self.reg_s as usize + 0x108),
        )
    }

    fn _dump_memory(&self, mm: &mut MemoryMap, addr: usize, len: u16) -> String {
        let mut s = String::from("");
        let mut addr = addr;

        for _ in 0..len {
            s = f!(
                "{}{:04x}: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}\n",
                s,
                addr,
                peek_u8!(mm, addr + 0),
                peek_u8!(mm, addr + 1),
                peek_u8!(mm, addr + 2),
                peek_u8!(mm, addr + 3),
                peek_u8!(mm, addr + 4),
                peek_u8!(mm, addr + 5),
                peek_u8!(mm, addr + 6),
                peek_u8!(mm, addr + 7)
            );
            addr += 8;
        }
        s
    }

    fn status_as_string(&self) -> String {
        f!(
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
                self.value = read_u8!(mm, self.reg_pc as usize);
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
        let ptr = self.ptr as usize;

        match m {
            AddressMode::Imp | AddressMode::Acc => {
                self.value = self.reg_a;
                self.cycle = 1;
            }
            AddressMode::Zp => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
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
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = self.ptr.wrapping_add(self.reg_x) as u16;
                    self.cycle += 1;
                }
                4 => {
                    debug_assert!((addr & 0xff00) == 0, "Error with zpx");
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
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
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
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
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.addr = self.addr.wrapping_add(self.reg_x as u16);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
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
            AddressMode::Aby => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.addr = self.addr.wrapping_add(self.reg_y as u16);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.value = read_u8!(mm, addr);
                    self.cycle += 1; // TODO: Handle the 4 cycle case
                }
                5 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izx => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.ptr.wrapping_add(self.reg_x);
                    self.cycle += 1;
                }
                4 => {
                    debug_assert!((ptr & 0xff00) == 0, "Error with izx");
                    self.addr = read_u8!(mm, ptr) as u16;
                    self.ptr = self.ptr.wrapping_add(1);
                    self.cycle += 1;
                }
                5 => {
                    // TODO: Handle variable clock cycles
                    self.addr |= (read_u8!(mm, ptr) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izy => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = read_u8!(mm, ptr) as u16;
                    self.ptr = self.ptr.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.addr |= (read_u8!(mm, ptr) as u16) << 8;
                    self.cycle += 1;
                }
                5 => {
                    // TODO: Handle variable clock cycles
                    self.debugu16 = self.addr;
                    self.addr = self.addr.wrapping_add(self.reg_y as u16);
                    self.cycle += 1;
                }
                6 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
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
                self.value = read_u8!(mm, pc);
                self.cycle = 1;
            }
            AddressMode::Imm => {
                self.value = read_u8!(mm, pc);
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle = 1;
            }
            AddressMode::Abs => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Abx => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.addr = self.addr.wrapping_add(self.reg_x as u16);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    //self.value = read_u8!(mm, addr);
                    self.cycle += 1; // TODO: Handle the 4 cycle case
                }
                5 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Aby => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.addr = self.addr.wrapping_add(self.reg_y as u16);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    //self.value = read_u8!(mm, addr);
                    self.cycle += 1; // TODO: Handle the 4 cycle case
                }
                5 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izx => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.ptr.wrapping_add(self.reg_x);
                    self.cycle += 1;
                }
                4 => {
                    debug_assert!((ptr & 0xff00) == 0, "Error with izx");
                    self.addr = peek_u8!(mm, ptr) as u16;
                    self.ptr = self.ptr.wrapping_add(1);
                    self.cycle += 1;
                }
                5 => {
                    // TODO: Handle variable clock cycles
                    self.addr |= (peek_u8!(mm, ptr) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izy => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = peek_u8!(mm, ptr) as u16;
                    self.ptr = self.ptr.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.addr |= (peek_u8!(mm, ptr) as u16) << 8;
                    self.cycle += 1;
                }
                5 => {
                    // TODO: Handle variable clock cycles
                    self.debugu16 = self.addr;
                    self.addr = self.addr.wrapping_add(self.reg_y as u16);
                    self.cycle += 1;
                }
                6 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zp => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpx => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = self.ptr.wrapping_add(self.reg_x) as u16;
                    self.cycle += 1;
                }
                4 => {
                    debug_assert!((addr & 0xff00) == 0, "Error with zpx");
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpy => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = self.ptr.wrapping_add(self.reg_y) as u16;
                    self.cycle += 1;
                }
                4 => {
                    debug_assert!((addr & 0xff00) == 0, "Error with zpy");
                    self.value = read_u8!(mm, addr);
                    self.debugu8 = self.value;
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
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.debugu8 = peek_u8!(mm, addr);
                    write_u8!(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Abx => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.addr = self.addr.wrapping_add(self.reg_x as u16);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    //self.value = read_u8!(mm, addr); // Value is ignored
                    self.cycle += 1;
                }
                5 => {
                    self.debugu8 = peek_u8!(mm, addr);
                    write_u8!(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Aby => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (read_u8!(mm, pc) as u16) << 8;
                    self.addr = self.addr.wrapping_add(self.reg_y as u16);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    //self.value = read_u8!(mm, addr); // Value is ignored
                    self.cycle += 1;
                }
                5 => {
                    self.debugu8 = peek_u8!(mm, addr);
                    write_u8!(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izx => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.ptr.wrapping_add(self.reg_x);
                    self.cycle += 1;
                }
                4 => {
                    debug_assert!((ptr & 0xff00) == 0, "Error with izx");
                    self.addr = read_u8!(mm, ptr) as u16;
                    self.ptr = self.ptr.wrapping_add(1);
                    self.cycle += 1;
                }
                5 => {
                    // TODO: Handle variable clock cycles
                    self.addr |= (read_u8!(mm, ptr) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.debugu8 = peek_u8!(mm, addr);
                    write_u8!(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izy => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = read_u8!(mm, ptr) as u16;
                    self.ptr = self.ptr.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.addr |= (read_u8!(mm, ptr) as u16) << 8;
                    self.cycle += 1;
                }
                5 => {
                    // TODO: Handle variable clock cycles
                    self.debugu16 = self.addr;
                    self.addr = self.addr.wrapping_add(self.reg_y as u16);
                    self.cycle += 1;
                }
                6 => {
                    self.debugu8 = peek_u8!(mm, addr);
                    write_u8!(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zp => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.debugu8 = peek_u8!(mm, addr);
                    write_u8!(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpx => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = self.ptr.wrapping_add(self.reg_x) as u16;
                    self.cycle += 1;
                }
                4 => {
                    debug_assert!((addr & 0xff00) == 0, "Error with zpx");
                    self.debugu8 = peek_u8!(mm, addr);
                    write_u8!(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpy => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = self.ptr.wrapping_add(self.reg_y) as u16;
                    self.cycle += 1;
                }
                4 => {
                    debug_assert!((addr & 0xff00) == 0, "Error with zpy");
                    self.debugu8 = peek_u8!(mm, addr);
                    write_u8!(mm, addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            _ => panic!("Invalid mode: {:?}", m),
        }
    }

    fn handle_interrupt(&mut self, mm: &mut MemoryMap, vector: usize, set_b: bool) {
        let s = self.reg_s as usize + 0x100;
        let addr_prev = self.addr_prev as usize;

        match self.cycle {
            2 => {
                self.addr_prev = self.reg_pc.wrapping_sub(1); // Save the current PC
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle += 1;
            }
            3 => {
                write_u8!(mm, s, (addr_prev >> 8) as u8);
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle += 1;
            }
            4 => {
                write_u8!(mm, s, (addr_prev & 0xff) as u8);
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle += 1;
            }
            5 => {
                let mut p = self.reg_p & 0b11001111;
                if set_b {
                    p |= 0b00100000 | B; // Set 'B' Flag
                } else {
                    p |= 0b00100000;
                }
                write_u8!(mm, s, p);
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle += 1;
            }
            6 => {
                self.reg_pc = read_u8!(mm, vector) as u16;
                self.cycle += 1;
            }
            7 => {
                self.reg_pc |= (read_u8!(mm, vector + 1) as u16) << 8;
                self.cycle = 1;
            }
            _ => (),
        }
    }

    // Handle interrupt instructions
    fn ex_nmi(&mut self, mm: &mut MemoryMap) {
        self.handle_interrupt(mm, NMI_VECTOR, false);
    }

    fn ex_irq(&mut self, mm: &mut MemoryMap) {
        self.handle_interrupt(mm, IRQ_VECTOR, false);

        if self.cycle == 6 {
            self.reg_p |= I; // Mask interrupts after pushing status to stack
        }
    }

    // Start specific Instruction handlers

    // ADC Add with carry A = A + M + C
    fn ex_adc(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            let t = self.reg_a as u16 + self.value as u16 + (self.reg_p & C == C) as u16;

            // Update C flag
            update_status!(self.reg_p, (t & 0x100) == 0x100, C);

            // Update V flag
            stat_v!(self.reg_p, self.reg_a, self.value, t & 0xff);

            self.reg_a = (t & 0xff) as u8;

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

    // ASL Arithmetic Shift Left
    fn ex_asl(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value << 1;

            if let AddressMode::Acc = m {
                self.reg_a = t;
            } else {
                write_u8!(mm, self.addr as usize, t);
            }

            // Update C flag
            update_status!(self.reg_p, (self.value & 0x80) == 0x80, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, t);
        }
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

    fn ex_bit(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            let t = self.value & self.reg_a;

            update_status!(self.reg_p, (t == 0), Z);

            update_status!(self.reg_p, (self.value & 0x40) == 0x40, V);

            update_status!(self.reg_p, (self.value & 0x80) == 0x80, N);
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

    fn ex_brk(&mut self, mm: &mut MemoryMap) {
        self.handle_interrupt(mm, IRQ_VECTOR, true);

        if self.cycle == 6 {
            self.reg_p |= I; // Mask interrupts after pushing status to stack
        }
    }

    fn ex_bvc(&mut self, mm: &mut MemoryMap) {
        self.handle_branch(mm);

        if self.cycle == 4 {
            if (self.reg_p & V) == 0 {
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

    fn ex_clv(&mut self) {
        self.reg_p &= !V;
        self.cycle = 1;
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

    // DEC Decrement memory
    fn ex_dec(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value.wrapping_sub(1);

            write_u8!(mm, self.addr as usize, t);

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

    // EOR A = A ^ M
    fn ex_eor(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_a = self.value ^ self.reg_a;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // INC Increment memory
    fn ex_inc(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value.wrapping_add(1);

            write_u8!(mm, self.addr as usize, t);

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

    // JMP Jump to address
    fn ex_jmp(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        let pc = self.reg_pc as usize;
        let ptr = self.ptr as usize;
        let addr = self.addr as usize;

        match m {
            AddressMode::Adr => match self.cycle {
                2 => {
                    self.addr = read_u8!(mm, pc) as u16;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.reg_pc = ((read_u8!(mm, pc) as u16) << 8) | self.addr;
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Ind => match self.cycle {
                2 => {
                    self.ptr = read_u8!(mm, pc);
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (read_u8!(mm, pc) as u16) << 8;
                    self.reg_pc = self.reg_pc.wrapping_add(1);
                    self.cycle += 1;
                }
                4 => {
                    self.reg_pc = read_u8!(mm, addr | ptr) as u16;
                    self.ptr = self.ptr.wrapping_add(1);
                    self.cycle += 1;
                }
                5 => {
                    self.reg_pc |= (read_u8!(mm, addr | ptr) as u16) << 8;
                    self.debugu16 = self.reg_pc;
                    self.cycle = 1;
                }
                _ => (),
            },
            _ => panic!("Invalid mode: {:?}", m),
        }
    }

    // JSR Jump to subroutine
    fn ex_jsr(&mut self, mm: &mut MemoryMap) {
        let pc = self.reg_pc as usize;
        let addr_prev = self.addr_prev as usize;
        let s = self.reg_s as usize + 0x100;

        match self.cycle {
            2 => {
                self.addr = read_u8!(mm, pc) as u16;
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle += 1;
            }
            3 => {
                self.addr_prev = self.reg_pc;
                self.cycle += 1;
            }
            4 => {
                write_u8!(mm, s, (addr_prev >> 8) as u8);
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle += 1;
            }
            5 => {
                write_u8!(mm, s, (addr_prev & 0xff) as u8);
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle += 1;
            }
            6 => {
                self.addr |= (read_u8!(mm, pc) as u16) << 8;
                self.reg_pc = self.addr;
                self.cycle = 1;
            }
            _ => (),
        }
    }

    // LDA Load to A
    fn ex_lda(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_a = self.value;

            // println!(
            //     "LDA: Computed ptr: {:04x} -> addr: {:04x} -> val: {:02x}",
            //     self.ptr, self.addr, self.value
            // );

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

    // LSR Logical Shift Right
    fn ex_lsr(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value >> 1;

            if let AddressMode::Acc = m {
                self.reg_a = t;
            } else {
                write_u8!(mm, self.addr as usize, t);
            }

            // Update C flag
            update_status!(self.reg_p, (self.value & 1) == 1, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, t);
        }
    }

    fn ex_nop(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);
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

    // Push accumulator
    fn ex_pha(&mut self, mm: &mut MemoryMap) {
        let s = self.reg_s as usize + 0x100;

        match self.cycle {
            2 => {
                self.cycle += 1;
            }
            3 => {
                write_u8!(mm, s, self.reg_a);
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle = 1;
            }
            _ => (),
        }
    }

    // Push processor status
    fn ex_php(&mut self, mm: &mut MemoryMap) {
        let s = self.reg_s as usize + 0x100;

        match self.cycle {
            2 => {
                self.cycle += 1;
            }
            3 => {
                write_u8!(mm, s, self.reg_p | 0b00110000); // Set the 'B' flag to 0b11
                self.reg_s = self.reg_s.wrapping_sub(1);
                self.cycle = 1;
            }
            _ => (),
        }
    }

    // Pull accumulator
    fn ex_pla(&mut self, mm: &mut MemoryMap) {
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
                self.reg_a = read_u8!(mm, s);
                stat_nz!(self.reg_p, self.reg_a);
                self.cycle = 1;
            }
            _ => (),
        }
    }

    // Pull processor status
    fn ex_plp(&mut self, mm: &mut MemoryMap) {
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
                //self.reg_p = read_u8!(mm, s) & 0b11001111; // Ignore the 'B' Flag
                self.reg_p = read_u8!(mm, s) & 0b11001111 | 0b00100000; // (Match NESTEST.NES)
                self.cycle = 1;
            }
            _ => (),
        }
    }

    // ROL Rotate Left
    fn ex_rol(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let mut t = self.value << 1;

            if (self.reg_p & C) == C {
                t |= 1;
            }

            if let AddressMode::Acc = m {
                self.reg_a = t;
            } else {
                write_u8!(mm, self.addr as usize, t);
            }

            // Update C flag
            update_status!(self.reg_p, (self.value & 0x80) == 0x80, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, t);
        }
    }

    // ROR Rotate Right
    fn ex_ror(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let mut t = self.value >> 1;

            if (self.reg_p & C) == C {
                t |= 0x80;
            }

            if let AddressMode::Acc = m {
                self.reg_a = t;
            } else {
                write_u8!(mm, self.addr as usize, t);
            }

            // Update C flag
            update_status!(self.reg_p, (self.value & 1) == 1, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, t);
        }
    }

    // RTI Return from interrupt
    fn ex_rti(&mut self, mm: &mut MemoryMap) {
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
                //self.reg_p = read_u8!(mm, s);
                self.reg_p = read_u8!(mm, s) | 0b00100000; // MATCH NESTEST
                self.reg_s = self.reg_s.wrapping_add(1);
                self.cycle += 1;
            }
            5 => {
                self.reg_pc = read_u8!(mm, s) as u16;
                self.reg_s = self.reg_s.wrapping_add(1);
                self.cycle += 1;
            }
            6 => {
                self.reg_pc |= (read_u8!(mm, s) as u16) << 8;
                self.cycle = 1;
            }
            _ => (),
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
                self.reg_pc = read_u8!(mm, s) as u16;
                self.reg_s = self.reg_s.wrapping_add(1);
                self.cycle += 1;
            }
            5 => {
                self.reg_pc |= (read_u8!(mm, s) as u16) << 8;
                self.cycle += 1;
            }
            6 => {
                self.reg_pc = self.reg_pc.wrapping_add(1);
                self.cycle = 1;
            }
            _ => (),
        }
    }

    fn ex_sbc(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.value = !self.value;
            let t = self.reg_a as u16 + self.value as u16 + (self.reg_p & C == C) as u16;

            // Update C flag
            update_status!(self.reg_p, (t & 0x100) == 0x100, C);

            // Update V flag
            stat_v!(self.reg_p, self.reg_a, self.value, t & 0xff);

            self.reg_a = (t & 0xff) as u8;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
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

    // Unusual instructions implementations

    // AHX Store A & X & (ADDR_HI + 1) to memory
    fn ex_ahx(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_a & self.reg_x & ((self.addr >> 8) as u8).wrapping_add(1);
        self.handle_write(mm, m);
    }

    // DCP Decrement then Compare (DEC, CMP)
    fn ex_dcp(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value.wrapping_sub(1);

            write_u8!(mm, self.addr as usize, t);

            // Update C flag
            update_status!(self.reg_p, self.reg_a >= t, C);

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a.wrapping_sub(t));
        }
    }

    // ISB Increment memory then SBC
    fn ex_isb(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            self.value = self.value.wrapping_add(1);
            write_u8!(mm, self.addr as usize, self.value);

            self.value = !self.value;
            let t = self.reg_a as u16 + self.value as u16 + (self.reg_p & C == C) as u16;

            // Update C flag
            update_status!(self.reg_p, (t & 0x100) == 0x100, C);

            // Update V flag
            stat_v!(self.reg_p, self.reg_a, self.value, t & 0xff);

            self.reg_a = (t & 0xff) as u8;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    fn ex_lax(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.handle_read(mm, m);

        if self.cycle == 1 {
            self.reg_a = self.value;
            self.reg_x = self.value;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // RLA
    fn ex_rla(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let mut t = self.value << 1;

            if (self.reg_p & C) == C {
                t |= 1;
            }

            write_u8!(mm, self.addr as usize, t);

            // Update C flag
            update_status!(self.reg_p, (self.value & 0x80) == 0x80, C);

            self.reg_a = t & self.reg_a;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // RRA Rotate Right then ADC
    fn ex_rra(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let mut t = self.value >> 1;

            if (self.reg_p & C) == C {
                t |= 0x80;
            }

            write_u8!(mm, self.addr as usize, t);

            let t2 = self.reg_a as u16 + t as u16 + ((self.value & 1) == 1) as u16;

            // Update C flag
            update_status!(self.reg_p, (t2 & 0x100) == 0x100, C);

            // Update V flag
            stat_v!(self.reg_p, self.reg_a, t, t2 & 0xff);

            self.reg_a = (t2 & 0xff) as u8;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // SAX Store A & X to memory
    fn ex_sax(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_a & self.reg_x;
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

    // SLO Arithmetic Shift Left then ORA
    fn ex_slo(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value << 1;

            write_u8!(mm, self.addr as usize, t);

            // Update C flag
            update_status!(self.reg_p, (self.value & 0x80) == 0x80, C);

            self.reg_a = t | self.reg_a;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // SRE Locical Shift Right then XOR
    fn ex_sre(&mut self, mm: &mut MemoryMap, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.handle_read_modify_write(mm, m);

        if self.cycle == 1 {
            let t = self.value >> 1;

            write_u8!(mm, self.addr as usize, t);

            // Update C flag
            update_status!(self.reg_p, (self.value & 1) == 1, C);

            self.reg_a = t ^ self.reg_a;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }
}

impl std::fmt::Display for Cpu6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "instruction count: {:}\n", self.inst_count)?;
        write!(f, "cycle count: {:}\n", self.cycle_count)?;
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

#[cfg(test)]
mod test {
    use super::Cpu6502;
    use super::*;
    use crate::mem::MemoryMap;
    use crate::rom::Rom;

    #[derive(Eq, PartialEq)]
    struct TestState {
        status: String,
        a: u8,
        x: u8,
        y: u8,
        sp: u16,
        pc: u16,
        cycle: u8,
    }

    impl std::default::Default for TestState {
        fn default() -> Self {
            TestState {
                a: 0,
                x: 0,
                y: 0,
                sp: 0x100,
                pc: 0x200,
                status: String::from("nv.bDizc"),
                cycle: 1,
            }
        }
    }

    impl std::fmt::Display for TestState {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(
                f,
                "PC:{:04x} C:{} A:{:02x} X:{:02x} Y:{:02x} SP:{:04x} STATUS:{}",
                self.pc, self.cycle, self.a, self.x, self.y, self.sp, self.status
            )?;
            Ok(())
        }
    }

    impl std::fmt::Debug for TestState {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(
                f,
                "PC:{:04x} C:{} A:{:02x} X:{:02x} Y:{:02x} SP:{:04x} STATUS:{}",
                self.pc, self.cycle, self.a, self.x, self.y, self.sp, self.status
            )?;
            Ok(())
        }
    }

    fn _set_state(cpu: &mut Cpu6502, s: &TestState) {
        cpu.reg_a = s.a;
        cpu.reg_x = s.x;
        cpu.reg_y = s.y;
        cpu.reg_s = (s.sp & 0xff) as u8;
        cpu.reg_pc = s.pc;
        cpu.cycle = s.cycle;

        for c in s.status.chars() {
            match c {
                'N' => cpu.reg_p |= N,
                'n' => cpu.reg_p &= !N,
                'V' => cpu.reg_p |= V,
                'v' => cpu.reg_p &= !V,
                'B' => cpu.reg_p |= B,
                'b' => cpu.reg_p &= !B,
                'D' => cpu.reg_p |= D,
                'd' => cpu.reg_p &= !D,
                'I' => cpu.reg_p |= I,
                'i' => cpu.reg_p &= !I,
                'Z' => cpu.reg_p |= Z,
                'z' => cpu.reg_p &= !Z,
                'C' => cpu.reg_p |= C,
                'c' => cpu.reg_p &= !C,
                '.' => (),
                _ => panic!("Bad test status"),
            }
        }
    }

    fn _get_state(cpu: &Cpu6502) -> TestState {
        let mut status = String::new();

        if cpu.reg_p & N == N {
            status.push('N');
        } else {
            status.push('n');
        }
        if cpu.reg_p & V == V {
            status.push('V');
        } else {
            status.push('v');
        }
        status.push('.');
        if cpu.reg_p & B == B {
            status.push('B');
        } else {
            status.push('b');
        }
        if cpu.reg_p & D == D {
            status.push('D');
        } else {
            status.push('d');
        }
        if cpu.reg_p & I == I {
            status.push('I');
        } else {
            status.push('i');
        }
        if cpu.reg_p & Z == Z {
            status.push('Z');
        } else {
            status.push('z');
        }
        if cpu.reg_p & C == C {
            status.push('C');
        } else {
            status.push('c');
        }

        TestState {
            a: cpu.reg_a,
            x: cpu.reg_x,
            y: cpu.reg_y,
            sp: 0x100 | cpu.reg_s as u16,
            pc: cpu.reg_pc,
            status: status,
            cycle: cpu.cycle,
        }
    }

    fn _run(cpu: &mut Cpu6502, mm: &mut MemoryMap, clks: usize) {
        let mut _events = VecDeque::new();
        for _ in 0..clks {
            cpu.tick(mm, &mut _events)
        }
    }

    #[test]
    fn test_nestest() {
        use crate::ppu::Ppu2c02;
        use std::fs::File;
        use std::io::{self, BufRead};
        use std::path::Path;

        fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
        where
            P: AsRef<Path>,
        {
            let file = File::open(filename)?;
            Ok(io::BufReader::new(file).lines())
        }

        fn disassemble_nestest(cpu: &Cpu6502, mm: &mut MemoryMap) -> String {
            let (pc, inst, a, x, y, p, s) = cpu.prev_state;

            // Disassembly info for debug
            let b0 = peek_u8!(mm, pc as usize);
            let b1 = peek_u8!(mm, pc as usize + 1);
            let b2 = peek_u8!(mm, pc as usize + 2);
            let imm_addr = (b2 as u16) << 8 | b1 as u16;
            let cal_addr = cpu.addr;
            let (name, operand, size) = inst.info((b1, imm_addr, cal_addr));
            f!(
                "{:04X}  {}{:>5} {:27} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                pc,
                match size {
                    1 => f!("{:02X}      ", b0),
                    2 => f!("{:02X} {:02X}   ", b0, b1),
                    3 => f!("{:02X} {:02X} {:02X}", b0, b1, b2),
                    _ => panic!("Bad Instruction Size"),
                },
                name.to_ascii_uppercase(),
                match inst {
                    Instruction::Adc(m)
                    | Instruction::Asl(m)
                    | Instruction::And(m)
                    | Instruction::Bit(m)
                    | Instruction::Cmp(m)
                    | Instruction::Cpx(m)
                    | Instruction::Cpy(m)
                    | Instruction::Dcp(m)
                    | Instruction::Dec(m)
                    | Instruction::Eor(m)
                    | Instruction::Inc(m)
                    | Instruction::Isb(m)
                    | Instruction::Jmp(m)
                    | Instruction::Lax(m)
                    | Instruction::Lda(m)
                    | Instruction::Ldx(m)
                    | Instruction::Ldy(m)
                    | Instruction::Lsr(m)
                    | Instruction::Nop(m)
                    | Instruction::Ora(m)
                    | Instruction::Rla(m)
                    | Instruction::Rol(m)
                    | Instruction::Ror(m)
                    | Instruction::Rra(m)
                    | Instruction::Sax(m)
                    | Instruction::Sbc(m)
                    | Instruction::Slo(m)
                    | Instruction::Sre(m)
                    | Instruction::Sta(m)
                    | Instruction::Stx(m)
                    | Instruction::Sty(m) => match m {
                        AddressMode::Abs | AddressMode::Zp =>
                            operand.to_ascii_uppercase()
                                + &f!(" = {:02X}", cpu.debugu8).to_owned(),
                        AddressMode::Zpx | AddressMode::Zpy =>
                            operand.to_ascii_uppercase()
                                + &f!(
                                    " @ {:02X} = {:02X}",
                                    cpu.addr.to_owned(),
                                    cpu.debugu8.to_owned(),
                                ),
                        AddressMode::Abx | AddressMode::Aby =>
                            operand.to_ascii_uppercase()
                                + &f!(
                                    " @ {:04X} = {:02X}",
                                    cpu.addr.to_owned(),
                                    cpu.debugu8.to_owned(),
                                ),
                        AddressMode::Izx =>
                            operand.to_ascii_uppercase()
                                + &f!(
                                    " @ {:02X} = {:04X} = {:02X}",
                                    cpu.ptr.wrapping_sub(1).to_owned(),
                                    cpu.addr.to_owned(),
                                    cpu.debugu8.to_owned()
                                ),
                        AddressMode::Izy =>
                            operand.to_ascii_uppercase()
                                + &f!(
                                    " = {:04X} @ {:04X} = {:02X}",
                                    cpu.debugu16.to_owned(),
                                    cpu.addr.to_owned(),
                                    cpu.debugu8.to_owned()
                                ),
                        AddressMode::Ind =>
                            operand.to_ascii_uppercase()
                                + &f!(" = {:04X}", cpu.debugu16.to_owned()),
                        _ => operand.to_ascii_uppercase(),
                    },
                    _ => operand.to_ascii_uppercase(),
                },
                a,
                x,
                y,
                p,
                s
            )
        }

        let path = Path::new("./test/nestest.nes");
        let rom = Rom::from_file(path).unwrap();
        let mut cpu = Cpu6502::new();
        let ppu = Ppu2c02::new(rom.chr_rom.to_owned());
        let mut mm = MemoryMap::new(&rom, ppu);
        let mut _events = VecDeque::new();

        let mut test_lines: Vec<String> = Vec::new();

        cpu.power_on_reset(&mut mm);

        // Jump to the start of the golden log file
        cpu.reg_pc = 0xc000;

        for _ in 0..50000 {
            cpu.tick(&mut mm, &mut _events);
            if cpu.cycle == 1 {
                let out = disassemble_nestest(&cpu, &mut mm);
                test_lines.push(out);
            }
        }

        if let Ok(lines) = read_lines("./test/nestest.log") {
            for (i, (l1, l2)) in test_lines.iter().zip(lines).enumerate() {
                if let Ok(l2) = l2 {
                    assert_eq!(&l1[0..15], &l2[0..15], "Execution error @ {}", i);
                    assert_eq!(&l1[16..48], &l2[16..48], "Instruction error @ {}", i);
                    assert_eq!(&l1[48..73], &l2[48..73], "Status error @ {}", i);
                }
            }
        }
    }

    #[test]
    fn test_reset() {
        let mut mm = MemoryMap::new_stub();
        let mut cpu = Cpu6502::new();
        cpu.reg_a = 0x55u8;

        assert_eq!(cpu.reg_a, 0x55);
        cpu.reset(&mut mm);
        assert_eq!(cpu.reg_a, 0x55);
    }

    #[test]
    fn test_power_on_reset() {
        let mut mm = MemoryMap::new_stub();
        let mut cpu = Cpu6502::new();
        cpu.reg_a = 0x55u8;

        assert_eq!(cpu.reg_a, 0x55);
        cpu.power_on_reset(&mut mm);
        assert_eq!(cpu.reg_a, 0);
    }
}
