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
use crate::MemoryMap;

const _OP_MODE_MASK: u8 = 0b0001_1100;
const _OP_MODE_0: u8 = 0b0000_0000;
const _OP_MODE_1: u8 = 0b0000_0100;
const _OP_MODE_2: u8 = 0b0000_1000;
const _OP_MODE_3: u8 = 0b0000_1100;
const _OP_MODE_4: u8 = 0b0001_0000;
const _OP_MODE_5: u8 = 0b0001_0100;
const _OP_MODE_6: u8 = 0b0001_1000;
const _OP_MODE_7: u8 = 0b0001_1100;

const N: u8 = 0b1000_0000; // Negitive
const V: u8 = 0b1000_0000; // Overflow
const D: u8 = 0b1000_0000; // Decimal
const I: u8 = 0b1000_0000; // Interrupt Disable
const Z: u8 = 0b1000_0000; // Zero
const C: u8 = 0b1000_0000; // Carry

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
    _Rel,
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
    fn _info(&self) -> (&str, &str) {
        use Instruction::*;

        match *self {
            Nop(_) => ("nop", "No operation"),
            Clc => ("clc", "Clear Carry"),
            Lda(_) => ("lda", "Load Accumulator"),
            _ => ("--", "--"),
        }
    }
}

#[derive(Debug)]
pub struct Cpu6502 {
    reg_a: u8, // Accumulator
    reg_x: u8,
    reg_y: u8,
    reg_pc: u16, // Program counter
    reg_s: u8,   // Stack pointer
    reg_p: u8,   // Status

    next_mem: u16,
    count: u64,
    mm: MemoryMap,

    inst: Instruction,
    addr: u16,
    ptr: u8,
    value: u8,
    cycle: u8,
}

impl Cpu6502 {
    pub fn new(mm: MemoryMap) -> Cpu6502 {
        Cpu6502 {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            reg_pc: 0,
            reg_p: 0,
            reg_s: 0,
            next_mem: 0xfffc,

            count: 0,
            mm: mm,

            inst: Instruction::Brk,
            addr: 0,
            ptr: 0,
            value: 0,
            cycle: 1,
        }
    }

    pub fn reset(&mut self) {
        info!("Reset CPU");

        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.reg_p = 0;

        // Load the reset vector
        self.reg_pc = self.mm.read_u16(0xfffc);
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
            0x20 => Jsr(Abs),
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

    fn process_op(&mut self, op: u8) {
        use Instruction::*;

        let inst = self.decode_op(op);

        //info!("{} - {} - {:?}", inst.info().0, inst.info().1, inst);

        match inst {
            Clc => self.ex_clc(),
            Cld => self.ex_cld(),
            Cli => self.ex_cli(),

            Sec => self.ex_sec(),
            Sed => self.ex_sed(),
            Sei => self.ex_sei(),

            Sta(m) => self.ex_sta(m),
            Stx(m) => self.ex_stx(m),
            Sty(m) => self.ex_sty(m),

            Lda(m) => self.ex_lda(m),
            Ldx(m) => self.ex_ldx(m),
            Ldy(m) => self.ex_ldy(m),

            Tax => self.ex_tax(),
            Tay => self.ex_tay(),
            Tsx => self.ex_tsx(),
            Txa => self.ex_txa(),
            Txs => self.ex_txs(),
            Tya => self.ex_tya(),

            Bcc => self.ex_bcc(),
            Bcs => self.ex_bcs(),
            Beq => self.ex_beq(),
            Bmi => self.ex_bmi(),
            Bne => self.ex_bne(),
            Bpl => self.ex_bpl(),
            Bvc => self.ex_bvc(),
            Bvs => self.ex_bvs(),

            _ => {
                warn!("Unimplemented Inst 0x{:02x}, {:?}", op, inst);
                self.reg_pc += 1;
            }
        }
    }

    pub fn tick(&mut self) {
        use Instruction::*;

        //let op = self.mm.read_u8(self.reg_pc as usize);

        //let inst = self.decode_op(op);

        //self.process_op(op);

        match self.cycle {
            1 => {
                // Fetch decode
                let op = self.mm.read_u8(self.reg_pc as usize);
                self.inst = self.decode_op(op);

                let operand = self.mm.read_u8(self.reg_pc as usize + 1);
                trace!(
                    "{:08}\t{:02}\t0x{:04x} - {:?} {:02x}",
                    self.count,
                    self.cycle,
                    self.reg_pc,
                    self.inst,
                    operand
                );
                self.reg_pc += 1;
                self.cycle = 2;
            }
            _ => {
                trace!(
                    "{:08}\t{:02}\t0x{:04x} - {:?}",
                    self.count,
                    self.cycle,
                    self.reg_pc,
                    self.inst
                );
                match self.inst {
                    Clc => self.ex_clc(),
                    Cld => self.ex_cld(),
                    Cli => self.ex_cli(),

                    Sec => self.ex_sec(),
                    Sed => self.ex_sed(),
                    Sei => self.ex_sei(),

                    Ahx(m) => self.ex_ahx(m),
                    Shx(m) => self.ex_shx(m),
                    Shy(m) => self.ex_shy(m),
                    Sax(m) => self.ex_sax(m),

                    Sta(m) => self.ex_sta(m),
                    Stx(m) => self.ex_stx(m),
                    Sty(m) => self.ex_sty(m),

                    Lda(m) => self.ex_lda(m),
                    Ldx(m) => self.ex_ldx(m),
                    Ldy(m) => self.ex_ldy(m),

                    Tax => self.ex_tax(),
                    Tay => self.ex_tay(),
                    Tsx => self.ex_tsx(),
                    Txa => self.ex_txa(),
                    Txs => self.ex_txs(),
                    Tya => self.ex_tya(),

                    Eor(m) => self.ex_eor(m),
                    Ora(m) => self.ex_ora(m),
                    And(m) => self.ex_and(m),
                    Adc(m) => self.ex_adc(m),
                    Sbc(m) => self.ex_sbc(m),
                    Cmp(m) => self.ex_cmp(m),
                    Bit(m) => self.ex_bit(m),
                    Lax(m) => self.ex_lax(m),
                    Nop(m) => self.ex_nop(m),

                    Bcc => self.ex_bcc(),
                    Bcs => self.ex_bcs(),
                    Beq => self.ex_beq(),
                    Bmi => self.ex_bmi(),
                    Bne => self.ex_bne(),
                    Bpl => self.ex_bpl(),
                    Bvc => self.ex_bvc(),
                    Bvs => self.ex_bvs(),

                    _ => (),
                }
            }
        }

        self.count += 1;
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

    fn ex_bcc(&mut self) {
        self.handle_branch();

        error!("Inclomplete");

        if self.cycle == 1 {
            if (self.reg_p & C) == 0 {
                self.reg_pc += self.value as u16;
            }
        }
    }

    fn ex_bcs(&mut self) {
        self.handle_branch();

        error!("Inclomplete");

        if self.cycle == 1 {
            if (self.reg_p & C) == C {
                self.reg_pc += self.value as u16;
            }
        }
    }

    fn ex_beq(&mut self) {
        self.handle_branch();

        error!("Inclomplete");

        if self.cycle == 1 {
            if (self.reg_p & Z) == Z {
                self.reg_pc += self.value as u16;
            }
        }
    }

    fn ex_bmi(&mut self) {
        self.handle_branch();

        error!("Inclomplete");

        if self.cycle == 1 {
            if (self.reg_p & N) == N {
                self.reg_pc += self.value as u16;
            }
        }
    }

    fn ex_bne(&mut self) {
        self.handle_branch();

        error!("Inclomplete");

        if self.cycle == 1 {
            if (self.reg_p & Z) == 0 {
                self.reg_pc += self.value as u16;
            }
        }
    }

    fn ex_bpl(&mut self) {
        self.handle_branch();

        error!("Inclomplete");

        if self.cycle == 1 {
            if (self.reg_p & N) == 0 {
                self.reg_pc = self.addr;
            }
        }
    }

    fn ex_bvc(&mut self) {
        self.handle_branch();

        error!("Inclomplete");

        if self.cycle == 1 {
            if (self.reg_p & V) == 0 {
                self.reg_pc += self.value as u16;
            }
        }
    }

    fn ex_bvs(&mut self) {
        self.handle_branch();

        error!("Inclomplete");

        if self.cycle == 1 {
            if (self.reg_p & V) == V {
                self.reg_pc += self.value as u16;
            }
        }
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

    fn handle_branch(&mut self) {
        let pc = self.reg_pc as usize;

        error!("Not cycle accurate yet");

        match self.cycle {
            2 => {
                self.value = self.mm.read_u8(pc);
                self.addr = 
                    if (0x80 & self.value) == 0x80 {
                        self.reg_pc - 3 - (!self.value) as u16 
                    } else {
                        self.reg_pc - 1 + self.value as u16
                    };
                self.reg_pc += 1;
                self.cycle = 1;
            }
            3 => {}
            4 => {}
            5 => {}
            _ => (),
        }
    }

    fn handle_read(&mut self, m: AddressMode) {
        let pc = self.reg_pc as usize;
        let addr = self.addr as usize;
        let ptr = self.ptr as usize;

        match m {
            AddressMode::Imp => {
                self.mm.read_u8(pc);
                self.cycle = 1;
            }
            AddressMode::Imm => {
                self.value = self.mm.read_u8(pc);
                self.reg_pc += 1;
                self.cycle = 1;
            }
            AddressMode::Abs => match self.cycle {
                2 => {
                    self.addr = self.mm.read_u8(pc) as u16;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (self.mm.read_u8(pc) as u16) << 8;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.mm.read_u8(addr);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Abx => {
                match self.cycle {
                    2 => {
                        self.addr = self.mm.read_u8(pc) as u16;
                        self.reg_pc += 1;
                        self.cycle += 1;
                    }
                    3 => {
                        self.addr = self.addr + self.reg_x as u16;
                        self.addr |= (self.mm.read_u8(pc) as u16) << 8;
                        self.reg_pc += 1;
                        self.cycle += 1;
                    }
                    4 => {
                        self.value = self.mm.read_u8(addr);
                        self.cycle += 1; // TODO: Handle the 4 cycle case
                    }
                    5 => {
                        self.value = self.mm.read_u8(addr);
                        self.cycle = 1;
                    }
                    _ => (),
                }
            }
            AddressMode::Aby => {
                match self.cycle {
                    2 => {
                        self.addr = self.mm.read_u8(pc) as u16;
                        self.reg_pc += 1;
                        self.cycle += 1;
                    }
                    3 => {
                        self.addr = self.addr + self.reg_y as u16;
                        self.addr |= (self.mm.read_u8(pc) as u16) << 8;
                        self.reg_pc += 1;
                        self.cycle += 1;
                    }
                    4 => {
                        self.value = self.mm.read_u8(addr);
                        self.cycle += 1; // TODO: Handle the 4 cycle case
                    }
                    5 => {
                        self.value = self.mm.read_u8(addr);
                        self.cycle = 1;
                    }
                    _ => (),
                }
            }
            AddressMode::Izx => match self.cycle {
                2 => {
                    self.ptr = self.mm.read_u8(pc);
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.mm.read_u8(ptr);
                    self.ptr += self.reg_x;
                    self.cycle += 1;
                }
                4 => {
                    self.addr = self.mm.read_u8(ptr) as u16;
                    self.cycle += 1;
                }
                5 => {
                    self.addr |= (self.mm.read_u8(ptr + 1) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.value = self.mm.read_u8(addr);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izy => match self.cycle {
                2 => {
                    self.ptr = self.mm.read_u8(pc);
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.mm.read_u8(ptr);
                    self.ptr += self.reg_y;
                    self.cycle += 1;
                }
                4 => {
                    self.addr = self.mm.read_u8(ptr) as u16;
                    self.cycle += 1;
                }
                5 => {
                    self.addr |= (self.mm.read_u8(ptr + 1) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.value = self.mm.read_u8(addr);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zp => match self.cycle {
                2 => {
                    self.addr = self.mm.read_u8(pc) as u16;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.value = self.mm.read_u8(addr);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpx => match self.cycle {
                2 => {
                    self.addr = self.mm.read_u8(pc) as u16;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.mm.read_u8(addr) + self.reg_x) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.mm.read_u8(addr & 0xff);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpy => match self.cycle {
                2 => {
                    self.addr = self.mm.read_u8(pc) as u16;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.mm.read_u8(addr) + self.reg_y) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.value = self.mm.read_u8(addr & 0xff);
                    self.cycle = 1;
                }
                _ => (),
            },
            _ => panic!("Invalid mode: {:?}", m),
        }
    }

    fn handle_write(&mut self, m: AddressMode) {
        let pc = self.reg_pc as usize;
        let addr = self.addr as usize;
        let ptr = self.ptr as usize;
        let value = self.value;

        match m {
            AddressMode::Abs => match self.cycle {
                2 => {
                    self.addr = self.mm.read_u8(pc) as u16;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.addr |= (self.mm.read_u8(pc) as u16) << 8;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                4 => {
                    self.mm.write_u8(addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Abx => {
                match self.cycle {
                    2 => {
                        self.addr = self.mm.read_u8(pc) as u16;
                        self.reg_pc += 1;
                        self.cycle += 1;
                    }
                    3 => {
                        self.addr = self.addr + self.reg_x as u16;
                        self.addr |= (self.mm.read_u8(pc) as u16) << 8;
                        self.reg_pc += 1;
                        self.cycle += 1;
                    }
                    4 => {
                        self.value = self.mm.read_u8(addr); // Value is ignored
                        self.cycle += 1;
                    }
                    5 => {
                        self.mm.write_u8(addr, value);
                        self.cycle = 1;
                    }
                    _ => (),
                }
            }
            AddressMode::Aby => {
                match self.cycle {
                    2 => {
                        self.addr = self.mm.read_u8(pc) as u16;
                        self.reg_pc += 1;
                        self.cycle += 1;
                    }
                    3 => {
                        self.addr = self.addr + self.reg_y as u16;
                        self.addr |= (self.mm.read_u8(pc) as u16) << 8;
                        self.reg_pc += 1;
                        self.cycle += 1;
                    }
                    4 => {
                        self.value = self.mm.read_u8(addr); // Value is ignored
                        self.cycle += 1;
                    }
                    5 => {
                        self.mm.write_u8(addr, value);
                        self.cycle = 1;
                    }
                    _ => (),
                }
            }
            AddressMode::Izx => match self.cycle {
                2 => {
                    self.ptr = self.mm.read_u8(pc);
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.mm.read_u8(ptr);
                    self.ptr += self.reg_x;
                    self.cycle += 1;
                }
                4 => {
                    self.addr = self.mm.read_u8(ptr) as u16;
                    self.cycle += 1;
                }
                5 => {
                    self.addr |= (self.mm.read_u8(ptr + 1) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.mm.write_u8(addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Izy => match self.cycle {
                2 => {
                    self.ptr = self.mm.read_u8(pc);
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.ptr = self.mm.read_u8(ptr);
                    self.ptr += self.reg_y;
                    self.cycle += 1;
                }
                4 => {
                    self.addr = self.mm.read_u8(ptr) as u16;
                    self.cycle += 1;
                }
                5 => {
                    self.addr |= (self.mm.read_u8(ptr + 1) as u16) << 8;
                    self.cycle += 1;
                }
                6 => {
                    self.mm.write_u8(addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zp => match self.cycle {
                2 => {
                    self.addr = self.mm.read_u8(pc) as u16;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.mm.write_u8(addr, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpx => match self.cycle {
                2 => {
                    self.addr = self.mm.read_u8(pc) as u16;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.mm.read_u8(addr) + self.reg_x) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.mm.write_u8(addr & 0xff, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            AddressMode::Zpy => match self.cycle {
                2 => {
                    self.addr = self.mm.read_u8(pc) as u16;
                    self.reg_pc += 1;
                    self.cycle += 1;
                }
                3 => {
                    self.addr = (self.mm.read_u8(addr) + self.reg_y) as u16;
                    self.cycle += 1;
                }
                4 => {
                    self.mm.write_u8(addr & 0xff, value);
                    self.cycle = 1;
                }
                _ => (),
            },
            _ => panic!("Invalid mode: {:?}", m),
        }
    }

    // LDA Load to A
    fn ex_lda(&mut self, m: AddressMode) {
        self.handle_read(m);

        if self.cycle == 1 {
            self.reg_a = self.value;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // LDX Load to X
    fn ex_ldx(&mut self, m: AddressMode) {
        self.handle_read(m);

        if self.cycle == 1 {
            self.reg_x = self.value;

            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_x);
        }
    }

    // LDY Load to Y
    fn ex_ldy(&mut self, m: AddressMode) {
        self.handle_read(m);

        if self.cycle == 1 {
            self.reg_y = self.value;
            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_y);
        }
    }

    // EOR A = A ^ M
    fn ex_eor(&mut self, m: AddressMode) {
        self.handle_read(m);

        if self.cycle == 1 {
            self.reg_a = self.value ^ self.reg_a;
            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // AND A = A & M
    fn ex_and(&mut self, m: AddressMode) {
        self.handle_read(m);

        if self.cycle == 1 {
            self.reg_a = self.value & self.reg_a;
            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // ORA A = A | M
    fn ex_ora(&mut self, m: AddressMode) {
        self.handle_read(m);

        if self.cycle == 1 {
            self.reg_a = self.value | self.reg_a;
            // Update N and Z flags
            stat_nz!(self.reg_p, self.reg_a);
        }
    }

    // ADC Add with carry A = A + M + C
    fn ex_adc(&mut self, m: AddressMode) {
        self.handle_read(m);

        //if self.cycle == 1 {
        //    self.reg_a = self.reg_a + self.value + (self.reg_p & C == C) as u8;
        //}

        // Update N and Z flags
        //stat_nz!(self.reg_p, self.reg_y);

        error!("Incomplete");
        //
    }

    fn ex_sbc(&mut self, m: AddressMode) {
        self.handle_read(m);

        error!("Incomplete");
    }

    fn ex_cmp(&mut self, m: AddressMode) {
        self.handle_read(m);

        error!("Incomplete");
    }

    fn ex_bit(&mut self, m: AddressMode) {
        self.handle_read(m);

        error!("Incomplete");
    }

    fn ex_lax(&mut self, m: AddressMode) {
        self.handle_read(m);

        error!("Incomplete");
    }

    fn ex_nop(&mut self, m: AddressMode) {
        self.handle_read(m);

        error!("Incomplete");
    }

    // AHX Store A & X & (ADDR_HI + 1) to memory
    fn ex_ahx(&mut self, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_a & self.reg_x & (self.addr >> 8) as u8 + 1;
        self.handle_write(m);
    }

    // SHX Store X & (ADDR_HI + 1)to memory
    fn ex_shx(&mut self, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_x & (self.addr >> 8) as u8 + 1;
        self.handle_write(m);
    }

    // SHY Store Y & (ADDR_HI + 1) to memory
    fn ex_shy(&mut self, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_y & (self.addr >> 8) as u8 + 1;
        self.handle_write(m);
    }

    // SAX Store A & X to memory
    fn ex_sax(&mut self, m: AddressMode) {
        info!("Unusual instrction {:?}", self.inst);
        self.value = self.reg_a & self.reg_x;
        self.handle_write(m);
    }

    // STA Store A to memory
    fn ex_sta(&mut self, m: AddressMode) {
        self.value = self.reg_a;
        self.handle_write(m);
    }

    // STX Store X to memory
    fn ex_stx(&mut self, m: AddressMode) {
        self.value = self.reg_x;
        self.handle_write(m);
    }

    // STY Store Y to memory
    fn ex_sty(&mut self, m: AddressMode) {
        self.value = self.reg_y;
        self.handle_write(m);
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
        write!(f, "PC:   {:04x}\nSTATUS: {:02x}\n", self.reg_pc, self.reg_p)?;
        write!(f, "================================================")?;
        Ok(())
    }
}
