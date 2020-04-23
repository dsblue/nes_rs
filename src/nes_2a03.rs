use crate::MemoryMap;

const OP_MODE_MASK: u8 = 0b0001_1100;
const OP_MODE_0: u8 = 0b0000_0000;
const OP_MODE_1: u8 = 0b0000_0100;
const OP_MODE_2: u8 = 0b0000_1000;
const OP_MODE_3: u8 = 0b0000_1100;
const OP_MODE_4: u8 = 0b0001_0000;
const OP_MODE_5: u8 = 0b0001_0100;
const OP_MODE_6: u8 = 0b0001_1000;
const OP_MODE_7: u8 = 0b0001_1100;

const N: u8 = 0b1000_0000; // Negitive
const V: u8 = 0b1000_0000; // Overflow
const D: u8 = 0b1000_0000; // Decimal
const I: u8 = 0b1000_0000; // Interrupt Disable
const Z: u8 = 0b1000_0000; // Zero
const C: u8 = 0b1000_0000; // Carry

#[derive(Debug)]
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
    Rel,
}

#[derive(Debug)]
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
    Nop,
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
    Ahx,
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
    Shx,
    Shy,
    Slo(AddressMode),
    Sre(AddressMode),
    Tas,
    Xaa,

    Invalid,
    Unimplemented,
}

impl Instruction {
    fn info(&self) -> (&str, &str) {
        use Instruction::*;

        match *self {
            Nop => ("nop", "No operation"),
            Clc => ("clc", "Clear Carry"),
            Lda(_) => ("lda", "Load Accumulator"),
            _ => ("--", "--"),
        }
    }
}

#[derive(Debug)]
pub struct Cpu6502 {
    reg_a: u8,      // Accumulator
    reg_x: u8,
    reg_y: u8,
    reg_pc: u16,    // Program counter
    reg_s: u8,  // Stack pointer
    reg_p: u8,  // Status

    next_mem: u16,
    count: u64,
    mm: MemoryMap,

    inst: Instruction,
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

            inst: Instruction::Nop,
            cycle: 0,
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

    fn fetch_op(&mut self) {
        let op = self.mm.read_u8(self.reg_pc as usize);

        self.inst = self.decode_op(op);
    }

    fn decode_op(&mut self, op: u8) -> Instruction {
        use AddressMode::*;
        use Instruction::*;

        match op {
            0x00 => Brk,
            0x01 => Ora(Izx),
            0x02 => Kil,
            0x03 => Slo(Izx),
            0x04 => Nop,
            0x05 => Ora(Zp),
            0x06 => Asl(Zp),
            0x07 => Slo(Zp),
            0x08 => Php,
            0x09 => Ora(Imm),
            0x0a => Asl(Imp),
            0x0b => Anc,
            0x0c => Nop,
            0x0d => Ora(Abs),
            0x0e => Asl(Abs),
            0x0f => Slo(Abs),
            0x10 => Bpl,
            0x11 => Ora(Izy),
            0x12 => Kil,
            0x13 => Slo(Izy),
            0x14 => Nop,
            0x15 => Ora(Zpx),
            0x16 => Asl(Zpx),
            0x17 => Slo(Zpx),
            0x18 => Clc,
            0x19 => Ora(Aby),
            0x1a => Nop,
            0x1b => Slo(Aby),
            0x1c => Nop,
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
            0x34 => Nop,
            0x35 => And(Zpx),
            0x36 => Rol(Zpx),
            0x37 => Rla(Zpx),
            0x38 => Sec,
            0x39 => And(Aby),
            0x3a => Nop,
            0x3b => Rla(Aby),
            0x3c => Nop,
            0x3d => And(Abx),
            0x3e => Rol(Abx),
            0x3f => Rla(Abx),
            0x40 => Rti,
            0x41 => Eor(Izx),
            0x42 => Kil,
            0x43 => Sre(Izx),
            0x44 => Nop,
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
            0x54 => Nop,
            0x55 => Eor(Zpx),
            0x56 => Lsr(Zpx),
            0x57 => Sre(Zpx),
            0x58 => Cli,
            0x59 => Eor(Aby),
            0x5a => Nop,
            0x5b => Sre(Aby),
            0x5c => Nop,
            0x5d => Eor(Abx),
            0x5e => Lsr(Abx),
            0x5f => Sre(Abx),
            0x60 => Rts,
            0x61 => Adc(Izx),
            0x62 => Kil,
            0x63 => Rra(Izx),
            0x64 => Nop,
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
            0x74 => Nop,
            0x75 => Adc(Zpx),
            0x76 => Ror(Zpx),
            0x77 => Rra(Zpx),
            0x78 => Sei,
            0x79 => Adc(Aby),
            0x7a => Nop,
            0x7b => Rra(Aby),
            0x7c => Nop,
            0x7d => Adc(Abx),
            0x7e => Ror(Abx),
            0x7f => Rra(Abx),
            0x80 => Nop,
            0x81 => Sta(Izx),
            0x82 => Nop,
            0x83 => Sax(Izx),
            0x84 => Sty(Zp),
            0x85 => Sta(Zp),
            0x86 => Stx(Zp),
            0x87 => Sax(Zp),
            0x88 => Dey,
            0x89 => Nop,
            0x8a => Txa,
            0x8b => Xaa,
            0x8c => Sty(Abs),
            0x8d => Sta(Abs),
            0x8e => Stx(Abs),
            0x8f => Sax(Abs),
            0x90 => Bcc,
            0x91 => Sta(Izy),
            0x92 => Kil,
            0x93 => Ahx,
            0x94 => Sty(Zpx),
            0x95 => Sta(Zpx),
            0x96 => Stx(Zpy),
            0x97 => Sax(Zpy),
            0x98 => Tya,
            0x99 => Sta(Aby),
            0x9a => Txs,
            0x9b => Tas,
            0x9c => Shy,
            0x9d => Sta(Abx),
            0x9e => Shx,
            0x9f => Ahx,
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
            0xc2 => Nop,
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
            0xd4 => Nop,
            0xd5 => Cmp(Zpx),
            0xd6 => Dec(Zpx),
            0xd7 => Dcp(Zpx),
            0xd8 => Cld,
            0xd9 => Cmp(Aby),
            0xda => Nop,
            0xdb => Dcp(Aby),
            0xdc => Nop,
            0xdd => Cmp(Abx),
            0xde => Dec(Abx),
            0xdf => Dcp(Abx),
            0xe0 => Cpx(Imm),
            0xe1 => Sbc(Izx),
            0xe2 => Nop,
            0xe3 => Isc(Izx),
            0xe4 => Cpx(Zp),
            0xe5 => Sbc(Zp),
            0xe6 => Inc(Zp),
            0xe7 => Isc(Zp),
            0xe8 => Inx,
            0xe9 => Sbc(Imm),
            0xea => Nop,
            0xeb => Sbc(Imm),
            0xec => Cpx(Abs),
            0xed => Sbc(Abs),
            0xee => Inc(Abs),
            0xef => Isc(Abs),
            0xf0 => Beq,
            0xf1 => Sbc(Izy),
            0xf2 => Kil,
            0xf3 => Isc(Izy),
            0xf4 => Nop,
            0xf5 => Sbc(Zpx),
            0xf6 => Inc(Zpx),
            0xf7 => Isc(Zpx),
            0xf8 => Sed,
            0xf9 => Sbc(Aby),
            0xfa => Nop,
            0xfb => Isc(Aby),
            0xfc => Nop,
            0xfd => Sbc(Abx),
            0xfe => Inc(Abx),
            0xff => Isc(Abx),
        }
    }

    fn process_op(&mut self, op: u8) {
        use AddressMode::*;
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
        let op = self.mm.read_u8(self.reg_pc as usize);

        let inst = self.decode_op(op);

        trace!(
            "{}: Addr: 0x{:04x} - Op: 0x{:02x} {:?}",
            self.count,
            self.reg_pc,
            op,
            inst
        );

        self.process_op(op);

        //self.ex_adc(0);

        //(INST_TABLE[0].exec)(self, 0);
        //self.process_op(0x00);
        self.count += 1;
    }

    fn set_status(&mut self, bits: u8) {
        self.reg_p |= bits;
    }

    fn clr_status(&mut self, bits: u8) {
        self.reg_p &= !bits;
    }

    fn ex_clc(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_p &= !C;
    }

    fn ex_cld(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_p &= !D;
    }

    fn ex_cli(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_p &= !I;
    }

    fn ex_sec(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_p |= C;
    }

    fn ex_sed(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_p |= D;
    }

    fn ex_sei(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_p |= I;
    }

    fn ex_bcc(&mut self) {
        let o = (self.reg_pc + 1) as usize;
        self.reg_pc += 1;
        self.cycle = 2; // More complicated
      
        if (self.reg_p & C == 0) {
            self.reg_pc += self.mm.read_u8(o) as u16;
        }
    }

    fn ex_bcs(&mut self) {
        let o = (self.reg_pc + 1) as usize;
        self.reg_pc += 1;
        self.cycle = 2; // More complicated
      
        if (self.reg_p & C == C) {
            self.reg_pc += self.mm.read_u8(o) as u16;
        }
    }

    fn ex_beq(&mut self) {
        let o = (self.reg_pc + 1) as usize;
        self.reg_pc += 1;
        self.cycle = 2; // More complicated
      
        if (self.reg_p & Z == Z) {
            self.reg_pc += self.mm.read_u8(o) as u16;
        }
    }

    fn ex_bmi(&mut self) {
        let o = (self.reg_pc + 1) as usize;
        self.reg_pc += 1;
        self.cycle = 2; // More complicated
      
        if (self.reg_p & N == N) {
            self.reg_pc += self.mm.read_u8(o) as u16;
        }
    }

    fn ex_bne(&mut self) {
        let o = (self.reg_pc + 1) as usize;
        self.reg_pc += 1;
        self.cycle = 2; // More complicated
      
        if (self.reg_p & Z == 0) {
            self.reg_pc += self.mm.read_u8(o) as u16;
        }
    }

    fn ex_bpl(&mut self) {
        let o = (self.reg_pc + 1) as usize;
        self.reg_pc += 1;
        self.cycle = 2; // More complicated
      
        if (self.reg_p & N == 0) {
            self.reg_pc += self.mm.read_u8(o) as u16;
        }
    }

    fn ex_bvc(&mut self) {
        let o = (self.reg_pc + 1) as usize;
        self.reg_pc += 1;
        self.cycle = 2; // More complicated
      
        if (self.reg_p & V == 0) {
            self.reg_pc += self.mm.read_u8(o) as u16;
        }
    }

    fn ex_bvs(&mut self) {
        let o = (self.reg_pc + 1) as usize;
        self.reg_pc += 1;
        self.cycle = 2; // More complicated
      
        if (self.reg_p & V == V) {
            self.reg_pc += self.mm.read_u8(o) as u16;
        }
    }

    // TAX Transfer A to X
    fn ex_tax(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_x = self.reg_a;

        if (self.reg_x == 0) {
            self.reg_p |= Z;
        }
        if (self.reg_x & 0x80 == 0x80) {
            self.reg_p |= N;
        }
    }

    // TAY Transfer A to Y
    fn ex_tay(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_y = self.reg_a;

        if (self.reg_y == 0) {
            self.reg_p |= Z;
        }
        if (self.reg_y & 0x80 == 0x80) {
            self.reg_p |= N;
        }
    }

    // TSX Transfer S to X
    fn ex_tsx(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_x = self.reg_s;

        if (self.reg_x == 0) {
            self.reg_p |= Z;
        }
        if (self.reg_x & 0x80 == 0x80) {
            self.reg_p |= N;
        }
    }

    // TXA Transfer X to A
    fn ex_txa(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_a = self.reg_x;

        if (self.reg_a == 0) {
            self.reg_p |= Z;
        }
        if (self.reg_a & 0x80 == 0x80) {
            self.reg_p |= N;
        }
    }

    // TXS Transfer X to stack pointer
    fn ex_txs(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_s = self.reg_x;
    }

    // TYA Transfer Y to A
    fn ex_tya(&mut self) {
        self.reg_pc += 1;
        self.cycle = 2;
        self.reg_a = self.reg_y;

        if (self.reg_a == 0) {
            self.reg_p |= Z;
        } else {
            self.reg_p &= !Z;
        }

        if (self.reg_a & 0x80 == 0x80) {
            self.reg_p |= N;
        } else {
            self.reg_p &= !N;
        }
    }

    // LDY Load to Y
    fn ex_ldy(&mut self, m: AddressMode) {
        let o = (self.reg_pc + 1) as usize;
        let x = self.reg_x as usize;

        let addr = match m {
            AddressMode::Imm => {
                self.reg_pc += 2;
                self.cycle = 2;
                o
            }
            AddressMode::Zp => {
                self.reg_pc += 2;
                self.cycle = 3;
                self.mm.read_u8(o) as usize
            }
            AddressMode::Zpy => {
                self.reg_pc += 2;
                self.cycle = 4;
                let o = self.mm.read_u8(o) as usize;
                self.mm.read_u8((o + x) & 0xff) as usize
            }
            AddressMode::Abs => {
                self.reg_pc += 3;
                self.cycle = 4;
                self.mm.read_u16(o) as usize
            }
            AddressMode::Aby => {
                self.reg_pc += 3;
                self.cycle = 5;
                self.mm.read_u16(o) as usize + x
            }
            _ => {
                panic!("Bad Addressing mode {:?}", m);
            }
        };

        self.reg_y = self.mm.read_u8(addr);

        if (self.reg_y == 0) {
            self.reg_p |= Z;
        } else {
            self.reg_p &= !Z;
        }

        if (self.reg_y & 0x80 == 0x80) {
            self.reg_p |= N;
        } else {
            self.reg_p &= !N;
        }
    }

    // LDX Load to X
    fn ex_ldx(&mut self, m: AddressMode) {
        let o = (self.reg_pc + 1) as usize;
        let y = self.reg_y as usize;

        let addr = match m {
            AddressMode::Imm => {
                self.reg_pc += 2;
                self.cycle = 2;
                o
            }
            AddressMode::Zp => {
                self.reg_pc += 2;
                self.cycle = 3;
                self.mm.read_u8(o) as usize
            }
            AddressMode::Zpy => {
                self.reg_pc += 2;
                self.cycle = 4;
                let o = self.mm.read_u8(o) as usize;
                self.mm.read_u8((o + y) & 0xff) as usize
            }
            AddressMode::Abs => {
                self.reg_pc += 3;
                self.cycle = 4;
                self.mm.read_u16(o) as usize
            }
            AddressMode::Aby => {
                self.reg_pc += 3;
                self.cycle = 5;
                self.mm.read_u16(o) as usize + y
            }
            _ => {
                panic!("Bad Addressing mode {:?}", m);
            }
        };

        self.reg_x = self.mm.read_u8(addr);

        if (self.reg_x == 0) {
            self.reg_p |= Z;
        } else {
            self.reg_p &= !Z;
        }

        if (self.reg_x & 0x80 == 0x80) {
            self.reg_p |= N;
        } else {
            self.reg_p &= !N;
        }
    }

    // LDA Load to A
    fn ex_lda(&mut self, m: AddressMode) {
        let o = (self.reg_pc + 1) as usize;
        let x = self.reg_x as usize;
        let y = self.reg_y as usize;

        let addr = match m {
            AddressMode::Imm => {
                self.reg_pc += 2;
                self.cycle = 2;
                o
            }
            AddressMode::Zp => {
                self.reg_pc += 2;
                self.cycle = 3;
                self.mm.read_u8(o) as usize
            }
            AddressMode::Zpx => {
                self.reg_pc += 2;
                self.cycle = 4;
                let o = self.mm.read_u8(o) as usize;
                self.mm.read_u8((o + x) & 0xff) as usize
            }
            AddressMode::Abs => {
                self.reg_pc += 3;
                self.cycle = 4;
                self.mm.read_u16(o) as usize
            }
            AddressMode::Abx => {
                self.reg_pc += 3;
                self.cycle = 5;
                self.mm.read_u16(o) as usize + x
            }
            AddressMode::Aby => {
                self.reg_pc += 3;
                self.cycle = 5;
                self.mm.read_u16(o) as usize + y
            }
            AddressMode::Izx => {
                self.reg_pc += 2;
                self.cycle = 6;
                let o = self.mm.read_u8(o) as usize;
                self.mm.read_u16((o + x) & 0xff) as usize
            }
            AddressMode::Izy => {
                self.reg_pc += 2;
                self.cycle = 6;
                let o = self.mm.read_u8(o) as usize;
                (self.mm.read_u16(o) as usize + y) & 0xff
            }
            _ => {
                panic!("Bad Addressing mode {:?}", m);
            }
        };

        self.reg_a = self.mm.read_u8(addr);

        if (self.reg_a == 0) {
            self.reg_p |= Z;
        } else {
            self.reg_p &= !Z;
        }

        if (self.reg_a & 0x80 == 0x80) {
            self.reg_p |= N;
        } else {
            self.reg_p &= !N;
        }
    }

    // STA Store A to memory
    fn ex_sta(&mut self, m: AddressMode) {
        let o = (self.reg_pc + 1) as usize;
        let x = self.reg_x as usize;
        let y = self.reg_y as usize;

        let addr = match m {
            AddressMode::Zp => {
                self.reg_pc += 2;
                self.cycle = 3;
                self.mm.read_u8(o) as usize
            }
            AddressMode::Zpx => {
                self.reg_pc += 2;
                self.cycle = 4;
                let o = self.mm.read_u8(o) as usize;
                self.mm.read_u8((o + x) & 0xff) as usize
            }
            AddressMode::Abs => {
                self.reg_pc += 3;
                self.cycle = 4;
                self.mm.read_u16(o) as usize
            }
            AddressMode::Abx => {
                self.reg_pc += 3;
                self.cycle = 5;
                self.mm.read_u16(o) as usize + x
            }
            AddressMode::Aby => {
                self.reg_pc += 3;
                self.cycle = 5;
                self.mm.read_u16(o) as usize + y
            }
            AddressMode::Izx => {
                self.reg_pc += 2;
                self.cycle = 6;
                let o = self.mm.read_u8(o) as usize;
                self.mm.read_u16((o + x) & 0xff) as usize
            }
            AddressMode::Izy => {
                self.reg_pc += 2;
                self.cycle = 6;
                let o = self.mm.read_u8(o) as usize;
                (self.mm.read_u16(o) as usize + y) & 0xff
            }
            _ => {
                panic!("Bad Addressing mode {:?}", m);
            }
        };

        self.mm.write_u8(addr, self.reg_a);
    }

    // STX Store X to memory
    fn ex_stx(&mut self, m: AddressMode) {
        let o = (self.reg_pc + 1) as usize;
        let x = self.reg_x as usize;
        let y = self.reg_y as usize;

        let addr = match m {
            AddressMode::Zp => {
                self.reg_pc += 2;
                self.cycle = 3;
                self.mm.read_u8(o) as usize
            }
            AddressMode::Abs => {
                self.reg_pc += 3;
                self.cycle = 4;
                self.mm.read_u16(o) as usize
            }
            AddressMode::Zpy => {
                self.reg_pc += 2;
                self.cycle = 4;
                let o = self.mm.read_u8(o) as usize;
                self.mm.read_u8((o + y) & 0xff) as usize
            }
            _ => {
                panic!("Bad Addressing mode {:?}", m);
            }
        };

        self.mm.write_u8(addr, self.reg_x);
    }

    // STY Store Y to memory
    fn ex_sty(&mut self, m: AddressMode) {
        let o = (self.reg_pc + 1) as usize;
        let x = self.reg_x as usize;
        let y = self.reg_y as usize;

        let addr = match m {
            AddressMode::Zp => {
                self.reg_pc += 2;
                self.cycle = 3;
                self.mm.read_u8(o) as usize
            }
            AddressMode::Zpx => {
                self.reg_pc += 2;
                self.cycle = 4;
                let o = self.mm.read_u8(o) as usize;
                self.mm.read_u8((o + x) & 0xff) as usize
            }
            AddressMode::Abs => {
                self.reg_pc += 3;
                self.cycle = 4;
                self.mm.read_u16(o) as usize
            }
            _ => {
                panic!("Bad Addressing mode {:?}", m);
            }
        };

        self.mm.write_u8(addr, self.reg_y);
    }

    // ADD to accumulator with carry
    fn ex_adc(&mut self, op: u8) {
        trace!("adc {}", op);
    }

    fn ex_nop(&mut self) {
        trace!("nop");
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
