const std = @import("std");
const opcodes = @import("opcodes.zig");
const Bus = @import("bus.zig").Bus;

const MEMORY_SIZE = 0x10000;
const RESET_VECTOR = 0xFFFC;
const DEFAULT_ROM_START = 0x8000;

const STACK = 0x0100;
const STACK_RESET_VALUE = 0xFD;

pub const CpuFlags = struct {
    pub const CARRY = @as(u8, 0b00000001);
    pub const ZERO = @as(u8, 0b00000010);
    pub const INTERRUPT_DISABLE = @as(u8, 0b00000100);
    pub const DECIMAL_MODE = @as(u8, 0b00001000);
    pub const BREAK = @as(u8, 0b00010000);
    pub const BREAK2 = @as(u8, 0b00100000);
    pub const OVERFLOW = @as(u8, 0b01000000);
    pub const NEGATIVE = @as(u8, 0b10000000);

    pub fn fromBitsTruncate(bits: u8) u8 {
        return bits;
    }

    pub fn contains(flags: u8, flag: u8) bool {
        return (flags & flag) != 0;
    }

    pub fn insert(flags: *u8, flag: u8) void {
        flags.* |= flag;
    }

    pub fn remove(flags: *u8, flag: u8) void {
        flags.* &= ~flag;
    }
};

pub const CpuError = error{
    OutOfMemory,
    InvalidOpCode,
    InvalidAddressing,
};

pub const AddressingMode = enum {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    NoneAddressing,
};

pub const CPU = struct {
    registerA: u8,
    registerX: u8,
    registerY: u8,
    status: u8,
    programCounter: u16,
    stackPointer: u8,
    opcodes: opcodes.OpCodesMap,
    bus: Bus,
    pub fn init(allocator: std.mem.Allocator) !CPU {
        return CPU {
            .registerA = 0,
            .registerX = 0,
            .registerY = 0,
            .status = CpuFlags.fromBitsTruncate(0b100100),
            .programCounter = 0,
            .stackPointer = STACK_RESET_VALUE,
            .opcodes = try opcodes.createOpcodesMap(allocator),
            .bus = Bus.init(),
        };
    }

    pub fn deinit(self: *CPU) void {
        self.opcodes.deinit();
    }

    pub fn debugState(self: *CPU) void {
        std.debug.print(
            \\CPU State:
                 \\  A: 0x{X:0>2}
                 \\  Direction: 0x{X:0>2}
                 \\  Status: 0b{b:0>8}
                 \\  PC: 0x{X:0>4}
                 \\
                , .{
                self.registerA,
                self.memRead(0x02),
                self.status,
                self.programCounter,
            }
        );
    }

    pub fn memRead(self: *const CPU, addr: u16) u8 {
        return self.bus.memRead(addr);
    }

    pub fn memWrite(self: *CPU, addr: u16, data: u8) void {
        self.bus.memWrite(addr, data);
    }

    pub fn memReadU16(self: *const CPU, pos: u16) u16 {
        const lo = @as(u16, self.memRead(pos));
        const hi = @as(u16, self.memRead(pos + 1));
        return (hi << 8) | lo;
    }

    pub fn memWriteU16(self: *CPU, pos: u16, data: u16) void {
        const hi = @as(u8, @truncate(data >> 8));
        const lo = @as(u8, @truncate(data & 0xff));
        self.memWrite(pos, lo);
        self.memWrite(pos + 1, hi);
    }

    fn updateZeroAndNegativeFlags(self: *CPU, result: u8) void {
        if (result == 0) {
            CpuFlags.insert(&self.status, CpuFlags.ZERO);
        } else {
            CpuFlags.remove(&self.status, CpuFlags.ZERO);
        }

        if ((result & 0b1000_0000) != 0) {
            CpuFlags.insert(&self.status, CpuFlags.NEGATIVE);
        } else {
            CpuFlags.remove(&self.status, CpuFlags.NEGATIVE);
        }
    }

    fn setFlag(self: *CPU, flag: u8, value: bool) void {
        if (value) {
            CpuFlags.insert(&self.status, flag);
        } else {
            CpuFlags.remove(&self.status, flag);
        }
    }

    fn setCarryFlag(self: *CPU) void {
        CpuFlags.insert(&self.status, CpuFlags.CARRY);
    }

    fn clearCarryFlag(self: *CPU) void {
        CpuFlags.remove(&self.status, CpuFlags.CARRY);
    }

    fn _ldy(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const value = self.memRead(addr);
        self.registerY = value;
        self.updateZeroAndNegativeFlags(self.registerY);
    }

    fn _ldx(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const value = self.memRead(addr);
        self.registerX = value;
        self.updateZeroAndNegativeFlags(self.registerX);
    }

    fn _lda(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const value = self.memRead(addr);
        self.registerA = value;
        self.updateZeroAndNegativeFlags(self.registerA);
    }

    fn _sta(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        self.memWrite(addr, self.registerA);
    }

    fn setRegisterA(self: *CPU, value: u8) void {
        self.registerA = value;
        self.updateZeroAndNegativeFlags(self.registerA);
    }

    fn _and(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const value = self.memRead(addr);
        self.setRegisterA(self.registerA & value);
    }

    fn _eor(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const value = self.memRead(addr);
        self.setRegisterA(self.registerA ^ value);
    }

    fn _ora(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const value = self.memRead(addr);
        self.setRegisterA(self.registerA | value);
    }

    fn _tax(self: *CPU) void {
        self.registerX = self.registerA;
        self.updateZeroAndNegativeFlags(self.registerX);
    }

    fn _inx(self: *CPU) void {
        self.registerX = self.registerX +% 1;
        self.updateZeroAndNegativeFlags(self.registerX);
    }

    fn _iny(self: *CPU) void {
        self.registerY = self.registerY +% 1;
        self.updateZeroAndNegativeFlags(self.registerY);
    }

    /// note: ignoring decimal mode
    /// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    fn addToRegisterA(self: *CPU, value: u8) void {
        const carry: u16 = if (self.status & CpuFlags.CARRY != 0) 1 else 0;
        const sum: u16 = @as(u16, self.registerA) + @as(u16, value) + carry;

        if (sum > 0xFF) {
            self.status |= CpuFlags.CARRY;
        } else {
            self.status &= ~CpuFlags.CARRY;
        }

        const result = @as(u8, @truncate(sum));

        if (((value ^ result) & (result ^ self.registerA) & 0x80) != 0) {
            self.status |= CpuFlags.OVERFLOW;
        } else {
            self.status &= ~CpuFlags.OVERFLOW;
        }

        self.registerA = result;
        self.updateZeroAndNegativeFlags(self.registerA);
    }

    fn _sbc(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const data = self.memRead(addr);
        const inverted = ~data;
        self.addToRegisterA(inverted);
    }

    fn _adc(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const data = self.memRead(addr);
        self.addToRegisterA(data);
    }

    fn stackPop(self: *CPU) u8 {
        self.stackPointer +%= 1;
        return self.memRead(@as(u16, STACK) + @as(u16, self.stackPointer));
    }

    fn stackPush(self: *CPU, value: u8) void {
        self.memWrite(@as(u16, STACK) + @as(u16, self.stackPointer), value);
        self.stackPointer -%= 1;
    }

    fn stackPushU16(self: *CPU, value: u16) void {
        const hi = @as(u8, @truncate(value >> 8));
        const lo = @as(u8, @truncate(value & 0xff));
        self.stackPush(hi);
        self.stackPush(lo);
    }

    fn stackPopU16(self: *CPU) u16 {
        const lo = @as(u16, self.stackPop());
        const hi = @as(u16, self.stackPop());
        return (hi << 8) | lo;
    }

    fn aslAccumulator(self: *CPU) void {
        // Check highest bit before shift
        const setBit7 = (self.registerA >> 7) == 1;

        // Perform shift
        self.registerA <<= 1;

        // Update carry flag based on old bit 7
        self.setFlag(CpuFlags.CARRY, setBit7);

        // Update other flags
        self.updateZeroAndNegativeFlags(self.registerA);
    }

    fn _asl(self: *CPU, mode: AddressingMode) u8 {
        const addr = self.getOperandAddress(mode);
        var data = self.memRead(addr);

        // Check if bit 7 is set before the shift
        if (data >> 7 == 1) {
            self.setCarryFlag();
        } else {
            self.clearCarryFlag();
        }

        data = data << 1;
        self.memWrite(addr, data);
        self.updateZeroAndNegativeFlags(data);
        return data;
    }

    fn lsrAccumulator(self: *CPU) void {
        // Check if bit 0 is set before the shift
        if (self.registerA & 1 == 1) {
            self.setCarryFlag();
        } else {
            self.clearCarryFlag();
        }

        self.registerA = self.registerA >> 1;
        self.setRegisterA(self.registerA);
    }

    fn _lsr(self: *CPU, mode: AddressingMode) u8 {
        const addr = self.getOperandAddress(mode);
        var data = self.memRead(addr);

        if (data & 1 == 1) {
            self.setCarryFlag();
        } else {
            self.clearCarryFlag();
        }

        data = data >> 1;
        self.memWrite(addr, data);
        self.updateZeroAndNegativeFlags(data);
        return data;
    }

    fn rolAccumulator(self: *CPU) void {
        const oldCarry = CpuFlags.contains(self.status, CpuFlags.CARRY);

        // Check highest bit before shift
        if (self.registerA >> 7 == 1) {
            self.setCarryFlag();
        } else {
            self.clearCarryFlag();
        }

        self.registerA = self.registerA << 1;
        if (oldCarry) {
            self.registerA |= 1;
        }
        self.setRegisterA(self.registerA);
    }

    fn _rol(self: *CPU, mode: AddressingMode) u8 {
        const addr = self.getOperandAddress(mode);
        var data = self.memRead(addr);
        const oldCarry = CpuFlags.contains(self.status, CpuFlags.CARRY);

        if (data >> 7 == 1) {
            self.setCarryFlag();
        } else {
            self.clearCarryFlag();
        }

        data = data << 1;
        if (oldCarry) {
            data |= 1;
        }

        self.memWrite(addr, data);
        self.updateZeroAndNegativeFlags(data);
        return data;
    }

    fn _ror(self: *CPU, mode: AddressingMode) u8 {
        const addr = self.getOperandAddress(mode);
        var data = self.memRead(addr);
        const oldCarry = CpuFlags.contains(self.status, CpuFlags.CARRY);

        if (data & 1 == 1) {
            self.setCarryFlag();
        } else {
            self.clearCarryFlag();
        }

        data = data >> 1;
        if (oldCarry) {
            data |= 0b10000000;
        }

        self.memWrite(addr, data);
        self.updateZeroAndNegativeFlags(data);
        return data;
    }

    fn rorAccumulator(self: *CPU) void {
        var data = self.registerA;
        const oldCarry = CpuFlags.contains(self.status, CpuFlags.CARRY);

        if (data & 1 == 1) {
            self.setCarryFlag();
        } else {
            self.clearCarryFlag();
        }

        data = data >> 1;
        if (oldCarry) {
            data |= 0b10000000;
        }

        self.setRegisterA(data);
    }

    fn _inc(self: *CPU, mode: AddressingMode) u8 {
        const addr = self.getOperandAddress(mode);
        var data = self.memRead(addr);
        data +%= 1;
        self.memWrite(addr, data);
        self.updateZeroAndNegativeFlags(data);
        return data;
    }

    fn _dey(self: *CPU) void {
        self.registerY -%= 1;
        self.updateZeroAndNegativeFlags(self.registerY);
    }

    fn _dex(self: *CPU) void {
        self.registerX -%= 1;
        self.updateZeroAndNegativeFlags(self.registerX);
    }

    fn _dec(self: *CPU, mode: AddressingMode) u8 {
        const addr = self.getOperandAddress(mode);
        const data = self.memRead(addr);
        std.debug.print("\nDEC operation on address 0x{X:0>4}:", .{addr});
        std.debug.print("\n  Before: 0x{X:0>2}", .{data});
        
        const result = data -% 1;
        std.debug.print("\n  After: 0x{X:0>2}", .{result});
        
        self.memWrite(addr, result);
        self.updateZeroAndNegativeFlags(result);
        return result;
    }

    fn _pla(self: *CPU) void {
        self.registerA = self.stackPop();
        self.setRegisterA(self.registerA);
    }

    fn _plp(self: *CPU) void {
        self.status = self.stackPop();
        CpuFlags.remove(&self.status, CpuFlags.BREAK);
        CpuFlags.insert(&self.status, CpuFlags.BREAK2);
    }

    fn _php(self: *CPU) void {
        // http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
        var flags = self.status;
        CpuFlags.insert(&flags, CpuFlags.BREAK);
        CpuFlags.insert(&flags, CpuFlags.BREAK2);
        self.stackPush(flags);
    }

    fn _bit(self: *CPU, mode: AddressingMode) void {
        const addr = self.getOperandAddress(mode);
        const data = self.memRead(addr);
        const maskedValue = self.registerA & data;

        if (maskedValue == 0) {
            CpuFlags.insert(&self.status, CpuFlags.ZERO);
        } else {
            CpuFlags.remove(&self.status, CpuFlags.ZERO);
        }

        // Set negative flag based on bit 7 of data
        self.setFlag(CpuFlags.NEGATIVE, (data & 0b10000000) > 0);

        // Set overflow flag based on bit 6 of data
        self.setFlag(CpuFlags.OVERFLOW, (data & 0b01000000) > 0);
    }

    fn compare(self: *CPU, mode: AddressingMode, compare_with: u8) void {
        const addr = self.getOperandAddress(mode);
        const data = self.memRead(addr);

        // Debug print
        std.debug.print("\nCompare: A=0x{X:0>2} with data=0x{X:0>2}\n", 
            .{compare_with, data});

        if (data <= compare_with) {
            CpuFlags.insert(&self.status, CpuFlags.CARRY);
        } else {
            CpuFlags.remove(&self.status, CpuFlags.CARRY);
        }

        const result = compare_with -% data;
        self.updateZeroAndNegativeFlags(result);
    }

    fn branch(self: *CPU, condition: bool) void {
        if (condition) {
            const jump = @as(i8, @bitCast(self.memRead(self.programCounter)));
            const jumpAddr = self.programCounter +% 1 // wrapping add
            +% @as(u16, @bitCast(@as(i16, jump))); // sign extend and wrapping add

            self.programCounter = jumpAddr;
        }
    }

    pub fn reset(self: *CPU) void {
        self.registerA = 0;
        self.registerX = 0;
        self.registerY = 0;
        self.stackPointer = STACK_RESET_VALUE;
        self.status = CpuFlags.fromBitsTruncate(0b100100);

        self.programCounter = self.memReadU16(RESET_VECTOR);
    }

    pub fn load(self: *CPU, program: []const u8, load_address: ?u16) !void {
        const addr = load_address orelse DEFAULT_ROM_START;

        var i: u16 = 0;
        while (i < program.len) : (i += 1) {
            self.memWrite(addr + i, program[i]);
        }
        self.memWriteU16(RESET_VECTOR, addr);
    }

    pub fn loadAndRun(self: *CPU, program: []const u8) !void {
        try self.load(program, null);
        self.reset();
        try self.run();
    }

    pub fn getOperandAddress(self: *const CPU, mode: AddressingMode) u16 {
        return switch (mode) {
            .Immediate => self.programCounter,
            .ZeroPage => @as(u16, self.memRead(self.programCounter)),
            .Absolute => self.memReadU16(self.programCounter),
            .ZeroPageX => {
                const pos = self.memRead(self.programCounter);
                return @as(u16, pos +% self.registerX);
            },
            .ZeroPageY => {
                const pos = self.memRead(self.programCounter);
                return @as(u16, pos +% self.registerY);
            },
            .AbsoluteX => {
                const base = self.memReadU16(self.programCounter);
                return base +% @as(u16, self.registerX);
            },
            .AbsoluteY => {
                const base = self.memReadU16(self.programCounter);
                return base +% @as(u16, self.registerY);
            },
            .IndirectX => {
                const base = self.memRead(self.programCounter);
                const ptr = base +% self.registerX;
                const lo = self.memRead(@as(u16, ptr));
                const hi = self.memRead(@as(u16, ptr +% 1));
                return (@as(u16, hi) << 8) | @as(u16, lo);
            },
            .IndirectY => {
                const base = self.memRead(self.programCounter);
                const lo = self.memRead(@as(u16, base));
                const hi = self.memRead(@as(u16, base +% 1));
                const deref_base = (@as(u16, hi) << 8) | @as(u16, lo);
                return deref_base +% @as(u16, self.registerY);
            },
            .NoneAddressing => {
                std.debug.panic("mode {any} is not supported", .{mode});
            },
        };
    }

    pub fn run(self: *CPU) !void {
        try self.runWithCallback({},struct {
            fn callback(ctx: void, cpu: *CPU) error{}!void {
                _ = cpu;
                _ = ctx;
            }
        }.callback);
    }

    pub fn runWithCallback(self: *CPU, context: anytype, comptime callback: fn (@TypeOf(context), *CPU) error{}!void) !void {
        while (true) {
            try callback(context, self);

            const code = self.memRead(self.programCounter);
            self.programCounter += 1;
            const program_counter_state = self.programCounter;
            std.debug.print("PC: 0x{X:0>4} Opcode: 0x{X:0>2}\n", .{
                self.programCounter,
                code,
            });
            self.debugState();

            const opcode = self.opcodes.get(code) orelse return error.InvalidOpCode;

            switch (code) {
                // ASL - Arithmetic Shift Left
                0x0a => self.aslAccumulator(), // ASL A

                0x06, 0x16, 0x0e, 0x1e => {
                    _ = self._asl(opcode.mode);
                },

                // LSR - Logical Shift Right  
                0x4a => self.lsrAccumulator(), 
                
                // LSR A
                0x46, 0x56, 0x4e, 0x5e => {
                    _ = self._lsr(opcode.mode);
                },

                // SEC - Set Carry Flag
                0x38 => self.setCarryFlag(),

                // LDA
                0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1 => {
                    self._lda(opcode.mode);
                },

                // LDX
                0xa2, 0xa6, 0xb6, 0xae, 0xbe => {
                    self._ldx(opcode.mode);
                },

                // LDY
                0xa0, 0xa4, 0xb4, 0xac, 0xbc => {
                    self._ldy(opcode.mode);
                },

                // STA
                0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91 => {
                    self._sta(opcode.mode);
                },

                // STX
                0x86, 0x96, 0x8e => {
                    const addr = self.getOperandAddress(opcode.mode);
                    self.memWrite(addr, self.registerX);
                },

                // STY
                0x84, 0x94, 0x8c => {
                    const addr = self.getOperandAddress(opcode.mode);
                    self.memWrite(addr, self.registerY);
                },

                // ROL
                0x2a => self.rolAccumulator(),
                0x26, 0x36, 0x2e, 0x3e => {
                    _ = self._rol(opcode.mode);
                },

                // ROR
                0x6a => self.rorAccumulator(),
                0x66, 0x76, 0x6e, 0x7e => {
                    _ = self._ror(opcode.mode);
                },

                // INC
                0xe6, 0xf6, 0xee, 0xfe => {
                    _ = self._inc(opcode.mode);
                },

                // // DEC
                // 0xc6, 0xd6, 0xce, 0xde => {
                //     _ = self._dec(opcode.mode);
                // },

                // DEC
                0xc6, 0xd6, 0xce, 0xde => {
                    std.debug.print("\nExecuting DEC instruction", .{});
                    _ = self._dec(opcode.mode);
                    std.debug.print("\nAfter DEC - Memory at $10: 0x{X:0>2}\n", .{self.memRead(0x10)});
                },

                // Stack operations
                0x48 => self.stackPush(self.registerA), // PHA
                0x68 => self._pla(), // PLA
                0x08 => self._php(), // PHP
                0x28 => self._plp(), // PLP

                // BIT
                0x24, 0x2c => self._bit(opcode.mode),

                // Compare operations
                0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1 => self.compare(opcode.mode, self.registerA), // CMP
                0xe0, 0xe4, 0xec => self.compare(opcode.mode, self.registerX), // CPX
                0xc0, 0xc4, 0xcc => self.compare(opcode.mode, self.registerY), // CPY

                0x20 => {
                    self.stackPushU16(self.programCounter + 2 - 1);
                    const target_address = self.memReadU16(self.programCounter);
                    self.programCounter = target_address;
                },

                // JMP Absolute
                0x4c => {
                    const mem_address = self.memReadU16(self.programCounter);
                    self.programCounter = mem_address;
                },

                // JMP Indirect
                0x6c => {
                    const addr = self.memReadU16(self.programCounter);
                    
                    const indirect_ref = if (addr & 0x00FF == 0x00FF) 
                        blk: {
                            const lo = @as(u16, self.memRead(addr));
                            const hi = @as(u16, self.memRead(addr & 0xFF00));
                            break :blk (hi << 8) | lo;
                        } else 
                        self.memReadU16(addr);

                    self.programCounter = indirect_ref;
                },

                // RTS
                0x60 => {
                    self.programCounter = self.stackPopU16() + 1;
                },

                // RTI
                0x40 => {
                    self.status = self.stackPop();
                    CpuFlags.remove(&self.status, CpuFlags.BREAK);
                    CpuFlags.insert(&self.status, CpuFlags.BREAK2);
                    self.programCounter = self.stackPopU16();
                },

                // Branch instructions
                0x10 => self.branch(!CpuFlags.contains(self.status, CpuFlags.NEGATIVE)), // BPL
                0x30 => self.branch(CpuFlags.contains(self.status, CpuFlags.NEGATIVE)), // BMI
                0x50 => self.branch(!CpuFlags.contains(self.status, CpuFlags.OVERFLOW)), // BVC
                0x70 => self.branch(CpuFlags.contains(self.status, CpuFlags.OVERFLOW)), // BVS
                0x90 => self.branch(!CpuFlags.contains(self.status, CpuFlags.CARRY)), // BCC
                0xB0 => self.branch(CpuFlags.contains(self.status, CpuFlags.CARRY)), // BCS
                0xD0 => self.branch(!CpuFlags.contains(self.status, CpuFlags.ZERO)), // BNE
                0xF0 => self.branch(CpuFlags.contains(self.status, CpuFlags.ZERO)), // BEQ

                0xaa => self._tax(), // TAX
                0xe8 => self._inx(), // INX
                0xc8 => self._iny(), // INY
                0x88 => self._dey(), // DEY
                0xca => self._dex(), // DEX

                // CLD
                0xd8 => self.status &= ~CpuFlags.DECIMAL_MODE,

                // CLI
                0x58 => self.status &= ~CpuFlags.INTERRUPT_DISABLE,

                // CLV
                0xb8 => self.status &= ~CpuFlags.OVERFLOW,

                // CLC - Clear Carry Flag
                0x18 => self.clearCarryFlag(),

                // SEI
                0x78 => self.status |= CpuFlags.INTERRUPT_DISABLE,

                // SED
                0xf8 => self.status |= CpuFlags.DECIMAL_MODE,

                // TAY
                0xa8 => {
                    self.registerY = self.registerA;
                    self.updateZeroAndNegativeFlags(self.registerY);
                },

                // TSX
                0xba => {
                    self.registerX = self.stackPointer;
                    self.updateZeroAndNegativeFlags(self.registerX);
                },

                // TXA
                0x8a => {
                    self.registerA = self.registerX;
                    self.updateZeroAndNegativeFlags(self.registerA);
                },

                // TXS
                0x9a => {
                    self.stackPointer = self.registerX;
                },

                // TYA
                0x98 => {
                    self.registerA = self.registerY;
                    self.updateZeroAndNegativeFlags(self.registerA);
                },

                // AND
                0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31 => {
                    self._and(opcode.mode);
                },

                // EOR
                0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51 => {
                    self._eor(opcode.mode);
                },

                // ORA
                0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11 => {
                    self._ora(opcode.mode);
                },

                // ADC
                0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71 => {
                    self._adc(opcode.mode);
                },

                // SBC
                0xe9, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1 => {
                    self._sbc(opcode.mode);
                },

                // NOP
                0xea => {},

                0x00 => return, // BRK
                else => return error.InvalidOpCode,
            }

            if (program_counter_state == self.programCounter) {
                self.programCounter += @as(u16, opcode.len - 1);
            }
        }
    }
};

test "0xa9 lda immediate load data" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0x00 });
    try std.testing.expectEqual(@as(u8, 5), cpu.registerA);
    try std.testing.expect((cpu.status & 0b0000_0010) == 0);
    try std.testing.expect((cpu.status & 0b1000_0000) == 0);
}

test "0xa9 lda zero flag" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x00, 0x00 });
    try std.testing.expect((cpu.status & 0b0000_0010) == 0b10);
}

test "0xa9 lda negative flag" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0xff, 0x00 });
    try std.testing.expect((cpu.status & 0b1000_0000) == 0b1000_0000);
}

test "LDA from memory" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    cpu.memWrite(0x10, 0x55);

    // In Zig we need to create a slice from an array
    const program = [_]u8{ 0xa5, 0x10, 0x00 };
    try cpu.loadAndRun(&program);

    try std.testing.expectEqual(@as(u8, 0x55), cpu.registerA);
}

test "0x85 sta store a in memory" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0x85, 0x10, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x05), cpu.memRead(0x10));
}

test "0xaa tax move a to x" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x0A, 0xaa, 0x00 });
    try std.testing.expectEqual(@as(u8, 10), cpu.registerX);
}

test "5 ops working together" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0xc0, 0xaa, 0xe8, 0x00 });
    try std.testing.expectEqual(@as(u8, 0xc1), cpu.registerX);
}

test "inx overflow" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00 });
    try std.testing.expectEqual(@as(u8, 1), cpu.registerX);
}

test "0xa2 ldx immediate load data" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa2, 0x05, 0x00 });
    try std.testing.expectEqual(@as(u8, 5), cpu.registerX);
    try std.testing.expect((cpu.status & 0b0000_0010) == 0);
    try std.testing.expect((cpu.status & 0b1000_0000) == 0);
}

test "0xa0 ldy immediate load data" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa0, 0x05, 0x00 });
    try std.testing.expectEqual(@as(u8, 5), cpu.registerY);
    try std.testing.expect((cpu.status & 0b0000_0010) == 0);
    try std.testing.expect((cpu.status & 0b1000_0000) == 0);
}

test "0xc8 iny increment y" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa0, 0x05, 0xc8, 0x00 });
    try std.testing.expectEqual(@as(u8, 6), cpu.registerY);
}

test "0x88 dey decrement y" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa0, 0x05, 0x88, 0x00 });
    try std.testing.expectEqual(@as(u8, 4), cpu.registerY);
}

test "0xca dex decrement x" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa2, 0x05, 0xca, 0x00 });
    try std.testing.expectEqual(@as(u8, 4), cpu.registerX);
}

test "0x29 and immediate" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0x29, 0x03, 0x00 });
    try std.testing.expectEqual(@as(u8, 1), cpu.registerA);
}

test "0x49 eor immediate" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0x49, 0x03, 0x00 });
    try std.testing.expectEqual(@as(u8, 6), cpu.registerA);
}

test "0x09 ora immediate" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0x09, 0x03, 0x00 });
    try std.testing.expectEqual(@as(u8, 7), cpu.registerA);
}

test "0x69 adc immediate" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0x69, 0x03, 0x00 });
    try std.testing.expectEqual(@as(u8, 8), cpu.registerA);
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == 0);
}

test "0xe9 sbc immediate" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x05, 0xe9, 0x03, 0x00 });
    try std.testing.expectEqual(@as(u8, 1), cpu.registerA);
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == 1);
}

test "0xa8 tay transfer a to y" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x0A, 0xa8, 0x00 });
    try std.testing.expectEqual(@as(u8, 10), cpu.registerY);
}

test "0xba tsx transfer stack pointer to x" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    // No need to manually set stack pointer, it's initialized to STACK_RESET_VALUE (0xFD)
    try cpu.loadAndRun(&[_]u8{ 0xba, 0x00 });
    try std.testing.expectEqual(@as(u8, STACK_RESET_VALUE), cpu.registerX);
}

test "0x8a txa transfer x to a" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    // Use LDX immediate to set X register, then transfer to A
    try cpu.loadAndRun(&[_]u8{ 0xa2, 0x10, 0x8a, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x10), cpu.registerA);
}

test "0x9a txs transfer x to stack pointer" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    // Use LDX immediate to set X register, then transfer to stack pointer
    try cpu.loadAndRun(&[_]u8{ 0xa2, 0x10, 0x9a, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x10), cpu.stackPointer);
}

test "0x98 tya transfer y to a" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    // Use LDY immediate to set Y register, then transfer to A
    try cpu.loadAndRun(&[_]u8{ 0xa0, 0x10, 0x98, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x10), cpu.registerA);
}

test "0x0a asl accumulator" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x02, 0x0a, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x04), cpu.registerA);
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == 0);
}

test "0x0a asl accumulator with carry" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0b10000010, 0x0a, 0x00 });
    try std.testing.expectEqual(@as(u8, 0b00000100), cpu.registerA);
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == CpuFlags.CARRY);
}

test "0x4a lsr accumulator" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x04, 0x4a, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x02), cpu.registerA);
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == 0);
}

test "0x4a lsr accumulator with carry" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0b00000011, 0x4a, 0x00 });
    try std.testing.expectEqual(@as(u8, 0b00000001), cpu.registerA);
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == CpuFlags.CARRY);
}

test "0x2a rol accumulator" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x02, 0x2a, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x04), cpu.registerA);
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == 0);
}

test "0x6a ror accumulator" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0xa9, 0x04, 0x6a, 0x00 });
    try std.testing.expectEqual(@as(u8, 0x02), cpu.registerA);
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == 0);
}

test "0xd8 cld clear decimal flag" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    cpu.status |= CpuFlags.DECIMAL_MODE;
    try cpu.loadAndRun(&[_]u8{ 0xd8, 0x00 });
    try std.testing.expect((cpu.status & CpuFlags.DECIMAL_MODE) == 0);
}

test "0x58 cli clear interrupt disable" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    cpu.status |= CpuFlags.INTERRUPT_DISABLE;
    try cpu.loadAndRun(&[_]u8{ 0x58, 0x00 });
    try std.testing.expect((cpu.status & CpuFlags.INTERRUPT_DISABLE) == 0);
}

test "0xb8 clv clear overflow" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    cpu.status |= CpuFlags.OVERFLOW;
    try cpu.loadAndRun(&[_]u8{ 0xb8, 0x00 });
    try std.testing.expect((cpu.status & CpuFlags.OVERFLOW) == 0);
}

test "0x18 clc clear carry" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    cpu.status |= CpuFlags.CARRY;
    try cpu.loadAndRun(&[_]u8{ 0x18, 0x00 });
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == 0);
}

test "0x38 sec set carry" {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    defer cpu.deinit();
    try cpu.loadAndRun(&[_]u8{ 0x38, 0x00 });
    try std.testing.expect((cpu.status & CpuFlags.CARRY) == CpuFlags.CARRY);
}

test "dec operation boundary conditions and snake movement simulation" {
    const allocator = std.heap.page_allocator;
    
    // Test case 1: Basic DEC operation with detailed debugging
    {
        var cpu = try CPU.init(allocator);
        defer cpu.deinit();
        
        const program = [_]u8{
            0xa9, 0x05,     // LDA #$05
            0x85, 0x10,     // STA $10
            0xc6, 0x10,     // DEC $10
            0x00,           // BRK
        };
        
        try cpu.load(&program, 0x0600);
        cpu.reset();
        
        // Debug initial state
        std.debug.print("\nInitial CPU state:", .{});
        std.debug.print("\nPC: 0x{X:0>4}", .{cpu.programCounter});
        std.debug.print("\nMemory at $10: 0x{X:0>2}", .{cpu.memRead(0x10)});
        
        // Execute LDA #$05
        try cpu.run();
        
        // Final debug output
        std.debug.print("\n\nFinal CPU state:", .{});
        std.debug.print("\nA: 0x{X:0>2}", .{cpu.registerA});
        std.debug.print("\nPC: 0x{X:0>4}", .{cpu.programCounter});
        std.debug.print("\nMemory at $10: 0x{X:0>2}", .{cpu.memRead(0x10)});
        std.debug.print("\nExpected memory at $10: 0x04\n", .{});
        
        try std.testing.expectEqual(@as(u8, 0x04), cpu.memRead(0x10));
    }
}

// test "dec operation boundary conditions and snake movement simulation" {
//     const allocator = std.heap.page_allocator;
    
//     // Test case 1: Basic DEC operation
//     {
//         var cpu = try CPU.init(allocator);
//         defer cpu.deinit();
        
//         // Load program
//         try cpu.load(&[_]u8{
//             0xa9, 0x05,     // LDA #$05
//             0x85, 0x10,     // STA $10
//             0xc6, 0x10,     // DEC $10
//             0x00,           // BRK
//         }, 0x0600);
        
//         // Reset and run
//         cpu.reset();
//         try cpu.run();

//         // Debug output
//         std.debug.print("\nTest Case 1:\n", .{});
//         std.debug.print("Memory at $10: 0x{X:0>2}\n", .{cpu.memRead(0x10)});
//         std.debug.print("Expected: 0x04\n", .{});
        
//         try std.testing.expectEqual(@as(u8, 0x04), cpu.memRead(0x10));
//     }

//     // Test case 2: DEC around zero
//     {
//         var cpu = try CPU.init(allocator);
//         defer cpu.deinit();
        
//         try cpu.load(&[_]u8{
//             0xa9, 0x01,     // LDA #$01
//             0x85, 0x10,     // STA $10
//             0xc6, 0x10,     // DEC $10
//             0x00,           // BRK
//         }, 0x0600);
        
//         cpu.reset();
//         try cpu.run();

//         std.debug.print("\nTest Case 2:\n", .{});
//         std.debug.print("Memory at $10: 0x{X:0>2}\n", .{cpu.memRead(0x10)});
//         std.debug.print("Expected: 0x00\n", .{});
        
//         try std.testing.expectEqual(@as(u8, 0x00), cpu.memRead(0x10));
//     }

//     // Test case 3: DEC wrapping from 0x00 to 0xFF
//     {
//         var cpu = try CPU.init(allocator);
//         defer cpu.deinit();
        
//         try cpu.load(&[_]u8{
//             0xa9, 0x00,     // LDA #$00
//             0x85, 0x10,     // STA $10
//             0xc6, 0x10,     // DEC $10
//             0x00,           // BRK
//         }, 0x0600);
        
//         cpu.reset();
//         try cpu.run();

//         std.debug.print("\nTest Case 3:\n", .{});
//         std.debug.print("Memory at $10: 0x{X:0>2}\n", .{cpu.memRead(0x10)});
//         std.debug.print("Expected: 0xFF\n", .{});
        
//         try std.testing.expectEqual(@as(u8, 0xFF), cpu.memRead(0x10));
//     }

//     // Test case 4: Simulate snake left movement
//     {
//         var cpu = try CPU.init(allocator);
//         defer cpu.deinit();
        
//         try cpu.load(&[_]u8{
//             0xa9, 0x11,     // LDA #$11
//             0x85, 0x10,     // STA $10
//             0xa9, 0x08,     // LDA #$08
//             0x85, 0x02,     // STA $02
//             0xc6, 0x10,     // DEC $10
//             0x00,           // BRK
//         }, 0x0600);
        
//         cpu.reset();
//         try cpu.run();

//         std.debug.print("\nTest Case 4:\n", .{});
//         std.debug.print("Memory at $10: 0x{X:0>2}\n", .{cpu.memRead(0x10)});
//         std.debug.print("Expected: 0x10\n", .{});
        
//         try std.testing.expectEqual(@as(u8, 0x10), cpu.memRead(0x10));
//     }
// }