const std = @import("std");
const opcodes = @import("opcodes.zig");

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
    memory: [MEMORY_SIZE]u8,
    opcodes: opcodes.OpCodesMap,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*CPU {
        const cpu: *CPU = try allocator.create(CPU);
        cpu.* = .{
            .registerA = 0,
            .registerX = 0,
            .registerY = 0,
            .status = CpuFlags.fromBitsTruncate(0b100100),
            .programCounter = 0,
            .stackPointer = STACK_RESET_VALUE,
            .memory = [_]u8{0} ** MEMORY_SIZE,
            .opcodes = try opcodes.createOpcodesMap(allocator),
            .allocator = allocator,
        };
        return cpu;
    }

    pub fn deinit(self: *CPU) void {
        self.opcodes.deinit();
        self.allocator.destroy(self);
    }

    pub fn memRead(self: *const CPU, addr: u16) u8 {
        return self.memory[addr];
    }

    pub fn memWrite(self: *CPU, addr: u16, data: u8) void {
        self.memory[addr] = data;
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
        self.setFlag(.CARRY, setBit7);

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
        var data = self.memRead(addr);
        data -%= 1;
        self.memWrite(addr, data);
        self.updateZeroAndNegativeFlags(data);
        return data;
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

        if (program.len + addr > self.memory.len) return error.OutOfMemory;
        @memcpy(self.memory[addr..][0..program.len], program);
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
        while (true) {
            const code = self.memRead(self.programCounter);
            self.programCounter += 1;
            const program_counter_state = self.programCounter;

            const opcode = self.opcodes.get(code) orelse return error.InvalidOpCode;

            switch (code) {
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
