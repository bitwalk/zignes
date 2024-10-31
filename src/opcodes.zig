const std = @import("std");
const cpu = @import("cpu.zig");

// Define the OpCode struct
pub const OpCode = struct {
    code: u8,
    mnemonic: []const u8,
    len: u8,
    cycles: u8,
    mode: cpu.AddressingMode,

    // Constructor function
    pub fn init(
        code: u8,
        mnemonic: []const u8,
        len: u8,
        cycles: u8,
        mode: cpu.AddressingMode,
    ) OpCode {
        return OpCode{
            .code = code,
            .mnemonic = mnemonic,
            .len = len,
            .cycles = cycles,
            .mode = mode,
        };
    }
};

// Create a type for our opcodes map
pub const OpCodesMap = std.AutoHashMap(u8, *const OpCode);

// Define all CPU opcodes
pub const cpu_ops_codes = [_]OpCode{
    // System
    OpCode.init(0x00, "BRK", 1, 7, .NoneAddressing),
    OpCode.init(0xea, "NOP", 1, 2, .NoneAddressing),

    // Arithmetic
    OpCode.init(0x69, "ADC", 2, 2, .Immediate),
    OpCode.init(0x65, "ADC", 2, 3, .ZeroPage),
    OpCode.init(0x75, "ADC", 2, 4, .ZeroPageX),
    OpCode.init(0x6d, "ADC", 3, 4, .Absolute),
    OpCode.init(0x7d, "ADC", 3, 4, .AbsoluteX), // +1 if page crossed
    OpCode.init(0x79, "ADC", 3, 4, .AbsoluteY), // +1 if page crossed
    OpCode.init(0x61, "ADC", 2, 6, .IndirectX),
    OpCode.init(0x71, "ADC", 2, 5, .IndirectY), // +1 if page crossed

    OpCode.init(0xe9, "SBC", 2, 2, .Immediate),
    OpCode.init(0xe5, "SBC", 2, 3, .ZeroPage),
    OpCode.init(0xf5, "SBC", 2, 4, .ZeroPageX),
    OpCode.init(0xed, "SBC", 3, 4, .Absolute),
    OpCode.init(0xfd, "SBC", 3, 4, .AbsoluteX), // +1 if page crossed
    OpCode.init(0xf9, "SBC", 3, 4, .AbsoluteY), // +1 if page crossed
    OpCode.init(0xe1, "SBC", 2, 6, .IndirectX),
    OpCode.init(0xf1, "SBC", 2, 5, .IndirectY), // +1 if page crossed

    OpCode.init(0x29, "AND", 2, 2, .Immediate),
    OpCode.init(0x25, "AND", 2, 3, .ZeroPage),
    OpCode.init(0x35, "AND", 2, 4, .ZeroPageX),
    OpCode.init(0x2d, "AND", 3, 4, .Absolute),
    OpCode.init(0x3d, "AND", 3, 4, .AbsoluteX), // +1 if page crossed
    OpCode.init(0x39, "AND", 3, 4, .AbsoluteY), // +1 if page crossed
    OpCode.init(0x21, "AND", 2, 6, .IndirectX),
    OpCode.init(0x31, "AND", 2, 5, .IndirectY), // +1 if page crossed

    OpCode.init(0x49, "EOR", 2, 2, .Immediate),
    OpCode.init(0x45, "EOR", 2, 3, .ZeroPage),
    OpCode.init(0x55, "EOR", 2, 4, .ZeroPageX),
    OpCode.init(0x4d, "EOR", 3, 4, .Absolute),
    OpCode.init(0x5d, "EOR", 3, 4, .AbsoluteX), // +1 if page crossed
    OpCode.init(0x59, "EOR", 3, 4, .AbsoluteY), // +1 if page crossed
    OpCode.init(0x41, "EOR", 2, 6, .IndirectX),
    OpCode.init(0x51, "EOR", 2, 5, .IndirectY), // +1 if page crossed

    OpCode.init(0x09, "ORA", 2, 2, .Immediate),
    OpCode.init(0x05, "ORA", 2, 3, .ZeroPage),
    OpCode.init(0x15, "ORA", 2, 4, .ZeroPageX),
    OpCode.init(0x0d, "ORA", 3, 4, .Absolute),
    OpCode.init(0x1d, "ORA", 3, 4, .AbsoluteX), // +1 if page crossed
    OpCode.init(0x19, "ORA", 3, 4, .AbsoluteY), // +1 if page crossed
    OpCode.init(0x01, "ORA", 2, 6, .IndirectX),
    OpCode.init(0x11, "ORA", 2, 5, .IndirectY), // +1 if page crossed

    // Shifts
    OpCode.init(0x0a, "ASL", 1, 2, .NoneAddressing), // Accumulator
    OpCode.init(0x06, "ASL", 2, 5, .ZeroPage),
    OpCode.init(0x16, "ASL", 2, 6, .ZeroPageX),
    OpCode.init(0x0e, "ASL", 3, 6, .Absolute),
    OpCode.init(0x1e, "ASL", 3, 7, .AbsoluteX),

    OpCode.init(0x4a, "LSR", 1, 2, .NoneAddressing), // Accumulator
    OpCode.init(0x46, "LSR", 2, 5, .ZeroPage),
    OpCode.init(0x56, "LSR", 2, 6, .ZeroPageX),
    OpCode.init(0x4e, "LSR", 3, 6, .Absolute),
    OpCode.init(0x5e, "LSR", 3, 7, .AbsoluteX),

    OpCode.init(0x2a, "ROL", 1, 2, .NoneAddressing), // Accumulator
    OpCode.init(0x26, "ROL", 2, 5, .ZeroPage),
    OpCode.init(0x36, "ROL", 2, 6, .ZeroPageX),
    OpCode.init(0x2e, "ROL", 3, 6, .Absolute),
    OpCode.init(0x3e, "ROL", 3, 7, .AbsoluteX),

    OpCode.init(0x6a, "ROR", 1, 2, .NoneAddressing), // Accumulator
    OpCode.init(0x66, "ROR", 2, 5, .ZeroPage),
    OpCode.init(0x76, "ROR", 2, 6, .ZeroPageX),
    OpCode.init(0x6e, "ROR", 3, 6, .Absolute),
    OpCode.init(0x7e, "ROR", 3, 7, .AbsoluteX),

    // Increments, Decrements
    OpCode.init(0xe6, "INC", 2, 5, .ZeroPage),
    OpCode.init(0xf6, "INC", 2, 6, .ZeroPageX),
    OpCode.init(0xee, "INC", 3, 6, .Absolute),
    OpCode.init(0xfe, "INC", 3, 7, .AbsoluteX),

    OpCode.init(0xe8, "INX", 1, 2, .NoneAddressing),
    OpCode.init(0xc8, "INY", 1, 2, .NoneAddressing),

    OpCode.init(0xc6, "DEC", 2, 5, .ZeroPage),
    OpCode.init(0xd6, "DEC", 2, 6, .ZeroPageX),
    OpCode.init(0xce, "DEC", 3, 6, .Absolute),
    OpCode.init(0xde, "DEC", 3, 7, .AbsoluteX),

    OpCode.init(0xca, "DEX", 1, 2, .NoneAddressing),
    OpCode.init(0x88, "DEY", 1, 2, .NoneAddressing),

    // Compare operations
    OpCode.init(0xc9, "CMP", 2, 2, .Immediate),
    OpCode.init(0xc5, "CMP", 2, 3, .ZeroPage),
    OpCode.init(0xd5, "CMP", 2, 4, .ZeroPageX),
    OpCode.init(0xcd, "CMP", 3, 4, .Absolute),
    OpCode.init(0xdd, "CMP", 3, 4, .AbsoluteX), // +1 if page crossed
    OpCode.init(0xd9, "CMP", 3, 4, .AbsoluteY), // +1 if page crossed
    OpCode.init(0xc1, "CMP", 2, 6, .IndirectX),
    OpCode.init(0xd1, "CMP", 2, 5, .IndirectY), // +1 if page crossed

    OpCode.init(0xe0, "CPX", 2, 2, .Immediate),
    OpCode.init(0xe4, "CPX", 2, 3, .ZeroPage),
    OpCode.init(0xec, "CPX", 3, 4, .Absolute),

    OpCode.init(0xc0, "CPY", 2, 2, .Immediate),
    OpCode.init(0xc4, "CPY", 2, 3, .ZeroPage),
    OpCode.init(0xcc, "CPY", 3, 4, .Absolute),

    // Branches
    OpCode.init(0x4c, "JMP", 3, 3, .NoneAddressing),
    OpCode.init(0x6c, "JMP", 3, 5, .NoneAddressing), // Indirect with 6502 bug

    OpCode.init(0x20, "JSR", 3, 6, .NoneAddressing),
    OpCode.init(0x60, "RTS", 1, 6, .NoneAddressing),

    OpCode.init(0x40, "RTI", 1, 6, .NoneAddressing),

    OpCode.init(0xd0, "BNE", 2, 2, .NoneAddressing), // +1 if branch succeeds +2 if to a new page
    OpCode.init(0x70, "BVS", 2, 2, .NoneAddressing),
    OpCode.init(0x50, "BVC", 2, 2, .NoneAddressing),
    OpCode.init(0x30, "BMI", 2, 2, .NoneAddressing),
    OpCode.init(0xf0, "BEQ", 2, 2, .NoneAddressing),
    OpCode.init(0xb0, "BCS", 2, 2, .NoneAddressing),
    OpCode.init(0x90, "BCC", 2, 2, .NoneAddressing),
    OpCode.init(0x10, "BPL", 2, 2, .NoneAddressing),

    OpCode.init(0x24, "BIT", 2, 3, .ZeroPage),
    OpCode.init(0x2c, "BIT", 3, 4, .Absolute),

    // Loads
    OpCode.init(0xa9, "LDA", 2, 2, .Immediate),
    OpCode.init(0xa5, "LDA", 2, 3, .ZeroPage),
    OpCode.init(0xb5, "LDA", 2, 4, .ZeroPageX),
    OpCode.init(0xad, "LDA", 3, 4, .Absolute),
    OpCode.init(0xbd, "LDA", 3, 4, .AbsoluteX), // +1 if page crossed
    OpCode.init(0xb9, "LDA", 3, 4, .AbsoluteY), // +1 if page crossed
    OpCode.init(0xa1, "LDA", 2, 6, .IndirectX),
    OpCode.init(0xb1, "LDA", 2, 5, .IndirectY), // +1 if page crossed

    OpCode.init(0xa2, "LDX", 2, 2, .Immediate),
    OpCode.init(0xa6, "LDX", 2, 3, .ZeroPage),
    OpCode.init(0xb6, "LDX", 2, 4, .ZeroPageY),
    OpCode.init(0xae, "LDX", 3, 4, .Absolute),
    OpCode.init(0xbe, "LDX", 3, 4, .AbsoluteY), // +1 if page crossed

    OpCode.init(0xa0, "LDY", 2, 2, .Immediate),
    OpCode.init(0xa4, "LDY", 2, 3, .ZeroPage),
    OpCode.init(0xb4, "LDY", 2, 4, .ZeroPageX),
    OpCode.init(0xac, "LDY", 3, 4, .Absolute),
    OpCode.init(0xbc, "LDY", 3, 4, .AbsoluteX), // +1 if page crossed

    // Stores
    OpCode.init(0x85, "STA", 2, 3, .ZeroPage),
    OpCode.init(0x95, "STA", 2, 4, .ZeroPageX),
    OpCode.init(0x8d, "STA", 3, 4, .Absolute),
    OpCode.init(0x9d, "STA", 3, 5, .AbsoluteX),
    OpCode.init(0x99, "STA", 3, 5, .AbsoluteY),
    OpCode.init(0x81, "STA", 2, 6, .IndirectX),
    OpCode.init(0x91, "STA", 2, 6, .IndirectY),

    OpCode.init(0x86, "STX", 2, 3, .ZeroPage),
    OpCode.init(0x96, "STX", 2, 4, .ZeroPageY),
    OpCode.init(0x8e, "STX", 3, 4, .Absolute),

    OpCode.init(0x84, "STY", 2, 3, .ZeroPage),
    OpCode.init(0x94, "STY", 2, 4, .ZeroPageX),
    OpCode.init(0x8c, "STY", 3, 4, .Absolute),

    // Flags clear
    OpCode.init(0xD8, "CLD", 1, 2, .NoneAddressing),
    OpCode.init(0x58, "CLI", 1, 2, .NoneAddressing),
    OpCode.init(0xb8, "CLV", 1, 2, .NoneAddressing),
    OpCode.init(0x18, "CLC", 1, 2, .NoneAddressing),
    OpCode.init(0x38, "SEC", 1, 2, .NoneAddressing),
    OpCode.init(0x78, "SEI", 1, 2, .NoneAddressing),
    OpCode.init(0xf8, "SED", 1, 2, .NoneAddressing),

    // Register transfers
    OpCode.init(0xaa, "TAX", 1, 2, .NoneAddressing),
    OpCode.init(0xa8, "TAY", 1, 2, .NoneAddressing),
    OpCode.init(0xba, "TSX", 1, 2, .NoneAddressing),
    OpCode.init(0x8a, "TXA", 1, 2, .NoneAddressing),
    OpCode.init(0x9a, "TXS", 1, 2, .NoneAddressing),
    OpCode.init(0x98, "TYA", 1, 2, .NoneAddressing),

    // Stack operations
    OpCode.init(0x48, "PHA", 1, 3, .NoneAddressing),
    OpCode.init(0x68, "PLA", 1, 4, .NoneAddressing),
    OpCode.init(0x08, "PHP", 1, 3, .NoneAddressing),
    OpCode.init(0x28, "PLP", 1, 4, .NoneAddressing),

};

// Function to create the opcodes map
pub fn createOpcodesMap(allocator: std.mem.Allocator) !OpCodesMap {
    var map = OpCodesMap.init(allocator);

    for (&cpu_ops_codes) |*opcode| {
        try map.put(opcode.code, opcode);
    }

    return map;
}

// Utility function to get an opcode
pub fn getOpcode(map: *const OpCodesMap, code: u8) ?*const OpCode {
    return map.get(code);
}