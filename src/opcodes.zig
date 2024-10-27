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
    OpCode.init(0x00, "BRK", 1, 7, .NoneAddressing),
    OpCode.init(0xaa, "TAX", 1, 2, .NoneAddressing),
    OpCode.init(0xe8, "INX", 1, 2, .NoneAddressing),

    OpCode.init(0xa9, "LDA", 2, 2, .Immediate),
    OpCode.init(0xa5, "LDA", 2, 3, .ZeroPage),
    OpCode.init(0xb5, "LDA", 2, 4, .ZeroPage_X),
    OpCode.init(0xad, "LDA", 3, 4, .Absolute),
    OpCode.init(0xbd, "LDA", 3, 4, .Absolute_X),
    OpCode.init(0xb9, "LDA", 3, 4, .Absolute_Y),
    OpCode.init(0xa1, "LDA", 2, 6, .Indirect_X),
    OpCode.init(0xb1, "LDA", 2, 5, .Indirect_Y),

    OpCode.init(0x85, "STA", 2, 3, .ZeroPage),
    OpCode.init(0x95, "STA", 2, 4, .ZeroPage_X),
    OpCode.init(0x8d, "STA", 3, 4, .Absolute),
    OpCode.init(0x9d, "STA", 3, 5, .Absolute_X),
    OpCode.init(0x99, "STA", 3, 5, .Absolute_Y),
    OpCode.init(0x81, "STA", 2, 6, .Indirect_X),
    OpCode.init(0x91, "STA", 2, 6, .Indirect_Y),
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
