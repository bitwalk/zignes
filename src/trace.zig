const std = @import("std");
const CPU = @import("cpu.zig").CPU;
const AddressingMode = @import("cpu.zig").AddressingMode;
const Mem = @import("cpu.zig").Mem;
const OpCode = @import("opcodes.zig").OpCode;
const Bus = @import("bus.zig").Bus;
const Rom = @import("rom.zig").Rom;
const testing = std.testing;

pub fn trace(cpu: *const CPU, allocator: std.mem.Allocator) ![]const u8 {
    const code = cpu.memRead(cpu.programCounter);
    const ops = cpu.opcodes.get(code) orelse return error.UnknownOpcode;

    const begin = cpu.programCounter;
    var hexDump = std.ArrayList(u8).init(allocator);
    defer hexDump.deinit();
    
    try hexDump.append(code);

    // Get memory address and stored value
    const memInfo = switch (ops.mode) {
        .Immediate, .NoneAddressing => .{ 0, 0 },
        else => blk: {
            const addr = cpu.getAbsoluteAddress(ops.mode, begin + 1);
            break :blk .{ addr, cpu.memRead(addr) };
        },
    };
    const memAddr = memInfo[0];
    const storedValue = memInfo[1];

    // Build instruction string
    var tmp = std.ArrayList(u8).init(allocator);
    defer tmp.deinit();

    switch (ops.len) {
        1 => {
            switch (ops.code) {
                0x0a, 0x4a, 0x2a, 0x6a => try tmp.appendSlice("A "),
                else => {},
            }
        },
        2 => {
            const address = cpu.memRead(begin + 1);
            try hexDump.append(address);

            switch (ops.mode) {
                .Immediate => try std.fmt.format(tmp.writer(), "#${X:0>2}", .{address}),
                .ZeroPage => try std.fmt.format(tmp.writer(), "${X:0>2} = {X:0>2}", .{ memAddr, storedValue }),
                .ZeroPage_X => try std.fmt.format(tmp.writer(), "${X:0>2},X @ {X:0>2} = {X:0>2}", .{ address, memAddr, storedValue }),
                .ZeroPage_Y => try std.fmt.format(tmp.writer(), "${X:0>2},Y @ {X:0>2} = {X:0>2}", .{ address, memAddr, storedValue }),
                .Indirect_X => try std.fmt.format(tmp.writer(), "(${X:0>2},X) @ {X:0>2} = {X:0>4} = {X:0>2}", .{
                    address,
                    address +% cpu.registerX,
                    memAddr,
                    storedValue,
                }),
                .Indirect_Y => try std.fmt.format(tmp.writer(), "(${X:0>2}),Y = {X:0>4} @ {X:0>4} = {X:0>2}", .{
                    address,
                    memAddr -% @as(u16, cpu.registerY),
                    memAddr,
                    storedValue,
                }),
                .NoneAddressing => {
                    const jumpAddr = @as(usize, @intCast(begin + 2)) +% @as(usize, @bitCast(@as(i8, @bitCast(address))));
                    try std.fmt.format(tmp.writer(), "${X:0>4}", .{jumpAddr});
                },
                else => @panic("unexpected addressing mode has ops-len 2"),
            }
        },
        3 => {
            const addressLo = cpu.memRead(begin + 1);
            const addressHi = cpu.memRead(begin + 2);
            try hexDump.append(addressLo);
            try hexDump.append(addressHi);

            const address = cpu.memReadU16(begin + 1);

            switch (ops.mode) {
                .NoneAddressing => {
                    if (ops.code == 0x6c) { // jmp indirect
                        const jmpAddr = if (address & 0x00FF == 0x00FF) blk: {
                            const lo = cpu.memRead(address);
                            const hi = cpu.memRead(address & 0xFF00);
                            break :blk (@as(u16, hi) << 8) | lo;
                        } else cpu.memReadU16(address);

                        try std.fmt.format(tmp.writer(), "(${X:0>4}) = {X:0>4}", .{ address, jmpAddr });
                    } else {
                        try std.fmt.format(tmp.writer(), "${X:0>4}", .{address});
                    }
                },
                .Absolute => try std.fmt.format(tmp.writer(), "${X:0>4} = {X:0>2}", .{ memAddr, storedValue }),
                .Absolute_X => try std.fmt.format(tmp.writer(), "${X:0>4},X @ {X:0>4} = {X:0>2}", .{ address, memAddr, storedValue }),
                .Absolute_Y => try std.fmt.format(tmp.writer(), "${X:0>4},Y @ {X:0>4} = {X:0>2}", .{ address, memAddr, storedValue }),
                else => @panic("unexpected addressing mode has ops-len 3"),
            }
        },
        else => {},
    }

    // Format final trace string
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    // Format hex dump
    var hexStr = std.ArrayList(u8).init(allocator);
    defer hexStr.deinit();
    
    for (hexDump.items) |byte| {
        try std.fmt.format(hexStr.writer(), "{X:0>2} ", .{byte});
    }

    // Build assembly string
    try std.fmt.format(result.writer(), 
        "{X:0>4}  {s: <8} {s: >4} {s}",
        .{
            begin,
            hexStr.items,
            ops.mnemonic,
            tmp.items,
        }
    );

    // Add CPU state
    try std.fmt.format(result.writer(),
        "{s: <47} A:{X:0>2} X:{X:0>2} Y:{X:0>2} P:{X:0>2} SP:{X:0>2}",
        .{
            result.items,
            cpu.registerA,
            cpu.registerX,
            cpu.registerY,
            cpu.status,
            cpu.stackPointer,
        }
    );

    // Convert to uppercase
    for (result.items) |*c| {
        c.* = std.ascii.toUpper(c.*);
    }

    return result.toOwnedSlice();
}

fn createTestRom(allocator: std.mem.Allocator) !Rom {
    var prgRom = try std.ArrayList(u8).initCapacity(allocator, 0x4000);
    try prgRom.appendNTimes(1, 0x4000 - 4);
    try prgRom.append(0x00);  // Reset vector low byte
    try prgRom.append(0x80);  // Reset vector high byte
    try prgRom.append(0x00);  // Interrupt vector
    try prgRom.append(0x00);  // Interrupt vector

    var chrRom = try std.ArrayList(u8).initCapacity(allocator, 0x2000);
    try chrRom.appendNTimes(2, 0x2000);

    return Rom{
        .prgRom = prgRom,
        .chrRom = chrRom,
        .mapper = 0,
        .screenMirroring = .horizontal,
    };
}

test "format trace" {
    // Create test environment
    var rom = try createTestRom(testing.allocator);
    defer rom.deinit();
    
    var bus = Bus.init(rom);
    
    // Write test program
    bus.memWrite(100, 0xa2);  // LDX #$01
    bus.memWrite(101, 0x01);
    bus.memWrite(102, 0xca);  // DEX
    bus.memWrite(103, 0x88);  // DEY
    bus.memWrite(104, 0x00);  // BRK

    var cpu = try CPU.init(testing.allocator, bus);
    defer cpu.deinit();

    // Set initial CPU state
    cpu.programCounter = 0x64;  // 100 in decimal
    cpu.registerA = 1;
    cpu.registerX = 2;
    cpu.registerY = 3;

    var result = std.ArrayList([]const u8).init(testing.allocator);
    defer {
        for (result.items) |item| {
            testing.allocator.free(item);
        }
        result.deinit();
    }

    // Run CPU with trace callback
    while (cpu.programCounter < 104) {
        const trace_result = try trace(&cpu, testing.allocator);
        try result.append(trace_result);
        try cpu.step();
    }

    try testing.expectEqualStrings(
        "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
        result.items[0]
    );
    try testing.expectEqualStrings(
        "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
        result.items[1]
    );
    try testing.expectEqualStrings(
        "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
        result.items[2]
    );
}

test "format mem access" {
    // Create test environment
    var rom = try createTestRom(testing.allocator);
    defer rom.deinit();
    
    var bus = Bus.init(rom);

    // Write test program
    bus.memWrite(100, 0x11);  // ORA ($33),Y
    bus.memWrite(101, 0x33);

    // Write test data
    bus.memWrite(0x33, 0x00);  // Low byte of target address
    bus.memWrite(0x34, 0x04);  // High byte of target address
    bus.memWrite(0x400, 0xAA); // Target value

    var cpu = try CPU.init(testing.allocator, bus);
    defer cpu.deinit();

    // Set initial CPU state
    cpu.programCounter = 0x64;  // 100 in decimal
    cpu.registerY = 0;

    const trace_result = try trace(&cpu, testing.allocator);
    defer testing.allocator.free(trace_result);

    try testing.expectEqualStrings(
        "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
        trace_result
    );
}
