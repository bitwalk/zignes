const std = @import("std");
const Mem = @import("cpu.zig").Mem;
const Rom = @import("rom.zig").Rom;
const testing = @import("std").testing;

//  _______________ $10000  _______________
// | PRG-ROM       |       |               |
// | Upper Bank    |       |               |
// |_ _ _ _ _ _ _ _| $C000 | PRG-ROM       |
// | PRG-ROM       |       |               |
// | Lower Bank    |       |               |
// |_______________| $8000 |_______________|
// | SRAM          |       | SRAM          |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// | Mirrors       |       | I/O Registers |
// | $2000-$2007   |       |               |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// | Mirrors       |       |               |
// | $0000-$07FF   |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// | RAM           |       | RAM           |
// |_ _ _ _ _ _ _ _| $0200 |               |
// | Stack         |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// | Zero Page     |       |               |
// |_______________| $0000 |_______________|
const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

pub const Bus = struct {
    cpuVram: [2048]u8,
    rom: Rom,
    test_mode: bool = false,

    pub fn init(rom: Rom) Bus {
        return Bus{
            .cpuVram = [_]u8{0} ** 2048,
            .rom = rom,
            .test_mode = false,
        };
    }

    pub fn initTest(rom: Rom) Bus {
        var bus = Bus.init(rom);
        bus.test_mode = true;
        return bus;
    }

    pub fn memRead(self: *const Bus, addr: u16) u8 {
        return switch (addr) {
            RAM...RAM_MIRRORS_END => {
                const mirrorDownAddr = addr & 0b00000111_11111111;
                return self.cpuVram[mirrorDownAddr];
            },
            PPU_REGISTERS...PPU_REGISTERS_MIRRORS_END => {
                const mirrorDownAddr = addr & 0b00100000_00000111;
                _ = mirrorDownAddr;
                @panic("PPU is not supported yet");
            },
            0x8000...0xFFFF => {
                const mirrorDownAddr = addr - 0x8000;
                // Handle ROM mirroring if PRG-ROM is 16KB
                const prgRomAddr = if (self.rom.prgRom.items.len == 0x4000)
                    mirrorDownAddr & 0x3FFF // Mirror every 16KB
                else
                    mirrorDownAddr;
                return self.rom.prgRom.items[prgRomAddr];
            },
            else => {
                std.debug.print("Ignoring mem access at {X}\n", .{addr});
                return 0;
            },
        };
    }

    pub fn memWrite(self: *Bus, addr: u16, data: u8) void {
        switch (addr) {
            RAM...RAM_MIRRORS_END => {
                const mirrorDownAddr = addr & 0b11111111111;
                self.cpuVram[mirrorDownAddr] = data;
            },
            PPU_REGISTERS...PPU_REGISTERS_MIRRORS_END => {
                const mirrorDownAddr = addr & 0b00100000_00000111;
                _ = mirrorDownAddr;
                @panic("PPU is not supported yet");
            },
            0x8000...0xFFFF => {
                if (self.test_mode) {
                    // Allow writes to ROM area during testing
                    const mirrorDownAddr = addr - 0x8000;
                    if (mirrorDownAddr < self.rom.prgRom.items.len) {
                        self.rom.prgRom.items[mirrorDownAddr] = data;
                    }
                } else if (self.rom.mapper == 0) {
                    std.debug.print("Attempting to write to ROM at {X}\n", .{addr});
                    return;
                } else {
                    @panic("Mapper not implemented yet");
                }
            },
            else => {
                std.debug.print("Ignoring mem write-access at {X}\n", .{addr});
            },
        }
    }
};

test "Bus - basic memory operations" {
    // Create a test ROM with known data
    var testRom = try createTestRom(testing.allocator);
    defer testRom.deinit();

    var bus = Bus.init(testRom);

    // Test RAM read/write
    try testing.expectEqual(@as(u8, 0), bus.memRead(0x0000));
    bus.memWrite(0x0000, 0x42);
    try testing.expectEqual(@as(u8, 0x42), bus.memRead(0x0000));

    // Test RAM mirroring
    try testing.expectEqual(@as(u8, 0x42), bus.memRead(0x0800));
    try testing.expectEqual(@as(u8, 0x42), bus.memRead(0x1000));
    try testing.expectEqual(@as(u8, 0x42), bus.memRead(0x1800));
}

test "Bus - ROM reading" {
    // Create a test ROM with known data
    var testRom = try createTestRom(testing.allocator);
    defer testRom.deinit();

    var bus = Bus.init(testRom);

    // Test reading from ROM area (0x8000-0xFFFF)
    try testing.expectEqual(@as(u8, 1), bus.memRead(0x8000));  // First byte of PRG ROM
    try testing.expectEqual(@as(u8, 0x00), bus.memRead(0xFFFC));  // Reset vector low byte
    try testing.expectEqual(@as(u8, 0x80), bus.memRead(0xFFFD));  // Reset vector high byte
}

test "Bus - ROM write protection" {
    // Create a test ROM with known data
    var testRom = try createTestRom(testing.allocator);
    defer testRom.deinit();

    var bus = Bus.init(testRom);

    // Try to write to ROM area (should be ignored for mapper 0)
    bus.memWrite(0x8000, 0x42);
    try testing.expectEqual(@as(u8, 1), bus.memRead(0x8000));  // Should still read original value
}

test "Bus - debug output test" {
    
    var testRom = try createTestRom(testing.allocator);
    defer testRom.deinit();

    const resetLo = testRom.prgRom.items[testRom.prgRom.items.len - 4];
    const resetHi = testRom.prgRom.items[testRom.prgRom.items.len - 3];
    const resetAddr = @as(u16, resetHi) << 8 | resetLo;

    var bus = Bus.init(testRom);
    
    // Read through bus like CPU would
    const busLo = bus.memRead(0xFFFC);
    const busHi = bus.memRead(0xFFFD);
    const busResetAddr = @as(u16, busHi) << 8 | busLo;
    
    try testing.expectEqual(resetAddr, busResetAddr);
}

// Helper function to create a test ROM
fn createTestRom(allocator: std.mem.Allocator) !Rom {
    // Create a minimal ROM with:
    // - 16KB PRG ROM filled with 1's
    // - Reset vector pointing to 0x8000 (not 0x8001)
    var prgRom = try std.ArrayList(u8).initCapacity(allocator, 0x4000);
    try prgRom.appendNTimes(1, 0x4000 - 4);  // Fill with 1's
    try prgRom.append(0x00);  // Reset vector low byte
    try prgRom.append(0x80);  // Reset vector high byte
    try prgRom.append(0x00);  // Interrupt vector
    try prgRom.append(0x00);  // Interrupt vector

    var chrRom = try std.ArrayList(u8).initCapacity(allocator, 0x2000);
    try chrRom.appendNTimes(2, 0x2000);  // Fill CHR ROM with 2's

    return Rom{
        .prgRom = prgRom,
        .chrRom = chrRom,
        .mapper = 0,  // Make sure mapper is 0
        .screenMirroring = .horizontal,
    };
}