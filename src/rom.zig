const std = @import("std");
const ArrayList = std.ArrayList;

const NES_TAG = [_]u8{ 0x4E, 0x45, 0x53, 0x1A };
const PRG_ROM_PAGE_SIZE: usize = 16384;
const CHR_ROM_PAGE_SIZE: usize = 8192;

pub const Mirroring = enum {
    vertical,
    horizontal,
    fourScreen,
};

pub const RomError = error{
    InvalidFormat,
    UnsupportedFormat,
};

pub const Rom = struct {
    prgRom: ArrayList(u8),
    chrRom: ArrayList(u8),
    mapper: u8,
    screenMirroring: Mirroring,

    pub fn init(allocator: std.mem.Allocator, raw: []const u8) !Rom {
        // Verify iNES header
        if (!std.mem.eql(u8, raw[0..4], &NES_TAG)) {
            return RomError.InvalidFormat;
        }

        const mapper = (raw[7] & 0b1111_0000) | (raw[6] >> 4);
        const inesVer = (raw[7] >> 2) & 0b11;
        if (inesVer != 0) {
            return RomError.UnsupportedFormat;
        }

        // Get ROM sizes
        const prgRomSize = @as(usize, raw[4]) * PRG_ROM_PAGE_SIZE;
        const chrRomSize = @as(usize, raw[5]) * CHR_ROM_PAGE_SIZE;

        const skipTrainer = raw[6] & 0b100 != 0;
        const prgRomStart = 16 + if (skipTrainer) @as(usize, 512) else 0;
        const chrRomStart = prgRomStart + prgRomSize;

        // Create ROM data arrays
        var prgRom = try ArrayList(u8).initCapacity(allocator, prgRomSize);
        var chrRom = try ArrayList(u8).initCapacity(allocator, chrRomSize);

        // Copy ROM data
        try prgRom.appendSlice(raw[prgRomStart..][0..prgRomSize]);
        try chrRom.appendSlice(raw[chrRomStart..][0..chrRomSize]);

        // Debug information
        std.debug.print("ROM Info:\n", .{});
        std.debug.print("  Mapper: {d}\n", .{mapper});
        std.debug.print("  PRG ROM size: {d} bytes\n", .{prgRomSize});
        std.debug.print("  CHR ROM size: {d} bytes\n", .{chrRomSize});

        // Verify reset vector
        if (prgRomSize >= 0x4000) {
            const resetLo = prgRom.items[prgRom.items.len - 4];
            const resetHi = prgRom.items[prgRom.items.len - 3];
            const resetAddr = @as(u16, resetHi) << 8 | resetLo;
            std.debug.print("  Reset vector: 0x{X:0>4}\n", .{resetAddr});

            // Verify reset vector points to valid PRG ROM address
            if (resetAddr < 0x8000 or resetAddr > 0xFFFF) {
                std.debug.print("Warning: Reset vector 0x{X:0>4} points outside PRG ROM space!\n", .{resetAddr});
            }
        } else {
            std.debug.print("Warning: PRG ROM too small to contain reset vector!\n", .{});
        }

        return Rom{
            .prgRom = prgRom,
            .chrRom = chrRom,
            .mapper = mapper,
            .screenMirroring = if ((raw[6] & 0b1000) != 0)
                Mirroring.fourScreen
            else if ((raw[6] & 0b1) != 0)
                Mirroring.vertical
            else
                Mirroring.horizontal,
        };
    }

    pub fn deinit(self: *Rom) void {
        self.prgRom.deinit();
        self.chrRom.deinit();
    }
};

test "basic ROM loading" {
    const allocator = std.testing.allocator;

    // Create test ROM data
    var testData = ArrayList(u8).init(allocator);
    defer testData.deinit();

    // Header
    try testData.appendSlice(&[_]u8{ 0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 });

    // PRG ROM
    var i: usize = 0;
    while (i < 2 * PRG_ROM_PAGE_SIZE) : (i += 1) {
        try testData.append(1);
    }

    // CHR ROM
    i = 0;
    while (i < 1 * CHR_ROM_PAGE_SIZE) : (i += 1) {
        try testData.append(2);
    }

    var rom = try Rom.init(allocator, testData.items);
    defer rom.deinit();

    try std.testing.expectEqual(rom.mapper, 3);
    try std.testing.expectEqual(rom.screenMirroring, Mirroring.vertical);
    try std.testing.expectEqual(rom.prgRom.items.len, 2 * PRG_ROM_PAGE_SIZE);
    try std.testing.expectEqual(rom.chrRom.items.len, 1 * CHR_ROM_PAGE_SIZE);
}

test "NES2.0 format is not supported" {
    const allocator = std.testing.allocator;

    var testData = ArrayList(u8).init(allocator);
    defer testData.deinit();

    // Header with NES 2.0 flag set
    try testData.appendSlice(&[_]u8{ 0x4E, 0x45, 0x53, 0x1A, 0x01, 0x01, 0x31, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 });

    // PRG ROM
    var i: usize = 0;
    while (i < PRG_ROM_PAGE_SIZE) : (i += 1) {
        try testData.append(1);
    }

    // CHR ROM
    i = 0;
    while (i < CHR_ROM_PAGE_SIZE) : (i += 1) {
        try testData.append(2);
    }

    const result = Rom.init(allocator, testData.items);
    try std.testing.expectError(RomError.UnsupportedFormat, result);
}

