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
        // Check header
        if (!std.mem.eql(u8, raw[0..4], &NES_TAG)) {
            return RomError.InvalidFormat;
        }

        const mapper = (raw[7] & 0b1111_0000) | (raw[6] >> 4);

        const inesVer = (raw[7] >> 2) & 0b11;
        if (inesVer != 0) {
            return RomError.UnsupportedFormat;
        }

        const fourScreen = (raw[6] & 0b1000) != 0;
        const verticalMirroring = (raw[6] & 0b1) != 0;
        const screenMirroring = if (fourScreen)
            Mirroring.fourScreen
        else if (verticalMirroring)
            Mirroring.vertical
        else
            Mirroring.horizontal;

        const prgRomSize = @as(usize, raw[4]) * PRG_ROM_PAGE_SIZE;
        const chrRomSize = @as(usize, raw[5]) * CHR_ROM_PAGE_SIZE;

        const skipTrainer = (raw[6] & 0b100) != 0;

        const prgRomStart = 16 + if (skipTrainer) @as(usize, 512) else 0;
        const chrRomStart = prgRomStart + prgRomSize;

        // Create ArrayLists and copy ROM data
        var prgRom = try ArrayList(u8).initCapacity(allocator, prgRomSize);
        var chrRom = try ArrayList(u8).initCapacity(allocator, chrRomSize);

        try prgRom.appendSlice(raw[prgRomStart..][0..prgRomSize]);
        try chrRom.appendSlice(raw[chrRomStart..][0..chrRomSize]);

        return Rom{
            .prgRom = prgRom,
            .chrRom = chrRom,
            .mapper = mapper,
            .screenMirroring = screenMirroring,
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

