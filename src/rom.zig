const std = @import("std");
const testing = std.testing;
const ArrayList = std.ArrayList;

pub const Mirroring = enum {
    horizontal,
    vertical,
    four_screen,
};

pub const RomError = error{
    InvalidFormat,
    UnsupportedFormat,
};

pub const Rom = struct {
    pub const NES_TAG = [_]u8{ 0x4E, 0x45, 0x53, 0x1A };
    pub const PRG_ROM_PAGE_SIZE: usize = 16384;
    pub const CHR_ROM_PAGE_SIZE: usize = 8192;
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
        const prgRomSize = @as(usize, raw[4]) * Rom.PRG_ROM_PAGE_SIZE;
        const chrRomSize = @as(usize, raw[5]) * Rom.CHR_ROM_PAGE_SIZE;

        const skipTrainer = raw[6] & 0b100 != 0;
        const prgRomStart = 16 + if (skipTrainer) @as(usize, 512) else 0;
        const chrRomStart = prgRomStart + prgRomSize;

        // Create ROM data arrays
        var prgRom = try ArrayList(u8).initCapacity(allocator, prgRomSize);
        var chrRom = try ArrayList(u8).initCapacity(allocator, chrRomSize);

        // Copy ROM data
        try prgRom.appendSlice(raw[prgRomStart..][0..prgRomSize]);
        try chrRom.appendSlice(raw[chrRomStart..][0..chrRomSize]);

        if (prgRomSize >= 0x4000) {
            // Get reset vector from last 4 bytes (NES CPU reads from 0xFFFC-0xFFFD)
            const resetLo = prgRom.items[prgRom.items.len - 4];
            const resetHi = prgRom.items[prgRom.items.len - 3];
            const resetAddr = @as(u16, resetHi) << 8 | resetLo;
            
            // Since ROM data is mapped to 0x8000-0xFFFF in CPU space,
            // we don't need to add 0x8000 to the reset vector as it's already
            // relative to 0x8000
            if (resetAddr < 0) {
                std.debug.print(
                    "Warning: Reset vector 0x{X:0>4} points below PRG ROM space (valid range: 0x0000-0x7FFF for ROM data)\n",
                    .{resetAddr}
                );
            } else if (resetAddr >= prgRomSize) {
                std.debug.print(
                    "Warning: Reset vector 0x{X:0>4} points beyond PRG ROM size of 0x{X:0>4}\n",
                    .{ resetAddr, prgRomSize }
                );
            }
        }

        return Rom{
            .prgRom = prgRom,
            .chrRom = chrRom,
            .mapper = mapper,
            .screenMirroring = if ((raw[6] & 0b1000) != 0)
                Mirroring.four_screen
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

// Helper function to create test ROM data
fn createTestRomData(allocator: std.mem.Allocator, header: []const u8, trainer: ?[]const u8, prg_rom: []const u8, chr_rom: []const u8) ![]u8 {
    const trainer_size = if (trainer) |t| t.len else 0;
    const total_size = header.len + trainer_size + prg_rom.len + chr_rom.len;
    
    var result = try allocator.alloc(u8, total_size);
    
    @memcpy(result[0..header.len], header);
    var offset = header.len;
    
    if (trainer) |t| {
        @memcpy(result[offset..][0..t.len], t);
        offset += t.len;
    }
    
    @memcpy(result[offset..][0..prg_rom.len], prg_rom);
    offset += prg_rom.len;
    
    @memcpy(result[offset..][0..chr_rom.len], chr_rom);
    
    return result;
}

pub fn createTestRom(allocator: std.mem.Allocator) !Rom {
    // Create test data matching Rust implementation
    const header = [_]u8{
        0x4E, 0x45, 0x53, 0x1A, // NES<EOF>
        0x02, // 2 PRG ROM pages
        0x01, // 1 CHR ROM page
        0x31, 0x00, // flags
        0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,
    };

    // Fill PRG ROM with 1s
    const prgRom = try allocator.alloc(u8, 2 * Rom.PRG_ROM_PAGE_SIZE);
    @memset(prgRom, 1);

    // Fill CHR ROM with 2s
    const chrRom = try allocator.alloc(u8, Rom.CHR_ROM_PAGE_SIZE);
    @memset(chrRom, 2);

    const rawData = try createTestRomData(allocator, &header, null, prgRom, chrRom);
    
    allocator.free(prgRom);
    allocator.free(chrRom);
    
    return Rom.init(allocator, rawData);
}

test "basic ROM loading" {
    const allocator = std.testing.allocator;

    // Create test ROM data
    var testData = ArrayList(u8).init(allocator);
    defer testData.deinit();

    // Header
    try testData.appendSlice(&[_]u8{ 0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 });

    // PRG ROM
    var i: usize = 0;
    while (i < 2 * Rom.PRG_ROM_PAGE_SIZE) : (i += 1) {
        try testData.append(1);
    }

    // CHR ROM
    i = 0;
    while (i < 1 * Rom.CHR_ROM_PAGE_SIZE) : (i += 1) {
        try testData.append(2);
    }

    var rom = try Rom.init(allocator, testData.items);
    defer rom.deinit();

    try std.testing.expectEqual(rom.mapper, 3);
    try std.testing.expectEqual(rom.screenMirroring, Mirroring.vertical);
    try std.testing.expectEqual(rom.prgRom.items.len, 2 * Rom.PRG_ROM_PAGE_SIZE);
    try std.testing.expectEqual(rom.chrRom.items.len, 1 * Rom.CHR_ROM_PAGE_SIZE);
}

test "NES2.0 format is not supported" {
    const allocator = std.testing.allocator;

    var testData = ArrayList(u8).init(allocator);
    defer testData.deinit();

    // Header with NES 2.0 flag set
    try testData.appendSlice(&[_]u8{ 0x4E, 0x45, 0x53, 0x1A, 0x01, 0x01, 0x31, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 });

    // PRG ROM
    var i: usize = 0;
    while (i < Rom.PRG_ROM_PAGE_SIZE) : (i += 1) {
        try testData.append(1);
    }

    // CHR ROM
    i = 0;
    while (i < Rom.CHR_ROM_PAGE_SIZE) : (i += 1) {
        try testData.append(2);
    }

    const result = Rom.init(allocator, testData.items);
    try std.testing.expectError(RomError.UnsupportedFormat, result);
}

test "test ROM creation matches Rust version" {
    // Create test data
    const prg_rom = try testing.allocator.alloc(u8, 2 * Rom.PRG_ROM_PAGE_SIZE);
    defer testing.allocator.free(prg_rom);
    @memset(prg_rom, 1);
    
    const chr_rom = try testing.allocator.alloc(u8, Rom.CHR_ROM_PAGE_SIZE);
    defer testing.allocator.free(chr_rom);
    @memset(chr_rom, 2);
    
    const header = [_]u8{
        0x4E, 0x45, 0x53, 0x1A, // NES<EOF>
        0x02, 0x01, 0x31, 0x00, // 2 PRG, 1 CHR, mapper 3, vertical mirroring
        0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,
    };
    
    const raw_data = try createTestRomData(testing.allocator, &header, null, prg_rom, chr_rom);
    defer testing.allocator.free(raw_data);
    
    var rom = try Rom.init(testing.allocator, raw_data);
    defer rom.deinit();
    
    try testing.expectEqualSlices(u8, chr_rom, rom.chrRom.items);
    try testing.expectEqualSlices(u8, prg_rom, rom.prgRom.items);
    try testing.expectEqual(@as(u8, 3), rom.mapper);
    try testing.expectEqual(Mirroring.vertical, rom.screenMirroring);
}

test "test ROM with trainer" {
    const prg_rom = try testing.allocator.alloc(u8, 2 * Rom.PRG_ROM_PAGE_SIZE);
    defer testing.allocator.free(prg_rom);
    @memset(prg_rom, 1);
    
    const chr_rom = try testing.allocator.alloc(u8, Rom.CHR_ROM_PAGE_SIZE);
    defer testing.allocator.free(chr_rom);
    @memset(chr_rom, 2);
    
    const trainer = try testing.allocator.alloc(u8, 512);
    defer testing.allocator.free(trainer);
    @memset(trainer, 0);
    
    const header = [_]u8{
        0x4E, 0x45, 0x53, 0x1A,
        0x02, 0x01, 0x31 | 0b100, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
    };
    
    const raw_data = try createTestRomData(testing.allocator, &header, trainer, prg_rom, chr_rom);
    defer testing.allocator.free(raw_data);
    
    var rom = try Rom.init(testing.allocator, raw_data);
    defer rom.deinit();
    
    try testing.expectEqualSlices(u8, chr_rom, rom.chrRom.items);
    try testing.expectEqualSlices(u8, prg_rom, rom.prgRom.items);
    try testing.expectEqual(@as(u8, 3), rom.mapper);
    try testing.expectEqual(Mirroring.vertical, rom.screenMirroring);
}

test "NES2.0 is not supported" {
    const prg_rom = try testing.allocator.alloc(u8, Rom.PRG_ROM_PAGE_SIZE);
    defer testing.allocator.free(prg_rom);
    @memset(prg_rom, 1);
    
    const chr_rom = try testing.allocator.alloc(u8, Rom.CHR_ROM_PAGE_SIZE);
    defer testing.allocator.free(chr_rom);
    @memset(chr_rom, 2);
    
    const header = [_]u8{
        0x4E, 0x45, 0x53, 0x1A,
        0x01, 0x01, 0x31, 0x08,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
    };
    
    const raw_data = try createTestRomData(testing.allocator, &header, null, prg_rom, chr_rom);
    defer testing.allocator.free(raw_data);
    
    const result = Rom.init(testing.allocator, raw_data);
    try testing.expectError(error.UnsupportedFormat, result);
}

