const std = @import("std");
const Mem = @import("cpu.zig").Mem;

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
    cpu_vram: [2048]u8,
    program_rom: [0x8000]u8,
    reset_vector: [2]u8,

    // Constructor
    pub fn init() Bus {
        return Bus{
            .cpu_vram = [_]u8{0} ** 2048,
            .program_rom = [_]u8{0} ** 0x8000,
            .reset_vector = [_]u8{0} ** 2,
        };
    }

    // Memory interface implementation
    pub fn memRead(self: *const Bus, addr: u16) u8 {
        return switch (addr) {
            RAM...RAM_MIRRORS_END => {
                const mirror_down_addr = addr & 0b00000111_11111111;
                return self.cpu_vram[mirror_down_addr];
            },
            PPU_REGISTERS...PPU_REGISTERS_MIRRORS_END => {
                const mirror_down_addr = addr & 0b00100000_00000111;
                _ = mirror_down_addr;
                @panic("PPU is not supported yet");
            },
            0x8000...0xFFFB => {
                const mirror_down_addr = addr - 0x8000;
                return self.program_rom[mirror_down_addr];
            },
            0xFFFC...0xFFFD => {
                return self.reset_vector[addr - 0xFFFC];
            },
            0xFFFE...0xFFFF => {
                const mirror_down_addr = addr - 0x8000;
                return self.program_rom[mirror_down_addr];
            },
            else => {
                std.debug.print("Ignoring mem access at {}\n", .{addr});
                return 0;
            },
        };
    }

    pub fn memWrite(self: *Bus, addr: u16, data: u8) void {
        switch (addr) {
            RAM...RAM_MIRRORS_END => {
                const mirror_down_addr = addr & 0b11111111111;
                self.cpu_vram[mirror_down_addr] = data;
            },
            PPU_REGISTERS...PPU_REGISTERS_MIRRORS_END => {
                const mirror_down_addr = addr & 0b00100000_00000111;
                _ = mirror_down_addr;
                @panic("PPU is not supported yet");
            },
            0x8000...0xFFFB => {
                const mirror_down_addr = addr - 0x8000;
                self.program_rom[mirror_down_addr] = data;
            },
            0xFFFC...0xFFFD => {
                self.reset_vector[addr - 0xFFFC] = data;
            },
            0xFFFE...0xFFFF => {
                const mirror_down_addr = addr - 0x8000;
                self.program_rom[mirror_down_addr] = data;
            },
            else => {
                std.debug.print("Ignoring mem write-access at {}\n", .{addr});
            },
        }
    }
};
