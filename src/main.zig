const std = @import("std");
const CPU = @import("cpu.zig").CPU;
const SDL = @import("sdl.zig").SDL;

const snake_game_code = [309]u8{
        0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02,
        0x85, 0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9,
        0x0f, 0x85, 0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85,
        0x00, 0xa5, 0xfe, 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20,
        0x8d, 0x06, 0x20, 0xc3, 0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c,
        0x38, 0x06, 0xa5, 0xff, 0xc9, 0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0,
        0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60, 0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85,
        0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0, 0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01,
        0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02, 0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05,
        0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06, 0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00,
        0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07, 0xe6, 0x03, 0xe6, 0x03, 0x20,
        0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06, 0xb5, 0x11, 0xc5, 0x11,
        0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c, 0x35, 0x07, 0x60,
        0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02, 0x4a, 0xb0,
        0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9, 0x20,
        0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6,
        0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10,
        0xb0, 0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5,
        0x10, 0x29, 0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe,
        0x91, 0x00, 0x60, 0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10,
        0x60, 0xa6, 0xff, 0xea, 0xea, 0xca, 0xd0, 0xfb, 0x60,
};

pub fn readScreenState(cpu: *CPU, sdl: *SDL, frame: *[32 * 3 * 32]u8) bool {
    var frame_idx: usize = 0;
    var update = false;
    
    var i: u16 = 0x0200;
    while (i < 0x0600) : (i += 1) {
        const color_idx = cpu.memRead(i);
        const rgb = sdl.color(color_idx);
        
        if (frame[frame_idx] != rgb.r or 
            frame[frame_idx + 1] != rgb.g or 
            frame[frame_idx + 2] != rgb.b) 
        {
            frame[frame_idx] = rgb.r;
            frame[frame_idx + 1] = rgb.g;
            frame[frame_idx + 2] = rgb.b;
            update = true;
        }
        frame_idx += 3;
    }
    return update;
}

fn handleUserInput(cpu: *CPU, sdl: *SDL) !void {
    try sdl.handleEvents(cpu);
}

const GameContext = struct {
    cpu: *CPU,
    sdl: SDL,
    screenState: [32 * 3 * 32]u8,
    prng: std.rand.DefaultPrng,

    pub fn init(alloc: std.mem.Allocator) !@This() {
        var cpu = try CPU.init(alloc);
        errdefer cpu.deinit();
        
        var sdl = try SDL.init();
        errdefer sdl.deinit();

        try cpu.load(snake_game_code[0..], 0x0600);
        cpu.reset();

        return .{
            .cpu = cpu,
            .sdl = sdl,
            .screenState = [_]u8{0} ** (32 * 3 * 32),
            .prng = std.rand.DefaultPrng.init(@intCast(std.time.timestamp())),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.cpu.deinit();
        self.sdl.deinit();
    }

    pub fn run(self: *@This()) !void {
        const callback = struct {
            ctx: *GameContext,

            pub fn create(ctx: *GameContext) @This() {
                return .{ .ctx = ctx };
            }

            pub fn call(cb: @This()) !void {
                try handleUserInput(cb.ctx.cpu, &cb.ctx.sdl);
                cb.ctx.cpu.memWrite(0xfe, cb.ctx.prng.random().intRangeAtMost(u8, 1, 16));
                
                if (readScreenState(cb.ctx.cpu, &cb.ctx.screenState)) {
                    std.time.sleep(70_000);
                }
            }
        }.create(self);

        try self.cpu.runWithCallback(callback);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var _cpu = try CPU.init(allocator);
    defer _cpu.deinit();

    var _sdl = try SDL.init();
    defer _sdl.deinit();

    var _prng = std.rand.DefaultPrng.init(@intCast(std.time.timestamp()));

    try _cpu.load(snake_game_code[0..], 0x0600);
    _cpu.reset();

    var _screen_state: [32 * 3 * 32]u8 = undefined;

    const Context = struct {
        sdl: *SDL,
        screen_state: *[32 * 3 * 32]u8,
        prng: *std.rand.DefaultPrng,
    };

    var _ctx = Context{
        .sdl = &_sdl,
        .screen_state = &_screen_state,
        .prng = &_prng,
    };

    try _cpu.runWithCallback(&_ctx, struct {
        pub fn call(ctx: *Context, cpu: *CPU) !void {
            try handleUserInput(cpu, ctx.sdl);

            cpu.memWrite(0xfe, ctx.prng.random().intRangeAtMost(u8, 1, 16));

            if (readScreenState(cpu, ctx.sdl, ctx.screen_state)) {
                ctx.sdl.updateScreen(ctx.screen_state);
            }
            std.time.sleep(70_000);
        }
    }.call);
}
