const std = @import("std");
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});
const CPU = @import("cpu.zig").CPU;

pub const SDL = struct {
    window: *c.SDL_Window,
    renderer: *c.SDL_Renderer,
    texture: *c.SDL_Texture,

    const RGB = struct {
        r: u8,
        g: u8,
        b: u8,
    };

    // fn color(idx: u8) RGB {
    //     return switch (idx) {
    //         0 => .{ .r = 0, .g = 0, .b = 0 }, // black
    //         1 => .{ .r = 255, .g = 255, .b = 255 }, // white
    //         // Add more colors as needed for your snake game
    //         else => .{ .r = 0, .g = 255, .b = 0 }, // default to green
    //     };
    // }

    pub fn color(_: *SDL, byte: u8) c.SDL_Color {
        return switch (byte) {
            0 => .{ .r = 0, .g = 0, .b = 0, .a = 255 }, // BLACK
            1 => .{ .r = 255, .g = 255, .b = 255, .a = 255 }, // WHITE
            2, 9 => .{ .r = 128, .g = 128, .b = 128, .a = 255 }, // GREY
            3, 10 => .{ .r = 255, .g = 0, .b = 0, .a = 255 }, // RED
            4, 11 => .{ .r = 0, .g = 255, .b = 0, .a = 255 }, // GREEN
            5, 12 => .{ .r = 0, .g = 0, .b = 255, .a = 255 }, // BLUE
            6, 13 => .{ .r = 255, .g = 0, .b = 255, .a = 255 }, // MAGENTA
            7, 14 => .{ .r = 255, .g = 255, .b = 0, .a = 255 }, // YELLOW
            else => .{ .r = 0, .g = 255, .b = 255, .a = 255 }, // CYAN
        };
    }

    pub fn init() !SDL {
        if (c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_EVENTS | c.SDL_INIT_AUDIO) != 0) {
            return error.SDLInitializationFailed;
        }
        errdefer c.SDL_Quit();

        const window = c.SDL_CreateWindow(
            "ZIGNES Emulator",
            c.SDL_WINDOWPOS_CENTERED,
            c.SDL_WINDOWPOS_CENTERED,
            (32 * 10),
            (32 * 10),
            c.SDL_WINDOW_SHOWN,
        ) orelse return error.SDLWindowCreationFailed;
        errdefer c.SDL_DestroyWindow(window);

        const renderer = c.SDL_CreateRenderer(
            window,
            -1,
            c.SDL_RENDERER_ACCELERATED | c.SDL_RENDERER_PRESENTVSYNC,
        ) orelse return error.SDLRendererCreationFailed;
        errdefer c.SDL_DestroyRenderer(renderer);

        // Create texture
        const texture = c.SDL_CreateTexture(
            renderer,
            c.SDL_PIXELFORMAT_RGB24,
            c.SDL_TEXTUREACCESS_TARGET,
            32,
            32,
        ) orelse return error.SDLTextureCreationFailed;
        errdefer c.SDL_DestroyTexture(texture);

        // Add scaling
        if (c.SDL_RenderSetScale(renderer, 10.0, 10.0) != 0) {
            return error.SDLScaleError;
        }

        return SDL{
            .window = window,
            .renderer = renderer,
            .texture = texture,
        };
    }

    pub fn deinit(self: *SDL) void {
        c.SDL_DestroyTexture(self.texture);
        c.SDL_DestroyRenderer(self.renderer);
        c.SDL_DestroyWindow(self.window);
        c.SDL_Quit();
    }

    pub fn handleEvents(_: *SDL, cpu: *CPU) !void {
        var event: c.SDL_Event = undefined;
        while (c.SDL_PollEvent(&event) != 0) {
            switch (event.type) {
                c.SDL_QUIT => {
                    // Handle quit
                },
                c.SDL_KEYDOWN => {
                    const scancode = event.key.keysym.scancode;
                    switch (scancode) {
                        c.SDL_SCANCODE_W, c.SDL_SCANCODE_UP => {
                            cpu.memWrite(0xff, 0x77);
                        },
                        c.SDL_SCANCODE_S, c.SDL_SCANCODE_DOWN => {
                            cpu.memWrite(0xff, 0x73);
                        },
                        c.SDL_SCANCODE_A, c.SDL_SCANCODE_LEFT => {
                            cpu.memWrite(0xff, 0x61);
                        },
                        c.SDL_SCANCODE_D, c.SDL_SCANCODE_RIGHT => {
                            cpu.memWrite(0xff, 0x64);
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }
    }

    pub fn clear(self: *SDL) !void {
        _ = c.SDL_SetRenderDrawColor(self.renderer, 0, 0, 0, 255);
        _ = c.SDL_RenderClear(self.renderer);
    }

    pub fn present(self: *SDL) void {
        c.SDL_RenderPresent(self.renderer);
    }

    pub fn updateScreen(self: *SDL, screen_state: []u8) void {
        _ = c.SDL_UpdateTexture(self.texture, null, screen_state.ptr, 32 * 3);
        _ = c.SDL_RenderCopy(self.renderer, self.texture, null, null);
        self.present();
    }
};
