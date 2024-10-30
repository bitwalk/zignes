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

    fn color(idx: u8) RGB {
        return switch(idx) {
            0 => .{ .r = 0, .g = 0, .b = 0 },    // black
            1 => .{ .r = 255, .g = 255, .b = 255 }, // white
            // Add more colors as needed for your snake game
            else => .{ .r = 0, .g = 255, .b = 0 },  // default to green
        };
    }

    pub fn init() !SDL {
        if (c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_EVENTS | c.SDL_INIT_AUDIO) != 0) {
            return error.SDLInitializationFailed;
        }
        errdefer c.SDL_Quit();

        const window = c.SDL_CreateWindow(
            "NES Emulator",
            c.SDL_WINDOWPOS_CENTERED, c.SDL_WINDOWPOS_CENTERED,
            (32 * 10), (32 * 10),
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

    pub fn handleEvents(_: *SDL) !bool {
        var event: c.SDL_Event = undefined;
        while (c.SDL_PollEvent(&event) != 0) {
            switch (event.type) {
                c.SDL_QUIT => return true,
                c.SDL_KEYDOWN => {
                    if (event.key.keysym.scancode == c.SDL_SCANCODE_ESCAPE) {
                        return true;
                    }
                },
                else => {},
            }
        }
        return false;
    }

    pub fn clear(self: *SDL) !void {
        _ = c.SDL_SetRenderDrawColor(self.renderer, 0, 0, 0, 255);
        _ = c.SDL_RenderClear(self.renderer);
    }

    pub fn present(self: *SDL) void {
        c.SDL_RenderPresent(self.renderer);
    }

    pub fn updateScreen(self: *SDL, cpu: *CPU, screen_state: []u8) !bool {
        var frame_idx: usize = 0;
        var update = false;
        
        var i: u16 = 0x0200;
        while (i < 0x600) : (i += 1) {
            const color_idx = cpu.memRead(i);
            const rgb = color(color_idx);
            
            if (screen_state[frame_idx] != rgb.r or 
                screen_state[frame_idx + 1] != rgb.g or 
                screen_state[frame_idx + 2] != rgb.b) {
                screen_state[frame_idx] = rgb.r;
                screen_state[frame_idx + 1] = rgb.g;
                screen_state[frame_idx + 2] = rgb.b;
                update = true;
            }
            frame_idx += 3;
        }

        if (update) {
            // Update texture with new screen state
            if (c.SDL_UpdateTexture(
                self.texture,
                null,
                screen_state.ptr,
                32 * 3
            ) != 0) return error.SDLTextureUpdateFailed;

            // Copy texture to renderer
            if (c.SDL_RenderCopy(
                self.renderer,
                self.texture,
                null,
                null
            ) != 0) return error.SDLRenderCopyFailed;

            self.present();
        }

        return update;
    }
}; 