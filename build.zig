const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "zignes",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add SDL2 package
    exe.addIncludePath(.{ .cwd_relative = "/opt/homebrew/opt/sdl2/include" });
    exe.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/opt/sdl2/lib" });

    exe.linkSystemLibrary("SDL2");
    exe.linkLibC();

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"), // Changed from .path to .root
        .target = target,
        .optimize = optimize,
    });

    // Add SDL2 configuration to tests
    unit_tests.linkSystemLibrary("sdl2");
    unit_tests.addIncludePath(.{ .cwd_relative = "/opt/homebrew/opt/sdl2/include" });
    unit_tests.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/opt/sdl2/lib" });


    const run_unit_tests = b.addRunArtifact(unit_tests);
    run_unit_tests.has_side_effects = true;

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
  
}
