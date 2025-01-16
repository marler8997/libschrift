const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("schrift", .{
        .root_source_file = b.path("schrift.zig"),
    });

    const c_lib = b.addStaticLibrary(.{
        .name = "schrift",
        .root_source_file = b.path("cschrift.zig"),
        .target = target,
        .optimize = optimize,
    });
    c_lib.addIncludePath(b.path("."));
    c_lib.linkLibC();
    c_lib.installHeader(b.path("schrift.h"), "schrift.h");

    {
        const exe = b.addExecutable(.{
            .name = "demo",
            .target = target,
        });
        exe.addCSourceFile(.{
            .file = b.path("demo.c"),
        });
        exe.linkLibrary(c_lib);
        exe.linkSystemLibrary("X11");
        exe.linkSystemLibrary("Xrender");
        exe.linkLibC();
        const install = b.addInstallArtifact(exe, .{});
        const run = b.addRunArtifact(exe);
        run.step.dependOn(&install.step);
        b.step("demo", "").dependOn(&run.step);
    }

    const test_step = b.step("test", "");

    {
        const exe = b.addExecutable(.{
            .name = "stress",
            .target = target,
        });
        exe.addCSourceFile(.{
            .file = b.path("stress.c"),
        });
        exe.linkLibrary(c_lib);
        exe.linkLibC();
        const install = b.addInstallArtifact(exe, .{});
        const run = b.addRunArtifact(exe);
        run.step.dependOn(&install.step);
        b.step("stress", "").dependOn(&run.step);
        test_step.dependOn(&run.step);
    }
}
