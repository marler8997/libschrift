const builtin = @import("builtin");
const std = @import("std");

const schrift = @import("schrift.zig");

const c = @cImport({
    @cInclude("stdlib.h");
    @cInclude("math.h");
    @cInclude("schrift.h");
});

pub const Font = struct {
    mem: []const u8,
    mapping: if (builtin.os.tag == .windows) ?std.os.windows.HANDLE else void,
    source: enum {
        mapped_file,
        user_supplied_memory,
    },
    info: schrift.TtfInfo,
    pub fn fromC(ptr: *c.SFT_Font) *Font {
        return @ptrCast(*Font, @alignCast(@alignOf(Font), ptr));
    }
    pub fn toC(self: *Font) *c.SFT_Font {
        return @ptrCast(*c.SFT_Font, self);
    }
};

export fn sft_version() [*:0]const u8 {
    return "0.10.2";
}

fn allocFont() error{OutOfMemory}!*Font {
    const ptr = c.calloc(1, @sizeOf(Font)) orelse return error.OutOfMemory;
    return @ptrCast(*Font, @alignCast(@alignOf(Font), ptr));
}

// Loads a font from a user-supplied memory range.
export fn sft_loadmem(mem: [*]u8, size: usize) ?*c.SFT_Font {
    if (size > std.math.maxInt(u32)) return null;

    const font = allocFont() catch return null;
    font.mem = mem[0..size];
    font.source = .user_supplied_memory;
    font.info = schrift.getTtfInfo(font.mem) catch {
        c.sft_freefont(font.toC());
        return null;
    };
    return font.toC();
}

// Loads a font from the file system. To do so, it has to map the entire font into memory.
export fn sft_loadfile(filename: [*:0]const u8) ?*c.SFT_Font {
    const font = allocFont() catch return null;
    map_file(font, filename) catch |err| {
        std.log.err("map file '{s}' failed with {s}", .{ filename, @errorName(err) });
        c.free(font);
        return null;
    };
    font.info = schrift.getTtfInfo(font.mem) catch {
        sft_freefont(font.toC());
        return null;
    };
    return font.toC();
}

export fn sft_freefont(font: ?*c.SFT_Font) void {
    const f = Font.fromC(font orelse return);
    if (f.source == .mapped_file) {
        unmapFile(f);
    }
    c.free(f);
}

export fn sft_lmetrics(sft: *const c.SFT, metrics: *c.SFT_LMetrics) c_int {
    const font = Font.fromC(sft.font orelse unreachable);
    const m = schrift.lmetrics(font.mem, font.info, sft.yScale) catch return -1;
    metrics.* = .{
        .ascender = m.ascender,
        .descender = m.descender,
        .lineGap = m.lineGap,
    };
    return 0;
}
export fn sft_lookup(sft: *const c.SFT, codepoint: c.SFT_UChar, glyph: *c.SFT_Glyph) c_int {
    const font = Font.fromC(sft.font orelse unreachable);
    if (schrift.lookupGlyph(font.mem, @intCast(u32, codepoint))) |g| {
        glyph.* = g;
        return 0;
    } else |_| {
        glyph.* = 0;
        return -1;
    }
}

export fn sft_gmetrics(sft: *c.SFT, glyph: c.SFT_Glyph, metrics: *c.SFT_GMetrics) c_int {
    @memset(@ptrCast([*]u8, metrics), 0, @sizeOf(@TypeOf(metrics.*)));

    const font = Font.fromC(sft.font orelse unreachable);
    const m = schrift.gmetrics(
        font.mem,
        font.info,
        (sft.flags & c.SFT_DOWNWARD_Y) != 0,
        .{ .x = sft.xScale, .y = sft.yScale },
        .{ .x = sft.xOffset, .y = sft.yOffset },
        @intCast(u32, glyph),
    ) catch return -1;
    metrics.* = .{
        .advanceWidth = m.advanceWidth,
        .leftSideBearing = m.leftSideBearing,
        .yOffset = @intCast(c_int, m.yOffset),
        .minWidth = @intCast(c_int, m.minWidth),
        .minHeight = @intCast(c_int, m.minHeight),
    };
    return 0;
}
const win32 = struct {
    const BOOL = i32;
    const PAGE_READONLY = 2;
    pub extern "kernel32" fn CreateFileMappingW(
        hFile: std.os.windows.HANDLE,
        lpFileMappingAttributes: ?*anyopaque,
        flProtect: u32,
        dwMaximumSizeHigh: u32,
        dwMaximumSizeLow: u32,
        lpName: ?[*:0]const u16,
    ) callconv(@import("std").os.windows.WINAPI) ?std.os.windows.HANDLE;
    pub const FILE_MAP_READ = 4;
    pub extern "kernel32" fn MapViewOfFile(
        hFileMappingObject: std.os.windows.HANDLE,
        dwDesiredAccess: u32,
        dwFileOffsetHigh: u32,
        dwFileOffsetLow: u32,
        dwNumberOfBytesToMap: ?*anyopaque,
    ) callconv(@import("std").os.windows.WINAPI) ?[*]u8;
    pub extern "kernel32" fn UnmapViewOfFile(
        lpBaseAddress: *const anyopaque,
    ) callconv(@import("std").os.windows.WINAPI) BOOL;
};

export fn sft_render(sft: *c.SFT, glyph: c.SFT_Glyph, image: c.SFT_Image) c_int {
    const font = Font.fromC(sft.font orelse unreachable);
    schrift.render(
        font.mem,
        font.info,
        (sft.flags & c.SFT_DOWNWARD_Y) != 0,
        .{ .x = sft.xScale, .y = sft.yScale },
        .{ .x = sft.xOffset, .y = sft.yOffset },
        @ptrCast([*]u8, image.pixels)[0 .. @intCast(usize, image.width) * @intCast(usize, image.height)],
        .{ .x = @intCast(i32, image.width), .y = @intCast(i32, image.height) },
        @intCast(u32, glyph),
    ) catch return -1;
    return 0;
}

fn map_file(font: *Font, filename: [*:0]const u8) !void {
    var file = try std.fs.cwd().openFileZ(filename, .{});
    defer file.close();
    const file_size = try file.getEndPos();
    if (builtin.os.tag == .windows) {
        font.mapping = win32.CreateFileMappingW(
            file.handle,
            null,
            win32.PAGE_READONLY,
            @intCast(u32, 0xffffffff & (file_size >> 32)),
            @intCast(u32, 0xffffffff & (file_size)),
            null,
        ) orelse switch (std.os.windows.kernel32.GetLastError()) {
            //.ACCESS_DENIED => return error.PermissionDenied,
            else => |err| return std.os.windows.unexpectedError(err),
        };
        errdefer {
            std.os.windows.CloseHandle(font.mapping.?);
            font.mapping = null;
        }
        const ptr = win32.MapViewOfFile(
            font.mapping.?,
            win32.FILE_MAP_READ,
            0,
            0,
            null,
        ) orelse switch (std.os.windows.kernel32.GetLastError()) {
            //.ACCESS_DENIED => return error.PermissionDenied,
            else => |err| return std.os.windows.unexpectedError(err),
        };
        font.mem = ptr[0..file_size];
    } else {
        font.mem = try std.os.mmap(null, file_size, std.os.PROT.READ, std.os.MAP.PRIVATE, file.handle, 0);
        std.debug.assert(font.mem.len == file_size);
    }
}

fn unmapFile(font: *Font) void {
    if (builtin.os.tag == .windows) {
        std.debug.assert(0 != win32.UnmapViewOfFile(font.memory));
        std.os.windows.CloseHandle(font.mapping.?);
    } else {
        //std.debug.assert(font.memory != std.os.MAP.FAILED);
        std.os.munmap(@alignCast(std.mem.page_size, font.mem));
    }
}
