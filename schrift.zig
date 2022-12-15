const builtin = @import("builtin");
const std = @import("std");
const c = @cImport({
    @cInclude("stdlib.h");
    @cInclude("math.h");
    @cInclude("schrift.h");
});

const Font = struct {
    mem: []const u8,
    mapping: if (builtin.os.tag == .windows) ?std.os.windows.HANDLE else void,
    source: enum {
        mapped_file,
        user_supplied_memory,
    },
    info: TtfInfo,
    pub fn fromC(ptr: *c.SFT_Font) *Font {
        return @ptrCast(*Font, @alignCast(@alignOf(Font), ptr));
    }
    pub fn toC(self: *Font) *c.SFT_Font {
        return @ptrCast(*c.SFT_Font, self);
    }
};

const TtfInfo = struct {
    unitsPerEm: c.uint_least16_t,
    locaFormat: c.int_least16_t,
    numLongHmtx: c.uint_least16_t,
};

const Point = struct { x: f64, y: f64 };
const Line = struct {
    beg: c.uint_least16_t,
    end: c.uint_least16_t,
};
const Curve = struct {
    beg: c.uint_least16_t,
    end: c.uint_least16_t,
    ctrl: c.uint_least16_t,
};

const Outline = struct {
    points: std.ArrayListUnmanaged(Point) = .{},
    curves: std.ArrayListUnmanaged(Curve) = .{},
    lines: std.ArrayListUnmanaged(Line) = .{},
};

const Cell = struct {
    area: f64,
    cover: f64,
};
const Raster = struct {
    cells: [*]Cell,
    width: c_int,
    height: c_int,
};

const ttf = struct {
    pub const file_magic_one = 0x00010000;
    pub const file_magic_two = 0x74727565;

    pub const point_is_on_curve = 0x01;
    pub const x_change_is_small = 0x02;
    pub const y_change_is_small = 0x04;
    pub const repeat_flag = 0x08;
    pub const x_change_is_zero = 0x10;
    pub const x_change_is_positive = 0x10;
    pub const y_change_is_zero = 0x20;
    pub const y_change_is_positive = 0x20;

    pub const offsets_are_large = 0x001;
    pub const actual_xy_offsets = 0x002;
    pub const got_a_single_scale = 0x008;
    pub const there_are_more_components = 0x020;
    pub const got_an_x_and_y_scale = 0x040;
    pub const got_a_scale_matrix = 0x080;
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
    font.info = getTtfInfo(font.mem) catch {
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
    font.info = getTtfInfo(font.mem) catch {
        sft_freefont(font.toC());
        return null;
    };
    return font.toC();
}

export fn sft_freefont(font: ?*c.SFT_Font) void {
    const f = Font.fromC(font orelse return);
    if (f.source == .mapped_file) {
        unmap_file(f);
    }
    c.free(f);
}

export fn sft_lmetrics(sft: *const c.SFT, metrics: *c.SFT_LMetrics) c_int {
    const font = Font.fromC(sft.font orelse unreachable);
    const m = lmetrics(font.mem, font.info, sft.yScale) catch return -1;
    metrics.* = .{
        .ascender = m.ascender,
        .descender = m.descender,
        .lineGap = m.lineGap,
    };
    return 0;
}
pub const LMetrics = struct {
    ascender: f64,
    descender: f64,
    lineGap: f64,
};
pub fn lmetrics(ttf_mem: []const u8, info: TtfInfo, yScale: f64) !LMetrics {
    const hhea = (try gettable2(ttf_mem, "hhea")) orelse
        return error.InvalidTtfNoHheaTable;
    const hhea_limit = hhea + 10;
    if (hhea_limit > ttf_mem.len)
        return error.InvalidTtfBadHheaTable;
    const factor = yScale / @intToFloat(f64, info.unitsPerEm);
    return LMetrics{
        .ascender = factor * @intToFloat(f64, std.mem.readIntBig(i16, ttf_mem[hhea + 4 ..][0..2])),
        .descender = factor * @intToFloat(f64, std.mem.readIntBig(i16, ttf_mem[hhea + 6 ..][0..2])),
        .lineGap = factor * @intToFloat(f64, std.mem.readIntBig(i16, ttf_mem[hhea + 8 ..][0..2])),
    };
}

export fn sft_lookup(sft: *const c.SFT, codepoint: c.SFT_UChar, glyph: *c.SFT_Glyph) c_int {
    const font = Font.fromC(sft.font orelse unreachable);
    if (glyph_id(font, codepoint)) |g| {
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
    const m = gmetrics(
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

pub fn XY(comptime T: type) type {
    return struct {
        x: T,
        y: T,
    };
}
pub const GMetrics = struct {
    advanceWidth: f64,
    leftSideBearing: f64,
    yOffset: i32,
    minWidth: i32,
    minHeight: i32,
};
pub fn gmetrics(
    ttf_mem: []const u8,
    info: TtfInfo,
    downward: bool,
    scale: XY(f64),
    offset: XY(f64),
    glyph: u32,
) !GMetrics {
    const hor = try hor_metrics(ttf_mem, info, glyph);
    const xScaleEm = scale.x / @intToFloat(f64, info.unitsPerEm);

    const advanceWidth = @intToFloat(f64, hor.advance_width) * xScaleEm;
    const leftSideBearing = @intToFloat(f64, hor.left_side_bearing) * xScaleEm + offset.x;

    const outline = (try outline_offset(ttf_mem, info, glyph)) orelse return GMetrics{
        .advanceWidth = advanceWidth,
        .leftSideBearing = leftSideBearing,
        .minWidth = 0,
        .minHeight = 0,
        .yOffset = 0,
    };
    const bbox = try glyph_bbox(ttf_mem, info, scale, offset, outline);
    //std.debug.assert(bbox.x_min < bbox.x_max);
    //std.debug.assert(bbox.y_min < bbox.y_max);
    const y_min = @floatToInt(i32, bbox.y_min);
    const y_max = @floatToInt(i32, bbox.y_max);
    return GMetrics{
        .advanceWidth = advanceWidth,
        .leftSideBearing = leftSideBearing,
        .minWidth = @floatToInt(i32, bbox.x_max) - @floatToInt(i32, bbox.x_min) + 1,
        .minHeight = y_max - y_min + 1,
        .yOffset = if (downward) -y_max else y_min,
    };
}

export fn sft_render(sft: *c.SFT, glyph: c.SFT_Glyph, image: c.SFT_Image) c_int {
    const font = Font.fromC(sft.font orelse unreachable);
    const outline = (outline_offset(font.mem, font.info, glyph) catch return -1) orelse return 0;
    const bbox = glyph_bbox(
        font.mem,
        font.info,
        .{ .x = sft.xScale, .y = sft.yScale },
        .{ .x = sft.xOffset, .y = sft.yOffset },
        outline,
    ) catch return -1;
    // Set up the transformation matrix such that
    // the transformed bounding boxes min corner lines
    // up with the (0, 0) point.
    var transform: [6]f64 = undefined;
    transform[0] = sft.xScale / @intToFloat(f64, font.info.unitsPerEm);
    transform[1] = 0.0;
    transform[2] = 0.0;
    transform[4] = sft.xOffset - bbox.x_min;
    if ((sft.flags & c.SFT_DOWNWARD_Y) != 0) {
        transform[3] = -sft.yScale / @intToFloat(f64, font.info.unitsPerEm);
        transform[5] = bbox.y_max - sft.yOffset;
    } else {
        transform[3] = sft.yScale / @intToFloat(f64, font.info.unitsPerEm);
        transform[5] = sft.yOffset - bbox.y_min;
    }

    var outl = Outline{};
    defer free_outline(&outl);
    init_outline(&outl) catch return -1;

    decode_outline(font, outline, 0, &outl) catch return -1;
    render_outline(&outl, &transform, image) catch return -1;
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

fn unmap_file(font: *Font) void {
    if (builtin.os.tag == .windows) {
        std.debug.assert(0 != win32.UnmapViewOfFile(font.memory));
        std.os.windows.CloseHandle(font.mapping.?);
    } else {
        //std.debug.assert(font.memory != std.os.MAP.FAILED);
        std.os.munmap(@alignCast(std.mem.page_size, font.mem));
    }
}

fn readTtf(comptime T: type, ttf_mem: []const u8) T {
    std.debug.assert(ttf_mem.len >= @sizeOf(T));
    return std.mem.readIntBig(T, ttf_mem[0..@sizeOf(T)]);
}

pub fn getTtfInfo(ttf_mem: []const u8) !TtfInfo {
    if (ttf_mem.len < 12) return error.InvalidTtfTooSmall;

    // Check for a compatible scalerType (magic number).
    const scalerType = readTtf(u32, ttf_mem);
    if (scalerType != ttf.file_magic_one and scalerType != ttf.file_magic_two)
        return error.InvalidTtfBadMagic;
    const head = (try gettable2(ttf_mem, "head")) orelse
        return error.InvalidTtfNoHeadTable;
    const head_limit = head + 52;
    if (head_limit > ttf_mem.len)
        return error.InvalidTtfBadHeadTable;
    const hhea = (try gettable2(ttf_mem, "hhea")) orelse
        return error.InvalidTtfNoHheaTable;
    const hhea_limit = hhea + 36;
    if (hhea_limit > ttf_mem.len)
        return error.InvalidTtfBadHheaTable;
    return TtfInfo{
        .unitsPerEm = readTtf(u16, ttf_mem[head + 18 ..]),
        .locaFormat = readTtf(i16, ttf_mem[head + 15 ..]),
        .numLongHmtx = readTtf(u16, ttf_mem[hhea + 34 ..]),
    };
}

fn midpoint(a: Point, b: Point) Point {
    return .{
        .x = 0.5 * (a.x + b.x),
        .y = 0.5 * (a.y + b.y),
    };
}

// Applies an affine linear transformation matrix to a set of points.
fn transform_points(points: []Point, trf: *const [6]f64) void {
    for (points) |*pt_ref| {
        const pt = pt_ref.*;
        pt_ref.* = .{
            .x = pt.x * trf[0] + pt.y * trf[2] + trf[4],
            .y = pt.x * trf[1] + pt.y * trf[3] + trf[5],
        };
    }
}

fn clip_points(points: []Point, width: f64, height: f64) void {
    for (points) |*pt| {
        if (pt.x < 0.0) {
            pt.x = 0.0;
        } else if (pt.x >= width) {
            pt.x = c.nextafter(width, 0.0);
        }
        if (pt.y < 0.0) {
            pt.y = 0.0;
        } else if (pt.y >= height) {
            pt.y = c.nextafter(height, 0.0);
        }
    }
}

fn malloc(comptime T: type, count: usize) error{OutOfMemory}![*]T {
    const ptr = c.malloc(count * @sizeOf(T)) orelse return error.OutOfMemory;
    return @ptrCast([*]T, @alignCast(@alignOf(T), ptr));
}

fn init_outline(outl: *Outline) error{OutOfMemory}!void {
    // TODO Smaller initial allocations
    outl.points.items.len = 0;
    outl.points.capacity = 64;
    outl.points.items.ptr = try malloc(Point, outl.points.capacity);
    outl.curves.items.len = 0;
    outl.curves.capacity = 64;
    outl.curves.items.ptr = try malloc(Curve, outl.curves.capacity);
    outl.lines.items.len = 0;
    outl.lines.capacity = 64;
    outl.lines.items.ptr = try malloc(Line, outl.lines.capacity);
}

fn free_outline(outl: *Outline) void {
    if (outl.points.capacity != 0) c.free(outl.points.items.ptr);
    if (outl.curves.capacity != 0) c.free(outl.curves.items.ptr);
    if (outl.lines.capacity != 0) c.free(outl.lines.items.ptr);
}

fn grow(comptime T: type, cap_ref: *usize, ptr_ref: *[*]T) error{ OutOfMemory, TooManyPrimitives }!void {
    std.debug.assert(cap_ref.* > 0);
    const next_cap = cap_ref.* * 2;
    std.debug.assert(next_cap > cap_ref.*);
    const ptr = c.realloc(ptr_ref.*, next_cap * @sizeOf(T)) orelse return error.OutOfMemory;
    cap_ref.* = next_cap;
    ptr_ref.* = @ptrCast([*]T, @alignCast(@alignOf(T), ptr));
}

fn grow_points(outline: *Outline) error{ OutOfMemory, TooManyPoints }!void {
    grow(Point, &outline.points.capacity, &outline.points.items.ptr) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TooManyPrimitives => return error.TooManyPoints,
    };
}

fn grow_curves(outline: *Outline) error{ OutOfMemory, TooManyCurves }!void {
    grow(Curve, &outline.curves.capacity, &outline.curves.items.ptr) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TooManyPrimitives => return error.TooManyCurves,
    };
}

fn grow_lines(outline: *Outline) error{ OutOfMemory, TooManyLines }!void {
    grow(Line, &outline.lines.capacity, &outline.lines.items.ptr) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TooManyPrimitives => return error.TooManyLines,
    };
}

fn is_safe_offset(font: *Font, offset: usize, margin: u32) bool {
    if (offset > font.mem.len) return false;
    if (font.mem.len - offset < margin) return false;
    return true;
}

fn bsearch(
    key: *const anyopaque,
    base: *const anyopaque,
    nmemb: usize,
    size: usize,
    compar: *const fn ([*]const u8, [*]const u8) i2,
) ?*const anyopaque {
    var next_base = base;
    var nel = nmemb;
    while (nel > 0) {
        const t = @ptrCast([*]const u8, next_base) + size * (nel / 2);
        const s = compar(@ptrCast([*]const u8, key), t);
        if (s < 0) {
            nel /= 2;
        } else if (s > 0) {
            next_base = t + size;
            nel -= nel / 2 + 1;
        } else {
            return t;
        }
    }
    return null;
}

// Like bsearch(), but returns the next highest element if key could not be found.
fn csearch(
    key: *const anyopaque,
    base: *const anyopaque,
    nmemb: usize,
    size: usize,
    compar: *const fn ([*]const u8, [*]const u8) i2,
) ?*const anyopaque {
    if (nmemb == 0) return null;

    const bytes = @ptrCast([*]const u8, base);
    var low: usize = 0;
    var high: usize = nmemb - 1;
    while (low != high) {
        const mid = low + (high - low) / 2;
        const sample = bytes + mid * size;
        if (compar(@ptrCast([*]const u8, key), sample) > 0) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    return bytes + low * size;
}

fn memcmp(l: [*]const u8, r: [*]const u8, len: usize) i2 {
    var i: usize = 0;
    while (true) : (i += 1) {
        if (i == len) return 0;
        if (l[i] > r[i]) return 1;
        if (l[i] < r[i]) return -1;
    }
}

// Used as a comparison function for [bc]search().
fn cmpu16(a: [*]const u8, b: [*]const u8) i2 {
    return memcmp(a, b, 2);
}

// Used as a comparison function for [bc]search().
fn cmpu32(a: [*]const u8, b: [*]const u8) i2 {
    return memcmp(a, b, 4);
}

fn geti8(font: *Font, offset: usize) i8 {
    return @bitCast(i8, font.mem[offset]);
}
fn geti16(font: *Font, offset: usize) i16 {
    std.debug.assert(offset + 2 <= font.mem.len);
    return std.mem.readIntBig(i16, @ptrCast(*const [2]u8, font.mem.ptr + offset));
}
fn getu16(font: *Font, offset: usize) u16 {
    std.debug.assert(offset + 2 <= font.mem.len);
    return std.mem.readIntBig(u16, @ptrCast(*const [2]u8, font.mem.ptr + offset));
}
fn getu32(font: *Font, offset: usize) u32 {
    std.debug.assert(offset + 4 <= font.mem.len);
    return std.mem.readIntBig(u32, @ptrCast(*const [4]u8, font.mem.ptr + offset));
}

fn gettable(font: *Font, tag: *const [4]u8) !?c.uint_fast32_t {
    return gettable2(font.mem, tag);
}
fn gettable2(ttf_mem: []const u8, tag: *const [4]u8) !?c.uint_fast32_t {
    // No need to bounds-check access to the first 12 bytes - this gets already checked by init_font().
    const numTables = readTtf(u16, ttf_mem[4..]);
    const limit = 12 + @intCast(usize, numTables) * 16;
    if (limit > ttf_mem.len)
        return error.InvalidTtfBadTables;
    const match_ptr = bsearch(tag, ttf_mem.ptr + 12, numTables, 16, cmpu32) orelse return null;
    const match_offset = @ptrToInt(match_ptr) - @ptrToInt(ttf_mem.ptr);
    return readTtf(u32, ttf_mem[match_offset + 8 ..]);
}

fn cmap_fmt4(ttf_mem: []const u8, table: usize, charCode: u32) !u32 {
    // cmap format 4 only supports the Unicode BMP.
    if (charCode > 0xFFFF)
        return 0;

    const shortCode = @intCast(c.uint_fast16_t, charCode);
    if (table + 8 > ttf_mem.len)
        return error.InvalidTtfBadCmapTable;
    const segCountX2 = readTtf(u16, ttf_mem[table..]);
    if (((segCountX2 & 1) != 0) or (0 == segCountX2))
        return error.InvalidTtfBadCmapTable;

    // Find starting positions of the relevant arrays.
    const endCodes = table + 8;
    const startCodes = endCodes + segCountX2 + 2;
    const idDeltas = startCodes + segCountX2;
    const idRangeOffsets = idDeltas + segCountX2;
    if (idRangeOffsets + segCountX2 > ttf_mem.len)
        return error.InvalidTtfBadCmapTable;

    // Find the segment that contains shortCode by binary searching over
    // the highest codes in the segments.
    const key = [2]u8{
        @intCast(u8, (charCode >> 8) & 0xff),
        @intCast(u8, (charCode >> 0) & 0xff),
    };
    const segAddr = @ptrToInt(csearch(&key, ttf_mem.ptr + endCodes, segCountX2 / 2, 2, cmpu16));
    const segIdxX2 = segAddr - (@ptrToInt(ttf_mem.ptr) + endCodes);
    // Look up segment info from the arrays & short circuit if the spec requires.
    const startCode = readTtf(u16, ttf_mem[startCodes + segIdxX2 ..]);
    if (startCode > charCode)
        return 0;
    const idDelta: u32 = readTtf(u16, ttf_mem[idDeltas + segIdxX2 ..]);
    const idRangeOffset = readTtf(u16, ttf_mem[idRangeOffsets + segIdxX2 ..]);
    if (idRangeOffset == 0) {
        // Intentional integer under- and overflow.
        // TODO: not sure if this is correct?
        return (charCode + idDelta) & 0xFFFF;
    }
    // Calculate offset into glyph array and determine ultimate value.
    const idOffset = idRangeOffsets + segIdxX2 + idRangeOffset + 2 * (shortCode - startCode);
    if (idOffset + 2 > ttf_mem.len)
        return error.InvalidTtfBadCmapTable;
    const id: u32 = readTtf(u16, ttf_mem[idOffset..]);
    // Intentional integer under- and overflow.
    return if (id == 0) 0 else ((id + idDelta) & 0xFFFF);
}

fn cmap_fmt12_13(table: []const u8, charCode: u32, which: c_int) !u32 {
    if (table.len < 16)
        return error.InvalidTtfBadCmapTable;
    const numEntries = @intCast(usize, readTtf(u32, table[12..]));
    if (16 + 12 * numEntries > table.len)
        return error.InvalidTtfBadCmapTable;
    var i: usize = 0;
    while (i < numEntries) : (i += 1) {
        const entry = 16 + i * 12;
        const firstCode = readTtf(u32, table[entry + 0 ..]);
        const lastCode = readTtf(u32, table[entry + 4 ..]);
        if (charCode < firstCode or charCode > lastCode)
            continue;
        const glyphOffset = readTtf(u32, table[entry + 8 ..]);
        return if (which == 12) (charCode - firstCode) + glyphOffset else glyphOffset;
    }
    return 0;
}

// Maps Unicode code points to glyph indices.
fn glyph_id(font: *Font, charCode: c.SFT_UChar) !c.SFT_Glyph {
    return try lookup_glyph(font.mem, @intCast(u21, charCode));
}
pub fn lookup_glyph(ttf_mem: []const u8, charCode: u32) !u32 {
    const cmap = (try gettable2(ttf_mem, "cmap")) orelse
        return error.InvalidTtfNoCmapTable;
    const cmap_limit = cmap + 4;
    if (cmap_limit > ttf_mem.len)
        return error.InvalidTtfBadCmapTable;
    const numEntries: usize = readTtf(u16, ttf_mem[cmap + 2 ..]);

    const entries_limit = 4 + numEntries * 8;
    if (entries_limit > ttf_mem.len)
        return error.InvalidTtfBadCmapTable;

    // First look for a 'full repertoire'/non-BMP map.
    {
        var idx: usize = 0;
        while (idx < numEntries) : (idx += 1) {
            const entry = cmap + 4 + idx * 8;
            const etype = readTtf(u16, ttf_mem[entry..]) * 0o100 + readTtf(u16, ttf_mem[entry + 2 ..]);
            // Complete unicode map
            if (etype == 0o004 or etype == 0o312) {
                const table = cmap + readTtf(u32, ttf_mem[entry + 4 ..]);
                if (table + 2 > ttf_mem.len)
                    return error.InvalidTtfBadCmapTable;
                switch (readTtf(u16, ttf_mem[table..])) {
                    12 => return cmap_fmt12_13(ttf_mem[table..], charCode, 12),
                    else => return error.InvalidTtfUnsupportedCmapFormat,
                }
            }
        }
    }

    // If no 'full repertoire' cmap was found, try looking for a BMP map.
    {
        var idx: usize = 0;
        while (idx < numEntries) : (idx += 1) {
            const entry = cmap + 4 + idx * 8;
            const etype = readTtf(u16, ttf_mem[entry..]) * 0o100 + readTtf(u16, ttf_mem[entry + 2 ..]);
            // Unicode BMP
            if (etype == 0o003 or etype == 0o301) {
                const table = cmap + readTtf(u32, ttf_mem[entry + 4 ..]);
                if (table + 6 > ttf_mem.len)
                    return error.InvalidTtfBadCmapTable;
                // Dispatch based on cmap format.
                switch (readTtf(u16, ttf_mem[table..])) {
                    4 => return cmap_fmt4(ttf_mem, table + 6, charCode),
                    //6 => return cmap_fmt6(font, table + 6, charCode, glyph),
                    6 => @panic("todo"),
                    else => return error.InvalidTtfUnsupportedCmapFormat,
                }
            }
        }
    }

    return error.UnsupportedCharCode; // I guess?
}

const HorMetrics = struct {
    advance_width: u16,
    left_side_bearing: i16,
};
fn hor_metrics(ttf_mem: []const u8, info: TtfInfo, glyph: u32) !HorMetrics {
    const hmtx = (try gettable2(ttf_mem, "hmtx")) orelse
        return error.InvalidTtfNoHmtxTable;

    if (glyph < info.numLongHmtx) {
        // glyph is inside long metrics segment.
        const offset = hmtx + 4 * glyph;
        if (offset + 4 > ttf_mem.len)
            return error.InvalidTtfBadHmtxTable;
        return .{
            .advance_width = readTtf(u16, ttf_mem[offset + 0 ..]),
            .left_side_bearing = readTtf(i16, ttf_mem[offset + 2 ..]),
        };
    }

    // glyph is inside short metrics segment.
    const boundary = hmtx + 4 * @intCast(usize, info.numLongHmtx);
    if (boundary < 4)
        return error.InvalidTtfBadHmtxTable;

    const width_offset = boundary - 4;
    if (width_offset + 2 > ttf_mem.len)
        return error.InvalidTtfBadHmtxTable;
    const bearing_offset = boundary + 2 * @intCast(usize, glyph - info.numLongHmtx);
    if (bearing_offset + 2 > ttf_mem.len)
        return error.InvalidTtfBadHmtxTable;
    return .{
        .advance_width = readTtf(u16, ttf_mem[width_offset..]),
        .left_side_bearing = readTtf(i16, ttf_mem[bearing_offset..]),
    };
}

const Bbox = struct {
    x_min: f64,
    x_max: f64,
    y_min: f64,
    y_max: f64,
};
fn glyph_bbox(ttf_mem: []const u8, info: TtfInfo, scale: XY(f64), offset: XY(f64), outline: usize) !Bbox {
    if (outline + 10 > ttf_mem.len)
        return error.InvalidTtfBadOutline;
    const box = [4]i16{
        readTtf(i16, ttf_mem[outline + 2 ..]),
        readTtf(i16, ttf_mem[outline + 4 ..]),
        readTtf(i16, ttf_mem[outline + 6 ..]),
        readTtf(i16, ttf_mem[outline + 8 ..]),
    };
    if (box[2] <= box[0] or box[3] <= box[1])
        return error.InvalidTtfBadBbox;
    const xScaleEm = scale.x / @intToFloat(f64, info.unitsPerEm);
    const yScaleEm = scale.y / @intToFloat(f64, info.unitsPerEm);
    return Bbox{
        .x_min = @floor(@intToFloat(f64, box[0]) * xScaleEm + offset.x),
        .x_max = @ceil(@intToFloat(f64, box[2]) * xScaleEm + offset.x),
        .y_min = @floor(@intToFloat(f64, box[1]) * yScaleEm + offset.y),
        .y_max = @ceil(@intToFloat(f64, box[3]) * yScaleEm + offset.y),
    };
}

// Returns the offset into the font that the glyph's outline is stored at.
fn outline_offset(ttf_mem: []const u8, info: TtfInfo, glyph: c.SFT_Glyph) !?usize {
    const loca = (try gettable2(ttf_mem, "loca")) orelse
        return error.InvalidTtfNoLocaTable;
    const glyf = (try gettable2(ttf_mem, "glyf")) orelse
        return error.InvalidttfNoGlyfTable;

    const entry = blk: {
        if (info.locaFormat == 0) {
            const base = loca + 2 * glyph;
            if (base + 4 > ttf_mem.len)
                return error.InvalidTtfBadLocaTable;
            break :blk .{
                .this = 2 * @as(u32, readTtf(u16, ttf_mem[base + 0 ..])),
                .next = 2 * @as(u32, readTtf(u16, ttf_mem[base + 2 ..])),
            };
        }

        const base = loca + 4 * glyph;
        if (base + 8 > ttf_mem.len)
            return error.InvalidTtfBadLocaTable;
        break :blk .{
            .this = readTtf(u32, ttf_mem[base + 0 ..]),
            .next = readTtf(u32, ttf_mem[base + 4 ..]),
        };
    };
    return if (entry.this == entry.next) null else glyf + entry.this;
}

// For a 'simple' outline, determines each point of the outline with a set of flags.
fn simple_flags(font: *Font, offset: c.uint_fast32_t, numPts: c.uint_fast16_t, flags: [*]u8) !c.uint_fast32_t {
    var off = offset;
    var repeat: u8 = 0;
    var value: u8 = 0;
    var point_index: c.uint_fast16_t = 0;
    while (point_index < numPts) : (point_index += 1) {
        if (repeat != 0) {
            repeat -= 1;
        } else {
            if (!is_safe_offset(font, off, 1))
                return error.InvalidTtfBadOutline;
            value = font.mem[off];
            off += 1;
            if ((value & ttf.repeat_flag) != 0) {
                if (!is_safe_offset(font, off, 1))
                    return error.InvalidTtfBadOutline;
                repeat = font.mem[off];
                off += 1;
            }
        }
        flags[point_index] = value;
    }
    return off;
}

fn resolveSign(comptime T: type, is_pos: bool, value: T) T {
    const all_ones = @bitCast(T, switch (T) {
        i32 => @as(u32, 0xffffffff),
        else => @compileError("not implemented"),
    });
    const xor_mask = if (is_pos) all_ones else 0;
    return (value ^ xor_mask) + @as(T, @boolToInt(is_pos));
}

// For a 'simple' outline, decodes both X and Y coordinates for each point of the outline. */
fn simple_points(
    font: *Font,
    offset: c.uint_fast32_t,
    numPts: c.uint_fast16_t,
    flags: [*]u8,
    points: [*]Point,
) !void {
    var off = offset;
    const Accum = i32;
    {
        var accum: Accum = 0;
        var i: c.uint_fast16_t = 0;
        while (i < numPts) : (i += 1) {
            if ((flags[i] & ttf.x_change_is_small) != 0) {
                if (!is_safe_offset(font, off, 1))
                    return error.InvalidTtfBadOutline;
                const value = font.mem[off];
                off += 1;
                const is_pos = (flags[i] & ttf.x_change_is_positive) != 0;
                accum -= resolveSign(Accum, is_pos, value);
            } else if (0 == (flags[i] & ttf.x_change_is_zero)) {
                if (!is_safe_offset(font, off, 2))
                    return error.InvalidTtfBadOutline;
                accum += geti16(font, off);
                off += 2;
            }
            points[i].x = @intToFloat(f64, accum);
        }
    }

    {
        var accum: Accum = 0;
        var i: c.uint_fast16_t = 0;
        while (i < numPts) : (i += 1) {
            if ((flags[i] & ttf.y_change_is_small) != 0) {
                if (!is_safe_offset(font, off, 1))
                    return error.InvalidTtfBadOutline;
                const value = font.mem[off];
                off += 1;
                const is_pos = (flags[i] & ttf.y_change_is_positive) != 0;
                accum -= resolveSign(Accum, is_pos, value);
            } else if (0 == (flags[i] & ttf.y_change_is_zero)) {
                if (!is_safe_offset(font, off, 2))
                    return error.InvalidTtfBadOutline;
                accum += geti16(font, off);
                off += 2;
            }
            points[i].y = @intToFloat(f64, accum);
        }
    }
}

fn decode_contour(
    flags_start: [*]u8,
    basePointStart: c.uint_fast16_t,
    count_start: c.uint_fast16_t,
    outl: *Outline,
) !void {
    // Skip contours with less than two points, since the following algorithm can't handle them and
    // they should appear invisible either way (because they don't have any area).
    if (count_start < 2) return;
    std.debug.assert(basePointStart <= std.math.maxInt(u16) - count_start);

    var flags = flags_start;
    var basePoint = basePointStart;
    var count = count_start;
    const looseEnd: c.uint_least16_t = blk: {
        if (0 != (flags[0] & ttf.point_is_on_curve)) {
            basePoint += 1;
            flags += 1;
            count -= 1;
            break :blk @intCast(c.uint_least16_t, basePoint - 1);
        }
        if (0 != (flags[count - 1] & ttf.point_is_on_curve)) {
            count -= 1;
            break :blk @intCast(c.uint_least16_t, basePoint + count);
        }

        if (outl.points.items.len >= outl.points.capacity)
            try grow_points(outl);

        outl.points.items.ptr[outl.points.items.len] = midpoint(outl.points.items.ptr[basePoint], outl.points.items.ptr[basePoint + count - 1]);
        const looseEnd = @intCast(c.uint_least16_t, outl.points.items.len);
        outl.points.items.len += 1;
        break :blk looseEnd;
    };
    var beg = looseEnd;
    var opt_ctrl: ?c.uint_least16_t = null;
    var i: c.uint_fast16_t = 0;
    while (i < count) : (i += 1) {
        // cur can't overflow because we ensure that basePoint + count < 0xFFFF before calling decode_contour().
        const cur = @intCast(c.uint_least16_t, basePoint + i);
        if (0 != (flags[i] & ttf.point_is_on_curve)) {
            if (opt_ctrl) |ctrl| {
                if (outl.curves.items.len >= outl.curves.capacity)
                    try grow_curves(outl);
                outl.curves.items.ptr[outl.curves.items.len] = Curve{ .beg = beg, .end = cur, .ctrl = ctrl };
                outl.curves.items.len += 1;
            } else {
                if (outl.lines.items.len >= outl.lines.capacity)
                    try grow_lines(outl);
                outl.lines.items.ptr[outl.lines.items.len] = Line{ .beg = beg, .end = cur };
                outl.lines.items.len += 1;
            }
            beg = cur;
            opt_ctrl = null;
        } else {
            if (opt_ctrl) |ctrl| {
                const center: c.uint_least16_t = @intCast(c.uint_least16_t, outl.points.items.len);
                if (outl.points.items.len >= outl.points.capacity)
                    try grow_points(outl);
                outl.points.items.ptr[center] = midpoint(outl.points.items.ptr[ctrl], outl.points.items.ptr[cur]);
                outl.points.items.len += 1;

                if (outl.curves.items.len >= outl.curves.capacity)
                    try grow_curves(outl);
                outl.curves.items.ptr[outl.curves.items.len] = Curve{ .beg = beg, .end = center, .ctrl = ctrl };
                outl.curves.items.len += 1;

                beg = center;
            }
            opt_ctrl = cur;
        }
    }
    if (opt_ctrl) |ctrl| {
        if (outl.curves.items.len >= outl.curves.capacity)
            try grow_curves(outl);
        outl.curves.items.ptr[outl.curves.items.len] = Curve{ .beg = beg, .end = looseEnd, .ctrl = ctrl };
        outl.curves.items.len += 1;
    } else {
        if (outl.lines.items.len >= outl.lines.capacity)
            try grow_lines(outl);
        outl.lines.items.ptr[outl.lines.items.len] = Line{ .beg = beg, .end = looseEnd };
        outl.lines.items.len += 1;
    }
}

fn StackBuf(comptime T: type, comptime stack_len: usize) type {
    return struct {
        buf: [stack_len]T = undefined,
        fn alloc(self: *@This(), len: usize) error{OutOfMemory}![*]T {
            if (len <= stack_len) return &self.buf;
            return try malloc(T, len);
        }
        fn free(self: *@This(), ptr: [*]T) void {
            if (ptr != &self.buf) {
                c.free(ptr);
            }
        }
    };
}
fn stackBuf(comptime T: type, comptime stack_len: usize) StackBuf(T, stack_len) {
    return .{};
}

fn simple_outline(
    font: *Font,
    offset_start: c.uint_fast32_t,
    numContours: u15,
    outl: *Outline,
) !void {
    std.debug.assert(numContours > 0);
    const basePoint: c.uint_fast16_t = @intCast(c.uint_fast16_t, outl.points.items.len);

    if (!is_safe_offset(font, offset_start, numContours * 2 + 2))
        return error.InvalidTtfBadOutline;
    var numPts = getu16(font, offset_start + (numContours - 1) * 2);
    if (numPts >= std.math.maxInt(u16))
        return error.InvalidTtfBadOutline;
    numPts += 1;
    if (outl.points.items.len > std.math.maxInt(u16) - numPts)
        return error.InvalidTtfBadOutline;

    // TODO: this should be a single realloc
    while (outl.points.capacity < basePoint + numPts) {
        try grow_points(outl);
    }

    // TODO: the following commented line should work but the zig compiler
    //       isn't optimizing it correctly which causes *extreme* slowdown
    //var endPtsStackBuf = stackBuf(c.uint_fast16_t, 16);
    var endPtsStackBuf: StackBuf(c.uint_fast16_t, 16) = undefined;
    const endPts = try endPtsStackBuf.alloc(numContours);
    defer endPtsStackBuf.free(endPts);

    // TODO: the following commented line should work but the zig compiler
    //       isn't optimizing it correctly which causes *extreme* slowdown
    //var flagsStackBuf = stackBuf(u8, 128);
    var flagsStackBuf: StackBuf(u8, 128) = undefined;
    const flags = try flagsStackBuf.alloc(numPts);
    defer flagsStackBuf.free(flags);

    var offset = offset_start;
    {
        var i: c_uint = 0;
        while (i < numContours) : (i += 1) {
            endPts[i] = getu16(font, offset);
            offset += 2;
        }
    }

    // Ensure that endPts are never falling.
    // Falling endPts have no sensible interpretation and most likely only occur in malicious input.
    // Therefore, we bail, should we ever encounter such input.
    {
        var i: c_uint = 0;
        while (i < numContours - 1) : (i += 1) {
            if (endPts[i + 1] < endPts[i] + 1)
                return error.InvalidTtfBadOutline;
        }
    }
    offset += 2 + @as(u32, getu16(font, offset));

    offset = try simple_flags(font, offset, numPts, flags);
    try simple_points(font, offset, numPts, flags, outl.points.items.ptr + basePoint);
    outl.points.items.len = @intCast(c.uint_least16_t, outl.points.items.len + numPts);

    var beg: c.uint_fast16_t = 0;
    {
        var i: c_uint = 0;
        while (i < numContours) : (i += 1) {
            const count: c.uint_fast16_t = endPts[i] - beg + 1;
            try decode_contour(flags + beg, basePoint + beg, count, outl);
            beg = endPts[i] + 1;
        }
    }
}

fn compound_outline(
    font: *Font,
    offset_start: c.uint_fast32_t,
    recDepth: u8,
    outl: *Outline,
) !void {
    // Guard against infinite recursion (compound glyphs that have themselves as component).
    if (recDepth >= 4)
        return error.InvalidTtfOutlineTooRecursive;
    var offset = offset_start;
    while (true) {
        var local = [_]f64{0} ** 6;
        if (!is_safe_offset(font, offset, 4))
            return error.InvalidTtfBadOutline;
        const flags = getu16(font, offset);
        const glyph = getu16(font, offset + 2);
        offset += 4;
        // We don't implement point matching, and neither does stb_truetype for that matter.
        if (0 == (flags & ttf.actual_xy_offsets))
            return error.TtfPointMatchingNotSupported;
        // Read additional X and Y offsets (in FUnits) of this component.
        if (0 != (flags & ttf.offsets_are_large)) {
            if (!is_safe_offset(font, offset, 4))
                return error.InvalidTtfBadOutline;
            local[4] = @intToFloat(f64, geti16(font, offset));
            local[5] = @intToFloat(f64, geti16(font, offset + 2));
            offset += 4;
        } else {
            if (!is_safe_offset(font, offset, 2))
                return error.InvalidTtfBadOutline;
            local[4] = @intToFloat(f64, geti8(font, offset));
            local[5] = @intToFloat(f64, geti8(font, offset + 1));
            offset += 2;
        }
        if (0 != (flags & ttf.got_a_single_scale)) {
            if (!is_safe_offset(font, offset, 2))
                return error.InvalidTtfBadOutline;
            local[0] = @intToFloat(f64, geti16(font, offset)) / 16384.0;
            local[3] = local[0];
            offset += 2;
        } else if (0 != (flags & ttf.got_an_x_and_y_scale)) {
            if (!is_safe_offset(font, offset, 4))
                return error.InvalidTtfBadOutline;
            local[0] = @intToFloat(f64, geti16(font, offset + 0)) / 16384.0;
            local[3] = @intToFloat(f64, geti16(font, offset + 2)) / 16384.0;
            offset += 4;
        } else if (0 != (flags & ttf.got_a_scale_matrix)) {
            if (!is_safe_offset(font, offset, 8))
                return error.InvalidTtfBadOutline;
            local[0] = @intToFloat(f64, geti16(font, offset + 0)) / 16384.0;
            local[1] = @intToFloat(f64, geti16(font, offset + 2)) / 16384.0;
            local[2] = @intToFloat(f64, geti16(font, offset + 4)) / 16384.0;
            local[3] = @intToFloat(f64, geti16(font, offset + 6)) / 16384.0;
            offset += 8;
        } else {
            local[0] = 1.0;
            local[3] = 1.0;
        }
        // At this point, Apple's spec more or less tells you to scale the matrix by its own L1 norm.
        // But stb_truetype scales by the L2 norm. And FreeType2 doesn't scale at all.
        // Furthermore, Microsoft's spec doesn't even mention anything like this.
        // It's almost as if nobody ever uses this feature anyway.
        if (try outline_offset(font.mem, font.info, glyph)) |outline| {
            const basePoint = outl.points.items.len;
            try decode_outline(font, outline, recDepth + 1, outl);
            transform_points(outl.points.items.ptr[basePoint..outl.points.items.len], &local);
        }

        if (0 == (flags & ttf.there_are_more_components)) break;
    }
}

fn decode_outline(font: *Font, offset: c.uint_fast32_t, recDepth: u8, outl: *Outline) !void {
    if (!is_safe_offset(font, offset, 10))
        return error.InvalidTtfBadOutline;
    const numContours = geti16(font, offset);
    if (numContours > 0) {
        // Glyph has a 'simple' outline consisting of a number of contours.
        return simple_outline(font, offset + 10, @intCast(u15, numContours), outl);
    } else if (numContours < 0) {
        // Glyph has a compound outline combined from mutiple other outlines.
        return compound_outline(font, offset + 10, recDepth, outl);
    }
}

// A heuristic to tell whether a given curve can be approximated closely enough by a line.
fn is_flat(outline: *Outline, curve: Curve) bool {
    const maxArea2: f64 = 2.0;
    const a = outline.points.items.ptr[curve.beg];
    const b = outline.points.items.ptr[curve.ctrl];
    const cpoint = outline.points.items.ptr[curve.end];
    const g = Point{ .x = b.x - a.x, .y = b.y - a.y };
    const h = Point{ .x = cpoint.x - a.x, .y = cpoint.y - a.y };
    const area2 = std.math.fabs(g.x * h.y - h.x * g.y);
    return area2 <= maxArea2;
}

fn tesselate_curve(curve_in: Curve, outline: *Outline) !void {
    // From my tests I can conclude that this stack barely reaches a top height
    // of 4 elements even for the largest font sizes I'm willing to support. And
    // as space requirements should only grow logarithmically, I think 10 is
    // more than enough.
    const STACK_SIZE = 10;
    var stack: [STACK_SIZE]Curve = undefined;
    var top: usize = 0;
    var curve = curve_in;
    while (true) {
        if (is_flat(outline, curve) or top >= STACK_SIZE) {
            if (outline.lines.items.len >= outline.lines.capacity)
                try grow_lines(outline);
            outline.lines.items.ptr[outline.lines.items.len] = .{ .beg = curve.beg, .end = curve.end };
            outline.lines.items.len += 1;
            if (top == 0) break;
            top -= 1;
            curve = stack[top];
        } else {
            const ctrl0 = @intCast(c.uint_least16_t, outline.points.items.len);
            if (outline.points.items.len >= outline.points.capacity)
                try grow_points(outline);
            outline.points.items.ptr[ctrl0] = midpoint(outline.points.items.ptr[curve.beg], outline.points.items.ptr[curve.ctrl]);
            outline.points.items.len += 1;

            const ctrl1 = @intCast(c.uint_least16_t, outline.points.items.len);
            if (outline.points.items.len >= outline.points.capacity)
                try grow_points(outline);
            outline.points.items.ptr[ctrl1] = midpoint(outline.points.items.ptr[curve.ctrl], outline.points.items.ptr[curve.end]);
            outline.points.items.len += 1;

            const pivot = @intCast(c.uint_least16_t, outline.points.items.len);
            if (outline.points.items.len >= outline.points.capacity)
                try grow_points(outline);
            outline.points.items.ptr[pivot] = midpoint(outline.points.items.ptr[ctrl0], outline.points.items.ptr[ctrl1]);
            outline.points.items.len += 1;

            stack[top] = .{ .beg = curve.beg, .end = pivot, .ctrl = ctrl0 };
            top += 1;
            curve = .{ .beg = pivot, .end = curve.end, .ctrl = ctrl1 };
        }
    }
}

fn sign(x: f64) i2 {
    if (x > 0) return 1;
    if (x < 0) return -1;
    return 0;
}
fn fast_floor(x: f64) c_int {
    return @floatToInt(c_int, std.math.floor(x));
}
fn fast_ceil(x: f64) c_int {
    return @floatToInt(c_int, std.math.ceil(x));
}

// Draws a line into the buffer. Uses a custom 2D raycasting algorithm to do so.
fn draw_line(buf: Raster, origin: Point, goal: Point) void {
    const delta = Point{
        .x = goal.x - origin.x,
        .y = goal.y - origin.y,
    };
    const dir = struct { x: i2, y: i2 }{
        .x = sign(delta.x),
        .y = sign(delta.y),
    };

    if (dir.y == 0) {
        return;
    }
    const crossingIncr = Point{
        .x = if (dir.x != 0) std.math.fabs(1.0 / delta.x) else 1.0,
        .y = std.math.fabs(1.0 / delta.y),
    };

    var pixel: struct { x: i32, y: i32 } = undefined;
    //struct { int x, y; } pixel;
    var nextCrossing: Point = undefined;
    var numSteps: c_int = 0;
    if (dir.x == 0) {
        pixel.x = fast_floor(origin.x);
        nextCrossing.x = 100.0;
    } else {
        if (dir.x > 0) {
            pixel.x = fast_floor(origin.x);
            nextCrossing.x = (origin.x - @intToFloat(f64, pixel.x)) * crossingIncr.x;
            nextCrossing.x = crossingIncr.x - nextCrossing.x;
            numSteps += fast_ceil(goal.x) - fast_floor(origin.x) - 1;
        } else {
            pixel.x = fast_ceil(origin.x) - 1;
            nextCrossing.x = (origin.x - @intToFloat(f64, pixel.x)) * crossingIncr.x;
            numSteps += fast_ceil(origin.x) - fast_floor(goal.x) - 1;
        }
    }

    if (dir.y > 0) {
        pixel.y = fast_floor(origin.y);
        nextCrossing.y = (origin.y - @intToFloat(f64, pixel.y)) * crossingIncr.y;
        nextCrossing.y = crossingIncr.y - nextCrossing.y;
        numSteps += fast_ceil(goal.y) - fast_floor(origin.y) - 1;
    } else {
        pixel.y = fast_ceil(origin.y) - 1;
        nextCrossing.y = (origin.y - @intToFloat(f64, pixel.y)) * crossingIncr.y;
        numSteps += fast_ceil(origin.y) - fast_floor(goal.y) - 1;
    }

    var nextDistance = std.math.min(nextCrossing.x, nextCrossing.y);
    const halfDeltaX = 0.5 * delta.x;
    var prevDistance: f64 = 0.0;
    var step: c_int = 0;
    while (step < numSteps) : (step += 1) {
        var xAverage = origin.x + (prevDistance + nextDistance) * halfDeltaX;
        const yDifference = (nextDistance - prevDistance) * delta.y;
        const cptr = &buf.cells[@intCast(usize, pixel.y * buf.width + pixel.x)];
        var cell = cptr.*;
        cell.cover += yDifference;
        xAverage -= @intToFloat(f64, pixel.x);
        cell.area += (1.0 - xAverage) * yDifference;
        cptr.* = cell;
        prevDistance = nextDistance;
        const alongX = nextCrossing.x < nextCrossing.y;
        pixel.x += if (alongX) dir.x else 0;
        pixel.y += if (alongX) 0 else dir.y;
        nextCrossing.x += if (alongX) crossingIncr.x else 0.0;
        nextCrossing.y += if (alongX) 0.0 else crossingIncr.y;
        nextDistance = std.math.min(nextCrossing.x, nextCrossing.y);
    }

    var xAverage = origin.x + (prevDistance + 1.0) * halfDeltaX;
    const yDifference = (1.0 - prevDistance) * delta.y;
    const cptr = &buf.cells[@intCast(usize, pixel.y * buf.width + pixel.x)];
    var cell = cptr.*;
    cell.cover += yDifference;
    xAverage -= @intToFloat(f64, pixel.x);
    cell.area += (1.0 - xAverage) * yDifference;
    cptr.* = cell;
}

fn draw_lines(outline: *Outline, buf: Raster) void {
    var i: usize = 0;
    while (i < outline.lines.items.len) : (i += 1) {
        const line = outline.lines.items.ptr[i];
        const origin = outline.points.items.ptr[line.beg];
        const goal = outline.points.items.ptr[line.end];
        draw_line(buf, origin, goal);
    }
}

// Integrate the values in the buffer to arrive at the final grayscale image.
fn post_process(buf: Raster, image: [*]u8) void {
    var accum: f64 = 0;
    const num = @intCast(usize, buf.width) * @intCast(usize, buf.height);
    var i: usize = 0;
    while (i < num) : (i += 1) {
        const cell = buf.cells[i];
        var value = std.math.fabs(accum + cell.area);
        value = std.math.min(value, 1.0);
        value = value * 255.0 + 0.5;
        image[i] = @floatToInt(u8, value);
        accum += cell.cover;
    }
}

fn render_outline(outl: *Outline, transform: *const [6]f64, image: c.SFT_Image) !void {
    transform_points(outl.points.items.ptr[0..outl.points.items.len], transform);
    clip_points(outl.points.items.ptr[0..outl.points.items.len], @intToFloat(f64, image.width), @intToFloat(f64, image.height));

    {
        var i: usize = 0;
        while (i < outl.curves.items.len) : (i += 1) {
            try tesselate_curve(outl.curves.items.ptr[i], outl);
        }
    }

    const numPixels = @intCast(usize, image.width) * @intCast(usize, image.height);
    // TODO: the following commented line should work but the zig compiler
    //       isn't optimizing it correctly which causes *extreme* slowdown
    // Zig's 'undefined' debug checks make this ungodly slow
    const stack_len = if (builtin.mode == .Debug) 0 else 128 * 128;
    //var cellStackBuf = stackBuf(Cell, stack_len);
    var cellStackBuf: StackBuf(Cell, stack_len) = undefined;
    const cells = try cellStackBuf.alloc(numPixels);
    defer cellStackBuf.free(cells);

    // TODO: I wonder if this could be removed?
    @memset(@ptrCast([*]u8, cells), 0, numPixels * @sizeOf(@TypeOf(cells[0])));
    const buf = Raster{
        .cells = cells,
        .width = image.width,
        .height = image.height,
    };
    draw_lines(outl, buf);
    post_process(buf, @ptrCast([*]u8, image.pixels));
}
