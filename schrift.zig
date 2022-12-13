const builtin = @import("builtin");
const std = @import("std");
const c = @cImport({
    @cInclude("string.h");
    @cInclude("stdlib.h");
    @cInclude("math.h");
    @cInclude("schrift.h");
});

const Font = struct {
    memory: [*]u8,
    // TODO: make this a usize
    size: c.uint_fast32_t,
    mapping: if (builtin.os.tag == .windows) ?std.os.windows.HANDLE else void,
    source: enum {
        mapped_file,
        user_supplied_memory,
    },
    unitsPerEm: c.uint_least16_t,
    locaFormat: c.int_least16_t,
    numLongHmtx: c.uint_least16_t,
    pub fn fromC(ptr: *c.SFT_Font) *Font {
        return @ptrCast(*Font, @alignCast(@alignOf(Font), ptr));
    }
    pub fn toC(self: *Font) *c.SFT_Font {
        return @ptrCast(*c.SFT_Font, self);
    }
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

    pub const point_is_on_curve     = 0x01;
    pub const x_change_is_small     = 0x02;
    pub const y_change_is_small     = 0x04;
    pub const repeat_flag           = 0x08;
    pub const x_change_is_zero      = 0x10;
    pub const x_change_is_positive  = 0x10;
    pub const y_change_is_zero      = 0x20;
    pub const y_change_is_positive  = 0x20;

    pub const offsets_are_large         =  0x001;
    pub const actual_xy_offsets         =  0x002;
    pub const got_a_single_scale        =  0x008;
    pub const there_are_more_components =  0x020;
    pub const got_an_x_and_y_scale      =  0x040;
    pub const got_a_scale_matrix        =  0x080;
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
    font.memory = mem;
    font.size = @intCast(u32, size);
    font.source = .user_supplied_memory;
    init_font(font) catch {
        c.sft_freefont(font.toC());
        return null;
    };
    return font.toC();
}

// Loads a font from the file system. To do so, it has to map the entire font into memory.
export fn sft_loadfile(filename: [*:0]const u8) ?*c.SFT_Font {
    const font = allocFont() catch return null;
    map_file(font, filename) catch |err| {
        std.log.err("map file '{s}' failed with {s}", .{filename, @errorName(err)});
	c.free(font);
	return null;
    };
    init_font(font) catch {
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
    @memset(@ptrCast([*]u8, metrics), 0, @sizeOf(@TypeOf(metrics.*)));
    var hhea: c.uint_fast32_t = undefined;
    const font = Font.fromC(sft.font orelse unreachable);
    if (gettable(font, "hhea", &hhea) < 0)
	return -1;
    if (!is_safe_offset(font, hhea, 36))
	return -1;
    const factor = sft.yScale / @intToFloat(f64, font.*.unitsPerEm);
    metrics.ascender  = @intToFloat(f64, geti16(font, hhea + 4)) * factor;
    metrics.descender = @intToFloat(f64, geti16(font, hhea + 6)) * factor;
    metrics.lineGap   = @intToFloat(f64, geti16(font, hhea + 8)) * factor;
    return 0;
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
    const hor = hor_metrics(font, glyph) catch return -1;
    const xScale = sft.xScale / @intToFloat(f64, font.*.unitsPerEm);
    metrics.advanceWidth    = @intToFloat(f64, hor.advance_width) * xScale;
    metrics.leftSideBearing = @intToFloat(f64, hor.left_side_bearing) * xScale + sft.xOffset;

    const outline = outline_offset(font, glyph) catch return -1;
    if (outline == 0)
	return 0;
    const bbox = glyph_bbox(sft, outline) catch return -1;
    metrics.minWidth  = bbox[2] - bbox[0] + 1;
    metrics.minHeight = bbox[3] - bbox[1] + 1;
    metrics.yOffset   = if ((sft.flags & c.SFT_DOWNWARD_Y) != 0) -bbox[3] else bbox[1];
    return 0;
}

export fn sft_render(sft: *c.SFT, glyph: c.SFT_Glyph, image: c.SFT_Image) c_int {
    const font = Font.fromC(sft.font orelse unreachable);
    const outline = outline_offset(font, glyph) catch return -1;
    if (outline == 0)
	return 0;
    const bbox = glyph_bbox(sft, outline) catch return -1;
    // Set up the transformation matrix such that
    // the transformed bounding boxes min corner lines
    // up with the (0, 0) point.
    var transform: [6]f64 = undefined;
    transform[0] = sft.xScale / @intToFloat(f64, font.*.unitsPerEm);
    transform[1] = 0.0;
    transform[2] = 0.0;
    transform[4] = sft.xOffset - @intToFloat(f64, bbox[0]);
    if ((sft.flags & c.SFT_DOWNWARD_Y) != 0) {
	transform[3] = -sft.yScale / @intToFloat(f64, font.*.unitsPerEm);
	transform[5] = @intToFloat(f64, bbox[3]) - sft.yOffset;
    } else {
	transform[3] = sft.yScale / @intToFloat(f64, font.*.unitsPerEm);
	transform[5] = sft.yOffset - @intToFloat(f64, bbox[1]);
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
    if (file_size > std.math.maxInt(u32))
        return error.FileTooBig;
    font.size = @intCast(u32, file_size);
    if (builtin.os.tag == .windows) {
        font.mapping = win32.CreateFileMappingW(
            file.handle,
            null,
            win32.PAGE_READONLY,
            0, font.size,
            null,
        ) orelse switch (std.os.windows.kernel32.GetLastError()) {
            //.ACCESS_DENIED => return error.PermissionDenied,
            else => |err| return std.os.windows.unexpectedError(err),
        };
        errdefer {
            std.os.windows.CloseHandle(font.mapping.?);
            font.mapping = null;
        }
        font.memory = win32.MapViewOfFile(
            font.mapping.?,
            win32.FILE_MAP_READ,
            0, 0, null,
        ) orelse switch (std.os.windows.kernel32.GetLastError()) {
            //.ACCESS_DENIED => return error.PermissionDenied,
            else => |err| return std.os.windows.unexpectedError(err),
        };
    } else {
        const mem = try std.os.mmap(null, font.size, std.os.PROT.READ, std.os.MAP.PRIVATE, file.handle, 0);
        std.debug.assert(mem.len == font.size);
        font.memory = mem.ptr;
    }
}

fn unmap_file(font: *Font) void {
    if (builtin.os.tag == .windows) {
        std.debug.assert(0 != win32.UnmapViewOfFile(font.memory));
        std.os.windows.CloseHandle(font.mapping.?);
    } else {
	//std.debug.assert(font.memory != std.os.MAP.FAILED);
	std.os.munmap(@alignCast(std.mem.page_size, font.memory)[0 .. font.size]);
    }
}

fn init_font(font: *Font) !void {
    if (!is_safe_offset(font, 0, 12))
        return error.InvalidTtfTooSmall;

    // Check for a compatible scalerType (magic number).
    const scalerType = getu32(font, 0);
    if (scalerType != ttf.file_magic_one and scalerType != ttf.file_magic_two)
	return error.InvalidTtfBadMagic;

    var head: c.uint_fast32_t = undefined;
    if (gettable(font, "head", &head) < 0)
	return error.InvalidTtfNoHeadTable;
    if (!is_safe_offset(font, head, 54))
        return error.InvalidTtfBadHeadTable;
    font.unitsPerEm = getu16(font, head + 18);
    font.locaFormat = geti16(font, head + 50);
    var hhea: c.uint_fast32_t = undefined;
    if (gettable(font, "hhea", &hhea) < 0)
        return error.InvalidTtfNoHheaTable;
    if (!is_safe_offset(font, hhea, 36))
        return error.InvalidTtfBadHheaTable;
    font.numLongHmtx = getu16(font, hhea + 34);
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

fn grow(comptime T: type, cap_ref: *usize, ptr_ref: *[*]T) error{OutOfMemory,TooManyPrimitives}!void {
    std.debug.assert(cap_ref.* > 0);
    const next_cap = cap_ref.* * 2;
    std.debug.assert(next_cap > cap_ref.*);
    const ptr = c.realloc(ptr_ref.*, next_cap * @sizeOf(T)) orelse return error.OutOfMemory;
    cap_ref.* = next_cap;
    ptr_ref.* = @ptrCast([*]T, @alignCast(@alignOf(T), ptr));
}

fn grow_points(outline: *Outline) c_int {
    grow(Point, &outline.points.capacity, &outline.points.items.ptr) catch return -1;
    return 0;
}

fn grow_curves(outline: *Outline) c_int {
    grow(Curve, &outline.curves.capacity, &outline.curves.items.ptr) catch return -1;
    return 0;
}

fn grow_lines(outline: *Outline) c_int {
    grow(Line, &outline.lines.capacity, &outline.lines.items.ptr) catch return -1;
    return 0;
}

fn is_safe_offset(font: *Font, offset: c.uint_fast32_t, margin: u32) bool {
    if (offset > font.size) return false;
    if (font.size - offset < margin) return false;
    return true;
}

// Like bsearch(), but returns the next highest element if key could not be found.
fn csearch(
    key: *const anyopaque,
    base: *const anyopaque,
    nmemb: usize,
    size: usize,
    compar: *const fn(?*const anyopaque, ?*const anyopaque) callconv(.C) c_int,
) ?*const anyopaque {

    if (nmemb == 0) return null;

    const bytes = @ptrCast([*]const u8, base);
    var low: usize = 0;
    var high: usize = nmemb - 1;
    while (low != high) {
	const mid = low + (high - low) / 2;
	const sample = bytes + mid * size;
	if (compar(key, sample) > 0) {
	    low = mid + 1;
	} else {
	    high = mid;
	}
    }
    return bytes + low * size;
}

// Used as a comparison function for [bc]search().
export fn cmpu16(a: ?*const anyopaque, b: ?*const anyopaque) c_int {
    return c.memcmp(a, b, 2);
}

// Used as a comparison function for [bc]search().
export fn cmpu32(a: ?*const anyopaque, b: ?*const anyopaque) c_int {
    return c.memcmp(a, b, 4);
}

fn getu8(font: *Font, offset: usize) u8 {
    std.debug.assert(offset + 1 <= font.size);
    return font.memory[offset];
}
fn geti8(font: *Font, offset: usize) i8 {
    return @bitCast(i8, getu8(font, offset));
}
fn geti16(font: *Font, offset: usize) i16 {
    std.debug.assert(offset + 2 <= font.size);
    return std.mem.readIntBig(i16, @ptrCast(*const [2]u8, font.memory + offset));
}
fn getu16(font: *Font, offset: usize) u16 {
    std.debug.assert(offset + 2 <= font.size);
    return std.mem.readIntBig(u16, @ptrCast(*const [2]u8, font.memory + offset));
}
fn getu32(font: *Font, offset: usize) u32 {
    std.debug.assert(offset + 4 <= font.size);
    return std.mem.readIntBig(u32, @ptrCast(*const [4]u8, font.memory + offset));
}

fn gettable(font: *Font, tag: *const [4]u8, offset: *c.uint_fast32_t) c_int {
    // No need to bounds-check access to the first 12 bytes - this gets already checked by init_font().
    const numTables = getu16(font, 4);
    if (!is_safe_offset(font, 12, numTables * 16))
	return -1;
    const match = c.bsearch(tag, font.memory + 12, numTables, 16, cmpu32) orelse return -1;
    offset.* = getu32(font, @intCast(c.uint_fast32_t, @ptrToInt(match) - @ptrToInt(font.memory) + 8));
    return 0;
}

fn cmap_fmt4(font: *Font, table: c.uint_fast32_t, charCode: c.SFT_UChar) !c.SFT_Glyph {
    // cmap format 4 only supports the Unicode BMP.
    if (charCode > 0xFFFF)
	return 0;

    const shortCode = @intCast(c.uint_fast16_t, charCode);
    if (!is_safe_offset(font, table, 8))
        return error.InvalidTtfBadCmapTable;
    const segCountX2 = getu16(font, table);
    if (((segCountX2 & 1) != 0) or (0 == segCountX2))
        return error.InvalidTtfBadCmapTable;

    // Find starting positions of the relevant arrays.
    const endCodes       = table + 8;
    const startCodes     = endCodes + segCountX2 + 2;
    const idDeltas       = startCodes + segCountX2;
    const idRangeOffsets = idDeltas + segCountX2;
    if (!is_safe_offset(font, idRangeOffsets, segCountX2))
        return error.InvalidTtfBadCmapTable;

    // Find the segment that contains shortCode by binary searching over
    // the highest codes in the segments.
    const key = [2]u8{ @intCast(u8, charCode >> 8), @intCast(u8, charCode & 0xff) };
    const segAddr = @ptrToInt(csearch(&key, font.memory + endCodes, segCountX2 / 2, 2, cmpu16));
    const segIdxX2 = @intCast(c.uint_fast32_t, (segAddr - (@ptrToInt(font.memory) + endCodes)));
    // Look up segment info from the arrays & short circuit if the spec requires.
    const startCode = getu16(font, startCodes + segIdxX2);
    if (startCode > shortCode)
	return 0;
    const idDelta = getu16(font, idDeltas + segIdxX2);
    const idRangeOffset = getu16(font, idRangeOffsets + segIdxX2);
    if (idRangeOffset == 0) {
	// Intentional integer under- and overflow.
        // TODO: not sure if this is correct?
	return @intCast(c.SFT_Glyph, (@intCast(u32, shortCode) + @intCast(u32, idDelta)) & 0xFFFF);
    }
    // Calculate offset into glyph array and determine ultimate value.
    const idOffset = idRangeOffsets + segIdxX2 + idRangeOffset + 2 * (shortCode - startCode);
    if (!is_safe_offset(font, idOffset, 2))
        return error.InvalidTtfBadCmapTable;
    const id = getu16(font, idOffset);
    // Intentional integer under- and overflow.
    return if (id == 0) 0 else @intCast(c.SFT_Glyph, ((@intCast(u32, id) + @intCast(u32, idDelta)) & 0xFFFF));
}

fn cmap_fmt12_13(font: *Font, table: c.uint_fast32_t, charCode: c.SFT_UChar, which: c_int) !c.SFT_Glyph {
    // check that the entire header is present
    if (!is_safe_offset(font, table, 16))
        return error.InvalidTtfBadCmapTable;

    const len = getu32(font, table + 4);
    // A minimal header is 16 bytes
    if (len < 16)
        return error.InvalidTtfBadCmapTable;
    if (!is_safe_offset(font, table, len))
        return error.InvalidTtfBadCmapTable;

    const numEntries = getu32(font, table + 12);
    var i: c.uint_fast32_t = 0;
    while (i < numEntries) : (i += 1) {
	const firstCode = getu32(font, table + (i * 12) + 16);
	const lastCode = getu32(font, table + (i * 12) + 16 + 4);
	if (charCode < firstCode or charCode > lastCode)
	    continue;
	const glyphOffset = getu32(font, table + (i * 12) + 16 + 8);
	return if (which == 12) (charCode-firstCode) + glyphOffset else glyphOffset;
    }
    return 0;
}

// Maps Unicode code points to glyph indices.
fn glyph_id(font: *Font, charCode: c.SFT_UChar) !c.SFT_Glyph {
    var cmap: c.uint_fast32_t = undefined;
    if (gettable(font, "cmap", &cmap) < 0)
        return error.InvalidTtfNoCmapTable;

    if (!is_safe_offset(font, cmap, 4))
        return error.InvalidTtfBadCmapTable;
    const numEntries: u32 = getu16(font, cmap + 2);

    if (!is_safe_offset(font, cmap, 4 + numEntries * 8))
        return error.InvalidTtfBadCmapTable;

    // First look for a 'full repertoire'/non-BMP map.
    {
        var idx: usize = 0;
        while (idx < numEntries) : (idx += 1) {
	    const entry = cmap + 4 + idx * 8;
	    const etype = getu16(font, entry) * 0o100 + getu16(font, entry + 2);
	    // Complete unicode map
	    if (etype == 0o004 or etype == 0o312) {
	        const table = cmap + getu32(font, entry + 4);
	        if (!is_safe_offset(font, table, 8))
                    return error.InvalidTtfBadCmapTable;
	        // Dispatch based on cmap format.
	        const format = getu16(font, table);
	        switch (format) {
		    12 => return cmap_fmt12_13(font, table, charCode, 12),
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
	    const etype = getu16(font, entry) * 0o100 + getu16(font, entry + 2);
	    // Unicode BMP
	    if (etype == 0o003 or etype == 0o301) {
	        const table = cmap + getu32(font, entry + 4);
	        if (!is_safe_offset(font, table, 6))
                    return error.InvalidTtfBadCmapTable;
	        // Dispatch based on cmap format.
		switch (getu16(font, table)) {
                    4 => return cmap_fmt4(font, table + 6, charCode),
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
fn hor_metrics(font: *Font, glyph: c.SFT_Glyph) !HorMetrics {
    var hmtx: c.uint_fast32_t = undefined;
    if (gettable(font, "hmtx", &hmtx) < 0)
        return error.InvalidTtfNoHmtxTable;

    if (glyph < font.numLongHmtx) {
	// glyph is inside long metrics segment.
	const offset = hmtx + 4 * glyph;
	if (!is_safe_offset(font, offset, 4))
            return error.InvalidTtfBadHmtxTable;
        return .{
            .advance_width = getu16(font, offset),
            .left_side_bearing = geti16(font, offset + 2),
        };
    }

    // glyph is inside short metrics segment.
    const boundary = hmtx + 4 * @intCast(c.uint_fast32_t, font.numLongHmtx);
    if (boundary < 4)
        return error.InvalidTtfBadHmtxTable;

    const width_offset = boundary - 4;
    if (!is_safe_offset(font, width_offset, 4))
        return error.InvalidTtfBadHmtxTable;
    const bearing_offset = boundary + 2 * (glyph - font.numLongHmtx);
    if (!is_safe_offset(font, bearing_offset, 2))
        return error.InvalidTtfBadHmtxTable;

    return .{
        .advance_width = getu16(font, width_offset),
	.left_side_bearing = geti16(font, bearing_offset),
    };
}

fn glyph_bbox(sft: *c.SFT, outline: c.uint_fast32_t) ![4]c_int {
    const font = Font.fromC(sft.font orelse unreachable);
    if (!is_safe_offset(font, outline, 10))
	return error.InvalidTtfBadOutline;
    const box = [4]i16{
        geti16(font, outline + 2),
        geti16(font, outline + 4),
        geti16(font, outline + 6),
        geti16(font, outline + 8),
    };
    if (box[2] <= box[0] or box[3] <= box[1])
	return error.InvalidTtfBadBbox;
    // Transform the bounding box into SFT coordinate space.
    const xScale = sft.xScale / @intToFloat(f64, font.*.unitsPerEm);
    const yScale = sft.yScale / @intToFloat(f64, font.*.unitsPerEm);
    return [_]c_int {
        @floatToInt(c_int, @floor(@intToFloat(f64, box[0]) * xScale + sft.xOffset)),
        @floatToInt(c_int, @floor(@intToFloat(f64, box[1]) * yScale + sft.yOffset)),
        @floatToInt(c_int, @ceil (@intToFloat(f64, box[2]) * xScale + sft.xOffset)),
        @floatToInt(c_int, @ceil (@intToFloat(f64, box[3]) * yScale + sft.yOffset)),
    };
}

// Returns the offset into the font that the glyph's outline is stored at.
fn outline_offset(font: *Font, glyph: c.SFT_Glyph) !c.uint_fast32_t {
    var loca: c.uint_fast32_t = undefined;
    if (gettable(font, "loca", &loca) < 0)
	return error.InvalidTtfNoLocaTable;

    var glyf: c.uint_fast32_t = undefined;
    if (gettable(font, "glyf", &glyf) < 0)
	return error.InvalidttfNoGlyfTable;

    const entry = blk: {
        if (font.locaFormat == 0) {
	    const base = loca + 2 * glyph;
	    if (!is_safe_offset(font, base, 4))
	        return error.InvalidTtfBadLocaTable;
            break :blk .{
	        .this = 2 * @intCast(u32, getu16(font, base)),
	        .next = 2 * @intCast(u32, getu16(font, base + 2)),
            };
        }

	const base = loca + 4 * glyph;
	if (!is_safe_offset(font, base, 8))
	    return error.InvalidTtfBadLocaTable;
        break :blk .{
	    .this = getu32(font, base),
	    .next = getu32(font, base + 4),
        };
    };
    return if (entry.this == entry.next) 0 else glyf + entry.this;
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
	    value = getu8(font, off);
            off += 1;
	    if ((value & ttf.repeat_flag) != 0) {
		if (!is_safe_offset(font, off, 1))
		    return error.InvalidTtfBadOutline;
		repeat = getu8(font, off);
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
	        const value = getu8(font, off);
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
	        const value = getu8(font, off);
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

	if (outl.points.items.len >= outl.points.capacity and grow_points(outl) < 0)
	    return error.InvalidTtfBadOutline;

	outl.points.items.ptr[outl.points.items.len] = midpoint(
	    outl.points.items.ptr[basePoint],
	    outl.points.items.ptr[basePoint + count - 1]);
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
		if (outl.curves.items.len >= outl.curves.capacity and grow_curves(outl) < 0)
	            return error.InvalidTtfBadOutline;
		outl.curves.items.ptr[outl.curves.items.len] = Curve{ .beg = beg, .end = cur, .ctrl = ctrl };
                outl.curves.items.len += 1;
	    } else {
		if (outl.lines.items.len >= outl.lines.capacity and grow_lines(outl) < 0)
	            return error.InvalidTtfBadOutline;
		outl.lines.items.ptr[outl.lines.items.len] = Line{ .beg = beg, .end = cur };
                outl.lines.items.len += 1;
	    }
	    beg = cur;
            opt_ctrl = null;
	} else {
	    if (opt_ctrl) |ctrl| {
		const center: c.uint_least16_t = @intCast(c.uint_least16_t, outl.points.items.len);
		if (outl.points.items.len >= outl.points.capacity and grow_points(outl) < 0)
	            return error.InvalidTtfBadOutline;
		outl.points.items.ptr[center] = midpoint(outl.points.items.ptr[ctrl], outl.points.items.ptr[cur]);
                outl.points.items.len += 1;

		if (outl.curves.items.len >= outl.curves.capacity and grow_curves(outl) < 0)
	            return error.InvalidTtfBadOutline;
		outl.curves.items.ptr[outl.curves.items.len] = Curve{ .beg = beg, .end = center, .ctrl = ctrl };
                outl.curves.items.len += 1;

		beg = center;
	    }
	    opt_ctrl = cur;
	}
    }
    if (opt_ctrl) |ctrl| {
	if (outl.curves.items.len >= outl.curves.capacity and grow_curves(outl) < 0)
	    return error.InvalidTtfBadOutline;
	outl.curves.items.ptr[outl.curves.items.len] = Curve{ .beg = beg, .end = looseEnd, .ctrl = ctrl };
        outl.curves.items.len += 1;
    } else {
	if (outl.lines.items.len >= outl.lines.capacity and grow_lines(outl) < 0)
	    return error.InvalidTtfBadOutline;
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
	if (grow_points(outl) < 0)
	    return error.InvalidTtfBadOutline;
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
        const outline = try outline_offset(font, glyph);
	if (outline != 0) {
	    const basePoint = outl.points.items.len;
	    try decode_outline(font, outline, recDepth + 1, outl);
	    transform_points(outl.points.items.ptr[basePoint .. outl.points.items.len], &local);
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
fn is_flat(outline: *Outline, curve: Curve) c_int {
    const maxArea2: f64 = 2.0;
    const a = outline.points.items.ptr[curve.beg];
    const b = outline.points.items.ptr[curve.ctrl];
    const cpoint = outline.points.items.ptr[curve.end];
    const g = Point{ .x = b.x-a.x, .y = b.y-a.y };
    const h = Point{ .x = cpoint.x-a.x, .y = cpoint.y-a.y };
    const area2 = std.math.fabs(g.x*h.y-h.x*g.y);
    return if (area2 <= maxArea2) 1 else 0;
}
fn is_flat_zig(outline: *Outline, curve: Curve) bool {
    return is_flat(outline, curve) != 0;
}

fn tesselate_curve(curve_in: Curve, outline: *Outline) c_int {
    // From my tests I can conclude that this stack barely reaches a top height
    // of 4 elements even for the largest font sizes I'm willing to support. And
    // as space requirements should only grow logarithmically, I think 10 is
    // more than enough.
    const STACK_SIZE = 10;
    var stack: [STACK_SIZE]Curve = undefined;
    var top: usize = 0;
    var curve = curve_in;
    while (true) {
	if (is_flat_zig(outline, curve) or top >= STACK_SIZE) {
	    if (outline.lines.items.len >= outline.lines.capacity and grow_lines(outline) < 0)
		return -1;
	    outline.lines.items.ptr[outline.lines.items.len] = .{ .beg = curve.beg, .end = curve.end };
            outline.lines.items.len += 1;
	    if (top == 0) break;
            top -= 1;
	    curve = stack[top];
	} else {
	    const ctrl0 = @intCast(c.uint_least16_t, outline.points.items.len);
	    if (outline.points.items.len >= outline.points.capacity and grow_points(outline) < 0)
		return -1;
	    outline.points.items.ptr[ctrl0] = midpoint(outline.points.items.ptr[curve.beg], outline.points.items.ptr[curve.ctrl]);
            outline.points.items.len += 1;

	    const ctrl1 = @intCast(c.uint_least16_t, outline.points.items.len);
	    if (outline.points.items.len >= outline.points.capacity and grow_points(outline) < 0)
		return -1;
	    outline.points.items.ptr[ctrl1] = midpoint(outline.points.items.ptr[curve.ctrl], outline.points.items.ptr[curve.end]);
            outline.points.items.len += 1;

	    const pivot = @intCast(c.uint_least16_t, outline.points.items.len);
	    if (outline.points.items.len >= outline.points.capacity and grow_points(outline) < 0)
		return -1;
	    outline.points.items.ptr[pivot] = midpoint(outline.points.items.ptr[ctrl0], outline.points.items.ptr[ctrl1]);
            outline.points.items.len += 1;

	    stack[top] = .{ .beg = curve.beg, .end = pivot, .ctrl = ctrl0 };
            top += 1;
	    curve = .{ .beg = pivot, .end = curve.end, .ctrl = ctrl1 };
	}
    }
    return 0;
}

fn tesselate_curves(outline: *Outline) c_int {
    var i: usize = 0;
    while (i < outline.curves.items.len) : (i += 1) {
        if (tesselate_curve(outline.curves.items.ptr[i], outline) < 0)
            return -1;
    }
    return 0;
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
    transform_points(outl.points.items.ptr[0 .. outl.points.items.len], transform);
    clip_points(outl.points.items.ptr[0 .. outl.points.items.len], @intToFloat(f64, image.width), @intToFloat(f64, image.height));

    if (tesselate_curves(outl) < 0)
	return error.SomethingFailed; // TODO: propagate correct error

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
    const buf = Raster {
        .cells = cells,
        .width = image.width,
        .height = image.height,
    };
    draw_lines(outl, buf);
    post_process(buf, @ptrCast([*]u8, image.pixels));
}
