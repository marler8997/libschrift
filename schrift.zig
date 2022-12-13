const builtin = @import("builtin");
const std = @import("std");
const c = @cImport({
    @cInclude("string.h");
    @cInclude("stdlib.h");
    @cInclude("math.h");
    @cInclude("schrift.h");
    @cInclude("private.h");
});

const ttf = struct {
    pub const file_magic_one = 0x00010000;
    pub const file_magic_two = 0x74727565;

    pub const x_change_is_small     = 0x02;
    pub const y_change_is_small     = 0x04;
    pub const repeat_flag           = 0x08;
    pub const x_change_is_zero      = 0x10;
    pub const x_change_is_positive  = 0x10;
    pub const y_change_is_zero      = 0x20;
    pub const y_change_is_positive  = 0x20;
};

export fn sft_version() [*:0]const u8 {
    return "0.10.2";
}

fn allocFont() ?*c.SFT_Font {
    const ptr = c.calloc(1, @sizeOf(c.SFT_Font)) orelse return null;
    return @ptrCast(*c.SFT_Font, @alignCast(@alignOf(c.SFT_Font), ptr));
}

// Loads a font from a user-supplied memory range.
export fn sft_loadmem(mem: [*]u8, size: usize) ?*c.SFT_Font {
    if (size > std.math.maxInt(u32)) return null;

    const font = allocFont() orelse return null;
    font.memory = mem;
    font.size = @intCast(u32, size);
    font.source = c.SrcUser;
    init_font(font) catch {
        c.sft_freefont(font);
        return null;
    };
    return font;
}

// Loads a font from the file system. To do so, it has to map the entire font into memory.
export fn sft_loadfile(filename: [*:0]const u8) ?*c.SFT_Font {
    const font = allocFont() orelse return null;
    map_file(font, filename) catch |err| {
        std.log.err("map file '{s}' failed with {s}", .{filename, @errorName(err)});
	c.free(font);
	return null;
    };
    init_font(font) catch {
	sft_freefont(font);
	return null;
    };
    return font;
}

export fn sft_freefont(font: ?*c.SFT_Font) void {
    const f = font orelse return;
    // Only unmap if we mapped it ourselves.
    if (f.source == c.SrcMapping) {
        unmap_file(f);
    }
    c.free(f);
}

export fn sft_lmetrics(sft: *const c.SFT, metrics: *c.SFT_LMetrics) c_int {
    @memset(@ptrCast([*]u8, metrics), 0, @sizeOf(@TypeOf(metrics.*)));
    var hhea: c.uint_fast32_t = undefined;
    if (c.gettable(sft.font, @ptrCast([*c]const u8, "hhea"), &hhea) < 0)
	return -1;
    if (!is_safe_offset_zig(sft.font, hhea, 36))
	return -1;
    const factor = sft.yScale / @intToFloat(f64, sft.font.*.unitsPerEm);
    metrics.ascender  = @intToFloat(f64, geti16(sft.font, hhea + 4)) * factor;
    metrics.descender = @intToFloat(f64, geti16(sft.font, hhea + 6)) * factor;
    metrics.lineGap   = @intToFloat(f64, geti16(sft.font, hhea + 8)) * factor;
    return 0;
}

export fn sft_lookup(sft: *const c.SFT, codepoint: c.SFT_UChar, glyph: *c.SFT_Glyph) c_int {
    if (glyph_id(sft.font, codepoint)) |g| {
        glyph.* = g;
        return 0;
    } else |_| {
        glyph.* = 0;
        return -1;
    }
}

export fn sft_gmetrics(sft: *c.SFT, glyph: c.SFT_Glyph, metrics: *c.SFT_GMetrics) c_int {
    @memset(@ptrCast([*]u8, metrics), 0, @sizeOf(@TypeOf(metrics.*)));

    const hor = hor_metrics(sft.font, glyph) catch return -1;
    const xScale = sft.xScale / @intToFloat(f64, sft.font.*.unitsPerEm);
    metrics.advanceWidth    = @intToFloat(f64, hor.advance_width) * xScale;
    metrics.leftSideBearing = @intToFloat(f64, hor.left_side_bearing) * xScale + sft.xOffset;

    const outline = outline_offset_zig(sft.font, glyph) catch return -1;
    if (outline == 0)
	return 0;
    const bbox = glyph_bbox(sft, outline) catch return -1;
    metrics.minWidth  = bbox[2] - bbox[0] + 1;
    metrics.minHeight = bbox[3] - bbox[1] + 1;
    metrics.yOffset   = if ((sft.flags & c.SFT_DOWNWARD_Y) != 0) -bbox[3] else bbox[1];
    return 0;
}

export fn sft_render(sft: *c.SFT, glyph: c.SFT_Glyph, image: c.SFT_Image) c_int {
    const outline = outline_offset_zig(sft.font, glyph) catch return -1;
    if (outline == 0)
	return 0;
    const bbox = glyph_bbox(sft, outline) catch return -1;
    // Set up the transformation matrix such that
    // the transformed bounding boxes min corner lines
    // up with the (0, 0) point.
    var transform: [6]f64 = undefined;
    transform[0] = sft.xScale / @intToFloat(f64, sft.font.*.unitsPerEm);
    transform[1] = 0.0;
    transform[2] = 0.0;
    transform[4] = sft.xOffset - @intToFloat(f64, bbox[0]);
    if ((sft.flags & c.SFT_DOWNWARD_Y) != 0) {
	transform[3] = -sft.yScale / @intToFloat(f64, sft.font.*.unitsPerEm);
	transform[5] = @intToFloat(f64, bbox[3]) - sft.yOffset;
    } else {
	transform[3] = sft.yScale / @intToFloat(f64, sft.font.*.unitsPerEm);
	transform[5] = sft.yOffset - @intToFloat(f64, bbox[1]);
    }

    var outl = std.mem.zeroes(c.Outline);
    defer free_outline(&outl);
    init_outline(&outl) catch return -1;

    if (c.decode_outline(sft.font, outline, 0, &outl) < 0)
        return -1;

    if (c.render_outline(&outl, &transform, image) < 0)
        return -1;

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

fn map_file(font: *c.SFT_Font, filename: [*:0]const u8) !void {
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

fn unmap_file(font: *c.SFT_Font) void {
    if (builtin.os.tag == .windows) {
        std.debug.assert(0 != win32.UnmapViewOfFile(font.memory));
        std.os.windows.CloseHandle(font.mapping.?);
    } else {
	//std.debug.assert(font.memory != std.os.MAP.FAILED);
	std.os.munmap(@alignCast(std.mem.page_size, font.memory)[0 .. font.size]);
    }
}

fn init_font(font: *c.SFT_Font) !void {
    if (!is_safe_offset_zig(font, 0, 12))
        return error.InvalidTtfTooSmall;

    // Check for a compatible scalerType (magic number).
    const scalerType = getu32(font, 0);
    if (scalerType != ttf.file_magic_one and scalerType != ttf.file_magic_two)
	return error.InvalidTtfBadMagic;

    var head: c.uint_fast32_t = undefined;
    if (c.gettable(font, "head", &head) < 0)
	return error.InvalidTtfNoHeadTable;
    if (!is_safe_offset_zig(font, head, 54))
        return error.InvalidTtfBadHeadTable;
    font.unitsPerEm = getu16(font, head + 18);
    font.locaFormat = geti16(font, head + 50);
    var hhea: c.uint_fast32_t = undefined;
    if (c.gettable(font, "hhea", &hhea) < 0)
        return error.InvalidTtfNoHheaTable;
    if (!is_safe_offset_zig(font, hhea, 36))
        return error.InvalidTtfBadHheaTable;
    font.numLongHmtx = getu16(font, hhea + 34);
}

export fn midpoint(a: c.Point, b: c.Point) c.Point {
    return .{
	.x = 0.5 * (a.x + b.x),
	.y = 0.5 * (a.y + b.y),
    };
}

// Applies an affine linear transformation matrix to a set of points.
export fn transform_points(numPts: c_uint, points: [*]c.Point, trf: *const [6]f64) void {
    var i: c_uint = 0;
    while (i < numPts) : (i += 1) {
	const pt = points[i];
	points[i] = .{
	    .x = pt.x * trf[0] + pt.y * trf[2] + trf[4],
	    .y = pt.x * trf[1] + pt.y * trf[3] + trf[5],
	};
    }
}

export fn clip_points(numPts: c_uint, points: [*]c.Point, width: f64, height: f64) void {
    var i: c_uint = 0;
    while (i < numPts) : (i += 1) {
	const pt = points[i];
	if (pt.x < 0.0) {
	    points[i].x = 0.0;
	}
	if (pt.x >= width) {
	    points[i].x = c.nextafter(width, 0.0);
	}
	if (pt.y < 0.0) {
	    points[i].y = 0.0;
	}
	if (pt.y >= height) {
	    points[i].y = c.nextafter(height, 0.0);
	}
    }
}

fn malloc(comptime T: type, count: usize) error{OutOfMemory}![*]T {
    const ptr = c.malloc(count * @sizeOf(T)) orelse return error.OutOfMemory;
    return @ptrCast([*]T, @alignCast(@alignOf(T), ptr));
}

fn init_outline(outl: *c.Outline) error{OutOfMemory}!void {
    // TODO Smaller initial allocations
    outl.numPoints = 0;
    outl.capPoints = 64;
    outl.points = try malloc(c.Point, outl.capPoints);
    outl.numCurves = 0;
    outl.capCurves = 64;
    outl.curves = try malloc(c.Curve, outl.capCurves);
    outl.numLines = 0;
    outl.capLines = 64;
    outl.lines = try malloc(c.Line, outl.capLines);
}

fn free_outline(outl: *c.Outline) void {
    c.free(outl.points);
    c.free(outl.curves);
    c.free(outl.lines);
}

fn grow(comptime T: type, cap_ref: *c.uint_least16_t, ptr_ref: *[*c]T) error{OutOfMemory,TooManyPrimitives}!void {
    std.debug.assert(cap_ref.* > 0);
    if (cap_ref.* > std.math.maxInt(u16) / 2)
	return error.TooManyPrimitives;
    const next_cap = cap_ref.* * 2;
    const ptr = c.realloc(ptr_ref.*, next_cap * @sizeOf(T)) orelse return error.OutOfMemory;
    cap_ref.* = next_cap;
    ptr_ref.* = @ptrCast([*c]T, @alignCast(@alignOf(T), ptr));
}

export fn grow_points(outline: *c.Outline) c_int {
    grow(c.Point, &outline.capPoints, &outline.points) catch return -1;
    return 0;
}

export fn grow_curves(outline: *c.Outline) c_int {
    grow(c.Curve, &outline.capCurves, &outline.curves) catch return -1;
    return 0;
}

export fn grow_lines(outline: *c.Outline) c_int {
    grow(c.Line, &outline.capLines, &outline.lines) catch return -1;
    return 0;
}

export fn is_safe_offset_zig(font: *c.SFT_Font, offset: c.uint_fast32_t, margin: u32) bool {
    return if (0 == is_safe_offset(font, offset, margin)) false else true;
}

export fn is_safe_offset(font: *c.SFT_Font, offset: c.uint_fast32_t, margin: u32) c_int {
    if (offset > font.size) return 0;
    if (font.size - offset < margin) return 0;
    return 1;
}

// Like bsearch(), but returns the next highest element if key could not be found.
export fn csearch(
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

fn getu8(font: *c.SFT_Font, offset: usize) u8 {
    std.debug.assert(offset + 1 <= font.size);
    return font.memory[offset];
}
//export fn geti8(font: *c.SFT_Font, offset: u32) c.int_least8 {
//    return @bitCast(c.int_least8, getU8(font, offset));
//}

fn geti16(font: *c.SFT_Font, offset: usize) i16 {
    std.debug.assert(offset + 2 <= font.size);
    return std.mem.readIntBig(i16, @ptrCast(*const [2]u8, font.memory + offset));
}
fn getu16(font: *c.SFT_Font, offset: usize) u16 {
    std.debug.assert(offset + 2 <= font.size);
    return std.mem.readIntBig(u16, @ptrCast(*const [2]u8, font.memory + offset));
}
fn getu32(font: *c.SFT_Font, offset: usize) u32 {
    std.debug.assert(offset + 4 <= font.size);
    return std.mem.readIntBig(u32, @ptrCast(*const [4]u8, font.memory + offset));
}

export fn gettable(font: *c.SFT_Font, tag: *const [4]u8, offset: *c.uint_fast32_t) c_int {
    // No need to bounds-check access to the first 12 bytes - this gets already checked by init_font().
    const numTables = getu16(font, 4);
    if (!is_safe_offset_zig(font, 12, numTables * 16))
	return -1;
    const match = c.bsearch(tag, font.memory + 12, numTables, 16, cmpu32) orelse return -1;
    offset.* = getu32(font, @intCast(c.uint_fast32_t, @ptrToInt(match) - @ptrToInt(font.memory) + 8));
    return 0;
}

fn cmap_fmt4(font: *c.SFT_Font, table: c.uint_fast32_t, charCode: c.SFT_UChar) !c.SFT_Glyph {
    // cmap format 4 only supports the Unicode BMP.
    if (charCode > 0xFFFF)
	return 0;

    const shortCode = @intCast(c.uint_fast16_t, charCode);
    if (!is_safe_offset_zig(font, table, 8))
        return error.InvalidTtfBadCmapTable;
    const segCountX2 = getu16(font, table);
    if (((segCountX2 & 1) != 0) or (0 == segCountX2))
        return error.InvalidTtfBadCmapTable;

    // Find starting positions of the relevant arrays.
    const endCodes       = table + 8;
    const startCodes     = endCodes + segCountX2 + 2;
    const idDeltas       = startCodes + segCountX2;
    const idRangeOffsets = idDeltas + segCountX2;
    if (!is_safe_offset_zig(font, idRangeOffsets, segCountX2))
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
    if (!is_safe_offset_zig(font, idOffset, 2))
        return error.InvalidTtfBadCmapTable;
    const id = getu16(font, idOffset);
    // Intentional integer under- and overflow.
    return if (id == 0) 0 else @intCast(c.SFT_Glyph, ((@intCast(u32, id) + @intCast(u32, idDelta)) & 0xFFFF));
}

fn cmap_fmt12_13(font: *c.SFT_Font, table: c.uint_fast32_t, charCode: c.SFT_UChar, which: c_int) !c.SFT_Glyph {
    // check that the entire header is present
    if (!is_safe_offset_zig(font, table, 16))
        return error.InvalidTtfBadCmapTable;

    const len = getu32(font, table + 4);
    // A minimal header is 16 bytes
    if (len < 16)
        return error.InvalidTtfBadCmapTable;
    if (!is_safe_offset_zig(font, table, len))
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
fn glyph_id(font: *c.SFT_Font, charCode: c.SFT_UChar) !c.SFT_Glyph {
    var cmap: c.uint_fast32_t = undefined;
    if (gettable(font, "cmap", &cmap) < 0)
        return error.InvalidTtfNoCmapTable;

    if (!is_safe_offset_zig(font, cmap, 4))
        return error.InvalidTtfBadCmapTable;
    const numEntries: u32 = getu16(font, cmap + 2);

    if (!is_safe_offset_zig(font, cmap, 4 + numEntries * 8))
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
	        if (!is_safe_offset_zig(font, table, 8))
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
	        if (!is_safe_offset_zig(font, table, 6))
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
fn hor_metrics(font: *c.SFT_Font, glyph: c.SFT_Glyph) !HorMetrics {
    var hmtx: c.uint_fast32_t = undefined;
    if (gettable(font, "hmtx", &hmtx) < 0)
        return error.InvalidTtfNoHmtxTable;

    if (glyph < font.numLongHmtx) {
	// glyph is inside long metrics segment.
	const offset = hmtx + 4 * glyph;
	if (!is_safe_offset_zig(font, offset, 4))
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
    if (!is_safe_offset_zig(font, width_offset, 4))
        return error.InvalidTtfBadHmtxTable;
    const bearing_offset = boundary + 2 * (glyph - font.numLongHmtx);
    if (!is_safe_offset_zig(font, bearing_offset, 2))
        return error.InvalidTtfBadHmtxTable;

    return .{
        .advance_width = getu16(font, width_offset),
	.left_side_bearing = geti16(font, bearing_offset),
    };
}

fn glyph_bbox(sft: *c.SFT, outline: c.uint_fast32_t) ![4]c_int {
    if (!is_safe_offset_zig(sft.font, outline, 10))
	return error.InvalidTtfBadOutline;
    const box = [4]i16{
        geti16(sft.font, outline + 2),
        geti16(sft.font, outline + 4),
        geti16(sft.font, outline + 6),
        geti16(sft.font, outline + 8),
    };
    if (box[2] <= box[0] or box[3] <= box[1])
	return error.InvalidTtfBadBbox;
    // Transform the bounding box into SFT coordinate space.
    const xScale = sft.xScale / @intToFloat(f64, sft.font.*.unitsPerEm);
    const yScale = sft.yScale / @intToFloat(f64, sft.font.*.unitsPerEm);
    return [_]c_int {
        @floatToInt(c_int, @floor(@intToFloat(f64, box[0]) * xScale + sft.xOffset)),
        @floatToInt(c_int, @floor(@intToFloat(f64, box[1]) * yScale + sft.yOffset)),
        @floatToInt(c_int, @ceil (@intToFloat(f64, box[2]) * xScale + sft.xOffset)),
        @floatToInt(c_int, @ceil (@intToFloat(f64, box[3]) * yScale + sft.yOffset)),
    };
}

export fn outline_offset(font: *c.SFT_Font, glyph: c.SFT_Glyph, offset: *c.uint_fast32_t) c_int {
    if (outline_offset_zig(font, glyph)) |o| {
        offset.* = o;
        return 0;
    } else |_| return -1;
}

// Returns the offset into the font that the glyph's outline is stored at.
fn outline_offset_zig(font: *c.SFT_Font, glyph: c.SFT_Glyph) !c.uint_fast32_t {
    var loca: c.uint_fast32_t = undefined;
    if (gettable(font, "loca", &loca) < 0)
	return error.InvalidTtfNoLocaTable;

    var glyf: c.uint_fast32_t = undefined;
    if (gettable(font, "glyf", &glyf) < 0)
	return error.InvalidttfNoGlyfTable;

    const entry = blk: {
        if (font.locaFormat == 0) {
	    const base = loca + 2 * glyph;
	    if (!is_safe_offset_zig(font, base, 4))
	        return error.InvalidTtfBadLocaTable;
            break :blk .{
	        .this = 2 * @intCast(u32, getu16(font, base)),
	        .next = 2 * @intCast(u32, getu16(font, base + 2)),
            };
        }

	const base = loca + 4 * glyph;
	if (!is_safe_offset_zig(font, base, 8))
	    return error.InvalidTtfBadLocaTable;
        break :blk .{
	    .this = getu32(font, base),
	    .next = getu32(font, base + 4),
        };
    };
    return if (entry.this == entry.next) 0 else glyf + entry.this;
}

// For a 'simple' outline, determines each point of the outline with a set of flags.
export fn simple_flags(font: *c.SFT_Font, offset_ref: *c.uint_fast32_t, numPts: c.uint_fast16_t, flags: [*]u8) c_int {
    var off = offset_ref.*;
    var repeat: u8 = 0;
    var value: u8 = 0;
    var point_index: c.uint_fast16_t = 0;
    while (point_index < numPts) : (point_index += 1) {
	if (repeat != 0) {
            repeat -= 1;
	} else {
	    if (!is_safe_offset_zig(font, off, 1))
		return -1;
	    value = getu8(font, off);
            off += 1;
	    if ((value & ttf.repeat_flag) != 0) {
		if (!is_safe_offset_zig(font, off, 1))
		    return -1;
		repeat = getu8(font, off);
                off += 1;
	    }
	}
	flags[point_index] = value;
    }
    offset_ref.* = off;
    return 0;
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
export fn simple_points(
    font: *c.SFT_Font,
    offset: c.uint_fast32_t,
    numPts: c.uint_fast16_t,
    flags: [*]u8,
    points: [*]c.Point,
) c_int {
    var off = offset;
    const Accum = i32;
    {
        var accum: Accum = 0;
        var i: c.uint_fast16_t = 0;
        while (i < numPts) : (i += 1) {
	    if ((flags[i] & ttf.x_change_is_small) != 0) {
	        if (!is_safe_offset_zig(font, off, 1))
		    return -1;
	        const value = getu8(font, off);
                off += 1;
	        const is_pos = (flags[i] & ttf.x_change_is_positive) != 0;
                accum -= resolveSign(Accum, is_pos, value);
	    } else if (0 == (flags[i] & ttf.x_change_is_zero)) {
	        if (!is_safe_offset_zig(font, off, 2))
		    return -1;
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
	        if (!is_safe_offset_zig(font, off, 1))
		    return -1;
	        const value = getu8(font, off);
                off += 1;
	        const is_pos = (flags[i] & ttf.y_change_is_positive) != 0;
                accum -= resolveSign(Accum, is_pos, value);
	    } else if (0 == (flags[i] & ttf.y_change_is_zero)) {
	        if (!is_safe_offset_zig(font, off, 2))
		    return -1;
	        accum += geti16(font, off);
	        off += 2;
	    }
	    points[i].y = @intToFloat(f64, accum);
        }
    }

    return 0;
}

// A heuristic to tell whether a given curve can be approximated closely enough by a line.
fn is_flat(outline: *c.Outline, curve: c.Curve) c_int {
    const maxArea2: f64 = 2.0;
    const a = outline.points[curve.beg];
    const b = outline.points[curve.ctrl];
    const cpoint = outline.points[curve.end];
    const g = c.Point{ .x = b.x-a.x, .y = b.y-a.y };
    const h = c.Point{ .x = cpoint.x-a.x, .y = cpoint.y-a.y };
    const area2 = std.math.fabs(g.x*h.y-h.x*g.y);
    return if (area2 <= maxArea2) 1 else 0;
}
fn is_flat_zig(outline: *c.Outline, curve: c.Curve) bool {
    return is_flat(outline, curve) != 0;
}

fn tesselate_curve(curve_in: c.Curve, outline: *c.Outline) c_int {
    // From my tests I can conclude that this stack barely reaches a top height
    // of 4 elements even for the largest font sizes I'm willing to support. And
    // as space requirements should only grow logarithmically, I think 10 is
    // more than enough.
    const STACK_SIZE = 10;
    var stack: [STACK_SIZE]c.Curve = undefined;
    var top: usize = 0;
    var curve = curve_in;
    while (true) {
	if (is_flat_zig(outline, curve) or top >= STACK_SIZE) {
	    if (outline.numLines >= outline.capLines and grow_lines(outline) < 0)
		return -1;
	    outline.lines[outline.numLines] = .{ .beg = curve.beg, .end = curve.end };
            outline.numLines += 1;
	    if (top == 0) break;
            top -= 1;
	    curve = stack[top];
	} else {
	    const ctrl0 = outline.numPoints;
	    if (outline.numPoints >= outline.capPoints and c.grow_points(outline) < 0)
		return -1;
	    outline.points[ctrl0] = c.midpoint(outline.points[curve.beg], outline.points[curve.ctrl]);
            outline.numPoints += 1;

	    const ctrl1 = outline.numPoints;
	    if (outline.numPoints >= outline.capPoints and c.grow_points(outline) < 0)
		return -1;
	    outline.points[ctrl1] = c.midpoint(outline.points[curve.ctrl], outline.points[curve.end]);
            outline.numPoints += 1;

	    const pivot = outline.numPoints;
	    if (outline.numPoints >= outline.capPoints and c.grow_points(outline) < 0)
		return -1;
	    outline.points[pivot] = c.midpoint(outline.points[ctrl0], outline.points[ctrl1]);
            outline.numPoints += 1;

	    stack[top] = .{ .beg = curve.beg, .end = pivot, .ctrl = ctrl0 };
            top += 1;
	    curve = .{ .beg = pivot, .end = curve.end, .ctrl = ctrl1 };
	}
    }
    return 0;
}

export fn tesselate_curves(outline: *c.Outline) c_int {
    var i: usize = 0;
    while (i < outline.numCurves) : (i += 1) {
        if (tesselate_curve(outline.curves[i], outline) < 0)
            return -1;
    }
    return 0;
}


export fn draw_lines(outline: *c.Outline, buf: c.Raster) void {
    var i: usize = 0;
    while (i < outline.numLines) : (i += 1) {
        const line = outline.lines[i];
        const origin = outline.points[line.beg];
        const goal = outline.points[line.end];
        c.draw_line(buf, origin, goal);
    }
}

// Integrate the values in the buffer to arrive at the final grayscale image.
export fn post_process(buf: c.Raster, image: [*]u8) void {
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
