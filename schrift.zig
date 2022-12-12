const builtin = @import("builtin");
const std = @import("std");
const c = @cImport({
    @cInclude("string.h");
    @cInclude("stdlib.h");
    @cInclude("math.h");
    @cInclude("schrift.h");
    @cInclude("private.h");
});

const file_magic_one = 0x00010000;
const file_magic_two = 0x74727565;

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

export fn sft_lookup(sft: *const c.SFT, codepoint: c.SFT_UChar, glyph: c.SFT_Glyph) c_int {
    return c.glyph_id(sft.font, codepoint, glyph);
}

export fn sft_gmetrics(sft: *c.SFT, glyph: c.SFT_Glyph, metrics: *c.SFT_GMetrics) c_int {
    @memset(@ptrCast([*]u8, metrics), 0, @sizeOf(@TypeOf(metrics.*)));

    var adv: c_int = undefined;
    var lsb: c_int = undefined;
    if (c.hor_metrics(sft.font, glyph, &adv, &lsb) < 0)
	return -1;
    const xScale = sft.xScale / @intToFloat(f64, sft.font.*.unitsPerEm);
    metrics.advanceWidth    = @intToFloat(f64, adv) * xScale;
    metrics.leftSideBearing = @intToFloat(f64, lsb) * xScale + sft.xOffset;

    var outline: c.uint_fast32_t = undefined;
    if (c.outline_offset(sft.font, glyph, &outline) < 0)
	return -1;
    if (outline == 0)
	return 0;
    var bbox: [4]c_int = undefined;
    if (c.glyph_bbox(sft, outline, &bbox) < 0)
	return -1;
    metrics.minWidth  = bbox[2] - bbox[0] + 1;
    metrics.minHeight = bbox[3] - bbox[1] + 1;
    metrics.yOffset   = if ((sft.flags & c.SFT_DOWNWARD_Y) != 0) -bbox[3] else bbox[1];
    return 0;
}

export fn sft_render(sft: *c.SFT, glyph: c.SFT_Glyph, image: c.SFT_Image) c_int {
    var outline: c.uint_fast32_t = undefined;
    if (c.outline_offset(sft.font, glyph, &outline) < 0)
	return -1;
    if (outline == 0)
	return 0;
    var bbox: [4]c_int = undefined;
    if (c.glyph_bbox(sft, outline, &bbox) < 0)
	return -1;
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

fn map_file(font: *c.SFT_Font, filename: [*:0]const u8) !void {
    if (builtin.os.tag == .windows) {
        @panic("todo");
//	HANDLE file;
//	DWORD high, low;
//
//	font->mapping = NULL;
//	font->memory  = NULL;
//
//	file = CreateFileA(filename, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
//	if (file == INVALID_HANDLE_VALUE) {
//		return -1;
//	}
//
//	low = GetFileSize(file, &high);
//	if (low == INVALID_FILE_SIZE) {
//		CloseHandle(file);
//		return -1;
//	}
//
//	font->size = (size_t)high << (8 * sizeof(DWORD)) | low;
//
//	font->mapping = CreateFileMapping(file, NULL, PAGE_READONLY, high, low, NULL);
//	if (!font->mapping) {
//		CloseHandle(file);
//		return -1;
//	}
//
//	CloseHandle(file);
//
//	font->memory = MapViewOfFile(font->mapping, FILE_MAP_READ, 0, 0, 0);
//	if (!font->memory) {
//		CloseHandle(font->mapping);
//		font->mapping = NULL;
//		return -1;
//	}
//
//	return 0;
    } else {
        var file = try std.fs.cwd().openFileZ(filename, .{});
        defer file.close();
        const file_size = try file.getEndPos();
        if (file_size > std.math.maxInt(u32))
            return error.FileTooBig;
        font.size = @intCast(u32, file_size);
        const mem = try std.os.mmap(null, font.size, std.os.PROT.READ, std.os.MAP.PRIVATE, file.handle, 0);
        std.debug.assert(mem.len == font.size);
        font.memory = mem.ptr;
    }
}

fn unmap_file(font: *c.SFT_Font) void {
    if (builtin.os.tag == .windows) {
        @panic("todo");
//	if (font->memory) {
//	    UnmapViewOfFile(font->memory);
//	    font->memory = NULL;
//	}
//	if (font->mapping) {
//	    CloseHandle(font->mapping);
//	    font->mapping = NULL;
//	}
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
    if (scalerType != file_magic_one and scalerType != file_magic_two)
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
    const ptr = c.reallocarray(ptr_ref.*, next_cap, @sizeOf(T)) orelse return error.OutOfMemory;
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
    base: *anyopaque,
    nmemb: usize,
    size: usize,
    compar: *const fn(?*const anyopaque, ?*const anyopaque) callconv(.C) c_int,
) ?*anyopaque {

    if (nmemb == 0) return null;

    const bytes = @ptrCast([*]u8, base);
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

//export fn getu8(font: *c.SFT_Font, offset: u32) c.uint_least8 {
//    std.debug.assert(offset + 1 <= font.size);
//    return @intCast(c.uint_least8, font.memory[offset]);
//}
//export fn geti8(font: *c.SFT_Font, offset: u32) c.int_least8 {
//    return @bitCast(c.int_least8, getU8(font, offset));
//}

fn geti16(font: *c.SFT_Font, offset: c.uint_fast32_t) i16 {
    std.debug.assert(offset + 2 <= font.size);
    return std.mem.readIntBig(i16, @ptrCast(*const [2]u8, font.memory + offset));
}
fn getu16(font: *c.SFT_Font, offset: c.uint_fast32_t) u16 {
    std.debug.assert(offset + 2 <= font.size);
    return std.mem.readIntBig(u16, @ptrCast(*const [2]u8, font.memory + offset));
}
fn getu32(font: *c.SFT_Font, offset: c.uint_fast32_t) u32 {
    std.debug.assert(offset + 4 <= font.size);
    return std.mem.readIntBig(u32, @ptrCast(*const [4]u8, font.memory + offset));
}

export fn gettable(font: *c.SFT_Font, tag: *const [4]u8, offset: *c.uint_fast32_t) c_int {
    // No need to bounds-check access to the first 12 bytes - this gets already checked by init_font().
    const numTables = getu16(font, 4);
    if (!is_safe_offset_zig(font, 12, numTables * 16))
	return -1;
    const match = c.bsearch(tag, font.memory + 12, numTables, 16, cmpu32) orelse return -1;
    offset.* = getu32(font, @ptrToInt(match) - @ptrToInt(font.memory) + 8);
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
