const builtin = @import("builtin");
const std = @import("std");
const c = @cImport({
    @cInclude("stdlib.h");
    @cInclude("schrift.h");
    @cInclude("private.h");
});

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
    if (c.init_font(font) < 0) {
        c.sft_freefont(font);
        return null;
    }
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
    if (c.init_font(font) < 0) {
	sft_freefont(font);
	return null;
    }
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

export fn sft_lookup(sft: *const c.SFT, codepoint: c.SFT_UChar, glyph: c.SFT_Glyph) c_int {
    return c.glyph_id(sft.font, codepoint, glyph);
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

export fn grow_points(outline: *c.Outline) c_int {
    std.debug.assert(outline.capPoints > 0);
    if (outline.capPoints > std.math.maxInt(u16) / 2)
	return -1;
    const cap = outline.capPoints * 2;
    const mem = c.reallocarray(outline.points, cap, @sizeOf(@TypeOf(outline.points[0]))) orelse return -1;
    outline.capPoints = cap;
    outline.points    = @ptrCast([*]c.Point, @alignCast(@alignOf(c.Point), mem));
    return 0;
}

export fn grow_lines(outline: *c.Outline) c_int {
    std.debug.assert(outline.capLines > 0);
    if (outline.capLines > std.math.maxInt(u16) / 2)
	return -1;
    const cap = outline.capLines * 2;
    const mem = c.reallocarray(outline.lines, cap, @sizeOf(@TypeOf(outline.lines[0]))) orelse return -1;
    outline.capLines = cap;
    outline.lines    = @ptrCast([*]c.Line, @alignCast(@alignOf(c.Line), mem));
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
