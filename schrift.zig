const builtin = @import("builtin");
const std = @import("std");

pub const TtfInfo = struct {
    unitsPerEm: u16,
    locaFormat: i16,
    numLongHmtx: u16,
};

const Point = struct { x: f64, y: f64 };
const Line = struct {
    beg: u16,
    end: u16,
};
const Curve = struct {
    beg: u16,
    end: u16,
    ctrl: u16,
};

const Outline = struct {
    points: std.ArrayListUnmanaged(Point) = .{},
    curves: std.ArrayListUnmanaged(Curve) = .{},
    lines: std.ArrayListUnmanaged(Line) = .{},
    // TODO: original C implementation used an initial size of 64,
    //       now the initial size will be 8.
    pub fn appendPoint(self: *Outline, allocator: std.mem.Allocator, p: Point) !void {
        try self.points.append(allocator, p);
    }
    pub fn appendCurve(self: *Outline, allocator: std.mem.Allocator, cv: Curve) !void {
        try self.curves.append(allocator, cv);
    }
    pub fn appendLine(self: *Outline, allocator: std.mem.Allocator, l: Line) !void {
        try self.lines.append(allocator, l);
    }
    pub fn deinit(self: *Outline, allocator: std.mem.Allocator) void {
        self.points.deinit(allocator);
        self.curves.deinit(allocator);
        self.lines.deinit(allocator);
    }
};

const Cell = struct {
    area: f64,
    cover: f64,
};
const Raster = struct {
    cells: []Cell,
    size: XY(i32),
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

pub const LMetrics = struct {
    ascender: f64,
    descender: f64,
    lineGap: f64,
};
pub fn lmetrics(ttf_mem: []const u8, info: TtfInfo, yScale: f64) !LMetrics {
    const hhea: usize = (try getTable(ttf_mem, "hhea")) orelse
        return error.TtfNoHheaTable;
    const hhea_limit = hhea + 10;
    if (hhea_limit > ttf_mem.len)
        return error.TtfBadHheaTable;
    const factor = yScale / @intToFloat(f64, info.unitsPerEm);
    return LMetrics{
        .ascender = factor * @intToFloat(f64, std.mem.readIntBig(i16, ttf_mem[hhea + 4 ..][0..2])),
        .descender = factor * @intToFloat(f64, std.mem.readIntBig(i16, ttf_mem[hhea + 6 ..][0..2])),
        .lineGap = factor * @intToFloat(f64, std.mem.readIntBig(i16, ttf_mem[hhea + 8 ..][0..2])),
    };
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
    const hor = try horMetrics(ttf_mem, info, glyph);
    const xScaleEm = scale.x / @intToFloat(f64, info.unitsPerEm);

    const advanceWidth = @intToFloat(f64, hor.advance_width) * xScaleEm;
    const leftSideBearing = @intToFloat(f64, hor.left_side_bearing) * xScaleEm + offset.x;

    const outline = (try getOutlineOffset(ttf_mem, info, glyph)) orelse return GMetrics{
        .advanceWidth = advanceWidth,
        .leftSideBearing = leftSideBearing,
        .minWidth = 0,
        .minHeight = 0,
        .yOffset = 0,
    };
    const bbox = try getGlyphBbox(ttf_mem, info, scale, offset, outline);
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

pub fn render(
    allocator: std.mem.Allocator,
    ttf_mem: []const u8,
    info: TtfInfo,
    downward: bool,
    scale: XY(f64),
    offset: XY(f64),
    out_pixels: []u8,
    out_size: XY(i32),
    glyph: u32,
) !void {
    std.debug.assert(out_size.x >= 0);
    std.debug.assert(out_size.y >= 0);
    std.debug.assert(out_pixels.len == @intCast(usize, out_size.x) * @intCast(usize, out_size.y));

    const outline_offset = (try getOutlineOffset(ttf_mem, info, glyph)) orelse return;
    const bbox = try getGlyphBbox(ttf_mem, info, scale, offset, outline_offset);

    // Set up the transformation matrix such that
    // the transformed bounding boxes min corner lines
    // up with the (0, 0) point.
    var transform: [6]f64 = undefined;
    transform[0] = scale.x / @intToFloat(f64, info.unitsPerEm);
    transform[1] = 0.0;
    transform[2] = 0.0;
    transform[4] = offset.x - bbox.x_min;
    if (downward) {
        transform[3] = -scale.y / @intToFloat(f64, info.unitsPerEm);
        transform[5] = bbox.y_max - offset.y;
    } else {
        transform[3] = scale.y / @intToFloat(f64, info.unitsPerEm);
        transform[5] = offset.y - bbox.y_min;
    }

    var outl = Outline{};
    defer outl.deinit(allocator);

    try decodeOutline(allocator, ttf_mem, info, outline_offset, 0, &outl);
    try renderOutline(allocator, &outl, &transform, out_pixels.ptr, out_size);
}

fn readTtf(comptime T: type, ttf_mem: []const u8) T {
    std.debug.assert(ttf_mem.len >= @sizeOf(T));
    return std.mem.readIntBig(T, ttf_mem[0..@sizeOf(T)]);
}

pub fn getTtfInfo(ttf_mem: []const u8) !TtfInfo {
    if (ttf_mem.len < 12) return error.TtfTooSmall;

    // Check for a compatible scalerType (magic number).
    const scalerType = readTtf(u32, ttf_mem);
    if (scalerType != ttf.file_magic_one and scalerType != ttf.file_magic_two)
        return error.TtfBadMagic;
    const head: usize = (try getTable(ttf_mem, "head")) orelse
        return error.TtfNoHeadTable;
    const head_limit = head + 52;
    if (head_limit > ttf_mem.len)
        return error.TtfBadHeadTable;
    const hhea: usize = (try getTable(ttf_mem, "hhea")) orelse
        return error.TtfNoHheaTable;
    const hhea_limit = hhea + 36;
    if (hhea_limit > ttf_mem.len)
        return error.TtfBadHheaTable;
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
fn transformPoints(points: []Point, trf: *const [6]f64) void {
    for (points) |*pt_ref| {
        const pt = pt_ref.*;
        pt_ref.* = .{
            .x = pt.x * trf[0] + pt.y * trf[2] + trf[4],
            .y = pt.x * trf[1] + pt.y * trf[3] + trf[5],
        };
    }
}

fn clipPoints(points: []Point, width: f64, height: f64) void {
    for (points) |*pt| {
        if (pt.x < 0.0) {
            pt.x = 0.0;
        } else if (pt.x >= width) {
            pt.x = nextafter(width, 0.0);
        }
        if (pt.y < 0.0) {
            pt.y = 0.0;
        } else if (pt.y >= height) {
            pt.y = nextafter(height, 0.0);
        }
    }
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

fn getTable(ttf_mem: []const u8, tag: *const [4]u8) !?u32 {
    // No need to bounds-check access to the first 12 bytes - this gets already checked by init_font().
    const numTables = readTtf(u16, ttf_mem[4..]);
    const limit = 12 + @intCast(usize, numTables) * 16;
    if (limit > ttf_mem.len)
        return error.TtfBadTables;
    const match_ptr = bsearch(tag, ttf_mem.ptr + 12, numTables, 16, cmpu32) orelse return null;
    const match_offset = @ptrToInt(match_ptr) - @ptrToInt(ttf_mem.ptr);
    return readTtf(u32, ttf_mem[match_offset + 8 ..]);
}

fn cmapFmt4(ttf_mem: []const u8, table: usize, charCode: u32) !u32 {
    // cmap format 4 only supports the Unicode BMP.
    if (charCode > 0xFFFF)
        return 0;

    if (table + 8 > ttf_mem.len)
        return error.TtfBadCmapTable;
    const segCountX2 = readTtf(u16, ttf_mem[table..]);
    if (((segCountX2 & 1) != 0) or (0 == segCountX2))
        return error.TtfBadCmapTable;

    // Find starting positions of the relevant arrays.
    const endCodes = table + 8;
    const startCodes = endCodes + segCountX2 + 2;
    const idDeltas = startCodes + segCountX2;
    const idRangeOffsets = idDeltas + segCountX2;
    if (idRangeOffsets + segCountX2 > ttf_mem.len)
        return error.TtfBadCmapTable;

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
    const idOffset = idRangeOffsets + segIdxX2 + idRangeOffset + 2 * (charCode - startCode);
    if (idOffset + 2 > ttf_mem.len)
        return error.TtfBadCmapTable;
    const id: u32 = readTtf(u16, ttf_mem[idOffset..]);
    // Intentional integer under- and overflow.
    return if (id == 0) 0 else ((id + idDelta) & 0xFFFF);
}

fn cmapFmt12_13(table: []const u8, charCode: u32, which: c_int) !u32 {
    if (table.len < 16)
        return error.TtfBadCmapTable;
    const numEntries = @intCast(usize, readTtf(u32, table[12..]));
    if (16 + 12 * numEntries > table.len)
        return error.TtfBadCmapTable;
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
pub fn lookupGlyph(ttf_mem: []const u8, charCode: u32) !u32 {
    const cmap: usize = (try getTable(ttf_mem, "cmap")) orelse
        return error.TtfNoCmapTable;
    const cmap_limit = cmap + 4;
    if (cmap_limit > ttf_mem.len)
        return error.TtfBadCmapTable;
    const numEntries: usize = readTtf(u16, ttf_mem[cmap + 2 ..]);

    const entries_limit = 4 + numEntries * 8;
    if (entries_limit > ttf_mem.len)
        return error.TtfBadCmapTable;

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
                    return error.TtfBadCmapTable;
                switch (readTtf(u16, ttf_mem[table..])) {
                    12 => return cmapFmt12_13(ttf_mem[table..], charCode, 12),
                    else => return error.TtfUnsupportedCmapFormat,
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
                    return error.TtfBadCmapTable;
                // Dispatch based on cmap format.
                switch (readTtf(u16, ttf_mem[table..])) {
                    4 => return cmapFmt4(ttf_mem, table + 6, charCode),
                    //6 => return cmap_fmt6(font, table + 6, charCode, glyph),
                    6 => @panic("todo"),
                    else => return error.TtfUnsupportedCmapFormat,
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
fn horMetrics(ttf_mem: []const u8, info: TtfInfo, glyph: u32) !HorMetrics {
    const hmtx: usize = (try getTable(ttf_mem, "hmtx")) orelse
        return error.TtfNoHmtxTable;

    if (glyph < info.numLongHmtx) {
        // glyph is inside long metrics segment.
        const offset = hmtx + 4 * glyph;
        if (offset + 4 > ttf_mem.len)
            return error.TtfBadHmtxTable;
        return .{
            .advance_width = readTtf(u16, ttf_mem[offset + 0 ..]),
            .left_side_bearing = readTtf(i16, ttf_mem[offset + 2 ..]),
        };
    }

    // glyph is inside short metrics segment.
    const boundary = hmtx + 4 * @intCast(usize, info.numLongHmtx);
    if (boundary < 4)
        return error.TtfBadHmtxTable;

    const width_offset = boundary - 4;
    if (width_offset + 2 > ttf_mem.len)
        return error.TtfBadHmtxTable;
    const bearing_offset = boundary + 2 * @intCast(usize, glyph - info.numLongHmtx);
    if (bearing_offset + 2 > ttf_mem.len)
        return error.TtfBadHmtxTable;
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
fn getGlyphBbox(ttf_mem: []const u8, info: TtfInfo, scale: XY(f64), offset: XY(f64), outline: usize) !Bbox {
    if (outline + 10 > ttf_mem.len)
        return error.TtfBadOutline;
    const box = [4]i16{
        readTtf(i16, ttf_mem[outline + 2 ..]),
        readTtf(i16, ttf_mem[outline + 4 ..]),
        readTtf(i16, ttf_mem[outline + 6 ..]),
        readTtf(i16, ttf_mem[outline + 8 ..]),
    };
    if (box[2] <= box[0] or box[3] <= box[1])
        return error.TtfBadBbox;
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
fn getOutlineOffset(ttf_mem: []const u8, info: TtfInfo, glyph: u32) !?usize {
    const loca: usize = (try getTable(ttf_mem, "loca")) orelse
        return error.TtfNoLocaTable;
    const glyf: usize = (try getTable(ttf_mem, "glyf")) orelse
        return error.TtfNoGlyfTable;

    const entry = blk: {
        if (info.locaFormat == 0) {
            const base = loca + 2 * glyph;
            if (base + 4 > ttf_mem.len)
                return error.TtfBadLocaTable;
            break :blk .{
                .this = 2 * @as(u32, readTtf(u16, ttf_mem[base + 0 ..])),
                .next = 2 * @as(u32, readTtf(u16, ttf_mem[base + 2 ..])),
            };
        }

        const base = loca + 4 * glyph;
        if (base + 8 > ttf_mem.len)
            return error.TtfBadLocaTable;
        break :blk .{
            .this = readTtf(u32, ttf_mem[base + 0 ..]),
            .next = readTtf(u32, ttf_mem[base + 4 ..]),
        };
    };
    return if (entry.this == entry.next) null else glyf + entry.this;
}

// For a 'simple' outline, determines each point of the outline with a set of flags.
fn simpleFlags(ttf_mem: []const u8, offset: usize, numPts: u16, flags: []u8) !usize {
    var off = offset;
    var repeat: u8 = 0;
    var value: u8 = 0;
    var point_index: u16 = 0;
    while (point_index < numPts) : (point_index += 1) {
        if (repeat != 0) {
            repeat -= 1;
        } else {
            if (off + 1 > ttf_mem.len)
                return error.TtfBadOutline;
            value = ttf_mem[off];
            off += 1;
            if ((value & ttf.repeat_flag) != 0) {
                if (off + 1 > ttf_mem.len)
                    return error.TtfBadOutline;
                repeat = ttf_mem[off];
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
fn simplePoints(
    ttf_mem: []const u8,
    offset: usize,
    numPts: u16,
    flags: []const u8,
    points: []Point,
) !void {
    var off = offset;
    const Accum = i32;
    {
        var accum: Accum = 0;
        var i: u16 = 0;
        while (i < numPts) : (i += 1) {
            if ((flags[i] & ttf.x_change_is_small) != 0) {
                if (off + 1 > ttf_mem.len)
                    return error.TtfBadOutline;
                const value = ttf_mem[off];
                off += 1;
                const is_pos = (flags[i] & ttf.x_change_is_positive) != 0;
                accum -= resolveSign(Accum, is_pos, value);
            } else if (0 == (flags[i] & ttf.x_change_is_zero)) {
                if (off + 2 > ttf_mem.len)
                    return error.TtfBadOutline;
                accum += readTtf(i16, ttf_mem[off..]);
                off += 2;
            }
            points[i].x = @intToFloat(f64, accum);
        }
    }

    {
        var accum: Accum = 0;
        var i: u16 = 0;
        while (i < numPts) : (i += 1) {
            if ((flags[i] & ttf.y_change_is_small) != 0) {
                if (off + 1 > ttf_mem.len)
                    return error.TtfBadOutline;
                const value = ttf_mem[off];
                off += 1;
                const is_pos = (flags[i] & ttf.y_change_is_positive) != 0;
                accum -= resolveSign(Accum, is_pos, value);
            } else if (0 == (flags[i] & ttf.y_change_is_zero)) {
                if (off + 2 > ttf_mem.len)
                    return error.TtfBadOutline;
                accum += readTtf(i16, ttf_mem[off..]);
                off += 2;
            }
            points[i].y = @intToFloat(f64, accum);
        }
    }
}

fn add(comptime T: type, a: T, b: T) ?T {
    var result: T = undefined;
    if (@addWithOverflow(T, a, b, &result)) return null;
    return result;
}

fn decodeContour(
    allocator: std.mem.Allocator,
    flags_start: []const u8,
    basePointStart: u16,
    count_start: u16,
    outl: *Outline,
) !void {
    // Skip contours with less than two points, since the following algorithm can't handle them and
    // they should appear invisible either way (because they don't have any area).
    if (count_start < 2) return;
    std.debug.assert(basePointStart <= std.math.maxInt(u16) - count_start);

    var flags = flags_start;
    var basePoint = basePointStart;
    var count = count_start;
    const looseEnd: u16 = blk: {
        if (0 != (flags[0] & ttf.point_is_on_curve)) {
            basePoint += 1;
            flags = flags[1..];
            count -= 1;
            break :blk basePoint - 1;
        }
        if (0 != (flags[count - 1] & ttf.point_is_on_curve)) {
            count -= 1;
            break :blk add(u16, basePoint, count) orelse return error.TtfTooManyPoints;
        }

        const looseEnd = std.math.cast(u16, outl.points.items.len) orelse return error.TtfTooManyPoints;
        const new_point = midpoint(outl.points.items[basePoint], outl.points.items[basePoint + count - 1]);
        try outl.appendPoint(allocator, new_point);
        break :blk looseEnd;
    };
    var beg = looseEnd;
    var opt_ctrl: ?u16 = null;
    var i: u16 = 0;
    while (i < count) : (i += 1) {
        // cur can't overflow because we ensure that basePoint + count < 0xFFFF before calling decodeContour().
        const cur = add(u16, basePoint, i) orelse return error.TtfTooManyPoints;
        if (0 != (flags[i] & ttf.point_is_on_curve)) {
            if (opt_ctrl) |ctrl| {
                try outl.appendCurve(allocator, .{ .beg = beg, .end = cur, .ctrl = ctrl });
            } else {
                try outl.appendLine(allocator, .{ .beg = beg, .end = cur });
            }
            beg = cur;
            opt_ctrl = null;
        } else {
            if (opt_ctrl) |ctrl| {
                const center = std.math.cast(u16, outl.points.items.len) orelse return error.TtfTooManyPoints;
                const new_point = midpoint(outl.points.items.ptr[ctrl], outl.points.items.ptr[cur]);
                try outl.appendPoint(allocator, new_point);
                try outl.appendCurve(allocator, .{ .beg = beg, .end = center, .ctrl = ctrl });
                beg = center;
            }
            opt_ctrl = cur;
        }
    }
    if (opt_ctrl) |ctrl| {
        try outl.appendCurve(allocator, .{ .beg = beg, .end = looseEnd, .ctrl = ctrl });
    } else {
        try outl.appendLine(allocator, .{ .beg = beg, .end = looseEnd });
    }
}

fn StackBuf(comptime T: type, comptime stack_len: usize) type {
    return struct {
        buf: [stack_len]T = undefined,
        fn alloc(self: *@This(), allocator: std.mem.Allocator, len: usize) error{OutOfMemory}![]T {
            if (len <= stack_len) return &self.buf;
            return try allocator.alloc(T, len);
        }
        fn free(allocator: std.mem.Allocator, buf: []T) void {
            if (buf.len <= stack_len) return;
            allocator.free(buf);
        }
    };
}
fn stackBuf(comptime T: type, comptime stack_len: usize) StackBuf(T, stack_len) {
    return .{};
}

fn simpleOutline(
    allocator: std.mem.Allocator,
    ttf_mem: []const u8,
    offset_start: usize,
    numContours: u15,
    outl: *Outline,
) !void {
    std.debug.assert(numContours > 0);
    const basePoint = std.math.cast(u16, outl.points.items.len) orelse return error.TtfTooManyPoints;

    const limit = offset_start + numContours * 2 + 2;
    if (limit > ttf_mem.len)
        return error.TtfBadOutline;
    const numPts = blk: {
        var num = readTtf(u16, ttf_mem[offset_start + (numContours - 1) * 2..]);
        break :blk add(u16, num, 1) orelse return error.TtfBadOutline;
    };
    if (outl.points.items.len + @intCast(usize, numPts) > std.math.maxInt(u16))
        return error.TtfBadOutline;
    const new_points_len = add(u16, basePoint, numPts) orelse return error.TtfTooManyPoints;
    try outl.points.ensureTotalCapacity(allocator, new_points_len);

    // TODO: the following commented line should work but the zig compiler
    //       isn't optimizing it correctly which causes *extreme* slowdown
    //var endPtsStackBuf = stackBuf(u16, 16);
    const PtsBuf = StackBuf(u16, 16);
    var endPtsStackBuf: PtsBuf = undefined;
    const endPts = try endPtsStackBuf.alloc(allocator, numContours);
    defer PtsBuf.free(allocator, endPts);

    // TODO: the following commented line should work but the zig compiler
    //       isn't optimizing it correctly which causes *extreme* slowdown
    //var flagsStackBuf = stackBuf(u8, 128);
    const FlagsBuf = StackBuf(u8, 128);
    var flagsStackBuf: FlagsBuf = undefined;
    const flags = try flagsStackBuf.alloc(allocator, numPts);
    defer FlagsBuf.free(allocator, flags);

    var offset = offset_start;
    {
        var i: c_uint = 0;
        while (i < numContours) : (i += 1) {
            endPts[i] = readTtf(u16, ttf_mem[offset..]);
            offset += 2;
        }
    }

    // Ensure that endPts are never falling.
    // Falling endPts have no sensible interpretation and most likely only occur in malicious input.
    // Therefore, we bail, should we ever encounter such input.
    {
        var i: @TypeOf(numContours) = 0;
        while (i < numContours - 1) : (i += 1) {
            const prev_limit = add(u16, endPts[i], 1) orelse return error.TtfBadOutline;
            if (endPts[i + 1] < prev_limit)
                return error.TtfBadOutline;
        }
    }
    offset += 2 + @as(usize, readTtf(u16, ttf_mem[offset..]));

    offset = try simpleFlags(ttf_mem, offset, numPts, flags);
    outl.points.items.len = new_points_len;
    try simplePoints(ttf_mem, offset, numPts, flags, outl.points.items[basePoint..]);

    var beg: u16 = 0;
    {
        var i: @TypeOf(numContours) = 0;
        while (i < numContours) : (i += 1) {
            const count = std.math.cast(u16, endPts[i] - beg + 1) orelse return error.TtfBadOutline;
            try decodeContour(
                allocator,
                flags[beg..],
                add(u16, basePoint, beg) orelse return error.TtfTooManyPoints,
                count,
                outl,
            );
            beg = endPts[i] + 1;
        }
    }
}

fn compoundOutline(
    allocator: std.mem.Allocator,
    ttf_mem: []const u8,
    info: TtfInfo,
    offset_start: usize,
    recDepth: u8,
    outl: *Outline,
) !void {
    // Guard against infinite recursion (compound glyphs that have themselves as component).
    if (recDepth >= 4)
        return error.TtfOutlineTooRecursive;
    var offset = offset_start;
    while (true) {
        var local = [_]f64{0} ** 6;
        if (offset + 4 > ttf_mem.len)
            return error.TtfBadOutline;
        const flags = readTtf(u16, ttf_mem[offset + 0..]);
        const glyph = readTtf(u16, ttf_mem[offset + 2..]);
        offset += 4;
        // We don't implement point matching, and neither does stb_truetype for that matter.
        if (0 == (flags & ttf.actual_xy_offsets))
            return error.TtfPointMatchingNotSupported;
        // Read additional X and Y offsets (in FUnits) of this component.
        if (0 != (flags & ttf.offsets_are_large)) {
            if (offset + 4 > ttf_mem.len)
                return error.TtfBadOutline;
            local[4] = @intToFloat(f64, readTtf(i16, ttf_mem[offset + 0..]));
            local[5] = @intToFloat(f64, readTtf(i16, ttf_mem[offset + 2..]));
            offset += 4;
        } else {
            if (offset + 2 > ttf_mem.len)
                return error.TtfBadOutline;
            local[4] = @intToFloat(f64, @bitCast(i8, ttf_mem[offset + 0]));
            local[5] = @intToFloat(f64, @bitCast(i8, ttf_mem[offset + 1]));
            offset += 2;
        }
        if (0 != (flags & ttf.got_a_single_scale)) {
            if (offset + 2 > ttf_mem.len)
                return error.TtfBadOutline;
            local[0] = @intToFloat(f64, readTtf(i16, ttf_mem[offset..])) / 16384.0;
            local[3] = local[0];
            offset += 2;
        } else if (0 != (flags & ttf.got_an_x_and_y_scale)) {
            if (offset + 4 > ttf_mem.len)
                return error.TtfBadOutline;
            local[0] = @intToFloat(f64, readTtf(i16, ttf_mem[offset + 0..])) / 16384.0;
            local[3] = @intToFloat(f64, readTtf(i16, ttf_mem[offset + 2..])) / 16384.0;
            offset += 4;
        } else if (0 != (flags & ttf.got_a_scale_matrix)) {
            if (offset + 8 > ttf_mem.len)
                return error.TtfBadOutline;
            local[0] = @intToFloat(f64, readTtf(i16, ttf_mem[offset + 0..])) / 16384.0;
            local[1] = @intToFloat(f64, readTtf(i16, ttf_mem[offset + 2..])) / 16384.0;
            local[2] = @intToFloat(f64, readTtf(i16, ttf_mem[offset + 4..])) / 16384.0;
            local[3] = @intToFloat(f64, readTtf(i16, ttf_mem[offset + 6..])) / 16384.0;
            offset += 8;
        } else {
            local[0] = 1.0;
            local[3] = 1.0;
        }
        // At this point, Apple's spec more or less tells you to scale the matrix by its own L1 norm.
        // But stb_truetype scales by the L2 norm. And FreeType2 doesn't scale at all.
        // Furthermore, Microsoft's spec doesn't even mention anything like this.
        // It's almost as if nobody ever uses this feature anyway.
        if (try getOutlineOffset(ttf_mem, info, glyph)) |outline| {
            const basePoint = outl.points.items.len;
            try decodeOutline(allocator, ttf_mem, info, outline, recDepth + 1, outl);
            transformPoints(outl.points.items.ptr[basePoint..outl.points.items.len], &local);
        }

        if (0 == (flags & ttf.there_are_more_components)) break;
    }
}

fn decodeOutline(
    allocator: std.mem.Allocator,
    ttf_mem: []const u8,
    info: TtfInfo,
    offset: usize,
    recDepth: u8,
    outl: *Outline,
) !void {
    if (offset + 10 > ttf_mem.len)
        return error.TtfBadOutline;
    const numContours = readTtf(i16, ttf_mem[offset..]);
    if (numContours > 0) {
        // Glyph has a 'simple' outline consisting of a number of contours.
        return simpleOutline(allocator, ttf_mem, offset + 10, @intCast(u15, numContours), outl);
    } else if (numContours < 0) {
        // Glyph has a compound outline combined from mutiple other outlines.
        return compoundOutline(allocator, ttf_mem, info, offset + 10, recDepth, outl);
    }
}

// A heuristic to tell whether a given curve can be approximated closely enough by a line.
fn isFlat(outline: Outline, curve: Curve) bool {
    const maxArea2: f64 = 2.0;
    const a = outline.points.items.ptr[curve.beg];
    const b = outline.points.items.ptr[curve.ctrl];
    const cpoint = outline.points.items.ptr[curve.end];
    const g = Point{ .x = b.x - a.x, .y = b.y - a.y };
    const h = Point{ .x = cpoint.x - a.x, .y = cpoint.y - a.y };
    const area2 = std.math.fabs(g.x * h.y - h.x * g.y);
    return area2 <= maxArea2;
}

fn tesselateCurve(allocator: std.mem.Allocator, curve_in: Curve, outline: *Outline) !void {
    // From my tests I can conclude that this stack barely reaches a top height
    // of 4 elements even for the largest font sizes I'm willing to support. And
    // as space requirements should only grow logarithmically, I think 10 is
    // more than enough.
    const STACK_SIZE = 10;
    var stack: [STACK_SIZE]Curve = undefined;
    var top: usize = 0;
    var curve = curve_in;
    while (true) {
        if (isFlat(outline.*, curve) or top >= STACK_SIZE) {
            try outline.appendLine(allocator, .{ .beg = curve.beg, .end = curve.end });
            if (top == 0) break;
            top -= 1;
            curve = stack[top];
        } else {
            const ctrl0 = std.math.cast(u16, outline.points.items.len) orelse return error.TtfTooManyPoints;
            {
                const new_point = midpoint(outline.points.items.ptr[curve.beg], outline.points.items.ptr[curve.ctrl]);
                try outline.appendPoint(allocator, new_point);
            }
            const ctrl1 = std.math.cast(u16, outline.points.items.len) orelse return error.TtfTooManyPoints;
            {
                const new_point = midpoint(outline.points.items.ptr[curve.ctrl], outline.points.items.ptr[curve.end]);
                try outline.appendPoint(allocator, new_point);
            }
            const pivot = std.math.cast(u16, outline.points.items.len) orelse return error.TtfTooManyPoints;
            {
                const new_point = midpoint(outline.points.items.ptr[ctrl0], outline.points.items.ptr[ctrl1]);
                try outline.appendPoint(allocator, new_point);
            }
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
fn fastFloor(x: f64) c_int {
    return @floatToInt(c_int, std.math.floor(x));
}
fn fastCeil(x: f64) c_int {
    return @floatToInt(c_int, std.math.ceil(x));
}

// Draws a line into the buffer. Uses a custom 2D raycasting algorithm to do so.
fn drawLine(buf: Raster, origin: Point, goal: Point) void {
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
        pixel.x = fastFloor(origin.x);
        nextCrossing.x = 100.0;
    } else {
        if (dir.x > 0) {
            pixel.x = fastFloor(origin.x);
            nextCrossing.x = (origin.x - @intToFloat(f64, pixel.x)) * crossingIncr.x;
            nextCrossing.x = crossingIncr.x - nextCrossing.x;
            numSteps += fastCeil(goal.x) - fastFloor(origin.x) - 1;
        } else {
            pixel.x = fastCeil(origin.x) - 1;
            nextCrossing.x = (origin.x - @intToFloat(f64, pixel.x)) * crossingIncr.x;
            numSteps += fastCeil(origin.x) - fastFloor(goal.x) - 1;
        }
    }

    if (dir.y > 0) {
        pixel.y = fastFloor(origin.y);
        nextCrossing.y = (origin.y - @intToFloat(f64, pixel.y)) * crossingIncr.y;
        nextCrossing.y = crossingIncr.y - nextCrossing.y;
        numSteps += fastCeil(goal.y) - fastFloor(origin.y) - 1;
    } else {
        pixel.y = fastCeil(origin.y) - 1;
        nextCrossing.y = (origin.y - @intToFloat(f64, pixel.y)) * crossingIncr.y;
        numSteps += fastCeil(origin.y) - fastFloor(goal.y) - 1;
    }

    var nextDistance = std.math.min(nextCrossing.x, nextCrossing.y);
    const halfDeltaX = 0.5 * delta.x;
    var prevDistance: f64 = 0.0;
    var step: c_int = 0;
    while (step < numSteps) : (step += 1) {
        var xAverage = origin.x + (prevDistance + nextDistance) * halfDeltaX;
        const yDifference = (nextDistance - prevDistance) * delta.y;
        const cptr = &buf.cells[@intCast(usize, pixel.y * buf.size.x + pixel.x)];
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
    const cptr = &buf.cells[@intCast(usize, pixel.y * buf.size.x + pixel.x)];
    var cell = cptr.*;
    cell.cover += yDifference;
    xAverage -= @intToFloat(f64, pixel.x);
    cell.area += (1.0 - xAverage) * yDifference;
    cptr.* = cell;
}

fn drawLines(outline: *Outline, buf: Raster) void {
    var i: usize = 0;
    while (i < outline.lines.items.len) : (i += 1) {
        const line = outline.lines.items.ptr[i];
        const origin = outline.points.items.ptr[line.beg];
        const goal = outline.points.items.ptr[line.end];
        drawLine(buf, origin, goal);
    }
}

// Integrate the values in the buffer to arrive at the final grayscale image.
fn postProcess(buf: Raster, image: [*]u8) void {
    var accum: f64 = 0;
    const num = @intCast(usize, buf.size.x) * @intCast(usize, buf.size.y);
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

fn renderOutline(
    allocator: std.mem.Allocator,
    outl: *Outline,
    transform: *const [6]f64,
    pixels: [*]u8,
    size: XY(i32),
) !void {
    transformPoints(outl.points.items.ptr[0..outl.points.items.len], transform);
    clipPoints(
        outl.points.items.ptr[0..outl.points.items.len],
        @intToFloat(f64, size.x),
        @intToFloat(f64, size.y),
    );

    {
        var i: usize = 0;
        while (i < outl.curves.items.len) : (i += 1) {
            try tesselateCurve(allocator, outl.curves.items.ptr[i], outl);
        }
    }

    const numPixels = @intCast(usize, size.x) * @intCast(usize, size.y);
    // TODO: the following commented line should work but the zig compiler
    //       isn't optimizing it correctly which causes *extreme* slowdown
    // Zig's 'undefined' debug checks make this ungodly slow
    const stack_len = if (builtin.mode == .Debug) 0 else 128 * 128;
    //var cellStackBuf = stackBuf(Cell, stack_len);
    const CellBuf = StackBuf(Cell, stack_len);
    var cellStackBuf: CellBuf = undefined;
    const cells = try cellStackBuf.alloc(allocator, numPixels);
    defer CellBuf.free(allocator, cells);

    // TODO: I wonder if this could be removed?
    @memset(@ptrCast([*]u8, cells), 0, numPixels * @sizeOf(@TypeOf(cells[0])));
    const buf = Raster{
        .cells = cells,
        .size = size,
    };
    drawLines(outl, buf);
    postProcess(buf, pixels);
}

const cextern = struct {
    pub extern "c" fn nextafter(x: f64, y: f64) f64;
};

fn nextafter(x: f64, y: f64) f64 {
    if (builtin.link_libc) return cextern.nextafter(x, y);
    @compileError("nextafter not implemented without libc");
}
