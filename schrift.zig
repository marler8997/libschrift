const std = @import("std");
const c = @cImport({
    @cInclude("schrift.h");
    @cInclude("private.h");
});

export fn sft_version() [*:0]const u8 {
    return "0.10.2";
}

export fn sft_lookup(sft: *const c.SFT, codepoint: c.SFT_UChar, glyph: c.SFT_Glyph) c_int {
    return c.glyph_id(sft.font, codepoint, glyph);
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
