const std = @import("std");
const schrift = @import("schrift.zig");
const Float = schrift.Float;

const font = struct {
    pub const ttf = @embedFile("resources/FiraGO-Regular_extended_with_NotoSansEgyptianHieroglyphs-Regular.ttf");
    pub const info = schrift.getTtfInfo(ttf) catch unreachable;
};

pub fn main() !void {
    const scale = schrift.XY(Float){ .x = 32.0, .y = 32.0 };
    const offset = schrift.XY(Float){ .x = 0, .y = 0 };

    const lmetrics = try schrift.lmetrics(font.ttf, font.info, scale.y);
    std.debug.assert(lmetrics.ascender >= 0);
    std.debug.assert(lmetrics.descender <= 0);
    std.debug.assert(lmetrics.line_gap >= 0);
    const line_gap: u32 = @intFromFloat(@ceil(lmetrics.line_gap));
    //const text_height = @intCast(u32, ascent - descent);
    std.log.info(
        "lmetrics: ascent={d:.2} descent={d:.2} gap={d:.2} ({})",
        .{ lmetrics.ascender, lmetrics.descender, lmetrics.line_gap, line_gap },
    );
    //std.log.info("          text_height = {d}", .{text_height});

    const downward = true;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // arena.deinit();
    const glass = try readFile(arena.allocator(), "resources/glass.utf8");

    var image_width: u32 = 0;
    var line_count: u32 = 0;
    var max_glyph_width: u32 = 0;
    var max_glyph_height: u32 = 0;
    var min_glyph_top: i32 = std.math.maxInt(i32);
    var max_glyph_bottom: i32 = std.math.minInt(i32);
    {
        var it = std.unicode.Utf8Iterator{ .bytes = glass, .i = 0 };
        var line_width: u32 = 0;
        var maybe_prev_gid: ?u32 = null;
        while (it.nextCodepoint()) |c| {
            if (c == '\n') {
                image_width = @max(image_width, line_width);
                line_count += 1;
                line_width = 0;
                maybe_prev_gid = null;
                continue;
            }

            const gid = schrift.lookupGlyph(font.ttf, c) catch |err| {
                std.log.err("failed to get glyph id for {}: {s}", .{ c, @errorName(err) });
                continue;
            };
            const kerning = if (maybe_prev_gid) |prev_gid| try schrift.kerning(
                font.ttf,
                font.info,
                scale,
                prev_gid,
                gid,
            ) else schrift.XY(Float){ .x = 0, .y = 0 };
            maybe_prev_gid = gid;

            const gmetrics = try schrift.gmetrics(font.ttf, font.info, downward, scale, offset, gid);
            const size = schrift.XY(i32){
                .x = std.mem.alignForward(i32, gmetrics.min_width, 4),
                .y = gmetrics.min_height,
            };

            //std.log.info("   metrics: {} x {}:  {}", .{size.x, size.y, gmetrics});
            const advance_width: u32 = @intFromFloat(@ceil(gmetrics.advance_width + kerning.x));
            line_width += advance_width;
            max_glyph_width = @max(max_glyph_width, @as(u32, @intCast(size.x)));
            max_glyph_height = @max(max_glyph_height, @as(u32, @intCast(size.y)));

            const top: i32 = @intFromFloat(@floor(lmetrics.ascender + @as(Float, @floatFromInt(gmetrics.y_offset)) + kerning.y));
            const bottom: i32 = top + @as(i32, @intFromFloat(@ceil(@as(Float, @floatFromInt(size.y)) + kerning.y)));

            min_glyph_top = @min(min_glyph_top, top);
            max_glyph_bottom = @max(max_glyph_bottom, bottom);
        }
    }

    const max_render_height: usize = @intCast(max_glyph_bottom - min_glyph_top);
    std.log.info("min_top={} max_bottom={} height={}", .{ min_glyph_top, max_glyph_bottom, max_render_height });

    const image_height = max_render_height * line_count;
    std.log.info("{} lines, image={}x{} max-glyph={}x{}", .{ line_count, image_width, image_height, max_glyph_width, max_glyph_height });

    const line_stride = image_width * 3;
    const line_buf = try arena.allocator().alloc(u8, max_render_height * line_stride);
    const background = 0x00;
    @memset(line_buf, background);

    // defer arena.allocator().free(line_buf);
    const glyph_pixel_buf = try arena.allocator().alloc(u8, max_glyph_width * max_glyph_height);
    // defer arena.allocator().free(glyph_pixel_buf);

    // TODO: put font name in the output filename by default?
    var out_file = try std.fs.cwd().createFile("glass.ppm", .{});
    defer out_file.close();
    const writer = out_file.writer();
    try writer.print("P6\n{} {}\n255\n", .{ image_width, image_height });

    {
        var it = std.unicode.Utf8Iterator{ .bytes = glass, .i = 0 };
        var x: usize = 0;
        var y: usize = 0;
        var maybe_prev_gid: ?u32 = null;
        while (it.nextCodepoint()) |c| {
            if (c == '\n') {
                try writer.writeAll(line_buf);
                @memset(line_buf, background);
                x = 0;
                y += max_render_height;
                //std.log.info("next line y={}!", .{y});
                maybe_prev_gid = null;
                continue;
            }

            const gid = schrift.lookupGlyph(font.ttf, c) catch unreachable;
            const kerning = if (maybe_prev_gid) |prev_gid| try schrift.kerning(
                font.ttf,
                font.info,
                scale,
                prev_gid,
                gid,
            ) else schrift.XY(Float){ .x = 0, .y = 0 };
            maybe_prev_gid = gid;

            const gmetrics = schrift.gmetrics(font.ttf, font.info, downward, scale, offset, gid) catch unreachable;
            const size = schrift.XY(i32){
                .x = std.mem.alignForward(i32, gmetrics.min_width, 4),
                .y = gmetrics.min_height,
            };

            var render_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer render_arena.deinit();

            //std.log.info("   metrics: {} x {}:  {}", .{size.x, size.y, gmetrics});
            // TODO: render directly into our line_buf by enhancing
            //       the API to take a "stride"
            const pixel_buf_len: usize = @as(usize, @intCast(size.x)) * @as(usize, @intCast(size.y));
            try schrift.render(
                render_arena.allocator(),
                font.ttf,
                font.info,
                downward,
                scale,
                offset,
                glyph_pixel_buf[0..pixel_buf_len],
                @intCast(size.x),
                size,
                gid,
            );

            const top: i32 = @intFromFloat(@floor(lmetrics.ascender + @as(Float, @floatFromInt(gmetrics.y_offset)) + kerning.y));
            const line_top: usize = @intCast(top - min_glyph_top);
            //std.log.info("{} {}x{} y_offset={} top={}", .{c, size.x, size.y, gmetrics.y_offset, top});
            copyBox(
                line_buf[(@as(usize, @intFromFloat(@as(f32, @floatFromInt(x)) + @ceil(kerning.x))) * 3) + (line_stride * line_top) ..],
                line_stride,
                glyph_pixel_buf.ptr,
                @intCast(size.x),
                @intCast(size.y),
            );
            //std.log.info("{} width={} lsb={d:.2} advance={d:.2}", .{
            //    c, size.x,
            //    gmetrics.left_side_bearing,
            //    gmetrics.advance_width,
            //});
            const advance_width: u32 = @intFromFloat(@ceil(gmetrics.advance_width + kerning.x));
            x += advance_width;
        }
    }
}

fn readFile(allocator: std.mem.Allocator, filename: []const u8) ![]u8 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, std.math.maxInt(usize));
}

fn copyBox(
    dst: []u8,
    dst_stride: usize,
    src: [*]const u8,
    src_width: usize,
    height: usize,
) void {
    var row: usize = 0;
    var dst_off: usize = 0;
    var src_off: usize = 0;
    while (row < height) : (row += 1) {
        {
            var i: usize = 0;
            while (i < src_width) : (i += 1) {
                dst[dst_off + 3 * i + 0] = src[src_off + i];
                dst[dst_off + 3 * i + 1] = src[src_off + i];
                dst[dst_off + 3 * i + 2] = src[src_off + i];
            }
        }
        dst_off += dst_stride;
        src_off += src_width;
    }
}
