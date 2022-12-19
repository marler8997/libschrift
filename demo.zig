const std = @import("std");
const schrift = @import("schrift.zig");
const Float = schrift.Float;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // arena.deinit();

    const ttf_mem = try readFile(
        arena.allocator(),
        "resources/FiraGO-Regular_extended_with_NotoSansEgyptianHieroglyphs-Regular.ttf",
    );
    const ttf_info = try schrift.getTtfInfo(ttf_mem);

    const scale = schrift.XY(Float) { .x = 32.0, .y = 32.0 };
    const offset = schrift.XY(Float) { .x = 0, .y = 0 };

    const lmetrics = try schrift.lmetrics(ttf_mem, ttf_info, scale.y);
    std.debug.assert(lmetrics.ascender >= 0);
    std.debug.assert(lmetrics.descender <= 0);
    std.debug.assert(lmetrics.line_gap >= 0);
    const ascent = @floatToInt(i32, @ceil(lmetrics.ascender));
    const descent = @floatToInt(i32, @floor(lmetrics.descender));
    const line_gap = @floatToInt(u32, @ceil(lmetrics.line_gap));
    //const text_height = @intCast(u32, ascent - descent);
    std.log.info("lmetrics: ascent={d:.2} ({}) descent={d:.2} ({}) gap={d:.2} ({})", .{
        lmetrics.ascender, ascent,
        lmetrics.descender, descent,
        lmetrics.line_gap, line_gap});
    //std.log.info("          text_height = {d}", .{text_height});

    const downward = true;

    const glass = try readFile(arena.allocator(), "resources/glass.utf8");

    const hgap = 1;

    var image_width: u32 = 0;
    var line_count: u32 = 0;
    var max_glyph_width: u32 = 0;
    var max_glyph_height: u32 = 0;
    var min_glyph_top: i32 = std.math.maxInt(i32);
    var max_glyph_bottom: i32 = std.math.minInt(i32);
    {
        var it = std.unicode.Utf8Iterator{ .bytes = glass, .i = 0 };
        var line_width: u32 = 0;
        while (it.nextCodepoint()) |c| {
            if (c == '\n') {
                image_width = std.math.max(image_width, line_width);
                line_count += 1;
                line_width = 0;
                continue;
            }
            const gid = schrift.lookupGlyph(ttf_mem, c) catch |err| {
                std.log.err("failed to get glyph id for {}: {s}", .{c, @errorName(err)});
                continue;
            };
            const gmetrics = try schrift.gmetrics(ttf_mem, ttf_info, downward, scale, offset, gid);
            var size = schrift.XY(i32){
                .x = std.mem.alignForwardGeneric(i32, gmetrics.min_width, 4),
                .y = gmetrics.min_height,
            };
            //std.log.info("   metrics: {} x {}:  {}", .{size.x, size.y, gmetrics});
            line_width += @intCast(u32, size.x) + hgap;
            max_glyph_width  = std.math.max(max_glyph_width, @intCast(u32, size.x));
            max_glyph_height = std.math.max(max_glyph_height, @intCast(u32, size.y));

            const top: i32 = ascent + gmetrics.y_offset;
            const bottom: i32 = top + size.y;

            min_glyph_top = std.math.min(min_glyph_top, top);
            max_glyph_bottom = std.math.max(max_glyph_bottom, bottom);
        }
    }

    const max_render_height = @intCast(usize, max_glyph_bottom - min_glyph_top);
    std.log.info("min_top={} max_bottom={} height={}", .{min_glyph_top, max_glyph_bottom, max_render_height});

    const image_height = max_render_height * line_count;
    std.log.info("{} lines, image={}x{} max-glyph={}x{}", .{
        line_count, image_width, image_height, max_glyph_width, max_glyph_height});

    const line_stride = image_width * 3;
    const line_buf = try arena.allocator().alloc(u8, max_render_height * line_stride);
    std.mem.set(u8, line_buf, 0x33);

    // defer arena.allocator().free(line_buf);
    const glyph_pixel_buf = try arena.allocator().alloc(u8, max_glyph_width * max_glyph_height);
    // defer arena.allocator().free(glyph_pixel_buf);

    // TODO: put font name in the output filename by default?
    var out_file = try std.fs.cwd().createFile("glass.ppm", .{});
    defer out_file.close();
    const writer = out_file.writer();
    try writer.print("P6\n{} {}\n255\n", .{image_width, image_height});

    {
        var it = std.unicode.Utf8Iterator{ .bytes = glass, .i = 0 };
        var x: usize = 0;
        var y: usize = 0;
        while (it.nextCodepoint()) |c| {
            if (c == '\n') {
                try writer.writeAll(line_buf);
                std.mem.set(u8, line_buf, 0x33);
                x = 0;
                y += max_render_height;
                //std.log.info("next line y={}!", .{y});
                continue;
            }
            const gid = schrift.lookupGlyph(ttf_mem, c) catch unreachable;
            const gmetrics = schrift.gmetrics(ttf_mem, ttf_info, downward, scale, offset, gid) catch unreachable;
            var size = schrift.XY(i32){
                .x = std.mem.alignForwardGeneric(i32, gmetrics.min_width, 4),
                .y = gmetrics.min_height,
            };

            var render_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer render_arena.deinit();

            //std.log.info("   metrics: {} x {}:  {}", .{size.x, size.y, gmetrics});
            // TODO: render directly into our line_buf by enhancing
            //       the API to take a "stride"
            const pixel_buf_len = @intCast(usize, size.x) * @intCast(usize, size.y);
            try schrift.render(
                render_arena.allocator(),
                ttf_mem,
                ttf_info,
                downward,
                scale,
                offset,
                glyph_pixel_buf[0 .. pixel_buf_len],
                size,
                gid,
            );

            const top = @intCast(usize, ascent + gmetrics.y_offset - min_glyph_top);
            //std.log.info("{} {}x{} y_offset={} top={}", .{c, size.x, size.y, gmetrics.y_offset, top});
            copyBox(
                line_buf[(x * 3) + (line_stride * top)..],
                line_stride,
                glyph_pixel_buf.ptr,
                @intCast(usize, size.x),
                @intCast(usize, size.y),
            );
            x += @intCast(usize, size.x) + hgap;
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
                dst[dst_off + 3*i + 0] = src[src_off + i];
                dst[dst_off + 3*i + 1] = src[src_off + i];
                dst[dst_off + 3*i + 2] = src[src_off + i];
            }
        }
        dst_off += dst_stride;
        src_off += src_width;
    }
}
