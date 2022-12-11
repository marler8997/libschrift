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
