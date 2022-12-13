/* This file is part of libschrift.
 *
 * © 2019-2022 Thomas Oltmann and contributors
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. */

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_MSC_VER)
# define restrict __restrict
#endif

#if defined(_WIN32)
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>
#else
# define _POSIX_C_SOURCE 1
# include <fcntl.h>
# include <sys/mman.h>
# include <sys/stat.h>
# include <unistd.h>
#endif

#include "schrift.h"
#include "private.h"

#define HORIZONTAL_KERNING         0x01
#define MINIMUM_KERNING            0x02
#define CROSS_STREAM_KERNING       0x04
#define OVERRIDE_KERNING           0x08

#define POINT_IS_ON_CURVE          0x01
#define X_CHANGE_IS_SMALL          0x02
#define Y_CHANGE_IS_SMALL          0x04
#define REPEAT_FLAG                0x08
#define X_CHANGE_IS_ZERO           0x10
#define X_CHANGE_IS_POSITIVE       0x10
#define Y_CHANGE_IS_ZERO           0x20
#define Y_CHANGE_IS_POSITIVE       0x20

#define OFFSETS_ARE_LARGE          0x001
#define ACTUAL_XY_OFFSETS          0x002
#define GOT_A_SINGLE_SCALE         0x008
#define THERE_ARE_MORE_COMPONENTS  0x020
#define GOT_AN_X_AND_Y_SCALE       0x040
#define GOT_A_SCALE_MATRIX         0x080

/* Allocate values on the stack if they are small enough, else spill to heap. */
#define STACK_ALLOC(var, type, thresh, count) \
	type var##_stack_[thresh]; \
	var = (count) <= (thresh) ? var##_stack_ : calloc(sizeof(type), count);
#define STACK_FREE(var) \
	if (var != var##_stack_) free(var);

/* structs */
/* function declarations */
/* simple mathematical operations */
static void transform_points(unsigned int numPts, Point *points, double trf[6]);
static void clip_points(unsigned int numPts, Point *points, double width, double height);
/* 'outline' data structure management */
/*static*/ int  grow_curves (Outline *outl);
/* TTF parsing utilities */
/*static*/ void *csearch(const void *key, const void *base,
	size_t nmemb, size_t size, int (*compar)(const void *, const void *));
/*static*/ int  cmpu16(const void *a, const void *b);
/*static*/ int  cmpu32(const void *a, const void *b);
static inline uint_least8_t  getu8 (SFT_Font *font, uint_fast32_t offset);
static inline int_least8_t   geti8 (SFT_Font *font, uint_fast32_t offset);
static inline uint_least16_t getu16(SFT_Font *font, uint_fast32_t offset);
static inline int_least16_t  geti16(SFT_Font *font, uint_fast32_t offset);
static inline uint_least32_t getu32(SFT_Font *font, uint_fast32_t offset);
/* post-processing */
/*static*/ void post_process(Raster buf, uint8_t *image);

/* function implementations */

int
sft_kerning(const SFT *sft, SFT_Glyph leftGlyph, SFT_Glyph rightGlyph,
            SFT_Kerning *kerning)
{
	void *match;
	uint_fast32_t offset;
	unsigned int numTables, numPairs, length, format, flags;
	int value;
	uint8_t key[4];

	memset(kerning, 0, sizeof *kerning);

	if (gettable(sft->font, "kern", &offset) < 0)
		return 0;

	/* Read kern table header. */
	if (!is_safe_offset(sft->font, offset, 4))
		return -1;
	if (getu16(sft->font, offset) != 0)
		return 0;
	numTables = getu16(sft->font, offset + 2);
	offset += 4;

	while (numTables > 0) {
		/* Read subtable header. */
		if (!is_safe_offset(sft->font, offset, 6))
			return -1;
		length = getu16(sft->font, offset + 2);
		format = getu8 (sft->font, offset + 4);
		flags  = getu8 (sft->font, offset + 5);
		offset += 6;

		if (format == 0 && (flags & HORIZONTAL_KERNING) && !(flags & MINIMUM_KERNING)) {
			/* Read format 0 header. */
			if (!is_safe_offset(sft->font, offset, 8))
				return -1;
			numPairs = getu16(sft->font, offset);
			offset += 8;
			/* Look up character code pair via binary search. */
			key[0] = (leftGlyph  >> 8) & 0xFF;
			key[1] =  leftGlyph  & 0xFF;
			key[2] = (rightGlyph >> 8) & 0xFF;
			key[3] =  rightGlyph & 0xFF;
			if ((match = bsearch(key, sft->font->memory + offset,
				numPairs, 6, cmpu32)) != NULL) {
				
				value = geti16(sft->font, (uint_fast32_t) ((uint8_t *) match - sft->font->memory + 4));
				if (flags & CROSS_STREAM_KERNING) {
					kerning->yShift += value;
				} else {
					kerning->xShift += value;
				}
			}

		}

		offset += length;
		--numTables;
	}

	kerning->xShift = kerning->xShift / sft->font->unitsPerEm * sft->xScale;
	kerning->yShift = kerning->yShift / sft->font->unitsPerEm * sft->yScale;

	return 0;
}

static inline uint_least8_t
getu8(SFT_Font *font, uint_fast32_t offset)
{
	assert(offset + 1 <= font->size);
	return *(font->memory + offset);
}

static inline int_least8_t
geti8(SFT_Font *font, uint_fast32_t offset)
{
	return (int_least8_t) getu8(font, offset);
}

static inline uint_least16_t
getu16(SFT_Font *font, uint_fast32_t offset)
{
	assert(offset + 2 <= font->size);
	const uint8_t *base = font->memory + offset;
	uint_least16_t b1 = base[0], b0 = base[1]; 
	return (uint_least16_t) (b1 << 8 | b0);
}

static inline int16_t
geti16(SFT_Font *font, uint_fast32_t offset)
{
	return (int_least16_t) getu16(font, offset);
}

static inline uint32_t
getu32(SFT_Font *font, uint_fast32_t offset)
{
	assert(offset + 4 <= font->size);
	const uint8_t *base = font->memory + offset;
	uint_least32_t b3 = base[0], b2 = base[1], b1 = base[2], b0 = base[3]; 
	return (uint_least32_t) (b3 << 24 | b2 << 16 | b1 << 8 | b0);
}

static int
cmap_fmt6(SFT_Font *font, uint_fast32_t table, SFT_UChar charCode, SFT_Glyph *glyph)
{
	unsigned int firstCode, entryCount;
	/* cmap format 6 only supports the Unicode BMP. */
	if (charCode > 0xFFFF) {
		*glyph = 0;
		return 0;
	}
	if (!is_safe_offset(font, table, 4))
		return -1;
	firstCode  = getu16(font, table);
	entryCount = getu16(font, table + 2);
	if (!is_safe_offset(font, table, 4 + 2 * entryCount))
		return -1;
	if (charCode < firstCode)
		return -1;
	charCode -= firstCode;
	if (!(charCode < entryCount))
		return -1;
	*glyph = getu16(font, table + 4 + 2 * charCode);
	return 0;
}

/*static*/ int
render_outline(Outline *outl, double transform[6], SFT_Image image)
{
	Cell *cells = NULL;
	Raster buf;
	unsigned int numPixels;
	
	numPixels = (unsigned int) image.width * (unsigned int) image.height;

	STACK_ALLOC(cells, Cell, 128 * 128, numPixels);
	if (!cells) {
		return -1;
	}
	memset(cells, 0, numPixels * sizeof *cells);
	buf.cells  = cells;
	buf.width  = image.width;
	buf.height = image.height;

	transform_points(outl->numPoints, outl->points, transform);

	clip_points(outl->numPoints, outl->points, image.width, image.height);

	if (tesselate_curves(outl) < 0) {
		STACK_FREE(cells);
		return -1;
	}

	draw_lines(outl, buf);

	post_process(buf, image.pixels);

	STACK_FREE(cells);
	return 0;
}

