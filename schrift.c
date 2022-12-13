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

/* macros */
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define SIGN(x)   (((x) > 0) - ((x) < 0))
/* Allocate values on the stack if they are small enough, else spill to heap. */
#define STACK_ALLOC(var, type, thresh, count) \
	type var##_stack_[thresh]; \
	var = (count) <= (thresh) ? var##_stack_ : calloc(sizeof(type), count);
#define STACK_FREE(var) \
	if (var != var##_stack_) free(var);

/* structs */
/* function declarations */
/* generic utility functions */
static inline int fast_floor(double x);
static inline int fast_ceil (double x);
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
/* decoding outlines */
static int  decode_contour(uint8_t *flags, uint_fast16_t basePoint, uint_fast16_t count, Outline *outl);
static int  simple_outline(SFT_Font *font, uint_fast32_t offset, unsigned int numContours, Outline *outl);
static int  compound_outline(SFT_Font *font, uint_fast32_t offset, int recDepth, Outline *outl);
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

/* TODO maybe we should use long here instead of int. */
static inline int
fast_floor(double x)
{
	int i = (int) x;
	return i - (i > x);
}

static inline int
fast_ceil(double x)
{
	int i = (int) x;
	return i + (i < x);
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
decode_outline(SFT_Font *font, uint_fast32_t offset, int recDepth, Outline *outl)
{
	int numContours;
	if (!is_safe_offset(font, offset, 10))
		return -1;
	numContours = geti16(font, offset);
	if (numContours > 0) {
		/* Glyph has a 'simple' outline consisting of a number of contours. */
		return simple_outline(font, offset + 10, (unsigned int) numContours, outl);
	} else if (numContours < 0) {
		/* Glyph has a compound outline combined from mutiple other outlines. */
		return compound_outline(font, offset + 10, recDepth, outl);
	} else {
		return 0;
	}
}

/* Draws a line into the buffer. Uses a custom 2D raycasting algorithm to do so. */
/*static*/ void
draw_line(Raster buf, Point origin, Point goal)
{
	Point delta;
	Point nextCrossing;
	Point crossingIncr;
	double halfDeltaX;
	double prevDistance = 0.0, nextDistance;
	double xAverage, yDifference;
	struct { int x, y; } pixel;
	struct { int x, y; } dir;
	int step, numSteps = 0;
	Cell *restrict cptr, cell;

	delta.x = goal.x - origin.x;
	delta.y = goal.y - origin.y;
	dir.x = SIGN(delta.x);
	dir.y = SIGN(delta.y);

	if (!dir.y) {
		return;
	}
	
	crossingIncr.x = dir.x ? fabs(1.0 / delta.x) : 1.0;
	crossingIncr.y = fabs(1.0 / delta.y);

	if (!dir.x) {
		pixel.x = fast_floor(origin.x);
		nextCrossing.x = 100.0;
	} else {
		if (dir.x > 0) {
			pixel.x = fast_floor(origin.x);
			nextCrossing.x = (origin.x - pixel.x) * crossingIncr.x;
			nextCrossing.x = crossingIncr.x - nextCrossing.x;
			numSteps += fast_ceil(goal.x) - fast_floor(origin.x) - 1;
		} else {
			pixel.x = fast_ceil(origin.x) - 1;
			nextCrossing.x = (origin.x - pixel.x) * crossingIncr.x;
			numSteps += fast_ceil(origin.x) - fast_floor(goal.x) - 1;
		}
	}

	if (dir.y > 0) {
		pixel.y = fast_floor(origin.y);
		nextCrossing.y = (origin.y - pixel.y) * crossingIncr.y;
		nextCrossing.y = crossingIncr.y - nextCrossing.y;
		numSteps += fast_ceil(goal.y) - fast_floor(origin.y) - 1;
	} else {
		pixel.y = fast_ceil(origin.y) - 1;
		nextCrossing.y = (origin.y - pixel.y) * crossingIncr.y;
		numSteps += fast_ceil(origin.y) - fast_floor(goal.y) - 1;
	}

	nextDistance = MIN(nextCrossing.x, nextCrossing.y);
	halfDeltaX = 0.5 * delta.x;

	for (step = 0; step < numSteps; ++step) {
		xAverage = origin.x + (prevDistance + nextDistance) * halfDeltaX;
		yDifference = (nextDistance - prevDistance) * delta.y;
		cptr = &buf.cells[pixel.y * buf.width + pixel.x];
		cell = *cptr;
		cell.cover += yDifference;
		xAverage -= (double) pixel.x;
		cell.area += (1.0 - xAverage) * yDifference;
		*cptr = cell;
		prevDistance = nextDistance;
		int alongX = nextCrossing.x < nextCrossing.y;
		pixel.x += alongX ? dir.x : 0;
		pixel.y += alongX ? 0 : dir.y;
		nextCrossing.x += alongX ? crossingIncr.x : 0.0;
		nextCrossing.y += alongX ? 0.0 : crossingIncr.y;
		nextDistance = MIN(nextCrossing.x, nextCrossing.y);
	}

	xAverage = origin.x + (prevDistance + 1.0) * halfDeltaX;
	yDifference = (1.0 - prevDistance) * delta.y;
	cptr = &buf.cells[pixel.y * buf.width + pixel.x];
	cell = *cptr;
	cell.cover += yDifference;
	xAverage -= (double) pixel.x;
	cell.area += (1.0 - xAverage) * yDifference;
	*cptr = cell;
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

