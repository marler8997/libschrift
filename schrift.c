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
static int  simple_flags(SFT_Font *font, uint_fast32_t *offset, uint_fast16_t numPts, uint8_t *flags);
static int  simple_points(SFT_Font *font, uint_fast32_t offset, uint_fast16_t numPts, uint8_t *flags, Point *points);
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

/* For a 'simple' outline, decodes both X and Y coordinates for each point of the outline. */
static int
simple_points(SFT_Font *font, uint_fast32_t offset, uint_fast16_t numPts, uint8_t *flags, Point *points)
{
	long accum, value, bit;
	uint_fast16_t i;

	accum = 0L;
	for (i = 0; i < numPts; ++i) {
		if (flags[i] & X_CHANGE_IS_SMALL) {
			if (!is_safe_offset(font, offset, 1))
				return -1;
			value = (long) getu8(font, offset++);
			bit = !!(flags[i] & X_CHANGE_IS_POSITIVE);
			accum -= (value ^ -bit) + bit;
		} else if (!(flags[i] & X_CHANGE_IS_ZERO)) {
			if (!is_safe_offset(font, offset, 2))
				return -1;
			accum += geti16(font, offset);
			offset += 2;
		}
		points[i].x = (double) accum;
	}

	accum = 0L;
	for (i = 0; i < numPts; ++i) {
		if (flags[i] & Y_CHANGE_IS_SMALL) {
			if (!is_safe_offset(font, offset, 1))
				return -1;
			value = (long) getu8(font, offset++);
			bit = !!(flags[i] & Y_CHANGE_IS_POSITIVE);
			accum -= (value ^ -bit) + bit;
		} else if (!(flags[i] & Y_CHANGE_IS_ZERO)) {
			if (!is_safe_offset(font, offset, 2))
				return -1;
			accum += geti16(font, offset);
			offset += 2;
		}
		points[i].y = (double) accum;
	}

	return 0;
}

static int
decode_contour(uint8_t *flags, uint_fast16_t basePoint, uint_fast16_t count, Outline *outl)
{
	uint_fast16_t i;
	uint_least16_t looseEnd, beg, ctrl, center, cur;
	unsigned int gotCtrl;

	/* Skip contours with less than two points, since the following algorithm can't handle them and
	 * they should appear invisible either way (because they don't have any area). */
	if (count < 2) return 0;

	assert(basePoint <= UINT16_MAX - count);

	if (flags[0] & POINT_IS_ON_CURVE) {
		looseEnd = (uint_least16_t) basePoint++;
		++flags;
		--count;
	} else if (flags[count - 1] & POINT_IS_ON_CURVE) {
		looseEnd = (uint_least16_t) (basePoint + --count);
	} else {
		if (outl->numPoints >= outl->capPoints && grow_points(outl) < 0)
			return -1;

		looseEnd = outl->numPoints;
		outl->points[outl->numPoints++] = midpoint(
			outl->points[basePoint],
			outl->points[basePoint + count - 1]);
	}
	beg = looseEnd;
	gotCtrl = 0;
	for (i = 0; i < count; ++i) {
		/* cur can't overflow because we ensure that basePoint + count < 0xFFFF before calling decode_contour(). */
		cur = (uint_least16_t) (basePoint + i);
		/* NOTE clang-analyzer will often flag this and another piece of code because it thinks that flags and
		 * outl->points + basePoint don't always get properly initialized -- even when you explicitly loop over both
		 * and set every element to zero (but not when you use memset). This is a known clang-analyzer bug:
		 * http://clang-developers.42468.n3.nabble.com/StaticAnalyzer-False-positive-with-loop-handling-td4053875.html */
		if (flags[i] & POINT_IS_ON_CURVE) {
			if (gotCtrl) {
				if (outl->numCurves >= outl->capCurves && grow_curves(outl) < 0)
					return -1;
				outl->curves[outl->numCurves++] = (Curve) { beg, cur, ctrl };
			} else {
				if (outl->numLines >= outl->capLines && grow_lines(outl) < 0)
					return -1;
				outl->lines[outl->numLines++] = (Line) { beg, cur };
			}
			beg = cur;
			gotCtrl = 0;
		} else {
			if (gotCtrl) {
				center = outl->numPoints;
				if (outl->numPoints >= outl->capPoints && grow_points(outl) < 0)
					return -1;
				outl->points[center] = midpoint(outl->points[ctrl], outl->points[cur]);
				++outl->numPoints;

				if (outl->numCurves >= outl->capCurves && grow_curves(outl) < 0)
					return -1;
				outl->curves[outl->numCurves++] = (Curve) { beg, center, ctrl };

				beg = center;
			}
			ctrl = cur;
			gotCtrl = 1;
		}
	}
	if (gotCtrl) {
		if (outl->numCurves >= outl->capCurves && grow_curves(outl) < 0)
			return -1;
		outl->curves[outl->numCurves++] = (Curve) { beg, looseEnd, ctrl };
	} else {
		if (outl->numLines >= outl->capLines && grow_lines(outl) < 0)
			return -1;
		outl->lines[outl->numLines++] = (Line) { beg, looseEnd };
	}

	return 0;
}

static int
simple_outline(SFT_Font *font, uint_fast32_t offset, unsigned int numContours, Outline *outl)
{
	uint_fast16_t *endPts = NULL;
	uint8_t *flags = NULL;
	uint_fast16_t numPts;
	unsigned int i;

	assert(numContours > 0);

	uint_fast16_t basePoint = outl->numPoints;

	if (!is_safe_offset(font, offset, numContours * 2 + 2))
		goto failure;
	numPts = getu16(font, offset + (numContours - 1) * 2);
	if (numPts >= UINT16_MAX)
		goto failure;
	numPts++;
	if (outl->numPoints > UINT16_MAX - numPts)
		goto failure;

	while (outl->capPoints < basePoint + numPts) {
		if (grow_points(outl) < 0)
			goto failure;
	}
	
	STACK_ALLOC(endPts, uint_fast16_t, 16, numContours);
	if (endPts == NULL)
		goto failure;
	STACK_ALLOC(flags, uint8_t, 128, numPts);
	if (flags == NULL)
		goto failure;

	for (i = 0; i < numContours; ++i) {
		endPts[i] = getu16(font, offset);
		offset += 2;
	}
	/* Ensure that endPts are never falling.
	 * Falling endPts have no sensible interpretation and most likely only occur in malicious input.
	 * Therefore, we bail, should we ever encounter such input. */
	for (i = 0; i < numContours - 1; ++i) {
		if (endPts[i + 1] < endPts[i] + 1)
			goto failure;
	}
	offset += 2U + getu16(font, offset);

	if (simple_flags(font, &offset, numPts, flags) < 0)
		goto failure;
	if (simple_points(font, offset, numPts, flags, outl->points + basePoint) < 0)
		goto failure;
	outl->numPoints = (uint_least16_t) (outl->numPoints + numPts);

	uint_fast16_t beg = 0;
	for (i = 0; i < numContours; ++i) {
		uint_fast16_t count = endPts[i] - beg + 1;
		if (decode_contour(flags + beg, basePoint + beg, count, outl) < 0)
			goto failure;
		beg = endPts[i] + 1;
	}

	STACK_FREE(endPts);
	STACK_FREE(flags);
	return 0;
failure:
	STACK_FREE(endPts);
	STACK_FREE(flags);
	return -1;
}

static int
compound_outline(SFT_Font *font, uint_fast32_t offset, int recDepth, Outline *outl)
{
	double local[6];
	uint_fast32_t outline;
	unsigned int flags, glyph, basePoint;
	/* Guard against infinite recursion (compound glyphs that have themselves as component). */
	if (recDepth >= 4)
		return -1;
	do {
		memset(local, 0, sizeof local);
		if (!is_safe_offset(font, offset, 4))
			return -1;
		flags = getu16(font, offset);
		glyph = getu16(font, offset + 2);
		offset += 4;
		/* We don't implement point matching, and neither does stb_truetype for that matter. */
		if (!(flags & ACTUAL_XY_OFFSETS))
			return -1;
		/* Read additional X and Y offsets (in FUnits) of this component. */
		if (flags & OFFSETS_ARE_LARGE) {
			if (!is_safe_offset(font, offset, 4))
				return -1;
			local[4] = geti16(font, offset);
			local[5] = geti16(font, offset + 2);
			offset += 4;
		} else {
			if (!is_safe_offset(font, offset, 2))
				return -1;
			local[4] = geti8(font, offset);
			local[5] = geti8(font, offset + 1);
			offset += 2;
		}
		if (flags & GOT_A_SINGLE_SCALE) {
			if (!is_safe_offset(font, offset, 2))
				return -1;
			local[0] = geti16(font, offset) / 16384.0;
			local[3] = local[0];
			offset += 2;
		} else if (flags & GOT_AN_X_AND_Y_SCALE) {
			if (!is_safe_offset(font, offset, 4))
				return -1;
			local[0] = geti16(font, offset + 0) / 16384.0;
			local[3] = geti16(font, offset + 2) / 16384.0;
			offset += 4;
		} else if (flags & GOT_A_SCALE_MATRIX) {
			if (!is_safe_offset(font, offset, 8))
				return -1;
			local[0] = geti16(font, offset + 0) / 16384.0;
			local[1] = geti16(font, offset + 2) / 16384.0;
			local[2] = geti16(font, offset + 4) / 16384.0;
			local[3] = geti16(font, offset + 6) / 16384.0;
			offset += 8;
		} else {
			local[0] = 1.0;
			local[3] = 1.0;
		}
		/* At this point, Apple's spec more or less tells you to scale the matrix by its own L1 norm.
		 * But stb_truetype scales by the L2 norm. And FreeType2 doesn't scale at all.
		 * Furthermore, Microsoft's spec doesn't even mention anything like this.
		 * It's almost as if nobody ever uses this feature anyway. */
		if (outline_offset(font, glyph, &outline) < 0)
			return -1;
		if (outline) {
			basePoint = outl->numPoints;
			if (decode_outline(font, outline, recDepth + 1, outl) < 0)
				return -1;
			transform_points(outl->numPoints - basePoint, outl->points + basePoint, local);
		}
	} while (flags & THERE_ARE_MORE_COMPONENTS);

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

