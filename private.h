#pragma once

enum { SrcMapping, SrcUser };

typedef struct Point   Point;
typedef struct Line    Line;
typedef struct Curve   Curve;
typedef struct Cell    Cell;
typedef struct Outline Outline;
typedef struct Raster  Raster;

struct Point { double x, y; };
struct Line  { uint_least16_t beg, end; };
struct Curve { uint_least16_t beg, end, ctrl; };
struct Cell  { double area, cover; };

struct Outline
{
	Point *points;
	Curve *curves;
	Line  *lines;
	uint_least16_t numPoints;
	uint_least16_t capPoints;
	uint_least16_t numCurves;
	uint_least16_t capCurves;
	uint_least16_t numLines;
	uint_least16_t capLines;
};

struct SFT_Font
{
	const uint8_t *memory;
	uint_fast32_t  size;
#if defined(_WIN32)
	HANDLE         mapping;
#endif
	int            source;

	uint_least16_t unitsPerEm;
	int_least16_t  locaFormat;
	uint_least16_t numLongHmtx;
};

struct Raster
{
	Cell *cells;
	int   width;
	int   height;
};

/* generic utility functions */
/*static*/ void *reallocarray(void *optr, size_t nmemb, size_t size);
/* file loading */
/*static*/ int  init_font (SFT_Font *font);
/* simple mathematical operations */
/*static*/ Point midpoint(Point a, Point b);
/* 'outline' data structure management */
/*static*/ int  grow_points (Outline *outl);
/*static*/ int  grow_lines  (Outline *outl);
/* codepoint to glyph id translation */
int glyph_id(SFT_Font *font, SFT_UChar charCode, uint_fast32_t *glyph);
/* tesselation */
//static int  is_flat(Outline *outl, Curve curve);
//static int  tesselate_curve(Curve curve, Outline *outl);
/*static*/ int  tesselate_curves(Outline *outl);
/* silhouette rasterization */
/*static*/ void draw_line(Raster buf, Point origin, Point goal);
/*static*/ void draw_lines(Outline *outl, Raster buf);
