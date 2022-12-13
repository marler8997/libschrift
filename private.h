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
/* TTF parsing utilities */
/*static*/ inline int is_safe_offset(SFT_Font *font, uint_fast32_t offset, uint_fast32_t margin);
/*static*/ int gettable(SFT_Font *font, const char tag[4], uint_fast32_t *offset);
/* glyph metrics lookup */
/*static*/ int  glyph_bbox(const SFT *sft, uint_fast32_t outline, int box[4]);
/*static*/ int  hor_metrics(SFT_Font *font, uint_fast32_t glyph, int *advanceWidth, int *leftSideBearing);
/* decoding outlines */
/*static*/ int  outline_offset(SFT_Font *font, uint_fast32_t glyph, uint_fast32_t *offset);
/*static*/ int  decode_outline(SFT_Font *font, uint_fast32_t offset, int recDepth, Outline *outl);
/* tesselation */
//static int  is_flat(Outline *outl, Curve curve);
//static int  tesselate_curve(Curve curve, Outline *outl);
/*static*/ int  tesselate_curves(Outline *outl);
/* silhouette rasterization */
/*static*/ void draw_line(Raster buf, Point origin, Point goal);
/*static*/ void draw_lines(Outline *outl, Raster buf);
/* glyph rendering */
/*static*/ int  render_outline(Outline *outl, double transform[6], SFT_Image image);
