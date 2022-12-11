#pragma once

int glyph_id(SFT_Font *font, SFT_UChar charCode, uint_fast32_t *glyph);

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
