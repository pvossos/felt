/* 
 * this code (except for the driver XImageToEPS) is taken straight from
 * pnmtops.c, which is part of the pbmplus image manipulation package.
 */

/* pnmtops.c - read a portable anymap and produce a PostScript file
**
** Copyright (C) 1989 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

# include <stdio.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/Intrinsic.h>
# include "proto.h"
# include "error.h"
# include "version.h"

#define MARGIN 0.95

typedef unsigned char xelval;
extern XImage *WidgetToXImage PROTO((Widget, XColor **, int *));
static int colorstobpp PROTO((int colors));
static void putinit PROTO(( char* name, int cols, int rows, int padright, int bps, float scale, int dpi, int pagewid, int pagehgt, int turnflag, int turnokflag, int rleflag ));
static void putitem PROTO(( void ));
static void putxelval PROTO(( xelval xv ));
static void putrest PROTO(( void ));
static void rleputbuffer PROTO(( void ));
static void rleputitem PROTO(( void ));
static void rleputxelval PROTO(( xelval xv ));
static void rleflush PROTO(( void ));
static void rleputrest PROTO(( void ));

static FILE 	*output;

void WidgetToEPS (filename, w)
    char	*filename;
    Widget	 w;
{
    XImage   *img;
    int       turnflag, turnokflag, rleflag;
    int       rows, cols, bps, padright, row, col;
    float     scale;
    int       dpi, pagewid, pagehgt;
    unsigned  i;
    int       ncells;
    XColor   *colors;
    int	      idx;

    output = fopen (filename, "w");
    if (output == NULL) {
       error ("could not open %s for writing.", filename);
       return;
    }

    img = WidgetToXImage(w, &colors, &ncells);
    for (i = 0 ; i < ncells ; i++)
       fprintf (stderr,"%d -> %u : %d %d %d\n", i, colors [i].pixel, colors [i].red, colors [i].green, colors [i].blue);

    scale = 1.0;
    turnflag = 0;
    turnokflag = 1;
    rleflag = 0;
    /* LaserWriter defaults. */
    dpi = 300;
    pagewid = 612;
    pagehgt = 762;

    cols = img -> width;
    rows = img -> height;

    /* Figure out bps. */
    bps = colorstobpp (ncells);
    if ( bps > 2 && bps < 4 ) 
	bps = 4;
    else if ( bps > 4 && bps < 8 ) 
	bps = 8;

    /* Compute padding to round cols * bps up to the nearest multiple of 8. */
    padright = ( ( ( cols * bps + 7 ) / 8 ) * 8 - cols * bps ) / bps;

    putinit(
	filename, cols, rows, padright, bps, scale, dpi, pagewid, pagehgt, 
        turnflag, turnokflag, rleflag );
    for ( row = 0; row < rows; ++row )
	{
	    /* First red. */
	for ( col = 0; col < cols; ++col ) 
            idx = XImageCellXY(img, col, row, colors, ncells);
                     
	    if ( rleflag )
	        rleputxelval( colors [idx].red/256 );
	    else
	        putxelval( colors [idx].red/256 );
	for ( col = 0; col < padright; ++col ) 
	    if ( rleflag )
	        rleputxelval( 0 );
	    else
	       putxelval( 0 );
	if ( rleflag )
	    rleflush();
	    /* Then green. */
	for ( col = 0; col < cols; ++col ) 
            idx = XImageCellXY(img, col, row, colors, ncells);

	    if ( rleflag )
		rleputxelval( colors [idx].green/256 );
            else
		putxelval( colors [idx].green/256 );
	for ( col = 0; col < padright; ++col )
	    if ( rleflag )
	       rleputxelval( 0 );
	    else
	       putxelval( 0 );
	if ( rleflag )
	    rleflush();
	    /* And blue. */
	for ( col = 0; col < cols; ++col )
            idx = XImageCellXY(img, col, row, colors, ncells);

	    if ( rleflag )
		rleputxelval( colors [idx].blue/256 );
	    else
	        putxelval( colors [idx].blue/256 );
	for ( col = 0; col < padright; ++col )
	    if ( rleflag )
	       rleputxelval( 0 );
	    else
	       putxelval( 0 );
	if ( rleflag )
	    rleflush();
        }


    if ( rleflag )
	rleputrest();
    else
	putrest();

    fclose (output);

    free(colors);

    return;
}

static int colorstobpp( colors )
   int colors;
{
    int bpp;

    if ( colors <= 1 )
        bpp = 1;
    else if ( colors <= 3 )
        bpp = 2;
    else if ( colors <= 7 )
        bpp = 3;
    else if ( colors <= 15 )
        bpp = 4;
    else if ( colors <= 31 )
        bpp = 5;
    else if ( colors <= 63 )
        bpp = 6;
    else if ( colors <= 127 )
        bpp = 7;
    else if ( colors <= 255 )
        bpp = 8;
    else
        bpp = 8;

   return bpp;
}

static int bitspersample, item, bitsperitem, bitshift, itemsperline, items;
static int rleitem, rlebitsperitem, rlebitshift;
static int repeat, itembuf[128], count, repeatitem, repeatcount;

#ifdef __STDC__
static void
putinit( char* name, int cols, int rows, int padright, int bps, float scale,
	 int dpi, int pagewid, int pagehgt, int turnflag,
	 int turnokflag, int rleflag )
#else /*__STDC__*/
static void
putinit( name, cols, rows, padright, bps, scale, dpi, pagewid, pagehgt,
	 turnflag, turnokflag, rleflag )
    char* name;
    int cols, rows, padright, bps;
    float scale;
    int dpi, pagewid, pagehgt, turnflag, turnokflag, rleflag;
#endif /*__STDC__*/
    {
    int icols, irows, devpix;
    float pixfac, scols, srows, llx, lly;

    /* Turn? */
    icols = cols;
    irows = rows;
    if ( turnflag || ( turnokflag && cols > rows ) )
	{
	turnflag = 1;
	cols = irows;
	rows = icols;
	}

    /* Figure out size. */
    devpix = dpi / 72.0 + 0.5;		/* device pixels per unit, approx. */
    pixfac = 72.0 / dpi * devpix;	/* 1, approx. */
    scols = scale * cols * pixfac;
    srows = scale * rows * pixfac;
    if ( scols > pagewid * MARGIN || srows > pagehgt * MARGIN )
	{
	if ( scols > pagewid * MARGIN )
	    {
	    scale *= pagewid / scols * MARGIN;
	    scols = scale * cols * pixfac;
	    srows = scale * rows * pixfac;
	    }
	if ( srows > pagehgt * MARGIN )
	    {
	    scale *= pagehgt / srows * MARGIN;
	    scols = scale * cols * pixfac;
	    srows = scale * rows * pixfac;
	    }
	error(
	    "warning, image too large for page, rescaling to %g", scale );
	}
    llx = ( pagewid - scols ) / 2;
    lly = ( pagehgt - srows ) / 2;

    fprintf(output, "%%!PS-Adobe-2.0 EPSF-2.0\n" );
    fprintf(output, "%%%%Creator: Velvet %s\n", VERSION );
    fprintf(output, "%%%%Title: %s\n", name );
    fprintf(output, "%%%%Pages: 1\n" );
    fprintf(output,
	"%%%%BoundingBox: %d %d %d %d\n",
	(int) llx, (int) lly,
	(int) ( llx + scols + 0.5 ), (int) ( lly + srows + 0.5 ) );
    fprintf(output, "%%%%EndComments\n" );
    if ( rleflag )
	{
	fprintf(output, "/rlestr1 1 string def\n" );
	fprintf(output, "/readrlestring {\n" );				/* s -- nr */
	fprintf(output, "  /rlestr exch def\n" );			/* - */
	fprintf(output, "  currentfile rlestr1 readhexstring pop\n" );	/* s1 */
	fprintf(output, "  0 get\n" );					/* c */
	fprintf(output, "  dup 127 le {\n" );				/* c */
	fprintf(output, "    currentfile rlestr 0\n" );			/* c f s 0 */
	fprintf(output, "    4 3 roll\n" );				/* f s 0 c */
	fprintf(output, "    1 add  getinterval\n" );			/* f s */
	fprintf(output, "    readhexstring pop\n" );			/* s */
	fprintf(output, "    length\n" );				/* nr */
	fprintf(output, "  } {\n" );					/* c */
	fprintf(output, "    256 exch sub dup\n" );			/* n n */
	fprintf(output, "    currentfile rlestr1 readhexstring pop\n" );/* n n s1 */
	fprintf(output, "    0 get\n" );				/* n n c */
	fprintf(output, "    exch 0 exch 1 exch 1 sub {\n" );		/* n c 0 1 n-1*/
	fprintf(output, "      rlestr exch 2 index put\n" );
	fprintf(output, "    } for\n" );				/* n c */
	fprintf(output, "    pop\n" );					/* nr */
	fprintf(output, "  } ifelse\n" );				/* nr */
	fprintf(output, "} bind def\n" );
	fprintf(output, "/readstring {\n" );				/* s -- s */
        fprintf(output, "  dup length 0 {\n" );				/* s l 0 */
	fprintf(output, "    3 copy exch\n" );				/* s l n s n l*/
	fprintf(output, "    1 index sub\n" );				/* s l n s n r*/
	fprintf(output, "    getinterval\n" );				/* s l n ss */
	fprintf(output, "    readrlestring\n" );			/* s l n nr */
	fprintf(output, "    add\n" );					/* s l n */
        fprintf(output, "    2 copy le { exit } if\n" );		/* s l n */
        fprintf(output, "  } loop\n" );					/* s l l */
        fprintf(output, "  pop pop\n" );				/* s */
	fprintf(output, "} bind def\n" );
	}
    else
	{
	fprintf(output, "/readstring {\n" );				/* s -- s */
	fprintf(output, "  currentfile exch readhexstring pop\n" );
	fprintf(output, "} bind def\n" );
	}

    fprintf(output, "/rpicstr %d string def\n", ( icols + padright ) * bps / 8 );
    fprintf(output, "/gpicstr %d string def\n", ( icols + padright ) * bps / 8 );
    fprintf(output, "/bpicstr %d string def\n", ( icols + padright ) * bps / 8 );

    fprintf(output, "%%%%EndProlog\n" );
    fprintf(output, "%%%%Page: 1 1\n" );
    fprintf(output, "gsave\n" );
    fprintf(output, "%g %g translate\n", llx, lly );
    fprintf(output, "%g %g scale\n", scols, srows );
    if ( turnflag )
	fprintf(output, "0.5 0.5 translate  90 rotate  -0.5 -0.5 translate\n" );
    fprintf(output, "%d %d %d\n", icols, irows, bps );
    fprintf(output, "[ %d 0 0 -%d 0 %d ]\n", icols, irows, irows );

    fprintf(output, "{ rpicstr readstring }\n" );
    fprintf(output, "{ gpicstr readstring }\n" );
    fprintf(output, "{ bpicstr readstring }\n" );
    fprintf(output, "true 3\n" );
    fprintf(output, "colorimage\n" );

    bitspersample = bps;
    itemsperline = items = 0;
    if ( rleflag )
	{
	rleitem = 0;
	rlebitsperitem = 0;
	rlebitshift = 8 - bitspersample;
	repeat = 1;
	count = 0;
	}
    else
	{
	item = 0;
	bitsperitem = 0;
	bitshift = 8 - bitspersample;
	}
    }

static void
putitem()
    {
    char* hexits = "0123456789abcdef";

    if ( itemsperline == 30 )
	{
	putc( '\n', output );
	itemsperline = 0;
	}
    putc( hexits[item >> 4], output );
    putc( hexits[item & 15], output );
    ++itemsperline;
    ++items;
    item = 0;
    bitsperitem = 0;
    bitshift = 8 - bitspersample;
    }

#if __STDC__
static void putxelval( xelval xv )
#else /*__STDC__*/
static void
putxelval( xv )
    xelval xv;
#endif /*__STDC__*/
    {
    if ( bitsperitem == 8 )
	putitem();
    item += xv << bitshift;
    bitsperitem += bitspersample;
    bitshift -= bitspersample;
    }

static void
putrest()
    {
    if ( bitsperitem > 0 )
	putitem();
    fprintf(output, "\n" );
    fprintf(output, "grestore\n" );
    fprintf(output, "showpage\n" );
    fprintf(output, "%%%%Trailer\n" );
    }

static void
rleputbuffer()
    {
    int i;

    if ( repeat )
	{
	item = 256 - count;
	putitem();
	item = repeatitem;
	putitem();
	}
    else
	{
	item = count - 1;
	putitem();
	for ( i = 0; i < count; ++i )
	    {
	    item = itembuf[i];
	    putitem();
	    }
	}
    repeat = 1;
    count = 0;
    }

static void
rleputitem()
    {
    int i;

    if ( count == 128 )
	rleputbuffer();

    if ( repeat && count == 0 )
	{ /* Still initializing a repeat buf. */
	itembuf[count] = repeatitem = rleitem;
	++count;
	}
    else if ( repeat )
	{ /* Repeating - watch for end of run. */
	if ( rleitem == repeatitem )
	    { /* Run continues. */
	    itembuf[count] = rleitem;
	    ++count;
	    }
	else
	    { /* Run ended - is it long enough to dump? */
	    if ( count > 2 )
		{ /* Yes, dump a repeat-mode buffer and start a new one. */
		rleputbuffer();
		itembuf[count] = repeatitem = rleitem;
		++count;
		}
	    else
		{ /* Not long enough - convert to non-repeat mode. */
		repeat = 0;
		itembuf[count] = repeatitem = rleitem;
		++count;
		repeatcount = 1;
		}
	    }
	}
    else
	{ /* Not repeating - watch for a run worth repeating. */
	if ( rleitem == repeatitem )
	    { /* Possible run continues. */
	    ++repeatcount;
	    if ( repeatcount > 3 )
		{ /* Long enough - dump non-repeat part and start repeat. */
		count = count - ( repeatcount - 1 );
		rleputbuffer();
		count = repeatcount;
		for ( i = 0; i < count; ++i )
		    itembuf[i] = rleitem;
		}
	    else
		{ /* Not long enough yet - continue as non-repeat buf. */
		itembuf[count] = rleitem;
		++count;
		}
	    }
	else
	    { /* Broken run. */
	    itembuf[count] = repeatitem = rleitem;
	    ++count;
	    repeatcount = 1;
	    }
	}

    rleitem = 0;
    rlebitsperitem = 0;
    rlebitshift = 8 - bitspersample;
    }

#if __STDC__
static void rleputxelval( xelval xv )
#else /*__STDC__*/
static void
rleputxelval( xv )
    xelval xv;
#endif /*__STDC__*/
    {
    if ( rlebitsperitem == 8 )
	rleputitem();
    rleitem += xv << rlebitshift;
    rlebitsperitem += bitspersample;
    rlebitshift -= bitspersample;
    }

static void
rleflush()
    {
    if ( rlebitsperitem > 0 )
	rleputitem();
    if ( count > 0 )
	rleputbuffer();
    }

static void
rleputrest()
    {
    rleflush();
    fprintf(output, "\n" );
    fprintf(output, "grestore\n" );
    fprintf(output, "showpage\n" );
    fprintf(output, "%%%%Trailer\n" );
    }
