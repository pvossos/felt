/* $XConsortium: xwd.c,v 1.56 91/07/25 18:00:15 rws Exp $ */

/* Copyright 1987 Massachusetts Institute of Technology */

/*
 * xwd.c MIT Project Athena, X Window system window raster image dumper.
 *
 */

/*
 * This copy of xwd.c contains no main, it was hacked out for inclusion
 * of this functionality into XFelt ... basically we just took out the
 * the functionality that we didn't need to form a simple window
 * dumper.
 */

# include <stdio.h>
# include <errno.h>

# include <X11/Xos.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/Xmu/WinUtil.h>
# include <X11/XWDFile.h>
# include <X11/Intrinsic.h>
# include <X11/Xaw/AsciiText.h>
# include <stdlib.h>
# include "xwd.h"
# include "error.h"

int Image_Size ( );
int Get_XColors ( );
void _swaplong ( );
void _swapshort ( );

Display *dpy;
int screen;

int DumpWidget(Widget widget, FILE *out)
{
    unsigned long swaptest = 1;
    XColor *colors;
    unsigned buffer_size;
    int win_name_size;
    int header_size;
    int ncolors, i;
    char *win_name;
    XWindowAttributes win_info;
    XImage *image;
    int absx, absy, x, y;
    unsigned width, height;
    int dwidth, dheight;
    int bw;
    Window dummywin;
    XWDFileHeader header;
    Window window;

    window = XtWindow (widget);
    dpy = XtDisplay (widget);
    screen = DefaultScreen (dpy);
    
    /*
     * Get the parameters of the window being dumped.
     */

    if(!XGetWindowAttributes(dpy, window, &win_info))  {
       error("could not get window attributes");
       return 1;
    }

    /* handle any frame window */
    if (!XTranslateCoordinates (dpy, window, RootWindow (dpy, screen), 0, 0,
				&absx, &absy, &dummywin)) {
        error ("unable to perform translation");
	return 1;
    }
    win_info.x = absx;
    win_info.y = absy;
    width = win_info.width;
    height = win_info.height;
    bw = 0;

    absx -= win_info.border_width;
    absy -= win_info.border_width;
    bw = win_info.border_width;
    width += (2 * bw);
    height += (2 * bw);

    dwidth = DisplayWidth (dpy, screen);
    dheight = DisplayHeight (dpy, screen);


    /* clip to window */
    if (absx < 0) width += absx, absx = 0;
    if (absy < 0) height += absy, absy = 0;
    if (absx + width > dwidth) width = dwidth - absx;
    if (absy + height > dheight) height = dheight - absy;

    win_name = "FElt_XWD";

    /* sizeof(char) is included for the null string terminator. */
    win_name_size = strlen(win_name) + sizeof(char);

    /*
     * Snarf the pixmap with XGetImage.
     */

    x = absx - win_info.x;
    y = absy - win_info.y;
    image = XGetImage (dpy, window, x, y, width, height, AllPlanes, ZPixmap);
    if (!image) {
        error ("unable to capture image");
        return 1;
    }

    /*
     * Determine the pixmap size.
     */
    buffer_size = Image_Size(image);


    ncolors = Get_XColors(&win_info, &colors);
    /*
     * Calculate header size.
     */
    header_size = sizeof(header) + win_name_size;

    /*
     * Write out header information.
     */
    header.header_size = (CARD32) header_size;
    header.file_version = (CARD32) XWD_FILE_VERSION;
    header.pixmap_format = (CARD32) ZPixmap;
    header.pixmap_depth = (CARD32) image->depth;
    header.pixmap_width = (CARD32) image->width;
    header.pixmap_height = (CARD32) image->height;
    header.xoffset = (CARD32) image->xoffset;
    header.byte_order = (CARD32) image->byte_order;
    header.bitmap_unit = (CARD32) image->bitmap_unit;
    header.bitmap_bit_order = (CARD32) image->bitmap_bit_order;
    header.bitmap_pad = (CARD32) image->bitmap_pad;
    header.bits_per_pixel = (CARD32) image->bits_per_pixel;
    header.bytes_per_line = (CARD32) image->bytes_per_line;
    header.visual_class = (CARD32) win_info.visual->class;
    header.red_mask = (CARD32) win_info.visual->red_mask;
    header.green_mask = (CARD32) win_info.visual->green_mask;
    header.blue_mask = (CARD32) win_info.visual->blue_mask;
    header.bits_per_rgb = (CARD32) win_info.visual->bits_per_rgb;
    header.colormap_entries = (CARD32) win_info.visual->map_entries;
    header.ncolors = ncolors;
    header.window_width = (CARD32) win_info.width;
    header.window_height = (CARD32) win_info.height;
    header.window_x = absx;
    header.window_y = absy;
    header.window_bdrwidth = (CARD32) win_info.border_width;

    if (*(char *) &swaptest) {
	_swaplong((char *) &header, sizeof(header));
	for (i = 0; i < ncolors; i++) {
	    _swaplong((char *) &colors[i].pixel, sizeof(long));
	    _swapshort((char *) &colors[i].red, 3 * sizeof(short));
	}
    }

    (void) fwrite((char *)&header, sizeof(header), 1, out);
    (void) fwrite(win_name, win_name_size, 1, out);

    /*
     * Write out the color maps, if any
     */

    (void) fwrite((char *) colors, sizeof(XColor), ncolors, out);

    /*
     * Write out the buffer.
     */

    /*
     *    This copying of the bit stream (data) to a file is to be replaced
     *  by an Xlib call which hasn't been written yet.  It is not clear
     *  what other functions of xwd will be taken over by this (as yet)
     *  non-existant X function.
     */
    (void) fwrite(image->data, (int) buffer_size, 1, out);

    /*
     * free the color buffer.
     */

    if(ncolors > 0) free(colors);

    /*
     * Free image
     */
    XDestroyImage(image);

    return 0;
}

XImage *WidgetToXImage(Widget widget, XColor **colors, int *ncolors)
{
    unsigned long swaptest = 1;
    unsigned buffer_size;
    int i;
    XWindowAttributes win_info;
    XImage *image;
    int absx, absy, x, y;
    unsigned width, height;
    int dwidth, dheight;
    int bw;
    Window dummywin;
    Window window;

    window = XtWindow (widget);
    dpy    = XtDisplay (widget);
    screen = DefaultScreen (dpy);
    
    /*
     * Get the parameters of the window being dumped.
     */

    if(!XGetWindowAttributes(dpy, window, &win_info))  {
       error("could not get window attributes");
       return NULL;
    }

    /* handle any frame window */
    if (!XTranslateCoordinates (dpy, window, RootWindow (dpy, screen), 0, 0,
				&absx, &absy, &dummywin)) {
        error ("unable to perform translation");
	return NULL;
    }
    win_info.x = absx;
    win_info.y = absy;
    width = win_info.width;
    height = win_info.height;
    bw = 0;

    absx -= win_info.border_width;
    absy -= win_info.border_width;
    bw = win_info.border_width;
    width += (2 * bw);
    height += (2 * bw);

    dwidth = DisplayWidth (dpy, screen);
    dheight = DisplayHeight (dpy, screen);


    /* clip to window */
    if (absx < 0) width += absx, absx = 0;
    if (absy < 0) height += absy, absy = 0;
    if (absx + width > dwidth) width = dwidth - absx;
    if (absy + height > dheight) height = dheight - absy;


    /*
     * Snarf the pixmap with XGetImage.
     */

    x = absx - win_info.x;
    y = absy - win_info.y;
    image = XGetImage (dpy, window, x, y, width, height, AllPlanes, ZPixmap);
    if (!image) {
        error ("unable to capture image");
        return NULL;
    }

    /*
     * Determine the pixmap size.
     */
    buffer_size = Image_Size(image);


    if (ncolors)
       *ncolors = Get_XColors(&win_info, colors);

    if (*(char *) &swaptest) {
	for (i = 0; i < *ncolors; i++) {
	    _swaplong((char *) &((*colors)[i].pixel), sizeof(long));
	    _swapshort((char *) &((*colors)[i].red), 3 * sizeof(short));
	}
    }

    return image;
}

int XImageCellXY(XImage *img, int x, int y, XColor *colors, int ncolors)
{
   int		 i;
   unsigned long point;
   int		 idx;

   point = XGetPixel(img, x, y);
   _swaplong((char *) &point, sizeof(long));
   fprintf (stderr,"%u\n", point);
   for (i = 0 ; i < ncolors ; i++)
      if (point == colors [i].pixel)
         return i; 

   return 0;
}

/*
 * Determine the pixmap size.
 */

int Image_Size(image)
     XImage *image;
{
    if (image->format != ZPixmap)
      return(image->bytes_per_line * image->height * image->depth);

    return(image->bytes_per_line * image->height);
}

#define lowbit(x) ((x) & (~(x) + 1))

/*
 * Get the XColors of all pixels in image - returns # of colors
 */
int Get_XColors(win_info, colors)
     XWindowAttributes *win_info;
     XColor **colors;
{
    int i, ncolors;
    Colormap cmap = win_info->colormap;

    ncolors = win_info->visual->map_entries;
    if (!(*colors = (XColor *) malloc (sizeof(XColor) * ncolors))) {
       error ("could not allocate color memory");
       return 1;
    }
    if (win_info->visual->class == DirectColor ||
	win_info->visual->class == TrueColor) {
	Pixel red, green, blue, red1, green1, blue1;

	red = green = blue = 0;
	red1 = lowbit(win_info->visual->red_mask);
	green1 = lowbit(win_info->visual->green_mask);
	blue1 = lowbit(win_info->visual->blue_mask);
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = red|green|blue;
	  (*colors)[i].pad = 0;
	  red += red1;
	  if (red > win_info->visual->red_mask)
	    red = 0;
	  green += green1;
	  if (green > win_info->visual->green_mask)
	    green = 0;
	  blue += blue1;
	  if (blue > win_info->visual->blue_mask)
	    blue = 0;
	}
    } else {
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = i;
	  (*colors)[i].pad = 0;
	}
    }

    XQueryColors(dpy, cmap, *colors, ncolors);
    
    return(ncolors);
}

void _swapshort (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
}

void _swaplong (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
}
