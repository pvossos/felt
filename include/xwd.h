#ifndef XWD_H
#define XWD_H

#include <stdio.h>
#include <X11/Xlib.h>

int XImageCellXY(XImage *img, int x, int y, XColor *colors, int ncolors);

int DumpWidget(Widget widget, FILE *out);

XImage *WidgetToXImage(Widget widget, XColor **colors, int *ncolors);

#endif
