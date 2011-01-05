/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/****************************************************************************
 * 
 * File:	modal.c
 *
 * Description:	Contains routines to draw the mode shapes for a modal
 *		analysis problem.  In a lot of ways this is like an animation
 *		problem because we build a record for each mode (analogous
 *		to a time-step in animation) so we can just whip quickly
 *		through them.
 *
 ****************************************************************************/

# include <vector>
# include <stdio.h>
# include <math.h>
# include <X11/Xos.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Shell.h>
# include "Layout.h"
# include "TabGroup.h"
# include "Solution.h"
# include "problem.h"
# include "Drawing.h"
# include "util.h"
# include "allocate.h"
# include "draw3d.h"
# include "procedures.h"

# include "forward.xbm"
# include "reverse.xbm"

extern Widget toplevel;
extern XtAppContext app_context;

static unsigned	DismissButton = 1;
static unsigned ForwardButton = 2;
static unsigned ReverseButton = 3;
static unsigned SaveButton    = 4;

static Widget	modalShell;
static Widget	modal_dw;
static Widget   mode_label;
static Widget   freq_label;

static unsigned	mode = 0;
static unsigned nmodes = 0;
static unsigned nelts = 0;

struct s_record {
   float	x;
   float	y;
};

static std::vector< std::vector<s_record> > s_table;
static std::vector<s_record> orig_s_table;

static std::vector< std::vector<unsigned> > connect;
static std::vector<unsigned> num_connections;

static Matrix	freq;

static char layout_string [ ] =
"vertical { \
    8 \
    horizontal { \
       8 \
       modal_dw <+inf -100% * +inf -100%> \
       8 \
    } \
    24 \
    horizontal { \
       8 \
       dismiss \
       4 \
       save \
       ((width modal_dw - width dismiss - width save - width freq_label - \
         width mode_label - 2*width forward - 28) /2 ) <+inf> \
       reverse \
       4 \
       forward \
       ((width modal_dw - width dismiss - width save - width freq_label - \
         width mode_label - 2*width forward - 28) /2 ) <+inf> \
       mode_label \
       4 \
       freq_label \
       8 \
    } \
    8 \
}";

static String table =
"<Key>space: AutoRepeat(off) set()\n\
 <Key>Return: AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) unset() ModalAction(button)\n\
 <KeyUp>space: AutoRepeat(saved) unset() ModalAction(button)";

static void ChangeDisplayedMode (int direction)
{
   Point	points [MaxNodesPerElement];
   unsigned	i,j;
   char		buffer [20];
   Arg		args [1];

   DW_RemoveAll (modal_dw);

   DW_SetAutoRedraw (modal_dw, False);

   mode += direction;
   if (mode > nmodes)
     mode = 1;
   else if (mode < 1)
     mode = nmodes;

   if (solution -> plot_orig) {
      DW_SetLineStyle (modal_dw, DW_LineDashed);
      for (i = 0 ; i < nelts ; i++) {
         if (num_connections [i] == 2) {
            DW_DrawLine (modal_dw, orig_s_table [connect [i][0] - 1].x,
                                     orig_s_table [connect [i][0] - 1].y,
                                     orig_s_table [connect [i][1] - 1].x,
                                     orig_s_table [connect [i][1] - 1].y);
         }
         else {
            for (j = 0 ; j < num_connections [i] ; j++) {
               points [j].x = orig_s_table [connect [i][j] - 1].x;   
               points [j].y = orig_s_table [connect [i][j] - 1].y;   
            }
            points [num_connections [i]].x = orig_s_table [connect [i][0] - 1].x;
            points [num_connections [i]].y = orig_s_table [connect [i][0] - 1].y;

            DW_DrawPolygon (modal_dw, True, points, num_connections [i] + 1);
         }
      }
      DW_SetLineStyle (modal_dw, DW_LineSolid);
   }

   for (i = 0 ; i < nelts ; i++) {
      if (num_connections [i] == 2) {
         DW_DrawLine (modal_dw, s_table [mode - 1][connect [i][0] - 1].x,
                                  s_table [mode - 1][connect [i][0] - 1].y,
                                  s_table [mode - 1][connect [i][1] - 1].x,
                                  s_table [mode - 1][connect [i][1] - 1].y);
      }
      else {
         for (j = 0 ; j < num_connections [i] ; j++) {
            points [j].x = s_table [mode - 1][connect [i][j] - 1].x;   
            points [j].y = s_table [mode - 1][connect [i][j] - 1].y;   
         }
         points [num_connections [i]].x = s_table [mode - 1][connect [i][0] - 1].x;
         points [num_connections [i]].y = s_table [mode - 1][connect [i][0] - 1].y;

         DW_DrawPolygon (modal_dw, True, points, num_connections [i] + 1);
      }
   }

   DW_SetAutoRedraw (modal_dw, True);

   sprintf (buffer, "mode #%d", mode);
   XtSetArg (args [0], XtNstring, buffer);
   XtSetValues (mode_label, args, 1);

   sprintf (buffer, "freq=%g", mdata(freq,mode,1));
   XtSetArg (args [0], XtNstring, buffer);
   XtSetValues (freq_label, args, 1);

   return;
}

static void ButtonCallback (Widget w, XtPointer client_data, XtPointer call_data)
{
   unsigned	selected = *(unsigned *) client_data;

   if (selected == DismissButton) 
      XtPopdown (modalShell);
   if (selected == SaveButton) 
      DumpDrawingArea (modal_dw, "Save Mode Shape", True);
   else if (selected == ForwardButton)
      ChangeDisplayedMode (1);
   else if (selected == ReverseButton)
      ChangeDisplayedMode (-1);
}                       

static void ModalAction (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
   if (strcmp (params [0], "delete") == 0)
      ButtonCallback ((Widget) NULL, (XtPointer) &DismissButton, (XtPointer) NULL);
   else
      XtCallCallbacks (w, XtNcallback, NULL);
}

static void CreateModalShell (void)
{
   Arg			args [10];
   Widget		group [4];
   Cardinal		n;
   Widget		layout;
   Widget		dismiss;
   Widget		save;
   Widget		forward, reverse;
   Pixel		highlight;
   Pixmap		forward_pix, reverse_pix;
   XtTranslations	translations;
   static XtActionsRec  actions [ ] = {{"ModalAction", ModalAction}};

   n = 0;
   XtSetArg (args [n], XtNtitle,    "Mode shapes"); n++;
   XtSetArg (args [n], XtNiconName, "Mode shapes"); n++;
   XtSetArg (args [n], XtNallowShellResize, True); n++;
   modalShell = XtCreatePopupShell ("modalShell",topLevelShellWidgetClass,
                                     toplevel, args, n);

   n = 0;
   XtSetArg (args [n],XtNlayout,StringToLayout(toplevel, layout_string)); n++;
   layout = XtCreateManagedWidget ("layout", layoutWidgetClass, 
                                   modalShell, args, n);

   n = 0;
   XtSetArg (args [n], XtNgrid, False); n++;
   XtSetArg (args [n], XtNborderWidth, 3); n++;
   modal_dw = XtCreateManagedWidget ("modal_dw", drawingWidgetClass, 
                                       layout, args, n);

	/*
	 * create the control buttons
	 */

   dismiss = XtCreateManagedWidget ("dismiss", commandWidgetClass,
                                         layout, NULL, 0);
   save    = XtCreateManagedWidget ("save", commandWidgetClass,
                                         layout, NULL, 0);
   forward = XtCreateManagedWidget ("forward", commandWidgetClass,
                                         layout, NULL, 0);
   reverse = XtCreateManagedWidget ("reverse", commandWidgetClass,
                                         layout, NULL, 0);

	/*
	 * create a text widget for the mode number display
	 */

   n = 0;
   XtSetArg (args [n], XtNeditType, XawtextRead); n++;
   XtSetArg (args [n], XtNwidth, 100); n++;
   XtSetArg (args [n], XtNpieceSize, 32); n++;
   XtSetArg (args [n], XtNdisplayCaret, False); n++;
   mode_label = XtCreateManagedWidget ("mode_label", asciiTextWidgetClass,
                                       layout, args, n);

   n = 0;
   XtSetArg (args [n], XtNeditType, XawtextRead); n++;
   XtSetArg (args [n], XtNwidth, 150); n++;
   XtSetArg (args [n], XtNpieceSize, 32); n++;
   XtSetArg (args [n], XtNdisplayCaret, False); n++;
   freq_label = XtCreateManagedWidget ("freq_label", asciiTextWidgetClass,
                                       layout, args, n);

	/*
	 * add the callbacks to the individual buttons
	 */

   XtAddCallback (dismiss, XtNcallback, ButtonCallback, &DismissButton);
   XtAddCallback (save,    XtNcallback, ButtonCallback, &SaveButton);
   XtAddCallback (forward, XtNcallback, ButtonCallback, &ForwardButton);
   XtAddCallback (reverse, XtNcallback, ButtonCallback, &ReverseButton);

	/*
	 * set the bitmaps for the control buttons
	 */

   forward_pix = XCreateBitmapFromData (XtDisplay (toplevel), 
                                 RootWindowOfScreen (XtScreen (toplevel)), 
                                 forward_bits, forward_width, forward_height);
   reverse_pix = XCreateBitmapFromData (XtDisplay (toplevel), 
                                 RootWindowOfScreen (XtScreen (toplevel)), 
                                 reverse_bits, reverse_width, reverse_height);

   XtSetArg (args [1], XtNlabel, "");

   XtSetArg (args [0], XtNbitmap, forward_pix);
   XtSetValues (forward, args, 2);

   XtSetArg (args [0], XtNbitmap, reverse_pix);
   XtSetValues (reverse, args, 2);

  	/*
	 * create a tab group for the buttons along the bottom of the shell
	 */

   group [0] = dismiss;
   group [1] = save;
   group [2] = reverse;
   group [3] = forward;

   XtSetArg (args [0], XtNborderColor, &highlight);
   XtGetValues (layout, args, 1);

   CreateTabGroup (modalShell, group, 4, highlight, True);

   XtRealizeWidget (modalShell);

   XtAppAddActions (app_context, actions, 1);

   translations = XtParseTranslationTable (table);
   XtOverrideTranslations (forward, translations);
   XtOverrideTranslations (reverse, translations);
   XtOverrideTranslations (dismiss, translations);
   XtOverrideTranslations (save,    translations);

   AddDeleteWindowProtocol (modalShell, "ModalAction(delete)");

   return; 
}

static void SetupArrays (Matrix phi, Element *element, unsigned int numelts, unsigned int numnodes)
{
   unsigned	i,j;

	/*
	 * create space for the connectivity table
	 */

   num_connections.clear();
   connect.clear();

   nelts = numelts;
 
   num_connections.resize(numelts);
   connect.resize(numelts);

   for (i = 0 ; i < numelts ; i++) {
      num_connections [i] = element [i+1] -> definition -> shapenodes;

      connect[i].resize(num_connections[i]);

      for (j = 0 ; j < num_connections [i] ; j++)
         connect [i][j] = element[i+1] -> node[j+1] -> number;
   } 

	/*	
	 * set-up the record of the nodal locations in each mode
	 */
	
   s_table.clear();
   orig_s_table.clear();

   orig_s_table.resize(numnodes);

   nmodes = MatrixRows (phi);

   s_table.resize(nmodes);
   for (i = 0 ; i < nmodes ; i++)
       s_table[i].resize(numnodes);

   return;
}

static int  	first_time = 1;

void DrawModeShapes (Matrix phi, Matrix lambda, Node *node, Element *element, unsigned int numnodes, unsigned int numelts)
{
   unsigned	i,j;
   float	x_max, x_min,
		y_max, y_min;
   float	maxX, minX, 
                maxY, minY, 
                Xscale, Yscale;
   float	x,y;
   Dimension	width, height;

   if (first_time) {
      CreateModalShell ();
      first_time = 0;
   }

   freq = CreateCopyMatrix (lambda);

	/*
	 * find the max and min displaced coordinates
	 */
   
   x_max = x_min = node[1] -> x + 
                   MatrixData (phi) [1][1]*solution -> magnify;
   y_max = y_min = node[1] -> y + 
                   MatrixData (phi) [1][2]*solution -> magnify; 

   for (i = 1 ; i <= MatrixRows (phi) ; i++) {
      for (j = 1 ; j <= MatrixCols (phi)/2 ; j++) {

         x = node[j] -> x+MatrixData (phi) [i][2*j - 1]*solution -> magnify;
         y = node[j] -> y+MatrixData (phi) [i][2*j]*solution -> magnify;

         if (x > x_max)
            x_max = x;
         else if (x < x_min)
            x_min = x;

         if (y > y_max) 
            y_max = y;
         else if (y < y_min) 
            y_min = y;
      }
   }

   if (x_min != x_max) {
      minX = x_min - 0.05*(x_max - x_min); 
      maxX = x_max + 0.05*(x_max - x_min);
   }
   else {
      minX = x_min - 0.05*(y_max - y_min);
      maxX = x_max + 0.05*(y_max - y_min);
   }

   if (y_min != y_max) {
      minY = y_min - 0.05*(y_max - y_min);
      maxY = y_max + 0.05*(y_max - y_min);
   }
   else {
      minY = y_min - 0.05*(x_max - x_min);
      maxY = y_max + 0.05*(x_max - x_min);
   }
   
   InitializeDrawingShell (modalShell, modal_dw, minX, maxX, minY, maxY,
                           &Xscale, &Yscale, &width, &height);

   SetFocus (XtNameToWidget (modalShell, "layout.forward"));

   SetupArrays (phi, element, numelts, numnodes);

   for (j = 1 ; j <= numnodes ; j++) {
      orig_s_table [node[j] -> number - 1].x = node[j] -> x;
      orig_s_table [node[j] -> number - 1].y = node[j] -> y;
      for (i = 1 ; i <= nmodes ; i++) {
         x = node[j] -> x +
             MatrixData (phi) [i][2*j - 1]*solution -> magnify;
         y = node[j] -> y +
             MatrixData (phi) [i][2*j]*solution -> magnify;

         s_table [i-1][node[j] -> number - 1].x = x;
         s_table [i-1][node[j] -> number - 1].y = y;
      }
   }

   DW_SetForeground (modal_dw, "black");

   mode = 0;
   ChangeDisplayedMode (1);
}

void DrawModeShapes3D (Matrix phi, Matrix lambda, Node *node, Element *element, unsigned int numnodes, unsigned int numelts)
{
   unsigned	i,j;
   float	maxX, minX, 
                maxY, minY, 
                maxZ, minZ, 
		Xscale, Yscale;
   float	xdiff, ydiff;
   float	x,y,z,
		sx,sy;
   Dimension	width, height;

   if (first_time) {
      CreateModalShell ();
      first_time = 0;
   }

   freq = CreateCopyMatrix (lambda);

   maxX = minX = node[1] -> x +
                 MatrixData (phi) [1][1]*solution -> magnify;
   maxY = minY = node[1] -> y +
                 MatrixData (phi) [1][2]*solution -> magnify;
   maxZ = minZ = node[1] -> z +
                 MatrixData (phi) [1][3]*solution -> magnify;

   for (i = 1 ; i <= MatrixRows (phi) ; i++) {
      for (j = 1 ; j <= MatrixCols (phi) / 3 ; j++) {

         x = node[j] -> x +
                 MatrixData (phi) [i][3*j - 2]*solution -> magnify;
         y = node[j] -> y +
                 MatrixData (phi) [i][3*j - 1]*solution -> magnify;
         z = node[j] -> z +
                 MatrixData (phi) [i][3*j]*solution -> magnify;

         if (x > maxX) maxX = x;
         else if (x < minX) minX = x;

         if (y > maxY) maxY = y;
         else if (y < minY) minY = y;

         if (z > maxZ) maxZ = z;
         else if (z < minZ) minY = z;
      }
   }

   Setup3D (minX,maxX,minY,maxY,minZ,maxZ);

   xdiff = maxX - minX;
   ydiff = maxY - minY;

   SetupArrays (phi, element, numelts, numnodes);

   for (j = 1 ; j <= numnodes ; j++) {

      Convert3Dto2D (node[j] -> x, node[j] -> y, node[j] -> z, 
                     xdiff, ydiff, &sx, &sy);
      orig_s_table [node[j] -> number - 1].x = sx;
      orig_s_table [node[j] -> number - 1].y = sy;

      for (i = 1 ; i <= nmodes ; i++) {
         x = node[j] -> x +
             MatrixData (phi) [i][3*j - 2]*solution -> magnify;
         y = node[j] -> y +
             MatrixData (phi) [i][3*j - 1]*solution -> magnify;
         z = node[j] -> z +
             MatrixData (phi) [i][3*j]*solution -> magnify;

         Convert3Dto2D (x, y, z, xdiff, ydiff, &sx, &sy);

         if (i == 1 && j == 1) {
            maxX = minX = sx;
            maxY = minY = sy;
         }
         else {
            if (sx > maxX) maxX = sx;
            else if (sx < minX) minX = sx;

            if (sy > maxY) maxY = sy;
            else if (sy < minY) minY = sy;
         } 
           
         s_table [i-1][node[j] -> number - 1].x = sx;
         s_table [i-1][node[j] -> number - 1].y = sy;
      }
   }

   if (maxX != minX) {
      maxX += 0.05*(maxX - minX);
      minX -= 0.05*(maxX - minX);
   }
   else {
      maxX += 0.05*(maxY - minY);
      minX -= 0.05*(maxY - minY);
   }

   if (maxY != minY) {
      maxY += 0.05*(maxY - minY);
      minY -= 0.05*(maxY - minY);
   }
   else {
      maxY += 0.05*(maxX - minX);
      minY -= 0.05*(maxX - minX);
   }

   InitializeDrawingShell (modalShell, modal_dw, minX, maxX, minY, maxY,
                           &Xscale, &Yscale, &width, &height);

   SetFocus (XtNameToWidget (modalShell, "layout.forward"));

   DW_SetForeground (modal_dw, "black");

   mode = 0;
   ChangeDisplayedMode (1);
}
