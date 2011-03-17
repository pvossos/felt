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
 * File:	animate.c
 *
 * Description:	Contains routines to animate a structural dynamics problem.
 *		You pretty much have to set-up the animation special -
 *		can't be just from a previous solve - because you have to
 *		make sure that there are displacement records for all nodes
 *		in the Tx and Ty DOFs.  Pushing play registers a work
 *		procedure that actually does the animating.  Stop removes
 *		the work proc and leaves the drawing in its last state.
 *		Both 2d and 3d line drawing are supported ... 
 *
 ****************************************************************************/

# include <vector>
# include <stdio.h>
# include <math.h>
# include <unistd.h>
# include <X11/Xos.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Repeater.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Shell.h>
# include "Layout.h"
# include "TabGroup.h"
# include "Solution.h"
# include "problem.h"
# include "Drawing.h"
# include "util.h"
# include "draw3d.h"
# include "procedures.h"

# include "forward.xbm"
# include "reverse.xbm"
# include "fforward.xbm"
# include "rreverse.xbm"
# include "stop.xbm"

# define INITIAL_DELAY 60000
# define DELAY_INCREMENT 5000

extern Widget toplevel;
extern XtAppContext app_context;

static unsigned	DismissButton = 1;
static unsigned ForwardButton = 2;
static unsigned ReverseButton = 3;
static unsigned StopButton    = 4;
static unsigned RReverseButton= 5;
static unsigned FForwardButton=6;

static Widget	animateShell;
static Widget	animate_dw;
static Widget   step_label;

static XtWorkProcId wpid = 0;

static int delay = INITIAL_DELAY;

static unsigned step;

struct s_record {
   float	x;
   float	y;
};

static std::vector< std::vector<s_record> > s_table;
static std::vector< std::vector<unsigned> > connect;
static std::vector<unsigned> num_connections;

static char layout_string [ ] =
"vertical { \
    8 \
    horizontal { \
       8 \
       animate_dw <+inf -100% * +inf -100%> \
       8 \
    } \
    24 \
    horizontal { \
       8 \
       dismiss \
       ((width animate_dw - width dismiss - \
         width step_label - 5*width stop - 16) /2 ) <+inf> \
       rreverse \
       4 \
       reverse \
       4 \
       stop \
       4 \
       forward \
       4 \
       fforward \
       ((width animate_dw - width dismiss - \
         width step_label - 5*width stop - 16) /2 ) <+inf> \
       step_label \
       8 \
    } \
    8 \
}";

static String table =
"<Key>space: AutoRepeat(off) set()\n\
 <Key>Return: AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) unset() AnimateAction(button)\n\
 <KeyUp>space: AutoRepeat(saved) unset() AnimateAction(button)";

static String repeater_table =
"<Key>space: set()\n\
 <Key>Return: set()\n\
 <KeyUp>Return: unset() AnimateAction(button)\n\
 <KeyUp>space: unset() AnimateAction(button)";

static Boolean AnimateOneStep (XtPointer client_data)
{
   struct timeval    timeout;
   Point	     points [MaxNodesPerElement];
   unsigned	     i,j;
   int		     direction = *((int *) &client_data);
   char		     buffer [20];
   Arg		     args [1];

   timeout.tv_sec = 0;
   timeout.tv_usec = delay;

   select (0, 0, 0, 0, &timeout);

   DW_RemoveAll (animate_dw);

   DW_SetAutoRedraw (animate_dw, False);

   for (i = 0 ; i < connect.size() ; i++) {
      if (num_connections [i] == 2) {
         DW_DrawLine (animate_dw, s_table [step - 1][connect [i][0] - 1].x,
                                  s_table [step - 1][connect [i][0] - 1].y,
                                  s_table [step - 1][connect [i][1] - 1].x,
                                  s_table [step - 1][connect [i][1] - 1].y);
      }
      else {
         for (j = 0 ; j < num_connections [i] ; j++) {
            points [j].x = s_table [step - 1][connect [i][j] - 1].x;   
            points [j].y = s_table [step - 1][connect [i][j] - 1].y;   
         }
         points [num_connections [i]].x = s_table [step - 1][connect [i][0] - 1].x;
         points [num_connections [i]].y = s_table [step - 1][connect [i][0] - 1].y;

         DW_DrawPolygon (animate_dw, True, points, num_connections [i] + 1);
      }
   }

   DW_SetAutoRedraw (animate_dw, True);

   sprintf (buffer, "%g", (step - 1)*analysis.step);
   XtSetArg (args [0], XtNstring, buffer);
   XtSetValues (step_label, args, 1);

   step += direction;
   if (step > s_table.size())
     step = 1;
   else if (step < 1)
     step = s_table.size();

   return False;
}

static void ChangeSpeed (int increment)
{
   delay += increment;

   if (delay < 0)
      delay = 0;
}

static void ButtonCallback (Widget w, XtPointer client_data, XtPointer call_data)
{
   unsigned	selected = *(unsigned *) client_data;

   if (selected == DismissButton) {
      if (wpid) {
         XtRemoveWorkProc (wpid);
         wpid = 0;
      }

      XtPopdown (animateShell);
   }
   else if (selected == FForwardButton)
      ChangeSpeed (-DELAY_INCREMENT);
   else if (selected == RReverseButton)
      ChangeSpeed (DELAY_INCREMENT);
   else if (selected == ForwardButton && !wpid)
      wpid = XtAppAddWorkProc (app_context, AnimateOneStep, (XtPointer) 1);
   else if (selected == ReverseButton && !wpid)
      wpid = XtAppAddWorkProc (app_context, AnimateOneStep, (XtPointer) -1);
   else if (selected == StopButton && wpid) {
      XtRemoveWorkProc (wpid); 
      wpid = 0;
   }
}                       

static void AnimateAction (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
   if (strcmp (params [0], "delete") == 0)
      ButtonCallback ((Widget) NULL, (XtPointer) &DismissButton, (XtPointer) NULL);
   else
      XtCallCallbacks (w, XtNcallback, NULL);
}

static void CreateAnimationShell (void)
{
   Arg			args [10];
   Cardinal		n;
   Widget		group [6];
   Pixel		highlight;
   Widget		layout;
   Widget		dismiss;
   Widget		stop, forward, reverse, fforward, rreverse;
   Pixmap		stop_pix, forward_pix, reverse_pix;
   Pixmap		fforward_pix, rreverse_pix;
   XtTranslations	translations;
   static XtActionsRec  actions [ ] = {{"AnimateAction", AnimateAction}};
   

   n = 0;
   XtSetArg (args [n], XtNtitle,    "Animation"); n++;
   XtSetArg (args [n], XtNiconName, "Animation"); n++;
   XtSetArg (args [n], XtNallowShellResize, True); n++;
   animateShell = XtCreatePopupShell ("animateShell",topLevelShellWidgetClass,
                                      toplevel, args, n);

   n = 0;
   XtSetArg (args [n],XtNlayout,StringToLayout(toplevel, layout_string)); n++;
   layout = XtCreateManagedWidget ("layout", layoutWidgetClass, 
                                   animateShell, args, n);

   n = 0;
   XtSetArg (args [n], XtNgrid, False); n++;
   XtSetArg (args [n], XtNborderWidth, 3); n++;
   animate_dw = XtCreateManagedWidget ("animate_dw", drawingWidgetClass, 
                                       layout, args, n);

	/*
	 * create the control buttons
	 */

   dismiss = XtCreateManagedWidget ("dismiss", commandWidgetClass,
                                         layout, NULL, 0);
   stop = XtCreateManagedWidget ("stop", commandWidgetClass,
                                         layout, NULL, 0);
   forward = XtCreateManagedWidget ("forward", commandWidgetClass,
                                         layout, NULL, 0);
   fforward = XtCreateManagedWidget ("fforward", repeaterWidgetClass,
                                         layout, NULL, 0);
   reverse = XtCreateManagedWidget ("reverse", commandWidgetClass,
                                         layout, NULL, 0);
   rreverse = XtCreateManagedWidget ("rreverse", repeaterWidgetClass,
                                         layout, NULL, 0);

	/*
	 * create a text widget for the time read-out
	 */

   n = 0;
   XtSetArg (args [n], XtNeditType, XawtextRead); n++;
   XtSetArg (args [n], XtNwidth, 80); n++;
   XtSetArg (args [n], XtNpieceSize, 32); n++;
   XtSetArg (args [n], XtNdisplayCaret, False); n++;
   step_label = XtCreateManagedWidget ("step_label", asciiTextWidgetClass,
                                       layout, args, n);

	/*
	 * add the callbacks to the individual buttons
	 */

   XtAddCallback (dismiss, XtNcallback, ButtonCallback, &DismissButton);
   XtAddCallback (forward, XtNcallback, ButtonCallback, &ForwardButton);
   XtAddCallback (reverse, XtNcallback, ButtonCallback, &ReverseButton);
   XtAddCallback (fforward, XtNcallback, ButtonCallback, &FForwardButton);
   XtAddCallback (rreverse, XtNcallback, ButtonCallback, &RReverseButton);
   XtAddCallback (stop, XtNcallback, ButtonCallback, &StopButton);

	/*
	 * create a tab group mechanism
	 */

   group [0] = dismiss;
   group [1] = rreverse;
   group [2] = reverse;
   group [3] = stop;
   group [4] = forward;
   group [5] = fforward;

   XtSetArg (args [0], XtNborderColor, &highlight);
   XtGetValues (layout, args, 1);

   CreateTabGroup (animateShell, group, 6, highlight, True);

	/*
	 * set the bitmaps for the control buttons
	 */

   stop_pix = XCreateBitmapFromData (XtDisplay (toplevel), 
                                 RootWindowOfScreen (XtScreen (toplevel)), 
                                 stop_bits, stop_width, stop_height);
   forward_pix = XCreateBitmapFromData (XtDisplay (toplevel), 
                                 RootWindowOfScreen (XtScreen (toplevel)), 
                                 forward_bits, forward_width, forward_height);
   reverse_pix = XCreateBitmapFromData (XtDisplay (toplevel), 
                                 RootWindowOfScreen (XtScreen (toplevel)), 
                                 reverse_bits, reverse_width, reverse_height);
   fforward_pix = XCreateBitmapFromData (XtDisplay (toplevel), 
                                 RootWindowOfScreen (XtScreen (toplevel)), 
                                 fforward_bits,fforward_width,fforward_height);
   rreverse_pix = XCreateBitmapFromData (XtDisplay (toplevel), 
                                 RootWindowOfScreen (XtScreen (toplevel)), 
                                 rreverse_bits,rreverse_width,rreverse_height);

   XtSetArg (args [1], XtNlabel, "");

   XtSetArg (args [0], XtNbitmap, stop_pix);
   XtSetValues (stop, args, 2);

   XtSetArg (args [0], XtNbitmap, forward_pix);
   XtSetValues (forward, args, 2);

   XtSetArg (args [0], XtNbitmap, reverse_pix);
   XtSetValues (reverse, args, 2);

   XtSetArg (args [0], XtNbitmap, fforward_pix);
   XtSetValues (fforward, args, 2);

   XtSetArg (args [0], XtNbitmap, rreverse_pix);
   XtSetValues (rreverse, args, 2);

   XtRealizeWidget (animateShell);

   XtAppAddActions (app_context, actions, 1);

   translations = XtParseTranslationTable (table);
   XtOverrideTranslations (forward, translations);
   XtOverrideTranslations (reverse, translations);
   XtOverrideTranslations (stop, translations);
   XtOverrideTranslations (dismiss, translations);

   translations = XtParseTranslationTable (repeater_table);
   XtOverrideTranslations (fforward, translations);
   XtOverrideTranslations (rreverse, translations);

   AddDeleteWindowProtocol (animateShell, "AnimateAction(delete)");

   return; 
}

static void SetupArrays (Matrix dtable, const Element *element, unsigned int numelts, unsigned int numnodes)
{
   unsigned	i,j;

	/*
	 * create space for the connectivity table
	 */

   num_connections.clear();
   connect.clear();
   
   num_connections.resize(numelts);
   connect.resize(numelts);

   for (i = 0 ; i < numelts ; i++) {
      num_connections [i] = element [i+1] -> definition -> shapenodes;

      connect[i].resize(num_connections[i]);

      for (j = 0 ; j < num_connections [i] ; j++)
         connect [i][j] = element[i+1] -> node[j+1] -> number;
   } 

	/*	
	 * set-up the record of node-time displacements
	 */
	
   s_table.clear();
   unsigned nsteps = MatrixRows (dtable);

   s_table.resize(nsteps);
   for (i =0; i < nsteps; i++)
       s_table[i].resize(numnodes);

   return;
}

static int  	first_time = 1;

void AnimateStructure (Matrix dtable, const Node *node, const Element *element, unsigned int numnodes, unsigned int numelts)
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
      CreateAnimationShell ();
      first_time = 0;
   }

	/*
	 * find the max and min displaced coordinates
	 */

   x_max = x_min = node[1] -> x + 
                   MatrixData (dtable) [1][1]*solution -> magnify;
   y_max = y_min = node[1] -> y + 
                   MatrixData (dtable) [1][2]*solution -> magnify; 

   for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
      for (j = 1 ; j <= MatrixCols (dtable)/2 ; j++) {

         x = node[j] -> x+MatrixData (dtable) [i][2*j - 1]*solution -> magnify;
         y = node[j] -> y+MatrixData (dtable) [i][2*j]*solution -> magnify;

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
   
   InitializeDrawingShell (animateShell, animate_dw, minX, maxX, minY, maxY,
                           &Xscale, &Yscale, &width, &height);

   SetFocus (XtNameToWidget (animateShell, "layout.forward"));

   SetupArrays (dtable, element, numelts, numnodes);

   for (i = 1 ; i <= s_table.size() ; i++) {
      for (j = 1 ; j <= numnodes ; j++) {
         x = node[j] -> x +
             MatrixData (dtable) [i][2*j - 1]*solution -> magnify;
         y = node[j] -> y +
             MatrixData (dtable) [i][2*j]*solution -> magnify;

         s_table [i-1][node[j] -> number - 1].x = x;
         s_table [i-1][node[j] -> number - 1].y = y;
      }
   }

   DW_SetForeground (animate_dw, "black");

   step = 1;
   AnimateOneStep ((XtPointer) 1);
}

void AnimateStructure3D (Matrix dtable, const Node *node, const Element *element, unsigned int numnodes, unsigned int numelts)
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
      CreateAnimationShell ();
      first_time = 0;
   }

   maxX = minX = node[1] -> x +
                 MatrixData (dtable) [1][1]*solution -> magnify;
   maxY = minY = node[1] -> y +
                 MatrixData (dtable) [1][2]*solution -> magnify;
   maxZ = minZ = node[1] -> z +
                 MatrixData (dtable) [1][3]*solution -> magnify;

   for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
      for (j = 1 ; j <= MatrixCols (dtable) / 3 ; j++) {

         x = node[j] -> x +
                 MatrixData (dtable) [i][3*j - 2]*solution -> magnify;
         y = node[j] -> y +
                 MatrixData (dtable) [i][3*j - 1]*solution -> magnify;
         z = node[j] -> z +
                 MatrixData (dtable) [i][3*j]*solution -> magnify;

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

   SetupArrays (dtable, element, numelts, numnodes);

   for (i = 1 ; i <= s_table.size() ; i++) {
      for (j = 1 ; j <= numnodes ; j++) {
         x = node[j] -> x +
             MatrixData (dtable) [i][3*j - 2]*solution -> magnify;
         y = node[j] -> y +
             MatrixData (dtable) [i][3*j - 1]*solution -> magnify;
         z = node[j] -> z +
             MatrixData (dtable) [i][3*j]*solution -> magnify;

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

   if (minY != maxY) { 
      maxY += 0.05*(maxY - minY);
      minY -= 0.05*(maxY - minY);
   }
   else {
      maxY += 0.05*(maxX - minX);
      minY -= 0.05*(maxX - minX);
   }

   InitializeDrawingShell (animateShell, animate_dw, minX, maxX, minY, maxY,
                           &Xscale, &Yscale, &width, &height);

   SetFocus (XtNameToWidget (animateShell, "layout.forward"));

   DW_SetForeground (animate_dw, "black");

   step = 1;
   AnimateOneStep ((XtPointer) 1);
}
