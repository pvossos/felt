/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson

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

/************************************************************************
 * File:	graph.c							*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		time-displacement graphing plots			*
 ************************************************************************/

# include <stdio.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Command.h>
# include <X11/Shell.h>
# include "Layout.h"
# include "Drawing.h"
# include "util.h"
# include "fe.h"
# include "error.h"
# include "problem.h"
# include "allocate.h"
# include "procedures.h"
# include "TabGroup.h"

# define MAJOR_TICK	10.0
# define MINOR_TICK	5.0
# define LEGEND_LINE	40.0
# define LEGEND_WIDTH	125.0
# define WIDTH		400.0
# define HEIGHT		300.0
# define BUFFER		75.0

	/*	
 	 * some servers have all of these fonts, but some only
	 * seem to have one set of the sizes or the other
	 */

# define LABEL_FONT1   "-adobe-helvetica-medium-r-normal--10*"
# define LABEL_FONT2   "-adobe-helvetica-medium-r-normal--11*"
# define LEGEND_FONT1  "-adobe-helvetica-medium-r-normal--14*"
# define LEGEND_FONT2  "-adobe-helvetica-medium-r-normal--14*"
# define TITLE_FONT1   "-adobe-helvetica-medium-r-normal--24*"
# define TITLE_FONT2   "-adobe-helvetica-medium-r-normal--25*"
# define AXIS_FONT1    "-adobe-helvetica-medium-r-normal--18*"
# define AXIS_FONT2    "-adobe-helvetica-medium-r-normal--17*"

static char		*axis_font_name;
static char		*label_font_name;
static char		*title_font_name;
static char		*legend_font_name;
static XFontStruct	*axis_font;
static XFontStruct	*label_font;
static XFontStruct	*title_font;
static XFontStruct	*legend_font;

extern Widget		toplevel;
extern XtAppContext	app_context;

static Widget	tdShell = NULL;
static Widget	td_dw;
static Widget	fpShell = NULL;
static Widget	fp_dw;
static Widget   forceShell = NULL;
static Widget   force_dw;

static char	*colors [ ] = {
   "black", "red", "green", "blue", "yellow", "brown", 
   "gray", "violet", "cyan", "magenta", "orange"
};

static int	lines [ ] = {
   DW_LineSolid, DW_LineDashed, DW_LineDotted, 
   DW_LineDotDashed, DW_LineLongDashed
};

typedef struct graph {
   double	xscale;
   double	yscale;
   double	min_y;
   double	max_y;
   double	min_x;
   double	max_x;
   double	y_inc;
   double	y_minor;
   double	x_inc;
   double	x_minor;
   unsigned	numcurves;
} graph;

static graph td_gr;
static graph fp_gr;
static graph force_gr;

static String table =
"<Key>space: AutoRepeat(off) set()\n\
 <Key>Return: AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) unset() GraphAction(button)\n\
 <KeyUp>space: AutoRepeat(saved) unset() GraphAction(button)";

static void DismissCallback (w, client_data, call_data)
   Widget	w;
   XtPointer	client_data;
   XtPointer	call_data;
{
   XtPopdown ((Widget) client_data); 
}

static void SaveCallback (w, client_data, call_data)
   Widget	w;
   XtPointer	client_data;
   XtPointer	call_data;
{
   Widget	graph_dw = *(Widget *) client_data;

   DumpDrawingArea (graph_dw, "Save Line Plot", True);
}

static void GraphAction (w, event, params, num_params)
   Widget	w;
   XEvent	*event;
   String	*params;
   Cardinal	*num_params;
{
   if (strcmp (params [0], "tdShell") == 0)
      DismissCallback (NULL, (XtPointer) &tdShell, NULL);
   else if (strcmp (params [0], "fpShell") == 0)
      DismissCallback (NULL, (XtPointer) &fpShell, NULL);
   else
      XtCallCallbacks (w, XtNcallback, NULL);
}

static void InitializeGraphShell (graphShell, graph_dw, gr)
   Widget	graphShell; 
   Widget	graph_dw;
   graph	*gr;
{
   Arg		arglist [12];
   Cardinal	count;
   Dimension	width, height;
   float	Xscale, Yscale;
   float	minX, maxX, minY, maxY;

   XtRealizeWidget (graphShell);

   SetFocus (XtNameToWidget (graphShell, "layout.dismiss"));

   width = WIDTH + BUFFER + ((gr -> numcurves - 1) / 16 + 1)*LEGEND_WIDTH;
   height = HEIGHT + 2*BUFFER;

   maxX = WIDTH + ((gr -> numcurves - 1) / 16 + 1)*LEGEND_WIDTH;
   minX = -BUFFER;
   maxY = HEIGHT + BUFFER;
   minY= -BUFFER;

   Xscale = Yscale = 1.0;

   count = 0;
   XtSetArg (arglist [count], XtNxMin, Float2Arg (minX)); count++;
   XtSetArg (arglist [count], XtNxMax, Float2Arg (maxX)); count++;
   XtSetArg (arglist [count], XtNyMin, Float2Arg (minY)); count++;
   XtSetArg (arglist [count], XtNyMax, Float2Arg (maxY)); count++;
   XtSetArg (arglist [count], XtNyScale, Float2Arg (Yscale)); count++;
   XtSetArg (arglist [count], XtNxScale, Float2Arg (Xscale)); count++;
   XtSetArg (arglist [count], XtNwidth, width); count++;
   XtSetArg (arglist [count], XtNheight, height); count++;
   XtSetValues (graph_dw, arglist, count);
}

static void InitializeFonts ( )
{
   axis_font = XLoadQueryFont (XtDisplay (toplevel), AXIS_FONT1);
   if (axis_font == NULL) {
      axis_font = XLoadQueryFont (XtDisplay (toplevel), AXIS_FONT2);
      if (axis_font == NULL) {
         axis_font = XLoadQueryFont (XtDisplay (toplevel), "fixed");
         axis_font_name = "fixed";
      }
      else
         axis_font_name = AXIS_FONT2;
   }
   else
      axis_font_name = AXIS_FONT1;

   label_font = XLoadQueryFont (XtDisplay (toplevel), LABEL_FONT1);
   if (label_font == NULL) {
      label_font = XLoadQueryFont (XtDisplay (toplevel), LABEL_FONT2);
      if (label_font == NULL) {
         label_font = XLoadQueryFont (XtDisplay (toplevel), "fixed");
         label_font_name = "fixed";
      }
      else
         label_font_name = LABEL_FONT2;
   }
   else
      label_font_name = LABEL_FONT1;

   title_font = XLoadQueryFont (XtDisplay (toplevel), TITLE_FONT1);
   if (title_font == NULL) {
      title_font = XLoadQueryFont (XtDisplay (toplevel), TITLE_FONT2);
      if (title_font == NULL) {
         title_font = XLoadQueryFont (XtDisplay (toplevel), "fixed");
         title_font_name = "fixed";
      }
      else
         title_font_name = TITLE_FONT2;
   }
   else
      title_font_name = TITLE_FONT1;

   legend_font = XLoadQueryFont (XtDisplay (toplevel), LEGEND_FONT1);
   if (legend_font == NULL) {
      legend_font = XLoadQueryFont (XtDisplay (toplevel), LEGEND_FONT2);
      if (legend_font == NULL) {
         legend_font = XLoadQueryFont (XtDisplay (toplevel), "fixed");
         legend_font_name = "fixed";
      }
      else
         legend_font_name = LEGEND_FONT2;
   }
   else
      legend_font_name = LEGEND_FONT1;
}


static Widget CreateGraphShell (dw, shell_name, dw_name)
   Widget		*dw;
   char			*shell_name;
   char			*dw_name;
{
   Widget		graphShell;
   Arg			args [10];
   Cardinal		count;
   Widget		layout;
   Widget		dismiss, save;
   Widget		group [2];
   Pixel		highlight;
   XtTranslations	translations;
   char			buffer [32];
   static XtActionsRec	actions [ ] = {{"GraphAction", GraphAction}};
   static char          layout_string [512];

   sprintf (layout_string, 
"vertical { \
    8 \
    horizontal { \
       8 \
       %s \
       8 \
    } \
    24 \
    horizontal { \
       8 \
       dismiss \
       8 \
       save \
       8 \
    } \
    8 \
}", dw_name);

   count = 0;
   XtSetArg (args [count], XtNtitle, "Velvet Line Plot");    count++;
   XtSetArg (args [count], XtNiconName, "Velvet Line Plot"); count++;
/*
   XtSetArg (args [count], XtNallowShellResize, True); count++;
*/
   XtSetArg (args [count], XtNwidth, 1); count++;
   XtSetArg (args [count], XtNheight, 1); count++;
   graphShell = XtCreatePopupShell (shell_name, topLevelShellWidgetClass,
                                    toplevel, args, count);

   count = 0;
   XtSetArg (args [count], XtNlayout, StringToLayout(toplevel, layout_string));
   count++;
   layout = XtCreateManagedWidget ("layout", layoutWidgetClass, 
                                   graphShell, args, count);

   count = 0;
   XtSetArg (args [count], XtNgrid, False);            count++;
   XtSetArg (args [count], XtNborderWidth, 3);         count++;
   *dw = XtCreateManagedWidget (dw_name, drawingWidgetClass, 
                                layout, args, count);

   dismiss = XtCreateManagedWidget ("dismiss", commandWidgetClass,
                                    layout, NULL, 0);
   save = XtCreateManagedWidget ("save", commandWidgetClass,
                                 layout, NULL, 0);

   group [0] = dismiss;
   group [1] = save;

   XtSetArg (args [0], XtNborderColor, &highlight);
   XtGetValues (layout, args, 1);

   CreateTabGroup (graphShell, group, 2, highlight, True);

   XtRealizeWidget (graphShell);

   XtAddCallback (dismiss, XtNcallback, DismissCallback, (XtPointer) graphShell);
   XtAddCallback (save,    XtNcallback, SaveCallback, (XtPointer) dw);

   XtAppAddActions (app_context, actions, 1);

   translations = XtParseTranslationTable (table);
   XtOverrideTranslations (dismiss, translations);
   XtOverrideTranslations (save,    translations);

   sprintf(buffer,"GraphAction(%s)", shell_name);
   AddDeleteWindowProtocol (graphShell, buffer);

   InitializeFonts ( );

   return graphShell;
}

	/*
	 * the following two routines are based on the heuristics used in
	 * xmgr to figure out axis extreme and tick spacing ...
	 */

static double NiceNumber (x, round_mode)
   double	x;
   Boolean	round_mode;
{
   double	order;
   double	fraction;
   double	y;

   order = floor (log10(x));
   fraction = x / pow(10.0, order);

   if (round_mode) {
      if (fraction < 1.5)
         y = 1.0;
      else if (fraction < 3.0)
         y = 2.0;
      else if (fraction < 7.0)
         y = 5.0;
      else
         y = 10.0;
   }
   else if (fraction <= 1.0)
      y = 1.0;
   else if (fraction <= 2.0)
      y = 2.0;
   else if (fraction <= 5.0)
      y = 5.0;
   else
      y = 10.0;

   return y*pow(10.0, order);
}

static void SetupYAxis (min, max, numticks, gr)
   double	min, max;
   int		numticks;
   graph	*gr;
{
   double	range;
   double	d;

   range = NiceNumber (max - min, False);
   d = NiceNumber (range / (numticks + 1.0), True);

   gr -> min_y = floor(min / d)*d;
   gr -> max_y = ceil(max / d)*d;
   gr -> y_inc = d;
   gr -> y_minor = d / 4;

   return;
}

static void SetupXAxis (min, max, numticks, gr)
   double	min, max;
   int		numticks;
   graph	*gr;
{
   double	range;
   double	d;

   range = NiceNumber (max - min, False);
   d = NiceNumber (range / (numticks + 1.0), True);

   gr -> min_x = floor(min / d)*d;
   gr -> max_x = ceil(max / d)*d;
   gr -> x_inc = d;
   gr -> x_minor = d / 4;

   return;
}

	/*
	 * if this seems a little hokey ... it is, but I wanted to make
	 * sure the time tick spacing bore some relation to the actual
	 * time step
	 */

static void SetupTimeAxis (gr)
   graph	*gr;
{
   int		numsteps;

   numsteps = (analysis.stop - analysis.start + analysis.step/2) / analysis.step;

   if (numsteps % 4 == 0) {
      gr -> x_inc = numsteps / 4 * analysis.step;
      gr -> x_minor = gr -> x_inc / 4.0;
   }
   else if (numsteps % 5 == 0) {
      gr -> x_inc = numsteps / 5 * analysis.step;
      gr -> x_minor = gr -> x_inc / 5.0;
   }
   else if (numsteps % 3 == 0) {
      gr -> x_inc = numsteps / 3 * analysis.step;
      gr -> x_minor = gr -> x_inc / 3.0;
   }
   else {
      gr -> x_inc = (analysis.stop - analysis.start) / 2;
      gr -> x_minor = gr -> x_inc / 4;
   }

   gr -> min_x = analysis.start;
   gr -> max_x = analysis.stop;
}

static void DrawLabel (data, axis, graph_dw, gr)
   double	data;
   int		axis;
   Widget	graph_dw;
   graph	*gr;
{
   char			buffer [20];
   XCharStruct  	cstruct;
   int			dr, fdr, far;
   int			width, height;
   float		x, y;

  
   sprintf (buffer,"%g", data);

   XTextExtents (label_font,buffer,strlen (buffer),&dr,&far,&fdr,&cstruct);
   width = cstruct.width;
   height = cstruct.ascent + cstruct.descent;

   if (axis == 1) {
      y = -4 - height;
      x = (data - gr -> min_x)*gr -> xscale - width/2;
   }
   else {
      y = (data - gr -> min_y)*gr -> yscale - height/2; 
      x = -4 - width;
   }
   
   DW_DrawText (graph_dw, False, x, y, buffer);

   return;
}
   
static void SetupGraphArea (min_x, max_x, min_y, max_y, graph_dw, gr, use_time)
   double	min_x, max_x;
   double	min_y, max_y;
   Widget	graph_dw;
   graph	*gr;
   unsigned	use_time;
{
   double	t, x, m;
 
	/*
	 * draw the border
	 */

   DW_DrawRectangle (graph_dw, False, 0.0, 0.0, WIDTH, HEIGHT);

   	/*
	 * draw the tick marks along the time axis
	 */

   DW_SetFont (graph_dw, label_font_name);

   if (use_time)
      SetupTimeAxis (gr);
   else
      SetupXAxis (min_x, max_x, 4, gr);

   gr -> xscale = WIDTH / (gr -> max_x - gr -> min_x);
   
   for (t = gr -> min_x ; t <= gr -> max_x ; t += gr -> x_inc) {
      for (m = t + gr -> x_minor ; m < t + gr -> x_inc && 
                                m < gr -> max_x ; m += gr -> x_minor)

         DW_DrawLine (graph_dw, (m - gr -> min_x)*gr -> xscale, 0.0, 
                                (m - gr -> min_x)*gr -> xscale, MINOR_TICK);

      DW_DrawLine (graph_dw, (t - gr -> min_x)*gr -> xscale, 0.0, 
                             (t - gr -> min_x)*gr -> xscale, MAJOR_TICK);

      DrawLabel (t, 1, graph_dw, gr);
   }

	/*
	 * draw the tick marks along the displacement axis
	 */

   SetupYAxis (min_y, max_y, 4, gr);

   gr -> yscale = HEIGHT / (gr -> max_y - gr -> min_y);

   for (x = gr -> min_y ; x <= gr -> max_y ; x += gr -> y_inc) {
      for (m = x + gr -> y_minor ; m < x + gr -> y_inc && 
                                m < gr -> max_y ; m += gr -> y_minor)

         DW_DrawLine (graph_dw, 0.0, (m - gr -> min_y)*gr -> yscale, 
                                MINOR_TICK, (m - gr -> min_y)*gr -> yscale);

      DW_DrawLine (graph_dw, 0.0, (x - gr -> min_y)*gr -> yscale, 
                             MAJOR_TICK, (x - gr -> min_y)*gr -> yscale);

      DrawLabel (x, 2, graph_dw, gr);
   }

   return;
}    

static void DrawCurveLegend (i, print_dof_names, graph_dw)
   int		i;
   Boolean	print_dof_names;
   Widget	graph_dw;
{
   static char		*symbols [ ] = {"", "Tx", "Ty", "Tz", "Rx", "Ry", "Rz"};
   static int   	first = 1;
   XCharStruct		cstruct;
   char			buffer [20];
   int			width, height;
   int			dr, fdr, far;
   static float		y_step;
   float		x, y;

   if (first) {
      y_step = HEIGHT / 16.0;

      first = 0;
   }

   if (print_dof_names)
      sprintf (buffer, "%s (%d)",
               symbols[(int) analysis.dofs[(i-1) % analysis.numdofs + 1]],
               analysis.nodes [(i-1) % analysis.numnodes + 1] -> number);
   else
      sprintf (buffer, "node %d", 
               analysis.nodes [(i-1) % analysis.numnodes + 1] -> number);

   XTextExtents (legend_font,buffer,strlen (buffer),&dr,&far,&fdr,&cstruct);
   width = cstruct.width;
   height = cstruct.ascent + cstruct.descent;

   x = WIDTH + 8 + (i-1)/16*LEGEND_WIDTH;
   y = HEIGHT - ((i - 1) % 16)*y_step;

   DW_DrawLine (graph_dw, x, y, x + LEGEND_LINE, y);
   DW_DrawText (graph_dw, False, x + LEGEND_LINE + 4, y - height/2, buffer);

   return;
}

static void DrawTransferLegend (i, l, curve, graph_dw)
   int		i, l;
   int		curve;
   Widget	graph_dw;
{
   static char		*symbols [ ] = {"", "Tx", "Ty", "Tz", "Rx", "Ry", "Rz"};
   static int   	first = 1;
   XCharStruct		cstruct;
   char			buffer [20];
   int			width, height;
   int			dr, fdr, far;
   static float		y_step;
   float		x, y;
   unsigned		inode, idof;

   if (first) {
      y_step = HEIGHT / 16.0;
      first = 0;
   }

   LocalDOF (i, &inode, &idof);

   sprintf (buffer, "%s(%d),%s(%d)",
            symbols[idof], inode, 
            symbols[(int) analysis.dofs[(l-1) % analysis.numdofs + 1]],
            analysis.nodes [(l-1) % analysis.numnodes + 1] -> number);

   XTextExtents (legend_font,buffer,strlen (buffer),&dr,&far,&fdr,&cstruct);
   width = cstruct.width;
   height = cstruct.ascent + cstruct.descent;

   x = WIDTH + 8 + (curve-1)/16*LEGEND_WIDTH;
   y = HEIGHT - ((curve - 1) % 16)*y_step;

   DW_DrawLine (graph_dw, x, y, x + LEGEND_LINE, y);
   DW_DrawText (graph_dw, False, x + LEGEND_LINE + 4, y - height/2, buffer);

   return;
}

static void DrawForceLegend (i, symbol, graph_dw)
   int		i;
   char		*symbol;
   Widget	graph_dw;
{
   static int   	first = 1;
   XCharStruct		cstruct;
   int			width, height;
   int			dr, fdr, far;
   static float		y_step;
   float		x, y;

   if (first) {
      y_step = HEIGHT / 16.0;

      first = 0;
   }

   XTextExtents (legend_font,symbol,strlen (symbol),&dr,&far,&fdr,&cstruct);
   width = cstruct.width;
   height = cstruct.ascent + cstruct.descent;

   x = WIDTH + 8 + (i-1)/16*LEGEND_WIDTH;
   y = HEIGHT - ((i - 1) % 16)*y_step;

   DW_DrawLine (graph_dw, x, y, x + LEGEND_LINE, y);
   DW_DrawText (graph_dw, False, x + LEGEND_LINE + 4, y - height/2, symbol);

   return;
}

static void PlaceTitles (alt_title, xlabel, ylabel, graph_dw, use_alt_title)
   char		*alt_title;
   char		*xlabel;
   char		*ylabel;
   Widget	graph_dw;
   int		use_alt_title;
{
   int		dr, far, fdr;
   XCharStruct	cstruct;
   int		width, height;
   char		buffer [256];
   float	x, y;


   if (problem.title == NULL || strcmp (problem.title, "") == 0 || use_alt_title)
      strcpy (buffer, alt_title);
   else
      strcpy (buffer, problem.title);

   XTextExtents (title_font,buffer,strlen (buffer),&dr,&far,&fdr,&cstruct);

   width = cstruct.width;
   height = cstruct.ascent + cstruct.descent;

   x = WIDTH / 2 - width / 2;
   y = HEIGHT + 15;

   DW_SetFont (graph_dw, title_font_name);
   DW_DrawText (graph_dw, False, x, y, buffer);

   XTextExtents (axis_font, xlabel, 4, &dr, &far, &fdr, &cstruct);

   width = cstruct.width;
   height = cstruct.ascent + cstruct.descent;

   x = WIDTH / 2 - width / 2;
   y = -25 - height;

   DW_SetFont (graph_dw, axis_font_name);
   DW_DrawText (graph_dw, False, x, y, xlabel);

   XTextExtents (axis_font, ylabel, 4, &dr, &far, &fdr, &cstruct);

   width = cstruct.width;
   height = cstruct.ascent + cstruct.descent;

   x = -BUFFER + 10;
   y = HEIGHT / 2 - height / 2;

   DW_DrawText (graph_dw, False, x, y, ylabel);

   return;
}

void VelvetPlotTD (dtable, ttable, xlabel, ylabel, alt_title, print_dof_names)
   Matrix	dtable;
   Matrix	ttable;
   char		*xlabel;
   char		*ylabel;
   char		*alt_title;
   Boolean	print_dof_names;
{
   Arg		args [1];
   int		depth;
   double	data;
   double	prev;
   double	t_prev;
   double	t;
   unsigned	i,j,k;
   double	min, max;
   double	min_t, max_t;
   unsigned	use_time;

   if (tdShell == NULL) 
      tdShell = CreateGraphShell (&td_dw, "tdShell", "td_dw");

   td_gr.numcurves = analysis.numnodes * analysis.numdofs;

   InitializeGraphShell  (tdShell, td_dw, &td_gr);

   DW_RemoveAll (td_dw);

   min = max = MatrixData (dtable) [1][1];
   for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
      for (j = 1 ; j <= analysis.numnodes ; j++) {
         for (k = 1 ; k <= analysis.numdofs ; k++) {

            data = MatrixData (dtable) [i][(j-1)*analysis.numdofs + k];

            if (data < min)
               min = data;
            else if (data > max)
               max = data;
         }
      }
   }

   XtPopup (tdShell, XtGrabNone);

   DW_SetForeground (td_dw, "black");
   DW_SetLineStyle (td_dw, DW_LineSolid);

   XtSetArg (args [0], XtNdepth, &depth);
   XtGetValues (toplevel, args, 1);

   if (ttable == NullMatrix) {
      min_t = 0.0;
      max_t = analysis.stop;
   }
   else {
      min_t = mdata(ttable,1,1);
      max_t = mdata(ttable,Mrows(ttable),1);
   }

   if (ttable == NULL)
      use_time = 1;
   else
      use_time = 0;

   SetupGraphArea (min_t, max_t, min, max, td_dw, &td_gr, use_time);
 
   DW_SetFont (td_dw, legend_font_name);
   for (i = 1 ; i <= td_gr.numcurves ; i++) { 

      if (depth > 1)
         DW_SetForeground (td_dw, colors [(i - 1) % XtNumber (colors)]);

      DW_SetLineStyle (td_dw, lines [(i - 1) % XtNumber (lines)]);

      prev = MatrixData (dtable) [1][i];
      t_prev = min_t;
      for (j = 2 ; j <= MatrixRows (dtable) ; j++) {

         if (ttable == NullMatrix)
            t = (j - 1)*analysis.step;
         else
            t = mdata(ttable,j,1);

         data = MatrixData (dtable) [j][i];
         DW_DrawLine (td_dw, t_prev*td_gr.xscale, 
                             (prev - td_gr.min_y)*td_gr.yscale, 
                             t*td_gr.xscale, 
                             (data - td_gr.min_y)*td_gr.yscale);

         t_prev = t;
         prev = data;
      }

      DrawCurveLegend (i, print_dof_names, td_dw);
   }

   DW_SetForeground (td_dw, "black");

   PlaceTitles (alt_title, xlabel, ylabel, td_dw, 0);
}

void VelvetPlotSpectra (P, xlabel, ylabel, alt_title, print_dof_names)
   Matrix	P;
   char		*xlabel;
   char		*ylabel;
   char		*alt_title;
   Boolean	print_dof_names;
{
   Arg		args [1];
   int		depth;
   double	data;
   double	prev;
   double	f;
   unsigned	i,j,k;
   double	min, max;

   if (fpShell == NULL) 
      fpShell = CreateGraphShell (&fp_dw, "fpShell", "fp_dw");

   fp_gr.numcurves = analysis.numnodes * analysis.numdofs;

   InitializeGraphShell (fpShell, fp_dw, &fp_gr);

   DW_RemoveAll (fp_dw);

   min = max = MatrixData (P) [1][1];
   for (i = 1 ; i <= MatrixRows (P) ; i++) {
      for (j = 1 ; j <= analysis.numnodes ; j++) {
         for (k = 1 ; k <= analysis.numdofs ; k++) {

            data = MatrixData (P) [i][(j-1)*analysis.numdofs + k];

            if (data < min)
               min = data;
            else if (data > max)
               max = data;
         }
      }
   }

   XtPopup (fpShell, XtGrabNone);

   DW_SetForeground (fp_dw, "black");
   DW_SetLineStyle (fp_dw, DW_LineSolid);

   XtSetArg (args [0], XtNdepth, &depth);
   XtGetValues (toplevel, args, 1);

   SetupGraphArea (analysis.start, analysis.stop, min, max, fp_dw, &fp_gr, 0);
 
   DW_SetFont (fp_dw, legend_font_name);
   for (i = 1 ; i <= fp_gr.numcurves ; i++) { 

      if (depth > 1)
         DW_SetForeground (fp_dw, colors [(i - 1) % XtNumber (colors)]);

      DW_SetLineStyle (fp_dw, lines [(i - 1) % XtNumber (lines)]);

      prev = MatrixData (P) [1][i];
      for (j = 2 ; j <= MatrixRows (P) ; j++) {

         f = (j - 1)*analysis.step;
         data = MatrixData (P) [j][i];
         DW_DrawLine (fp_dw, (f - analysis.step - fp_gr.min_x)*fp_gr.xscale, 
                             (prev - fp_gr.min_y)*fp_gr.yscale, 
                             (f - fp_gr.min_x)*fp_gr.xscale, 
                             (data - fp_gr.min_y)*fp_gr.yscale);

         prev = data;
      }

      DrawCurveLegend (i, print_dof_names, fp_dw);
   }

   DW_SetForeground (fp_dw, "black");

   PlaceTitles (alt_title, xlabel, ylabel, fp_dw, 0);
}

void VelvetPlotTransferFunctions (H, forced, numforced, xlabel, ylabel, alt_title)
   Matrix	*H;
   unsigned	*forced;
   unsigned	numforced;
   char		*xlabel;
   char		*ylabel;
   char		*alt_title;
{
   Arg		args [1];
   int		depth;
   double	data;
   double	prev;
   double	f;
   unsigned	i,j,k,l;
   unsigned	curve;
   unsigned	nn;
   double	min, max;

   if (fpShell == NULL) 
      fpShell = CreateGraphShell (&fp_dw, "fpShell", "fp_dw");

   nn = analysis.numdofs * analysis.numnodes;
   fp_gr.numcurves = nn * numforced;

   InitializeGraphShell (fpShell, fp_dw, &fp_gr);

   DW_RemoveAll (fp_dw);

   min = max = MatrixData (H [1]) [1][1];
   for (i = 1 ; i <= MatrixRows (H [1]) ; i++) {
      for (l = 1 ; l <= numforced ; l++) {
         for (j = 1 ; j <= analysis.numnodes ; j++) {
            for (k = 1 ; k <= analysis.numdofs ; k++) {

               data = MatrixData (H [l]) [i][(j-1)*analysis.numdofs + k];

               if (data < min)
                  min = data;
               else if (data > max)
                  max = data;
            }
         }
      }
   }

   XtPopup (fpShell, XtGrabNone);

   DW_SetForeground (fp_dw, "black");
   DW_SetLineStyle (fp_dw, DW_LineSolid);

   XtSetArg (args [0], XtNdepth, &depth);
   XtGetValues (toplevel, args, 1);

   SetupGraphArea (analysis.start, analysis.stop, min, max, fp_dw, &fp_gr, 0);
 
   DW_SetFont (fp_dw, legend_font_name);
   for (i = 1 ; i <= numforced ; i++) { 
      for (l = 1 ; l <= nn ; l++) {

         curve = (i - 1)*nn + l;

         if (depth > 1)
            DW_SetForeground (fp_dw, colors [(curve - 1) % XtNumber (colors)]);

         DW_SetLineStyle (fp_dw, lines [(curve - 1) % XtNumber (lines)]);

         prev = MatrixData (H [i]) [1][l];
         for (j = 2 ; j <= MatrixRows (H [1]) ; j++) {

            f = (j - 1)*analysis.step;
            data = MatrixData (H [i]) [j][l];
            DW_DrawLine (fp_dw, (f - analysis.step - fp_gr.min_x)*fp_gr.xscale, 
                                (prev - fp_gr.min_y)*fp_gr.yscale, 
                                (f - fp_gr.min_x)*fp_gr.xscale, 
                                (data - fp_gr.min_y)*fp_gr.yscale);

            prev = data;
         }

         DrawTransferLegend (forced [i], l, curve, fp_dw);
      }
   }

   DW_SetForeground (fp_dw, "black");

   PlaceTitles (alt_title, xlabel, ylabel, fp_dw, 0);
}

void VelvetPlotForce (force, quantity)
   Force	force;
   char		*quantity;
{
   static char *symbols [] = {"", "Fx", "Fy", "Fz", "Mx", "My", "Mz"};
   char		*symbol [4];
   Arg		args [1];
   int		depth;
   double	data;
   double	prev;
   double	t;
   unsigned	i,j;
   double	min, max;
   Matrix  	ftable;	
   int		status_x, status_y, status_z;
   int		offset;
   int		n; 

   if (analysis.stop <= 0.0 || analysis.step <= 0.0) {
      error ("improper time step or time duration information");
      return;
   }

   n = analysis.stop / analysis.step + 1;

   offset = 0;	/* gcc -Wall */
   if (strcmp(quantity, "force") == 0) 
      offset = 1;
   else if (strcmp(quantity, "moment") == 0)
      offset = 4;

   status_x = status_y = status_z = 0;

   if (force -> force [offset].expr != NULL)
      status_x = 1;
   else if (force -> force [offset].value) 
      status_x = 2;

   if (force -> force [offset+1].expr != NULL)
      status_y = 1;
   else if (force -> force [offset+1].value) 
      status_y = 2;

   if (force -> force [offset+2].expr != NULL)
      status_z = 1;
   else if (force -> force [offset+2].value) 
      status_z = 2;

   force_gr.numcurves = (status_x > 0) + (status_y > 0) + (status_z > 0);

   if (force_gr.numcurves == 0) {
      error ("nothing to plot");
      return;
   }

   j = 0;
   if (status_x)
      symbol [j+1] = symbols [offset + j++];
   if (status_y)
      symbol [j+1] = symbols [offset + j++];
   if (status_z)
      symbol [j+1] = symbols [offset + j++];

   ftable = CreateMatrix (n, force_gr.numcurves);

   if (forceShell == NULL) 
      forceShell = CreateGraphShell (&force_dw, "forceShell", "force_dw");

   InitializeGraphShell  (forceShell, force_dw, &force_gr);

   DW_RemoveAll (force_dw);

   for (i = 1 ; i <= n ; i++) {
      t = (i - 1)*analysis.step;

      j = 1;

      if (status_x == 1)
         sdata(ftable,i,j++) = EvalCode (force -> force [offset].expr, t);
      else if (status_x == 2)
         sdata(ftable,i,j++) = force -> force [offset].value;

      if (status_y == 1)
         sdata(ftable,i,j++) = EvalCode (force -> force [offset+1].expr, t);
      else if (status_y == 2)
         sdata(ftable,i,j++) = force -> force [offset+1].value;

      if (status_z == 1)
         sdata(ftable,i,j++) = EvalCode (force -> force [offset+2].expr, t);
      else if (status_z == 2)
         sdata(ftable,i,j++) = force -> force [offset+2].value;
   }

   min = max = mdata(ftable,1,1);

   for (i = 1 ; i <= n ; i++) {
      for (j = 1 ; j <= force_gr.numcurves ; j++) {
         data = mdata(ftable,i,j);

         if (data < min)
            min = data;
         else if (data > max)
            max = data;
      }
   }

   XtPopup (forceShell, XtGrabNone);

   DW_SetForeground (force_dw, "black");
   DW_SetLineStyle (force_dw, DW_LineSolid);

   XtSetArg (args [0], XtNdepth, &depth);
   XtGetValues (toplevel, args, 1);

   SetupGraphArea (0.0, analysis.stop, min, max, force_dw, &force_gr, 1);
 
   DW_SetFont (force_dw, legend_font_name);
   for (i = 1 ; i <= force_gr.numcurves ; i++) { 

      if (depth > 1)
         DW_SetForeground (force_dw, colors [(i - 1) % XtNumber (colors)]);

      DW_SetLineStyle (force_dw, lines [(i - 1) % XtNumber (lines)]);

      prev = mdata(ftable,1,i);
      for (j = 2 ; j <= n ; j++) {

         t = (j - 1)*analysis.step;
         data = MatrixData (ftable) [j][i];
         DW_DrawLine (force_dw, (t - analysis.step)*force_gr.xscale, 
                             (prev - force_gr.min_y)*force_gr.yscale, 
                             t*force_gr.xscale, 
                             (data - force_gr.min_y)*force_gr.yscale);

         prev = data;
      }

      DrawForceLegend (i, symbol [i], force_dw);
   }

   DW_SetForeground (force_dw, "black");

   PlaceTitles (force -> name, "time", (offset == 1 ? "F" : "M"), force_dw, 1);

   DestroyMatrix (ftable);

   return;
}

void VelvetPlotLoadRange (dtable)
   Matrix	dtable;
{
   Arg		args [1];
   int		depth;
   double	data;
   double	prev;
   double	f_prev;
   double	f;
   unsigned	i,j,k;
   double	min, max;
   double	min_f, max_f;

   if (tdShell == NULL) 
      tdShell = CreateGraphShell (&td_dw, "tdShell", "td_dw");

   td_gr.numcurves = analysis.numnodes * analysis.numdofs;

   InitializeGraphShell  (tdShell, td_dw, &td_gr);

   DW_RemoveAll (td_dw);

   min = max = MatrixData (dtable) [1][1];
   for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
      for (j = 1 ; j <= analysis.numnodes ; j++) {
         for (k = 1 ; k <= analysis.numdofs ; k++) {

            data = MatrixData (dtable) [i][(j-1)*analysis.numdofs + k];

            if (data < min)
               min = data;
            else if (data > max)
               max = data;
         }
      }
   }

   XtPopup (tdShell, XtGrabNone);

   DW_SetForeground (td_dw, "black");
   DW_SetLineStyle (td_dw, DW_LineSolid);

   XtSetArg (args [0], XtNdepth, &depth);
   XtGetValues (toplevel, args, 1);

   if (analysis.step >= 0) {
      min_f = analysis.start;
      max_f = analysis.stop;
   }
   else {
      max_f = analysis.start;
      min_f = analysis.stop;
   }

   SetupGraphArea (min_f, max_f, min, max, td_dw, &td_gr, 0);
 
   DW_SetFont (td_dw, legend_font_name);
   for (i = 1 ; i <= td_gr.numcurves ; i++) { 

      if (depth > 1)
         DW_SetForeground (td_dw, colors [(i - 1) % XtNumber (colors)]);

      DW_SetLineStyle (td_dw, lines [(i - 1) % XtNumber (lines)]);

      prev = MatrixData (dtable) [1][i];
      f_prev = analysis.start;
      for (j = 2 ; j <= MatrixRows (dtable) ; j++) {

         f = (j - 1)*analysis.step + analysis.start;

         data = MatrixData (dtable) [j][i];
         DW_DrawLine (td_dw, (f_prev - td_gr.min_x)*td_gr.xscale, 
                             (prev - td_gr.min_y)*td_gr.yscale, 
                             (f - td_gr.min_x)*td_gr.xscale, 
                             (data - td_gr.min_y)*td_gr.yscale);

         f_prev = f;
         prev = data;
      }

      DrawCurveLegend (i, 1, td_dw);
   }

   DW_SetForeground (td_dw, "black");

   PlaceTitles ("Load Range Result", "force", "dx", td_dw, 0);
}
