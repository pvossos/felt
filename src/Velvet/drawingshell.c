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

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Command.h>
# include "Layout.h"
# include "TabGroup.h"
# include "Drawing.h"
# include "Canvas.h"
# include "util.h"

extern XtAppContext	app_context;
extern Widget		toplevel;

static char layout_string [ ] =
"vertical { \
   8 \
   horizontal { \
      8 \
      dw <+inf -100% * +inf -100%> \
      8 \
   } \
   24 \
   horizontal { \
      8 \
      dismiss \
      8 \
      save \
      8 <+inf -100%> \
   } \
   8 \
}";

static String table =
"<Key>space: AutoRepeat(off) set()\n\
 <Key>Return: AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) unset() ShellAction(button)\n\
 <KeyUp>space: AutoRepeat(saved) unset() ShellAction(button)";

static void Dismiss (w, client_data, call_data)
   Widget	w;
   XtPointer	client_data;
   XtPointer	call_data;
{
   XtPopdown ((Widget) client_data);
}

static void Action (w, event, params, num_params)
   Widget	 w;
   XEvent       *event;
   String	*params;
   Cardinal	*num_params;
{
   if (strcmp (params [0], "delete") == 0)
      w = XtNameToWidget (w, "layout.dismiss");

   XtCallCallbacks (w, XtNcallback, NULL);
}

Widget CreateDrawingShell (name, title, callback, dw)
   String		name;
   String		title;
   XtCallbackProc	callback;
   Widget		*dw;
{
   Widget		group [2];
   Widget		shell;
   Arg			args [10];
   Cardinal		n;
   Pixel		highlight;
   Widget		dismiss, save;
   Widget		layout;
   XtTranslations	translations;
   static XtActionsRec  actions [ ] = {{"ShellAction", Action}};

	/*
	 * create the main shell for this window
	 */

   n = 0;
   XtSetArg (args [n], XtNtitle,    title); n++;
   XtSetArg (args [n], XtNiconName, title); n++;
   XtSetArg (args [n], XtNallowShellResize, True); n++;
   shell = XtCreatePopupShell (name, topLevelShellWidgetClass, 
                               toplevel, args, n);

   n = 0;
   XtSetArg (args [n], XtNlayout, 
             StringToLayout (toplevel, layout_string)); n++;

   layout = XtCreateManagedWidget ("layout", layoutWidgetClass, 
                                   shell, args, n);

	/*
	 * create the drawing widget
	 */

   n = 0;
   XtSetArg (args [n], XtNgrid, False); n++;
   XtSetArg (args [n], XtNborderWidth, 3); n++;
   *dw = XtCreateManagedWidget ("dw", drawingWidgetClass, 
                                layout, args, n);

	/*
	 * create the buttons and put them in a tab group
	 */

   dismiss = XtCreateManagedWidget ("dismiss", commandWidgetClass, 
                                    layout, NULL, 0);
   save = XtCreateManagedWidget ("save", commandWidgetClass, 
                                 layout, NULL, 0);

   group [0] = dismiss;
   group [1] = save;

   XtSetArg (args [0], XtNborderColor, &highlight);
   XtGetValues (layout, args, 1);

   CreateTabGroup (shell, group, 2, highlight, True);

   XtRealizeWidget (shell);

	/*
	 * add the callbacks, actions and keyboard translations
 	 */

   XtAddCallback (dismiss, XtNcallback, Dismiss, (XtPointer) shell);
   XtAddCallback (save, XtNcallback, callback, NULL);

   XtAppAddActions (app_context, actions, 1);

   translations = XtParseTranslationTable (table);
   XtOverrideTranslations (save, translations);
   XtOverrideTranslations (dismiss, translations);

   AddDeleteWindowProtocol (shell, "ShellAction(delete)");

   return shell; 
}

static int ready;

static void ClearToDraw (w, client_data, call_data)
   Widget	w;
   XtPointer	client_data;
   XtPointer	call_data;
{
   ready = 1;
}
 
void InitializeDrawingShell (shell, dw, minX, maxX, minY, maxY,
                             x_scale, y_scale, wx, hy)
   Widget	shell, dw;
   float	maxX, minX,
		minY, maxY;	
   float	*x_scale, *y_scale;
   Dimension	*wx, *hy;
{
   static String	buffer = "resize window then click to plot";
   float		label_width;
   float		label_height; 
   Arg			arglist [12];
   Cardinal		count;
   Dimension		width, height;
   float		xmin, xmax, ymin, ymax;
   float		w,h;
   float		Xscale, Yscale;
   XEvent		event;

   XtRealizeWidget (shell);

   XtPopup (shell, XtGrabNone);

   SetFocus (XtNameToWidget (shell, "layout.dismiss"));

	/*
	 * first we get the current extents of the drawing widget
	 */

   count = 0;
   XtSetArg (arglist [count], XtNxMin, &xmin); count++;
   XtSetArg (arglist [count], XtNxMax, &xmax); count++;
   XtSetArg (arglist [count], XtNyMin, &ymin); count++;
   XtSetArg (arglist [count], XtNyMax, &ymax); count++;
   XtGetValues (dw, arglist, count);

	/*
	 * clear the drawing widget and put in out prompt for a click
	 */

   DW_RemoveAll (dw);

   if (DW_SetFont (dw, canvas -> tool_font) == False)
      DW_SetFont (dw, "fixed");

   DW_GetTextExtents (dw, buffer, &label_width, &label_height);

   DW_DrawText (dw, True, (xmax - xmin)/2 + xmin - label_width/2, 
                          (ymax - ymin)/2 + ymin - label_height/2, buffer);

	/*
	 * quash all the events until we get a click in the drawing widget
	 */

   XtAddCallback (dw, XtNbuttonCallback, ClearToDraw, NULL);

   ready = 0;
   while (!ready) {
      XtAppNextEvent (app_context, &event);
      if (event.type == Expose || event.type == ConfigureNotify ||
          event.xbutton.window == XtWindow (dw))

         XtDispatchEvent (&event);
   }

   XtRemoveAllCallbacks (dw, XtNbuttonCallback);
   DW_RemoveAll (dw);
  

	/*
	 * now that the size is finalized, get the width and height
	 * and optimize the aspect ratio for this size
	 */

   count = 0;
   XtSetArg (arglist [count], XtNwidth, &width); count++;
   XtSetArg (arglist [count], XtNheight, &height); count++;
   XtGetValues (dw, arglist, count);

   w = (float) width;
   h = (float) height;

   if ((maxX - minX)/w > (maxY - minY)/h) {
      Xscale = w / (maxX - minX);
      Yscale = Xscale;
      height = (Dimension) ((maxY - minY)*Yscale);
   }
   else {
      Yscale = h / (maxY - minY);
      Xscale = Yscale;
      width = (Dimension) ((maxX - minX)*Xscale);
   }

	/*
	 * set all the new dimensions into the drawing widget
	 */

   count = 0;
   XtSetArg (arglist [count], XtNxMin, Float2Arg (minX)); count++;
   XtSetArg (arglist [count], XtNxMax, Float2Arg (maxX)); count++;
   XtSetArg (arglist [count], XtNyMin, Float2Arg (minY)); count++;
   XtSetArg (arglist [count], XtNyMax, Float2Arg (maxY)); count++;
   XtSetArg (arglist [count], XtNyScale, Float2Arg (Yscale)); count++;
   XtSetArg (arglist [count], XtNxScale, Float2Arg (Xscale)); count++;
   XtSetArg (arglist [count], XtNwidth, width); count++;
   XtSetArg (arglist [count], XtNheight, height); count++;
   XtSetValues (dw, arglist, count);

   *x_scale = Xscale;
   *y_scale = Yscale;
   *wx = width;
   *hy = height;
}
