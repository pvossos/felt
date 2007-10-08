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

/************************************************************************
 * File:	opengl.c						*
 *									*
 * Description:	This file contains the public and private functions	*
 *		needed to do post-processing graphics with OpenGL	*
 ************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/GLwDrawA.h> 
#include "FileDialog.h"
#include "Layout.h"
#include "problem.h"
#include "fe.h"
#include "colormap.h"
#include "globals.h"
#include "util.h"
#include "eps.h"
#include "ppm.h"
#include "xwd.h"
#include "opengl.h"

#define WEDGEWIDTH 100

static Display    *dpy;
static GLXContext  cx;
static GLboolean   doubleBuffer = GL_TRUE;
static GLuint      fontbase;

typedef struct {
   Widget    shell;
   Widget    mesa;
   double    smin, smax;
   GLfloat   xrot, yrot, zrot;
   GLfloat   xbase, ybase, zbase;
   GLuint    list;
   int       wedge;
   Dimension winW, winH;
   Boolean   contour;
} info;

typedef int face[4];

face bface[] = 
{
    {0, 1, 2, 3},
    {0, 3, 7, 4},
    {4, 7, 6, 5},
    {1, 2, 6, 5},
    {2, 3, 7, 6},
    {0, 1, 5, 4},
};

static void MakeFont(int *fwidth, int *fheight)
{
    XFontStruct *fontInfo;
    Font id;
    unsigned int first, last;

    fontInfo = XLoadQueryFont(dpy, "6x12");
    if (fontInfo == NULL) {
       *fwidth = 0;
       *fheight = 0;
       return;
    }

    id = fontInfo->fid;
    first = fontInfo->min_char_or_byte2;
    last = fontInfo->max_char_or_byte2;

    fontbase = glGenLists((GLuint) last+1);
    if (fontbase == 0) {
       *fwidth = 0;
       *fheight = 0;
       return;
    }
    glXUseXFont(id, first, last-first+1, fontbase+first);

    *fheight = fontInfo -> ascent + fontInfo -> descent;
    *fwidth  = fontInfo -> max_bounds.width;
}

static void PrintString(GLfloat x, GLfloat y, GLfloat z, char *s)
{
    glRasterPos3f(x, y, z);

    glPushAttrib (GL_LIST_BIT);
    glListBase(fontbase);
    glCallLists(strlen(s), GL_UNSIGNED_BYTE, (GLubyte *)s);
    glPopAttrib ();
}

static void DrawElements (info *inf)
{
    glCallList(inf -> list);
}

static void InitDisplay (info *inf)
{
  Arg	      args [2];
  Dimension   winW, winH;
  Widget      w;

  w = inf -> mesa;

  XtSetArg(args [0], XtNwidth, &winW);
  XtSetArg(args [1], XtNheight, &winH);
  XtGetValues(w, args, 2);

  inf -> winW = winW;
  inf -> winH = winH;

  glClearDepth (1.0);
  glClearColor (0.0, 0.0, 0.0, 0.0);

  glViewport(0, 0, winW - inf -> wedge, winH);
  glScissor(0, 0, winW - inf -> wedge, winH);

  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  gluPerspective(40, (double) (winW - inf -> wedge)/(double) winH, 1.0, 100.0);

  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();
}

static void RefreshDisplay (info *inf)
{
  glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glPushMatrix();	/* push I down the stack */

  glTranslatef (0.0, 0.0, -3.5); 
  glRotatef (inf -> xrot, 1.0, 0.0, 0.0);
  glRotatef (inf -> yrot, 0.0, 1.0, 0.0);
  glRotatef (inf -> zrot, 0.0, 0.0, 1.0);

  DrawElements (inf);

  glPopMatrix();	/* pop I back to stack top */

  glXSwapBuffers (dpy, XtWindow (inf -> mesa)); 
  if (!glXIsDirect(dpy, cx))
     glFinish ();
}

static void SaveFunction(Widget w, XtPointer client_data, XtPointer call_data)
{
   extern FileDialog  dumpd;
   info              *inf = (info *) client_data;
   String             save_file;
   String             format;
   FILE *fp;

   FileDialogSetToggles (dumpd, "PPM", "EPS");
   CenterOnScreen (XtNameToWidget (toplevel, "fileDialog"), True);
   WarpToCenter (XtNameToWidget (toplevel, "fileDialog"));
   FileDialogSelect (dumpd, "Save Plot", "Save file name:", 
                     "", &save_file, &format);

   if (save_file == NULL)
      return;

   if (strcmp (format, "EPS") == 0)
      WidgetToEPS (save_file, inf -> mesa);
   else if (strcmp(format, "PPM") == 0)
      WidgetToPPM (save_file, inf -> mesa);

   fp = fopen("temp.xwd", "wb");
   DumpWidget(inf -> mesa, fp);

   return;
}

static void DismissFunction(Widget w, XtPointer client_data, XtPointer call_data)
{
   int	 i;
   info *inf = (info *) client_data;

   XtDestroyWidget(inf -> shell);
   free(inf);
}

static void ToggleFunction(Widget w, XtPointer client_data, XtPointer call_data)
{
   Boolean	 state;
   Widget        dw = (Widget) client_data;
   Arg		 args [1];
   info		*inf = (info *) client_data;

   GLwDrawingAreaMakeCurrent (inf -> mesa, cx);

   XtSetArg (args [0], XtNstate, &state);
   XtGetValues (w, args, 1);

   inf -> contour = !state;
   glPolygonMode (GL_FRONT_AND_BACK, inf -> contour ? GL_FILL : GL_LINE);
   
   RefreshDisplay(inf);
}

static Boolean RotateZPos (XtPointer client_data)
{
   info		*inf = (info *) client_data;

   GLwDrawingAreaMakeCurrent (inf -> mesa, cx);

   inf -> zrot += 3;

   if (inf -> zrot >= 360 || inf -> zrot <= -360)
         inf -> zrot = 0.0;

   RefreshDisplay(inf);

   return False;
}

static Boolean RotateZNeg (XtPointer client_data)
{
   info		*inf = (info *) client_data;

   GLwDrawingAreaMakeCurrent (inf -> mesa, cx);

   inf -> zrot -= 3;

   if (inf -> zrot >= 360 || inf -> zrot <= -360)
         inf -> zrot = 0.0;

   RefreshDisplay(inf);

   return False;
}

static void
InputFunction(Widget w, XtPointer client_data, XtPointer call_data)
{
   info				*inf = (info *) client_data;
   static int           	 prev_x, prev_y;
   static int           	 rotating = 0;
   static XtWorkProcId  	 wpid = 0;
   int                           button;
   GLwDrawingAreaCallbackStruct *gs=(GLwDrawingAreaCallbackStruct *) call_data;

   GLwDrawingAreaMakeCurrent (inf -> mesa, cx);

   switch (gs -> event -> type) {

   case ButtonPress:
      button = gs -> event -> xbutton.button;
      if (button == 1) {
         prev_x = gs -> event -> xbutton.x;
         prev_y = gs -> event -> xbutton.y;
         rotating = 1;
      }
      else if (button == 2 && !wpid)  
         wpid = XtAppAddWorkProc (app_context, RotateZPos, (XtPointer) inf);
      else if (button == 3 && !wpid) 
         wpid = XtAppAddWorkProc (app_context, RotateZNeg, (XtPointer) inf);

      break;
   
   case ButtonRelease:
      button = gs -> event -> xbutton.button;
      if (button == 1) {
         inf -> xbase = inf -> xrot;
         inf -> ybase = inf -> yrot;
         rotating = 0;
      }
      else if (wpid) {
         XtRemoveWorkProc (wpid);
         wpid = 0;
      }

      break;

   case MotionNotify:
      if (rotating) {
         inf -> xrot = inf -> xbase + gs -> event -> xmotion.y - prev_y;
         inf -> yrot = inf -> ybase + gs -> event -> xmotion.x - prev_x;

         if (inf -> xrot >= 360 || inf -> xrot <= -360)
            inf -> xrot = 0.0;
         if (inf -> yrot >= 360 || inf -> yrot <= -360)
            inf -> yrot = 0.0;

         RefreshDisplay(inf);
      }
      break;
   }
}

	/*
	 * this does both initialization and drawing for the wedge
	 * because we need to set both projection and model view
 	 * matrices away from their normal 3D values every time
	 * we redraw the wedge ... we also need to be sure that
	 * we store and restore on exit all of the 3D viewport
	 * and transformation information
	 */

static void DrawWedge (info *inf)
{
   static int  fwidth, fheight;
   static int  init = 0;
   Arg         args [2];
   int	       i;
   double      dy;
   char        buffer [32];
   char	      *ptr;
   double      fscale;
   GLint       prev_mode;
   Widget      w;

   w = inf -> mesa;

   if (!init) {
      MakeFont (&fwidth, &fheight);
      init = 1;
   }
   
   glViewport(inf -> winW - inf -> wedge, 0, inf -> wedge, inf -> winH);

   glMatrixMode (GL_PROJECTION);
   glPushMatrix();
   glLoadIdentity ();
   gluOrtho2D(-1.0, 1.0, -1.0, 1.0);

   glMatrixMode (GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity ();

   dy = 1.8/253;

   glGetIntegerv(GL_POLYGON_MODE, &prev_mode); 
   glPolygonMode (GL_FRONT_AND_BACK, GL_FILL); 

   glBegin (GL_QUAD_STRIP);
      glColor3fv (colors [0]);
      glVertex3f (-0.8, -0.9, 0.0);
      glVertex3f (-0.2, -0.9, 0.0);
      for (i = 1 ; i < 254 ; i++) {
         glColor3fv (colors [i]);
         glVertex2f (-0.8, -0.9 + dy*i);
         glVertex2f (-0.2, -0.9 + dy*i);
      }
   glEnd ();

   if (fwidth) {
      fscale = 2.0/inf -> winH;

      glColor3f(1.0, 1.0, 1.0);

      sprintf (buffer, "%11.5g", inf -> smin);
      ptr = buffer; 
      while (isspace (*ptr)) ptr++;
      PrintString(-0.15, -0.9, 0.0, ptr);

      sprintf (buffer, "%11.5g", inf -> smax);
      ptr = buffer; 
      while (isspace (*ptr)) ptr++;
      PrintString(-0.15, 0.9 - fheight*fscale, 0.0, ptr);
   }

   glXSwapBuffers (dpy, XtWindow (w));
   if (!glXIsDirect(dpy, cx))
      glFinish ();
  
   glMatrixMode (GL_PROJECTION);
   glPopMatrix();

   glMatrixMode (GL_MODELVIEW);
   glPopMatrix();

   glPolygonMode (GL_FRONT_AND_BACK, prev_mode); 
   glViewport(0, 0, inf -> winW - inf -> wedge, inf -> winH);

   return;
}

	/*
	 * for safety sake we have to re-initialize the perspective
	 * view and the viewport because another GL widget may have
	 * been recently active and thus those transformation matrices
	 * are actually on the stack right now ... we'd have to do this
	 * any way on a resize, the safety part is doing it for expose
	 */

static void RedrawFunction(Widget w, XtPointer client_data, XtPointer call_data)
{
   info	*inf = (info *) client_data;

   GLwDrawingAreaMakeCurrent (w, cx);
   glPolygonMode (GL_FRONT_AND_BACK, inf -> contour ? GL_FILL : GL_LINE);

   InitDisplay(inf);

   glDisable(GL_SCISSOR_TEST);

   RefreshDisplay(inf);
  
   if (inf -> wedge)
      DrawWedge(inf);

   glEnable(GL_SCISSOR_TEST);
 
   return;
}

	/*
	 * also make sure things get re-initialized just once whenever
	 * we get the focus back ... this will happen before any input
	 * callbacks (user requested rotations) so that things start
	 * out right for the newly focused widget
	 */

static void HandleFocusEvent(Widget w, XtPointer client_data, 
                             XEvent *event, Boolean *cont)
{
   info	          *inf = (info *) client_data;
   static Window   prev_w = 0;

	/*
 	 * bail if we're losing focus or if this window is the most
	 * most recently focused window (resize or re-focus clicks
	 * generate FocusChange events)
	 */

   if (event -> type == FocusOut || prev_w == event -> xfocus.window) 
      return;
   
   prev_w = event -> xfocus.window; 

   RedrawFunction(inf -> mesa, (XtPointer) inf, NULL);
}
   
static void LoadResults(info *inf, Element *element, int numelts, float mag)
{
   int		i, j, k, nd;
   int		epn;
   double	x, y, z;
   double	xmax, xmin, ymax, ymin, zmax, zmin;
   double	xctr, yctr, zctr;
   double	xdist, ydist, zdist;
   double	scale;
   GLenum       modes[] = {0, 0, GL_LINES, GL_TRIANGLES, GL_QUADS};
   Node	  	n;

   xmin = ymin = zmin = inf -> smin = 1e12;
   xmax = ymax = zmax = inf -> smax = -1e12; 

   for (i = 0 ; i < numelts ; i++) {
      epn = element [i+1] -> definition -> shapenodes;

       for (j = 0 ; j < epn ; j++) {
         n = element [i+1] -> node [j+1];
         x = n -> x + mag*n -> dx [1];
         y = n -> y + mag*n -> dx [2];
         z = n -> z + mag*n -> dx [3];

         if (x  > xmax) 
            xmax = x;
         else if (x < xmin)
            xmin = x;

         if (y > ymax)
            ymax = y;
         else if (y < ymin)
            ymin = y;

         if (z  > zmax) 
            zmax = z;
         else if (z < zmin)
            zmin = z;
      }
   }

   xctr = (xmax + xmin)/2.0;
   yctr = (ymax + ymin)/2.0;
   zctr = (zmax + zmin)/2.0;
   xdist = xmax - xmin;
   ydist = ymax - ymin;
   zdist = zmax - zmin;
   
   if (xdist >= ydist && xdist >= zdist && xdist)
      scale = 2.0/xdist;
   else if (ydist >= xdist && ydist >= zdist && ydist)
      scale = 2.0/ydist;
   else if (zdist)
      scale = 2.0/zdist;
   else
      scale = 1.0;
 
   glNewList(inf -> list, GL_COMPILE);
   glColor3f(1.0, 1.0, 1.0);

   for (i = 0 ; i < numelts ; i++) {

      epn = element [i+1] -> definition -> shapenodes;

      if (element [i+1] -> definition -> shape == Solid) {	/* Solid */
         glBegin (GL_QUADS);
         for (j = 0 ; j < 6 ; j++) {
            for (k = 0 ; k < 4 ; k++) {
               nd = bface[j][k];
               n = element [i+1] -> node [nd+1];
               x = ((n -> x + mag*n -> dx [1]) - xctr)*scale;
               y = ((n -> y + mag*n -> dx [2]) - yctr)*scale;
               z = ((n -> z + mag*n -> dx [3]) - zctr)*scale;

               glVertex3f (x, y, z);
            }
         }
         glEnd ();
      }  
      else { /* Linear or Planar */
         glBegin (modes [epn]);
         for (j = 0 ; j < epn ; j++) {
            n = element [i+1] -> node [j+1];
            x = ((n -> x + mag*n -> dx [1]) - xctr)*scale;
            y = ((n -> y + mag*n -> dx [2]) - yctr)*scale;
            z = ((n -> z + mag*n -> dx [3]) - zctr)*scale;

            glVertex3f (x, y, z);
         }
         glEnd();
      }
   }

   glEndList();
}

static void LoadContourResults(info *inf, Boolean stress, int comp, Element *element, int numelts)
{
   int		i, j, k, nd;
   int		epn;
   int		idx;
   float        s;
   double	x, y, z;
   double	xmax, xmin, ymax, ymin, zmax, zmin;
   double	xctr, yctr, zctr;
   double	xdist, ydist, zdist;
   double	scale;
   GLenum       modes[] = {0, 0, GL_LINES, GL_TRIANGLES, GL_QUADS};

   xmin = ymin = zmin = inf -> smin = 1e12;
   xmax = ymax = zmax = inf -> smax = -1e12; 

   for (i = 0 ; i < numelts ; i++) {
      epn = element [i+1] -> definition -> shapenodes;

       for (j = 0 ; j < epn ; j++) {
         x = element [i+1] -> node [j+1] -> x;
         y = element [i+1] -> node [j+1] -> y;
         z = element [i+1] -> node [j+1] -> z;

         if (stress)
            s = element [i+1] -> node [j+1] -> stress [comp];
         else   
            s = element [i+1] -> node [j+1] -> dx [comp];
 
         if (s  > inf -> smax) 
            inf -> smax = s;
         else if (s < inf -> smin)
            inf -> smin = s;

         if (x  > xmax) 
            xmax = x;
         else if (x < xmin)
            xmin = x;

         if (y > ymax)
            ymax = y;
         else if (y < ymin)
            ymin = y;

         if (z  > zmax) 
            zmax = z;
         else if (z < zmin)
            zmin = z;
      }
   }

   xctr = (xmax + xmin)/2.0;
   yctr = (ymax + ymin)/2.0;
   zctr = (zmax + zmin)/2.0;
   xdist = xmax - xmin;
   ydist = ymax - ymin;
   zdist = zmax - zmin;
   
   if (xdist >= ydist && xdist >= zdist && xdist)
      scale = 2.0/xdist;
   else if (ydist >= xdist && ydist >= zdist && ydist)
      scale = 2.0/ydist;
   else if (zdist)
      scale = 2.0/zdist;
   else
      scale = 1.0;
 
   inf -> list = glGenLists(1);
   glNewList(inf -> list, GL_COMPILE);
   
   for (i = 0 ; i < numelts ; i++) {

      epn = element [i+1] -> definition -> shapenodes;

      if (element [i+1] -> definition -> shape == Solid) {	/* Solid */
         glBegin (GL_QUADS);
         for (j = 0 ; j < 6 ; j++) {
            for (k = 0 ; k < 4 ; k++) {
               nd = bface[j][k];
               x = (element [i+1] -> node [nd+1] -> x - xctr)*scale;
               y = (element [i+1] -> node [nd+1] -> y - yctr)*scale;
               z = (element [i+1] -> node [nd+1] -> z - zctr)*scale;

               if (stress)
                  s = element [i+1] -> node [nd+1] -> stress [comp];
               else   
                  s = element [i+1] -> node [nd+1] -> dx [comp];

               idx = (int) (253*(s - inf -> smin)
                                /(inf -> smax - inf -> smin));

               glColor3fv (colors [idx]);
               glVertex3f (x, y, z);
            }
         }
         glEnd ();
      }  
      else { /* Linear or Planar */
         glBegin (modes [epn]);
         for (j = 0 ; j < epn ; j++) {
            x = (element [i+1] -> node [j+1] -> x - xctr)*scale;
            y = (element [i+1] -> node [j+1] -> y - yctr)*scale;
            z = (element [i+1] -> node [j+1] -> z - zctr)*scale;

            if (stress)
               s = element [i+1] -> node [j+1] -> stress [comp];
            else   
               s = element [i+1] -> node [j+1] -> dx [comp];

            idx = (int) (253*(s - inf -> smin)
                             /(inf -> smax - inf -> smin));

            glColor3fv (colors [idx]);
            glVertex3f (x, y, z);
         }
         glEnd();
      }
   }

   glEndList();
}


static int dblBuf[] = {
    GLX_DOUBLEBUFFER, GLX_RGBA, GLX_DEPTH_SIZE, 16,
    GLX_RED_SIZE, 1, GLX_GREEN_SIZE, 1, GLX_BLUE_SIZE, 1,
    None
};

static int *snglBuf = &dblBuf[1];

static char layout_string [ ] =
"vertical { \
   8 \
   horizontal { \
      8 \
      mesa <+inf -100% * +inf -100%> \
      8 \
   } \
   24 \
   horizontal { \
      8 \
      dismiss \
      8 \
      save \
      8 \
      toggle \
      8 <+inf -100%> \
   } \
   8 \
}";

static String table =
"<Key>space: AutoRepeat(off) set()\n\
 <Key>Return: AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) unset() ShellAction(button)\n\
 <KeyUp>space: AutoRepeat(saved) unset() ShellAction(button)";

static void Action (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
   if (strcmp (params [0], "delete") == 0)
      w = XtNameToWidget (w, "layout.dismiss");

   XtCallCallbacks (w, XtNcallback, NULL);
}

void CreateOpenGLShell(String name, String title, Boolean stress,
                       int comp, Element *element, unsigned numelts, Boolean contour)
{
   static XVisualInfo  *vi = NULL;
   Widget	        group [3];
   Widget               mesa, shell, layout, dismiss, save, toggle;
   Cardinal             n;
   Arg	                args [10];
   Pixel	        highlight;
   XtTranslations       translations;
   static XtActionsRec  actions [ ] = {{"ShellAction", Action}};
   info                *inf;

   inf = (info *) malloc(sizeof(info));

   inf -> wedge = contour ? WEDGEWIDTH : 0;

   n = 0;
   XtSetArg (args [n], XtNtitle,    title); n++;
   XtSetArg (args [n], XtNiconName, title); n++;
   XtSetArg (args [n], XtNallowShellResize, True); n++;
   shell = XtCreatePopupShell (name, topLevelShellWidgetClass,
                               toplevel, args, n);

   inf -> shell = shell;
 
   if (vi == NULL) {
      dpy = XtDisplay(toplevel);
      vi = glXChooseVisual(dpy, DefaultScreen(dpy), dblBuf);
      if (vi == NULL) {
         vi = glXChooseVisual(dpy, DefaultScreen(dpy), snglBuf);
         if (vi == NULL)
            XtAppError(app_context, "no RGB visual with depth buffer");

         doubleBuffer = GL_FALSE;
      }

      cx = glXCreateContext(dpy, vi, None, GL_TRUE);
      if (cx == NULL)
         XtAppError(app_context, "could not create rendering context");
   }

   n = 0;
   XtSetArg (args [n], XtNlayout, StringToLayout (toplevel,layout_string)); n++;
   layout = XtCreateManagedWidget ("layout", layoutWidgetClass,
                                   shell, args, n);


   mesa = XtVaCreateManagedWidget ("mesa", glwDrawingAreaWidgetClass, layout,
    				     GLwNvisualInfo, vi, 
			  	     GLwNinstallColormap, True, 
				     XtNheight, 500, 
                                     XtNwidth, 500 + inf -> wedge, NULL);

   inf -> mesa = mesa;

   dismiss = XtVaCreateManagedWidget ("dismiss", commandWidgetClass, layout, NULL);
   save   = XtVaCreateManagedWidget ("save", commandWidgetClass, layout, NULL);
   toggle = XtVaCreateManagedWidget ("toggle", toggleWidgetClass, layout, 
                                     XtNlabel, "wireframe", NULL);

   XtAddCallback (dismiss, XtNcallback,      DismissFunction, (XtPointer) inf);
   XtAddCallback (save,   XtNcallback,       SaveFunction,    (XtPointer) inf);
   XtAddCallback (toggle,XtNcallback,        ToggleFunction,  (XtPointer) inf);
   XtAddCallback (mesa,  GLwNinputCallback,  InputFunction,   (XtPointer) inf);
   XtAddCallback (mesa,  GLwNresizeCallback, RedrawFunction,  (XtPointer) inf);
   XtAddCallback (mesa,  GLwNexposeCallback, RedrawFunction,  (XtPointer) inf);

   XtAddEventHandler(shell, FocusChangeMask, False, HandleFocusEvent, (XtPointer) inf);

   XtRealizeWidget (shell);

# ifdef SMOOTHLINES
   glEnable (GL_LINE_SMOOTH);
   glEnable (GL_BLEND);
   glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
# endif

   GLwDrawingAreaMakeCurrent (mesa, cx);

   glEnable (GL_DEPTH_TEST);
   glPolygonMode (GL_FRONT_AND_BACK, contour ? GL_FILL : GL_LINE);
   inf -> contour = contour;

   XtSetSensitive(toggle, contour);

   inf -> xrot = inf -> yrot = inf -> zrot = 0.0;
 
   inf -> list = glGenLists(1);

   if (contour)
      LoadContourResults(inf, stress, comp, element, numelts);
   else
      LoadResults(inf, element, numelts, 0.0);

/*
   group [0] = dismiss;
   group [1] = save;
   group [2] = toggle;

   XtSetArg (args [0], XtNborderColor, &highlight);
   XtGetValues (layout, args, 1);

   CreateTabGroup (shell, group, 3, highlight, True);
*/
   XtAppAddActions (app_context, actions, 1);

   translations = XtParseTranslationTable (table);
   XtOverrideTranslations (save, translations);
   XtOverrideTranslations (dismiss, translations);

   AddDeleteWindowProtocol (shell, "ShellAction(delete)");

   InitDisplay (inf);
   XtPopup(shell, XtGrabNone);

   return;
}
