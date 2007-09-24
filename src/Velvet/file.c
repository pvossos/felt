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
 * File:	file.c							*
 *									*
 * Description:	This file contains the function definitions related to	*
 *		the file menu.						*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <X11/Xos.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Shell.h>
# include "problem.h"
# include "util.h"
# include "error.h"
# include "FileDialog.h"
# include "OutputDialog.h"
# include "ElementList.h"
# include "Constraint.h"
# include "Element.h"
# include "Node.h"
# include "Material.h"
# include "Force.h"
# include "Load.h"
# include "Canvas.h"
# include "Solution.h"
# include "Analysis.h"
# include "LoadCase.h"
# include "Colors.h"
# include "globals.h"
# include "procedures.h"
# include "objects.h"
# include "Drawing.h"
# include "vfe.h"


extern FileDialog 	filed;
extern FileDialog	dumpd;
extern ConstraintDialog constraint_d;
extern ForceDialog	force_d;
extern LoadDialog	load_d;
extern MaterialDialog	material_d;
extern ElementList	element_l;
extern CanvasDialog	canvas_d;
extern SolutionDialog	solution_d;
extern AnalysisDialog   analysis_d;
extern ElementDialog	element_d;
extern NodeDialog	node_d;
extern ColorsDialog	colors_d;
extern LoadCaseDialog   loadcase_d;

static void CanvasToAppearance ( );


/************************************************************************
 * Function:	OpenFile						*
 *									*
 * Description:	Prompt for and attempt to open a file.			*
 ************************************************************************/

void OpenFile ( )
{
   String	ans;
   static char	suggestion [256] = "";


   if (changeflag == True)
      if (!QuerySave ()) return;

   strcpy (suggestion, filename);
         
   CenterOnScreen (XtNameToWidget (toplevel, "fileDialog"), True);
   WarpToCenter (XtNameToWidget (toplevel, "fileDialog"));
   FileDialogSelect (filed, "Open Problem","Problem name:", 
                     suggestion, &ans, NULL);

   if (ans != NULL) {
      if (!VelvetReadFeltFile (ans)) {
	 strcpy (filename, ans);
	 UpdateFilenameDisplay ( );
	 changeflag = False;
      }
   }
}


/************************************************************************
 * Function:	RestoreOriginal						*
 *									*
 * Description:	Reload the current file.				*
 ************************************************************************/

void RestoreOriginal ( )
{
   String	selected;


   if (changeflag == True) {
      OutputDialogPrintf (proceed_dialog, "Are you sure?");
      CenterOnWidget (OutputDialogShell(proceed_dialog), toplevel, True);
      WarpToCenter (OutputDialogShell (proceed_dialog));
      selected = OutputDialogSelect (proceed_dialog, "Restore Problem", "okay");
 
      if (selected && strcmp (selected,"okay"))
	 return;

      changeflag = False;
   }

   if (!VelvetReadFeltFile (filename))
      UpdateFilenameDisplay ( );
} 


/************************************************************************
 * Function:	WriteNamedFile						*
 *									*
 * Description:	Writes a named file.					*
 ************************************************************************/
   
void WriteNamedFile (dump_all)
   Boolean	dump_all;
{
   String	ans;

   CenterOnScreen (XtNameToWidget (toplevel, "fileDialog"), True);
   WarpToCenter (XtNameToWidget (toplevel, "fileDialog"));
   if (!dump_all)
      FileDialogSelect (filed, "Save Problem","Problem name:", 
                        filename, &ans, NULL);
   else
      FileDialogSelect (filed, "Save All","Filename:", filename, &ans, NULL);

   if (ans != NULL) {
      strcpy (filename, ans);
      UpdateFilenameDisplay ( );
      WriteVFeltFile (dump_all);
      changeflag = False;
   }
}


/************************************************************************
 * Function:	WriteVFeltFile						*
 *									*
 * Description:	Writes the current file.				*
 ************************************************************************/

int WriteVFeltFile (dump_all)
    Boolean	dump_all;
{
    CanvasToAppearance ( );

    CompactNodeNumbers ( );
    CompactElementNumbers ( );

    if (dump_all)
       DumpFeltFile (filename);
    else
       WriteFeltFile (filename); 

    return 0;
}


/************************************************************************
 * Function:	VelvetReadFeltFile					*
 *									*
 * Descripton:	Reads a named file.					*
 ************************************************************************/

int VelvetReadFeltFile (file)
    char *file;
{
    int      status;
    Element  elt;


    BufferErrors (True);
    status = ReadFeltFile (file);
    BufferErrors (False);

    if (status)
	return 1;


    solution -> title = problem.title;

    DW_RemoveAll (drawing);
    DrawProblem (0.0);

    ConstraintDialogUpdate (constraint_d, problem.constraint_tree);
    ForceDialogUpdate	   (force_d, problem.force_tree);
    LoadDialogUpdate	   (load_d, problem.distributed_tree);
    MaterialDialogUpdate   (material_d, problem.material_tree);
    NodeDialogUpdate	   (node_d, problem.node_tree, problem.force_tree,
			    problem.constraint_tree);
    ElementDialogUpdate	   (element_d, problem.element_tree,
			    problem.material_tree, problem.distributed_tree,
			    problem.node_tree);
    LoadCaseDialogUpdate   (loadcase_d, problem.loadcase_tree,
                            problem.force_tree, problem.distributed_tree);

    ColorsDialogUpdateObjectList (colors_d, problem.material_tree, False);
    ColorsDialogUpdateObjectList (colors_d, problem.constraint_tree, False);
    ColorsDialogUpdateObjectList (colors_d, problem.force_tree, False);
    ColorsDialogUpdateObjectList (colors_d, problem.distributed_tree, False);

    AnalysisDialogUpdate (analysis_d, True);
    SolutionDialogUpdate (solution_d);

    elt = (Element) TreeMinimum (problem.element_tree);
    if (elt != NULL) 
        ElementListSet (element_l, elt -> definition);
    else 
        ElementListSet (element_l, NULL);

    return 0;
}


/************************************************************************
 * Function:	StartNew						*
 *									*
 * Description:	Start a new problem.					*
 ************************************************************************/

void StartNew ()
{
    if (changeflag) 
       if (!QuerySave ()) return; 

    changeflag = False;
    filename [0] = 0;
    UpdateFilenameDisplay ();
    solution -> title = XtNewString ("Unnamed");
    SolutionDialogUpdate (solution_d);

    DW_RemoveAll (drawing);
    DestroyProblem ((ItemDestructor) DestroyMaterial);
    TreeDestroy (figure_tree);

    ElementListSet (element_l, NULL);

    ReadFeltFile (NULL);

    AnalysisDialogUpdate (analysis_d, True);

    figure_tree	= TreeCreate (figure_cmp);

    ConstraintDialogUpdate (constraint_d, problem.constraint_tree);
    ForceDialogUpdate	   (force_d, problem.force_tree);
    LoadDialogUpdate	   (load_d, problem.distributed_tree);
    MaterialDialogUpdate   (material_d, problem.material_tree);
    NodeDialogUpdate	   (node_d, problem.node_tree, problem.force_tree,
			    problem.constraint_tree);
    ElementDialogUpdate	   (element_d, problem.element_tree,
			    problem.material_tree, problem.distributed_tree,
			    problem.node_tree);
    LoadCaseDialogUpdate   (loadcase_d, problem.loadcase_tree,
                            problem.force_tree, problem.distributed_tree);

    ColorsDialogUpdateObjectList (colors_d, problem.material_tree, False);
    ColorsDialogUpdateObjectList (colors_d, problem.constraint_tree, False);
    ColorsDialogUpdateObjectList (colors_d, problem.force_tree, False);
    ColorsDialogUpdateObjectList (colors_d, problem.distributed_tree, False);
}


/************************************************************************
 * Function:	QuerySave						*
 *									*
 * Description:	Queries the user to save a file.			*
 ************************************************************************/

Boolean QuerySave ( )
{
    String	selected;

    OutputDialogPrintf (qsave_dialog,"Changes not saved, save now?");
    CenterOnWidget (OutputDialogShell(qsave_dialog), toplevel, True);
    WarpToCenter (OutputDialogShell (qsave_dialog));
    selected = OutputDialogSelect(qsave_dialog,"Are You Sure?","no");

    if (selected == NULL || strcmp (selected,"cancel") == 0) 
        return False;
    else if (strcmp (selected,"yes") == 0) {
       WriteVFeltFile (False);
       return True;
    }
  
    return True;
}


/************************************************************************
 * Function:	QuitVelvet						*
 *									*
 * Description:	Exits velvet.						*
 ************************************************************************/
    
void QuitVelvet ( )
{
   if (changeflag) 
      if (!QuerySave ( ))
	 return;

   XtUnmapWidget (toplevel);
   XtDestroyApplicationContext (app_context);
   exit (0);
}


/************************************************************************
 ************************************************************************/

Problem     saved;

static int UpdateMaterial (item)
    Item item;
{
    Item     found;
    Material old;
    Material new;


    found = TreeInsert (saved.material_tree, item);
    if (found != item) {
	new = (Material) item;
	old = (Material) found;
	old -> E = new -> E;
	old -> A = new -> A;
	old -> Ix = new -> Ix;
	old -> Iy = new -> Iy;
	old -> Iz = new -> Iz;
	old -> J = new -> J;
	old -> G = new -> G;
	old -> nu = new -> nu;
	old -> t = new -> t;
	old -> rho = new -> rho;
	old -> kappa = new -> kappa;
	DestroyMaterial (new);
    }
    return 0;
}


/************************************************************************
 ************************************************************************/

void OpenMaterialFile ( )
{
    String	ans;
    static char suggestion [256] = "";
    int         status;


    CenterOnScreen (XtNameToWidget (toplevel, "fileDialog"), True);
    WarpToCenter (XtNameToWidget (toplevel, "fileDialog"));
    FileDialogSelect (filed, "Open Material Database","Database name:",
                      suggestion, &ans, NULL);

    if (ans == NULL)
	return;

    saved = problem;

    BufferErrors (True);
    status = ReadFeltFile (ans);
    BufferErrors (False);

    if (status)
	DestroyProblem ((ItemDestructor) DestroyMaterial);
    else {
	(void) TreeSetIterator (problem.material_tree, UpdateMaterial);
	(void) TreeIterate (problem.material_tree);
    }

    DestroyProblem (NULL);

    problem = saved;

    MaterialDialogUpdate (material_d, problem.material_tree);
    ColorsDialogUpdateObjectList (colors_d, problem.material_tree, False);
    ElementDialogUpdate	(element_d, problem.element_tree,
			 problem.material_tree, NULL, NULL);

    changeflag = True;
}


/************************************************************************
 ************************************************************************/

static FILE *fp;

static int WriteMaterial (item)
    Item item;
{
    Material material = (Material) item;


    if (strpbrk (material -> name, "- \t\n=[]\",+*/()#"))
	fprintf (fp, "\"%s\"  ", material -> name);
    else
	fprintf (fp, "%s  ", material -> name);

    fprintf (fp, "E=%g A=%g ", material -> E, material -> A);
    fprintf (fp, "Ix=%g Iy=%g ", material -> Ix, material -> Iy);
    fprintf (fp, "Iz=%g J=%g ", material -> Iz, material -> J);
    fprintf (fp, "G=%g nu=%g ", material -> G, material -> nu);
    fprintf (fp, "t=%g rho=%g ", material -> t, material -> rho);
    fprintf (fp, "Rk=%g Rm=%g ", material -> Rk, material -> Rm);
    fprintf (fp, "kappa=%g\n", material -> kappa);
    return 0;
}


/************************************************************************
 ************************************************************************/

void WriteMaterialFile ( )
{
    String 	ans;
    static char suggestion [256] = "";


    CenterOnScreen (XtNameToWidget (toplevel, "fileDialog"), True);
    WarpToCenter (XtNameToWidget (toplevel, "fileDialog"));
    FileDialogSelect (filed, "Update Material Database","Database name:",
                      suggestion, &ans, NULL);

    if (ans == NULL)
	return;

    if ((fp = fopen (ans, "w")) == NULL) {
	error ("Unable to open file %s.\n", ans);
	return;
    }

    fprintf (fp, "material properties\n");
    (void) TreeSetIterator (problem.material_tree, WriteMaterial);
    (void) TreeIterate (problem.material_tree);
    fprintf (fp, "\nend\n");
    (void) fclose (fp);
}


/************************************************************************
 ************************************************************************/

void UpdateFilenameDisplay ()
{
   Arg	arglist [1];

   if (filename [0])
      XtSetArg (arglist [0], XtNtitle, filename);
   else
      XtSetArg (arglist [0], XtNtitle, "UnNamed");

   XtSetValues (toplevel, arglist, 1);
}


/************************************************************************
 ************************************************************************/

void SaveWidgetPS (widget, ps_file)
   Widget	widget;
   String	ps_file;
{
   int		status;
   Figure	*figures;
   unsigned     num_figures;
   Point	points [4];
   float	xmin, xmax;
   float	ymin, ymax;
   Arg		args [4];

   if (widget == drawing) {
      points [0].x = canvas -> xmin;
      points [0].y = canvas -> ymin;
      points [1].x = canvas -> xmax;
      points [1].y = canvas -> ymin;
      points [2].x = canvas -> xmax;
      points [2].y = canvas -> ymax;
      points [3].x = canvas -> xmin;
      points [3].y = canvas -> ymax;

      figures = DW_SearchArea (drawing, points, 4, &num_figures); 
  
      xmin = canvas -> xmin;
      xmax = canvas -> xmax;
      ymin = canvas -> ymin;
      ymax = canvas -> ymax;
   }
   else {
      figures = DW_RetrieveAll (widget, True, &num_figures);

      XtSetArg (args [0], XtNxMin, &xmin);
      XtSetArg (args [1], XtNxMax, &xmax);
      XtSetArg (args [2], XtNyMin, &ymin);
      XtSetArg (args [3], XtNyMax, &ymax);

      XtGetValues (widget, args, 4);
   }

   if (figures != NULL && num_figures > 0) {
      status = WritePostscriptFigures (figures, num_figures, drawing, 
                                       xmin, xmax, ymin, ymax, ps_file);
      XtFree ((char *) figures);
   }
   else
      error ("nothing to dump");

   return;
}


/************************************************************************
 ************************************************************************/

void DumpDrawingArea (widget, title, allow_ps)
   Widget	widget;
   String	title;
   Boolean	allow_ps;
{
   String	save_file;
   String	format;

   if (allow_ps) {
      FileDialogSetToggles (dumpd, "XWD", "PostScript");
      CenterOnScreen (XtNameToWidget (toplevel, "dumpDialog"), True);
      WarpToCenter (XtNameToWidget (toplevel, "dumpDialog"));
      FileDialogSelect (dumpd, title, "Save file name:", "", 
                        &save_file, &format);
   }
   else {
      CenterOnScreen (XtNameToWidget (toplevel, "fileDialog"), True);
      WarpToCenter (XtNameToWidget (toplevel, "fileDialog"));
      FileDialogSelect (filed, title, "Save file name:", "", &save_file, NULL);

      format = "XWD";
   }

   if (save_file == NULL)
      return;
  
   if (strcmp (format, "XWD") == 0)
      SaveWidgetXWD (widget, save_file);
   else if (strcmp (format, "PostScript") == 0)
      SaveWidgetPS (widget, save_file); 
}


/************************************************************************
 ************************************************************************/

void SaveWidgetXWD (widget, xwd_file)
   Widget	widget;
   String	xwd_file;
{
   FILE		*output;

   output = fopen (xwd_file, "w");
   if (output == NULL) {
      error ("could not open file for writing");
      return;
   }
 
   if (DumpWidget (widget, output)) {
      fclose (output);
      unlink (xwd_file);
      return;
   }

   fclose (output); 
}


/************************************************************************
 * Function:	CanvasToAppearance					*
 *									*
 * Description:	Transfer the canvas structure and figure tree to the	*
 *		appearance structure.					*
 ************************************************************************/

static void CanvasToAppearance ( )
{
    Arg		     args [3];
    float	     scale;
    unsigned	     size;
    int		     i;
    int		     j;
    Position	     x;
    Position	     y;
    Dimension	     width;
    Dimension	     height;
    unsigned	     num_figures;
    Figure	    *figure_list;
    FigInfo	    *info;
    FigureAttributes attributes;
    char	    *last_color;
    char	    *last_font;


    /* Retrieve information about the drawing widget. */

    XtSetArg (args [0], XtNx,	   &x);
    XtSetArg (args [1], XtNy,	   &y);
    XtSetArg (args [2], XtNxScale, &scale);
    XtGetValues (drawing, args, 3);

    XtSetArg (args [0], XtNwidth,  &width);
    XtSetArg (args [1], XtNheight, &height);
    XtGetValues (viewport, args, 2);


    /* Initialize the appearance structure, freeing any existing appearance. */

    InitAppearance ( );


    /* Construct the appearance structure. */

    appearance.x_pos  = x;
    appearance.y_pos  = y;
    appearance.width  = width;
    appearance.height = height;
    appearance.scale  = scale;

    appearance.x_min = canvas -> xmin;
    appearance.x_max = canvas -> xmax;
    appearance.y_min = canvas -> ymin;
    appearance.y_max = canvas -> ymax;

    appearance.grid	       = canvas -> grid;
    appearance.snap	       = canvas -> snap;
    appearance.snap_size       = canvas -> snap_size;
    appearance.grid_size       = canvas -> grid_size;
    appearance.node_numbers    = canvas -> node_numbers;
    appearance.element_numbers = canvas -> element_numbers;

    appearance.node_color    = strdup (canvas -> node_color);
    appearance.element_color = strdup (canvas -> element_color);
    appearance.label_font    = strdup (canvas -> label_font);
    appearance.tool_color    = strdup (canvas -> tool_color);
    appearance.tool_font     = strdup (canvas -> tool_font);


    /* Initialize the structure and retrieve the display list. */

    appearance.num_figures = 0;
    size = TreeSize (figure_tree) * sizeof (FigInfo);
    appearance.figures = (FigInfo *) XtMalloc (size);

    last_font = NULL;
    last_color = NULL;
    figure_list = DW_RetrieveAll (drawing, True, &num_figures);


    /* Build each figure. */

    for (i = num_figures - 1; i >= 0; i --) {
	DW_GetAttributes (drawing, figure_list [i], &attributes);
	if (attributes.user_data)
	    continue;

	info = &appearance.figures [appearance.num_figures ++];
	info -> font = NULL;
	info -> text = NULL;
	info -> color = NULL;

	if (!last_color || strcmp (last_color, attributes.color))
	    info -> color = last_color = strdup (attributes.color);

	switch (attributes.type) {
	case RectangleFigure:
	    info -> type   = RECTANGLE;
	    info -> x      = attributes.x;
	    info -> y      = attributes.y;
	    info -> width  = attributes.width;
	    info -> height = attributes.height;
	    break;

	case LineFigure:
	case PolygonFigure:
	    info -> type = POLYLINE;
	    size = attributes.npoints * sizeof (FigInfoPair);
	    info -> num_points = attributes.npoints;
	    info -> points = (FigInfoPair *) XtMalloc (size);
	    for (j = 0; j < attributes.npoints; j ++) {
		info -> points [j].x = attributes.points [j].x;
		info -> points [j].y = attributes.points [j].y;
	    }
	    break;

	case TextFigure:
	    info -> type   = TEXT;
	    info -> x      = attributes.x;
	    info -> y      = attributes.y;
	    info -> text   = strdup (attributes.text);
	    if (!last_font || strcmp (last_font, attributes.font))
		info -> font = last_font = strdup (attributes.font);
	    break;

	case ArcFigure:
	    info -> type   = ARC;
	    info -> x      = attributes.x;
	    info -> y      = attributes.y;
	    info -> width  = attributes.width;
	    info -> height = attributes.height;
	    info -> start  = attributes.arc_start;
	    info -> length = attributes.arc_length;
	    break;

	default:
	    break;
	}
    }

    XtFree ((char *) figure_list);
}
