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
 * File:	velvet.c						*
 *									*
 * Description:	This file contains the routines for the top level of	*
 *		the velvet application.					*
 ************************************************************************/

# include <stdio.h>
# include <unistd.h>
# include <stdlib.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xresource.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Dialog.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/Reports.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/Sme.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/Viewport.h>
# include "problem.h"
# include "fe.h"
# include "error.h"
# include "definition.h"
# include "Drawing.h"
# include "FileDialog.h"
# include "OutputDialog.h"
# include "ElementList.h"
# include "Constraint.h"
# include "Element.h"
# include "Force.h"
# include "Material.h"
# include "Load.h"
# include "Colors.h"
# include "Node.h"
# include "Canvas.h"
# include "Solution.h"
# include "Analysis.h"
# include "LoadCase.h"
# include "Wireframe.h"
# include "Contour.h"
# include "dialog.h"
# include "procedures.h"
# include "globals.h"
# include "callbacks.h"
# include "panel.h"
# include "vfe.h"
# include "interface.h"
# include "check.xbm"
# include "question.xbm"
# include "menu16.xbm"
# include "popup.xbm"
# include "FElt.icon"


static String defaults_2d = "\
    *shapeStyle:                          Oval\n\
    *control*spacer.height:               119\n\
    *control*MenuButton*height:           19\n\
    *control*Command*height:              19\n\
    *elementDialog*MenuButton*shapeStyle: Rectangle\n\
    *nodeDialog*MenuButton*shapeStyle:    Rectangle\n\
    *elementDialog*help*shapeStyle:       Oval\n\
    *nodeDialog*help*shapeStyle:          Oval\n\
";


static String defaults_3d = "\
    *shapeStyle:                Rectangle\n\
    *control*spacer.height:     43\n\
    *control*MenuButton*height: 26\n\
    *control*Command*height:    26\n\
";


static char *defaults [ ] = {
# include "velvet.ad.h"
NULL
};


static char *about_message = "\
Velvet v3.02 copyright 1993-2000 by\n\
Jason I. Gobat (jgobat@mit.edu) and\n\
Darren C. Atkinson (atkinson@ucsd.edu)";


/* the global dialogs that we will create in main() or on the fly */

FileDialog	  dumpd;
FileDialog	  filed;
CanvasDialog	  canvas_d;
SolutionDialog	  solution_d;
WireframeDialog	  wireframe_d;
ContourDialog	  contour_d;
ConstraintDialog  constraint_d;
ForceDialog	  force_d;
LoadDialog	  load_d;
LoadCaseDialog	  loadcase_d;
MaterialDialog	  material_d;
ElementDialog	  element_d;
NodeDialog	  node_d;
ElementList	  element_l;
AnalysisDialog	  analysis_d;
ColorsDialog	  colors_d;

Dialog 		*info_dialog;

PanelId  last_command = InvalidId;

/************************************************************************
 * Function:	 PanelCallback						*
 *									*
 * Parameters:	 widget     - callback widget (ignored)			*
 *		 clientData - pointer to panel id			*
 *		 callData   - callback data (ignored)			*
 *									*
 * Return value: none							*
 *									*
 * Calls:	 ???							*
 *									*
 * Called by:	 X11							*
 *									*
 * Global data:	 changeflag may be modified				*
 *									*
 * Description:	 Panelcallback is called when an action is taken on the	*
 *		 control panel.  The client data is a pointer to the	*
 *		 panel id which indicates what action is to be taken.	*
 ************************************************************************/

void PanelCallback (Widget widget, XtPointer clientData, XtPointer callData)
{
    if (!edit_mode) {

       last_command = *(PanelId *) clientData;
       switch (*(PanelId *) clientData) {
       case AddNode:
           EditAddNode ();
           break;

       case DeleteNode:
	   EditDeleteNode ();
	   break;

       case EditNode:
           EditNodeNumber ();
           break;

       case MoveNodeId:
	   MoveNodeNumber ();
	   break;

       case MassNode:
           EditNodalMass ();
           break;

       case RenumberId:
           OptimizeNumbering ();
           break; 

       case RenumberSolveId:
           ToggleRenumberStatus ();
           break;

       case AddElt:
           EditAddElement ();
	   break;

       case DeleteElt:
	   EditDeleteElement ();
	   break;

       case EditElement:
           EditElementNumber ();
           break;

       case SetType:
           ElementListPopup (element_l);
           break;

       case Generate:
           GenerateElements ();
	   break;

       case DefMaterial:
           MaterialDialogPopup (material_d);
	   break;

       case OpenMaterial:
           OpenMaterialFile ();
	   break;

       case SaveMaterial:
           WriteMaterialFile ();
	   break;

       case ApplyMaterial:
           EditApplyMaterial ();
           break;

       case ApplyForce:
	   EditApplyForce ();
	   break;

       case ApplyLoad:
	   EditApplyLoad ( );
	   break;

       case ApplyConstraint:
	   EditApplyConstraint ();
	   break;

       case DefForce:
           ForceDialogPopup (force_d);
	   break;

       case DefLoad:
           LoadDialogPopup (load_d);
	   break;

       case DefConstraint:
           ConstraintDialogPopup (constraint_d);
	   break;

       case ProbAnalysis:
           AnalysisDialogPopup (analysis_d);
           break;

       case LoadCases:
           LoadCaseDialogPopup (loadcase_d);
           break;

       case AnimateId:
           SetupAnimate ( );
           break;

       case Solve:
           SetupAndSolve ( );
           break;

       case Output:
           SolutionDialogPopup (solution_d);
           break;

       case Wireframe:
           WireframeDialogPopup (wireframe_d);
           break;

       case Contour:
           ContourDialogPopup (contour_d);
           break;

       case PlotDisplacement:
           SetupDisplacements (True);
           break;
 
       case PlotStress:
           SetupStresses (True);
           break;

       case PlotStructure:
           SetupStructure (True);
           break;

       case Configure:
           CanvasDialogPopup (canvas_d);
	   break;

       case ColorControl:
	   ColorsDialogPopup (colors_d);
           break;

       case Recolor:
	   RecolorCanvas ( );
           break;

       case ZoomOut:
           ZoomAll ();
           break;

       case ZoomIn:
           ZoomStart ();
           break;
 
       case SaveXWD:
           DumpDrawingArea (drawing, "Save Drawing Area", True);
           break;

       case NodeNumbersOnOff:
           ToggleNodeNumberStatus ();
           break;

       case EltNumbersOnOff:
           ToggleEltNumberStatus ();
           break;

       case SnapOnOff:
           ToggleSnapStatus ();
           break;

       case GridOnOff:
           ToggleGridStatus ();
           break;

       case LineTool:
           ToolsDrawLine ();
	   break;

       case Circle:
           ToolsDrawCircle ();
	   break;

       case Arc:
           ToolsDrawArc ();
	   break;

       case Rectangle:
           ToolsDrawRectangle ();
	   break;

       case Polygon:
           ToolsDrawPolygon ();
	   break;

       case Text:
	   ToolsDrawText ();
           break;

       case DeleteTool:
	   ToolsDeleteFigure ();
           break;

       case MoveFigure:
           MoveTool ();
           break;

       case NewId:
           StartNew ();
	   break;

       case Open:
           OpenFile ();
	   break;

       case Save:
	   if (!filename [0])
	      WriteNamedFile (False);
	   else {
              if (!WriteVFeltFile (False))
	 	 changeflag = False;
	   }
	   break;

       case SaveAll:
           WriteNamedFile (True);
           break;

       case SaveAs:
           WriteNamedFile (False);
	   break;

       case Restore:
           RestoreOriginal ();
	   break;

       case Exit:
           QuitVelvet ();
           break;

       case Info:
	   PopupDialog (info_dialog, about_message, NULL, NULL);
	   break;

       case Quit:
       case Abort:
	   break;

       } /* end switch top of control panel */

    } /* end if !edit_mode */
  
    else {

       switch (*(PanelId *) clientData) {
       case Configure:
           CanvasDialogPopup (canvas_d);
	   break;

       case ColorControl:
	   ColorsDialogPopup (colors_d);
           break;

       case SnapOnOff:
           ToggleSnapStatus ();
           break;

       case GridOnOff:
           ToggleGridStatus ();
           break;

       case NodeNumbersOnOff:
           ToggleNodeNumberStatus ();
           break;

       case EltNumbersOnOff:
           ToggleEltNumberStatus ();
           break;

       case DefMaterial:
           MaterialDialogPopup (material_d);
	   break;

       case DefForce:
           ForceDialogPopup (force_d);
	   break;

       case DefLoad:
           LoadDialogPopup (load_d);
	   break;

       case DefConstraint:
           ConstraintDialogPopup (constraint_d);
	   break;

       case Info:
	   PopupDialog (info_dialog, about_message, NULL, NULL);
	   break;

       default:
           XBell (XtDisplay (toplevel), 20);

       } /* end switch bottom of control panel */
    }
}


/************************************************************************
 * Function:	SelectCallback
 ************************************************************************/

void SelectCallback (Widget w, XtPointer clientData, XtPointer callData)
{
    DrawingReport   *report;
    FigureAttributes attributes;
    Figure           figure;
    Node             node;
    Element          element;
    Drawn            drawn;


    report = (DrawingReport *) callData;

    if (report -> event -> type != ButtonPress)
	return;

    figure = DW_FindFigure (w, report -> unsnapped.x, report -> unsnapped.y);

    if (figure == NULL)
	return;

    DW_GetAttributes (w, figure, &attributes);
    if (attributes.user_data == NULL)
	return;

    node = (Node) attributes.user_data;
    element = (Element) attributes.user_data;
    drawn = (Drawn) node -> aux;

    if (drawn -> type == DrawnNode) {
        if (report -> event -> xbutton.button == 1) {
	   NodeDialogDisplay (node_d, node);
	   NodeDialogPopup (node_d);
	} else if (report -> event -> xbutton.button == 2)
	   DoMoveNode (node, True);
        else if (report -> event -> xbutton.button == 3) {
           ConstraintDialogDisplay (constraint_d, node -> constraint);
           ConstraintDialogPopup (constraint_d);
           if (node -> force) {
              ForceDialogDisplay (force_d, node -> force);
              ForceDialogPopup (force_d);
           }
	   NodeDialogDisplay (node_d, node);
	   NodeDialogPopup (node_d);
	}
    } else if (drawn -> type == DrawnElement) {
        if (report -> event -> xbutton.button == 1) {
	    ElementDialogDisplay (element_d, element);
	    ElementDialogPopup (element_d);
	}
        else if (report -> event -> xbutton.button == 3) {
	    ElementDialogDisplay (element_d, element);
	    ElementDialogPopup (element_d);
            MaterialDialogDisplay (material_d, element -> material);
            MaterialDialogPopup (material_d);
            if (element -> numdistributed > 0) {
               LoadDialogDisplay (load_d, element -> distributed [1]);
               LoadDialogPopup (load_d);
            }
	}
    }
}


static XrmOptionDescRec options [ ] = {
    {"-sensitive",    "*sensitiveMenus", XrmoptionNoArg, "True"},
    {"+sensitive",    "*sensitiveMenus", XrmoptionNoArg, "False"},
    {"-numbers",      "*numbers",        XrmoptionNoArg, "True"},
    {"+numbers",      "*numbers",        XrmoptionNoArg, "False"},
    {"-nodeColor",    "*nodeColor",      XrmoptionSepArg, NULL},
    {"-elementColor", "*elementColor",   XrmoptionSepArg, NULL},
    {"-toolColor",    "*toolColor",      XrmoptionSepArg, NULL},
    {"-labelFont",    "*labelFont",      XrmoptionSepArg, NULL},
    {"-toolFont",     "*toolFont",       XrmoptionSepArg, NULL},
};

struct resources {
    Boolean sensitive;
    Boolean numbers;
    String  nodecolor;
    String  elementcolor;
    String  toolcolor;
    String  labelfont;
    String  toolfont;
} appResources;

# define offset(field) XtOffsetOf (struct resources, field)
static XtResource Resources [ ] = {
{"sensitiveMenus", "SensitiveMenus", XtRBoolean, sizeof (Boolean),
    offset (sensitive), XtRImmediate, (XtPointer) True},
{"numbers", "Numbers", XtRBoolean, sizeof (Boolean),
    offset (numbers), XtRImmediate, (XtPointer) True},
{"nodeColor", "NodeColor", XtRString, sizeof (String),
    offset (nodecolor), XtRImmediate, (XtPointer) "blue"},
{"elementColor", "ElementColor", XtRString, sizeof (String),
    offset (elementcolor), XtRImmediate, (XtPointer) "black"},
{"toolColor", "ToolColor", XtRString, sizeof (String),
    offset (toolcolor), XtRImmediate, (XtPointer) "red"},
{"labelFont", "LabelFont", XtRString, sizeof (String),
    offset (labelfont), XtRImmediate, (XtPointer) "5x8"},
{"toolFont", "ToolFont", XtRString, sizeof (String),
    offset (toolfont), XtRImmediate, (XtPointer) "fg-22"},
};
# undef offset


static void GetArgs (void)
{
    XtGetApplicationResources (toplevel, &appResources, Resources,
				XtNumber (Resources), NULL, 0);

    sensitive_menus	      = appResources.sensitive;
    canvas -> node_color      = appResources.nodecolor;
    canvas -> element_color   = appResources.elementcolor;
    canvas -> tool_color      = appResources.toolcolor;
    canvas -> label_font      = appResources.labelfont;
    canvas -> tool_font       = appResources.toolfont;
    canvas -> node_numbers    = appResources.numbers;
    canvas -> element_numbers = appResources.numbers;
}


/************************************************************************
 * Function:	 main							*
 *									*
 * Description:	 Main is the startup function for the velvet program.	*
 ************************************************************************/

int main (int argc, char **argv)
{
    int		   status = 0;
    int		   i, j;
    char	   string [32];
    Arg		   arglist [14];
    Dimension	   width;
    Dimension      height;
    float	   xscale;
    float     	   yscale;
    Cardinal	   count;
    XtTranslations trans;
    Window	   window;
    Pixmap	   icon_pixmap;
    Pixmap 	   question;
    Pixmap 	   menu16;
    Pixmap	   popup;
    Widget	   form,
                   control,
                   bottom;
    Widget	   coord;
    Widget	   dummy;
    XrmDatabase    our_db;
    XrmDatabase    real_db;
    static String  dump_toggles [ ] = {"XWD", "PostScript"};
    static String  error_buttons [ ]   = {"okay"};
    static String  output_buttons [ ]  = {"dismiss", "save"};
    static String  proceed_buttons [ ] = {"okay", "cancel"};
    static String  qsave_buttons [ ]   = {"yes", "no", "cancel"};


    /* Parse the initial command line options */

    if (ParseCppOptions (&argc, argv)) {
       fputs ("Oops", stderr);
       exit (1);
    }

    add_all_definitions ( );

    /* Create the top level widgets. */

    toplevel = XtAppInitialize (&app_context, "Velvet", options,
		XtNumber (options), &argc, argv, defaults, NULL, 0);


    /* Determine if the 2D or 3D defaults should be used and load them. */

    width = 0;
    dummy = XtVaCreateWidget ("dummy", commandWidgetClass, toplevel, NULL);
    XtVaGetValues (dummy, "shadowWidth", &width, NULL);
    XtDestroyWidget (dummy);

    our_db = XrmGetStringDatabase (width != 0 ? defaults_3d : defaults_2d);

    real_db = XtDatabase (XtDisplay (toplevel));
    XrmCombineDatabase (our_db, &real_db, False);


    /* Create the primary widgets. */

    icon_pixmap = XCreateBitmapFromData (XtDisplay (toplevel),
                      RootWindowOfScreen (XtScreen (toplevel)),
                      FElt_bits, FElt_width, FElt_height);

    XtSetArg (arglist [0], XtNiconPixmap, icon_pixmap);
    XtSetValues (toplevel, arglist, 1);

    form     = XtCreateManagedWidget
		("form", formWidgetClass, toplevel, NULL, 0);

    control  = XtCreateManagedWidget
		("control", formWidgetClass, form, NULL, 0);

    viewport = XtCreateManagedWidget
		("viewport", viewportWidgetClass, form, NULL, 0);

    drawing  = XtCreateManagedWidget
		("drawing", drawingWidgetClass, viewport, NULL, 0);

    coord    = XtCreateManagedWidget
		("coord", labelWidgetClass, form, NULL, 0);

    bottom   = XtCreateManagedWidget
		("bottom", formWidgetClass, form, NULL, 0);

    statusline = XtCreateManagedWidget
		("status", labelWidgetClass, bottom, NULL, 0);

    count = 0;
    XtSetArg (arglist [count], XtNeditType, XawtextEdit); count ++;

    entry    = XtCreateManagedWidget
		("entry", asciiTextWidgetClass, bottom, arglist, count);


    /* Focus all keyboard input to the text entry widget. */

    XtSetKeyboardFocus (form, entry);


    /* Create the bitmaps. */

    window = RootWindowOfScreen (XtScreen (toplevel));
    checkmark = XCreateBitmapFromData (XtDisplay(toplevel), window,
				        check_bits, check_width, check_height);

    question = XCreateBitmapFromData (XtDisplay (toplevel), window,
			        question_bits, question_width, question_height);

    menu16 = XCreateBitmapFromData (XtDisplay (toplevel), window,
				    menu16_bits, menu16_width, menu16_height);

    popup  = XCreateBitmapFromData (XtDisplay (toplevel), window,
				    popup_bits, popup_width, popup_height);


    /* Create the control panel menus and buttons. */

    for (i = 0; i < XtNumber (panel); i ++) {
        count = 0;
	if (panel [i].klass == &menuButtonWidgetClass) {
	    XtSetArg (arglist [0], XtNleftBitmap, menu16); count++;
        }
	else if (strcmp(panel [i].name,"info") == 0) {
	    XtSetArg (arglist [0], XtNleftBitmap, question); count++;
        }
	else if (strcmp(panel [i].name,"materials_d") == 0) {
	    XtSetArg (arglist [0], XtNleftBitmap, popup); count++;
        }
	else if (strcmp(panel [i].name,"forces_d") == 0) {
	    XtSetArg (arglist [0], XtNleftBitmap, popup); count++;
        }
	else if (strcmp(panel [i].name,"loads_d") == 0) {
	    XtSetArg (arglist [0], XtNleftBitmap, popup); count++;
        }
	else if (strcmp(panel [i].name,"constraints_d") == 0) {
	    XtSetArg (arglist [0], XtNleftBitmap, popup); count++;
        }
	else if (panel [i].klass != &commandWidgetClass) {
	    XtSetArg (arglist [0], XtNlabel, ""); count++;
        }

	panel [i].button = XtCreateManagedWidget (panel [i].name,
                                 *panel [i].klass,control,arglist, count);

	if (panel [i].klass == &menuButtonWidgetClass) {
	    sprintf (string, "%sMenu", panel [i].name);
	    panel [i].menu = XtCreatePopupShell
			     (string, simpleMenuWidgetClass, control, NULL, 0);

	    for (j = 0; j < panel [i].numentries; j ++) {
		panel [i].menuentry [j].widget = XtCreateManagedWidget
		 (panel [i].menuentry [j].name, smeBSBObjectClass,
		  panel [i].menu, NULL, 0);

		XtAddCallback (panel [i].menuentry [j].widget, XtNcallback,
		 PanelCallback, &panel [i].menuentry [j].id);
	    }

	} else if (panel [i].klass == &commandWidgetClass)
	    XtAddCallback (panel [i].button, XtNcallback, PanelCallback,
	     &panel [i].menuentry [0].id);
    }

    quitbutton = XtCreateManagedWidget ("quit", commandWidgetClass,
                      control, NULL, 0);
    XtAddCallback (quitbutton, XtNcallback, QuitEditCB, NULL);

    abortbutton = XtCreateManagedWidget ("abort", commandWidgetClass,
                      control, NULL, 0);
    XtAddCallback (abortbutton, XtNcallback, AbortEditCB, NULL);
 

    /* Create the canvas configuration so we can do the args */

    canvas_d     = CanvasDialogCreate (toplevel, drawing, 
                                       "canvasDialog", "Canvas");

    GetArgs ( );

    /* Create the dialogs which we cannot simply create at first pop-up. */

    info_dialog    = CreateDialog (toplevel, "infod", Okay);

    filed          = FileDialogCreate (toplevel, "fileDialog", NULL); 
    dumpd	   = FileDialogCreate (toplevel, "dumpDialog", dump_toggles);

    error_dialog   = OutputDialogCreate (toplevel, "errorDialog",
                          error_buttons, XtNumber (error_buttons));
    proceed_dialog = OutputDialogCreate (toplevel, "proceedDialog",
                          proceed_buttons, XtNumber (proceed_buttons));
    qsave_dialog   = OutputDialogCreate (toplevel, "qsaveDialog",
                          qsave_buttons, XtNumber (qsave_buttons));
    output_dialog  = OutputDialogCreate (toplevel, "outputDialog",
                          output_buttons, XtNumber (output_buttons));

    XtSetArg (arglist [0], XtNicon, icon_pixmap);
    XtSetValues (info_dialog -> dialogwidget, arglist, 1);

    element_l = ElementListCreate (toplevel, "elementList", "Element Type");

    solution_d   = SolutionDialogCreate (toplevel, "solutionDialog",
		   "Output solution control");

    wireframe_d  = WireframeDialogCreate (toplevel, "wireframeDialog",
                   "Wireframe Controls");

    contour_d    = ContourDialogCreate (toplevel, "contourDialog",
                   "Contour Controls");

    element_d	 = ElementDialogCreate (toplevel, "elementDialog",
		   "Elements", ElementDialogChanged, (XtPointer) NULL);

    node_d	 = NodeDialogCreate (toplevel, "nodeDialog",
		   "Nodes", NodeDialogChanged, (XtPointer) element_d);

    material_d = MaterialDialogCreate (toplevel, "materialDialog",
                                       "Materials", MaterialDialogChanged, 
                                       (XtPointer) element_d);

    force_d = ForceDialogCreate (toplevel, "forceDialog", "Forces", 
                                 ForceDialogChanged, (XtPointer) node_d);

    load_d = LoadDialogCreate (toplevel, "loadDialog", "Loads", 
                               LoadDialogChanged, (XtPointer) element_d);

    constraint_d = ConstraintDialogCreate (toplevel, "constraintDialog",
                                           "Constraints", ConstraintDialogChanged, 
                                           (XtPointer) node_d);

    analysis_d   = AnalysisDialogCreate (toplevel, "analysisDialog",
                                         "Problem and Analysis");

    loadcase_d   = LoadCaseDialogCreate (toplevel, "loadcaseDialog", "Load Cases");

    colors_d = ColorsDialogCreate (toplevel, "colorsDialog", "Colors");

    /* register the action table and set-up the translations */

    XtAppAddActions (app_context, actiontable, XtNumber (actiontable));
    trans = XtParseTranslationTable (default_translations);

    XtOverrideTranslations (entry, trans);


    /* Realize the top level widget */

    XtRealizeWidget (toplevel);

    /* Configure the drawing widget */

    count = 0;
    XtSetArg (arglist [count], XtNheight, &height); count++;
    XtSetArg (arglist [count], XtNwidth, &width); count++;
    XtGetValues (drawing, arglist, count);

    canvas -> xmin         = 0.0;
    canvas -> xmax         = 10.0;
    canvas -> ymin         = 0.0;
    canvas -> ymax         = 10.0;
    canvas -> grid_size    = 1.0;
    canvas -> snap_size    = 0.25;
    canvas -> snap         = False;
    canvas -> grid         = False;

    xscale = (float) width / (canvas -> xmax - canvas -> xmin);
    yscale = (float) height / (canvas -> ymax - canvas -> ymin);
    canvas -> scale = xscale;

    count = 0;
    XtSetArg (arglist [count], XtNxScale, Float2Arg (xscale)); count++;
    XtSetArg (arglist [count], XtNyScale, Float2Arg (yscale)); count++;
    XtSetArg (arglist [count], XtNcoordinates, coord); count++;
    XtSetValues (drawing, arglist, count); 

    CanvasDialogSet (canvas_d);

    /* Configure the initial solution parameters */

    solution -> eigen = False;
    solution -> orthonormal = False;
    solution -> transfer = False;
    solution -> felt = True;
    solution -> stress = False;
    solution -> displacement = False;
    solution -> structure = False;
    solution -> plot = False;
    solution -> mode_shapes = False;
    solution -> summary = False;
    solution -> debug = False;
    solution -> matrices = False;
    solution -> details = False;
    solution -> magnify = 100.0;
    solution -> s_component = 1;
    solution -> d_component = 1;
    solution -> zscale = 0.2;
    solution -> title = NULL;
    solution -> xrot = 40.0;
    solution -> yrot = -20.0;
    solution -> zrot = 0.0;
    solution -> hlhsr = False;
    solution -> plot_orig = True;

    SolutionDialogUpdate (solution_d);
    WireframeDialogUpdate (wireframe_d);
    ContourDialogUpdate (contour_d);

    solution -> renumber = True;
    ToggleRenumberStatus ();

    /* check to see if the user started with a filename */

    StartNew ( );
    if (argc > 1) {
       if (access (argv[1], F_OK) == 0) {
          if (access (argv[1], R_OK) != 0)
             error ("File %s is not accessible, check file permissions.",
                    argv[1]);
          else {
	     strcpy (filename, argv [1]);
             status = VelvetReadFeltFile (filename);
          }
       }
       else
	  strcpy (filename, argv [1]);
    }
    else 
       filename [0] = 0;
    
    if (status == 0) {
	UpdateFilenameDisplay ();
	SetNormalMode ( );
	canvas -> node_numbers = !canvas -> node_numbers;
	canvas -> element_numbers = !canvas -> element_numbers;
	ToggleNodeNumberStatus ( );
	ToggleEltNumberStatus ( );
    }

    /* Enter the main event loop */

    changeflag = False;
    XtAppMainLoop (app_context);

    return 0; /* NOTREACHED */
}
