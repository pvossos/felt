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
 * File:	generate.c
 *
 ***************************************************************************/

# include <X11/Intrinsic.h>
# include <stdlib.h>
# include "allocate.h"
# include "fe.h"
# include "ElementList.h"
# include "Constraint.h"
# include "Material.h"
# include "Trimesh.h"
# include "Grid.h"
# include "Canvas.h"
# include "OutputDialog.h"
# include "Drawing.h"
# include "vfe.h"
# include "procedures.h"
# include "problem.h"
# include "globals.h"
# include "text_entry.h"
# include "error.h"
# include "util.h"
# include "meshgen.hpp"

extern ConstraintDialog constraint_d;
extern MaterialDialog	material_d;
extern ElementList	element_l;

static unsigned		op_count;
static TriMesh          trimesh;
static unsigned		maxvc;
static unsigned		curr_vc;
static unsigned		curr_curve;
static Figure		marker [100];
static unsigned		mk_count;

typedef double	dbl_pair [2];

# define LINES	1
# define QUADS	2

static int	grid_type;

static void VelvetCoalesceNodes (void)
{
   unsigned		i;
   unsigned		nn, ne;
   Node			*new_nodes;
   FigureAttributes	attr;
   unsigned		num_figures;
   Figure		*figure;

   nn = CompactNodeNumbers ( );
   ne = CompactElementNumbers ( );
   if (nn == 0 || ne == 0)
      return;

   for (i = 1 ; i <= nn ; i++)
       problem.node_set.erase(problem.nodes[i]);

   bool samep = CoalesceProblemNodes();
   if (samep) {
      for (i = 1 ; i <= nn ; i++) 
          problem.node_set.insert(problem.nodes[i]);
      return;
   }
   
   DW_SetAutoRedraw (drawing, False);

   figure = DW_RetrieveAll (drawing, False, &num_figures);
   for (i = 0 ; i < num_figures ; i++) {
      DW_GetAttributes (drawing, figure [i], &attr);
      if (!attr.user_data.empty())
         DW_RemoveFigure (drawing, figure [i]);
   }

   XtFree ((char *) figure); 

   for (i = 1 ; i <= nn ; i++) {
      Deallocate (problem.nodes [i] -> aux);
      problem.nodes [i] -> aux = NULL;

      DrawNode (problem.nodes [i]);
   }

   for (i = 1 ; i <= ne ; i++) {
      problem.element_set.erase(problem.elements[i]);

      Deallocate (problem.elements [i] -> aux);
      problem.elements [i] -> aux = NULL;

      DrawElement (problem.elements [i]);
   }

   DW_SetAutoRedraw (drawing, True);

   return;
}

void GenerateElements (void)
{ 
   Definition	active_definition;


   if (ConstraintDialogActive (constraint_d) == NULL) {
      error ("No active constraint defined.");
      return;
   }
   if (MaterialDialogActive (material_d) == NULL) {
      error ("No active material defined.");
      return;
   }

   active_definition = ElementListDefinition (element_l);
   if (active_definition == NULL) {
      error ("No active element type defined.");
      return;
   } 

   op_count = 0;
   if (active_definition -> numnodes == 2) {
      grid_type = LINES;
      SetupGridGeneration ( );
   }
   else if (active_definition -> numnodes == 3)
      SetupTriangleGeneration ( );
   else if (active_definition -> shapenodes == 4) {
      grid_type = QUADS;
      SetupGridGeneration ( );
   }
   else 
      error ("The current type of element cannot be generated."); 
}

void SetupGridGeneration (void)
{
   static GridDialog	grid_d = NULL;
   unsigned	i;
   Grid	    	grid;

   unsigned mxnode_number
       = !problem.node_set.empty()
       ? (*problem.node_set.rbegin())->number
       : 0;

   unsigned mxelt_number
       = !problem.element_set.empty()
       ? (*problem.element_set.rbegin())->number
       : 0;

   if (grid_d == NULL)
      grid_d = GridDialogCreate (toplevel, "gridDialog", "Generation Parameters");

   grid = GridDialogPopup (grid_d);
   if (grid == NULL)
      return;

   grid -> definition = ElementListDefinition (element_l);

   cvector1<Element> element;
   cvector1<Node> node;
   
   if (grid_type == LINES) {
      if (GenerateGrid (grid,element,node,mxnode_number,mxelt_number))
         return;
   }
   else if (grid_type == QUADS) {
      if (GenerateQuadGrid (grid,element,node,mxnode_number,mxelt_number))
         return;
   }

   for (i = 1 ; i <= element.size(); i++) {
      element [i] -> material = MaterialDialogActive (material_d);

      DrawElement (element [i]);
   }

   for (i = 1 ; i <= node.size(); i++) {
      node[i] -> constraint = ConstraintDialogActive (constraint_d);

      DrawNode (node [i]);
   }

   VelvetCoalesceNodes ( );

   OutputDialogPrintf (error_dialog,"Generated %d nodes and %d elements",node.size(), element.size());
   CenterOnWidget (OutputDialogShell (error_dialog), toplevel, True);
   WarpToCenter (OutputDialogShell (error_dialog));
   OutputDialogSelect (error_dialog, "Generation status", "okay");

   changeflag = True;
}
   

void SetupTriangleGeneration (void)
{
   static unsigned		flag = 1;
   static TrimeshDialog		trimesh_d;
   unsigned			i;
  
   if (flag) {
      trimesh_d = TrimeshDialogCreate(toplevel,"trimeshDialog",
						"Generation Parameters");
      flag = 0;
   }

   trimesh = TrimeshDialogPopup (trimesh_d); 
   if (trimesh == NULL) 
      return;

   trimesh -> curves = Allocate (Curve, trimesh -> numcurves);

   maxvc = 20;

   for (i = 0 ; i < trimesh -> numcurves ; i++) {
      trimesh -> curves [i] = Allocate (struct _curve, 1);
   
      trimesh -> curves [i] -> vcl = Allocate (dbl_pair, maxvc);
      trimesh -> curves [i] -> numvc = 0;
   }

   AssignQuitAbort (FinishCurveCB, "FinishCurve", AbortTriMeshCB, "AbortTriMesh");

   XtOverrideTranslations (entry,
      XtParseTranslationTable ("Shift<Key>BackSpace: BackupOnePoint()"));

   XtOverrideTranslations (entry,
       XtParseTranslationTable ("<Key>Return: AddCurvePointAP()"));

   XtRemoveAllCallbacks (drawing, XtNbuttonCallback);

   XtAddCallback (drawing, XtNbuttonCallback, AddCurvePointCB, NULL);

   if (DW_SetForeground (drawing, canvas -> tool_color.c_str()) == False)
      (void) DW_SetForeground (drawing, "black");

   mk_count = 0;

   op_count = 0;   
   curr_curve = 0;
   curr_vc = 0;
   ChangeStatusLine ("Select first boundary point:", True);
   SetEditMode ();
}

void AddCurvePointAP (Widget w, XEvent *event, String *params, Cardinal *num)
{
   char		*status;
   float	x,y;

   status = GetTextCoordinates (&x, &y, NULL);
   if (status != NULL)
      return;

   DoAddCurvePoint (x,y);
}

void AddCurvePointCB (Widget w, XtPointer clientData, XtPointer callData)
{
   DrawingReport	*report;

   report = (DrawingReport *) callData;
   if (report -> event -> type != ButtonPress)
      return;

   if (report -> event -> xbutton.button == 3) {
        FinishCurveCB (w, clientData, callData);
      return;
   } 
   else if (report -> event -> xbutton.button != 1)
      return;

   DoAddCurvePoint (report -> snapped.x, report -> snapped.y);
}

static char *ordinals [ ] = {"", "first", "second", "third", "fourth", "fifth",
                        "sixth", "seventh", "eighth", "ninth", "tenth",
                        "eleventh", "twelfth", "thirteenth", "fourteenth"};

void DoAddCurvePoint (float x, float y)
{
   static char 		message [80];

   trimesh -> curves [curr_curve] -> vcl [curr_vc][0] = x;
   trimesh -> curves [curr_curve] -> vcl [curr_vc][1] = y;

   marker [mk_count++] =  DW_FillArc (drawing, False, x, y,6.0,6.0,0.0,360.0);

   curr_vc++;
   if (curr_vc >= maxvc) {
      trimesh -> curves [curr_curve] -> vcl = 
         Reallocate (trimesh -> curves [curr_curve] -> vcl, dbl_pair, 20);

      maxvc += 20;
   }

   sprintf (message, "Select %s %s point:", 
            (curr_vc > 13 ? "next" : ordinals [curr_vc + 1]),
            (curr_curve == 0 ? "boundary" : "hole"));

   ChangeStatusLine (message, True);
}

static void
DoTriMeshGeneration(void)
{
    unsigned	i;

  
   unsigned maxnode
       = !problem.node_set.empty()
       ? (*problem.node_set.rbegin())->number
       : 0;

   unsigned maxelt 
       = !problem.element_set.empty()
       ? (*problem.element_set.rbegin())->number
       : 0;
 
   DW_SetAutoRedraw (drawing, False);
    
   for (i = 0 ; i < mk_count ; i++)
      DW_RemoveFigure (drawing, marker [i]);
 
   DW_SetAutoRedraw (drawing, True);

   trimesh -> definition = ElementListDefinition (element_l);

   cvector1<Element> element;
   cvector1<Node> node;
   
   if (GenerateTriMesh (trimesh,element,node,maxnode,maxelt)) {
      for (i = 0 ; i < trimesh -> numcurves ; i++) {
         Deallocate (trimesh -> curves [i] -> vcl);
         Deallocate (trimesh -> curves [i]);
      }
      Deallocate (trimesh -> curves);

      return;
   }

   for (i = 1 ; i <= element.size(); i++) {
      element [i] -> material = MaterialDialogActive (material_d);

      DrawElement (element [i]);
   }     
 
   for (i = 1 ; i <= node.size() ; i++) {
      node[i] -> constraint = ConstraintDialogActive (constraint_d);

      DrawNode (node [i]);
   }
  
   for (i = 0 ; i < trimesh -> numcurves ; i++) {
      Deallocate (trimesh -> curves [i] -> vcl);
      Deallocate (trimesh -> curves [i]);
   }

   Deallocate (trimesh -> curves);

   VelvetCoalesceNodes ( );

   OutputDialogPrintf (error_dialog,"Generated %d nodes and %d elements",node.size(), element.size());
   CenterOnWidget (OutputDialogShell (error_dialog), toplevel, True);
   WarpToCenter (OutputDialogShell (error_dialog));
   OutputDialogSelect (error_dialog, "Generation status", "okay");

   changeflag = True;
}

void FinishCurveCB (Widget w, XtPointer closure, XtPointer data)
{
     FinishCurve(w, NULL, NULL, NULL);
}

void FinishCurve (Widget w, XEvent *event, String *params, Cardinal *num)
{
   trimesh -> curves [curr_curve] -> numvc = curr_vc;
   maxvc = 20;
   curr_vc = 0;
   curr_curve++; 

   if (curr_curve >= trimesh -> numcurves) {
      DoTriMeshGeneration ();
      XtOverrideTranslations (entry,
         XtParseTranslationTable ("Shift<Key>BackSpace: no-op()"));

      SetNormalMode ();
   }
   else 
      ChangeStatusLine ("Select first hole point:", True);
}

void AbortTriMeshCB (Widget w, XtPointer closure, XtPointer data)
{
     AbortTriMesh(w, NULL, NULL, NULL);
}

void AbortTriMesh (Widget w, XEvent *event, String *params, Cardinal *num)
{
   unsigned	i;

   for (i = 0 ; i < trimesh -> numcurves ; i++) {
      Deallocate (trimesh -> curves [i] -> vcl);
      Deallocate (trimesh -> curves [i]);
   }

   Deallocate (trimesh -> curves);  

   DW_SetAutoRedraw (drawing, False);

   for (i = 0 ; i < mk_count ; i++)
      DW_RemoveFigure (drawing, marker [i]);

   DW_SetAutoRedraw (drawing, True);

   XtOverrideTranslations (entry,
      XtParseTranslationTable ("Shift<Key>BackSpace: no-op()"));

   SetNormalMode ();
}

void BackupOnePoint (Widget w, XEvent *event, String *params, Cardinal *num)
{
   char		message [80];

   if (curr_vc == 0)
      return;

   curr_vc--;
   sprintf (message,"Deleted. Select %s %s point:",
      (curr_vc > 13 ? "next" : ordinals [curr_vc + 1]),
      (curr_curve == 0 ? "boundary" : "hole"));

   ChangeStatusLine (message, True);

   mk_count --;
   DW_RemoveFigure (drawing, marker [mk_count]);
}

