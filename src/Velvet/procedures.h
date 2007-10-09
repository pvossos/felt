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
* 
* File:		procedures.h
*
* Description:	Contains function prototypes for the various specific
*		callback and action routines
*
*************************************************************************/

# ifndef _PROCEDURES_H
# define _PROCEDURES_H

#include "Drawing.h"
#include "Item.h"
#include "fe.h"

	/*
 	 * a bunch of routines that I'm too busy to classify
	 */

void SelectCallback (Widget w, XtPointer clientData, XtPointer callData);
	 
void EditAddNode (void);
void ToggleSnapStatus (void);
void ToggleGridStatus (void);
void DumpDrawingArea (Widget widget, String title, Boolean allow_ps);
void SaveWidgetXWD (Widget widget, String xwd_file);
void SaveWidgetPS (Widget widget, String ps_file);

void ZoomAll (void);
void ZoomStart (void);
void ZoomAP (void);
void ZoomSelect (Widget w, XtPointer clientData, XtPointer callData);
void ZoomEnd (Figure box);

void OpenFile (void);
int WriteVFeltFile (Boolean dump_all);
int  VelvetReadFeltFile (char *file);
void WriteMaterialFile (void);
void OpenMaterialFile (void);
void WriteNamedFile (Boolean dump_all);
void StartNew (void);
void RestoreOriginal (void);
Boolean QuerySave (void);

void QuitVelvet (void);

void ParseEntryLine (void);
void MenuAction (Widget w, XEvent *event, String *params, Cardinal *num_params);
void AssignQuitAbort (XtCallbackProc quitCB, String quitAP, XtCallbackProc abortCB, String abortAP);
void QuitEdit(Widget, XtPointer, XtPointer);
void AbortEdit(Widget widget, XtPointer closure, XtPointer data);
void SetEditMode (void);
void SetNormalMode (void);
void SetWaitCursor (Widget w);
void SetNormalCursor (Widget w);
void ChangeStatusLine (String new_label, Boolean allow_input);
void UpdateFilenameDisplay (void);

	/*
	 * for the construction tools
	 */

void ToolsDeleteFigure (void);
void DeleteToolCB (Widget w, XtPointer clientData, XtPointer callData);
void ToolsDrawLine (void);
void DoLineCB (Widget w, XtPointer clientData, XtPointer callData);
void DoLineAP (void);
void ToolsDrawCircle (void);
void DoCircleCB (Widget w, XtPointer clientData, XtPointer callData);
void DoCircleAP (void);
void DoPolygonAP (void);
void ToolsDrawArc (void);
void DoArcCB (Widget w, XtPointer clientData, XtPointer callData);
void ToolsDrawRectangle (void);
void DoRectangleCB (Widget w, XtPointer clientData, XtPointer callData);
void DoRectangleAP (void);
void ToolsDrawText (void);
void DoTextAP (void);
void DoTextCB (Widget w, XtPointer clientData, XtPointer callData);
void ToolsDrawPolygon (void);
void DoPolygonMotionCB (Widget w, XtPointer clientData, XtPointer callData);
void DoPolygonButtonCB (Widget w, XtPointer clientData, XtPointer callData);
void SelectGroupAP (void);
void SelectGroup (XtPointer call_data, void (*op) (Figure *, unsigned));
void QuitMoveTool (Widget w, XtPointer closure, XtPointer call_data);
void AbortMoveTool (Widget w, XtPointer closure, XtPointer call_data);
void QuitPolygon (Widget w, XtPointer closure, XtPointer call_data);
void AbortPolygon (Widget w, XtPointer closure, XtPointer call_data);
void MoveTool (void);
int  figure_cmp (Item item1, Item item2);

	/*
	 * for editing the node list
	 */

void AddNodeCB (Widget w, XtPointer client_data, XtPointer call_data);
void AddNodeAP (void);
void DoAddNode (float x, float y, float z);
void DeleteNodeCB (Widget w, XtPointer client_data, XtPointer call_data);
void DeleteNodeAP (void);
void DoAssignMass (Node node);
void AssignMassAP (void);
void SetMassAP (void);
void AssignMassCB (Widget w, XtPointer client_data, XtPointer call_data);
void EditNodalMass (void);
void EditNodeNumber (void);
void EditNodeAP (void);
void EditNodeCB (Widget w, XtPointer client_data, XtPointer call_data);
void MoveElement (Element element, Node *old_nodes);
void MoveNodeNumber (void);
void MoveNodeAP (void);
void MoveNodeCB (Widget w, XtPointer client_data, XtPointer call_data);
void WalkNodeAP (Widget widget, XtPointer closure, XtPointer data);
void WalkNodeCB (Widget w, XtPointer client_data, XtPointer call_data);
void QuitMoveNode (Widget w, XtPointer closure, XtPointer call_data);
void EditDeleteNode (void);
void DoMoveNode (Node node, Boolean motion);
void DoWalkNode (Node node);
void OptimizeNumbering (void);
void ToggleRenumberStatus (void);
int DrawNode (Node node);

	/*
	 * for editing the element list
	 */

void AddElementCB (Widget w, XtPointer client_data, XtPointer call_data);
void AddElementAP (void);
void DoAddElement (Node node);
void EditAddElement (void);
void EditDeleteElement (void);
void DeleteEltCB (Widget w, XtPointer client_data, XtPointer call_data);
void DeleteEltAP (void);
void EditElementNumber (void);
void EditElementCB (Widget w, XtPointer client_data, XtPointer call_data);
void EditElementAP (void);
int DrawElement (Element element);
void AbortAddElement (Widget w, XtPointer closure, XtPointer call_data);
void ComputeCenter (Element element, float *x, float *y);

void ApplyForceCB (Widget w, XtPointer client_data, XtPointer call_data);
void ApplyForceAP (void);
void EditApplyForce (void);

void EditApplyLoad (void);
void ApplyLoadCB (Widget w, XtPointer client_data, XtPointer call_data);
void ApplyLoadAP (void);

void EditApplyMaterial (void);
void ApplyMaterialAP (void);
void ApplyMaterialCB (Widget w, XtPointer client_data, XtPointer call_data);

void EditApplyConstraint (void);
void ApplyConstraintCB (Widget w, XtPointer client_data, XtPointer call_data);
void ApplyConstraintAP (void);

void SetupGridGeneration (void);
void SetupTriangleGeneration (void);
void GenerateElements (void);
void ToggleNodeNumberStatus (void);
void ToggleEltNumberStatus (void);
void SetNodeNumberFlag(void);
void SetEltNumberFlag(void);

void FinishCurve (Widget w, XtPointer closure, XtPointer call_data);
void AbortTriMesh (Widget w, XtPointer closure, XtPointer call_data);
void AddCurvePointAP (void);
void AddCurvePointCB (Widget w, XtPointer clientData, XtPointer callData);
void DoAddCurvePoint (float x, float y);
void BackupOnePoint (void);

	/*
	 * defining and solving the problem
	 */

void SetupAndSolve (void);
void SetupAnimate (void);
void AnimateStructure (Matrix dtable, Node *node, Element *element, unsigned int numnodes, unsigned int numelts);
int  CompactNodeNumbers (void);
int  CompactElementNumbers (void);
int  SolveProblem (void);

	/*
	 * miscellaneous functions
	 */

void OutputButtonActions (Widget w, XtPointer client_data, XtPointer call_data);
void OutputWindowPopup (void);
void PanelCallback (Widget widget, XtPointer clientData, XtPointer callData);
void BufferErrors (Boolean flag);
int  DumpWidget (Widget, FILE *);
int BivariateInterp (int ndp, float *xd, float *yd, float *zd,
                     int nxi, int nyi, float *xi, float *yi, float **zi,
                     unsigned char **mask);

void VelvetPlotTD (Matrix dtable, Matrix ttable, char *xlabel, char *ylabel, char *alt_title, Boolean print_dof_names);
void VelvetPlotSpectra(Matrix P, char *xlabel, char *ylabel, char *alt_title, Boolean print_dof_names);
void VelvetPlotTransferFunctions(Matrix *H, unsigned int *forced, unsigned int numforced, char *xlabel, char *ylabel, char *alt_title);
void VelvetPlotForce(Force force, char *quantity);
void VelvetPlotLoadRange(Matrix dtable);
void DrawModeShapes (Matrix phi, Matrix lambda, Node *node, Element *element, unsigned int numnodes, unsigned int numelts);
void DrawModeShapes3D (Matrix phi, Matrix lambda, Node *node, Element *element, unsigned int numnodes, unsigned int numelts);
void PlotStressField (char *out, Element *element, unsigned numelts, int comp,
                      int equalize, int plot_elt, int width, int height);
void PlotDisplacementField (char *out, Node *node, unsigned numnodes,
                            Element *element, unsigned numelts, int comp, 
                            int equalize, int plot_elt, int width, int height);
void SetupStresses (Boolean build_elt);
void SetupDisplacements (Boolean build_arrays);
void SetupStructure (Boolean build_elt);
void SetupModeShapes (Matrix phi, Matrix lambda);
void VisualizeStructure (Element *element, unsigned int numelts);
void VisualizeStructure3D (Element *element, unsigned int numelts);
void AnimateStructure3D (Matrix dtable, Node *node, Element *element, unsigned int numnodes, unsigned int numelts);

Widget
CreateDrawingShell(String name, String title,
                   XtCallbackProc callback, Widget *dw);

void
InitializeDrawingShell(Widget shell, Widget dw, 
                       float minX, float maxX, float minY, float maxY,
                       float *x_scale, float *y_scale,
                       Dimension *wx, Dimension *hy);

int  WritePostscriptFigures (Figure *f, unsigned int n, Widget widget, double xmin, double xmax, double ymin, double ymax, char *ps_filename);

# endif /* _PROCEDURES_H */
