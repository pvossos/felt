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
* 
* File:		procedures.h
*
* Description:	Contains function prototypes for the various specific
*		callback and action routines
*
*************************************************************************/

# ifndef _PROCEDURES_H
# define _PROCEDURES_H

	/*
 	 * a bunch of routines that I'm too busy to classify
	 */

void SelectCallback ();
	 
void EditAddNode ();
void SetActiveElementType ();
void GetForceDefinition ();
void GetMaterialDefinition ();
void GetConstraintDefinition ();
void SetActiveForce ();
void SetActiveMaterial ();

void ToggleSnapStatus ();
void ToggleGridStatus ();
void GetCanvasParameters ();
void DumpDrawingArea ();
void SaveWidgetXWD ();
void SaveWidgetPS ();
void GetConfiguration ();

void ZoomAll ();
void ZoomStart ();
void ZoomAP ();
void ZoomSelect ();
void ZoomEnd ();

void OpenFile ();
int WriteVFeltFile ();
int  VelvetReadFeltFile ();
void WriteVelvetFile ();
void ReadVelvetFile ();
String FormVelvetFilename ();
void WriteMaterialFile ();
void OpenMaterialFile ();
void WriteNamedFile ();
void StartNew ();
void RestoreOriginal ();
Boolean QuerySave ();

void QuitVelvet ();

void ParseEntryLine ();
void MenuAction ();
void AssignQuitAbort ();
void QuitEdit ();
void AbortEdit ();
void SetEditMode ();
void SetNormalMode ();
void SetWaitCursor ();
void SetNormalCursor ();
void ChangeStatusLine ();
void UpdateFilenameDisplay ();

	/*
	 * for the construction tools
	 */

void ToolsDeleteFigure ();
void DeleteToolCB ();
void ToolsDrawLine ();
void DoLineCB ();
void DoLineAP ();
void ToolsDrawCircle ();
void DoCircleCB ();
void DoCircleAP ();
void DoPolygonAP ();
void ToolsDrawArc ();
void DoArcCB ();
void ToolsDrawRectangle ();
void DoRectangleCB ();
void DoRectangleAP ();
void ToolsDrawText ();
void DoTextAP ();
void DoTextCB ();
void ToolsDrawPolygon ();
void DoPolygonMotionCB ();
void DoPolygonButtonCB ();
void SelectGroupAP ();
void SelectGroup ();
void QuitMoveTool ();
void AbortMoveTool ();
void QuitPolygon ();
void AbortPolygon ();
void MoveTool ();
int  figure_cmp ();

	/*
	 * for editing the node list
	 */

void AddNodeCB ();
void AddNodeAP ();
void DoAddNode ();
void DeleteNodeCB ();
void DeleteNodeAP ();
void DoAssignMass ();
void AssignMassAP ();
void SetMassAP ();
void AssignMassCB ();
void EditNodalMass ();
void EditNodeNumber ();
void EditNodeAP ();
void EditNodeCB ();
void MoveElement ();
void MoveNodeNumber ();
void MoveNodeAP ();
void MoveNodeCB ();
void WalkNodeAP ();
void WalkNodeCB ();
void QuitMoveNode ();
void EditDeleteNode ();
void GetNodeInformation ();
void DoMoveNode ();
void DoWalkNode ();
void EditNodeInfo ();
void OptimizeNumbering ();
void ToggleRenumberStatus ();
int DrawNode ();

	/*
	 * for editing the element list
	 */

void GetElementInformation ();
void EditElementInfo ();
void AddElementCB ();
void AddElementAP ();
void DoAddElement ();
void EditAddElement ();
void EditDeleteElement ();
void DeleteEltCB ();
void DeleteEltAP ();
void EditElementNumber ();
void EditElementCB ();
void EditElementAP ();
int DrawElement ();
void AbortAddElement ();
void ComputeCenter ();

void ApplyForceCB ();
void ApplyForceAP ();
void EditApplyForce ();

void EditApplyLoad ();
void ApplyLoadCB ();
void ApplyLoadAP ();

void EditApplyMaterial ();
void ApplyMaterialAP ();
void ApplyMaterialCB ();

void EditApplyConstraint ();
void ApplyConstraintCB ();
void ApplyConstraintAP ();

void SetupGridGeneration ();
void SetupTriangleGeneration ();
void GenerateElements ();
void ToggleNodeNumberStatus ();
void ToggleEltNumberStatus ();
void SetNodeNumberFlag();
void SetEltNumberFlag();

void FinishCurve ();
void AbortTriMesh ();
void AddCurvePointAP ();
void AddCurvePointCB ();
void DoAddCurvePoint ();
void BackupOnePoint ();

	/*
	 * defining and solving the problem
	 */

void SetupAndSolve ( );
void SetupAnimate ( );
void AnimateTransient ( );
void AnimateStructure ( );
int  CompactNodeNumbers ( );
int  CompactElementNumbers ( );
int  SolveProblem ( );

	/*
	 * miscellaneous functions
	 */

void OutputButtonActions ();
void OutputWindowPopup ();
void DeleteGroup ();
void PanelCallback ();
void BufferErrors ();
int  DumpWidget ();
int  BivariateInterp ();

void VelvetPlotTD ();
void VelvetPlotSpectra();
void VelvetPlotTransferFunctions();
void VelvetPlotForce();
void VelvetPlotLoadRange();
void DrawModeShapes ();
void DrawModeShapes3D ();
void PlotStressField ();
void PlotDisplacementField ();
void XImageToPPM ();
void XImageToEPS ();
void SetupStresses ();
void SetupDisplacements ();
void SetupStructure ();
void SetupModeShapes ();
void VisualizeStructure ();
void VisualizeStructure3D ();
void AnimateStructure3D ();
void InitializeDrawingShell ();
int  WritePostscriptFigures ();

# endif /* _PROCEDURES_H */
