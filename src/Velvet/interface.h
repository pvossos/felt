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

/*************************************************************************
*
* File:		interface.h
*
* Description:	contains information to describe the gui for velvet
*		(menu members, buttons, forms, etc.)
*
* Note:		This isn't exactly the traditional way to organize
*		all this stuff, but it keeps velvet.c from getting
*		way out of hand.  This file should ONLY be included
*		by velvet.c (essentially, it's a part of that file, not
*		a traditional header file).
*
**************************************************************************/

# ifndef _INTERFACE_H
# define _INTERFACE_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

/* The control panel */

Panel panel [ ] = {
    {"file",	    &menuButtonWidgetClass, 9,
        {{"new", NewId}, {"open", Open}, {"save", Save}, {"save_as", SaveAs},
        {"restore", Restore}, {"save_all", SaveAll},
	{"openDB", OpenMaterial}, {"updateDB", SaveMaterial},
	{"exit", Exit}}},

    {"problem",	    &menuButtonWidgetClass, 5,
        {{"solve", Solve}, {"animate", AnimateId}, 
        {"output", Output}, {"prob_analysis", ProbAnalysis},
        {"load_cases", LoadCases}}}, 

    {"postprocess", &menuButtonWidgetClass, 5,
       {{"plot_stress", PlotStress}, {"plot_displ", PlotDisplacement},
        {"plot_struct", PlotStructure}, {"contour", Contour},
        {"wireframe", Wireframe}}},

    {"canvas",	    &menuButtonWidgetClass, 9,
	{{"configure", Configure}, 
        {"colors", ColorControl}, {"zoomOut", ZoomOut}, 
        {"zoomIn", ZoomIn}, {"save_xwd", SaveXWD},
        {"node_numbering", NodeNumbersOnOff},
	{"elt_numbering", EltNumbersOnOff},
	{"snap", SnapOnOff}, {"grid", GridOnOff}}},

    {"tools",	    &menuButtonWidgetClass, 8,
	{{"line", LineTool}, {"circle", Circle}, {"arc", Arc},
	{"rectangle", Rectangle}, {"polygon",Polygon}, 
        {"text",Text}, {"move", MoveFigure}, {"delete",DeleteTool}}},

    {"apply", 	    &menuButtonWidgetClass, 4,
        {{"forces", ApplyForce}, {"materials", ApplyMaterial},
        {"loads", ApplyLoad}, {"constraints", ApplyConstraint}}},

    {"nodes",	    &menuButtonWidgetClass, 7,
	{{"add", AddNode}, {"delete", DeleteNode},
         {"edit", EditNode}, {"move",MoveNodeId}, {"mass", MassNode},
         {"renumber", RenumberId}, {"renumber_solve", RenumberSolveId}}},

    {"elements",    &menuButtonWidgetClass, 5,
	{{"add", AddElt}, {"delete", DeleteElt}, {"edit",EditElement},
        {"set_type", SetType}, {"generate", Generate}}},

    {"materials_d",   &commandWidgetClass, 0, {{"", DefMaterial}}},

    {"constraints_d", &commandWidgetClass, 0, {{"", DefConstraint}}}, 

    {"forces_d",    &commandWidgetClass, 0, {{"", DefForce}}},

    {"loads_d",	    &commandWidgetClass, 0, {{"", DefLoad}}},

    {"info",	    &commandWidgetClass, 0, {{"", Info}}},

    {"spacer",	    &labelWidgetClass,   0},

};

/* 
 * The format of these are simple, the command as it would be typed
 * and a PanelId for the appropriate action.
 */

TextCommand commands [ ] = {
   {"add node", AddNode},
   {"delete node", DeleteNode},
   {"edit node", EditNode},
   {"move node", MoveNodeId},
   {"nodal mass", MassNode}, {"lumped mass", MassNode},
   {"renumber nodes", RenumberId},
   {"toggle renumber", RenumberSolveId}, 
   {"toggle node renumbering", RenumberSolveId},

   {"add element", AddElt},
   {"delete element", DeleteElt},
   {"set element", SetType}, {"set element type", SetType}, {"set type", SetType},
   {"generate", Generate},
   {"edit element", EditElement},

   {"edit material", DefMaterial},
   {"apply material", ApplyMaterial},
   {"open database", OpenMaterial},
   {"save database", SaveMaterial}, {"update database", SaveMaterial},

   {"edit force", DefForce},
   {"apply force", ApplyForce},

   {"edit load", DefLoad},
   {"apply load", ApplyLoad},

   {"edit constraint",DefConstraint},
   {"apply constraint", ApplyConstraint},

   {"solve", Solve},
   {"animate", AnimateId},
   {"define problem", ProbAnalysis},
   {"define analysis", ProbAnalysis},
   {"define output", Output},
   {"define load cases", LoadCases},

   {"plot stresses", PlotStress},
   {"plot displacements", PlotDisplacement},
   {"plot structure", PlotStructure},
   {"wireframe", Wireframe}, {"define wireframes", Wireframe},
   {"contour", Contour}, {"define contours", Contour},

   {"new", NewId},
   {"open", Open}, {"load", Open},
   {"save", Save},
   {"save as", SaveAs},
   {"restore", Restore},
   {"exit", Exit}, {"quit", Exit},

   {"zoom all", ZoomOut}, {"zoom out", ZoomOut},
   {"zoom", ZoomIn}, {"zoom window", ZoomIn},
   {"configure", Configure},
   {"colors", ColorControl},
   {"recolor canvas", Recolor}, {"recolor", Recolor},
   {"dump", SaveXWD},
   {"node numbers", NodeNumbersOnOff},
   {"element numbers", EltNumbersOnOff},
   {"snap", SnapOnOff},
   {"grid", GridOnOff},

   {"draw line" ,LineTool},
   {"draw rectangle", Rectangle},
   {"draw circle", Circle},
   {"draw arc", Arc},
   {"draw polygon", Polygon},
   {"draw text", Text},
   {"move figure", MoveFigure}, {"move tool", MoveFigure},
   {"delete figure", DeleteTool}, {"delete tool", DeleteTool},

   {"dummy", InvalidId},
};


/* 
 * The default translations.  For the keyboard shortcuts to menu entries
 * I call ActionToPanelConverter with the menuentry name (not the PanelId)
 * of the appropriate action, because I have to pass it as a string.
 */

char default_translations [ ] = 
   "Ctrl<Key>c:     AbortEdit()\n\
    <Key>Escape:    QuitEdit()\n\
    <Key>Return:    ParseEntryLine()"; 

/* and some actions to go along with them */

XtActionsRec actiontable [ ] = {
   {"ParseEntryLine", ParseEntryLine},
   {"QuitEdit", QuitEdit},
   {"AbortEdit", AbortEdit},
   {"AbortAddElement", AbortAddElement},
   {"QuitMoveNode", QuitMoveNode},
   {"QuitMoveTool", QuitMoveTool},
   {"AbortPolygon", AbortPolygon},
   {"QuitPolygon", QuitPolygon},
   {"AbortMoveTool", AbortMoveTool},
   {"SetMassAP", SetMassAP},
   {"AssignMassAP", AssignMassAP},
   {"AddNodeAP", AddNodeAP},
   {"EditNodeAP", EditNodeAP},
   {"MoveNodeAP", MoveNodeAP},
   {"WalkNodeAP", WalkNodeAP},
   {"DeleteNodeAP", DeleteNodeAP},
   {"AddElementAP", AddElementAP},
   {"DeleteEltAP", DeleteEltAP},
   {"EditElementAP", EditElementAP},
   {"ApplyForceAP", ApplyForceAP},
   {"ApplyLoadAP", ApplyLoadAP},
   {"ApplyMaterialAP", ApplyMaterialAP},
   {"ApplyConstraintAP", ApplyConstraintAP},
   {"DoRectangleAP", DoRectangleAP},
   {"DoCircleAP", DoCircleAP},
   {"DoPolygonAP", DoPolygonAP},
   {"DoLineAP", DoLineAP},
   {"DoTextAP", DoTextAP},
   {"ZoomAP", ZoomAP},
   {"FinishCurve", FinishCurve},
   {"AbortTriMesh", AbortTriMesh},
   {"AddCurvePointAP", AddCurvePointAP},
   {"BackupOnePoint", BackupOnePoint},
   {"MenuAction", MenuAction},
   {"SelectGroupAP", SelectGroupAP},
};

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _INTERFACE_H */
