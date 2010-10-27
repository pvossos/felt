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
 * File:	panel.h							*
 *									*
 * Description:	This file contains the definitions related to the 	*
 *		main panel and text commands.				*
 ************************************************************************/

# ifndef _PANEL_H
# define _PANEL_H

    /* Unique ids for each menu item on the control panel */

typedef enum {
    AddNode, DeleteNode, EditNode, MassNode,
    MoveNodeId, RenumberId, RenumberSolveId,
    AddElt, DeleteElt, SetType, Generate, EditElement,
    DefMaterial, OpenMaterial, SaveMaterial, ApplyMaterial,
    DefForce, ApplyForce,
    DefConstraint, ApplyConstraint,
    DefLoad, ApplyLoad,
    ZoomOut, ZoomIn, SaveXWD,
    Solve, ProbAnalysis, Output, LoadCases, AnimateId,
    PlotStress, PlotStructure, PlotDisplacement, Contour, Wireframe,
    SnapOnOff, NodeNumbersOnOff, EltNumbersOnOff,
    GridOnOff, Configure, ColorControl, Recolor,
    LineTool, Circle, Arc, Rectangle, Polygon, DeleteTool, Text, MoveFigure,
    NewId, Open, Save, SaveAs, Restore, Exit, SaveAll,
    Info, Quit, Abort
} PanelId;


typedef struct {
    char	*name;
    WidgetClass *klass;
    Cardinal	 numentries;
    struct {
	char	*name;
	PanelId  id;
	Widget	 widget;
    } menuentry [10];
    Widget 	 button;
    Widget	 menu;
} Panel;


typedef struct {
   char     *name;
   PanelId  id;
} TextCommand;

# endif /* _PANEL_H */
