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
 * File:	Solution.h						*
 *									*
 * Description:	This file contains the function and type declarations	*
 *		for the solution dialog box.				*
 ************************************************************************/

# ifndef _Solution_h
# define _Solution_h

typedef struct solution_dialog *SolutionDialog;

typedef struct solution {
   String	title;
   Boolean	eigen;
   Boolean	orthonormal;
   Boolean	transfer;
   Boolean	felt;
   Boolean	plot;
   Boolean	renumber;
   Boolean	debug;
   Boolean	matrices;
   Boolean 	details;
   Boolean	summary;
   Boolean	stress;
   Boolean	displacement;
   Boolean	structure;
   Boolean	mode_shapes;
   Boolean	s_equalize;
   Boolean	d_equalize;
   Boolean      s_plot_elt;
   Boolean      d_plot_elt;
   char		contours;
   Boolean	hlhsr;
   Boolean	plot_orig;
   unsigned	s_component;
   unsigned	d_component;
   float	xrot;
   float	yrot;
   float	zrot;
   float	zscale;
   float	magnify;
} *Solution;

extern Solution	solution;


SolutionDialog SolutionDialogCreate (
# if NeedFunctionPrototypes
    Widget		/* parent */,
    String		/* name   */,
    String		/* title  */
# endif
);

void SolutionDialogPopup (
# if NeedFunctionPrototypes
    SolutionDialog		/* solution_dialog */
# endif
);

void SolutionDialogUpdate (
# if NeedFunctionPrototypes
    SolutionDialog		/* solution_dialog */
# endif
);

# endif /* _Solution_h */
