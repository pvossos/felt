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
 * File:	Analysis.h						*
 *									*
 * Description:	This file contains the function and type declarations	*
 *		for the problem / analysis dialog box.			*
 ************************************************************************/

# ifndef _Analysis_h
# define _Analysis_h

typedef struct analysis_dialog *AnalysisDialog;

AnalysisDialog AnalysisDialogCreate (
# if NeedFunctionPrototypes
    Widget		/* parent */,
    String		/* name   */,
    String		/* title  */
# endif
);

void AnalysisDialogPopup (
# if NeedFunctionPrototypes
    AnalysisDialog		/* analysis_dialog */
# endif
);

void AnalysisDialogUpdate (
# if NeedFunctionPrototypes
    AnalysisDialog		/* analysis_dialog */,
# if NeedWidePrototypes
    int				/* clear node flag */
# else
    Boolean			/* clear node flag */
# endif
# endif
);

# endif /* _Analysis_h */
