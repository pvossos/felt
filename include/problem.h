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
 * File:	problem.h						*
 *									*
 * Description:	This file contains the public type and function		*
 *		declarations for the problem instance.			*
 ************************************************************************/

# ifndef _PROBLEM_H
# define _PROBLEM_H
# include "matrix.h"
# include "fe.h"
# include "Tree.h"
# include "appearance.h"

# define MaxNodesPerElement 32

typedef struct {
    AnalysisType mode;			/* analysis mode	   */
    char	*input;			/* possible lexer input	   */
    char	*title;			/* problem title	   */
    char	*filename;		/* file name		   */
    Node	*nodes;			/* array of nodes	   */
    Element	*elements;		/* array of elements	   */
    LoadCase	*loadcases;
    Tree	 definition_tree;	/* element defn tree	   */
    Tree	 node_tree;		/* node tree		   */
    Tree	 element_tree;		/* element tree		   */
    Tree	 material_tree;		/* material tree	   */
    Tree	 distributed_tree;	/* distributed load tree   */
    Tree	 force_tree;		/* force tree		   */
    Tree	 constraint_tree;	/* constraint tree	   */
    Tree	 loadcase_tree;		/* load case tree	   */
    unsigned	 dofs_pos [7];		/* global DOF position map */
    unsigned	 dofs_num [7];		/* global DOF number map   */
    unsigned	 num_dofs;		/* number of global DOF    */
    unsigned	 num_nodes;		/* number of nodes	   */
    unsigned	 num_elements;		/* number of elements	   */
    unsigned	 num_loadcases;		/* number of load cases	   */
    unsigned	 num_errors;		/* number of errors	   */
    unsigned	 line;			/* current line number	   */

    /* The following used to be in the felt main() function        */
    Matrix	 M, K, C;		/* global matrices	   */
    Matrix	 Mcond, Ccond, Kcond;	/* condensed matrices	   */
    Matrix	 Mm, Km, Cm;		/* modal matrices	   */
    unsigned	 *old_numbers;		/* original node numbering */
    Matrix	 *H;			/* transfer     matrices   */
    Matrix	  S;			/* output spectra	   */
    Vector	  F,			/* force vector		   */
		  Fcond;		/* condensed force vector  */
    Vector	  d;			/* displacement vectors	   */
    Matrix	  x;			/* eigenvectors		   */
    Vector	  lambda;		/* eigenvalues		   */
    NodeDOF	 *forced;		/* forced nodal DOF        */
    unsigned	  numforced;		/* number of forced DOF	   */
    Reaction	 *R;			/* reaction force vector   */
    unsigned	  numreactions;		/* the number of reactions */
    Matrix	  dtable;		/* time-displacement table */
    Matrix	  ttable;		/* time step table	   */
} Problem;

extern Problem  problem;
extern Analysis analysis;

extern char        *copy_input	    PROTO ((int));
extern void	    init_lexer	    PROTO ((FILE *));
extern int	    yyparse	    PROTO ((void));
extern Definition   defnlookup	    PROTO ((char *));
extern int	    ParseCppOptions PROTO ((int *, char **));
extern AnalysisType SetAnalysisMode PROTO ((void));
extern int	    ReadFeltFile    PROTO ((const char *));
extern int	    WriteFeltFile   PROTO ((char *));
extern int	    DumpFeltFile    PROTO ((char *));
extern int	    fWriteFeltFile  PROTO ((FILE *));
extern int	    fDumpFeltFile   PROTO ((FILE *));
extern void 	    detail PROTO ((char *, ...));
extern void         SetDetailStream PROTO ((FILE *));
extern FILE        *GetDetailStream PROTO (( ));

# endif /* _PROBLEM_H */
