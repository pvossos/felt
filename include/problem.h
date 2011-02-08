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

# include <vector>
# include <set>
# include "cvector1.hpp"
# include "fe.h"
# include "Tree.h"
# include "appearance.h"
# include "inptypes.h"

/*----------------------------------------------------------------------*/

# define MaxNodesPerElement 32

typedef struct {
    typedef std::set<Definition, LtDefinition> DefinitionSet;
    typedef std::set<Material, LtMaterial> MaterialSet;
    typedef std::set<Element, LtElement> ElementSet;
     
    AnalysisType mode;			/* analysis mode	   */
    char	*title;			/* problem title	   */
    cvector1<Node> nodes;			/* array of nodes	   */
    cvector1<Element> elements;		/* array of elements	   */
    cvector1<LoadCase> loadcases;
    DefinitionSet definition_set;
    Tree	 node_tree;		/* node tree		   */
    ElementSet element_set;
    MaterialSet material_set;
    Tree	 distributed_tree;	/* distributed load tree   */
    Tree	 force_tree;		/* force tree		   */
    Tree	 constraint_tree;	/* constraint tree	   */
    Tree	 loadcase_tree;		/* load case tree	   */
    unsigned	 dofs_pos [7];		/* global DOF position map */
    unsigned	 dofs_num [7];		/* global DOF number map   */
    unsigned	 num_dofs;		/* number of global DOF    */
    unsigned	 num_errors;		/* number of errors	   */
} Problem;

extern Problem  problem;
extern Analysis analysis;

Definition defnlookup(char *name);

/*!
  Parses and removes the preprocesor options from the command line
  arguments.
*/
int ParseCppOptions(int *argc, char **argv);

/*!
 Returns the current analysis mode for given problem instance.
 Applications cannot simply use problem.mode blindly because we may
 want to modify the mode to reflect factors other than those that the
 user can specify with analysis= (i.e., we want to allow for
 sub-modes).
*/
AnalysisType SetAnalysisMode(void);

/*!
  Reads a felt file using the preprocessor if desired.  A filename of
  "-" indicates standard input (can only be used initially) and a NULL
  filename indicates no file (an empty problem is created).
*/
int ReadFeltFile(const char *filename);

/*!
  Writes a felt file -- only referenced objects will be written.
*/
int WriteFeltFile(const char *filename);

/*!
 Dumps a felt file -- referenced and unreferenced objects will be
 written.
*/
int DumpFeltFile(const char *filename);

/*!
  Writes a felt file -- only referenced objects will be written.
*/
int fWriteFeltFile(FILE *stream);

/*!
  Dumps a felt file -- referenced and unreferenced objects will be
  written.
 */
int fDumpFeltFile(FILE *stream);

/*!
  Checks the state of the detail print flag and if it	
  is on, prints a message to the current detail stream.
*/
void detail(const char *format, ...);

/*!
  Turns on (or off) and sets the stream that describes where detail
  messages should be printed.  To toggle detail messages off, set the
  stream to NULL.
*/
void SetDetailStream(FILE *fp);

FILE* GetDetailStream(void);

/*----------------------------------------------------------------------*/

# endif /* _PROBLEM_H */
