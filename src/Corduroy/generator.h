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
 * File:	generator.h						*
 *									*
 * Description:	This file contains the type and function declarations 	*
 *		which relate to the generator instance.			*
 ************************************************************************/

# ifndef _GENERATOR_H
# define _GENERATOR_H

# include <vector>
# include "fe.h"
# include "mesh.h"
# include "geninput.h"

/*----------------------------------------------------------------------*/

typedef struct {
    char     *material;			/* material name		*/
    char     *constraint;		/* constraint name		*/
    std::vector<Line> lines;			/* array of lines		*/
    std::vector<Grid> grids;			/* array of grids		*/
    std::vector<Grid> quadgrids;		/* array of quadrilateral grids */
    std::vector<Grid> brickgrids;		/* array of brick grids         */
    std::vector<TriMesh> trimeshes;		/* array of trianglular meshes	*/
    unsigned  start_node;		/* starting node number		*/
    unsigned  start_element;		/* starting element number	*/
} Generator;

extern Generator generator;

extern "C" void init_cord_lexer (FILE *fp);

/*!
  Parses and removes the preprocesor options from the command line
  arguments.
*/
int CorduroyCppOptions (int *argc, char **argv);

/*!
  Reads a corduroy file using the preprocessor if desired.  A filename
  of "-" indicates standard input (can only be used initially) and a
  NULL filename indicates no file (an empty generation instance is
  created).
*/
int ReadCorduroyFile (const char *input_name);

int WriteCorduroyFile (const char *filename);

/*----------------------------------------------------------------------*/

# endif /* _GENERATOR_H */
