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
# include "fe.h"
# include "mesh.h"

typedef struct {
    char     *filename;			/* file name			*/
    char     *material;			/* material name		*/
    char     *constraint;		/* constraint name		*/
    Line     *lines;			/* array of lines		*/
    Grid     *grids;			/* array of grids		*/
    Grid     *quadgrids;		/* array of quadrilateral grids */
    Grid     *brickgrids;		/* array of brick grids         */
    TriMesh  *trimeshes;		/* array of trianglular meshes	*/
    unsigned  start_node;		/* starting node number		*/
    unsigned  start_element;		/* starting element number	*/
    unsigned  num_lines;		/* number of line generators	*/
    unsigned  num_grids;		/* number of grid generators	*/
    unsigned  num_quadgrids;		/* number of qgrid generators   */
    unsigned  num_brickgrids;		/* number of brick generators   */
    unsigned  num_trimeshes;		/* number of tri-meshes		*/
    unsigned  line;			/* current line number		*/
    unsigned  num_errors;		/* number of errors		*/
} Generator;

extern Generator generator;

extern void	  init_lexer	      PROTO ((FILE *));
extern int	  yyparse	      PROTO ((void));
extern int	  CorduroyCppOptions  PROTO ((int *, char **));
extern int	  ReadCorduroyFile    PROTO ((char *));
extern int	  WriteCorduroyFile   PROTO ((char *));
extern Definition defnlookup	      PROTO ((char *));

# endif /* _GENERATOR_H */
