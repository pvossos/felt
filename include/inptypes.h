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

/*
  Why not include the following in fe.h? Because lexer.l needs them,
  and we want to be able to include c++ code in the rest of fe.h and
  problem.h.
*/

#ifndef INPTYPES_H
#define INPTYPES_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

/* Analysis types */

typedef enum {
    Static = 1,
    Transient = 2,
    Modal = 3,
    StaticThermal = 4,
    TransientThermal = 5,
    Spectral = 6,
    StaticSubstitution = 7,
    StaticIncremental = 8,
    StaticLoadCases = 9,
    StaticLoadRange = 10,
    StaticSubstitutionLoadRange = 11,
    StaticIncrementalLoadRange = 12,
} AnalysisType;


/* Degrees of freedom */

typedef enum {
    Tx = 1, Fx = 1,			/* translation/force along x axis */
    Ty = 2, Fy = 2,			/* translation/force along y axis */
    Tz = 3, Fz = 3,			/* translation/force along z axis */
    Rx = 4, Mx = 4,			/* rotation/moment about x axis   */
    Ry = 5, My = 5,			/* rotation/moment about y axis   */
    Rz = 6, Mz = 6			/* rotation/moment about z axis   */
} DOF;


/* Load directions */

typedef enum {
    LocalX        = 1,
    LocalY        = 2,
    LocalZ        = 3,
    GlobalX	  = 4,
    GlobalY	  = 5,
    GlobalZ 	  = 6,
    Parallel      = 7,
    Perpendicular = 8,
    Radial	  = 9,
    Axial	  = 10
} Direction;


/* A node-magnitude pair */

typedef struct pair {
    unsigned node;			/* node number */
    double   magnitude;			/* magnitude   */
} Pair;


typedef struct casepair {
   unsigned	 noe;		/* node or element number */
   char		*fol;		/* force or load name     */
} CasePair;


/* cut-off from struct problem */

typedef struct 
{
    char	*input;			/* possible lexer input	   */
    char	*filename;		/* file name		   */
    unsigned	 line;			/* current line number	   */
} ProblemSource;

extern ProblemSource psource;

/*!
  Specifies whether yytext should be copied into a local buffer.  The
  text is put into a canonical form in which leading and trailing
  white space is eliminated and all other sequences of white space are
  replaced by a space.
*/
char *copy_input (int flag);

/*!
  Initializes the lexer for a new file.			
*/
void init_felt_lexer (FILE *fp);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
