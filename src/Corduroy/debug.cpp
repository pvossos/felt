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
 * File:	debug.c							*
 *									*
 * Description:	This file contains the code the writing a corduroy	*
 *		file.							*
 ************************************************************************/

# include <stdio.h>
# include "generator.h"

int WriteCorduroyFile (const char *filename)
{
    unsigned	 i;
    unsigned	 j;
    unsigned	 k;
    Line	 line;
    Grid	 grid;
    TriMesh	 mesh;
    Curve	 curve;
    static const char	*rules [ ] = {"linear", "cosinusoidal", "sinusoidal", 
                              "logarithmic", "reverse-logarithmic",
                              "parabolic", "reverse-parabolic"};

    printf ("start-node    = %u\n", generator.start_node);
    printf ("start-element = %u\n", generator.start_element);

    if (generator.constraint)
	printf ("constraint    = %s\n", generator.constraint);

    if (generator.material)
	printf ("material      = %s\n", generator.material);

    printf ("\n");


    for (i = 0; i < generator.num_lines; i ++) {
	line = generator.lines [i];

	printf ("line\n");
        printf ("element-type = %s\n", line -> definition -> name); 
	printf ("start  = ");
	printf ("(%g,%g,%g)\n", line -> xs, line -> ys, line -> zs);
	printf ("end    = ");
	printf ("(%g,%g,%g)\n", line -> xe, line -> ye, line -> ze);
	printf ("number = %u\n", line -> number);
	printf ("rule   = %s\n", rules [line -> rule]);
	printf ("\n");
    }

    for (i = 0; i < generator.num_grids; i ++) {
	grid = generator.grids [i];

	printf ("grid\n");
        printf ("element-type = %s\n", grid -> definition -> name); 
	printf ("start    = ");
	printf ("(%g,%g,%g)\n", grid -> xs, grid -> ys, grid -> zs);
	printf ("end      = ");
	printf ("(%g,%g,%g)\n", grid -> xe, grid -> ye, grid -> ze);
	printf ("x-number = %u\n", grid -> xnumber);
	printf ("y-number = %u\n", grid -> ynumber);
	printf ("z-number = %u\n", grid -> znumber);
	printf ("x-rule   = %s\n", rules [grid -> xrule]);
	printf ("y-rule   = %s\n", rules [grid -> yrule]);
	printf ("z-rule   = %s\n", rules [grid -> zrule]);
	printf ("\n");
    }

    for (i = 0; i < generator.num_quadgrids; i ++) {
	grid = generator.quadgrids [i];

	printf ("quadrilateral grid\n");
        printf ("element-type = %s\n", grid -> definition -> name); 
	printf ("start    = ");
	printf ("(%g,%g)\n", grid -> xs, grid -> ys);
	printf ("end      = ");
	printf ("(%g,%g)\n", grid -> xe, grid -> ye);
	printf ("x-number = %u\n", grid -> xnumber);
	printf ("y-number = %u\n", grid -> ynumber);
	printf ("x-rule   = %s\n", rules [grid -> xrule]);
	printf ("y-rule   = %s\n", rules [grid -> yrule]);
	printf ("\n");
    }

    for (i = 0; i < generator.num_brickgrids; i ++) {
	grid = generator.brickgrids [i];

	printf ("brick grid\n");
        printf ("element-type = %s\n", grid -> definition -> name); 
	printf ("start    = ");
	printf ("(%g,%g,%g)\n", grid -> xs, grid -> ys, grid -> zs);
	printf ("end      = ");
	printf ("(%g,%g,%g)\n", grid -> xe, grid -> ye, grid -> ze);
	printf ("x-number = %u\n", grid -> xnumber);
	printf ("y-number = %u\n", grid -> ynumber);
	printf ("z-number = %u\n", grid -> znumber);
	printf ("x-rule   = %s\n", rules [grid -> xrule]);
	printf ("y-rule   = %s\n", rules [grid -> yrule]);
	printf ("z-rule   = %s\n", rules [grid -> zrule]);
	printf ("\n");
    }

    for (i = 0; i < generator.num_trimeshes; i ++) {
	mesh = generator.trimeshes [i];

	printf ("triangular mesh\n");
        printf ("element-type = %s\n", mesh -> definition -> name); 
	printf ("target = %d\n", mesh -> target);
	printf ("alpha  = %g\n", mesh -> alpha);

	for (j = 0; j < mesh -> numcurves; j ++) {
	    curve = mesh -> curves [j];

	    printf (j ? "\nhole = [\n" : "\nboundary = [\n");

	    for (k = 0; k < curve -> numvc; k ++) {
		printf ("    (%g", curve -> vcl [k] [0]);
		printf (",%g)\n", curve -> vcl [k] [1]);
	    }

	    printf ("]\n");
	}

	printf ("\n");
    }

    printf ("end\n\n");
    return 0;
}
