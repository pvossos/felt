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

/****************************************************************************
 *
 * File:         patchwork.c
 *
 * Description:  Contains code for the driver application for the format
 *               conversion package for FElt
 *
 *****************************************************************************/

# include <stdio.h>
# include <string.h>
# include "problem.h"
# include "error.h"
# include "allocate.h"
# include "patchwork.h"
# include "definition.h"

# define streq(a,b)	!strcmp(a,b)

# define FELT  1
# define DXF   2
# define LOGAN 3
# define GRAPH 4
# define OOGL  5

# ifndef DOS
static char *usage = "\
usage: patchwork -[in format] [filename] -[out format] [filename] [options]\n\
\n\
       Available input formats:\n\
       dxf           AutoCAD style DXF files\n\
       felt          standard FElt files\n\
       graph         standard ASCII graph data (gnuplot)\n\
\n\
       Available output formats:\n\
       dxf           AutoCAD style DXF files\n\
       felt          standard FElt files\n\
       graph         standard ASCII graph data (gnuplot)\n\
       logan         files formatted for the software from Logan's book\n\
\n\
       Available pre-processor options for FElt input files:\n\
       -nocpp              do not use a preprocessor\n\
       -cpp filename       preprocessor to use\n\
       -Dname[=value]      define a macro\n\
       -Uname              undefine a macro\n\
       -Idirectory         specify include directory\n\
";
# else
static char *usage = "\
usage: patchwork -[in format] [filename] -[out format] [filename] [options]\n\
\n\
       Available input formats:\n\
       dxf           AutoCAD style DXF files\n\
       felt          standard FElt files\n\
       graph         standard ASCII graph data (gnuplot)\n\
\n\
       Available output formats:\n\
       dxf           AutoCAD style DXF files\n\
       felt          standard FElt files\n\
       graph         standard ASCII graph data (gnuplot)\n\
       logan         files formatted for the software from Logan's book\n\
       oogl          the OOGL Geom File Format to view with Geomview\n\
";
# endif

static char *input_name = NULL;
static char *output_name = NULL;

static int input_format = 0;
static int output_format = 0;

/************************************************************************
 * Function:	ParseOptions						*
 *									*
 * Description:	Parses the command line options.			*
 ************************************************************************/

static int ParseOptions (int *argc, char **argv)
{
    int   	i;
    int   	j;
    char 	*arg;
    int	  	format;

    j = 1;
    for (i = 1; i < *argc; i ++) {
        format = 0;
	if (streq ((arg = argv [i]), "-help")) {
	    fputs (usage, stderr);
	    exit (0);
	} else if (streq (arg, "-felt")) {
	    if (++ i == *argc) 
		return 1;

            format = FELT; 
        } else if (streq (arg, "-dxf")) {
            if (++ i == *argc) 
               return 1;

            format = DXF; 
        } else if (streq (arg, "-logan")) {
            if (++ i == *argc)
               return 1;
            
            format = LOGAN; 
        } else if (streq (arg, "-graph")) {
            if (++ i == *argc)
               return 1;
            
            format = GRAPH; 
        } else if (streq (arg, "-oogl")) {
            if (++ i == *argc)
               return 1;
            
            format = OOGL; 
	} else
	    argv [j ++] = arg;

        if (format) {
           if (input_format) {
              output_format = format;
              output_name = argv [i];
           }
           else {
              input_format = format;
              input_name = argv [i];
           } 
        }
    }

    argv [*argc = j] = NULL;
    return 0;
}


/************************************************************************
 * Function:	 main							*
 *									*
 * Description:	 Main is the driver function for the patchwork 		*
 *		 application.						*
 ************************************************************************/

int main (int argc, char **argv)
{
    int	  status;

	/*
	 * what the heck - just in case the input file is FElt style
	 */

# ifndef DOS
    if (ParseCppOptions (&argc, argv)) {
        fputs (usage, stderr);
        exit (1);
    }
# endif

	/*
	 * get the patchwork specific options
	 */

    if (ParseOptions (&argc, argv)) {
	fputs (usage, stderr);
	exit (1);
    }

    if (!input_format || !output_format || !input_name || !output_name) {
	fputs (usage, stderr);
	exit (1);
    }

    add_all_definitions ( );

	/*
	 * read the input file
	 */

    if (input_format == FELT)
       status = ReadFeltFile (input_name);
    else if (input_format == DXF)
       status = ReadDXFFile (input_name);
    else if (input_format == GRAPH)
       status = ReadGraphFile (input_name);
    else {
       error ("unrecognized input format.");
       status = 1;
    }
 
    if (status)
       exit (1);

	/*
	 * write the ouput file
	 */

    if (output_format == FELT)
       DumpFeltFile (output_name);
    else if (output_format == DXF) 
       WriteDXFFile (output_name);
    else if (output_format == GRAPH) 
       WriteGraphFile (output_name);
    else if (output_format == LOGAN) 
       WriteLoganFile (output_name);
    else if (output_format == OOGL) 
       WriteOoglFile (output_name);
    else {
       error ("unrecognized output format.");
       exit (1);
    }

    exit (0);
}
