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
 * File:         yardstick.c
 *
 * Description:  Contains code for the driver application for the unit
 *               conversion package for FElt
 *
 *****************************************************************************/

# include <stdio.h>
# include <string.h>
# include "problem.h"
# include "error.h"
# include "allocate.h"
# include "definition.h"
# include "yardstick.h"
# include "units.h"

# define streq(a,b)	!strcmp(a,b)

static char *usage = "\
unit conversion: yardstick -if [force units] -il [length units]\n\
                           -of [force units] -ol [length units]\n\
                           [Cpp options] [filename]\n\
\n\
problem scaling: yardstick -fs [force scale factor] -ls [length scale factor]\n\
                           [Cpp options] [filename]\n\
\n\
       Available force units:    lbs, kips, N, MN, GN\n\
       Available length formats: in, ft, m, mm, cm\n\
\n\
       Cpp options:\n\
       -nocpp              do not use a preprocessor\n\
       -cpp filename       preprocessor to use\n\
       -Dname[=value]      define a macro\n\
       -Uname              undefine a macro\n\
       -Idirectory         specify include directory\n\
";

static char *ilength = NULL;
static char *iforce  = NULL;

static char *olength = NULL;
static char *oforce  = NULL;

static double fscale = UnspecifiedValue;
static double lscale = UnspecifiedValue;

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

    j = 1;
    for (i = 1; i < *argc; i ++) {
	if (streq ((arg = argv [i]), "-help")) {
	    fputs (usage, stderr);
	    exit (0);
	} else if (streq (arg, "-il") || streq (arg, "--input-length")) {
	    if (++ i == *argc) 
		return 1;

           ilength = argv [i]; 
        } else if (streq (arg, "-if") || streq (arg, "--input-force")) {
            if (++ i == *argc) 
               return 1;

           iforce = argv [i];
        } else if (streq (arg, "-of") || streq (arg, "--output-force")) {
            if (++ i == *argc) 
               return 1;

           oforce = argv [i];
	} else if (streq (arg, "-ol") || streq (arg, "--output-length")) {
	    if (++ i == *argc) 
		return 1;

           olength = argv [i]; 
	} else if (streq (arg, "-fs") || streq (arg, "--force-scale")) {
	    if (++ i == *argc) 
		return 1;

           fscale = atof (argv [i]); 
	} else if (streq (arg, "-ls") || streq (arg, "--length-scale")) {
	    if (++ i == *argc) 
		return 1;

           lscale = atof (argv [i]); 
	} else
	    argv [j ++] = arg;
    }

    argv [*argc = j] = NULL;
    return 0;
}


/************************************************************************
 * Function:	 main							*
 *									*
 * Description:	 Main is the driver function for the yardstick 		*
 *		 application.						*
 ************************************************************************/

int main (int argc, char **argv)
{
    int		mode;
    unsigned	i;

	/*
	 * just in case there are cpp directives in the input
	 */

    if (ParseCppOptions (&argc, argv)) {
        fputs (usage, stderr);
        exit (1);
    }

	/*
	 * get the yardstick specific options
	 */

    if (ParseOptions (&argc, argv)) {
	fputs (usage, stderr);
	exit (1);
    }

  	/*
	 * check the validity of all the possible option combinations
 	 * (and if this isn't a silly way to do this ...)
	 */

    mode = 0;
    if (oforce) mode ++;
    if (iforce) mode ++;
    if (olength) mode ++;
    if (ilength) mode ++; 

    if (fscale != UnspecifiedValue) mode += 10;
    if (lscale != UnspecifiedValue) mode += 10;

    if (mode != 4 && mode != 20) {
       if (mode < 4)
          Fatal ("must specify input and output force and length units.");

       if (mode == 10)
          Fatal ("must specify length and force scale factors.");

       Fatal ("scaling and unit conversion are exclusive options.");
    }

	/*
	 * figure out the scale factors based on the given units
	 * if we are doing unit conversion and not just scaling
	 */

    if (mode == 4) {
       fscale = 1.0;
       for (i = 0 ; i < sizeof (force_units) / sizeof (Unit) ; i++) {
          if (streq (force_units [i].name, iforce))
             fscale /= force_units [i].scale;
     
          if (streq (force_units [i].name, oforce))
             fscale *= force_units [i].scale;
       }

       lscale = 1.0;
       for (i = 0 ; i < sizeof (length_units) / sizeof (Unit) ; i++) {
          if (streq (length_units [i].name, ilength))
             lscale /= length_units [i].scale;
     
          if (streq (length_units [i].name, olength))
             lscale *= length_units [i].scale;
       }
    }

    add_all_definitions ( );

    	/*
	 * read the input file	
	 */ 

    if (ReadFeltFile (argc == 2 ? argv [1] : "-"))
       exit (1);
   
	/*
	 * scale all the numerical quantities in the input
	 */

    ScaleFeltFile (lscale, fscale);

	/*
	 * dump it back out to standard output
	 */

    DumpFeltFile ("-");

    exit (0);
}
