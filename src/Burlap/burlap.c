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
 * File:	burlap.c						*
 *									*
 * Description:	This file contains the main function definition for	*
 *		burlap - a mathematical notation / language for finite	*
 *		element analysis.					*
 ************************************************************************/

# include <stdio.h>
# include <errno.h>
# include <string.h>
# include <stdlib.h>
# include "felt.h"
# include "debug.h"
# include "lexer.h"
# include "globals.h"
# include "interactive.h"


static const char *usage = "\
usage: burlap [cpp-options] [-f file] [-s file] [-q] [-a] [-n | -i] [file ...]\n\
       -f | -felt file        load FElt file on start-up\n\
       -s | -source file      read commands from file on start-up\n\
       -q | -quiet            start quietly without copyright information\n\
       -i | -interactive      enter interactive mode after processing files\n\
       -n | -no-interactive   do not enter interactive mode if no files given\n\
       -a | -alias            do not define default aliases\n\
";


/************************************************************************
 * Function:	main							*
 *									*
 * Description:	Main function for burlap.				*
 ************************************************************************/

int main (int argc, char **argv)
{
    int   i;
    int   a_flag;
    int   d_flag;
    int   h_flag;
    int   i_flag;
    int   n_flag;
    int   q_flag;
    int   files;
    int   status;
    char *felt_file;
    char *source_file;
    char *arg;


    a_flag = 0;
    d_flag = 0;
    h_flag = 0;
    i_flag = 0;
    n_flag = 0;
    q_flag = 0;
    felt_file = NULL;
    source_file = NULL;


    global_init ( );
    if (init_felt (&argc, argv))
	exit (1);


    for (i = 1; i < argc; i ++)
	if (!strcmp (arg = argv [i], "-f") || !strcmp (arg, "-felt"))
	    felt_file = ++ i != argc ? argv [i] : NULL;
	else if (!strcmp (arg, "-s") || !strcmp (arg, "-source"))
	    source_file = ++ i != argc ? argv [i] : NULL;
	else if (!strcmp (arg, "-d") || !strcmp (arg, "-debug"))
	    d_flag = 1;
	else if (!strcmp (arg, "-i") || !strcmp (arg, "-interactive"))
	    i_flag = 1;
	else if (!strcmp (arg, "-n") || !strcmp (arg, "-no-interactive"))
	    n_flag = 1;
	else if (!strcmp (arg, "-q") || !strcmp (arg, "-quiet"))
	    q_flag = 1;
	else if (!strcmp (arg, "-h") || !strcmp (arg, "-help"))
	    h_flag = 1;
	else if (!strcmp (arg, "-a") || !strcmp (arg, "-alias"))
	    a_flag = 1;
	else
	    break;

    if (h_flag || (n_flag && i_flag)) {
	printf ("%s", usage);
	exit (!h_flag);
    }


    if (felt_file && read_felt (felt_file)) {
	perror (felt_file);
	exit (errno);
    }


    debug = d_flag;
    files = i < argc;
    init_interactive (argv [0], source_file, q_flag, a_flag);

    while (i < argc) {
	if ((status = burlap_yyinclude (argv [i])) == -1) {
	    perror (argv [i]);
	    exit (errno);
	}

	if (status)
	    exit (1);

	i ++;
    }

    if ((!files && !n_flag) || i_flag) {
	if (burlap_yyinclude (NULL))
	    exit (1);
	printf ("\n");
    }

    return 0;
}
