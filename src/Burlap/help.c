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
 * File:	help.c							*
 *									*
 * Description:	This file contains the public and private function and	*
 *		variable definitions for the help related intrinsic	*
 *		functions.						*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include "help.h"
# include "debug.h"
# include "coerce.h"
# include "execute.h"
# include "helptab.h"
# include "functab.h"
# include "our-stdlib.h"


static char *copyright = "\
\n\
    This file is part of the FElt finite element analysis package.\n\
    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson\n\
\n\
    This program is free software; you can redistribute it and/or modify\n\
    it under the terms of the GNU General Public License as published by\n\
    the Free Software Foundation; either version 2 of the License, or\n\
    (at your option) any later version.\n\
\n\
    This program is distributed in the hope that it will be useful,\n\
    but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
    GNU General Public License for more details.\n\
\n\
    You should have received a copy of the GNU General Public License\n\
    along with this program; if not, write to the Free Software\n\
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n\
\n\
";


/************************************************************************
 * File:	list_topics						*
 *									*
 * Description:	Lists all the valid help topics.  Note that this	*
 *		functions assumes that the help topics are in a		*
 *		stylized format.					*
 ************************************************************************/

static void list_topics (void)
{
    int   i;
    int   j;
    int   width;
    int   across;
    int   last_type;
    char *pager;
    FILE *stream;


    j = 0;
    width = 0;
    across = 0;
    last_type = -1;

# ifdef DOS
    stream = stdout;
# else
    if (!(pager = getenv ("PAGER")))
	pager = "more";

    if (!(stream = popen (pager, "w")))
	stream = stdout;
# endif

    fprintf (stream, "\nHelp is available on the following topics:");

    for (i = 0; i < sizeof (help) / sizeof (*help); i ++) {
	if (help [i].type != last_type) {
	    last_type = help [i].type;
	    across = help_topics [last_type].across;
	    width = help_topics [last_type].width;
	    fprintf (stream, "\n\n%s:\n", help_topics [last_type].title);
	    j = 0;
	}

	if (j ++ % across == 0)
	    fprintf (stream, "\n");

	fprintf (stream, "%*s", width, help [i].key);
    }

    fprintf (stream, "\n");
    fprintf (stream, "\n");

# ifndef DOS
    if (stream != stdout)
	pclose (stream);
# endif
}


/************************************************************************
 * Function:	help_func						*
 *									*
 * Description:	Pops the descriptor on the top of the stack and treats	*
 *		it as a request for help on an operator, function, or	*
 *		other high-level topic.  The following types are legal:	*
 *									*
 *		help (null)	 -> double (success code)		*
 *		help (string)	 -> double (success code)		*
 *		help (intrinsic) -> double (success code)		*
 *									*
 *		An attempt is first made to coerce the argument to a	*
 *		string value.						*
 ************************************************************************/

int help_func (int n)
{
    char       *string;
    descriptor *arg;
    descriptor *result;
    descriptor	temp;
    int		type_error;
    int		status;
    int		i;


    result = top ( );
    temp = *result;
    arg = &temp;


    status = 0;
    type_error = F_False;
    arg = CoerceData (deref (arg), T_String);


    switch (D_Type (arg)) {
    case T_String:
    case T_Intrinsic:
	if (D_Type (arg) == T_Intrinsic)
	    string = functab [D_Intrinsic (arg)].name;
	else
	    string = *D_String (arg);

	if (!strcmp (string, "copyright"))
	    fputs (copyright, stdout);
	else if (!strcmp (string, "help") || !strcmp (string, ""))
	    list_topics ( );
	else {
	    status = 1;
	    for (i = 0; i < sizeof (help) / sizeof (*help); i ++)
		if (!strcmp (string, help [i].key)) {
		    printf ("\n%s\n", help [i].message);
		    status = 0;
		    break;
		}

	    if (status == 1)
		printf ("\nNo help available for '%s'.\n\n", string);
	}

	D_Type	  (result) = T_Double;
	D_Temp	  (result) = F_False;
	D_Trapped (result) = F_False;
	D_Double  (result) = dbllit (status);
	break;


    case T_Null:
	list_topics ( );
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("help", arg, NULL, NULL, F_True);


    RecycleData (arg);
    return type_error == F_True || status != 0;
}
