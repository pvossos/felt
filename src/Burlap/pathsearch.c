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
 * File:	pathsearch.c						*
 *									*
 * Description:	This file contains the function definitions for		*
 *		searching a path for a file.				*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include <unistd.h>
# include <pwd.h>
# include "allocate.h"
# include "our-stdlib.h"
# include "pathsearch.h"


/************************************************************************
 * Function:	pathsearch						*
 *									*
 * Description:	Searches a colon separated path of directories for the	*
 *		specified file, without and then with a specified	*
 *		suffix.  If a readable file is found, then the path is	*
 *		returned.  Otherwise, the file name or NULL is returned	*
 *		as desired.  If the file name starts with a "/" then	*
 *		the path is ignored.  If the file name starts with a	*
 *		"~" then a lookup of the user name is performed.	*
 ************************************************************************/

char *pathsearch (path, file, suffix, def_flag)
    char *path;
    char *file;
    char *suffix;
    int   def_flag;
{
    char	   old;
    char	  *ptr;
    char	  *name;
    char	  *suff;
    char	  *copy;
    int		   length;
    static int	   buf_length;
    static char	  *buffer;
    struct passwd *entry;


    if (!strncmp (file, "~/", 2)) {
	path = getenv ("HOME");
	file += 2;
    } else if (!strcmp (file, "~")) {
	path = getenv ("HOME");
	file ++;
    } else if (file [0] == '~') {
	ptr = file;
	while (*ptr && *ptr != '/')
	    ptr ++;

	old = *ptr;
	*ptr = 0;
	if ((entry = getpwnam (file + 1))) {
	    path = entry -> pw_dir;
	    file = ptr + 1;
	}
	*ptr = old;

    } else if (file [0] == '/')
	path = "/";


    if (!path)
	path = "./";


    ptr = path;
    length = strlen (path) + strlen (file) + strlen (suffix) + 2;

    if (buf_length < length)
	Reallocate (buffer, char, length);

    while (*ptr) {
	name = file;
	suff = suffix;
	copy = buffer;

	while (*ptr && *ptr != ':')
	    *copy ++ = *ptr ++;

	if (copy == buffer)
	    *copy ++ = '.';

	if (copy [-1] != '/')
	    *copy ++ = '/';

	while ((*copy ++ = *name ++));

	if (access (buffer, R_OK) != -1)
	    return buffer;

	copy --;
	while ((*copy ++ = *suff ++));

	if (access (buffer, R_OK) != -1)
	    return buffer;

	if (*ptr)
	    ptr ++;
    }

    return def_flag ? file : NULL;
}
