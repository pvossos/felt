/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson

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
 * File:	our-stdlib.h						*
 *									*
 * Description:	This file contains the function declarations for those	*
 *		functions that should be declared in stdlib.h, if your	*
 *		system has it.  Unfortunately, what belongs in stdlib.h	*
 *		varies from system to system, so we provide weak	*
 *		prototypes of the functions that we need.  If stdlib.h	*
 *		is included by some other system header file, the	*
 *		declarations here should not conflict with the		*
 *		system declarations.					*
 ************************************************************************/

# ifndef _OUR_STDLIB_H
# define _OUR_STDLIB_H

# undef drand48
# undef getenv
# undef qsort
# undef srand48
# undef strtod
# undef strtol
# undef system

extern double drand48 ( );
extern char  *getenv  ( );
extern void   qsort   ( );
extern void   srand48 ( );
extern double strtod  ( );
extern long   strtol  ( );
extern int    system  ( );

# endif /* _OUR_STDLIB_H */
