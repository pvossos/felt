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
 * File:	debug.h							*
 *									*
 * Description:	This file contains some macro definitions and function	*
 *		declarations for debugging.				*
 ************************************************************************/

# ifndef _DEBUG_H
# define _DEBUG_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

extern int  debug;
extern void eprintf (const char *, ...);

# ifdef EXECDEBUG

# define d_dump		if (debug) dump
# define d_printf	if (debug) eprintf
# define d_PrintData	if (debug) PrintData

# else

# ifdef DEBUG

# define d_dump		dump
# define d_printf	eprintf
# define d_PrintData	PrintData

# else

# define d_dump		if (0) dump
# define d_printf	if (0) eprintf
# define d_PrintData	if (0) PrintData

# endif /* DEBUG */

# endif /* EXECDEBUG */

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _DEBUG_H */
