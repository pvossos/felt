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
 * File:	mathfunc.h						*
 *									*
 * Description:	This file contains the function declarations for the	*
 *		math library intrinsic functions.			*
 ************************************************************************/

# ifndef _MATHFUNC_H
# define _MATHFUNC_H
# include "proto.h"			/* function declarations */

extern int ceil_func  PROTO ((int));
extern int cos_func   PROTO ((int));
extern int exp_func   PROTO ((int));
extern int fabs_func  PROTO ((int));
extern int floor_func PROTO ((int));
extern int fmod_func  PROTO ((int));
extern int hypot_func PROTO ((int));
extern int log_func   PROTO ((int));
extern int log10_func PROTO ((int));
extern int pow_func   PROTO ((int));
extern int sin_func   PROTO ((int));
extern int sqrt_func  PROTO ((int));
extern int tan_func   PROTO ((int));

# endif /* _MATHFUNC_H */
