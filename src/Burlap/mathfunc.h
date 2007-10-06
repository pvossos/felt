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
 * File:	mathfunc.h						*
 *									*
 * Description:	This file contains the function declarations for the	*
 *		math library intrinsic functions.			*
 ************************************************************************/

# ifndef _MATHFUNC_H
# define _MATHFUNC_H

int ceil_func  (int n);
int cos_func   (int n);
int exp_func   (int n);
int fabs_func  (int n);
int floor_func (int n);
int fmod_func  (int n);
int hypot_func (int n);
int log_func   (int n);
int log10_func (int n);
int pow_func   (int n);
int sin_func   (int n);
int sqrt_func  (int n);
int tan_func   (int n);

# endif /* _MATHFUNC_H */
