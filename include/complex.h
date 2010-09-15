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
 * File:	complex.h    
 *	
 * Description:	
 ************************************************************************/

# ifndef _COMPLEX_H
# define _COMPLEX_H

# include "proto.h"

typedef struct {
   double	r;
   double	i;
} complex;

double	re PROTO ((complex));
double 	im PROTO ((complex));
double  modulus PROTO ((complex));
complex negate PROTO ((complex));
complex recip PROTO ((complex));
complex mult PROTO ((complex, complex));
complex add PROTO ((complex, complex));
complex cdiv PROTO ((complex, complex));
complex sub PROTO ((complex, complex));
complex conj PROTO ((complex));
complex scale PROTO ((complex, double, double));
complex csqrt PROTO ((complex));
complex cexp PROTO ((double));
complex ccos PROTO ((complex));
complex csin PROTO ((complex));
char *cprint PROTO ((complex));
unsigned is_zero PROTO ((complex));
complex zero PROTO (( ));

# endif /* _COMPLEX_H */
