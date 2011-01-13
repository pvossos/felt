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

# ifndef _COMPLEX_H
# define _COMPLEX_H

typedef struct {
   double	r;
   double	i;
} complex;

double re(complex x);
double im(complex x);
double modulus(complex x);
complex negate(complex x);
complex recip(complex x);
complex mult(complex x, complex y);
complex add(complex x, complex y);
complex cdiv(complex x, complex y);
complex sub(complex x, complex y);
complex felt_conj (complex x);
complex scale(complex x, double factor, double offset);
complex felt_csqrt(complex x);
complex felt_cexp(double x);
complex felt_ccos(complex x);
complex felt_csin(complex x);
char *cprint (complex x);
unsigned is_zero(complex x);
complex zero(void);

# endif /* _COMPLEX_H */
