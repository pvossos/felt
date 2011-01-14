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
 * File:	pslib.h							*
 *									*
 * Description:								*
 *									*
 ************************************************************************/

# ifndef _PSLIB_H
# define _PSLIB_H

# include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

int pssetmode(int mode, FILE *ps_fp);

void drawps(int x2, int y2, int mode);

int xconvps(double x);

int yconvps(double y);

int pssetcolor(int c);

int pssetlinewidth(int c);

int pssetlinestyle(int style);

void pssetfont(int n);

void pssetfontsize(double size);

void dispstrps(int x, int y, int rot, char *s, int just, int fudge);

int pssetpat(int k);

void psfill(int n, int *px, int *py);

void psfillcolor(int n, int *px, int *py);

void psdrawarc(int x, int y, int r, int start, int end);

void psfillarc(int x, int y, int r, int start, int end);

void psdrawellipse(int x, int y, int xm, int ym, int start, int end);

void psfillellipse(int x, int y, int xm, int ym, int start, int end);

int psgetextents (int *x, int *y);

void psleavegraphics(FILE *fp);

int psinitgraphics(int dmode, FILE *ps_fp);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _PSLIB_H */
