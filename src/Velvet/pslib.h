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
 * File:	pslib.h							*
 *									*
 * Description:								*
 *									*
 ************************************************************************/

# ifndef _PSLIB_H
# define _PSLIB_H

# include <stdio.h>

int pssetmode( );
void drawps ( );
int xconvps ( );
int yconvps ( );
int pssetcolor ( );
int pssetlinewidth ( );
int pssetlinestyle ( );
void pssetfont ( );
void pssetfontsize ( );
void dispstrps ( );
int pssetpat ( );
void psfill ( );
void psfillcolor ( );
void psdrawarc ( );
void psfillarc ( );
void psdrawellipse ( );
void psfillellipse ( );
int psgetextents ( );
void psleavegraphics ( );
int psinitgraphics ( );

# endif /* _PSLIB_H */
