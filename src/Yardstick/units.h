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

/*****************************************************************************
 *
 * File:	units.h
 * 
 * Description:	contains definitions for all of the unit conversion
 *		structures.  Reference units are Newtons, meters and
 *		kilograms.
 *
 ****************************************************************************/

# ifndef _UNITS_H
# define _UNITS_H

static Unit force_units [ ] = {
    {"N",	1.0},
    {"MN",	1.0e-6},
    {"GN",	1.0e-9},
    {"lbs",	0.22480894},
    {"kips",	0.00022480894}
};    

static Unit length_units [ ] = {
    {"m",	1.0},
    {"mm",	1000.0},
    {"cm",	100.0},
    {"in",	39.370079},
    {"ft",      3.2808399}
};

# endif /* _UNITS_H */
