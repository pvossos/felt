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
 * File:	consttab.h						*
 *									*
 * Description:	This file contains the table containing the names and	*
 *		values of each predefined constant.			*
 ************************************************************************/

# ifndef _CONSTTAB_H
# define _CONSTTAB_H
# include "fe.h"

# define NumConstants (sizeof (consttab) / sizeof (*consttab))

static struct {
    char  *name;
    double value;
} consttab [ ] = {

    /* analysis types */

    {"&static",		   Static},
    {"&transient",	   Transient},
    {"&modal",		   Modal},
    {"&static_thermal",	   StaticThermal},
    {"&transient_thermal", TransientThermal},
    {"&spectral",	   Spectral},


    /* element shapes */

    {"&linear", Linear},
    {"&planar", Planar},
    {"&solid",  Solid},


    /* load directions */

    {"&local_x",       LocalX},
    {"&local_y",       LocalY},
    {"&local_z",       LocalZ},
    {"&global_x",      GlobalX},
    {"&global_y",      GlobalY},
    {"&global_z",      GlobalZ},
    {"&parallel",      Parallel},
    {"&perpendicular", Perpendicular},


    /* degrees of freedom */

    {"&tx", Tx},
    {"&ty", Ty},
    {"&tz", Tz},
    {"&rx", Rx},
    {"&ry", Ry},
    {"&rz", Rz},
    {"&fx", Fx},
    {"&fy", Fy},
    {"&fz", Fz},
    {"&mx", Mx},
    {"&my", My},
    {"&mz", Mz},


    /* miscellanous constants */

    {"&unconstrained", 0},
    {"&constrained",   1},
    {"&hinged",	       'h'},

    {"&lumped",	    'l'},
    {"&consistent", 'c'},

    {"&true",  1},
    {"&false", 0},
};

# endif /* _CONSTTAB_H */
