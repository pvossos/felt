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
 * File:	trap.c							*
 *									*
 * Description:	This file contains the function definitions for		*
 *		handling trapped variables.				*
 ************************************************************************/

# include "trap.h"
# include "debug.h"

# define MaxHandlers 32


static unsigned    num_handlers = 1;
static TrapHandler handlers [MaxHandlers];


/************************************************************************
 * Function:	AddTrap							*
 *									*
 * Description:	Adds a trapped variable handler.  If the handler	*
 *		exists then it is added to the list of handlers.  The	*
 *		index of the handler is returned.			*
 ************************************************************************/

int AddTrap (handler)
    TrapHandler handler;
{
    unsigned i;


    for (i = 1; i < num_handlers; i ++)
	if (handlers [i] == handler)
	    return i;

    handlers [num_handlers] = handler;
    d_printf ("new handler is %d\n", num_handlers);
    return num_handlers ++;
}


/************************************************************************
 * Function:	CallTrap						*
 *									*
 * Description:	Calls a trap handler.					*
 ************************************************************************/

int CallTrap (index, arg1, arg2)
    int		 index;
    descriptor	*arg1;
    descriptor **arg2;
{
    d_printf ("calling handler %d\n", index);
    return handlers [index] (arg1, arg2);
}


/************************************************************************
 * Function:	NumTraps						*
 *									*
 * Description:	Returns the number of currently active trapped variable	*
 *		handlers.						*
 ************************************************************************/

int NumTraps ( )
{
    return num_handlers;
}
