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
 * File:	assignment.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		assignment related virtual machine instructions.	*
 ************************************************************************/

# include <string.h>
# include "debug.h"
# include "coerce.h"
# include "execute.h"
# include "allocate.h"
# include "assignment.h"


/************************************************************************
 * Function:	asgn_op							*
 *									*
 * Description:	Pops the descriptor on the top of the stack and assigns	*
 *		its data to the descriptor now on the top of the stack.	*
 *		Most of the work is done by AssignData().		*
 ************************************************************************/

int asgn_op ( )
{
    descriptor *src;
    descriptor *dest;
    int		status;


    src  = pop ( );
    dest = top ( );
    src  = deref (src);


    if (!assignable (dest)) {
	TypeError ("cannot assign to", NULL, dest, NULL, F_False);
	RecycleData (src);
	return 1;
    }

    dest = deref (dest);
    src = CollapseMatrix (src);
    status = AssignData (dest, &src);
    RecycleData (src);
    D_Temp (dest) = F_False;

    return status;
}


/************************************************************************
 * Function:	copy_op							*
 *									*
 * Description:	Pushes a copy of the scalar descriptor on the top of	*
 *		the stack onto the stack.				*
 ************************************************************************/

int copy_op ( )
{
    double	value;
    descriptor *d;


    d = top ( );
    d = CoerceData (deref (d), T_Double);

    if (D_Type (d) != T_Double) {
	TypeError ("in conditional context", d, NULL, NULL, F_False);
	return 1;
    }

    value = *D_Double (d);
    d = push ( );
    CreateData (d, NULL, NULL, T_Double);
    *D_Double (d) = value;

    d_printf ("copy\n");
    return 0;
}


/************************************************************************
 * Function:	pop_op							*
 *									*
 * Description:	Pops and recycles the specified number of descriptors	*
 *		from the stack.						*
 ************************************************************************/

int pop_op ( )
{
    int i;
    int n;


    n = fetch (pc ++).ival;
    for (i = 0; i < n; i ++)
	RecycleData (pop ( ));

    d_printf ("pop\n");
    return 0;
}


/************************************************************************
 * Function:	test_op							*
 *									*
 * Description:	Pops and tests the descriptor on the top of the stack.	*
 *		If the descriptor is zero then a zero descriptor is	*
 *		pushed on the stack.  Otherwise a one descriptor is	*
 *		pushed on the stack.					*
 ************************************************************************/

int test_op ( )
{
    double	value;
    descriptor *d;


    d = pop ( );
    d = CoerceData (deref (d), T_Double);

    if (D_Type (d) != T_Double) {
	TypeError ("in conditional context", d, NULL, NULL, F_False);
	return 1;
    }

    value = *D_Double (d);
    d = push ( );
    D_Type    (d) = T_Double;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_Double  (d) = dbllit (value != 0);

    d_printf ("test\n");
    return 0;
}
