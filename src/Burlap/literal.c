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
 * File:	literal.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		virtual machine instructions involving literals.	*
 ************************************************************************/

# include "debug.h"
# include "execute.h"
# include "literal.h"


/************************************************************************
 * Function:	arg_op							*
 *									*
 * Descriptor:	Pushes an argument descriptor on the stack.		*
 ************************************************************************/

int arg_op ( )
{
    descriptor *a;
    descriptor *d;



    d = push ( );
    a = argument (fetch (pc ++).ival);

    D_Type     (d) = T_Variable;
    D_Temp     (d) = F_False;
    D_Trapped  (d) = F_False;
    D_Variable (d) = D_Type (a) == T_Variable ? D_Variable (a) : a;

    d_printf ("arg\t%p\n", deref (d));
    return 0;
}


/************************************************************************
 * Function:	dbl_op							*
 *									*
 * Description:	Pushes a double literal descriptor on the stack.	*
 ************************************************************************/

int dbl_op ( )
{
    descriptor *d;


    d = push ( );
    D_Type    (d) = T_Double;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_Double  (d) = dbllit (fetch (pc ++).ival);

    d_printf ("dbl\t%g\n", *D_Double (d));
    return 0;
}


/************************************************************************
 * Function:	glbl_op							*
 *									*
 * Descriptor:	Pushes a global variable descriptor on the stack.	*
 ************************************************************************/

int glbl_op ( )
{
    descriptor *d;


    d = push ( );
    D_Type     (d) = T_Variable;
    D_Temp     (d) = F_False;
    D_Trapped  (d) = F_False;
    D_Variable (d) = global (fetch (pc ++).ival);

    d_printf ("glbl\t%p\n", deref (d));
    return 0;
}


/************************************************************************
 * Function:	local_op						*
 *									*
 * Description:	Pushes a local variable descriptor on the stack.	*
 ************************************************************************/

int local_op ( )
{
    descriptor *d;


    d = push ( );
    D_Type     (d) = T_Variable;
    D_Temp     (d) = F_False;
    D_Trapped  (d) = F_False;
    D_Variable (d) = local (fetch (pc ++).ival);

    d_printf ("local\t%p\n", deref (d));
    return 0;
}


/************************************************************************
 * Function:	null_op							*
 *									*
 * Description:	Pushes a null descriptor on the stack.			*
 ************************************************************************/

int null_op ( )
{
    descriptor *d;


    d = push ( );
    D_Type     (d) = T_Null;;
    D_Temp     (d) = F_False;
    D_Trapped  (d) = F_False;
    D_Variable (d) = NULL;

    d_printf ("null\n");
    return 0;
}


/************************************************************************
 * Function:	str_op							*
 *									*
 * Description:	Pushes a string literal descriptor on the stack.	*
 ************************************************************************/

int str_op ( )
{
    descriptor *d;


    d = push ( );
    D_Type    (d) = T_String;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_String  (d) = strlit (fetch (pc ++).ival);

    d_printf ("str\t%s\n", *D_String (d));
    return 0;
}
