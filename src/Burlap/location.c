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
 * File:	location.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		location related virtual machine instructions.		*
 ************************************************************************/

# include "debug.h"
# include "error.h"
# include "lexer.h"
# include "execute.h"
# include "location.h"


/************************************************************************
 * Function:	file_op							*
 *									*
 * Description:	Updates the current file name and number.		*
 ************************************************************************/

int file_op ( )
{
    curr_file_num = fetch (pc ++).ival;
    curr_file_name = *strlit (curr_file_num);
    d_printf ("file\t%s\n", curr_file_name);
    return 0;
}


/************************************************************************
 * Function:	line_op							*
 *									*
 * Description:	Updates the current line number.			*
 ************************************************************************/

int line_op ( )
{
    curr_line_num = fetch (pc ++).ival;
    d_printf ("line\t%d\n", curr_line_num);
    return 0;
}
