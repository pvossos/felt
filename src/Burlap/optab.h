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
 * File:	optab.h							*
 *									*
 * Description:	This file contains the table indicating the operand	*
 *		types and opcode names of each opcode of the virtual	*
 *		machine.  Note that this table must be kept in		*
 *		correspondence with the opcodes listed in opcodes.h!	*
 ************************************************************************/

# ifndef _OPTAB_H
# define _OPTAB_H

# define OP_NONE	0	/* no operand		    */
# define OP_ADDR	1	/* address operand	    */
# define OP_INT		2	/* integer constant operand */

static struct {
    char *name;
    int   type;
} optab [ ] = {
    {"add",	OP_NONE},
    {"apply",	OP_INT},
    {"arg",	OP_INT},
    {"asgn",	OP_NONE},
    {"bkslv",	OP_NONE},
    {"copy",	OP_NONE},
    {"dbl",	OP_INT},
    {"div",	OP_NONE},
    {"eq",	OP_NONE},
    {"fail",	OP_ADDR},
    {"field",	OP_INT},
    {"file",	OP_INT},
    {"gen",	OP_ADDR},
    {"ge",	OP_NONE},
    {"glbl",	OP_INT},
    {"gt",	OP_NONE},
    {"halt",	OP_NONE},
    {"jmp",	OP_ADDR},
    {"jnz",	OP_ADDR},
    {"jz",	OP_ADDR},
    {"le",	OP_NONE},
    {"line",	OP_INT},
    {"local",	OP_INT},
    {"lt",	OP_NONE},
    {"mod",	OP_NONE},
    {"mtx",	OP_INT},
    {"mul",	OP_NONE},
    {"ne",	OP_NONE},
    {"neg",	OP_NONE},
    {"not",	OP_NONE},
    {"null",	OP_NONE},
    {"plus",	OP_NONE},
    {"pop",	OP_INT},
    {"pow",	OP_NONE},
    {"range",	OP_INT},
    {"row",	OP_INT},
    {"rtn",	OP_NONE},
    {"str",	OP_INT},
    {"sub",	OP_NONE},
    {"test",	OP_NONE},
    {"trans",	OP_NONE},
};

# endif /* _OPTAB_H */
