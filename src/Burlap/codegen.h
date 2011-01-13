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
 * File:	codegen.h						*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for code generation for the virtual	*
 *		machine.						*
 ************************************************************************/

# ifndef _CODEGEN_H
# define _CODEGEN_H
# define _CODE_H
# include "opcodes.h"		/* need Address and Opcode types */

typedef struct cs *Code;

typedef union {
    Opcode  op;
    int     ival;
    Address addr;
} Word;

extern Code    cs;
extern Address ip;

void emit (Opcode op, ...);

void patch (Address addr, Opcode op, ...);

Word fetch (Address addr);

void dump (Code cs);

void free_cs (Code cs);

void reset (void);

Code new_cs (void);

# endif /* _CODEGEN_H */
