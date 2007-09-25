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
 * File:	codegen.c						*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for code generation for the virtual	*
 *		machine.						*
 ************************************************************************/

# include <stdio.h>
# include "optab.h"
# include "codegen.h"
# include "allocate.h"
# include VAR_ARGS_INCLUDE

# define InitCodeSize 256


Address ip;
Code    cs;

struct cs {
    Word    *memory;
    unsigned count;
    unsigned size;
};


/************************************************************************
 * Function:	emit_instr						*
 *									*
 * Description:	Emits an instruction at the specified address expanding	*
 *		the current code segment if necessary.  An address	*
 *		operand is converted from an absolute address to a	*
 *		relative address which will allow the code to be moved	*
 *		to another starting address if necessary.		*
 ************************************************************************/

static int
emit_instr(Address addr, Opcode op, va_list ap)
{
    if (cs -> count + 2 >= cs -> size)
	Reallocate (cs -> memory, Word, cs -> size <<= 1);

    switch (optab [op].type) {
    case OP_NONE:
	cs -> memory [addr].op = op;
	return 1;

    case OP_ADDR:
	cs -> memory [addr ++].op = op;
	cs -> memory [addr].addr = va_arg (ap, Address) - addr - 1;
	return 2;

    case OP_INT:
	cs -> memory [addr ++].op = op;
	cs -> memory [addr].ival = va_arg (ap, int);
	return 2;
    }

    return 0;
}


/************************************************************************
 * Function:	emit							*
 *									*
 * Description:	Emits an instruction at the address given by the	*
 *		instruction pointer which is then incremented.		*
 ************************************************************************/

# ifdef UseFunctionPrototypes
void emit (Opcode op, ...)
# else
void emit (op, va_alist)
    Opcode op;
    va_dcl
# endif
{
    int     inc;
    va_list ap;


    VA_START (ap, op);
    inc = emit_instr (ip, op, ap);
    cs -> count += inc;
    ip += inc;
    va_end (ap);
}


/************************************************************************
 * Function:	patch							*
 *									*
 * Description:	Emits an instruction at the specified address without	*
 *		changing the instruction pointer.			*
 ************************************************************************/

# ifdef UseFunctionPrototypes
void patch (Address addr, Opcode op, ...)
# else
void patch (addr, op, va_alist)
    Address addr;
    Opcode  op;
    va_dcl
# endif
{
    va_list ap;


    VA_START (ap, op);
    emit_instr (addr, op, ap);
    va_end (ap);
}


/************************************************************************
 * Function:	fetch							*
 *									*
 * Description:	Returns the word at the specified address from the	*
 *		current code segment.					*
 ************************************************************************/

Word fetch (addr)
    Address addr;
{
    static Word empty_word;


    return addr < 0 || addr > cs -> count ? empty_word : cs -> memory [addr];
}


/************************************************************************
 * Function:	reset							*
 *									*
 * Description:	Resets the current code segment.			*
 ************************************************************************/

void reset ( )
{
    ip = 0;
    cs -> count = 0;
}


/************************************************************************
 * Function:	free_cs							*
 *									*
 * Description:	Deallocates a code segment.				*
 ************************************************************************/

void free_cs (cs)
    Code cs;
{
    Deallocate (cs -> memory);
    Delete (cs);
}


/************************************************************************
 * Function:	new_cs							*
 *									*
 * Description:	Creates, initializes, and returns a new code segment.	*
 ************************************************************************/

Code new_cs ( )
{
    Code cs;


    cs = New (struct cs);

    cs -> count = 0;
    cs -> size = InitCodeSize;
    cs -> memory = Allocate (Word, cs -> size);

    return cs;
}


/************************************************************************
 * Function:	dump							*
 *									*
 * Description:	Dumps a code segment to standard output.		*
 ************************************************************************/

void dump (cs)
    Code cs;
{
    Opcode   op;
    unsigned i;


    if (!cs)
	return;


    for (i = 0; i < cs -> count; i ++) {
	op = cs -> memory [i].op;
	fprintf (stderr, "%03u\t%s", i, optab [op].name);
	switch (optab [op].type) {
	case OP_NONE:
	    fprintf (stderr, "\n");
	    break;

	case OP_ADDR:
	    i ++;
	    fprintf (stderr, "\t%d\n", cs -> memory [i].addr + i + 1);
	    break;

	case OP_INT:
	    fprintf (stderr, "\t%d\n", cs -> memory [++ i].ival);
	    break;
	}
    }
}
