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
 * File:	opcodes.h						*
 *									*
 * Description:	This file contains the opcode declarations for the	*
 *		virtual machine.					*
 ************************************************************************/

# ifndef _OPCODES_H
# define _OPCODES_H

typedef enum {
    AddOp,		/* add				*/
    ApplyOp,		/* application			*/
    ArgOp,		/* argument			*/
    AsgnOp,		/* assign			*/
    BkslvOp,		/* backsolve			*/
    CopyOp,		/* copy				*/
    DblOp,		/* double constant		*/
    DivOp,		/* divide			*/
    EqOp,		/* equal			*/
    FailOp,		/* fail				*/
    FieldOp,		/* record field access		*/
    FileOp,		/* file name			*/
    GenOp,		/* generate			*/
    GeOp,		/* greater than or equal	*/
    GlblOp,		/* global variable		*/
    GtOp,		/* greater than			*/
    HaltOp,		/* halt execution		*/
    JmpOp,		/* unconditional jump		*/
    JnzOp,		/* jump if not equal to zero	*/
    JzOp,		/* jump if equal to zero	*/
    LeOp,		/* less than or equal		*/
    LineOp,		/* line number			*/
    LocalOp,		/* local variable		*/
    LtOp,		/* less than			*/
    ModOp,		/* modulo			*/
    MtxOp,		/* matrix			*/
    MulOp,		/* multiply			*/
    NeOp,		/* not equal			*/
    NegOp,		/* arithmetical negate		*/
    NotOp,		/* logical negate		*/
    NullOp,		/* null value			*/
    PlusOp,		/* arithmetical positive	*/
    PopOp,		/* pop				*/
    PowOp,		/* exponent			*/
    RangeOp,		/* range			*/
    RowOp,		/* matrix row			*/
    RtnOp,		/* return			*/
    StrOp,		/* string constant		*/
    SubOp,		/* subtract			*/
    TestOp,		/* test				*/
    TransOp		/* transpose			*/
} Opcode;

typedef int Address;

# endif /* _OPCODES_H */
