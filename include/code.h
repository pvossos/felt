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
 * File:	code.h							*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the stack machine code.		*
 ************************************************************************/

# ifndef _CODE_H
# define _CODE_H

typedef union instruction *Code;

typedef enum {
    JmpOp,		/* unconditional jump	    */
    JnzOp,		/* jump if not zero	    */
    JzOp,		/* jump if zero		    */
    PushOp,		/* push value		    */
    PopOp,		/* pop top of stack	    */
    CopyOp,		/* copy top of stack	    */
    TestOp,		/* test top of stack	    */
    NegOp,		/* unary negation	    */
    NotOp,		/* logical negation	    */
    InvOp,		/* bitwise negation	    */
    MulOp,		/* multiplication	    */
    DivOp,		/* division		    */
    ModOp,		/* modulo		    */
    AddOp,		/* addition		    */
    SubOp,		/* subtraction		    */
    LsftOp,		/* left shift		    */
    RsftOp,		/* right shift		    */
    LtOp,		/* less than		    */
    GtOp,		/* greater than		    */
    LteqOp,		/* less than or equal	    */
    GteqOp,		/* greater than or equal    */
    EqOp,		/* equality		    */
    NeqOp,		/* inequality		    */
    AndOp,		/* bitwise and		    */
    XorOp,		/* bitwise xor		    */
    OrOp,		/* bitwise or		    */
    TimeOp,		/* current time		    */
    SinOp,		/* sin function		    */
    CosOp,		/* cos function		    */
    TanOp,		/* tan function		    */
    ExpOp,		/* exp function		    */
    LnOp,		/* log function		    */
    LogOp,		/* log10 function	    */
    PowOp,		/* pow function		    */
    SqrtOp,		/* sqrt function	    */
    HypotOp,		/* hypot function	    */
    FloorOp,		/* floor function	    */
    CeilOp,		/* ceil function	    */
    FmodOp,		/* fmod function	    */
    FabsOp,		/* fabs function	    */
    TableOp,		/* table of values	    */
    CycleOp,		/* circular table of values */
    HaltOp		/* halt execution	    */
} Opcode;


extern Code   InCore;

/*!
  Adds an instruction to the current piece of code.
*/
void EmitCode(Opcode op, ...);

/*!
  Copies a piece of code.
*/
Code CopyCode(Code code);

/*!
  Deallocates a copied program.
*/
void FreeCode(Code code);

/*!
  Evaluates a piece of code.
*/
double EvalCode(Code code, double time);

/*!
  Print a piece of stack code as instructions.
*/
void DebugCode(Code code);

/*!
  Compiles a string to a piece of code.
*/
int CompileCode(char *text);

/*!
  Determines if a piece of code is constant.
*/
int IsConstant(Code code);

/*!
  Sets the address of the instruction pointer.
*/
void SetIP(int new_ip);

/*!
  Returns the address of the instruction pointer.
*/
int GetIP(void);

/*!
  Converts a string representing an expression to a double (works just
  like strtod ( )).
*/
double exptod(char *expr, char **ptr);

# endif /* _CODE_H */
