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
 * File:	code.c							*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the stack machine code.		*
 ************************************************************************/

# include <stdio.h>
# include <math.h>
# include "code.h"
# include "allocate.h"
# include VAR_ARGS_INCLUDE


# define MaxStackDepth	1024
# define MaxCodeSize	1024

# define push(x)	(* ++ sp = (x))
# define pop()		(* sp --)
# define top()		(* sp)

# define deg2rad(x)	((x) * M_PI / 180.0)


# define None	 0
# define Integer 1
# define Double  2
# define Array	 3

static struct {
    char *opcode;
    int   arg_type;
} data [ ] = {
    {"jmp",   Integer},
    {"jnz",   Integer},
    {"jz",    Integer},
    {"push",  Double},
    {"pop",   None},
    {"copy",  None},
    {"test",  None},
    {"neg",   None},
    {"not",   None},
    {"inv",   None},
    {"mul",   None},
    {"div",   None},
    {"mod",   None},
    {"add",   None},
    {"sub",   None},
    {"lsft",  None},
    {"rsht",  None},
    {"lt",    None},
    {"gt",    None},
    {"lteq",  None},
    {"gteq",  None},
    {"eq",    None},
    {"neq",   None},
    {"and",   None},
    {"xor",   None},
    {"or",    None},
    {"time",  None},
    {"sin",   None},
    {"cos",   None},
    {"tan",   None},
    {"exp",   None},
    {"ln",    None},
    {"log",   None},
    {"pow",   None},
    {"sqrt",  None},
    {"hypot", None},
    {"floor", None},
    {"ceil",  None},
    {"fmod",  None},
    {"fabs",  None},
    {"table", Array},
    {"cycle", Array},
    {"halt",  None},
};


typedef union instruction {
    Opcode op;
    int    offset;
    double arg;
} Instruction;


static Instruction in_core [MaxCodeSize];
static Code	   ip = in_core;

static double	   stack [MaxStackDepth];
static double	  *sp;

Code InCore = in_core;


/************************************************************************
 * Function:	EmitCode						*
 *									*
 * Description:	Adds an instruction to the current piece of code.	*
 ************************************************************************/

# ifdef UseFunctionPrototypes
void EmitCode (Opcode op, ...)
# else
void EmitCode (op, va_alist)
    Opcode op;
    va_dcl
# endif
{
    va_list ap;
    int     i;
    int     length;
    double *array;


    ip ++ -> op = op;

    switch (data [op].arg_type) {
    case Integer:
	VA_START (ap, op);
	ip ++ -> offset = va_arg (ap, int);
	va_end (ap);
	break;

    case Double:
	VA_START (ap, op);
	ip ++ -> arg = va_arg (ap, double);
	va_end (ap);
	break;

    case Array:
	VA_START (ap, op);
	array = va_arg (ap, double *);
	ip ++ -> offset = length = va_arg (ap, int);
	va_end (ap);
	for (i = 0; i < length; i ++)
	    ip ++ -> arg = array [i];
	break;
    }
}


/************************************************************************
 * Function:	CopyCode						*
 *									*
 * Description:	Copies a piece of code.					*
 ************************************************************************/

Code CopyCode (code)
    Code code;
{
    Code     pc;
    Code     ptr;
    Code     copy;
    Opcode   op;
    unsigned size;


    size = 0;
    if (!(pc = code))
	return NULL;

    while (1) {
	size ++;
	if ((op = pc ++ -> op) == HaltOp)
	    break;
	if (data [op].arg_type == Array) {
	    size += pc -> offset;
	    pc += pc -> offset;
	}
	if (data [op].arg_type != None) {
	    size ++;
	    pc ++;
	}
    }

    if (!(copy = Allocate (Instruction, size)))
	return NULL;

    pc = code;
    ptr = copy;

    while (size --)
	*ptr ++ = *pc ++;

    return copy;
}


/************************************************************************
 * Function:	FreeCode						*
 *									*
 * Description:	Deallocates a copied program.				*
 ************************************************************************/

void FreeCode (code)
    Code code;
{
    if (code != InCore)
	Deallocate (code);
}


/************************************************************************
 * Function:	SetIP							*
 *									*
 * Description:	Sets the address of the instruction pointer.		*
 ************************************************************************/

void SetIP (new_ip)
    int new_ip;
{
    ip = InCore + new_ip;
}


/************************************************************************
 * Function:	GetIP							*
 *									*
 * Description:	Returns the address of the instruction pointer.		*
 ************************************************************************/

int GetIP ( )
{
    return ip - InCore;
}


/************************************************************************
 * Function:	EvalTable						*
 *									*
 * Description:	Evaluates a table or cycle opcode.			*
 ************************************************************************/

static double EvalTable (array, length, time, flag)
    double *array;
    int     length;
    double  time;
    int     flag;
{
    int    i1;
    int    i2;
    double max;
    double t1;
    double t2;
    double x1;
    double x2;


    if (length == 2)
	return time == array [0] ? array [1] : 0;

    if (flag) {
	max = array [length - 2];
	while (time > max)
	    time -= max;
    }

    for (i1 = 0; i1 < length; i1 += 2)
	if (time <= array [i1])
	    break;

    if (i1 == length) {
	i1 = i1 - 2;
	i2 = i1 - 2;
    } else if (i1)
	i2 = i1 - 2;
    else
	i2 = i1 + 2;

    t1 = array [i1];
    t2 = array [i2];
    x1 = array [i1 + 1];
    x2 = array [i2 + 1];

    /*printf ("(%g,%g) %g (%g,%g)\n", t1, x1, time, t2, x2);*/
    return t1 != t2 ? (x2 - x1) / (t2 - t1) * (time - t1) + x1 : x2;
}


/************************************************************************
 * Function:	EvalCode						*
 *									*
 * Description:	Evaluates a piece of code.				*
 ************************************************************************/

double EvalCode (code, time)
    Code code;
    double time;
{
    int    x;
    int    y;
    double a;
    double b;
    Code   pc;


    sp = stack;
    if (!(pc = code))
	return 0;

    while (1)
	switch (pc ++ -> op) {
	case JmpOp:
	    y = pc ++ -> offset;
	    pc += y;
	    break;

	case JnzOp:
	    x = pop ( );
	    y = pc ++ -> offset;
	    if (x) pc += y;
	    break;

	case JzOp:
	    x = pop ( );
	    y = pc ++ -> offset;
	    if (!x) pc += y;
	    break;

	case PushOp:
	    push (pc ++ -> arg);
	    break;

	case PopOp:
	    a = pop ( );
	    break;

	case CopyOp:
	    a = top ( );
	    push (a);
	    break;

	case TestOp:
	    a = pop ( );
	    push (a != 0);
	    break;

	case NegOp:
	    a = pop ( );
	    push (-a);
	    break;

	case NotOp:
	    a = pop ( );
	    push (!a);
	    break;

	case InvOp:
	    x = pop ( );
	    push (~x);
	    break;

	case MulOp:
	    b = pop ( );
	    a = pop ( );
	    push (a * b);
	    break;

	case DivOp:
	    b = pop ( );
	    a = pop ( );
	    push (b ? a / b : 0);
	    break;

	case ModOp:
	    y = pop ( );
	    x = pop ( );
	    push (y ? x % y : 0);
	    break;

	case AddOp:
	    b = pop ( );
	    a = pop ( );
	    push (a + b);
	    break;

	case SubOp:
	    b = pop ( );
	    a = pop ( );
	    push (a - b);
	    break;

	case LsftOp:
	    y = pop ( );
	    x = pop ( );
	    push (x << y);
	    break;

	case RsftOp:
	    y = pop ( );
	    x = pop ( );
	    push (x >> y);
	    break;

	case LtOp:
	    b = pop ( );
	    a = pop ( );
	    push (a < b);
	    break;

	case GtOp:
	    b = pop ( );
	    a = pop ( );
	    push (a > b);
	    break;

	case LteqOp:
	    b = pop ( );
	    a = pop ( );
	    push (a <= b);
	    break;

	case GteqOp:
	    b = pop ( );
	    a = pop ( );
	    push (a >= b);
	    break;

	case EqOp:
	    b = pop ( );
	    a = pop ( );
	    push (a == b);
	    break;

	case NeqOp:
	    b = pop ( );
	    a = pop ( );
	    push (a != b);
	    break;

	case AndOp:
	    y = pop ( );
	    x = pop ( );
	    push (x & y);
	    break;

	case XorOp:
	    y = pop ( );
	    x = pop ( );
	    push (x ^ y);
	    break;

	case OrOp:
	    y = pop ( );
	    x = pop ( );
	    push (x | y);
	    break;

	case TimeOp:
	    push (time);
	    break;

	case SinOp:
	    a = pop ( );
	    push (sin (a));
	    break;

	case CosOp:
	    a = pop ( );
	    push (cos (a));
	    break;

	case TanOp:
	    a = pop ( );
	    push (tan (a));
	    break;

	case ExpOp:
	    a = pop ( );
	    push (exp (a));
	    break;

	case LnOp:
	    a = pop ( );
	    push (a > 0 ? log (a) : 0);
	    break;

	case LogOp:
	    a = pop ( );
	    push (a > 0 ? log10 (a) : 0);
	    break;

	case PowOp:
	    b = pop ( );
	    a = pop ( );
	    push (a >= 0 || b == (int) b ? pow (a, b) : 0);
	    break;

	case SqrtOp:
	    a = pop ( );
	    push (a >= 0 ? sqrt (a) : 0);
	    break;

	case HypotOp:
	    b = pop ( );
	    a = pop ( );
	    push (hypot (a, b));
	    break;

	case FloorOp:
	    a = pop ( );
	    push (floor (a));
	    break;

	case CeilOp:
	    a = pop ( );
	    push (ceil (a));
	    break;

	case FmodOp:
	    b = pop ( );
	    a = pop ( );
	    push (b ? fmod (a, b) : 0);
	    break;

	case FabsOp:
	    a = pop ( );
	    push (fabs (a));
	    break;

	case TableOp:
	    y = pc ++ -> offset;
	    push (EvalTable (pc, y, time, 0));
	    pc += y;
	    break;

	case CycleOp:
	    y = pc ++ -> offset;
	    push (EvalTable (pc, y, time, 1));
	    pc += y;
	    break;

	case HaltOp:
	    return pop ( );
	}
}


/************************************************************************
 * Function:	DebugCode						*
 *									*
 * Description:	Print a piece of stack code as instructions.		*
 ************************************************************************/

void DebugCode (code)
    Code code;
{
    int     i;
    int     x;
    Opcode  op;
    Code    pc;


    if (!(pc = code))
	return;

    while (1) {
	op = pc -> op;
	printf ("%x\t%s", (int) (pc ++), data [op].opcode);

	switch (data [op].arg_type) {
	case Integer:
	    x = pc ++ -> offset;
	    printf ("\t%x\n", (int) (pc + x));
	    break;

	case Double:
	    printf ("\t%g\n", pc ++ -> arg);
	    break;

	case Array:
	    x = pc ++ -> offset;
	    printf ("\t");
	    for (i = 0; i < x; i ++)
		printf ("%g ", pc ++ -> arg);
	    printf ("\n");
	    break;

	default:
	    printf ("\n");
	}

	if (op == HaltOp)
	    return;
    }
}


/************************************************************************
 * Function:	IsConstant						*
 *									*
 * Description:	Determines if a piece of code is constant.		*
 ************************************************************************/

int IsConstant (code)
    Code code;
{
    Opcode op;
    Code   pc;


    if (!(pc = code))
	return 1;

    while (1)
	switch (op = pc ++ -> op) {
	case TimeOp:
	case TableOp:
	case CycleOp:
	    return 0;

	case HaltOp:
	    return 1;

	default:
	    if (data [op].arg_type != None)
		pc ++;
    }
}
