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
 * File:	loop.c							*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for translating loop constructs during	*
 *		parsing.						*
 ************************************************************************/

# include "loop.h"
# include "error.h"
# include "lexer.h"
# include "codegen.h"
# include "allocate.h"
# include "descriptor.h"
# include "tokens.h"


struct break_node {
    break_node link;
    Address    addr;
    int        type;
};

struct loop_node {
    loop_node  link;
    break_node queue;
    int        type;
};

static loop_node loop_stack;


/************************************************************************
 * Function:	start_break						*
 *									*
 * Description:	Starts a break or next expression by removing the top	*
 *		entry of the loop stack and popping the generator state	*
 *		if necessary.						*
 ************************************************************************/

loop_node start_break (type)
    int type;
{
    loop_node node;


    if ((node = loop_stack)) {
	loop_stack = node -> link;
	if (type == BREAK && node -> type == FOR)
	    emit (PopOp, 3);
    } else if (type == BREAK)
	cterror ("break expression not within a loop");
    else
	cterror ("next expression not within a loop");

    return node;
}


/************************************************************************
 * Function:	end_break						*
 *									*
 * Description:	Ends a break or next expression by restoring the top of	*
 *		the loop stack, adding the expression to the queue for	*
 *		the current depth, and emitting a jump instruction.	*
 ************************************************************************/

void end_break (type, lnode)
    int       type;
    loop_node lnode;
{
    break_node bnode;


    if (lnode) {
	loop_stack = lnode;
	bnode = New (struct break_node);
	bnode -> link  = lnode -> queue;
	bnode -> type  = type;
	bnode -> addr  = ip;
	lnode -> queue = bnode;
	emit (JmpOp, 0);
    }
}


/************************************************************************
 * Function:	enter_loop						*
 *									*
 * Description:	Enters a while or for loop by adding a new entry to the	*
 *		loop stack.						*
 ************************************************************************/

void enter_loop (type)
    int type;
{
    loop_node node;


    node = New (struct loop_node);
    node -> type  = type;
    node -> queue = NULL;
    node -> link  = loop_stack;
    loop_stack    = node;
}


/************************************************************************
 * Function:	exit_loop						*
 *									*
 * Description:	Exits a while or for loop by removing the top entry of	*
 *		the loop stack and processing any break or next		*
 *		expressions on its queue.				*
 ************************************************************************/

void exit_loop (start, end)
    Address start;
    Address end;
{
    loop_node  lnode;
    break_node bnode;
    break_node next;


    if ((lnode = loop_stack)) {
	loop_stack = lnode -> link;
	for (bnode = lnode -> queue; bnode; bnode = next) {
	    patch (bnode -> addr, JmpOp, bnode -> type == BREAK ? end : start);
	    next = bnode -> link;
	    Delete (bnode);
	}
	Delete (lnode);
    }
}


/************************************************************************
 * Function:	exit_all						*
 *									*
 * Description:	Exits all loops by repeatedly popping the loop stack	*
 *		and destroying the queues.				*
 ************************************************************************/

void exit_all ( )
{
    loop_node  lnode;
    break_node bnode;
    loop_node  next_lnode;
    break_node next_bnode;


    for (lnode = loop_stack; lnode; lnode = next_lnode) {
	for (bnode = lnode -> queue; bnode; bnode = next_bnode) {
	    next_bnode = bnode -> link;
	    Delete (bnode);
	}
	next_lnode = lnode -> link;
	Delete (lnode);
    }
}
