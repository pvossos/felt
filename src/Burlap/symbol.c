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
 * File:	symbol.c						*
 *									*
 * Description:	This file contains the public and private function	*
 *		definitions for the symbol table and symbol table	*
 *		entries.						*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include "symbol.h"
# include "allocate.h"

/************************************************************************
 * Function:	st_hash							*
 *									*
 * Description:	Computes and returns a hash value for a string.  This	*
 *		is from p. 436 of Aho, Sehti, and Ullman.		*
 ************************************************************************/

static int st_hash (char *s)
{
    unsigned g;
    unsigned h;


    for (h = 0; *s; s ++)
	if ((g = (h = (h << 4) + *s) & 0xf0000000))
	    h ^= (g >> 24) ^ g;

    return h % ST_SIZE;
}


/************************************************************************
 * Function:	st_lookup						*
 *									*
 * Description:	Looks up a symbol table entry by name.  If the entry is	*
 *		found then it is returned.  Otherwise, NULL is		*
 *		returned.						*
 ************************************************************************/

ste *st_lookup (st *table, char *name)
{
    ste *head;
    ste *ptr;


    head = &table -> bucket [st_hash (name)];
    for (ptr = head -> next; ptr; ptr = ptr -> next)
	if (!strcmp (name, ptr -> name))
	    return ptr;

    return NULL;
}


/************************************************************************
 * Function:	st_insert						*
 *									*
 * Description:	Inserts a name into a symbol table.  If a symbol table	*
 *		entry with the name already exists then it is returned.	*
 *		Otherwise, a new entry is created, given a copy of the	*
 *		name, and then returned.				*
 ************************************************************************/

ste *st_insert (st *table, char *name, Opcode op)
{
    ste *head;
    ste *ptr;


    head = &table -> bucket [st_hash (name)];
    for (ptr = head -> next; ptr; ptr = ptr -> next)
	if (!strcmp (name, ptr -> name))
	    return ptr;

    ptr = New (ste);
    ptr -> name = Strdup (name);
    ptr -> op = op;
    ptr -> idx = table -> num_syms ++;

    ptr -> next = head -> next;
    head -> next = ptr;

    return ptr;
}


/************************************************************************
 * Function:	st_index						*
 *									*
 * Description:	Looks up a symbol table entry by index number.  If the	*
 *		entry is found then it is returned.  Otherwise, NULL is	*
 *		returned.						*
 ************************************************************************/

ste *st_index (st *table, int idx)
{
    int  i;
    ste *ptr;
    ste *head;


    for (i = 0; i < ST_SIZE; i ++) {
	head = &table -> bucket [i];
	for (ptr = head -> next; ptr; ptr = ptr -> next)
	    if (ptr -> idx == idx)
		return ptr;
    }

    return NULL;
}


/************************************************************************
 * Function:	st_names						*
 *									*
 * Description:	Returns an array of names of symbol table entries.	*
 ************************************************************************/

char **st_names (st *table)
{
    int    i;
    ste   *ptr;
    ste   *head;
    char **names;


    names = Allocate (char *, table -> num_syms);

    for (i = 0; i < ST_SIZE; i ++) {
	head = &table -> bucket [i];
	for (ptr = head -> next; ptr; ptr = ptr -> next)
	    names [ptr -> idx] = Strdup (ptr -> name);
    }

    return names;
}


/************************************************************************
 * Function:	st_init							*
 *									*
 * Description:	Initializes a symbol table to have no symbols.		*
 ************************************************************************/

void st_init (st *table)
{
    int i;


    for (i = 0; i < ST_SIZE; i ++)
	table -> bucket [i].next = NULL;

    table -> num_syms = 0;
}


/************************************************************************
 * Function:	st_fini							*
 *									*
 * Description:	Deletes all symbols from a symbol table.		*
 ************************************************************************/

void st_fini (st *table)
{
    int  i;
    ste *head;
    ste *prev;
    ste *ptr;


    for (i = 0; i < ST_SIZE; i ++) {
	prev = NULL;
	head = &table -> bucket [i];
	for (ptr = head -> next; ptr; prev = ptr, ptr = ptr -> next) {
	    Deallocate (ptr -> name);
	    Deallocate (prev);
	}
	Deallocate (prev);
    }
}
