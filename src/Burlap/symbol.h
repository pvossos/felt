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
 * File:	symbol.h						*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the symbol table and symbol table	*
 *		entries.						*
 ************************************************************************/

# ifndef _SYMBOL_H
# define _SYMBOL_H
# include "proto.h"		/* function declarations */
# include "opcodes.h"		/* Opcode type		 */

# define ST_SIZE 211

typedef struct ste {
    struct ste *next;		/* next symbol in list */
    char       *name;		/* symbol name	       */
    Opcode	op;		/* associated opcode   */
    int		idx;		/* array index	       */
} ste;

typedef struct {
    ste bucket [ST_SIZE];	/* array of symbols    */
    int num_syms;		/* number of symbols   */
} st;


extern ste   *st_lookup PROTO ((st *, char *name));
extern ste   *st_insert PROTO ((st *, char *name, Opcode op));
extern ste   *st_index  PROTO ((st *, int));
extern char **st_names	PROTO ((st *));
extern void   st_init   PROTO ((st *));
extern void   st_fini   PROTO ((st *));

# endif /* _SYMBOL_H */
