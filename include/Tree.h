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
 * File:	Tree.h							*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the red-black trees.			*
 ************************************************************************/

# ifndef _Tree_h
# define _Tree_h
# ifndef _Item_h
# include "Item.h"
# endif

typedef struct tree *Tree;

extern Tree TreeCreate        PROTO ((ItemComparator));
extern int  TreeDestroy       PROTO ((Tree));
extern int  TreeIterate       PROTO ((Tree));
extern int  TreeSize          PROTO ((Tree));

extern int  TreePreorder      PROTO ((Tree));
extern int  TreeInorder       PROTO ((Tree));
extern int  TreePostorder     PROTO ((Tree));

extern Item TreeInsert        PROTO ((Tree, Item));
extern Item TreeDelete        PROTO ((Tree, Item));
extern Item TreeSearch        PROTO ((Tree, Item));

extern Item TreeMinimum       PROTO ((Tree));
extern Item TreeMaximum       PROTO ((Tree));
extern Item TreePredecessor   PROTO ((Tree, Item));
extern Item TreeSuccessor     PROTO ((Tree, Item));

extern int  TreeSetIterator   PROTO ((Tree, ItemIterator));
extern int  TreeSetDestructor PROTO ((Tree, ItemDestructor));
extern int  TreeSetDuplicator PROTO ((Tree, ItemDuplicator));

# endif /* _Tree_h */
