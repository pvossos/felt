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

Tree TreeCreate(ItemComparator compare);

int TreeDestroy(Tree tree);

int TreeSize(Tree tree);

int TreeIterate(Tree tree);

int TreePreorder(Tree tree);

int TreeInorder(Tree tree);

int TreePostorder(Tree tree);

Item TreeInsert(Tree tree, Item item);

Item TreeDelete(Tree tree, Item item);

Item TreeSearch(Tree tree, Item item);

Item TreeMinimum(Tree tree);

Item TreeMaximum(Tree tree);

Item TreePredecessor(Tree tree, Item item);

Item TreeSuccessor(Tree tree, Item item);

int TreeSetIterator(Tree tree, ItemIterator iterate);

int TreeSetDestructor(Tree tree, ItemDestructor destroy);

int TreeSetDuplicator(Tree tree, ItemDuplicator copy);

# endif /* _Tree_h */
