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
 * File:	Tree.c							*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the red-black trees as described	*
 *		in Cormen, Leiserson, and Rivest (CLR): "Introduction	*
 *		to Algorithms".						*
 ************************************************************************/

# include "Tree.h"
# define NIL (&sentinel)
# define NULL 0

typedef enum {
    Red, Black
} Color;

typedef struct node {
    Item         key;
    Color        color;
    struct node *left;
    struct node *right;
    struct node *parent;
} *Node;

struct tree {
    Node           root;
    int            count;
    ItemIterator   iterate;
    ItemComparator compare;
    ItemDestructor destroy;
    ItemDuplicator copy;
};


static struct node    sentinel = {NULL, Black, NULL, NULL, NULL};
static ItemDestructor destructor;
static ItemIterator   iterator;
static int            error_flag;

extern char *malloc ( );
extern void  free   ( );


static void TreeDestroy_ (tree, node)
    Tree tree;
    Node node;
{
    if (!error_flag && node != NIL) {
	TreeDestroy_ (tree, node -> left);
	if (!error_flag)
	    TreeDestroy_ (tree, node -> right);
	if (!error_flag && destructor != NULL)
	    error_flag = destructor (node -> key);
	if (!error_flag) {
	    tree -> count --;
	    free ((char *) node);
	}
    }
}


static void preorder (node)
    Node node;
{
    if (!error_flag && node != NIL) {
	error_flag = iterator (node -> key);
	if (!error_flag)
	    preorder (node -> left);
	if (!error_flag)
	    preorder (node -> right);
    }
}


static void inorder (node)
    Node node;
{
    if (!error_flag && node != NIL) {
	inorder (node -> left);
	if (!error_flag)
	    error_flag = iterator (node -> key);
	if (!error_flag)
	    inorder (node -> right);
    }
}


static void postorder (node)
    Node node;
{
    if (!error_flag && node != NIL) {
	postorder (node -> left);
	if (!error_flag)
	    postorder (node -> right);
	if (!error_flag)
	    error_flag = iterator (node -> key);
    }
}


static void LeftRotate (tree, x)
    Tree tree;
    Node x;
{
    Node y;


    /* Left-Rotate from CLR p. 266 */

    y = x -> right;
    x -> right = y -> left;
    if (y -> left != NIL)
	y -> left -> parent = x;
    y -> parent = x -> parent;
    if (x -> parent == NIL)
	tree -> root = y;
    else if (x == x -> parent -> left)
	x -> parent -> left = y;
    else
	x -> parent -> right = y;
    y -> left = x;
    x -> parent = y;
}


static void RightRotate (tree, x)
    Tree tree;
    Node x;
{
    Node y;


    /* Right-Rotate from CLR p. 266 */

    y = x -> left;
    x -> left = y -> right;
    if (y -> right != NIL)
	y -> right -> parent = x;
    y -> parent = x -> parent;
    if (x -> parent == NIL)
	tree -> root = y;
    else if (x == x -> parent -> right)
	x -> parent -> right = y;
    else
	x -> parent -> left = y;
    y -> right = x;
    x -> parent = y;
}


static Node TreeMinimum_ (x)
    Node x;
{
    /* Tree-Minimum from CLR p. 248 */

    while (x -> left != NIL)
	x = x -> left;

    return x;
}


static Node TreeMaximum_ (x)
    Node x;
{
    /* Tree-Maximum from CLR p. 248 */

    while (x -> right != NIL)
	x = x -> right;

    return x;
}


static Node TreePredecessor_ (x)
    Node x;
{
    Node y;


    /* Tree-Predecessor from CLR p. 249 */

    if (x -> left != NIL)
	return TreeMaximum_ (x -> left);

    y = x -> parent;
    while (y != NIL && x == y -> left) {
	x = y;
	y = y -> parent;
    }

    return y;
}


static Node TreeSuccessor_ (x)
    Node x;
{
    Node y;


    /* Tree-Successor from CLR p. 249 */

    if (x -> right != NIL)
	return TreeMinimum_ (x -> right);

    y = x -> parent;
    while (y != NIL && x == y -> right) {
	x = y;
	y = y -> parent;
    }

    return y;
}


static Node TreeSearch_ (tree, item)
    Tree tree;
    Item item;
{
    int  cmp;
    Node x;


    /* Iterative-Tree-Search from CLR p. 248 */

    x = tree -> root;
    while (x != NIL && (cmp = tree -> compare (item, x -> key)))
	x = cmp < 0 ? x -> left : x -> right;

    return x;
}


Tree TreeCreate (compare)
    ItemComparator compare;
{
    Tree tree;


    if (compare == NULL || !(tree = (Tree) malloc (sizeof (struct tree))))
	return NULL;

    tree -> root    = NIL;
    tree -> copy    = NULL;
    tree -> iterate = NULL;
    tree -> destroy = NULL;
    tree -> compare = compare;
    tree -> count   = 0;

    return tree;
}


int TreeDestroy (tree)
    Tree tree;
{
    if (tree == NULL)
	return -1;

    error_flag = 0;
    destructor = tree -> destroy;
    TreeDestroy_ (tree, tree -> root);

    if (!error_flag)
	free ((char *) tree);

    return error_flag;
}


int TreeSize (tree)
    Tree tree;
{
    return tree == NULL ? -1 : tree -> count;
}


int TreeIterate (tree)
    Tree tree;
{
    return TreeInorder (tree);
}


int TreePreorder (tree)
    Tree tree;
{
    if (tree == NULL || tree -> iterate == NULL)
	return -1;

    error_flag = 0;
    iterator = tree -> iterate;
    preorder (tree -> root);

    return error_flag;
}


int TreeInorder (tree)
    Tree tree;
{
    if (tree == NULL || tree -> iterate == NULL)
	return -1;

    error_flag = 0;
    iterator = tree -> iterate;
    inorder (tree -> root);

    return error_flag;
}


int TreePostorder (tree)
    Tree tree;
{
    if (tree == NULL || tree -> iterate == NULL)
	return -1;

    error_flag = 0;
    iterator = tree -> iterate;
    postorder (tree -> root);

    return error_flag;
}


Item TreeInsert (tree, item)
    Tree tree;
    Item item;
{
    int  cmp;
    Node x, y, z;


    if (tree == NULL)
	return NULL;


    /* Tree-Insert from CLR p. 251 */

    y = NIL;
    x = tree -> root;
    while (x != NIL) {
	y = x;
	if (!(cmp = tree -> compare (item, x -> key)))
	    return x -> key;
	x = cmp < 0 ? x -> left : x -> right;
    }

    if (!(z = (Node) malloc (sizeof (struct node))))
	return NULL;

    z -> left   = NIL;
    z -> right  = NIL;
    z -> parent = y;
    if (!(z -> key = tree -> copy != NULL ? tree -> copy (item) : item))
	return NULL;

    if (y == NIL)
	tree -> root = z;
    else if (tree -> compare (z -> key, y -> key) < 0)
	y -> left = z;
    else
	y -> right = z;


    /* RB-Insert from CLR p. 268 */

    x = z;
    x -> color = Red;
    while (x != tree -> root && x -> parent -> color == Red)
	if (x -> parent == x -> parent -> parent -> left) {
	    y = x -> parent -> parent -> right;
	    if (y -> color == Red) {
		x -> parent -> color = Black;
		y -> color = Black;
		x -> parent -> parent -> color = Red;
		x = x -> parent -> parent;
	    } else {
		if (x == x -> parent -> right) {
		    x = x -> parent;
		    LeftRotate (tree, x);
		}
		x -> parent -> color = Black;
		x -> parent -> parent -> color = Red;
		RightRotate (tree, x -> parent -> parent);
	    }
	} else {
	    y = x -> parent -> parent -> left;
	    if (y -> color == Red) {
		x -> parent -> color = Black;
		y -> color = Black;
		x -> parent -> parent -> color = Red;
		x = x -> parent -> parent;
	    } else {
		if (x == x -> parent -> left) {
		    x = x -> parent;
		    RightRotate (tree, x);
		}
		x -> parent -> color = Black;
		x -> parent -> parent -> color = Red;
		LeftRotate (tree, x -> parent -> parent);
	    }
	}

    tree -> root -> color = Black;

    tree -> count ++;
    return item;
}


Item TreeDelete (tree, item)
    Tree tree;
    Item item;
{
    Node w, x, y, z;


    if (tree == NULL)
	return NULL;

    if ((z = TreeSearch_ (tree, item)) == NIL)
	return NULL;

    item = z -> key;


    /* RB-Delete from CLR p. 273 */

    y = z -> left == NIL || z -> right == NIL ? z : TreeSuccessor_ (z);
    x = y -> left != NIL ? y -> left : y -> right;
    x -> parent = y -> parent;
    if (y -> parent == NIL)
	tree -> root = x;
    else if (y == y -> parent -> left)
	y -> parent -> left = x;
    else
	y -> parent -> right = x;

    if (y != z)
	z -> key = y -> key;

    if (y -> color == Black) {


	/* RB-Delete-Fixup from CLR p. 274 */

	while (x != tree -> root && x -> color == Black)
	    if (x == x -> parent -> left) {
		w = x -> parent -> right;
		if (w -> color == Red) {
		    w -> color = Black;
		    x -> parent -> color = Red;
		    LeftRotate (tree, x -> parent);
		    w = x -> parent -> right;
		}
		if (w -> left -> color ==Black && w -> right -> color ==Black) {
		    w -> color = Red;
		    x = x -> parent;
		} else {
		    if (w -> right -> color == Black) {
			w -> left -> color = Black;
			w -> color = Red;
			RightRotate (tree, w);
			w = x -> parent -> right;
		    }
		    w -> color = x -> parent -> color;
		    x -> parent -> color = Black;
		    w -> right -> color = Black;
		    LeftRotate (tree, x -> parent);
		    x = tree -> root;
		}
	    } else {
		w = x -> parent -> left;
		if (w -> color == Red) {
		    w -> color = Black;
		    x -> parent -> color = Red;
		    RightRotate (tree, x -> parent);
		    w = x -> parent -> left;
		}
		if (w -> right -> color ==Black && w -> left -> color ==Black) {
		    w -> color = Red;
		    x = x -> parent;
		} else {
		    if (w -> left -> color == Black) {
			w -> right -> color = Black;
			w -> color = Red;
			LeftRotate (tree, w);
			w = x -> parent -> left;
		    }
		    w -> color = x -> parent -> color;
		    x -> parent -> color = Black;
		    w -> left -> color = Black;
		    RightRotate (tree, x -> parent);
		    x = tree -> root;
		}
	    }

	x -> color = Black;
    }

    free ((char *) y);
    tree -> count --;
    return item;
}


Item TreeSearch (tree, item)
    Tree tree;
    Item item;
{
    Node x;


    x = TreeSearch_ (tree, item);
    return x != NIL ? x -> key : NULL;
}


Item TreeMinimum (tree)
    Tree tree;
{
    if (tree == NULL || tree -> root == NIL)
	return NULL;

    return TreeMinimum_ (tree -> root) -> key;
}


Item TreeMaximum (tree)
    Tree tree;
{
    if (tree == NULL || tree -> root == NIL)
	return NULL;

    return TreeMaximum_ (tree -> root) -> key;
}


Item TreePredecessor (tree, item)
    Tree tree;
    Item item;
{
    Node x;


    if (tree == NULL)
	return NULL;

    if ((x = TreeSearch_ (tree, item)) == NIL)
	return NULL;

    x = TreePredecessor_ (x);
    return x != NIL ? x -> key : NULL;
}


Item TreeSuccessor (tree, item)
    Tree tree;
    Item item;
{
    Node x;


    if (tree == NULL)
	return NULL;

    if ((x = TreeSearch_ (tree, item)) == NIL)
	return NULL;

    x = TreeSuccessor_ (x);
    return x != NIL ? x -> key : NULL;
}


int TreeSetIterator (tree, iterate)
    Tree         tree;
    ItemIterator iterate;
{
    if (tree == NULL)
	return -1;

    tree -> iterate = iterate;
    return 0;
}


int TreeSetDestructor (tree, destroy)
    Tree           tree;
    ItemDestructor destroy;
{
    if (tree == NULL)
	return -1;

    tree -> destroy = destroy;
    return 0;
}


int TreeSetDuplicator (tree, copy)
    Tree           tree;
    ItemDuplicator copy;
{
    if (tree == NULL)
	return -1;

    tree -> copy = copy;
    return 0;
}
