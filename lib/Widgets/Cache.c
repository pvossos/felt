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
 * File:	Cache.c							*
 *									*
 * Description:	This file contains the private functions definitions	*
 *		for the	caching mechanism of the Drawing widget.	*
 ************************************************************************/

# include <stdio.h>
# include <X11/IntrinsicP.h>
# include "Cache.h"

# define CacheSize (1 << 4)
# define CacheMask (CacheSize - 1)

# define RemoveUnreferenced 0


/************************************************************************
   Function:	StringHash
   Description:	Returns a hash value for a string.
 ************************************************************************/

static unsigned StringHash (const char *s)
{
    unsigned g;
    unsigned h;
    size_t ln = strlen(s);
    size_t i = 0;

    for (h = 0; i < ln; i++)
	if ((g = (h = (h << 4) + s[i]) & 0xf0000000))
	    h ^= (g >> 24) ^ g;

    return h;
}


/************************************************************************
   Function:	DW_CacheCreate
   Description:	Creates, initialize, and returns a new cache.
 ************************************************************************/

Cache DW_CacheCreate (void)
{
    Cache    cache;
    unsigned i;


    cache = (Cache) XtMalloc (sizeof (struct cache_data) * CacheSize);

    for (i = 0; i < CacheSize; i ++)
	cache [i].next = NULL;

    return cache;
}


/************************************************************************
   Function:	DW_CacheDestroy
   Description:	Destroys a cache.
 ************************************************************************/

void DW_CacheDestroy (Cache cache)
{
    unsigned  i;
    CacheData ptr;
    CacheData last;

    for (i = 0; i < CacheSize; i ++) {
	last = NULL;
	for (ptr = cache [i].next; ptr; last = ptr, ptr = ptr -> next)
	    XtFree ((char *) last);
	XtFree ((char *) last);
    }

    XtFree ((char *) cache);
}


/************************************************************************
   Function:	DW_CacheLookup
   Description:	Looks up a named value in a cache.
 ************************************************************************/

CacheData DW_CacheLookup (Cache cache, String name)
{
    unsigned  idx;
    CacheData ptr;


    idx = StringHash (name) & CacheMask;
    for (ptr = cache [idx].next; ptr; ptr = ptr -> next)
	if (!strcmp (ptr -> name, name))
	    return ptr;

    return NULL;
}


/************************************************************************
   Function:	DW_CacheInsert
   Description:	Inserts a named value in a cache.
 ************************************************************************/

CacheData DW_CacheInsert (Cache cache, String name, XtArgVal value)
{
    unsigned  idx;
    CacheData ptr;


    idx = StringHash (name) & CacheMask;
    ptr = XtNew (struct cache_data);

    ptr -> name = XtNewString (name);
    ptr -> value = value;
    ptr -> ref_count = 1;

    if ((ptr -> next = cache [idx].next))
	ptr -> next -> prev = ptr;
    ptr -> prev = &cache [idx];
    cache [idx].next = ptr;

    return ptr;
}


/************************************************************************
   Function:	DW_CacheAddRef
   Description:	Increments the reference count of a named value.
 ************************************************************************/

void DW_CacheAddRef (CacheData data)
{
    data -> ref_count ++;
}


/************************************************************************
   Function:	DW_CacheDelRef
   Description:	Decrements the reference count of a named value.
 ************************************************************************/

void DW_CacheDelRef (CacheData data)
{
    data -> ref_count --;

# if RemoveUnreferenced
    if (!data -> ref_count) {
	if (data -> next)
	    data -> next -> prev = data -> prev;
	data -> prev -> next = data -> next;
	XtFree ((char *) data -> name);
	XtFree ((char *) data);
    }
# endif
}
