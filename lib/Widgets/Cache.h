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
 * File:	Cache.h							*
 *									*
 * Description:	This file contains the private function declarations	*
 *		for the caching mechanism of the Drawing widget.	*
 ************************************************************************/

# ifndef _Cache_h
# define _Cache_h

typedef struct cache_data *CacheData, *Cache;

struct cache_data {
    CacheData prev;
    CacheData next;
    String    name;
    XtArgVal  value;
    unsigned  ref_count;
};


extern Cache DW_CacheCreate (
# if NeedFunctionPrototypes
    void
# endif
);


extern void DW_CacheDestroy (
# if NeedFunctionPrototypes
    Cache		/* cache */
# endif
);


extern CacheData DW_CacheLookup (
# if NeedFunctionPrototypes
    Cache,		/* cache */
    String		/* name  */
# endif
);


extern CacheData DW_CacheInsert (
# if NeedFunctionPrototypes
    Cache,		/* cache */
    String,		/* name  */
    XtArgVal		/* value */
# endif
);


extern void DW_CacheAddRef (
# if NeedFunctionPrototypes
    CacheData		/* data  */
# endif
);


extern void DW_CacheDelRef (
# if NeedFunctionPrototypes
    CacheData		/* data  */
# endif
);

# endif /* _Cache_h */
