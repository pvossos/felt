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
 * File:	Cache.h							*
 *									*
 * Description:	This file contains the private function declarations	*
 *		for the caching mechanism of the Drawing widget.	*
 ************************************************************************/

# ifndef _Cache_h
# define _Cache_h

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

typedef struct cache_data *CacheData, *Cache;

struct cache_data {
    CacheData prev;
    CacheData next;
    String    name;
    XtArgVal  value;
    unsigned  ref_count;
};

Cache DW_CacheCreate (void);

void DW_CacheDestroy (Cache cache);

CacheData DW_CacheLookup (Cache cache, String name);

CacheData DW_CacheInsert (Cache cache, String name, XtArgVal value);

void DW_CacheAddRef (CacheData data);

void DW_CacheDelRef (CacheData data);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _Cache_h */
