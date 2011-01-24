/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 2010, 2011 Panagiotis A. Vossos

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

#include <cstdlib>
#include "udwpaux.hpp"

extern "C" {
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

#include "config.h"
#include "problem.h"
#include "definition.h"
#include "objects.h"

//----------------------------------------------------------------------!

struct ElementArray 
{
    Element *data;
    unsigned size;
};

//----------------------------------------------------------------------!

static ElementArray* pushElementArray(lua_State *L, Element* data, unsigned size);

//----------------------------------------------------------------------!

// Node utilities

template<>
struct udwp<node> 
{ static std::string name() { return "FElt.Node"; } };

typedef udwpaux<node> wNode;

// Node accessors

static int Node_get_number(lua_State *L)
{
    Node nd = wNode::check(L, 1);
    lua_pushnumber(L, nd->number);
    return 1;
}

static int Node_set_number(lua_State *L)
{
    Node nd = wNode::check(L, 1);
    int num = luaL_checkint(L, 2);
    nd->number = num;
    return 0;
}

static int Node_get_x(lua_State *L)
{
    Node nd = wNode::check(L, 1);
    lua_pushnumber(L, nd->x);
    return 1;
}

static int Node_get_y(lua_State *L)
{
    Node nd = wNode::check(L, 1);
    lua_pushnumber(L, nd->y);
    return 1;
}

static int Node_get_z(lua_State *L)
{
    Node nd = wNode::check(L, 1);
    lua_pushnumber(L, nd->z);
    return 1;
}

static int Node_get_m(lua_State *L)
{
    Node nd = wNode::check(L, 1);
    lua_pushnumber(L, nd->m);
    return 1;
}

// Node meta functions

static int Node_tostring (lua_State *L)
{
    Node im = wNode::check(L, 1);
    lua_pushfstring(L, "Node %d @ [%f, %f, %f]",
                    im->number, im->x, im->y, im->z);
    return 1;
}

// Node methods struct

static const luaL_reg Node_meta[] = {
    { "__tostring", Node_tostring },
    { "__index", wNode::index_wprop },
    { "__newindex", wNode::newindex_wprop },
    { "get_number", Node_get_number },
    { "set_number", Node_set_number },
    { "get_x", Node_get_x },
    { "get_y", Node_get_y },
    { "get_z", Node_get_z },
    { "get_m", Node_get_m },
    {0, 0}
};

//----------------------------------------------------------------------!

// NodeArray utilities

#define NODEARRAY "FElt.NodeArray"

struct NodeArray {
    Node *data;
    unsigned size;
};

static NodeArray* toNodeArray(lua_State *L, int index)
{
    NodeArray *nn = (NodeArray *) lua_touserdata(L, index);
    if (nn == NULL) 
        luaL_typerror(L, index, NODEARRAY);
    return nn;
}

static NodeArray* checkNodeArray(lua_State *L, int index)
{
    luaL_checktype(L, index, LUA_TUSERDATA);
    NodeArray *nn = (NodeArray *) luaL_checkudata(L, index, NODEARRAY);
    if (nn == NULL) 
        luaL_typerror(L, index, NODEARRAY);
    return nn;
}

static NodeArray* pushNodeArray(lua_State *L, Node* data, unsigned size)
{
    NodeArray *nn = (NodeArray *) lua_newuserdata(L, sizeof(NodeArray));
    nn->data = data;
    nn->size = size;
    luaL_getmetatable(L, NODEARRAY);
    lua_setmetatable(L, -2);
    return nn;
}

//----------------------------------------------------------------------!

// NodeArray meta functions

static int NodeArray_idx(lua_State *L)
{
    NodeArray *ptr = checkNodeArray(L, 1);
    int idx = luaL_checknumber(L, 2);
    if (idx > ptr->size || idx <= 0) {
        lua_pushnil(L);
        return 1;
    }
    wNode::push(L, ptr->data[idx]);
    return 1;
}

static int NodeArray_size(lua_State *L)
{
    NodeArray *ptr = checkNodeArray(L, 1);
    lua_pushnumber(L, ptr->size);
    return 1;
}

static const luaL_reg NodeArray_meta[] = {
    { "__len", NodeArray_size },
    { "__index", NodeArray_idx },
    { 0, 0 }
};

//----------------------------------------------------------------------!

// various non-member functions

static int
version(lua_State *L)
{
    lua_pushfstring(L, "%d.%d.%d", FELT_VERSION_MAJOR, FELT_VERSION_MINOR, FELT_VERSION_MICRO);
    return 1;
}

static int
felt(lua_State *L)
{
    const char *inp = luaL_checkstring(L, 1);
    int ret = ReadFeltFile(inp);
    lua_pushinteger(L, ret);
}

static int
pnodes(lua_State *L)
{
    pushNodeArray(L, problem.nodes, problem.num_nodes);
    return 1;
}

static int
pelements(lua_State *L)
{
    pushElementArray(L, problem.elements, problem.num_elements);
    return 1;
}

static const struct luaL_reg felt_reg[] = {
    {"version", version},
    {"felt", felt},
    {"pnodes", pnodes},
    {"pelements", pelements},

    {NULL, NULL} /* sentinel */
};

//----------------------------------------------------------------------!

// Element utilities

template<>
struct udwp<element> 
{ static std::string name() { return "FElt.Element"; } };

typedef udwpaux<element> wElement;

// Element accessors

static int Element_get_number(lua_State *L)
{
    Element ee = wElement::check(L, 1);
    lua_pushnumber(L, ee->number);
    return 1;
}

static int Element_get_nodes(lua_State *L)
{
    Element ee = wElement::check(L, 1);
    pushNodeArray(L, ee->node, ee->definition->numnodes);
    return 1;
}

// Element meta functions

static int Element_tostring(lua_State *L)
{
    Element ee = wElement::check(L, 1);
    lua_pushfstring(L, "[Element %d @ %p]", ee->number, ee);
    return 1;
}

// Element methods struct

static const luaL_reg Element_meta[] = {
    { "__tostring", Element_tostring },
    { "__index", wElement::index_wprop },
    { "get_number", Element_get_number },
    { "get_nodes", Element_get_nodes },
    { 0, 0 }
};

//----------------------------------------------------------------------!

// ElementArray utilities

#define ELEMENTARRAY "FElt.ElementArray"

static ElementArray* toElementArray(lua_State *L, int index)
{
    ElementArray *aa = (ElementArray *) lua_touserdata(L, index);
    if (aa == NULL)
        luaL_typerror(L, index, ELEMENTARRAY);
    return aa;
}

static ElementArray* checkElementArray(lua_State *L, int index)
{
    luaL_checktype(L, index, LUA_TUSERDATA);
    ElementArray *ee = (ElementArray *) luaL_checkudata(L, index, ELEMENTARRAY);
    if (ee == NULL) 
        luaL_typerror(L, index, ELEMENTARRAY);
    return ee;
}

static ElementArray* pushElementArray(lua_State *L, Element* data, unsigned size)
{
    ElementArray *aa = (ElementArray *) lua_newuserdata(L, sizeof(ElementArray));
    aa->data = data;
    aa->size = size;
    luaL_getmetatable(L, ELEMENTARRAY);
    lua_setmetatable(L, -2);
    return aa;
}

//----------------------------------------------------------------------!

// ElementArray meta functions

static int ElementArray_idx(lua_State *L)
{
    ElementArray *aa = checkElementArray(L, 1);
    int idx = luaL_checknumber(L, 2);
    if (idx > aa->size || idx <= 0) {
        lua_pushnil(L);
        return 1;
    }
    wElement::push(L, aa->data[idx]);
    return 1;
}

static int ElementArray_size(lua_State *L)
{
    ElementArray *ptr = checkElementArray(L, 1);
    lua_pushnumber(L, ptr->size);
    return 1;
}

static const luaL_reg ElementArray_meta[] = {
    { "__len", ElementArray_size },
    { "__index", ElementArray_idx },
    { 0 , 0 }
};

//----------------------------------------------------------------------!

static int
register_funs(lua_State *L, const char *tbl)
{
    add_all_definitions ( );

    wNode::setup(L, Node_meta, NULL);

    // node array methods
    luaL_newmetatable(L, NODEARRAY);
    luaL_register(L, NULL, NodeArray_meta);

    wElement::setup(L, Element_meta, NULL);

    // element array methods
    luaL_newmetatable(L, ELEMENTARRAY);
    luaL_register(L, NULL, ElementArray_meta);
    
    // non-member functions
    luaL_register(L, tbl, felt_reg);

    return 1;
}

extern "C" int
luaopen_felt_g(lua_State *L)
{
    return register_funs(L, "_G");
}

extern "C" int
luaopen_felt(lua_State *L)
{
    return register_funs(L, "felt");
}

extern "C"
void Fatal (const char *format, ...)
{
    va_list ap;
    va_start (ap, format);
    fprintf (stderr, "burlap: ");
    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    va_end (ap);
    exit (1);
}
