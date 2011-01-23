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

// Node utilities

#define NODE "FElt.Node"

static Node toNode(lua_State *L, int index)
{
    Node *nn = (Node *) lua_touserdata(L, index);
    if (nn == NULL) 
        luaL_typerror(L, index, NODE);
    return *nn;
}

static Node checkNode (lua_State *L, int index)
{
    luaL_checktype(L, index, LUA_TUSERDATA);
    Node *pi = (Node *) luaL_checkudata(L, index, NODE);
    if (pi == NULL)
        luaL_typerror(L, index, NODE);
    Node im = *pi;
    if (!im)
        luaL_error(L, "null Node");
    return im;
}

static Node* pushNode (lua_State *L, Node im)
{
    Node *pi = (Node *) lua_newuserdata(L, sizeof(Node));
    *pi = im;
    luaL_getmetatable(L, NODE);
    lua_setmetatable(L, -2);
    return pi;
}

//----------------------------------------------------------------------!

// Node ctor + dtor

static int Node_new(lua_State *L)
{
    int num = luaL_checkint(L, 1);
    pushNode(L, CreateNode(num));
    return 1;
}

static int Node_gc(lua_State *L)
{
    Node nd = toNode(L, 1);
    DestroyNode(nd);
    return 0;
}

//----------------------------------------------------------------------!

// Node accessors

static int Node_get_number(lua_State *L)
{
    Node nd = checkNode(L, 1);
    lua_pushnumber(L, nd->number);
    return 1;
}

static int Node_set_number(lua_State *L)
{
    Node nd = checkNode(L, 1);
    int num = luaL_checkint(L, 2);
    nd->number = num;
    return 0;
}

static int Node_get_x(lua_State *L)
{
    Node nd = checkNode(L, 1);
    lua_pushnumber(L, nd->x);
    return 1;
}

static int Node_get_y(lua_State *L)
{
    Node nd = checkNode(L, 1);
    lua_pushnumber(L, nd->y);
    return 1;
}

static int Node_get_z(lua_State *L)
{
    Node nd = checkNode(L, 1);
    lua_pushnumber(L, nd->z);
    return 1;
}

static int Node_get_m(lua_State *L)
{
    Node nd = checkNode(L, 1);
    lua_pushnumber(L, nd->m);
    return 1;
}

//----------------------------------------------------------------------!

// Node meta functions

static int Node_tostring (lua_State *L)
{
    Node im = toNode(L, 1);
    lua_pushfstring(L, "Node %d @ [%f, %f, %f]",
                    im->number, im->x, im->y, im->z);
    return 1;
}

static int Node_idx(lua_State *L)
{
    // check if key actually exists in metatable
    luaL_getmetatable(L, NODE);
    lua_pushvalue(L, 2);
    lua_rawget(L, -2);
    if (!lua_isnil(L, -1))
        return 1;
    lua_pop(L, 1);

    // check if getter exists in metatable
    const char *key = luaL_checkstring(L, 2);
    lua_pushfstring(L, "get_%s", key);
    lua_rawget(L, -2);
    if (!lua_isnil(L, -1)) {
        lua_pushvalue(L, 1);
        lua_call(L, 1, 1);
        return 1;
    }

    // nothing found
    lua_pushnil(L);
    return 1;
}

static int Node_newidx(lua_State *L)
{
    // check if key actually exists in metatable
    luaL_getmetatable(L, NODE);
    lua_pushvalue(L, 2);
    lua_rawget(L, -2);
    if (!lua_isnil(L, -1))
        return 1;
    lua_pop(L, 1);
    
    // check if setter exists in metatable
    const char *key = luaL_checkstring(L, 2);
    lua_pushfstring(L, "set_%s", key);
    lua_rawget(L, -2);
    if (!lua_isnil(L, -1)) {
        lua_pushvalue(L, 1);
        lua_pushvalue(L, 3);
        lua_call(L, 2, 0);
        return 0;
    }
    
    // nothing found, signal an error
    luaL_error(L, "%s: property not found", key);
    return 0;
}

//----------------------------------------------------------------------!

// Node methods struct

static const luaL_reg Node_meta[] = {
    { "__gc",      Node_gc },
    { "__tostring", Node_tostring },
    { "__index", Node_idx },
    { "__newindex", Node_newidx},
    { "get_number", Node_get_number},
    { "set_number", Node_set_number},
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
    pushNode(L, ptr->data[idx]);
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

static const struct luaL_reg felt_reg[] = {
    {"version", version},
    {"felt", felt},
    {"pnodes", pnodes},
    {"Node", Node_new},
    {NULL, NULL} /* sentinel */
};

//----------------------------------------------------------------------!

static int
register_funs(lua_State *L, const char *tbl)
{
    add_all_definitions ( );

    // nodes methods
    luaL_newmetatable(L, NODE);
    lua_pushliteral(L, "__index");
    lua_pushvalue(L, -2);
    lua_rawset(L, -3);
    luaL_register(L, NULL, Node_meta);

    // node array methods
    luaL_newmetatable(L, NODEARRAY);
    luaL_register(L, NULL, NodeArray_meta);

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
