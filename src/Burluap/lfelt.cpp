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
#include "templua.hpp"

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
std::string tl_metaname<Node>()
{
    return "FElt.Node";
};

// Node meta functions

static int Node_tostring (lua_State *L)
{
    Node im = tl_check<Node>(L, 1);
    lua_pushfstring(L, "Node %d @ [%f, %f, %f]",
                    im->number, im->x, im->y, im->z);
    return 1;
}

// Node methods struct

#define GETTER(typ, field) tl_getter<Node, typ, offsetof(struct node, field)>

static const luaL_reg Node_meta[] = {
    { "__tostring", Node_tostring },
    { "__index", tl_index_wprop<Node> },
    { "__newindex", tl_newindex_wprop<Node> },
    { "get_number", GETTER(unsigned, number) },
    { "get_x", GETTER(double, x) },
    { "get_y", GETTER(double, y) },
    { "get_z", GETTER(double, z) },
    { "get_m", GETTER(double, m) },
    {0, 0}
};

#undef GETTER

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
    tl_push(L, ptr->data[idx]);
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
std::string tl_metaname<Element>()
{
    return "FElt.Element";
}

static int Element_get_nodes(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    pushNodeArray(L, ee->node, ee->definition->numnodes);
    return 1;
}

// Element meta functions

static int Element_tostring(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    lua_pushfstring(L, "[Element %d @ %p]", ee->number, ee);
    return 1;
}

// Element methods struct

#define GETTER(typ, field) tl_getter<Element, typ, offsetof(struct element, field)>

static const luaL_reg Element_meta[] = {
    { "__tostring", Element_tostring },
    { "__index", tl_index_wprop<Element> },
    { "get_number", GETTER(unsigned, number) },
    { "get_nodes", Element_get_nodes },
    { "get_material", GETTER(Material, material) },
    { 0, 0 }
};

#undef GETTER

//----------------------------------------------------------------------!

// Materials

template<>
std::string tl_metaname<Material>()
{
    return "FElt.Material";
}

static int Material_tostring(lua_State *L)
{
    Material mt = tl_check<Material>(L, 1);
    lua_pushfstring(L, "[Material %s @ %p]", mt->name, mt);
    return 1;
}

#define GETTER(typ, field) tl_getter<Material, typ, offsetof(struct material, field)>

static const luaL_reg Material_meta[] = {
    { "__tostring", Material_tostring },
    { "__index", tl_index_wprop<Material> },
    { "get_E", GETTER(double, E) },
    { "get_Ix", GETTER(double, Ix) },
    { "get_Iy", GETTER(double, Iy) },
    { "get_Iz", GETTER(double, Iz) },
    { "get_A", GETTER(double, A) },
    { "get_J", GETTER(double, J) },
    { "get_G", GETTER(double, G) },
    { "get_t", GETTER(double, t) },
    { "get_rho", GETTER(double, rho) },
    { "get_nu", GETTER(double, nu) },
    { "get_kappa", GETTER(double, kappa) },
    { "get_Rk", GETTER(double, Rk) },
    { "get_Rm", GETTER(double, Rm) },        
    { "get_Kx", GETTER(double, Kx) },        
    { "get_Ky", GETTER(double, Ky) },        
    { "get_Kz", GETTER(double, Kz) },        
    { "get_c", GETTER(double, c) },
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
    tl_push<Element>(L, aa->data[idx]);
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

    tl_setup<Node>(L, Node_meta, NULL);

    tl_setup<Material>(L, Material_meta, NULL);
    
    // node array methods
    luaL_newmetatable(L, NODEARRAY);
    luaL_register(L, NULL, NodeArray_meta);

    tl_setup<Element>(L, Element_meta, NULL);

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
