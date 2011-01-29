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

#define LUA_ENUM(L, name, val) \
    lua_pushlstring(L, #name, sizeof(#name)-1); \
    lua_pushnumber(L, val);                     \
    lua_settable(L, -3);                        \
    lua_pushnumber(L, val);                     \
    lua_pushlstring(L, #name, sizeof(#name)-1); \
    lua_settable(L, -3);

//----------------------------------------------------------------------!

template<>
std::string tl_metaname<double>()
{
    return "FElt.double";
}

//----------------------------------------------------------------------!
// Nodes

template<>
std::string tl_metaname<Node>()
{
    return "FElt.Node";
};

static int Node_tostring (lua_State *L)
{
    Node im = tl_check<Node>(L, 1);
    lua_pushfstring(L, "Node %d @ [%f, %f, %f]",
                    im->number, im->x, im->y, im->z);
    return 1;
}

static int Node_get_dx(lua_State *L)
{
    Node nn = tl_check<Node>(L, 1);
    double *dx = nn->dx;
    tl_pushn<double>(L, dx+1, 6);
    return 1;
}

#define GETTER(typ, field) tl_getter<Node, typ, offsetof(struct node, field)>

static const luaL_reg Node_meta[] = {
    { "__tostring", Node_tostring },
    { "__index", tl_index_wprop<Node> },
    { "__newindex", tl_newindex_wprop<Node> },
    { "get_dx", Node_get_dx },
    { "get_number", GETTER(unsigned, number) },
    { "get_x", GETTER(double, x) },
    { "get_y", GETTER(double, y) },
    { "get_z", GETTER(double, z) },
    { "get_m", GETTER(double, m) },
    { "get_constraint", GETTER(Constraint, constraint) },
    { "get_force", GETTER(Force, force) },
    {0, 0}
};

#undef GETTER

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
    tl_pushn<Node>(L, problem.nodes+1, problem.num_nodes);
    return 1;
}

static int
pelements(lua_State *L)
{
    tl_pushn<Element>(L, problem.elements+1, problem.num_elements);
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

// Elements

template<>
std::string tl_metaname<Element>()
{
    return "FElt.Element";
}

static int Element_get_nodes(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    tl_pushn<Node>(L, ee->node+1, ee->definition->numnodes);
    return 1;
}

static int Element_tostring(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    lua_pushfstring(L, "[Element %d @ %p]", ee->number, ee);
    return 1;
}

static int Element_get_distributed(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    if (0 == ee->numdistributed)
        return 0;
    Distributed *dd = ee->distributed;
    tl_pushn<Distributed>(L, dd+1, ee->numdistributed);
    return 1;
}

#define GETTER(typ, field) tl_getter<Element, typ, offsetof(struct element, field)>

static const luaL_reg Element_meta[] = {
    { "__tostring", Element_tostring },
    { "__index", tl_index_wprop<Element> },
    { "get_number", GETTER(unsigned, number) },
    { "get_nodes", Element_get_nodes },
    { "get_distributed", Element_get_distributed },
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

#undef GETTER
        
//----------------------------------------------------------------------!

// VarExpr arrays


//typedef VarExpr* VarExprPtr;

template<>
std::string tl_metaname<VarExpr>()
{
    return "FElt.VarExpr";
}

template<>
int tl_array_index<VarExpr>(lua_State *L)
{
    size_t size;
    VarExpr *data = tl_checkn<VarExpr>(L, 1, size);
    int idx = luaL_checknumber(L, 2);
    if (idx > size || idx <= 0) {
        lua_pushnil(L);
        return 1;
    }
    if (data[idx-1].expr)
        lua_pushstring(L, data[idx-1].text);
    else
        lua_pushnumber(L, data[idx-1].value);
    return 1;
}

//----------------------------------------------------------------------!

// Forces

template<>
std::string tl_metaname<Force>()
{
    return "FElt.Force";
}

static int Force_tostring(lua_State *L)
{
    Force ff = tl_check<Force>(L, 1);
    lua_pushfstring(L, "[Force %s @ %p]", ff->name, ff);
    return 1;
}

static int Force_get_force(lua_State *L)
{
    Force ff = tl_check<Force>(L, 1);
    VarExpr *dd = ff->force;
    tl_pushn<VarExpr>(L, dd+1, 6);
    return 1;
}

static int Force_get_spectrum(lua_State *L)
{
    Force ff = tl_check<Force>(L, 1);
    VarExpr *ve = ff->spectrum;
    tl_pushn<VarExpr>(L, ve+1, 6);
    return 1;
}

static const luaL_reg Force_meta[] = {
    { "__tostring", Force_tostring },
    { "__index", tl_index_wprop<Force> },
    { "get_force", Force_get_force },
    { "get_spectrum", Force_get_spectrum },
    { 0, 0 }
};

//----------------------------------------------------------------------!

// Constraints

template<>
std::string tl_metaname<Constraint>()
{
    return "FElt.Constraint";
}

static int Constraint_tostring(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    lua_pushfstring(L, "[Constraint %s @ %p]", cc->name, cc);
    return 1;
}

static int Constraint_get_constraint(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    char *pp = cc->constraint;
    tl_pushn<char>(L, pp+1, 6);
    return 1;
}

static int Constraint_get_ix(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    double *pp = cc->ix;
    tl_pushn<double>(L, pp+1, 6);
    return 1;
}

static int Constraint_get_vx(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    double *pp = cc->vx;
    tl_pushn<double>(L, pp+1, 3);
    return 1;
}

static int Constraint_get_ax(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    double *pp = cc->ax;
    tl_pushn<double>(L, pp+1, 3);
    return 1;
}

static int Constraint_get_dx(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    VarExpr *pp = cc->dx;
    tl_pushn<VarExpr>(L, pp+1, 6);
    return 1;
}

static const luaL_reg Constraint_meta[] = {
    { "__tostring", Force_tostring },
    { "__index", tl_index_wprop<Force> },
    { "get_constraint", Constraint_get_constraint },
    { "get_ix", Constraint_get_ix },
    { "get_vx", Constraint_get_vx },
    { "get_ax", Constraint_get_ax },
    { "get_dx", Constraint_get_dx },
    { 0, 0 }
};

//----------------------------------------------------------------------!

// Distributed

static int get_direction(lua_State *L, int argno)
{
    lua_pushliteral(L, "Direction");
    lua_gettable(L, LUA_ENVIRONINDEX);

    lua_pushvalue(L, argno);
    lua_gettable(L, -2);
    int val = lua_tonumber(L, -1);
    if (lua_isnil(L, -1)) {
        lua_pop(L, 1);
        luaL_typerror(L, argno, "Direction");
    }
    return (Direction) val;
}

template<> void tl_push<Direction>(lua_State *L, Direction dd)
{
    lua_pushliteral(L, "Direction");
    lua_gettable(L, LUA_ENVIRONINDEX);
    lua_pushinteger(L, (int) dd);
    lua_gettable(L, -2);
}

template<> std::string tl_metaname<Distributed>()
{
    return "FElt.Distributed";
}

static int Distributed_tostring(lua_State *L)
{
    Distributed dd = tl_check<Distributed>(L, 1);
    lua_pushfstring(L, "[Distributed %s @ %p]", dd->name, dd);
    return 1;
}

static int Distributed_values_iter(lua_State *L)
{
    int nvalues = luaL_checkint(L, lua_upvalueindex(1));
    int i = luaL_checkint(L, lua_upvalueindex(2));
    
    Distributed dd = (Distributed) lua_touserdata(L, 1);
    if (i > nvalues) {
        lua_pushnil(L);
        return 1;
    }

    lua_pushinteger(L, dd->value[i].node);
    lua_pushnumber(L, dd->value[i].magnitude);

    i++;
    lua_pushinteger(L, i);
    lua_replace(L, lua_upvalueindex(2));

    return 2;
}

static int Distributed_values(lua_State *L)
{
    Distributed dd = tl_check<Distributed>(L, 1);

    // push upvalues
    lua_pushinteger(L, dd->nvalues);
    lua_pushinteger(L, 1);
    
    // push closure
    lua_pushcclosure(L, &Distributed_values_iter, 2);
    
    // push state
    lua_pushlightuserdata(L, dd);
    
    // push control var
    lua_pushboolean(L, 1);
    
    return 3;
}

#define GETTER(typ, field) tl_getter<Distributed, typ, offsetof(struct distributed, field)>

static const luaL_reg Distributed_meta[] = {
    { "__tostring", Distributed_tostring },
    { "__index", tl_index_wprop<Distributed> },
    { "values", Distributed_values },
    { "get_direction",  GETTER(Direction, direction) },
    { 0, 0 }
};

#undef GETTER

//----------------------------------------------------------------------!

static int
register_funs(lua_State *L, const char *tbl)
{
    add_all_definitions ( );

    // setup environment
    lua_newtable(L);

    // setup Direction enum
    lua_pushliteral(L, "Direction");
    lua_newtable(L);
    {
        int i = 1;
        LUA_ENUM(L, LocalX, i); i++;
        LUA_ENUM(L, LocalY, i); i++;
        LUA_ENUM(L, LocalZ, i); i++;
        LUA_ENUM(L, GlobalX, i); i++;
        LUA_ENUM(L, GlobalY, i); i++;
        LUA_ENUM(L, GlobalZ, i); i++;
        LUA_ENUM(L, Parallel, i); i++;
        LUA_ENUM(L, Perpendicular, i); i++;
        LUA_ENUM(L, Radial, i); i++;
        LUA_ENUM(L, Axial, i); i++;
    }
    lua_settable(L, -3);
    
    lua_replace(L, LUA_ENVIRONINDEX);
    
    tl_setup<Node>(L, Node_meta, NULL);

    tl_setup<Material>(L, Material_meta, NULL);
    
    tl_setup<Element>(L, Element_meta, NULL);

    tl_setup<Force>(L, Force_meta, NULL);

    tl_setup<Constraint>(L, Constraint_meta, NULL);

    tl_setup<Distributed>(L, Distributed_meta, NULL);
    
    tl_array_wrapper<Node>().registerm(L);

    tl_array_wrapper<Element>().registerm(L);
    
    tl_array_wrapper<double>().registerm(L);

    tl_array_wrapper<VarExpr>().registerm(L);

    tl_array_wrapper<Distributed>().registerm(L);
    
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
