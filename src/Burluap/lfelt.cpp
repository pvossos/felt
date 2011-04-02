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
#include <cstring>
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
#include "transient.hpp"
#include "renumber.hpp"

//----------------------------------------------------------------------!

void SetupStressMemory(Element element);
double ElementLength(Element element, unsigned int coords);
double ElementArea(Element e, unsigned int n);

//----------------------------------------------------------------------!

#define LUA_ENUM(L, name, val) \
    lua_pushlstring(L, #name, sizeof(#name)-1); \
    lua_pushnumber(L, val);                     \
    lua_settable(L, -3);                        \
    lua_pushnumber(L, val);                     \
    lua_pushlstring(L, #name, sizeof(#name)-1); \
    lua_settable(L, -3);

//----------------------------------------------------------------------!

template<> std::string tl_metaname<double>()
{
    return "double";
}

template<> std::string tl_metaname<unsigned>()
{
    return "unsigned";
}

//----------------------------------------------------------------------!

// Nodes

template<> std::string tl_metaname<Node>() 
{
    return "FElt.Node";
};

template<> int tl_tostring<Node>(lua_State *L)
{
    Node im = tl_check<Node>(L, 1);
    lua_pushfstring(L, "Node %d @ [%f, %f, %f]",
                    im->number, im->x, im->y, im->z);
    return 1;
}

static int Node_get_eq_force(lua_State *L)
{
    Node nn = tl_check<Node>(L, 1);
    tl_pushn<double>(L, nn->stress.c_ptr(), nn->stress.size());
    return 1;
}

static int Node_get_dx(lua_State *L)
{
    Node nn = tl_check<Node>(L, 1);
    tl_pushn<double>(L, nn->dx+1, 6);
    return 1;
}

static int Node_new(lua_State *L)
{
    unsigned num = luaL_checkint(L, 1);
    Node nn(new node_t(num));
    tl_push<Node>(L, nn);
    return 1;
}

#define GETTER(typ, field) tl_getter_shared<node_t, typ, &node_t::field>
#define GETSET(typ, field) tl_getset_shared<node_t, typ, &node_t::field>()

static void
register_Nodes(lua_State *L)
{
    tl_wrapper<Node> w(true);
    w.prop("dx", Node_get_dx);
    w.prop("number", GETTER(unsigned, number));
    w.prop("x", GETSET(double, x));
    w.prop("y", GETSET(double, y));
    w.prop("z", GETSET(double, z));
    w.prop("m", GETSET(double, m));
    w.prop("constraint", GETTER(Constraint, constraint));
    w.prop("force", GETTER(Force, force));
    w.prop("eq_force", Node_get_eq_force);
    w.registerm(L);
}

#undef GETTER
#undef GETSET

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
    tl_pushn<Node>(L, problem.nodes.c_ptr(), problem.nodes.size());
    return 1;
}

static int
pelements(lua_State *L)
{
    tl_pushn<Element>(L, problem.elements.c_ptr(), problem.elements.size());
    return 1;
}

//----------------------------------------------------------------------!

// Stresses

template<> std::string tl_metaname<Stress>()
{
    return "FElt.Stress";
}

template<> int tl_tostring<Stress>(lua_State *L)
{
    Stress ss = tl_check<Stress>(L, 1);
    lua_pushfstring(L, "Stress @ [%f, %f, %f]",
                    ss->x, ss->y, ss->z);
    return 1;
}

static int Stress_len(lua_State *L)
{
    Stress ss = tl_check<Stress>(L, 1);
    lua_pushinteger(L, ss->values.size());
    return 1;
}

/*
static int Stress_get_values(lua_State *L)
{
    Stress ss = tl_check<Stress>(L, 1);
    tl_pushn<double>(L, ss->values.c_ptr(), ss->values.size());
    return 1;
}
*/

static int Stress_numeric_index(lua_State *L)
{
    Stress ss = tl_check<Stress>(L, 1);
    unsigned ii = tl_check<unsigned>(L, 2);
    if (!(ii >= 1 && ii <= ss->values.size()))
        return 0;
    tl_push(L, ss->values[ii]);
    return 1;
}

#define GETSET(typ, field) tl_getset<stress, typ, &stress::field>()

void register_Stresses(lua_State *L)
{
    tl_wrapper<Stress> w(true);
    w.prop("x", GETSET(double, x));
    w.prop("y", GETSET(double, y));
    w.prop("z", GETSET(double, z));
    w["__len"] = Stress_len;
    w["__numeric_index"] = Stress_numeric_index;
    //w.prop("values", Stress_get_values);
    w.registerm(L);
}

#undef GETSET

//----------------------------------------------------------------------!

// Elements

template<> std::string tl_metaname<Element>()
{
    return "FElt.Element";
}

template<> int tl_tostring<Element>(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    lua_pushfstring(L, "[Element %d]", ee->number);
    return 1;
}

static int Element_get_nodes(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    assert(ee->definition->numnodes == ee->node.size());
    tl_pushn<Node>(L, ee->node.c_ptr(), ee->node.size());
    return 1;
}

static int Element_get_ninteg(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    tl_push(L, ee->ninteg);
    return 1;
}

static int Element_set_ninteg(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    unsigned ninteg = tl_check<unsigned>(L, 2);
    ee->ninteg = ninteg;
    SetupStressMemory(ee);
    return 0;
}

static int Element_get_stresses(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    assert(ee->ninteg == ee->stress.size());
    tl_pushn<Stress>(L, ee->stress.c_ptr(), ee->stress.size());
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

static int Element_new(lua_State *L)
{
    unsigned num = luaL_checkint(L, 1);
    Element ee(new element_t(num));
    tl_push<Element>(L, ee);
    return 1;
}

static int Element_length(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    luaL_argcheck(L, ee->definition->shape == Linear, 1, 
                  "length of non-linear element is undefined");
    double val = ElementLength(ee, 3);
    lua_pushnumber(L, val);
    return 1;
}

static int Element_area(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    luaL_argcheck(L, ee->definition->shape == Planar, 1,
                  "area of non-planar element is undefined");
    double val = ElementArea(ee, ee->definition->shapenodes);
    lua_pushnumber(L, val);
    return 1;
}

#define GETTER(typ, field) tl_getter_shared<element_t, typ, &element_t::field>
#define GETSET(typ, field) tl_getset_shared<element_t, typ, &element_t::field>()

void register_Elements(lua_State *L)
{
    tl_wrapper<Element> w(true);
    w.prop("number", GETTER(unsigned, number));
    w.prop("nodes", Element_get_nodes);
    w.prop("K", GETSET(Matrix, K));
    w.prop("M", GETSET(Matrix, M));
    w.prop("ninteg", Element_get_ninteg, Element_set_ninteg);
    w.prop("distributed", Element_get_distributed);
    w.prop("stresses", Element_get_stresses);
    w.prop("material", GETTER(Material, material));
    w.prop("definition", GETTER(Definition, definition));
    w.prop("length", Element_length);
    w.prop("area", Element_area);
    w.registerm(L);
}

#undef GETTER
#undef GETSET

//----------------------------------------------------------------------!

// Materials

template<>
std::string tl_metaname<Material>()
{
    return "FElt.Material";
}

template<>
int tl_tostring<Material>(lua_State *L)
{
    Material mt = tl_check<Material>(L, 1);
    lua_pushfstring(L, "[Material %s]", mt->name.c_str());
    return 1;
}

#define GETTER(typ, field) tl_getter_shared<material_t, typ, &material_t::field>

static void
register_Materials(lua_State *L)
{
    tl_wrapper<Material> w(true);
    w.prop("E", GETTER(double, E));
    w.prop("Ix", GETTER(double, Ix));
    w.prop("Iy", GETTER(double, Iy));
    w.prop("Iz", GETTER(double, Iz));
    w.prop("A", GETTER(double, A));
    w.prop("J", GETTER(double, J));
    w.prop("G", GETTER(double, G));
    w.prop("t", GETTER(double, t));
    w.prop("rho", GETTER(double, rho));
    w.prop("nu", GETTER(double, nu));
    w.prop("kappa", GETTER(double, kappa));
    w.prop("Rk", GETTER(double, Rk));
    w.prop("Rm", GETTER(double, Rm));        
    w.prop("Kx", GETTER(double, Kx));        
    w.prop("Ky", GETTER(double, Ky));        
    w.prop("Kz", GETTER(double, Kz));        
    w.prop("c", GETTER(double, c));
    w.registerm(L);
}

#undef GETTER
        
//----------------------------------------------------------------------!

// VarExpr arrays

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

template<>
VarExpr tl_check<VarExpr>(lua_State *L, int index) 
{
    // FIXME
    VarExpr ve;
    return ve;
    /*
    const std::string name = tl_metaname<T>();
    luaL_checktype(L, index, LUA_TUSERDATA);
    T* v = (T*) luaL_checkudata(L, index, name.c_str());
    if (!v) luaL_typerror(L, index, name.c_str());
    T p = *v;
    if (!p) {
        char buf[100];
        std::sprintf(buf, "NULL %s", name.c_str());
        luaL_error(L, buf);
    }
    return p;
    */
}


//----------------------------------------------------------------------!

// Forces

template<>
std::string tl_metaname<Force>()
{
    return "FElt.Force";
}

template<>
int tl_tostring<Force>(lua_State *L)
{
    Force ff = tl_check<Force>(L, 1);
    lua_pushfstring(L, "[Force %s]", ff->name.c_str());
    return 1;
}

static int Force_get_force(lua_State *L)
{
    Force ff = tl_check<Force>(L, 1);
    tl_pushn<VarExpr>(L, ff->force+1, 6);
    return 1;
}

static int Force_get_spectrum(lua_State *L)
{
    Force ff = tl_check<Force>(L, 1);
    tl_pushn<VarExpr>(L, ff->spectrum+1, 6);
    return 1;
}

void register_Forces(lua_State *L)
{
    tl_wrapper<Force> w(true);
    w.prop("force", Force_get_force);
    w.prop("spectrum", Force_get_spectrum);
    w.registerm(L);
}

//----------------------------------------------------------------------!

// Constraints

template<>
std::string tl_metaname<Constraint>()
{
    return "FElt.Constraint";
}

template<>
int tl_tostring<Constraint>(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    lua_pushfstring(L, "[Constraint %s]", cc->name.c_str());
    return 1;
}

static int Constraint_get_constraint(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    tl_pushn<char>(L, cc->constraint+1, 6);
    return 1;
}

static int Constraint_get_ix(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    tl_pushn<double>(L, cc->ix+1, 3);
    return 1;
}

static int Constraint_get_vx(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    tl_pushn<double>(L, cc->vx+1, 3);
    return 1;
}

static int Constraint_get_ax(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    tl_pushn<double>(L, cc->ax+1, 3);
    return 1;
}

static int Constraint_get_dx(lua_State *L)
{
    Constraint cc = tl_check<Constraint>(L, 1);
    tl_pushn<VarExpr>(L, cc->dx+1, 3);
    return 1;
}

void register_Constraints(lua_State *L)
{
    tl_wrapper<Constraint> w(true);
    w.prop("constraint", Constraint_get_constraint);
    w.prop("ix", Constraint_get_ix);
    w.prop("vx", Constraint_get_vx);
    w.prop("ax", Constraint_get_ax);
    w.prop("dx", Constraint_get_dx);
    w.registerm(L);
};

//----------------------------------------------------------------------!

// Shape enum

template<> Shape tl_check<Shape>(lua_State *L, int argno)
{
    lua_pushliteral(L, "Shape");
    lua_gettable(L, LUA_ENVIRONINDEX);

    lua_pushvalue(L, argno);
    lua_gettable(L, -2);
    int val = lua_tonumber(L, -1);
    if (lua_isnil(L, -1)) {
        lua_pop(L, 1);
        luaL_typerror(L, argno, "Shape");
    }
    return (Shape) val;
}

template<> void tl_push<Shape>(lua_State *L, Shape dd)
{
    lua_pushliteral(L, "Shape");
    lua_gettable(L, LUA_ENVIRONINDEX);
    lua_pushinteger(L, (int) dd);
    lua_gettable(L, -2);
}

//----------------------------------------------------------------------!


// Direction enum

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

//----------------------------------------------------------------------!

// Distributed

template<> std::string tl_metaname<Distributed>()
{
    return "FElt.Distributed";
}

template<> int tl_tostring<Distributed>(lua_State *L)
{
    Distributed dd = tl_check<Distributed>(L, 1);
    lua_pushfstring(L, "[Distributed %s]", dd->name.c_str());
    return 1;
}

static int Distributed_values_iter(lua_State *L)
{
    int nvalues = luaL_checkint(L, lua_upvalueindex(1));
    int i = luaL_checkint(L, lua_upvalueindex(2));
    
    distributed_t *dd = (distributed_t *) lua_touserdata(L, 1);
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
    lua_pushinteger(L, dd->value.size());
    lua_pushinteger(L, 1);
    
    // push closure
    lua_pushcclosure(L, &Distributed_values_iter, 2);
    
    // push state
    lua_pushlightuserdata(L, dd.get());
    
    // push control var
    lua_pushboolean(L, 1);

    return 3;
}

#define GETTER(typ, field) tl_getter_shared<distributed_t, typ, &distributed_t::field>

void register_Distributed(lua_State *L)
{
    tl_wrapper<Distributed> w(true);
    w["values"] = Distributed_values;
    w.prop("direction", GETTER(Direction, direction));
    w.registerm(L);
};

#undef GETTER

//----------------------------------------------------------------------!

// Definitions

struct DefnUdata
{
    lua_State *L;
    int setup_ref;
    int stress_ref;
};

template<> std::string tl_metaname<Definition>()
{
    return "FElt.Definition";
}

template<> int tl_tostring<Definition>(lua_State *L)
{
    Definition dd = tl_check<Definition>(L, 1);
    lua_pushfstring(L, "[Definition %s]", dd->name.c_str());
    return 1;
}

static void push_mass_mode(lua_State *L, char mass_mode)
{
    if ('c' == mass_mode)
        lua_pushliteral(L, "consistent");
    else if ('l' == mass_mode)
        lua_pushliteral(L, "lumped");
    else {
        assert(0 == mass_mode);
        lua_pushnil(L);
    }
}

static int setup_callback(Element ee, char mass_mode, int tangent)
{
    // get user pointer
    DefnUdata *ud = (DefnUdata *) ee->definition->udata;
    lua_State *L = ud->L;

    // get function
    lua_getref(L, ud->setup_ref);
    
    // setup arguments
    tl_push<Element>(L, ee);
    push_mass_mode(L, mass_mode);
    lua_pushinteger(L, tangent);
    
    // call function + return
    lua_call(L, 3, 1);
    return lua_tointeger(L, -1);
}

static int stress_callback(Element ee)
{
    // get user pointer 
    DefnUdata *ud = (DefnUdata *) ee->definition->udata;
    lua_State *L = ud->L;
    
    // get function
    lua_getref(L, ud->stress_ref);
    
    // setup args
    tl_push<Element>(L, ee);
    
    // call function + return
    lua_call(L, 1, 1);
    return lua_tointeger(L, -1);
}

static int Definition_new(lua_State *L)
{
    const char *name = luaL_checkstring(L, 1);
    luaL_checktype(L, 2, LUA_TFUNCTION);
    luaL_checktype(L, 3, LUA_TFUNCTION);
    Shape shape = tl_check<Shape>(L, 4);
    int numnodes = luaL_checkint(L, 5);
    int shapenodes = luaL_checkint(L, 6);
    int numstresses = luaL_checkint(L, 7);
    luaL_checktype(L, 8, LUA_TTABLE);
    bool retainK = lua_toboolean(L, 9);
    
    Definition df(new definition_t(name));
    df->shape = shape;
    df->numnodes = numnodes;
    df->shapenodes = shapenodes;
    df->numstresses = numstresses;
    df->retainK = (unsigned) retainK;
    
    lua_pushvalue(L, 8);
    for (int i = 1; i <= 6; i++) {
        df->dofs[i] = 0;
        lua_pushinteger(L, i);
        lua_gettable(L, -2);
        if (!lua_isnil(L, -1))
            df->dofs[i] = lua_tointeger(L, -1);
        lua_pop(L, 1);
    }
    
    // setup udata & callbacks

    // ATTN: why not call new here? because this is stored as a void*
    // in definition struct, and delete wouldn't know how to
    // deallocate it. free otoh, works ok.
    DefnUdata *du = (DefnUdata *) calloc(1, sizeof(DefnUdata));
    du->L = L;
    lua_pushvalue(L, 2);
    du->setup_ref = luaL_ref(L, LUA_REGISTRYINDEX);
    lua_pushvalue(L, 3);
    du->stress_ref = luaL_ref(L, LUA_REGISTRYINDEX);
    df->setup = setup_callback;
    df->stress = stress_callback;
    df->udata = (void *) du;

    tl_push<Definition>(L, df);
    return 1;
}

template<> int tl_gc<Definition>(lua_State *L)
{
    Definition dd = tl_check<Definition>(L, 1);

    // if it was created in lua, it will have the following pointer,
    // and we need to cleanup.
    DefnUdata *du = (DefnUdata *) dd->udata;
    if (du) {
        luaL_unref(L, LUA_REGISTRYINDEX, du->setup_ref);
        luaL_unref(L, LUA_REGISTRYINDEX, du->stress_ref);
    }

    // continue with normal gc code
    delete tl_unboxpointer<Definition*>(L, 1);
    return 0;
}

static int Definition_get_dofs(lua_State *L)
{
    Definition dd = tl_check<Definition>(L, 1);
    tl_pushn<unsigned>(L, dd->dofs+1, dd->numdofs);
    return 1;
}

#define GETTER(typ, field) tl_getter_shared<definition_t, typ, &definition_t::field>
#define GETSET(typ, field) tl_getset_shared<definition_t, typ, &definition_t::field>()

void register_Definitions(lua_State *L)
{
    tl_wrapper<Definition> w(true);
    w.prop("num_nodes", GETSET(unsigned, numnodes));
    w.prop("shape_nodes", GETSET(unsigned, shapenodes));
    w.prop("num_stresses", GETSET(unsigned, numstresses));
    w.prop("retainK", GETSET(unsigned, retainK));
    w.prop("dofs", Definition_get_dofs);
    w.registerm(L);
}

#undef GETTER
#undef GETSET

//----------------------------------------------------------------------!

template<> void tl_push<AnalysisType>(lua_State *L, AnalysisType at)
{
    lua_pushliteral(L, "AnalysisType");
    lua_gettable(L, LUA_ENVIRONINDEX);
    lua_pushinteger(L, (int) at);
    lua_gettable(L, -2);
}

//----------------------------------------------------------------------!

// Analysis struct

typedef struct analysis* AnalysisPtr;

template<> std::string tl_metaname<AnalysisPtr>()
{
    return "FElt.Analysis";
}

template<> int tl_tostring<AnalysisPtr>(lua_State *L)
{
    AnalysisPtr ap = tl_check<AnalysisPtr>(L, 1);
    lua_pushfstring(L, "[Analysis @ %p]", ap);
    return 1;
}

static int AnalysisPtr_get_dofs(lua_State *L)
{
    AnalysisPtr ap = tl_check<AnalysisPtr>(L, 1);
    lua_newtable(L);
    for (size_t i = 1; i <= analysis.numdofs; i++) {
        tl_push(L, i);
        tl_push<int>(L, analysis.dofs[i]);
        lua_settable(L, -3);
    }
    return 1;
}

static int AnalysisPtr_set_dofs(lua_State *L)
{
    AnalysisPtr ap = tl_check<AnalysisPtr>(L, 1);
    luaL_checktype(L, 2, LUA_TTABLE);
    lua_pushvalue(L, 2);

    analysis.numdofs = 0;
    for (size_t i = 1; i <= 6; i++) {
        lua_pushinteger(L, i);
        lua_gettable(L, -2);
        if (lua_isnil(L, -1))
            break;
        int dof = lua_tointeger(L, -1);
        analysis.dofs[i] = dof;
        analysis.numdofs++;
        lua_pop(L, 1);
    }
    return 0;
}

#define GETTER(typ, field) tl_getter<struct analysis, typ, &analysis::field>
#define GETSET(typ, field) tl_getset<struct analysis, typ, &analysis::field>()

void register_AnalysisPtr(lua_State *L)
{
    tl_wrapper<AnalysisPtr> w(true);
    w.prop("start", GETSET(double, start));
    w.prop("step", GETSET(double, step));
    w.prop("stop", GETSET(double, stop));
    w.prop("gamma", GETSET(double, gamma));
    w.prop("beta", GETSET(double, beta));
    w.prop("alpha", GETSET(double, alpha));
    w.prop("Rk", GETSET(double, Rk));
    w.prop("Rm", GETSET(double, Rm));
    w.prop("mass_mode", GETSET(char, mass_mode));
    w.prop("dofs", AnalysisPtr_get_dofs, AnalysisPtr_set_dofs);
    w.registerm(L);
};

#undef GETTER
#undef GETSET

//----------------------------------------------------------------------!

// Problem struct

typedef Problem* ProblemPtr;

template<> std::string tl_metaname<ProblemPtr>()
{
    return "FElt.Problem";
}

template<> int tl_tostring<ProblemPtr>(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);
    lua_pushfstring(L, "[Problem '%s' @ %p]", pp->title, pp);
    return 1;
}

static int ProblemPtr_get_nodes(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);    
    tl_pushn<Node>(L, pp->nodes.c_ptr(), pp->nodes.size());
    return 1;
}

static int ProblemPtr_get_elements(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);    
    tl_pushn<Element>(L, pp->elements.c_ptr(), pp->elements.size());
    return 1;
}

static int ProblemPtr_add_definition(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);
    Definition dd = tl_check<Definition>(L, 2);
    int irv = AddDefinition(dd);
    lua_pushboolean(L, irv);
    return 1;
}

static int ProblemPtr_remove_definition(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);
    Definition dd = tl_check<Definition>(L, 2);
    int irv = RemoveDefinition(dd);
    lua_pushboolean(L, irv);
    return 1;
}

static int ProblemPtr_lookup_definition(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);
    const char *name = luaL_checkstring(L, 2);
    Definition dd = LookupDefinition(name);
    if (!dd)
        return 0;
    tl_push<Definition>(L, dd);
    return 1;
}

static int ProblemPtr_get_mode(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);
    tl_push<AnalysisType>(L, pp->mode);
    return 1;
}

static int ProblemPtr_clear_nodes(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);
    ClearNodes();
    return 0;
}

#define GETTER(typ, field) tl_getter<Problem, typ, &Problem::field>

void register_ProblemPtr(lua_State *L)
{
    tl_wrapper<ProblemPtr> w(true);
    w.prop("mode", GETTER(AnalysisType, mode));
    w.prop("nodes", ProblemPtr_get_nodes);
    w.prop("elements", ProblemPtr_get_elements);
    w["add_definition"] = ProblemPtr_add_definition;
    w["remove_definition"] = ProblemPtr_remove_definition;
    w["lookup_definition"] = ProblemPtr_lookup_definition;
    w["clear_nodes"] = ProblemPtr_clear_nodes;
    w.registerm(L);
};

#undef GETTER

//----------------------------------------------------------------------!

// Matrices

template<> std::string tl_metaname<Matrix>()
{
    return "FElt.Matrix";
}

template<> int tl_tostring<Matrix>(lua_State *L)
{
    Matrix mt = tl_check<Matrix>(L, 1);
    lua_pushfstring(L, "Matrix %dx%d @ %p",
                    mt->nrows, mt->ncols, mt.get());
    return 1;
}

static int Matrix_new(lua_State *L)
{
    if (lua_istable(L, 1)) {
        // no explicit dimensions, construct from given table
        lua_pushvalue(L, 1);
        size_t nrows = lua_objlen(L, -1);
        size_t ncols = 0;
        Matrix mt;
        
        for (size_t i = 1; i <= nrows; i++) {
            lua_pushinteger(L, i);
            lua_gettable(L, -2);
            luaL_argcheck(L, lua_istable(L, -1), 1, "Invalid initializer expression");
            if (1 == i) {
                ncols = lua_objlen(L, -1);
                mt = CreateFullMatrix(nrows, ncols);
            } else {
                luaL_argcheck(L, ncols == lua_objlen(L, -1), 1, "Invalid initializer expression");
            }
            
            for (size_t j = 1; j <= ncols; j++) {
                lua_pushinteger(L, j);
                lua_gettable(L, -2);
                double val = lua_tonumber(L, -1);
                sdata(mt.get(), i, j) = val;

                // pop i,j value
                lua_pop(L, 1);
            }
            
            // pop row
            lua_pop(L, 1);
        }

        tl_push<Matrix>(L, mt);
        return 1;
        
    } else {
        // just construct from given dimensions, no init
        unsigned rows = luaL_checkint(L, 1);
        unsigned cols = luaL_checkint(L, 2);
        Matrix mt = CreateFullMatrix(rows, cols);
        tl_push<Matrix>(L, mt);
        return 1;
    }
}

// FIXME: maybe move the code in ref & set in Matrix_index2 &
// Matrix_newindex2 closures, since we don't really need two separate
// functions from the lua side.
static int Matrix_ref(lua_State *L)
{
    Matrix mt = tl_check<Matrix>(L, 1);
    unsigned ii = luaL_checkint(L, 2);
    luaL_argcheck(L, ii >= 1 && ii <= mt->nrows, 1, "Invalid row index");
    unsigned jj = luaL_checkint(L, 3);
    luaL_argcheck(L, jj >= 1 && jj <= mt->ncols, 2, "Invalid column index");

    double val = sdata(mt.get(), ii, jj);
    lua_pushnumber(L, val);
    return 1;
}

static int Matrix_set(lua_State *L)
{
    Matrix mt = tl_check<Matrix>(L, 1);
    unsigned ii = luaL_checkint(L, 2);
    luaL_argcheck(L, ii >= 1 && ii <= mt->nrows, 1, "Invalid row index");
    unsigned jj = luaL_checkint(L, 3);
    luaL_argcheck(L, jj >= 1 && jj <= mt->ncols, 2, "Invalid column index");

    double newval = luaL_checknumber(L, 4);
    printf("in set(%d, %d) = %f\n", ii, jj, newval);
    sdata(mt.get(), ii, jj) = newval;
    return 0;
}

static int Matrix_index2(lua_State *L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    unsigned jj = luaL_checkint(L, 2);
    
    // get original matrix
    lua_pushvalue(L, lua_upvalueindex(1));

    // get ref function
    lua_pushliteral(L, "ref");
    lua_gettable(L, -2);
    
    // setup argument 1 (the matrix itself)
    lua_pushvalue(L, -2);

    // setup arg 2, the row index
    lua_pushvalue(L, lua_upvalueindex(2));

    // setup arg 3, the column index
    lua_pushinteger(L, jj);
    
    // call and return
    lua_call(L, 3, 1);
    return 1;
}

static int Matrix_newindex2(lua_State *L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    unsigned jj = luaL_checkint(L, 2);
    double val = luaL_checknumber(L, 3);
    printf("val = %g\n", val);
    
    // get original matrix
    lua_pushvalue(L, lua_upvalueindex(1));

    // get ref function
    lua_pushliteral(L, "set");
    lua_gettable(L, -2);
    
    // setup argument 1 (the matrix itself)
    lua_pushvalue(L, -2);

    // setup arg 2, the row index
    lua_pushvalue(L, lua_upvalueindex(2));

    // setup arg 3, the column index
    lua_pushinteger(L, jj);
    
    // setup arg 4, the new value
    lua_pushvalue(L, 3);
    
    // call and return
    lua_call(L, 4, 0);
    return 0;
}

static int Matrix_index1(lua_State *L)
{
    Matrix mt = tl_check<Matrix>(L, 1);
    unsigned ii = luaL_checkint(L, 2);

    // create an empty table, so that we can hack its metatable
    lua_newtable(L);

    // create a metatable from scratch, and set its __index &
    // __newindex metamethods. 
    lua_newtable(L);
    lua_pushliteral(L, "__index");
    lua_pushvalue(L, 1);
    lua_pushvalue(L, 2);
    lua_pushcclosure(L, Matrix_index2, 2);
    lua_settable(L, -3);
    
    lua_pushliteral(L, "__newindex");
    lua_pushvalue(L, 1);
    lua_pushvalue(L, 2);
    lua_pushcclosure(L, Matrix_newindex2, 2);
    lua_settable(L, -3);
    
    // set metatable + return temp table.
    lua_setmetatable(L, -2);

    return 1;
}

static int Matrix_add(lua_State *L)
{
    Matrix ma = tl_check<Matrix>(L, 1);
    Matrix mb = tl_check<Matrix>(L, 2);
    Matrix mc = CreateFullMatrix(ma->nrows, ma->ncols);
    AddMatrices(mc, ma, mb);
    tl_push<Matrix>(L, mc);
    return 1;
}

static int Matrix_sub(lua_State *L)
{
    Matrix ma = tl_check<Matrix>(L, 1);
    Matrix mb = tl_check<Matrix>(L, 2);
    Matrix mc = CreateFullMatrix(ma->nrows, ma->ncols);
    SubtractMatrices(mc, ma, mb);
    tl_push<Matrix>(L, mc);
    return 1;
}

static int Matrix_mul(lua_State *L)
{
    Matrix ma = tl_check<Matrix>(L, 1);
    Matrix mb = tl_check<Matrix>(L, 2);
    Matrix mc = CreateFullMatrix(ma->nrows, mb->ncols);
    MultiplyMatrices(mc, ma, mb);
    tl_push<Matrix>(L, mc);
    return 1;
}

static int Matrix_transpose(lua_State *L)
{
    Matrix ma = tl_check<Matrix>(L, 1);
    Matrix mb = CreateFullMatrix(ma->ncols, ma->nrows);
    TransposeMatrix(mb, ma);
    tl_push<Matrix>(L, mb);
    return 1;
}

// FIXME: maybe merge in __tostring
static int Matrix_print(lua_State *L)
{
    Matrix m = tl_check<Matrix>(L, 1);
    PrintMatrix(m, NULL);
    return 0;
}

static int Matrix_chol(lua_State *L)
{
    Matrix a = tl_check<Matrix>(L, 1);
    Matrix b = CreateCopyMatrix(a);
    CholeskyFactorMatrix(b, a);
    tl_push(L, b);
    return 1;
}

#define GETTER(typ, field) tl_getter_shared<matrix, typ, &matrix::field>

static void
register_Matrices(lua_State *L)
{
    tl_wrapper<Matrix> w(true);
    w.prop("rows", GETTER(unsigned, nrows));
    w.prop("cols", GETTER(unsigned, ncols));
    w["ref"] = Matrix_ref;
    w["set"] = Matrix_set;
    w["print"] = Matrix_print;
    w["transpose"] = Matrix_transpose;
    w["__numeric_index"] = Matrix_index1;
    w["__add"] = Matrix_add;
    w["__sub"] = Matrix_sub;
    w["__mul"] = Matrix_mul;
    w["chol"] = Matrix_chol;
    w.registerm(L);
}

#undef GETTER

//----------------------------------------------------------------------!

// various free functions

static int renumber_nodes(lua_State *L)
{
    Node *node = problem.nodes.c_ptr1();
    Element *element = problem.elements.c_ptr1();
    unsigned numnodes = problem.nodes.size();
    unsigned numelts = problem.elements.size();
    unsigned *old_numbers = new unsigned[numnodes];
    size_t nret = RenumberNodes(node, element, numnodes, numelts, old_numbers-1);
    if (0 == nret)
        return 0;
    tl_pushn<unsigned>(L, old_numbers, nret, true);
    return 1;
}

static int restore_numbers(lua_State *L)
{
    size_t size;
    unsigned *data = tl_checkn<unsigned>(L, 1, size);
    RestoreNodeNumbers(problem.nodes.c_ptr1(), data-1, problem.nodes.size());
    return 0;
}

static int find_dofs(lua_State *L)
{
    int num_dofs = FindDOFS();
    tl_push(L, num_dofs);
    return 1;
}

static int construct_stiffness(lua_State *L)
{
    int status;
    Matrix K = ConstructStiffness(&status);
    tl_push(L, K);
    return 1;
}

static int construct_dynamic(lua_State *L)
{
    Matrix K, M, C;
    ConstructDynamic(&K, &M, &C);
    tl_push(L, K);
    tl_push(L, M);
    tl_push(L, C);
    return 3;
}

static int construct_forces(lua_State *L)
{
    Vector F = ConstructForceVector();
    tl_push(L, F);
    return 1;
}

static int zero_constrained(lua_State *L)
{
    Matrix k = tl_check<Matrix>(L, 1);
    Matrix f = tl_check<Matrix>(L, 2);
    Matrix kc = CreateCopyMatrix(k);
    Matrix fc = CreateCopyMatrix(f);
    // ATTN: the following funtions doesn't seem to work properly, so
    // we wrap the other one.
    //ZeroConstrainedMatrixDOF(mc, m);
    ZeroConstrainedDOF(k, f, &kc, &fc);
    tl_push<Matrix>(L, kc);
    tl_push<Matrix>(L, fc);
    return 2;
}

static int remove_constrained(lua_State *L)
{
    Matrix K = tl_check<Matrix>(L, 1);
    Matrix M = tl_check<Matrix>(L, 2);
    Matrix C = tl_check<Matrix>(L, 3);
    Matrix Kc, Mc, Cc;
    RemoveConstrainedDOF(K, M, C, Kc, Mc, Cc);
    tl_push<Matrix>(L, Kc);
    tl_push<Matrix>(L, Mc);
    tl_push<Matrix>(L, Cc);
    return 3;
}

static int solve_displacements(lua_State *L)
{
    Matrix K = tl_check<Matrix>(L, 1);
    Vector F = tl_check<Vector>(L, 2);
    Vector D = SolveForDisplacements(K, F);
    tl_push<Vector>(L, D);
    return 1;
}

static int compute_modes(lua_State *L)
{
    Matrix K = tl_check<Matrix>(L, 1);
    Matrix M = tl_check<Matrix>(L, 2);
    Matrix lambda_r, x_r;
    ComputeEigenModes(K, M, lambda_r, x_r);
    tl_push<Matrix>(L, lambda_r);
    tl_push<Matrix>(L, x_r);
    return 2;
}

static int normalize_by_first(lua_State *L)
{
    Matrix a = tl_check<Matrix>(L, 1);
    Matrix b = CreateCopyMatrix(a);
    NormalizeByFirst(b, a);
    tl_push<Matrix>(L, b);
    return 1;
}

static int form_modal(lua_State *L)
{
    Matrix u = tl_check<Matrix>(L, 1);
    Matrix Mc = tl_check<Matrix>(L, 2);
    Matrix Cc = tl_check<Matrix>(L, 3);
    Matrix Kc = tl_check<Matrix>(L, 4);
    bool orthop = tl_check<bool>(L, 5);
    Matrix Mm, Cm, Km;
    FormModalMatrices(u, Mc, Cc, Kc, Mm, Cm, Km, (int) orthop);
    tl_push(L, Mm);
    tl_push(L, Cm);
    tl_push(L, Km);
    return 3;
}

static int element_stresses(lua_State *L)
{
    ElementStresses();
    return 0;
}

static int integrate_hyperbolic(lua_State *L)
{
    Matrix K = tl_check<Matrix>(L, 1);
    Matrix M = tl_check<Matrix>(L, 2);
    Matrix C = tl_check<Matrix>(L, 3);
    
    Matrix res = IntegrateHyperbolicDE(K, M, C);
    tl_push<Matrix>(L, res);
    return 1;
}

static int integrate_parabolic(lua_State *L)
{
    Matrix K = tl_check<Matrix>(L, 1);
    Matrix M = tl_check<Matrix>(L, 2);

    Matrix res = IntegrateParabolicDE(K, M);
    tl_push<Matrix>(L, res);
    return 1;
}

//----------------------------------------------------------------------!

static const struct luaL_reg felt_reg[] = {
    {"version", version},
    {"felt", felt},
    {"pnodes", pnodes},
    {"pelements", pelements},
    {"Node", Node_new},
    {"Element", Element_new},
    {"Matrix", Matrix_new},

    {"find_dofs", find_dofs},
    {"construct_stiffness", construct_stiffness},
    {"construct_dynamic", construct_dynamic},
    {"construct_forces", construct_forces},
    {"zero_constrained", zero_constrained},
    {"remove_constrained", remove_constrained},
    {"solve_displacements", solve_displacements},
    {"compute_modes", compute_modes},
    {"normalize_by_first", normalize_by_first},
    {"form_modal", form_modal},
    {"element_stresses", element_stresses},
    {"integrate_hyperbolic", integrate_hyperbolic},
    {"integrate_parabolic", integrate_parabolic},
    {"renumber_nodes", renumber_nodes},
    {"restore_numbers", restore_numbers},
    
    {NULL, NULL} /* sentinel */
};

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

    // setup Shape enum
    lua_pushliteral(L, "Shape");
    lua_newtable(L);
    {
        int i = 1;
        LUA_ENUM(L, Linear, i); i++;
        LUA_ENUM(L, Planar, i); i++;
        LUA_ENUM(L, Solid, i); i++;
    }
    lua_settable(L, -3);
        
    // setup AnalysisType enum
    lua_pushliteral(L, "AnalysisType");
    lua_newtable(L);
    {
        int i = 1;
        LUA_ENUM(L, Static, i); i++;
        LUA_ENUM(L, Transient, i); i++;
        LUA_ENUM(L, Modal, i); i++;
        LUA_ENUM(L, StaticThermal, i); i++;
        LUA_ENUM(L, TransientThermal, i); i++;
        LUA_ENUM(L, Spectral, i); i++;
        LUA_ENUM(L, StaticSubstitution, i); i++;        
        LUA_ENUM(L, StaticIncremental, i); i++;        
        LUA_ENUM(L, StaticLoadCases, i); i++;        
        LUA_ENUM(L, StaticLoadRange, i); i++;        
        LUA_ENUM(L, StaticSubstitutionLoadRange, i); i++;        
        LUA_ENUM(L, StaticIncrementalLoadRange, i); i++;
    }
    lua_settable(L, -3);
        
    lua_replace(L, LUA_ENVIRONINDEX);

    //tl_setup<DefnUdata>(L, NULL, NULL);
    
    register_Definitions(L);

    register_Nodes(L);

    register_Materials(L);
    
    register_Elements(L);
    
    register_Stresses(L);

    register_Forces(L);
    
    register_Constraints(L);

    register_Distributed(L);
    
    register_AnalysisPtr(L);
    
    register_ProblemPtr(L);

    register_Matrices(L);
    
    tl_array_wrapper<unsigned>().registerm(L);
    
    tl_array_wrapper<Node>().registerm(L);

    tl_array_wrapper<Element>().registerm(L);
    
    tl_array_wrapper<double>().registerm(L);

    tl_array_wrapper<VarExpr>().registerm(L);

    tl_array_wrapper<Distributed>().registerm(L);
    
    tl_array_wrapper<Stress>().registerm(L);
    
    // non-member functions
    luaL_register(L, tbl, felt_reg);

    // global structs
    lua_pushliteral(L, "analysis");
    tl_push<AnalysisPtr>(L, &analysis);
    lua_settable(L, -3);

    lua_pushliteral(L, "problem");
    tl_push<ProblemPtr>(L, &problem);
    lua_settable(L, -3);
    
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
