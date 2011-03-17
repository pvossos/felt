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
    tl_pushn<double>(L, nn->stress+1, 6);
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
    /*
      FIXME: this is error-prone, wait for smart_ptr integration.

    unsigned num = luaL_checkint(L, 1);
    Node nn = CreateNode(num);
    lua_pushlightuserdata(L, (void *) nn);
    lua_pushboolean(L, 1);
    lua_settable(L, LUA_ENVIRONINDEX);
    tl_push<Node>(L, nn);
    */
    return 0;
}

static int Node_gc(lua_State *L)
{
    /*
      FIXME: this is error-prone, wait for smart_ptr integration.

    Node nn = tl_check<Node>(L, 1);
    lua_pushlightuserdata(L, (void *) nn);
    lua_gettable(L, LUA_ENVIRONINDEX);
    if (!lua_isnil(L, -1)) {
        lua_pushlightuserdata(L, (void *) nn);
        lua_pushnil(L);
        lua_settable(L, LUA_ENVIRONINDEX);
        printf("Node_gc for Node @ %p\n", nn);
        DestroyNode(nn);
    }
    */
    return 0;
}

#define GETTER(typ, field) tl_getter<node, typ, &node::field>
#define GETSET(typ, field) tl_getset<node, typ, &node::field>()

static void
register_Nodes(lua_State *L)
{
    tl_wrapper<node> w(true);
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
    {"Node", Node_new},
    {NULL, NULL} /* sentinel */
};

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

static int Stress_get_values(lua_State *L)
{
    Stress ss = tl_check<Stress>(L, 1);
    tl_pushn<double>(L, ss->values+1, ss->numvalues);
    return 1;
}

#define GETSET(typ, field) tl_getset<stress, typ, &stress::field>()

void register_Stresses(lua_State *L)
{
    tl_wrapper<stress> w(true);
    w.prop("x", GETSET(double, x));
    w.prop("y", GETSET(double, y));
    w.prop("z", GETSET(double, z));
    w.prop("values", Stress_get_values);
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
    lua_pushfstring(L, "[Element %d @ %p]", ee->number, ee);
    return 1;
}

static int Element_get_nodes(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    tl_pushn<Node>(L, ee->node+1, ee->definition->numnodes);
    return 1;
}

static int Element_get_stresses(lua_State *L)
{
    Element ee = tl_check<Element>(L, 1);
    tl_pushn<Stress>(L, ee->stress+1, ee->ninteg);
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

#define GETTER(typ, field) tl_getter<element, typ, &element::field>

void register_Elements(lua_State *L)
{
    tl_wrapper<element> w(true);
    w.prop("number", GETTER(unsigned, number));
    w.prop("nodes", Element_get_nodes);
    w.prop("distributed", Element_get_distributed);
    w.prop("stresses", Element_get_stresses);
    w.prop("material", GETTER(Material, material));
    w.prop("definition", GETTER(Definition, definition));
    w.registerm(L);
}

#undef GETTER

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
    lua_pushfstring(L, "[Material %s @ %p]", mt->name, mt);
    return 1;
}

#define GETTER(typ, field) tl_getter<material, typ, &material::field>

static void
register_Materials(lua_State *L)
{
    tl_wrapper<material> w(true);
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
    lua_pushfstring(L, "[Force %s @ %p]", ff->name, ff);
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
    tl_wrapper<force> w(true);
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
    lua_pushfstring(L, "[Constraint %s @ %p]", cc->name, cc);
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
    tl_wrapper<constraint> w(true);
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

#define GETTER(typ, field) tl_getter<distributed, typ, &distributed::field>

void register_Distributed(lua_State *L)
{
    tl_wrapper<distributed> w(true);
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
    lua_pushfstring(L, "[Definition %s @ %p]", dd->name, dd);
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
    
    Definition df = new definition;
    df->name = strdup(name);
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
    DefnUdata *du = new DefnUdata;
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

static int Definition_gc(lua_State *L)
{
    Definition dd = tl_check<Definition>(L, 1);
    DefnUdata *du = (DefnUdata *) dd->udata;

    //FIXME
    //lua_unref(L, LUA_REGISTRYINDEX, du->setup_ref);
    return 0;
}

static int Definition_get_dofs(lua_State *L)
{
    Definition dd = tl_check<Definition>(L, 1);
    tl_pushn<unsigned>(L, dd->dofs+1, dd->numdofs);
    return 1;
}

#define GETTER(typ, field) tl_getter<definition, typ, &definition::field>
#define GETSET(typ, field) tl_getset<definition, typ, &definition::field>()

void register_Definitions(lua_State *L)
{
    tl_wrapper<struct definition> w(true);
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

#define GETTER(typ, field) tl_getter<struct analysis, typ, &analysis::field>
#define GETSET(typ, field) tl_getset<struct analysis, typ, &analysis::field>()

void register_AnalysisPtr(lua_State *L)
{
    tl_wrapper<struct analysis> w(true);
    w.prop("start", GETSET(double, start));
    w.prop("step", GETSET(double, step));
    w.prop("stop", GETSET(double, stop));
    w.prop("gamma", GETSET(double, gamma));
    w.prop("beta", GETSET(double, beta));
    w.prop("alpha", GETSET(double, alpha));
    w.prop("Rk", GETSET(double, Rk));
    w.prop("Rm", GETSET(double, Rm));
    w.prop("mass_mode", GETSET(char, mass_mode));
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
    tl_pushn<Node>(L, pp->nodes+1, pp->num_nodes);
    return 1;
}

static int ProblemPtr_get_elements(lua_State *L)
{
    ProblemPtr pp = tl_check<ProblemPtr>(L, 1);    
    tl_pushn<Element>(L, pp->elements+1, pp->num_elements);
    return 1;
}

void register_ProblemPtr(lua_State *L)
{
    tl_wrapper<Problem> w(true);
    w.prop("nodes", ProblemPtr_get_nodes);
    w.prop("elements", ProblemPtr_get_elements);
    w.registerm(L);
};

//----------------------------------------------------------------------!

/*
template<> std::string tl_metaname<DefnUdata>()
{
    return "FElt.DefnUdata";
}
*/


int add_definition(lua_State *L)
{
    const char* name = luaL_checkstring(L, 1);
    luaL_checktype(L, 2, LUA_TFUNCTION);
    luaL_checktype(L, 3, LUA_TFUNCTION);




    // alloc
    Definition def = new definition;

    return 0;
}

int remove_definition(lua_State *L)
{
    const char *name = luaL_checkstring(L, 1);
    
    //lua_unref(L, LUA_REGISTRYINDEX,

}

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
    lua_settable(L, -1);
        
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

    tl_array_wrapper<unsigned>().registerm(L);
    
    tl_array_wrapper<Node>().registerm(L);

    tl_array_wrapper<Element>().registerm(L);
    
    tl_array_wrapper<double>().registerm(L);

    tl_array_wrapper<VarExpr>().registerm(L);

    tl_array_wrapper<Distributed>().registerm(L);

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
