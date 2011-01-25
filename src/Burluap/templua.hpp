/*
  User Data With Pointer AUXiliary template.

  Mostly inspired by `User Data With Pointer Example' on the lua wiki,
  provides definitions for various almost-always-used functions.  The
  second argument is a trait that has only one method `name', that
  returns a string for the wrapped type.  Note that `alloc' does not
  allocate anything (we do not know if the wrapped type has a default
  constructor), just sets up the passed pointer on the lua stack.
*/

#ifndef TEMPLUA_HPP
#define TEMPLUA_HPP

#include <cassert>
#include <string>
#include <cstdio>

extern "C" {
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

//namespace templua {

template<typename T>
std::string tl_metaname() 
{
    assert(false);
    return "NONE";
}
    
template<typename T>
int tl_setup(lua_State *L, const luaL_reg *m, const luaL_reg *f) 
{
    const std::string name = tl_metaname<T>();
    luaL_newmetatable(L, name.c_str());
    lua_pushstring(L, "__index");
    lua_pushvalue(L, -2);
    lua_settable(L, -3);
    luaL_register(L, NULL, m);
    if (f)
        luaL_register(L, name.c_str(), f);
    return 1;
}
    
template<typename T>
T tl_check(lua_State *L, int index) 
{
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
}
    
template<typename T>
void tl_push(lua_State *L, T p) 
{
    const std::string name = tl_metaname<T>();
    T* v = (T*) lua_newuserdata(L, sizeof(T));
    *v = p;
    luaL_getmetatable(L, name.c_str());
    lua_setmetatable(L, -2);
#ifdef SHOWNEWDELETE
    std::printf("NEW %s [%p]\n", name.c_str(), v);
#endif
}

template<>
void tl_push(lua_State *L, int ii)
{
    lua_pushinteger(L, ii);
}

template<>
void tl_push(lua_State *L, unsigned val)
{
    lua_pushinteger(L, val);
}

template<>
void tl_push(lua_State *L, double val)
{
    lua_pushnumber(L, val);
}

template<>
void tl_push(lua_State *L, float val)
{
    lua_pushnumber(L, val);
}

template<typename T>
int tl_index_wprop(lua_State *L)
{
    const std::string name = tl_metaname<T>();
        
    // check if key actually exists in metatable
    luaL_getmetatable(L, name.c_str());
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

template<typename T>
int tl_newindex_wprop(lua_State *L)
{
    const std::string name = tl_metaname<T>();
            
    // check if key actually exists in metatable
    luaL_getmetatable(L, name.c_str());
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

template<typename T>
int tl_gc(lua_State *L) 
{
    const std::string name = tl_metaname<T>();
    T* p = (T*) lua_touserdata(L, 1);
    if (!p) luaL_typerror(L, 1, name.c_str());
    assert(*p);
    delete *p;
#ifdef SHOWNEWDELETE
    std::printf("DELETE %s [%p]!\n", name.c_str(), p);
#endif
    return 0;
}

template<typename T, typename V, size_t offs>
int tl_getter(lua_State *L)
{
    T p = tl_check<T>(L, 1);
    char *pp = (char *) p;
    V val = *((V *) (pp + offs));
    tl_push<V>(L, val);
    return 1;
}

#endif
