/*
  User Data With Pointer AUXiliary template.

  Mostly inspired by `User Data With Pointer Example' on the lua wiki,
  provides definitions for various almost-always-used functions.  The
  second argument is a trait that has only one method `name', that
  returns a string for the wrapped type.  Note that `alloc' does not
  allocate anything (we do not know if the wrapped type has a default
  constructor), just sets up the passed pointer on the lua stack.
*/

#ifndef PAVUDWPAUX_HPP
#define PAVUDWPAUX_HPP

#include <cassert>
#include <string>
#include <cstdio>

extern "C" {
    
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

template<typename T>
struct udwp
{
    static std::string name() { assert(false); return "NONE"; }
};
    

template<typename T> //, typename Tr>
struct udwpaux
{
    typedef T* ptr;
    typedef T** pptr;
        
    static int setup(lua_State *L, const luaL_reg *m, const luaL_reg *f) 
        {
            const std::string name = udwp<T>::name();
            luaL_newmetatable(L, name.c_str());
            lua_pushstring(L, "__index");
            lua_pushvalue(L, -2);
            lua_settable(L, -3);
            luaL_register(L, NULL, m);
            if (f)
                luaL_register(L, name.c_str(), f);
            return 1;
        }

    static ptr check(lua_State *L, int index) 
        {
            const std::string name = udwp<T>::name();
            luaL_checktype(L, index, LUA_TUSERDATA);
            pptr v = (pptr) luaL_checkudata(L, index, name.c_str());
            if (!v) luaL_typerror(L, index, name.c_str());
            ptr p = *v;
            if (!p) {
                char buf[100];
                std::sprintf(buf, "NULL %s", name.c_str());
                luaL_error(L, buf);
            }
            return p;
        }
        
    static pptr push(lua_State *L, ptr p) 
        {
            const std::string name = udwp<T>::name();
            pptr v = (pptr) lua_newuserdata(L, sizeof(ptr));
            *v = p;
            luaL_getmetatable(L, name.c_str());
            lua_setmetatable(L, -2);
#ifdef SHOWNEWDELETE
            std::printf("NEW %s [%p]\n", name.c_str(), v);
#endif
            return v;
        }

    static int index_wprop(lua_State *L)
        {
            const std::string name = udwp<T>::name();
            
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

    static int newindex_wprop(lua_State *L)
        {
            const std::string name = udwp<T>::name();
            
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

    static int gc(lua_State *L) 
        {
            const std::string name = udwp<T>::name();
            pptr p = (pptr) lua_touserdata(L, 1);
            if (!p) luaL_typerror(L, 1, name.c_str());
            assert(*p);
            delete *p;
#ifdef SHOWNEWDELETE
            std::printf("DELETE %s [%p]!\n", name.c_str(), p);
#endif
            return 0;
        }
};

#endif // PAVUDWPAUX_HPP

