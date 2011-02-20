/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson
    Copyright (C) 2010 Panagiotis Vossos

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

#ifndef CVECTOR1_HPP
#define CVECTOR1_HPP

#include <new>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <vector>

//----------------------------------------------------------------------!

// Wrap traditional C arrays around a convenient class. In order to be
// compatible with older felt code and minimize diffs, indexing starts
// at 1. Memory is managed automatically, but the underlying pointer
// can also be released and managed manually (useful for example, in
// combination with an external garbage collector).

// UPDATE: nowadays, we no longer use C arrays, just a std::vector,
// and override the operator[] to provide 1-based indexing.

template<typename Value_T>
class cvector1
{
public:
    // public typedefs
    typedef Value_T value_type;
    typedef Value_T* iterator;
    typedef const Value_T* const_iterator;
    typedef Value_T& reference;
    typedef const Value_T& const_reference;
    typedef size_t size_type;
    typedef cvector1 self;

    // ctors & dtor
    cvector1(): vv() { /* NO-OP */ }
    cvector1(size_type n) : vv(n) { /* NO-OP */ }
    cvector1(size_type n, const Value_T &val) : vv(n, val) { /* NO-OP */ }
    ~cvector1() { /* NO-OP */ }
    
    // assignment op
    cvector1& operator=(const cvector1 &av) { this->vv = av.vv; return *this; }

    // copy ctor
    cvector1(const cvector1 &av) { this->vv = av.vv; }
    
    // add + remove elements
    void resize(size_type n) { vv.resize(n); }
    void resize(size_type n, const Value_T &init) { vv.resize(n, init); }
    void push_back(const Value_T &val) { vv.push_back(val); }
    void clear() { vv.clear(); }

    // access methods
    iterator begin( ) { return vv.begin(); }
    iterator end( ) { return vv.end(); }
    const_iterator begin( ) const { return vv.begin(); }
    const_iterator end( ) const { return vv.end(); }
    reference operator[](size_type n) { assert(n > 0 && n <= vv.size()); return vv[n-1]; }
    const_reference operator[](size_type n) const { assert(n > 0 && n <= vv.size()); return vv[n-1]; }
    size_type size() const { return vv.size(); }    
    bool empty() const { return vv.empty(); }
    Value_T* c_ptr() { return &vv[0]; }
    const Value_T* c_ptr() const { return &vv[0]; }

    // equality op
    bool operator==(const cvector1 &rhs) { return vv == rhs.vv; }
    
    // ATTN: deprecate asap. the original felt code, moved pointers to
    // allocated arrays one position backwards, in order to use
    // 1-based indexing (see UnitOffset macro). By overriding
    // operator[], we no longer need such (invalid) hacks in new
    // code. However, we do provide the following methods as
    // convenience, so that we don't have to change a lot of old code
    // at once.
    Value_T* c_ptr1() { return (0 != vv.size()) ? &vv[0]-1 : NULL; }
    const Value_T* c_ptr1() const { return (0 != vv.size()) ? &vv[0]-1 : NULL; }

private:
    std::vector<Value_T> vv;
};
 
//----------------------------------------------------------------------!

// useful typedefs

typedef cvector1<char>     cvector1c;
typedef cvector1<unsigned> cvector1u;
typedef cvector1<int>      cvector1i;
typedef cvector1<float>   cvector1f;
typedef cvector1<double>   cvector1d;

//----------------------------------------------------------------------!

#endif
