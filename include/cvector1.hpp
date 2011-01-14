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

//----------------------------------------------------------------------!

// Wrap traditional C arrays around a convenient class. In order to be
// compatible with older felt code and minimize diffs, indexing starts
// at 1. Memory is managed automatically, but the underlying pointer
// can also be released and managed manually (useful for example, in
// combination with an external garbage collector).

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

    // constructors
    cvector1()
        {
            m = NULL;
            N = 0;
        }
    cvector1(size_type n) 
        {
            m = (Value_T *) std::malloc(n * sizeof(Value_T));
            if (!m && n != 0) throw std::bad_alloc();
            N = n;
        }
    cvector1(size_type n, const Value_T &val) 
        {
            m = (Value_T *) std::malloc(n * sizeof(Value_T));
            if (!m && n != 0) throw std::bad_alloc();
            for (size_t i = 0; i < n; i++)
                m[i] = val;
            N = n;
        }
    cvector1(Value_T *ptr, size_type n)
        {
            m = ptr;
            N = n;
        }
    
    // assignment operator, handles self-assignment
    cvector1& operator=(const cvector1 &av)
        {
            Value_T *origm = m;
            const size_t nbytes = av.size() * sizeof(Value_T);
            m = (Value_T *) std::malloc(nbytes);
            if (!m && nbytes != 0) {
                m = origm;
                throw std::bad_alloc();
            }
            std::memcpy(m, av.c_ptr(), nbytes);
            if (origm)
                std::free(origm);
            N = av.size();
            return *this;
        }

    // copy constructor
    cvector1(const cvector1 &av)
        {
            m = NULL;
            N = 0;
            *this = av;
        }
    
    // resize, compatible interface with std::vector
    void resize(size_type n)
        {
            Value_T *origm = m;
            m = (Value_T *) std::realloc(m, n * sizeof(Value_T));
            if (!m && n != 0) {
                m = origm;
                throw std::bad_alloc();
            }
            N = n;
        }
    
    // destructor. ATTN: non-virtual by design, do NOT subclass.
    ~cvector1() { if (m) std::free(m); }
    
    // release the pointer, no longer manage it
    Value_T* release()
        {
            Value_T *origm = m;
            N = 0;
            m = NULL;
            return origm;
        }

    // member functions
    iterator begin( ) { return m; }
    iterator end( ) { return m + N; }
    const_iterator begin( ) const { return m; }
    const_iterator end( ) const { return m + N; }
    reference operator[](size_type n) { assert(n > 0 && n <= N); return m[n-1]; }
    const_reference operator[](size_type n) const { assert(n > 0 && n <= N); return m[n-1]; }
    size_type size() const { return N; }    
    bool empty() const { return 0 == N; }
    Value_T* c_ptr() { return &m[0]; }
    const Value_T* c_ptr() const { return &m[0]; }
    
    // ATTN: deprecate asap. the original felt code, moved pointers to
    // allocated arrays one position backwards, in order to use
    // 1-based indexing (see UnitOffset macro). By overriding
    // operator[], we no longer need such (invalid) hacks in new
    // code. However, we do provide the following methods as
    // convenience, so that we don't have to change a lot of old code
    // at once.
    Value_T* c_ptr1() { return (0 != N) ? m-1 : NULL; }
    const Value_T* c_ptr1() const { return (0 != N) ? m-1 : NULL; }
    Value_T* release1() 
        {
            Value_T *origm = m;
            size_type origN = N;
            N = 0;
            m = NULL;
            return (0 != origN) ? origm-1 : NULL;
        }
    
private:
    Value_T *m;
    size_type N;
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
