/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson
    Copyright (C) 2010,2011 Panagiotis A.Vossos

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

#ifndef SETAUX_HPP
#define SETAUX_HPP

#include <set>
#include <string>

//----------------------------------------------------------------------!

// search by name
template<typename Key, typename Comp>
Key* SetSearch(const std::set<Key*, Comp> &ss, const std::string &key)
{
    Key dummy;
    dummy.name = key;
    typename std::set<Key*,Comp>::const_iterator it = ss.find(&dummy);
    return it != ss.end() ? *it : NULL;
}

// search by number
template<typename Key, typename Comp>
Key* SetSearch(const std::set<Key*, Comp> &ss, unsigned nn)
{
    Key dummy;
    dummy.number = nn;
    typename std::set<Key*,Comp>::const_iterator it = ss.find(&dummy);
    return it != ss.end() ? *it : NULL;
}

// return previous element, or NULL
template<typename Key, typename Compare>
Key SetPredecessor(const std::set<Key, Compare> &ss, const Key &val)
{
    typename std::set<Key,Compare>::const_iterator it = ss.lower_bound(val);
    return it != ss.begin() ? *(--it) : NULL;
}

// return next element, or NULL
template<typename Key, typename Comp>
Key SetSuccessor(const std::set<Key, Comp> &ss, const Key &val)
{
    typename std::set<Key,Comp>::const_iterator it = ss.upper_bound(val);
    return it != ss.end() ? *it : NULL;
}

// beginning of set, or NULL
template<typename Key, typename Comp>
Key* SetMinimum(const std::set<Key*, Comp> &ss)
{
    if (ss.empty())
        return NULL;
    else return *ss.begin();
}

// end of set, or NULL
template<typename Key, typename Comp>
Key* SetMaximum(const std::set<Key*, Comp> &ss)
{
    if (ss.empty())
        return NULL;
    else 
        return *ss.rbegin();
}

//----------------------------------------------------------------------!

#include<boost/shared_ptr.hpp>
#include<boost/make_shared.hpp>

template<typename Key, typename Comp>
boost::shared_ptr<Key> SetSearch(const std::set<boost::shared_ptr<Key>, Comp> &ss, unsigned nn)
{
    typename boost::shared_ptr<Key> kp = boost::make_shared<Key>(nn);
    
    typename std::set<boost::shared_ptr<Key>,Comp>::iterator it = ss.find(kp);
    if (it != ss.end())
        return *it;
    else 
        return boost::shared_ptr<Key>();
}

template<typename Key, typename Comp>
boost::shared_ptr<Key> SetSearch(const std::set<boost::shared_ptr<Key>, Comp> &ss, const std::string &key)
{
    typename boost::shared_ptr<Key> kp = boost::make_shared<Key>(key.c_str());
    
    typename std::set<boost::shared_ptr<Key>,Comp>::iterator it = ss.find(kp);
    if (it != ss.end())
        return *it;
    else 
        return boost::shared_ptr<Key>();
}

template<typename Key, typename Comp>
boost::shared_ptr<Key> SetMinimum(const std::set<boost::shared_ptr<Key>, Comp> &ss)
{
    if (ss.empty())
        return boost::shared_ptr<Key>();
    else 
        return *ss.begin();
}

template<typename Key, typename Comp>
boost::shared_ptr<Key> SetMaximum(const std::set<boost::shared_ptr<Key>, Comp> &ss)
{
    if (ss.empty())
        return boost::shared_ptr<Key>();
    else 
        return *ss.rbegin();
}

template<typename Key, typename Comp>
boost::shared_ptr<Key> SetPredecessor(const std::set<boost::shared_ptr<Key>, Comp> &ss,
                                      const boost::shared_ptr<Key> &val)
{
    typename std::set<boost::shared_ptr<Key>,Comp>::const_iterator it = ss.lower_bound(val);
    return it != ss.begin() ? *(--it) : boost::shared_ptr<Key>();
}

template<typename Key, typename Comp>
boost::shared_ptr<Key> SetSuccessor(const std::set<boost::shared_ptr<Key>, Comp> &ss,
                                    const boost::shared_ptr<Key> &val)
{
    typename std::set<boost::shared_ptr<Key>,Comp>::const_iterator it = ss.upper_bound(val);
    return it != ss.end() ? *it : boost::shared_ptr<Key>();
}

//----------------------------------------------------------------------!

#endif
