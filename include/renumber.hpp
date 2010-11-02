#ifndef RENUMBER_HPP
#define RENUMBER_HPP

#include "cvector1.hpp"

cvector1u RenumberNodes	(Node *, Element *, unsigned, unsigned);
void RestoreNodeNumbers (Node *, unsigned*, unsigned);

#endif
