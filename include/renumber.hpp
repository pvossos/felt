#ifndef RENUMBER_HPP
#define RENUMBER_HPP

#include "cvector1.hpp"

cvector1u RenumberNodes(Node *, Element *, unsigned, unsigned);

cvector1u RenumberProblemNodes();

void RestoreNodeNumbers(Node *, const unsigned*, unsigned);

void RestoreProblemNodeNumbers(const cvector1u &old);

#endif
