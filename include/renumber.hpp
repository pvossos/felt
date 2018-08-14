#ifndef RENUMBER_HPP
#define RENUMBER_HPP

#include "cvector1.hpp"

size_t RenumberNodes(Node *, Element *, unsigned, unsigned, unsigned *);

cvector1u RenumberProblemNodes();

void RestoreNodeNumbers(Node *, const unsigned*, unsigned);

void RestoreProblemNodeNumbers(const cvector1u &old);

#endif
