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

#ifndef FE_HPP
#define FE_HPP

#include "cvector1.hpp"

/* A reaction force */

struct Reaction {
    double   force;			/* reaction force             */
    unsigned node;			/* node number                */
    unsigned dof;			/* affected degree of freedom */
};

/*!
  Pretty simple really, first we find how many reaction forces there
  should be, then we allocate space for them, then we multiply rows of
  the stiffness matrix by the global displacement vector to get an
  entry that was previously unknown in the global force vector
*/

cvector1<Reaction> SolveForReactions(Vector K, Vector d, unsigned int *old_numbers);

unsigned
SolveForReactions(const Vector &K, const Vector &d, const unsigned int *old_numbers,
Reaction *reac, unsigned num_reactions);

/*!
  Builds a list of global DOF numbers which have some sort of input
  applied to them.  We make two passes rather than dealing with
  reallocation (and deallocation in the case of no forcing)
 */
cvector1<NodeDOF> FindForcedDOF();


size_t FindForcedDOF(NodeDOF *forced, size_t n);

/*!
  Computes the frequency domain transfer function between inputs at
  forced DOF and the output at the DOF described by nodes= and dofs=
  in the analysis parameters.
*/
cvector1<Matrix> ComputeTransferFunctions(Matrix M, Matrix C, Matrix K, const cvector1<NodeDOF> &forced);

void
ComputeTransferFunctions(const Matrix &M, const Matrix &C, const Matrix &K,
                         const NodeDOF *forced, size_t numforced,Matrix *H);

Matrix ComputeOutputSpectra(const cvector1<Matrix> &H, const cvector1<NodeDOF> &forced);

Matrix
ComputeOutputSpectra(const Matrix *H, const NodeDOF *forced, size_t numforced);

#endif
