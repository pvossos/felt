/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson

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

#ifndef TRANSIENT_HPP
#define TRANSIENT_HPP

#include "cvector1.hpp"

/*!
  See the description of ConstructStiffness () in fe.c.  This routine
  does the same thing except it includes code to assemble the global
  mass matrix in addition to the global stiffness matrix.  Having two
  separate routines is basically a performance consideration (i.e., why
  do all the mass checks in the static case?)
*/
int ConstructDynamic(Vector *Kr, Vector *Mr, Vector *Cr);

void AssembleTransientForce(double t, Vector F);

cvector1i BuildConstraintMask(void);

/*!
  Fills in the displacement and velocity vectors at time t = 0 given
  the nodal constraint conditions.
*/
int BuildHyperbolicIC(Vector d, Vector v, Vector a);

/*!
  Fills in the displacement vector at time t = 0 given the nodal
  constraint initial conditions.
*/
void BuildParabolicIC(Vector d);

/*!
  Solves the discrete equation of motion, Ma + Cv + Kd = F for the
  length of a model using Newmark's method with the
  Hilbert-Hughes-Taylor alpha correction for improved accuracy with
  numerical damping.
  The first important numerical thing that we do is to solve for the
  initial acceleration vector: Ma(0) = F(0) - Kd(0) - Cv(0). From
  there we can begin the iterations - the iterations proceed by
  solving for d(i+1) implicity and then using this information with
  Newmark's update equations to get a(i+1) and v(i+1)
 */
Matrix IntegrateHyperbolicDE(Vector K, Vector M, Vector C);

/*
 Solves the discrete equation of motion, Ma + Cv + Ky = F(t) starting
 from initial values v(0) and y(0). Uses modified L-stable,
 single-step, three-stage Rosenbrock method with adaptive step-size
 control and error estimation
*/
Matrix RosenbrockHyperbolicDE(Matrix k0, Matrix m, Matrix c0, Matrix *ttable);

/*!
  Solves the discrete parabolic differential equation Mv + Kd = F for
  the length of a model using a generalized trapezoidal method.  The
  implementation we use here does not explicitly make use of the v
  vector because it is slightly more efficient to factor it out from
  the start.
*/
Matrix IntegrateParabolicDE(Vector K, Vector M);


/*!
 Basically like ZeroConstrainedDOF () for the static case, but here we
 only make adjustments for displacement boundary conditions (i.e., we
 don't bother with zeroing rows and columns of the stiffness matrix).
*/
void ResolveBC(double t, Vector K, Vector F);

#endif
