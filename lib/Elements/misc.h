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

/*****************************************************************************
 *
 * File:	misc.h
 *
 * Description:	contains prototypes of the element convenience routines
 *		found in element.c
 *
 *****************************************************************************/

# ifndef _MISC_H
# define _MISC_H

double ElementLength(Element element, unsigned int coords);

/*!
  Finds the area of a planar element of n nodes.
*/
double ElementArea(Element e, unsigned int n);

/*!
  Sets an array containing the appropriate Gauss points for a given
  number of points for Gaussian quadrature.
*/
unsigned GaussPoints(unsigned int npoints, double **xpoints, double **weights);

Matrix PlaneStressD(Element element);

Matrix PlaneStrainD(Element element);

Matrix AxisymmetricD(Element element);

Matrix IsotropicD(Element element);

/*!
  Multiplies A(trans)*B*A without actually transposing and with no
  full size temporary storage.  It is the caller's responsibility to
  create storage for C and to make sure that dimensions match.
*/
void MultiplyAtBA(Matrix C, Matrix A, Matrix B);

/*!
  Given a hinged DOF, we need to knock out the rows and columns
  associated with that DOF in the element stiffness matrix.  We also
  need to adjust all the coefficients in that stiffness matrix
  according to:
 
  a(i,j) += [-a(m,j)/a(m,m)]*a(i,m)
 
  where m is the row number of the hinged DOF.  The downside to this
  procedure is that we will _not_ be able to get displacements at this
  DOF.  In general, the end displacements of elements connected at a
  hinged DOF will not be continuous.  Given the way FElt deals with
  displacements (i.e., as a solution), I figured it was a better
  compromise to put as much of this in as I could without completely
  changing the output paradigm (i.e., I don't want to start outputting
  element end displacement in lieu of or in addition to the global
  displacements that we already calculate.)
*/
void ResolveHingeConditions(Element element);

void SetupStressMemory(Element element);

void AllocationError(Element e, char *msg);

void SetEquivalentForceMemory(Element element);

void AllocateNodalStress(Node node);

/*!
  Zeros out the row and column given by dof.  Places a one on the
  diagonal.
*/
Matrix ZeroRowCol(Matrix K, unsigned int dof);

void PrincipalStresses3D(double *stress);

void PrincipalStresses2D(double *stress);

# endif /* _MISC_H */
