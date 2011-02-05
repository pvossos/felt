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

/***************************************************************************
 *
 * File:	transient.c
 *
 * Description:	Contains code to implement transient (time-dependent) 
 *		analysis features of the finite element method.
 *
 * Notes:	The routines in this file are _closely_ tied in with
 *		those in fe.c (the main algorithmic engine stuff).  The
 *		only reason they're here instead of there is for 
 *		simplicity.
 *
 * History:	v2.30 by Jason Gobat and Darren Atkinson	
 *
 ***************************************************************************/

# include <stdio.h>
# include <math.h>
# include "fe.h"
# include "error.h"
# include "problem.h"
# include "transient.hpp"

int
ConstructDynamic(Vector *Kr, Vector *Mr, Vector *Cr)
{
   Element	*element;
   unsigned	numelts;
   unsigned	active;
   unsigned	*dofs;
   Vector	M, K, C;
   unsigned	row,
		col,
		i,
		j,
		l,
		k,
		m;
   unsigned	size,
		ndofs,
		nodes;
   unsigned	base_row,
		base_col,
		affected_row_dof,
		affected_col_dof;
   unsigned	address;
   double	mvalue;
   double	kvalue;
   int	 	err,
		err_count;

   active   = problem.num_dofs;
   const Node *node = problem.nodes.c_ptr1();
   element  = problem.elements;
   numelts  = problem.num_elements;
   const unsigned numnodes = problem.nodes.size();
   dofs     = problem.dofs_pos;

   err_count = 0;

	/*	
	 * first we make a pass over the elements to see how all the 
	 * stiffnesses fit together so we can set up our compact column
	 * storage sceme.  Inefficient as hell I concede.
	 */

   size = numnodes*active;

   cvector1u ht(size, 0);
   cvector1u dg(size, 0);

   for (i = 1 ; i <= numelts ; i++) {
      err = ElementSetup (element [i], analysis.mass_mode);

      if (element [i] -> M == NullMatrix) {
         error("mass matrix not defined for element %d", element [i] -> number);
         err_count ++;
      } 

      if (err) {
         err_count += err;
         continue;
      } 
         
      ndofs = element[i] -> definition -> numdofs;
      nodes = element[i] -> definition -> numnodes;
     
      if (element[i] -> K == NullMatrix || element[i] -> M == NullMatrix ||
          !IsSquare(element[i] -> K) || !IsSquare(element[i] -> M) ||
          Mrows(element[i] -> K) != Mrows(element[i] -> M) ||
          Mrows(element[i] -> K) > ndofs*nodes) {

          error ("invalid element matrices setup for %s element %d",
                 element[i] -> definition -> name, element[i] -> number);

          err_count ++;
          continue;
      }
         
      for (j = 1 ; j <= nodes ; j++) {
         if (element [i] -> node[j] == NULL) continue;
         base_row = (element[i] -> node[j] -> number - 1)*active;

         for (k = 1 ; k <= nodes ; k++) {
            if (element [i] -> node[k] == NULL) continue;
            base_col = (element[i] -> node[k] -> number - 1)*active;

            for (l = 1 ; l <= ndofs ; l++) {
               affected_row_dof = dofs[element[i] -> definition -> dofs[l]];
               row = base_row + affected_row_dof;

               for (m = 1 ; m <= ndofs ; m++) {
                  affected_col_dof = dofs[element[i] -> definition -> dofs[m]];
                  col = base_col + affected_col_dof;
                  kvalue =  MatrixData (element[i] -> K) [(j-1)*ndofs + l]
                                                         [(k-1)*ndofs + m]; 
                  mvalue =  MatrixData (element[i] -> M) [(j-1)*ndofs + l]
                                                         [(k-1)*ndofs + m]; 
                  if ((kvalue != 0.0 || mvalue != 0.0) && row <= col) { 
                     if (col-(row-1) > ht [col])
                        ht [col] = col - (row - 1);
                  }
               }
            }
         }
      }
   } /* end first loop over elements */

   if (err_count) 
      return err_count;

	/*
	 * setup the diagonal address array and figure out how big
	 * we need to make the compact column vector
	 */

   dg[1] = 1;
   size = 1;
   if (ht [1] == 0)
      ht [1] = 1;

   for (i = 2 ; i <= numnodes*active ; i++) {
      if (ht[i] == 0)
         ht[i] = 1;

      size += ht[i];
      dg [i] = ht [i] + dg [i-1];
   }

   K = CreateCompactMatrix (numnodes*active, numnodes*active, size, dg.c_ptr1());
   M = CreateCompactMatrix (numnodes*active, numnodes*active, size, dg.c_ptr1());
   C = CreateCompactMatrix (numnodes*active, numnodes*active, size, dg.c_ptr1());

   ZeroMatrix (K);
   ZeroMatrix (M);
   ZeroMatrix (C);

	/*
	 * now we make just about the identical passes over the elements,
	 * the result of this pass however will be that we actually
	 * start sticking stuff into the vector which is the compact
	 * column representation of the global stiffness matrix
	 */

   for (i = 1 ; i <= numelts ; i++) {
      ndofs = element[i] -> definition -> numdofs;
      nodes = element[i] -> definition -> numnodes;
      
      for (j = 1 ; j <= nodes ; j++) {
         if (element [i] -> node[j] == NULL) continue;
         base_row = (element[i] -> node[j] -> number - 1)*active;

         for (k = 1 ; k <= nodes ; k++) {
            if (element [i] -> node[k] == NULL) continue;
            base_col = (element[i] -> node[k] -> number - 1)*active;

            for (l = 1 ; l <= ndofs ; l++) {
               affected_row_dof = dofs[element[i] -> definition -> dofs[l]];
               row = base_row + affected_row_dof;

               for (m = 1 ; m <= ndofs ; m++) {
                  affected_col_dof = dofs[element[i] -> definition -> dofs[m]];
                  col = base_col + affected_col_dof;
                  kvalue =  MatrixData (element[i] -> K) [(j-1)*ndofs + l]
                                                         [(k-1)*ndofs + m]; 
                  mvalue =  MatrixData (element[i] -> M) [(j-1)*ndofs + l]
                                                         [(k-1)*ndofs + m]; 

                  if (row <= col) {
                     address = ConvertRowColumn (row, col, K);
                     if (address) {
                        VectorData (K) [address] += kvalue;
                        VectorData (M) [address] += mvalue;
                        VectorData (C) [address] += 
                                   element[i] -> material -> Rk * kvalue +
                                   element[i] -> material -> Rm * mvalue;
                     }
                  }
               }
            }
         }
      }

      if (!element[i] -> definition -> retainK) {
         DestroyMatrix (element[i] -> K);
         element[i] -> K = NullMatrix;
      }

      DestroyMatrix (element[i] -> M);
      element[i] -> M = NullMatrix;

   } /* end second loop over elements */

	/*
	 * now we need to make one quick pass over the _nodes_
	 * to take care of any nodally lumped masses in the problem
	 */

   for (i = 1 ; i <= numnodes ; i++) {

      base_row = active*(node[i] -> number - 1);

      for (j = 1 ; j <= 3 ; j++) {
         if (dofs [j]) 
            MatrixData (M) [dg[base_row + dofs[j]]][1] += node[i] -> m;
      }
   }
      
	/*
 	 * if they specified an Rm or an Rk in the analysis parameters
	 * then they want globally based Rayleigh damping, not the
	 * elemental stuff that we normally do
	 */

   if (analysis.Rk || analysis.Rm) {
      for (i = 1 ; i <= Msize(M) ; i++) 
         C -> data [i][1] = M -> data[i][1] * analysis.Rm +
                            K -> data[i][1] * analysis.Rk;
   } 

	/*
	 * set some things up for the return
	 */

   *Kr = K;
   *Mr = M;
   *Cr = C;

   return 0;
}

void
AssembleTransientForce(double t, Vector F)
{
   unsigned	active;
   unsigned	*dofs;
   unsigned	i,j,
		base_dof;
   unsigned	size;
   double	force;
   double	factor;

   const Node *node = problem.nodes.c_ptr1();
   const unsigned numnodes = problem.nodes.size();
   active = problem.num_dofs;
   dofs = problem.dofs_num;

   size = numnodes*active;

   for (i = 1 ; i <= size ; i++) 
      VectorData (F) [i] = 0;

   for (i = 1 ; i <= numnodes ; i++) {

      base_dof = active*(node[i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         force = 0.0;
         if (node[i] -> force != NULL) {
            if (node[i] -> force -> force [dofs[j]].expr != NULL) {
               factor = EvalCode (node[i] -> force -> force [dofs[j]].expr, t);
               force += factor;
            }
            else if (node[i] -> force -> force[dofs[j]].value) 
               force += node[i] -> force -> force[dofs[j]].value;
         }
         if (node[i] -> eq_force != NULL) {
            if (node[i] -> eq_force[j])
               force += node[i] -> eq_force[dofs[j]];
         }
         VectorData (F) [base_dof + j] += force;
      }
   }

   return;
}

Matrix
IntegrateHyperbolicDE(Vector K, Vector M, Vector C)
{
   unsigned	count;
   unsigned	i,j;
   Matrix	dtable;
   Vector	d;
   Vector	a;
   Vector	v;
   Vector	F;
   Matrix	Kp;
   Matrix	Kp_fact;
   Matrix	Mt;
   double	vpred, dpred, value;
   unsigned	size;
   double	c1,c2, c3, c4, c5, c6;
   unsigned	step;
   unsigned	nsteps;
   int		address;
   int		build_a0; 
   double	t;

   const Node *node = problem.nodes.c_ptr1();
   const unsigned numnodes = problem.nodes.size();
   count = problem.num_dofs;

	/*
	 * a few constants that we will need
	 */

   size = numnodes*count;
   c1 = (1.0 - 2.0*analysis.beta) * (analysis.step*analysis.step)/2.0;
   c2 = analysis.step * (1.0 - analysis.gamma);
   c3 = analysis.step * analysis.step * analysis.beta;
   c4 = analysis.step * analysis.gamma;
   c5 = (1.0 + analysis.alpha);
   c6 = analysis.alpha * analysis.step;

	/*
	 * create vectors to hold the conditions at timesteps i and i+1
	 */

   d  = CreateVector (size);
   a  = CreateVector (size);
   v  = CreateVector (size);
   F  = CreateVector (size);   

	/*
	 * create the table of nodal time displacements
	 */

   nsteps = (analysis.stop + analysis.step/2.0) / analysis.step + 1.0;
   dtable = CreateMatrix (nsteps, analysis.nodes.size()*analysis.numdofs);

	/*
	 * create the K' matrix
	 */

   Kp = CreateCopyMatrix (K);
   for (i = 1 ; i <= Msize (K) ; i++)
      VectorData (Kp) [i] = VectorData (M) [i]/c3 + 
                            VectorData (C) [i]*c4/c3 + 
                            VectorData (K) [i]*c5;

	/*
	 * create a constrained copy of K' and do a one-time
	 * factorization on it.  This is the matrix that we will
	 * use as the RHS of our implicit update equation
	 */

   ZeroConstrainedDOF (Kp, NULL, &Kp_fact, NULL);
   if (CroutFactorMatrix (Kp_fact)) {
      error ("singular K' matrix in hyperbolic integration - cannot proceed");
      return NullMatrix;
   }
   
	/* 
	 * build the initial displacement and velocity vectors from the
	 * initial conditions	
 	 */

   cvector1i constraint_mask = BuildConstraintMask ( );
   build_a0 = BuildHyperbolicIC (d, v, a);

	/*
	 * build the F(0) vector, we only need this to get a(0),
	 * after this, we really will use F as F(i+1)
	 */

   AssembleTransientForce (0.0, F);

	/*
	 * solve for the initial acceleration vector.  First we factorize
	 * the mass matrix, then we form the right hand side vector
	 * as F(0) - Kd(0) - Cv(0) and solve the system to get a(0)
	 */

   if (build_a0) {
      ZeroConstrainedDOF (M, NULL, &Mt, NULL);

      if (CroutFactorMatrix (Mt)) {
         error ("singular M matrix in hyperbolic integration - cannot proceed");
         return NullMatrix;
      }

      MultiplyMatrices (a, K, d);
      SubtractMatrices (a, F, a);
      MultiplyMatrices (F, C, v);
      SubtractMatrices (a, a, F);

      if (CroutBackSolveMatrix (Mt, a)) {
         error ("singular M matrix in hyperbolic integration - cannot proceed");
         return NullMatrix;
      }
   }
   else
      Mt = NullMatrix;


	/*
	 * Copy the initial displacement vector into the table.
	 * This is basically a copy of the code at the end of the loop.
	 */

   for (i = 1 ; i <= analysis.nodes.size() ; i++) {
      for (j = 1 ; j <= analysis.numdofs ; j++) {
         MatrixData (dtable) [1][(i-1)*analysis.numdofs + j] = 
           sdata(d, GlobalDOF(analysis.nodes [i] -> number,analysis.dofs[j]),1);
      }
   }

	/*
	 * iterate over every time step.  Fill up dtable with
	 * the results
	 */

   for (step = 2 ; step <= nsteps ; step++) {
      
	/*
	 * setup F'(i+1).  First find F(i+1) = F(t + dt), then
	 * adjust it by tacking on the rest of the stuff on the RHS
	 * of our implicit update equation.
	 */

      t = (step - 1.0)*analysis.step;	
      AssembleTransientForce (t+c6, F);      

	/*
	 * form the left hand side vector (F'(i+1))
	 */

      for (i = 1 ; i <= size ; i++) {
         if (!constraint_mask [i]) {

            value = 0.0;
            for (j = 1 ; j <= size ; j++) {
               if (!constraint_mask [j]) {
                  address = ConvertRowColumn (i, j, K);
                  if (address) {
                     dpred = VectorData (d) [j] + 
                             analysis.step*VectorData (v) [j] +
                             c1*VectorData (a) [j];
                     vpred = VectorData (v) [j] + c2*VectorData (a) [j];
   
                     value += VectorData (M) [address]*dpred/c3 +
                              VectorData (C) [address]*(dpred*c4/c3 - vpred*c5 +
                                             analysis.alpha*VectorData (v) [j]) +
                              VectorData (K) [address]*VectorData (d) [j]*analysis.alpha;
                  }
               }
            }
            VectorData (F) [i] += value;
         }
         else
            VectorData (F) [i] = 0;
      }

      ResolveBC (t, Kp, F);

	/*
	 * solve for K'd(i+1) = F'(i+1) ... the result will go into F 
	 */

      CroutBackSolveMatrix (Kp_fact, F);
   
	/*
	 * from here we'll solve for a(i+1) and v(i+1)
	 */
      
      for (i = 1 ; i <= size ; i++) {
         dpred = VectorData (d) [i] + 
                 analysis.step*VectorData (v) [i] + c1*VectorData (a) [i];
         vpred = VectorData (v) [i] + c2*VectorData (a) [i];

         VectorData (d) [i] = VectorData (F) [i];
         VectorData (a) [i] = (VectorData (d) [i] - dpred) / c3;
         VectorData (v) [i] = vpred + c4*VectorData (a) [i];
      }

	/*
	 * copy the relevant parts of the displacement vector
	 * into the displacement table
	 */

      for (i = 1 ; i <= analysis.nodes.size() ; i++) {
         for (j = 1 ; j <= analysis.numdofs ; j++) {
            MatrixData (dtable) [step][(i-1)*analysis.numdofs + j] = 
               sdata(d, GlobalDOF(analysis.nodes [i] -> number,analysis.dofs[j]),1);
         }
      }
   }    

	/*
	 * clean up ... 
	 */

   DestroyVector (F);
   DestroyVector (a);
   DestroyVector (d);
   DestroyVector (v);
   DestroyMatrix (Kp);
   DestroyMatrix (Kp_fact);
   if (Mt)
      DestroyMatrix (Mt);

   return dtable;
}

Matrix
IntegrateParabolicDE(Vector K, Vector M)
{
   unsigned	count;
   unsigned	i, j;
   Matrix	dtable;
   Vector	d;
   Vector	F, F1;
   Matrix	Kp, Kp_fact;
   double	value;
   unsigned	size;
   double	c1,c2;
   unsigned	step;
   unsigned	nsteps;
   int		address;
   double	curr_time;

   count = problem.num_dofs;
   const Node *node = problem.nodes.c_ptr1();
   const unsigned numnodes = problem.nodes.size();

	/*
	 * a few constants that we will need
	 */

   size = numnodes*count;
   c1 = analysis.step * analysis.alpha;
   c2 = (1.0 - analysis.alpha) * analysis.step;

	/*
	 * create vectors to hold the conditions at timesteps i and i+1
	 */

   d  = CreateVector (size);
   F  = CreateVector (size);   
   F1 = CreateVector (size);   

	/*
	 * create the table of nodal time displacements
	 */

   nsteps = (analysis.stop + analysis.step/2.0) / analysis.step + 1.0;
   dtable = CreateMatrix (nsteps, analysis.nodes.size()*analysis.numdofs);

	/*
	 * create the K' matrices and do a one time factorization.  We
	 * knock out the constrained rows and columns in the matrix that
	 * we will factor but we don't bother with adjusting the force
	 * vector since it will change with transient forces and transient
 	 * adjustments due to time varying boundary conditions.
	 */

   Kp = CreateCopyMatrix (K);
   for (i = 1 ; i <= Msize (K) ; i++) 
      VectorData (Kp) [i] = VectorData (M) [i] + 
                            VectorData (Kp) [i]*c1;

   ZeroConstrainedDOF (Kp, NULL, &Kp_fact, NULL);
   if (CroutFactorMatrix (Kp_fact)) {
      error ("error in parabolic integration - K' matrix is singular.");
      return NullMatrix;
   }

	/* 
	 * build the initial displacement vector from the
	 * initial conditions	
 	 */

   cvector1i constraint_mask = BuildConstraintMask ( );
   BuildParabolicIC (d);

	/*
	 * Copy the initial displacement vector into the table.
	 */

   for (i = 1 ; i <= analysis.nodes.size() ; i++) {
      for (j = 1 ; j <= analysis.numdofs ; j++) {
         MatrixData (dtable) [1][(i-1)*analysis.numdofs + j] = 
           VectorData (d)[GlobalDOF (analysis.nodes [i] -> number,  analysis.dofs[j])];
      }
   }

	/* 
	 * construct the force vector at time t = 0
	 */

   AssembleTransientForce (0.0, F);      

	/*
	 * iterate over every time step.  Fill up dtable with
	 * the results.  
	 */

   for (step = 2 ; step <= nsteps ; step++) {
      curr_time = (step - 1.0)*analysis.step;
  
	/*
	 * setup the adjusted force vector at time t(i+1)
	 */

      AssembleTransientForce (curr_time, F1);      

	/*
	 * form the RHS of the update equation
	 */

      for (i = 1 ; i <= size ; i++) {
         if (!constraint_mask [i]) {
            value = 0.0;
            for (j = 1 ; j <= size ; j++) {
               address = ConvertRowColumn (i, j, K);
               if (address) 
                  value += (VectorData(M) [address] - 
                            c2*VectorData(K) [address])*VectorData(d) [j];
            }
            VectorData (F) [i] = value +
                           (c2*VectorData(F) [i] + c1*VectorData(F1) [i]);
         }
         else
            VectorData (F) [i] = 0.0;
      }
     
      ResolveBC (curr_time, Kp, F);

	/*
	 * solve K'd(i+1) = (M - c2*K)d(i) + c1*F(i+1) + c2*F(i) ...
         * we have the entire RHS stored in F and that's where
	 * the result will go as well
	 */

      CroutBackSolveMatrix (Kp_fact, F);
 
	/*
	 * copy the relevant parts of the displacement vector
	 * into the displacement table
	 */

      CopyMatrix (d, F);	/* copy d(i+1) to d(i) for the next step */

      for (i = 1 ; i <= analysis.nodes.size() ; i++) {
         for (j = 1 ; j <= analysis.numdofs ; j++) {
            MatrixData (dtable) [step][(i-1)*analysis.numdofs + j] = 
              VectorData (d)[GlobalDOF (analysis.nodes [i] -> number,  analysis.dofs[j])];
         }
      }

      CopyMatrix (F, F1); 	/* copy F(i+1) to F(i) for the next step */
   }    

	/*
	 * clean up ... 
	 */

   DestroyVector (F);
   DestroyVector (F1);
   DestroyVector (d);
   DestroyMatrix (Kp);
   DestroyMatrix (Kp_fact);

   return dtable;
}

int
BuildHyperbolicIC(Vector d, Vector v, Vector a)
{
   unsigned	active;
   unsigned	*dofs;
   unsigned	i,j,
		base_dof;
   unsigned	size;
   int		build_a0;

   const unsigned numnodes = problem.nodes.size();
   active  = problem.num_dofs;
   const Node *node = problem.nodes.c_ptr1();
   dofs = problem.dofs_num;

   size = numnodes*active;

   for (i = 1 ; i <= size ; i++) {
      VectorData (d) [i] = 0.0;
      VectorData (v) [i] = 0.0;
      VectorData (a) [i] = 0.0;
   } 

   build_a0 = 1;

   for (i = 1 ; i <= numnodes ; i++) {

      base_dof = active*(node[i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         VectorData (d) [base_dof+j] = node[i] -> constraint -> ix[dofs[j]];
         if (dofs[j] <= 3) {
            VectorData (v) [base_dof+j] = node[i] -> constraint -> vx[dofs[j]];
            if (node [i] -> constraint -> ax[dofs[j]] != UnspecifiedValue) {
               build_a0 = 0;
               VectorData (a) [base_dof+j] = 
                                 node[i] -> constraint -> ax[dofs[j]];
            }
         }
         else {
            VectorData (v) [base_dof+j] = 0.0;
            VectorData (a) [base_dof+j] = 0.0;
         }
      }
   }

   return build_a0;
}

void
BuildParabolicIC(Vector d)
{
   unsigned	*dofs;
   unsigned	i,j,
		base_dof;
   unsigned	size;

   const Node *node = problem.nodes.c_ptr1();
   dofs = problem.dofs_num;

   size = problem.nodes.size()*problem.num_dofs;

   for (i = 1 ; i <= size ; i++) 
      VectorData (d) [i] = 0.0;

   for (i = 1 ; i <= problem.nodes.size() ; i++) {

      base_dof = problem.num_dofs*(node[i] -> number - 1);

      for (j = 1 ; j <= problem.num_dofs ; j++) 
         VectorData (d) [base_dof + j] = node[i] -> constraint -> ix[dofs[j]];
   }

   return;
}

cvector1i
BuildConstraintMask(void)
{
   unsigned	active;
   unsigned	*dofs;
   unsigned	i, j;
   unsigned	base_dof;
   int		numdofs;

   const Node *node = problem.nodes.c_ptr1();
   const unsigned numnodes = problem.nodes.size();
   active = problem.num_dofs;
   dofs = problem.dofs_num;

   numdofs = numnodes * active;
   
   cvector1i mask(numdofs, 0);

   for (i = 1 ; i <= numnodes ; i++) {
      base_dof = active*(node[i] -> number - 1);
      for (j = 1 ; j <= active ; j++) {

         if (node[i] -> constraint -> constraint[dofs[j]]) 
            mask [base_dof + j] = 1;
      }
   }

   return mask;
}

void
ResolveBC(double t, Vector K, Vector F)
{
   unsigned	*dofs;
   unsigned	active;
   unsigned	i,j;
   unsigned	curr_dof,
		base_dof;
   unsigned	numdofs;
   double	dx;

   const Node *node = problem.nodes.c_ptr1();
   active = problem.num_dofs;
   dofs = problem.dofs_num;

   numdofs = active*problem.nodes.size();

   for (i = 1 ; i <= problem.nodes.size() ; i++) {
      base_dof = active*(node[i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         curr_dof = base_dof + j;

         if (node[i] -> constraint -> constraint[dofs[j]]) {

            if (node[i] -> constraint -> dx [dofs[j]].expr != NULL)
               dx = EvalCode (node[i] -> constraint -> dx [dofs[j]].expr, t);
            else
               dx = node[i] -> constraint -> dx [dofs[j]].value;

            AdjustForceVector (F, K, curr_dof, dx);
            VectorData (F) [curr_dof] = dx;

         } /* end nodal DOF constrained check */
      }	/* end DOF loop */
   } /* end node loop */

   return;
}
