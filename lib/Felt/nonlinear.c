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
 * File:	nonlinear.c
 *
 * Description:	Contains code to implement nonlinear static, large
 *		deformation analysis.
 *
 ***************************************************************************/

# include <stdio.h>
# include <math.h>
# include "fe.h"
# include "allocate.h"
# include "error.h"
# include "problem.h"



/****************************************************************************
 *
 * Function:	CreateNonlinearStiffness
 *
 * Description:	For a given set of elements (possibly of varying types)
 *		this will create compact column storage for the global
 *		stiffness matrix in a nonlinear problem.  We can't really
 *		base this one simply on the non-zero entries in the 
 *		element stiffness matrices, because as the element
 *		stiffness matrices change with geometry we're not
 *		sure where the zeros might be.  Instead we make sure
 *		that we have at least enough room for all possible 
 *		element DOF.	
 *		
 ****************************************************************************/

Matrix CreateNonlinearStiffness (status)
   int		*status;
{
   Element	*e;
   unsigned	 ne,
		 nn;
   unsigned	 active;
   unsigned	*dofs;
   unsigned	 row,
		 col,
		 i,
		 j,
		 l,
		 k,
		 m;
   unsigned	 size,
		 ndofs,
		 nodes;
   unsigned	 base_row,
		 base_col,
		 affected_row_dof,
		 affected_col_dof;
   unsigned	*ht,*dg;
   Vector 	 K;
   int	 	 err_count;

   e = problem.elements;
   ne = problem.num_elements;
   nn = problem.num_nodes;
   active = problem.num_dofs;
   dofs = problem.dofs_pos;

   err_count = 0;

	/*	
	 * make a pass over the elements to see how all the 
	 * stiffnesses fit together
	 */

   size = nn*active;

   ht = Allocate (unsigned, size);
   if (ht == NULL)
      Fatal ("allocation error setting up compact column heights");

   UnitOffset (ht);

   dg = Allocate (unsigned, size);
   if (dg == NULL)
      Fatal ("allocation error setting up compact column diagonal addresses");

   UnitOffset (dg);

   for (i = 1; i <= size ; i++) 
      ht[i] = dg[i] = 0;

   for (i = 1 ; i <= ne ; i++) {
      ndofs = e [i] -> definition -> numdofs;
      nodes = e [i] -> definition -> numnodes;

      for (j = 1 ; j <= nodes ; j++) {
         if (e [i] -> node[j] == NULL) continue;
         base_row = (e [i] -> node[j] -> number - 1)*active + 1;

         for (k = 1 ; k <= nodes ; k++) {
            if (e [i] -> node[k] == NULL) continue;
            base_col = (e [i] -> node[k] -> number - 1)*active + 1;

            for (l = 1 ; l <= ndofs ; l++) {
               affected_row_dof = dofs [e [i] -> definition -> dofs[l]];
               row = base_row + affected_row_dof - 1;

               for (m = 1 ; m <= ndofs ; m++) {
                  affected_col_dof = dofs [e [i] -> definition -> dofs[m]];
                  col = base_col + affected_col_dof - 1;
                  if (row <= col && col-(row-1) > ht [col])
                     ht [col] = col - (row - 1);
               }
            }
         }
      }
   } /* end loop over elements */

   if (err_count) {
      *status = err_count;
      return NULL;
   }

	/*
	 * setup the diagonal address array and figure out how big
	 * we need to make the compact column vector
	 */

   dg[1] = 1;
   size = 1;
   if (ht [1] == 0)
      ht [1] = 1;

   for (i = 2 ; i <= nn*active ; i++) {
      if (ht[i] == 0)
         ht[i] = 1;

      size += ht[i];
      dg [i] = ht [i] + dg [i-1];
   }

   ZeroOffset (ht);
   Deallocate (ht);

   K = CreateCompactMatrix (nn*active, nn*active, size, dg);
   ZeroMatrix (K);

   *status = err_count;

   return K;
}

/****************************************************************************
 *
 * Function:	
 *
 * Description:	
 *
 ****************************************************************************/

int AssembleCurrentState (K, F, tangent)
   Matrix	K;
   Matrix	F;
   int		tangent;
{
   Element	*element;
   Element	e;
   unsigned	numelts;
   unsigned	active;
   unsigned	*dofs;
   unsigned	row, col;
   unsigned	i, j, l, k, m;
   unsigned	ndofs, nodes;
   unsigned	base_row, base_col;
   unsigned	affected_row_dof, affected_col_dof;
   unsigned	address;
   double	value;
   int	 	err;

   element = problem.elements;
   numelts = problem.num_elements;
   active = problem.num_dofs;
   dofs = problem.dofs_pos;

   ZeroMatrix (K);
   if (F != NullMatrix)
      ZeroMatrix (F);

   for (i = 1 ; i <= numelts ; i++) {
      e = element [i];
      err = e -> definition -> setup (e, 0, tangent);

      ndofs = e -> definition -> numdofs;
      nodes = e -> definition -> numnodes;
      
      for (j = 1 ; j <= nodes ; j++) {
         if (e -> node[j] == NULL) continue;
         base_row = (e -> node[j] -> number - 1)*active + 1;

         for (k = 1 ; k <= nodes ; k++) {
            if (e -> node[k] == NULL) continue;
            base_col = (e -> node[k] -> number - 1)*active + 1;

            for (l = 1 ; l <= ndofs ; l++) {
               affected_row_dof = dofs[e -> definition -> dofs[l]];
               row = base_row + affected_row_dof - 1;

               if (F != NullMatrix) {
                  value = mdata(e -> f, (j - 1)*ndofs + l, 1);
                  sdata(F, row, 1) += value;
               }
 
               for (m = 1 ; m <= ndofs ; m++) {
                  affected_col_dof = dofs[e -> definition -> dofs[m]];
                  col = base_col + affected_col_dof - 1;
                  value =  mdata(e -> K, (j-1)*ndofs + l, (k-1)*ndofs + m); 

                  if (row <= col) {
                     address = ConvertRowColumn (row, col, K);
                     if (address) 
                        VectorData (K) [address] += value;
                  }
               }
            }
         }
      }
   } 

   return 0;
}

/****************************************************************************
 *
 * Function:	
 *
 * Description:	
 *
 ****************************************************************************/
 
int AssembleCurrentForce (F, Fnodal)
   Matrix	F;
   Matrix	Fnodal;
{
   Node		*node;
   unsigned	active;
   unsigned	numnodes;
   unsigned	*dofs;
   unsigned	i,j,
		base_dof;
   double	force;

   node     = problem.nodes;
   active   = problem.num_dofs;
   dofs     = problem.dofs_num;
   numnodes = problem.num_nodes;
   
   for (i = 1 ; i <= numnodes ; i++) {

      base_dof = active*(node[i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         force = mdata(Fnodal, base_dof + j, 1); 

         if (node[i] -> eq_force != NULL && node[i] -> eq_force[dofs[j]]) 
            force += node[i] -> eq_force[dofs[j]];
         
         sdata(F, base_dof + j, 1) = force;    
      }
   }

   return 0;
}

int RestoreCoordinates (d)
   Matrix	d;
{
   int		i, j;
   int		base_dof;
   int		prob_dof;
   Node	       *node;
   unsigned	active;
   unsigned    *dofs;

   active = problem.num_dofs;
   dofs   = problem.dofs_pos; 
   node   = problem.nodes;

   for (i = 1 ; i <= problem.num_nodes ; i++) {
      base_dof = active*(node[i] -> number - 1);
      prob_dof = 1;

	/*
	 * take the cumulative displacements back off from the
	 * actual nodal coordinates and reset them onto the
	 * nodal displacements
	 */

      if (dofs [1]) {
         node [i] -> x -= sdata(d, base_dof + prob_dof, 1);
         node [i] -> dx [1] = sdata(d, base_dof + prob_dof, 1);
         prob_dof++;
      }
      if (dofs [2]) {
         node [i] -> y -= sdata(d, base_dof + prob_dof, 1);
         node [i] -> dx [2] = sdata(d, base_dof + prob_dof, 1);
         prob_dof++;
      }
      if (dofs [3]) {
         node [i] -> z -= sdata(d, base_dof + prob_dof, 1);
         node [i] -> dx [3] = sdata(d, base_dof + prob_dof, 1);
         prob_dof++;
      }

	/*
	 * finish filling out the nodal displacements for the
	 * rotational DOF
	 */

      for (j = 4 ; j <= 6 ; j++) {
         if (dofs [j]) {
            node [i] -> dx [j] = sdata(d, base_dof + prob_dof, 1);
            prob_dof++;
         }
      }
   }

   return 0;
}

int UpdateCoordinates (d)
   Matrix	d;
{
   int		i;
   int		base_dof;
   int		prob_dof;
   Node	       *node;
   unsigned	active;
   unsigned    *dofs;

   active = problem.num_dofs;
   dofs   = problem.dofs_pos; 
   node   = problem.nodes;

   for (i = 1 ; i <= problem.num_nodes ; i++) {
      base_dof = active*(node[i] -> number - 1);
      prob_dof = 1;
      if (dofs [1]) {
         node [i] -> x += sdata(d, base_dof + prob_dof, 1);
         prob_dof++;
      }
      if (dofs [2]) {
         node [i] -> y += sdata(d, base_dof + prob_dof, 1);
         prob_dof++;
      }
      if (dofs [3]) {
         node [i] -> z += sdata(d, base_dof + prob_dof, 1);
         prob_dof++;
      }
   }

   return 0;
}

/****************************************************************************
 *
 * Function:	StaticNonlinearDisplacements 
 *
 * Description:	solves a geometrically nonlinear, large deformation
 *		problem by iteratively solving Kd = F where K
 *		is updated at every iteration according to the 
 *		displacements from the previous iteration - the
 *		value of tangent determines what kind of stiffness
 *		matrix is formed (simple linear or nonlinear tangent)
 *		and how the force residuals are calculated
 *		
 ****************************************************************************/

Matrix StaticNonlinearDisplacements (K, Fnodal, tangent)
   Matrix	K;
   Matrix	Fnodal;
   int		tangent;
{
   Matrix	  residual;
   Matrix	  Felement;
   Matrix	  d;
   Matrix	  d_cum;
   Matrix	  F;
   int		  converged;
   int		  iter;
   int		  n;
   double	  norm;
   int		  step;

   n = Mrows(K);

   residual = CreateColumnVector (n);
   F        = CreateColumnVector (n);
   d        = CreateColumnVector (n);
   d_cum    = CreateColumnVector (n);

   if (tangent)
      Felement = CreateColumnVector (n);
   else
      Felement = NullMatrix;

   ZeroMatrix (d_cum);

   ScaleMatrix (Fnodal, Fnodal, 1.0 / (double) analysis.load_steps, 0.0);

   converged = 0; /* gcc -Wall */
   for (step = 1 ; step <= analysis.load_steps ; step ++) {

      ZeroMatrix (d);

      converged = 0;
      for (iter = 1; iter <= analysis.iterations ; iter++) {
         AssembleCurrentForce (F, Fnodal);   
         AssembleCurrentState (K, Felement, tangent);
       
         if (!tangent) {
            MultiplyMatrices(residual, K, d);
            SubtractMatrices(residual, F, residual);
         }
         else 
            SubtractMatrices(residual, F, Felement);

         ZeroConstrainedMatrixDOF(K, K);
         ZeroConstrainedMatrixDOF(residual, residual);

         CroutFactorMatrix(K);
         CroutBackSolveMatrix(K, residual);

         PNormVector (&norm, residual, "2");
         if (norm < analysis.tolerance) {
            converged = 1;
            break;
         }

         UpdateCoordinates (residual);
         AddMatrices (d, residual, d);
      }

      if (!converged)
         return NullMatrix;

      detail("step %d converged in %d iterations", step, iter);

      AddMatrices (d_cum, d, d_cum);
   }

   RestoreCoordinates (d_cum);
      
   if (!converged) 
      return NullMatrix;

   return d;
}


/****************************************************************************
 *
 * Function:	SolveNonlinearLoadRange
 *
 ****************************************************************************/

Matrix SolveNonlinearLoadRange (K, Fnodal, tangent)
   Matrix     K;
   Matrix     Fnodal;
   int	      tangent;
{
   unsigned	  num_cases;
   Matrix	  dtable;
   Matrix	  residual;
   Matrix	  d;
   Matrix	  d_cum;
   Matrix	  F;
   Matrix	  Felement;
   double	  Fidof;
   int		  converged;
   int		  iter;
   int		  n;
   double	  norm;
   int		  step;
   int		  idof;
   int		  ca;
   int		  k, j;
  
   num_cases = (fabs(analysis.stop - analysis.start) + 0.5*fabs(analysis.step))
                / fabs(analysis.step) + 1;

   dtable = CreateFullMatrix (num_cases, analysis.numdofs * analysis.numnodes);

   n = Mrows(K);

   residual = CreateColumnVector (n);
   F        = CreateColumnVector (n);
   d        = CreateColumnVector (n);
   d_cum    = CreateColumnVector (n);

   if (tangent)
      Felement = CreateColumnVector (n);
   else
      Felement = NullMatrix;

   idof = GlobalDOF (analysis.input_node -> number, analysis.input_dof);
   Fidof = mdata(Fnodal, idof, 1);


   converged = 0;  /* gcc -Wall */
   for (ca = 1 ; ca <= num_cases ; ca ++) {
      sdata(Fnodal, idof, 1) = Fidof + 
                               (analysis.start + (ca - 1)*analysis.step) / 
                               analysis.load_steps;

      ZeroMatrix (d_cum);

      for (step = 1 ; step <= analysis.load_steps ; step ++) {

         ZeroMatrix (d);

         converged = 0;
         for (iter = 1; iter <= analysis.iterations ; iter++) {
            AssembleCurrentForce (F, Fnodal);
            AssembleCurrentState (K, Felement, tangent);
       
            if (!tangent) {
               MultiplyMatrices(residual, K, d);
               SubtractMatrices(residual, F, residual);
            }
            else 
               SubtractMatrices(residual, F, Felement);

            ZeroConstrainedMatrixDOF(K, K);
            ZeroConstrainedMatrixDOF(residual, residual);

            CroutFactorMatrix(K);
            CroutBackSolveMatrix(K, residual);

            PNormVector (&norm, residual, "2");
            if (norm < analysis.tolerance) {
               converged = 1;
               break;
            }

            UpdateCoordinates (residual);
            AddMatrices (d, residual, d);
         }

         if (!converged) {
            detail("convergence failure at force level %d, step %d", ca, step);
            return NullMatrix;
         }

         detail("force level %d, step %d converged in %d iterations", 
                ca, step, iter);

         AddMatrices (d_cum, d, d_cum);
      }

      for (k = 1 ; k <= analysis.numnodes ; k++) {
         for (j = 1 ; j <= analysis.numdofs ; j++) {
            sdata(dtable, ca, (k-1)*analysis.numdofs + j) =
              mdata(d_cum, GlobalDOF (analysis.nodes [k] -> number, analysis.dofs[j]), 1);
         }
      }

      RestoreCoordinates (d_cum);
   }

      
   if (!converged) 
      return NullMatrix;

   return dtable;
}
