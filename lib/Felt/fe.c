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
 * File:	fe.c
 *
 * Description:	Contains code to implement various mathematical features of 
 *		the finite element method.
 *
 * Notes:	The compact column storage scheme is managed invisibly by the
 *		low-level matrix manipulation routines sdata and mdata.
 *
 * History:	v2.3x by Jason Gobat and Darren Atkinson	
 *
 ***************************************************************************/

# include <stdio.h>
# include <math.h>
# include "allocate.h"
# include "problem.h"
# include "fe.h"
# include "error.h"

extern Matrix ZeroRowCol ( );

/**************************************************************************
 *
 * Function:	 FindDOFS
 *
 * Description:  FindDOFS will search through all of the elements for a
 *		 problem and determine which DOFs (out of the six that
 *		 are physically possible) must be considered in this
 *		 problem based on the different element types.  The
 *		 list of affected DOFs is built and ...
 *	
 *****************************************************************************/

int
FindDOFS(void)
{
   Element	*e;
   unsigned	ne;
   unsigned	i,
		j;
   Definition	type,
		otype;
   unsigned	flag[7];
   unsigned	count;

   ne = problem.num_elements;
   e = problem.elements;

   for (i = 1 ; i <= 6 ; i++) {
      flag[i] = 0;
      problem.dofs_pos[i] = 0;
      problem.dofs_num[i] = 0;
   }

   otype = NULL;	
 
   for (i = 1 ; i <= ne ; i++) {

      type = e[i] -> definition;
      if (type != otype) {
       
         for (j = 1 ; j <= e[i] -> definition -> numdofs ; j++) 
            flag [e[i] -> definition -> dofs[j]] = 1;

         otype = type;
      }
   }

   count = 0;

   for (i = 1 ; i <= 6 ; i++) {
      if (flag[i]) {
         problem.dofs_pos [i] = ++count;
         problem.dofs_num [count] = i;
      }
   }

   problem.num_dofs = count;

   return count;
}
      
/****************************************************************************
 *
 * Function:	ConstructStiffness
 *
 * Description:	For a given set of elements (possibly of varying types)
 *		this will assemble all element stiffness matrices into
 *		the global stiffness matrix according to what DOFs each
 *		individual element affects (a function of both its node
 *		numbers and the DOFs that it affects and their relation
 *		to the global DOFs as indexed by dofs).
 *		
 *		Before we do anything we figure out how the compact
 *		column storage scheme is going to work, i.e., we need
 *		to set up the height and diag arrays which contain
 *		information needed to go from a standard row,column
 *		notation (which I think is easier to read) to 
 *		an address into the compact column vector representation
 *		of the global stiffness matrix
 *
 ****************************************************************************/

Matrix
ConstructStiffness(int *status)
{
   Element	*element;
   unsigned	numelts,
		numnodes;
   unsigned	active;
   unsigned	*dofs;
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
   unsigned	*ht,*dg;
   unsigned	address;
   double	value;
   Vector 	K;
   int	 	err,
		err_count;

   element = problem.elements;
   numelts = problem.num_elements;
   numnodes = problem.num_nodes;
   active = problem.num_dofs;
   dofs = problem.dofs_pos;

   err_count = 0;

	/*	
	 * first we make a pass over the elements to see how all the 
	 * stiffnesses fit together so we can set up our compact column
	 * storage sceme.  Inefficient as hell I concede.
	 */

   size = numnodes*active;

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

   for (i = 1 ; i <= numelts ; i++) {
      err = ElementSetup (element [i], 0);
      if (err) {
         err_count += err;
         continue;
      } 
         
      ndofs = element[i] -> definition -> numdofs;
      nodes = element[i] -> definition -> numnodes;

      if (element[i] -> K == NullMatrix || !IsSquare(element[i] -> K) || 
          Mrows(element[i] -> K) > ndofs*nodes) {

         error ("%s element %d has an invalid stiffness matrix",
                element[i] -> definition -> name, element[i] -> number);

         err_count ++;
         continue;
      }

      for (j = 1 ; j <= nodes ; j++) {
         if (element [i] -> node[j] == NULL) continue;
         base_row = (element[i] -> node[j] -> number - 1)*active + 1;

         for (k = 1 ; k <= nodes ; k++) {
            if (element [i] -> node[k] == NULL) continue;
            base_col = (element[i] -> node[k] -> number - 1)*active + 1;

            for (l = 1 ; l <= ndofs ; l++) {
               affected_row_dof = dofs[element[i] -> definition -> dofs[l]];
               row = base_row + affected_row_dof - 1;

               for (m = 1 ; m <= ndofs ; m++) {
                  affected_col_dof = dofs[element[i] -> definition -> dofs[m]];
                  col = base_col + affected_col_dof - 1;
                  value =  MatrixData (element[i] -> K) [(j-1)*ndofs + l]
                                                        [(k-1)*ndofs + m]; 
                  if (value != 0.0 && row <= col) { 
                     if (col-(row-1) > ht [col])
                        ht [col] = col - (row - 1);
                  }
               }
            }
         }
      }
   } /* end first loop over elements */

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

   for (i = 2 ; i <= numnodes*active ; i++) {
      if (ht[i] == 0)
         ht[i] = 1;

      size += ht[i];
      dg [i] = ht [i] + dg [i-1];
   }

   ZeroOffset (ht);
   Deallocate (ht);

   K = CreateCompactMatrix (numnodes*active, numnodes*active, size, dg);

   detail ("stiffness matrix size is %d", size);

   ZeroMatrix (K);

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
         base_row = (element[i] -> node[j] -> number - 1)*active + 1;

         for (k = 1 ; k <= nodes ; k++) {
            if (element [i] -> node[k] == NULL) continue;
            base_col = (element[i] -> node[k] -> number - 1)*active + 1;

            for (l = 1 ; l <= ndofs ; l++) {
               affected_row_dof = dofs[element[i] -> definition -> dofs[l]];
               row = base_row + affected_row_dof - 1;

               for (m = 1 ; m <= ndofs ; m++) {
                  affected_col_dof = dofs[element[i] -> definition -> dofs[m]];
                  col = base_col + affected_col_dof - 1;
                  value =  MatrixData (element[i] -> K) [(j-1)*ndofs + l]
                                                        [(k-1)*ndofs + m]; 

                  if (row <= col) {
                     address = ConvertRowColumn (row, col, K);
                     if (address) 
                        VectorData (K) [address] += value;
                  }
               }
            }
         }
      }

      if (!element[i] -> definition -> retainK) {
         DestroyMatrix (element[i] -> K);
         element[i] -> K = NullMatrix;
      }

   } /* end second loop over elements */

	/*
	 * set some things up for the return
	 */

   *status = err_count;

   return K;
}

/****************************************************************************
 *
 * Function:	RemoveConstrainedDOF
 *
 * Description: As opposed to simply zeroing out the rows and columns
 *		associated with a constrained DOF, here we actually reduce
 *		the size of the stiffness and mass matrices by removing 
 *		those rows and columns entirely.
 *
 ****************************************************************************/

void
RemoveConstrainedDOF(Matrix K, Matrix M, Matrix C, Matrix *Kcond, Matrix *Mcond, Matrix *Ccond)
{
   Node		*node;
   unsigned	numnodes,
		active;
   unsigned	*dofs;
   unsigned	orig_dofs;
   unsigned	new_dofs;
   unsigned	height;
   char		*dof_map;
   Matrix	Kc, Mc, Cc;
   unsigned	size;
   unsigned	*diag;
   unsigned	start;
   unsigned	i, j, n, m,
		affected_dof,
		base_dof;

   numnodes = problem.num_nodes;
   node     = problem.nodes;
   active   = problem.num_dofs;
   dofs     = problem.dofs_num;
   
   orig_dofs = numnodes * active;

   dof_map = Allocate (char, orig_dofs);
   UnitOffset (dof_map);

   for (i = 1 ; i <= orig_dofs ; i++) 
      dof_map [i] = 1;

   size = K -> size;
   new_dofs = orig_dofs;

	/*
	 * first we get a count of the number of constrained DOF and build 
	 * a bitmap vector to tell us where they are in the original scheme
	 */

   for (i = 1 ; i <= numnodes ; i++) {
      base_dof = active*(node[i] -> number - 1);
      for (j = 1 ; j <= active ; j++) {

         if (node [i] -> constraint -> constraint [dofs[j]]) {
            affected_dof = base_dof + j;
            dof_map [affected_dof] = 0; 
            new_dofs --;
            if (affected_dof == 1)
               size -= 1;
            else
               size -= (K -> diag [affected_dof] - 
                        K -> diag [affected_dof - 1]);
         }
      }
   }

	/*
	 * now we know how much space we are saving so we can allocate 
	 * space for the condensed stiffness and mass matrices
	 */

   Cc = NullMatrix;
   Kc = CreateCompactMatrix (new_dofs, new_dofs, size, NULL);
   Mc = CreateCompactMatrix (new_dofs, new_dofs, size, NULL);
   if (C != NullMatrix)
      Cc = CreateCompactMatrix (new_dofs, new_dofs, size, NULL);

   diag = Allocate (unsigned, new_dofs);
   UnitOffset (diag);

   n = 1;
   m = 1;

	/*
	 * now we make a column loop over all of the original DOF to see
	 * which ones to copy through
	 */

   for (i = 1 ; i <= orig_dofs ; i++) {
      if (dof_map [i]) {
         if (i == 1) {
            height = 1;
            start = 1;
         }
         else {
            height = K -> diag [i] - K -> diag [i-1];
            start = K -> diag [i - 1] + 1;
         }

	/*
	 * check all the active rows in this column and copy them through
	 * if necessary - updating the diagonal address array for this
	 * column once we are through
	 */

         for (j = start ; j <= K -> diag [i] ; j++) {
            affected_dof = i - height  + 1 + (j - start);             

            if (dof_map [affected_dof]) {
               Kc -> data [m][1] = K -> data [j][1];
               Mc -> data [m][1] = M -> data [j][1];
               if (C != NullMatrix)
                  Cc -> data [m][1] = C -> data [j][1]; 

               m++; 
            }
         }
         diag [n++] = m - 1;
      }
   }

   Kc -> diag = diag;

	/*
	 * allocate, copy and assign a different diag pointer for the mass
	 * matrix to allow for the possibility that the stiffness and the
	 * mass matrices will be destroyed at different times
	 */

   Mc -> diag = Allocate (unsigned, new_dofs);
   UnitOffset (Mc -> diag);
 
   for (i = 1 ; i <= new_dofs ; i++)
      Mc -> diag [i] = diag [i];

   if (C != NullMatrix) {
      Cc -> diag = Allocate (unsigned, new_dofs);
      UnitOffset (Cc -> diag);
 
      for (i = 1 ; i <= new_dofs ; i++)
         Cc -> diag [i] = diag [i];
   }


	/* 
	 * set the pointers for return
	 */

   *Kcond = Kc;
   *Mcond = Mc;
   if (C != NullMatrix)
      *Ccond = Cc;

   ZeroOffset (dof_map); 
   Deallocate (dof_map);

   return;
}

/****************************************************************************
 *
 * Function:	ZeroConstrainedDOF
 *
 * Description: For a fixed BC at a given DOF all we'll do is zero
 *		out the rows and columns of the stiffness matrix
 *		associated with that DOF (with a one on the diagonal
 *		for stability).  For a displacement BC, we'll need
 *		to adjust the force vector, also, we should put
 *		the displacement into the force vector so when we
 *		go to solve, we'll just go ahead and get that
 *		displacement right back.  We don't deal with hinges
 *		here really because we already dealt with them when we had
 *		the element stiffness matrix laying around.  All we'll
 *		do here is make sure that the displacement will come 
 *		out zero.
 *
 ****************************************************************************/

void
ZeroConstrainedDOF(Vector K, Vector F, Vector *Kc, Vector *Fc)
{
   Node		*node;
   unsigned	active;
   unsigned	*dofs;
   Vector	Kcond;
   Vector	Fcond;
   unsigned	i,j,
		affected_dof,
		base_dof;

   node = problem.nodes;
   active = problem.num_dofs;
   dofs = problem.dofs_num;

	/*
	 * allocate and copy the condensed objects
	 */

   Kcond = CreateCopyMatrix (K);

   Fcond = NullMatrix;
   if (F != NULL)
      Fcond = CreateCopyMatrix (F);
   
   for (i = 1 ; i <= problem.num_nodes ; i++) {

      base_dof = active*(node[i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         if (node[i] -> constraint -> constraint[dofs[j]]) {
            affected_dof = base_dof + j;

            if (node[i] -> constraint -> dx[dofs[j]].value == 0.0 ||
                node[i] -> constraint -> constraint[dofs[j]] == 'h') {
               Kcond = ZeroCompactRowCol (Kcond, affected_dof);

               if (F != NULL)
                  VectorData (Fcond) [affected_dof] = 0; 
            }
            else {
               if (F != NULL) {
                  AdjustForceVector (Fcond, Kcond, affected_dof,
                               node[i] -> constraint -> dx[dofs[j]].value);
                  VectorData (Fcond) [affected_dof] = 
                                      node[i] -> constraint -> dx[dofs[j]].value;
               }
               Kcond = ZeroCompactRowCol (Kcond, affected_dof);
            }
         }
      }
   }
   *Kc = Kcond;

   if (F != NULL)
      *Fc = Fcond;

   return;
}

/****************************************************************************
 *
 * Function:	AdjustForceVector
 *
 * Description: Given a displacement boundary condition, we can't just
 *		knock out the rows and columns of K for that DOF.  We
 *		need to adjust the force vector by adding the effect
 *		of the stiffness in that DOF times the given displacement
 *		to the external force vector.  The simplest way to do
 *		that is to just loop over all DOFs and adjust them
 *		as appropriate.  In some cases the adjustment is unnecessary
 *		because the displaced DOF may not affect all DOFs or
 *		a given DOF may be fixed anyways.  It won't hurt to
 *		operate in these cases anyways though, so that's what I do.
 *		
 ****************************************************************************/

void
AdjustForceVector(Vector Fcond, Vector Kcond, unsigned int affected_dof, double dx)
{
    unsigned	i;
    unsigned	address;
    unsigned	size;

    size = Mrows(Fcond);

    for (i = 1 ; i <= size ; i++) {
       address = ConvertRowColumn (i, affected_dof, Kcond);
       if (address)
          VectorData (Fcond) [i] -= VectorData (Kcond) [address]*dx;
    }

    return;
}

/****************************************************************************
 *
 * Function:	ZeroCompactRowCol
 *
 * Description:	Zeros out the row and column given by dof.  Places
 *		a one on the diagonal.
 *
 ****************************************************************************/

Vector
ZeroCompactRowCol(Vector K, unsigned int dof)
{
   unsigned	i;
   unsigned	address;
   unsigned	size;

   size = Mrows(K);

   for (i = 1 ; i <= size ; i++) {
      address = ConvertRowColumn (i, dof, K);
      if (address)
          VectorData (K) [address] = 0;
   }

   address = ConvertRowColumn (dof, dof, K);

   if (address) /* though this should always be valid */
      VectorData (K) [address] = 1;

   return K;
}

/****************************************************************************
 *
 * Function:	ConstructForceVector		
 *
 * Description:	Constructs the global nodal force vector based on all
 *		nodal forces and the global DOFs active at those nodes.
 *		Global DOF determination is by node number and the
 *		and the relationship between the force and its actual
 *		physical DOF and the location of this DOF in problem space.
 *
 ****************************************************************************/
 
Vector
ConstructForceVector(void)
{
   Node		*node;
   unsigned	active;
   unsigned	numnodes;
   unsigned	*dofs;
   unsigned	i,j,
		base_dof;
   unsigned	size;
   double	force;
   Vector	F;

   node     = problem.nodes;
   active   = problem.num_dofs;
   dofs     = problem.dofs_num;
   numnodes = problem.num_nodes;
   
   size = numnodes*active;

   F = CreateVector (size);

   if (F == NullVector)
      Fatal ("allocation error constructing global nodal force vector");

   for (i = 1 ; i <= size ; i++) 
      VectorData (F) [i] = 0;

   for (i = 1 ; i <= numnodes ; i++) {

      base_dof = active*(node[i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         force = 0.0;
         if (node[i] -> force != NULL) {
            if (node[i] -> force -> force[dofs[j]].value) 
               force += node[i] -> force -> force[dofs[j]].value;
         }
         if (node[i] -> eq_force != NULL) {
            if (node[i] -> eq_force[dofs[j]])
               force += node[i] -> eq_force[dofs[j]];
         }
         VectorData (F) [base_dof + j] = force; 
      }
   }

   return F;
}

/****************************************************************************
 *
 * Function:	ClearNodes
 *
 * Description:	sets all the displacements on the nodes to zero and
 *		clears the equivalent force vector
 *
 ****************************************************************************/

void
ClearNodes(void)
{
   unsigned	i,j;

   for (i = 1 ; i <= problem.num_nodes ; i++) {
      for (j = 1 ; j <= 6 ; j++) 
         problem.nodes [i] -> dx[j] = 0.0;

      if (problem.nodes [i] -> eq_force != NULL)
         for (j = 1 ; j <= 6 ; j++)
            problem.nodes [i] -> eq_force[j] = 0.0;
   }
}
  

/****************************************************************************
 *
 * Function:	FactorStiffnessMatrix
 *
 * Description: Factorizes the problem stiffness matrix in place 
 *
 ****************************************************************************/

int
FactorStiffnessMatrix(Vector K)
{
   Node		*node;
   unsigned      numnodes;
   unsigned	 active;
   unsigned	*dofs;
   unsigned	 i;
   unsigned	 size;

   active = problem.num_dofs;
   dofs = problem.dofs_pos;
   node = problem.nodes;
   numnodes = problem.num_nodes;

   size = active*numnodes;

   for (i = 1 ; i <= size ; i++) {
      if (VectorData (K) [K -> diag[i]] == 0.0) {
         error ("zero on the diagonal (row %d) of stiffness matrix",i);
         return 1;
      }
   }

   if (CroutFactorMatrix (K)) {
      error ("could not factorize global stiffness matrix");
      return 1;
   }

   return 0;
}

 
/****************************************************************************
 *
 * Function:	SolveForDisplacements
 *
 * Description: Solves the linear system Kd=F for the vector of global
 *		nodal displacements.  The system must not be singular
 *		(i.e. K and F should be condensed)
 *
 ****************************************************************************/

Vector
SolveForDisplacements(Vector K, Vector F)
{
   if (FactorStiffnessMatrix (K))
      return NullVector;

   if (CroutBackSolveMatrix (K, F)) {
      error ("could not back substitute for nodal displacements");
      return NullVector;
   }

   ApplyNodalDisplacements (F);

   return F;
}
 
/****************************************************************************
 *
 * Function:	SolveStaticLoadCases
 *
 * Description: builds a table of nodal DOF displacements for all defined
 *		loadcases
 *
 ****************************************************************************/

Matrix
SolveStaticLoadCases(Matrix K, Matrix Fbase)
{
   unsigned	 i,j,k;
   Matrix	 dtable;
   Matrix	 F;
   LoadCase	 lc;
   int		*mask;

   if (FactorStiffnessMatrix (K))
      return NullMatrix;

   F = CreateColumnVector (Mrows(Fbase));

   mask     = BuildConstraintMask ( );

   dtable = CreateFullMatrix (problem.num_loadcases, 
                              analysis.numnodes * analysis.numdofs);

   for (i = 1 ; i <= problem.num_loadcases ; i++) {
      lc = problem.loadcases [i];

      ZeroMatrix (F);
      AssembleLoadCaseForce (F, lc); 

	/*
	 * Fbase already contains everything we need to know
	 * about displacment BC so all we need to do is to
 	 * make sure that we add _nothing_ at all into the
 	 * force vector at any constrained DOF
	 */

      for (j = 1 ; j <= Mrows(F) ; j++)
         sdata(F, j, 1) = (mask [j] ? 0.0 :  mdata(F,j,1));

      AddMatrices (F, F, Fbase);

      if (CroutBackSolveMatrix (K, F)) {
         error ("could not back substitute for displacements in loadcase %s",
                 lc -> name);
         return NullMatrix;
      }

      for (k = 1 ; k <= analysis.numnodes ; k++) {
         for (j = 1 ; j <= analysis.numdofs ; j++) {
            sdata(dtable, i, (k-1)*analysis.numdofs + j) =
              mdata(F, GlobalDOF (analysis.nodes [k] -> number, analysis.dofs[j]), 1);
         }
      }
   }

   ZeroOffset (mask);      Deallocate (mask);

   return dtable;
}

/****************************************************************************
 *
 * Function:	SolveStaticLoadRange
 *
 * Description: builds a table of nodal DOF displacements for 
 *		input forcing at a single DOF over a range of
 *		force magnitudes
 *
 ****************************************************************************/

Matrix
SolveStaticLoadRange(Matrix K, Matrix Fbase)
{
   unsigned	 i,j,k;
   Matrix	 dtable;
   int		*mask;
   unsigned	 num_cases;
   double	 force;
   unsigned	 input_pos;
   Matrix	 F;

   if (FactorStiffnessMatrix (K))
      return NullMatrix;

   mask     = BuildConstraintMask ( );

   num_cases = (fabs(analysis.stop - analysis.start) + 0.5*fabs(analysis.step)) 
               / fabs(analysis.step) + 1;

   dtable = CreateFullMatrix (num_cases, analysis.numnodes * analysis.numdofs);
   
   input_pos = GlobalDOF (analysis.input_node -> number, analysis.input_dof);

   F = CreateColumnVector (Mrows(Fbase));

   for (i = 1 ; i <= num_cases ; i++) {

      force = analysis.start + (i - 1)*analysis.step;

      CopyMatrix (F, Fbase);
    
      if (!mask [input_pos]) 
         sdata(F, input_pos, 1) = mdata(F,input_pos,1) + force;
 
      if (CroutBackSolveMatrix (K, F)) {
         error ("could not back substitute for displacements");
         return NullMatrix;
      }

      for (k = 1 ; k <= analysis.numnodes ; k++) {
         for (j = 1 ; j <= analysis.numdofs ; j++) {
            sdata(dtable, i, (k-1)*analysis.numdofs + j) =
              mdata(F, GlobalDOF (analysis.nodes [k] -> number, analysis.dofs[j]), 1);
         }
      }
   }

   ZeroOffset (mask);      Deallocate (mask);

   return dtable;
}

/***************************************************************************
 *
 * Function:	AssembleLoadCaseForce
 *
 * Description:	
 *
 ****************************************************************************/

void
AssembleLoadCaseForce(Matrix F, LoadCase lc)
{
   unsigned	 active;
   unsigned	*dofs;
   unsigned	 i,j;
   unsigned	 base_dof;
   double	 force;

   active   = problem.num_dofs;
   dofs     = problem.dofs_num;
   
   for (i = 1 ; i <= lc -> numforces ; i++) {
     
      base_dof = active*(lc -> nodes [i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         force = 0.0;
         force += lc -> forces [i] -> force[dofs[j]].value;
/*
         if (node[i] -> eq_force != NULL) {
            if (node[i] -> eq_force[dofs[j]])
               force += node[i] -> eq_force[dofs[j]];
         }
*/
         sdata(F, base_dof + j, 1) = force;
      }
   }

   return;     
}

/***************************************************************************
 *
 * Function:	ApplyNodalDisplacements
 *
 * Description:	
 *
 ****************************************************************************/

void
ApplyNodalDisplacements(Matrix d)
{
   unsigned	i, j; 
   unsigned	base_dof;
   unsigned	prob_dof;
   Node	       *node;
   unsigned    *dofs;
   unsigned	numnodes; 
   unsigned	active;

   active = problem.num_dofs;
   dofs = problem.dofs_pos;
   node = problem.nodes;
   numnodes = problem.num_nodes;

   for (i = 1 ; i <= numnodes ; i++) {
      base_dof = active*(node[i] -> number - 1);
      prob_dof = 1;
      for (j = 1 ; j <= 6 ; j++) {
         if (dofs [j]) {
            node[i] -> dx[j] = mdata(d, base_dof + prob_dof, 1);
            prob_dof++;
         }
         else
            node[i] -> dx[j] = 0.0;
      }
   }

   return;
}

/***************************************************************************
 *
 * Function:	SolveForReactions
 *
 * Description:	Pretty simple really, first we find how many reaction
 *		forces there should be, then we allocate space for them,
 *		then we multiply rows of the stiffness matrix by the
 *		global displacement vector to get an entry that was
 *		previously unknown in the global force vector
 *
 ****************************************************************************/

unsigned
SolveForReactions(Vector K, Vector d, unsigned int *old_numbers, Reaction **reac)
{
   Node		*node;
   unsigned	numnodes,
		active,
		*dofs;
   unsigned	i,j,k,m,
		affected_dof,
		base_dof,
		num_reactions;
   unsigned	size;
   unsigned	address;
   double	sum;

   node = problem.nodes;
   numnodes = problem.num_nodes;
   active = problem.num_dofs;
   dofs = problem.dofs_num;

   size = active * numnodes;

	/*
	 * find the number of reactions and allocate some space for them
	 */

   num_reactions = 0; 
   for (i = 1 ; i <= numnodes ; i++) {
      for (j = 1 ; j <= active ; j++) {
         if (node[i] -> constraint -> constraint[dofs[j]] == 1) 
            num_reactions++; 
      }
   }

   if (num_reactions == 0) 
      return 0;

   if (!(*reac = Allocate(Reaction, num_reactions))) 
      Fatal ("allocation error finding reactions");

   UnitOffset (*reac);

   for (i = 1 ; i <= num_reactions ; i++) {
      if (!((*reac) [i] = Allocate (struct reaction, 1)))
         Fatal ("allocation error finding reactions");
   }    

   m = 1;
   for (i = 1 ; i <= numnodes ; i++) {

      base_dof = active*(node[i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         if (node[i] -> constraint -> constraint[dofs[j]] == 1) {
            sum = 0;
            affected_dof = base_dof + j;

            for (k = 1 ; k <= size ; k++) {
               address = ConvertRowColumn (affected_dof, k, K);
               if (address)
                   sum += VectorData (K) [address]*VectorData (d) [k];
            }

            if (old_numbers == NULL)
               (*reac) [m] -> node = node[i] -> number;
            else
               (*reac) [m] -> node = old_numbers [i];

            (*reac) [m] -> dof = dofs[j];
            if (node [i] -> eq_force != NULL)
               sum -= node [i] -> eq_force [dofs[j]];

            (*reac) [m++] -> force = sum; 
         }
      }
   }

   return num_reactions;
}

/***************************************************************************
 *
 * Function:	ElementSetup
 *
 * Description:	calls the appropriate function to assemble the element
 *		stiffness matrix for an element.  Each element stiffness
 *		function should be of the form: xxxSetup(element,mass_mode)
 *		where x is the element type (as defined in element.h)
 *		element is the element to assemble the stiffness for
 *		and mass_mode is 0 in static cases or 'c' or 'l' in
 *		transient analysis when a mass matrix should be formed.
 *
 ****************************************************************************/

int
ElementSetup(Element element, char mass_mode)
{
    int		status;
  
    status = element -> definition -> setup (element, mass_mode, 0);

    return status;
}

/***************************************************************************
 *
 * Function:	ElementStresses
 *
 * Description:	calls the element stress functions for all of the elements
 *
 ****************************************************************************/

int
ElementStresses(void)
{
    Element	*e;
    Node	*n;
    unsigned	 ne;
    unsigned	 nn;
    int		 i, j, status;
 
    e = problem.elements;
    ne = problem.num_elements;
    n = problem.nodes;
    nn = problem.num_nodes;
 
    status = 0;

    for (i = 1 ; i <= ne ; i++)
	status += e [i] -> definition -> stress (e [i]);

	/*
	 * compute the nodally averaged stresses
	 */

    for (i = 1 ; i <= nn ; i++) {
       if (n [i] -> stress && n [i] -> numelts) {
          for (j = 1 ; j <= 10 ; j++)
             n [i] -> stress [j] /= n [i] -> numelts;
       }
    }

    return status;
}

/***************************************************************************
 *
 * Function:	CheckAnalysisParameters
 *
 * Description:	Verifies that everything in the analysis parameters
 *		section is set (or at least the minimum number of
 *		things that we need) for the given analysis type.
 *
 ***************************************************************************/

int
CheckAnalysisParameters(AnalysisType mode)
{
   unsigned	count;

   count = 0;

   switch (mode) {

   case StaticLoadCases:
      if (analysis.numnodes <= 0) {
         error ("need to specify a node list for load cases w/static analysis");
         count ++;
      }

      if (analysis.numdofs <= 0) {
         error ("need to specify a DOF list for load cases w/static analysis");
         count ++;
      }
      break;

   case StaticLoadRange:
      if (analysis.numnodes == 0) {
         error ("need to specify node list for load ranges w/static analysis");
         count++;
      }

      if (analysis.numdofs == 0) {
         error ("need to specify DOF list for load ranges w/static analysis");
         count++;
      }

      if (analysis.input_dof == 0) {
         error ("need to specify input DOF for load ranges w/static analysis");
         count++;
      }

      if (analysis.input_node == 0) {
         error ("need to specify input node for load ranges w/static analysis");
         count++;
      }

      if (analysis.start == analysis.stop) {
         error ("start cannot equal stop for load ranges w/static analysis");
      }

      if (analysis.step == 0.0) {
         error ("step must be non-zero for load ranges w/static analysis");
         count++;
      }

      if (analysis.step * (analysis.stop - analysis.start) < 0) {
         error ("start and stop not compatible with step direction");
         count ++;
      }

      break;

   case StaticSubstitution:
      if (analysis.tolerance <= 0.0) {
         error ("tolerance must be defined for static substitution analysis");
         count++;
      }

      if (analysis.iterations <= 0) {
          error ("iterations must be defined for static substitution analysis");
          count ++;
      }

      if (analysis.load_steps <= 0) {
          error ("load steps must be defined for static substitution analysis");
          count ++;
      }
      break;

   case StaticIncremental:
      if (analysis.load_steps <= 0) {
         error ("load-steps must be defined for static substitution analysis");
         count++;
      }

      break;

   case Transient:
      if (analysis.mass_mode == 0) {
         error ("mass-mode must be defined for transient analysis");
         count++;
      }

      if (analysis.numnodes == 0) {
         error ("need to specify a node list for transient analysis");
         count++;
      }

      if (analysis.numdofs == 0) {
         error ("need to specify a list of DOFs for transient analysis");
         count++;
      }

      if (analysis.beta <= 0) {
         error ("beta musty be greater than zero for transient analysis");
         count++;
      }

      if (analysis.stop <= 0) {
         error ("duration needs to be greater than zero for transient analysis");
         count++;
      }

      if (analysis.step <= 0) {
         error ("time step needs to be greater than zero for transient analysis");
         count++;
      }

      break;

   case Spectral:
      if (analysis.mass_mode == 0) {
         error ("mass-mode must be defined for spectral analysis");
         count++;
      }

      if (analysis.numnodes == 0) {
         error ("need to specify an output node list for spectral analysis");
         count++;
      }

      if (analysis.numdofs == 0) {
         error ("need to specify a list of output DOFs for spectral analysis");
         count++;
      }

      if (analysis.step <= 0) {
         error ("frequency scale increment must be greater than zero");
         count ++;
      }

      if (analysis.start > analysis.stop) {
         error ("frequency range stop must be greater than start");
         count ++; 
      }

      break;

   case TransientThermal:
      if (analysis.mass_mode == 0) {
         error ("mass-mode must be defined for transient analysis");
         count++;
      }

      if (analysis.numnodes == 0) {
         error ("need to specify a node list for transient analysis");
         count++;
      }

      if (analysis.stop <= 0) {
         error ("duration needs to be greater than zero for transient analysis");
         count++;
      }

      if (analysis.step <= 0) {
         error ("time step needs to be greater than zero for transient analysis");
         count++;
      }

      break;

   case Modal:
      if (analysis.mass_mode == 0) {
         error ("mass-mode must be defined for modal analysis");
         count++;
      }

      break;


   default:
      break;

   }

   return count;
}

/***************************************************************************
 *
 * Function:    GlobalDOF
 *
 * Description: calculates the global DOF number based on a given local
 *              DOF (Tx ... Rz), a node number and a dofs map.  Zero is
 *              returned if the given local DOF is not active in the
 *              current dofs map.
 *
 ***************************************************************************/

int
GlobalDOF(unsigned int node, unsigned int dx)
{
   if (!problem.dofs_pos [dx]) 
      return 0;

   return problem.num_dofs*(node - 1) + problem.dofs_pos [dx];
}

/***************************************************************************
 *
 * Function:    LocalDOF
 *
 * Description: finds the node and local DOF of a given global DOF
 *
 ***************************************************************************/

void 
LocalDOF(unsigned int global_dof, unsigned int *node, unsigned int *local_dof)
{
   unsigned	i;
   unsigned	active;

   active = problem.num_dofs;
   i = (global_dof - 1) % active + 1;

   *local_dof = problem.dofs_num [i];
   *node = (global_dof - i) / active + 1;

   return;
}
 
/***************************************************************************
 *
 * Function:	FindForcedDOF
 *
 * Description:	builds a list of global DOF numbers which have some sort
 *		of input applied to them.  We make two passes rather than
 *		dealing with reallocation (and deallocation in the case
 *		of no forcing) 
 *
 ***************************************************************************/

void
FindForcedDOF(NodeDOF **forced, unsigned int *numforced)
{
   Node		*node;
   unsigned	numnodes;
   unsigned	*dofs;
   unsigned	active;
   unsigned	i, j;
   unsigned	n;

   node     = problem.nodes;
   active   = problem.num_dofs;
   node     = problem.nodes;
   numnodes = problem.num_nodes;
   dofs     = problem.dofs_num;

	/*
 	 * make one pass to figure out how many forced DOF 
	 * we are dealing with
	 */

   n = 0;
   for (i = 1 ; i <= numnodes ; i++) {
      for (j = 1 ; j <= active ; j++) {
         if (node [i] -> force != NULL)
            if (node [i] -> force -> force [dofs[j]].value || 
                node [i] -> force -> force [dofs[j]].expr ||
                node [i] -> force -> spectrum [dofs[j]].value ||
                node [i] -> force -> spectrum [dofs[j]].expr)

               n++;
      }
   }

   *numforced = n;

   if (n > 0) {
      *forced = Allocate (NodeDOF, n);
      UnitOffset (*forced);
   
      for (i = 1 ; i <= n ; i++)
         (*forced) [i] = AllocNew (struct nodeDOF);

      n = 1;
      for (i = 1 ; i <= numnodes ; i++) {
         for (j = 1 ; j <= active ; j++) {
            if (node [i] -> force != NULL) {
               if (node [i] -> force -> force [dofs[j]].value || 
                   node [i] -> force -> force [dofs[j]].expr ||
                   node [i] -> force -> spectrum [dofs[j]].value ||
                   node [i] -> force -> spectrum [dofs[j]].expr) {

                  (*forced) [n] -> dof = dofs[j];
                  (*forced) [n++] -> node = node[i];
               }
            }
         }
      }
   }
   else
      *forced = NULL;

   return;
} 

/****************************************************************************
 *
 * Function:	RemoveConstrainedMatrixDOF
 *
 * Description: a generalized form of RemoveConstrainedDOF for a single
 *		matrix.  If a matrix is input, it needs to be in compact
 *		column format.
 *
 ****************************************************************************/

Matrix
RemoveConstrainedMatrixDOF(Matrix a)
{
   Node		*node;
   unsigned	numnodes,
		active;
   unsigned	*dofs;
   unsigned	orig_dofs;
   unsigned	new_dofs;
   unsigned	height;
   char		*dof_map;
   Matrix	b;
   unsigned	size;
   unsigned	*diag;
   unsigned	start;
   unsigned	i, j, n, m,
		affected_dof,
		base_dof;

   numnodes = problem.num_nodes;
   node     = problem.nodes;
   active   = problem.num_dofs;
   dofs     = problem.dofs_num;
   
   orig_dofs = numnodes * active;
   j = 0;  /* gcc -Wall */

   dof_map = Allocate (char, orig_dofs);
   UnitOffset (dof_map);

   for (i = 1 ; i <= orig_dofs ; i++) 
      dof_map [i] = 1;

   size = a -> size;
   new_dofs = orig_dofs;

	/*
	 * first we get a count of the number of constrained DOF and build 
	 * a bitmap vector to tell us where they are in the original scheme
	 */

   for (i = 1 ; i <= numnodes ; i++) {
      base_dof = active*(node[i] -> number - 1);
      for (j = 1 ; j <= active ; j++) {

         if (node [i] -> constraint -> constraint [dofs[j]]) {
            affected_dof = base_dof + j;
            dof_map [affected_dof] = 0; 
            new_dofs --;

            if (IsCompact(a)) {
               if (affected_dof == 1)
                  size -= 1;
               else
                  size -= (a -> diag [affected_dof] - 
                           a -> diag [affected_dof - 1]);
            }
         }
      }
   }

	/*
	 * now we know how much space we are saving so we can allocate 
	 * space for the stiffness and mass matrices
	 */

   if (IsCompact(a)) {
      b = CreateCompactMatrix (new_dofs, new_dofs, size, NULL);
      diag = Allocate (unsigned, new_dofs);
      UnitOffset (diag);
  
      n = 1;
      m = 1;

	/*
	 * now we make a column loop over all of the original DOF to see
	 * which ones to copy through
	 */

      for (i = 1 ; i <= orig_dofs ; i++) {
         if (dof_map [i]) {
            if (i == 1) {
               height = 1;
               start = 1;
            }
            else {
               height = a -> diag [i] - a -> diag [i-1];
               start = a -> diag [i - 1] + 1;
            }

	/*
	 * check all the active rows in this column and copy them through
	 * if necessary - updating the diagonal address array for this
	 * column once we are through
	 */

            for (j = start ; j <= a -> diag [i] ; j++) {
               affected_dof = i - height  + 1 + (j - start);             

               if (dof_map [affected_dof]) {
                  b -> data [m][1] = a -> data [j][1];
                  m++; 
               }
            }
            diag [n++] = m - 1;
         }
      }

      b -> diag = diag;

   }
   else if (IsColumnVector(a)) {
      b = CreateColumnVector (new_dofs);

      m = 1;
      for (i = 1 ; i <= orig_dofs ; i++) {
         if (dof_map [i]) {
            sdata(b, m ,1) = mdata(a,i,j); 
            m ++;
         }
      }
   }
   else {
      b = CreateFullMatrix (new_dofs, new_dofs);

      m = 1;
      n = 1;
      for (i = 1 ; i <= orig_dofs ; i++) {

         if (dof_map [i]) {
            for (j = 1 ; j <= orig_dofs ; j++) {
               if (dof_map [j]) {
                  sdata(b, n, m) = mdata(a,i,j);
                  m ++;
               }
            }
            n ++;
         }
      } 
   }

   ZeroOffset (dof_map); 
   Deallocate (dof_map);

   return b;
}

/****************************************************************************
 *
 * Function:	ZeroConstrainedMatrixDOF
 *
 * Description: sort of like ZeroConstrainedDOF only simpler and more
 *		general because it only works on one thing at a time.
 *
 ****************************************************************************/

int
ZeroConstrainedMatrixDOF(Matrix b, Matrix a)
{
   Node		*node;
   unsigned	active;
   unsigned	*dofs;
   unsigned	i,j,
		affected_dof,
		base_dof;

   if (Mrows(a) != Mrows(b) || Mcols(a) != Mcols(b))
      return M_SIZEMISMATCH;

   node = problem.nodes;
   active = problem.num_dofs;
   dofs = problem.dofs_num;

   if (b != a)
      CopyMatrix (b, a);

   for (i = 1 ; i <= problem.num_nodes ; i++) {

      base_dof = active*(node[i] -> number - 1);

      for (j = 1 ; j <= active ; j++) {
         if (node[i] -> constraint -> constraint[dofs[j]]) {
            affected_dof = base_dof + j;

            if (IsCompact(b))
               b = ZeroCompactRowCol (b, affected_dof);
            else if (IsColumnVector(b))
               sdata(b, affected_dof, 1) = 0.0;
            else
               b = ZeroRowCol (b, affected_dof); 
         }
      }
   }

   return 0;
}
