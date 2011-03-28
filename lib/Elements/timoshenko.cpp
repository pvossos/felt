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

/************************************************************************
 * File:	timoshenko.c						*
 *									*
 * Description:	This file contains the definition structure and		*
 *		stiffness function for a Timoshenko beam element.	*
 ************************************************************************/

# include <math.h>
# include "fe.h"
# include "error.h"
# include "misc.h"
# include "definition.h"

	/*	
         * Here's the definition structure.  This is a very simple
         * implementation, 2 nodes, possible effect on 3 global DOF
         * per node.  We need to prototype the setup and stress functions
         * thing so we can use them in the definition declaration.
	 */

static int timoshenkoEltSetup (Element element, char mass_mode, int tangent);
static int timoshenkoEltStress (Element element);

void timoshenkoInit()
{
    Definition dd(new definition_t("timoshenko"));
    dd->setup = timoshenkoEltSetup;
    dd->stress = timoshenkoEltStress;
    dd->shape = Linear;
    dd->numnodes = 2;
    dd->shapenodes = 2;
    dd->numstresses = 2;
    dd->numdofs = 3;
    dd->dofs = {0, 1, 2, 6, 0, 0, 0};
    dd->retainK = 1;
    AddDefinition(dd);
}

	/*
	 * We'll declare these three functions as static because other
	 * people might use these same names for their element.  The
	 * static declaration makes them private to this file.
	 * There is nothing magical about them.  They could be called
	 * anything, your element may not use any local functions,
	 * etc., etc.  It's all a matter of preference and style. 
	 */

static Matrix LocalK (Element element);
static Matrix TransformMatrix (Element element);
static Matrix LumpedMassMatrix (Element element);
static Matrix ConsistentMassMatrix (Element element);
static int    EquivNodalForces (Element element, Matrix T, Vector *eq_stress, int mode);

	/*
	 * The element setup function (the one that the general
	 * routines actually call to define element -> K for
	 * Timoshenko beams).  We'll break it up a little more
	 * for our own internal purposes and call some functions
	 * of our own to actually fill out the guts of the thing.
	 */

static int
timoshenkoEltSetup(Element element, char mass_mode, int tangent)
{
    int 	   count;	/* a count of errors encountered	   */
    Matrix	   T;		/* transform matrix			   */
    Matrix	   khat;	/* local coordinate stiffness matrix	   */
    Matrix         mhat; 	/* local coordinate mass matrix		   */
	
	/*
	 * Since we're nice and we like to do as much error checking
	 * as possible, we'll also check to make sure that all necessary
	 * material properties are set for this element
	 */

    count = 0;
    if (element -> material -> E == 0.0) {
        error ("timoshenko element %d has 0.0 for Young's modulus (E)",
               element -> number);
        count++;
    }
    if (element -> material -> Ix == 0.0) {
        error ("timoshenko element %d has 0.0 for moment of inertia (Ix)",
               element -> number);
        count++;
    }
    if (element -> material -> G == 0.0) {
        error ("timoshenko element %d has 0.0 for bulk modulus (G)",
               element -> number);
        count++;
    }
    if (element -> material -> A == 0.0) {
        error ("timoshenko element %d has 0.0 for cross-section area (A)",
               element -> number);
        count++;
    }
   
	/*
	 * nu and kappa are somewhat special because we have to have
	 * at least one.  If we have nu we'll use it to estimate
	 * element -> kappa according to Cowper's (1966) approximation.
	 * If we have kappa we will of course always use it.  If
	 * we have neither, it's an error
	 */

    if (element -> material -> kappa == 0.0) {
        if (element -> material -> nu == 0.0) {
            error ("timoshenko element %d has 0.0 for Poisson's ratio (nu)",
                   element -> number);
            count++;
        }
        else {
            element -> material -> kappa = 
                 10.0*(1.0 + element -> material -> nu)/
                 (12.0 + 11.0*element -> material -> nu);
        }
    }

	/*
	 * if we've had any errors there is no point in continuing
	 */

    if (count)
        return count;

	/*
	 * get the local stiffness matrix and the transform matrix.
	 * we never allocated any memory for these two because the
	 * functions that we are calling will do that.
	 */

    khat = LocalK (element);
    if (!khat)
       return 1;
 
    T = TransformMatrix (element);

	/*
	 * We can form the element stiffness matrix now through
	 * some simple matrix multiplications.  I like to create
	 * it first just for clarity and to make sure the allocation
	 * went OK (the multiply routine could do it for me, but ...).
 	 * The multiply function here just saves us having to allocate
	 * some temporary space and actually transposing the transform
	 * matrix, it will simply carry out k = T(trans) * K * T
	 */

    if (!element -> K)
       element -> K = CreateMatrix (6,6);

    MultiplyAtBA (element -> K, T, khat);

	/*
	 * Things can get a little tricky here; we'll check if there
	 * are any distributed loads - if there are we need to resolve
	 * them and modify this element's node's equivalent nodal forces.
	 * If not we're home free and we can bail.  In this case I have
	 * relegated all the distributed load handling to a separate
	 * little module.
	 */

    if (element -> numdistributed > 0) {
        count = EquivNodalForces (element, T, NULL, 1);

        if (count)
            return count;
    }
	
	/* 
	 * there's also the possibility that some of this element's nodes
	 * have a hinged DOF ... that's easy to deal with we have a
	 * convenience routine to do all the checking and modifying for us.
	 */

    ResolveHingeConditions (element);

	/*
	 * check to see if we need to form a mass matrix
	 */

    if (mass_mode) {
        if (mass_mode == 'c')
           mhat = ConsistentMassMatrix (element);
        else if (mass_mode == 'l')
           mhat = LumpedMassMatrix (element);
        else
            mhat.reset();

        if (!mhat)
           return 1;

        if (!element -> M)
           element -> M = CreateMatrix (6,6);

        MultiplyAtBA (element -> M, T, mhat);
    }

	/*
	 * we made it here, everything must have worked!
	 */

    return 0;
}

	/*
	 * The element stress function that actually gets called
	 * to fill in the element's stress structures.  I realize
	 * that a lot of this seems awfully inefficient ... beam type
	 * elements are a bit of an anomaly because they need their
	 * stiffness matrix back and a bunch of local<->global transforms.
	 */

static int
timoshenkoEltStress(Element element)
{
    unsigned	    i;			/* loop index			 */
    int		    count;		/* count of errors		 */
    static Vector   dlocal;	/* local nodal displacements	 */
    static Vector   d;			/* global nodal displacements	 */
    static Vector   f;  		/* actual internal forces	 */
    Vector	    equiv;		/* equivalent nodal forces	 */
    Matrix	    T;			/* transform matrix		 */
    static Matrix   khat;		/* local stiffness matrix	 */
    static Matrix   Tt;			/* transpose of transform	 */

	/*
	 * our usual trick to set-up the matrices and vectors that
	 * we need memory for, but that are really just local to this function.
	 */

    if (!dlocal) {
        dlocal = CreateVector (4);
        d = CreateVector (6);
        f = CreateVector (4);
        khat = CreateMatrix (4,4);
        Tt = CreateMatrix (6,4);
    }
        
	/*
	 * set the number of points where we will calculate stresses.
	 * In this case it's two (one at each end).
	 */

    element -> ninteg = 2;

    	/*
	 * Fill out a vector with the element's nodal displacements.
	 * These are in global coordinates of course.  We need to
	 * do a transformation to get them into local coordinates.
	 */

    VectorData (d) [1] = element -> node[1] -> dx[Tx];
    VectorData (d) [2] = element -> node[1] -> dx[Ty];
    VectorData (d) [3] = element -> node[1] -> dx[Rz];
    VectorData (d) [4] = element -> node[1] -> dx[Tx];
    VectorData (d) [5] = element -> node[2] -> dx[Ty];
    VectorData (d) [6] = element -> node[2] -> dx[Rz];

    T = TransformMatrix (element);

    MultiplyMatrices (dlocal, T, d);
   
    	/*
	 * We already have the element stiffness matrix because we
	 * set element -> retainK = 1 in the definition structure.  This
	 * means that the global stiffness assembly routine didn't
	 * trash element -> K after it was done with it and we can
	 * use it again.  We will have to transform it back to local
	 * coordinates, however.
	 */

    TransposeMatrix (Tt, T);

    MultiplyAtBA (khat, Tt, element -> K);

	/*
	 * we can get the internal force vector through a simple
	 * matrix multiplication.
	 */

    MultiplyMatrices (f, khat, dlocal);

	/*
	 * Of course, we may need to modify that for equiv nodal forces
	 */

    if (element -> numdistributed > 0) {
        count = EquivNodalForces (element, Matrix(), &equiv, 2);
        if (count)
            return count;

        for (i = 1; i <= 4; i++)
            VectorData (f) [i] -= VectorData (equiv) [i];
    }

	/*
	 * set-up some memory for the stress structure and for the values
	 * in the stress structure.  We'll just use a quicky little
	 * convenience routine to do it for us.  It's important to
	 * set element -> ninteg before we call this function.
	 */

    SetupStressMemory (element);

	/*
	 * establish the location of the stresses and the magnitudes
	 * of the stresses at each point.  This particular loop
	 * only works because there are two stress points and two
	 * stress values at each point.
	 */


    for (i = 1; i <= 2; i++) {
        element -> stress[i] -> x = element -> node[i] -> x;
        element -> stress[i] -> y = element -> node[i] -> y;

        element -> stress[1] -> values[i] = VectorData (f)[i];
        element -> stress[2] -> values[i] = VectorData (f)[i+2];
    }
    
    return 0; 
}
   
	/* 
	 * Our own function to define the stiffness matrix in
	 * local coordinates.
	 */

static Matrix
LocalK(Element element)
{
    static Matrix k;	/* the local stiffness matrix	       */
    double	  L;		/* the element length		       */
    double	  phi;		/* bending stiffness / shear stiffness */
    double	  factor;	/* common factor in stiffness matrix   */

    	/*
	 * Our same old trick to make sure we only allocate this memory
	 * once and then use it over and over again each time we need to
	 * create an element of this kind.
	 */

    if (!k) 
        k = CreateMatrix (4,4);

    L = ElementLength (element, 2);
    if (L <= TINY) {
        error ("length of element %d is zero to machine precision",
               element -> number);
        return Matrix();
    }   

    phi = 12.0/(L*L)*(element -> material -> E*element -> material -> Ix/
                      (element -> material -> kappa*
                       element -> material -> G*element -> material -> A));

    	/*
	 * We know how the integration works out for the stiffness
	 * matrix so we're just going to fill it out an entry at
	 * a time.  For some element types this wouldn't be possible and
	 * we would do some integrating right here to fill in k.
	 * Also, because this is a symmetric matrix we'll just
	 * fill in everything above the diagonal and then use MirrorMatrix
	 */

   MatrixData (k) [1][1] = 12.0;
   MatrixData (k) [1][2] = 6.0*L;
   MatrixData (k) [1][3] = -12.0;
   MatrixData (k) [1][4] = 6.0*L;
   MatrixData (k) [2][2] = (4.0 + phi)*L*L;
   MatrixData (k) [2][3] = -6.0*L;
   MatrixData (k) [2][4] = (2.0 - phi)*L*L;
   MatrixData (k) [3][3] = 12.0;
   MatrixData (k) [3][4] = -6*L;
   MatrixData (k) [4][4] = (4.0 + phi)*L*L;

   MirrorMatrix (k);

	/*
	 * the above numbers aren't quite right, we've got a term out
	 * front of the matrix that we need to scale the entire
	 * matrix by
	 */

   factor = (element -> material -> E*element -> material -> Ix)/
            ((1.0 +phi)*L*L*L);

   ScaleMatrix (k, k, factor, 0.0);

	/*
	 * that's all for this part
	 */

   return k;
}

	/* 
	 * much like the local K function above all we do here is fill in
	 * the mass matrix - this function fills it out for consistent
	 * mass, the following function is used if the user wanted a lumped
	 * mass
	 */

static Matrix
ConsistentMassMatrix(Element element)
{
    static Matrix m;       /* the local stiffness matrix	          */
    double	  L;		  /* the element length		          */
    double	  phi;		  /* bending stiffness / shear stiffness  */
    double        phi2;           /* phi squared		          */
    double	  const1;	  /* constant term for rotational mass    */
    double	  const2;	  /* constant term for translational mass */

    if (!m) 
        m = CreateMatrix (4, 4);

	/*
	 * the constants that we'll need, including the constant terms
	 * in front of the rotational (first terms) and translational
	 * (second terms) portions of the matrix.
	 */

    L = ElementLength (element, 2);
    phi = 12.0/(L*L)*(element -> material -> E*element -> material -> Ix/
                      (element -> material -> kappa*
                       element -> material -> G*element -> material -> A));
    phi2 = phi*phi;
    const1 = element -> material -> rho * element -> material -> Ix /
             (30.0*(1.0 + phi)*(1.0 + phi)*L);
    const2 = element -> material -> rho * element -> material -> A * L /
             (210.0*(1.0 + phi)*(1.0 + phi));

	/*
	 * fill out the top half of the mass matrix (no need to 
	 * explicitly integrate of course)
	 */

   MatrixData (m) [1][1] = 36.0*const1 + (70.0*phi2 + 147.0*phi + 78)*const2;
   MatrixData (m) [1][2] = -L*(15.0*phi - 3.0)*const1 + 
                           (35.0*phi2 + 77.0*phi + 44.0)*L/4.0*const2;
   MatrixData (m) [1][3] = -36.0*const1 + (35.0*phi2 + 63.0*phi + 27.0)*const2;
   MatrixData (m) [1][4] = -L*(15.0*phi - 3.0)*const1 - 
                           (35.0*phi2 + 63.0*phi + 26.0)*L/4.0*const2;
   MatrixData (m) [2][2] = (10.0*phi2 + 5.0*phi + 4)*L*L*const1 +
                           (7.0*phi2 + 14.0*phi + 8.0)*L*L/4.0*const2;
   MatrixData (m) [2][3] = -MatrixData (m) [1][4];
   MatrixData (m) [2][4] = (5.0*phi2 - 5.0*phi - 1.0)*L*L*const1 -
                           (7.0*phi2 + 14.0*phi + 6.0)*L*L/4.0*const2;
   MatrixData (m) [3][3] = 36.0*const1 + (70.0*phi2 + 147.0*phi + 78.0)*const2;
   MatrixData (m) [3][4] = -MatrixData (m) [1][2];
   MatrixData (m) [4][4] = (10.0*phi2 + 5.0*phi + 4.0)*L*L*const1 +
                           (7.0*phi2 + 14.0*phi + 8.0)*L*L/4.0*const2;

	/*	
	 * complete it by mirroring
	 */

   MirrorMatrix (m);

	/*
	 * and we're done;
	 */

   return m;
}

static Matrix
LumpedMassMatrix(Element element)
{
    static Matrix m;       /* the local stiffness matrix	 */
    double	  factor ;	  /* constant term		 */
    double	  I_factor;
    double	  L;

    if (!m) {
        m = CreateMatrix (4, 4);
        ZeroMatrix (m);
    }

    L = ElementLength (element, 2);
    factor = L * element -> material -> rho * element -> material -> A / 2;
    I_factor = factor*L*L/12.0;

    MatrixData (m) [1][1] = factor;
    MatrixData (m) [2][2] = I_factor;
    MatrixData (m) [3][3] = factor;
    MatrixData (m) [4][4] = I_factor;

    return m;
}

	/*
	 * a simple little function to compute the transform matrix
	 * for a simple 2d beam element with no axial DOF.
	 * This should be a convenience routine, but none of the other
	 * elements actually use this one because they are more complicated.
	 */

static Matrix
TransformMatrix(Element element)
{
    double         s,c; 	/* direction cosines			*/
    static Matrix  T; 	/* transform matrix to return		*/
    double	   L;		/* element length			*/

	/*
	 * no surprise here, we only want to allocate memory for this
	 * guy once!
	 */

    if (!T) 
       T = CreateMatrix (4,6);

	/*
	 * This is a pretty sparse matrix so we'll just zero it out
	 * then fill in the few relevant entries. 
	 */

    ZeroMatrix (T);

    L = ElementLength (element, 2);
    c = (element -> node[2] -> x - element -> node[1] -> x) / L;
    s = (element -> node[2] -> y - element -> node[1] -> y) / L;

    MatrixData (T) [1][1] = -s;
    MatrixData (T) [1][2] = c;
    MatrixData (T) [2][3] = 1.0;
    MatrixData (T) [3][4] = -s;
    MatrixData (T) [3][5] = c;
    MatrixData (T) [4][6] = 1.0;

    return T;
}

	/*
  	 * We need to compute the equivalent nodal load
  	 * vector here. Just for convenience we are going to call 
	 * this function in two different ways (mode=1 and mode=2).
	 * The first way is for the element stiffness function
	 * which just wants to get the forces applied to the 
	 * element's nodes.  The second is for the stress routine
	 * which actually needs the equiv force vector in local coordinates.
	 * There are lots of ways to handle all these cases;
	 * see the Bernoulli beam elements for example. In mode 1,
	 * eq_stress can be NULL, in mode 2, T can be NULL.
	 */

/* passing it in saves a few FLOPs */ 
/* vector pointer to set in mode 2 */
/* mode of operation		   */

static int
EquivNodalForces(Element element, Matrix T, Vector *eq_stress, int mode)
{
    static Vector  equiv;	/* the equiv vector in local coord */
    static Vector  eq_global;		/* equiv in global coordinates     */
    double	   wa, wb;		/* values of load at nodes	   */
    double	   L;			/* the element length		   */
    unsigned	   i,j;			/* some loop conuters		   */
    double	   factor;		/* constant factor for sloped load */
    double	   phi;			/* bending / shear stiffness	   */
    int		   count;		/* error count			   */
    static Matrix  Tt;			/* transpose of transform matrixi  */

    if (!equiv) {
        equiv     = CreateVector (4);
        eq_global = CreateVector (6);
        Tt	  = CreateMatrix (6,4);
    }

    ZeroMatrix (equiv);

    count = 0;
    wa = wb = 0; /* gcc -Wall */

	/*
	 * Again, we want to do as much error checking and descriptive
	 * error reporting as possible.  Seem like overkill?  It probably
	 * is, but it's not hurting anybody either :-)
	 */

    if (element -> numdistributed > 2) {
       error ("Timoshenko beam element %d has more than 2 distributed loads",
              element -> number);
       count++;
    }

    L = ElementLength (element, 2);
    if (L <= TINY) {
        error ("length of element %d is zero to machine precision",
               element -> number);
        count++;
    }   

    for (i = 1; i <= element -> numdistributed; i++) {
        if (element -> distributed[i] -> value.size() != 2) {
            error ("Timoshenko beam element %d must have 2 values for load",
                   element -> number);
            count++;
        }

	/*
	 * We only want to deal with loads in the perpendicular (LocalY)
	 * direction ... this is a very simple instantiation of this
	 * element after all.
	 */

        if (element -> distributed[i] -> direction != LocalY &&
            element -> distributed[i] -> direction != Perpendicular) {
        
            error ("invalid direction for element %d distributed load",
                   element -> number);
            count++;
        }
              
	/*
	 * make sure that the user isn't try to apply part of this
	 * load to a non-existent node (some local node other than
	 * number 1 or 2)
	 */

        for (j = 1 ;j <= element -> distributed[i] -> value.size(); j++) {
            if (element -> distributed[i] -> value[j].node < 1 ||
                element -> distributed[i] -> value[j].node > 2) {

                error ("invalid node numbering for elt %d distributed load %s",
                       element -> number, element -> distributed[i] -> name.c_str());
                count++;
            }
        }

        if (element -> distributed[i] -> value[1].node ==
            element -> distributed[i] -> value[2].node) {

            error ("incorrect node numbering for elt %d distributed load %s",
                   element -> number, element -> distributed[i] -> name.c_str());
            count++;
        }
    }

	/* 
	 * Have we had any errors? If so bail out.
 	 */

    if (count) 
        return count;

    phi = 12.0/(L*L)*(element -> material -> E*element -> material -> Ix/
                      (element -> material -> kappa*
                       element -> material -> G*element -> material -> A));

	/*
	 * loop over all of the applied distributed loads, superposing
	 * the effects of each
	 */

    for (i = 1 ; i <= element -> numdistributed ; i++) {

	/*
	 * First we have to sort out what order the load values
	 * were supplied in.  We need to get it so that wa is
	 * the value on element node 1 and wb is the value on 
	 * element node 2.
	 */

        if (element -> distributed[i] -> value[1].node == 1) {
            wa = element -> distributed[i] -> value[1].magnitude;
            wb = element -> distributed[i] -> value[2].magnitude;
        }
        else if (element -> distributed[i] -> value[1].node == 2) {
            wb = element -> distributed[i] -> value[1].magnitude;
            wa = element -> distributed[i] -> value[2].magnitude;
        }

	/*
	 * Again, since we know how the integration turns out, we'll
	 * just go head and plug straight into the entries in the equiv
	 * vector.  The order of entries in equiv is Fy1,Mz1,Fy2,Mz2.
	 * There are three cases we need to deal with.  The first is 
	 * a uniform load.  The second two are sloped loads which we'll
	 * treat as the superposition of the uniform case and a case
	 * in which the load can be treated as q(x) = q0*(1 - x/L)
	 * (i.e., a load which goes from q0 to 0)
	 */

        if (wa == wb) {				/* uniform distributed load   */
            VectorData (equiv) [1] += wa*L/2.0;
            VectorData (equiv) [3] += wa*L/2.0;
            VectorData (equiv) [2] += wa*L*L/12.0;
            VectorData (equiv) [4] += -wa*L*L/12.0;
        }
        else if (fabs(wa) > fabs(wb)) {		/* load sloping node 1-node 2 */
            factor = (wa - wb)*L/120.0/(1.0 + phi);
            VectorData (equiv) [1] += wb*L/2.0 + factor*(42.0 + 40.0*phi);
            VectorData (equiv) [3] += wb*L/2.0 + factor*(18.0 + 20.0*phi);
            VectorData (equiv) [2] += wb*L*L/12.0 + factor*(6.0 + 5.0*phi)*L;
            VectorData (equiv) [4] += -wb*L*L/12.0 - factor*(4.0 + 5.0*phi)*L;
        }
	else if (fabs (wa) < fabs (wb)) {	/* load sloping node 2-node 1 */
            factor = (wb - wa)*L/120.0/(1.0 + phi);
            VectorData (equiv) [1] += wa*L/2.0 + factor*(18.0 + 20.0*phi);
            VectorData (equiv) [3] += wa*L/2.0 + factor*(42.0 + 40.0*phi);
            VectorData (equiv) [2] += wa*L*L/12.0 + factor*(4.0 + 5.0*phi)*L;
            VectorData (equiv) [4] += -wa*L*L/12.0 - factor*(6.0 + 5.0*phi)*L;
        } 
    }

	/*
	 * if this is mode 2, we're done, just hand the equiv vector 
	 * back by setting eq_stress.
	 */

    if (mode == 2) {
        *eq_stress = equiv;
        return 0;
    }

	/* 
	 * We have the load vector in local coordinates now.  
	 * All of this is taken care of by a convenience routine.
	 * What it is doing is checking if the eq_force array has been 
	 * allocated for this element's nodes.  If it hasn't it will set
	 * it up.  If it has it will do nothing and simply return
	 * to us.  It has to allocate space for six doubles (even
	 * though we will only ever use two entries for Timoshenko
	 * elements) because other element types may try to insert
	 * something into this array in different locations. Also,
	 * remember that we will access it as a standard array, 
	 * it's not a Vector or Matrix type.
	 */

    SetEquivalentForceMemory (element);

	/*
	 * The equiv vector has four things in it.  We need to transform
	 * these to global coordinate and then add them 
	 * incrementally into the eq_force [] array on the nodes
	 * because some other element may have also already added 
	 * something onto this node.  Note the use of Tx, Ty and Rz
	 * to access the eq_force array.  These are just enumerated
	 * so that they expand to 2 and 6 ... no real magic there, it
	 * is just little more intuitive to look at.
 	 */

    TransposeMatrix (Tt, T);
    MultiplyMatrices (eq_global, Tt, equiv);
    element -> node[1] -> eq_force[Tx] += VectorData (eq_global) [1];
    element -> node[1] -> eq_force[Ty] += VectorData (eq_global) [2];
    element -> node[1] -> eq_force[Rz] += VectorData (eq_global) [3];
    element -> node[2] -> eq_force[Tx] += VectorData (eq_global) [4];
    element -> node[2] -> eq_force[Ty] += VectorData (eq_global) [5];
    element -> node[2] -> eq_force[Rz] += VectorData (eq_global) [6];

    return 0;
}
