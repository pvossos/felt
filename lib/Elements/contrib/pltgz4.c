/*
Fri Mar 25 10:56:32 MET 1994

This file is for the FElt finite element analysis package.
Copyright (C) 1993,1994 Jason I. Gobat and Darren C. Atkinson

The PLTGZ4 element has to be checked by some benchmarks in the x-y plane.
It is the simplest possible triangular plate element I can provide.
It is integrated analytically. I did not check for errors after codeing.
It is provided for discussion only WITHOUT ANY WARRANTY.
 
Let me know if there is any interest for a more developed formulation.
(arbitary Poisson ratio, stress evaluation, arbitray location in 3D,
loads, anisitropic material, ...)

You should not remove the following headers in every further use related to
the PLTGZ4 element formulation.

Best wishes
Gerhard Zirwas

----------------------------------------------------------------------------
Lehrstuhl fuer Baumechanik, TU-Muenchen, Arcisstr. 21, D-80333 Muenchen

Dipl.-Ing. Gerhard Zirwas

priv to    Schwojerstr. 68c, D-82140 Olching
mail to    t5121ak@sunmail.lrz-muenchen.de
           or zirwas@jarrett.baume.bauwesen.tu-muenchen.de
phone to   +49 89 2105 8341 or +49 8142 40693 (private)
FAX to     +49 89 2105 8665
-------------------------------------- Confitemini domino quoniam bonus. ---
*/

/************************************************************************
 * File:	pltgz4.c						*
 *									*
 * Description: This file contains the definition structure and the	*
 *		stiffness function for an PLTGZ4			*
 *									*
 *		PLate elemet with 					*
 *		Triangular shape, where energy due to the shearmodulus	*
 *		G is 							*	
 *		Zeroed,	and the poisson ratio nu is simplified to 	*
 *		1/4							*
 *									*
 *		The energy is zero obviously only for the shears	*
 *  		involving the direction normal to the plate,		*
 * 		by locking the normals to the plate as normals 		*
 * 		after deformation. (Kirchhoff plate theory).		*
 *		The poisson ratio is simplified to nu=1/4 by		*
 *		setting the Lame coefficiant lambda=mu. (mu=G)		*
 ************************************************************************/

/* To add the element to Felt look into the Users Guide and Reference Manual
at 9.5 "Putting it all together"
The procedure is something like this:

copy this file to ../../FElt-2.0/lib/Elements/pltgz4.c

Add  PLTGZ4 to    ../../FElt-2.0/lib/Elements/elements.db

Do an make element.h in ../../FElt-2.0/lib/Elements

Add pltgz4.o to the Makefile in ../../FElt-2.0/lib/Elements which will then
look like:

OBJS	= misc.o beam.o beam3d.o cst.o truss.o iso_2d.o iso_quad.o\
          timoshenko.o pltgz4.o 

Go to the ../../FElt-2.0 and do a

make clean

and

make all

Now you should be able to run Felt with the plate element.
*/

# include <math.h>
# include <stdio.h>
# include "allocate.h"
# include "fe.h"
# include "error.h"
# include "../misc.h"

void    PLTGZ4LumpedMassMatrix ( );
Matrix  PLTGZ4LocalK 	    ( );
int	PLTGZ4ElementSetup ( );
int	PLTGZ4ElementStress    ( );
void	PLTGZ4mpl ( );

struct definition PLTGZ4Definition = {
    "PLTGZ4",
    PLTGZ4ElementSetup,
    PLTGZ4ElementStress,
    Planar, 			/* The shape of this element		   	*/
    3, 				/* 3 nodes per element			   	*/
    3, 				/* 3 nodes define the shape (triangl)	   	*/
    0, 				/* 0,6 magnitudes in each stress structure	*/
    3, 				/* 3 global DOF / node			   	*/
   {0, 3, 4, 5, 0, 0, 0},      	/* DOF 1 is Tz, DOF 2 is Rx DOF 3 is Ry .. 	*/
    0				/* retain stiffness after assembling	   	*/
};

int PLTGZ4ElementSetup (element, mass_mode)
   Element	element;
   char		mass_mode;
{
   unsigned		i;
   Vector		equiv;
   int			count;
   Matrix		Kb;
   double		factor;
   double		area;

   if (element -> material -> nu != 0.25) {
      error ("PLTGZ4 element %d has Poisson's ratio (nu=1/4=0.25)", element -> number);
      return 1;
   }
   if (element -> material -> E == 0) {
      error ("PLTGZ4 element %d has 0.0 for material modulus E  ", element -> number);
      return 1;
   }
   if (element -> material -> t == 0) {
      error ("PLTGZ4 element %d has 0.0 for thickness (t)", element -> number);
      return 1;
   }

   Kb = PLTGZ4LocalK (element, &area);
   if (Kb == NullMatrix)
      return 1;
   
   element -> K = CreateMatrix (9,9);
   if (element -> K == NullMatrix) 
      Fatal ("allocation error creating element %d stiffness matrix", element -> number);

   factor = 1.0;
   ScaleMatrix (element -> K, Kb, factor, 0.0);

   if (element -> K == NullMatrix)
      Fatal ("element %d K matrix is null after definition",element -> number);

	/*	
	 * form the element mass matrix if necessary (note that we only
	 * have a lumped formulation for now)
	 */

   if (mass_mode) {
      element -> M = CreateMatrix (9,9);
      if (element -> M == NullMatrix)
         AllocationError (element, "mass matrix");

      if (mass_mode == 'l')
         PLTGZ4LumpedMassMatrix (element, area);
      else
         PLTGZ4LumpedMassMatrix (element, area);
   }

   return 0;
}
/*--------------------------------------------------------------------------------*/
int PLTGZ4ElementStress (element)
   Element	element;
{
   static Vector	stress = NullMatrix,
			d;
   
   if (stress == NullMatrix) {
      stress = CreateVector (3);
      d = CreateVector (9);

      if (stress == NullMatrix || d == NullMatrix )
         Fatal ("allocation error in element %d stresses", element -> number);
   }

/* stress evaluation will be plugged in. */

   return 0;
} 
/*--------------------------------------------------------------------------------*/
void PLTGZ4LumpedMassMatrix (element, area)
   Element	element;
   double	area;
{
   double	factor;
   unsigned	i;

   ZeroMatrix (element -> M); 

   factor = (element -> material -> t * element -> material -> rho * area)/3.0;

      MatrixData (element -> M) [1][1] = factor;   
      MatrixData (element -> M) [4][4] = factor;   
      MatrixData (element -> M) [7][7] = factor;   
   
   return;
}
/*--------------------------------------------------------------------------------*/
Matrix PLTGZ4LocalK (element, area)
   Element	element;
   double	*area;
{
   static Matrix 	Kb = NullMatrix;
   double		xc1,yc1,
			xc2,yc2,
			xc3,yc3,
			A,
			factor;
   double		xb,yb,xc,yc,h,		/* shifted coordinates; thickness	*/
			Em,lambda,mu,n;		/* constants of lin. elast. material 	*/
   double  		K_mpl[10][10];		/* Matrix coded by MapleV 		*/
   unsigned		i1,i2;

   if (Kb == NullMatrix) {
      Kb = CreateMatrix (9,9);

      if (Kb == NullMatrix)
         Fatal ("allocation error creating element stiffness matrix");
   }

   ZeroMatrix (Kb);

   h = element -> material -> t;

   xc1 = element -> node[1] -> x;
   xc2 = element -> node[2] -> x;
   xc3 = element -> node[3] -> x;
   yc1 = element -> node[1] -> y;
   yc2 = element -> node[2] -> y;
   yc3 = element -> node[3] -> y;
 
   xb=xc2-xc1;
   yb=yc2-yc1;
   xc=xc3-xc1;
   yc=yc3-yc1;
 
   A = 0.5*(xb*yc-xc*yb);
   
   if (A < 0) {
      error("incorrect node ordering for element %d (must be ccw)",element -> number);
      return NullMatrix;
   }
   if (A == 0) {
      error ("area of element %d is zero, check node numbering",element -> number);
      return NullMatrix;
   }

   if (area != NULL)
      (*area) = A;
   
   PLTGZ4mpl(xb,yb,xc,yc,h,&K_mpl[0][0]);
    
   for (i1 = 1 ; i1 <= 9 ; i1++) {
     for (i2 = 1 ; i2 <= 9 ; i2++) {
       MatrixData (Kb) [i1][i2] = K_mpl[i1][i2];
     }
   }

   Em = element -> material -> E;
   n  = element -> material -> nu;

   if ( n >= 0.5) {
      error("poisson ratio for element %d (must be less then 0.5) ",element -> number);
      return NullMatrix;
   }

   lambda = -Em*n/(2.0*n*n+n-1.0);
   mu 	  =  Em/(n+1.0)/2;

   factor = mu;
   ScaleMatrix (Kb,Kb,factor,0.0);

   return Kb;
}


/*--------------------------------------------------------------------------------*/
void PLTGZ4mpl (xb,yb,xc,yc,h,TPLGZ4)
   double	xb,yb,xc,yc,h;
   double   	TPLGZ4[10][10];
{
   unsigned		i1,i2;
   double		xc2, xc3, xc4, xc5, xc6;
   double		xb2, xb3, xb4, xb5, xb6;
   double		yb2, yb3, yb4, yc5, yc6;
   double		yc2, yc3, yc4, yb5, yb6;

	/*
	 * I added these to try to speed up compilation time :-) (JG)
 	 */ 

   xc2 = xc2;
   xc3 = xc2*xc;
   xc4 = xc3*xc;
   xc5 = xc4*xc;
   xc6 = xc5*xc;

   xb2 = xb2;
   xb3 = xb3;
   xb4 = xb3*xb;
   xb5 = xb4*xb;
   xb6 = xb5*xb;

   yc2 = yc2;
   yc3 = yc2*yc;
   yc4 = yc3*yc;
   yc5 = yc4*yc;
   yc6 = yc5*yc;

   yb2 = yb2;
   yb3 = yb3;
   yb4 = yb3*yb;
   yb5 = yb4*yb;
   yb6 = yb5*yb;

      TPLGZ4[3][6] = -h*h*h*(-9.0*xb*xc5-27.0*xc*xb5+18.0*xc2
*xb4+15.0*xc3*xb3+3.0*xc6+6.0*xb6+123.0*yc2
*xc2*xb2+26.0*yc2*xc4+26.0*yc2*xb4+3.0*yc4*
xc2-3.0*yc4*xc*xb-52.0*yc2*xc3*xb-97.0*yc2*xc*xb3+6.0*
yb*yc3*xc*xb-90.0*yb*yc*xc3*xb+52.0*yb2*xb4+6.0*yb4
*xb2+90.0*yb*yc*xc2*xb2+78.0*yb*yc*xc*xb3-30.0*yb*xb2*yc3
-26.0*yb*yc*xc4+52.0*yb2*xc2*xb2-52.0*yb*yc*xb4+21.0*
yb2*yc2*xc*xb-6.0*xb*yc*xc*yb3-33.0*yb2*xc4+24.0*yc*xc2*
yb3-21.0*yb2*yc2*xc2+18.0*yb2*xb2*yc2+137.0*yb2*xc3*xb
-12.0*xb2*yc*yb3-15.0*xc*yb4*xb-182.0*yb2*xc*xb3-6.0*yb*
yc3*xc2+6.0*xc2*yb4+15.0*xb2*yc4)/pow(-xb*yc+xc*yb,
3.0)/432;
      TPLGZ4[6][4] = 0.0;
      TPLGZ4[1][2] = h*h*h*(92.0*xb*yc*xc*yb2+92.0*yb*yc2*xc*xb+26.0*yc2
*yc*xc2+58.0*xc2*yb3+21.0*yb*xc4-3.0*yb*yc4+6.0*yc2
*yc*yb2+6.0*yc2*yb3-3.0*yc*yb4+58.0*xb2*yc3-98.0*yc*
xc2*yb2+78.0*yb*yc2*xc2-98.0*yb*xb2*yc2-36.0*yb*xc3*xb+42.0*yb
*xc2*xb2+78.0*xb2*yc*yb2-104.0*xc*yb3*xb-24.0*yb*xc*xb3-104.0
*yc3*xc*xb-24.0*yc*xc3*xb+42.0*yc*xc2*xb2-36.0*yc*xc*xb3+3.0
*yc5+3.0*yc*xc4+3.0*yb5+3.0*yb*xb4+26.0*yb2
*yb*xb2+21.0*yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[9][3] = 0.0;
      TPLGZ4[9][6] = 0.0;
      TPLGZ4[7][9] = -h*h*h*(3.0*xb5+26.0*yb2*xb3+3.0*xb*yb4
-24.0*xc*yb4-12.0*yc2*xc*xb2+156.0*xb*yc2*xc2+136.0*xb*
xc2*yb2+12.0*xb*yc3*yb-12.0*xb*yc2*yb2+52.0*xb3*yc*yb-156.0*
xc*yb2*xb2+12.0*xb*yc*yb3+104.0*yc*xc3*yb+24.0*xc*yc3*yb
-48.0*xc*yc2*yb2+36.0*xc*yc*yb3-12.0*xc5-20.0*xb3*yc2+
30.0*xb*xc4-36.0*xc3*xb2+24.0*xc2*xb3+6.0*xb*yc4
-12.0*xc*xb4-64.0*xc3*yb2+92.0*xb2*yc*xc*yb-236.0*xb*yc*xc2*
yb-104.0*yc2*xc3-12.0*xc*yc4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[9][5] = 0.0;
      TPLGZ4[1][6] = -h*h*h*(6.0*xb5+52.0*yb2*xb3+6.0*xb*yb4
-3.0*xc*yb4-272.0*yc2*xc*xb2+156.0*xb*yc2*xc2+162.0*xb*
xc2*yb2-72.0*xb*yc3*yb+78.0*xb*yc2*yb2-208.0*xb3*yc*yb-234.0*
xc*yb2*xb2-48.0*xb*yc*yb3-104.0*yc*xc3*yb-24.0*xc*yc3*yb+
24.0*xc*yc2*yb2+12.0*xc*yc*yb3+3.0*xc5+110.0*xb3*yc2+
6.0*xb*xc4-48.0*xc3*xb2+90.0*xc2*xb3+30.0*xb*yc4
-51.0*xc*xb4+40.0*xc3*yb2+508.0*xb2*yc*xc*yb-184.0*xb*yc*xc2
*yb+26.0*yc2*xc3+3.0*xc*yc4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[2][1] = 0.0;
      TPLGZ4[6][9] = -h*h*h*(-27.0*xb2*xc4-9.0*xb*xc5-9.0*xc*
xb5-27.0*xc2*xb4+63.0*xc3*xb3+6.0*xc6+6.0*
xb6-182.0*yc2*xc2*xb2+52.0*yc2*xc4+26.0*yc2*xb4
+6.0*yc4*xc2+3.0*yc4*xc*xb-26.0*yc2*xc3*xb+97.0*
yc2*xc*xb3-42.0*yb*yc3*xc*xb-78.0*yb*yc*xc3*xb+52.0*yb2*xb4
+6.0*yb4*xb2+352.0*yb*yc*xc2*xb2-78.0*yb*yc*xc*xb3+
6.0*yb*xb2*yc3-52.0*yb*yc*xc4-182.0*yb2*xc2*xb2-52.0*yb*yc
*xb4+51.0*yb2*yc2*xc*xb-42.0*xb*yc*xc*yb3+26.0*yb2*xc4
+6.0*yc*xc2*yb3+18.0*yb2*yc2*xc2+18.0*yb2*xb2*yc2+97.0*
yb2*xc3*xb-12.0*xb2*yc*yb3+3.0*xc*yb4*xb-26.0*yb2*xc*
xb3-12.0*yb*yc3*xc2-3.0*xc2*yb4-3.0*xb2*yc4)/
pow(-xb*yc+xc*yb,3.0)/432;
      TPLGZ4[4][6] = h*h*h*(12.0*xb5+104.0*yb2*xb3+12.0*xb*yb4
-6.0*xc*yb4-136.0*yc2*xc*xb2+156.0*xb*yc2*xc2+12.0*xb*
xc2*yb2-36.0*xb*yc3*yb+48.0*xb*yc2*yb2-104.0*xb3*yc*yb-156.0*
xc*yb2*xb2-24.0*xb*yc*yb3-52.0*yc*xc3*yb-12.0*xc*yc3*yb+12.0
*xc*yc2*yb2-12.0*xc*yc*yb3-3.0*xc5+64.0*xb3*yc2+12.0*xb
*xc4-24.0*xc3*xb2+36.0*xc2*xb3+24.0*xb*yc4-30.0*
xc*xb4+20.0*xc3*yb2+236.0*xb2*yc*xc*yb-92.0*xb*yc*xc2*yb
-26.0*yc2*xc3-3.0*xc*yc4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[1][9] = -h*h*h*(3.0*xb5+26.0*yb2*xb3+3.0*xb*yb4
+30.0*xc*yb4+162.0*yc2*xc*xb2-234.0*xb*yc2*xc2-272.0*xb*
xc2*yb2+12.0*xb*yc3*yb+24.0*xb*yc2*yb2-104.0*xb3*yc*yb+156.0*
xc*yb2*xb2-24.0*xb*yc*yb3-208.0*yc*xc3*yb-48.0*xc*yc3*yb+
78.0*xc*yc2*yb2-72.0*xc*yc*yb3+6.0*xc5+40.0*xb3*yc2
-51.0*xb*xc4+90.0*xc3*xb2-48.0*xc2*xb3-3.0*xb*yc4
+6.0*xc*xb4+110.0*xc3*yb2-184.0*xb2*yc*xc*yb+508.0*xb*yc*xc2
*yb+52.0*yc2*xc3+6.0*xc*yc4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[1][1] = -h*h*h*(15.0*xb4-208.0*xc*yb2*xb-48.0*xc*xb2
*xb-48.0*yc*yb3-208.0*xb2*yc*yb+416.0*xb*yc*xc*yb+130.0*yc2*xc2+
104.0*xb2*yc2+104.0*xc2*yb2-48.0*xc3*xb+72.0*xc2*xb2-48.0*
yc3*yb+72.0*yc2*yb2-208.0*yc2*xc*xb-208.0*yc*xc2*yb+15.0*xc4
+15.0*yc4+130.0*yb2*xb2+15.0*yb4)/pow(-xb*yc+xc*yb,3.0)
/54;
      TPLGZ4[6][8] = h*h*h*(48.0*yc*xc2*xb3+6.0*xb*yc5+52.0*
yc3*xc3+6.0*xc*yc5+6.0*yc*xc5+58.0*xb3*yc3
-6.0*yc*xb*xc4-18.0*yc*xc3*xb2-39.0*yc*xc*xb4-130.0*
yc3*xc*xb2+15.0*yb*xc3*xb2-104.0*xb2*yc*xc*yb2+162.0*xb*yc*
xc2*yb2+313.0*yb*yc2*xc*xb2-104.0*yb*xb*yc2*xc2+13.0*xc3*yb2
*yb+6.0*yb*xb5+52.0*yb3*xb3+6.0*xb*yb5-3.0*xc*yb5
+6.0*yc*xb5-130.0*yb*xb3*yc2-9.0*xc*yc*yb4-9.0*yb*xb
*xc4+12.0*yb*xc2*xb3-39.0*yb*xb*yc4-15.0*yb*xc*xb4
-78.0*yb*yc2*xc3-15.0*yb*xc*yc4-18.0*xb*yc2*yb3+12.0*
xc*yc3*yb2+48.0*xb*yc3*yb2-78.0*xc*yb3*xb2-6.0*xb*yc*yb4
+15.0*xc*yc2*yb3-3.0*yb*xc5)/pow(-xb*yc+xc*yb,3.0)/432;
      TPLGZ4[5][8] = -h*h*h*(18.0*yc2*xc2*xb2+6.0*yc2*xc4-3.0*
yc2*xb4+52.0*yc4*xc2-52.0*yc4*xc*xb-12.0*yc2*
xc3*xb+6.0*yc2*xc*xb3-78.0*yb*yc3*xc*xb-42.0*yb*yc*xc3*xb
+6.0*yb2*xb4+52.0*yb4*xb2+6.0*pow(yb,6.0)+51.0*yb*yc*xc2*
xb2-42.0*yb*yc*xc*xb3+97.0*yb*xb2*yc3+3.0*yb*yc*xc4+18.0*
yb2*xc2*xb2+3.0*yb*yc*xb4+352.0*yb2*yc2*xc*xb-27.0*yc2*yb4
-9.0*yb*yc5+63.0*yc3*yb3-9.0*yc*yb5-78.0*xb*yc
*xc*yb3-3.0*yb2*xc4-27.0*yb2*yc4+6.0*yc6+97.0*
yc*xc2*yb3-182.0*yb2*yc2*xc2-182.0*yb2*xb2*yc2+6.0*yb2*
xc3*xb-26.0*xb2*yc*yb3-52.0*xc*yb4*xb-12.0*yb2*xc*xb2
*xb-26.0*yb*yc3*xc2+26.0*xc2*yb4+26.0*xb2*yc4)/pow(-
xb*yc+xc*yb,3.0)/432;
      TPLGZ4[9][1] = 0.0;
      TPLGZ4[9][4] = 0.0;
      TPLGZ4[7][8] = h*h*h*(92.0*xb*yc*xc*yb2-236.0*yb*yc2*xc*xb-104.0*
yc3*xc2-20.0*xc2*yb3+6.0*yb*xc4+30.0*yb*yc4-36.0*
yc3*yb2+24.0*yc2*yb3-12.0*yc*yb4-64.0*xb2*yc3-12.0
*yc*xc2*yb2+156.0*yb*yc2*xc2+136.0*yb*xb2*yc2+12.0*yb*xc3*xb
-12.0*yb*xc2*xb2-156.0*xb2*yc*yb2+52.0*xc*yb3*xb+12.0*yb*xc*xb2
*xb+104.0*yc3*xc*xb+24.0*yc*xc3*xb-48.0*yc*xc2*xb2+36.0*yc*xc*
xb3-12.0*yc5-12.0*yc*xc4+3.0*yb5+3.0*yb*pow(xb,4.0
)+26.0*yb3*xb2-24.0*yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[5][2] = 0.0;
      TPLGZ4[8][1] = 0.0;
      TPLGZ4[3][5] = h*h*h*(33.0*yc*xc2*xb3-6.0*xb*yc5+26.0*
yc3*xc3+3.0*xc*yc5+3.0*yc*xc5+13.0*xb3*yc3
-12.0*yc*xb*xc4-9.0*yc*xc3*xb2+19.0*yc3*xc*xb2-78.0*xb*
yc3*xc2-18.0*yb*xc3*xb2+130.0*xb2*yc*xc*yb2+51.0*xb*yc*xc2*
yb2+46.0*yb*yc2*xc*xb2+40.0*yb*xb*yc2*xc2+20.0*xc3*yb3+6.0*
yb*xb5+52.0*yb3*xb3+6.0*xb*yb5-12.0*xc*yb5
-3.0*yc*xb5+6.0*xc*yc*yb4+9.0*yb*xb*xc4+18.0*yb*xc2*
xb3+27.0*yb*xb*yc4-24.0*yb*xc*xb4-3.0*yb*xc*yc4+
12.0*xb*yc2*yb3-59.0*yc*xc3*yb2-27.0*xc*yc3*yb2-21.0*xb*
yc3*yb2-78.0*xb3*yc*yb2-156.0*xc*yb3*xb2-15.0*xb*yc*pow(yb
,4.0)+36.0*xc*yc2*yb3+3.0*yb*xc5+26.0*xb*xc2*yb3)/pow(-xb
*yc+xc*yb,3.0)/432;
      TPLGZ4[1][3] = -h*h*h*(3.0*xb5+26.0*yb2*xb3+3.0*xb*pow(yb,
4.0)+21.0*xc*yb4-98.0*yc2*xc*xb2+78.0*xb*yc2*xc2-98.0*xb*xc2*
yb2-36.0*xb*yc3*yb+42.0*xb*yc2*yb2-104.0*xb3*yc*yb+78.0*xc*
yb2*xb2-24.0*xb*yc*yb3-104.0*yc*xc3*yb-24.0*xc*yc3*yb+42.0*
xc*yc2*yb2-36.0*xc*yc*yb3+3.0*xc5+58.0*xb3*yc2-3.0*xb*
xc4+6.0*xc3*xb2+6.0*xc2*xb3+21.0*xb*yc4-3.0*xc*
xb4+58.0*xc3*yb2+92.0*xb2*yc*xc*yb+92.0*xb*yc*xc2*yb+26.0*
yc2*xc3+3.0*xc*yc4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[4][4] = -h*h*h*(6.0*xb4-52.0*xc*yb2*xb-12.0*xc*xb3
-12.0*yc*yb3-52.0*xb2*yc*yb+104.0*xb*yc*xc*yb+130.0*yc2*xc2+26.0*
xb2*yc2+26.0*xc2*yb2-12.0*xc3*xb+18.0*xc2*xb2-12.0*yc3*yb
+18.0*yc2*yb2-52.0*yc2*xc*xb-52.0*yc*xc2*yb+15.0*xc4+15.0*pow(
yc,4.0)+52.0*yb2*xb2+6.0*yb4)/pow(-xb*yc+xc*yb,3.0)/54;
      TPLGZ4[4][9] = h*h*h*(6.0*xb5+52.0*yb2*xb3+6.0*xb*pow(yb,
4.0)+6.0*xc*yb4+150.0*yc2*xc*xb2-78.0*xb*yc2*xc2-136.0*xb*xc2
*yb2+24.0*xb*yc3*yb+12.0*xb*yc2*yb2-52.0*xb3*yc*yb-12.0*xb*yc*
yb3-104.0*yc*xc3*yb-24.0*xc*yc3*yb+30.0*xc*yc2*yb2-36.0*xc*
yc*yb3-6.0*xc5+20.0*xb3*yc2-21.0*xb*xc4+54.0*xc2
*xc*xb2-24.0*xc2*xb3+3.0*xb*yc4-6.0*xc*xb4+46.0*xc2
*xc*yb2-92.0*xb2*yc*xc*yb+272.0*xb*yc*xc2*yb-52.0*yc2*xc3-6.0*xc*
yc4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[8][2] = 0.0;
      TPLGZ4[7][5] = 0.0;
      TPLGZ4[2][7] = h*h*h*(-28.0*xb*yc*xc*yb2-64.0*yb*yc2*xc*xb-52.0*yc2
*yc*xc2-38.0*xc2*yb3+12.0*yb*xc4+24.0*yb*yc4-12.0*
yc3*yb2+6.0*yc2*yb3-21.0*yc*yb4-20.0*xb2*yc3-20.0*
yc*xc2*yb2+156.0*yb*yc2*xc2+118.0*yb*xb2*yc2-30.0*yb*xc2*xb2
-234.0*xb2*yc*yb2+52.0*xc*yb3*xb+12.0*yb*xc*xb3+52.0*yc3*xc*
xb+12.0*yc*xc3*xb-12.0*yc*xc2*xb2+36.0*yc*xc*xb3-6.0*yc5
-6.0*yc*xc4+3.0*yb5+3.0*yb*xb4+26.0*yb3*xb2-33.0
*yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[7][6] = 0.0;
      TPLGZ4[2][8] = -h*h*h*(18.0*yc2*xc2*xb2+6.0*yc2*xc4+15.0*
yc2*xb4+52.0*yc4*xc2-52.0*yc4*xc*xb-12.0*yc2*
xc3*xb-30.0*yc2*xc*xb3+78.0*yb*yc3*xc*xb-6.0*yb*yc*xc3*xb
+3.0*yb2*xb4+26.0*yb4*xb2+3.0*pow(yb,6.0)+21.0*yb*yc*xc2*
xb2+6.0*yb*yc*xc*xb3-97.0*yb*xb2*yc3-15.0*yb*yc*xc4-21.0*
yb2*xc2*xb2-3.0*yb*yc*xb4+90.0*yb2*yc2*xc*xb-27.0*yb*pow(yc,
5.0)+15.0*yc3*yb3-9.0*yc*yb5-90.0*xb*yc*xc*yb3+6.0*yb2
*xc4+18.0*yb2*yc4+6.0*yc6+137.0*yc*xc2*yb3+
52.0*yb2*yc2*xc2+123.0*yb2*xb2*yc2+24.0*yb2*xc3*xb-52.0*
xb2*yc*yb3-26.0*xc*yb4*xb-6.0*yb2*xc*xb3-182.0*yb*yc2
*yc*xc2-33.0*xc2*yb4+26.0*xb2*yc4)/pow(-xb*yc+xc*yb,3.0)
/432;
      TPLGZ4[1][8] = h*h*h*(-184.0*xb*yc*xc*yb2+508.0*yb*yc2*xc*xb+52.0*
yc3*xc2+40.0*xc2*yb3-3.0*yb*xc4-51.0*yb*yc4+90.0*
yc3*yb2-48.0*yc2*yb3+6.0*yc*yb4+110.0*xb2*yc3+
162.0*yc*xc2*yb2-234.0*yb*yc2*xc2-272.0*yb*xb2*yc2+12.0*yb*xc3
*xb+24.0*yb*xc2*xb2+156.0*xb2*yc*yb2-104.0*xc*yb3*xb-24.0*yb*xc*
xb3-208.0*yc3*xc*xb-48.0*yc*xc3*xb+78.0*yc*xc2*xb2-72.0*yc*
xc*xb3+6.0*yc5+6.0*yc*xc4+3.0*yb5+3.0*yb*pow(xb,
4.0)+26.0*yb3*xb2+30.0*yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[2][4] = -h*h*h*(64.0*xb*yc*xc*yb2+28.0*yb*yc2*xc*xb-26.0*yc2
*yc*xc2+20.0*xc2*yb3+33.0*yb*xc4+21.0*yb*yc4-6.0*yc2
*yc*yb2+12.0*yc2*yb3-24.0*yc*yb4+38.0*xb2*yc3-118.0*yc*
xc2*yb2+234.0*yb*yc2*xc2+20.0*yb*xb2*yc2-36.0*yb*xc3*xb+12.0*
yb*xc2*xb2-156.0*xb2*yc*yb2-52.0*xc*yb3*xb-12.0*yb*xc*xb3
-52.0*yc3*xc*xb-12.0*yc*xc3*xb+30.0*yc*xc2*xb2-3.0*yc5
-3.0*yc*xc4+6.0*yb5+6.0*yb*xb4+52.0*yb3*xb2-12.0
*yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[8][6] = 0.0;
      TPLGZ4[5][3] = 0.0;
      TPLGZ4[6][6] = -h*h*h*(3.0*xb2*xc4-36.0*xc*xb5+39.0*
xc2*xb4-18.0*xc3*xb3+3.0*xc6+12.0*xb6+99.0*
yc2*xc2*xb2+26.0*yc2*xc4+76.0*yc2*xb4+3.0*yc4
*xc2+6.0*yc4*xc*xb+26.0*yc2*xc3*xb-180.0*yc2*xc*xb3
-60.0*yb*yc3*xc*xb-120.0*yb*yc*xc3*xb+104.0*yb2*xb4+12.0*
yb4*xb2-82.0*yb*yc*xc2*xb2+264.0*yb*yc*xc*xb3-48.0*yb*xb2*
yc3-26.0*yb*yc*xc4-2.0*yb2*xc2*xb2-104.0*yb*yc*xb4+
12.0*yb2*yc2*xc*xb-24.0*xb*yc*xc*yb3+47.0*yb2*xc4+18.0*yc*
xc2*yb3+27.0*yb2*yc2*xc2+60.0*yb2*xb2*yc2+106.0*yb2*xc2
*xc*xb-24.0*xb2*yc*yb3-12.0*xc*yb4*xb-208.0*yb2*xc*xb3
-6.0*yb*yc3*xc2+3.0*xc2*yb4+36.0*xb2*yc4)/pow(-xb*yc
+xc*yb,3.0)/432;
      TPLGZ4[5][5] = -h*h*h*(27.0*yc2*xc2*xb2+3.0*yc2*xc4+3.0*
yc2*xb4+26.0*yc4*xc2-26.0*yc4*xc*xb-6.0*yc2*xc2
*xc*xb+18.0*yc2*xc*xb3-120.0*yb*yc3*xc*xb-60.0*yb*yc*xc3*xb+
12.0*yb2*xb4+104.0*yb4*xb2+12.0*pow(yb,6.0)+12.0*yb*yc*
xc2*xb2-24.0*yb*yc*xc*xb3+106.0*yb*xb2*yc3+6.0*yb*yc*pow(xc,4.0
)+60.0*yb2*xc2*xb2-12.0*yb*yc*xb4-82.0*yb2*yc2*xc*xb+39.0*
yc2*yb4-18.0*yc3*yb3-36.0*yc*yb5+264.0*xb*yc*xc*
yb3+36.0*yb2*xc4+3.0*yb2*yc4+3.0*yc6-180.0*yc*
xc2*yb3+99.0*yb2*yc2*xc2-2.0*yb2*xb2*yc2-48.0*yb2*xc3
*xb-208.0*xb2*yc*yb3-104.0*xc*yb4*xb-24.0*yb2*xc*xb3+26.0
*yb*yc3*xc2+76.0*xc2*yb4+47.0*xb2*yc4)/pow(-xb*yc+xc
*yb,3.0)/432;
      TPLGZ4[1][7] = h*h*h*(12.0*xb4-104.0*xc*yb2*xb-24.0*xc*xb3
-24.0*yc*yb3-104.0*xb2*yc*yb+208.0*xb*yc*xc*yb+26.0*yc2*xc2+52.0*
xb2*yc2+52.0*xc2*yb2-24.0*xc3*xb+36.0*xc2*xb2-24.0*yc3*yb
+36.0*yc2*yb2-104.0*yc2*xc*xb-104.0*yc*xc2*yb+3.0*xc4+3.0*pow(
yc,4.0)+104.0*yb2*xb2+12.0*yb4)/pow(-xb*yc+xc*yb,3.0)/54;
      TPLGZ4[4][1] = 0.0;
      TPLGZ4[5][7] = h*h*h*(-272.0*xb*yc*xc*yb2+92.0*yb*yc2*xc*xb-52.0*
yc3*xc2-46.0*xc2*yb3-6.0*yb*xc4+6.0*yb*yc4+24.0*
yc3*yb2-54.0*yc2*yb3+21.0*yc*yb4-20.0*xb2*yc3+
136.0*yc*xc2*yb2-150.0*yb*xb2*yc2+36.0*yb*xc3*xb-30.0*yb*xc2*
xb2+78.0*xb2*yc*yb2+104.0*xc*yb3*xb+24.0*yb*xc*xb3+52.0*yc2
*yc*xc*xb+12.0*yc*xc3*xb-12.0*yc*xc2*xb2-24.0*yc*xc*xb3-6.0*pow(
yc,5.0)-6.0*yc*xc4+6.0*yb5+6.0*yb*xb4+52.0*yb3*
xb2-3.0*yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[4][8] = -h*h*h*(-92.0*xb*yc*xc*yb2+272.0*yb*yc2*xc*xb-52.0*
yc3*xc2+20.0*xc2*yb3+3.0*yb*xc4-21.0*yb*yc4+54.0*
yc3*yb2-24.0*yc2*yb3-6.0*yc*yb4+46.0*xb2*yc3+150.0
*yc*xc2*yb2-78.0*yb*yc2*xc2-136.0*yb*xb2*yc2+24.0*yb*xc3*xb+
12.0*yb*xc2*xb2-52.0*xc*yb3*xb-12.0*yb*xc*xb3-104.0*yc3*xc*
xb-24.0*yc*xc3*xb+30.0*yc*xc2*xb2-36.0*yc*xc*xb3-6.0*yc5
-6.0*yc*xc4+6.0*yb5+6.0*yb*xb4+52.0*yb3*xb2+6.0*
yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[3][8] = h*h*h*(-18.0*yc*xc2*xb3-12.0*xb*yc5+52.0*
yc3*xc3+6.0*xc*yc5+6.0*yc*xc5+20.0*xb3*yc3
-24.0*yc*xb*xc4+18.0*yc*xc3*xb2+9.0*yc*xc*xb4+26.0*yc2
*yc*xc*xb2-156.0*xb*yc3*xc2+33.0*yb*xc3*xb2+40.0*xb2*yc*xc*
yb2+46.0*xb*yc*xc2*yb2+51.0*yb*yc2*xc*xb2+130.0*yb*xb*yc2*xc2+
13.0*xc3*yb3+3.0*yb*xb5+26.0*yb3*xb3+3.0*xb*pow(yb,
5.0)-6.0*xc*yb5+3.0*yc*xb5-59.0*yb*xb3*yc2+27.0*xc*yc*
yb4-9.0*yb*xc2*xb3+6.0*yb*xb*yc4-12.0*yb*xc*xb4
-78.0*yb*yc2*xc3-15.0*yb*xc*yc4-27.0*xb*yc2*yb3+12.0*xc*
yc3*yb2+36.0*xb*yc3*yb2-78.0*xc*yb3*xb2-3.0*xb*yc*pow(yb,
4.0)-21.0*xc*yc2*yb3-3.0*yb*xc5+19.0*xb*xc2*yb3)/pow(-xb*
yc+xc*yb,3.0)/432;
      TPLGZ4[3][2] = 0.0;
      TPLGZ4[7][7] = -h*h*h*(15.0*xb4-52.0*xc*yb2*xb-12.0*xc*xb3
-12.0*yc*yb3-52.0*xb2*yc*yb+104.0*xb*yc*xc*yb+52.0*yc2*xc2+26.0*
xb2*yc2+26.0*xc2*yb2-12.0*xc3*xb+18.0*xc2*xb2-12.0*yc3*yb
+18.0*yc2*yb2-52.0*yc2*xc*xb-52.0*yc*xc2*yb+6.0*xc4+6.0*pow(yc,
4.0)+130.0*yb2*xb2+15.0*yb4)/pow(-xb*yc+xc*yb,3.0)/54;
      TPLGZ4[8][7] = 0.0;
      TPLGZ4[7][2] = 0.0;
      TPLGZ4[8][4] = 0.0;
      TPLGZ4[9][2] = 0.0;
      TPLGZ4[2][3] = h*h*h*(-27.0*yc*xc2*xb3-6.0*xb*yc5+26.0*
yc3*xc3+3.0*xc*yc5+3.0*yc*xc5+43.0*xb3*yc3
-12.0*yc*xb*xc4+15.0*yc*xc3*xb2+33.0*yc*xc*xb4-5.0*yc2
*yc*xc*xb2-78.0*xb*yc3*xc2-27.0*yb*xc3*xb2+426.0*xb2*yc*xc*
yb2-277.0*xb*yc*xc2*yb2-277.0*yb*yc2*xc*xb2+426.0*yb*xb*yc2*xc2+
43.0*xc3*yb3+3.0*yb*xb5+26.0*yb3*xb3+3.0*xb*pow(yb,
5.0)-6.0*xc*yb5-6.0*yc*xb5-5.0*yb*xb3*yc2+33.0*xc*yc*
yb4+33.0*yb*xb*xc4+15.0*yb*xc2*xb3+33.0*yb*xb*pow(yc,4.0
)-12.0*yb*xc*xb4-78.0*yb*yc2*xc3-12.0*yb*xc*yc4+15.0*xb*
yc2*yb3-5.0*yc*xc3*yb2+15.0*xc*yc3*yb2-27.0*xb*yc3*
yb2-78.0*xb3*yc*yb2-78.0*xc*yb3*xb2-12.0*xb*yc*yb4-27.0
*xc*yc2*yb3-6.0*yb*xc5-5.0*xb*xc2*yb3)/pow(-xb*yc+xc*yb,
3.0)/432;
      TPLGZ4[7][1] = 0.0;
      TPLGZ4[7][3] = 0.0;
      TPLGZ4[3][4] = h*h*h*(6.0*xb5+52.0*yb2*xb3+6.0*xb*pow(yb,
4.0)-12.0*xc*yb4-118.0*yc2*xc*xb2+234.0*xb*yc2*xc2+20.0*xb*
xc2*yb2-36.0*xb*yc3*yb+12.0*xb*yc2*yb2-52.0*xb3*yc*yb-156.0*
xc*yb2*xb2-12.0*xb*yc*yb3-52.0*yc*xc3*yb-12.0*xc*yc3*yb+30.0
*xc*yc2*yb2-3.0*xc5+20.0*xb3*yc2+21.0*xb*xc4-6.0*
xc3*xb2+12.0*xc2*xb3+33.0*xb*yc4-24.0*xc*xb4+38.0
*xc3*yb2+64.0*xb2*yc*xc*yb+28.0*xb*yc*xc2*yb-26.0*yc2*xc3-3.0
*xc*yc4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[6][5] = 0.0;
      TPLGZ4[6][3] = 0.0;
      TPLGZ4[8][8] = -h*h*h*(60.0*yc2*xc2*xb2+12.0*yc2*xc4+36.0
*yc2*xb4+104.0*yc4*xc2-104.0*yc4*xc*xb-24.0*yc2*
xc3*xb-48.0*yc2*xc*xb3+264.0*yb*yc3*xc*xb-24.0*yb*yc*xc3*
xb+3.0*yb2*xb4+26.0*yb4*xb2+3.0*pow(yb,6.0)+12.0*yb*yc*
xc2*xb2-60.0*yb*yc*xc*xb3-180.0*yb*xb2*yc3-12.0*yb*yc*pow(xc,
4.0)+27.0*yb2*xc2*xb2+6.0*yb*yc*xb4-82.0*yb2*yc2*xc*xb+3.0*
yc2*yb4-36.0*yb*yc5-18.0*yc3*yb3-120.0*xb*yc*xc*
yb3+3.0*yb2*xc4+39.0*yb2*yc4+12.0*yc6+106.0*yc
*xc2*yb3-2.0*yb2*yc2*xc2+99.0*yb2*xb2*yc2+18.0*yb2*xc2
*xc*xb+26.0*xb2*yc*yb3-26.0*xc*yb4*xb-6.0*yb2*xc*xb3
-208.0*yb*yc3*xc2+47.0*xc2*yb4+76.0*xb2*yc4)/pow(-xb
*yc+xc*yb,3.0)/432;
      TPLGZ4[4][3] = 0.0;
      TPLGZ4[2][6] = h*h*h*(36.0*yc*xc2*xb3+3.0*xb*yc5+26.0*
yc3*xc3+3.0*xc*yc5+3.0*yc*xc5+20.0*xb3*yc3
-3.0*yc*xb*xc4-27.0*yc*xc3*xb2+6.0*yc*xc*xb4-59.0*yc2
*yc*xc*xb2-21.0*yb*xc3*xb2+130.0*xb2*yc*xc*yb2+46.0*xb*yc*xc2*
yb2+51.0*yb*yc2*xc*xb2+40.0*yb*xb*yc2*xc2+13.0*xc3*yb3+6.0*
yb*xb5+52.0*yb3*xb3+6.0*xb*yb5-3.0*xc*yb5
-12.0*yc*xb5+26.0*yb*xb3*yc2+27.0*yb*xb*xc4+12.0*yb*
xc2*xb3+9.0*yb*xb*yc4-15.0*yb*xc*xb4-78.0*yb*yc2*xc2
*xc-12.0*yb*xc*yc4+18.0*xb*yc2*yb3+19.0*yc*xc3*yb2-9.0*xc
*yc3*yb2-18.0*xb*yc3*yb2-156.0*xb3*yc*yb2-78.0*xc*yb3
*xb2-24.0*xb*yc*yb4+33.0*xc*yc2*yb3-6.0*yb*xc5)/pow(-
xb*yc+xc*yb,3.0)/432;
      TPLGZ4[9][7] = 0.0;
      TPLGZ4[9][9] = -h*h*h*(39.0*xb2*xc4-36.0*xb*xc5+3.0*
xc2*xb4-18.0*xc3*xb3+12.0*xc6+3.0*xb6-2.0*
yc2*xc2*xb2+104.0*yc2*xc4+47.0*yc2*xb4+12.0*pow(yc,
4.0)*xc2-12.0*yc4*xc*xb-208.0*yc2*xc3*xb+106.0*yc2*xc*xb2
*xb-24.0*yb*yc3*xc*xb+264.0*yb*yc*xc3*xb+26.0*yb2*xb4+3.0*
yb4*xb2-82.0*yb*yc*xc2*xb2-120.0*yb*yc*xc*xb3+18.0*yb*xb2*
yc3-104.0*yb*yc*xc4+99.0*yb2*xc2*xb2-26.0*yb*yc*xb4+
12.0*yb2*yc2*xc*xb-60.0*xb*yc*xc*yb3+76.0*yb2*xc4-48.0*yc*
xc2*yb3+60.0*yb2*yc2*xc2+27.0*yb2*xb2*yc2-180.0*yb2*xc2
*xc*xb-6.0*xb2*yc*yb3+6.0*xc*yb4*xb+26.0*yb2*xc*xb3-24.0*
yb*yc3*xc2+36.0*xc2*yb4+3.0*xb2*yc4)/pow(-xb*yc+xc*
yb,3.0)/432;
      TPLGZ4[4][7] = h*h*h*(3.0*xb4+52.0*xc*yb2*xb+12.0*xc*xb3+
12.0*yc*yb3+52.0*xb2*yc*yb-104.0*xb*yc*xc*yb+26.0*yc2*xc2-26.0*xb2
*yc2-26.0*xc2*yb2+12.0*xc3*xb-18.0*xc2*xb2+12.0*yc3*yb-18.0
*yc2*yb2+52.0*yc2*xc*xb+52.0*yc*xc2*yb+3.0*xc4+3.0*yc4+
26.0*yb2*xb2+3.0*yb4)/pow(-xb*yc+xc*yb,3.0)/54;
      TPLGZ4[8][3] = 0.0;
      TPLGZ4[1][4] = h*h*h*(3.0*xb4-104.0*xc*yb2*xb-24.0*xc*xb3
-24.0*yc*yb3-104.0*xb2*yc*yb+208.0*xb*yc*xc*yb+104.0*yc2*xc2+52.0*
xb2*yc2+52.0*xc2*yb2-24.0*xc3*xb+36.0*xc2*xb2-24.0*yc3*yb
+36.0*yc2*yb2-104.0*yc2*xc*xb-104.0*yc*xc2*yb+12.0*xc4+12.0*
yc4+26.0*yb2*xb2+3.0*yb4)/pow(-xb*yc+xc*yb,3.0)/54;
      TPLGZ4[5][6] = h*h*h*(24.0*yc*xc2*xb3+3.0*xb*yc5+26.0*
yc3*xc3+3.0*xc*yc5+3.0*yc*xc5+14.0*xb3*yc3
-3.0*yc*xb*xc4-3.0*yc*xc3*xb2-9.0*yc*xc*xb4-83.0*yc2
*yc*xc*xb2-42.0*yb*xc3*xb2+290.0*xb2*yc*xc*yb2-92.0*xb*yc*xc2*
yb2-92.0*yb*yc2*xc*xb2+192.0*yb*xb*yc2*xc2+14.0*xc3*yb3+
12.0*yb*xb5+104.0*yb3*xb3+12.0*xb*yb5-6.0*xc*pow(yb,
5.0)-6.0*yc*xb5+24.0*yb*xb3*yc2-9.0*xc*yc*yb4+6.0*yb*xb*
xc4+48.0*yb*xc2*xb3+6.0*yb*xb*yc4-30.0*yb*xc*xb4
-3.0*yb*xc*yc4+48.0*xb*yc2*yb3-83.0*yc*xc3*yb2-3.0*xc*
yc3*yb2-42.0*xb*yc3*yb2-156.0*xb3*yc*yb2-156.0*xc*yb3
*xb2-30.0*xb*yc*yb4+24.0*xc*yc2*yb3+3.0*yb*xc5+24.0*xb
*xc2*yb3)/pow(-xb*yc+xc*yb,3.0)/432;
      TPLGZ4[4][2] = 0.0;
      TPLGZ4[3][7] = -h*h*h*(3.0*xb5+26.0*yb2*xb3+3.0*xb*pow(yb,
4.0)-33.0*xc*yb4-20.0*yc2*xc*xb2+156.0*xb*yc2*xc2+118.0*xb*
xc2*yb2-30.0*xb*yc2*yb2+52.0*xb3*yc*yb-234.0*xc*yb2*xb2+12.0*
xb*yc*yb3+52.0*yc*xc3*yb+12.0*xc*yc3*yb-12.0*xc*yc2*yb2+36.0
*xc*yc*yb3-6.0*xc5-38.0*xb3*yc2+24.0*xb*xc4-12.0*
xc3*xb2+6.0*xc2*xb3+12.0*xb*yc4-21.0*xc*xb4-20.0*
xc3*yb2-28.0*xb2*yc*xc*yb-64.0*xb*yc*xc2*yb-52.0*yc2*xc3-6.0*
xc*yc4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[2][5] = -h*h*h*(-21.0*yc2*xc2*xb2+3.0*yc2*xc4+6.0*
yc2*xb4+26.0*yc4*xc2-26.0*yc4*xc*xb-6.0*yc2*xc2
*xc*xb+24.0*yc2*xc*xb3-90.0*yb*yc3*xc*xb+6.0*yb*yc*xc3*xb+6.0*
yb2*xb4+52.0*yb4*xb2+6.0*pow(yb,6.0)+21.0*yb*yc*xc2*xb2
-6.0*yb*yc*xc*xb3+137.0*yb*xb2*yc3-3.0*yb*yc*xc4+18.0*yb2
*xc2*xb2-15.0*yb*yc*xb4+90.0*yb2*yc2*xc*xb+18.0*yc2*yb4
-9.0*yb*yc5+15.0*yc3*yb3-27.0*yc*yb5+78.0*xb*yc*
xc*yb3+15.0*yb2*xc4+3.0*yc6-97.0*yc*xc2*yb3+123.0
*yb2*yc2*xc2+52.0*yb2*xb2*yc2-30.0*yb2*xc3*xb-182.0*xb2*yc
*yb3-52.0*xc*yb4*xb-12.0*yb2*xc*xb3-52.0*yb*yc3*xc2+
26.0*xc2*yb4-33.0*xb2*yc4)/pow(-xb*yc+xc*yb,3.0)/432;
      TPLGZ4[5][1] = 0.0;
      TPLGZ4[3][3] = -h*h*h*(48.0*xb2*xc4-18.0*xb*xc5-18.0*xc
*xb5+48.0*xc2*xb4-54.0*xc3*xb3+3.0*xc6+3.0*
xb6+333.0*yc2*xc2*xb2+26.0*yc2*xc4+47.0*yc2*xb4
+3.0*yc4*xc2-12.0*yc4*xc*xb-130.0*yc2*xc3*xb-200.0
*yc2*xc*xb3-24.0*yb*yc3*xc*xb+36.0*yb*yc*xc3*xb+26.0*yb2*
xb4+3.0*yb4*xb2-68.0*yb*yc*xc2*xb2+36.0*yb*yc*xc*xb3
-66.0*yb*xb2*yc3-26.0*yb*yc*xc4+333.0*yb2*xc2*xb2-26.0*yb*
yc*xb4+78.0*yb2*yc2*xc*xb-24.0*xb*yc*xc*yb3+47.0*yb2*xc4
-66.0*yc*xc2*yb3+27.0*yb2*yc2*xc2+27.0*yb2*xb2*yc2-200.0
*yb2*xc3*xb-6.0*xb2*yc*yb3-12.0*xc*yb4*xb-130.0*yb2*xc*
xb3-6.0*yb*yc3*xc2+45.0*xc2*yb4+45.0*xb2*yc4)/
pow(-xb*yc+xc*yb,3.0)/432;
      TPLGZ4[6][1] = 0.0;
      TPLGZ4[5][4] = 0.0;
      TPLGZ4[6][2] = 0.0;
      TPLGZ4[4][5] = -h*h*h*(236.0*xb*yc*xc*yb2-92.0*yb*yc2*xc*xb-26.0*
yc3*xc2+64.0*xc2*yb3+24.0*yb*xc4+12.0*yb*yc4-24.0
*yc3*yb2+36.0*yc2*yb3-30.0*yc*yb4+20.0*xb2*yc3
-136.0*yc*xc2*yb2+156.0*yb*yc2*xc2+12.0*yb*xb2*yc2-36.0*yb*xc3
*xb+48.0*yb*xc2*xb2-156.0*xb2*yc*yb2-104.0*xc*yb3*xb-24.0*yb*xc*
xb3-52.0*yc3*xc*xb-12.0*yc*xc3*xb+12.0*yc*xc2*xb2-12.0*yc*xc
*xb3-3.0*yc5-3.0*yc*xc4+12.0*yb5+12.0*yb*xb4
+104.0*yb3*xb2-6.0*yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[2][2] = -h*h*h*(27.0*yc2*xc2*xb2+3.0*yc2*xc4+45.0*
yc2*xb4+26.0*yc4*xc2-26.0*yc4*xc*xb-6.0*yc2*xc2
*xc*xb-66.0*yc2*xc*xb3+36.0*yb*yc3*xc*xb-24.0*yb*yc*xc3*xb+3.0
*yb2*xb4+26.0*yb4*xb2+3.0*pow(yb,6.0)+78.0*yb*yc*xc2*
xb2-24.0*yb*yc*xc*xb3-200.0*yb*xb2*yc3-12.0*yb*yc*xc4+
27.0*yb2*xc2*xb2-12.0*yb*yc*xb4-68.0*yb2*yc2*xc*xb+48.0*yc2
*yb4-18.0*yb*yc5-54.0*yc3*yb3-18.0*yc*yb5+
36.0*xb*yc*xc*yb3+45.0*yb2*xc4+48.0*yb2*yc4+3.0*yc6
-200.0*yc*xc2*yb3+333.0*yb2*yc2*xc2+333.0*yb2*xb2*yc2
-66.0*yb2*xc3*xb-130.0*xb2*yc*yb3-26.0*xc*yb4*xb-6.0*
yb2*xc*xb3-130.0*yb*yc3*xc2+47.0*xc2*yb4+47.0*xb2*
yc4)/pow(-xb*yc+xc*yb,3.0)/432;
      TPLGZ4[5][9] = h*h*h*(15.0*yc*xc2*xb3-3.0*xb*yc5+52.0*
yc3*xc3+6.0*xc*yc5+6.0*yc*xc5+13.0*xb3*yc3
-15.0*yc*xb*xc4+12.0*yc*xc3*xb2-9.0*yc*xc*xb4-78.0*xb*
yc3*xc2+48.0*yb*xc3*xb2-104.0*xb2*yc*xc*yb2+313.0*xb*yc*xc2
*yb2+162.0*yb*yc2*xc*xb2-104.0*yb*xb*yc2*xc2+58.0*xc3*yb3+
6.0*yb*xb5+52.0*yb3*xb3+6.0*xb*yb5+6.0*xc*yb5
-3.0*yc*xb5-39.0*xc*yc*yb4-39.0*yb*xb*xc4-18.0*yb*xc2
*xb3-9.0*yb*xb*yc4-6.0*yb*xc*xb4-6.0*yb*xc*yc4+
12.0*xb*yc2*yb3-130.0*yc*xc3*yb2-18.0*xc*yc3*yb2+15.0*xb*
yc3*yb2-78.0*xb3*yc*yb2-15.0*xb*yc*yb4+48.0*xc*yc2*
yb3+6.0*yb*xc5-130.0*xb*xc2*yb3)/pow(-xb*yc+xc*yb,3.0)/432;
      TPLGZ4[7][4] = 0.0;
      TPLGZ4[3][1] = 0.0;
      TPLGZ4[8][5] = 0.0;
      TPLGZ4[1][5] = h*h*h*(508.0*xb*yc*xc*yb2-184.0*yb*yc2*xc*xb+26.0*
yc3*xc2+110.0*xc2*yb3+30.0*yb*xc4+6.0*yb*yc4-48.0
*yc3*yb2+90.0*yc2*yb3-51.0*yc*yb4+40.0*xb2*yc3
-272.0*yc*xc2*yb2+156.0*yb*yc2*xc2+162.0*yb*xb2*yc2-72.0*yb*xc2
*xc*xb+78.0*yb*xc2*xb2-234.0*xb2*yc*yb2-208.0*xc*yb3*xb-48.0*yb*xc
*xb3-104.0*yc3*xc*xb-24.0*yc*xc3*xb+24.0*yc*xc2*xb2+12.0*yc*
xc*xb3+3.0*yc5+3.0*yc*xc4+6.0*yb5+6.0*yb*pow(xb,
4.0)+52.0*yb3*xb2-3.0*yc*xb4)/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[8][9] = h*h*h*(-42.0*yc*xc2*xb3-6.0*xb*yc5+104.0*
yc3*xc3+12.0*xc*yc5+12.0*yc*xc5+14.0*xb3*yc2
*yc-30.0*yc*xb*xc4+48.0*yc*xc3*xb2+6.0*yc*xc*xb4+24.0*
yc3*xc*xb2-156.0*xb*yc3*xc2+24.0*yb*xc3*xb2+192.0*xb2*yc
*xc*yb2-92.0*xb*yc*xc2*yb2-92.0*yb*yc2*xc*xb2+290.0*yb*xb*yc2*xc2
+14.0*xc3*yb3+3.0*yb*xb5+26.0*yb3*xb3+3.0*xb*yb5
+3.0*xc*yb5+3.0*yc*xb5-83.0*yb*xb3*yc2+6.0*xc*yc*
yb4-9.0*yb*xb*xc4-3.0*yb*xc2*xb3-9.0*yb*xb*yc4
-3.0*yb*xc*xb4-156.0*yb*yc2*xc3-30.0*yb*xc*yc4-3.0*xb*
yc2*yb3+24.0*yc*xc3*yb2+48.0*xc*yc3*yb2+24.0*xb*yc3*
yb2-3.0*xb*yc*yb4-42.0*xc*yc2*yb3-6.0*yb*xc5-83.0*xb*
xc2*yb3)/pow(-xb*yc+xc*yb,3.0)/432;
      TPLGZ4[2][9] = h*h*h*(-21.0*yc*xc2*xb3-3.0*xb*yc5+52.0*
yc3*xc3+6.0*xc*yc5+6.0*yc*xc5+13.0*xb3*yc3
-15.0*yc*xb*xc4+12.0*yc*xc3*xb2+27.0*yc*xc*xb4-78.0*xb*
yc3*xc2+36.0*yb*xc3*xb2+40.0*xb2*yc*xc*yb2+51.0*xb*yc*xc2*
yb2+46.0*yb*yc2*xc*xb2+130.0*yb*xb*yc2*xc2+20.0*xc3*yb3+3.0
*yb*xb5+26.0*yb3*xb3+3.0*xb*yb5+3.0*xc*yb5
-6.0*yc*xb5+19.0*yb*xb3*yc2+9.0*xc*yc*yb4+6.0*yb*xb*pow(
xc,4.0)-27.0*yb*xc2*xb3-3.0*yb*xc*xb4-156.0*yb*yc2*xc3
-24.0*yb*xc*yc4-9.0*xb*yc2*yb3+26.0*yc*xc3*yb2+18.0*xc*
yc3*yb2+33.0*xb*yc3*yb2-78.0*xb3*yc*yb2-12.0*xb*yc*yb4
-18.0*xc*yc2*yb3-12.0*yb*xc5-59.0*xb*xc2*yb3)/pow(-xb
*yc+xc*yb,3.0)/432;
      TPLGZ4[6][7] = -h*h*h*(6.0*xb5+52.0*yb2*xb3+6.0*xb*yb4
-3.0*xc*yb4+136.0*yc2*xc*xb2-150.0*xb*xc2*yb2+36.0*xb*yc2
*yc*yb-30.0*xb*yc2*yb2+104.0*xb3*yc*yb+78.0*xc*yb2*xb2+24.0*xb*yc*
yb3+52.0*yc*xc3*yb+12.0*xc*yc3*yb-12.0*xc*yc2*yb2-24.0*xc*yc
*yb3-6.0*xc5-46.0*xb3*yc2+6.0*xb*xc4+24.0*xc3*
xb2-54.0*xc2*xb3-6.0*xb*yc4+21.0*xc*xb4-20.0*xc3*
yb2-272.0*xb2*yc*xc*yb+92.0*xb*yc*xc2*yb-52.0*yc2*xc3-6.0*xc*yc4)
/pow(-xb*yc+xc*yb,3.0)/216;
      TPLGZ4[9][8] = 0.0;
      TPLGZ4[3][9] = -h*h*h*(18.0*xb2*xc4-27.0*xb*xc5-9.0*xc*
xb5+15.0*xc3*xb3+6.0*xc6+3.0*xb6+52.0*yc2*
xc2*xb2+52.0*yc2*xc4-33.0*yc2*xb4+6.0*yc4*xc2
-15.0*yc4*xc*xb-182.0*yc2*xc3*xb+137.0*yc2*xc*xb3-6.0*yb*
yc3*xc*xb+78.0*yb*yc*xc3*xb+26.0*yb2*xb4+3.0*yb4*
xb2+90.0*yb*yc*xc2*xb2-90.0*yb*yc*xc*xb3+24.0*yb*xb2*yc3-52.0
*yb*yc*xc4+123.0*yb2*xc2*xb2-26.0*yb*yc*xb4+21.0*yb2*
yc2*xc*xb+6.0*xb*yc*xc*yb3+26.0*yb2*xc4-30.0*yc*xc2*yb3
+18.0*yb2*yc2*xc2-21.0*yb2*xb2*yc2-97.0*yb2*xc3*xb-6.0*xb2
*yc*yb3-3.0*xc*yb4*xb-52.0*yb2*xc*xb3-12.0*yb*yc3*
xc2+15.0*xc2*yb4+6.0*xb2*yc4)/pow(-xb*yc+xc*yb,3.0)/432;

/* Sub triangl. */

   for (i2 = 1 ; i2 <= 9 ; i2++) {
     for (i1 = i2+1 ; i1 <= 9 ; i1++) {
       TPLGZ4[i1][i2] = TPLGZ4[i2][i1];
     }
   }

return;
}
