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
 *
 * File:	rosenbrock.c	
 *				
 * Description:	contains code for a variable time step hyperbolic DE
 *		integration routine based on the Rosenbrock method.
 *		Thanks to Paavo Nevalainen for writing it and
 *		putting it into a form we could use.
 *
 * Author:	Paavo Nevalainen, paavo@hihna.me.tut.fi
 *
 ************************************************************************/

# include <stdio.h>
# include <math.h>
# include "fe.h"
# include "error.h"
# include "problem.h"
# include "transient.hpp"

/*--------------------------------------------*
 * called only from IntegrateHyperbolicDE2()  * 
 * numerical appr. of dp/dt                   * 
 *--------------------------------------------*/

static void
ConstructTFD(double t, double h, Vector dp)
{
  static Vector  temp = NullMatrix;
  static int     size;
  int            i;
    
  if (temp == NullMatrix) {
      size= problem.nodes.size() * problem.num_dofs; 
     temp= CreateVector(size); 
  }
   
  if(t == 0.0)
    {
    AssembleTransientForce(0.0, temp);
    AssembleTransientForce(0.0+h/4, dp);
    for(i= 1; i<= size; i++)
      VectorData(dp)[i]= (VectorData(dp)[i] - VectorData(temp)[i])/(h/4);
    }
  else
    { 
    AssembleTransientForce(t-h/4, temp); 
    AssembleTransientForce(t+h/4, dp);
    for(i= 1; i<= size; i++)
      VectorData(dp)[i]= (VectorData(dp)[i] - VectorData(temp)[i])/(h/2);
    }

    return;

  } /* eo ConstructTFD() */ 

Matrix
RosenbrockHyperbolicDE(Matrix k0, Matrix m, Matrix c0, Matrix *ttable)
{
  unsigned      count, i,j;
  Matrix        dtable;
  Vector        y0, v0, a_dummy, p0, p0d; 
  Vector        b0, bhalf, 
                phalf, p1,              /* load in three stages */ 
                r0, rhalf, r1,          /* r(d,v) in three stages */
                d1, e1, d2, e2, d3, e3, /* three stages         */ 
                vhalf, v1, err; 
  Vector        k0d1; 
  double        r0_i, k0v0_i, rhalf_i, k0d1_i, k0vhalf_i, sum_i, 
                k0v1_i, k0d2_i, me1_i, b1_i, me2_i, r1_i; 
                
  Matrix        M0, M0_fact;
  unsigned      size;
  double        gamma, e32, gh, h; 
  const double  beta1= 0.0, beta2= 1.0; 
  unsigned      step, nsteps;
  int           address, build_a0;
  double        t;
     

  const Node *node = problem.nodes.c_ptr1();
  const unsigned numnodes = problem.nodes.size();
  count = problem.num_dofs;


        /*
         * constants that we will need
         */
  size=  numnodes*count;
  h=     analysis.step; 
  gamma= 1.0/(2.0+sqrt(2.0));
  e32=   6.0+sqrt(2.0);
  gh=    gamma*h;
 
        /*
         * create vectors to hold the conditions at timesteps i and i+1
         */
  a_dummy= CreateVector(size); 
  b0= CreateVector(size);      p0= CreateVector(size); 
  p0d= CreateVector(size);     phalf= CreateVector(size); 
  p1= CreateVector(size);      r0= CreateVector(size); 
  rhalf= CreateVector(size);   r1= CreateVector(size); 
  d1= CreateVector(size);      e1= CreateVector(size); 
  d2= CreateVector(size);      e2= CreateVector(size);
  d3= CreateVector(size);      e3= CreateVector(size);      
  y0= CreateVector(size);      v0= CreateVector(size);      
  vhalf= CreateVector(size);   v1= CreateVector(size);      
  k0d1= CreateVector(size);    bhalf= CreateVector(size);   
  err= CreateVector(size); 


        /*
         * create the table of nodal time displacements
         */
  nsteps= (analysis.stop + analysis.step/2.0) / analysis.step + 1.0;
  dtable= CreateMatrix(nsteps, analysis.nodes.size()*analysis.numdofs);
  *ttable= CreateVector(nsteps); /* CHANGE: pn */ 
  

        /*
         * create the M0_fact matrix
         */
  M0= CreateCopyMatrix(k0);
  for(i= 1; i<= Msize(k0); i++)
    VectorData(M0)[i]= VectorData(m)[i]       +
                       VectorData(c0)[i]*gh   +
                       VectorData(k0)[i]*gh*gh ; 


        /*
         * create a constrained copy of M0 and do a one-time
         * factorization on it.  This is the matrix that we will
         * use as the RHS of our implicit update equation
         */
  ZeroConstrainedDOF(M0, NULL, &M0_fact, NULL);
  if(CroutFactorMatrix(M0_fact))
    {
    error("singular M0 matrix in hyperbolic integration - cannot proceed");
    return(NullMatrix);
    }


        /*
         * build the initial displacement and velocity vectors from the
         * initial conditions
         */
  cvector1i constraint_mask = BuildConstraintMask();
  build_a0 = BuildHyperbolicIC(y0, v0, a_dummy);



        /*
         * Copy the initial displacement vector into the table.
         * This is basically a copy of the code at the end of the loop.
         */
  for(i= 1; i<= analysis.nodes.size(); i++)
    for(j= 1; j<= analysis.numdofs; j++)
      {
      MatrixData(dtable) [1][(i-1)*analysis.numdofs+ j] =
        VectorData(y0)[GlobalDOF(analysis.nodes [i] -> number, analysis.dofs[j])];
      }



        /*
         * iterate over every time step.  Fill up dtable with
         * the results
         */
  AssembleTransientForce(t= 0.0, p0);
  for(step= 1; step<= nsteps; step++)
    {
     


        /*
         * setup p'(i+1) and dp'(i+1).  
         * The numerical integration here is only a temporary solution. 
         */
    t = (step - 1.0)*analysis.step;
    ConstructTFD(t, h, p0d); 


      
        /*--------------------------------------*
         * do the stage 1                       *      
         *--------------------------------------*/
    for(i= 1; i<= size; i++)
      if(!constraint_mask[i])
        {
        r0_i= k0v0_i= 0.0; 
        for(j= 1; j<= size; j++)
          if(!constraint_mask[j])
            if((address= ConvertRowColumn(i, j, k0))) 
              {
              /* r0= k0* y0+ c0* v0; %  r(y0, v0); */ 
              r0_i+=  VectorData(k0)[address]* VectorData(y0)[j]+ 
                      VectorData(c0)[address]* VectorData(v0)[j]; 
       
              /* b0= -gh* k0* v0+ p0- r0;         %%%%%% t0 term */
              k0v0_i+= VectorData(k0)[address]* VectorData(v0)[j];
              } 

        /* b0= -gh* k0* v0+ p0- r0;    %%% t0 term */
        VectorData(b0)[i]= -gh*k0v0_i+ VectorData(p0)[i]- r0_i;    

        /* e1= h* (b0+ gh*p0d); */
        VectorData(e1)[i]= h*(VectorData(b0)[i]+ gh*VectorData(p0d)[i]);    
        }
      else
        VectorData(e1)[i]= VectorData(b0)[i]= 0.0; 


    /* e1= U\(L\e1); */
    ResolveBC(t, M0, e1);
    if(CroutBackSolveMatrix(M0_fact, e1)) /* e1 := M0^(-1)*e1 */ 
      {
      error("singular M0 matrix in hyperbolic integration - cannot proceed");
      return(NullMatrix);
      }

    /* phalf= p(t0+0.5*h); */
    AssembleTransientForce(t+0.5*h,  phalf);



    for(i= 1; i<= size; i++)
      if(!constraint_mask[i])
        /* d1= h* (v0+gamma*e1); */
        VectorData(d1)[i]   = h*( VectorData(v0)[i]+ gamma*VectorData(e1)[i] ); 
      else 
        VectorData(d1)[i]= 0.0; 

    for(i= 1; i<= size; i++)
      if(!constraint_mask[i])
        { 
    

        rhalf_i= k0d1_i= 0.0; 
        for(j= 1; j<= size; j++)
          if(!constraint_mask[j])
            if((address= ConvertRowColumn(i, j, k0)))
              { 
              /* rhalf= k0* (y0+ 0.5*d1)+ c0* (v0+0.5*e1); */
              rhalf_i+= VectorData(k0)[address] *
                          (VectorData(y0)[j]+ 0.5*VectorData(d1)[j]) +
                        VectorData(c0)[address] *
                          (VectorData(v0)[j]+ 0.5*VectorData(e1)[j]);
              /* k0d1=  k0*d1;  */
              k0d1_i+=  VectorData(k0)[address] * VectorData(d1)[j]; 
              }

        /* vhalf= v0+0.5*e1; */
        VectorData(vhalf)[i]= VectorData(v0)[i]+ 0.5*  VectorData(e1)[i]; 
        VectorData(rhalf)[i]= rhalf_i;
        VectorData(k0d1)[i]= k0d1_i;
        } 
      else
        VectorData(vhalf)[i]= VectorData(rhalf)[i]= VectorData(k0d1)[i]= 0.0; 



        /*-----------------------------------*
         * do the stage 2                    *
         *-----------------------------------*/
    for(i= 1; i<= size; i++)
      if(!constraint_mask[i])
        {
        k0vhalf_i= sum_i= 0.0; 
        for(j= 1; j<= size; j++)
          if(!constraint_mask[j])
            if((address= ConvertRowColumn(i, j, k0)))
              {

              /* bhalf = -gh*h*k0* vhalf+ h*(phalf- rhalf); %%  t0+0.5h term */
              k0vhalf_i+= VectorData(k0)[address]* VectorData(vhalf)[j];  

              /* e2= bhalf+  gh*k0d1+  gh* c0 *e1+  gh*gh* k0* e1; */
              sum_i+= ( gh* VectorData(c0)[address]+ 
                        gh*gh* VectorData(k0)[address])* VectorData(e1)[j] ; 
              } 

        /* bhalf = -gh*h*k0* vhalf+ h*(phalf- rhalf); %%  t0+0.5h term */
        VectorData(bhalf)[i]= -gh*h* k0vhalf_i+ 
                              h*( VectorData(phalf)[i]- VectorData(rhalf)[i]); 

        /* e2= bhalf+  gh*k0d1+  gh* c0 *e1+  gh*gh* k0* e1; */
        VectorData(e2)[i]= VectorData(bhalf)[i]+ 
                           gh*VectorData(k0d1)[i]+ sum_i; 
        } 
      else
        { VectorData(bhalf)[i]= VectorData(e2)[i]= 0.0; }  

    /* e2= U\(L\e2);   */
    ResolveBC(t, M0, e2);
    if(CroutBackSolveMatrix(M0_fact, e2)) /* e2 := M0^(-1)*e2 */
      {
      error("singular M0 matrix in hyperbolic integration- cannot proceed");
      return(NullMatrix);
      }


    /* p1= p(t0+h); */
    AssembleTransientForce(t+h, p1);
 
    for(i= 1; i<= size; i++)
      if(!constraint_mask[i])
        { 
        /* d2= gh* e2+ h* vhalf- gh* e1; */
        VectorData(d2)[i]= gh*VectorData(e2)[i]  
                           +h*VectorData(vhalf)[i]  
                          -gh*VectorData(e1)[i];

        /* v1= v0+e2;   */
        VectorData(v1)[i]= VectorData(v0)[i]+ VectorData(e2)[i]; 

        r1_i= 0.0; 
        for(j= 1; j<= size; j++)
          if(!constraint_mask[j])
            if((address= ConvertRowColumn(i, j, k0)))
              {
      
              /* r1= k0* (y0+ d2)+ c0* (v0+e2); % r(y0+ d2, v0+e2); */
              r1_i+= VectorData(k0)[address]* 
                       (VectorData(y0)[j]+ VectorData(d2)[j])+ 
                     VectorData(c0)[address]* 
                       (VectorData(v0)[j]+ VectorData(e2)[j]); 
  
              }  

         VectorData(r1)[i]=  r1_i;   
         }
       else
         VectorData(r1)[i]= 0;




        /*------------------------------*
         * do the stage 3               *
         *------------------------------*/
    for(i= 1; i<= size; i++)
      if(!constraint_mask[i])
        {
        k0v1_i= k0d2_i= me1_i= me2_i= 0.0;  
        for(j= 1; j<= size; j++)
          if(!constraint_mask[j])
            if((address= ConvertRowColumn(i, j, k0)))
              {
              /* b1= -gh* k0* v1+ p1- r1;      %%   t0+h term */
              k0v1_i+=  VectorData(k0)[address]* VectorData(v1)[j]; 

              /* e3 = h* b1+ e32*gh* k0* d2 -e32* m* e2+ 
                      e32*h *bhalf- 2*gh*k0d1 -2* m* e1; */
              k0d2_i+=  VectorData(k0)[address]* VectorData(d2)[j]; 
              me1_i+=  VectorData(m)[address]* VectorData(e1)[j]; 
              me2_i+=  VectorData(m)[address]* VectorData(e2)[j]; 

              } 


        /* b1= -gh* k0* v1+ p1- r1;      %%   t0+h term */
        b1_i= -gh* k0v1_i + VectorData(p1)[i]- VectorData(r1)[i]; 

        /* e3= h* b1+ e32*gh* k0* d2 -e32* m* e2+ 
               e32*h *bhalf- 2*gh*k0d1 -2* m* e1; */
        VectorData(e3)[i]=  h* b1_i+ e32*gh* k0d2_i -e32* me2_i 
                           +e32*h* VectorData(bhalf)[i]
                           -2*gh* VectorData(k0d1)[i]- 2* me1_i; 


        /* e3 = e3+ 2*h* b0+ gh*h* p0d; */
        VectorData(e3)[i]= VectorData(e3)[i]+ 2*h* VectorData(b0)[i]+ 
                           gh*h* VectorData(p0d)[i]; 

        }
      else
        VectorData(e3)[i]= 0.0; 



    /* e3= U\L\e3; */
    ResolveBC(t, M0, e3); 
    if(CroutBackSolveMatrix(M0_fact, e3)) /* e3 := M0^(-1)*e3 */
      {
      error("singular M0 matrix in hyperbolic integration- cannot proceed");
      return(NullMatrix);
      }


    /* d3= gh* e3+ h* v1- e32* d2+ e32*h* vhalf+ 2*h* v0; */
    for(i= 1; i<= size; i++)
      if(!constraint_mask[i])
        /* d3= gh* e3+ h* v1- e32* d2+ e32*h* vhalf+ 2*h* v0; */
        VectorData(d3)[i]=  gh*  VectorData(e3)[i] 
                            -h*  VectorData(v1)[i] 
                          -e32*  VectorData(d2)[i] 
                          +e32*h*VectorData(vhalf)[i]  
                            +2*h*VectorData(v0)[i];  
      else 
        VectorData(d3)[i]= 0.0; 
 
        /* 
         * it is time to solve the next system state
         */ 
    for(i = 1 ; i <= size ; i++)
      { 
      VectorData(y0)[i]+= beta1* VectorData(d1)[i]+ beta2* VectorData(d2)[i];
      VectorData(v0)[i]+= beta1* VectorData(e1)[i]+ beta2* VectorData(e2)[i];
      /* only the displacement error! */ 
      VectorData(err)[i]= 1/6*(   VectorData(d1)[i] 
                               -2*VectorData(d2)[i]
                                + VectorData(d3)[i]); 
      VectorData(p0)[i]= VectorData(p1)[i]; 
      } 

        /*
         * copy the relevant parts of the displacement vector
         * into the displacement table
         */
      for(i= 1; i<= analysis.nodes.size(); i++)
        for(j= 1; j<= analysis.numdofs; j++)
          MatrixData(dtable)[step][(i-1)*analysis.numdofs+ j] =
               VectorData(y0)[GlobalDOF(analysis.nodes [i] -> number, analysis.dofs[j])];
      VectorData(*ttable)[step]= t; /* CHANGE: pn */ 

    } /* eo for steps */


        /*
         * clean up ...
         */
  DestroyVector(a_dummy); DestroyVector(b0);
  DestroyVector(p0);      DestroyVector(p0d);   DestroyVector(phalf);
  DestroyVector(p1);      DestroyVector(r0);    DestroyVector(rhalf);
  DestroyVector(r1);      DestroyVector(d1);    DestroyVector(e1);
  DestroyVector(d2);      DestroyVector(e2);    DestroyVector(d3);
  DestroyVector(e3);      DestroyVector(y0);    DestroyVector(v0);
  DestroyVector(vhalf);   DestroyVector(v1);    DestroyVector(k0d1);
  DestroyVector(bhalf);   DestroyVector(err); 

  DestroyMatrix(M0_fact); DestroyMatrix(M0); 

  return(dtable);
} /* eo IntegrateHyperbolicDE2() */
