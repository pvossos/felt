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
 * File:	bivar.c
 *
 * Description:	contains routines to do bivariate interpolation using
 *		the method if Akima (1975 and 1984).  The algorithm
 *		is publicly available in Fortran form from ACM-TOMS
 *		(algorithm 526).  All we've really done here is translate
 *		the code into C to make it easier to interface to 
 * 		the rest of our code.  Yes it was a pain in the ass,
 *		but hey, its a lot easier to work with now.
 *
 ***************************************************************************/

# include <math.h>
# include <stdio.h>
# include "bivar.h"
# include "allocate.h"
# include "error.h"

# define EPSILON 1.0e-6
# define NREP 100

# define SPDT_g(u1,v1,u2,v2,u3,v3) (((u1)-(u2))*((u3)-(u2)) + \
                                    ((v1)-(v2))*((v3)-(v2)))
# define VPDT_g(u1,v1,u2,v2,u3,v3) (((u1)-(u3))*((v2)-(v3)) - \
                                    ((v1)-(v3))*((u2)-(u3)))
# define SPDT_t(u1,v1,u2,v2,u3,v3) (((u2)-(u1))*((u3)-(u1)) + \
                                    ((v2)-(v1))*((v3)-(v1)))
# define VPDT_t(u1,v1,u2,v2,u3,v3) (((v3)-(v1))*((u2)-(u1)) - \
                                    ((u3)-(u1))*((v2)-(v1)))
# define DSQF(u1,v1,u2,v2) (((u2)-(u1))*((u2)-(u1)) + \
                            ((v2)-(v1))*((v2)-(v1)))

# define AMIN(u,v) ((u) < (v) ? (u) : (v))
# define AMAX(u,v) ((u) > (v) ? (u) : (v))

float  idptip();
void   idgrid();
int    idtang();
void   idpdrv();
int    idxchg();

/****************************************************************************
 *
 * Function:	BivariateInterp()
 *
 * Description:	The user entry point for the bivariate interpolation
 *		routines.  Traditionally this routine is called idsfft().
 *		Given an array of known z values at given x,y coordinates,
 *		this routine will interpolate z values to every point
 *		over an x,y grid (defined by the xi,yi arrays below).
 * 
 * Parameters:	ndp ....... the number of data points
 *		xd ........ array of x values for data points (unit offset)
 *		yd ........ array of y values for data points (unit offset)
 *	        zd ........ array of z values for data points (unit offset)
 *              nxi ....... the size of the grid array x 
 *		nyi ....... the size of the grid array y
 *              xi ........ array of grid x values (unit offset)
 *              yi ........ array of grid y values (unit offset)
 *              zi ........ two-dimensional array which will be filled
 *                          with the interpolated z values.  It is the 
 *                          calling functions's responsibility to allocate
 *                          space for this array (nxi x nyi).  Because 
 *			    this array is used in many imaging applications
 *	                    it should be zero offset, unlike the rest of
 *                          the input vectors.
 *
 * Returns:	0 on success, 1 on error 
 *
 ***************************************************************************/

/* zi should be ZERO offset! */
int BivariateInterp (int ndp, float *xd, float *yd, float *zd,
                     int nxi, int nyi, float *xi, float *yi, float **zi,
                     char **mask)
{
   float	*wk;
   int		*iwk;
   int		ndp0,nxi0,nyi0;
   int		nt,nl,ngp0,ngp1;
   int		jwiwl,jwngp0,jwipt,jwipl,jwiwp,jwigp0,jwwpd,jwngp;
   int		itpv,jig0mx,jig1mn,jig0mn,nngp,jngp,iti,il1,il2;
   int		jig1mx,jwigp,jigp,ixi,iyi,izi;
   int		status;


   if (ndp < 4) {
      error ("not enough data points to do the interpolation");
      return 1;
   }

   ndp0 = ndp;
   nxi0 = nxi;
   nyi0 = nyi;

   iwk = Allocate (int, 31*ndp + nxi*nyi);
   UnitOffset (iwk);

   wk = Allocate (float, 6*ndp);
   UnitOffset (wk);

   iwk[1] = ndp0;
   iwk[3] = nxi0;
   iwk[4] = nyi0;
   
	/*
	 * allocation of storage areas in the iwk array.  
	 */

   jwipt=16;
   jwiwl=6*ndp0+1;
   jwngp0=jwiwl-1;
   jwipl=24*ndp0+1;
   jwiwp=30*ndp0+1;
   jwigp0=31*ndp0;
   jwwpd=5*ndp0+1;
	
	/*
	 * triangulates the x-y plane. 
	 */

   status = idtang(ndp0,xd,yd,&nt,&(iwk[jwipt-1]),&nl,&(iwk[jwipl-1]),
                   &(iwk[jwiwl-1]),&(iwk[jwiwp-1]),wk);

   if (status)
      return 1;

   iwk[5]=nt;
   iwk[6]=nl;
   
   if (nt == 0) 
      return 1;

	/*
	 * sorts output grid points in ascending order of the triangle
	 * number and the border line segment number.  
	 */

  idgrid (xd,yd,nt,&(iwk[jwipt-1]),nl,&(iwk[jwipl-1]),nxi0,nyi0,
           xi,yi,&(iwk[jwngp0+1-1]),&(iwk[jwigp0+1-1]));

	/*
	 * estimates partial derivatives at all data points.
	 */

  idpdrv(ndp0,xd,yd,zd,nt,&(iwk[jwipt-1]),wk,&(wk[jwwpd-1]));

	/*
	 * interpolates the zi values.  
	 */

  itpv=0;
  jig0mx=0;
  jig1mn=nxi0*nyi0+1;
  nngp=nt+2*nl;
  for (jngp = 1; jngp <= nngp ; jngp++) {
     iti=jngp;
     if (jngp > nt) {
        il1=(jngp-nt+1)/2;
        il2=(jngp-nt+2)/2;
        if(il2 > nl)     il2=1;
        iti=il1*(nt+nl)+il2;
     }
     jwngp=jwngp0+jngp;
     ngp0=iwk[jwngp];
     if (ngp0 != 0) {
        jig0mn=jig0mx+1;
        jig0mx=jig0mx+ngp0;
        for (jigp=jig0mn ; jigp <= jig0mx ; jigp++) {
           jwigp=jwigp0+jigp;
           izi=iwk[jwigp];
           iyi=(izi-1)/nxi0+1;
           ixi=izi-nxi0*(iyi-1);
        
           if (mask[iyi-1][ixi-1])
              zi[iyi-1][ixi-1]=idptip(xd,yd,zd,nt,&(iwk[jwipt-1]),nl,
                                   &(iwk[jwipl-1]),wk,iti,xi [ixi],yi [iyi]);
        }
     }
     jwngp=jwngp0+2*nngp+1-jngp;
     ngp1=iwk[jwngp];
     if (ngp1 != 0) {
        jig1mx=jig1mn-1;
        jig1mn=jig1mn-ngp1;
        for (jigp = jig1mn ; jigp <= jig1mx ; jigp++) {
           jwigp=jwigp0+jigp;
           izi=iwk[jwigp];
           iyi=(izi-1)/nxi0+1;
           ixi=izi-nxi0*(iyi-1);

           if (mask[iyi-1][ixi-1])
              zi[iyi-1][ixi-1]=idptip(xd,yd,zd,nt,&(iwk[jwipt-1]),nl,
                                   &(iwk[jwipl-1]),wk,iti,xi [ixi],yi [iyi]);
        }
     }
  }
 
   ZeroOffset (iwk); Deallocate (iwk);
   ZeroOffset (wk); Deallocate (wk);

  return 0;
}

/****************************************************************************
 *
 * Function:	idtang()
 *
 * Description:	
 * 
 * Returns:	none
 *
 ***************************************************************************/

int idtang (ndp, xd, yd, nt, ipt, nl, ipl, iwl, iwp, wk)
   int		ndp,*nt,*nl;
   float	*xd,*yd;
   float	*wk;
   int		*ipt,*ipl,*iwl,*iwp;
{
   int		itf[3];
   int		ndp0,ip1,ip2,ip3,ndpm1,ipmn1,ipmn2;
   int		jpmx,jpc,jp,jp1,jp2,its,nt0,ntt3,ipl1,ipl2,ilt3,nlnt3;
   int		iliv,ixvspv,ixvs,ilvs,jl1,jl2,nlsh,nlsht3,nln,it,itt3;
   int		ipti,jwl,nlf,nlt3,ntf,ipti2,ipti1,it2t3,it1t3,nlfc,jwl1mn;
   int		ilf,itt3r,ipt1,ipt2,ipt3,jlt3,iplj1,iplj2,jwl1;
   int		nlft2,ip1p1,jpmn,ip,nl0,il,ntt3p3,irep;
   int		flag;
   float	sp,vp;
   float	x1,y1,x2,y2,x3,y3;
   float	dsqmn,dsqi,xdmp,ydmp;

   ndp0 = ndp;
   ndpm1 = ndp0 - 1;

   dsqmn = DSQF(xd[1],yd[1],xd[2],yd[2]);

   nlnt3 = iliv = ixvs = ilvs = nln = ip = 0;	/* gcc -Wall */
   
   ipmn1=1;
   ipmn2=2;
   for (ip1 = 1 ; ip1 <= ndpm1 ; ip1++) {
      x1=xd[ip1];
      y1=yd[ip1];
      ip1p1=ip1+1;
      for (ip2 = ip1p1 ; ip2 <= ndp0 ; ip2++) {
         dsqi=DSQF(x1,y1,xd[ip2],yd[ip2]);
         if (dsqi == 0.) {
            error ("cannot interpolate when two data points are co-located");
            return 1;
         }
         if(dsqi >= dsqmn) continue;
         dsqmn=dsqi;
         ipmn1=ip1;
         ipmn2=ip2;
      }
   }
   xdmp=(xd[ipmn1]+xd[ipmn2])/2.0;
   ydmp=(yd[ipmn1]+yd[ipmn2])/2.0;
   
	/*
	 * sorts the other (ndp-2) data points in ascending order of
	 * distance from the midpoint and stores the sorted data point
	 * numbers in the iwp array.
	 */

   jp1=2;
   for (ip1 = 1 ; ip1 <= ndp0 ; ip1++) {
      if (ip1 == ipmn1 || ip1 == ipmn2) continue;
      jp1=jp1+1;
      iwp[jp1]=ip1;
      wk[jp1]=DSQF(xdmp,ydmp,xd[ip1],yd[ip1]);
   }
   for (jp1 = 3 ; jp1 <= ndpm1 ; jp1++) { 
      dsqmn=wk[jp1];
      jpmn=jp1;
      for (jp2 = jp1 ; jp2 <= ndp0 ; jp2++) {
         if(wk[jp2] >= dsqmn) continue;
         dsqmn=wk[jp2];
         jpmn=jp2;
      }
      its=iwp[jp1];
      iwp[jp1]=iwp[jpmn];
      iwp[jpmn]=its;
      wk[jpmn]=wk[jp1];
   }

	/*
	 * if necessary, modifies the ordering in such a way that the
	 * first three data points are not collinear.
	 */

   x1=xd[ipmn1];
   y1=yd[ipmn1];
   x2=xd[ipmn2];
   y2=yd[ipmn2];

   flag = 1;
   for (jp = 3 ; jp <= ndp0 ; jp++) {
      ip=iwp[jp];
      sp=SPDT_t(xd[ip],yd[ip],x1,y1,x2,y2);
      vp=VPDT_t(xd[ip],yd[ip],x1,y1,x2,y2);
      if (fabs(vp) > (fabs(sp)*EPSILON)) {
         flag = 0;
         break; 
      }
   }

   if (flag) {
      error ("cannot interpolate when all points are collinear");
      return 1;
   }

   if (jp != 3) {
      jpmx=jp;
      for (jpc = 4 ; jpc <= jpmx ; jpc++) {
         jp=jpmx+4-jpc;
         iwp[jp]=iwp[jp-1];
      }
      iwp[3]=ip;
   }

	/*
	 * forms the first triangle.  stores point numbers of the ver-
	 * texes of the triangle in the ipt array, and stores point num-
	 * bers of the border line segments and the triangle number in
	 * the ipl array.
	 */

  ip1=ipmn1;
  ip2=ipmn2;
  ip3=iwp[3];
  if (VPDT_t(xd[ip1],yd[ip1],xd[ip2],yd[ip2],xd[ip3],yd[ip3]) < 0.0) {
     ip1=ipmn2;
     ip2=ipmn1;
  }
  nt0=1;
  ntt3=3;
  ipt[1]=ip1;
  ipt[2]=ip2;
  ipt[3]=ip3;
  nl0=3;
  nlt3=9;
  ipl[1]=ip1;
  ipl[2]=ip2;
  ipl[3]=1;
  ipl[4]=ip2;
  ipl[5]=ip3;
  ipl[6]=1;
  ipl[7]=ip3;
  ipl[8]=ip1;
  ipl[9]=1;

	/*
	 * adds the remaining (ndp-3) data points, one by one.
	 */

  for (jp1 = 4 ; jp1 <= ndp0 ; jp1++) {
     ip1=iwp[jp1];
     x1=xd[ip1];
     y1=yd[ip1];

	/*
	 * determines the first invisible and visible border line seg-
	 * ments, iliv and ilvs.
	 */

     flag = 0;
     for (il = 1 ; il <= nl0 ; il++) {
        ip2=ipl[3*il-2];
        ip3=ipl[3*il-1];
        x2=xd[ip2];
        y2=yd[ip2];
        x3=xd[ip3];
        y3=yd[ip3];
        sp=SPDT_t(x1,y1,x2,y2,x3,y3);
        vp=VPDT_t(x1,y1,x2,y2,x3,y3);
        if(il == 1) {
           ixvs=0;
           if (vp <= (fabs(sp)*(-EPSILON))) ixvs=1;
           iliv=1;
           ilvs=1;
           continue;
        }
        ixvspv=ixvs;
        if (vp <= (fabs(sp)*(-EPSILON))) {
           ixvs=1;
           if(ixvspv == 1) continue;
           ilvs=il;
           if(iliv != 1) {
              flag = 1;
              break;
           }       
           continue;
        }
        ixvs=0;
        if(ixvspv == 0) continue;
        iliv=il;
        if (ilvs != 1) {
           flag = 1;       
           break;
        }
     }

     if (!flag) {
        if(iliv == 1 && ilvs == 1)  ilvs=nl0;
     }

     if(ilvs < iliv)  ilvs=ilvs+nl0;

	/*
 	 * shifts (rotates) the ipl array to have the invisible border
 	 * line segments contained in the first part of the ipl array.
	 */

     if (iliv != 1) {
        nlsh=iliv-1;
        nlsht3=nlsh*3;
        for (jl1 = 1 ; jl1 <= nlsht3 ; jl1++) {
           jl2=jl1+nlt3;
           ipl[jl2]=ipl[jl1];
        }
        for (jl1 = 1 ; jl1 <= nlt3 ; jl1++) {
           jl2=jl1+nlsht3;
           ipl[jl1]=ipl[jl2];
        }
        ilvs=ilvs-nlsh;
     }

	/*
	 * adds triangles to the ipt array, updates border line
	 * segments in the ipl array, and sets flags for the border
	 * line segments to be reexamined in the iwl array.
         */

      jwl=0;
      for (il = ilvs ; il <= nl0 ; il++) {
         ilt3=il*3;
         ipl1=ipl[ilt3-2];
         ipl2=ipl[ilt3-1];
         it  =ipl[ilt3];

	/*
	 * adds a triangle to the ipt array.
	 */

         nt0=nt0+1;
         ntt3=ntt3+3;
         ipt[ntt3-2]=ipl2;
         ipt[ntt3-1]=ipl1;
         ipt[ntt3]  =ip1;

	/*
	 * updates border line segments in the ipl array.
	 */

         if (il == ilvs) { 
            ipl[ilt3-1]=ip1;
            ipl[ilt3]  =nt0;
         }

         if(il == nl0) {
            nln=ilvs+1;
            nlnt3=nln*3;
            ipl[nlnt3-2]=ip1;
            ipl[nlnt3-1]=ipl[1];
            ipl[nlnt3]  =nt0;
         }

	/*
	 * determines the vertex that does not lie on the border
	 * line segments.
	 */

         itt3=it*3;
         ipti=ipt[itt3-2];
         if (ipti == ipl1 || ipti == ipl2) {
            ipti=ipt[itt3-1];
            if (ipti == ipl1 || ipti == ipl2) 
               ipti=ipt[itt3];
         }

	/*
	 * checks if the exchange is necessary.
	 */

         if(idxchg(xd,yd,ip1,ipti,ipl1,ipl2) == 0) continue;

	/*
	 * modifies the ipt array when necessary.
	 */

         ipt[itt3-2]=ipti;
         ipt[itt3-1]=ipl1;
         ipt[itt3]  =ip1;
         ipt[ntt3-1]=ipti;
         if(il == ilvs)  ipl[ilt3]=it;
         if(il == nl0 && ipl[3] == it)      ipl[3]=nt0;

	/*
	 * sets flags in the iwl array.
	 */

         jwl=jwl+4;
         iwl[jwl-3]=ipl1;
         iwl[jwl-2]=ipti;
         iwl[jwl-1]=ipti;
         iwl[jwl]  =ipl2;
      } 

      nl0=nln;
      nlt3=nlnt3;
      nlf=jwl/2;
      if(nlf == 0) continue; 

	/*
	 * improves triangulation.
	 */

      ntt3p3=ntt3+3;
      for (irep = 1 ; irep <= NREP ; irep++) {
         for (ilf = 1 ; ilf <= nlf ; ilf++) {
            ipl1=iwl[2*ilf-1];
            ipl2=iwl[2*ilf];

	/*
	 * locates in the ipt array two triangles on both sides of
	 *  the flagged line segment.
	 */

            ntf=0;
            flag = 1;
            for (itt3r = 3 ; itt3r <= ntt3 ; itt3r += 3) {
               itt3=ntt3p3-itt3r;
               ipt1=ipt[itt3-2];
               ipt2=ipt[itt3-1];
               ipt3=ipt[itt3];
               if (ipl1 != ipt1 && ipl1 != ipt2 && ipl1 != ipt3) continue;
               if (ipl2 != ipt1 && ipl2 != ipt2 && ipl2 != ipt3) continue;
               ntf=ntf+1;
               itf[ntf]=itt3/3;
               if (ntf == 2) {
                  flag = 0;
                  break;
               }
            }

            if (flag) {
               if(ntf < 2) continue;
            }

	/*
	 * determines the vertexes of the triangles that do not lie
	 * on the line segment.
	 */

            it1t3=itf[1]*3;
            ipti1=ipt[it1t3-2];
            if (ipti1 == ipl1 || ipti1 == ipl2) {
               ipti1=ipt[it1t3-1];
               if (ipti1 == ipl1 || ipti1 == ipl2)
                  ipti1=ipt[it1t3];
            }
            it2t3=itf[2]*3;
            ipti2=ipt[it2t3-2];
            if (ipti2 == ipl1 || ipti2 == ipl2) { 
               ipti2=ipt[it2t3-1];
               if (ipti2 == ipl1 || ipti2 == ipl2) 
                  ipti2=ipt[it2t3];
            }

	/*
	 * checks if the exchange is necessary.
	 */

            if (idxchg(xd,yd,ipti1,ipti2,ipl1,ipl2) == 0) continue;

	/*
	 * modifies the ipt array when necessary.
	 */

            ipt[it1t3-2]=ipti1;
            ipt[it1t3-1]=ipti2;
            ipt[it1t3]  =ipl1;
            ipt[it2t3-2]=ipti2;
            ipt[it2t3-1]=ipti1;
            ipt[it2t3]  =ipl2;

	/*
	 * sets new flags.
	 */

            jwl=jwl+8;
            iwl[jwl-7]=ipl1;
            iwl[jwl-6]=ipti1;
            iwl[jwl-5]=ipti1;
            iwl[jwl-4]=ipl2;
            iwl[jwl-3]=ipl2;
            iwl[jwl-2]=ipti2;
            iwl[jwl-1]=ipti2;
            iwl[jwl]  =ipl1;
            for (jlt3 = 3 ; jlt3 <= nlt3 ; jlt3 += 3) {
               iplj1=ipl[jlt3-2];
               iplj2=ipl[jlt3-1];
               if ((iplj1 == ipl1 && iplj2 == ipti2) ||
                   (iplj2 == ipl1 && iplj1 == ipti2))   ipl[jlt3]=itf[1];
               if ((iplj1 == ipl2 && iplj2 == ipti1) ||
                   (iplj2 == ipl2 && iplj1 == ipti1))   ipl[jlt3]=itf[2];
            }
          }
          nlfc=nlf;
          nlf=jwl/2;
          if (nlf == nlfc) break;

	/*
	 * resets the iwl array for the next round.
	 */

          jwl1mn=2*nlfc+1;
          nlft2=nlf*2;
          for (jwl1 = jwl1mn ; jwl1 <= nlft2 ; jwl1++) {
             jwl=jwl1+1-jwl1mn;
             iwl[jwl]=iwl[jwl1];
          }
          nlf=jwl/2;
      } 
   }

	/*
	 * rearranges the ipt array so that the vertexes of each triangle
	 * are listed counter-clockwise.
	 */

   for (itt3 = 3 ; itt3 <= ntt3 ; itt3 += 3) {
      ip1=ipt[itt3-2];
      ip2=ipt[itt3-1];
      ip3=ipt[itt3];
      if (VPDT_t(xd[ip1],yd[ip1],xd[ip2],yd[ip2],xd[ip3],yd[ip3]) >= 0.0)
         continue;

      ipt[itt3-2]=ip2;
      ipt[itt3-1]=ip1;
   }
   *nt=nt0;
   *nl=nl0;

   return 0;
}

/****************************************************************************
 *
 * Function:	idgrid()
 *
 * Description:	
 * 
 * Returns:	none
 *
 ***************************************************************************/

void idgrid (xd, yd, nt, ipt, nl, ipl, nxi, nyi, xi, yi, ngp, igp)
   float	*xd,*yd;
   int		*ipt,*ipl;
   int		nt,nl,nxi,nyi;
   int		*ngp,*igp;
   float	*xi,*yi;
{
   int		nt0,nl0,nxi0,nyi0;
   int      	nxinyi;
   float	ximn,ximx,yimn,yimx;
   int		jngp0,jngp1,jigp0,jigp1,jigp1i,ngp0,ngp1,
		it0t3,it0,ip1,ip2,ip3;
   int		il0,il0t3;
   float	x1,y1,x2,y2,x3,y3,
		xmn,xmx,ymn,ymx;
   int		insd,ixi,iximx,iximn,iyi,izi;
   int		l;
   float	yii,xii;    
   int		ilp1,ilp1t3;
   int		flag;
   float 	dummy;
   float	vp1,vp2,vp3;

   nt0=nt;
   nl0=nl;
   nxi0=nxi;
   nyi0=nyi;
   nxinyi=nxi0*nyi0;
   ximn=AMIN(xi [1],xi [nxi0]);
   ximx=AMAX(xi [1],xi [nxi0]);
   yimn=AMIN(yi [1],yi [nyi0]);
   yimx=AMAX(yi [1],yi [nyi0]);

   iximx = iximn = 0;	/* gcc -Wall */

	/*
 	 * determines grid points inside the data area.
	 */

   jngp0=0;
   jngp1=2*(nt0+2*nl0)+1;
   jigp0=0;
   jigp1=nxinyi+1;
   for (it0 = 1 ; it0 <= nt0; it0++) {
      ngp0=0;
      ngp1=0;
      it0t3=it0*3;
      ip1=ipt [it0t3-2];
      ip2=ipt [it0t3-1];
      ip3=ipt [it0t3];
      x1=xd [ip1];
      y1=yd [ip1];
      x2=xd [ip2];
      y2=yd [ip2];
      x3=xd [ip3];
      y3=yd [ip3];
      dummy = AMIN(x1,x2);
      xmn=AMIN(dummy,x3);
      dummy = AMAX(x1,x2);
      xmx=AMAX(dummy,x3);
      dummy = AMIN(y1,y2);
      ymn=AMIN(dummy,y3);
      dummy = AMAX(y1,y2);
      ymx=AMAX(dummy,y3);

      insd=0;
     
      flag = 1;
      for (ixi = 1 ; ixi <= nxi0 ; ixi++) {
         if (xi [ixi] >= xmn && xi [ixi] <= xmx) {   
            if (insd == 1) continue;
            insd = 1;
            iximn = ixi;
         }
         else {
            if (insd == 0) continue;
            iximx=ixi-1;
            flag = 0;
            break;
         }
      }

      if (flag && insd != 0)
         iximx = nxi0; 

      if (insd != 0) {

         for (iyi = 1 ; iyi <= nyi0 ; iyi++) {
            yii=yi [iyi];
            if(yii < ymn || yii > ymx) continue;
            for (ixi = iximn ; ixi <= iximx ; ixi++) {
               xii=xi [ixi];
               l=0;
               vp1 = VPDT_g(x1,y1,x2,y2,xii,yii);
               vp2 = VPDT_g(x2,y2,x3,y3,xii,yii);
               vp3 = VPDT_g(x3,y3,x1,y1,xii,yii);

               if (vp1 < 0) continue;
               else if (vp1 == 0) l=1;

               if (vp2 < 0) continue;
               else if (vp2 == 0) l=1; 

               if (vp3 < 0) continue;
               else if (vp3 == 0) l=1;

               izi=nxi0*(iyi-1)+ixi;

               if(l != 1) {
                  ngp0=ngp0+1;
                  jigp0=jigp0+1;
                  igp [jigp0]=izi;
                  continue;
               }

               if (jigp1 <= nxinyi) {
                  flag = 0;
                  for (jigp1i = jigp1 ; jigp1i <= nxinyi ; jigp1i++) {
                     if (izi == igp [jigp1i]) {
                        flag = 1;
                        break;
                     }
                  }
                  if (flag) continue;
               }

               ngp1=ngp1+1;
               jigp1=jigp1-1;
               igp [jigp1]=izi;
            }
         }
      }
     jngp0=jngp0+1;
     ngp [jngp0]=ngp0;
     jngp1=jngp1-1;
     ngp [jngp1]=ngp1;
   }

	/*
 	 * determines grid points outside the data area.
 	 * in semi-infinite rectangular area.
 	 */

   for (il0 = 1 ; il0 <= nl0 ; il0++) {
      ngp0=0;
      ngp1=0;
      il0t3=il0*3;
      ip1=ipl [il0t3-2];
      ip2=ipl [il0t3-1];
      x1=xd [ip1];
      y1=yd [ip1];
      x2=xd [ip2];
      y2=yd [ip2];
      xmn=ximn;
      xmx=ximx;
      ymn=yimn;
      ymx=yimx;
      if(y2 >= y1)      xmn=AMIN(x1,x2);
      if(y2 <= y1)      xmx=AMAX(x1,x2);
      if(x2 <= x1)      ymn=AMIN(y1,y2);
      if(x2 >= x1)      ymx=AMAX(y1,y2);
      insd=0;

      flag = 1;
      for (ixi = 1 ; ixi <= nxi0 ; ixi++) {
         if (xi [ixi] >= xmn && xi [ixi] <= xmx) {   
            if (insd == 1) continue;
            insd = 1;
            iximn = ixi;
         }
         else {
            if (insd == 0) continue;
            iximx=ixi-1;
            flag = 0;
            break;
         }
      }

      if (flag && insd != 0)
         iximx = nxi0; 

      if (insd != 0) {

         for (iyi = 1 ; iyi <= nyi0 ; iyi++) {
            yii=yi [iyi];
            if(yii < ymn || yii > ymx) continue;
            for (ixi = iximn ; ixi <= iximx ; ixi++) {
               xii=xi [ixi];
               l=0;
               vp1 = VPDT_g(x1,y1,x2,y2,xii,yii);
               vp2 = SPDT_g(x2,y2,x1,y1,xii,yii);
               vp3 = SPDT_g(x1,y1,x2,y2,xii,yii);

               if (vp1 > 0) continue;
               else if (vp1 == 0) l=1;

               if (vp2 < 0) continue;
               else if (vp2 == 0) l=1; 

               if (vp3 < 0) continue;
               else if (vp3 == 0) l=1;

               izi=nxi0*(iyi-1)+ixi;

               if(l != 1) {
                  ngp0=ngp0+1;
                  jigp0=jigp0+1;
                  igp [jigp0]=izi;
                  continue;
               }

               if (jigp1 <= nxinyi) {
                  flag = 0;
                  for (jigp1i = jigp1 ; jigp1i <= nxinyi ; jigp1i++) {
                     if (izi == igp [jigp1i]) {
                        flag = 1;
                        break;
                     }
                  }
                  if (flag) continue;
               }

               ngp1=ngp1+1;
               jigp1=jigp1-1;
               igp [jigp1]=izi;
            }
         }
      }
      jngp0=jngp0+1;
      ngp [jngp0]=ngp0;
      jngp1=jngp1-1;
      ngp [jngp1]=ngp1;
     
      ngp0 = 0;
      ngp1 = 0;
      ilp1 = (il0 % nl0) + 1;
      ilp1t3 = ilp1*3;
      ip3 = ipl [ilp1t3 - 1];
      x3 = xd[ip3];
      y3 = yd[ip3];
      xmn = ximn;
      xmx = ximx;
      ymn = yimn;
      ymx = yimx; 

      if(y3 >= y2 && y2 >= y1)      xmn=x2;
      if(y3 <= y2 && y2 <= y1)      xmx=x2;
      if(x3 <= x2 && x2 <= x1)      ymn=y2;
      if(x3 >= x2 && x2 >= x1)      ymx=y2;

      insd=0;

      flag = 1;
      for (ixi = 1 ; ixi <= nxi0 ; ixi++) {
         if (xi [ixi] >= xmn && xi [ixi] <= xmx) {   
            if (insd == 1) continue;
            insd = 1;
            iximn = ixi;
         }
         else {
            if (insd == 0) continue;
            iximx=ixi-1;
            flag = 0;
            break;
         }
      }

      if (flag && insd != 0)
         iximx = nxi0; 

      if (insd != 0) {

         for (iyi = 1 ; iyi <= nyi0 ; iyi++) {
            yii=yi [iyi];
            if(yii < ymn || yii > ymx) continue;
            for (ixi = iximn ; ixi <= iximx ; ixi++) {
               xii=xi [ixi];
               l=0;
               vp1 = SPDT_g(x1,y1,x2,y2,xii,yii);
               vp2 = SPDT_g(x3,y3,x2,y2,xii,yii);

               if (vp1 > 0) continue;
               else if (vp1 == 0) l=1;

               if (vp2 > 0) continue;
               else if (vp2 == 0) l=1; 

               izi=nxi0*(iyi-1)+ixi;

               if(l != 1) {
                  ngp0=ngp0+1;
                  jigp0=jigp0+1;
                  igp [jigp0]=izi;
                  continue;
               }

               if (jigp1 <= nxinyi) {
                  flag = 0;
                  for (jigp1i = jigp1 ; jigp1i <= nxinyi ; jigp1i++) {
                     if (izi == igp [jigp1i]) {
                        flag = 1;
                        break;
                     }
                  }
                  if (flag) continue;
               }

               ngp1=ngp1+1;
               jigp1=jigp1-1;
               igp [jigp1]=izi;
            }
         }
      }
      jngp0=jngp0+1;
      ngp [jngp0]=ngp0;
      jngp1=jngp1-1;
      ngp [jngp1]=ngp1;
   }
}

/****************************************************************************
 *
 * Function:	idpdrv()
 *
 * Description:	
 * 
 * Returns:	none
 *
 ***************************************************************************/

void idpdrv (ndp,xd,yd,zd,nt,ipt,pd,wk)
   float	*xd,*yd,*zd;
   int		ndp,nt;
   int		*ipt;
   float	*wk,*pd;
{
   int		ipti[4];
   float	xv[4],yv[4],zv[4],zxv[4],zyv[4],
		w1[4],w2[4];
   int		ndp0,nt0,jpdmx,jpd,idp,
		jpt,jpt0,iv,it,jpd0;
   float	vpxx,vpyy,vpyx,vpxy;
   float	dx1,dy1,dz1,dx2,dy2,dz2;
   float	dzx1,dzy1,dzy2,dzx2;
   float	vpx,vpy,vpz,vpzmn;
   float	d12,d31,d23;
   float	wi;

   ndp0=ndp;
   nt0=nt;

	/*
 	 * clears the pd array.
	 */

   jpdmx=5*ndp0;
   for (jpd = 1 ; jpd <= jpdmx ; jpd++)
      pd [jpd] =0.0;

   for (idp = 1 ; idp <= ndp ; idp++)
      wk [idp]=0.0;

	/*
 	 * estimates zx and zy.
	 */

   for (it = 1 ; it <= nt0 ; it++) {
      jpt0=3*(it-1);
     
      for (iv = 1; iv <= 3 ; iv++) {
         jpt=jpt0+iv;
         idp=ipt [jpt];
         ipti [iv]=idp;
         xv [iv]=xd [idp];
         yv [iv]=yd [idp];
         zv [iv]=zd [idp];
      }
      dx1=xv[2]-xv[1];
      dy1=yv[2]-yv[1];
      dz1=zv[2]-zv[1];
      dx2=xv[3]-xv[1];
      dy2=yv[3]-yv[1];
      dz2=zv[3]-zv[1];
      vpx=dy1*dz2-dz1*dy2;
      vpy=dz1*dx2-dx1*dz2;
      vpz=dx1*dy2-dy1*dx2;
      vpzmn=fabs(dx1*dx2+dy1*dy2)*EPSILON;

      if(fabs(vpz) <= vpzmn) continue;

      d12=sqrt((xv[2]-xv[1])*(xv[2]-xv[1])+(yv[2]-yv[1])*(yv[2]-yv[1]));
      d23=sqrt((xv[3]-xv[2])*(xv[3]-xv[2])+(yv[3]-yv[2])*(yv[3]-yv[2]));
      d31=sqrt((xv[1]-xv[3])*(xv[1]-xv[3])+(yv[1]-yv[3])*(yv[1]-yv[3]));

      w1[1]=1.0/(d31*d12);
      w1[2]=1.0/(d12*d23);
      w1[3]=1.0/(d23*d31);

      w2[1]=vpz*w1[1];
      w2[2]=vpz*w1[2];
      w2[3]=vpz*w1[3];
      for (iv = 1 ; iv <= 3; iv++) {
         idp=ipti [iv];
         jpd0=5*(idp-1);
         wi=(w1 [iv]*w1 [iv])*w2 [iv];
         pd [jpd0+1]=pd [jpd0+1]+vpx*wi;
         pd [jpd0+2]=pd [jpd0+2]+vpy*wi;
         wk [idp]=wk [idp]+vpz*wi;
      }
   }

   for (idp = 1 ; idp <= ndp0 ; idp++) {
      jpd0 = 5*(idp-1);
      pd [jpd0+1] = -pd [jpd0+1]/wk [idp];
      pd [jpd0+2] = -pd [jpd0+2]/wk [idp];
   }

	/*
 	 * estimates zxx, zxy, and zyy.
	 */

   for (it = 1 ; it <= nt0 ; it++) {
      jpt0=3*(it-1);
      for (iv = 1 ; iv <= 3 ; iv++) {
         jpt=jpt0+iv;
         idp=ipt[jpt];
         ipti [iv]=idp;
         xv [iv]=xd[idp];
         yv [iv]=yd[idp];
         jpd0=5*(idp-1);
         zxv [iv]=pd[jpd0+1];
         zyv [iv]=pd[jpd0+2];
      }
      dx1=xv[2]-xv[1];
      dy1=yv[2]-yv[1];
      dzx1=zxv[2]-zxv[1];
      dzy1=zyv[2]-zyv[1];
      dx2=xv[3]-xv[1];
      dy2=yv[3]-yv[1];
      dzx2=zxv[3]-zxv[1];
      dzy2=zyv[3]-zyv[1];
      vpxx=dy1*dzx2-dzx1*dy2;
      vpxy=dzx1*dx2-dx1*dzx2;
      vpyx=dy1*dzy2-dzy1*dy2;
      vpyy=dzy1*dx2-dx1*dzy2;
      vpz=dx1*dy2-dy1*dx2;
      vpzmn=fabs(dx1*dx2+dy1*dy2)*EPSILON;
      if(fabs(vpz) <= vpzmn) continue;
      d12=sqrt((xv[2]-xv[1])*(xv[2]-xv[1])+(yv[2]-yv[1])*(yv[2]-yv[1]));
      d23=sqrt((xv[3]-xv[2])*(xv[3]-xv[2])+(yv[3]-yv[2])*(yv[3]-yv[2]));
      d31=sqrt((xv[1]-xv[3])*(xv[1]-xv[3])+(yv[1]-yv[3])*(yv[1]-yv[3]));
      w1[1]=1.0/(d31*d12);
      w1[2]=1.0/(d12*d23);
      w1[3]=1.0/(d23*d31);
      w2[1]=vpz*w1[1];
      w2[2]=vpz*w1[2];
      w2[3]=vpz*w1[3];
      for (iv = 1 ; iv <= 3 ; iv++) {
         idp=ipti[iv];
         jpd0=5*(idp-1);
         wi=(w1[iv]*w1[iv])*w2[iv];
         pd[jpd0+3]=pd[jpd0+3]+vpxx*wi;
         pd[jpd0+4]=pd[jpd0+4]+(vpxy+vpyx)*wi;
         pd[jpd0+5]=pd[jpd0+5]+vpyy*wi;
      }
   }

   for (idp = 1 ; idp <= ndp0 ; idp++) {
      jpd0=5*(idp-1);
      pd [jpd0+3] = -pd [jpd0+3]/wk [idp];
      pd [jpd0+4] = -pd [jpd0+4]/(2.0*wk [idp]);
      pd [jpd0+5] = -pd [jpd0+5]/wk [idp];
   }
}

/****************************************************************************
 *
 * Function:	idptip()
 *
 * Description:	
 * 
 * Returns:	an interpolated z value at grid coordinate xi,yi
 *
 ***************************************************************************/

float idptip (xd,yd,zd,nt,ipt,nl,ipl,pdd,iti,xii,yii)
   float	*xd,*yd,*zd;
   float	*pdd;
   int		*ipt,*ipl;
   float	xii,yii;
   int		nt,nl,iti;
{
   float	zii;
   float	x[4],y[4],z[4],pd[16];
   float	zu[4],zv[4],zuu[4],zuv[4],zvv[4];
   float	lu,lv;
   int		i,kpd;
   float	u,v;
   static int	itpv;
   static float	x0,y0,ap,bp,cp,dp;
   static float	p00,p10,p20,p30,p40,p50,p01,p11,p21,p31,p41;
   static float p02,p12,p22,p32,p03,p13,p23,p04,p14,p05; 
   float	z0;
   float	p0,p1,p2,p3,p4,*p5;
   float	g1,g2;
   int		it0,ntl,il1,il2,jpd,jipt,idp,jpdd,jipl;
   float	h1,h2,h3;
   float	a,b,c,d,ad,bc,dlt,aa,ab,adbc,bb,dd,bdt2,cd,cc,act2,ac;
   float	thus,thuv,thsv,csuv,thxu,dx,dy;

   p5 = &p50;

   it0 = iti;
   ntl = nt+nl;
   if (it0 <= ntl) {
      if (it0 != itpv) {

         jipt=3*(it0-1);
         jpd=0;
         for (i = 1 ; i <= 3 ; i++) {
            jipt=jipt+1;
            idp=ipt[jipt];
            x[i]=xd[idp];
            y[i]=yd[idp];
            z[i]=zd[idp];
            jpdd=5*(idp-1);
            for (kpd = 1 ; kpd <= 5 ; kpd++) {
               jpd=jpd+1;
               jpdd=jpdd+1;
               pd[jpd]=pdd[jpdd];
            }
         }

	/*
 	 * determines the coefficients for the coordinate system
 	 * transformation from the x-y system to the u-v system
 	 * and vice versa.
	 */

         x0=x[1];
         y0=y[1];
         a=x[2]-x0;
         b=x[3]-x0;
         c=y[2]-y0;
         d=y[3]-y0;
         ad=a*d;
         bc=b*c;
         dlt=ad-bc;
         ap = d/dlt;
         bp = -b/dlt;
         cp = -c/dlt;
         dp = a/dlt;

	/*
	 * converts the partial derivatives at the vertexes of the
	 * triangle for the u-v coordinate system.
	 */

         aa=a*a;
         act2=2.0*a*c;
         cc=c*c;
         ab=a*b;
         adbc=ad+bc;
         cd=c*d;
         bb=b*b;
         bdt2=2.0*b*d;
         dd=d*d;
         for (i = 1 ; i <= 3 ; i++) {
            jpd=5*i;
            zu[i]=a*pd[jpd-4]+c*pd[jpd-3];
            zv[i]=b*pd[jpd-4]+d*pd[jpd-3];
            zuu[i]=aa*pd[jpd-2]+act2*pd[jpd-1]+cc*pd[jpd];
            zuv[i]=ab*pd[jpd-2]+adbc*pd[jpd-1]+cd*pd[jpd];
            zvv[i]=bb*pd[jpd-2]+bdt2*pd[jpd-1]+dd*pd[jpd];
         }

	/*
	 *  calculates the coefficients of the polynomial.
	 */

         p00=z[1];
         p10=zu[1];
         p01=zv[1];
         p20=0.5*zuu[1];
         p11=zuv[1];
         p02=0.5*zvv[1];
         h1=z[2]-p00-p10-p20;
         h2=zu[2]-p10-zuu[1];
         h3=zuu[2]-zuu[1];
         p30= 10.0*h1-4.0*h2+0.5*h3;
         p40= -15.0*h1+7.0*h2    -h3;
         p50=  6.0*h1-3.0*h2+0.5*h3;
         h1=z[3]-p00-p01-p02;
         h2=zv[3]-p01-zvv[1];
         h3=zvv[3]-zvv[1];
         p03= 10.0*h1-4.0*h2+0.5*h3;
         p04= -15.0*h1+7.0*h2    -h3;
         p05=  6.0*h1-3.0*h2+0.5*h3;
         lu=sqrt(aa+cc);
         lv=sqrt(bb+dd);
         thxu=atan2(c,a);
         thuv=atan2(d,b)-thxu;
         csuv=cos(thuv);
         p41=5.0*lv*csuv/lu*p50;
         p14=5.0*lu*csuv/lv*p05;
         h1=zv[2]-p01-p11-p41;
         h2=zuv[2]-p11-4.0*p41;
         p21= 3.0*h1-h2;
         p31= -2.0*h1+h2;
         h1=zu[3]-p10-p11-p14;
         h2=zuv[3]-p11-4.0*p14;
         p12= 3.0*h1-h2;
         p13= -2.0*h1+h2;
         thus=atan2(d-c,b-a)-thxu;
         thsv=thuv-thus;
         aa= sin(thsv)/lu;
         bb= -cos(thsv)/lu;
         cc= sin(thus)/lv;
         dd= cos(thus)/lv;
         ac=aa*cc;
         ad=aa*dd;
         bc=bb*cc;
         g1=aa*ac*(3.0*bc+2.0*ad);
         g2=cc*ac*(3.0*ad+2.0*bc);
         h1= -aa*aa*aa*(5.0*aa*bb*p50+(4.0*bc+ad)*p41) -
             cc*cc*cc*(5.0*cc*dd*p05+(4.0*ad+bc)*p14);
         h2=0.5*zvv[2]-p02-p12;
         h3=0.5*zuu[3]-p20-p21;
         p22=(g1*h2+g2*h3-h1)/(g1+g2);
         p32=h2-p22;
         p23=h3-p22;
         itpv=it0;
      }
	/*
	 * converts xii and yii to u-v system.
	 */

      dx=xii-x0;
      dy=yii-y0;
      u=ap*dx+bp*dy;
      v=cp*dx+dp*dy;

	/*
	 * evaluates the polynomial.
	 */

      p0=p00+v*(p01+v*(p02+v*(p03+v*(p04+v*p05))));
      p1=p10+v*(p11+v*(p12+v*(p13+v*p14)));
      p2=p20+v*(p21+v*(p22+v*p23));
      p3=p30+v*(p31+v*p32);
      p4=p40+v*p41;
      zii=p0+u*(p1+u*(p2+u*(p3+u*(p4+u*(*p5)))));

      return zii;
   }
   else {
      il1 = it0/ntl;
      il2 = it0 - il1*ntl;
      if (il1 == il2) {
          
	/*
	 * calculation of zii by extrapolation in the rectangle.
	 * checks if the necessary coefficients have been calculated.
	 */

         if (it0 != itpv) {

	/*
	 * loads coordinate and partial derivative values at the end
	 * points of the border line segment.
	 */

            jipl=3*(il1-1);
            jpd=0;
            for (i = 1 ; i <= 2 ; i++) {
               jipl=jipl+1;
               idp=ipl[jipl];
               x[i]=xd[idp];
               y[i]=yd[idp];
               z[i]=zd[idp];
               jpdd=5*(idp-1);
               for (kpd = 1 ; kpd <= 5 ; kpd++) {
                  jpd=jpd+1;
                  jpdd=jpdd+1;
                  pd[jpd]=pdd[jpdd];
               }
            }
    
	/*
	 * determines the coefficients for the coordinate system
	 * transformation from the x-y system to the u-v system
	 * and vice versa.
	 */

            x0=x[1];
            y0=y[1];
            a=y[2]-y[1];
            b=x[2]-x[1];
            c= -b;
            d=a;
            ad=a*d;
            bc=b*c;
            dlt=ad-bc;
            ap= d/dlt;
            bp= -b/dlt;
            cp= -bp;
            dp= ap;

	/*
	 * converts the partial derivatives at the end points of the
	 * border line segment for the u-v coordinate system.
	 */

            aa=a*a;
            act2=2.0*a*c;
            cc=c*c;
            ab=a*b;
            adbc=ad+bc;
            cd=c*d;
            bb=b*b;
            bdt2=2.0*b*d;
            dd=d*d;
            for (i = 1 ; i <= 2 ; i++) {
               jpd=5*i;
               zu[i]=a*pd[jpd-4]+c*pd[jpd-3];
               zv[i]=b*pd[jpd-4]+d*pd[jpd-3];
               zuu[i]=aa*pd[jpd-2]+act2*pd[jpd-1]+cc*pd[jpd];
               zuv[i]=ab*pd[jpd-2]+adbc*pd[jpd-1]+cd*pd[jpd];
               zvv[i]=bb*pd[jpd-2]+bdt2*pd[jpd-1]+dd*pd[jpd];
            }

	/*
	 * calculates the coefficients of the polynomial.
	 */

            p00=z[1];
            p10=zu[1];
            p01=zv[1];
            p20=0.5*zuu[1];
            p11=zuv[1];
            p02=0.5*zvv[1];
            h1=z[2]-p00-p01-p02;
            h2=zv[2]-p01-zvv[1];
            h3=zvv[2]-zvv[1];
            p03= 10.0*h1-4.0*h2+0.5*h3;
            p04= -15.0*h1+7.0*h2    -h3;
            p05=  6.0*h1-3.0*h2+0.5*h3;
            h1=zu[2]-p10-p11;
            h2=zuv[2]-p11;
            p12= 3.0*h1-h2;
            p13= -2.0*h1+h2;
            p21=0.0;
            p23= -zuu[2]+zuu[1];
            p22= -1.5*p23;
            itpv=it0;
         }

	/*
	 * converts xii and yii to u-v system.
	 */

         dx=xii-x0;
         dy=yii-y0;
         u=ap*dx+bp*dy;
         v=cp*dx+dp*dy;

	/*
	 * evaluates the polynomial.
	 */

         p0=p00+v*(p01+v*(p02+v*(p03+v*(p04+v*p05))));
         p1=p10+v*(p11+v*(p12+v*p13));
         p2=p20+v*(p21+v*(p22+v*p23));
         zii=p0+u*(p1+u*p2);
         return zii;
      }
      else {
         if (it0 != itpv) {

	/*
	 * loads coordinate and partial derivative values at the vertex
	 * of the triangle.
	 */

            jipl=3*il2-2;
            idp=ipl[jipl];
            x0=xd[idp];
            y0=yd[idp];
            z0=zd[idp];
            jpdd=5*(idp-1);
            for (kpd = 1 ; kpd <= 5 ; kpd++) {
               jpdd=jpdd+1;
               pd[kpd]=pdd[jpdd];
            }

	/*
	 * calculates the coefficients of the polynomial.
	 */

            p00=z0;
            p10=pd[1];
            p01=pd[2];
            p20=0.5*pd[3];
            p11=pd[4];
            p02=0.5*pd[5];
            itpv=it0;
         }

	/*
	 * converts xii and yii to u-v system.
	 */

         u=xii-x0;
         v=yii-y0;
	
	/*
	 * evaluates the polynomial.
	 */

         p0=p00+v*(p01+v*p02);
         p1=p10+v*p11;
         zii=p0+u*(p1+u*p20);
         return zii;
      }
   }
}

/****************************************************************************
 *
 * Function:	idxchg()
 *
 * Description:	
 * 
 * Returns:	1 if a triangle interchange is needed, 0 otherwise
 *
 ***************************************************************************/

int idxchg (x,y,i1,i2,i3,i4)
   float	*x,*y;
   int		i1,i2,i3,i4;
{
   int		idx;
   float	u1,u2,u3,u4;
   float	s1sq,s2sq,s3sq,s4sq;
   float	x1,x2,x3,x4,y1,y2,y3,y4;
   float	a1sq,b1sq,c1sq,a2sq,b2sq,c3sq;

   x1=x[i1];
   y1=y[i1];
   x2=x[i2];
   y2=y[i2];
   x3=x[i3];
   y3=y[i3];
   x4=x[i4];
   y4=y[i4];

	/*
	 * calculation
	 */

   idx=0;
   u3=(y2-y3)*(x1-x3)-(x2-x3)*(y1-y3);
   u4=(y1-y4)*(x2-x4)-(x1-x4)*(y2-y4);
   if (u3*u4 <= 0.0)  
      return idx;

   u1=(y3-y1)*(x4-x1)-(x3-x1)*(y4-y1);
   u2=(y4-y2)*(x3-x2)-(x4-x2)*(y3-y2);

   a1sq=(x1-x3)*(x1-x3)+(y1-y3)*(y1-y3);
   b1sq=(x4-x1)*(x4-x1)+(y4-y1)*(y4-y1);
   c1sq=(x3-x4)*(x3-x4)+(y3-y4)*(y3-y4);
   a2sq=(x2-x4)*(x2-x4)+(y2-y4)*(y2-y4);
   b2sq=(x3-x2)*(x3-x2)+(y3-y2)*(y3-y2);
   c3sq=(x2-x1)*(x2-x1)+(y2-y1)*(y2-y1);

   s1sq=u1*u1/(c1sq*AMAX(a1sq,b1sq));
   s2sq=u2*u2/(c1sq*AMAX(a2sq,b2sq));
   s3sq=u3*u3/(c3sq*AMAX(b2sq,a1sq));
   s4sq=u4*u4/(c3sq*AMAX(b1sq,a2sq));
 
   if ((AMIN(s3sq,s4sq)-AMIN(s1sq,s2sq)) > EPSILON)
      idx=1;

   return idx;
}
