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
 * File:	renumber.c
 *									
 * Description:	Contains code to implement the Gibbs-Poole-Stockmeyer and
 *		Gibbs-King node renumbering algorithms for bandwidth/
 * 		profile reduction.  Basically the code is taken from
 *		ACM-TOMS algorithms 508 and 509 and translated, by hand,
 *		into C code - thus the reason some of it looks fairly
 *		nasty.  The driver routine sitting on top of the 
 *		original algorithms builds the necessary connectivity
 *		table from the node and element arrays.  If the renumbered
 *		graph does not have an improved bandwidth (or profile
 *		depending on the option being used) then no renumbering
 *		is done.
 *
 * History:	C translations, modifications and additions were
 *		initially made for v1.4 of FElt by J.I. Gobat
 *		
 ************************************************************************/

# include <vector>
# include <stdio.h>
# include <math.h>
# include "problem.h"
# include "renumber.hpp"
# include "cvector1.hpp"

using std::vector;

static int idpth;
static cvector1i nacum;
static cvector1i nhigh;
static cvector1i nlow;

static cvector1u Reduce (vector<cvector1u> &ndstk, const unsigned *nd_degrees, unsigned *old_numbers, unsigned int numnodes, unsigned int max_degree, unsigned int prof);
static int      SortBySize (int *size, int *stpt, int xc);
static int	 FindDiameter (int *snd1, int *snd2, vector<cvector1u> &ndstk, unsigned int numnodes, const unsigned *nd_degrees,
                           int *lvl, int *lvls1, int *lvls2, int *iwk, int *ndlst);
static void     PickLevel(int *lvls1, int *lvls2, int *ccstor, int idflt, int *isdir, int xc,
                          int *size, int *stpt);
static void     SortByDegree(int *stk1, int *stk2, int *x1, int x2, const unsigned *nd_degrees);
static void     SetupLevels (int *lvl, int *lvls1, int *lvls2, unsigned int numnodes);
static void     ProduceNumbering (int snd, int *num, vector<cvector1u> &ndstk, int *lvls2, const unsigned *nd_degrees,
                                  unsigned *renum, int *lvlst, int *lstpt, unsigned int numnodes,
                                  int nflg, int *ibw2, int *ipf2, int *ipfa, int isdir, unsigned int ideg, int *stkd);
static void     ReduceProfile (unsigned int numnodes, vector<cvector1u> &ndstk, unsigned *new_numbers, const unsigned *nd_degrees,
                               int *lvls2, int *lvlst, int *lstpt, int *nxtnum, int *conect, int *smlst);
static int      MinimumConnection (int *x, int xsze, int *y, int ysze, int *conlst, int *consze, vector<cvector1u> &ndstk, 
                                   const unsigned *nd_degrees, int *smlst);
static void     DeleteGraphElement (int *set, int *setsze, int elemnt);
static void     FormLevel (int *set, int *setsze, int *lstpt, int *lvlst, int level);
static void     CheckReverse (int *bestbw, int *bestpf, unsigned *new_numbers, vector<cvector1u> &ndstk, unsigned int numnodes,
                              const unsigned *nd_degrees, unsigned *iwk);
static void     ComputeBandwidth (vector<cvector1u> &ndstk, unsigned int numnodes,
                                  const unsigned *nd_degrees, const unsigned *old_numbers, int *ibw1, int *ipf1);
static void     DropTree (int iroot, vector<cvector1u> &ndstk, int *lvl, int *iwk, 
                          const unsigned *nd_degrees, int *lvlwth, int *lvlbot, int *lvln, int *maxlw, int ibort);

void
RestoreNodeNumbers(Node *node, unsigned *old_numbers, unsigned int numnodes)
{
   if (!(old_numbers != NULL))
       return;

   for (size_t i = 1 ; i <= numnodes ; i++)
      node [i] -> number = old_numbers [i]; 

   return;
}

cvector1u
RenumberNodes(Node *node, Element *element, unsigned int numnodes, unsigned int numelts)
{
   char		flag;
   unsigned	i,j;
   unsigned	size_deg;
   unsigned	max_degree;
   unsigned	degree;
   unsigned	connect;
   unsigned	n;
   unsigned	number;

   cvector1u old_numbers(numnodes);
   for (i = 1 ; i <= numnodes ; i++)
      old_numbers [i] = node [i] -> number;

   cvector1<char> flags(numnodes);

   cvector1<unsigned> nd_degrees(numnodes);

   // this struct is quite weird, so it's simpler to waste one empty
   // cvector1u for simplicity.
   vector<cvector1u> ndstk(numnodes+1);
   for (i = 1 ; i <= numnodes ; i++)
       ndstk[i] = cvector1u(10);

	/*
	 * these need to be dimensioned to the maximum
	 * number of levels - there should never be more than numnodes
	 * levels
	 */

   nacum.resize(numnodes);
   nhigh.resize(numnodes);
   nlow.resize(numnodes);

	/* 
	 * form the connectivity table, ndstk, from the node and 
	 * element arrays 
	 */ 

   max_degree = 0; 
   for (n = 1 ; n <= numnodes ; n++) { 
      degree = 0; 
      size_deg = 10; 
      number = node[n] -> number; 
      for (i = 1; i <= numnodes ; i++)
         flags [i] = 0;
 
      for (i = 1 ; i <= numelts ; i++) {
         flag = 0; 
         for (j = 1 ; j <= element[i] -> definition -> numnodes ; j++) {
            if (element [i] -> node[j] && element[i] -> node[j] -> number == number) {
               flag = 1;
               break;
            }
         }

         if (flag) {
            for (j = 1 ; j <= element[i] -> definition -> numnodes ; j++) {
               if (element[i] -> node[j]) {
                  connect = element[i] -> node[j] -> number;
                  if (flags [connect] || connect == number)
                     continue;
                 
                  degree ++;
                  if (degree > size_deg) {
                     size_deg += 10;
                     ndstk[number].resize(size_deg);
                  } 
 
                  ndstk [number][degree] = connect;
                  flags [connect] = 1;
               }
            }
         }   
      }
      if (degree > max_degree)
         max_degree = degree;

      nd_degrees [number] = degree;
   }

	/*
	 * call the main renumbering routine
	 */
               
   cvector1u new_numbers = Reduce (ndstk,nd_degrees.c_ptr1(),old_numbers.c_ptr1(),numnodes,max_degree,1);

	/*
	 * Renumber the nodes and free the new_numbers array if we got
	 * something other than our old numbers back.  If we got NULL
	 * for new_numbers then we don't even need to keep the old_numbers
	 * because we're not going to rearrange anything and we'll just 
	 * pass NULL back for old_numbers to indicate that nothing
	 * has been done.
	 */

   if (!new_numbers.empty()) {
      for (i = 1 ; i <= numnodes ; i++) 
         node[i] -> number = new_numbers [i];
   } else {
       old_numbers.resize(0);
   }
   
   // not strictly necessary, but free up some space till next call
   nacum.resize(0);
   nhigh.resize(0);
   nlow.resize(0);

   return old_numbers;
}

static cvector1u
Reduce(vector<cvector1u> &ndstk, const unsigned *nd_degrees, unsigned *old_numbers, unsigned int numnodes, unsigned int max_degree, unsigned int prof)
{
   unsigned	i;
   unsigned	flag;
   int		stnode, rvnode, xc,
		stnum, sbnum;
   int		ibw2, ipf2, ibw1, ipf1;
   int		nflg;
   int		isdir;
   int		num;
   int		lroot;
   int		lvln, lvlwth, lvlbot, maxlw;
   int		idflt,lowdg;

   cvector1u new_numbers(numnodes+1);
   cvector1i ccstor(numnodes);
   cvector1i lvl(numnodes+1);
   cvector1i lvls1(numnodes);
   cvector1i lvls2(numnodes);

	/*
	 * these are working arrays that other routines will need
	 */

   cvector1u iwk(numnodes);
   cvector1i ccblock(numnodes);

	/*
	 * these need to be as large as the number of connected components;
	 * there shouldn't be more than numnodes components
	 */

   cvector1i stpt(numnodes);
   cvector1i size(numnodes);

   ibw2 = ipf2 = 0;

   for (i = 1 ; i <= numnodes ; i++)
      new_numbers [i] = 0;

   ComputeBandwidth (ndstk, numnodes, nd_degrees, old_numbers, &ibw1, &ipf1);
   
   sbnum = 1;
   stnum = 1;

   for (i = 1 ; i <= numnodes ; i++) {
      if (nd_degrees [i] == 0) {
         new_numbers [i] = stnum;
         stnum--;
      }
   }

   flag = 1;
   while (flag) {
      lowdg = max_degree + 1;
      nflg = 1;
      isdir = 1;
      for (i = 1 ; i <= numnodes ; i++) {
         if (nd_degrees [i] >= lowdg || new_numbers [i] > 0)
            continue;

         lowdg = nd_degrees [i];
         stnode = i;
      }

      idflt = FindDiameter (&stnode, &rvnode, ndstk, numnodes, nd_degrees, 
                            lvl.c_ptr1(), lvls1.c_ptr1(), lvls2.c_ptr1(), ccstor.c_ptr1(), ccblock.c_ptr1());

      if (!prof) {
         if (nd_degrees [stnode] > nd_degrees [rvnode]) {
            nflg = -1;
            stnode = rvnode;
         }
      }

      SetupLevels (lvl.c_ptr1(), lvls1.c_ptr1(), lvls2.c_ptr1(), numnodes);

      xc = 0;
      lroot = 1;
      lvln = 1;
      for (i = 1 ; i <= numnodes ; i++) {
         if (lvl [i] != 0)
            continue;
      
         xc++;
         stpt [xc] = lroot;
         DropTree (i, ndstk, lvl.c_ptr1(), ccstor.c_ptr1(), nd_degrees, 
                   &lvlwth, &lvlbot, &lvln, &maxlw, numnodes);

         size [xc] = lvlbot + lvlwth - lroot;
         lroot = lvlbot + lvlwth;
         lvln = lroot;
      }

      if (SortBySize (size.c_ptr1(), stpt.c_ptr1(), xc) != 0) 
          PickLevel (lvls1.c_ptr1(), lvls2.c_ptr1(), ccstor.c_ptr1(), idflt, &isdir, xc, size.c_ptr1(), stpt.c_ptr1());

      isdir *= nflg;
      num = sbnum;
   
      if (prof) {
          ReduceProfile (numnodes, ndstk, new_numbers.c_ptr1(), nd_degrees,
                         lvls2.c_ptr1(), lvls1.c_ptr1(), lvl.c_ptr1(), &num, ccblock.c_ptr1(), ccstor.c_ptr1());

         sbnum = num;

         if (sbnum <= stnum)
            continue; 

         CheckReverse (&ibw2, &ipf2, new_numbers.c_ptr1(), ndstk, 
                       numnodes, nd_degrees, iwk.c_ptr1());

         flag = 0;
      }
      else {
         if (isdir < 0)
            num = stnum;

         ProduceNumbering (stnode, &num, ndstk, lvls2.c_ptr1(), nd_degrees, new_numbers.c_ptr1(),
                           lvls1.c_ptr1(), lvl.c_ptr1(), numnodes,nflg,&ibw2,&ipf2,ccstor.c_ptr1(),isdir,
                           max_degree,ccblock.c_ptr1());

         if (isdir < 0)
            stnum = num;
         else if (isdir > 0)
            sbnum = num;

         if (sbnum > stnum)
            flag = 0;
      }
   }

   detail ("Initial Bandwidth = %d",ibw1);
   detail ("Initial Profile   = %d",ipf1);
   detail ("Final Bandwidth   = %d",ibw2);
   detail ("Final Profile     = %d",ipf2);

   if ((!prof && ibw2 < ibw1) || (prof && ipf2 < ipf1))
      return new_numbers;
   else 
      return cvector1u(0);
}

static void
ComputeBandwidth(vector<cvector1u> &ndstk, unsigned int numnodes, const unsigned *nd_degrees, const unsigned *old_numbers, int *ibw1, int *ipf1)
{
   unsigned	i,j;
   int		itst,idif,irw;

   *ibw1 = *ipf1 = 0;

   for (i = 1 ; i <= numnodes ; i++) {

      irw = 0;
      for (j = 1 ; j <= nd_degrees [i] ; j++) {
         itst = ndstk [i][j];
         idif = old_numbers[i] - old_numbers[itst];

         if (irw < idif)
            irw = idif; 
      }

      *ipf1 += irw;
      if (irw > *ibw1)
         *ibw1 = irw;
   }

   return;
}

static int
FindDiameter(int *snd1, int *snd2, vector<cvector1u> &ndstk, unsigned int numnodes, const unsigned *nd_degrees, 
             int *lvl, int *lvls1, int *lvls2, int *iwk, int *ndlst)
{
   int		idflt;
   int		snd, mtw1, mtw2;
   int		lvln, lvlbot;
   int		lvlwth, maxlw, ndxn, ndxl;
   unsigned	i;
   unsigned	flag;
   unsigned	do_tree;

   ndxn = 0;
   ndxl = 0;
   mtw1 = 0;
   maxlw = 0;
   lvlwth = 0;
   lvln = 0;
   lvlbot = 0;
   mtw2 = numnodes;
   snd = *snd1;

   flag = 0;
   do_tree = 1;
   while (1) {
      if (do_tree) {
         for (i = 1 ; i <= numnodes ; i++)
            lvl [i] = 0;

         lvln = 1;
         
         DropTree (snd, ndstk, lvl, iwk, nd_degrees, &lvlwth, 
                   &lvlbot, &lvln, &maxlw, mtw2);

      }

      if (flag == 0 || do_tree == 0) {
         flag = 1;

         idpth = lvln - 1;
         mtw1 = maxlw;
     
         for (i = 1 ; i <= numnodes ; i++)
            lvls1 [i] = lvl [i];

         ndxn = 1;
         ndxl = 0;
         mtw2 = numnodes;

         SortByDegree (ndlst, iwk + lvlbot - 1, &ndxl, 
                       lvlwth, nd_degrees);
         snd = ndlst [1];

         do_tree = 1;
         continue;
      }
      else {
         if (idpth < lvln - 1) {
            *snd1 = snd;
            do_tree = 0;
            continue;
         }
 
         if (maxlw < mtw2) {
            mtw2 = maxlw;
            *snd2 = snd;

            for (i = 1 ; i <= numnodes ; i++)
               lvls2 [i] = lvl [i];
         }

         if (ndxn != ndxl) {
            ndxn++;
            snd = ndlst [ndxn];
            do_tree = 1;
            continue;
         }

         if (mtw2 <= mtw1)
            idflt = 2;
         else
            idflt = 1;

         break;
      }
   }

   return idflt;
}

static void
DropTree(int iroot, vector<cvector1u> &ndstk, int *lvl, int *iwk,
         const unsigned *nd_degrees, int *lvlwth, int *lvlbot, int *lvln, int *maxlw, int ibort)
{
   unsigned	j;
   int		itop; 
   int		inow;
   int		itest;
   int		lvltop;
   int		iwknow,ndrow;

   *maxlw = 0;
   itop = *lvln;
   inow = *lvln;
   *lvlbot = *lvln;
   lvltop = *lvln + 1;
   *lvln = 1;

   lvl [iroot] = 1;
   iwk [itop] = iroot;

   while (1) {
      *lvln = *lvln + 1;
      while (1) {
         iwknow = iwk [inow];
         ndrow = nd_degrees [iwknow]; 
 
         for (j = 1 ; j <= ndrow ; j++) {
            itest = ndstk [iwknow][j];
            if (lvl [itest] == 0) {
               lvl [itest] = *lvln;
               itop++;
               iwk [itop] = itest;
            }
         }

         inow++;
         if (inow < lvltop)
            continue;

         *lvlwth = lvltop - *lvlbot;
         if (*maxlw < *lvlwth)
            *maxlw = *lvlwth;

         if (*maxlw >= ibort || itop < lvltop)
            return;

         *lvlbot = inow;
         lvltop = itop + 1;
         break;
      }
   }
}
  
static void
SortByDegree(int *stk1, int *stk2, int *x1, int x2, const unsigned *nd_degrees)
{
       
   int		ind, istk2, jstk2;
   int		temp;
   int		itest;
   unsigned	i,j;

   ind = x2;
   while (1) {
      itest = 0;
      ind--;
       
      if (ind < 1)
         break;

      for (i = 1 ; i <= ind ; i++) {
         j = i+1;
         istk2 = stk2 [i];
         jstk2 = stk2 [j];

         if (nd_degrees [istk2] <= nd_degrees [jstk2])
            continue;

         itest = 1;
         temp = stk2 [i];
         stk2 [i] = stk2 [j];
         stk2 [j] = temp;
      }
      if (itest == 1)
         continue;
      else
         break;
   }

   for (i = 1 ; i <= x2 ; i++) {
      (*x1) ++;
      stk1 [*x1] = stk2 [i];
   }

   return;
}

static void
SetupLevels(int *lvl, int *lvls1, int *lvls2, unsigned int numnodes)
{
   unsigned	i;
   int		itemp;

   for (i = 1 ; i <= idpth ; i++)
      nacum [i] = 0;

   for (i = 1 ; i <= numnodes ; i++) {
      lvl [i] = 1;
      lvls2 [i] = idpth + 1 - lvls2 [i];
      itemp = lvls2 [i]; 
     
      if (itemp > idpth)
         continue;
      
      if (itemp == lvls1 [i]) {
         nacum [itemp] = nacum [itemp] + 1;
         continue;
      }
    
      lvl [i] = 0;
   }

   return;
}

static int
SortBySize(int *size, int *stpt, int xc)
{
   int		ind, temp, itest;
   unsigned	i,j;

   if (xc == 0)
      return 0;

   ind = xc;
   while (1) {
      itest = 0;
      ind--;
      if (ind < 1)
         return 1;

      for (i = 1 ; i <= ind ; i++) {
         j = i + 1;
         if (size [i] >= size [j])
            continue;

         itest = 1;
         temp = size [i];
         size [i] = size [j];
         size [j] = temp;

         temp = stpt [i];
         stpt [i] = stpt [j];
         stpt [j] = temp;
      }

      if (itest == 1)
         continue;
      else
         break;
   }

   return 1;
}
     
static void
PickLevel(int *lvls1, int *lvls2, int *ccstor, int idflt, int *isdir, int xc, int *size, int *stpt)
{
   unsigned	i,k,j;
   int		end, it;
   int		max1, max2;
   int		inode;
   int		lvlnh, lvlnl;

   for (i = 1 ; i <= xc ; i++) {
      j = stpt [i];
      end = size [i] + j - 1;

      for (k = 1 ; k <= idpth ; k++) {
         nhigh [k] = nacum [k];
         nlow [k] = nacum [k];
      }

      for (k = j ; k <= end ; k++) {
         inode = ccstor [k];

         lvlnh = lvls1 [inode];
         nhigh [lvlnh] ++;

         lvlnl = lvls2 [inode];
         nlow [lvlnl] ++;
      }
      max1 = max2 = 0;

      for (k = 1 ; k <= idpth ; k++) {
         if (2*nacum [k] != nlow [k] + nhigh [k]) {
            if (nhigh [k] > max1)
               max1 = nhigh [k];
             
            if (nlow [k] > max2)
               max2 = nlow [k];
         }
      }

      it = 1;
      if (max1 > max2)
         it = 2;
      else if (max1 == max2)
         it = idflt;

      if (it != 2) {
         if (i == 1)
            *isdir = -1;

         for (k = j ; k <= end ; k++) {
            inode = ccstor [k];
            lvls2 [inode] = lvls1 [inode];
         }

         for (k = 1 ; k <= idpth ; k++) 
            nacum [k] = nhigh [k];

         continue;
      }

      for (k = 1 ; k <= idpth ; k++)
         nacum [k] = nlow [k];
   }

   return;
}

static void
ProduceNumbering(int snd, int *num, vector<cvector1u> &ndstk, int *lvls2, const unsigned *nd_degrees, 
                 unsigned *renum, int *lvlst, int *lstpt, unsigned int numnodes,
                 int nflg, int *ibw2, int *ipf2, int *ipfa, int isdir, unsigned int ideg, int *stkd)
{
   int		*stka, *stkb, *stkc;
   int 		xa, xb, xc, xd, cx;
   int		end;
   int		test, nstpt, lvln;
   int		inx, nbw;
   int		lst, lnd, ipro;
   unsigned	i,j;
   int 		max;

   stka = nhigh.c_ptr1();
   stkb = nlow.c_ptr1();
   stkc = nacum.c_ptr1();

   for (i = 1 ; i <= numnodes ; i++)
      ipfa [i] = 0;

   nstpt = 1;
   for (i = 1 ; i <= idpth ; i++) {
      lstpt [i] = nstpt;
      for (j = 1 ; j <= numnodes ; j++) {
         if (lvls2 [j] == i) {
            lvlst [nstpt] = j;
            nstpt++;
         }
      }
   }

   lstpt [idpth + 1] = nstpt;
   lvln = 0;
   if (nflg < 0)
      lvln = idpth + 1;

   xc = 1;
   stkc [xc] = snd;

   while (1) {

      cx = 1;
      xd = 0;
      lvln += nflg;
      lst = lstpt [lvln];
      lnd = lstpt [lvln + 1] - 1; 

      while (1) {
         ipro = stkc [cx];
         renum [ipro] = *num;
         *num += isdir;
         end = nd_degrees [ipro];
         xa = xb = 0; 
         for (i = 1 ; i <= end ; i++) {
            test = ndstk [ipro][i];
            inx = renum [test];

            if (inx != 0) {
               if (inx < 0)
                  continue;

               nbw = (renum [ipro] - inx) * isdir;
               if (isdir > 0)
                  inx = renum [ipro];

               if (ipfa [inx] < nbw)
                  ipfa [inx] = nbw;

               continue;
            }

            renum [test] = -1;
            if (lvls2 [test] != lvls2 [ipro]) {
               xb++;
               stkb [xb] = test;
               continue;
            }

            xa++;
            stka [xa] = test;
         }

         if (xa != 0) {
            if (xa != 1) 
               SortByDegree (stkc, stka, &xc, xa, nd_degrees);
            else {
               xc++; 
               stkc [xc] = stka [xa];
            }
         }

         if (xb != 0) {
            if (xb != 1)
               SortByDegree (stkd, stkb, &xd, xb, nd_degrees);
            else {
               xd++;
               stkd [xd] = stkb [xb];
            }
         }

         cx++;
         if (xc >= cx)
            continue;

         max = ideg + 1; 
         snd = numnodes + 1;

         for (i = lst ; i <= lnd ; i++) {
            test = lvlst [i];

            if (renum [test] != 0 || nd_degrees [test] >= max)
               continue;

            renum [snd] = 0;
            renum [test] = -1;
            max = nd_degrees [test];
            snd = test;
         }

         if (snd != numnodes + 1) {
            xc++;
            stkc [xc] = snd;
            continue;
         }

         break;
      }
    
      if (xd == 0)
         break;

      for (i = 1 ; i <= xd ; i++)
         stkc [i] = stkd [i];

      xc = xd;
      continue;
   }

   for (i = 1 ; i <= numnodes ; i++) {
      if (ipfa [i] > *ibw2)
         *ibw2 = ipfa [i];
        
      *ipf2 += ipfa [i];
   }

   return;
}

static void
ReduceProfile(unsigned int numnodes, vector<cvector1u> &ndstk, unsigned *new_numbers, const unsigned *nd_degrees,
              int *lvls2, int *lvlst, int *lstpt, int *nxtnum, int *conect, int *smlst)
{
   int		*s2,*s3,*q;
   int		s2sze, s3sze, qptr, consze;
   int		nstpt;
   unsigned	i,j;
   int		level;
   unsigned	quit_flag;
   int		iq, ns2;
   int		m;

   s2 = nhigh.c_ptr1();
   s3 = nlow.c_ptr1();
   q = nacum.c_ptr1();

   nstpt = 1;
   for (i = 1 ; i <= idpth ; i++) {
      lstpt [i] = nstpt;
      for (j = 1 ; j <= numnodes ; j++) {
         if (lvls2 [j] == i) {
            lvlst [nstpt] = j;
            nstpt++;
         }
      }
   }
   
   lstpt [idpth + 1] = nstpt;
   level = 1;

   FormLevel (s2, &s2sze, lstpt, lvlst, level);
   
   while (1) {
      FormLevel (s3, &s3sze, lstpt, lvlst, level+1);
      qptr = 0;

      while (1) {
         m = MinimumConnection (s2, s2sze, s3, s3sze, conect, &consze, 
                                ndstk, nd_degrees, smlst);

         new_numbers [m] = *nxtnum;
         (*nxtnum) ++;
         DeleteGraphElement (s2, &s2sze, m);

         if (consze > 0) {
            for (i = 1 ; i <= consze ; i++) {
               qptr++;
               q [qptr] = conect [i];
               DeleteGraphElement (s3, &s3sze, conect [i]);
            }
         }

         if (s2sze > 0) {

            if (s3sze > 0) 
               continue;

            for (i = 1 ; i <= s2sze ; i++) {
               ns2 = s2 [i];
               new_numbers [ns2] = *nxtnum;
               (*nxtnum) ++;
            } 
         }
         else {
            if (s3sze > 0) {
               for (i = 1 ; i <= s3sze ; i++) {
                  qptr++;
                  q [qptr] = s3 [i];
               }
            }
         }

         level++;
         if (level >= idpth) {
            quit_flag = 1;
            break;
         }

         for (i = 1 ; i <= qptr ; i++) 
            s2 [i] = q[i];

         s2sze = qptr;

         quit_flag = 0;
         break;
      }
      if (quit_flag)
         break;
   }

   for (i = 1 ; i <= qptr ; i++) {
      iq = q [i];
      new_numbers [iq] = *nxtnum;
      (*nxtnum) ++;
   }

   return;
}
         
static int
MinimumConnection(int *x, int xsze, int *y, int ysze, int *conlst, int *consze, vector<cvector1u> &ndstk,
                  const unsigned *nd_degrees, int *smlst)
{
   unsigned	i,j,k;
   int		ix;
   int		lstsze;
   int		mincon;
   int		irowdg;
   unsigned	quit_flag;
     
   mincon = 0;
   *consze = ysze + 1;
   for (i = 1 ; i <= xsze ; i++) {

      lstsze = 0;
      ix = x [i];
      irowdg = nd_degrees [ix];

      quit_flag = 0;
      for (j = 1 ; j <= ysze ; j++) {
         for (k = 1 ; k <= irowdg ; k++) {

            ix = x [i];
            if (ndstk [ix][k] != y [j])
               continue;

            smlst [lstsze + 1] = y [j];  
            lstsze ++;

            if (lstsze >= *consze) {
               quit_flag = 1;
               break;
            }

            break;
         }
         if (quit_flag)
            break;
      }

      if (lstsze <= 0) {
         *consze = 0;
         return x [i];
      }

      *consze = lstsze;
      for (j = 1 ; j <= lstsze ; j++)
         conlst [j] = smlst [j];

      mincon = x [i];
   }

   return mincon;
}

static void
DeleteGraphElement(int *set, int *setsze, int elemnt)
{
   unsigned	i,j;

   if (*setsze <= 1) {
      *setsze = 0;

      return;
   }

   for (i = 1 ; i <= *setsze ; i++) {
      if (set [i] == elemnt)
         break;
   }

   (*setsze) --;
   for (j = i ; j <= *setsze ; j++) 
      set [j] = set [j+1];

   return;
}
  
static void 
FormLevel(int *set, int *setsze, int *lstpt, int *lvlst, int level)
{
   int		upper, lower;
   unsigned	i;

   lower = lstpt [level];
   upper = lstpt [level + 1] - 1;
   *setsze = 1;

   for (i = lower ; i <= upper ; i++) {
      set [*setsze] = lvlst [i];
      (*setsze) ++;
   }

   (*setsze) --;
   return;
}   
     
static void
CheckReverse(int *bestbw, int *bestpf, unsigned *new_numbers, vector<cvector1u> &ndstk, unsigned int numnodes, 
             const unsigned *nd_degrees, unsigned *iwk)
{
   unsigned	i;
   int		rev_bw, rev_pf;

   for (i = 1 ; i <= numnodes ; i++)
      iwk [i] = numnodes - new_numbers [i] + 1;

   ComputeBandwidth (ndstk,numnodes,nd_degrees,new_numbers,bestbw,bestpf);
   ComputeBandwidth (ndstk,numnodes,nd_degrees,iwk,&rev_bw,&rev_pf);

   detail ("best bandwidth     = %d, best profile     = %d",*bestbw,*bestpf);
   detail ("reversed bandwidth = %d, reversed profile = %d",rev_bw, rev_pf);

   if (rev_pf >= *bestpf)
      return;

   for (i = 1 ; i <= numnodes ; i++)
      new_numbers [i] = iwk [i];

   *bestpf = rev_pf;
   *bestbw = rev_bw;

   return;
}
