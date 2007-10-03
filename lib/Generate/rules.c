/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993 Jason I. Gobat and Darren C. Atkinson

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

/****************************************************************************
 *
 * File:	rules.c
 *
 * Description: each function returns the position of a node along a line
 *		given that this is the i_th node along this line
 *		of length L spanned by ne elements (or nn + 1 nodes)
 *
 ***************************************************************************/

# include <math.h>
# include "fe.h"
# include "mesh.h"
# include "rules.h"

void *AssignRule (Rule type)
{
   double (*func) ();

   if (type == CosRule)
      func = cos_rule;
   else if (type == SinRule)
      func = sin_rule;
   else if (type == LogRule)
      func = log_rule;
   else if (type == ParabolicRule)
      func = parabolic_rule;
   else if (type == RevLogRule)
      func = reverse_log_rule;
   else if (type == RevParabolicRule)
      func = reverse_parabolic_rule;
   else
      func = linear_rule;

   return (void *) func;
}
 
double linear_rule (int i, int ne, double L)
{
   if (ne <= 0)
      return 0.0;
   
   return L * ((double) (i - 1) / (double) ne);
}

double cos_rule (int i, int ne, double L)
{
   double   n;
   double   x;

   if (ne <= 0)
      return 0.0;

   n = M_PI_2 * ((double) (i - 1) / (double) ne);

   x = L - L*cos(n);

   return x;
}   

double sin_rule (int i, int ne, double L)
{
   double   n;
   double   x;

   if (ne <= 0)
      return 0.0;

   n = M_PI_2 * ((double) (i - 1) / (double) ne);
   
   x = L*sin(n);
   
   return x;
}   

double log_rule (int i, int ne, double L)
{
   double   n;
   double   x;

   if (ne <= 0)
      return 0.0;

   n = 9.0 * ((double) (i - 1) / (double) ne);
   
   x = L*log10(n + 1);
   
   return x;
}   

double parabolic_rule (int i, int ne, double L)
{
   double   n;
   double   x;

   if (ne <= 0)
      return 0.0;

   n = (double) (i - 1) / (double) ne;
   
   x = L*n*n;
   
   return x;
}   

double reverse_log_rule (int i, int ne, double L)
{
   double   n;
   double   x;

   if (ne <= 0)
      return 0.0;

   n = 9.0 * ((double) (i - 1) / (double) ne);
   
   x = L - L*log10(10 - n);
   
   return x;
}   

double reverse_parabolic_rule (int i, int ne, double L)
{
   double   n;
   double   x;

   if (ne <= 0)
      return 0.0;

   n = (double) (i - 1) / (double) ne;
   
   x = L*sqrt(n);
   
   return x;
}   
