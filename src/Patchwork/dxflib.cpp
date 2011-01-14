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

/******************************************************************************
 *
 * File:	dxflib.c
 *
 * Description:	contains low-level code to read and write DXF files
 *
 *****************************************************************************/

# include <math.h>
# include <stdio.h>
# include <string.h>
# include "dxf.h"

extern "C" void error (const char *format, ...);

int WriteDXFHeader (FILE *output)
{
   fprintf (output, "  %d\nSECTION\n",STRING);
   fprintf (output, "  %d\nENTITIES\n",HEADER);

   return 0;
}

int ReadDXFHeader (FILE *input)
{
   int		dummy;
   char		buffer [256];

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, STRING))
      return 1;

   fscanf (input, "%s", buffer);
   if (strcmp (buffer, "SECTION"))
      return 1;

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, HEADER))
      return 1;

   fscanf (input, "%s", buffer);
   if (strcmp (buffer, "ENTITIES"))
      return 1;

   return 0;
} 

int ReadDXFEntity (FILE *input)
{
   int		dummy;
   char		buffer [256];

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, STRING)) 
      return DXF_ERROR;

   fscanf (input, "%s", buffer);
   if (!strcmp (buffer, "ENDSEC")) 
      return DXF_ENDSEC;
   else if (!strcmp (buffer, "POLYLINE"))
      return DXF_POLYLINE;
   else if (!strcmp (buffer, "LINE"))
      return DXF_LINE;
   else
      return DXF_UNKNOWN;
}
 
int WriteDXFTrailer (FILE *output)
{
   fprintf (output, "  %d\nENDSEC\n",STRING);
   fprintf (output, "  %d\nEOF\n",STRING);

   return 0;
}

int ReadDXFTrailer (FILE *input)
{
   int		dummy;
   char		buffer [256];

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, STRING))
      return 1;

   fscanf (input, "%s", buffer);
   if (strcmp (buffer, "EOF"))
      return 1;

   return 0;
}

int WriteDXFLine (DXFLine *line, const char *layer, FILE *output)
{
   fprintf (output,"  %d\nLINE\n",STRING); 
   fprintf (output,"  %d\n%s\n",LAYER,layer);
   fprintf (output,"  %d\n%f\n",XSTART,line -> xa);
   fprintf (output,"  %d\n%f\n",YSTART,line -> ya);
   fprintf (output,"  %d\n%f\n",ZSTART,line -> za);
   fprintf (output,"  %d\n%f\n",XEND,line -> xb);
   fprintf (output,"  %d\n%f\n",YEND,line -> yb);
   fprintf (output,"  %d\n%f\n",ZEND,line -> zb);

   return 0;
}

int ReadDXFLine (DXFLine *line, char *layer, FILE *input)
{
   int		dummy; 
   char		buffer [256];

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, LAYER))
      return 1;

   fscanf (input, "%s", buffer);
   if (layer != NULL)
      strcpy (layer, buffer);

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, XSTART))
      return 1;
   fscanf (input, "%lf", &(line -> xa));

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, YSTART))
      return 1;
   fscanf (input, "%lf", &(line -> ya));

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, ZSTART))
      return 1;
   fscanf (input, "%lf", &(line -> za));

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, XEND))
      return 1;
   fscanf (input, "%lf", &(line -> xb));

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, YEND))
      return 1;
   fscanf (input, "%lf", &(line -> yb));

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, ZEND))
      return 1;
   fscanf (input, "%lf", &(line -> zb));

   return 0;
}

int WriteDXFPolyline (DXFPolyline *poly, const char *layer, FILE *output)
{
   unsigned	i;

   fprintf (output,"   %d\nPOLYLINE\n",STRING);
   fprintf (output,"   %d\n%s\n",LAYER,layer);
   fprintf (output,"   %d\n%d\n",POLYCODE1,1);
   fprintf (output,"   %d\n%f\n",XSTART,0.0); 
   fprintf (output,"   %d\n%f\n",YSTART,0.0); 
   fprintf (output,"   %d\n%f\n",ZSTART,0.0); 
   fprintf (output,"   %d\n%d\n",POLYCODE2,1);
   for (i = 0 ; i < poly -> n ; i++) {
      fprintf (output,"   %d\nVERTEX\n",STRING);
      fprintf (output,"   %d\n%s\n",LAYER,layer);
      fprintf (output,"   %d\n%f\n",XSTART,poly -> x[i]);
      fprintf (output,"   %d\n%f\n",YSTART,poly -> y[i]);
      fprintf (output,"   %d\n%f\n",ZSTART,poly -> z[i]);
   }
   fprintf (output, "   %d\nSEQEND\n",STRING);
   fprintf (output, "   %d\n%s\n",LAYER,layer);

   return 0;
}

int ReadDXFPolyline (DXFPolyline *poly, char *layer, FILE *input)
{
   int		dummy; 
   double	fdummy;
   char		buffer [256];
   unsigned	count,flag;

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, LAYER))
      return 1;
   fscanf (input, "%s", buffer);
   if (layer != NULL)
      strcpy (layer, buffer);

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, POLYCODE1))
      return 1;
   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, 1)) 	/* I don't know what these mean */
      return 1;

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, XSTART))
      return 1;
   fscanf (input, "%lf", &fdummy);

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, YSTART))
      return 1;
   fscanf (input, "%lf", &fdummy);

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, ZSTART))
      return 1;
   fscanf (input, "%lf", &fdummy);

   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, POLYCODE2))
      return 1;
   fscanf (input, "%d", &dummy);
   if (CheckValidity (dummy, 1))
      return 1;

   count = 0;
   flag = 1;
   while (flag) {
      fscanf (input, "%d", &dummy);
      if (CheckValidity (dummy, STRING))
         return 1;
      fscanf (input, "%s", buffer);
      if (strcmp (buffer, "VERTEX") == 0) {
         fscanf (input, "%d", &dummy);
         if (CheckValidity (dummy, LAYER))
            return 1;
         fscanf (input, "%s", buffer);
         if (layer != NULL)
            strcpy (layer, buffer);

         fscanf (input, "%d", &dummy);
         if (CheckValidity (dummy, XSTART))
            return 1;
         fscanf (input, "%lf", &(poly -> x[count]));

         fscanf (input, "%d", &dummy);
         if (CheckValidity (dummy, YSTART))
            return 1;
         fscanf (input, "%lf", &(poly -> y[count]));

         fscanf (input, "%d", &dummy);
         if (CheckValidity (dummy, ZSTART))
            return 1;
         fscanf (input, "%lf", &(poly -> z[count]));
         count++;
         if (count >= POLYMAX) {
            error ("dxf: max poly points exceeded.");
            return 1;
         } 
      }
      else if (strcmp (buffer, "SEQEND") == 0) {
         fscanf (input,"%d", &dummy);
         if (CheckValidity (dummy, LAYER))
            return 1;
         fscanf (input, "%s", buffer);
         if (layer != NULL)
            strcpy (layer, buffer);
         flag = 0;
      }
   }
   poly -> n = count;
   return 0;
}

int WriteDXFPolyHeader (const char *layer, FILE *output)
{
   fprintf (output,"   %d\nPOLYLINE\n",STRING);
   fprintf (output,"   %d\n%s\n",LAYER,layer);
   fprintf (output,"   %d\n%d\n",POLYCODE1,1);
   fprintf (output,"   %d\n%f\n",XSTART,0.0); 
   fprintf (output,"   %d\n%f\n",YSTART,0.0); 
   fprintf (output,"   %d\n%f\n",ZSTART,0.0); 
   fprintf (output,"   %d\n%d\n",POLYCODE2,1);

   return 0;
}

int WriteDXFPolyTrailer (const char *layer, FILE *output)
{
   fprintf (output, "   %d\nSEQEND\n",STRING);
   fprintf (output, "   %d\n%s\n",LAYER,layer);

   return 0;
}

int WriteDXFVertex (double x, double y, double z, const char *layer, FILE *output)
{
   fprintf (output,"   %d\nVERTEX\n",STRING);
   fprintf (output,"   %d\n%s\n",LAYER,layer);
   fprintf (output,"   %d\n%f\n",XSTART,x);
   fprintf (output,"   %d\n%f\n",YSTART,y);
   fprintf (output,"   %d\n%f\n",ZSTART,z);

   return 0;
}

int
WriteDXF3dMeshVertex(double x, double y, double z, const char *layer, FILE *output)
{
   WriteDXFVertex (x, y, z, layer, output);
   fprintf (output,"   %d\n%d\n",POLYCODE2,MESHCODE2);

   return 0;
}

int WriteDXF3dMeshHeader (int m, int n, const char *layer, FILE *output)
{
   fprintf (output,"   %d\nPOLYLINE\n",STRING);
   fprintf (output,"   %d\n%s\n",LAYER,layer);
   fprintf (output,"   %d\n%d\n",POLYCODE1,1);
   fprintf (output,"   %d\n%f\n",XSTART,0.0); 
   fprintf (output,"   %d\n%f\n",YSTART,0.0); 
   fprintf (output,"   %d\n%f\n",ZSTART,0.0); 
   fprintf (output,"   %d\n%d\n",POLYCODE2,MESHCODE1);
   fprintf (output,"   %d\n%d\n",MMESHLINES,m);
   fprintf (output,"   %d\n%d\n",NMESHLINES,n);

   return 0;
}

unsigned CheckValidity (unsigned int read, unsigned int wanted)
{
   if (read != wanted) {
      error ("dxf: expected DXF code %d, got %d",wanted,read);
      return 1;
   }
   else return 0;
}
