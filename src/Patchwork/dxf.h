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

/*****************************************************************************
 *
 * File:	dxf.h
 * 
 * Description:	contains header information for the DXF routines 
 *
 ****************************************************************************/

#ifndef DXF_H
#define DXF_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

# define STRING		0
# define HEADER		2
# define LAYER		8
# define POLYCODE1	66
# define POLYCODE2	70
# define MESHCODE1	16
# define MESHCODE2	64
# define MMESHLINES	71
# define NMESHLINES	72
# define XSTART 	10
# define YSTART		20
# define ZSTART		30
# define XEND		11
# define YEND		21
# define ZEND		31

# define POLYMAX	20

	/*	
 	 * Entity return codes (arbitrary)
	 */

# define DXF_ENDSEC	101
# define DXF_LINE	102
# define DXF_POLYLINE	103
# define DXF_UNKNOWN	110
# define DXF_ERROR	120

typedef struct {
   double	xa,ya,za,
		xb,yb,zb;
} DXFLine;

typedef struct {
   int		n;
   double	x [POLYMAX],
		y [POLYMAX],
		z [POLYMAX];
} DXFPolyline;

int WriteDXFLine (DXFLine *line, const char *layer, FILE *output);
int ReadDXFLine (DXFLine *line, char *layer, FILE *input);
int WriteDXFHeader (FILE *output);
int ReadDXFHeader (FILE *input);
int ReadDXFTrailer (FILE *input);
int ReadDXFEntity (FILE *input);
int WriteDXFTrailer (FILE *output);
int WriteDXFPolyline (DXFPolyline *poly, const char *layer, FILE *output);
int ReadDXFPolyline (DXFPolyline *poly, char *layer, FILE *input);
int WriteDXFPolyTrailer (const char *layer, FILE *output);
int WriteDXFPolyHeader (const char *layer, FILE *output);
int WriteDXFVertex (double x, double y, double z, const char *layer, FILE *output);
int WriteDXF3dMeshHeader (int m, int n, const char *layer, FILE *output);
unsigned CheckValidity (unsigned int read, unsigned int wanted);

int WriteDXF3dMeshVertex(double x, double y, double z, const char *layer, FILE *output);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
