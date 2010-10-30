#ifndef DRAW_H
#define DRAW_H

#include <stdio.h>
#include "fe.h"

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

void DrawStructureASCII (FILE *fp, unsigned cols, unsigned rows);

void WriteWireframe2D (FILE *fp, Node **table, unsigned n, double mag);

void WriteWireframe3D (FILE *fp, Node **table, unsigned n, double mag, 
                       double xrot, double yrot, double zrot, double zsc);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
