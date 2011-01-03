#ifndef DRAW_HPP
#define DRAW_HPP

#include <vector>
#include <stdio.h>
#include "cvector1.hpp"
#include "fe.h"

/*----------------------------------------------------------------------*/

void DrawStructureASCII (FILE *fp, unsigned cols, unsigned rows);

void WriteWireframe2D (FILE *fp, const std::vector< cvector1<Node> > &table, double mag);

void WriteWireframe3D (FILE *fp, const std::vector< cvector1<Node> > &table, double mag, 
                       double xrot, double yrot, double zrot, double zsc);

/*----------------------------------------------------------------------*/

#endif
