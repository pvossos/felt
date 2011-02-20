#ifndef CONTOUR_H
#define CONTOUR_H

#include "fe.h"

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

void PlotStressField (char *out, const Element *element, unsigned numelts, int comp,
                      int equalize, int plot_elt, int width, int height);

void PlotDisplacementField (char *out, const Node *node, unsigned numnodes,
                            const Element *element, unsigned numelts, int comp, 
                            int equalize, int plot_elt, int width, int height);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
