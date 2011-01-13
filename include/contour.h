#ifndef CONTOUR_H
#define CONTOUR_H

#include "fe.h"

void PlotStressField (char *out, Element *element, unsigned numelts, int comp,
                      int equalize, int plot_elt, int width, int height);

void PlotDisplacementField (char *out, Node *node, unsigned numnodes,
                            Element *element, unsigned numelts, int comp, 
                            int equalize, int plot_elt, int width, int height);

#endif
