#ifndef GRAPH_H
#define GRAPH_H

#include "matrix.h"

void WriteLineGraph (Matrix d, char *alt_title,
                     char *xlabel, char *ylabel, char *output);

void WriteLineGraphTransferFunctions (Matrix *H, unsigned *forced,
                                      unsigned numforced, char *output);

#endif
