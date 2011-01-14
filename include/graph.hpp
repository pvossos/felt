#ifndef GRAPH_H
#define GRAPH_H

#include "fe.h"
#include "matrix.h"

//----------------------------------------------------------------------!

void WriteLineGraph (Matrix d, const char *alt_title,
                     const char *xlabel, const char *ylabel, const char *output);

void WriteLineGraphTransferFunctions (const Matrix *H, const NodeDOF *forced,
                                      unsigned numforced, const char *output);

//----------------------------------------------------------------------!

#endif
