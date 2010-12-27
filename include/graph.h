#ifndef GRAPH_H
#define GRAPH_H

#include "fe.h"
#include "matrix.h"

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

void WriteLineGraph (Matrix d, const char *alt_title,
                     const char *xlabel, const char *ylabel, const char *output);

void WriteLineGraphTransferFunctions (Matrix *H, NodeDOF *forced,
                                      unsigned numforced, const char *output);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
