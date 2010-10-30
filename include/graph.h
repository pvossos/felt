#ifndef GRAPH_H
#define GRAPH_H

#include "fe.h"
#include "matrix.h"

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

void WriteLineGraph (Matrix d, char *alt_title,
                     char *xlabel, char *ylabel, char *output);

void WriteLineGraphTransferFunctions (Matrix *H, NodeDOF *forced,
                                      unsigned numforced, char *output);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
