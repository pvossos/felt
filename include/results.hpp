#ifndef RESULTS_H
#define RESULTS_H

#include "problem.h"
#include "cvector1.hpp"
#include "fe.hpp"

/*----------------------------------------------------------------------*/

int WriteAllMatlab(char *filename, Problem *data);

int WriteGraphicsFile (char *filename, double mag);

int WriteMaterialStatistics (FILE *output);

void WriteLoadCaseTable (Matrix dtable, FILE *fp);

void WriteLoadRangeTable (Matrix dtable, FILE *fp);

void WriteModalResults (FILE *fp, Matrix M, Matrix C, Matrix K, Matrix lambda);

void WriteTransientTable (Matrix dtable, Matrix ttable, FILE *fp);

void WriteTransferFunctions (Matrix *H, NodeDOF *forced, unsigned numforced, FILE *fp);

void WriteStructuralResults (FILE *output, char *title, const cvector1<Reaction> &R);

void WriteTemperatureResults (FILE *fp, char *title);

void WriteEigenResults (Matrix lambda, Matrix x, char *title, FILE *output);

void WriteOutputSpectra (Matrix P, FILE *fp);

void PlotTransientTable (Matrix dtable, Matrix ttable, double dt, FILE *fp);

void PlotLoadRangeTable (Matrix dtable, FILE *fp);

void PlotLoadCaseTable (Matrix dtable, FILE *fp);

void PlotOutputSpectra (Matrix P, FILE *fp);

void PlotModeShapes (Matrix x, FILE *output);

void PlotTransferFunctions (Matrix *H, NodeDOF *forced, unsigned numforced, FILE *fp);

void PrintGlobalMatrices (FILE *fp, Matrix M, Matrix C, Matrix K);

int MatlabGlobalMatrices (char *filename, Matrix M, Matrix C, Matrix K);

/*----------------------------------------------------------------------*/

#endif
