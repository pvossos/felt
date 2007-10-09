#ifndef BIVAR_H
#define BIVAR_H

int BivariateInterp (int ndp, float *xd, float *yd, float *zd,
                     int nxi, int nyi, float *xi, float *yi, float **zi,
                     unsigned char **mask);

#endif
