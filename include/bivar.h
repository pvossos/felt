#ifndef BIVAR_H
#define BIVAR_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

int BivariateInterp (int ndp, float *xd, float *yd, float *zd,
                     int nxi, int nyi, float *xi, float *yi, float **zi,
                     unsigned char **mask);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
