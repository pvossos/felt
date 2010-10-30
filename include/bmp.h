#ifndef BMP_H
#define BMP_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

void ImageDataToBMP(char *out, unsigned char **image, int rows, int cols,
                    unsigned char *red, unsigned char *green, unsigned char *blue);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
