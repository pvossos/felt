#ifndef BMP_H
#define BMP_H

void ImageDataToBMP(char *out, unsigned char **image, int rows, int cols,
                    unsigned char *red, unsigned char *green, unsigned char *blue);

#endif
