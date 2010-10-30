#ifndef WIREFRAME_H
#define WIREFRAME_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

void WriteWireframeFile (char *filename, double mag, 
                         double xrot, double yrot, double zrot, double zsc);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
