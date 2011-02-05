#ifndef OPENGL_H
#define OPENGL_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

void CreateOpenGLShell(String name, String title, Boolean stress,
                       int comp, const Element *element, unsigned numelts, Boolean contour);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

#endif
