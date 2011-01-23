#ifndef MESHGEN_HPP
#define MESHGEN_HPP

#include "mesh.h"
#include "cvector1.hpp"

/*!
A simple procedure to generate a 1-d line of line elements with all
the elements along a single line.
*/
unsigned 
GenerateLine (Line line, cvector1<Element> &element, cvector1<Node> &node,
              unsigned int bnode, unsigned int belement);

/*!
  A simple procedure to generate a 3-d grid of line elements with all
  the elements running parallel to one of the axes.
*/
unsigned
GenerateGrid(Grid grid, cvector1<Element> &element, cvector1<Node> &node,
             unsigned int bnode, unsigned int belement);

/*!
  A simple procedure to generate a 2-d grid of quadrilateral (four
  shape nodes) elements with all the elements running parallel to one
  of the axes.
*/
unsigned
GenerateQuadGrid(Grid grid, cvector1<Element> &element, cvector1<Node> &node,
                 unsigned int bnode, unsigned int belement);

/*!
  A simple procedure to generate a 3-d grid of solid brick (eight
  shape nodes) elements with all the elements running parallel to one
  of the axes.
 */
unsigned
GenerateBrickGrid(Grid grid, cvector1<Element> &element, cvector1<Node> &node, 
                  unsigned int bnode, unsigned int belement);

/*!
 A procedure to interface to Triangle and generate a mesh of
 triangular elements. 
 */
unsigned
GenerateTriMesh(TriMesh trimesh, cvector1<Element> &element, cvector1<Node> &node, 
                unsigned int bnode, unsigned int belement);

cvector1<Node>
CoalesceNodes(cvector1<Node> &node, cvector1<Element> &element);

bool
CoalesceProblemNodes();

#endif
