/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/************************************************************************
 * File:	corduroy.c						*
 *									*
 * Description:	This file contains the code the driver for the corduroy	*
 *		application.						*
 ************************************************************************/

# include <stdio.h>
# include <ctype.h>
# include <string.h>
# include "allocate.h"
# include "meshgen.hpp"
# include "generator.h"
# include "error.h"
# include "definition.h"


# define streq(a,b)	!strcmp(a,b)

static char *usage = "\
usage: corduroy [options] [filename]\n\
       -debug           write debugging output\n\
       -nocpp           do not use a preprocessor\n\
       -cpp filename    preprocessor to use\n\
       -Dname[=value]   define a macro\n\
       -Uname           undefine a macro\n\
       -Idirectory      specify include directory\n\
";

/************************************************************************
 * Function:	Quote							*
 *									*
 * Description:	Quotes a string if necessary.				*
 ************************************************************************/

static char *Quote (char *s)
{
    char	c;
    char       *ptr;
    static char buffer [256];


    for (ptr = s; (c = *ptr); ptr ++)
	if (!isalpha (c) && c != '_' && (isdigit (c) ? ptr == s : 1)) {
	    sprintf (buffer, "\"%s\"", s);
	    return buffer;
	}

    return s;
}


/************************************************************************
 ************************************************************************/

static void AddNewArrays (cvector1<Node> &node, cvector1<Element> &element,
                          cvector1<Node> &all_nodes, cvector1<Element> &all_elements)
{
   unsigned	i;
   
   const size_t numnodes = node.size();
   const size_t nn = all_nodes.size();
   all_nodes.resize(nn + numnodes);
   
   const size_t numelts = element.size();
   const size_t ne = all_elements.size();
   all_elements.resize(ne + numelts);

   for (i = 1 ; i <= numnodes ; i++) 
      all_nodes[i + nn] = node [i];

   for (i = 1 ; i <= numelts ; i++) 
       all_elements[i + ne] = element [i];
}



/************************************************************************
 * Function:	main							*
 *									*
 * Description:	Main the the driver function for the corduroy		*
 *		application.						*
 ************************************************************************/

int main (int argc, char **argv)
{
   Definition	definition;
   Element	*element;
   Node		*node;
   unsigned	start_node;
   unsigned	start_element;
   Line		*line;
   Grid		*grid;
   Grid		*quadgrid;
   Grid		*brickgrid;
   TriMesh	*trimesh;
   unsigned	i,j;
   int		 debug = 0;
   unsigned	 code;


	/*
	 * check usage and read the input file
	 */

   if (CorduroyCppOptions (&argc, argv)) {
	fputs (usage, stderr);
	exit (1);
   }

   j = 1;
   for (i = 1; i < argc; i ++) {
	if (streq (argv [i], "-help")) {
	    fputs (usage, stderr);
	    exit (0);
	} else if (streq (argv [i], "-debug"))
	    debug = 1;
	else
	    argv [j ++] = argv [i];
   }

   argv [argc = j] = NULL;

   if (argc > 2) {
	fputs (usage, stderr);
	exit (1);
   }

   add_all_definitions ( );
   if (ReadCorduroyFile (argc == 2 ? argv [1] : "-"))
	exit (1);
   
   if (debug) {
	WriteCorduroyFile ("-");
        exit (1);
   }

	/*
	 * initialize some arrays and counters
	 */

   line = generator.lines;
   grid = generator.grids;
   quadgrid = generator.quadgrids;
   brickgrid = generator.brickgrids;
   trimesh = generator.trimeshes;

   start_node = generator.start_node - 1;
   start_element = generator.start_element - 1;
   
   cvector1<Node> all_nodes;
   cvector1<Element> all_elements;

	/*
	 * generate along lines
	 */

   for (i = 0 ; i < generator.num_lines ; i++) {
      cvector1<Node> node;
      cvector1<Element> element;

      GenerateLine (line[i], element, node,start_node, start_element);

      start_node += node.size();
      start_element += element.size();

      AddNewArrays (node, element, all_nodes, all_elements);
   }

	/*
	 * generate grids of line elements
	 */

   for (i = 0 ; i < generator.num_grids ; i++) {
      cvector1<Node> node;
      cvector1<Element> element;

      GenerateGrid (grid[i], element, node, start_node, start_element);

      AddNewArrays (node, element, all_nodes, all_elements);

      start_node += node.size();
      start_element += element.size();
   }

	/*
	 * generate grids of quadrilateral elements
	 */

   for (i = 0 ; i < generator.num_quadgrids ; i++) {
      cvector1<Node> node;
      cvector1<Element> element;
      
      GenerateQuadGrid (quadgrid[i], element, node, start_node, start_element);

      AddNewArrays (node, element, all_nodes, all_elements);

      start_node += node.size();
      start_element += element.size();
   }

	/*
	 * generate grids of brick elements
	 */

   for (i = 0 ; i < generator.num_brickgrids ; i++) {
      cvector1<Node> node;
      cvector1<Element> element;

      GenerateBrickGrid (brickgrid[i], element, node, start_node, start_element);

      AddNewArrays (node, element, all_nodes, all_elements);

      start_node += node.size();
      start_element += element.size();
   }

	/*
	 * generate meshes of triangular meshes
	 */

   for (i = 0 ; i < generator.num_trimeshes ; i++) {
      cvector1<Node> node;
      cvector1<Element> element;
      code = GenerateTriMesh (trimesh[i], element, node, start_node, start_element);
      if (code)
         continue;

      AddNewArrays (node, element, all_nodes, all_elements);

      start_node += node.size();
      start_element += element.size();
   }

	/*
	 * coalesce the nodes
	 */

   if (all_nodes.empty() || all_elements.empty())
	exit (1);

   all_nodes = CoalesceNodes (all_nodes, all_elements);

   const size_t nn = all_nodes.size();
   const size_t ne = all_elements.size();

	/*	
	 * write everything out
	 */

   printf ("problem description\n");
   printf ("nodes=%d elements=%d\n\n", nn, ne);

   printf ("nodes\n");
   if (nn)
	if (generator.constraint)
	    printf ("%d  x=%g   y=%g   z=%g constraint=%s\n",
	      all_nodes [1] -> number, all_nodes [1] -> x, all_nodes [1] -> y,
	      all_nodes [1] -> z, Quote (generator.constraint));
	else
	    printf ("%d  x=%g   y=%g   z=%g\n", all_nodes [1] -> number, 
              all_nodes [1] -> x, all_nodes [1] -> y, all_nodes [1] -> z);

   for (i = 2 ; i <= nn ; i++) 
      printf ("%d  x=%g   y=%g   z=%g\n", all_nodes [i] -> number, 
              all_nodes [i] -> x, all_nodes [i] -> y, all_nodes [i] -> z);

   definition = all_elements [1] -> definition;
   printf ("\n%s elements\n", definition -> name);

   for (i = 1 ; i <= ne ; i++) {
      if (all_elements [i] -> definition != definition) {
         definition = all_elements [i] -> definition;
         printf ("\n%s elements\n", definition -> name);
      }

      printf ("%d   nodes=[%d,", all_elements [i] -> number, 
              all_elements [i] -> node[1] -> number);

      for (j = 2 ; j < definition -> numnodes ; j++) 
         printf ("%d,", all_elements [i] -> node[j] -> number);

      printf ("%d]",all_elements[i] -> node[definition->numnodes] -> number);
      if (i == 1 && generator.material)
	printf (" material=%s", Quote (generator.material));
      printf ("\n");
   }
      
   printf ("\nend\n");
   return 0;
}
