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

/***************************************************************************
 *
 * File:	results.c
 *
 * Description:	Contains code to print various kinds of results for the
 *		various kinds of analyses.
 *
 ***************************************************************************/

# include <stdio.h>
# include <math.h>
# include "results.h"
# include "allocate.h"
# include "problem.h"
# include "fe.h"
# include "error.h"


double ElementArea (Element e, unsigned int n);

/**************************************************************************
 *
 * Function:	WriteStructuralResults
 *
 * Parameters:	title		user supplied title of problem
 *		element		array of elements
 *		node		array of nodes
 *		R		array of reaction forces
 *		numreactions	number of reactions
 *
 * Return value:none
 *
 ***************************************************************************/

void
WriteStructuralResults(FILE *output, char *title,
                        Reaction *R, unsigned numreactions)
{
    FILE       *fd;
    Element    *element;
    Node       *node;
    unsigned	numelts,
		numnodes;
    unsigned	i,j,k;
    unsigned	count;
    static const char	*dof_names [ ] = {"","Tx","Ty","Tz","Rx","Ry","Rz"};

    element = problem.elements;
    node    = problem.nodes;
    numnodes = problem.num_nodes;
    numelts  = problem.num_elements;

    fd = GetDetailStream( );
    if (fd)
       SetDetailStream (output);
 
    fprintf (output,"\n** %s **\n\n",title);
    fprintf (output,"Nodal Displacements\n");
    fprintf (output,"-----------------------------------------------------------------------------\n");
    fprintf (output,"Node #      DOF 1       DOF 2       DOF 3       DOF 4       DOF 5       DOF 6\n");
    fprintf (output,"-----------------------------------------------------------------------------\n");
    for (i = 1; i <= numnodes; i ++) {
	fprintf (output,"%3d   %11.5g %11.5g %11.5g %11.5g %11.5g %11.5g\n", 
                 node [i] -> number, 
                 node [i] -> dx[1], node [i] -> dx[2], 
                 node [i] -> dx[3], node [i] -> dx[4], 
                 node [i] -> dx[5], node [i] -> dx[6]);
    }

    fprintf (output,"\nElement Stresses\n");
    fprintf (output,"-------------------------------------------------------------------------------\n");
    for (i = 1; i <= numelts ; i++) {
        fprintf (output,"%3d: ", element[i] -> number);
        if (element [i] -> ninteg == 0 || element[i] -> stress == NULL)
           fprintf (output,"  No stresses available for this element\n");
        else {
           count = 0;
           for (j = 1 ; j <= element[i] -> ninteg ; j++) {
              detail ("(%g %g %g)  ", 
		       element [i] -> stress [j] -> x,
		       element [i] -> stress [j] -> y,
		       element [i] -> stress [j] -> z);
              for (k = 1 ; k <= element[i] -> definition -> numstresses ; k++) {
                 fprintf (output," % 11.5g", element[i]->stress[j]->values[k]);
                 count++;
                 if (count == 6) {
                    fprintf (output,"\n");
                    count = 0;
                    if (j < element[i] -> ninteg || 
                        k < element[i] -> definition -> numstresses) {
                       fprintf (output,"     ");
                    } 
                 }
              }
           } 
           if (count)
              fprintf (output,"\n");
        }
    }    

    if (numreactions) {
       fprintf (output,"\n\nReaction Forces\n");
       fprintf (output,"-----------------------------------\n");
       fprintf (output,"Node #     DOF     Reaction Force\n");
       fprintf (output,"-----------------------------------\n");

       for (i = 1 ; i <= numreactions ; i++)
          fprintf (output,"%3d        %s        % 11.5g\n",R[i] -> node, 
                   dof_names [R[i] -> dof], R[i] -> force);
    }

    if (fd)
       SetDetailStream(fd);

    return;
}

/****************************************************************************
 *
 * Function:	WriteTemperatureResults
 *
 ***************************************************************************/

void
WriteTemperatureResults(FILE *fp, char *title)
{
   Node		*node;
   unsigned	numnodes;
   unsigned	i;

   node    = problem.nodes;
   numnodes = problem.num_nodes;

   fprintf (fp,"** %s **\n\n",title);
   fprintf (fp,"Steady State Nodal Temperatures\n");
   fprintf (fp,"-------------------------------------\n");
   fprintf (fp,"Node #      Temperature\n");
   fprintf (fp,"-------------------------------------\n");
   for (i = 1; i <= numnodes; i ++) 
      fprintf (fp,"%3d   %11.5g\n", node [i] -> number, node [i] -> dx[1]);

   fprintf (fp, "\n");

   return;
}

/***************************************************************************
 *
 * Function:	WriteEigenResults
 *
 * Description:	Output a prettified list of mode frequencies and a clear 
 *		table giving the mode shapes.
 *
 ***************************************************************************/

void
WriteEigenResults(Matrix lambda, Matrix x, char *title, FILE *output)
{
   unsigned	n;
   unsigned	i,j;
   unsigned	start;

   n = Mrows(lambda); 
   
   fprintf (output,"** %s **\n\n",title);
   fprintf (output,"Modal frequencies (rad/sec)\n");
   fprintf (output,"------------------------\n");
   fprintf (output,"Mode #      Frequency\n");
   fprintf (output,"------------------------\n");
   for (i = 1; i <= n; i ++) 
      fprintf (output,"%3d   %11.5g  (%11.5g Hz)\n", i, mdata(lambda,i,1), mdata(lambda,i,1)/2.0/M_PI);
               

   fprintf (output,"\nMode shapes\n");
   fprintf (output,"------------------------------------------------------------------------------\n");

   for (start = 1 ; start <= n ; start += 6) {

      fprintf (output,"   ");
      for (i = start ; i <= start+5 && i <= n ; i++) 
         fprintf (output,"Mode %3d    ", i);

      fprintf (output,"\n------------------------------------------------------------------------------\n");

      for (j = 1 ; j <= n ; j++) {
         for (i = start ; i <= start+5 && i <= n ; i++) 
            fprintf (output,"%11.5g ", mdata(x,j,i));

         fprintf (output,"\n");
      }

      fprintf (output,"\n");
   } 

   return;
}

/***************************************************************************
 *
 * Function:	PlotModeShapes
 *
 * Description:	Someday this should give some sort of graphical presentation
 *		of the mode shapes.
 *
 ***************************************************************************/

void
PlotModeShapes(Matrix x, FILE *output)
{
   error ("mode shape plots are not implemented yet - sorry.");
   return;
}

static const char symbols[] = {0,'x','o','=','*','?','#','v','$','&','@'};
static const char *labels [] = {"","Tx","Ty","Tz","Rx","Ry","Rz"};

/******************************************************************************
 *
 * Function:	WriteTransientTable
 *
 * Parameters:	dtable		matrix of nodal displacements with time
 *		fp		file pointer to output table to
 *
 * Description:	Pretty simple really. The only slightly tricky part is that
 *		we don't want to print a whole bunch of DOFs all in one
 *		table ... so we'll break it up into 4 DOFs per table and
 *		then print as many of them as we need
 *	
 ******************************************************************************/

void
WriteTransientTable(Matrix dtable, Matrix ttable, FILE *fp)
{
   unsigned	i,j,k,m,n;
   unsigned	table;
   unsigned	node, dof;
   unsigned	start_dof;
   unsigned	number;
   unsigned	ntables;

   ntables = (unsigned) (analysis.numnodes * analysis.numdofs / 4);
   if ((analysis.numdofs * analysis.numnodes) % 4 != 0)
      ntables++;

	/*
	 * for each table, print the appropriate headers and then
	 * the displacement data for those same DOFs
	 */

   node = 1;
   dof = 1;
   n = 0; /* gcc -Wall */
   for (table = 1 ; table <= ntables ; table++) {
      start_dof = dof;
      fprintf (fp,"\n------------------------------------------------------------------\n");
      fprintf (fp,"       time");
      number = 1;
      for (m = node ; m <= analysis.numnodes && number <= 4 ; m++) {
         for (n = dof ; n <= analysis.numdofs && number <= 4 ; n++) {
            fprintf (fp,"        %s(%d)", labels[(int) analysis.dofs[n]],
                     analysis.nodes[m] -> number);
            
            number++; 
         }
         dof = 1;
      }
      fprintf (fp,"\n------------------------------------------------------------------\n");

	/*
	 * output the displacement at this time step for each DOF 
	 *in this table
	 */

      for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
         if (ttable == NullMatrix)
            fprintf (fp,"%11.5g",(i-1)*analysis.step);
         else
            fprintf (fp,"%11.5g",mdata(ttable,i,1));

         number = 1;
         dof = start_dof;
         for (j = node ; j <= analysis.numnodes && number <= 4  ; j++) {
            for (k = dof ; k <= analysis.numdofs && number <= 4 ; k++) {
               fprintf (fp, "  %11.5g",
                        MatrixData (dtable)[i][(j-1)*analysis.numdofs + k]);
               number++;
            }
            dof = 1;
         }
         fprintf (fp,"\n");
      }
    
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}

/******************************************************************************
 *
 * Function:	PlotTransientTable
 *
 * Parameters:	dtable		matrix of nodal displacements with time
 *		ttable		optional table of time points (may be variable)
 *		dt		time step of simulation
 *		fp		file pointer to output table to
 *
 * Description:	Again this should be pretty simple but we can only plot 10 
 *		DOFs per graph (even this is pushing cluttered).  So we plot
 *		multiple graphs if we have to.
 *	
 ******************************************************************************/

void
PlotTransientTable(Matrix dtable, Matrix ttable, double dt, FILE *fp)
{
   unsigned	i,j,k;
   unsigned	m,n;
   double	data;
   double	max;
   double	min;
   double	dscale;
   char		buffer [80];
   char		buffer1 [80];
   char		buffer2 [80];
   unsigned	graph, ngraphs;
   unsigned	start_dof, number;
   unsigned	dof, node;
   int		position;

   ngraphs = (unsigned) (analysis.numnodes * analysis.numdofs / 10);
   if ((analysis.numdofs * analysis.numnodes) % 10 != 0)
      ngraphs++;

   node = 1;
   dof = 1;
   m = n = start_dof = 0;
   for (graph = 1 ; graph <= ngraphs ; graph++) {

	/*
	 * find the max and the min for this graph, then compute the scale
	 */

      min = max = MatrixData (dtable) [1][(node-1)*analysis.numdofs + dof]; 
      for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
         start_dof = dof;
         number = 1;

         for (m = node ; m <= analysis.numnodes && number <= 10 ; m++) {
            for (n = dof ; n <= analysis.numdofs && number <= 10 ; n++) {
        
               data = MatrixData (dtable)[i][(m-1)*analysis.numdofs + n];

               if (data < min)
                  min = data;
               else if (data > max)
                  max = data;

               number++;
            }
            dof = 1;
         }
      }

      dscale = 67.0/(max - min);
   
      fprintf (fp,"\n");
      fprintf (fp,"           %-11.5g             %11.5g                      %11.5g\n",min,(min+max)/2.0,max);
      fprintf (fp,"           +------------------------------+-----------------------------------+\n");

	/*
	 * for each line (time step) form a line of the plot by placing
	 * each DOF in the appropriate place in the line buffer.  If that
	 * space is occupied we try the line above it and then the line
	 * below it.  If all available spaces are filled, we skip this point.
	 */

      for (i = 1; i <= MatrixRows (dtable) ; i++) {
         number = 1;
         dof = start_dof;
         sprintf (buffer1,"|                                                                   ");
         sprintf (buffer2,"|                                                                   ");
         sprintf (buffer,"|                                                                   ");

         for (j = node ; j <= analysis.numnodes && number <= 10  ; j++) {
            for (k = dof ; k <= analysis.numdofs && number <= 10 ; k++) {
               data = MatrixData (dtable)[i][(j-1)*analysis.numdofs + k];
               position = (int) ((data - min)*dscale);
               if (buffer [position] == ' ' || buffer [position] == '|')
                  buffer [position] = (char) symbols [number];
               else {
                  if (buffer1 [position] == ' ' || buffer1 [position] == '|')
                     buffer1 [position] = (char) symbols [number];
                  else if (buffer2[position] == ' ' || buffer2[position] == '|')
                     buffer2 [position] = (char) symbols [number];
               }

               number++;
            }
            dof = 1;
         }
         fprintf (fp,"           %s\n",buffer1);
         if (ttable == NullMatrix)
            fprintf (fp,"%10.5g %67s\n",(i-1)*dt,buffer);
         else
            fprintf (fp,"%10.5g %67s\n",mdata(ttable,i,1),buffer);
         fprintf (fp,"           %s\n",buffer2);          
      }
      fprintf (fp, "\n");

	/*
	 * print the legend for this graph
	 */

      number = 1;
      dof = start_dof;
      for (i = node ; i <= analysis.numnodes && number <= 10 ; i++) {
         for (j = dof ; j <= analysis.numdofs && number <= 10 ; j++) {
            fprintf (fp,"         %c %c %c    %s(%d)\n", symbols [number],
                     symbols [number], symbols [number],
                     labels[(int) analysis.dofs[j]], analysis.nodes[i] -> number);

            number++;
         }
         dof = 1;
      }
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}

static const char *spectra_labels [] = {"","S_Tx","S_Ty","S_Tz","S_Rx","S_Ry","S_Rz"};

/*****************************************************************************
 *
 * Function:	WriteOutputSpectra
 *
 * Description:	Basically the same as WriteTransientTable but we print out
 *		tables of frequency vs. power spectra at each DOF.  Again
 *		we limit it to 4 DOF per table.	
 *	
 *****************************************************************************/

void
WriteOutputSpectra(Matrix P, FILE *fp)
{
   unsigned	i,j,k,m,n;
   unsigned	table;
   unsigned	node, dof;
   unsigned	start_dof;
   unsigned	number;
   unsigned	ntables;
   double	freq;

   ntables = (unsigned) (analysis.numnodes * analysis.numdofs / 4);
   if ((analysis.numdofs * analysis.numnodes) % 4 != 0)
      ntables++;

	/*
	 * for each table, print the appropriate headers and then
	 * the power spectrum data for those same DOFs
	 */

   node = 1;
   dof = 1;
   n = 0;
   for (table = 1 ; table <= ntables ; table++) {
      start_dof = dof;
      fprintf (fp,"\n------------------------------------------------------------------\n");
      fprintf (fp,"       freq");
      number = 1;
      for (m = node ; m <= analysis.numnodes && number <= 4 ; m++) {
         for (n = dof ; n <= analysis.numdofs && number <= 4 ; n++) {
            fprintf (fp,"       %s(%d)", spectra_labels[(int) analysis.dofs[n]],
                     analysis.nodes[m] -> number);
            
            number++; 
         }
         dof = 1;
      }
      fprintf (fp,"\n------------------------------------------------------------------\n");

	/*
	 * output the power at this frequency for each DOF 
	 */

      freq = analysis.start;
      for (i = 1 ; i <= Mrows (P) ; i++) {
         fprintf (fp,"%11.5g", freq);
         number = 1;
         dof = start_dof;
         for (j = node ; j <= analysis.numnodes && number <= 4  ; j++) {
            for (k = dof ; k <= analysis.numdofs && number <= 4 ; k++) {
               fprintf (fp, "  %11.5g",
                        mdata(P,i,(j-1)*analysis.numdofs + k));
               number++;
            }
            dof = 1;
         }
         fprintf (fp,"\n");

         freq += analysis.step;
      }
    
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}

/******************************************************************************
 *
 * Function:	PlotOutputSpectra
 *
 * Description:	an awful lot like PlotTransientTable
 *	
 ******************************************************************************/

void
PlotOutputSpectra(Matrix P, FILE *fp)
{
   unsigned	i,j,k;
   unsigned	m,n;
   double	data;
   double	max;
   double	min;
   double	dscale;
   char		buffer [80];
   char		buffer1 [80];
   char		buffer2 [80];
   unsigned	graph, ngraphs;
   unsigned	start_dof, number;
   unsigned	dof, node;
   int		position;
   double	freq;

   ngraphs = (unsigned) (analysis.numnodes * analysis.numdofs / 10);
   if ((analysis.numdofs * analysis.numnodes) % 10 != 0)
      ngraphs++;

   node = 1;
   dof = 1;
   m = n = start_dof = 0; /* gcc -Wall */
   for (graph = 1 ; graph <= ngraphs ; graph++) {

	/*
	 * find the max and the min for this graph, then compute the scale
	 */

      min = max = MatrixData (P) [1][(node-1)*analysis.numdofs + dof]; 
      for (i = 1 ; i <= MatrixRows (P) ; i++) {
         start_dof = dof;
         number = 1;

         for (m = node ; m <= analysis.numnodes && number <= 10 ; m++) {
            for (n = dof ; n <= analysis.numdofs && number <= 10 ; n++) {
        
               data = MatrixData (P)[i][(m-1)*analysis.numdofs + n];

               if (data < min)
                  min = data;
               else if (data > max)
                  max = data;

               number++;
            }
            dof = 1;
         }
      }

      dscale = 67.0/(max - min);
   
      fprintf (fp,"\n");
      fprintf (fp,"           %-11.5g             %11.5g                      %11.5g\n",min,(min+max)/2.0,max);
      fprintf (fp,"           +------------------------------+-----------------------------------+\n");

	/*
	 * for each line (frequency) form a line of the plot by placing
	 * each DOF in the appropriate place in the line buffer.  If that
	 * space is occupied we try the line above it and then the line
	 * below it.  If all available spaces are filled, we skip this point.
	 */

      freq = analysis.start;
      for (i = 1; i <= MatrixRows (P) ; i++) {
         number = 1;
         dof = start_dof;
         sprintf (buffer1,"|                                                                   ");
         sprintf (buffer2,"|                                                                   ");
         sprintf (buffer,"|                                                                   ");

         for (j = node ; j <= analysis.numnodes && number <= 10  ; j++) {
            for (k = dof ; k <= analysis.numdofs && number <= 10 ; k++) {
               data = MatrixData (P)[i][(j-1)*analysis.numdofs + k];
               position = (int) ((data - min)*dscale);
               if (buffer [position] == ' ' || buffer [position] == '|')
                  buffer [position] = (char) symbols [number];
               else {
                  if (buffer1 [position] == ' ' || buffer1 [position] == '|')
                     buffer1 [position] = (char) symbols [number];
                  else if (buffer2[position] == ' ' || buffer2[position] == '|')
                     buffer2 [position] = (char) symbols [number];
               }

               number++;
            }
            dof = 1;
         }
         fprintf (fp,"           %s\n",buffer1);
         fprintf (fp,"%10.5g %67s\n",freq,buffer);
         fprintf (fp,"           %s\n",buffer2);          

         freq += analysis.step;
      }
      fprintf (fp, "\n");

	/*
	 * print the legend for this graph
	 */

      number = 1;
      dof = start_dof;
      for (i = node ; i <= analysis.numnodes && number <= 10 ; i++) {
         for (j = dof ; j <= analysis.numdofs && number <= 10 ; j++) {
            fprintf (fp,"         %c %c %c    %s(%d)\n", symbols [number],
                     symbols [number], symbols [number],
                     spectra_labels[(int) analysis.dofs[j]], 
                     analysis.nodes[i] -> number);

            number++;
         }
         dof = 1;
      }
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}

/*****************************************************************************
 *
 * Function:	WriteTransferFunctions
 *
 * Description:	
 *	
 *****************************************************************************/

void
WriteTransferFunctions(Matrix *H, NodeDOF *forced, unsigned numforced, FILE *fp)
{
   double	w;
   unsigned	i,j,k,l,m,n;
   unsigned	table;
   unsigned	node, dof;
   unsigned	start_dof;
   unsigned	start_node;
   unsigned	number;
   unsigned	ntables;
   unsigned	idof;
   unsigned	inode;
   unsigned	input;

   ntables = (unsigned) (numforced*analysis.numnodes*analysis.numdofs / 4);
   if ((numforced * analysis.numdofs * analysis.numnodes) % 4 != 0)
      ntables++;

	/*
	 * for each table, print the appropriate headers and then
	 * the transfer function data for those same DOFs
	 */

   input = 1;
   node = 1;
   dof = 1;
   m = n = 0; /* gcc -Wall */
   for (table = 1 ; table <= ntables ; table++) {
      start_dof = dof;
      start_node = node;
      fprintf (fp,"\n------------------------------------------------------------------\n");
      fprintf (fp,"       freq ");
      number = 1;
      for (i = input ; i <= numforced && number <= 4 ; i++) {

         inode = forced [i] -> node -> number;
         idof = forced [i] -> dof;

         for (m = node ; m <= analysis.numnodes && number <= 4 ; m++) {
            for (n = dof ; n <= analysis.numdofs && number <= 4 ; n++) {
               fprintf (fp," %s(%d)->%s(%d)", labels[idof], inode, 
                        labels[(int) analysis.dofs[n]], 
                        analysis.nodes[m] -> number);
            
               number++; 
            }
            dof = 1;
         }
         node = 1;
      }

      fprintf (fp,"\n------------------------------------------------------------------\n");

	/*
	 * output the transfer function at this frequency step for each DOF 
	 * in this table
	 */

      w = analysis.start;
      for (l = 1 ; l <= Mrows (H[1]) ; l++) {
         fprintf (fp,"%11.5g", w);
         number = 1;
         dof = start_dof;
         node = start_node;
         for (i = input ; i <= numforced && number <= 4 ; i++) {
            for (j = node ; j <= analysis.numnodes && number <= 4  ; j++) {
               for (k = dof ; k <= analysis.numdofs && number <= 4 ; k++) {
                  fprintf (fp, "  %11.5g",
                           mdata(H[i],l,(j-1)*analysis.numdofs + k));
                  number++;
               }
               dof = 1;
            }
            node = 1;
         }
         fprintf (fp,"\n");
 
         w += analysis.step;
      }
    
      input = i - 1;
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}

void
PlotTransferFunctions(Matrix *H, NodeDOF *forced, unsigned numforced, FILE *fp)
{
   unsigned	i,j,k,l;
   unsigned	m,n,o;
   double	data;
   double	max;
   double	min;
   double	dscale;
   unsigned	inode, idof;
   char		buffer [80];
   char		buffer1 [80];
   char		buffer2 [80];
   unsigned	graph, ngraphs;
   unsigned	start_dof, start_node, number;
   unsigned	dof, node, input;
   int		position;
   double	freq;

   ngraphs = (unsigned) (analysis.numnodes * analysis.numdofs * numforced / 10);
   if ((analysis.numdofs * analysis.numnodes * numforced) % 10 != 0)
      ngraphs++;

   node = 1;
   dof = 1;
   input = 1;
   m = n = o = start_node = start_dof = 0; /* gcc -Wall */
   for (graph = 1 ; graph <= ngraphs ; graph++) {

	/*
	 * find the max and the min for this graph, then compute the scale
	 */

      min = max = MatrixData (H [input]) [1][(node-1)*analysis.numdofs + dof]; 
      for (i = 1 ; i <= MatrixRows (H [input]) ; i++) {
         start_dof = dof;
         start_node = node;
         number = 1;

         for (o = input ; o <= numforced && number <= 10 ; o++) {
            for (m = node ; m <= analysis.numnodes && number <= 10 ; m++) {
               for (n = dof ; n <= analysis.numdofs && number <= 10 ; n++) {
        
                  data = MatrixData (H [o])[i][(m-1)*analysis.numdofs + n];

                  if (data < min)
                     min = data;
                  else if (data > max)
                     max = data;

                  number++;
               }
               dof = 1;
            }
            node = 1; 
         }
      }

      dscale = 67.0/(max - min);
   
      fprintf (fp,"\n");
      fprintf (fp,"           %-11.5g             %11.5g                      %11.5g\n",min,(min+max)/2.0,max);
      fprintf (fp,"           +------------------------------+-----------------------------------+\n");

	/*
	 * for each line (frequency) form a line of the plot by placing
	 * each DOF in the appropriate place in the line buffer.  If that
	 * space is occupied we try the line above it and then the line
	 * below it.  If all available spaces are filled, we skip this point.
	 */

      freq = analysis.start;
      for (i = 1; i <= MatrixRows (H [input]) ; i++) {
         number = 1;
         dof = start_dof;
         sprintf (buffer1,"|                                                                   ");
         sprintf (buffer2,"|                                                                   ");
         sprintf (buffer,"|                                                                   ");

         for (l = input ; l <= numforced && number <= 10 ; l++) {
            for (j = node ; j <= analysis.numnodes && number <= 10  ; j++) {
               for (k = dof ; k <= analysis.numdofs && number <= 10 ; k++) {
                  data = MatrixData (H [l])[i][(j-1)*analysis.numdofs + k];
                  position = (int) ((data - min)*dscale);
                  if (buffer [position] == ' ' || buffer [position] == '|')
                     buffer [position] = (char) symbols [number];
                  else {
                     if (buffer1 [position] == ' ' || buffer1 [position] == '|')
                        buffer1 [position] = (char) symbols [number];
                     else if (buffer2[position] == ' ' || buffer2[position] == '|')
                        buffer2 [position] = (char) symbols [number];
                  }

                  number++;
               }
               dof = 1;
            }
            node = 1;
         }

         fprintf (fp,"           %s\n",buffer1);
         fprintf (fp,"%10.5g %67s\n",freq,buffer);
         fprintf (fp,"           %s\n",buffer2);          

         freq += analysis.step;
      }

      fprintf (fp, "\n");

	/*
	 * print the legend for this graph
	 */

      number = 1;
      dof = start_dof;
      node = start_node;
      for (l = input ; l <= numforced && number <= 10 ; l++) {

         inode = forced [i] -> node -> number;
         idof = forced [i] -> dof;

         for (i = node ; i <= analysis.numnodes && number <= 10 ; i++) {
            for (j = dof ; j <= analysis.numdofs && number <= 10 ; j++) {
               fprintf (fp,"         %c %c %c    %s(%d)->%s(%d)\n", 
                        symbols [number], symbols [number], symbols [number],
                        labels [idof], inode, 
                        labels [(int) analysis.dofs[j]], 
                        analysis.nodes[i] -> number);

               number++;
            }
            dof = 1;
         }
         node = 1;
      }

      input = o - 1;
      node  = m - 1;
      dof   = n; 
   }
   fprintf (fp,"\n");

   return;
}

/**************************************************************************
 *
 * Function:	WriteMaterialStatistics	
 *
 * Parameters:	element		array of elements
 *		numelts		number of elements
 *		output		output stream to write to
 *
 * Return value:none	
 *
 ***************************************************************************/

int
WriteMaterialStatistics(FILE *output)
{
   Element	*element;
   unsigned	numelts;
   unsigned	i,j,
 		num_materials,
		number [50];
   char		*names [50];
   unsigned	flag;
   double	length [50],
                volume [50],
		area [50],
  		mass [50],
		l,v,a,
		tot_mass;

   element = problem.elements;
   numelts = problem.num_elements;

   num_materials = 0;
   flag = 0;

   for (i = 0 ; i < 50 ; i++) {
      number [i] = length [i] = area [i] = volume [i] = mass [i] = 0;
      names [i] = NULL;
   }

   for (i = 1 ; i <= numelts ; i++) {
      flag = 0;
      for (j = 0 ; j < num_materials ; j++) {
         if (element [i] -> material -> name == names [j]) {
            flag = 1;
            break;
          }
      } 
      
      if (!flag)
         names [num_materials++] = element [i] -> material -> name;

      if (num_materials >= 50) {
         error ("too many materials used for summary stats");
         return 1;
      }
   }
 
   for (i = 1 ; i <= numelts ; i++) {

      for (j = 0 ; j < num_materials ; j++) {
         if (element [i] -> material -> name == names [j]) {
            flag = j;
            break;
         }
      }      

      number [flag]++;

      switch (element [i] -> definition -> shape) {

      case Linear:

         l = (element [i] -> node[1] -> x - element [i] -> node[2] -> x)* 
             (element [i] -> node[1] -> x - element [i] -> node[2] -> x) +
             (element [i] -> node[1] -> y - element [i] -> node[2] -> y)* 
             (element [i] -> node[1] -> y - element [i] -> node[2] -> y) +
             (element [i] -> node[1] -> z - element [i] -> node[2] -> z)* 
             (element [i] -> node[1] -> z - element [i] -> node[2] -> z); 
         l = sqrt (l);
         length [flag] += l;
         mass [flag] += element [i] -> material -> rho * l *
                          element [i] -> material -> A;

         break;

      case Planar:

         a = ElementArea (element [i], element [i] -> definition -> shapenodes);
         area [flag] += a;
         mass [flag] += element [i] -> material -> rho * a *
                          element [i] -> material -> t;

         break;

      case Solid:

         v = 0;
         volume [flag] += v;
         mass [flag] += element [i] -> material -> rho*v;
         break;
      }
   }

   fprintf (output,"\nMaterial Usage Summary\n");
   fprintf (output,"--------------------------\n");

   tot_mass = 0;
   for (i = 0 ; i < num_materials ; i++) {

      fprintf (output,"Material: %s\n",names [i]);
      fprintf (output,"Number:   %d\n", number[i]);
      if (length [i] > 0)
         fprintf (output,"Length:   %8.4f\n",length [i]);
      if (area [i] > 0)
         fprintf (output,"Area:     %8.4f\n",area [i]);
      if (volume [i] > 0)
         fprintf (output,"Volume:   %8.4f\n",volume [i]);

      fprintf (output,"Mass:     %8.4f\n\n",mass [i]);

      tot_mass += mass [i];
   }

   fprintf (output,"Total mass: %10.4f\n", tot_mass);

   return 1;
}

/**************************************************************************
 *
 * Function:	WriteGraphicsFile
 *
 * Parameters:	filename	name of output file
 *		mag		magnification to apply to dx's
 *				(0.0 means you get the undisplaced structure)
 *
 * Return value:0 upon success
 *		1 on error
 *
 ***************************************************************************/

int
WriteGraphicsFile(char *filename, double mag)
{
   Element	*element;
   unsigned	numelts;
   FILE		*output;
   unsigned	i,j;

   numelts = problem.num_elements;
   element = problem.elements;

   if ((output = fopen (filename, "w")) == NULL)
      return 1;

   for (i = 1 ; i <= numelts ; i++) {
      for (j = 1 ; j <= element [i] -> definition -> shapenodes ; j++) {
         if (element [i] -> node[j] == NULL) break;
         fprintf (output,"%g %g %g\n", 
                  element [i] -> node [j] -> x + mag*element [i] -> node [j] -> dx [Tx],
                  element [i] -> node [j] -> y + mag*element [i] -> node [j] -> dx [Ty], 
                  element [i] -> node [j] -> z + mag*element [i] -> node [j] -> dx [Tz]); 
      }

      if (element [i] -> definition -> shapenodes > 2)
         fprintf (output,"%g %g %g\n", 
                  element [i] -> node [1] -> x + mag*element [i] -> node [1] -> dx [Tx],
                  element [i] -> node [1] -> y + mag*element [i] -> node [1] -> dx [Ty], 
                  element [i] -> node [1] -> z + mag*element [i] -> node [1] -> dx [Tz]); 

      fprintf (output,"\n");
   }

   fclose (output);

   return 0;
}

void
PrintGlobalMatrices(FILE *fp, Matrix M, Matrix C, Matrix K)
{

   if (M != NullMatrix) {
      if (!IsZeroMatrix (M)) {
         fprintf (fp, "M = \n");
         PrintMatrix (M, fp);
      }
      else
         fprintf (fp, "M = [0]\n\n");
   }

   if (C != NullMatrix) {
      if (!IsZeroMatrix (C)) {
         fprintf (fp, "C = \n");
         PrintMatrix (C, fp);
      }
      else
         fprintf (fp, "C = [0]\n\n");
   }

   if (K != NullMatrix) {
      if (!IsZeroMatrix (K)) {
         fprintf (fp, "K = \n");
         PrintMatrix (K, fp);
      }
      else
         fprintf (fp, "K = [0]\n\n");
   }
}

int
MatlabGlobalMatrices(char *filename, Matrix M, Matrix C, Matrix K)
{
   FILE	    *fp;

   fp = fopen (filename, "w");

   if (fp == NULL) {
      error ("could not open file %s for writing", filename);
      return 1;
   }

   if (M != NullMatrix) 
      MatrixToMatlab (M, fp, "M");

   if (C != NullMatrix) 
      MatrixToMatlab (C, fp, "C");

   if (K != NullMatrix) 
      MatrixToMatlab (K, fp, "K");

   return 0;
}

void
WriteModalResults(FILE *fp, Matrix M, Matrix C, Matrix K, Matrix lambda)
{
   unsigned	i;

   if (!IsZeroMatrix (M)) {
      fprintf (fp, "modal M = \n");
      PrintMatrix (M, fp);
   }
   else 
      fprintf (fp, "modal M = [0]\n\n"); 

   if (!IsZeroMatrix (K)) {
      fprintf (fp, "modal K = \n");
      PrintMatrix (K, fp);
   }
   else 
      fprintf (fp, "modal K = [0]\n\n"); 

   if (!IsZeroMatrix (C)) {
      fprintf (fp, "modal C = \n");
      PrintMatrix (C, fp);
   }
   else
      fprintf (fp, "modal C = [0]\n\n");

   if (!IsZeroMatrix (C)) {
      fprintf (fp, "-------------------------\n");
      fprintf (fp, "Modal damping ratios\n");
      fprintf (fp, "-------------------------\n");
      for (i = 1 ; i <= Mrows(M) ; i++)
         fprintf (fp, "%3d     %11.5f\n", i, 
                  mdata(C,i,i)/(2.0*mdata(M,i,i)*mdata(lambda,i,1))); 
   }

   return;
}

/******************************************************************************
 *
 * Function:	WriteLoadCaseTable
 *
 * Parameters:	dtable		matrix of nodal displacements for loadcases
 *		fp		file pointer to output table to
 *
 ******************************************************************************/

void
WriteLoadCaseTable(Matrix dtable, FILE *fp)
{
   unsigned	i,j,k,m,n;
   unsigned	table;
   unsigned	node, dof;
   unsigned	start_dof;
   unsigned	number;
   unsigned	ntables;

   ntables = (unsigned) (analysis.numnodes * analysis.numdofs / 4);
   if ((analysis.numdofs * analysis.numnodes) % 4 != 0)
      ntables++;

	/*
	 * for each table, print the appropriate headers and then
	 * the displacement data for those same DOFs
	 */

   node = 1;
   dof = 1;
   n = 0; /* gcc -Wall */
   for (table = 1 ; table <= ntables ; table++) {
      start_dof = dof;
      fprintf (fp,"\n--------------------------------------------------------------------\n");
      fprintf (fp,"   loadcase");
      number = 1;
      for (m = node ; m <= analysis.numnodes && number <= 4 ; m++) {
         for (n = dof ; n <= analysis.numdofs && number <= 4 ; n++) {
            fprintf (fp,"        %s(%d)", labels[(int) analysis.dofs[n]],
                     analysis.nodes[m] -> number);
            
            number++; 
         }
         dof = 1;
      }
      fprintf (fp,"\n--------------------------------------------------------------------\n");

	/*
	 * output the displacement in this loadcase for each DOF in this table
	 */

      for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
         fprintf (fp,"%11s", problem.loadcases [i] -> name);

         number = 1;
         dof = start_dof;
         for (j = node ; j <= analysis.numnodes && number <= 4  ; j++) {
            for (k = dof ; k <= analysis.numdofs && number <= 4 ; k++) {
               fprintf (fp, "  %11.5g",
                        MatrixData (dtable)[i][(j-1)*analysis.numdofs + k]);
               number++;
            }
            dof = 1;
         }
         fprintf (fp,"\n");
      }
    
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}

/******************************************************************************
 *
 * Function:	PlotLoadCaseTable
 *
 * Parameters:	dtable		matrix of nodal displacements with time
 *		fp		file pointer to output table to
 *
 ******************************************************************************/

void
PlotLoadCaseTable(Matrix dtable, FILE *fp)
{
   unsigned	i,j,k;
   unsigned	m,n;
   double	data;
   double	max;
   double	min;
   double	dscale;
   char		buffer [80];
   char		buffer1 [80];
   char		buffer2 [80];
   unsigned	graph, ngraphs;
   unsigned	start_dof, number;
   unsigned	dof, node;
   int		position;

   ngraphs = (unsigned) (analysis.numnodes * analysis.numdofs / 10);
   if ((analysis.numdofs * analysis.numnodes) % 10 != 0)
      ngraphs++;

   node = 1;
   dof = 1;
   m = n = start_dof = 0;
   for (graph = 1 ; graph <= ngraphs ; graph++) {

	/*
	 * find the max and the min for this graph, then compute the scale
	 */

      min = max = MatrixData (dtable) [1][(node-1)*analysis.numdofs + dof]; 
      for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
         start_dof = dof;
         number = 1;

         for (m = node ; m <= analysis.numnodes && number <= 10 ; m++) {
            for (n = dof ; n <= analysis.numdofs && number <= 10 ; n++) {
        
               data = MatrixData (dtable)[i][(m-1)*analysis.numdofs + n];

               if (data < min)
                  min = data;
               else if (data > max)
                  max = data;

               number++;
            }
            dof = 1;
         }
      }

      dscale = 67.0/(max - min);
   
      fprintf (fp,"\n");
      fprintf (fp,"           %-11.5g             %11.5g                      %11.5g\n",min,(min+max)/2.0,max);
      fprintf (fp,"           +------------------------------+-----------------------------------+\n");

	/*
	 * for each line (time step) form a line of the plot by placing
	 * each DOF in the appropriate place in the line buffer.  If that
	 * space is occupied we try the line above it and then the line
	 * below it.  If all available spaces are filled, we skip this point.
	 */

      for (i = 1; i <= MatrixRows (dtable) ; i++) {
         number = 1;
         dof = start_dof;
         sprintf (buffer1,"|                                                                   ");
         sprintf (buffer2,"|                                                                   ");
         sprintf (buffer,"|                                                                   ");

         for (j = node ; j <= analysis.numnodes && number <= 10  ; j++) {
            for (k = dof ; k <= analysis.numdofs && number <= 10 ; k++) {
               data = MatrixData (dtable)[i][(j-1)*analysis.numdofs + k];
               position = (int) ((data - min)*dscale);
               if (buffer [position] == ' ' || buffer [position] == '|')
                  buffer [position] = (char) symbols [number];
               else {
                  if (buffer1 [position] == ' ' || buffer1 [position] == '|')
                     buffer1 [position] = (char) symbols [number];
                  else if (buffer2[position] == ' ' || buffer2[position] == '|')
                     buffer2 [position] = (char) symbols [number];
               }

               number++;
            }
            dof = 1;
         }
         fprintf (fp,"           %s\n",buffer1);

         fprintf (fp,"%10s %67s\n", problem.loadcases [i] -> name, buffer);
         fprintf (fp,"           %s\n",buffer2);          
      }
      fprintf (fp, "\n");

	/*
	 * print the legend for this graph
	 */

      number = 1;
      dof = start_dof;
      for (i = node ; i <= analysis.numnodes && number <= 10 ; i++) {
         for (j = dof ; j <= analysis.numdofs && number <= 10 ; j++) {
            fprintf (fp,"         %c %c %c    %s(%d)\n", symbols [number],
                     symbols [number], symbols [number],
                     labels[(int) analysis.dofs[j]], 
                     analysis.nodes[i] -> number);

            number++;
         }
         dof = 1;
      }
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}

/******************************************************************************
 *
 * Function:	WriteLoadRangeTable
 *
 ******************************************************************************/

void
WriteLoadRangeTable(Matrix dtable, FILE *fp)
{
   unsigned	i,j,k,m,n;
   unsigned	table;
   unsigned	node, dof;
   unsigned	start_dof;
   unsigned	number;
   unsigned	ntables;

   ntables = (unsigned) (analysis.numnodes * analysis.numdofs / 4);
   if ((analysis.numdofs * analysis.numnodes) % 4 != 0)
      ntables++;

	/*
	 * for each table, print the appropriate headers and then
	 * the displacement data for those same DOFs
	 */

   node = 1;
   dof = 1;
   n = 0; /* gcc -Wall */
   for (table = 1 ; table <= ntables ; table++) {
      start_dof = dof;
      fprintf (fp,"\n------------------------------------------------------------------\n");
      fprintf (fp,"       input");
      number = 1;
      for (m = node ; m <= analysis.numnodes && number <= 4 ; m++) {
         for (n = dof ; n <= analysis.numdofs && number <= 4 ; n++) {
            fprintf (fp,"        %s(%d)", labels[(int) analysis.dofs[n]],
                     analysis.nodes[m] -> number);
            
            number++; 
         }
         dof = 1;
      }
      fprintf (fp,"\n------------------------------------------------------------------\n");

	/*
	 * output the displacement at this time step for each DOF 
	 *in this table
	 */

      for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
         fprintf (fp,"%11.5g",analysis.start + (i-1)*analysis.step);

         number = 1;
         dof = start_dof;
         for (j = node ; j <= analysis.numnodes && number <= 4  ; j++) {
            for (k = dof ; k <= analysis.numdofs && number <= 4 ; k++) {
               fprintf (fp, "  %11.5g",
                        MatrixData (dtable)[i][(j-1)*analysis.numdofs + k]);
               number++;
            }
            dof = 1;
         }
         fprintf (fp,"\n");
      }
    
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}

/******************************************************************************
 *
 * Function:	PlotLoadRangeTable
 *
 * Parameters:	dtable		matrix of nodal displacements with time
 *		fp		file pointer to output table to
 *
 ******************************************************************************/

void
PlotLoadRangeTable(Matrix dtable, FILE *fp)
{
   unsigned	i,j,k;
   unsigned	m,n;
   double	data;
   double	max;
   double	min;
   double	dscale;
   char		buffer [80];
   char		buffer1 [80];
   char		buffer2 [80];
   unsigned	graph, ngraphs;
   unsigned	start_dof, number;
   unsigned	dof, node;
   int		position;

   ngraphs = (unsigned) (analysis.numnodes * analysis.numdofs / 10);
   if ((analysis.numdofs * analysis.numnodes) % 10 != 0)
      ngraphs++;


   node = 1;
   dof = 1;
   m = n = start_dof = 0;
   for (graph = 1 ; graph <= ngraphs ; graph++) {

	/*
	 * find the max and the min for this graph, then compute the scale
	 */

      min = max = MatrixData (dtable) [1][(node-1)*analysis.numdofs + dof]; 
      for (i = 1 ; i <= MatrixRows (dtable) ; i++) {
         start_dof = dof;
         number = 1;

         for (m = node ; m <= analysis.numnodes && number <= 10 ; m++) {
            for (n = dof ; n <= analysis.numdofs && number <= 10 ; n++) {
        
               data = MatrixData (dtable)[i][(m-1)*analysis.numdofs + n];

               if (data < min)
                  min = data;
               else if (data > max)
                  max = data;

               number++;
            }
            dof = 1;
         }
      }

      dscale = 67.0/(max - min);
   
      fprintf (fp,"\n");
      fprintf (fp,"           %-11.5g             %11.5g                      %11.5g\n",min,(min+max)/2.0,max);
      fprintf (fp,"           +------------------------------+-----------------------------------+\n");

	/*
	 * for each line (time step) form a line of the plot by placing
	 * each DOF in the appropriate place in the line buffer.  If that
	 * space is occupied we try the line above it and then the line
	 * below it.  If all available spaces are filled, we skip this point.
	 */

      for (i = 1; i <= MatrixRows (dtable) ; i++) {
         number = 1;
         dof = start_dof;
         sprintf (buffer1,"|                                                                   ");
         sprintf (buffer2,"|                                                                   ");
         sprintf (buffer,"|                                                                   ");

         for (j = node ; j <= analysis.numnodes && number <= 10  ; j++) {
            for (k = dof ; k <= analysis.numdofs && number <= 10 ; k++) {
               data = MatrixData (dtable)[i][(j-1)*analysis.numdofs + k];
               position = (int) ((data - min)*dscale);
               if (buffer [position] == ' ' || buffer [position] == '|')
                  buffer [position] = (char) symbols [number];
               else {
                  if (buffer1 [position] == ' ' || buffer1 [position] == '|')
                     buffer1 [position] = (char) symbols [number];
                  else if (buffer2[position] == ' ' || buffer2[position] == '|')
                     buffer2 [position] = (char) symbols [number];
               }

               number++;
            }
            dof = 1;
         }
         fprintf (fp,"           %s\n",buffer1);
         fprintf (fp,"%10.5g %67s\n", 
                  analysis.start + (i-1)*analysis.step,buffer);
         fprintf (fp,"           %s\n",buffer2);          
      }
      fprintf (fp, "\n");

	/*
	 * print the legend for this graph
	 */

      number = 1;
      dof = start_dof;
      for (i = node ; i <= analysis.numnodes && number <= 10 ; i++) {
         for (j = dof ; j <= analysis.numdofs && number <= 10 ; j++) {
            fprintf (fp,"         %c %c %c    %s(%d)\n", symbols [number],
                     symbols [number], symbols [number],
                     labels[(int) analysis.dofs[j]], 
                     analysis.nodes[i] -> number);

            number++;
         }
         dof = 1;
      }
      node = m - 1;
      dof = n; 
   }
   fprintf (fp,"\n");

   return;
}
