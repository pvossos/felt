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

# include <stdio.h>
# include <unistd.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Simple.h>
# include "Canvas.h" 
# include "Element.h"
# include "Node.h"
# include "Solution.h"
# include "allocate.h"
# include "problem.h"
# include "globals.h"
# include "Drawing.h"
# include "vfe.h"
# include "procedures.h"
# include "error.h"

extern ElementDialog element_d;
extern NodeDialog    node_d;

/****************************************************************************
 * Functions:	BuildNodeArray and BuildElementArray
 *
 * Description:	These are two tree iterators that build the node and element
 *		arrays from the node and element trees
 ****************************************************************************/

static unsigned	array_count;

static int BuildNodeArray (item)
   Item		item;
{
   FigureAttributes	attr;
   char			buffer [10];
   Node			node;
   Drawn		drawn; 

   node = (Node) item;
   problem.nodes [++array_count] = node;

   if (array_count != node -> number) {
      node -> number = array_count;

      drawn = (Drawn) node -> aux;
      if (drawn -> label != NULL) {
	  sprintf (attr.text = buffer, " %d", node -> number);
	  DW_SetAttributes (drawing, drawn -> label, DW_FigureText, &attr);
      }
   }

   return 0;
}

static int BuildElementArray (item)
   Item		item;
{
   FigureAttributes	attr;
   char			buffer [10];
   Element		element;
   Drawn		drawn; 

   element = (Element) item;
   problem.elements [++array_count] = element;

   if (array_count != element -> number) {
      element -> number = array_count;

      drawn = (Drawn) element -> aux;
      if (drawn -> label != NULL) {
         ComputeCenter (element, &attr.x, &attr.y);
         sprintf (attr.text = buffer, "%d", element -> number);
         DW_SetAttributes (drawing, drawn -> label, DW_FigureText |
                                DW_FigureLocation, &attr);
      }
   }

   return 0;
}

/****************************************************************************
 *
 * Function:	SolveProblem
 *
 * Description:	Basically main from the Felt application ... pretty 
 *		straightforward
 *
 ***************************************************************************/

int SolveProblem ( )
{
    unsigned	 numnodes;		/* total number of nodes	*/
    unsigned	 numelts;		/* total number of elements	*/
    Node	*node;			/* the array of nodes		*/
    Element	*element;		/* array of elements		*/
    Matrix	 K,			/* global stiffness matrix	*/
		 Kcond;			/* condensed stiffness matrix	*/
    Matrix	 M, C;
    Matrix	 Ccond;
    Matrix	 Mcond;			/* condensed mass matrix	*/
    Matrix	 Mm, Cm, Km;
    Matrix	*H;			/* matrix of transfer functions */
    Matrix	 S;
    Vector	 F,			/* force vector			*/
		 Fcond,			/* condensed force vector	*/
		 d;			/* displacement vector		*/
    Matrix	 x;			/* eigenvectors			*/
    Vector	 lambda;		/* eigenvalues			*/
    unsigned	 numforced;
    NodeDOF	*forced;
    int		 status;		/* return status		*/
    Reaction	*R;			/* reaction force vector	*/
    unsigned	 numreactions;		/* the number of reactions	*/
    char	*temp_out;
    FILE	*output;
    unsigned	*old_numbers;
    Matrix	 dtable;
    Matrix	 ttable;
    int		 error_flag;
    unsigned	 i;
    AnalysisType mode;

   	/*
	 * make sure the node and element numbering doesn't have any gaps
	 * and set-up the node and element arrays
	 */

    numnodes = CompactNodeNumbers ( );
    numelts  = CompactElementNumbers ( );
    node     = problem.nodes;
    element  = problem.elements;

    mode     = SetAnalysisMode ( );

    if (numnodes == 0 || numelts == 0) {
       error ("nothing to do!");
       return 1;
    }

    ClearNodes ( );

	/*
	 * create a temporary file for output
	 */

#ifdef _CONVEX_SOURCE
    temp_out = tmpnam ("velv");
#else
    temp_out = tempnam (NULL,"velv");
#endif

    output = fopen (temp_out,"w");
    if (output == NULL) {
       error ("Could not open temporary file for output.");
       return 1;
    }

	/*
	 * generate a debug file if requested
	 */

    if (solution -> debug)
       fWriteFeltFile (output);

	/*
	 * set the details stream if needed
	 */

    if (solution -> details)
        SetDetailStream (output);
    else
        SetDetailStream (NULL);

	/*	
	 * initialize all these so we won't have trouble destroying them
	 * if they never actually get allocated
	 */

    K = Kcond = Km = C = Ccond = Cm = M = Mcond = Mm = NullMatrix;
    x = NullMatrix;
    dtable = ttable = S = NullMatrix;
    H = NULL;
    F = Fcond = d = lambda = NullVector;
   
    
	/*
	 * find the active DOFs and renumber the nodes if desired (common
	 * steps in all problem modes
	 */

    FindDOFS ( );

    if (solution -> renumber)
       old_numbers = RenumberNodes (node, element, numnodes, numelts);
    else
       old_numbers = NULL;

    error_flag = 0;

	/*
	 * switch on the problem mode depending on whether we not a
	 * static and transient analysis
	 */

    switch (mode) {

    case Transient:
       status = CheckAnalysisParameters (Transient);
    
       if (status) {
          error ("%d errors found in analysis parameters.", status);
          error_flag = 1;
          break;
       }

       status = ConstructDynamic (&K, &M, &C);
     
       if (status) {
          error ("%d errors in stiffness and mass definitions.", status);
          error_flag = 1;
          break;
       }

       if (solution -> matrices) 
          PrintGlobalMatrices (output, M, C, K);

       if (analysis.step == 0.0) 
          dtable = IntegrateHyperbolicDE (K, M, C);
       else
          dtable = RosenbrockHyperbolicDE (K, M, C, &ttable);

       if (dtable == NullMatrix) {
          error ("could not perform integration - probably a singularity.");
          error_flag = 1;
          break;
       }
      
       if (old_numbers != NULL)
          RestoreNodeNumbers (node, old_numbers, numnodes);
       
       WriteTransientTable (dtable, ttable, output);

       if (solution -> plot)
          VelvetPlotTD (dtable, ttable, "time", "dx", "Nodal Time-Displacement", True);

       break;

    case Spectral:
       status = CheckAnalysisParameters (Spectral);
    
       if (status) {
          error ("%d errors found in analysis parameters.", status);
          error_flag = 1;
          break;
       }

       status = ConstructDynamic (&K, &M, &C);
     
       if (status) {
          error ("%d errors in stiffness and mass definitions.", status);
          error_flag = 1;
          break;
       }

       ZeroConstrainedDOF (K, NullMatrix, &Kcond, NULL);
       ZeroConstrainedDOF (M, NullMatrix, &Mcond, NULL);
       ZeroConstrainedDOF (C, NullMatrix, &Ccond, NULL);

       if (solution -> matrices)
          PrintGlobalMatrices (output, Mcond, Ccond, Kcond);

       FindForcedDOF (&forced, &numforced);

       H = ComputeTransferFunctions (Mcond, Ccond, Kcond, forced, numforced);

       if (!solution -> transfer)
          S = ComputeOutputSpectra (H, forced, numforced);

       if (old_numbers != NULL)
          RestoreNodeNumbers (node, old_numbers, numnodes);
       
       if (solution -> transfer) {
          WriteTransferFunctions (H, forced, numforced, output);

          if (solution -> plot)
             VelvetPlotTransferFunctions (H, forced, numforced, 
                                          "frequency", "H", 
                                          "Spectral Transfer Function");

          break;
       }

       WriteOutputSpectra (S, output);

       if (solution -> plot)
          VelvetPlotSpectra (S, "frequency", "S", "Output Power Spectra", True);

       break;

    case Static:

       K = ConstructStiffness (&status);

       if (status) {
   	  error ("%d fatal errors in element stiffness definitions.", status);
          error_flag = 1;
          break;
       }

       if (solution -> matrices)
          PrintGlobalMatrices (output, NullMatrix, NullMatrix, K);

       F = ConstructForceVector ( );

       ZeroConstrainedDOF (K, F, &Kcond, &Fcond);

	/*
	 * the result will not have new space - it will get returned
	 * in the same space that Fcond used to occupy ...
	 */

       d = SolveForDisplacements (Kcond, Fcond);

       if (d == NullVector) {
          error ("singluarity in final system of equations - cannot proceed.");
          error_flag = 1;
          break;
       }    

       numreactions = SolveForReactions (K, d, old_numbers, &R);

       status = ElementStresses ( );
       if (status) {
          error ("%d errors found computing element stresses.",status);
          error_flag = 1;
          break;
        }   

       if (old_numbers != NULL)
          RestoreNodeNumbers (node, old_numbers, numnodes);

       WriteStructuralResults (output, solution -> title, R, numreactions);

       if (numreactions) {
   	  for (i = 1; i <= numreactions; i ++)
	     XtFree ((char *) R [i]);

	  XtFree ((char *) (R + 1));
       }

       break;

    case StaticLoadCases:
    case StaticLoadRange:
       status = CheckAnalysisParameters (mode);
       if (status) {
          error ("%d errors found in analysis parameters.", status);
          error_flag = 1;
          break;
       }

       K = ConstructStiffness(&status);
       if (status) {
   	  error ("%d fatal errors in element stiffness definitions.", status);
          error_flag = 1;
          break;
       }

       if (solution -> matrices)
          PrintGlobalMatrices (output, NullMatrix, NullMatrix, K);

       F = ConstructForceVector ( );
          
       ZeroConstrainedDOF (K, F, &Kcond, &Fcond);

       if (mode == StaticLoadCases)
          dtable = SolveStaticLoadCases (Kcond, Fcond);
       else
          dtable = SolveStaticLoadRange (Kcond, Fcond);

       if (dtable == NullMatrix) {
          error ("could not solve for global displacements.");
          error_flag = 1;
          break;
       }    
         
       if (old_numbers != NULL)
          RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

       if (mode == StaticLoadCases)
          WriteLoadCaseTable (dtable, output);
       else 
          WriteLoadRangeTable (dtable, output);

       if (solution -> plot && mode == StaticLoadRange) 
          VelvetPlotLoadRange (dtable);

       break;

    case StaticSubstitutionLoadRange:
    case StaticIncrementalLoadRange:
       status = CheckAnalysisParameters (StaticSubstitution);
       status += CheckAnalysisParameters (StaticLoadRange);
       if (status) {
          error ("%d errors found in analysis parameters.", status);
          error_flag = 1;
          break;
       }

       K = CreateNonlinearStiffness (&status);
       if (status) {
   	  error ("could not create nonlinear stiffness matrix");
          error_flag = 1;
          break;
       }

       F = ConstructForceVector ( );
          
       if (mode == StaticSubstitutionLoadRange)
          dtable = SolveNonlinearLoadRange (K, F, 0);
       else
          dtable = SolveNonlinearLoadRange (K, F, 0); /* should be 1 */

       if (dtable == NullMatrix) {
          error ("did not converge on a solution");
          error_flag = 1;
          break;
       }    
         
       if (old_numbers != NULL)
          RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

       WriteLoadRangeTable (dtable, output);

       if (solution -> plot)
          VelvetPlotLoadRange (dtable);

       break;

    case StaticSubstitution:
    case StaticIncremental:
       status = CheckAnalysisParameters (mode);
       if (status) {
          error ("%d errors found in analysis parameters.", status);
          error_flag = 1;
          break;
       }

       K = CreateNonlinearStiffness (&status);
       if (status) {
   	  error ("could not create nonlinear stiffness matrix");
          error_flag = 1;
          break;
       }

       F = ConstructForceVector ( );
          
       if (mode == StaticSubstitution)
          d = StaticNonlinearDisplacements (K, F, 0);
       else
          d = StaticNonlinearDisplacements (K, F, 0); /* should be 1 */

       if (d == NullMatrix) {
          error ("did not converge on a solution");
          error_flag = 1;
          break;
       }    
        
       if (old_numbers != NULL)
          RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

       WriteStructuralResults (output, solution -> title, NULL, 0);

       break;

    case Modal:
       status = CheckAnalysisParameters (Modal);

       if (status) {
          error ("%d errors found in analysis parameters.", status);
          error_flag = 1;
          break;
       }

       status = ConstructDynamic (&K, &M, &C);
          
       if (status) {
          error ("%d errors in stiffness and mass definitions.", status);
          error_flag = 1;
          break;
       }

       RemoveConstrainedDOF (K, M, C, &Kcond, &Mcond, &Ccond);
 
       if (solution -> matrices)
          PrintGlobalMatrices (output, Mcond, Ccond, Kcond);

       status = ComputeEigenModes (Kcond, Mcond, &lambda, &x);
       if (status == M_NOTPOSITIVEDEFINITE) {
          error ("coefficient matrix is not positive definite.");
          error_flag = 1;
          break;
       }
       else if (status) {
          error ("error computing eigenmodes (report status code %d).", status);
          error_flag = 1;
          break;
       }

       if (solution -> mode_shapes) {
          dtable = ModalNodalDisplacements (x);
          SetupModeShapes (dtable, lambda);
       }
         
       NormalizeByFirst (x, x);
       WriteEigenResults (lambda, x, solution -> title, output);
       
       if (!solution -> eigen) {
          FormModalMatrices (x, Mcond, Ccond, Kcond, &Mm, &Cm, &Km, solution -> orthonormal);
          WriteModalResults (output, Mm, Cm, Km, lambda);
       }

       if (old_numbers != NULL)
          RestoreNodeNumbers (node, old_numbers, numnodes);
                    
       break;

    case TransientThermal:
       analysis.dofs [1] = Tx;
       analysis.numdofs = 1;
       status = CheckAnalysisParameters (TransientThermal);

       if (status) {
          error ("%d Errors found in analysis parameters.", status);
          error_flag = 1;
          break;
       }

       status = ConstructDynamic (&K, &M, &C);
       
       if (status) {
          error ("%d errors in stiffness and mass definitions.", status);
          error_flag = 1;
          break;
       }

       if (solution -> matrices)
          PrintGlobalMatrices (output, M, NullMatrix, K);

       dtable = IntegrateParabolicDE (K, M);

       if (dtable == NullMatrix) {
          error ("could not perform integration - probably a singularity.");
          error_flag = 1;
          break;
       }
      
       if (old_numbers != NULL)
          RestoreNodeNumbers (node, old_numbers, numnodes);

       WriteTransientTable (dtable, NullMatrix, output);

       if (solution -> plot)
          VelvetPlotTD (dtable, NullMatrix, "time", "T", "Nodal Time-Temperature", False);

       break;

    case StaticThermal:

       K = ConstructStiffness(&status);

       if (status)
          Fatal ("%d Fatal errors in element stiffness definitions", status);

       if (solution -> matrices)
          PrintGlobalMatrices (output, NullMatrix, NullMatrix, K);

       F = ConstructForceVector ( );
       
       ZeroConstrainedDOF (K, F, &Kcond, &Fcond);
  
       d = SolveForDisplacements (Kcond, Fcond);

       if (d == NullVector) {
          error ("singluarity in final system of equations - cannot proceed.");
          error_flag = 1;
          break;
       }    

       if (old_numbers != NULL)
          RestoreNodeNumbers (node, old_numbers, numnodes);

       WriteTemperatureResults (output, problem.title);

       break;
    }
   
    if (error_flag && old_numbers != NULL)
       RestoreNodeNumbers (node, old_numbers, numnodes);
    
    if (!error_flag && solution -> summary)
       WriteMaterialStatistics (output);
 
    if (dtable != NullMatrix) 
       DestroyMatrix (dtable);
    if (ttable != NullMatrix)
       DestroyMatrix (ttable);

    if (K != NullMatrix)
       DestroyMatrix (K);
    if (M != NullMatrix) 
       DestroyMatrix (M);
    if (C != NullMatrix) 
       DestroyMatrix (C);
    if (F != NullVector) 
       DestroyVector (F);

    if (Kcond != NullMatrix) 
       DestroyMatrix (Kcond);
    if (Mcond != NullMatrix) 
       DestroyMatrix (Mcond);
    if (Ccond != NullMatrix) 
       DestroyMatrix (Ccond);

    if (Km != NullMatrix) 
       DestroyMatrix (Km);
    if (Mm != NullMatrix) 
       DestroyMatrix (Mm);
    if (Cm != NullMatrix) 
       DestroyMatrix (Cm);

    if (x != NullMatrix)
       DestroyMatrix (x);
    if (lambda != NullVector)
       DestroyVector (lambda);

    if (d == NullVector && Fcond != NullVector)
       DestroyVector (Fcond);
    else if (d != NullVector) 
       DestroyVector (d);

    if (old_numbers) {
       ZeroOffset (old_numbers); 
       Deallocate (old_numbers);
    }

    fclose (output);
   
    if (error_flag)
       return 1;
 
    if (solution -> felt) 
       OutputDialogView (output_dialog, temp_out, 20, 80);

    unlink (temp_out);
    XtFree ((char *) temp_out);
    
    if (solution -> felt) 
       OutputWindowPopup ( );

    if (solution -> stress) 
       SetupStresses (False);

    if (solution -> displacement) 
       SetupDisplacements (False);
       
    if (solution -> structure)
       SetupStructure (False);

    return 0;
}

int CompactNodeNumbers ( )
{
    unsigned		numnodes;

    numnodes = TreeSize (problem.node_tree);
    if (numnodes == 0)
       return problem.num_nodes = 0;

	/*
	 * build the array of _consecutive_ pointers to nodes by
	 * creating new space for it and then iterating on the tree
	 */

    if (problem.nodes) {
       ZeroOffset (problem.nodes); 
       Deallocate (problem.nodes);
    }
    problem.nodes = Allocate (Node, numnodes);
    UnitOffset (problem.nodes);

    DW_SetAutoRedraw (drawing, False);

    array_count = 0;
    TreeSetIterator (problem.node_tree, BuildNodeArray);
    TreeIterate (problem.node_tree);

    if (canvas -> node_numbers)
       DW_SetAutoRedraw (drawing, True);

    return problem.num_nodes = numnodes;
}          
    
int CompactElementNumbers ( )
{
    unsigned		numelts;

    numelts = TreeSize (problem.element_tree);
    if (numelts == 0) 
       return problem.num_elements = 0;

    if (problem.elements) {
       ZeroOffset (problem.elements); 
       Deallocate (problem.elements);
    }

    problem.elements = Allocate (Element, numelts);
    UnitOffset (problem.elements);

    DW_SetAutoRedraw (drawing, False);

    array_count = 0;
    TreeSetIterator (problem.element_tree, BuildElementArray);
    TreeIterate (problem.element_tree);

    if (canvas -> element_numbers)
       DW_SetAutoRedraw (drawing, True);

    return problem.num_elements = numelts;
}

void SetupAndSolve ( )
{
    SetWaitCursor (drawing);

    BufferErrors (True);
    SolveProblem ( );
    BufferErrors (False);
   
    NodeDialogDisplay (node_d, NodeDialogActive (node_d));
    ElementDialogDisplay (element_d, ElementDialogActive (element_d));

    SetNormalCursor (drawing);
}

void SetupAnimate ( )
{
    int			status1, status2;
    unsigned		i,j;
    unsigned		count;
    unsigned		dofs [7];
    Matrix		M, C, K;
    unsigned		numnodes, numelts;
    Matrix		dtable;
    unsigned		*old_numbers;
    Node		*anodes;
    char		adofs [7];
    unsigned		anumnodes;
    unsigned		anumdofs;
    double		z_plane;
    Boolean		draw3d;

    if (problem.mode != Transient) {
       error ("problem mode must be transient for animation\n");
       return;
    }

    BufferErrors (True);

    numnodes = CompactNodeNumbers ( );
    if (numnodes == 0)
       return;

    numelts = CompactElementNumbers ( );
    if (numelts == 0)
       return;

    SetWaitCursor (drawing);

    draw3d = False;
    z_plane = problem.elements[1] -> node[1] -> z;

    for (i = 1; i <= numelts ; i++) {
       for (j=1 ; j <= problem.elements[i] -> definition -> shapenodes ; j++) {
       
          if (problem.elements[i] -> node[j] -> z != z_plane) {
             draw3d = True;
             break;
          }
       }
       if (draw3d)
          break;
    }
 
    anumdofs = analysis.numdofs;
    for (i = 1 ; i <= analysis.numdofs ; i++)
       adofs [i] = analysis.dofs [i];

    anodes = analysis.nodes;
    analysis.nodes = problem.nodes;

    anumnodes = analysis.numnodes;
    analysis.numnodes = numnodes;

    if (draw3d) {
       analysis.numdofs = 3;
       analysis.dofs [1] = Tx;
       analysis.dofs [2] = Ty;
       analysis.dofs [3] = Tz;
    }
    else {
       analysis.numdofs = 2;
       analysis.dofs [1] = Tx;
       analysis.dofs [2] = Ty;
    }


    if (solution -> renumber)
       old_numbers = RenumberNodes (problem.nodes, problem.elements, 
                                    numnodes, numelts);
    else
       old_numbers = NULL;

    K = M = C = dtable = NullMatrix;
 
    count = FindDOFS (problem.elements, numelts, dofs);

    status1 = CheckAnalysisParameters (Transient);
    status2 = 0;

    if (!status1)
       status2 = ConstructDynamic (&K, &M, &C);

    if (status1 || status2) 
       error ("errors found - could not animate.");
    else {
       dtable = IntegrateHyperbolicDE (K, M, C);

       if (draw3d)
          AnimateStructure3D (dtable, problem.nodes, problem.elements,
                              numnodes, numelts);
       else
          AnimateStructure (dtable, problem.nodes, problem.elements,
                            numnodes, numelts);

       if (old_numbers)
          RestoreNodeNumbers (problem.nodes, old_numbers, numnodes);
    }

    analysis.nodes = anodes;
    analysis.numnodes = anumnodes;
    analysis.numdofs = anumdofs;

    for (i = 1 ; i <= analysis.numdofs ; i++) 
       analysis.dofs [i] = adofs [i];

    if (old_numbers) {
       ZeroOffset (old_numbers);
       Deallocate (old_numbers);
    }
      
    if (K != NullMatrix) 
       DestroyVector (K);
    if (M != NullMatrix) 
       DestroyVector (M);
    if (C != NullMatrix) 
       DestroyVector (C);
    if (dtable != NullMatrix) 
       DestroyMatrix (dtable);

    BufferErrors (False);

    SetNormalCursor (drawing);
    return;
}

void SetupStresses (build_elt)
    Boolean	build_elt;
{
    Element    *e;
    unsigned	numelts;
    unsigned	nd,i,j;
    int		depth;
    Arg		arglist [1];
    int		flag;

	/*
	 * technically someone on a 1-bit screen should never get here ...
	 * but just to be sure.
	 */

    XtSetArg (arglist[0], XtNdepth, &depth);
    XtGetValues (toplevel, arglist, 1);
    if (depth < 8) {
        error ("you must have a > 8-bit deep screen to do stress plots");
        return;
    }

    if (build_elt) 
        numelts = CompactElementNumbers ( );
    else
        numelts = problem.num_elements;

    if (numelts == 0) 
       return;

    e = problem.elements;

    nd = 0;
    flag = 1;
    for (i = 1; i <= numelts ; i++) {
         if (e [i] -> node [1] -> stress == NULL) {
            error ("could not get nodally averaged stresses for all elements");
            return;
         }
         if (solution -> s_component > e [i] -> definition -> numstresses) { 
            error ("invalid stress component for element %d",i);
            return;
         }

         if (flag) {
            for (j = 1 ; j <= e [i] -> ninteg ; j++) {
               if (e [i] -> stress [j] -> 
                   values [solution -> s_component] != 0)
            
                  flag = 0;
            }
         }

         nd += problem.elements[i] -> ninteg;
    }

    if (flag) {
       error ("all stresses are zero for specified component");
       return;
    }

    SetWaitCursor (drawing);
    CreateOpenGLShell("stressShell", "Stress Plot", True,
                      solution -> s_component, e, numelts, True);
    SetNormalCursor (drawing);

    return;
}

void SetupDisplacements (build_arrays)
    Boolean	build_arrays;
{
    unsigned    numnodes;
    unsigned	numelts;
    unsigned	i;
    unsigned	flag;
    int		depth;
    Arg		arglist [1];

    XtSetArg (arglist[0], XtNdepth, &depth);
    XtGetValues (toplevel, arglist, 1);
    if (depth < 8) {
        error ("you must have a > 8-bit deep screen to do displacement plots");
        return;
    }

    if (build_arrays) {
        numelts = CompactElementNumbers ( );
        numnodes = CompactNodeNumbers ( );
    }
    else {
        numelts = problem.num_elements;
        numnodes = problem.num_nodes;
    }

    if (numelts == 0 || numnodes == 0)
       return;

    flag = 1;
    for (i = 1 ; i <= numnodes ; i++) {

       if (problem.nodes [i] -> dx [solution -> d_component] != 0) {
          flag = 0;
          break;
       }
    }

    if (flag) {
       error ("all displacements are zero for specified component");
       return;
    }

    SetWaitCursor (drawing);
    CreateOpenGLShell("displShell", "Displacement Plot", False,
                      solution -> d_component, problem.elements, numelts, True);
    SetNormalCursor (drawing);

    return;
}

void SetupModeShapes (phi, lambda)
    Matrix	phi;
    Matrix	lambda;
{
    unsigned	i,j;
    double	z_plane;
    Boolean	draw3d;
    unsigned	numelts;

    numelts = problem.num_elements;

    draw3d = False;
    z_plane = problem.elements[1] -> node[1] -> z;

    for (i = 1; i <= numelts ; i++) {
       for (j = 1 ; j <= problem.elements[i] -> definition -> shapenodes ; j++) {
       
          if (problem.elements[i] -> node[j] -> z != z_plane) {
             draw3d = True;
             break;
          }
       }
       if (draw3d)
          break;
    }
 
    if (draw3d)
       DrawModeShapes3D (phi, lambda, problem.nodes, problem.elements, 
                         problem.num_nodes, problem.num_elements);
    else
       DrawModeShapes (phi, lambda, problem.nodes, problem.elements, 
                       problem.num_nodes, problem.num_elements);

    return;
}

void SetupStructure (build_elt) 
    Boolean	build_elt;
{
    unsigned	i,j;
    double	z_plane;
    unsigned	numelts;

    if (build_elt) 
       numelts = CompactElementNumbers ( );
    else
       numelts = problem.num_elements;

    if (numelts == 0)
       return;

    CreateOpenGLShell("structShell", "Structure Plot", False, 0,
                      problem.elements, numelts, False);
}

void OptimizeNumbering ( )
{
    char		buffer [10];
    FigureAttributes	attr;
    unsigned		numnodes, numelts;
    unsigned		i;
    Drawn 		drawn;
    Node		node;

    numnodes = CompactNodeNumbers ( );
    if (numnodes == 0)
       return;

    numelts = CompactElementNumbers ( );
    if (numelts == 0) 
       return;
  
    for (i = 1 ; i <= numnodes ; i++) 
       TreeDelete (problem.node_tree, problem.nodes [i]);

    (unsigned *) RenumberNodes (problem.nodes, problem.elements, 
                                numnodes, numelts);

    DW_SetAutoRedraw (drawing, False);
    for (i = 1 ; i <= numnodes ; i++) {
       node = problem.nodes [i];
       TreeInsert (problem.node_tree, node);

       drawn = (Drawn) (node -> aux);
       if (drawn -> label != NULL) {     
          sprintf (attr.text = buffer, " %d", node -> number); 
          DW_SetAttributes (drawing, drawn -> label, DW_FigureText, &attr);
       }
    }

    if (canvas -> node_numbers)
       DW_SetAutoRedraw (drawing, True);

    return;
}
