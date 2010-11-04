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

/****************************************************************************
 *
 * File:         loom.c
 *
 * Description:  Contains code for the driver application for the 
 *		 FElt application loom, which is the command line
 *		 based solution and post-processing engine intended
 *		 mainly as the mathematical engine to be called from
 *		 encapsulators such as WinFElt.
 *		
 *		 It is not, nor is it meant to be, particularly 
 *		 user-friendly.  It's meant to be called by other
 *		 application using shell() or system() commands
 *		 and has such has very mechanical, very overdone
 *		 command-line specification which is probably built
 *		 by the calling application based on check boxes
 *		 and text entries ...
 *
 *****************************************************************************/

# include <stdio.h>
# include <string.h>
# include <unistd.h>
# include "problem.h"
# include "fe.h"
# include "error.h"
# include "allocate.h"
# include "definition.h"
# include "options.h"
# include "wireframe.h"
# include "graph.h"
# include "contour.h"
# include "results.hpp"
# include "renumber.hpp"
# include "transient.hpp"

typedef int	Boolean;

static    FILE	*fp_err = NULL;
static    FILE	*fp_out = NULL;

static char	*in = NULL;
static char	*out = NULL;
static char	*err_out = NULL;
static Boolean  unlink_input = 0;

	/*
	 * Basic felt options
	 */

static Boolean	 solve = 0;
static Boolean   details = 0;
static Boolean   debug = 0;
static Boolean   no_err = 0;
static Boolean   summary = 0;
static Boolean   matrices = 0;
static Boolean   transfer = 0;
static Boolean   eigen = 0;
static Boolean   orthonormal = 0;
static Boolean   table = 0;
static Boolean   renumber = 0;

static char 	*structure_out = NULL;
static char 	*displaced_out = NULL;
static double   displaced_mag;
static double   xrot;
static double   yrot;
static double   zrot;
static double   zsc;
static int	contour_width = 0;
static int	contour_height = 0;

static char	*graph_out = NULL;

static char    **contour_out = NULL;
static int       num_contour;
static char     *contour_type;
static char	*contour_result;
static char	*contour_format;
static int      *contour_component;
static Boolean	 contour_hequal = 0;
static Boolean	 contour_overlay = 0;

# define Check(n)         if (n < 0) return 1		    /* simple error */
# define ReqCheck(n)      if (n < 1) return 1		    /* required     */
# define OptCheck(n, opt) if (n < 1 && (opt)) return 1      /* req if opt   */
# define NeedCheck(n, opt) if (n == 1 && !(opt)) return 1   /* requires opt */
 
/************************************************************************
 * Function:	ParseOptions						*
 *									*
 * Description:	Parses the specific command line options.		*
 ************************************************************************/

static int ParseOptions (int argc, char **argv)
{
    int		n;

    n = GetSoloStringOption(argc, argv, "in", &in);          	ReqCheck(n);
    n = GetSoloStringOption(argc, argv, "out", &out);        	ReqCheck(n);
    n = GetSoloStringOption(argc, argv, "err_out", &err_out);  	ReqCheck(n);
    n = GetBooleanOption(argc, argv, "unlink", &unlink_input);	Check(n);
    
    n = GetBooleanOption(argc, argv, "solve",       &solve);	Check(n);
    n = GetBooleanOption(argc, argv, "no_err", &no_err); Check(n);
    n = GetBooleanOption(argc, argv, "debug",       &debug);	Check(n);
    n = GetBooleanOption(argc, argv, "details",     &details);	Check(n);
    n = GetBooleanOption(argc, argv, "summary",     &summary);	Check(n);
    n = GetBooleanOption(argc, argv, "matrices",    &matrices);	NeedCheck(n, solve);
    n = GetBooleanOption(argc, argv, "transfer",    &transfer);	NeedCheck(n, solve);
    n = GetBooleanOption(argc, argv, "eigen",       &eigen);	NeedCheck(n, solve);
    n = GetBooleanOption(argc, argv, "table",       &table);	NeedCheck(n, solve);
    n = GetBooleanOption(argc, argv, "orthonormal", &orthonormal);  NeedCheck(n, solve);
    n = GetBooleanOption(argc, argv, "renumber",    &renumber);	Check(n);
    
    n = GetSoloStringOption(argc, argv, "graph_out", &graph_out); 
    NeedCheck(n, solve);
   
    n = GetSoloStringOption(argc, argv, "structure_out", &structure_out);  
    Check(n);

    n = GetSoloStringOption(argc, argv, "displaced_out", &displaced_out);  
    NeedCheck(n, solve);
    n = GetSoloDoubleOption(argc, argv, "displaced_mag", &displaced_mag);  
    OptCheck(n, displaced_out);


    n = GetSoloDoubleOption(argc, argv, "xrot", &xrot);
    OptCheck(n, displaced_out || structure_out);
    n = GetSoloDoubleOption(argc, argv, "yrot", &yrot);
    OptCheck(n, displaced_out || structure_out);
    n = GetSoloDoubleOption(argc, argv, "zrot", &zrot);
    OptCheck(n, displaced_out || structure_out);
    n = GetSoloDoubleOption(argc, argv, "zscale", &zsc);
    OptCheck(n, displaced_out || structure_out);

    n = GetStringOption(argc, argv, "contour_out", &contour_out);
    NeedCheck(n, solve);
    num_contour = n;

    n = GetIntegerOption(argc, argv, "contour_component", &contour_component);
    OptCheck(n, contour_out);

    if (n != num_contour)
       return 1;
       
    n = GetBooleanOption(argc, argv, "contour_overlay", &contour_overlay);
    NeedCheck(n, contour_out);
    n = GetBooleanOption(argc, argv, "contour_hequal", &contour_hequal);
    NeedCheck(n, contour_out);
    n = GetSoloStringOption(argc, argv, "contour_result", &contour_result);
    OptCheck(n, contour_out);
    n = GetSoloStringOption(argc, argv, "contour_format", &contour_format);
    NeedCheck(n, contour_out);
    n = GetSoloStringOption(argc, argv, "contour_type", &contour_type);
    NeedCheck(n, contour_out);
    n = GetSoloIntegerOption(argc, argv, "contour_width", &contour_width);
    NeedCheck(n, contour_out);
    n = GetSoloIntegerOption(argc, argv, "contour_height", &contour_height);
    NeedCheck(n, contour_out);
    
    if (ArgsUsed ( ) + 1 < argc)
       return 1;

    return 0;
}

/* UNUSED
static void DumpOptions (void)
{
   printf ("in = %s\n", in);
   printf ("out = %s\n", out);
   printf ("err_out = %s\n\n", err_out);

   printf ("graph_out = %s\n", graph_out);
   printf ("contour_out = %s\n", contour_out);
   printf ("structure_out = %s\n", structure_out);
   printf ("displaced_out = %s\n\n", displaced_out);

   printf ("xrot = %g\n", xrot);
   printf ("yrot = %g\n", yrot);
   printf ("zrot = %g\n", zrot);
   printf ("zsc = %g\n", zsc);

   printf ("solve = %d\n\n", solve);

   printf ("debug = %d\n", debug);
   printf ("details = %d\n", details);
   printf ("summary = %d\n", summary);
   printf ("matrices = %d\n", matrices);
   printf ("transfer = %d\n", transfer);
   printf ("eigen = %d\n", eigen);
   printf ("orthonormal = %d\n", orthonormal);
   printf ("renumber = %d\n", renumber);
   printf ("table = %d\n", table);
}
*/

/************************************************************************
 * Function:	 ExitLoom						*
 *									*
 * Description:	 							*
 ************************************************************************/

extern "C" void
ExitLoom(int status)
{

    if (fp_out)
        fclose(fp_out);

    if (fp_err)
        fclose(fp_err);

    if (unlink_input)
       unlink(in); 

    exit(status);
}

/************************************************************************
 * Function:	 main							*
 *									*
 * Description:	 Main is the driver function for loom.			*
 ************************************************************************/

int main (int argc, char **argv)
{
    int		 i;
    char	*title;			/* title of problem		*/
    Matrix	 M, K, C;		/* global matrices		*/
    Matrix	 Mcond, Ccond, Kcond;	/* condensed matrices		*/
    Matrix	 Mm, Km, Cm;		/* modal matrices		*/
    Matrix	 *H;			/* transfer function matrices   */
    Matrix	 S;			/* output spectra		*/
    Vector	 F,			/* force vector			*/
		 Fcond;			/* condensed force vector	*/
    Vector	 d;			/* displacement vector		*/
    Matrix	 x;			/* eigenvectors			*/
    Vector	 lambda;		/* eigenvalues			*/
    NodeDOF	 *forced;		/* array of forced DOF numbers  */
    unsigned	 numforced;		/* number of forced DOF		*/
    int		 status;		/* return status		*/
    cvector1<Reaction>	 R;			/* reaction force vector	*/
    Matrix	 dtable;		/* time-displacement table	*/
    cvector1u    old_numbers;		/* original node numbering	*/
    AnalysisType mode;


	/*
	 * if there are any option errors then do nothing 
	 * at all because there must be a bug ...
	 */

    if (ParseOptions (argc, argv)) {
        error ("options error");
        ExitLoom (1);
    }
/*
    DumpOptions ( );
*/
    if (in == NULL || out == NULL || err_out == NULL) {
        error ("file error");
        ExitLoom (1);
    }

    if (!no_err)
       fp_err = freopen(err_out, "w", stderr);
    else 
       fp_err = stderr;

	/*
	 * we just have to bail if this didn't work because we don't
	 * know where any error output should go
	 */

    if (fp_err == NULL) {
        error ("file redirect error");
        ExitLoom (1);
    }

    fp_out = fopen(out, "w");
    if (fp_out == NULL)
       Fatal ("temporary file error -> output");

    add_all_definitions ( );

	/*
	 * this will generate its own syntax errors so we don't need to
	 * say anything
	 */

    if (ReadFeltFile (in)) 
        ExitLoom (1);

    title    = problem.title;

	/*
	 * If debugging write the problem as we understand it
	 */

    if (debug) 
	fDumpFeltFile (fp_out);

    if (problem.num_nodes == 0 || problem.num_elements == 0) 
        Fatal ("nothing to do");

	/*
	 * Write a graphics file if necessary
	 */

    if (structure_out) 
       WriteWireframeFile (structure_out, 0.0, xrot, yrot, zrot, zsc);

	/*
	 * compute material statistics if desired
	 */
    
    if (summary)
       WriteMaterialStatistics (fp_out);

	/*
	 * if we're not actually going to solve for anything
	 * we can bail out right here
	 */

    if (!solve)
       ExitLoom (0);

        /*
         * if the user wanted analysis details we need
         * to set the detail stream
         */

    if (details)
        SetDetailStream (fp_out);

	/*
	 * find all the active DOFs in this problem	
	 */

    FindDOFS ( );
       
	/*
	 * renumber the nodes if desired
	 */

    if (renumber) 
        old_numbers = RenumberNodes (problem.nodes, problem.elements, 
                                     problem.num_nodes, problem.num_elements);

	/*
	 * switch on the problem type
	 */

    mode = SetAnalysisMode ( );

    switch (mode) {

       case Transient:
          status = CheckAnalysisParameters (Transient);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&K, &M, &C);
          if (matrices)
             PrintGlobalMatrices (fp_out, M, C, K);
 
          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);

          dtable = IntegrateHyperbolicDE (K, M, C);

          if (dtable == NullMatrix)
             Fatal ("fatal error in integration (probably a singularity).");

          if (table)
             WriteTransientTable (dtable, NullMatrix, fp_out);
    
          if (graph_out)
             WriteLineGraph (dtable, "Nodal Time-Displacement", "time", "dx", graph_out);

          break;
       
       case TransientThermal:
          analysis.dofs [1] = Tx;
          analysis.numdofs = 1;
          status = CheckAnalysisParameters (TransientThermal);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&K, &M, &C);
          
          if (matrices) 
             PrintGlobalMatrices (fp_out, M, NullMatrix, K);
 
          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);

          dtable = IntegrateParabolicDE (K, M);

          if (dtable == NullMatrix)
             Fatal ("fatal error in integration (probably a singularity).");

          if (table)
             WriteTransientTable (dtable, NullMatrix, fp_out);
    
          if (graph_out)
             WriteLineGraph (dtable, "Nodal Time-Temperature", "time", "T", graph_out);

          break;

       case Static:

          K = ConstructStiffness(&status);
          if (status)
             Fatal ("%d Fatal errors in element stiffness definitions", status);

          if (matrices)
             PrintGlobalMatrices (fp_out, NullMatrix, NullMatrix, K);

          F = ConstructForceVector ( );
          
          ZeroConstrainedDOF (K, F, &Kcond, &Fcond);
     
          d = SolveForDisplacements (Kcond, Fcond);
          if (d == NullVector)
             Fatal("could not solve for displacements, probably a singularity");
    
          status = ElementStresses ( );

          if (status) 
             Fatal ("%d Fatal errors found computing element stresses", status);

          R = SolveForReactions (K, d, old_numbers.c_ptr1());

          RestoreNodeNumbers (problem.nodes, old_numbers.c_ptr1(), problem.num_nodes);

          if (table)
             WriteStructuralResults (fp_out, title, R);

          if (displaced_out) 
             WriteWireframeFile (displaced_out, displaced_mag, 
                                 xrot, yrot, zrot, zsc);

          if (contour_out) {
             for (i = 0 ; i < num_contour ; i++) {
                if (strcmp(contour_result, "stress") == 0)
                   PlotStressField (contour_out [i], problem.elements, 
				    problem.num_elements, contour_component [i],
				    contour_hequal, contour_overlay,
                                    contour_width, contour_height);
                else if (strcmp(contour_result, "displacement") == 0)
                   PlotDisplacementField (contour_out [i], problem.nodes, 
					  problem.num_nodes, problem.elements, 
				          problem.num_elements,
                                          contour_component [i], 
					  contour_hequal, contour_overlay,
                                          contour_width, contour_height);
             }
          }

          break;

       case StaticThermal:

          K = ConstructStiffness(&status);

          if (status)
             Fatal ("%d Fatal errors in element stiffness definitions", status);

          if (matrices)
             PrintGlobalMatrices (fp_out, NullMatrix, NullMatrix, K);

          F = ConstructForceVector ( );
          
          ZeroConstrainedDOF (K, F, &Kcond, &Fcond);
     
          d = SolveForDisplacements (Kcond, Fcond);
          if (d == NullVector)
             Fatal("could not solve for displacements, probably a singularity");

          RestoreNodeNumbers (problem.nodes, old_numbers.c_ptr1(), problem.num_nodes);

          if (table)
             WriteTemperatureResults (fp_out, title);

          if (contour_out) {
             if (strcmp(contour_result, "displacement") == 0)
                PlotDisplacementField (contour_out [0], problem.nodes, 
				       problem.num_nodes, problem.elements, 
				       problem.num_elements, 1,
                                       contour_hequal, contour_overlay,
                                       contour_width, contour_height);
          }

          break;

       case StaticLoadCases:
       case StaticLoadRange:
          status = CheckAnalysisParameters (mode);
          if (status)
             Fatal ("%d errors found in analysis parameters.", status);

          K = ConstructStiffness(&status);
          if (status)
             Fatal ("%d Fatal errors in element stiffness definitions", status);

          if (matrices)
             PrintGlobalMatrices (fp_out, NullMatrix, NullMatrix, K);

          F = ConstructForceVector ( );
          
          ZeroConstrainedDOF (K, F, &Kcond, &Fcond);
           
          if (mode == StaticLoadCases)
             dtable = SolveStaticLoadCases (Kcond, Fcond);
          else
             dtable = SolveStaticLoadRange (Kcond, Fcond);

          if (dtable == NullMatrix)
             Fatal ("could not solve for global displacements");
            
          RestoreNodeNumbers (problem.nodes, old_numbers.c_ptr1(), problem.num_nodes);

          if (table)
             if (mode == StaticLoadCases)
                WriteLoadCaseTable (dtable, fp_out);
             else 
                WriteLoadRangeTable (dtable, fp_out);

          if (graph_out && mode == StaticLoadRange)
             WriteLineGraph (dtable, "Displacement vs. Force Level", "force", "dx", graph_out);

          break; 

       case StaticSubstitutionLoadRange:
       case StaticIncrementalLoadRange:
          status = CheckAnalysisParameters (StaticSubstitution);
          status += CheckAnalysisParameters (StaticLoadRange);
          if (status) 
             Fatal ("%d errors found in analysis parameters.", status);

          K = CreateNonlinearStiffness (&status);
          if (status)
             Fatal ("could not create global stiffness matrix");
         
          F = ConstructForceVector ( );
 
          if (mode == StaticSubstitutionLoadRange)
             dtable = SolveNonlinearLoadRange (K, F, 0);
          else
             dtable = SolveNonlinearLoadRange (K, F, 0); /* should be 1*/
             
          if (dtable == NullMatrix)
             Fatal ("did not converge on a solution");
         
          RestoreNodeNumbers (problem.nodes, old_numbers.c_ptr1(), problem.num_nodes);
                
          if (table)
             WriteLoadRangeTable (dtable, fp_out);

          if (graph_out)
             WriteLineGraph (dtable, "Displacement vs. Force Level", "force", "dx", graph_out);

          break;

       case StaticSubstitution:
       case StaticIncremental:
          status = CheckAnalysisParameters (mode);
          if (status) 
             Fatal ("%d errors found in analysis parameters.", status);

          K = CreateNonlinearStiffness (&status);
          if (status)
             Fatal ("could not create global stiffness matrix");
         
          F = ConstructForceVector ( );
 
          if (mode == StaticSubstitution)
             d = StaticNonlinearDisplacements (K, F, 0);
          else
             d = StaticNonlinearDisplacements (K, F, 0); /* should be 1 */
             
          if (d == NullMatrix)
             Fatal ("did not converge on a solution");
         
          RestoreNodeNumbers (problem.nodes, old_numbers.c_ptr1(), problem.num_nodes);
                
          if (table)
              WriteStructuralResults (fp_out, title, cvector1<Reaction>(0));

          if (contour_out) {
             if (strcmp(contour_result, "displacement") == 0)
                for (i = 0 ; i < num_contour ; i++)
                   PlotDisplacementField (contour_out [i], problem.nodes, 
				          problem.num_nodes, problem.elements, 
				          problem.num_elements, 
				  	  contour_component [i],
                                          contour_hequal, contour_overlay,
                                          contour_width, contour_height);
          }

          break;

       case Modal:
          status = CheckAnalysisParameters (Modal);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&K, &M, &C);
          
          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);

          RemoveConstrainedDOF (K, M, C, &Kcond, &Mcond, &Ccond);

          if (matrices)
             PrintGlobalMatrices (fp_out, Mcond, Ccond, Kcond);
 
          status = ComputeEigenModes (Kcond, Mcond, &lambda, &x);

          if (status == M_NOTPOSITIVEDEFINITE)
             Fatal ("coefficient matrix is not positive definite.");
          else if (status)
             Fatal ("could not compute eigenmodes (report status code %d).", status);

          NormalizeByFirst (x, x);
          WriteEigenResults (lambda, x, title, fp_out);
/*
          if (doplot)
             PlotModeShapes (x, fp_out);
*/           
          if (!eigen) {
             FormModalMatrices (x, Mcond, Ccond, Kcond, &Mm, &Cm, &Km, orthonormal);
             WriteModalResults (fp_out, Mm, Cm, Km, lambda);
          }
           
          break;

       case Spectral:
          status = CheckAnalysisParameters (Spectral);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&K, &M, &C);

          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);
 
          ZeroConstrainedDOF (K, NullMatrix, &Kcond, NULL);
          ZeroConstrainedDOF (M, NullMatrix, &Mcond, NULL);
          ZeroConstrainedDOF (C, NullMatrix, &Ccond, NULL);

          if (matrices)
             PrintGlobalMatrices (fp_out, Mcond, Ccond, Kcond);

          FindForcedDOF (&forced, &numforced);

          H = ComputeTransferFunctions (Mcond, Ccond, Kcond, forced, numforced);

          if (transfer) {
             if (table)
                WriteTransferFunctions (H, forced, numforced, fp_out);
             if (graph_out)
                WriteLineGraphTransferFunctions (H, forced, numforced, graph_out);
          }
          else {
             S = ComputeOutputSpectra (H, forced, numforced);
             if (S == NullMatrix)
                break; 

             if (table)  
                WriteOutputSpectra (S, fp_out);
       
             if (graph_out)
                WriteLineGraph (S, "Output Power Spectra", "frequency", "S", graph_out);
          }

          break;
    }

    ExitLoom (0);
  
    return 0;
}
