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
 * File:         felt.c
 *
 * Description:  Contains code for the driver application for the Finite 
 *               ELemenT (FELT) package currently under development.
 *
 *****************************************************************************/

# include <stdio.h>
# include <string.h>
# include "problem.h"
# include "fe.h"
# include "error.h"
# include "allocate.h"
# include "version.h"
# include "definition.h"
# include "results.h"
# include "draw.h"

# define streq(a,b)	!strcmp(a,b)

static char *usage = "\
usage: felt [options] [filename]\n\
       -debug              write debugging output\n\
       -preview            write a simple ASCII rendering of the problem\n\
       +table              do not print tabular dynamic results\n\
       -plot               plot dynamic, load case, or load range results\n\
       -transfer           only show transfer functions for spectral results\n\
       -eigen              only compute eigen results for modal analysis\n\
       -orthonormal        use orthonormal mode shapes for modal matrices\n\
       -renumber           force automatic node renumbering\n\
       -summary            include material summary statistics\n\
       -matrices           print the global matrices\n\
       -details            print ancillary analysis details\n\
       -matlab filename    write the global matrices to a file\n\
       -graphics filename  create file for structure visualization\n\
       -version            print version information and exit\n\
       -nocpp              do not use a preprocessor\n\
       -cpp filename       preprocessor to use\n\
       -Dname[=value]      define a macro\n\
       -Uname              undefine a macro\n\
       -Idirectory         specify include directory\n\
";

static int   debug = 0;
static int   preview = 0;
static int   summary = 0;
static int   matrices = 0;
static int   dospectra = 1;
static int   domodal = 1;
static int   orthonormal = 0;
static int   doplot = 0;
static int   dotable = 1;
static int   renumber = 0;
static int   details = 0;
static char *graphics = NULL;
static char *matlab = NULL;


/************************************************************************
 * Function:	ParseFeltOptions					*
 *									*
 * Description:	Parses the felt specific command line options.		*
 ************************************************************************/

static int ParseFeltOptions (int *argc, char *argv[])
{
    int   i;
    int   j;
    char *arg;


    j = 1;
    for (i = 1; i < *argc; i ++)
	if (streq ((arg = argv [i]), "-help")) {
	    fputs (usage, stderr);
	    exit (0);
        } else if (streq (arg, "-version")) {
	    fprintf (stderr, "felt %s\n", VERSION);
            exit (0);
	} else if (streq (arg, "-debug")) {
	    debug = 1;
	} else if (streq (arg, "-preview")) {
	    preview = 1;
	} else if (streq (arg, "-matrices")) {
	    matrices = 1;
	} else if (streq (arg, "-summary")) {
	    summary = 1;
	} else if (streq (arg, "-plot")) {
	    doplot = 1;
	} else if (streq (arg, "+plot")) {
	    doplot = 0;
	} else if (streq (arg, "-table")) {
	    dotable = 1;
	} else if (streq (arg, "+table")) {
	    dotable = 0;
        } else if (streq (arg, "-transfer")) {
            dospectra = 0;
        } else if (streq (arg, "-eigen")) {
            domodal = 0;
        } else if (streq (arg, "-orthonormal")) {
            orthonormal = 1;
        } else if (streq (arg, "-renumber")) {
            renumber = 1;
        } else if (streq (arg, "-details")) {
            details = 1;
	} else if (streq (arg, "-matlab")) {
	    if (++ i == *argc) {
		fputs (usage, stderr);
		return 1;
	    }
	    matlab = argv [i];
	} else if (streq (arg, "-graphics")) {
	    if (++ i == *argc) {
		fputs (usage, stderr);
		return 1;
	    }
	    graphics = argv [i];
	} else
	    argv [j ++] = arg;

    argv [*argc = j] = NULL;
    return 0;
}

/************************************************************************
 * Function:	 main							*
 *									*
 * Description:	 Main is the driver function for the felt package.	*
 ************************************************************************/

int main (int argc, char *argv[])
{
    char	 *title;		/* title of problem		*/
    Matrix	  M, K, C;		/* global matrices		*/
    Matrix	  Mcond, Ccond, Kcond;	/* condensed matrices		*/
    Matrix	  Mm, Km, Cm;		/* modal matrices		*/
    Matrix	 *H;			/* transfer function matrices   */
    Matrix	  S;			/* output spectra		*/
    Vector	  F,			/* force vector			*/
		  Fcond;		/* condensed force vector	*/
    Vector	  d;			/* displacement vectors		*/
    Matrix	  x;			/* eigenvectors			*/
    Vector	  lambda;		/* eigenvalues			*/
    NodeDOF	 *forced;		/* array of forced nodal DOF    */
    unsigned	  numforced;		/* number of forced DOF		*/
    int		  status;		/* return status		*/
    Reaction	 *R;			/* reaction force vector	*/
    unsigned	  numreactions;		/* the number of reactions	*/
    Matrix	  dtable;		/* time-displacement table	*/
    Matrix	  ttable;		/* time step table		*/
    unsigned	 *old_numbers;		/* original node numbering	*/
    AnalysisType  mode;			/* current analysis type	*/

        /*
         * Do everything to setup the problem
         */

    if (ParseCppOptions (&argc, argv)) {
	fputs (usage, stderr);
	exit (1);
    }

    if (ParseFeltOptions (&argc, argv)) {
	fputs (usage, stderr);
	exit (1);
    }

    if (argc > 2) {
	fputs (usage, stderr);
	exit (1);
    }

    add_all_definitions ( );

    if (ReadFeltFile (argc == 2 ? argv [1] : "-"))
	exit (1);

    title    = problem.title;


	/*
	 * If debugging write the problem as we understand it to stdout
	 */

    if (debug) 
	WriteFeltFile ("-");

    if (problem.num_nodes == 0 || problem.num_elements == 0) 
       Fatal ("nothing to do");

    if (preview)
       DrawStructureASCII (stdout, 78, 22);

	/*
	 * Write a graphics file if necessary
	 */

    if (graphics != NULL) {
       status = WriteGraphicsFile (graphics, 0.0);
       if (status)
          Fatal ("could not open graphics file for output");
    }

	/*
	 * if the user wanted analysis details we need
	 * to set the detail stream
	 */

    if (details)
        SetDetailStream (stdout);

	/*
	 * find all the active DOFs in this problem	
	 */

    FindDOFS ( );
       
	/*
	 * renumber the nodes if desired
	 */

    old_numbers = NULL;

    if (renumber) 
	old_numbers = RenumberNodes (problem.nodes, problem.elements, 
                                     problem.num_nodes, problem.num_elements);

	/*
	 * switch on the problem type (transient or static)
	 */

    mode = SetAnalysisMode ( );

    switch (mode) {

       case Transient:
          status = CheckAnalysisParameters (Transient);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&K, &M, &C);
          if (matrices)
             PrintGlobalMatrices (stdout, M, C, K);

          if (matlab) 
             MatlabGlobalMatrices (matlab, M, C, K);
 
          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);

          if (analysis.step > 0.0) {
             dtable = IntegrateHyperbolicDE (K, M, C);
             ttable = NullMatrix;
          }
          else
             dtable = RosenbrockHyperbolicDE (K, M, C, &ttable);

          if (old_numbers != NULL)
             RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

          if (dtable == NullMatrix)
             Fatal ("fatal error in integration (probably a singularity).");

          if (dotable)
             WriteTransientTable (dtable, ttable, stdout);
    
          if (doplot)
             PlotTransientTable (dtable, ttable, analysis.step, stdout);

          break;
       
       case TransientThermal:
          analysis.dofs [1] = Tx;
          analysis.numdofs = 1;
          status = CheckAnalysisParameters (TransientThermal);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&K, &M, &C);
          
          if (matrices) 
             PrintGlobalMatrices (stdout, M, NullMatrix, K);

          if (matlab) 
             MatlabGlobalMatrices (matlab, M, NullMatrix, K);
 
          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);

          dtable = IntegrateParabolicDE (K, M);

          if (dtable == NullMatrix)
             Fatal ("fatal error in integration (probably a singularity).");

          if (old_numbers != NULL)
             RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

          if (dotable)
             WriteTransientTable (dtable, NullMatrix, stdout);
    
          if (doplot)
             PlotTransientTable (dtable, NullMatrix, analysis.step, stdout);

          break;

       case Static:

          K = ConstructStiffness(&status);
          if (status)
             Fatal ("%d Fatal errors in element stiffness definitions", status);

          if (matrices)
             PrintGlobalMatrices (stdout, NullMatrix, NullMatrix, K);

          if (matlab) 
             MatlabGlobalMatrices (matlab, NullMatrix, NullMatrix, K);

          F = ConstructForceVector ( );

          ZeroConstrainedDOF (K, F, &Kcond, &Fcond);

          d = SolveForDisplacements (Kcond, Fcond);
          if (d == NullMatrix)
             Fatal ("could not solve for global displacements");

          status = ElementStresses ( );
          if (status) 
             Fatal ("%d Fatal errors found computing element stresses", status);
    
          numreactions = SolveForReactions (K, d, old_numbers, &R);

          if (old_numbers != NULL)
             RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

          WriteStructuralResults (stdout, title, R, numreactions);

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
             PrintGlobalMatrices (stdout, NullMatrix, NullMatrix, K);

          if (matlab) 
             MatlabGlobalMatrices (matlab, NullMatrix, NullMatrix, K);

          F = ConstructForceVector ( );
          
          ZeroConstrainedDOF (K, F, &Kcond, &Fcond);
           
          if (mode == StaticLoadCases)
             dtable = SolveStaticLoadCases (Kcond, Fcond);
          else
             dtable = SolveStaticLoadRange (Kcond, Fcond);

          if (dtable == NullMatrix)
             Fatal ("could not solve for global displacements");
            
          if (old_numbers != NULL)
             RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

          if (dotable)
             if (mode == StaticLoadCases)
                WriteLoadCaseTable (dtable, stdout);
             else 
                WriteLoadRangeTable (dtable, stdout);

          if (doplot)
             if (mode == StaticLoadCases)
                PlotLoadCaseTable (dtable, stdout);
             else 
                PlotLoadRangeTable (dtable, stdout);

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
         
          if (old_numbers != NULL)
             RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);
                
          if (dotable)
             WriteLoadRangeTable (dtable, stdout);

          if (doplot)
             PlotLoadRangeTable (dtable, stdout);

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
         
          if (old_numbers != NULL)
             RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);
                
          WriteStructuralResults (stdout, title, NULL, 0);

          break;

       case StaticThermal:

          K = ConstructStiffness(&status);

          if (status)
             Fatal ("%d Fatal errors in element stiffness definitions", status);

          if (matrices)
             PrintGlobalMatrices (stdout, NullMatrix, NullMatrix, K);

          if (matlab) 
             MatlabGlobalMatrices (matlab, NullMatrix, NullMatrix, K);

          F = ConstructForceVector ( );
          
          ZeroConstrainedDOF (K, F, &Kcond, &Fcond);
     
          d = SolveForDisplacements (Kcond, Fcond);
          if (d == NullVector)
             exit (1);

          if (old_numbers != NULL)
             RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

          WriteTemperatureResults (stdout, title);

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
             PrintGlobalMatrices (stdout, Mcond, Ccond, Kcond);
 
          if (matlab) 
             MatlabGlobalMatrices (matlab, Mcond, Ccond, Kcond);

          status = ComputeEigenModes (Kcond, Mcond, &lambda, &x);

          if (status == M_NOTPOSITIVEDEFINITE)
             Fatal ("coefficient matrix is not positive definite.");
          else if (status)
             Fatal ("could not compute eigenmodes (report status code %d).", status);

          NormalizeByFirst (x, x);
          WriteEigenResults (lambda, x, title, stdout);

          if (doplot)
             PlotModeShapes (x, stdout);
            
          if (domodal) {
             FormModalMatrices (x, Mcond, Ccond, Kcond, &Mm, &Cm, &Km, orthonormal);
             WriteModalResults (stdout, Mm, Cm, Km, lambda);
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
             PrintGlobalMatrices (stdout, Mcond, Ccond, Kcond);

          if (matlab) 
             MatlabGlobalMatrices (matlab, Mcond, Ccond, Kcond);

          FindForcedDOF (&forced, &numforced);

          H = ComputeTransferFunctions (Mcond, Ccond, Kcond, forced, numforced);

          if (dospectra) {
             S = ComputeOutputSpectra (H, forced, numforced);
             if (S == NullMatrix)
                break; 
          }

          if (old_numbers != NULL)
             RestoreNodeNumbers (problem.nodes, old_numbers, problem.num_nodes);

          if (!dospectra) {
             if (dotable)
                WriteTransferFunctions (H, forced, numforced, stdout);
             if (doplot)
                PlotTransferFunctions (H, forced, numforced, stdout);

             break;
          }

          if (dotable)  
             WriteOutputSpectra (S, stdout);
       
          if (doplot)
             PlotOutputSpectra (S, stdout);

          break;
    }

    if (summary)
       WriteMaterialStatistics (stdout);

    exit (0);
}
