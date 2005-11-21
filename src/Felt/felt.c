/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-2006 Jason I. Gobat and Darren C. Atkinson

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

# define streq(a,b)	!strcmp(a,b)


# ifndef DOS
static char *usage = "\
usage: felt [options] [filename]\n\
       -debug               write debugging output\n\
       -preview             write a simple ASCII rendering of the problem\n\
       +table               do not print tabular dynamic results\n\
       -plot                plot dynamic, load case, or load range results\n\
       -transfer            only show transfer functions for spectral results\n\
       -eigen               only compute eigen results for modal analysis\n\
       -orthonormal         use orthonormal mode shapes for modal matrices\n\
       -renumber            force automatic node renumbering\n\
       -summary             include material summary statistics\n\
       -matrices            print the global matrices\n\
       -details             print ancillary analysis details\n\
       -matlab filename     write the global matrices to a file\n\
       -matlab-all filename write all matrices to a file\n\
       -gnuplot             output tables readable by gnuplot\n\
       -graphics filename   create file for structure visualization\n\
       -version             print version information and exit\n\
       -nocpp               do not use a preprocessor\n\
       -cpp filename        preprocessor to use\n\
       -Dname[=value]       define a macro\n\
       -Uname               undefine a macro\n\
       -Idirectory          specify include directory\n\
";
# else
static char *usage = "\
usage: felt [options] [filename]\n\
       -debug               write debugging output\n\
       -preview             write a simple ASCII rendering of the problem\n\
       +table               do not print tabular dynamic results\n\
       -plot                do graphical plot for dynamic results\n\
       -transfer            only show transfer functions for spectral results\n\
       -eigen               only compute eigen results for modal analysis\n\
       -orthonormal         use orthonormal mode shapes for modal matrices\n\
       -renumber            force automatic node renumbering\n\
       -summary             include material summary statistics\n\
       -matrices            print the global matrices\n\
       -details             print ancillary analysis details\n\
       -matlab filename     write the global matrices to a file\n\
       -matlab-all filename write all matrices to a file\n\
       -gnuplot             output tables readable by gnuplot\n\
       -graphics filename   create file for structure visualization\n\
       -version             print version information and exit\n\
";
# endif


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
static int   gnuplot = 0;
static int   details = 0;
static char *graphics = NULL;
static char *matlab = NULL;
static char *matlab_all = NULL;

static void WriteGnuplotTable( Matrix, Matrix, FILE*); 


static void InitData( data)
    Problem *data;
{
    data->M      = NullMatrix;    /* Initialize all matrices to Null */
    data->C      = NullMatrix;
    data->K      = NullMatrix;
    data->Mcond  = NullMatrix;
    data->Ccond  = NullMatrix;
    data->Kcond  = NullMatrix;
    data->Mm     = NullMatrix;
    data->Cm     = NullMatrix;
    data->Km     = NullMatrix;
    data->H      = NULL;          /* This is a pointer to a matrix   */
    data->S      = NullMatrix;
    data->F      = NullMatrix;
    data->Fcond  = NullMatrix;
    data->d      = NullMatrix;
    data->x      = NullMatrix;
    data->lambda = NullMatrix;
    data->dtable = NullMatrix;
    data->ttable = NullMatrix;
}
/************************************************************************
 * Function:	ParseFeltOptions					*
 *									*
 * Description:	Parses the felt specific command line options.		*
 ************************************************************************/

static int ParseFeltOptions (argc, argv)
    int  *argc;
    char *argv [ ];
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
        } else if (streq (arg, "-gnuplot")) {
            gnuplot = 1;
        } else if (streq (arg, "-details")) {
            details = 1;
	} else if (streq (arg, "-matlab")) {
	    if (++ i == *argc) {
		fputs (usage, stderr);
		return 1;
	    }
	    matlab = argv [i];
	} else if (streq (arg, "-matlab-all")) {
	    if (++ i == *argc) {
		fputs (usage, stderr);
		return 1;
	    }
	    matlab_all = argv [i];
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

int main (argc, argv)
    int   argc;
    char *argv [ ];
{
    int		  status;		/* return status		*/

    InitData( &problem);

        /*
         * Do everything to setup the problem
         */

# ifndef DOS
    if (ParseCppOptions (&argc, argv)) {
	fputs (usage, stderr);
	exit (1);
    }
# endif

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

    problem.old_numbers = NULL;

    if (renumber) 
	problem.old_numbers = RenumberNodes (problem.nodes, problem.elements, 
                                     problem.num_nodes, problem.num_elements);

	/*
	 * switch on the problem type (transient or static)
	 */

    problem.mode = SetAnalysisMode ( );

    switch (problem.mode) {

       case Transient:
          status = CheckAnalysisParameters (Transient);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&(problem.K), &(problem.M), &(problem.C));
          if (matrices)
             PrintGlobalMatrices (stdout, (problem.M), (problem.C), (problem.K));

          if (matlab) 
             MatlabGlobalMatrices (matlab, (problem.M), (problem.C), (problem.K));
 
          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);

          if (analysis.step > 0.0) {
             (problem.dtable) =
		 IntegrateHyperbolicDE (problem.K, problem.M, problem.C);
             (problem.ttable) = NullMatrix;
          }
          else
             (problem.dtable) =
	       RosenbrockHyperbolicDE (problem.K, problem.M, problem.C, &(problem.ttable));

          if ((problem.old_numbers) != NULL)
             RestoreNodeNumbers (problem.nodes, problem.old_numbers, problem.num_nodes);

          if ((problem.dtable) == NullMatrix)
             Fatal ("fatal error in integration (probably a singularity).");

          if (dotable)
             gnuplot ? WriteGnuplotTable( problem.dtable, problem.ttable, stdout) :
		 WriteTransientTable (problem.dtable, problem.ttable, stdout);
    
          if (doplot)
             PlotTransientTable (problem.dtable, problem.ttable, analysis.step, stdout);

          break;
       
       case TransientThermal:
          analysis.dofs [1] = Tx;
          analysis.numdofs = 1;
          status = CheckAnalysisParameters (TransientThermal);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&(problem.K), &(problem.M), &(problem.C));
          
          if (matrices) 
             PrintGlobalMatrices (stdout, (problem.M), NullMatrix, (problem.K));

          if (matlab) 
             MatlabGlobalMatrices (matlab, (problem.M), NullMatrix, (problem.K));
 
          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);

          (problem.dtable) = IntegrateParabolicDE ((problem.K), (problem.M));

          if ((problem.dtable) == NullMatrix)
             Fatal ("fatal error in integration (probably a singularity).");

          if ((problem.old_numbers) != NULL)
             RestoreNodeNumbers (problem.nodes, (problem.old_numbers), problem.num_nodes);

          if (dotable)
             WriteTransientTable ((problem.dtable), NullMatrix, stdout);
    
          if (doplot)
             PlotTransientTable ((problem.dtable), NullMatrix, analysis.step, stdout);

          break;

       case Static:

          (problem.K) = ConstructStiffness(&status);
          if (status)
             Fatal ("%d Fatal errors in element stiffness definitions", status);

          if (matrices)
             PrintGlobalMatrices (stdout, NullMatrix, NullMatrix, problem.K);

          if (matlab) 
             MatlabGlobalMatrices (matlab, NullMatrix, NullMatrix, problem.K);

          problem.F = ConstructForceVector ( );

          ZeroConstrainedDOF (problem.K, problem.F, &(problem.Kcond), &(problem.Fcond));

          problem.d = SolveForDisplacements (problem.Kcond, problem.Fcond);
          if ((problem.d) == NullMatrix)
             Fatal ("could not solve for global displacements");

          status = ElementStresses ( );
          if (status) 
             Fatal ("%d Fatal errors found computing element stresses", status);
    
          (problem.numreactions) =
	    SolveForReactions ((problem.K), (problem.d), (problem.old_numbers), &(problem.R));

          if ((problem.old_numbers) != NULL)
             RestoreNodeNumbers (problem.nodes, (problem.old_numbers), problem.num_nodes);

          WriteStructuralResults (stdout, (problem.title), (problem.R), (problem.numreactions));

          break;

       case StaticLoadCases:
       case StaticLoadRange:
          status = CheckAnalysisParameters ((problem.mode));
          if (status)
             Fatal ("%d errors found in analysis parameters.", status);

          (problem.K) = ConstructStiffness(&status);
          if (status)
             Fatal ("%d Fatal errors in element stiffness definitions", status);

          if (matrices)
             PrintGlobalMatrices (stdout, NullMatrix, NullMatrix, (problem.K));

          if (matlab) 
             MatlabGlobalMatrices (matlab, NullMatrix, NullMatrix, (problem.K));

          (problem.F) = ConstructForceVector ( );
          
          ZeroConstrainedDOF ((problem.K), (problem.F), &(problem.Kcond), &(problem.Fcond));
           
          if ((problem.mode) == StaticLoadCases)
             (problem.dtable) = SolveStaticLoadCases ((problem.Kcond), (problem.Fcond));
          else
             (problem.dtable) = SolveStaticLoadRange ((problem.Kcond), (problem.Fcond));

          if ((problem.dtable) == NullMatrix)
             Fatal ("could not solve for global displacements");
            
          if ((problem.old_numbers) != NULL)
             RestoreNodeNumbers (problem.nodes, (problem.old_numbers), problem.num_nodes);

          if (dotable)
             if ((problem.mode) == StaticLoadCases)
                WriteLoadCaseTable ((problem.dtable), stdout);
             else 
                WriteLoadRangeTable ((problem.dtable), stdout);

          if (doplot)
             if ((problem.mode) == StaticLoadCases)
                PlotLoadCaseTable ((problem.dtable), stdout);
             else 
                PlotLoadRangeTable ((problem.dtable), stdout);

          break; 

       case StaticSubstitutionLoadRange:
       case StaticIncrementalLoadRange:
          status = CheckAnalysisParameters (StaticSubstitution);
          status += CheckAnalysisParameters (StaticLoadRange);
          if (status) 
             Fatal ("%d errors found in analysis parameters.", status);

          (problem.K) = CreateNonlinearStiffness (&status);
          if (status)
             Fatal ("could not create global stiffness matrix");
         
          (problem.F) = ConstructForceVector ( );
 
          if ((problem.mode) == StaticSubstitutionLoadRange)
             (problem.dtable) = SolveNonlinearLoadRange ((problem.K), (problem.F), 0);
          else
             (problem.dtable) =
	       SolveNonlinearLoadRange ((problem.K), (problem.F), 0); /* should be 1*/
             
          if ((problem.dtable) == NullMatrix)
             Fatal ("did not converge on a solution");
         
          if ((problem.old_numbers) != NULL)
             RestoreNodeNumbers (problem.nodes, (problem.old_numbers), problem.num_nodes);
                
          if (dotable)
             WriteLoadRangeTable ((problem.dtable), stdout);

          if (doplot)
             PlotLoadRangeTable ((problem.dtable), stdout);

          break;

       case StaticSubstitution:
       case StaticIncremental:
          status = CheckAnalysisParameters ((problem.mode));
          if (status) 
             Fatal ("%d errors found in analysis parameters.", status);

          (problem.K) = CreateNonlinearStiffness (&status);
          if (status)
             Fatal ("could not create global stiffness matrix");
         
          (problem.F) = ConstructForceVector ( );
 
          if ((problem.mode) == StaticSubstitution)
             (problem.d) = StaticNonlinearDisplacements ((problem.K), (problem.F), 0);
          else
             (problem.d) =
	       StaticNonlinearDisplacements ((problem.K), (problem.F), 0); /* should be 1 */
             
          if ((problem.d) == NullMatrix)
             Fatal ("did not converge on a solution");
         
          if ((problem.old_numbers) != NULL)
             RestoreNodeNumbers (problem.nodes, (problem.old_numbers), problem.num_nodes);
                
          WriteStructuralResults (stdout, (problem.title), NULL, 0);

          break;

       case StaticThermal:

          (problem.K) = ConstructStiffness(&status);

          if (status)
             Fatal ("%d Fatal errors in element stiffness definitions", status);

          if (matrices)
             PrintGlobalMatrices (stdout, NullMatrix, NullMatrix, (problem.K));

          if (matlab) 
             MatlabGlobalMatrices (matlab, NullMatrix, NullMatrix, (problem.K));

          (problem.F) = ConstructForceVector ( );
          
          ZeroConstrainedDOF ((problem.K), (problem.F), &(problem.Kcond), &(problem.Fcond));
     
          (problem.d) = SolveForDisplacements ((problem.Kcond), (problem.Fcond));
          if ((problem.d) == NullVector)
             exit (1);

          if ((problem.old_numbers) != NULL)
             RestoreNodeNumbers (problem.nodes, (problem.old_numbers), problem.num_nodes);

          WriteTemperatureResults (stdout, (problem.title));

          break;

       case Modal:
          status = CheckAnalysisParameters (Modal);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&(problem.K), &(problem.M), &(problem.C));
          
          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);

          RemoveConstrainedDOF ((problem.K), (problem.M), (problem.C),
	      &(problem.Kcond), &(problem.Mcond), &(problem.Ccond));

          if (matrices)
             PrintGlobalMatrices (stdout, (problem.Mcond), (problem.Ccond), (problem.Kcond));
 
          if (matlab) 
             MatlabGlobalMatrices (matlab, (problem.Mcond), (problem.Ccond), (problem.Kcond));

          status =
	    ComputeEigenModes ((problem.Kcond), (problem.Mcond), &(problem.lambda), &(problem.x));

          if (status == M_NOTPOSITIVEDEFINITE)
             Fatal ("coefficient matrix is not positive definite.");
          else if (status)
             Fatal ("could not compute eigenmodes (report status code %d).", status);

          NormalizeByFirst ((problem.x), (problem.x));
          WriteEigenResults ((problem.lambda), (problem.x), (problem.title), stdout);

          if (doplot)
             PlotModeShapes ((problem.x), stdout);
            
          if (domodal) {
             FormModalMatrices ((problem.x), (problem.Mcond), (problem.Ccond), (problem.Kcond),
		 &(problem.Mm), &(problem.Cm), &(problem.Km), orthonormal);
             WriteModalResults (stdout, (problem.Mm), (problem.Cm), (problem.Km), (problem.lambda));
          }
           
          break;

       case Spectral:
          status = CheckAnalysisParameters (Spectral);

          if (status) 
             Fatal ("%d Errors found in analysis parameters.", status);

          status = ConstructDynamic (&(problem.K), &(problem.M), &(problem.C));

          if (status)
             Fatal ("%d fatal errors in stiffness and mass definitions",status);
 
          ZeroConstrainedDOF ((problem.K), NullMatrix, &(problem.Kcond), NULL);
          ZeroConstrainedDOF ((problem.M), NullMatrix, &(problem.Mcond), NULL);
          ZeroConstrainedDOF ((problem.C), NullMatrix, &(problem.Ccond), NULL);

          if (matrices)
             PrintGlobalMatrices (stdout, (problem.Mcond), (problem.Ccond), (problem.Kcond));

          if (matlab) 
             MatlabGlobalMatrices (matlab, (problem.Mcond), (problem.Ccond), (problem.Kcond));

          FindForcedDOF (&(problem.forced), &(problem.numforced));

          (problem.H) = ComputeTransferFunctions ((problem.Mcond), (problem.Ccond),
		       (problem.Kcond), (problem.forced), (problem.numforced));

          if (dospectra) {
             (problem.S) = ComputeOutputSpectra ((problem.H), (problem.forced), (problem.numforced));
             if ((problem.S) == NullMatrix)
                break; 
          }

          if ((problem.old_numbers) != NULL)
             RestoreNodeNumbers (problem.nodes, (problem.old_numbers), problem.num_nodes);

          if (!dospectra) {
             if (dotable)
                WriteTransferFunctions ((problem.H), (problem.forced), (problem.numforced), stdout);
             if (doplot)
                PlotTransferFunctions ((problem.H), (problem.forced), (problem.numforced), stdout);

             break;
          }

          if (dotable)  
             WriteOutputSpectra ((problem.S), stdout);
       
          if (doplot)
             PlotOutputSpectra ((problem.S), stdout);

          break;
    }

    if( matlab_all)
      WriteAllMatlab( matlab_all, &problem);

    if (summary)
       WriteMaterialStatistics (stdout);

    exit (0);
}

/* put this here for now so that we don't have to change too much */

void WriteGnuplotTable(dtable, ttable, fp)
   Matrix	dtable;
   Matrix	ttable;
   FILE		*fp;
{
   unsigned	i,j,k;
   static char *labels [] = {"","Tx","Ty","Tz","Rx","Ry","Rz"};

      fprintf (fp,"#      time");
      for (i = 1 ; i <= analysis.numnodes ; i++) {
         for (j = 1 ; j <= analysis.numdofs ; j++) {
            fprintf (fp,"        %s(%d)", labels[(int) analysis.dofs[j]],
                     analysis.nodes[i] -> number);
            
         }
      }
      fprintf (fp,"\n#------------------------------------------------------------------\n");

	/*
	 * output the displacement at this time step for each DOF 
	 *in this table
	 */

      for (i = 1 ; i <= MatrixRows (dtable) ; i++) {

	  /* print the current time */
         if (ttable == NullMatrix)
            fprintf (fp,"%11.5g",(i-1)*analysis.step);
         else
            fprintf (fp,"%11.5g",mdata(ttable,i,1));

         for (j = 1 ; j <= analysis.numnodes ; j++) {
            for (k = 1 ; k <= analysis.numdofs ; k++) {
               fprintf (fp, "  %11.5g",
                        MatrixData (dtable)[i][(j-1)*analysis.numdofs + k]);
            }
         }
         fprintf (fp,"\n");
      }
    
   fprintf (fp,"\n");

   return;
}
