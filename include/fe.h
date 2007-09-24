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
 * File:	fe.h							*
 *									*
 * Description:	This file contains the primary type and function	*
 *		declarations for the finite element package.		*
 ************************************************************************/

# ifndef _FE_H
# define _FE_H
# include "code.h"
# include "matrix.h"


# define TINY 1.0e-60
# define UnspecifiedValue (-99999999)

# ifndef M_PI
# define M_PI 3.14159265358979323846
# endif

# ifndef M_PI_2
#define M_PI_2          1.57079632679489661923  /* pi/2 */
# endif

# ifndef M_PI_4
#define M_PI_4          0.78539816339744830962  /* pi/4 */
# endif

/* Analysis types */

typedef enum {
    Static = 1,
    Transient = 2,
    Modal = 3,
    StaticThermal = 4,
    TransientThermal = 5,
    Spectral = 6,
    StaticSubstitution = 7,
    StaticIncremental = 8,
    StaticLoadCases = 9,
    StaticLoadRange = 10,
    StaticSubstitutionLoadRange = 11,
    StaticIncrementalLoadRange = 12,
} AnalysisType;


/* Element shapes */

typedef enum {
    Linear = 1,
    Planar = 2,
    Solid  = 3
} Shape;


/* Load directions */

typedef enum {
    LocalX        = 1,
    LocalY        = 2,
    LocalZ        = 3,
    GlobalX	  = 4,
    GlobalY	  = 5,
    GlobalZ 	  = 6,
    Parallel      = 7,
    Perpendicular = 8,
    Radial	  = 9,
    Axial	  = 10
} Direction;


/* Degrees of freedom */

typedef enum {
    Tx = 1, Fx = 1,			/* translation/force along x axis */
    Ty = 2, Fy = 2,			/* translation/force along y axis */
    Tz = 3, Fz = 3,			/* translation/force along z axis */
    Rx = 4, Mx = 4,			/* rotation/moment about x axis   */
    Ry = 5, My = 5,			/* rotation/moment about y axis   */
    Rz = 6, Mz = 6			/* rotation/moment about z axis   */
} DOF;


/* A node-magnitude pair */

typedef struct pair {
    unsigned node;			/* node number */
    double   magnitude;			/* magnitude   */
} Pair;


/* A reaction force */

typedef struct reaction {
    double   force;			/* reaction force             */
    unsigned node;			/* node number                */
    unsigned dof;			/* affected degree of freedom */
} *Reaction;


/* Element stress */

typedef struct stress {
    char   *aux;			/* auxiliary data pointer */
    double  x;				/* x coordinate		  */
    double  y;				/* y coordinate		  */
    double  z;				/* z coordinate		  */
    double *values;			/* computed stress values */
} *Stress;


/* Variable expression */

typedef struct {
    double value;		/* value at time zero */
    Code   expr;		/* expression code    */
    char  *text;		/* text of expression */
} VarExpr;


/* An element definition */

typedef struct definition {
    char    *name;		/* element name			      */
    int    (*setup) ( );	/* initialization function	      */
    int    (*stress) ( );	/* stress resultant function	      */
    Shape    shape;		/* element dimensional shape          */
    unsigned numnodes;		/* number of nodes in element         */
    unsigned shapenodes;	/* number of nodes which define shape */
    unsigned numstresses;	/* number of computed stress values   */
    unsigned numdofs;		/* number of degrees of freedom       */
    unsigned dofs [7];		/* degrees of freedom                 */
    unsigned retainK;		/* retain element K after assemblage  */
} *Definition;


/* A distributed load */

typedef struct distributed {
    char     *aux;			/* auxillary data pointer   	  */
    char     *name;			/* name of distributed load 	  */
    char     *color;			/* name of color to use in velvet */
    Direction direction;		/* direction of load        	  */
    unsigned  nvalues;			/* number of values         	  */
    Pair     *value;			/* nodes and magnitudes     	  */
} *Distributed;


/* A force */

typedef struct force {
    char   *aux;			/* auxillary data pointer 	  */
    char   *name;			/* name of force          	  */
    char   *color;			/* name of color to use in velvet */
    VarExpr force [7];			/* force vector			  */
    VarExpr spectrum [7];		/* input spectra		  */
} *Force;


/* A constraint (boundary and initial conditions) */

typedef struct constraint {
    char   *aux;			/* auxillary data pointer 	  */
    char   *name;			/* name of constraint     	  */
    char   *color;			/* name of color to use in velvet */
    char    constraint [7];		/* constraint vector     	  */
    double  ix [7];			/* initial displacement vector    */
    double  vx [4];			/* initial velocity vector	  */
    double  ax [4];			/* initial acceleration vector    */
    VarExpr dx [7];			/* boundary displacement vector	  */
} *Constraint;


/* A material */

typedef struct material {
    char  *aux;				/* auxillary data pointer           */
    char  *name;			/* name of material                 */
    char  *color;			/* name of color to use in velvet   */
    double E;				/* Young's modulus                  */
    double Ix;				/* moment of inertia about x-x axis */
    double Iy;				/* moment of inertia about y-y axis */
    double Iz;				/* moment of inertia about z-z axis */
    double A;				/* cross-sectional area             */
    double J;				/* polar moment of inertia          */
    double G;				/* bulk (shear) modulus             */
    double t;				/* thickness                        */
    double rho;				/* density                          */
    double nu;				/* Poisson's ratio                  */
    double kappa;			/* shear force correction           */
    double Rk;				/* Rayleigh damping coefficient (K) */
    double Rm;				/* Rayleigh damping coefficient (M) */
    double Kx;				/* conductivity in x direction      */
    double Ky;				/* conductivity in y direction      */
    double Kz;				/* conductivity in z direction      */
    double c;				/* heat capacity		    */
} *Material;


/* A node */

typedef struct node {
    char      *aux;			/* auxillary data pointer         */
    unsigned   number;			/* node number                    */
    Constraint constraint;		/* constrained degrees of freedom */
    Force      force;			/* force acting on node           */
    double     m;			/* lumped mass			  */
    double    *eq_force;		/* equivalent force               */
    double     dx [7];			/* displacement                   */
    double     x;			/* x coordinate                   */
    double     y;			/* y coordinate                   */
    double     z;			/* z coordinate                   */
    double    *stress;                  /* nodally averaged stress vector */
    int        numelts;			/* num of elts that use this node */
} *Node;


/* An element */

typedef struct element {
    char       *aux;			/* auxillary data pointer 	*/
    unsigned    number;			/* element number         	*/
    Node       *node;			/* array of nodes         	*/
    Matrix      K;			/* stiffness matrix       	*/
    Matrix	M;			/* mass matrix			*/
    Matrix	f;			/* element residual force	*/
    Material    material;		/* material               	*/
    Definition  definition;		/* definition of element        */
    Distributed distributed [4];	/* distributed loads      	*/
    unsigned    numdistributed;		/* number of loads        	*/
    Stress     *stress;			/* element stresses             */
    unsigned    ninteg;			/* number of integration points */
} *Element;


/* A nodal DOF */

typedef struct nodeDOF {
    DOF		dof;
    Node	node;
} *NodeDOF;


typedef struct casepair {
   unsigned	 noe;		/* node or element number */
   char		*fol;		/* force or load name     */
} CasePair;


/* A Load Case */

typedef struct loadcase {
    char	*name;
    unsigned	 numforces;
    unsigned	 numloads;
    Node	*nodes;
    Element	*elements;
    Force	*forces;
    Distributed *loads;
} *LoadCase;


/* The different analyses definitions */

typedef struct analysis {
    double	start;			/* time/freq start point	*/
    double	step;			/* time/freq step delta		*/
    double	stop;			/* time/freq stop point		*/
    double	gamma;			/* parameter in Newmark integr.	*/
    double	beta;			/* parameter in Newmark integr.	*/
    double	alpha;			/* parameter in H-H-T-alpha 	*/
    double	Rk;			/* global Rayleigh K damping    */
    double	Rm;			/* global Rayleigh M damping    */
    double      gravity [4];		/* gravitational accel vector	*/
    double      tolerance;		/* convergence control factor   */
    double	relaxation;		/* iterative relaxation factor  */
    unsigned	iterations;		/* iteration count control      */
    unsigned	load_steps;		/* number of incremental steps  */
    char	mass_mode;		/* 'c'onsistent or 'l'umped	*/
    Node       *nodes;			/* list of nodes of interest    */
    unsigned	numnodes;		/* number of nodes of interest  */
    char	dofs [7];		/* dofs of interest		*/
    unsigned	numdofs;		/* number of dofs of interest	*/
    unsigned	input_dof;		/* input DOF # for range loads  */
    Node  	input_node;		/* input node # for range loads */
} Analysis;


	/*
	 * prototypes of routines in fe.c
	 */

extern int      FindDOFS		  PROTO (( ));
extern Matrix   ConstructStiffness	  PROTO ((int *));
extern void     ZeroConstrainedDOF	  PROTO ((Matrix, Vector, 
                                                  Matrix *, Vector *));
extern void	RemoveConstrainedDOF	  PROTO	((Matrix, Matrix, Matrix, 
                                                  Matrix *, Matrix *, Matrix *));
extern Vector   ZeroCompactRowCol	  PROTO ((Vector, unsigned));
extern void     AdjustForceVector	  PROTO ((Vector, Matrix, unsigned, double));
extern unsigned SolveForReactions	  PROTO ((Matrix, Vector, 
						  unsigned *, Reaction **));
extern Vector   ConstructForceVector	  PROTO (( ));
extern Matrix   SolveStaticLoadCases	  PROTO ((Matrix, Matrix));
extern Matrix   SolveStaticLoadRange 	  PROTO ((Matrix, Matrix));
extern void	AssembleLoadCaseForce	  PROTO ((Matrix, LoadCase));
extern Vector   SolveForDisplacements	  PROTO ((Matrix, Vector));
extern int      FactorStiffnessMatrix	  PROTO ((Matrix));
extern void     ApplyNodalDisplacements   PROTO ((Matrix));
extern int      ElementSetup		  PROTO ((Element, char));
extern void     ClearNodes		  PROTO (( ));
extern int      ElementStresses		  PROTO (( ));
extern int      GlobalDOF                 PROTO ((unsigned, unsigned));
extern void     LocalDOF                  PROTO ((unsigned, unsigned *,
                                                  unsigned *));
extern int      ZeroConstrainedMatrixDOF  PROTO ((Matrix, Matrix));
extern Matrix   RemoveConstrainedMatrixDOF PROTO ((Matrix));
extern void	FindForcedDOF		  PROTO ((NodeDOF **, unsigned *));
extern int	CheckAnalysisParameters   PROTO ((AnalysisType));

	/*
	 * routines for automatic node renumbering in renumber.c
	 */

extern unsigned *RenumberNodes		  PROTO ((Node *, Element *, unsigned,
						  unsigned));
extern void     RestoreNodeNumbers	  PROTO ((Node *, unsigned *,
						  unsigned));

	/*
	 * prototypes of routines in transient.c
	 */

extern int	ConstructDynamic	  PROTO ((Matrix *, Matrix *, Matrix *));
extern void	AssembleTransientForce	  PROTO ((double, Vector));
extern int     *BuildConstraintMask	  PROTO (( ));
extern int	BuildHyperbolicIC	  PROTO ((Vector, Vector, Vector));
extern void	BuildParabolicIC	  PROTO ((Vector));
extern Matrix	IntegrateHyperbolicDE	  PROTO ((Vector, Vector, Vector));
extern Matrix	RosenbrockHyperbolicDE	  PROTO ((Vector, Vector, Vector, Matrix *));
extern Matrix	IntegrateParabolicDE	  PROTO ((Vector, Vector));
extern void	ResolveBC 		  PROTO ((double, Vector, Vector));

	/*
	 * routines in modal.c
	 */

extern int	ComputeEigenModes	  PROTO ((Matrix, Matrix, 
						  Matrix *, Matrix *));
extern Matrix	ModalNodalDisplacements	  PROTO ((Matrix));
extern int	FormModalMatrices	  PROTO ((Matrix, Matrix, Matrix, Matrix,
                                                  Matrix *, Matrix *, Matrix *,
                                                  int));

	/*
	 * routines in spectral.c
	 */

extern int 	FastFourierTransform 	   PROTO ((double *, double *, 
                                                   int, int, int));
extern int 	Spectrum 		   PROTO ((Vector, Vector *, Vector *, 
                                                   double, int));
extern int	ComputeOutputSpectraFFT	   PROTO ((Matrix, Matrix *, 
                                                   Vector *, int));
extern Matrix	*ComputeTransferFunctions  PROTO ((Matrix, Matrix, Matrix,
                                                   NodeDOF *, unsigned));
extern Matrix	ComputeOutputSpectra	   PROTO ((Matrix *, NodeDOF *,
                                                   unsigned));

	/*
	 * routines in nonlinear.c
	 */

extern Matrix 	CreateNonlinearStiffness     PROTO ((int *));
extern int	AssembleCurrentState         PROTO ((Matrix, Matrix, int));
extern int	AssembleCurrentForce	     PROTO ((Matrix, Matrix));
extern int      RestoreCoordinates           PROTO ((Matrix));
extern int      UpdateCoordinates            PROTO ((Matrix));
extern Matrix   StaticNonlinearDisplacements PROTO ((Matrix, Matrix, int));
extern Matrix   SolveNonlinearLoadRange      PROTO ((Matrix, Matrix, int));

# endif /* _FE_H */
