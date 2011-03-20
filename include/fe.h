
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

# include <boost/shared_ptr.hpp>
# include <string>
# include "cvector1.hpp"
# include "code.h"
# include "matrix.h"
# include "inptypes.h"

/*----------------------------------------------------------------------*/

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


/* Element shapes */

typedef enum {
    Linear = 1,
    Planar = 2,
    Solid  = 3
} Shape;


/* Element stress */

typedef struct stress {
    char   *aux;			/* auxiliary data pointer */
    double  x;				/* x coordinate		  */
    double  y;				/* y coordinate		  */
    double  z;				/* z coordinate		  */
    cvector1<double> values;			/* computed stress values */
} *Stress;


/* Variable expression */

typedef struct {
    double value;		/* value at time zero */
    Code   expr;		/* expression code    */
    char  *text;		/* text of expression */
} VarExpr;

struct element_t;
typedef boost::shared_ptr<element_t> Element;

/* An element definition */

struct definition_t {
    definition_t(const char*name = NULL);
    ~definition_t();
    std::string name;		/* element name			      */
    int    (*setup) (Element, char, int);	/* initialization function	      */
    int    (*stress) (Element);	/* stress resultant function	      */
    Shape    shape;		/* element dimensional shape          */
    unsigned numnodes;		/* number of nodes in element         */
    unsigned shapenodes;	/* number of nodes which define shape */
    unsigned numstresses;	/* number of computed stress values   */
    unsigned numdofs;		/* number of degrees of freedom       */
    unsigned dofs [7];		/* degrees of freedom                 */
    unsigned retainK;		/* retain element K after assemblage  */
    void *udata;            /* user data pointer */
};

typedef boost::shared_ptr<definition_t> Definition;

struct LtDefinition
{
     bool operator()(const Definition a, const Definition b) const
          {
               return a->name < b->name;
          }
};

/* A distributed load */

struct distributed_t {
    distributed_t(const char* name = NULL, unsigned nvalues = 0);
    ~distributed_t();
    char     *aux;			/* auxillary data pointer   	  */
    std::string name;			/* name of distributed load 	  */
    std::string color;			/* name of color to use in velvet */
    Direction direction;		/* direction of load        	  */
    cvector1<Pair> value;			/* nodes and magnitudes     	  */
};

typedef boost::shared_ptr<distributed_t> Distributed;

struct LtDistributed
{
     bool operator()(const Distributed a, const Distributed b) const
          {
               return a->name < b->name;
          }
};


/* A force */

struct force_t {
    force_t(const char* name = NULL);
    ~force_t();
    char   *aux;			/* auxillary data pointer 	  */
    std::string name;			/* name of force          	  */
    std::string color;			/* name of color to use in velvet */
    VarExpr force [7];			/* force vector			  */
    VarExpr spectrum [7];		/* input spectra		  */
};

typedef boost::shared_ptr<force_t> Force;

struct LtForce
{
     bool operator()(const Force a, const Force b) const
          {
               return a->name < b->name;
          }
};


/* A constraint (boundary and initial conditions) */

struct constraint_t {
    constraint_t(const char* name = NULL);
    ~constraint_t();
    char   *aux;			/* auxillary data pointer 	  */
    std::string name;		/* name of constraint     	  */
    std::string color;			/* name of color to use in velvet */
    char    constraint [7];		/* constraint vector     	  */
    double  ix [7];			/* initial displacement vector    */
    double  vx [4];			/* initial velocity vector	  */
    double  ax [4];			/* initial acceleration vector    */
    VarExpr dx [7];			/* boundary displacement vector	  */
};

typedef boost::shared_ptr<constraint_t> Constraint;

struct LtConstraint
{
     bool operator()(const Constraint a, const Constraint b) const
          {
               return a->name < b->name;
          }
};


/* A material */

struct material_t {
    material_t(const char *name = NULL);
    ~material_t();
    char  *aux;				/* auxillary data pointer           */
    std::string name;		/* name of material                 */
    std::string color;			/* name of color to use in velvet   */
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
};

typedef boost::shared_ptr<material_t> Material;

struct LtMaterial
{
     bool operator()(const Material a, const Material b) const
          {
               return a->name < b->name;
          }
};


/* A node */

struct node_t {
    node_t(unsigned number = 0);
    ~node_t();
    char      *aux;			/* auxillary data pointer         */
    unsigned   number;			/* node number                    */
    Constraint constraint;		/* constrained degrees of freedom */
    Force      force;			/* force acting on node           */
    double     m;			/* lumped mass			  */
    cvector1<double> eq_force;		/* equivalent force               */
    double     dx [7];			/* displacement                   */
    double     x;			/* x coordinate                   */
    double     y;			/* y coordinate                   */
    double     z;			/* z coordinate                   */
    cvector1<double> stress;                  /* nodally averaged stress vector */
    int        numelts;			/* num of elts that use this node */
};

typedef boost::shared_ptr<node_t> Node;

struct LtNode
{
     bool operator()(const Node a, const Node b) const
          {
               return a->number < b->number;
          }
};

/* An element */

struct element_t {
    element_t(unsigned number = 0, Definition defn = Definition());
    ~element_t();
    char       *aux;			/* auxillary data pointer 	*/
    unsigned    number;			/* element number         	*/
    cvector1<Node> node;			/* array of nodes         	*/
    Matrix      K;			/* stiffness matrix       	*/
    Matrix	M;			/* mass matrix			*/
    Matrix	f;			/* element residual force	*/
    Material    material;		/* material               	*/
    Definition  definition;		/* definition of element        */
    Distributed distributed [4];	/* distributed loads      	*/
    unsigned    numdistributed;		/* number of loads        	*/
    cvector1<Stress> stress;			/* element stresses             */
    unsigned    ninteg;			/* number of integration points */
};

struct LtElement
{
     bool operator()(const Element a, const Element b) const
          {
               return a->number < b->number;
          }
};


/* A nodal DOF */

struct NodeDOF {
    DOF		dof;
    Node	node;
};


/* A Load Case */

struct loadcase_t {
     loadcase_t(const char *name = NULL);
     ~loadcase_t();
     std::string name;
     cvector1<Node> nodes;
     cvector1<Element> elements;
     cvector1<Force> forces;
     cvector1<Distributed> loads;
};

typedef boost::shared_ptr<loadcase_t> LoadCase;

struct LtLoadCase
{
     bool operator()(const LoadCase a, const LoadCase b) const
          {
               return a->name < b->name;
          }
};


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
    cvector1<Node> nodes;			/* list of nodes of interest    */
    char	dofs [7];		/* dofs of interest		*/
    unsigned	numdofs;		/* number of dofs of interest	*/
    unsigned	input_dof;		/* input DOF # for range loads  */
    Node  	input_node;		/* input node # for range loads */
} Analysis;


	/*
	 * prototypes of routines in fe.c
	 */

/*!
  FindDOFS will search through all of the elements for a problem and
  determine which DOFs (out of the six that are physically possible)
  must be considered in this problem based on the different element
  types.  The list of affected DOFs is built and ...
*/
int FindDOFS(void);

/*!
  For a given set of elements (possibly of varying types) this will
  assemble all element stiffness matrices into the global stiffness
  matrix according to what DOFs each individual element affects (a
  function of both its node numbers and the DOFs that it affects and
  their relation to the global DOFs as indexed by dofs).
  
  Before we do anything we figure out how the compact column storage
  scheme is going to work, i.e., we need to set up the height and diag
  arrays which contain information needed to go from a standard
  row,column notation (which I think is easier to read) to an address
  into the compact column vector representation of the global stiffness
  matrix
*/ 
Matrix ConstructStiffness(int *status);

/*!
  For a fixed BC at a given DOF all we'll do is zero out the rows and
  columns of the stiffness matrix associated with that DOF (with a one
  on the diagonal for stability).  For a displacement BC, we'll need
  to adjust the force vector, also, we should put the displacement
  into the force vector so when we go to solve, we'll just go ahead
  and get that displacement right back.  We don't deal with hinges
  here really because we already dealt with them when we had the
  element stiffness matrix laying around.  All we'll do here is make
  sure that the displacement will come out zero.
*/
void ZeroConstrainedDOF(const Vector &K, const Vector &F, Vector *Kc, Vector *Fc);

/*!
  As opposed to simply zeroing out the rows and columns associated
  with a constrained DOF, here we actually reduce the size of the
  stiffness and mass matrices by removing those rows and columns
  entirely.
*/
void RemoveConstrainedDOF(Matrix K, Matrix M, Matrix C, 
                          Matrix *Kcond, Matrix *Mcond, Matrix *Ccond);

/*!
  Zeros out the row and column given by dof.  Places a one on the
  diagonal.
*/
Vector ZeroCompactRowCol(Vector K, unsigned int dof);

/*!
  Given a displacement boundary condition, we can't just knock out the
  rows and columns of K for that DOF.  We need to adjust the force
  vector by adding the effect of the stiffness in that DOF times the
  given displacement to the external force vector.  The simplest way
  to do that is to just loop over all DOFs and adjust them as
  appropriate.  In some cases the adjustment is unnecessary because
  the displaced DOF may not affect all DOFs or a given DOF may be
  fixed anyways.  It won't hurt to operate in these cases anyways
  though, so that's what I do.
*/
void AdjustForceVector(Vector Fcond, Vector Kcond, unsigned int affected_dof, double dx);

/*
  Constructs the global nodal force vector based on all nodal forces
  and the global DOFs active at those nodes.  Global DOF determination
  is by node number and the and the relationship between the force and
  its actual physical DOF and the location of this DOF in problem
  space.
*/
Vector ConstructForceVector(void);

/*!
  Builds a table of nodal DOF displacements for all defined loadcases.
*/
Matrix SolveStaticLoadCases(Matrix K, Matrix Fbase);

/*!
 Builds a table of nodal DOF displacements for input forcing at a
 single DOF over a range of force magnitudes.
*/
Matrix SolveStaticLoadRange(Matrix K, Matrix Fbase);

void AssembleLoadCaseForce(Matrix F, LoadCase lc);

/*!
 Solves the linear system Kd=F for the vector of global nodal
 displacements.  The system must not be singular (i.e. K and F should
 be condensed).
*/
Vector SolveForDisplacements(Vector &K, Vector &F);

/*!
  Factorizes the problem stiffness matrix in place.
*/
int FactorStiffnessMatrix(Vector &K);

void ApplyNodalDisplacements(Matrix d);

/*!
  Calls the appropriate function to assemble the element stiffness
  matrix for an element.  Each element stiffness function should be of
  the form: xxxSetup(element,mass_mode) where x is the element type
  (as defined in element.h) element is the element to assemble the
  stiffness for and mass_mode is 0 in static cases or 'c' or 'l' in
  transient analysis when a mass matrix should be formed.
 */
int ElementSetup(Element element, char mass_mode);

/*!
 Sets all the displacements on the nodes to zero and clears the
 equivalent force vector.
*/
void ClearNodes(void);

/*!
  Calls the element stress functions for all of the elements.
*/
int ElementStresses(void);

/*!
  Calculates the global DOF number based on a given local DOF (Tx
  ... Rz), a node number and a dofs map.  Zero is returned if the
  given local DOF is not active in the current dofs map.
*/
int GlobalDOF(unsigned int node, unsigned int dx);

/*!
  Finds the node and local DOF of a given global DOF.
*/
void LocalDOF(unsigned int global_dof, unsigned int *node, unsigned int *local_dof);

/*!
 Sort of like ZeroConstrainedDOF only simpler and more general because
 it only works on one thing at a time.
*/
int ZeroConstrainedMatrixDOF(Matrix &b, const Matrix &a);

/*
  A generalized form of RemoveConstrainedDOF for a single matrix.  If
  a matrix is input, it needs to be in compact column format.
*/
Matrix RemoveConstrainedMatrixDOF(Matrix a);

/*!
 Verifies that everything in the analysis parameters section is set
 (or at least the minimum number of things that we need) for the given
 analysis type.
*/
int CheckAnalysisParameters(AnalysisType mode);

	/*
	 * routines in modal.c
	 */

int ComputeEigenModes(Matrix K, Matrix M, Matrix *lambda_r, Matrix *x_r);

/*!
  Given a table of mode shapes and a list of nodes and active dofs,
  put together a table of nodal displacements at each node (including
  constrained nodes) for translational DOFs in each separate mode of
  vibration.  The resulting table should be semi-analogous to the
  displacement table in transient analysis.
*/
Matrix ModalNodalDisplacements(Matrix x);

int FormModalMatrices(Matrix u, Matrix m, Matrix c, Matrix k, 
                      Matrix *Mr, Matrix *Cr, Matrix *Kr, 
                      int ortho);

	/*
	 * routines in spectral.c
	 */


int FastFourierTransform(double *Xr, double *Xi, int n, int n2, int direction);

int Spectrum(Vector x, Vector *P, Vector *F, double delta_t, int nfft);



/*!
 Computes the spectrum for each DOF in the time series results matrix,
 dtable.  This isn't very efficient because we end up creating and
 destroying the PSD and frequency vectors for each DOF that we do, but
 oh well, we probably won't be doing this all that much (i.e., just at
 the end of a run and only for a few selected DOFs).
*/
int ComputeOutputSpectraFFT(Matrix dtable, Matrix *Pr, Vector *Fr, int nfft);

	/*
	 * routines in nonlinear.c
	 */

/*!
  For a given set of elements (possibly of varying types) this will
  create compact column storage for the global stiffness matrix in a
  nonlinear problem.  We can't really base this one simply on the
  non-zero entries in the element stiffness matrices, because as the
  element stiffness matrices change with geometry we're not sure where
  the zeros might be.  Instead we make sure that we have at least
  enough room for all possible element DOF.
*/
Matrix CreateNonlinearStiffness(int *status);

int AssembleCurrentState(Matrix K, Matrix F, int tangent);

int AssembleCurrentForce(Matrix F, Matrix Fnodal);

int RestoreCoordinates(Matrix d);

int UpdateCoordinates(Matrix d);

/*!
  Solves a geometrically nonlinear, large deformation problem by
  iteratively solving Kd = F where K is updated at every iteration
  according to the displacements from the previous iteration - the
  value of tangent determines what kind of stiffness matrix is formed
  (simple linear or nonlinear tangent) and how the force residuals are
  calculated.
*/
Matrix StaticNonlinearDisplacements(Matrix K, Matrix Fnodal, int tangent);

Matrix SolveNonlinearLoadRange(Matrix K, Matrix Fnodal, int tangent);

/*----------------------------------------------------------------------*/

# endif /* _FE_H */
