%{
/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson

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
 * File:	parser.y						*
 *									*
 * Description:	This file contains the yacc specification for the	*
 *		parser for the FElt 2.0 system.				*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include "code.h"
# include "error.h"
# include "objects.h"
# include "problem.h"
# include "fe.h"
# include "allocate.h"

# if !defined (__GNUC__) && !defined (__sparc__)
# define alloca malloc		/* prevents alloca from being called */
# endif

# define VariableExpression 2	/* not 0, 1, or, 'h' */

extern "C" void yyerror (const char *msg);
extern "C" int  yylex  (void);

/* Last parameters (default for some parameters is to inherit the last). */

static double		  last_x;		/* last x coordinate	   */
static double		  last_y;		/* last y coordinate	   */
static double		  last_z;		/* last z coordinate	   */
static char		 *last_constraint;	/* name of last constraint */
static char		 *last_material;	/* name of last material   */



/* Current objects (inherited attributes). */

static Item		  found;		/* current object	   */
static Node		  node;			/* current node		   */
static Element		  element;		/* current element	   */
static Material		  material;		/* current material	   */
static Distributed	  load;			/* current load		   */
static Force		  force;		/* current force	   */
static Constraint	  constraint;		/* current constraint	   */
static Definition	  definition;		/* current definition	   */
static LoadCase		  loadcase;		/* current loadcase	   */


/* Dummy strucutures.  If an object is defined twice, the second definition
   is illegal and the current object is set to a dummy object. */

static struct node	  dummy_node;		/* dummy node		   */
static struct element	  dummy_element;	/* dummy element	   */
static struct material	  dummy_material;	/* dummy material	   */
static struct distributed dummy_load;		/* dummy distributed load  */
static struct force	  dummy_force;		/* dummy force		   */
static struct constraint  dummy_constraint;	/* dummy constraint	   */
static struct loadcase    dummy_loadcase;	/* dummy loadcase	   */


/* Temporary arrays. */

static int		  int_array [1024];	/* temporary integer array */
static int		 *int_ptr;		/* pointer into array	   */
static Pair		  pair_array [1024];	/* temporary pair array	   */
static Pair		 *pair_ptr;		/* pointer into array	   */
static CasePair		  case_array [1024];	/* temporary case pair array */
static CasePair		 *case_ptr;		/* pointer into case array */


/* Temporary variables for gravity triplet */

static double		  triple_x;		
static double		  triple_y;
static double		  triple_z;


/* Discrete expression variables. */

static int		  table_error = 0;	/* error indicator	   */
static double		  last_time = 0;	/* last time coordinate	   */
static double		 *table = NULL;		/* table of values	   */
static unsigned		  table_count = 0;	/* count of values	   */
static unsigned		  table_size = 0;	/* size of table	   */


/* Figure list variables. */

static float		  figure_x;		/* current x-coordinate	   */
static float		  figure_y;		/* current y-coordinate	   */
static unsigned		  figure_size;		/* size of figure list	   */
static unsigned		  fig_point_size;	/* size of point list	   */
static FigInfo		 *figure;		/* current figure	   */
%}

%union {
    int       i;
    double    d;
    char     *s;
    Pair      p;
    CasePair  cp;
    char      c;
}

%right	'?' ':'
%left	OR
%left	AND
%left	'|'
%left	'^'
%left	'&'
%left	EQUALS NEQUAL
%left	'<' '>' LT_EQ GT_EQ
%left	LSHIFT RSHIFT
%left	'+' '-'
%left	'*' '/' '%'
%right	UNARY '!' '~'


%token	NAME INTEGER DOUBLE BOOLEAN TIME

%token	SIN COS TAN POW EXP LOG LOG10 SQRT HYPOT FLOOR CEIL FMOD FABS

%token	ANALYSIS_TYPE DIRECTION CONSTRAINT HINGED NODE_DOF MASS_MODE

%token	PROBLEM ANALYSIS LOAD_CASES END
%token  NODES ELEMENTS MATERIALS LOADS FORCES CONSTRAINTS

%token	TITLE_EQ NODES_EQ ELEMENTS_EQ ANALYSIS_EQ

%token	X_EQ Y_EQ Z_EQ FORCE_EQ CONSTRAINT_EQ MASS_EQ

%token	LOAD_EQ MATERIAL_EQ

%token	E_EQ IX_EQ IY_EQ IZ_EQ A_EQ J_EQ G_EQ T_EQ RHO_EQ NU_EQ KAPPA_EQ RK_EQ
%token	RM_EQ KX_EQ KY_EQ KZ_EQ C_EQ

%token	DIRECTION_EQ VALUES_EQ

%token	FX_EQ FY_EQ FZ_EQ MX_EQ MY_EQ MZ_EQ
%token	SFX_EQ SFY_EQ SFZ_EQ SMX_EQ SMY_EQ SMZ_EQ

%token	TX_EQ TY_EQ TZ_EQ RX_EQ RY_EQ RZ_EQ 
%token  ITX_EQ ITY_EQ ITZ_EQ IRX_EQ IRY_EQ IRZ_EQ 
%token  VX_EQ VY_EQ VZ_EQ AX_EQ AY_EQ AZ_EQ

%token	ALPHA_EQ BETA_EQ GAMMA_EQ DOFS_EQ MASS_MODE_EQ
%token	START_EQ STOP_EQ STEP_EQ GRAVITY_EQ
%token  ITERATIONS_EQ TOLERANCE_EQ LOAD_STEPS_EQ RELAXATION_EQ
%token  INPUT_RANGE_EQ INPUT_DOF_EQ INPUT_NODE_EQ

%token  NODE_FORCES_EQ ELEMENT_LOADS_EQ

%token	CANVAS FIGURES NODE_NUM_EQ ELT_NUM_EQ SNAP_EQ GRID_EQ SNAP_SIZE_EQ
%token	GRID_SIZE_EQ X_MIN_EQ X_MAX_EQ Y_MIN_EQ Y_MAX_EQ SCALE_EQ X_POS_EQ
%token	Y_POS_EQ WIDTH_EQ HEIGHT_EQ NODE_COLOR_EQ ELT_COLOR_EQ LABEL_FONT_EQ
%token	TOOL_COLOR_EQ TOOL_FONT_EQ FONT_EQ COLOR_EQ LENGTH_EQ
%token	TEXT_EQ POINTS_EQ FIGURE_TYPE

%type	<i> INTEGER BOOLEAN ANALYSIS_TYPE DIRECTION CONSTRAINT HINGED NODE_DOF
%type	<i> MASS_MODE FIGURE_TYPE
%type	<p> value_pair 
%type   <cp> loadcase_pair
%type   <c> translation rotation 
%type	<d> DOUBLE constant_expression opt_z_coordinate
%type	<s> NAME ELEMENTS
%type	<i> element_node expression function or_action and_action if_action
	    else_action node_number_expression element_number_expression
%%

specification
	: initialize problem_description section_list END
	| X_EQ variable_expression END
	;


initialize
	: /* empty */
	    {
		last_x = 0;
		last_y = 0;
		last_z = 0;
		last_material = NULL;
		last_constraint = NULL;
	    }
	;


/* Problem description */

problem_description
	: PROBLEM problem_parameter_list
	| /* empty */
	;


problem_parameter_list
	: problem_parameter_list problem_parameter
	| /* empty */
	;


problem_parameter
	: TITLE_EQ NAME
	    {
		Deallocate (problem.title);
		problem.title = $2;
	    }

	| NODES_EQ INTEGER
	    {
             problem.nodes.resize($2);
	    }

	| ELEMENTS_EQ INTEGER
	    {
             problem.elements.resize($2);
	    }

	| ANALYSIS_EQ ANALYSIS_TYPE
	    {
		problem.mode = (AnalysisType) $2;
	    }

	| error
	;


/* Sections */

section_list
	: section_list section
	| /* empty */
	;


section
	: node_section
	| element_section
	| material_section
	| load_section
	| force_section
	| constraint_section
	| analysis_section
        | loadcase_section
	| canvas_section
	| figure_section
	| END
	;


/* Node section */

node_section
	: NODES node_definition_list
	;


node_definition_list
	: node_definition_list node_definition
	| /* empty */
	;


node_definition
	: node_number node_parameter_list
	;


node_number
	: node_number_expression
	    {
             if ($1 < 1 || $1 > problem.nodes.size()) {
                  error ("node number %u is illegal", $1);
                  node = &dummy_node;
                  break;
             }
             
		node = CreateNode ($1);
		found = TreeInsert (problem.node_tree, node);

		if (found != (Item) node) {
		    error ("node number %u is repeated", $1);
		    DestroyNode (node);
		    node = &dummy_node;
		    break;
		}

		node -> x = last_x;
		node -> y = last_y;
		node -> z = last_z;
		node -> constraint = last_constraint ?
			(Constraint) strdup (last_constraint) : NULL;
	    }
	;


node_number_expression
	: INTEGER
	| '(' constant_expression ')'
	    {$$ = $2;}
	;


node_parameter_list
	: node_parameter_list node_parameter
	| /* empty */
	;


node_parameter
	: X_EQ constant_expression
	    {
		node -> x = last_x = $2;
	    }

	| Y_EQ constant_expression
	    {
		node -> y = last_y = $2;
	    }

	| Z_EQ constant_expression
	    {
		node -> z = last_z = $2;
	    }

        | MASS_EQ constant_expression
            {
                node -> m = $2;
            }

	| FORCE_EQ NAME
	    {
		Deallocate (node -> force);
		node -> force = (Force) $2;
	    }

	| CONSTRAINT_EQ NAME
	    {
		node -> constraint = (Constraint) (last_constraint = $2);
	    }

	| error
	;


/* Element section */

element_section
	: element_header element_definition_list
	;


element_header
	: ELEMENTS
	    {
		definition = defnlookup ($1);
		Deallocate ($1);
		if (!definition)
		    return 1;
	    }
	;


element_definition_list
	: element_definition_list element_definition
	| /* empty */
	;


element_definition
	: element_number element_parameter_list
	;


element_number
	: element_number_expression
	    {
		if ($1 < 1 || $1 > problem.elements.size()) {
		    error ("element number %u is illegal", $1);
		    element = &dummy_element;
		    break;
		}

		element = CreateElement ($1, definition);

        if (!problem.element_set.insert(element).second) {
             error ("element number %u is repeated", $1);
             DestroyElement (element);
             element = &dummy_element;
             break;
        } 

        if (!element->material)
             element -> material = new struct material;
		element -> material->name = last_material ? last_material : ""; 
	    }
	;


element_number_expression
	: INTEGER
	| '(' constant_expression ')'
	    {$$ = $2;}
	;


element_parameter_list
	: element_parameter_list element_parameter
	| /* empty */
	;


element_parameter
	: NODES_EQ '[' element_node_list ']'
	    {
		unsigned i;
		unsigned size;
		unsigned number;


		if (element == &dummy_element)
		    break;

		size = int_ptr - int_array;

		if (size != element -> definition -> numnodes) {
		    number = element -> number;
		    error ("incorrect number of nodes for element %u", number);
		    break;
		}

		for (i = 1; i <= size; i ++)
		    element -> node [i] = (Node) int_array [i - 1];
	    }

	| MATERIAL_EQ NAME
	    {
             last_material = $2;
             if (!element->material)
                  element->material = new struct material;
             element -> material -> name = last_material;
	    }

	| LOAD_EQ element_load_list

	| error
	;


element_node_list
	: element_node_list ',' element_node
	    {
		*int_ptr ++ = $3;
	    }

	| element_node_list element_node
	    {
		*int_ptr ++ = $2;
	    }

	| element_node
	    {
		int_ptr = int_array;
		*int_ptr ++ = $1;
	    }
	;


element_node
	: node_number_expression
	    {
             if ($1 > problem.nodes.size())
                  error ("node number %u is illegal", $1);
	    }
	;


element_load_list
	: element_load_list ',' NAME
	    {
		if (element -> numdistributed == 3) {
		    error ("element %u has too many loads", element -> number);
		    break;
		}

		element -> numdistributed ++;
		Deallocate (element -> distributed [element -> numdistributed]);
		element -> distributed [element -> numdistributed] =
		  (Distributed) $3;
	    }

	| element_load_list NAME
	    {
		if (element -> numdistributed == 3) {
		    error ("element %u has too many loads", element -> number);
		    break;
		}

		element -> numdistributed ++;
		Deallocate (element -> distributed [element -> numdistributed]);
		element -> distributed [element -> numdistributed] =
		  (Distributed) $2;
	    }

	| NAME
	    {
		element -> numdistributed = 1;
		Deallocate (element -> distributed [element -> numdistributed]);
		element -> distributed [element -> numdistributed] =
		  (Distributed) $1;
	    }
	;


/* Material section */

material_section
	: MATERIALS material_definition_list
	;


material_definition_list
	: material_definition_list material_definition
	| /* empty */
	;


material_definition
	: material_name material_parameter_list
	;


material_name
	: NAME
	    {
             material = CreateMaterial ($1);
             
             if (problem.material_set.count(material) > 0) {
                  error ("material %s is previously defined", $1);
                  DestroyMaterial (material);
                  material = &dummy_material;
             } else 
                  problem.material_set.insert(material);
	    }
	;

material_parameter_list
	: material_parameter_list material_parameter
	| /* empty */
	;


material_parameter
	: COLOR_EQ NAME
	    {
             material -> color = $2;
	    }

	| E_EQ constant_expression
	    {
		material -> E = $2;
	    }

	| IX_EQ constant_expression
	    {
		material -> Ix = $2;
	    }

	| IY_EQ constant_expression
	    {
		material -> Iy = $2;
	    }

	| IZ_EQ constant_expression
	    {
		material -> Iz = $2;
	    }

	| A_EQ constant_expression
	    {
		material -> A = $2;
	    }

	| J_EQ constant_expression
	    {
		material -> J = $2;
	    }

	| G_EQ constant_expression
	    {
		material -> G = $2;
	    }

	| T_EQ constant_expression
	    {
		material -> t = $2;
	    }

	| RHO_EQ constant_expression
	    {
		material -> rho = $2;
	    }

	| NU_EQ constant_expression
	    {
		material -> nu = $2;
	    }

	| KAPPA_EQ constant_expression
	    {
		material -> kappa = $2;
	    }

	| RK_EQ constant_expression
	    {
		material -> Rk = $2;
	    }

	| RM_EQ constant_expression
	    {
		material -> Rm = $2;
	    }

        | KX_EQ constant_expression
            {
                material -> Kx = $2;
            }

        | KY_EQ constant_expression
            {
                material -> Ky = $2;
            } 

        | KZ_EQ constant_expression
            {
                material -> Kz = $2;
            } 

        | C_EQ constant_expression
            {
                material -> c = $2;
            } 

	| error
	;


/* Load section */

load_section
	: LOADS load_definition_list
	;


load_definition_list
	: load_definition_list load_definition
	| /* empty */
	;


load_definition
	: load_name load_parameter_list
	;


load_name
	: NAME
	    {
		load = CreateDistributed ($1, 0);
		found = TreeInsert (problem.distributed_tree, load);

		if (found != (Item) load) {
		    error ("load %s is previously defined", $1);
		    DestroyDistributed (load);
		    load = &dummy_load;
		}
	    }
	;


load_parameter_list
	: load_parameter_list load_parameter
	| /* empty */
	;


load_parameter
	: COLOR_EQ NAME
	    {
             load -> color = $2;
	    }

	| DIRECTION_EQ DIRECTION
	    {
		load -> direction = (Direction) $2;
	    }

	| VALUES_EQ value_pair_list
	    {
		unsigned i;
		unsigned size;


		if (load == &dummy_load)
		    break;

		size = pair_ptr - pair_array;

        load -> value.resize(size);

		for (i = 1; i <= size; i ++)
		    load -> value [i] = pair_array [i - 1];
	    }

	| error
	;


value_pair_list
	: value_pair_list ',' value_pair
	    {
		*pair_ptr ++ = $3;
	    }

	| value_pair_list value_pair
	    {
		*pair_ptr ++ = $2;
	    }

	| value_pair
	    {
		pair_ptr = pair_array;
		*pair_ptr ++ = $1;
	    }
	;


value_pair
	: '(' node_number_expression ',' constant_expression ')'
	    {
             if ($2 < 1 || $2 > problem.nodes.size())
                  error ("node number %u is illegal", $2);

		$$.node = $2;
		$$.magnitude = $4;
	    }

	| '(' node_number_expression constant_expression ')'
	    {
             if ($2 < 1 || $2 > problem.nodes.size())
                  error ("node number %u is illegal", $2);
             
		$$.node = $2;
		$$.magnitude = $3;
	    }
	;


/* Force section */

force_section
	: FORCES force_definition_list
	;


force_definition_list
	: force_definition_list force_definition
	| /* empty */
	;


force_definition
	: force_name force_parameter_list
	;


force_name
	: NAME
	    {
		force = CreateForce ($1);
		found = TreeInsert (problem.force_tree, force);

		if (found != (Item) force) {
		    error ("force %s is previously defined", $1);
		    DestroyForce (force);
		    force = &dummy_force;
		}
	    }
	;


force_parameter_list
	: force_parameter_list force_parameter
	| /* empty */
	;


force_parameter
	: COLOR_EQ NAME
	    {
             force -> color = $2;
	    }

	| FX_EQ enable_copy variable_expression
	    {
		AssignForce (force, Fx, InCore, copy_input (0));
	    }

	| FY_EQ enable_copy variable_expression
	    {
		AssignForce (force, Fy, InCore, copy_input (0));
	    }

	| FZ_EQ enable_copy variable_expression
	    {
		AssignForce (force, Fz, InCore, copy_input (0));
	    }

	| MX_EQ enable_copy variable_expression
	    {
		AssignForce (force, Mx, InCore, copy_input (0));
	    }

	| MY_EQ enable_copy variable_expression
	    {
		AssignForce (force, My, InCore, copy_input (0));
	    }

	| MZ_EQ enable_copy variable_expression
	    {
		AssignForce (force, Mz, InCore, copy_input (0));
	    }

	| SFX_EQ enable_copy variable_expression
	    {
		AssignSpectrum (force, Fx, InCore, copy_input (0));
	    }

	| SFY_EQ enable_copy variable_expression
	    {
		AssignSpectrum (force, Fy, InCore, copy_input (0));
	    }

	| SFZ_EQ enable_copy variable_expression
	    {
		AssignSpectrum (force, Fz, InCore, copy_input (0));
	    }

	| SMX_EQ enable_copy variable_expression
	    {
		AssignSpectrum (force, Mx, InCore, copy_input (0));
	    }

	| SMY_EQ enable_copy variable_expression
	    {
		AssignSpectrum (force, My, InCore, copy_input (0));
	    }

	| SMZ_EQ enable_copy variable_expression
	    {
		AssignSpectrum (force, Mz, InCore, copy_input (0));
	    }

	| error
	;


/* Constraint section */

constraint_section
	: CONSTRAINTS constraint_definition_list
	;


constraint_definition_list
	: constraint_definition_list constraint_definition
	| /* empty */
	;


constraint_definition
	: constraint_name constraint_parameter_list
	;


constraint_name
	: NAME
	    {
		constraint = CreateConstraint ($1);
		found = TreeInsert (problem.constraint_tree, constraint);

		if (found != (Item) constraint) {
		    error ("constraint %s is previously defined", $1);
		    DestroyConstraint (constraint);
		    constraint = &dummy_constraint;
		}
	    }
	;


constraint_parameter_list
	: constraint_parameter_list constraint_parameter
	| /* empty */
	;


constraint_parameter
	: COLOR_EQ NAME
	    {
             constraint -> color = $2;
	    }

	| TX_EQ enable_copy translation
	    {
                if ($3 == VariableExpression)
                   AssignConstraint (constraint, Tx, InCore, copy_input(0), 1);
                else  {
                   AssignConstraint (constraint, Tx, NULL, NULL, $3);
		   copy_input (0);
		}
	    }

	| TY_EQ enable_copy translation
	    {
                if ($3 == VariableExpression)
                   AssignConstraint (constraint, Ty, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Ty, NULL, NULL, $3);
		   copy_input (0);
		}
	    }

	| TZ_EQ enable_copy translation
	    {
                if ($3 == VariableExpression)
                   AssignConstraint (constraint, Tz, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Tz, NULL, NULL, $3);
		   copy_input (0);
		}
	    }

	| RX_EQ enable_copy rotation
	    {
                if ($3 == VariableExpression)
                   AssignConstraint (constraint, Rx, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Rx, NULL, NULL, $3);
		   copy_input (0);
		}
	    }

	| RY_EQ enable_copy rotation
	    {
                if ($3 == VariableExpression)
                   AssignConstraint (constraint, Ry, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Ry, NULL, NULL, $3);
		   copy_input (0);
		}
	    }

	| RZ_EQ enable_copy rotation
	    {
                if ($3 == VariableExpression)
                   AssignConstraint (constraint, Rz, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Rz, NULL, NULL, $3);
		   copy_input (0);
		}
	    }

        | ITX_EQ constant_expression
            {
                constraint -> ix [Tx] = $2;
            }

        | ITY_EQ constant_expression
            {
                constraint -> ix [Ty] = $2;
            }

        | ITZ_EQ constant_expression
            {
                constraint -> ix [Tz] = $2;
            }

        | IRX_EQ constant_expression
            {
                constraint -> ix [Rx] = $2;
            }

        | IRY_EQ constant_expression
            {
                constraint -> ix [Ry] = $2;
            }

        | IRZ_EQ constant_expression
            {
                constraint -> ix [Rz] = $2;
            }

	| VX_EQ constant_expression
	    {
		constraint -> vx [Tx] = $2;
	    }

	| VY_EQ constant_expression
	    {
		constraint -> vx [Ty] = $2;
	    }

	| VZ_EQ constant_expression
	    {
		constraint -> vx [Tz] = $2;
	    }

	| AX_EQ constant_expression
	    {
		constraint -> ax [Tx] = $2;
	    }

	| AY_EQ constant_expression
	    {
		constraint -> ax [Ty] = $2;
	    }

	| AZ_EQ constant_expression
	    {
		constraint -> ax [Tz] = $2;
	    }

	| error
	;


translation
	: variable_expression
	    {
		$$ = VariableExpression;
	    }

	| CONSTRAINT
	    {
		$$ = $1;
	    }
	;


rotation
	: variable_expression
	    {
		$$ = VariableExpression;
	    }

	| CONSTRAINT
	    {
		$$ = $1;
	    }

	| HINGED
	    {
		$$ = $1;
	    }
	;


/* Load case section */

loadcase_section
	: LOAD_CASES loadcase_definition_list
	;


loadcase_definition_list
	: loadcase_definition_list loadcase_definition
	| /* empty */
	;

loadcase_definition
	: loadcase_name loadcase_parameter_list
	;

loadcase_name
	: NAME
	    {
		loadcase = CreateLoadCase ($1);
		found = TreeInsert (problem.loadcase_tree, loadcase);

		if (found != (Item) loadcase) {
		    error ("loadcase %s is previously defined", $1);
		    DestroyLoadCase (loadcase);
		    loadcase = &dummy_loadcase;
		}
	    }
	;

loadcase_parameter_list
	: loadcase_parameter_list loadcase_parameter
	| /* empty */
	;

loadcase_parameter
	: NODE_FORCES_EQ loadcase_pair_list
	    {
		unsigned i;
		unsigned size;


		if (loadcase == &dummy_loadcase)
		    break;

		size = case_ptr - case_array;

        loadcase->nodes.resize(size);
        loadcase->forces.resize(size);
        
		for (i = 1; i <= size; i ++) {
		    loadcase -> nodes [i] = (Node) case_array [i - 1].noe;
                    loadcase -> forces [i]   = (Force) case_array [i - 1].fol;
                }
	    }

	| ELEMENT_LOADS_EQ loadcase_pair_list
	    {
		unsigned i;
		unsigned size;


		if (loadcase == &dummy_loadcase)
		    break;

		size = case_ptr - case_array;

        loadcase->elements.resize(size);
        loadcase->loads.resize(size);
        
		for (i = 1; i <= size; i ++) {
		    loadcase -> elements [i] = (Element) case_array [i - 1].noe;
                    loadcase -> loads [i]   = (Distributed) case_array [i - 1].fol;
                }
	    }

	| error
	;


loadcase_pair_list
	: loadcase_pair_list ',' loadcase_pair
	    {
		*case_ptr ++ = $3;
	    }

	| loadcase_pair_list loadcase_pair
	    {
		*case_ptr ++ = $2;
	    }

	| loadcase_pair
	    {
		case_ptr = case_array;
		*case_ptr ++ = $1;
	    }
	;


loadcase_pair
	: '(' node_number_expression ',' NAME ')'
	    {
             if ($2 < 1 || $2 > problem.nodes.size())
                  error ("node number %u is illegal", $2);

		$$.noe = $2;
		$$.fol = $4;
	    }

	| '(' node_number_expression NAME ')'
	    {
             if ($2 < 1 || $2 > problem.nodes.size())
                  error ("node number %u is illegal", $2);

		$$.noe = $2;
		$$.fol = $3;
	    }
	;

/* Analysis section */

analysis_section
	: ANALYSIS analysis_parameter_list
	;


analysis_parameter_list
	: analysis_parameter_list analysis_parameter
	| /* empty */
	;


analysis_parameter
	: ALPHA_EQ constant_expression
	    {
		analysis.alpha = $2;
	    }

	| BETA_EQ constant_expression
	    {
		analysis.beta = $2;
	    }

	| GAMMA_EQ constant_expression
	    {
		analysis.gamma = $2;
	    }

        | RK_EQ constant_expression
            {
                analysis.Rk = $2;
            }

        | RM_EQ constant_expression
            {
                analysis.Rm = $2;
            }

	| START_EQ constant_expression
	    {
		analysis.start = $2;
	    }

	| STEP_EQ constant_expression
	    {
		analysis.step = $2;
	    }

	| STOP_EQ constant_expression
	    {
		analysis.stop = $2;
	    }

	| ITERATIONS_EQ INTEGER
	    {
		analysis.iterations = $2;
	    }

	| LOAD_STEPS_EQ INTEGER
	    {
		analysis.load_steps = $2;
	    }

	| RELAXATION_EQ constant_expression
	    {
		analysis.relaxation = $2;
	    }

	| TOLERANCE_EQ constant_expression
	    {
		analysis.tolerance = $2;
	    }

        | INPUT_DOF_EQ NODE_DOF
            {
                analysis.input_dof = $2;
            }

        | INPUT_NODE_EQ node_number_expression
            {
                analysis.input_node = (Node) $2;
            }

	| NODES_EQ '[' analysis_node_list ']'
	    {
		unsigned i;


        analysis.nodes.resize(int_ptr - int_array);

		for (i = 1; i <= analysis.nodes.size(); i ++)
		    analysis.nodes [i] = (Node) int_array [i - 1];
	    }

	| DOFS_EQ '[' analysis_dof_list ']'
	    {
		int i;


		analysis.numdofs = 0;
		for (i = 1; i <= 6; i ++)
		    if (analysis.dofs [i])
			analysis.dofs [++ analysis.numdofs] = i;
	    }

	| MASS_MODE_EQ MASS_MODE
	    {
		analysis.mass_mode = $2;
	    }

        | GRAVITY_EQ triple
            {
                analysis.gravity [1] = triple_x;
                analysis.gravity [2] = triple_y;
                analysis.gravity [3] = triple_z;
            }

	| error
	;


analysis_node_list
	: analysis_node_list ',' node_number_expression
	    {
		*int_ptr ++ = $3;
	    }

	| analysis_node_list node_number_expression
	    {
		*int_ptr ++ = $2;
	    }

	| /* empty */
	    {
		int_ptr = int_array;
	    }
	;


analysis_dof_list
	: analysis_dof_list ',' NODE_DOF
	    {
		analysis.dofs [$3] = 1;
	    }

	| analysis_dof_list NODE_DOF
	    {
		analysis.dofs [$2] = 1;
	    }

	| /* empty */
	    {
		int i;


		for (i = 1; i <= 6; i ++)
		     analysis.dofs [i] = 0;
	    }
	;


triple
	: '(' constant_expression ',' constant_expression opt_z_coordinate ')'
	    {
		triple_x = $2;
		triple_y = $4;
		triple_z = $5;
	    }
	;


opt_z_coordinate
	: ',' constant_expression
	    {
		$$ = $2;
	    }

        | /* empty */
            {
                $$ = 0.0;
            }
	;


/* Canvas section */

canvas_section
	: CANVAS canvas_parameter_list
	;


canvas_parameter_list
	: canvas_parameter_list canvas_parameter
	| /* empty */
	;


canvas_parameter
	: NODE_NUM_EQ BOOLEAN
	    {
		appearance.node_numbers = $2;
	    }

	| ELT_NUM_EQ BOOLEAN
	    {
		appearance.element_numbers = $2;
	    }

	| SNAP_EQ BOOLEAN
	    {
		appearance.snap = $2;
	    }

	| GRID_EQ BOOLEAN
	    {
		appearance.grid = $2;
	    }

	| SNAP_SIZE_EQ constant_expression
	    {
		appearance.snap_size = $2;
	    }

	| GRID_SIZE_EQ constant_expression
	    {
		appearance.grid_size = $2;
	    }

	| X_MIN_EQ constant_expression
	    {
		appearance.x_min = $2;
	    }

	| X_MAX_EQ constant_expression
	    {
		appearance.x_max = $2;
	    }

	| Y_MIN_EQ constant_expression
	    {
		appearance.y_min = $2;
	    }

	| Y_MAX_EQ constant_expression
	    {
		appearance.y_max = $2;
	    }

	| X_POS_EQ constant_expression
	    {
		appearance.x_pos = $2;
	    }

	| Y_POS_EQ constant_expression
	    {
		appearance.y_pos = $2;
	    }

	| WIDTH_EQ constant_expression
	    {
		appearance.width = $2;
	    }

	| HEIGHT_EQ constant_expression
	    {
		appearance.height = $2;
	    }

	| SCALE_EQ constant_expression
	    {
		appearance.scale = $2;
	    }

	| NODE_COLOR_EQ NAME
	    {
		Deallocate (appearance.node_color);
		appearance.node_color = $2;
	    }

	| ELT_COLOR_EQ NAME
	    {
		Deallocate (appearance.element_color);
		appearance.element_color = $2;
	    }

	| LABEL_FONT_EQ NAME
	    {
		Deallocate (appearance.label_font);
		appearance.label_font = $2;
	    }

	| TOOL_COLOR_EQ NAME
	    {
		Deallocate (appearance.tool_color);
		appearance.tool_color = $2;
	    }

	| TOOL_FONT_EQ NAME
	    {
		Deallocate (appearance.tool_font);
		appearance.tool_font = $2;
	    }
	;


/* Figure section */

figure_section
	: figure_header figure_definition_list
	;


figure_header
	: FIGURES
	    {
		figure_size = 0;
	    }
	;


figure_definition_list
	: figure_definition_list figure_definition
	| /* empty */
	;


figure_definition
	: figure_type figure_parameter_list
	;


figure_type
	: FIGURE_TYPE
	    {
		if (appearance.num_figures == figure_size) {
		    figure_size = figure_size ? figure_size <<= 1 : 4;
		    if (!Reallocate (appearance.figures, FigInfo, figure_size))
			Fatal ("unable to allocate figure list");
		}

		figure = &appearance.figures [appearance.num_figures ++];
		figure -> type = $1;
		figure -> x = 0;
		figure -> y = 0;
		figure -> width = 0;
		figure -> height = 0;
		figure -> start = 0;
		figure -> length = 0;
		figure -> num_points = 0;
		figure -> points = NULL;
		figure -> font = NULL;
		figure -> text = NULL;
		figure -> color = NULL;
	    }
	;


figure_parameter_list
	: figure_parameter_list figure_parameter
	| /* empty */
	;


figure_parameter
	: X_EQ constant_expression
	    {
		figure -> x = $2;
	    }

	| Y_EQ constant_expression
	    {
		figure -> y = $2;
	    }

	| WIDTH_EQ constant_expression
	    {
		figure -> width = $2;
	    }

	| HEIGHT_EQ constant_expression
	    {
		figure -> height = $2;
	    }

	| START_EQ constant_expression
	    {
		figure -> start = $2;
	    }

	| LENGTH_EQ constant_expression
	    {
		figure -> length = $2;
	    }

	| TEXT_EQ NAME
	    {
		Deallocate (figure -> text);
		figure -> text = $2;
	    }

	| COLOR_EQ NAME
	    {
		Deallocate (figure -> color);
		figure -> color = $2;
	    }

	| FONT_EQ NAME
	    {
		Deallocate (figure -> font);
		figure -> font = $2;
	    }

	| POINTS_EQ '[' figure_pair_list ']'
	;


figure_pair_list
	: figure_pair_list ',' figure_pair
	    {
		if (fig_point_size == figure -> num_points) {
		    fig_point_size <<= 1;
		    Reallocate (figure -> points, FigInfoPair, fig_point_size);
		    if (figure -> points == NULL)
			Fatal ("unable to allocate figure points");
		}

		figure -> points [figure -> num_points].x = figure_x;
		figure -> points [figure -> num_points ++].y = figure_y;
	    }

	| figure_pair_list figure_pair
	    {
		if (fig_point_size == figure -> num_points) {
		    fig_point_size <<= 1;
		    Reallocate (figure -> points, FigInfoPair, fig_point_size);
		    if (figure -> points == NULL)
			Fatal ("unable to allocate figure points");
		}

		figure -> points [figure -> num_points].x = figure_x;
		figure -> points [figure -> num_points ++].y = figure_y;
	    }

	| /* empty */
	    {
		figure -> points = Allocate (FigInfoPair, 2);
		if (figure -> points == NULL)
		    Fatal ("unable to allocate figure points");

		fig_point_size = 2;
		figure -> num_points = 0;
	    }
	;


figure_pair
	: '(' constant_expression ',' constant_expression ')'
	    {
		figure_x = $2;
		figure_y = $4;
	    }
	;


/* Expressions */

variable_expression
	: expression
	    {
		EmitCode (HaltOp);
		SetIP (0);
	    }

	| discrete_pair_list
	    {
		if (table_error)
		    EmitCode (PushOp, 0.0);
		else
		    EmitCode (TableOp, table, table_count);

		EmitCode (HaltOp);
		table_count = 0;
		table_error = 0;
		last_time = 0;
		SetIP (0);
	    }

	| discrete_pair_list '+'
	    {
		if (table_error)
		    EmitCode (PushOp, 0.0);
		else
		    EmitCode (CycleOp, table, table_count);

		EmitCode (HaltOp);
		table_count = 0;
		table_error = 0;
		last_time = 0;
		SetIP (0);
	    }
	;


discrete_pair_list
	: discrete_pair_list ',' discrete_pair
	| discrete_pair_list discrete_pair
	| discrete_pair
	;


discrete_pair
	: '(' constant_expression ',' constant_expression ')'
	    {
		if ($2 < last_time) {
		    error ("point not in nondecreasing order");
		    table_error = 1;
		    break;
		}

		if (table_count == table_size) {
		    table_size = table_size ? table_size << 1 : 8;
		    if (!Reallocate (table, double, table_size))
			Fatal ("unable to expand table");
		}

		table [table_count ++] = last_time = $2;
		table [table_count ++] = $4;
	    }
	;


enable_copy
	: /* empty */
	    {
		copy_input (1);
	    }
	;


constant_expression
	: expression
	    {
		EmitCode (HaltOp);
		SetIP (0);
		$$ = EvalCode (InCore, 0.0);
	    }
	;


expression
	: expression '?' if_action expression ':' else_action expression
	    {
		int ip = GetIP ( );
		SetIP (ip - $7 - 2);
		EmitCode (JmpOp, $7);
		SetIP (GetIP ( ) - $4 - 4);
		EmitCode (JzOp, $4 + 2);
		SetIP (ip);
		$$ = $1 + $3 + $4 + $6 + $7;
	    }

	| expression OR or_action expression
	    {
		int ip = GetIP ( );
		SetIP (ip - $4 - 3);
		EmitCode (JnzOp, $4 + 1);
		SetIP (ip);
		EmitCode (TestOp);
		$$ = $1 + $3 + $4 + 1;
	    }

	| expression AND and_action expression
	    {
		int ip = GetIP ( );
		SetIP (ip - $4 - 3);
		EmitCode (JzOp, $4 + 1);
		SetIP (ip);
		EmitCode (TestOp);
		$$ = $1 + $3 + $4 + 1;
	    }

	| expression '|' expression
	    {
		EmitCode (OrOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '^' expression
	    {
		EmitCode (XorOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '&' expression
	    {
		EmitCode (AndOp);
		$$ = $1 + 1 + $3;
	    }

	| expression EQUALS expression
	    {
		EmitCode (EqOp);
		$$ = $1 + 1 + $3;
	    }

	| expression NEQUAL expression
	    {
		EmitCode (NeqOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '<' expression
	    {
		EmitCode (LtOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '>' expression
	    {
		EmitCode (GtOp);
		$$ = $1 + 1 + $3;
	    }

	| expression LT_EQ expression
	    {
		EmitCode (LteqOp);
		$$ = $1 + 1 + $3;
	    }

	| expression GT_EQ expression
	    {
		EmitCode (GteqOp);
		$$ = $1 + 1 + $3;
	    }

	| expression LSHIFT expression
	    {
		EmitCode (LsftOp);
		$$ = $1 + 1 + $3;
	    }

	| expression RSHIFT expression
	    {
		EmitCode (RsftOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '+' expression
	    {
		EmitCode (AddOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '-' expression
	    {
		EmitCode (SubOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '*' expression
	    {
		EmitCode (MulOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '/' expression
	    {
		EmitCode (DivOp);
		$$ = $1 + 1 + $3;
	    }

	| expression '%' expression
	    {
		EmitCode (ModOp);
		$$ = $1 + 1 + $3;
	    }

	| '+' expression		%prec UNARY
	    {
		$$ = $2;
	    }

	| '-' expression		%prec UNARY
	    {
		EmitCode (NegOp);
		$$ = 1 + $2;
	    }

	| '!' expression
	    {
		EmitCode (NotOp);
		$$ = 1 + $2;
	    }

	| '~' expression
	    {
		EmitCode (InvOp);
		$$ = 1 + $2;
	    }

	| '(' expression ')'
	    {
		$$ = $2;
	    }

	| INTEGER
	    {
		EmitCode (PushOp, (double) $1);
		$$ = 2;
	    }

	| DOUBLE
	    {
		EmitCode (PushOp, $1);
		$$ = 2;
	    }

	| TIME
	    {
		EmitCode (TimeOp);
		$$ = 1;
	    }

	| function
	;


function
	: SIN '(' expression ')'
	    {
		EmitCode (SinOp);
		$$ = $3 + 1;
	    }

	| COS '(' expression ')'
	    {
		EmitCode (CosOp);
		$$ = $3 + 1;
	    }

	| TAN '(' expression ')'
	    {
		EmitCode (TanOp);
		$$ = $3 + 1;
	    }

	| POW '(' expression ',' expression ')'
	    {
		EmitCode (PowOp);
		$$ = $3 + $5 + 1;
	    }

	| EXP '(' expression ')'
	    {
		EmitCode (ExpOp);
		$$ = $3 + 1;
	    }

	| LOG '(' expression ')'
	    {
		EmitCode (LnOp);
		$$ = $3 + 1;
	    }

	| LOG10 '(' expression ')'
	    {
		EmitCode (LogOp);
		$$ = $3 + 1;
	    }

	| SQRT '(' expression ')'
	    {
		EmitCode (SqrtOp);
		$$ = $3 + 1;
	    }

	| HYPOT '(' expression ',' expression ')'
	    {
		EmitCode (HypotOp);
		$$ = $3 + $5 + 1;
	    }

	| FLOOR '(' expression ')'
	    {
		EmitCode (FloorOp);
		$$ = $3 + 1;
	    }

	| CEIL '(' expression ')'
	    {
		EmitCode (CeilOp);
		$$ = $3 + 1;
	    }

	| FMOD '(' expression ',' expression ')'
	    {
		EmitCode (FmodOp);
		$$ = $3 + $5 + 1;
	    }

	| FABS '(' expression ')'
	    {
		EmitCode (FabsOp);
		$$ = $3 + 1;
	    }
	;


if_action
	: /* empty */
	    {
		EmitCode (JzOp, 0);
		$$ = 2;
	    }
	;


else_action
	: /* empty */
	    {
		EmitCode (JmpOp, 0);
		$$ = 2;
	    }
	;


or_action
	: /* empty */
	    {
		EmitCode (CopyOp);
		EmitCode (JnzOp, 0);
		EmitCode (PopOp);
		$$ = 4;
	    }
	;


and_action
	: /* empty */
	    {
		EmitCode (CopyOp);
		EmitCode (JzOp, 0);
		EmitCode (PopOp);
		$$ = 4;
	    }
	;

%%

# ifdef YYBYACC
char *felt_suppress_warnings_from_gcc = yysccsid;
# endif
