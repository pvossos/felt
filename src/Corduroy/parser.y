%{
/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993,1994 Jason I. Gobat and Darren C. Atkinson

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
 *		parser for the corduroy element generator.		*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include "code.h"
# include "error.h"
# include "generator.h"
# include "allocate.h"
# include "definition.h"

# if !defined (__GNUC__) && !defined (__sparc__)
# define alloca malloc		/* prevents alloca from being called */
# endif

extern "C" void cord_yyerror (const char *msg);
extern "C" int  cord_yylex   (void);


typedef double xy_pair [2];


static double		x;
static double		y;
static double		z;

static double		last_z;

static Definition	last_line_definition;
static Definition	last_grid_definition;
static Definition	last_quadgrid_definition;
static Definition	last_brickgrid_definition;
static Definition	last_trimesh_definition;

static unsigned		line_size;
static unsigned		grid_size;
static unsigned		quadgrid_size;
static unsigned		brickgrid_size;
static unsigned		trimesh_size;
static unsigned		curve_size;
static unsigned		vcl_size;

static struct _line	line;
static struct _grid	grid;
static struct _grid     quadgrid;
static struct _grid     brickgrid;
static struct _trimesh	trimesh;
static struct _curve	curve;
%}

%union {
    int    i;
    double d;
    char  *s;
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


%token	NAME INTEGER DOUBLE 

%token  LINEAR SINUSOIDAL COSINUSOIDAL LOGARITHMIC PARABOLIC
%token  REVERSE_LOGARITHMIC REVERSE_PARABOLIC

%token	SIN COS TAN POW EXP LOG LOG10 SQRT HYPOT FLOOR CEIL FMOD FABS

%token	LINE GRID QUADGRID BRICKGRID TRIMESH END

%token	NODE_EQ ELEMENT_EQ CONSTRAINT_EQ MATERIAL_EQ TYPE_EQ

%token	START_EQ END_EQ NUMBER_EQ RULE_EQ

%token	X_NUMBER_EQ Y_NUMBER_EQ Z_NUMBER_EQ X_RULE_EQ Y_RULE_EQ Z_RULE_EQ

%token	TARGET_EQ ALPHA_EQ
%token	BOUNDARY_EQ HOLE_EQ

%type	<i> INTEGER rule
%type	<d> DOUBLE constant_expression opt_z_coordinate
%type	<s> NAME
%type	<i> expression function or_action and_action if_action else_action
%%

specification
	: initialize start_parameter_list generator_list END
	;


initialize
	: /* empty */
	    {
		line_size      = 0;
		grid_size      = 0;
                quadgrid_size  = 0;
                brickgrid_size = 0;
		trimesh_size   = 0;

		last_line_definition      = LookupDefinition ("truss");
		last_grid_definition      = last_line_definition;
		last_quadgrid_definition  = LookupDefinition ("quad_PlaneStress");
		last_brickgrid_definition = LookupDefinition ("brick");
		last_trimesh_definition   = LookupDefinition ("CSTPlaneStress");
	    }
	;


/* Starting parameters */

start_parameter_list
	: start_parameter_list start_parameter
	| /* empty */
	;


start_parameter
	: NODE_EQ INTEGER
	    {
		generator.start_node = $2;
	    }

	| ELEMENT_EQ INTEGER
	    {
		generator.start_element = $2;
	    }

	| MATERIAL_EQ NAME
	    {
		Deallocate (generator.material);
		generator.material = $2;
	    }

	| CONSTRAINT_EQ NAME
	    {
		Deallocate (generator.constraint);
		generator.constraint = $2;
	    }
	;


/* Generators */

generator_list
	: generator_list generator
	| /* empty */
	;


generator
	: line_generator
	| grid_generator
        | quadgrid_generator
        | brickgrid_generator
	| trimesh_generator
	;


/* Line generation */

line_generator
	: line_specifier line_parameter_list
	    {
		Line new_line;


		if (generator.num_lines == line_size) {
		    line_size = line_size ? line_size << 1 : 4;
		    if (!Reallocate (generator.lines, Line, line_size))
			Fatal ("unable to allocate line array");
		}

		if (!(new_line = AllocNew (struct _line)))
		    Fatal ("unable to allocate new line");

		*new_line = line;
		generator.lines [generator.num_lines ++] = new_line;
		last_line_definition = line.definition;
	    }
	;


line_specifier
	: LINE
	    {
		last_z = 0;

		line.xs = line.ys = line.zs = 0;
		line.xe = line.ye = line.ze = 0;
		line.rule = LinearRule;
		line.number = 1;

		line.definition = last_line_definition;
	    }
	;


line_parameter_list
	: line_parameter_list line_parameter
	| /* */
	;


line_parameter
	: TYPE_EQ NAME
	    {
		line.definition = defnlookup ($2);
		Deallocate ($2);
	    }

	| START_EQ triple
	    {
		line.xs = x;
		line.ys = y;
		line.zs = z;
	    }

	| END_EQ triple
	    {
		line.xe = x;
		line.ye = y;
		line.ze = z;
	    }

	| NUMBER_EQ constant_expression
	    {
		line.number = $2;
	    }

	| RULE_EQ rule
	    {
             line.rule = (Rule) $2;
	    }
	;


/* Grid generation */

grid_generator
	: grid_specifier grid_parameter_list
	    {
		Grid new_grid;


		if (generator.num_grids == grid_size) {
		    grid_size = grid_size ? grid_size << 1 : 4;
		    if (!Reallocate (generator.grids, Grid, grid_size))
			Fatal ("unable to allocate grid array");
		}

		if (!(new_grid = AllocNew (struct _grid)))
		    Fatal ("unable to allocate new grid");

		*new_grid = grid;
		generator.grids [generator.num_grids ++] = new_grid;
		last_grid_definition = grid.definition;
	    }
	;


grid_specifier
	: GRID
	    {
		last_z = 0;

		grid.xs = grid.ys = grid.zs = 0;
		grid.xe = grid.ye = grid.ze = 0;
		grid.xnumber = grid.ynumber = grid.znumber = 1;
		grid.xrule = grid.yrule = grid.zrule = LinearRule;
		grid.definition = last_grid_definition;
	    }
	;


grid_parameter_list
	: grid_parameter_list grid_parameter
	| /* empty */
	;


grid_parameter
	: TYPE_EQ NAME
	    {
		grid.definition = defnlookup ($2);
		Deallocate ($2);
	    }

	| START_EQ triple
	    {
		grid.xs = x;
		grid.ys = y;
		grid.zs = z;
	    }

	| END_EQ triple
	    {
		grid.xe = x;
		grid.ye = y;
		grid.ze = z;
	    }

	| X_NUMBER_EQ constant_expression
	    {
		grid.xnumber = $2;
	    }

	| Y_NUMBER_EQ constant_expression
	    {
		grid.ynumber = $2;
	    }

	| Z_NUMBER_EQ constant_expression
	    {
		grid.znumber = $2;
	    }

	| X_RULE_EQ rule
	    {
             grid.xrule = (Rule) $2;
	    }

	| Y_RULE_EQ rule
	    {
             grid.yrule = (Rule) $2;
	    }

	| Z_RULE_EQ rule
	    {
             grid.zrule = (Rule) $2;
	    }
	;


/* QuadGrid generation */

quadgrid_generator
	: quadgrid_specifier quadgrid_parameter_list
	    {
		Grid new_quadgrid;


		if (generator.num_quadgrids == quadgrid_size) {
		    quadgrid_size = quadgrid_size ? quadgrid_size << 1 : 4;
		    if (!Reallocate (generator.quadgrids, Grid, quadgrid_size))
			Fatal ("unable to allocate quadgrid array");
		}

		if (!(new_quadgrid = AllocNew (struct _grid)))
		    Fatal ("unable to allocate new quadgrid");

		*new_quadgrid = quadgrid;
		generator.quadgrids [generator.num_quadgrids ++] = new_quadgrid;
		last_quadgrid_definition = quadgrid.definition;
	    }
	;


quadgrid_specifier
	: QUADGRID
	    {
		quadgrid.xs = quadgrid.ys = 0;
		quadgrid.xe = quadgrid.ye = 0;
		quadgrid.xnumber = quadgrid.ynumber = 1;
		quadgrid.xrule = quadgrid.yrule = LinearRule;
		quadgrid.definition = last_quadgrid_definition;
	    }
	;


quadgrid_parameter_list
	: quadgrid_parameter_list quadgrid_parameter
	| /* empty */
	;


quadgrid_parameter
	: TYPE_EQ NAME
	    {
		quadgrid.definition = defnlookup ($2);
		Deallocate ($2);
	    }

	| START_EQ pair
	    {
		quadgrid.xs = x;
		quadgrid.ys = y;
	    }

	| END_EQ pair
	    {
		quadgrid.xe = x;
		quadgrid.ye = y;
	    }

	| X_NUMBER_EQ constant_expression
	    {
		quadgrid.xnumber = $2;
	    }

	| Y_NUMBER_EQ constant_expression
	    {
		quadgrid.ynumber = $2;
	    }

	| X_RULE_EQ rule
	    {
             quadgrid.xrule = (Rule) $2;
	    }

	| Y_RULE_EQ rule
	    {
             quadgrid.yrule = (Rule) $2;
	    }
	;

/* BrickGrid generation */

brickgrid_generator
	: brickgrid_specifier brickgrid_parameter_list
	    {
		Grid new_brickgrid;


		if (generator.num_brickgrids == brickgrid_size) {
		    brickgrid_size = brickgrid_size ? brickgrid_size << 1 : 4;
		    if (!Reallocate (generator.brickgrids, Grid, brickgrid_size))
			Fatal ("unable to allocate brickgrid array");
		}

		if (!(new_brickgrid = AllocNew (struct _grid)))
		    Fatal ("unable to allocate new brickgrid");

		*new_brickgrid = brickgrid;
		generator.brickgrids [generator.num_brickgrids ++] = new_brickgrid;
		last_brickgrid_definition = brickgrid.definition;
	    }
	;


brickgrid_specifier
	: BRICKGRID
	    {
		brickgrid.xs = brickgrid.ys = 0;
		brickgrid.xe = brickgrid.ye = 0;
		brickgrid.ze = brickgrid.ze = 0;
		brickgrid.xnumber = 1;
                brickgrid.ynumber = 1;
                brickgrid.znumber = 1;
		brickgrid.xrule = LinearRule;
                brickgrid.yrule = LinearRule;
                brickgrid.zrule = LinearRule;
		brickgrid.definition = last_brickgrid_definition;
	    }
	;


brickgrid_parameter_list
	: brickgrid_parameter_list brickgrid_parameter
	| /* empty */
	;


brickgrid_parameter
	: TYPE_EQ NAME
	    {
		brickgrid.definition = defnlookup ($2);
		Deallocate ($2);
	    }

	| START_EQ triple
	    {
		brickgrid.xs = x;
		brickgrid.ys = y;
		brickgrid.zs = z;
	    }

	| END_EQ triple
	    {
		brickgrid.xe = x;
		brickgrid.ye = y;
		brickgrid.ze = z;
	    }

	| X_NUMBER_EQ constant_expression
	    {
		brickgrid.xnumber = $2;
	    }

	| Y_NUMBER_EQ constant_expression
	    {
		brickgrid.ynumber = $2;
	    }

	| Z_NUMBER_EQ constant_expression
	    {
		brickgrid.znumber = $2;
	    }

	| X_RULE_EQ rule
	    {
             brickgrid.xrule = (Rule) $2;
	    }

	| Y_RULE_EQ rule
	    {
             brickgrid.yrule = (Rule) $2;
	    }

	| Z_RULE_EQ rule
	    {
             brickgrid.zrule = (Rule) $2;
	    }
	;


/* Triangular mesh generation */

trimesh_generator
	: trimesh_specifier trimesh_parameter_list
	    {
		TriMesh new_mesh;


		if (generator.num_trimeshes == trimesh_size) {
		    trimesh_size = trimesh_size ? trimesh_size << 1 : 4;
		    if (!Reallocate (generator.trimeshes, TriMesh, trimesh_size))
			Fatal ("unable to allocate mesh array");
		}

		if (!(new_mesh = AllocNew (struct _trimesh)))
		    Fatal ("unable to allocate new trimesh");

		*new_mesh = trimesh;
		generator.trimeshes [generator.num_trimeshes ++] = new_mesh;
		last_trimesh_definition = trimesh.definition;
	    }
	;


trimesh_specifier
	: TRIMESH
	    {
		trimesh.numcurves  = 0;
		trimesh.alpha	   = 2.0;
		trimesh.target	   = 100;

		trimesh.definition = last_trimesh_definition;

		curve_size = 2;
		if (!(trimesh.curves = Allocate (Curve, curve_size)))
		    Fatal ("unable to allocate curve array");
	    }
	;


trimesh_parameter_list
	: trimesh_parameter_list trimesh_parameter
	| /* empty */
	;


trimesh_parameter
	: TYPE_EQ NAME
	    {
		trimesh.definition = defnlookup ($2);
		Deallocate ($2);
	    }

	| ALPHA_EQ constant_expression
	    {
		trimesh.alpha = $2;
	    }

	| TARGET_EQ INTEGER
	    {
		trimesh.target = $2;
	    }

	| BOUNDARY_EQ '[' pair_list ']'
	    {
		Curve new_curve;


		if (!(new_curve = AllocNew (struct _curve)))
		    Fatal ("unable to allocate new curve");

		*new_curve = curve;
		trimesh.curves [0] = new_curve;

		if (trimesh.numcurves == 0)
		    trimesh.numcurves = 1;
	    }

	| HOLE_EQ '[' pair_list ']'
	    {
		Curve new_curve;


		if (trimesh.numcurves == curve_size)
		    if (!Reallocate (trimesh.curves, Curve, curve_size <<= 1))
			Fatal ("unable to allocate curve array");

		if (!(new_curve = AllocNew (struct _curve)))
		    Fatal ("unable to allocate new curve");

		*new_curve = curve;
		if (trimesh.numcurves == 0)
		    trimesh.numcurves = 1;
		trimesh.curves [trimesh.numcurves ++] = new_curve;
	    }
	;


/* Rules */

rule
	: LINEAR
	    {
		$$ = LinearRule;
	    }

	| COSINUSOIDAL
	    {
		$$ = CosRule;
	    }

	| SINUSOIDAL
	    {
		$$ = SinRule;
	    }

	| LOGARITHMIC
	    {
		$$ = LogRule;
	    }

	| PARABOLIC
	    {
		$$ = ParabolicRule;
	    }

	| REVERSE_LOGARITHMIC
	    {
		$$ = RevLogRule;
	    }

	| REVERSE_PARABOLIC
	    {
		$$ = RevParabolicRule;
	    }
	;


/* Coordinates */

pair_list
	: pair_list ',' pair
	    {
		if (curve.numvc == vcl_size)
		    if (!Reallocate (curve.vcl, xy_pair, vcl_size <<= 1))
			Fatal ("unable to allocate pair array");

		curve.vcl [curve.numvc] [0] = x;
		curve.vcl [curve.numvc ++] [1] = y;
	    }

	| pair_list pair
	    {
		if (curve.numvc == vcl_size)
		    if (!Reallocate (curve.vcl, xy_pair, vcl_size <<= 1))
			Fatal ("unable to allocate pair array");

		curve.vcl [curve.numvc] [0] = x;
		curve.vcl [curve.numvc ++] [1] = y;
	    }

	| /* empty */
	    {
		vcl_size = 8;
		curve.numvc = 0;
		if (!(curve.vcl = Allocate (xy_pair, vcl_size)))
		    Fatal ("unable to allocate pair array");
	    }
	;


triple
	: '(' constant_expression ',' constant_expression opt_z_coordinate ')'
	    {
		x = $2;
		y = $4;
		z = $5;
	    }
	;


opt_z_coordinate
	: ',' constant_expression
	    {
		$$ = $2;
	    }

	| /* empty */
	    {
		$$ = last_z;
	    }
	;


pair
	: '(' constant_expression ',' constant_expression ')'
	    {
		x = $2;
		y = $4;
	    }
	;



/* Expressions */

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
