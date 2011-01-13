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
 * File:	felt.c							*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		interface to the FElt data structures.			*
 ************************************************************************/

# include <stdio.h>
# include <unistd.h>
# include <stdarg.h>
# include "felt.h"
# include "error.h"
# include "field.h"
# include "coerce.h"
# include "execute.h"
# include "problem.h"
# include "allocate.h"
# include "definition.h"

/* These are hacks.  We can't include felt's code.h since there would be
   type conflicts with the types defined in codegen.h.  Fortunately code is
   the same size all over the world. */

extern Code   InCore;
int CompileCode(char *text);
int IsConstant(Code code);
double EvalCode(Code code, double time);
void FreeCode(Code code);
Code CopyCode(Code code);

/* This is only in misc.h. */

void SetupStressMemory(Element element);

# define NUMBER(x)	(sizeof (x) / sizeof (*x))
# define OFFSET(x,y)	((unsigned) (((char *) (&(((x)0)->y))) - ((char *) 0)))


typedef struct field {
    char       *name;
    int		type;
    unsigned	offset;
    TrapHandler	handler;
} *Field;

static int last_handler;
static int first_handler;

static int non_null	        (descriptor *dest, descriptor **source);
static int num_loads	    (descriptor *dest, descriptor **source);
static int direction	    (descriptor *dest, descriptor **source);
static int read_only	    (descriptor *dest, descriptor **source);
static int code_expression  (descriptor *dest, descriptor **source);
static int code_assignment  (descriptor *dest, descriptor **source);

static int load_array	    (descriptor *dest, descriptor **source);
static int node_array	    (descriptor *dest, descriptor **source);
static int force_array	    (descriptor *dest, descriptor **source);
static int stress_array	    (descriptor *dest, descriptor **source);
static int element_array    (descriptor *dest, descriptor **source);
static int problem_array    (descriptor *dest, descriptor **source);
static int analysis_array   (descriptor *dest, descriptor **source);
static int constraint_array (descriptor *dest, descriptor **source);
static int definition_array (descriptor *dest, descriptor **source);

static void invalidate	    (descriptor *d);


/* Analysis fields
	missing fields: none
	aliased fields: num_nodes, num_dofs */

# undef  OFF
# define OFF(x) OFFSET (Analysis *,x)

static struct field analysis_fields [ ] = {
    {"gamma",	   T_Double, OFF (gamma),        strict_assignment},
    {"beta",	   T_Double, OFF (beta),         strict_assignment},
    {"alpha",	   T_Double, OFF (alpha),        strict_assignment},
    {"mass_mode",  T_Byte,   OFF (mass_mode),    strict_assignment},
    {"nodes",	   T_Array,  OFF (nodes),        analysis_array},
    {"numnodes",   T_Int,    OFF (numnodes),     read_only},
    {"dofs",	   T_Array,  OFF (dofs),         analysis_array},
    {"numdofs",	   T_Int,    OFF (numdofs),      read_only},
    {"start",	   T_Double, OFF (start),	 strict_assignment},
    {"step",	   T_Double, OFF (step),	 strict_assignment},
    {"stop",	   T_Double, OFF (stop),	 strict_assignment},
    {"Rk",	   T_Double, OFF (Rk),		 strict_assignment},
    {"Rm",	   T_Double, OFF (Rm),		 strict_assignment},
    {"num_nodes",  T_Int,    OFF (numnodes),     read_only},
    {"num_dofs",   T_Int,    OFF (numdofs),      read_only},
};


/* Constraint fields
	missing fields:	aux, expr, text
	aliased fields:	Tx, Ty, Tz, Rx, Ry, Rz, Vx, Vy, Vz, Ax, Ay, Az,
			iTx, iTy, iTz, iRx, iRy, iRz */

# undef  OFF
# define OFF(x) OFFSET (Constraint,x)

static struct field constraint_fields [ ] = {
    {"name",	   T_String, OFF (name),       read_only},
    {"color",	   T_String, OFF (color),      strict_assignment},
    {"constraint", T_Array,  OFF (constraint), constraint_array},
    {"dx",	   T_Array,  OFF (dx),	       constraint_array},
    {"ix",	   T_Array,  OFF (ix),	       constraint_array},
    {"vx",	   T_Array,  OFF (vx),	       constraint_array},
    {"ax",	   T_Array,  OFF (ax),	       constraint_array},
    {"Tx",	   T_Double, OFF (dx [Tx]),    code_expression},
    {"Ty",	   T_Double, OFF (dx [Ty]),    code_expression},
    {"Tz",	   T_Double, OFF (dx [Tz]),    code_expression},
    {"Rx",	   T_Double, OFF (dx [Rx]),    code_expression},
    {"Ry",	   T_Double, OFF (dx [Ry]),    code_expression},
    {"Rz",	   T_Double, OFF (dx [Rz]),    code_expression},
    {"iTx",	   T_Double, OFF (ix [Tx]),    strict_assignment},
    {"iTy",	   T_Double, OFF (ix [Ty]),    strict_assignment},
    {"iTz",	   T_Double, OFF (ix [Tz]),    strict_assignment},
    {"iRx",	   T_Double, OFF (ix [Rx]),    strict_assignment},
    {"iRy",	   T_Double, OFF (ix [Ry]),    strict_assignment},
    {"iRz",	   T_Double, OFF (ix [Rz]),    strict_assignment},
    {"Vx",	   T_Double, OFF (vx [Tx]),    strict_assignment},
    {"Vy",	   T_Double, OFF (vx [Ty]),    strict_assignment},
    {"Vz",	   T_Double, OFF (vx [Tz]),    strict_assignment},
    {"Ax",	   T_Double, OFF (ax [Tx]),    strict_assignment},
    {"Ay",	   T_Double, OFF (ax [Ty]),    strict_assignment},
    {"Az",	   T_Double, OFF (ax [Tz]),    strict_assignment},
};


/* Definition fields
	missing fields: none
	aliased fields: num_nodes, shape_nodes, num_stresses, num_dofs */

# undef  OFF
# define OFF(x) OFFSET(Definition,x)

static struct field definition_fields [ ] = {
    {"name",	     T_String,	 OFF (name),	    read_only},
    {"setup",	     T_External, OFF (setup),	    read_only},
    {"stress",	     T_External, OFF (stress),	    read_only},
    {"shape",	     T_Int,	 OFF (shape),	    read_only},
    {"numnodes",     T_Int,	 OFF (numnodes),    read_only},
    {"shapenodes",   T_Int,	 OFF (shapenodes),  read_only},
    {"numstresses",  T_Int,	 OFF (numstresses), read_only},
    {"numdofs",	     T_Int,	 OFF (numdofs),	    read_only},
    {"dofs",	     T_Array,	 OFF (dofs),	    definition_array},
    {"retainK",	     T_Int,	 OFF (retainK),	    strict_assignment},
    {"num_nodes",    T_Int,	 OFF (numnodes),    read_only},
    {"shape_nodes",  T_Int,	 OFF (shapenodes),  read_only},
    {"num_stresses", T_Int,	 OFF (numstresses), read_only},
    {"num_dofs",     T_Int,	 OFF (numdofs),	    read_only},
};


/* Element fields
	missing fields:	aux
	aliased fields:	nodes, loads, stresses, num_loads, num_distributed */

# undef  OFF
# define OFF(x) OFFSET(Element,x)

static struct field element_fields [ ] = {
    {"number",		T_Int,	      OFF (number),	    read_only},
    {"node",		T_Array,      OFF (node),	    element_array},
    {"K",		T_MatrixPtr,  OFF (K),		    strict_assignment},
    {"M",		T_MatrixPtr,  OFF (M),		    strict_assignment},
    {"material",	T_Material,   OFF (material),	    non_null},
    {"definition",	T_Definition, OFF (definition),     read_only},
    {"distributed",	T_Array,      OFF (distributed),    element_array},
    {"numdistributed",	T_Int,	      OFF (numdistributed), num_loads},
    {"stress",		T_Array,      OFF (stress),	    element_array},
    {"ninteg",		T_Int,	      OFF (ninteg),	    strict_assignment},
    {"nodes",		T_Array,      OFF (node),	    element_array},
    {"loads",		T_Array,      OFF (distributed),    element_array},
    {"stresses",	T_Array,      OFF (stress),	    element_array},
    {"num_loads",	T_Int,	      OFF (numdistributed), num_loads},
    {"num_distributed",	T_Int,	      OFF (numdistributed), num_loads},
};


/* Force fields
	missing fields:	aux, expr, text
	aliased fields: spectra, Fx, Fy, Fz, Mx, My, Mz,
			Sfx, Sfy, Sfz, Smx, Smy, Smz */

# undef  OFF
# define OFF(x) OFFSET(Force,x)

static struct field force_fields [ ] = {
    {"name",	T_String, OFF (name),	       read_only},
    {"color",	T_String, OFF (color),	       strict_assignment},
    {"force",	T_Array,  OFF (force),	       force_array},
    {"spectum", T_Array,  OFF (spectrum),      force_array},
    {"spectra", T_Array,  OFF (spectrum),      force_array},
    {"Fx",	T_Double, OFF (force [Fx]),    code_expression},
    {"Fy",	T_Double, OFF (force [Fy]),    code_expression},
    {"Fz",	T_Double, OFF (force [Fz]),    code_expression},
    {"Mx",	T_Double, OFF (force [Mx]),    code_expression},
    {"My",	T_Double, OFF (force [My]),    code_expression},
    {"Mz",	T_Double, OFF (force [Mz]),    code_expression},
    {"Sfx",	T_Double, OFF (spectrum [Fx]), code_expression},
    {"Sfy",	T_Double, OFF (spectrum [Fy]), code_expression},
    {"Sfz",	T_Double, OFF (spectrum [Fz]), code_expression},
    {"Smx",	T_Double, OFF (spectrum [Mx]), code_expression},
    {"Smy",	T_Double, OFF (spectrum [My]), code_expression},
    {"Smz",	T_Double, OFF (spectrum [Mz]), code_expression},
};


/* Load fields
	missing fields:	aux
	aliased fields:	num_values, values */

# undef  OFF
# define OFF(x) OFFSET(Distributed,x)

static struct field load_fields [ ] = {
    {"name",	   T_String, OFF (name),      read_only},
    {"color",	   T_String, OFF (color),     strict_assignment},
    {"direction",  T_Int,    OFF (direction), direction},
    {"nvalues",	   T_Int,    OFF (nvalues),   strict_assignment},
    {"value",	   T_Array,  OFF (value),     load_array},
    {"num_values", T_Int,    OFF (nvalues),   strict_assignment},
    {"values",	   T_Array,  OFF (value),     load_array},
};


/* Material fields
	missing fields:	aux
	aliased fields:	none */

# undef  OFF
# define OFF(x) OFFSET(Material,x)

static struct field material_fields [ ] = {
    {"name",  T_String, OFF (name),  read_only},
    {"color", T_String, OFF (color), strict_assignment},
    {"E",     T_Double, OFF (E),     strict_assignment},
    {"Ix",    T_Double, OFF (Ix),    strict_assignment},
    {"Iy",    T_Double, OFF (Iy),    strict_assignment},
    {"Iz",    T_Double, OFF (Iz),    strict_assignment},
    {"A",     T_Double, OFF (A),     strict_assignment},
    {"J",     T_Double, OFF (J),     strict_assignment},
    {"G",     T_Double, OFF (G),     strict_assignment},
    {"t",     T_Double, OFF (t),     strict_assignment},
    {"rho",   T_Double, OFF (rho),   strict_assignment},
    {"nu",    T_Double, OFF (nu),    strict_assignment},
    {"kappa", T_Double, OFF (kappa), strict_assignment},
    {"Rk",    T_Double, OFF (Rk),    strict_assignment},
    {"Rm",    T_Double, OFF (Rm),    strict_assignment},
};


/* Node fields
	missing fields: aux
	aliased fields: Tx, Ty, Tz, Rx, Ry, Rz */

# undef  OFF
# define OFF(x) OFFSET(Node,x)

static struct field node_fields [ ] = {
    {"number",	   T_Int,	 OFF (number),	   read_only},
    {"constraint", T_Constraint, OFF (constraint), non_null},
    {"force",	   T_Force,	 OFF (force),	   strict_assignment},
    {"eq_force",   T_Array,	 OFF (eq_force),   node_array},
    {"dx",	   T_Array,	 OFF (dx),	   node_array},
    {"x",	   T_Double,	 OFF (x),	   strict_assignment},
    {"y",	   T_Double,	 OFF (y),	   strict_assignment},
    {"z",	   T_Double,	 OFF (z),	   strict_assignment},
    {"Tx",	   T_Double,	 OFF (dx [Tx]),    strict_assignment},
    {"Ty",	   T_Double,	 OFF (dx [Ty]),    strict_assignment},
    {"Tz",	   T_Double,	 OFF (dx [Tz]),    strict_assignment},
    {"Rx",	   T_Double,	 OFF (dx [Rx]),    strict_assignment},
    {"Ry",	   T_Double,	 OFF (dx [Ry]),    strict_assignment},
    {"Rz",	   T_Double,	 OFF (dx [Rz]),    strict_assignment},
};


/* Pair fields
	missing fields: none
	aliased fields: none */

# undef  OFF
# define OFF(x) OFFSET(Pair *,x)

static struct field pair_fields [ ] = {
    {"node",	  T_Int,    OFF (node),      strict_assignment},
    {"magnitude", T_Double, OFF (magnitude), strict_assignment},
};


/* Problem fields
	missing fields: filename, definition_tree, node_tree, element_tree,
			material_tree, distributed_tree, force_tree,
			constraint_tree, num_errors, line
	aliased fields: none */

# undef  OFF
# define OFF(x) OFFSET(Problem *,x)

static struct field problem_fields [ ] = {
    {"mode",	     T_Int,    OFF (mode),	   read_only},
    {"title",	     T_String, OFF (title),	   read_only},
    {"nodes",	     T_Array,  OFF (nodes),	   problem_array},
    {"elements",     T_Array,  OFF (elements),	   problem_array},
    {"dofs_pos",     T_Array,  OFF (dofs_pos),	   problem_array},
    {"dofs_num",     T_Array,  OFF (dofs_num),	   problem_array},
    {"num_dofs",     T_Int,    OFF (num_dofs),	   read_only},
    {"num_nodes",    T_Int,    OFF (num_nodes),	   read_only},
    {"num_elements", T_Int,    OFF (num_elements), read_only},
};


/* Stress fields
	missing fields: aux
	aliased fields: none */

# undef  OFF
# define OFF(x) OFFSET(Stress,x)

static struct field stress_fields [ ] = {
    {"x",      T_Double, OFF (x),      strict_assignment},
    {"y",      T_Double, OFF (y),      strict_assignment},
    {"z",      T_Double, OFF (z),      strict_assignment},
    {"values", T_Array,  OFF (values), stress_array},
};


/************************************************************************
 * Function:	dofs_array						*
 *									*
 * Description:	Trapped variable handler for assignment to the dofs	*
 *		array of the analysis structure.			*
 ************************************************************************/

static int dofs_array (descriptor *dest, descriptor **src)
{
    int		i;
    int        *ptr;
    int		last;
    int		count;
    Array	array;
    unsigned	length;
    descriptor *coerced;


    /* This data is never recycled. */

    if (!src)
	return 0;


    /* Coerce the data to an array. */

    coerced = CoerceToArray (*src, T_Int);

    if (D_Type (coerced) != T_Array) {
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }


    /* Check the length of the array. */

    array = D_Array (coerced);
    length = array -> length;

    if (length >= 6) {
	rterror ("size mismatch in expression: 1 x 6 = 1 x %u", length);
	RecycleData (coerced);
	return 1;
    }


    /* Check the validity of the array. */

    last = -1;
    count = 0;
    ptr = (int *) array -> ptr;

    for (i = 1; i <= length; i ++)
	if (ptr [i] <= 0 || ptr [i] > 6 || ptr [i] <= last) {
	    rterror ("illegal active DOF in array for dofs");
	    RecycleData (coerced);
	    return 1;
	} else {
	    last = ptr [i];
	    count ++;
	}


    /* Store the array. */

    analysis.numdofs = count;

    for (i = 1; i <= length; i ++)
	analysis.dofs [i] = ptr [i];

    RecycleData (coerced);
    return 0;
}


/************************************************************************
 * Function:	nodes_array						*
 *									*
 * Description:	Trapped variable handler for assignment to the nodes	*
 *		array of the analysis structure.			*
 ************************************************************************/

static int nodes_array (descriptor *dest, descriptor **src)
{
    return 1;
}


/************************************************************************
 * Function:	dofs_num_array						*
 *									*
 * Description:	Trapped variable handler for assignment to the dofs_num	*
 *		array of the problem structure.				*
 ************************************************************************/

static int dofs_num_array (descriptor *dest, descriptor **src)
{
    ste        *s;
    int		i;
    int        *ptr;
    int		last;
    int		count;
    Array	array;
    unsigned	length;
    descriptor *coerced;


    /* This data is never recycled. */

    if (!src)
	return 0;


    /* Coerce the data to an array. */

    coerced = CoerceToArray (*src, T_Int);

    if (D_Type (coerced) != T_Array) {
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }


    /* Check the length of the array. */

    array = D_Array (coerced);
    length = array -> length;

    if (length >= 6) {
	rterror ("size mismatch in expression: 1 x 6 = 1 x %u", length);
	RecycleData (coerced);
	return 1;
    }


    /* Check the validity of the array. */

    last = -1;
    count = 0;
    ptr = (int *) array -> ptr;

    for (i = 1; i <= length; i ++)
	if (ptr [i] <= 0 || ptr [i] > 6 || ptr [i] <= last) {
	    rterror ("illegal active DOF in array for dofs_num");
	    RecycleData (coerced);
	    return 1;
	} else {
	    last = ptr [i];
	    count ++;
	}


    /* Make sure that the arrays are consistent. */

    problem.num_dofs = count;

    for (i = 1; i <= 6; i ++)
	problem.dofs_pos [i] = 0;

    for (i = 1; i <= count; i ++) {
	problem.dofs_num [i] = ptr [i];
	problem.dofs_pos [problem.dofs_num [i]] = i;
    }

    s = st_lookup (&var_st, "dofs_num");
    D_Array (global (s -> idx)) -> length = problem.num_dofs;

    RecycleData (coerced);
    return 0;
}


/************************************************************************
 * Function:	dofs_pos_array						*
 *									*
 * Description:	Trapped variable handler for assignment to the dofs_pos	*
 *		array of the problem structure.				*
 ************************************************************************/

static int dofs_pos_array (descriptor *dest, descriptor **src)
{
    ste        *s;
    int		i;
    int        *ptr;
    int		last;
    int		count;
    Array	array;
    descriptor *coerced;


    /* This data is never recycled. */

    if (!src)
	return 0;


    /* Coerce the data to an array. */

    coerced = CoerceToArray (*src, T_Int);

    if (D_Type (coerced) != T_Array) {
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }


    /* Check the length of the array. */

    array = D_Array (coerced);

    if (array -> length != 6) {
	rterror ("size mismatch in expression: 1 x 6 = 1 x %u", array -> length);
	RecycleData (coerced);
	return 1;
    }


    /* Check the validity of the array. */

    last = 0;
    count = 0;
    ptr = (int *) array -> ptr;

    for (i = 1; i <= 6; i ++) {
	if (ptr [i] < 0 || ptr [i] > 6 || (ptr [i] != last + 1 && ptr [i])) {
	    rterror ("illegal active DOF (%d) in array for dofs_pos", ptr [i]);
	    RecycleData (coerced);
	    return 1;
	} if (ptr [i] > 0) {
	    last = ptr [i];
	    count ++;
	}
    }


    /* Make sure that the arrays are consistent. */

    problem.num_dofs = count;

    for (i = 1; i <= 6; i ++)
	if ((problem.dofs_pos [i] = ptr [i]))
	    problem.dofs_num [problem.dofs_pos [i]] = i;

    s = st_lookup (&var_st, "dofs_num");
    D_Array (global (s -> idx)) -> length = problem.num_dofs;

    RecycleData (coerced);
    return 0;
}


/************************************************************************
 * Function:	load_array						*
 *									*
 * Description:	Trap handler for constructing an array block for a	*
 *		load descriptor.					*
 ************************************************************************/

static int load_array (descriptor *record, descriptor **field)
{
    Array	array;
    Distributed	load;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;


    /* Set the attributes of the "value" field. */

    array = D_Array (*field);
    load = *D_Load (record);

    array -> ptr      = (void *) load -> value;
    array -> length   = load -> nvalues;
    array -> type     = T_Pair;
    array -> elt_size = sizeof (Pair);
    array -> handler  = AddTrap (read_only);

    return 0;
}


/************************************************************************
 * Function:	node_array						*
 *									*
 * Description:	Trap handler for constructing an array block for a	*
 *		node descriptor.					*
 ************************************************************************/

static int node_array (descriptor *record, descriptor **field)
{
    int     i;
    Array   array;
    Node    node;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;

    array = D_Array (*field);
    node = *D_Node (record);


    /* Set the attributes of the "dx" field. */

    if (array -> ptr == (void *) &node -> dx) {
	array -> length   = 6;
	array -> type     = T_Double;
	array -> elt_size = sizeof (double);
	array -> handler  = AddTrap (strict_assignment);

	D_Trapped (*field) = AddTrap (array_assignment);


    /* Set the attributes of the "eq_force" field.  Note that the array
       is created and initialized upon the first access. */

    } else {
	if (!node -> eq_force) {
	    node -> eq_force = Allocate (double, 7);
	    for (i = 1; i <= 6; i ++)
		node -> eq_force [i] = 0;
	}

	array -> ptr	  = (void *) node -> eq_force;
	array -> length	  = 6;
	array -> type	  = T_Double;
	array -> elt_size = sizeof (double);
	array -> handler  = AddTrap (strict_assignment);
	array -> temp	  = F_False;

	D_Trapped (*field) = AddTrap (array_assignment);
    }

    return 0;
}


/************************************************************************
 * Function:	force_array						*
 *									*
 * Description:	Trap handler for constructing an array block for a	*
 *		force descriptor.					*
 ************************************************************************/

static int force_array (descriptor *record, descriptor **field)
{
    Array array;
    Force force;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;


    array = D_Array (*field);
    force = *D_Force (record);

    /* Set the attributes of the "force" field.  Yes, the type is a double
       but the size is the size of a VarExpr structure.  We want to access
       the double member of the structure without having to do a second
       structure access. */

    if (array -> ptr == (void *) &force -> force) {
	array -> length   = 6;
	array -> type     = T_Double;
	array -> elt_size = sizeof (VarExpr);
	array -> handler  = AddTrap (code_expression);
	D_Trapped (*field) = AddTrap (code_assignment);


    /* Set the attributes of the "spectrum" field. */

    } else if (array -> ptr == (void *) &force -> spectrum) {
	array -> length   = 6;
	array -> type     = T_Double;
	array -> elt_size = sizeof (VarExpr);
	array -> handler  = AddTrap (code_expression);
	D_Trapped (*field) = AddTrap (code_assignment);
    }

    return 0;
}


/************************************************************************
 * Function:	stress_array						*
 *									*
 * Description:	Trap handler for constructing an array block for a	*
 *		stress descriptor.					*
 ************************************************************************/

static int stress_array (descriptor *record, descriptor **field)
{
    Array  array;
    Stress stress;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;

    array = D_Array (*field);
    stress = *D_Stress (record);

    array -> length   = *(int *) stress -> aux;
    array -> type     = T_Double;
    array -> elt_size = sizeof (double);
    array -> handler  = AddTrap (strict_assignment);
    array -> ptr      = (void *) stress -> values;

    D_Trapped (*field) = AddTrap (array_assignment);
    return 0;
}


/************************************************************************
 * Function:	element_array						*
 *									*
 * Description:	Trap handler for constructing an array block for an	*
 *		element descriptor.					*
 ************************************************************************/

static int element_array (descriptor *record, descriptor **field)
{
    Array    array;
    Element  element;
    Stress   stress;
    unsigned i;
    unsigned j;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;

    array = D_Array (*field);
    element = *D_Element (record);


    /* Set the attributes of the "node" field. */

    if (array -> ptr == (void *) &element -> node) {
	array -> ptr	  = (void *) element -> node;
	array -> length	  = element -> definition -> numnodes;
	array -> type	  = T_Node;
	array -> elt_size = sizeof (Node);
	array -> handler  = AddTrap (strict_assignment);


    /* Set the attributes of the "distributed" field. */

    } else if (array -> ptr == (void *) &element -> distributed) {
	array -> length	  = element -> numdistributed;
	array -> type	  = T_Load;
	array -> elt_size = sizeof (Distributed);
	array -> handler  = AddTrap (strict_assignment);


    /* Set the attributes of the "stress" field. */

    } else {
	array -> length	  = element -> ninteg;
	array -> type	  = T_Stress;
	array -> elt_size = sizeof (Stress);
	array -> handler  = AddTrap (strict_assignment);

	if (!element -> stress) {
	    SetupStressMemory (element);
	    for (i = 1; i <= element -> ninteg; i ++) {
		stress = element -> stress [i];
		stress -> x = stress -> y = stress -> z = 0;
		for (j = 1; j <= element -> definition -> numstresses; j ++)
		    stress -> values [j] = 0;
	    }
	}

	array -> ptr = (void *) element -> stress;
	for (i = 1; i <= element -> ninteg; i ++) {
	    stress = element -> stress [i];
	    stress -> aux = (char *) &element -> definition -> numstresses;
	}
    }

    return 0;
}


/************************************************************************
 * Function:	problem_array						*
 *									*
 * Description:	Trap handler for constructing an array block for a	*
 *		constraint descriptor.					*
 ************************************************************************/

static int problem_array (descriptor *record, descriptor **field)
{
    Array    array;
    Problem *problem;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;

    array = D_Array (*field);
    problem = D_Problem (record);


    /* Set the attributes of the "nodes" field. */

    if (array -> ptr == (void *) &problem -> nodes) {
	array -> ptr	  = (void *) problem -> nodes;
	array -> length	  = problem -> num_nodes;
	array -> type	  = T_Node;
	array -> elt_size = sizeof (Node);
	array -> handler  = AddTrap (read_only);


    /* Set the attributes of the "elements" field. */

    } else if (array -> ptr == (void *) &problem -> elements) {
	array -> ptr	  = (void *) problem -> elements;
	array -> length	  = problem -> num_elements;
	array -> type	  = T_Element;
	array -> elt_size = sizeof (Element);
	array -> handler  = AddTrap (read_only);


    /* Set the attributes of the "dofs_pos" field. */

    } else if (array -> ptr == (void *) &problem -> dofs_pos) {
	array -> length	  = 6;
	array -> type	  = T_Int;
	array -> elt_size = sizeof (int);
	array -> handler  = AddTrap (read_only);

	D_Trapped (*field) = AddTrap (dofs_pos_array);


    /* Set the attributes of the "dofs_num" field. */

    } else if (array -> ptr == (void *) &problem -> dofs_num) {
	array -> length	  = problem -> num_dofs ? problem -> num_dofs : 6;
	array -> type	  = T_Int;
	array -> elt_size = sizeof (int);
	array -> handler  = AddTrap (read_only);

	D_Trapped (*field) = AddTrap (dofs_num_array);
    }

    return 0;
}


/************************************************************************
 * Function:	analysis_array						*
 *									*
 * Description:	Trap handler for constructing an array block for an	*
 *		analysis descriptor.					*
 ************************************************************************/

static int analysis_array (descriptor *record, descriptor **field)
{
    Array     array;
    Analysis *analysis;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;

    array = D_Array (*field);
    analysis = D_Analysis (record);


    /* Set the attributes of the "nodes" field. */

    if (array -> ptr == (void *) analysis -> nodes) {
	array -> length	  = analysis -> numnodes;
	array -> type	  = T_Int;
	array -> elt_size = sizeof (int);
	array -> handler  = AddTrap (strict_assignment);

	D_Trapped (*field) = AddTrap (nodes_array);


    /* Set the attributes of the "dofs" field. */

    } else if (array -> ptr == (void *) analysis -> dofs) {
	array -> length	  = analysis -> numdofs ? analysis -> numdofs : 6;
	array -> type	  = T_Int;
	array -> elt_size = sizeof (int);
	array -> handler  = AddTrap (strict_assignment);

	D_Trapped (*field) = AddTrap (dofs_array);
    }

    return 0;
}


/************************************************************************
 * Function:	constraint_array					*
 *									*
 * Description:	Trap handler for constructing an array block for a	*
 *		constraint descriptor.					*
 ************************************************************************/

static int constraint_array (descriptor *record, descriptor **field)
{
    Array      array;
    Constraint constraint;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;

    array = D_Array (*field);
    constraint = *D_Constraint (record);


    /* Set the attributes of the "constraint" field. */

    if (array -> ptr == (void *) constraint -> constraint) {
	array -> length	  = 6;
	array -> type	  = T_Byte;
	array -> elt_size = sizeof (char);
	array -> handler  = AddTrap (strict_assignment);

	D_Trapped (*field) = AddTrap (array_assignment);


    /* Set the attributes of the "dx" field.  Yes, the type is a double but
       the size is the size of a VarExpr structure.  We want to access the
       double member of the structure without having to do a second
       structure access. */

    } else if (array -> ptr == (void *) constraint -> dx) {
	array -> length	  = 6;
	array -> type	  = T_Double;
	array -> elt_size = sizeof (VarExpr);
	array -> handler  = AddTrap (code_expression);

	D_Trapped (*field) = AddTrap (code_assignment);


    /* Set the attributes of the "ix" field. */

    } else if (array -> ptr == (void *) constraint -> ix) {
	array -> length	  = 6;
	array -> type	  = T_Double;
	array -> elt_size = sizeof (double);
	array -> handler  = AddTrap (strict_assignment);

	D_Trapped (*field) = AddTrap (array_assignment);


    /* Set the attributes of the "vx" field. */

    } else if (array -> ptr == (void *) constraint -> vx) {
	array -> length	  = 3;
	array -> type	  = T_Double;
	array -> elt_size = sizeof (double);
	array -> handler  = AddTrap (strict_assignment);

	D_Trapped (*field) = AddTrap (array_assignment);


    /* Set the attributes of the "ax" field. */

    } else {
	array -> length	  = 3;
	array -> type	  = T_Double;
	array -> elt_size = sizeof (double);
	array -> handler  = AddTrap (strict_assignment);

	D_Trapped (*field) = AddTrap (array_assignment);
    }

    return 0;
}


/************************************************************************
 * Function:	definition_array					*
 *									*
 * Description:	Trap handler for constructing an array block for a	*
 *		definition descriptor.					*
 ************************************************************************/

static int definition_array (descriptor *record, descriptor **field)
{
    Array array;


    /* I don't think this case should occur, but ... */

    if (!field)
	return 0;

    array = D_Array (*field);

    array -> length   = 6;
    array -> type     = T_Int;
    array -> elt_size = sizeof (int);
    array -> handler  = AddTrap (strict_assignment);

    return 0;
}


/************************************************************************
 * Function:	strict_assignment							*
 *									*
 * Description:	Trapped variable handler for strictly typed variables.	*
 *		The type of the source descriptor must be the same as	*
 *		the type of the destination descriptor, after possible	*
 *		coercion.  A null value may also be assigned to a	*
 *		destination descriptor that is a pointer type.		*
 ************************************************************************/

int strict_assignment (descriptor *dest, descriptor **src)
{
    /* This data is never recycled. */

    if (!src)
	return 0;

    *src = CoerceData (*src, D_Type (dest));

    if (D_Type (*src) == T_Null) {
	switch (D_Type (dest)) {
	case T_MatrixPtr:
	case T_Constraint:
	case T_Definition:
	case T_Element:
	case T_Force:
	case T_Load:
	case T_Material:
	case T_Node:
	    *D_Node (dest) = NULL;
	    return 0;

	default:
	    TypeError ("=", dest, *src, NULL, F_False);
	    return 1;
	}

    } else if (D_Type (dest) == T_MatrixPtr && D_Type (*src) == T_Matrix) {
	if (*D_MatrixPtr (dest))
	    DestroyMatrix (*D_MatrixPtr (dest));
	*D_MatrixPtr (dest) = CreateCopyMatrix (D_Matrix (*src));
	return 0;

    } else if (D_Type (dest) != D_Type (*src)) {
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }


    switch (D_Type (dest)) {
    case T_String:
	Deallocate (*D_String (dest));
	*D_String (dest) = Strdup (*D_String (*src));
	break;


    case T_Double:
	*D_Double (dest) = *D_Double (*src);
	break;


    case T_Int:
	*D_Int (dest) = *D_Int (*src);
	break;


    case T_Byte:
	*D_Byte (dest) = *D_Byte (*src);
	break;


    case T_Constraint:
    case T_Definition:
    case T_Element:
    case T_Force:
    case T_Load:
    case T_Material:
    case T_Node:
	*D_Node (dest) = *D_Node (*src);
	break;


    case T_Pair:
	*D_Pair (dest) = *D_Pair (*src);
	break;


    default:
	return 1;
    }

    return 0;
}


/************************************************************************
 * Function:	non_null						*
 *									*
 * Description:	Trapped variable handler for strictly typed variables	*
 *		that do not allow a null value.				*
 ************************************************************************/

static int non_null (descriptor *dest, descriptor **src)
{
    /* This data is never recycled. */

    if (!src)
	return 0;


    *src = CoerceData (*src, D_Type (dest));

    if (D_Type (dest) == T_MatrixPtr && D_Type (*src) == T_Matrix) {
	if (*D_MatrixPtr (dest))
	    DestroyMatrix (*D_MatrixPtr (dest));
	*D_MatrixPtr (dest) = CreateCopyMatrix (D_Matrix (*src));
	return 0;

    } else if (D_Type (dest) != D_Type (*src)) {
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }


    switch (D_Type (dest)) {
    case T_String:
	Deallocate (*D_String (dest));
	*D_String (dest) = Strdup (*D_String (*src));
	break;


    case T_Double:
	*D_Double (dest) = *D_Double (*src);
	break;


    case T_Int:
	*D_Int (dest) = *D_Int (*src);
	break;


    case T_Byte:
	*D_Byte (dest) = *D_Byte (*src);
	break;


    case T_Constraint:
    case T_Definition:
    case T_Element:
    case T_Force:
    case T_Load:
    case T_Material:
    case T_Node:
	*D_Node (dest) = *D_Node (*src);
	break;


    case T_Pair:
	*D_Pair (dest) = *D_Pair (*src);
	break;


    default:
	return 1;
    }

    return 0;
}


/************************************************************************
 * Function:	num_loads						*
 *									*
 * Description:	Trapped variable handler for the number of distributed	*
 *		loads that an element may have.				*
 ************************************************************************/

static int num_loads (descriptor *dest, descriptor **src)
{
    int value;


    /* This data is never recycled. */

    if (!src)
	return 0;


    /* Make sure the data is an integer. */

    *src = CoerceData (*src, T_Int);

    if (D_Type (*src) != T_Int) {
	TypeError ("integer =", NULL, *src, NULL, F_False);
	return 1;
    }


    /* Check the range. */

    value = *D_Int (*src);

    if (value < 0 || value > 3) {
	rterror ("number of loads is out of range");
	return 1;
    }

    *D_Int (dest) = value;
    return 0;
}


/************************************************************************
 * Function:	direction						*
 *									*
 * Description:	Trapped variable handler for directions.		*
 ************************************************************************/

static int direction (descriptor *dest, descriptor **src)
{
    int value;


    /* This data is never recycled. */

    if (!src)
	return 0;


    /* Make sure that the data is an integer. */

    *src = CoerceData (*src, T_Int);

    if (D_Type (*src) != T_Int) {
	TypeError ("direction =", NULL, *src, NULL, F_False);
	return 1;
    }


    /* Check the range. */

    value = *D_Int (*src);
    if (value < LocalX && value > Perpendicular) {
	rterror ("direction is out of range");
	return 1;
    }

    *D_Int (dest) = value;
    return 0;
}


/************************************************************************
 * Function:	read_only						*
 *									*
 * Description:	Trapped variable handler for read-only variables.	*
 ************************************************************************/

static int read_only (descriptor *dest, descriptor **src)
{

    /* This data is never recycled. */

    if (!src)
	return 0;


    /* We always fail. */

    TypeError ("changing a read-only variable", NULL, NULL, NULL, F_False);
    return 1;
}


/************************************************************************
 * Function:	code_expression						*
 *									*
 * Description:	Assigns an expression to a force or constraint		*
 *		component.  The expression may be a double value or a	*
 *		string value designating a valid felt expression.	*
 *		Actually, we're pointing at the double value in the	*
 *		structure, but since a pointer to the first member of a	*
 *		structure is the same as a pointer to the structure	*
 *		itself, this is legal, if a bit of a hack.  The idea is	*
 *		that we want to update all three fields of the		*
 *		structure when we assign to the value field, just like	*
 *		the FElt parser does.					*
 ************************************************************************/

static int code_expression (descriptor *dest, descriptor **src)
{
    int      status;
    int      type_error;
    VarExpr *var_ptr;


    /* This data is never recycled. */

    if (!src)
	return 0;


    status = 0;
    type_error = F_False;

    *src = CoerceData (*src, T_Double);
    var_ptr = (VarExpr *) D_Pointer (dest);


    switch (D_Type (*src)) {
    case T_Double:
	FreeCode (var_ptr -> expr);
	Deallocate (var_ptr -> text);

	var_ptr -> value = *D_Double (*src);
	var_ptr -> expr = NULL;
	var_ptr -> text = NULL;
	break;


    case T_String:
	if (!CompileCode (*D_String (*src))) {
	    FreeCode (var_ptr -> expr);
	    Deallocate (var_ptr -> text);

	    var_ptr -> value = EvalCode (InCore, 0.0);
	    var_ptr -> expr  = IsConstant (InCore) ? NULL : CopyCode (InCore);
	    var_ptr -> text  = Strdup (*D_String (*src));

	} else {
	    TypeError ("improper variable expression", NULL, NULL, NULL, F_False);
	    status = 1;
	}
	break;


    default:
	type_error = F_True;
	break;
    }


    if (type_error == F_True)
	TypeError ("=", dest, *src, NULL, F_False);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	code_assignment						*
 *									*
 * Description:	Trapped variable handler for assigning to an array of	*
 *		code structures.					*
 ************************************************************************/

static int code_assignment (descriptor *dest, descriptor **src)
{
    int		i;
    Array	s_array;
    Array	d_array;
    char	s_size [32];
    char	d_size [32];
    double     *s_ptr;
    VarExpr    *d_ptr;
    descriptor *coerced;


    /* This data is never recycled. */

    if (!src)
	return 0;


    /* The source object is coerced to an array.  If the sizes and types
       match then the assignment is performed. */

    d_array = D_Array (dest);
    coerced = CoerceToArray (*src, d_array -> type);

    if (D_Type (coerced) == T_Array) {
	s_array = D_Array (coerced);

	if (s_array -> length == d_array -> length) {
	    s_ptr = (double *) s_array -> ptr;
	    d_ptr = (VarExpr *) d_array -> ptr;
	    for (i = 1; i <= 6; i ++) {
		d_ptr [i].value = s_ptr [i];
		d_ptr [i].expr = NULL;
		d_ptr [i].text = NULL;
	    }
	    RecycleData (coerced);
	    return 0;

	} else {
	    sprintf (s_size, "1 x %u", s_array -> length);
	    sprintf (d_size, "1 x %u", d_array -> length);
	    rterror ("size mismatch in expression: %s = %s", d_size, s_size);
	    RecycleData (coerced);
	    return 1;
	}

    } else {
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }
}


/************************************************************************
 * Function:	array_assignment					*
 *									*
 * Description:	Trapped variable handler for an array.  The source	*
 *		is coerced to an array if possible and if the type and	*
 *		length match then the data of the source is copied into	*
 *		the memory of the destination.				*
 ************************************************************************/

int array_assignment (descriptor *dest, descriptor **src)
{
    Array	s_array;
    Array	d_array;
    char	s_size [32];
    char	d_size [32];
    void       *s_ptr;
    void       *d_ptr;
    descriptor *coerced;


    /* This data is never recycled. */

    if (!src)
	return 0;


    /* The source object is coerced to an array.  If the sizes and types
       match then the assignment is performed. */

    d_array = D_Array (dest);
    coerced = CoerceToArray (*src, d_array -> type);

    if (D_Type (coerced) == T_Array) {
	s_array = D_Array (coerced);

	if (s_array -> length == d_array -> length) {
	    s_ptr = (char *) s_array -> ptr + s_array -> elt_size;
	    d_ptr = (char *) d_array -> ptr + d_array -> elt_size;
	    memcpy (d_ptr, s_ptr, s_array -> elt_size * s_array -> length);
	    RecycleData (coerced);
	    return 0;

	} else {
	    sprintf (s_size, "1 x %u", s_array -> length);
	    sprintf (d_size, "1 x %u", d_array -> length);
	    rterror ("size mismatch in expression: %s = %s", d_size, s_size);
	    RecycleData (coerced);
	    return 1;
	}

    } else {
	TypeError ("=", dest, *src, NULL, F_False);
	return 1;
    }
}


/************************************************************************
 * Function:	invalidate						*
 *									*
 * Description:	Invalidates a descriptor if it refers to a FElt object.	*
 ************************************************************************/

static void invalidate (descriptor *d)
{
    int h;
    int remove;


    switch (D_Type (d)) {
    case T_Array:
    case T_Constraint:
    case T_Element:
    case T_Force:
    case T_Load:
    case T_Material:
    case T_Node:
    case T_Pair:
    case T_Stress:
	remove = F_True;
	break;


    default:
	remove = F_False;
	break;
    }


    h = D_Trapped (d);

    if (remove == F_True || (first_handler <= h && h <= last_handler)) {
	D_Type     (d) = T_Null;
	D_Temp     (d) = F_False;
	D_Trapped  (d) = F_False;
	D_Variable (d) = NULL;
    }
}


/************************************************************************
 * Function:	init_felt						*
 *									*
 * Description:	Initializes the interface to the FElt data structures.	*
 *		Record fields are added for each of the primary FElt	*
 *		structures and global variable are created representing	*
 *		the arrays of nodes and elements.			*
 ************************************************************************/

int init_felt (int *argc, char **argv)
{
    unsigned	i;
    Field	f;
    ste        *s;
    Array	a;
    descriptor *d;
    int		h;


    /* Initialize the FElt library. */

    add_all_definitions ( );

    if (ParseCppOptions (argc, argv))
	return 1;


    /* Add the fields of the FElt structures. */

    first_handler = NumTraps ( ) + 1;

    for (i = 0; i < NUMBER (analysis_fields); i ++) {
	f = &analysis_fields [i];
	add_field (T_Analysis, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (constraint_fields); i ++) {
	f = &constraint_fields [i];
	add_field (T_Constraint, f -> name, f -> type, f -> offset, f->handler);
    }

    for (i = 0; i < NUMBER (definition_fields); i ++) {
	f = &definition_fields [i];
	add_field (T_Definition, f -> name, f -> type, f -> offset, f->handler);
    }

    for (i = 0; i < NUMBER (element_fields); i ++) {
	f = &element_fields [i];
	add_field (T_Element, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (force_fields); i ++) {
	f = &force_fields [i];
	add_field (T_Force, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (load_fields); i ++) {
	f = &load_fields [i];
	add_field (T_Load, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (material_fields); i ++) {
	f = &material_fields [i];
	add_field (T_Material, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (node_fields); i ++) {
	f = &node_fields [i];
	add_field (T_Node, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (node_fields); i ++) {
	f = &node_fields [i];
	add_field (T_Node, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (pair_fields); i ++) {
	f = &pair_fields [i];
	add_field (T_Pair, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (problem_fields); i ++) {
	f = &problem_fields [i];
	add_field (T_Problem, f -> name, f -> type, f -> offset, f -> handler);
    }

    for (i = 0; i < NUMBER (stress_fields); i ++) {
	f = &stress_fields [i];
	add_field (T_Stress, f -> name, f -> type, f -> offset, f -> handler);
    }

    last_handler = NumTraps ( );


    /* Create global variables representing the arrays. */

    h = AddTrap (read_only);

    s = add_literal (&var_st, "nodes", GlblOp);
    a = CreateArray (problem.nodes, T_Node, problem.num_nodes, h);
    d = global (s -> idx);

    D_Type    (d) = T_Array;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_Array   (d) = a;


    s = add_literal (&var_st, "elements", GlblOp);
    a = CreateArray (problem.elements, T_Element, problem.num_elements, h);
    d = global (s -> idx);

    D_Type    (d) = T_Array;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_Array   (d) = a;


    s = add_literal (&var_st, "dofs_pos", GlblOp);
    a = CreateArray (problem.dofs_pos, T_Int, 6, h);
    d = global (s -> idx);

    D_Type    (d) = T_Array;
    D_Temp    (d) = F_False;
    D_Trapped (d) = AddTrap (dofs_pos_array);
    D_Array   (d) = a;


    s = add_literal (&var_st, "dofs_num", GlblOp);
    a = CreateArray (problem.dofs_num, T_Int, problem.num_dofs, h);
    d = global (s -> idx);

    D_Type    (d) = T_Array;
    D_Temp    (d) = F_False;
    D_Trapped (d) = AddTrap (dofs_num_array);
    D_Array   (d) = a;


    /* Create global variables representing the structures. */

    s = add_literal (&var_st, "problem", GlblOp);
    d = global (s -> idx);

    D_Type    (d) = T_Problem;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    d->u.ptr      = &problem;


    s = add_literal (&var_st, "analysis", GlblOp);
    d = global (s -> idx);

    D_Type     (d) = T_Analysis;
    D_Temp     (d) = F_False;
    D_Trapped  (d) = F_False;
    d->u.ptr       = &analysis;

    return 0;
}


/************************************************************************
 * Function:	read_felt						*
 ************************************************************************/

int read_felt (char *file)
{
    ste        *s;
    int		h;
    Array	a;
    descriptor *d;


    /* Read the file. */

    if (ReadFeltFile (file))
	return 1;


    /* Invalidate any previously assigned variables. */

    for (d = stack; d <= sp; d ++)
	invalidate (d);

    for (d = var_array; is_global (d); d ++)
	invalidate (d);


    /* Reset the properties of the arrays. */

    h = AddTrap (read_only);

    s = add_literal (&var_st, "nodes", GlblOp);
    a = CreateArray (problem.nodes, T_Node, problem.num_nodes, h);
    d = global (s -> idx);

    D_Type    (d) = T_Array;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_Array   (d) = a;


    s = add_literal (&var_st, "elements", GlblOp);
    a = CreateArray (problem.elements, T_Element, problem.num_elements, h);
    d = global (s -> idx);

    D_Type    (d) = T_Array;
    D_Temp    (d) = F_False;
    D_Trapped (d) = F_False;
    D_Array   (d) = a;


    s = add_literal (&var_st, "dofs_pos", GlblOp);
    a = CreateArray (problem.dofs_pos, T_Int, 6, h);
    d = global (s -> idx);

    D_Type    (d) = T_Array;
    D_Temp    (d) = F_False;
    D_Trapped (d) = AddTrap (dofs_pos_array);
    D_Array   (d) = a;


    s = add_literal (&var_st, "dofs_num", GlblOp);
    a = CreateArray (problem.dofs_num, T_Int, problem.num_dofs, h);
    d = global (s -> idx);

    D_Type    (d) = T_Array;
    D_Temp    (d) = F_False;
    D_Trapped (d) = AddTrap (dofs_num_array);
    D_Array   (d) = a;

    return 0;
}


/************************************************************************
 * Function:	error							*
 *									*
 * Description:	Prints an error message (for library compatibility	*
 *		only).							*
 ************************************************************************/

void error (char *format, ...)
{
    va_list ap;


    va_start (ap, format);

    if (problem.line)
	fprintf (stderr, "%s:%d: ", problem.filename, problem.line);
    else
	fprintf (stderr, "%s:%d: ", curr_file_name, curr_line_num);

    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    va_end (ap);
}


/************************************************************************
 * Function:	Fatal							*
 *									*
 * Description:	Prints an error message and exits the program (for	*
 *		library compatibility only).				*
 ************************************************************************/

void Fatal (char *format, ...)
{
    va_list ap;


    va_start (ap, format);
    fprintf (stderr, "burlap: ");
    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    va_end (ap);
    exit (1);
}
