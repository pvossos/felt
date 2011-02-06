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
 * File:	problem.c						*
 *									*
 * Description:	This file contains the public and private function	*
 *		definitions for the creation of a problem instance.	*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include <unistd.h>
# include "error.h"
# include "problem.h"
# include "allocate.h"
# include "definition.h"

# define streq(a,b)	!strcmp(a,b)
# define strneq(a,b)	strcmp(a,b)

# ifndef LIBDIR
# define LIBDIR "/usr/local/lib/felt"
# endif

# ifndef CPP
# define CPP "/lib/cpp"
# endif

int felt_yyparse (void);

Problem    problem;
ProblemSource psource;
Analysis   analysis;
Appearance appearance;

static struct material default_material = {NULL, "default_material"};
static struct constraint default_constraint = {NULL, "default_constraint"};

static char *cpp;
static char  cpp_command [2048];

Definition
defnlookup(char *name)
{
    char      *ptr;
    Definition definition;


    ptr = name;
    while (*ptr != ' ' && *ptr != 0) {
	ptr ++;
    }

    *ptr = 0;
    if (!(definition = LookupDefinition (name)))
	error ("%s elements have no definition", name);

    return definition;
}


/************************************************************************
 * Function:	node_cmp						*
 *									*
 * Description:	Compares two nodes by number.				*
 ************************************************************************/

static int
node_cmp(Item n1, Item n2)
{
    return ((Node) n1) -> number - ((Node) n2) -> number;
}


/************************************************************************
 * Function:	element_cmp						*
 *									*
 * Description:	Compares two elements by number.			*
 ************************************************************************/

static int
element_cmp(Item e1, Item e2)
{
    return ((Element) e1) -> number - ((Element) e2) -> number;
}


/************************************************************************
 * Function:	material_cmp						*
 *									*
 * Description:	Compares two materials by name.				*
 ************************************************************************/

static int 
material_cmp(Item m1, Item m2)
{
    return strcmp (((Material) m1) -> name, ((Material) m2) -> name);
}


/************************************************************************
 * Function:	distributed_cmp						*
 *									*
 * Description:	Compares two distributed loads by name.			*
 ************************************************************************/

static int 
distributed_cmp(Item d1, Item d2)
{
    return strcmp (((Distributed) d1) -> name, ((Distributed) d2) -> name);
}


/************************************************************************
 * Function:	force_cmp						*
 *									*
 * Description:	Compares two forces by name.				*
 ************************************************************************/

static int 
force_cmp(Item f1, Item f2)
{
    return strcmp (((Force) f1) -> name, ((Force) f2) -> name);
}


/************************************************************************
 * Function:	constraint_cmp						*
 *									*
 * Description:	Compares two constraints by name.			*
 ************************************************************************/

static int
constraint_cmp(Item c1, Item c2)
{
    return strcmp (((Constraint) c1) -> name, ((Constraint) c2) -> name);
}


/************************************************************************
 * Function:	loadcase_cmp						*
 *									*
 * Description:	Compares two load cases by name.			*
 ************************************************************************/

static int 
loadcase_cmp(Item lc1, Item lc2)
{
    return strcmp (((LoadCase) lc1) -> name.c_str(), ((LoadCase) lc2) -> name.c_str());
}


/************************************************************************
 * Function:	resolve_node						*
 *									*
 * Description:	Resolve the names of objects for a node.		*
 ************************************************************************/

static int 
resolve_node(Item item)
{
    struct force      f;
    struct constraint c;
    unsigned	      number;
    Tree	      tree;
    Node	      node;



    /* Store the node in the array. */

    node = (Node) item;
    number = node -> number;
    problem.nodes [number] = node;


    /* Resolve the constraint. */

    tree = problem.constraint_tree;
    c.name = (char *) node -> constraint;

    if (c.name) {
	node -> constraint = (Constraint) TreeSearch (tree, &c);

	if (!node -> constraint)
	    error ("node %u used undefined constraint %s", number, c.name);

	Deallocate (c.name);
    } else
	node -> constraint = &default_constraint;


    /* Resolve the force. */

    tree = problem.force_tree;
    f.name = (char *) node -> force;

    if (f.name) {
	node -> force = (Force) TreeSearch (tree, &f);

	if (!node -> force)
	    error ("node %u uses undefined force %s", number, f.name);

	Deallocate (f.name);
    }

    return 0;
}


/************************************************************************
 * Function:	resolve_element						*
 *									*
 * Descriptin:	Resolve the names of objects for an element.		*
 ************************************************************************/

static int
resolve_element(Item item)
{
    struct distributed d;
    struct material    m;
    unsigned	       i;
    unsigned	       number;
    Tree	       tree;
    Element	       element;


    /* Store the element in the array. */

    element = (Element) item;
    number = element -> number;
    problem.elements [number] = element;



    /* Resolve the material. */

    tree = problem.material_tree;
    m.name = (char *) element -> material;

    if (m.name) {
	element -> material = (Material) TreeSearch (tree, &m);

	if (!element -> material)
	    error ("element %u uses undefined material %s", number, m.name);

	Deallocate (m.name);
    } else
	element -> material = &default_material;


    /* Resolve the loads. */

    tree = problem.distributed_tree;

    for (i = 1; i <= element -> numdistributed; i ++) {
	d.name = (char *) element -> distributed [i];
	element -> distributed [i] = (Distributed) TreeSearch (tree, &d);

	if (!element -> distributed)
	    error ("element %u used undefined load %s", number, d.name);

	Deallocate (d.name);
    }


    /* Set the pointers to the nodes. */

    if (element -> definition)
	for (i = 1; i <= element -> definition -> numnodes; i ++)
	    if (element -> node [i])
		element -> node [i] = problem.nodes [(int) element -> node [i]];

    return 0;
}

static int case_count;

/************************************************************************
 * Function:	resolve_loadcase					*	
 *									*
 * Description:	Resolve the names of objects for a loadcase.		*
 ************************************************************************/

static int
resolve_loadcase(Item item)
{
    struct force       f;
    struct node	       n;
    struct distributed l;
    struct element     e;
    LoadCase	       loadcase;
    unsigned	       i;


    /* Store the loadcase in the array. */

    loadcase = (LoadCase) item;
    problem.loadcases [case_count] = loadcase;

    for (i = 1 ; i <= loadcase->forces.size(); i++) {
       f.name = (char *) loadcase -> forces [i];
       n.number = (unsigned) loadcase -> nodes [i];

       loadcase -> nodes [i] = (Node) TreeSearch (problem.node_tree, &n);
       if (!loadcase -> nodes [i])
           error ("load case %s used undefined node %d", loadcase->name.c_str(), n.number);

       loadcase -> forces [i] = (Force) TreeSearch (problem.force_tree, &f);
       if (!loadcase -> forces [i])
           error ("load case %s used undefined force %s", loadcase->name.c_str(), f.name);

       Deallocate (f.name);
    }

    for (i = 1 ; i <= loadcase->loads.size(); i++) {
       l.name = (char *) loadcase -> loads [i];
       e.number = (unsigned) loadcase -> elements [i];

       loadcase -> elements [i] = (Element) TreeSearch (problem.element_tree, &e);
       if (!loadcase -> elements [i])
           error ("load case %s used undefined element %d", loadcase->name.c_str(), e.number);

       loadcase -> loads [i] = (Distributed) TreeSearch (problem.distributed_tree, &f);
       if (!loadcase -> loads [i])
           error ("load case %s used undefined load %s", loadcase->name.c_str(), l.name);

       Deallocate (l.name);
    }

    case_count ++;

    return 0;
}

/************************************************************************
 * Function:	resolve_names						*
 *									*
 * Description:	Resolves the names and numbers of objects making sure	*
 *		all that are specified are actually defined.  We've	*
 *		actually cheated and stored the name or number of the	*
 *		object instead of a pointer to the object in the node	*
 *		and element structures.  This allows nodes and elements	*
 *		to reference objects before they are defined.  We fix	*
 *		those pointers here.					*
 ************************************************************************/

static void
resolve_names(void)
{
    unsigned      i;
    struct node   n;


    if (!problem.nodes.empty()) {

        TreeSetIterator (problem.node_tree, resolve_node);
        TreeIterate (problem.node_tree);
        
        for (i = 1; i <= problem.nodes.size(); i ++)
            if (!problem.nodes [i])
                error ("node %u is not defined", i);
    }

    if (!problem.elements.empty()) {

        TreeSetIterator (problem.element_tree, resolve_element);
        TreeIterate (problem.element_tree);

        for (i = 1; i <= problem.elements.size(); i ++)
            if (!problem.elements [i])
                error ("element %u is not defined", i);
    }

    problem.loadcases.resize(TreeSize(problem.loadcase_tree), NULL);
    
    if (!problem.loadcases.empty()) {

        case_count = 1;
        TreeSetIterator (problem.loadcase_tree, resolve_loadcase);
        TreeIterate (problem.loadcase_tree);
    }

	/*
	 * resolve any node references given in the analysis parameters
	 */

    if (!analysis.nodes.empty()) {
        for (i = 1 ; i <= analysis.nodes.size() ; i++) {
            n.number = (unsigned) analysis.nodes [i];
            analysis.nodes [i] = (Node) TreeSearch (problem.node_tree, &n);
            if (analysis.nodes [i] == NULL)
                error ("analysis node %d not defined", n.number);
        }
    }

    if (analysis.input_node) {
        n.number = (unsigned) analysis.input_node;
        analysis.input_node = (Node) TreeSearch (problem.node_tree, &n);
        if (analysis.input_node == NULL)
            error ("analysis input node %d not defined", n.number);
    }
}

int
ReadFeltFile(const char *filename)
{
    unsigned i;
    char     buffer [2048];
    const char    *plural;
    FILE    *input;


    /* Open the file and send it through the preprocessor. */

    if (filename) {

	if (cpp != NULL) {
	    if (streq (filename, "-"))
		sprintf (buffer, "%s", cpp_command);
	    else {
		if (access (filename, R_OK)) {
		    error ("Unable to open %s", filename);
		    return 1;
		}
		sprintf (buffer, "%s %s", cpp_command, filename);
	    }

	    if (!(input = popen (buffer, "r"))) {
		error ("Unable to execute %s", cpp);
		return 1;
	    }

	} else

	{
	    if (streq (filename, "-"))
		input = stdin;
	    else if (!(input = fopen (filename, "r"))) {
		error ("Unable to open %s", filename);
		return 1;
	    }
	}
    } else
	input = NULL;


    /* Initialize the problem instance. */

    problem.mode	     = Static;
    problem.title	     = strdup ("");
    problem.num_dofs	     = 0;
    problem.loadcases.clear();
    problem.num_errors	     = 0;
    psource.line	     = 1;
    problem.nodes.clear();
    problem.elements.clear();
    problem.node_tree	     = TreeCreate (node_cmp);
    problem.element_tree     = TreeCreate (element_cmp);
    problem.material_tree    = TreeCreate (material_cmp);
    problem.distributed_tree = TreeCreate (distributed_cmp);
    problem.force_tree	     = TreeCreate (force_cmp);
    problem.constraint_tree  = TreeCreate (constraint_cmp);
    problem.loadcase_tree    = TreeCreate (loadcase_cmp);

    if (filename)
	psource.filename = strdup (streq (filename, "-") ? "stdin" : filename);
    else
	psource.filename = strdup ("");


    /* Initialize the analysis structure. */

    analysis.start     = 0.0;
    analysis.step      = 0.0;
    analysis.stop      = 0.0;
    analysis.gamma     = 0.0;
    analysis.beta      = 0.0;
    analysis.alpha     = 0.0;
    analysis.Rk	       = 0.0;
    analysis.Rm        = 0.0;
    analysis.iterations = 0;
    analysis.load_steps = 0;
    analysis.tolerance = 0.0;
    analysis.relaxation = 0.0;
    analysis.mass_mode = 0;
    analysis.nodes.clear();
    analysis.numdofs   = 0;
    analysis.input_node = NULL;
    analysis.input_dof = 0;

    for (i = 1 ; i <= 3 ; i++)
       analysis.gravity [i] = 0.0;


    /* Initialize the appearance structure. */

    InitAppearance ( );


    /* Parse the input and resolve the names. */

    if (filename) {
	init_felt_lexer (input);
	felt_yyparse ( );
	psource.line = 0;

	if (cpp)
	    pclose (input);
	else if (input != stdin)
	    fclose (input);

	if (!problem.num_errors)
	    resolve_names ( );


	/* Report any errors. */

	if (problem.num_errors) {
	    plural = problem.num_errors != 1 ? "errors" : "error";
	    error ("%u %s found in input", problem.num_errors, plural);
	    return problem.num_errors;
	}
    }

    return 0;
}

AnalysisType
SetAnalysisMode(void)
{
    if (problem.mode == Static && problem.loadcases.size() > 0)
        return StaticLoadCases;
    else if (problem.mode == Static && 
             (analysis.input_node || analysis.input_dof))
        return StaticLoadRange;
    else if (problem.mode == StaticSubstitution && 
             (analysis.input_node || analysis.input_dof)) 
       return StaticSubstitutionLoadRange;
    else if (problem.mode == StaticIncremental && 
             (analysis.input_node || analysis.input_dof)) 
       return StaticSubstitutionLoadRange;

    return problem.mode;        
}

int
ParseCppOptions(int *argc, char **argv)
{
    int   i;
    int   j;
    char *arg;
    char  cpp_args [2048];


    j = 1;
    cpp = CPP;
    cpp_args [0] = 0;

    for (i = 1; i < *argc; i ++)
	if ((arg = argv [i]) [0] != '-') {
	    argv [j ++] = arg;
	} else if (streq (arg, "-nocpp")) {
	    cpp = NULL;
	} else if (streq (arg, "-cpp")) {
	    if (++ i == *argc)
		return 1;
	    cpp = argv [i];
	} else if (arg [1] == 'D' || arg [1] == 'U' || arg [1] == 'I') {
	    strcat (cpp_args, " '");
	    strcat (cpp_args, arg);
	    strcat (cpp_args, "'");
	} else
	    argv [j ++] = arg;

    if (cpp)
	sprintf (cpp_command, "%s -I%s %s", cpp, LIBDIR, cpp_args);

    argv [*argc = j] = NULL;
    return 0;
}

int
CompileCode(char *text)
{
    char input [2048];


    sprintf (input, "x = %s end", text);

    init_felt_lexer (NULL);
    psource.input = input;
    return felt_yyparse ( );
}

double
exptod(char *expr, char **ptr)
{
    Code code;


    code = CompileCode (expr) ? NULL : InCore;

    if (ptr)
	*ptr = psource.input;

    return EvalCode (code, 0.0);
}

void
InitAppearance(void)
{
    unsigned i;


    appearance.node_numbers    = UnspecifiedValue;
    appearance.element_numbers = UnspecifiedValue;
    appearance.snap	       = UnspecifiedValue;
    appearance.grid	       = UnspecifiedValue;
    appearance.snap_size       = UnspecifiedValue;
    appearance.grid_size       = UnspecifiedValue;
    appearance.x_min	       = UnspecifiedValue;
    appearance.x_max	       = UnspecifiedValue;
    appearance.y_min	       = UnspecifiedValue;
    appearance.y_max	       = UnspecifiedValue;
    appearance.x_pos	       = UnspecifiedValue;
    appearance.y_pos	       = UnspecifiedValue;
    appearance.scale	       = UnspecifiedValue;
    appearance.width	       = UnspecifiedValue;
    appearance.height	       = UnspecifiedValue;
    appearance.num_figures     = 0;

    for (i = 0; i < appearance.num_figures; i ++) {
	Deallocate (appearance.figures [i].color);
	Deallocate (appearance.figures [i].text);
	Deallocate (appearance.figures [i].font);
	Deallocate (appearance.figures [i].points);
    }

    Deallocate (appearance.figures);
    Deallocate (appearance.label_font);
    Deallocate (appearance.node_color);
    Deallocate (appearance.element_color);

    appearance.figures	     = NULL;
    appearance.label_font    = NULL;
    appearance.node_color    = NULL;
    appearance.element_color = NULL;
    appearance.tool_color    = NULL;
    appearance.tool_font     = NULL;
}
