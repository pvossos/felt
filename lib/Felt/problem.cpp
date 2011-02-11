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

# include <algorithm>
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
    return strcmp (((Material) m1) -> name.c_str(), ((Material) m2) -> name.c_str());
}


/************************************************************************
 * Function:	distributed_cmp						*
 *									*
 * Description:	Compares two distributed loads by name.			*
 ************************************************************************/

static int 
distributed_cmp(Item d1, Item d2)
{
    return strcmp (((Distributed) d1) -> name.c_str(), ((Distributed) d2) -> name.c_str());
}


/************************************************************************
 * Function:	force_cmp						*
 *									*
 * Description:	Compares two forces by name.				*
 ************************************************************************/

static int 
force_cmp(Item f1, Item f2)
{
    return strcmp (((Force) f1) -> name.c_str(), ((Force) f2) -> name.c_str());
}


/************************************************************************
 * Function:	constraint_cmp						*
 *									*
 * Description:	Compares two constraints by name.			*
 ************************************************************************/

static int
constraint_cmp(Item c1, Item c2)
{
    return strcmp (((Constraint) c1) -> name.c_str(), ((Constraint) c2) -> name.c_str());
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
resolve_node(Node node)
{
    struct force      f;
    struct constraint c;
    unsigned	      number;
    Tree	      tree;
    char *buf;

    /* Store the node in the array. */

    number = node -> number;
    problem.nodes [number] = node;


    /* Resolve the constraint. */

    tree = problem.constraint_tree;
    buf = (char *) node->constraint;

    if (buf) {
        c.name = buf;
        node -> constraint = (Constraint) TreeSearch (tree, &c);
        
        if (!node -> constraint)
            error ("node %u used undefined constraint %s", number, c.name.c_str());
        
        c.name = "";
    } else
	node -> constraint = &default_constraint;


    /* Resolve the force. */

    buf = (char *) node -> force;

    if (buf) {
        f.name = buf;
        Problem::ForceSet::iterator it = problem.force_set.find(&f);
        node -> force = it != problem.force_set.end() ? *it : NULL;
        
        if (!node -> force)
            error ("node %u uses undefined force %s", number, f.name.c_str());
        
        f.name = "";
    }

    return 0;
}


/************************************************************************
 * Function:	resolve_element						*
 *									*
 * Descriptin:	Resolve the names of objects for an element.		*
 ************************************************************************/

static int
resolve_element(Element element)
{
    struct distributed d;
    struct material    m;
    unsigned	       i;
    unsigned	       number;
    char *buf;

    /* Store the element in the array. */

    number = element -> number;
    problem.elements [number] = element;



    /* Resolve the material. */

    m.name = element->material->name;
    delete element->material;

    if (!m.name.empty()) {
        Problem::MaterialSet::iterator it = problem.material_set.find(&m);
        if (it == problem.material_set.end())
            error ("element %u uses undefined material %s", number, m.name.c_str());
        element->material = *it;
    } else
        element -> material = &default_material;


    /* Resolve the loads. */

    for (i = 1; i <= element -> numdistributed; i ++) {
        d.name = (char *) element -> distributed [i];
        Problem::DistributedSet::iterator it = problem.distributed_set.find(&d);
        if (it == problem.distributed_set.end())
            error ("element %u used undefined load %s", number, d.name.c_str());
        element -> distributed [i] = *it;
        d.name = "";
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
resolve_loadcase(LoadCase loadcase)
{
    struct force       f;
    struct node	       n;
    struct distributed l;
    struct element     e;
    unsigned	       i;


    /* Store the loadcase in the array. */

    problem.loadcases [case_count] = loadcase;

    for (i = 1 ; i <= loadcase->forces.size(); i++) {
       f.name = (char *) loadcase -> forces [i];
       n.number = (unsigned) loadcase -> nodes [i];

       Problem::NodeSet::iterator it = problem.node_set.find(&n);
       loadcase -> nodes [i] = it != problem.node_set.end() ? *it : NULL;
       if (!loadcase -> nodes [i])
           error ("load case %s used undefined node %d", loadcase->name.c_str(), n.number);

       Problem::ForceSet::iterator itf = problem.force_set.find(&f);
       loadcase -> forces [i] = itf != problem.force_set.end() ? *itf : NULL;
       if (!loadcase -> forces [i])
           error ("load case %s used undefined force %s", loadcase->name.c_str(), f.name.c_str());

       f.name = "";
    }

    for (i = 1 ; i <= loadcase->loads.size(); i++) {
       l.name = (char *) loadcase -> loads [i];
       e.number = (unsigned) loadcase -> elements [i];

       Problem::ElementSet::iterator it = problem.element_set.find(&e);
       loadcase -> elements [i] = it != problem.element_set.end() ? *it : NULL;
       if (!loadcase -> elements [i])
           error ("load case %s used undefined element %d", loadcase->name.c_str(), e.number);

       Problem::DistributedSet::iterator itl = problem.distributed_set.find(&l);
       loadcase -> loads [i] = itl != problem.distributed_set.end() ? *itl : NULL;
       if (!loadcase -> loads [i])
           error ("load case %s used undefined load %s", loadcase->name.c_str(), l.name.c_str());

       l.name = "";
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

        std::for_each(problem.node_set.begin(), problem.node_set.end(), resolve_node);
        
        for (i = 1; i <= problem.nodes.size(); i ++)
            if (!problem.nodes [i])
                error ("node %u is not defined", i);
    }

    if (!problem.elements.empty()) {

        std::for_each(problem.element_set.begin(), problem.element_set.end(), resolve_element);

        for (i = 1; i <= problem.elements.size(); i ++)
            if (!problem.elements [i])
                error ("element %u is not defined", i);
    }

    problem.loadcases.resize(problem.loadcase_set.size(), NULL);
    
    if (!problem.loadcases.empty()) {

        case_count = 1;
        std::for_each(problem.loadcase_set.begin(), problem.loadcase_set.end(), resolve_loadcase);
    }

	/*
	 * resolve any node references given in the analysis parameters
	 */

    Problem::NodeSet::iterator it;
                
    if (!analysis.nodes.empty()) {
        for (i = 1 ; i <= analysis.nodes.size() ; i++) {
            n.number = (unsigned) analysis.nodes [i];
            it = problem.node_set.find(&n);
            analysis.nodes [i] = it != problem.node_set.end() ? *it : NULL;
            if (analysis.nodes [i] == NULL)
                error ("analysis node %d not defined", n.number);
        }
    }

    if (analysis.input_node) {
        n.number = (unsigned) analysis.input_node;
        it = problem.node_set.find(&n);
        analysis.input_node = it != problem.node_set.end() ? *it : NULL;
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
    problem.constraint_tree  = TreeCreate (constraint_cmp);

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
