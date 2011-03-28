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
 * File:	file.c							*
 *									*
 * Description:	This file contains the public and private function	*
 *		definitions for writing FElt files and objects.		*
 ************************************************************************/

# include <algorithm>
# include <stdio.h>
# include <ctype.h>
# include <string.h>
# include "fe.h"
# include "error.h"
# include "problem.h"


/* Nasty macros for printing things just oh so right. */

# define cfprintf(fp,f,x) \
	if (x) fprintf (fp, f, x)

# define PrintHeader(fp) \
	if (!printed_header ++) fprintf (fp, "\ncanvas configuration\n"); \
	if (last_section) {fprintf (fp, "\n"); last_section = 0;} \
	this_section = 1

# define PrintBoolean(fp,n,x) \
	if (x != UnspecifiedValue) {\
	    PrintHeader(fp); \
	    fprintf (fp, "%s%s=", this_line ++ ? " " : "", n); \
	    fprintf (fp, "%s", x ? "true" : "false"); \
	}

# define PrintFloat(fp,n,x) \
	if (x != UnspecifiedValue) {\
	    PrintHeader(fp); \
	    fprintf (fp, "%s%s=%g", this_line ++ ? " " : "", n, x); \
	}

# define PrintInteger(fp,n,x) \
	if (x != UnspecifiedValue) {\
	    PrintHeader(fp); \
	    fprintf (fp, "%s%s=%d", this_line ++ ? " " : "", n, x); \
	}

# define PrintString(fp,n,x) \
	if (x != NULL) {\
	    PrintHeader(fp); \
	    fprintf (fp, "%s%s=%s", this_line ++ ? " " : "", n, Quote (x)); \
	}

# define InitFormat(fp) \
	printed_header = this_line = last_section = 0

# define StartSection(fp) \
	if (!last_section) last_section = this_section; \
	this_section = 0

# define EndSection(fp) \

# define PrintNewline(fp) \
	if (this_line) {fprintf (fp, "\n"); this_line = 0;}

static int     printed_header;
static int     this_line;
static int     this_section;
static int     last_section;

static char   *mark_flag;
static FILE   *fp;
static Node    prev_node;
static Element prev_element;


/************************************************************************
 * Function:	ConstraintSymbol					*
 *									*
 * Description:	Format a constraint into a proper string.		*
 ************************************************************************/

static const char*
ConstraintSymbol(Constraint constraint, DOF dof)
{
    static char buffer [32];


    if (constraint -> constraint [dof] == 0)
	return "u";

    if (constraint -> constraint [dof] == 'h')
	return "h";

    if (constraint -> dx [dof].value == 0)
	return "c";

    sprintf (buffer, "%g", constraint -> dx [dof].value);
    return buffer;
}


/************************************************************************
 * Function:	Quote							*
 *									*
 * Description:	Quotes a string if necessary.				*
 ************************************************************************/

static char*
Quote(const char *s)
{
    char	c;
    char       *ptr;
    static char buffer [256];

    if (s == NULL || strcmp (s, "") == 0) {
        strcpy(buffer, "\"\"");
        return buffer;
    }

    strcpy(buffer, s);

    for (ptr = buffer; (c = *ptr); ptr ++) {
	if (!isalpha (c) && c != '_' && (isdigit (c) ? ptr == s : 1)) {
	    sprintf (buffer, "\"%s\"", s);
	    return buffer;
	}
    }
    
    return buffer;
}


/************************************************************************
 * Function:	WriteNode						*
 *									*
 * Description:	Writes a node to the specified stream.			*
 ************************************************************************/

static void
WriteNode(Node node)
{
    fprintf (fp, "%-2u x=%g ", node -> number, node -> x);
    fprintf (fp, "y=%g z=%g", node -> y, node -> z);

    if (!prev_node || node -> constraint != prev_node -> constraint)
	fprintf (fp, " constraint=%s", Quote (node -> constraint -> name.c_str()));

    if (node -> force)
	fprintf (fp, " force=%s", Quote (node -> force -> name.c_str()));

    if (node -> m)
        fprintf (fp, " mass=%g", node -> m);

    fprintf (fp, "\n");
}


/************************************************************************
 * Function:	WriteElement						*
 *									*
 * Description:	Writes an element to the specified stream.		*
 ************************************************************************/

static void
WriteElement(Element element)
{
    unsigned i;
    unsigned numnodes;


    fprintf (fp, "%-2u nodes=[", element -> number);

    numnodes = element -> definition -> numnodes;
    for (i = 1; i <= numnodes; i ++) {
	if (element -> node [i])
	    fprintf (fp, "%u", element -> node [i] -> number);
	else
	    fprintf (fp, "0");
	fprintf (fp, i != numnodes ? "," : "]");
    }

    if (!prev_element || element -> material != prev_element -> material)
	fprintf (fp, " material=%s", Quote (element -> material -> name.c_str()));

    for (i = 1; i <= element -> numdistributed; i ++) {
	fprintf (fp, i == 1 ? " load=" : " ");
	fprintf (fp, "%s", Quote (element -> distributed [i] -> name.c_str()));
    }

    fprintf (fp, "\n");
}


/************************************************************************
 * Function:	WriteMaterial						*
 *									*
 * Description:	Writes a material to the specified stream if the aux	*
 *		pointer matches the mark flag.  The pointer is then	*
 *		cleared.						*
 ************************************************************************/

static int
WriteMaterial(Material material)
{
    if (material -> aux == mark_flag) {
	fprintf (fp, "%s", Quote (material -> name.c_str()));

        if (material -> color.c_str())
            fprintf (fp, " color=%s", material -> color.c_str());

	cfprintf (fp, " E=%g",     material -> E);
	cfprintf (fp, " A=%g",     material -> A);
	cfprintf (fp, " Ix=%g",    material -> Ix);
	cfprintf (fp, " Iy=%g",    material -> Iy);
	cfprintf (fp, " Iz=%g",    material -> Iz);
	cfprintf (fp, " J=%g",     material -> J);
	cfprintf (fp, " G=%g",     material -> G);
	cfprintf (fp, " nu=%g",    material -> nu);
	cfprintf (fp, " t=%g",     material -> t);
	cfprintf (fp, " rho=%g",   material -> rho);
	cfprintf (fp, " kappa=%g", material -> kappa);
	cfprintf (fp, " Rk=%g",    material -> Rk);
	cfprintf (fp, " Rm=%g",    material -> Rm);

	fprintf (fp, "\n");
    }

    material -> aux = NULL;
    return 0;
}


/************************************************************************
 * Function:	WriteLoad						*
 *									*
 * Description:	Writes a distributed load to the specified stream if	*
 *		the aux pointer matches the mark flag.  The pointer is	*
 *		then cleared.						*
 ************************************************************************/

static int
WriteLoad(Distributed load)
{
    unsigned	 i;
    static const char *direction_names [ ] = {"", "LocalX", "LocalY", "LocalZ",
					"GlobalX", "GlobalY", "GlobalZ",
					"parallel", "perpendicular", 
					"radial", "axial"};


    if (load -> aux == mark_flag) {
	fprintf (fp, "%s", Quote (load -> name.c_str()));

        if (load -> color.c_str())
            fprintf (fp, " color=%s", load -> color.c_str());

	fprintf (fp, " direction=%s", direction_names [load -> direction]);

	for (i = 1; i <= load -> value.size(); i ++) {
	    fprintf (fp, i == 1 ? " values=" : " ");
	    fprintf (fp, "(%u,", load -> value [i].node);
	    fprintf (fp, "%g)", load -> value [i].magnitude);
	}

	fprintf (fp, "\n");
    }

    load -> aux = NULL;
    return 0;
}


/************************************************************************
 * Function:	WriteConstraint						*
 *									*
 * Description:	Writes a constraint to the specified stream if the aux	*
 *		pointer matches the mark flag.  The pointer is then	*
 *		cleared.						*
 ************************************************************************/

static int
WriteConstraint(Constraint constraint)
{
    if (constraint -> aux == mark_flag) {
	fprintf (fp, "%s", Quote (constraint -> name.c_str()));

        if (constraint -> color.c_str())
            fprintf (fp, " color=%s", constraint -> color.c_str());

        if (constraint -> dx [Tx].expr == NULL)
	   fprintf (fp, " Tx=%s", ConstraintSymbol (constraint, Tx));
        else
           fprintf (fp, " Tx=%s", constraint -> dx [Tx].text);

        if (constraint -> dx [Ty].expr == NULL)
	   fprintf (fp, " Ty=%s", ConstraintSymbol (constraint, Ty));
        else
           fprintf (fp, " Ty=%s", constraint -> dx [Ty].text);

        if (constraint -> dx [Tz].expr == NULL)
	   fprintf (fp, " Tz=%s", ConstraintSymbol (constraint, Tz));
        else
           fprintf (fp, " Tz=%s", constraint -> dx [Tz].text);

        if (constraint -> dx [Rx].expr == NULL)
	   fprintf (fp, " Rx=%s", ConstraintSymbol (constraint, Rx));
        else
           fprintf (fp, " Rx=%s", constraint -> dx [Rx].text);

        if (constraint -> dx [Ry].expr == NULL)
	   fprintf (fp, " Ry=%s", ConstraintSymbol (constraint, Ry));
        else
           fprintf (fp, " Ry=%s", constraint -> dx [Ry].text);

        if (constraint -> dx [Rz].expr == NULL)
	   fprintf (fp, " Rz=%s", ConstraintSymbol (constraint, Rz));
        else
           fprintf (fp, " Rz=%s", constraint -> dx [Rz].text);

        cfprintf (fp, " ITx=%g", constraint -> ix [Tx]);
        cfprintf (fp, " ITy=%g", constraint -> ix [Ty]);
        cfprintf (fp, " ITz=%g", constraint -> ix [Tz]);
        cfprintf (fp, " IRx=%g", constraint -> ix [Rx]);
        cfprintf (fp, " IRy=%g", constraint -> ix [Ry]);
        cfprintf (fp, " IRz=%g", constraint -> ix [Rz]);

	cfprintf (fp, " Vx=%g", constraint -> vx [Tx]);
	cfprintf (fp, " Vy=%g", constraint -> vx [Ty]);
	cfprintf (fp, " Vz=%g", constraint -> vx [Tz]);

	if (constraint -> ax [Tx] != UnspecifiedValue)
	    fprintf (fp, " Ax=%g", constraint -> ax [Tx]);

	if (constraint -> ax [Ty] != UnspecifiedValue)
	    fprintf (fp, " Ay=%g", constraint -> ax [Ty]);

	if (constraint -> ax [Tz] != UnspecifiedValue)
	    fprintf (fp, " Az=%g", constraint -> ax [Tz]);

	fprintf (fp, "\n");
    }

    constraint -> aux = NULL;
    return 0;
}


/************************************************************************
 * Function:	WriteForce						*
 *									*
 * Description:	Writes a force to the specified stream if the aux	*
 *		pointer matches the mark flag.  The pointer is then	*
 *		cleared.						*
 ************************************************************************/

static int
WriteForce(Force force)
{
    if (force -> aux == mark_flag) {
	fprintf (fp, "%s", Quote (force -> name.c_str()));

        if (force -> color.c_str())
            fprintf (fp, " color=%s", force -> color.c_str());

	if (force -> force [Fx].expr)
	    fprintf (fp, " Fx=%s", force -> force [Fx].text);
	else if (force -> force [Fx].value)
	    fprintf (fp, " Fx=%g", force -> force [Fx].value);

	if (force -> force [Fy].expr)
	    fprintf (fp, " Fy=%s", force -> force [Fy].text);
	else if (force -> force [Fy].value)
	    fprintf (fp, " Fy=%g", force -> force [Fy].value);

	if (force -> force [Fz].expr)
	    fprintf (fp, " Fz=%s", force -> force [Fz].text);
	else if (force -> force [Fz].value)
	    fprintf (fp, " Fz=%g", force -> force [Fz].value);

	if (force -> force [Mx].expr)
	    fprintf (fp, " Mx=%s", force -> force [Mx].text);
	else if (force -> force [Mx].value)
	    fprintf (fp, " Mx=%g", force -> force [Mx].value);

	if (force -> force [My].expr)
	    fprintf (fp, " My=%s", force -> force [My].text);
	else if (force -> force [My].value)
	    fprintf (fp, " My=%g", force -> force [My].value);

	if (force -> force [Mz].expr)
	    fprintf (fp, " Mz=%s", force -> force [Mz].text);
	else if (force -> force [Mz].value)
	    fprintf (fp, " Mz=%g", force -> force [Mz].value);

	if (force -> spectrum [Fx].expr)
	    fprintf (fp, " Sfx=%s", force -> spectrum [Fx].text);
	else if (force -> spectrum [Fx].value)
	    fprintf (fp, " Sfx=%g", force -> spectrum [Fx].value);

	if (force -> spectrum [Fy].expr)
	    fprintf (fp, " Sfy=%s", force -> spectrum [Fy].text);
	else if (force -> spectrum [Fy].value)
	    fprintf (fp, " Sfy=%g", force -> spectrum [Fy].value);

	if (force -> spectrum [Fz].expr)
	    fprintf (fp, " Sfz=%s", force -> spectrum [Fz].text);
	else if (force -> spectrum [Fz].value)
	    fprintf (fp, " Sfz=%g", force -> spectrum [Fz].value);

	if (force -> spectrum [Mx].expr)
	    fprintf (fp, " Smx=%s", force -> spectrum [Mx].text);
	else if (force -> spectrum [Mx].value)
	    fprintf (fp, " Smx=%g", force -> spectrum [Mx].value);

	if (force -> spectrum [My].expr)
	    fprintf (fp, " Smy=%s", force -> spectrum [My].text);
	else if (force -> spectrum [My].value)
	    fprintf (fp, " Smy=%g", force -> spectrum [My].value);

	if (force -> spectrum [Mz].expr)
	    fprintf (fp, " Smz=%s", force -> spectrum [Mz].text);
	else if (force -> spectrum [Mz].value)
	    fprintf (fp, " Smz=%g", force -> spectrum [Mz].value);

	fprintf (fp, "\n");
    }

    force -> aux = NULL;
    return 0;
}

/************************************************************************
 * Function:	WriteLoadCase 						*
 *									*
 * Description:	Writes out a load case definition			*
 ************************************************************************/

static int
WriteLoadCase(LoadCase lc)
{
   unsigned     i;

   fprintf (fp, "%s\n", lc -> name.c_str());

   if (!lc->forces.empty()) {
       fprintf (fp, "node-forces=");
       for (i = 1 ; i <= lc->forces.size(); i++)
           fprintf (fp, "(%d, %s) ", lc -> nodes [i] -> number, lc -> forces [i] -> name.c_str());
       
      fprintf (fp, "\n");
   }

   if (!lc->loads.empty()) {
      fprintf (fp, "element-loads=");
      for (i = 1 ; i <= lc->loads.size(); i++)
          fprintf (fp, "(%d, %s) ", lc -> elements [i] -> number, lc -> loads [i] -> name.c_str());

      fprintf (fp, "\n");
   }


   return 0;
}

   
/************************************************************************
 * Function:	WriteAnalysisParameters					*
 *									*
 * Description:	Writes out the analysis parameters section.		*
 ************************************************************************/

static void
WriteAnalysisParameters(void)
{
    unsigned	 i;
    static const char *dof_symbols [ ] = {"", "Tx", "Ty", "Tz", "Rx", "Ry", "Rz"};


    if (analysis.start || analysis.step || analysis.stop) {
        fprintf (fp, "start=%g ", analysis.start);
        fprintf (fp, "stop=%g ", analysis.stop);
        fprintf (fp, "step=%g\n", analysis.step);
    }

    if (analysis.beta || analysis.gamma || analysis.alpha) {
        fprintf (fp, "beta=%g ", analysis.beta);
        fprintf (fp, "gamma=%g ", analysis.gamma);
        fprintf (fp, "alpha=%g\n", analysis.alpha);
    }

    if (analysis.Rk || analysis.Rm) {
        fprintf (fp, "Rk=%g ", analysis.Rk);
        fprintf (fp, "Rm=%g\n", analysis.Rm);
    }

    if (analysis.iterations || analysis.load_steps || analysis.tolerance || analysis.relaxation) {
        fprintf (fp, "iterations=%d ", analysis.iterations); 
        fprintf (fp, "load-steps=%d ", analysis.load_steps); 
        fprintf (fp, "tolerance=%g ", analysis.tolerance); 
        fprintf (fp, "relaxation=%g\n", analysis.relaxation); 
    }

    if (analysis.input_dof || analysis.input_node) {
        fprintf (fp,"input-node=%d ", analysis.input_node -> number);
        fprintf (fp,"input-dof=%s\n", dof_symbols [(int) analysis.input_dof]);
    }

    if (analysis.mass_mode == 'l')
	fprintf (fp, "mass-mode=lumped\n");
    else if (analysis.mass_mode == 'c')
	fprintf (fp, "mass-mode=consistent\n");

    if (!analysis.nodes.empty()) {
        fprintf (fp, "nodes=[");
        for (i = 1; i <= analysis.nodes.size(); i ++)
    	    if (i < analysis.nodes.size())	
                fprintf (fp, "%d, ", analysis.nodes [i] -> number);
            else
                fprintf (fp, "%d]\n", analysis.nodes [i] -> number);
    }

    if (analysis.numdofs > 0) {
        fprintf (fp, "dofs=[");
        for (i = 1; i <= analysis.numdofs; i ++)
   	    if (i < analysis.numdofs)	
	        fprintf (fp, "%s, ", dof_symbols [(int) analysis.dofs [i]]);
	    else
	        fprintf (fp, "%s]\n", dof_symbols [(int) analysis.dofs [i]]);
    }
}


/************************************************************************
 * Function:	WriteCanvasConfiguration				*
 *									*
 * Description:	Writes the canvas configuration section if necessary.	*
 ************************************************************************/

static void
WriteCanvasConfiguration(void)
{
    InitFormat   (fp);
    StartSection (fp);
    PrintBoolean (fp, "node-numbers",    appearance.node_numbers);
    PrintBoolean (fp, "element-numbers", appearance.element_numbers);
    PrintBoolean (fp, "snap",		 appearance.snap);
    PrintBoolean (fp, "grid",		 appearance.grid);
    PrintNewline (fp);

    PrintFloat   (fp, "snap-size", appearance.snap_size);
    PrintFloat   (fp, "grid-size", appearance.grid_size);
    PrintNewline (fp);
    EndSection   (fp);

    StartSection (fp);
    PrintString  (fp, "node-color",    appearance.node_color.c_str());
    PrintString  (fp, "element-color", appearance.element_color.c_str());
    PrintString  (fp, "label-font",    appearance.label_font.c_str());
    PrintNewline (fp);

    PrintString  (fp, "tool-color", appearance.tool_color.c_str());
    PrintString  (fp, "tool-font",  appearance.tool_font.c_str());
    PrintNewline (fp);
    EndSection   (fp);

    StartSection (fp);
    PrintFloat   (fp, "x-min", appearance.x_min);
    PrintFloat   (fp, "x-max", appearance.x_max);
    PrintFloat   (fp, "y-min", appearance.y_min);
    PrintFloat   (fp, "y-max", appearance.y_max);
    PrintNewline (fp);

    PrintInteger (fp, "x-pos",  appearance.x_pos);
    PrintInteger (fp, "y-pos",  appearance.y_pos);
    PrintInteger (fp, "width",  appearance.width);
    PrintInteger (fp, "height", appearance.height);
    PrintFloat   (fp, "scale",  appearance.scale);
    PrintNewline (fp);
    EndSection   (fp);
}


/************************************************************************
 * Function:	WriteFigureList						*
 *									*
 * Description:	Writes the figure list section.				*
 ************************************************************************/

static void
WriteFigureList(void)
{
    unsigned i;
    unsigned j;
    std::string last_font = "";
    std::string last_color = "";
    FigInfo *figure;


    fprintf (fp, "\nfigure list\n");

    for (i = 0; i < appearance.figures.size(); i ++) {
	figure = &appearance.figures [i];

	switch (figure -> type) {
	case RECTANGLE:
	    fprintf (fp, "rectangle");
	    fprintf (fp, " x=%g y=%g", figure -> x, figure -> y);
	    fprintf (fp, " width=%g", figure -> width);
	    fprintf (fp, " height=%g", figure -> height);
	    break;

	case POLYLINE:
	    fprintf (fp, "polyline");
	    if (!figure->points.empty()) {
		fprintf (fp, " points=[");
		for (j = 0; j < figure->points.size(); j ++) {
		    if (j) fprintf (fp, " ");
		    fprintf (fp, "(%g,", figure -> points [j].x);
		    fprintf (fp, "%g)", figure -> points [j].y);
		}
		fprintf (fp, "]");
	    }
	    break;

	case TEXT:
	    fprintf (fp, "text");
	    fprintf (fp, " x=%g y=%g", figure -> x, figure -> y);
	    if (!figure -> text.empty())
            fprintf (fp, " text=%s", Quote (figure -> text.c_str()));
	    if (!figure -> font.empty())
            if (last_font.empty() || last_font != figure->font) {
                fprintf (fp, " font=%s", Quote (figure -> font.c_str()));
                last_font = figure -> font;
            }
	    break;

	case ARC:
	    fprintf (fp, "arc");
	    fprintf (fp, " x=%g y=%g", figure -> x, figure -> y);
	    fprintf (fp, " width=%g", figure -> width);
	    fprintf (fp, " height=%g", figure -> height);
	    fprintf (fp, " start=%g", figure -> start);
	    fprintf (fp, " length=%g", figure -> length);
	    break;
	}

	if (!figure -> color.empty())
	    if (last_color.empty() || figure->color != last_color) {
            fprintf (fp, " color=%s", Quote (figure -> color.c_str()));
            last_color = figure -> color;
	    }

	fprintf (fp, "\n");
    }
}


/************************************************************************
 * Function:	WriteFile						*
 *									*
 * Description:	Writes a felt file.  A filename of "-" indicates	*
 *		standard output.					*
 ************************************************************************/

static int
WriteFile(char *flag)
{
    int		 any_forces;
    int		 any_loads;
    Node	 node;
    Element	 element;
    unsigned	 i;
    unsigned	 j;
    Definition	 definition;
    static const char	*analysis_names [ ] = {"", "static", "transient", "modal",
                                       "static-thermal", "transient-thermal",
                                       "spectral","static-substitution",
                                       "static-incremental","static","static",
                                       "static-substitution",
                                       "static-incremental"};


    /* Write the problem description section. */

    fprintf (fp, "problem description\n");
    if (problem.title != NULL && strcmp (problem.title, ""))   
       fprintf (fp, "title=%s\n", Quote (problem.title));

    fprintf (fp, "nodes=%u ", problem.nodes.size());
    fprintf (fp, "elements=%u", problem.elements.size());

    if (problem.mode != Static)
	fprintf (fp, " analysis=%s", analysis_names [problem.mode]);

    fprintf (fp, "\n");


    /* Write the analysis parameters section if the mode is not static. */

    if (problem.mode != Static) {
        fprintf (fp, "\nanalysis parameters\n");
	WriteAnalysisParameters ( );	
    }


    /* Write and load case definitions */

    if (!problem.loadcase_set.empty()) {
        fprintf (fp, "\nload cases\n");
        std::for_each(problem.loadcase_set.begin(), problem.loadcase_set.end(), WriteLoadCase);
    }

    /* Write the nodes section marking referenced objects. */

    mark_flag = flag;
    any_forces = 0;
    any_loads = 0;

    if (!problem.nodes.empty()) {
    prev_node.reset();
	fprintf (fp, "\nnodes\n");

	for (i = 1; i <= problem.nodes.size(); i ++) {
	    node = problem.nodes [i];
	    WriteNode (node);
	    prev_node = node;

	    node -> constraint -> aux = flag;
	    if (node -> force) {
		node -> force -> aux = flag;
		any_forces = 1;
	    }
	}
    }


    /* Write the elements section marking referenced objects. */

    if (!problem.elements.empty()) {
        prev_element.reset();

	for (i = 1; i <= problem.elements.size(); i ++) {
	    element = problem.elements [i];
	    definition = element -> definition;

	    if (!prev_element || prev_element -> definition != definition)
            fprintf(fp,"\n%s elements\n", Quote (definition -> name.c_str()));

	    WriteElement (element);
	    prev_element = element;

	    element -> material -> aux = flag;
	    if (element -> numdistributed)
		any_loads = 1;

	    for (j = 1; j <= element -> numdistributed; j ++)
		element -> distributed [j] -> aux = flag;
	}
    }


    /* Write the materials section. */

    if (!problem.material_set.empty()) {
        fprintf (fp, "\nmaterial properties\n");
        std::for_each(problem.material_set.begin(), problem.material_set.end(), WriteMaterial);
    }


    /* Write the distributed loads section. */

    if (any_loads || !mark_flag) {
        fprintf (fp, "\ndistributed loads\n");
        std::for_each(problem.distributed_set.begin(), problem.distributed_set.end(), WriteLoad);
    }


    /* Write the constraints section. */

    if (!problem.constraint_set.empty()) {
        fprintf (fp, "\nconstraints\n");
        std::for_each(problem.constraint_set.begin(), problem.constraint_set.end(), WriteConstraint);
    }


    /* Write the forces section. */

    if (any_forces || !mark_flag) {
        fprintf (fp, "\nforces\n");
        std::for_each(problem.force_set.begin(), problem.force_set.end(), WriteForce);
    }


    /* Write the canvas configuration and figure list .*/

    WriteCanvasConfiguration ( );

    if (!appearance.figures.empty())
        WriteFigureList ( );


    /* Clean up, close the file, and return success. */

    fprintf (fp, "\nend\n");

    return 0;
}

static int
OpenFile(const char *filename)
{
    if (strcmp (filename, "-")) {
	if (!(fp = fopen (filename, "w"))) {
	    error ("Unable to open %s", filename);
	    return 1;
	}
    } else
	fp = stdout;

    return 0;
}

static void
CloseFile(void)
{
    if (fp != stdout)
	fclose (fp);

    return;
}

int
WriteFeltFile(const char *filename)
{
    if (OpenFile (filename))
       return 1;

    if (WriteFile ((char *) 1))
       return 1;

    CloseFile ( );

    return 0;
}

int
DumpFeltFile(const char *filename)
{
    if (OpenFile (filename))
       return 1;

    if (WriteFile ((char *) 0))
       return 1;;

    CloseFile ( );

    return 0;
}

int
fWriteFeltFile(FILE *stream)
{
    fp = stream;

    if (WriteFile ((char *) 1))
       return 1;

    return 0;
}

int 
fDumpFeltFile(FILE *stream)
{
    fp = stream;

    if (WriteFile ((char *) 0))
       return 1;;

    return 0;
}
