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
 * File:	lexer.l							*
 *									*
 * Description:	This file contains the flex specification for the	*
 *		lexical analyzer for the FElt 2.0 system.		*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <unistd.h>
# include "error.h"
# include "problem.h"
# include "y.tab.h"

static char *filename;
static void  update_line (void);

static int   copy_flag = 0;
static char  copied_text [2048];
static char *copy_ptr = copied_text;

# define YY_USER_ACTION \
	if (copy_flag) { \
	    char *ptr = yytext; \
	    while ((*copy_ptr = *ptr ++)) copy_ptr ++; \
	}

# undef  YY_INPUT
# define YY_INPUT(buf,result,max_size) \
	if (problem.input) \
	    result = (*buf = *problem.input ++) ? 1 : (problem.input --, 0); \
	else \
	    if ((result = read (fileno (yyin), (char *) buf, max_size)) < 0) \
		YY_FATAL_ERROR ("read() in flex scanner failed");
%}

digit	[0-9]
exp	[eE][+-]?{digit}+
letter	[a-zA-Z_]
id	{letter}({letter}|{digit})*
eq	[ \t]*=

%x	comment
%option noyywrap
%%


%{
/* Section headings */
%}

problem\ description		{return PROBLEM;}
nodes				{return NODES;}
{id}\ elements			{felt_yylval.s = strdup (yytext); return ELEMENTS;}
material\ properties		{return MATERIALS;}
distributed\ loads		{return LOADS;}
forces				{return FORCES;}
constraints			{return CONSTRAINTS;} 
analysis\ parameters		{return ANALYSIS;}
load\ cases			{return LOAD_CASES;}
end				{return END;}


%{
/* Reserved words (in expression or right hand side of assignment) */
%}

t				{return TIME;}
t/{eq}=				{return TIME;}
w				{return TIME;}
w/{eq}=				{return TIME;}
sin				{return SIN;}
cos				{return COS;}
tan				{return TAN;}
pow				{return POW;}
exp				{return EXP;}
log				{return LOG;}
log10				{return LOG10;}
sqrt				{return SQRT;}
hypot				{return HYPOT;}
floor				{return FLOOR;}
ceil				{return CEIL;}
fmod				{return FMOD;}
fabs				{return FABS;}
static				{felt_yylval.i = Static; return ANALYSIS_TYPE;}
transient			{felt_yylval.i = Transient; return ANALYSIS_TYPE;}
modal				{felt_yylval.i = Modal; return ANALYSIS_TYPE;}
static-thermal			{felt_yylval.i = StaticThermal; return ANALYSIS_TYPE;}
transient-thermal		{felt_yylval.i = TransientThermal; return ANALYSIS_TYPE;}
static-incremental		{felt_yylval.i = StaticSubstitution; return ANALYSIS_TYPE;}
static-substitution		{felt_yylval.i = StaticSubstitution; return ANALYSIS_TYPE;}
spectral			{felt_yylval.i = Spectral; return ANALYSIS_TYPE;}
localx				{felt_yylval.i = LocalX; return DIRECTION;}
localy				{felt_yylval.i = LocalY; return DIRECTION;}
localz				{felt_yylval.i = LocalZ; return DIRECTION;}
globalx				{felt_yylval.i = GlobalX; return DIRECTION;}
globaly				{felt_yylval.i = GlobalY; return DIRECTION;}
globalz				{felt_yylval.i = GlobalZ; return DIRECTION;}
par(allel)?			{felt_yylval.i = Parallel; return DIRECTION;}
perp(endicular)?		{felt_yylval.i = Perpendicular; return DIRECTION;}
axial				{felt_yylval.i = Axial; return DIRECTION;}
radial				{felt_yylval.i = Radial; return DIRECTION;}
u(nconstrained)?		{felt_yylval.i = 0; return CONSTRAINT;}
c(onstrained)?			{felt_yylval.i = 1; return CONSTRAINT;}
h(inged)?			{felt_yylval.i = 'h'; return HINGED;}
lumped				{felt_yylval.i = 'l'; return MASS_MODE;}
consistent			{felt_yylval.i = 'c'; return MASS_MODE;}
tx				{felt_yylval.i = Tx; return NODE_DOF;}
ty				{felt_yylval.i = Ty; return NODE_DOF;}
tz				{felt_yylval.i = Tz; return NODE_DOF;}
rx				{felt_yylval.i = Rx; return NODE_DOF;}
ry				{felt_yylval.i = Ry; return NODE_DOF;}
rz				{felt_yylval.i = Rz; return NODE_DOF;}


%{
/* Assignments */
%}

title{eq}			{return TITLE_EQ;}
nodes{eq}			{return NODES_EQ;}
elements{eq}			{return ELEMENTS_EQ;}
analysis{eq}			{return ANALYSIS_EQ;}
x{eq}				{return X_EQ;}
y{eq}				{return Y_EQ;}
z{eq}				{return Z_EQ;}
mass{eq}			{return MASS_EQ;}
force{eq}			{return FORCE_EQ;}
constraint{eq}			{return CONSTRAINT_EQ;}
load{eq}			{return LOAD_EQ;}
material{eq}			{return MATERIAL_EQ;}
e{eq}				{return E_EQ;}
ix{eq}				{return IX_EQ;}
iy{eq}				{return IY_EQ;}
iz{eq}				{return IZ_EQ;}
a{eq}				{return A_EQ;}
j{eq}				{return J_EQ;}
g{eq}				{return G_EQ;}
t{eq}				{return T_EQ;}
rho{eq}				{return RHO_EQ;}
nu{eq}				{return NU_EQ;}
kappa{eq}			{return KAPPA_EQ;}
rk{eq}				{return RK_EQ;}
rm{eq}				{return RM_EQ;}
kx{eq}				{return KX_EQ;}
ky{eq}				{return KY_EQ;}
kz{eq}				{return KZ_EQ;}
c{eq}				{return C_EQ;}
direction{eq}			{return DIRECTION_EQ;}
values{eq}			{return VALUES_EQ;}
fx{eq}				{return FX_EQ;}
fy{eq}				{return FY_EQ;}
fz{eq}				{return FZ_EQ;}
mx{eq}				{return MX_EQ;}
my{eq}				{return MY_EQ;}
mz{eq}				{return MZ_EQ;}
sfx{eq}				{return SFX_EQ;}
sfy{eq}				{return SFY_EQ;}
sfz{eq}				{return SFZ_EQ;}
smx{eq}				{return SMX_EQ;}
smy{eq}				{return SMY_EQ;}
smz{eq}				{return SMZ_EQ;}
tx{eq}				{return TX_EQ;}
ty{eq}				{return TY_EQ;}
tz{eq}				{return TZ_EQ;}
rx{eq}				{return RX_EQ;}
ry{eq}				{return RY_EQ;}
rz{eq}				{return RZ_EQ;}
itx{eq}				{return ITX_EQ;}
ity{eq}				{return ITY_EQ;}
itz{eq}				{return ITZ_EQ;}
irx{eq}				{return IRX_EQ;}
iry{eq}				{return IRY_EQ;}
irz{eq}				{return IRZ_EQ;}
vx{eq}				{return VX_EQ;}
vy{eq}				{return VY_EQ;}
vz{eq}				{return VZ_EQ;}
ax{eq}				{return AX_EQ;}
ay{eq}				{return AY_EQ;}
az{eq}				{return AZ_EQ;}
alpha{eq}			{return ALPHA_EQ;}
beta{eq}			{return BETA_EQ;}
gamma{eq}			{return GAMMA_EQ;}
dt{eq}				{return STEP_EQ;}
duration{eq}			{return STOP_EQ;}
start{eq}			{return START_EQ;}
stop{eq}			{return STOP_EQ;}
step{eq}			{return STEP_EQ;}
dofs{eq}			{return DOFS_EQ;}
mass-mode{eq}			{return MASS_MODE_EQ;}
gravity{eq}			{return GRAVITY_EQ;}
iterations{eq}			{return ITERATIONS_EQ;}
tolerance{eq}			{return TOLERANCE_EQ;}
relaxation{eq}			{return RELAXATION_EQ;}
input-dof{eq}			{return INPUT_DOF_EQ;}
input-node{eq}			{return INPUT_NODE_EQ;}
input-range{eq}			{return INPUT_RANGE_EQ;}
load-steps{eq}			{return LOAD_STEPS_EQ;}
node-forces{eq}			{return NODE_FORCES_EQ;}
element-loads{eq}		{return ELEMENT_LOADS_EQ;}


%{
/* Velvet-specific constructs */
%}

canvas\ configuration		{return CANVAS;}
figure\ list			{return FIGURES;}
true				{felt_yylval.i = 1; return BOOLEAN;}
false				{felt_yylval.i = 0; return BOOLEAN;}
rectangle			{felt_yylval.i = RECTANGLE; return FIGURE_TYPE;}
polyline			{felt_yylval.i = POLYLINE; return FIGURE_TYPE;}
text				{felt_yylval.i = TEXT; return FIGURE_TYPE;}
arc				{felt_yylval.i = ARC; return FIGURE_TYPE;}

node-numbers{eq}		{return NODE_NUM_EQ;}
element-numbers{eq}		{return ELT_NUM_EQ;}
snap{eq}			{return SNAP_EQ;}
grid{eq}			{return GRID_EQ;}
snap-size{eq}			{return SNAP_SIZE_EQ;}
grid-size{eq}			{return GRID_SIZE_EQ;}
x-min{eq}			{return X_MIN_EQ;}
x-max{eq}			{return X_MAX_EQ;}
y-min{eq}			{return Y_MIN_EQ;}
y-max{eq}			{return Y_MAX_EQ;}
scale{eq}			{return SCALE_EQ;}
x-pos{eq}			{return X_POS_EQ;}
y-pos{eq}			{return Y_POS_EQ;}
width{eq}			{return WIDTH_EQ;}
height{eq}			{return HEIGHT_EQ;}
node-color{eq}			{return NODE_COLOR_EQ;}
element-color{eq}		{return ELT_COLOR_EQ;}
label-font{eq}			{return LABEL_FONT_EQ;}
tool-color{eq}			{return TOOL_COLOR_EQ;}
tool-font{eq}			{return TOOL_FONT_EQ;}
color{eq}			{return COLOR_EQ;}
font{eq}			{return FONT_EQ;}
text{eq}			{return TEXT_EQ;}
length{eq}			{return LENGTH_EQ;}
points{eq}			{return POINTS_EQ;}


%{
/* Operators */
%}

"&&"				{return AND;}
"||"				{return OR;}
"=="				{return EQUALS;}
"!="				{return NEQUAL;}
"<="				{return LT_EQ;}
">="				{return GT_EQ;}
"<<"				{return LSHIFT;}
">>"				{return RSHIFT;}


%{
/* Numbers and names */
%}

{digit}+			{felt_yylval.i = atoi (yytext); return INTEGER;}
{digit}+{exp}			{felt_yylval.d = atof (yytext); return DOUBLE;}
{digit}+\.{digit}*{exp}?	{felt_yylval.d = atof (yytext); return DOUBLE;}
{digit}*\.{digit}+{exp}?	{felt_yylval.d = atof (yytext); return DOUBLE;}

{id}				{felt_yylval.s = strdup (yytext); return NAME;}
\"[^"]*\"			{felt_yylval.s = strdup (yytext + 1);
				 felt_yylval.s [yyleng - 2] = 0; return NAME;}


%{
/* Comments, white space, and remaining preprocessor lines (the comment
   rules are straight from the flex documentation). */
%}

#[^\n]*$			{update_line ( );}
[ \t]+				{}
\n				{problem.line ++;}
"/*"				{BEGIN comment;}

<comment>[^*\n]*		{}
<comment>[^*\n]*\n		{problem.line ++;}
<comment>"*"+[^*/\n]*		{}
<comment>"*"+[^*/\n]*\n		{problem.line ++;}
<comment>"*"+"/"		{BEGIN INITIAL;}

.				{return *yytext;}

%%

/************************************************************************
 * Function:	yyerror							*
 *									*
 * Description:	Writes a syntax error message reporting the unexpected	*
 *		token only if input if being read from a file.		*
 ************************************************************************/

void felt_yyerror (const char *msg)
{
    extern int felt_yychar;


    if (!problem.input)
	if (felt_yychar)
	    error ("parse error before `%s'", yytext);
	else
	    error ("parse error before end of input");
    else
	problem.input -= yyleng;
}


/************************************************************************
 * Function:	init_felt_lexer						*
 *									*
 * Description:	Initializes the lexer for a new file.			*
 ************************************************************************/

void init_felt_lexer (FILE *fp)
{
    static int first_time = 1;


    if (first_time) {
	first_time = 0;
	yyin = fp;
    } else
	yyrestart (fp);

    if (fp) {
	problem.input = NULL;
	filename = problem.filename;
    }
}


/************************************************************************
 * Function:	update_line						*
 *									*
 * Description:	Updates the current line and file from a preprocessor	*
 *		directive.						*
 ************************************************************************/

static void update_line (void)
{
     char	*ptr1;
     char	*ptr2;
     int         line;
     static char include_filename [256];


     ptr2 = include_filename;
     line = strtol (yytext + 1, &ptr1, 10) - 1;

     if (ptr1) {
	problem.line = line;

	while (*ptr1 && *ptr1 ++ != '"');

	if (*ptr1)
	    problem.filename = filename;

	while (*ptr1 && *ptr1 != '"')
	    *ptr2 ++ = *ptr1 ++;

	*ptr2 = 0;
	if (ptr2 != include_filename)
	    problem.filename = include_filename;
     }
}


/************************************************************************
 * Function:	copy_input						*
 *									*
 * Description:	Specifies whether yytext should be copied into a local	*
 *		buffer.  The text is put into a canonical form in which	*
 *		leading and trailing white space is eliminated and all	*
 *		other sequences of white space are replaced by a space.	*
 ************************************************************************/

char *copy_input (int flag)
{
    char       *src;
    char       *dest;
    static char canonical_text [2048];


    if (!(copy_flag = flag)) {
	*(copy_ptr - yyleng) = 0;
	dest = canonical_text;

	src = strtok (copied_text, " \t\n");
	while (src) {
	    while ((*dest ++ = *src ++));
	    src = strtok (NULL, " \t\n");
	    *(dest - 1) = ' ';
	}

	*(dest - 1) = 0;
	*(copy_ptr = copied_text) = 0;
	return canonical_text;
    }

    return copied_text;
}

#if 0
# undef yywrap
int yywrap (void)
{
    if (0) unput (0);
    return 1;
}
#endif
