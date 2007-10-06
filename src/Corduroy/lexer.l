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
 * File:	lexer.l							*
 *									*
 * Description:	This file contains the flex specification for the	*
 *		lexical analyzer for the corduroy element generator.	*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include "error.h"
# include "generator.h"

# ifdef DOS
# include "yxtab.h"
# else
# include "y.tab.h"
# endif

# undef atof
extern double atof (const char *);


static char *filename;
static void  update_line PROTO ((void));
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

line				{return LINE;}
grid				{return GRID;}
quadrilateral\ grid		{return QUADGRID;}
brick\ grid			{return BRICKGRID;}
triangular\ mesh		{return TRIMESH;}
end				{return END;}


%{
/* Reserved words (in expression or right hand side of assignment) */
%}

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
linear				{return LINEAR;}
logarithmic			{return LOGARITHMIC;}
reverse-logarithmic		{return REVERSE_LOGARITHMIC;}
cosinusoidal			{return COSINUSOIDAL;}
sinusoidal			{return SINUSOIDAL;}
parabolic			{return PARABOLIC;}
reverse-parabolic		{return REVERSE_PARABOLIC;}

%{
/* Assignments */
%}

start-node{eq}			{return NODE_EQ;}
start-element{eq}		{return ELEMENT_EQ;}
constraint{eq}			{return CONSTRAINT_EQ;}
material{eq}			{return MATERIAL_EQ;}
element-type{eq}		{return TYPE_EQ;}
start{eq}			{return START_EQ;}
end{eq}				{return END_EQ;}
number{eq}			{return NUMBER_EQ;}
rule{eq}			{return RULE_EQ;}
x-number{eq}			{return X_NUMBER_EQ;}
y-number{eq}			{return Y_NUMBER_EQ;}
z-number{eq}			{return Z_NUMBER_EQ;}
x-rule{eq}			{return X_RULE_EQ;}
y-rule{eq}			{return Y_RULE_EQ;}
z-rule{eq}			{return Z_RULE_EQ;}
target{eq}			{return TARGET_EQ;}
alpha{eq}			{return ALPHA_EQ;}
boundary{eq}			{return BOUNDARY_EQ;}
hole{eq}			{return HOLE_EQ;}


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

{digit}+			{yylval.i = atoi (yytext); return INTEGER;}
{digit}+{exp}			{yylval.d = atof (yytext); return DOUBLE;}
{digit}+\.{digit}*{exp}?	{yylval.d = atof (yytext); return DOUBLE;}
{digit}*\.{digit}+{exp}?	{yylval.d = atof (yytext); return DOUBLE;}

{id}				{yylval.s = strdup (yytext); return NAME;}
\"[^"]*\"			{yylval.s = strdup (yytext + 1);
				 yylval.s [yyleng - 2] = 0; return NAME;}


%{
/* Comments, white space, and remaining preprocessor lines (the comment
   rules are straight from the flex documentation). */
%}

#[^\n]*$			{update_line ( );}
[ \t]+				{}
\n				{generator.line ++;}
"/*"				{BEGIN comment;}

<comment>[^*\n]*		{}
<comment>[^*\n]*\n		{generator.line ++;}
<comment>"*"+[^*/\n]*		{}
<comment>"*"+[^*/\n]*\n		{generator.line ++;}
<comment>"*"+"/"		{BEGIN INITIAL;}

.				{return *yytext;}

%%

/************************************************************************
 * Function:	yyerror							*
 *									*
 * Description:	Writes a syntax error message reporting the unexpected	*
 *		token.							*
 ************************************************************************/

void yyerror (char *msg)
{
    extern int yychar;


    if (yychar)
	error ("parse error before `%s'", yytext);
    else
	error ("parse error before end of input");
}


/************************************************************************
 * Function:	init_lexer						*
 *									*
 * Description:	Initializes the lexer for a new file.			*
 ************************************************************************/

void init_lexer (FILE *fp)
{
    static int first_time = 1;


    if (first_time) {
	first_time = 0;
	yyin = fp;
    } else
	yyrestart (fp);

    filename = generator.filename;
}


/************************************************************************
 * Function:	update_line						*
 *									*
 * Description:	Updates the current line and file from a preprocessor	*
 *		directive.						*
 ************************************************************************/

static void update_line (void)
{
    char       *ptr1;
    char       *ptr2;
    int		line;
    static char include_filename [256];


    ptr2 = include_filename;
    line = strtol (yytext + 1, &ptr1, 10) - 1;

    if (ptr1) {
	generator.line = line;

	while (*ptr1 && *ptr1 ++ != '"');

	if (*ptr1)
	    generator.filename = filename;

	while (*ptr1 && *ptr1 != '"')
	    *ptr2 ++ = *ptr1 ++;

	*ptr2 = 0;
	if (ptr2 != include_filename)
	    generator.filename = include_filename;
    }
}

#if 0
# undef yywrap
int yywrap ( ) {
    if (0) unput (0);
    return 1;
}
#endif
