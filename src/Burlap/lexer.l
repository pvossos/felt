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
 * Description:	This file contains the specification for the lexical	*
 *		analyzer for burlap - a mathematical notation /		*
 *		language for finite element analysis.  The lexer has	*
 *		several idiosyncracies.  First, all occurrences of "yy"	*
 *		are replaced by "bf" to avoid naming conflicts.  Next,	*
 *		an IF token is replaced by an ELIF token if the last	*
 *		token returned to the parser was an ELSE.  This allows	*
 *		a single END for a sequence of nested IF-THEN-ELSEs by	*
 *		resolving a conflict in the parser.  If the lexer is in	*
 *		interactive mode (reading from the keyboard) and a line	*
 *		ends with a token that may end an expression, then a	*
 *		semicolon will be returned on the next call to yylex().	*
 *		If the lexer is reading from a file and a line ends	*
 *		with a token that may end an expression and the next	*
 *		token may begin an expression, then the token that	*
 *		would have been returned is saved and a semicolon is	*
 *		returned (a semicolon is "inserted" into the input).	*
 *		The saved token is returned on the next call to		*
 *		yylex().  Finally, an EOF rule is used to return a	*
 *		semicolon to "terminate" a file.  Note that since a	*
 *		re-entrant parser is needed to support the nested	*
 *		processing of files, yylval is an argument to yylex(),	*
 *		rather than a global variable.				*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include <unistd.h>
# include "error.h"
# include "lexer.h"
# include "parser.h"
# include "toktab.h"
# include "execute.h"
# include "globals.h"
# include "allocate.h"
# include "interactive.h"

# undef YY_INPUT
# define YY_INPUT(buf,result,max_size)\
    (result = interactive ? (buf [0] = readchar ( )) != 0 :\
				fread (buf, 1, max_size, yyin))

# define YY_DECL int yylex (yylval) YYSTYPE *yylval;


int   line_num;
int   file_num;
char *file_name;
int   interactive;

static int	depth;
static int	newline;
static int	last_token;
static int	saved_token;
static YYSTYPE	saved_value;
static YYSTYPE *value;

static int process	    PROTO ((int));
static int process_operator PROTO ((int));
static int process_literal  PROTO ((int));
static int process_newline  PROTO ((void));
static int process_eof	    PROTO ((void));
%}

digit		[0-9]
exp		[eE][+-]?{digit}+
alpha		[a-zA-Z_]

%x		quit
%x		comment
%%
%{
    /* If a saved token exists then return it to the parser.  This code
       will be placed at the top of yylex().   The private variable
       "value" is used to convey the value of yylval, which is now a
       parameter to yylex() to the called functions. */

    value = yylval;

    if (saved_token) {
	int token = saved_token;
	*yylval = saved_value;
	last_token = token;
	saved_token = 0;
	return token;
    }
%}


break				{return process_operator (BREAK);}
do				{return process_operator (DO);}
else				{return process_operator (ELSE);}
end				{return process_operator (END);}
for				{return process_operator (FOR);}
function			{return process_operator (FUNCTION);}
global				{return process_operator (GLOBAL);}
if				{return process_operator (IF);}
in				{return process_operator (IN);}
next				{return process_operator (NEXT);}
return				{return process_operator (RETURN);}
shared				{return process_operator (SHARED);}
then				{return process_operator (THEN);}
while				{return process_operator (WHILE);}

"="|":="			{return process_operator (ASSIGN);}
"||"|or				{return process_operator (OR);}
"&&"|and			{return process_operator (AND);}
"=="				{return process_operator (EQ);}
"!="|"<>"			{return process_operator (NE);}
"<"				{return process_operator (LT);}
">"				{return process_operator (GT);}
"<="				{return process_operator (LE);}
">="				{return process_operator (GE);}
":"				{return process_operator (COLON);}
"+"				{return process_operator (PLUS);}
"-"				{return process_operator (MINUS);}
"*"				{return process_operator (MULT);}
"/"|div				{return process_operator (DIV);}
"%"|mod				{return process_operator (MOD);}
"\\"				{return process_operator (BKSLV);}
"^"|"**"			{return process_operator (POW);}
"'"				{return process_operator (TRANS);}
"!"|not				{return process_operator (NOT);}
"."				{return process_operator (DOT);}
"("				{return process_operator (LPAREN);}
")"				{return process_operator (RPAREN);}
"["				{return process_operator (LBRACK);}
"]"				{return process_operator (RBRACK);}
","				{return process_operator (COMMA);}
";"				{return process_operator (SEMICOL);}

"&"{alpha}+			{return process_literal (CONSTANT);}

\"(\\(.|\n)|[^\\"])*\"		{return process_literal (STRLIT);}
{alpha}({alpha}|{digit})*\??	{return process_literal (ID);}

{digit}+			{return process_literal (NUMLIT);}
{digit}+{exp}			{return process_literal (NUMLIT);}
{digit}+\.{digit}*{exp}?	{return process_literal (NUMLIT);}
{digit}*\.{digit}+{exp}?	{return process_literal (NUMLIT);}

"/*"				{BEGIN (comment);}
<comment>[^*\n]*
<comment>[^*\n]*\n		{if (process_newline ( )) return SEMICOL;}
<comment>"*"+[^*/\n]*
<comment>"*"+[^*/\n]*\n		{if (process_newline ( )) return SEMICOL;}
<comment>"*"+"/"		{BEGIN (INITIAL);}

\\\n				{if (!interactive) line_num ++;}
\n				{if (process_newline ( )) return SEMICOL;}
#[^\n]*\n			{if (process_newline ( )) return SEMICOL;}
.|[ \t\f\r]+

<INITIAL><<EOF>>		{return process_eof ( );}
%%


/************************************************************************
 * Function:	yyerror							*
 *									*
 * Description:	Reports a syntax error using the error mechanism.	*
 ************************************************************************/

void yyerror (message)
    char *message;
{
    int c;


    if (*yytext == '\n') {
	line_num --;
	cterror ("%s before end of line", message);
	line_num ++;
    } else if (*yytext)
	cterror ("%s before '%s'", message, yytext);
    else
	cterror ("%s before end of file", message);


    if (interactive) {
	while ((c = input ( )) && c != '\n');
	last_token = 0;
	newline = 0;
	line_num ++;
    }
}


/************************************************************************
 * Function:	process							*
 *									*
 * Description:	Processes the current token.  This entails inserting a	*
 *		semicolon in the input and mapping an IF token to an	*
 *		ELIF token if necessary as described above.		*
 ************************************************************************/

static int process (this_token)
    int this_token;
{
    if (newline && toktab [last_token].end && toktab [this_token].begin) {
	saved_token = this_token;
	saved_value = *value;
	this_token = SEMICOL;
	value -> loc.line = line_num;
	value -> loc.file = file_num;
    } else if (last_token == ELSE && this_token == IF)
	this_token = ELIF;

    newline = 0;
    last_token = this_token;
    return this_token;
}


/************************************************************************
 * Function:	process_operator					*
 *									*
 * Description:	Process the current operator (symbol or keyword) token.	*
 *		The location information consisting of the current line	*
 *		and file number is set.					*
 ************************************************************************/

static int process_operator (this_token)
    int this_token;
{
    value -> loc.line = line_num;
    value -> loc.file = file_num;

    return process (this_token);
}


/************************************************************************
 * Function:	process_literal						*
 *									*
 * Description:	Process the current literal token.			*
 ************************************************************************/

static int process_literal (this_token)
    int this_token;
{
    char *dest;
    char *src;


    value -> sval = Strdup (yytext);

    if (this_token == STRLIT) {
	dest = value -> sval;
	for (src = yytext + 1; *src; src ++)
	    if (*src == '\\')
		switch (*++ src) {
		case '\n': line_num ++; break;
		case 'a':  *dest ++ = '\a'; break;
		case 'b':  *dest ++ = '\b'; break;
		case 'f':  *dest ++ = '\f'; break;
		case 'n':  *dest ++ = '\n'; break;
		case 'r':  *dest ++ = '\r'; break;
		case 't':  *dest ++ = '\t'; break;
		case 'v':  *dest ++ = '\v'; break;
		default:   *dest ++ = *src; break;
		}
	    else
		*dest ++ = *src;
	*(dest - 1) = 0;
    }

    return process (this_token);
}


/************************************************************************
 * Function:	process_newline						*
 *									*
 * Description:	Processes a newline token.				*
 ************************************************************************/

static int process_newline ( )
{
    value -> loc.line = line_num;
    value -> loc.file = file_num;

    if (interactive) {
	if (toktab [last_token].end) {
	    last_token = SEMICOL;
	    newline = 0;
	    line_num ++;
	    return 1;
	}
    } else
	line_num ++;

    newline = 1;
    return 0;
}


/************************************************************************
 * Function:	process_eof						*
 *									*
 * Description:	Processes an end of file.  If a file was being read	*
 *		then a semicolon is returned to terminate the file.	*
 ************************************************************************/

static int process_eof ( )
{
    BEGIN (quit);
    return interactive ? YY_NULL : SEMICOL;
}


/************************************************************************
 * Function:	yyinclude						*
 *									*
 * Description:	Includes a named file for interpreting.  If the file is	*
 *		successfully opened then any existing state information	*
 *		is saved on the stack, the lexer is initialized, and	*
 *		the file is parsed.  After parsing, any state		*
 *		information is restored.				*
 ************************************************************************/

int yyinclude (file)
    char *file;
{
    int		    old_newline;
    int		    old_last_token;
    int		    old_saved_token;
    int		    old_start_state;
    int		    old_line_num;
    int		    old_file_num;
    int		    old_interactive;
    int		    old_curr_line_num;
    int		    old_curr_file_num;
    char	   *old_curr_file_name;
    char	   *old_file_name;
    YYSTYPE	    old_saved_value;
    FILE	   *stream;
    ste		   *s;
    int		    status;
    ExecState	    state;
    YY_BUFFER_STATE buffer;


    /* Determine the file name and stream. */

    if (!file) {
	stream = NULL;
	file = "stdin";
    } else if (!strcmp (file, "-")) {
	stream = stdin;
	file = "stdin";
    } else if (!(stream = fopen (file, "r")))
	return -1;


    /* Save the current state. */

    if (depth ++) {
	SaveState (&state);
	old_start_state	= yy_start;

	old_line_num  = line_num;
	old_file_num  = file_num;
	old_file_name = file_name;

	old_curr_line_num  = curr_line_num;
	old_curr_file_num  = curr_file_num;
	old_curr_file_name = curr_file_name;

	old_newline	= newline;
	old_last_token	= last_token;
	old_saved_token	= saved_token;
	old_saved_value	= saved_value;
	old_interactive	= interactive;

	buffer = YY_CURRENT_BUFFER;
	yy_switch_to_buffer (yy_create_buffer (stream, YY_BUF_SIZE));

    } else if (!yyin)
	yyin = stream;
    else
	yyrestart (stream);


    /* Initialize and parse. */

    BEGIN (INITIAL);

    newline = 0;
    line_num = 1;
    last_token = 0;
    saved_token = 0;
    interactive = !stream;

    s = add_literal (&str_st, file, StrOp);
    file_name = s -> name;
    file_num = s -> idx;

    global_cs = cs = new_cs ( );
    status = yyparse ( );
    free_cs (cs);


    /* Restore the old state. */

    if (-- depth) {
	RestoreState (&state);
	global_cs = cs;
	yy_start = old_start_state;

	line_num  = old_line_num;
	file_num  = old_file_num;
	file_name = old_file_name;

	curr_line_num  = old_curr_line_num;
	curr_file_num  = old_curr_file_num;
	curr_file_name = old_curr_file_name;

	newline     = old_newline;
	last_token  = old_last_token;
	saved_token = old_saved_token;
	interactive = old_interactive;
	saved_value = old_saved_value;

	yy_delete_buffer (YY_CURRENT_BUFFER);
	yy_switch_to_buffer (buffer);
    }


    /* Close the file and return the status. */

    if (stream && stream != stdin)
	fclose (stream);

    return status;
}


/************************************************************************
 * Function:	yywrap							*
 *									*
 * Description:	Return nonzero, indicating the end of input.		*
 ************************************************************************/

int yywrap ( )
{
    if (0) unput (0);
    return 1;
}
