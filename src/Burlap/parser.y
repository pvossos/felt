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
 * Description:	This file contains the specification for the parser for	*
 *		burlap - a mathematical notation / language for finite	*
 *		element analysis.					*
 ************************************************************************/

# include <stdio.h>
# include "loop.h"
# include "debug.h"
# include "error.h"
# include "lexer.h"
# include "parser.h"
# include "globals.h"
# include "execute.h"
# include "allocate.h"

# if !defined (__GNUC__) && !defined (__sparc__)
# define alloca malloc		/* prevents alloca from being called */
# endif

static st  *vars;
static st   local_st;

static void emitloc (burlap_yyloc loc);

static char arg_types [256];		/* should be plenty */
static int  last_line_num = -1;
static int  last_file_num = -1;
static int  syntax_error;
%}

%union {
    int		ival;
    Address	addr;
    char       *sval;
    descriptor *desc;
    burlap_yyloc	loc;
    void       *ptr;
}

%pure_parser

%token	BREAK DO ELIF ELSE END FOR FUNCTION GLOBAL IF IN NEXT RETURN SHARED THEN
%token	WHILE ASSIGN OR AND EQ NE LT GT LE GE COLON PLUS MINUS MULT DIV MOD
%token	BKSLV POW TRANS NOT DOT LPAREN RPAREN LBRACK RBRACK COMMA SEMICOL
%token	ID STRLIT NUMLIT CONSTANT

%right	COLON

%type	<loc>  BREAK DO ELIF ELSE END FOR FUNCTION GLOBAL IF IN NEXT RETURN
%type	<loc>  SHARED THEN WHILE ASSIGN OR AND EQ NE LT GT LE GE COLON PLUS
%type	<loc>  MINUS MULT DIV MOD BKSLV TRANS NOT POW DOT LPAREN RPAREN
%type	<loc>  LBRACK RBRACK COMMA SEMICOL

%type	<sval> ID STRLIT NUMLIT CONSTANT
%type	<ival> opt_formal_list formal_list expression_list matrix_row_list
%type	<ival> opt_index_expression_list index_expression_list arg_type
%type	<addr> or and else mark fail do_while in_expression
%type	<desc> function_id
%type	<loc>  semicol_list
%type	<ptr>  break
%%

program
	: function_or_expression_list
	    {syntax_error = 0;}

	| error error_action function_or_expression_list
	    {syntax_error = 0;}
	;


error_action
	: /* empty */
	    {
		yyerrok;
		yyclearin;
		cs = global_cs;
		exit_all ( );
		reset ( );

		if (!interactive)
		    syntax_error = 1;
	    }
	;


function_or_expression_list
	: function_or_expression_list function_definition SEMICOL

	| function_or_expression_list expression SEMICOL
	    {
		if (!syntax_error) {
		    emit (HaltOp);
		    if (execute (cs, var_array, NULL) != -1 && !interactive)
			return 1;
		}

		reset ( );
		last_line_num = -1;
		last_file_num = -1;
	    }

	| function_or_expression_list SEMICOL

	| /* empty */
	    {
		vars = &var_st;
		reset ( );
	    }
	;


function_definition
	: function_id LPAREN opt_formal_list RPAREN SEMICOL function_body END
	    {
		int i;


		D_Function ($1) -> num_args = $3;
		D_Function ($1) -> num_locals = vars -> num_syms;
		D_Function ($1) -> local_names = st_names (vars);

		D_Function ($1) -> arg_types = Allocate (char, $3);
		for (i = 0; i < $3; i ++)
		    D_Function ($1) -> arg_types [i] = arg_types [i];

		emit (RtnOp);
		st_fini (vars);
		st_fini (&import_st);

		d_dump (cs);
		cs = global_cs;
		vars = &var_st;
		st_init (&import_st);
		reset ( );

		last_file_num = -1;
		last_line_num = -1;
	    }
	;


function_id
	: FUNCTION ID
	    {
		ste	   *s;
		descriptor  d;


		s = add_literal (&var_st, $2, GlblOp);
		d = *($$ = &var_array [s -> idx]);
		Deallocate ($2);

		FreeData ($$);
		CreateData ($$, NULL, NULL, T_Function, s -> name);
		D_Temp ($$) = F_False;

		cs = D_Function ($$) -> cs;
		st_init (vars = &local_st);
		st_init (&import_st);
		reset ( );

		last_file_num = -1;
		last_line_num = -1;
		emitloc ($1);
	    }
	;


opt_formal_list
	: formal_list

	| /* empty */
	    {$$ = 0;}
	;


formal_list
	: arg_type ID
	    {
		st_insert (&import_st, $2, ArgOp);
		arg_types [0] = $1; $$ = 1;
		Deallocate ($2);
	    }

	| formal_list COMMA arg_type ID
	    {
		ste *s = st_lookup (&import_st, $4);
		if (s) {
		    cterror ("duplicate argument '%s'", $4);
		    $$ = -1;
		} else if ($1 > 0) {
		    st_insert (&import_st, $4, ArgOp);
		    arg_types [$1] = $3; $$ = $1 + 1;
		}
		Deallocate ($4);
	    }
	;


arg_type
	: SHARED
	    {$$ = SharedArg;}

	| /* empty */
	    {$$ = ValueArg;}
	;


function_body
	: global_declarations expression_block
	;


global_declarations
	: global_declarations GLOBAL global_id_list SEMICOL
	| /* empty */
	;


global_id_list
	: global_id_list COMMA global_id
	| global_id
	;


global_id
	: ID
	    {
		ste *s = add_literal (&var_st, $1, GlblOp);
		add_literal (&import_st, $1, GlblOp) -> idx = s -> idx;
		Deallocate ($1);
	    }
	;


expression_block
	: expression_block SEMICOL
	| expression_block SEMICOL pop expression
	| expression
	| /* empty */
	    {emit (DblOp, 0);}
	;


expression_list
	: expression_list COMMA expression
	    {$$ = $1 + 1;}
	| expression
	    {$$ = 1;}
	;


opt_expression
	: expression
	| /* empty */
	    {emit (NullOp);}
	;


expression
	: assignment_expression
	| break opt_expression
	    {end_break (BREAK, $1);}
	| RETURN opt_expression
	    {
		if (cs == global_cs) {
		    cterror ("return not within a function");
		    emit (NullOp);
		} else
		    emitloc ($1); emit (RtnOp);
	    }
	;


assignment_expression
	: or_expression ASSIGN assignment_expression
	    {emitloc ($2); emit (AsgnOp);}
	| or_expression
	;


or_expression
	: or_expression or and_expression
	    {patch ($2, JnzOp, ip); emit (TestOp);}
	| and_expression
	;


and_expression
	: and_expression and equality_expression
	    {patch ($2, JzOp, ip); emit (TestOp);}
	| equality_expression
	;


equality_expression
	: equality_expression EQ relational_expression
	    {emitloc ($2); emit (EqOp);}
	| equality_expression NE relational_expression
	    {emitloc ($2); emit (NeOp);}
	| relational_expression
	;


relational_expression
	: relational_expression LT range_expression
	    {emitloc ($2); emit (LtOp);}
	| relational_expression GT range_expression
	    {emitloc ($2); emit (GtOp);}
	| relational_expression LE range_expression
	    {emitloc ($2); emit (LeOp);}
	| relational_expression GE range_expression
	    {emitloc ($2); emit (GeOp);}
	| range_expression
	;


range_expression
	: range_expression COLON additive_expression
	    {emitloc ($2); emit (RangeOp, 2);}
	| range_expression COLON additive_expression COLON additive_expression
	    {emitloc ($2); emit (RangeOp, 3);}
	| additive_expression
	;


additive_expression
	: additive_expression PLUS multiplicative_expression
	    {emitloc ($2); emit (AddOp);}
	| additive_expression MINUS multiplicative_expression
	    {emitloc ($2); emit (SubOp);}
	| multiplicative_expression
	;

multiplicative_expression
	: multiplicative_expression MULT exponentiation_expression
	    {emitloc ($2); emit (MulOp);}
	| multiplicative_expression DIV exponentiation_expression
	    {emitloc ($2); emit (DivOp);}
	| multiplicative_expression MOD exponentiation_expression
	    {emitloc ($2); emit (ModOp);}
	| multiplicative_expression BKSLV exponentiation_expression
	    {emitloc ($2); emit (BkslvOp);}
	| exponentiation_expression
	;


exponentiation_expression
	: postfix_expression POW exponentiation_expression
	    {emitloc ($2); emit (PowOp);}
	| postfix_expression
	;


postfix_expression
	: postfix_expression TRANS
	    {emitloc ($2); emit (TransOp);}
	| prefix_expression
	;


prefix_expression
	: NOT prefix_expression
	    {emitloc ($1); emit (NotOp);}
	| MINUS prefix_expression
	    {emitloc ($1); emit (NegOp);}
	| PLUS prefix_expression
	    {emitloc ($1); emit (PlusOp);}
	| primary_expression
	;


primary_expression
	: LPAREN expression_block RPAREN
	    {/* prevents warnings */}

	| primary_expression DOT ID
	    {
		ste *s = st_lookup (&field_st, $3);
		emitloc ($2);
		emit (FieldOp, s ? s -> idx : 0);
		Deallocate ($3);
	    }

	| primary_expression LPAREN opt_index_expression_list RPAREN
	    {emitloc ($2); emit (ApplyOp, $3);}

	| control_expression
	| matrix_expression
	| literal
	;


opt_index_expression_list
	: index_expression_list
	| /* empty */
	    {$$ = 0;}
	;


index_expression_list
	: index_expression_list COMMA index_expression
	    {$$ = $1 + 1;}
	| index_expression
	    {$$ = 1;}
	;


index_expression
	: expression
	| COLON
	    {emitloc ($1); emit (RangeOp, 0);}
	;


control_expression
	: if expression fail THEN expression_block else_expression
	| WHILE mark expression do_while expression_block END
	    {emit (PopOp, 1); emit (JmpOp, $2); patch ($4, FailOp, ip);
	     exit_loop ($2, ip);}
	| FOR expression in_expression do_for expression_block END
	    {emit (PopOp, 1); emit (JmpOp, $3); patch ($3, GenOp, ip);
	     exit_loop ($3, ip);}
	| NEXT
	    {end_break (NEXT, start_break (NEXT));}
	;


else_expression
	: END
	    {patch ($<ival>-2, FailOp, ip);}
	| else expression_block END
	    {patch ($1, JmpOp, ip);}
	| else ELIF expression fail THEN expression_block else_expression
	    {patch ($1, JmpOp, ip);}
	;


matrix_expression
	: LBRACK matrix_row_list RBRACK
	    {emitloc ($1); emit (MtxOp, $2);}
	;


matrix_row_list
	: matrix_row_list semicol_list expression_list
	    {emitloc ($2); emit (RowOp, $3); $$ = $1 + 1;}
	| expression_list
	    {emit (RowOp, $1); $$ = 1;}
	;


semicol_list
	: semicol_list SEMICOL
	    {$$ = $2;}
	| SEMICOL
	    {$$ = $1;}
	;


literal
	: ID
	    {
		ste *s = add_literal (vars, $1, LocalOp);
		emit (s -> op, s -> idx);
		Deallocate ($1);
	    }

	| STRLIT
	    {
		ste *s = add_literal (&str_st, $1, StrOp);
		emit (StrOp, s -> idx);
		Deallocate ($1);
	    }

	| NUMLIT
	    {
		ste *s = add_literal (&dbl_st, $1, DblOp);
		emit (DblOp, s -> idx);
		Deallocate ($1);
	    }

	| CONSTANT
	    {
		ste *s = st_lookup (&var_st, $1);
		if (!s) {
		    cterror ("unknown constant '%s' (using null value)", $1);
		    s = add_literal (vars, $1, NullOp);
		}
		emit (s -> op, s -> idx);
		Deallocate ($1);
	    }
	;


pop
	: /* empty */
	    {emit (PopOp, 1);}
	;


or
	: OR
	    {emitloc ($1); emit (CopyOp); $$ = ip; emit (JnzOp, 0);
	     emit (PopOp, 1);}
	;


and
	: AND
	    {emitloc ($1); emit (CopyOp); $$ = ip; emit (JzOp, 0);
	     emit (PopOp, 1);}
	;


if
	: IF
	    {emitloc ($1);}
	;


else
	: ELSE
	    {$$ = ip; emit (JmpOp, 0); patch ($<ival>-2, JzOp, ip);}
	;


mark
	: /* empty */
	    {$$ = ip;}
	;


fail
	: /* empty */
	    {$$ = ip; emit (FailOp, 0);}
	;


in_expression
	: IN expression
	    {emit (DblOp, 1); emitloc ($1); $$ = ip; emit (GenOp, 0);}
	;


break
	: BREAK
	    {$$ = (void *) start_break (BREAK);}
	;


do_while
	: DO
	    {emitloc ($1); enter_loop (WHILE); $$ = ip; emit (FailOp, 0);}
	;


do_for
	: DO
	    {enter_loop (FOR);}
	;

%%

/************************************************************************
 * Function:	emitloc							*
 *									*
 * Description:	Emits a file and/or line instruction if they have	*
 *		changed since the last time that they were emitted.	*
 ************************************************************************/

static void emitloc (burlap_yyloc loc)
{
    if (last_file_num != loc.file)
	emit (FileOp, last_file_num = loc.file);

    if (last_line_num != loc.line)
	emit (LineOp, last_line_num = loc.line);
}
