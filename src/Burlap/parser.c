
/*  A Bison parser, made from parser.y with Bison version GNU Bison version 1.22
  */

#define BFBISON 1  /* Identify Bison output.  */

#define	BREAK	258
#define	DO	259
#define	ELIF	260
#define	ELSE	261
#define	END	262
#define	FOR	263
#define	FUNCTION	264
#define	GLOBAL	265
#define	IF	266
#define	IN	267
#define	NEXT	268
#define	RETURN	269
#define	SHARED	270
#define	THEN	271
#define	WHILE	272
#define	ASSIGN	273
#define	OR	274
#define	AND	275
#define	EQ	276
#define	NE	277
#define	LT	278
#define	GT	279
#define	LE	280
#define	GE	281
#define	COLON	282
#define	PLUS	283
#define	MINUS	284
#define	MULT	285
#define	DIV	286
#define	MOD	287
#define	BKSLV	288
#define	POW	289
#define	TRANS	290
#define	NOT	291
#define	DOT	292
#define	LPAREN	293
#define	RPAREN	294
#define	LBRACK	295
#define	RBRACK	296
#define	COMMA	297
#define	SEMICOL	298
#define	ID	299
#define	STRLIT	300
#define	NUMLIT	301
#define	CONSTANT	302

#line 1 "parser.y"

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

Code global_cs;

static st  *vars;
static st   local_st;

static void emitloc PROTO ((bfloc));

static char arg_types [256];		/* should be plenty */
static int  last_line_num = -1;
static int  last_file_num = -1;
static int  syntax_error;

#line 56 "parser.y"
typedef union {
    int		ival;
    Address	addr;
    char       *sval;
    descriptor *desc;
    bfloc	loc;
    void       *ptr;
} BFSTYPE;

#ifndef BFLTYPE
typedef
  struct bfltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  bfltype;

#define BFLTYPE bfltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	BFFINAL		171
#define	BFFLAG		-32768
#define	BFNTBASE	48

#define BFTRANSLATE(x) ((unsigned)(x) <= 302 ? bftranslate[x] : 96)

static const char bftranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47
};

#if BFDEBUG != 0
static const short bfprhs[] = {     0,
     0,     2,     6,     7,    11,    15,    18,    19,    27,    30,
    32,    33,    36,    41,    43,    44,    47,    52,    53,    57,
    59,    61,    64,    69,    71,    72,    76,    78,    80,    81,
    83,    86,    89,    93,    95,    99,   101,   105,   107,   111,
   115,   117,   121,   125,   129,   133,   135,   139,   145,   147,
   151,   155,   157,   161,   165,   169,   173,   175,   179,   181,
   184,   186,   189,   192,   195,   197,   201,   205,   210,   212,
   214,   216,   218,   219,   223,   225,   227,   229,   236,   243,
   250,   252,   254,   258,   266,   270,   274,   276,   279,   281,
   283,   285,   287,   289,   290,   292,   294,   296,   298,   299,
   300,   303,   305,   307
};

static const short bfrhs[] = {    50,
     0,     1,    49,    50,     0,     0,    50,    51,    43,     0,
    50,    63,    43,     0,    50,    43,     0,     0,    52,    38,
    53,    39,    43,    56,     7,     0,     9,    44,     0,    54,
     0,     0,    55,    44,     0,    54,    42,    55,    44,     0,
    15,     0,     0,    57,    60,     0,    57,    10,    58,    43,
     0,     0,    58,    42,    59,     0,    59,     0,    44,     0,
    60,    43,     0,    60,    43,    85,    63,     0,    63,     0,
     0,    61,    42,    63,     0,    63,     0,    63,     0,     0,
    64,     0,    93,    62,     0,    14,    62,     0,    65,    18,
    64,     0,    65,     0,    65,    86,    66,     0,    66,     0,
    66,    87,    67,     0,    67,     0,    67,    21,    68,     0,
    67,    22,    68,     0,    68,     0,    68,    23,    69,     0,
    68,    24,    69,     0,    68,    25,    69,     0,    68,    26,
    69,     0,    69,     0,    69,    27,    70,     0,    69,    27,
    70,    27,    70,     0,    70,     0,    70,    28,    71,     0,
    70,    29,    71,     0,    71,     0,    71,    30,    72,     0,
    71,    31,    72,     0,    71,    32,    72,     0,    71,    33,
    72,     0,    72,     0,    73,    34,    72,     0,    73,     0,
    73,    35,     0,    74,     0,    36,    74,     0,    29,    74,
     0,    28,    74,     0,    75,     0,    38,    60,    39,     0,
    75,    37,    44,     0,    75,    38,    76,    39,     0,    79,
     0,    81,     0,    84,     0,    77,     0,     0,    77,    42,
    78,     0,    78,     0,    63,     0,    27,     0,    88,    63,
    91,    16,    60,    80,     0,    17,    90,    63,    94,    60,
     7,     0,     8,    63,    92,    95,    60,     7,     0,    13,
     0,     7,     0,    89,    60,     7,     0,    89,     5,    63,
    91,    16,    60,    80,     0,    40,    82,    41,     0,    82,
    83,    61,     0,    61,     0,    83,    43,     0,    43,     0,
    44,     0,    45,     0,    46,     0,    47,     0,     0,    19,
     0,    20,     0,    11,     0,     6,     0,     0,     0,    12,
    63,     0,     3,     0,     4,     0,     4,     0
};

#endif

#if BFDEBUG != 0
static const short bfrline[] = { 0,
    89,    92,    98,   113,   115,   128,   130,   139,   169,   196,
   198,   204,   211,   227,   230,   236,   241,   242,   247,   248,
   253,   263,   264,   265,   266,   272,   274,   280,   281,   287,
   288,   290,   302,   304,   309,   311,   316,   318,   323,   325,
   327,   332,   334,   336,   338,   340,   345,   347,   349,   354,
   356,   358,   362,   364,   366,   368,   370,   375,   377,   382,
   384,   389,   391,   393,   395,   400,   403,   411,   414,   415,
   416,   421,   422,   428,   430,   436,   437,   443,   444,   447,
   450,   456,   458,   460,   466,   472,   474,   480,   482,   488,
   495,   502,   509,   523,   529,   536,   543,   548,   554,   560,
   566,   572,   578,   584
};

static const char * const bftname[] = {   "$","error","$illegal.","BREAK","DO",
"ELIF","ELSE","END","FOR","FUNCTION","GLOBAL","IF","IN","NEXT","RETURN","SHARED",
"THEN","WHILE","ASSIGN","OR","AND","EQ","NE","LT","GT","LE","GE","COLON","PLUS",
"MINUS","MULT","DIV","MOD","BKSLV","POW","TRANS","NOT","DOT","LPAREN","RPAREN",
"LBRACK","RBRACK","COMMA","SEMICOL","ID","STRLIT","NUMLIT","CONSTANT","program",
"error_action","function_or_expression_list","function_definition","function_id",
"opt_formal_list","formal_list","arg_type","function_body","global_declarations",
"global_id_list","global_id","expression_block","expression_list","opt_expression",
"expression","assignment_expression","or_expression","and_expression","equality_expression",
"relational_expression","range_expression","additive_expression","multiplicative_expression",
"exponentiation_expression","postfix_expression","prefix_expression","primary_expression",
"opt_index_expression_list","index_expression_list","index_expression","control_expression",
"else_expression","matrix_expression","matrix_row_list","semicol_list","literal",
"pop","or","and","if","else","mark","fail","in_expression","break","do_while",
"do_for",""
};
#endif

static const short bfr1[] = {     0,
    48,    48,    49,    50,    50,    50,    50,    51,    52,    53,
    53,    54,    54,    55,    55,    56,    57,    57,    58,    58,
    59,    60,    60,    60,    60,    61,    61,    62,    62,    63,
    63,    63,    64,    64,    65,    65,    66,    66,    67,    67,
    67,    68,    68,    68,    68,    68,    69,    69,    69,    70,
    70,    70,    71,    71,    71,    71,    71,    72,    72,    73,
    73,    74,    74,    74,    74,    75,    75,    75,    75,    75,
    75,    76,    76,    77,    77,    78,    78,    79,    79,    79,
    79,    80,    80,    80,    81,    82,    82,    83,    83,    84,
    84,    84,    84,    85,    86,    87,    88,    89,    90,    91,
    92,    93,    94,    95
};

static const short bfr2[] = {     0,
     1,     3,     0,     3,     3,     2,     0,     7,     2,     1,
     0,     2,     4,     1,     0,     2,     4,     0,     3,     1,
     1,     2,     4,     1,     0,     3,     1,     1,     0,     1,
     2,     2,     3,     1,     3,     1,     3,     1,     3,     3,
     1,     3,     3,     3,     3,     1,     3,     5,     1,     3,
     3,     1,     3,     3,     3,     3,     1,     3,     1,     2,
     1,     2,     2,     2,     1,     3,     3,     4,     1,     1,
     1,     1,     0,     3,     1,     1,     1,     6,     6,     6,
     1,     1,     3,     7,     3,     3,     1,     2,     1,     1,
     1,     1,     1,     0,     1,     1,     1,     1,     0,     0,
     2,     1,     1,     1
};

static const short bfdefact[] = {     0,
     3,     1,     7,   102,     0,     0,    97,    81,    29,    99,
     0,     0,     0,    25,     0,     6,    90,    91,    92,    93,
     0,     0,     0,    30,    34,    36,    38,    41,    46,    49,
    52,    57,    59,    61,    65,    69,    70,    71,     0,    29,
     2,     0,     9,    32,    28,     0,    64,    63,    62,     0,
    24,    87,    27,     0,     4,    11,     5,     0,    95,     0,
    96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    60,     0,    73,   100,
    31,     0,     0,     0,    66,    94,     0,    85,    89,     0,
    14,     0,    10,     0,    33,    35,    37,    39,    40,    42,
    43,    44,    45,    47,    50,    51,    53,    54,    55,    56,
    58,    67,    77,    76,     0,    72,    75,     0,   101,   104,
    25,   103,    25,     0,    26,    88,    86,     0,    15,    12,
     0,    68,     0,    25,     0,     0,    23,    18,     0,    48,
    74,     0,    80,    79,     0,    25,    13,    98,    82,    78,
    25,     8,     0,    16,     0,     0,    21,     0,    20,   100,
    83,     0,    17,     0,    19,    25,     0,    84,     0,     0,
     0
};

static const short bfdefgoto[] = {   169,
     3,     2,    21,    22,    92,    93,    94,   145,   146,   158,
   159,    50,    52,    44,    51,    24,    25,    26,    27,    28,
    29,    30,    31,    32,    33,    34,    35,   115,   116,   117,
    36,   150,    37,    54,    90,    38,   124,    60,    62,    39,
   151,    46,   118,    83,    40,   123,   121
};

static const short bfpact[] = {    14,
-32768,    73,-32768,-32768,   232,   -20,-32768,-32768,   232,-32768,
   245,   245,   245,   232,   232,-32768,-32768,-32768,-32768,-32768,
   -17,     8,    10,-32768,    11,    36,    -1,    46,    48,     7,
    65,-32768,    80,-32768,    -5,-32768,-32768,-32768,   232,   232,
    73,    61,-32768,-32768,-32768,   232,-32768,-32768,-32768,   -37,
-32768,    41,-32768,   -25,-32768,   -10,-32768,   245,-32768,   245,
-32768,   245,   245,   245,   245,   245,   245,   245,   245,   245,
   245,   245,   245,   245,   245,   245,-32768,    34,   121,-32768,
-32768,   232,    85,   106,-32768,     2,   232,-32768,-32768,   161,
-32768,    82,    70,    79,-32768,    36,    -1,    46,    46,    48,
    48,    48,    48,    64,    65,    65,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,    89,    88,-32768,   117,-32768,-32768,
   232,-32768,   232,   232,-32768,-32768,    41,    96,   125,-32768,
   245,-32768,   121,   232,    -3,     5,-32768,-32768,    97,     7,
-32768,     4,-32768,-32768,   135,   174,-32768,-32768,-32768,-32768,
   219,-32768,    99,   101,   232,    12,-32768,    57,-32768,-32768,
-32768,    99,-32768,   129,-32768,   232,     4,-32768,   146,   147,
-32768
};

static const short bfpgoto[] = {-32768,
-32768,   148,-32768,-32768,-32768,-32768,    23,-32768,-32768,-32768,
    -7,   -72,    68,   116,    -2,   102,-32768,    94,   100,    44,
    38,   -68,    66,    -8,-32768,   114,-32768,-32768,-32768,    30,
-32768,     3,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,    13,-32768,-32768,-32768,-32768
};


#define	BFLAST		292


static const short bftable[] = {    23,
   104,    85,    42,   143,    91,    86,    45,   -22,   -22,   148,
   149,   144,    53,    -7,     1,    88,    -7,    89,   161,    63,
    64,    -7,    -7,    43,    -7,    55,    -7,    -7,    58,    59,
    -7,    78,    79,   -15,    70,    71,    80,    45,    23,    86,
   -22,    -7,    -7,    84,   -22,    56,    86,    86,   135,    -7,
   136,    -7,    57,    -7,    86,    61,    -7,    -7,    -7,    -7,
    -7,   142,   140,   107,   108,   109,   110,   111,    65,    66,
    67,    68,    82,   154,    69,     4,   114,   112,   156,   119,
     5,     6,    87,     7,   125,     8,     9,    53,   120,    10,
   131,    70,    71,   167,    72,    73,    74,    75,   162,   163,
    11,    12,   100,   101,   102,   103,    98,    99,    13,   122,
    14,   129,    15,    76,    77,    16,    17,    18,    19,    20,
   128,   137,   130,     4,    47,    48,    49,   132,     5,   133,
   114,     7,   134,     8,     9,   105,   106,    10,   138,    91,
   147,   152,   157,    86,   166,   170,   171,   113,    11,    12,
    41,   139,   160,    96,   165,    81,    13,   127,    14,    95,
    15,    97,   141,     4,    17,    18,    19,    20,     5,   168,
     0,     7,   164,     8,     9,     0,     4,    10,     0,     0,
     0,     5,     0,   153,     7,     0,     8,     9,    11,    12,
    10,     0,     0,     0,     0,     0,    13,     0,    14,     0,
    15,    11,    12,   126,    17,    18,    19,    20,     0,    13,
     0,    14,     0,    15,     0,     0,     0,    17,    18,    19,
    20,     4,     0,   155,     0,     0,     5,     0,     0,     7,
     0,     8,     9,     0,     4,    10,     0,     0,     0,     5,
     0,     0,     7,     0,     8,     9,    11,    12,    10,     0,
     0,     0,     5,     0,    13,     7,    14,     8,    15,    11,
    12,    10,    17,    18,    19,    20,     0,    13,     0,    14,
     0,    15,    11,    12,     0,    17,    18,    19,    20,     0,
    13,     0,    14,     0,    15,     0,     0,     0,    17,    18,
    19,    20
};

static const short bfcheck[] = {     2,
    69,    39,     5,     7,    15,    43,     9,     6,     7,     6,
     7,     7,    15,     0,     1,    41,     3,    43,     7,    21,
    22,     8,     9,    44,    11,    43,    13,    14,    18,    19,
    17,    37,    38,    44,    28,    29,    39,    40,    41,    43,
    39,    28,    29,    46,    43,    38,    43,    43,   121,    36,
   123,    38,    43,    40,    43,    20,    43,    44,    45,    46,
    47,   134,   131,    72,    73,    74,    75,    76,    23,    24,
    25,    26,    12,   146,    27,     3,    79,    44,   151,    82,
     8,     9,    42,    11,    87,    13,    14,    90,     4,    17,
    27,    28,    29,   166,    30,    31,    32,    33,    42,    43,
    28,    29,    65,    66,    67,    68,    63,    64,    36,     4,
    38,    42,    40,    34,    35,    43,    44,    45,    46,    47,
    39,   124,    44,     3,    11,    12,    13,    39,     8,    42,
   133,    11,    16,    13,    14,    70,    71,    17,    43,    15,
    44,     7,    44,    43,    16,     0,     0,    27,    28,    29,
     3,   129,   155,    60,   162,    40,    36,    90,    38,    58,
    40,    62,   133,     3,    44,    45,    46,    47,     8,   167,
    -1,    11,   160,    13,    14,    -1,     3,    17,    -1,    -1,
    -1,     8,    -1,    10,    11,    -1,    13,    14,    28,    29,
    17,    -1,    -1,    -1,    -1,    -1,    36,    -1,    38,    -1,
    40,    28,    29,    43,    44,    45,    46,    47,    -1,    36,
    -1,    38,    -1,    40,    -1,    -1,    -1,    44,    45,    46,
    47,     3,    -1,     5,    -1,    -1,     8,    -1,    -1,    11,
    -1,    13,    14,    -1,     3,    17,    -1,    -1,    -1,     8,
    -1,    -1,    11,    -1,    13,    14,    28,    29,    17,    -1,
    -1,    -1,     8,    -1,    36,    11,    38,    13,    40,    28,
    29,    17,    44,    45,    46,    47,    -1,    36,    -1,    38,
    -1,    40,    28,    29,    -1,    44,    45,    46,    47,    -1,
    36,    -1,    38,    -1,    40,    -1,    -1,    -1,    44,    45,
    46,    47
};
#define BFPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define bferrok		(bferrstatus = 0)
#define bfclearin	(bfchar = BFEMPTY)
#define BFEMPTY		-2
#define BFEOF		0
#define BFACCEPT	return(0)
#define BFABORT 	return(1)
#define BFERROR		goto bferrlab1
/* Like BFERROR except do call bferror.
   This remains here temporarily to ease the
   transition to the new meaning of BFERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define BFFAIL		goto bferrlab
#define BFRECOVERING()  (!!bferrstatus)
#define BFBACKUP(token, value) \
do								\
  if (bfchar == BFEMPTY && bflen == 1)				\
    { bfchar = (token), bflval = (value);			\
      bfchar1 = BFTRANSLATE (bfchar);				\
      BFPOPSTACK;						\
      goto bfbackup;						\
    }								\
  else								\
    { bferror ("syntax error: cannot back up"); BFERROR; }	\
while (0)

#define BFTERROR	1
#define BFERRCODE	256

#ifndef BFPURE
#define BFLEX		bflex()
#endif

#ifdef BFPURE
#ifdef BFLSP_NEEDED
#define BFLEX		bflex(&bflval, &bflloc)
#else
#define BFLEX		bflex(&bflval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef BFPURE

int	bfchar;			/*  the lookahead symbol		*/
BFSTYPE	bflval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef BFLSP_NEEDED
BFLTYPE bflloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int bfnerrs;			/*  number of parse errors so far       */
#endif  /* not BFPURE */

#if BFDEBUG != 0
int bfdebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  BFINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	BFINITDEPTH
#define BFINITDEPTH 200
#endif

/*  BFMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if BFMAXDEPTH == 0
#undef BFMAXDEPTH
#endif

#ifndef BFMAXDEPTH
#define BFMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int bfparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __bf_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__bf_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__bf_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/usr/lib/bison.simple"
int
bfparse()
{
  register int bfstate;
  register int bfn;
  register short *bfssp;
  register BFSTYPE *bfvsp;
  int bferrstatus;	/*  number of tokens to shift before error messages enabled */
  int bfchar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	bfssa[BFINITDEPTH];	/*  the state stack			*/
  BFSTYPE bfvsa[BFINITDEPTH];	/*  the semantic value stack		*/

  short *bfss = bfssa;		/*  refer to the stacks thru separate pointers */
  BFSTYPE *bfvs = bfvsa;	/*  to allow bfoverflow to reallocate them elsewhere */

#ifdef BFLSP_NEEDED
  BFLTYPE bflsa[BFINITDEPTH];	/*  the location stack			*/
  BFLTYPE *bfls = bflsa;
  BFLTYPE *bflsp;

#define BFPOPSTACK   (bfvsp--, bfssp--, bflsp--)
#else
#define BFPOPSTACK   (bfvsp--, bfssp--)
#endif

  int bfstacksize = BFINITDEPTH;

#ifdef BFPURE
  int bfchar;
  BFSTYPE bflval;
  int bfnerrs;
#ifdef BFLSP_NEEDED
  BFLTYPE bflloc;
#endif
#endif

  BFSTYPE bfval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int bflen;

#if BFDEBUG != 0
  if (bfdebug)
    fprintf(stderr, "Starting parse\n");
#endif

  bfstate = 0;
  bferrstatus = 0;
  bfnerrs = 0;
  bfchar = BFEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  bfssp = bfss - 1;
  bfvsp = bfvs;
#ifdef BFLSP_NEEDED
  bflsp = bfls;
#endif

/* Push a new state, which is found in  bfstate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
bfnewstate:

  *++bfssp = bfstate;

  if (bfssp >= bfss + bfstacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      BFSTYPE *bfvs1 = bfvs;
      short *bfss1 = bfss;
#ifdef BFLSP_NEEDED
      BFLTYPE *bfls1 = bfls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = bfssp - bfss + 1;

#ifdef bfoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef BFLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if bfoverflow is a macro.  */
      bfoverflow("parser stack overflow",
		 &bfss1, size * sizeof (*bfssp),
		 &bfvs1, size * sizeof (*bfvsp),
		 &bfls1, size * sizeof (*bflsp),
		 &bfstacksize);
#else
      bfoverflow("parser stack overflow",
		 &bfss1, size * sizeof (*bfssp),
		 &bfvs1, size * sizeof (*bfvsp),
		 &bfstacksize);
#endif

      bfss = bfss1; bfvs = bfvs1;
#ifdef BFLSP_NEEDED
      bfls = bfls1;
#endif
#else /* no bfoverflow */
      /* Extend the stack our own way.  */
      if (bfstacksize >= BFMAXDEPTH)
	{
	  bferror("parser stack overflow");
	  return 2;
	}
      bfstacksize *= 2;
      if (bfstacksize > BFMAXDEPTH)
	bfstacksize = BFMAXDEPTH;
      bfss = (short *) alloca (bfstacksize * sizeof (*bfssp));
      __bf_bcopy ((char *)bfss1, (char *)bfss, size * sizeof (*bfssp));
      bfvs = (BFSTYPE *) alloca (bfstacksize * sizeof (*bfvsp));
      __bf_bcopy ((char *)bfvs1, (char *)bfvs, size * sizeof (*bfvsp));
#ifdef BFLSP_NEEDED
      bfls = (BFLTYPE *) alloca (bfstacksize * sizeof (*bflsp));
      __bf_bcopy ((char *)bfls1, (char *)bfls, size * sizeof (*bflsp));
#endif
#endif /* no bfoverflow */

      bfssp = bfss + size - 1;
      bfvsp = bfvs + size - 1;
#ifdef BFLSP_NEEDED
      bflsp = bfls + size - 1;
#endif

#if BFDEBUG != 0
      if (bfdebug)
	fprintf(stderr, "Stack size increased to %d\n", bfstacksize);
#endif

      if (bfssp >= bfss + bfstacksize - 1)
	BFABORT;
    }

#if BFDEBUG != 0
  if (bfdebug)
    fprintf(stderr, "Entering state %d\n", bfstate);
#endif

  goto bfbackup;
 bfbackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* bfresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  bfn = bfpact[bfstate];
  if (bfn == BFFLAG)
    goto bfdefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* bfchar is either BFEMPTY or BFEOF
     or a valid token in external form.  */

  if (bfchar == BFEMPTY)
    {
#if BFDEBUG != 0
      if (bfdebug)
	fprintf(stderr, "Reading a token: ");
#endif
      bfchar = BFLEX;
    }

  /* Convert token to internal form (in bfchar1) for indexing tables with */

  if (bfchar <= 0)		/* This means end of input. */
    {
      bfchar1 = 0;
      bfchar = BFEOF;		/* Don't call BFLEX any more */

#if BFDEBUG != 0
      if (bfdebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      bfchar1 = BFTRANSLATE(bfchar);

#if BFDEBUG != 0
      if (bfdebug)
	{
	  fprintf (stderr, "Next token is %d (%s", bfchar, bftname[bfchar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef BFPRINT
	  BFPRINT (stderr, bfchar, bflval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  bfn += bfchar1;
  if (bfn < 0 || bfn > BFLAST || bfcheck[bfn] != bfchar1)
    goto bfdefault;

  bfn = bftable[bfn];

  /* bfn is what to do for this token type in this state.
     Negative => reduce, -bfn is rule number.
     Positive => shift, bfn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (bfn < 0)
    {
      if (bfn == BFFLAG)
	goto bferrlab;
      bfn = -bfn;
      goto bfreduce;
    }
  else if (bfn == 0)
    goto bferrlab;

  if (bfn == BFFINAL)
    BFACCEPT;

  /* Shift the lookahead token.  */

#if BFDEBUG != 0
  if (bfdebug)
    fprintf(stderr, "Shifting token %d (%s), ", bfchar, bftname[bfchar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (bfchar != BFEOF)
    bfchar = BFEMPTY;

  *++bfvsp = bflval;
#ifdef BFLSP_NEEDED
  *++bflsp = bflloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (bferrstatus) bferrstatus--;

  bfstate = bfn;
  goto bfnewstate;

/* Do the default action for the current state.  */
bfdefault:

  bfn = bfdefact[bfstate];
  if (bfn == 0)
    goto bferrlab;

/* Do a reduction.  bfn is the number of a rule to reduce with.  */
bfreduce:
  bflen = bfr2[bfn];
  if (bflen > 0)
    bfval = bfvsp[1-bflen]; /* implement default value of the action */

#if BFDEBUG != 0
  if (bfdebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       bfn, bfrline[bfn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = bfprhs[bfn]; bfrhs[i] > 0; i++)
	fprintf (stderr, "%s ", bftname[bfrhs[i]]);
      fprintf (stderr, " -> %s\n", bftname[bfr1[bfn]]);
    }
#endif


  switch (bfn) {

case 1:
#line 90 "parser.y"
{syntax_error = 0;;
    break;}
case 2:
#line 93 "parser.y"
{syntax_error = 0;;
    break;}
case 3:
#line 99 "parser.y"
{
		bferrok;
		bfclearin;
		cs = global_cs;
		exit_all ( );
		reset ( );

		if (!interactive)
		    syntax_error = 1;
	    ;
    break;}
case 5:
#line 116 "parser.y"
{
		if (!syntax_error) {
		    emit (HaltOp);
		    if (execute (cs, var_array, NULL) != -1 && !interactive)
			return 1;
		}

		reset ( );
		last_line_num = -1;
		last_file_num = -1;
	    ;
    break;}
case 7:
#line 131 "parser.y"
{
		vars = &var_st;
		reset ( );
	    ;
    break;}
case 8:
#line 140 "parser.y"
{
		int i;


		D_Function (bfvsp[-6].desc) -> num_args = bfvsp[-4].ival;
		D_Function (bfvsp[-6].desc) -> num_locals = vars -> num_syms;
		D_Function (bfvsp[-6].desc) -> local_names = st_names (vars);

		D_Function (bfvsp[-6].desc) -> arg_types = Allocate (char, bfvsp[-4].ival);
		for (i = 0; i < bfvsp[-4].ival; i ++)
		    D_Function (bfvsp[-6].desc) -> arg_types [i] = arg_types [i];

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
	    ;
    break;}
case 9:
#line 170 "parser.y"
{
		ste	   *s;
		descriptor  d;


		s = add_literal (&var_st, bfvsp[0].sval, GlblOp);
		d = *(bfval.desc = &var_array [s -> idx]);
		Deallocate (bfvsp[0].sval);

		FreeData (bfval.desc);
		CreateData (bfval.desc, NULL, NULL, T_Function, s -> name);
		D_Temp (bfval.desc) = F_False;

		cs = D_Function (bfval.desc) -> cs;
		st_init (vars = &local_st);
		st_init (&import_st);
		reset ( );

		last_file_num = -1;
		last_line_num = -1;
		emitloc (bfvsp[-1].loc);
	    ;
    break;}
case 11:
#line 199 "parser.y"
{bfval.ival = 0;;
    break;}
case 12:
#line 205 "parser.y"
{
		st_insert (&import_st, bfvsp[0].sval, ArgOp);
		arg_types [0] = bfvsp[-1].ival; bfval.ival = 1;
		Deallocate (bfvsp[0].sval);
	    ;
    break;}
case 13:
#line 212 "parser.y"
{
		ste *s = st_lookup (&import_st, bfvsp[0].sval);
		if (s) {
		    cterror ("duplicate argument '%s'", bfvsp[0].sval);
		    bfval.ival = -1;
		} else if (bfvsp[-3].ival > 0) {
		    st_insert (&import_st, bfvsp[0].sval, ArgOp);
		    arg_types [bfvsp[-3].ival] = bfvsp[-1].ival; bfval.ival = bfvsp[-3].ival + 1;
		}
		Deallocate (bfvsp[0].sval);
	    ;
    break;}
case 14:
#line 228 "parser.y"
{bfval.ival = SharedArg;;
    break;}
case 15:
#line 231 "parser.y"
{bfval.ival = ValueArg;;
    break;}
case 21:
#line 254 "parser.y"
{
		ste *s = add_literal (&var_st, bfvsp[0].sval, GlblOp);
		add_literal (&import_st, bfvsp[0].sval, GlblOp) -> idx = s -> idx;
		Deallocate (bfvsp[0].sval);
	    ;
    break;}
case 25:
#line 267 "parser.y"
{emit (DblOp, 0);;
    break;}
case 26:
#line 273 "parser.y"
{bfval.ival = bfvsp[-2].ival + 1;;
    break;}
case 27:
#line 275 "parser.y"
{bfval.ival = 1;;
    break;}
case 29:
#line 282 "parser.y"
{emit (NullOp);;
    break;}
case 31:
#line 289 "parser.y"
{end_break (BREAK, bfvsp[-1].ptr);;
    break;}
case 32:
#line 291 "parser.y"
{
		if (cs == global_cs) {
		    cterror ("return not within a function");
		    emit (NullOp);
		} else
		    emitloc (bfvsp[-1].loc); emit (RtnOp);
	    ;
    break;}
case 33:
#line 303 "parser.y"
{emitloc (bfvsp[-1].loc); emit (AsgnOp);;
    break;}
case 35:
#line 310 "parser.y"
{patch (bfvsp[-1].addr, JnzOp, ip); emit (TestOp);;
    break;}
case 37:
#line 317 "parser.y"
{patch (bfvsp[-1].addr, JzOp, ip); emit (TestOp);;
    break;}
case 39:
#line 324 "parser.y"
{emitloc (bfvsp[-1].loc); emit (EqOp);;
    break;}
case 40:
#line 326 "parser.y"
{emitloc (bfvsp[-1].loc); emit (NeOp);;
    break;}
case 42:
#line 333 "parser.y"
{emitloc (bfvsp[-1].loc); emit (LtOp);;
    break;}
case 43:
#line 335 "parser.y"
{emitloc (bfvsp[-1].loc); emit (GtOp);;
    break;}
case 44:
#line 337 "parser.y"
{emitloc (bfvsp[-1].loc); emit (LeOp);;
    break;}
case 45:
#line 339 "parser.y"
{emitloc (bfvsp[-1].loc); emit (GeOp);;
    break;}
case 47:
#line 346 "parser.y"
{emitloc (bfvsp[-1].loc); emit (RangeOp, 2);;
    break;}
case 48:
#line 348 "parser.y"
{emitloc (bfvsp[-3].loc); emit (RangeOp, 3);;
    break;}
case 50:
#line 355 "parser.y"
{emitloc (bfvsp[-1].loc); emit (AddOp);;
    break;}
case 51:
#line 357 "parser.y"
{emitloc (bfvsp[-1].loc); emit (SubOp);;
    break;}
case 53:
#line 363 "parser.y"
{emitloc (bfvsp[-1].loc); emit (MulOp);;
    break;}
case 54:
#line 365 "parser.y"
{emitloc (bfvsp[-1].loc); emit (DivOp);;
    break;}
case 55:
#line 367 "parser.y"
{emitloc (bfvsp[-1].loc); emit (ModOp);;
    break;}
case 56:
#line 369 "parser.y"
{emitloc (bfvsp[-1].loc); emit (BkslvOp);;
    break;}
case 58:
#line 376 "parser.y"
{emitloc (bfvsp[-1].loc); emit (PowOp);;
    break;}
case 60:
#line 383 "parser.y"
{emitloc (bfvsp[0].loc); emit (TransOp);;
    break;}
case 62:
#line 390 "parser.y"
{emitloc (bfvsp[-1].loc); emit (NotOp);;
    break;}
case 63:
#line 392 "parser.y"
{emitloc (bfvsp[-1].loc); emit (NegOp);;
    break;}
case 64:
#line 394 "parser.y"
{emitloc (bfvsp[-1].loc); emit (PlusOp);;
    break;}
case 66:
#line 401 "parser.y"
{/* prevents warnings */;
    break;}
case 67:
#line 404 "parser.y"
{
		ste *s = st_lookup (&field_st, bfvsp[0].sval);
		emitloc (bfvsp[-1].loc);
		emit (FieldOp, s ? s -> idx : 0);
		Deallocate (bfvsp[0].sval);
	    ;
    break;}
case 68:
#line 412 "parser.y"
{emitloc (bfvsp[-2].loc); emit (ApplyOp, bfvsp[-1].ival);;
    break;}
case 73:
#line 423 "parser.y"
{bfval.ival = 0;;
    break;}
case 74:
#line 429 "parser.y"
{bfval.ival = bfvsp[-2].ival + 1;;
    break;}
case 75:
#line 431 "parser.y"
{bfval.ival = 1;;
    break;}
case 77:
#line 438 "parser.y"
{emitloc (bfvsp[0].loc); emit (RangeOp, 0);;
    break;}
case 79:
#line 445 "parser.y"
{emit (PopOp, 1); emit (JmpOp, bfvsp[-4].addr); patch (bfvsp[-2].addr, FailOp, ip);
	     exit_loop (bfvsp[-4].addr, ip);;
    break;}
case 80:
#line 448 "parser.y"
{emit (PopOp, 1); emit (JmpOp, bfvsp[-3].addr); patch (bfvsp[-3].addr, GenOp, ip);
	     exit_loop (bfvsp[-3].addr, ip);;
    break;}
case 81:
#line 451 "parser.y"
{end_break (NEXT, start_break (NEXT));;
    break;}
case 82:
#line 457 "parser.y"
{patch (bfvsp[-3].ival, FailOp, ip);;
    break;}
case 83:
#line 459 "parser.y"
{patch (bfvsp[-2].addr, JmpOp, ip);;
    break;}
case 84:
#line 461 "parser.y"
{patch (bfvsp[-6].addr, JmpOp, ip);;
    break;}
case 85:
#line 467 "parser.y"
{emitloc (bfvsp[-2].loc); emit (MtxOp, bfvsp[-1].ival);;
    break;}
case 86:
#line 473 "parser.y"
{emitloc (bfvsp[-1].loc); emit (RowOp, bfvsp[0].ival); bfval.ival = bfvsp[-2].ival + 1;;
    break;}
case 87:
#line 475 "parser.y"
{emit (RowOp, bfvsp[0].ival); bfval.ival = 1;;
    break;}
case 88:
#line 481 "parser.y"
{bfval.loc = bfvsp[0].loc;;
    break;}
case 89:
#line 483 "parser.y"
{bfval.loc = bfvsp[0].loc;;
    break;}
case 90:
#line 489 "parser.y"
{
		ste *s = add_literal (vars, bfvsp[0].sval, LocalOp);
		emit (s -> op, s -> idx);
		Deallocate (bfvsp[0].sval);
	    ;
    break;}
case 91:
#line 496 "parser.y"
{
		ste *s = add_literal (&str_st, bfvsp[0].sval, StrOp);
		emit (StrOp, s -> idx);
		Deallocate (bfvsp[0].sval);
	    ;
    break;}
case 92:
#line 503 "parser.y"
{
		ste *s = add_literal (&dbl_st, bfvsp[0].sval, DblOp);
		emit (DblOp, s -> idx);
		Deallocate (bfvsp[0].sval);
	    ;
    break;}
case 93:
#line 510 "parser.y"
{
		ste *s = st_lookup (&var_st, bfvsp[0].sval);
		if (!s) {
		    cterror ("unknown constant '%s' (using null value)", bfvsp[0].sval);
		    s = add_literal (vars, bfvsp[0].sval, NullOp);
		}
		emit (s -> op, s -> idx);
		Deallocate (bfvsp[0].sval);
	    ;
    break;}
case 94:
#line 524 "parser.y"
{emit (PopOp, 1);;
    break;}
case 95:
#line 530 "parser.y"
{emitloc (bfvsp[0].loc); emit (CopyOp); bfval.addr = ip; emit (JnzOp, 0);
	     emit (PopOp, 1);;
    break;}
case 96:
#line 537 "parser.y"
{emitloc (bfvsp[0].loc); emit (CopyOp); bfval.addr = ip; emit (JzOp, 0);
	     emit (PopOp, 1);;
    break;}
case 97:
#line 544 "parser.y"
{emitloc (bfvsp[0].loc);;
    break;}
case 98:
#line 549 "parser.y"
{bfval.addr = ip; emit (JmpOp, 0); patch (bfvsp[-3].ival, JzOp, ip);;
    break;}
case 99:
#line 555 "parser.y"
{bfval.addr = ip;;
    break;}
case 100:
#line 561 "parser.y"
{bfval.addr = ip; emit (FailOp, 0);;
    break;}
case 101:
#line 567 "parser.y"
{emit (DblOp, 1); emitloc (bfvsp[-1].loc); bfval.addr = ip; emit (GenOp, 0);;
    break;}
case 102:
#line 573 "parser.y"
{bfval.ptr = (void *) start_break (BREAK);;
    break;}
case 103:
#line 579 "parser.y"
{emitloc (bfvsp[0].loc); enter_loop (WHILE); bfval.addr = ip; emit (FailOp, 0);;
    break;}
case 104:
#line 585 "parser.y"
{enter_loop (FOR);;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/lib/bison.simple"

  bfvsp -= bflen;
  bfssp -= bflen;
#ifdef BFLSP_NEEDED
  bflsp -= bflen;
#endif

#if BFDEBUG != 0
  if (bfdebug)
    {
      short *ssp1 = bfss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != bfssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++bfvsp = bfval;

#ifdef BFLSP_NEEDED
  bflsp++;
  if (bflen == 0)
    {
      bflsp->first_line = bflloc.first_line;
      bflsp->first_column = bflloc.first_column;
      bflsp->last_line = (bflsp-1)->last_line;
      bflsp->last_column = (bflsp-1)->last_column;
      bflsp->text = 0;
    }
  else
    {
      bflsp->last_line = (bflsp+bflen-1)->last_line;
      bflsp->last_column = (bflsp+bflen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  bfn = bfr1[bfn];

  bfstate = bfpgoto[bfn - BFNTBASE] + *bfssp;
  if (bfstate >= 0 && bfstate <= BFLAST && bfcheck[bfstate] == *bfssp)
    bfstate = bftable[bfstate];
  else
    bfstate = bfdefgoto[bfn - BFNTBASE];

  goto bfnewstate;

bferrlab:   /* here on detecting error */

  if (! bferrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++bfnerrs;

#ifdef BFERROR_VERBOSE
      bfn = bfpact[bfstate];

      if (bfn > BFFLAG && bfn < BFLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -bfn if nec to avoid negative indexes in bfcheck.  */
	  for (x = (bfn < 0 ? -bfn : 0);
	       x < (sizeof(bftname) / sizeof(char *)); x++)
	    if (bfcheck[x + bfn] == x)
	      size += strlen(bftname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (bfn < 0 ? -bfn : 0);
		       x < (sizeof(bftname) / sizeof(char *)); x++)
		    if (bfcheck[x + bfn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, bftname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      bferror(msg);
	      free(msg);
	    }
	  else
	    bferror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* BFERROR_VERBOSE */
	bferror("parse error");
    }

  goto bferrlab1;
bferrlab1:   /* here on error raised explicitly by an action */

  if (bferrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (bfchar == BFEOF)
	BFABORT;

#if BFDEBUG != 0
      if (bfdebug)
	fprintf(stderr, "Discarding token %d (%s).\n", bfchar, bftname[bfchar1]);
#endif

      bfchar = BFEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  bferrstatus = 3;		/* Each real token shifted decrements this */

  goto bferrhandle;

bferrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  bfn = bfdefact[bfstate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (bfn) goto bfdefault;
#endif

bferrpop:   /* pop the current state because it cannot handle the error token */

  if (bfssp == bfss) BFABORT;
  bfvsp--;
  bfstate = *--bfssp;
#ifdef BFLSP_NEEDED
  bflsp--;
#endif

#if BFDEBUG != 0
  if (bfdebug)
    {
      short *ssp1 = bfss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != bfssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

bferrhandle:

  bfn = bfpact[bfstate];
  if (bfn == BFFLAG)
    goto bferrdefault;

  bfn += BFTERROR;
  if (bfn < 0 || bfn > BFLAST || bfcheck[bfn] != BFTERROR)
    goto bferrdefault;

  bfn = bftable[bfn];
  if (bfn < 0)
    {
      if (bfn == BFFLAG)
	goto bferrpop;
      bfn = -bfn;
      goto bfreduce;
    }
  else if (bfn == 0)
    goto bferrpop;

  if (bfn == BFFINAL)
    BFACCEPT;

#if BFDEBUG != 0
  if (bfdebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++bfvsp = bflval;
#ifdef BFLSP_NEEDED
  *++bflsp = bflloc;
#endif

  bfstate = bfn;
  goto bfnewstate;
}
#line 588 "parser.y"


/************************************************************************
 * Function:	emitloc							*
 *									*
 * Description:	Emits a file and/or line instruction if they have	*
 *		changed since the last time that they were emitted.	*
 ************************************************************************/

static void emitloc (loc)
    bfloc loc;
{
    if (last_file_num != loc.file)
	emit (FileOp, last_file_num = loc.file);

    if (last_line_num != loc.line)
	emit (LineOp, last_line_num = loc.line);
}
