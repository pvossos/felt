
/*  A Bison parser, made from parser.y  */

#define YYBISON 1  /* Identify Bison output.  */

#define	OR	258
#define	AND	259
#define	EQUALS	260
#define	NEQUAL	261
#define	LT_EQ	262
#define	GT_EQ	263
#define	LSHIFT	264
#define	RSHIFT	265
#define	UNARY	266
#define	NAME	267
#define	INTEGER	268
#define	DOUBLE	269
#define	LINEAR	270
#define	SINUSOIDAL	271
#define	COSINUSOIDAL	272
#define	LOGARITHMIC	273
#define	PARABOLIC	274
#define	REVERSE_LOGARITHMIC	275
#define	REVERSE_PARABOLIC	276
#define	SIN	277
#define	COS	278
#define	TAN	279
#define	POW	280
#define	EXP	281
#define	LOG	282
#define	LOG10	283
#define	SQRT	284
#define	HYPOT	285
#define	FLOOR	286
#define	CEIL	287
#define	FMOD	288
#define	FABS	289
#define	LINE	290
#define	GRID	291
#define	QUADGRID	292
#define	BRICKGRID	293
#define	TRIMESH	294
#define	END	295
#define	NODE_EQ	296
#define	ELEMENT_EQ	297
#define	CONSTRAINT_EQ	298
#define	MATERIAL_EQ	299
#define	TYPE_EQ	300
#define	START_EQ	301
#define	END_EQ	302
#define	NUMBER_EQ	303
#define	RULE_EQ	304
#define	X_NUMBER_EQ	305
#define	Y_NUMBER_EQ	306
#define	Z_NUMBER_EQ	307
#define	X_RULE_EQ	308
#define	Y_RULE_EQ	309
#define	Z_RULE_EQ	310
#define	TOLIN_EQ	311
#define	ANGSPC_EQ	312
#define	ANGTOL_EQ	313
#define	DMIN_EQ	314
#define	KAPPA_EQ	315
#define	MIN_EQ	316
#define	MAX_EQ	317
#define	BOUNDARY_EQ	318
#define	HOLE_EQ	319

#line 1 "parser.y"

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

extern void yyerror ( );
extern int  yylex   ( );


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

#line 75 "parser.y"
typedef union {
    int    i;
    double d;
    char  *s;
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __STDC__
#define const
#endif



#define	YYFINAL		266
#define	YYFLAG		-32768
#define	YYNTBASE	84

#define YYTRANSLATE(x) ((unsigned)(x) <= 319 ? yytranslate[x] : 122)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    24,     2,     2,     2,    22,     9,     2,    82,
    83,    20,    18,    81,    19,     2,    21,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     4,     2,    12,
     2,    13,     3,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    79,     2,    80,     8,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     7,     2,    25,     2,     2,     2,     2,
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
     2,     2,     2,     2,     2,     1,     2,     5,     6,    10,
    11,    14,    15,    16,    17,    23,    26,    27,    28,    29,
    30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
    40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
    50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
    60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
    70,    71,    72,    73,    74,    75,    76,    77,    78
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     5,     6,     9,    10,    13,    16,    19,    22,    25,
    26,    28,    30,    32,    34,    36,    39,    41,    44,    45,
    48,    51,    54,    57,    60,    63,    65,    68,    69,    72,
    75,    78,    81,    84,    87,    90,    93,    96,    99,   101,
   104,   105,   108,   111,   114,   117,   120,   123,   126,   129,
   131,   134,   135,   138,   141,   144,   147,   150,   153,   156,
   159,   162,   165,   167,   170,   171,   174,   177,   180,   183,
   186,   189,   192,   195,   200,   205,   207,   209,   211,   213,
   215,   217,   219,   223,   226,   227,   234,   237,   238,   244,
   246,   254,   259,   264,   268,   272,   276,   280,   284,   288,
   292,   296,   300,   304,   308,   312,   316,   320,   324,   328,
   331,   334,   337,   340,   344,   346,   348,   350,   355,   360,
   365,   372,   377,   382,   387,   392,   399,   404,   409,   416,
   421,   422,   423,   424
};

#endif

static const short yyrhs[] = {    85,
    86,    88,    54,     0,     0,    86,    87,     0,     0,    55,
    27,     0,    56,    27,     0,    58,    26,     0,    57,    26,
     0,    88,    89,     0,     0,    90,     0,    94,     0,    98,
     0,   102,     0,   106,     0,    91,    92,     0,    49,     0,
    92,    93,     0,     0,    59,    26,     0,    60,   112,     0,
    61,   112,     0,    62,   115,     0,    63,   110,     0,    95,
    96,     0,    50,     0,    96,    97,     0,     0,    59,    26,
     0,    60,   112,     0,    61,   112,     0,    64,   115,     0,
    65,   115,     0,    66,   115,     0,    67,   110,     0,    68,
   110,     0,    69,   110,     0,    99,   100,     0,    51,     0,
   100,   101,     0,     0,    59,    26,     0,    60,   114,     0,
    61,   114,     0,    64,   115,     0,    65,   115,     0,    67,
   110,     0,    68,   110,     0,   103,   104,     0,    52,     0,
   104,   105,     0,     0,    59,    26,     0,    60,   112,     0,
    61,   112,     0,    64,   115,     0,    65,   115,     0,    66,
   115,     0,    67,   110,     0,    68,   110,     0,    69,   110,
     0,   107,   108,     0,    53,     0,   108,   109,     0,     0,
    59,    26,     0,    70,   115,     0,    71,   115,     0,    72,
   115,     0,    73,   115,     0,    74,   115,     0,    75,   115,
     0,    76,   115,     0,    77,    79,   111,    80,     0,    78,
    79,   111,    80,     0,    29,     0,    31,     0,    30,     0,
    32,     0,    33,     0,    34,     0,    35,     0,   111,    81,
   114,     0,   111,   114,     0,     0,    82,   115,    81,   115,
   113,    83,     0,    81,   115,     0,     0,    82,   115,    81,
   115,    83,     0,   116,     0,   116,     3,   118,   116,     4,
   119,   116,     0,   116,     5,   120,   116,     0,   116,     6,
   121,   116,     0,   116,     7,   116,     0,   116,     8,   116,
     0,   116,     9,   116,     0,   116,    10,   116,     0,   116,
    11,   116,     0,   116,    12,   116,     0,   116,    13,   116,
     0,   116,    14,   116,     0,   116,    15,   116,     0,   116,
    16,   116,     0,   116,    17,   116,     0,   116,    18,   116,
     0,   116,    19,   116,     0,   116,    20,   116,     0,   116,
    21,   116,     0,   116,    22,   116,     0,    18,   116,     0,
    19,   116,     0,    24,   116,     0,    25,   116,     0,    82,
   116,    83,     0,    27,     0,    28,     0,   117,     0,    36,
    82,   116,    83,     0,    37,    82,   116,    83,     0,    38,
    82,   116,    83,     0,    39,    82,   116,    81,   116,    83,
     0,    40,    82,   116,    83,     0,    41,    82,   116,    83,
     0,    42,    82,   116,    83,     0,    43,    82,   116,    83,
     0,    44,    82,   116,    81,   116,    83,     0,    45,    82,
   116,    83,     0,    46,    82,   116,    83,     0,    47,    82,
   116,    81,   116,    83,     0,    48,    82,   116,    83,     0,
     0,     0,     0,     0
};

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   120,   125,   145,   146,   151,   156,   161,   167,   178,   179,
   184,   185,   186,   187,   188,   195,   217,   232,   233,   238,
   244,   251,   258,   263,   273,   295,   309,   310,   315,   321,
   328,   335,   340,   345,   350,   355,   360,   370,   392,   404,
   405,   410,   416,   422,   428,   433,   438,   443,   452,   474,
   491,   492,   497,   503,   510,   517,   522,   527,   532,   537,
   542,   552,   574,   595,   596,   601,   607,   612,   617,   622,
   627,   632,   637,   642,   657,   680,   685,   690,   695,   700,
   705,   710,   720,   730,   740,   751,   761,   766,   774,   786,
   796,   807,   817,   827,   833,   839,   845,   851,   857,   863,
   869,   875,   881,   887,   893,   899,   905,   911,   917,   923,
   928,   934,   940,   946,   951,   957,   963,   968,   974,   980,
   986,   992,   998,  1004,  1010,  1016,  1022,  1028,  1034,  1040,
  1049,  1058,  1067,  1078
};

static const char * const yytname[] = {   "$","error","$illegal.","'?'","':'",
"OR","AND","'|'","'^'","'&'","EQUALS","NEQUAL","'<'","'>'","LT_EQ","GT_EQ","LSHIFT",
"RSHIFT","'+'","'-'","'*'","'/'","'%'","UNARY","'!'","'~'","NAME","INTEGER",
"DOUBLE","LINEAR","SINUSOIDAL","COSINUSOIDAL","LOGARITHMIC","PARABOLIC","REVERSE_LOGARITHMIC",
"REVERSE_PARABOLIC","SIN","COS","TAN","POW","EXP","LOG","LOG10","SQRT","HYPOT",
"FLOOR","CEIL","FMOD","FABS","LINE","GRID","QUADGRID","BRICKGRID","TRIMESH",
"END","NODE_EQ","ELEMENT_EQ","CONSTRAINT_EQ","MATERIAL_EQ","TYPE_EQ","START_EQ",
"END_EQ","NUMBER_EQ","RULE_EQ","X_NUMBER_EQ","Y_NUMBER_EQ","Z_NUMBER_EQ","X_RULE_EQ",
"Y_RULE_EQ","Z_RULE_EQ","TOLIN_EQ","ANGSPC_EQ","ANGTOL_EQ","DMIN_EQ","KAPPA_EQ",
"MIN_EQ","MAX_EQ","BOUNDARY_EQ","HOLE_EQ","'['","']'","','","'('","')'","specification",
"initialize","start_parameter_list","start_parameter","generator_list","generator",
"line_generator","line_specifier","line_parameter_list","line_parameter","grid_generator",
"grid_specifier","grid_parameter_list","grid_parameter","quadgrid_generator",
"quadgrid_specifier","quadgrid_parameter_list","quadgrid_parameter","brickgrid_generator",
"brickgrid_specifier","brickgrid_parameter_list","brickgrid_parameter","trimesh_generator",
"trimesh_specifier","trimesh_parameter_list","trimesh_parameter","rule","pair_list",
"triple","opt_z_coordinate","pair","constant_expression","expression","function",
"if_action","else_action","or_action","and_action",""
};
#endif

static const short yyr1[] = {     0,
    84,    85,    86,    86,    87,    87,    87,    87,    88,    88,
    89,    89,    89,    89,    89,    90,    91,    92,    92,    93,
    93,    93,    93,    93,    94,    95,    96,    96,    97,    97,
    97,    97,    97,    97,    97,    97,    97,    98,    99,   100,
   100,   101,   101,   101,   101,   101,   101,   101,   102,   103,
   104,   104,   105,   105,   105,   105,   105,   105,   105,   105,
   105,   106,   107,   108,   108,   109,   109,   109,   109,   109,
   109,   109,   109,   109,   109,   110,   110,   110,   110,   110,
   110,   110,   111,   111,   111,   112,   113,   113,   114,   115,
   116,   116,   116,   116,   116,   116,   116,   116,   116,   116,
   116,   116,   116,   116,   116,   116,   116,   116,   116,   116,
   116,   116,   116,   116,   116,   116,   116,   117,   117,   117,
   117,   117,   117,   117,   117,   117,   117,   117,   117,   117,
   118,   119,   120,   121
};

static const short yyr2[] = {     0,
     4,     0,     2,     0,     2,     2,     2,     2,     2,     0,
     1,     1,     1,     1,     1,     2,     1,     2,     0,     2,
     2,     2,     2,     2,     2,     1,     2,     0,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     1,     2,
     0,     2,     2,     2,     2,     2,     2,     2,     2,     1,
     2,     0,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     1,     2,     0,     2,     2,     2,     2,     2,
     2,     2,     2,     4,     4,     1,     1,     1,     1,     1,
     1,     1,     3,     2,     0,     6,     2,     0,     5,     1,
     7,     4,     4,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     2,
     2,     2,     2,     3,     1,     1,     1,     4,     4,     4,
     6,     4,     4,     4,     4,     6,     4,     4,     6,     4,
     0,     0,     0,     0
};

static const short yydefact[] = {     2,
     4,    10,     0,     0,     0,     0,     3,     0,     5,     6,
     8,     7,    17,    26,    39,    50,    63,     1,     9,    11,
    19,    12,    28,    13,    41,    14,    52,    15,    65,    16,
    25,    38,    49,    62,     0,     0,     0,     0,     0,    18,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    27,
     0,     0,     0,     0,     0,     0,     0,    40,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    64,    20,
     0,    21,    22,     0,     0,     0,     0,   115,   116,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    23,    90,   117,    76,    78,    77,    79,
    80,    81,    82,    24,    29,    30,    31,    32,    33,    34,
    35,    36,    37,    42,     0,    43,    44,    45,    46,    47,
    48,    53,    54,    55,    56,    57,    58,    59,    60,    61,
    66,    67,    68,    69,    70,    71,    72,    73,    85,    85,
     0,   110,   111,   112,   113,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   131,
   133,   134,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   114,     0,     0,     0,    94,
    95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
   105,   106,   107,   108,   109,     0,    74,     0,    84,    75,
    88,   118,   119,   120,     0,   122,   123,   124,   125,     0,
   127,   128,     0,   130,     0,    92,    93,     0,    83,     0,
     0,     0,     0,     0,   132,    89,    87,    86,   121,   126,
   129,     0,    91,     0,     0,     0
};

static const short yydefgoto[] = {   264,
     1,     2,     7,     8,    19,    20,    21,    30,    40,    22,
    23,    31,    50,    24,    25,    32,    58,    26,    27,    33,
    68,    28,    29,    34,    79,   114,   190,    82,   251,   229,
   104,   105,   106,   207,   262,   208,   209
};

static const short yypact[] = {-32768,
-32768,    27,   -15,   -14,    12,    14,-32768,   525,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -54,
   616,    63,   627,    86,    33,   -21,   -21,   545,   154,-32768,
    54,   -21,   -21,   545,   545,   545,   154,   154,   154,-32768,
    76,    39,    39,   545,   545,   154,   154,-32768,   103,   -21,
   -21,   545,   545,   545,   154,   154,   154,-32768,   109,   545,
   545,   545,   545,   545,   545,   545,    67,    68,-32768,-32768,
   545,-32768,-32768,   545,   545,   545,   545,-32768,-32768,    84,
    87,    89,    90,    92,    93,    94,    95,    96,    98,    99,
   108,   122,   545,-32768,   596,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   545,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
    55,-32768,-32768,-32768,-32768,   545,   545,   545,   545,   545,
   545,   545,   545,   545,   545,   545,   545,   545,    36,-32768,
-32768,-32768,   545,   545,   545,   545,   545,   545,   545,   545,
   545,   545,   545,   545,   545,   545,   545,   545,   110,   -46,
    19,   545,    57,   202,   222,   499,   242,   262,   301,   321,
   519,   341,   361,   539,   400,-32768,   545,   545,   545,   476,
   639,   652,   181,   181,   681,   681,   681,   681,     3,     3,
   133,   133,-32768,-32768,-32768,   545,-32768,    39,-32768,-32768,
   145,-32768,-32768,-32768,   545,-32768,-32768,-32768,-32768,   545,
-32768,-32768,   545,-32768,   625,   280,   378,    66,-32768,   545,
   163,   420,   440,   460,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   545,   596,   167,   168,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,    85,   116,    83,-32768,   -49,
   -44,   -70,-32768,-32768,-32768,-32768,-32768
};


#define	YYLAST		703


static const short yytable[] = {   118,
   119,   120,   126,   127,    35,    36,    37,    38,    39,   128,
   129,     9,    10,   152,   153,   154,   155,   135,   136,   137,
   184,   185,   186,   187,   188,   142,   143,   144,   145,   146,
   147,   148,   169,   227,   228,   125,   151,    11,   170,    12,
   171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
   181,   182,   183,   184,   185,   186,   187,   188,    80,   170,
    81,   171,   172,   173,   174,   175,   176,   177,   178,   179,
   180,   181,   182,   183,   184,   185,   186,   187,   188,   115,
   189,     3,     4,     5,     6,   193,   194,   195,   196,   197,
   198,   199,   200,   201,   202,   203,   204,   205,   230,   228,
   125,   124,   210,   211,   212,   213,   214,   215,   216,   217,
   218,   219,   220,   221,   222,   223,   224,   225,   206,    83,
   125,    51,    52,    53,   116,   117,    54,    55,   132,    56,
    57,   121,   122,   123,   141,   192,   245,   246,   247,   232,
   130,   131,   133,   134,    69,   149,   150,   231,   256,   138,
   139,   140,   186,   187,   188,    70,    71,    72,    73,    74,
    75,    76,    77,    78,   252,   156,   265,   266,   157,   253,
   158,   159,   254,   160,   161,   162,   163,   164,   249,   165,
   166,   248,   107,   108,   109,   110,   111,   112,   113,   167,
   226,   263,   178,   179,   180,   181,   182,   183,   184,   185,
   186,   187,   188,   168,   170,   257,   171,   172,   173,   174,
   175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
   185,   186,   187,   188,   170,   250,   171,   172,   173,   174,
   175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
   185,   186,   187,   188,   170,   258,   171,   172,   173,   174,
   175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
   185,   186,   187,   188,   170,   191,   171,   172,   173,   174,
   175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
   185,   186,   187,   188,   233,   172,   173,   174,   175,   176,
   177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
   187,   188,     0,   170,   234,   171,   172,   173,   174,   175,
   176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
   186,   187,   188,   170,   236,   171,   172,   173,   174,   175,
   176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
   186,   187,   188,   170,   237,   171,   172,   173,   174,   175,
   176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
   186,   187,   188,   170,     0,   171,   172,   173,   174,   175,
   176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
   186,   187,   188,   238,   173,   174,   175,   176,   177,   178,
   179,   180,   181,   182,   183,   184,   185,   186,   187,   188,
     0,     0,   170,   239,   171,   172,   173,   174,   175,   176,
   177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
   187,   188,   170,   241,   171,   172,   173,   174,   175,   176,
   177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
   187,   188,   170,   242,   171,   172,   173,   174,   175,   176,
   177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
   187,   188,   170,     0,   171,   172,   173,   174,   175,   176,
   177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
   187,   188,   244,   174,   175,   176,   177,   178,   179,   180,
   181,   182,   183,   184,   185,   186,   187,   188,     0,     0,
     0,   170,   259,   171,   172,   173,   174,   175,   176,   177,
   178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
   188,   170,   260,   171,   172,   173,   174,   175,   176,   177,
   178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
   188,   170,   261,   171,   172,   173,   174,   175,   176,   177,
   178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
   188,     0,    84,    85,     0,     0,     0,     0,    86,    87,
     0,    88,    89,    13,    14,    15,    16,    17,    18,   235,
    90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
   100,   101,   102,     0,     0,     0,     0,     0,   170,   240,
   171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
   181,   182,   183,   184,   185,   186,   187,   188,     0,   243,
     0,     0,     0,     0,     0,     0,   103,   170,   255,   171,
   172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
   182,   183,   184,   185,   186,   187,   188,   175,   176,   177,
   178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
   188,   176,   177,   178,   179,   180,   181,   182,   183,   184,
   185,   186,   187,   188,    41,    42,    43,     0,     0,    44,
    45,    46,    47,    48,    49,    59,    60,    61,     0,     0,
    62,    63,    64,    65,    66,    67,   182,   183,   184,   185,
   186,   187,   188
};

static const short yycheck[] = {    44,
    45,    46,    52,    53,    59,    60,    61,    62,    63,    54,
    55,    27,    27,    84,    85,    86,    87,    62,    63,    64,
    18,    19,    20,    21,    22,    70,    71,    72,    73,    74,
    75,    76,   103,    80,    81,    82,    81,    26,     3,    26,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    26,     3,
    82,     5,     6,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    26,
   125,    55,    56,    57,    58,   156,   157,   158,   159,   160,
   161,   162,   163,   164,   165,   166,   167,   168,    80,    81,
    82,    26,   173,   174,   175,   176,   177,   178,   179,   180,
   181,   182,   183,   184,   185,   186,   187,   188,    83,    37,
    82,    59,    60,    61,    42,    43,    64,    65,    26,    67,
    68,    47,    48,    49,    26,    81,   207,   208,   209,    83,
    56,    57,    60,    61,    59,    79,    79,   192,    83,    65,
    66,    67,    20,    21,    22,    70,    71,    72,    73,    74,
    75,    76,    77,    78,   235,    82,     0,     0,    82,   240,
    82,    82,   243,    82,    82,    82,    82,    82,   228,    82,
    82,   226,    29,    30,    31,    32,    33,    34,    35,    82,
    81,   262,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,    82,     3,   250,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,     3,    81,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,     3,    83,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,     3,   150,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,    83,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    -1,     3,    83,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,     3,    83,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,     3,    83,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,     3,    -1,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,    83,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    -1,    -1,     3,    83,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,     3,    83,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,     3,    83,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,     3,    -1,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    83,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    -1,    -1,
    -1,     3,    83,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,     3,    83,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,     3,    83,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    -1,    18,    19,    -1,    -1,    -1,    -1,    24,    25,
    -1,    27,    28,    49,    50,    51,    52,    53,    54,    81,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    -1,    -1,    -1,    -1,    -1,     3,    81,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    -1,    81,
    -1,    -1,    -1,    -1,    -1,    -1,    82,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,    59,    60,    61,    -1,    -1,    64,
    65,    66,    67,    68,    69,    59,    60,    61,    -1,    -1,
    64,    65,    66,    67,    68,    69,    16,    17,    18,    19,
    20,    21,    22
};
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
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
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

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
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
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 169 "/usr/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
#ifdef YYLSP_NEEDED
		 &yyls1, size * sizeof (*yylsp),
#endif
		 &yystacksize);

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 2:
#line 126 "parser.y"
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
	    ;
    break;}
case 5:
#line 152 "parser.y"
{
		generator.start_node = yyvsp[0].i;
	    ;
    break;}
case 6:
#line 157 "parser.y"
{
		generator.start_element = yyvsp[0].i;
	    ;
    break;}
case 7:
#line 162 "parser.y"
{
		Deallocate (generator.material);
		generator.material = yyvsp[0].s;
	    ;
    break;}
case 8:
#line 168 "parser.y"
{
		Deallocate (generator.constraint);
		generator.constraint = yyvsp[0].s;
	    ;
    break;}
case 16:
#line 196 "parser.y"
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
	    ;
    break;}
case 17:
#line 218 "parser.y"
{
		last_z = 0;

		line.xs = line.ys = line.zs = 0;
		line.xe = line.ye = line.ze = 0;
		line.rule = LinearRule;
		line.number = 1;

		line.definition = last_line_definition;
	    ;
    break;}
case 20:
#line 239 "parser.y"
{
		line.definition = defnlookup (yyvsp[0].s);
		Deallocate (yyvsp[0].s);
	    ;
    break;}
case 21:
#line 245 "parser.y"
{
		line.xs = x;
		line.ys = y;
		line.zs = z;
	    ;
    break;}
case 22:
#line 252 "parser.y"
{
		line.xe = x;
		line.ye = y;
		line.ze = z;
	    ;
    break;}
case 23:
#line 259 "parser.y"
{
		line.number = yyvsp[0].d;
	    ;
    break;}
case 24:
#line 264 "parser.y"
{
		line.rule = yyvsp[0].i;
	    ;
    break;}
case 25:
#line 274 "parser.y"
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
	    ;
    break;}
case 26:
#line 296 "parser.y"
{
		last_z = 0;

		grid.xs = grid.ys = grid.zs = 0;
		grid.xe = grid.ye = grid.ze = 0;
		grid.xnumber = grid.ynumber = grid.znumber = 1;
		grid.xrule = grid.yrule = grid.zrule = LinearRule;
		grid.definition = last_grid_definition;
	    ;
    break;}
case 29:
#line 316 "parser.y"
{
		grid.definition = defnlookup (yyvsp[0].s);
		Deallocate (yyvsp[0].s);
	    ;
    break;}
case 30:
#line 322 "parser.y"
{
		grid.xs = x;
		grid.ys = y;
		grid.zs = z;
	    ;
    break;}
case 31:
#line 329 "parser.y"
{
		grid.xe = x;
		grid.ye = y;
		grid.ze = z;
	    ;
    break;}
case 32:
#line 336 "parser.y"
{
		grid.xnumber = yyvsp[0].d;
	    ;
    break;}
case 33:
#line 341 "parser.y"
{
		grid.ynumber = yyvsp[0].d;
	    ;
    break;}
case 34:
#line 346 "parser.y"
{
		grid.znumber = yyvsp[0].d;
	    ;
    break;}
case 35:
#line 351 "parser.y"
{
		grid.xrule = yyvsp[0].i;
	    ;
    break;}
case 36:
#line 356 "parser.y"
{
		grid.yrule = yyvsp[0].i;
	    ;
    break;}
case 37:
#line 361 "parser.y"
{
		grid.zrule = yyvsp[0].i;
	    ;
    break;}
case 38:
#line 371 "parser.y"
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
	    ;
    break;}
case 39:
#line 393 "parser.y"
{
		quadgrid.xs = quadgrid.ys = 0;
		quadgrid.xe = quadgrid.ye = 0;
		quadgrid.xnumber = quadgrid.ynumber = 1;
		quadgrid.xrule = quadgrid.yrule = LinearRule;
		quadgrid.definition = last_quadgrid_definition;
	    ;
    break;}
case 42:
#line 411 "parser.y"
{
		quadgrid.definition = defnlookup (yyvsp[0].s);
		Deallocate (yyvsp[0].s);
	    ;
    break;}
case 43:
#line 417 "parser.y"
{
		quadgrid.xs = x;
		quadgrid.ys = y;
	    ;
    break;}
case 44:
#line 423 "parser.y"
{
		quadgrid.xe = x;
		quadgrid.ye = y;
	    ;
    break;}
case 45:
#line 429 "parser.y"
{
		quadgrid.xnumber = yyvsp[0].d;
	    ;
    break;}
case 46:
#line 434 "parser.y"
{
		quadgrid.ynumber = yyvsp[0].d;
	    ;
    break;}
case 47:
#line 439 "parser.y"
{
		quadgrid.xrule = yyvsp[0].i;
	    ;
    break;}
case 48:
#line 444 "parser.y"
{
		quadgrid.yrule = yyvsp[0].i;
	    ;
    break;}
case 49:
#line 453 "parser.y"
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
	    ;
    break;}
case 50:
#line 475 "parser.y"
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
	    ;
    break;}
case 53:
#line 498 "parser.y"
{
		brickgrid.definition = defnlookup (yyvsp[0].s);
		Deallocate (yyvsp[0].s);
	    ;
    break;}
case 54:
#line 504 "parser.y"
{
		brickgrid.xs = x;
		brickgrid.ys = y;
		brickgrid.zs = z;
	    ;
    break;}
case 55:
#line 511 "parser.y"
{
		brickgrid.xe = x;
		brickgrid.ye = y;
		brickgrid.ze = z;
	    ;
    break;}
case 56:
#line 518 "parser.y"
{
		brickgrid.xnumber = yyvsp[0].d;
	    ;
    break;}
case 57:
#line 523 "parser.y"
{
		brickgrid.ynumber = yyvsp[0].d;
	    ;
    break;}
case 58:
#line 528 "parser.y"
{
		brickgrid.znumber = yyvsp[0].d;
	    ;
    break;}
case 59:
#line 533 "parser.y"
{
		brickgrid.xrule = yyvsp[0].i;
	    ;
    break;}
case 60:
#line 538 "parser.y"
{
		brickgrid.yrule = yyvsp[0].i;
	    ;
    break;}
case 61:
#line 543 "parser.y"
{
		brickgrid.zrule = yyvsp[0].i;
	    ;
    break;}
case 62:
#line 553 "parser.y"
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
	    ;
    break;}
case 63:
#line 575 "parser.y"
{
		trimesh.numcurves  = 0;
		trimesh.tolin	   = 0;
		trimesh.angspc	   = 30;
		trimesh.angtol	   = 20;
		trimesh.dmin	   = .5;
		trimesh.kappa	   = .25;
		trimesh.min	   = 50;
		trimesh.max	   = 100;

		trimesh.definition = last_trimesh_definition;

		curve_size = 2;
		if (!(trimesh.curves = Allocate (Curve, curve_size)))
		    Fatal ("unable to allocate curve array");
	    ;
    break;}
case 66:
#line 602 "parser.y"
{
		trimesh.definition = defnlookup (yyvsp[0].s);
		Deallocate (yyvsp[0].s);
	    ;
    break;}
case 67:
#line 608 "parser.y"
{
		trimesh.tolin = yyvsp[0].d;
	    ;
    break;}
case 68:
#line 613 "parser.y"
{
		trimesh.angspc = yyvsp[0].d;
	    ;
    break;}
case 69:
#line 618 "parser.y"
{
		trimesh.angtol = yyvsp[0].d;
	    ;
    break;}
case 70:
#line 623 "parser.y"
{
		trimesh.dmin = yyvsp[0].d;
	    ;
    break;}
case 71:
#line 628 "parser.y"
{
		trimesh.kappa = yyvsp[0].d;
	    ;
    break;}
case 72:
#line 633 "parser.y"
{
		trimesh.min = yyvsp[0].d;
	    ;
    break;}
case 73:
#line 638 "parser.y"
{
		trimesh.max = yyvsp[0].d;
	    ;
    break;}
case 74:
#line 643 "parser.y"
{
		Curve new_curve;


		if (!(new_curve = AllocNew (struct _curve)))
		    Fatal ("unable to allocate new curve");

		*new_curve = curve;
		trimesh.curves [0] = new_curve;

		if (trimesh.numcurves == 0)
		    trimesh.numcurves = 1;
	    ;
    break;}
case 75:
#line 658 "parser.y"
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
	    ;
    break;}
case 76:
#line 681 "parser.y"
{
		yyval.i = LinearRule;
	    ;
    break;}
case 77:
#line 686 "parser.y"
{
		yyval.i = CosRule;
	    ;
    break;}
case 78:
#line 691 "parser.y"
{
		yyval.i = SinRule;
	    ;
    break;}
case 79:
#line 696 "parser.y"
{
		yyval.i = LogRule;
	    ;
    break;}
case 80:
#line 701 "parser.y"
{
		yyval.i = ParabolicRule;
	    ;
    break;}
case 81:
#line 706 "parser.y"
{
		yyval.i = RevLogRule;
	    ;
    break;}
case 82:
#line 711 "parser.y"
{
		yyval.i = RevParabolicRule;
	    ;
    break;}
case 83:
#line 721 "parser.y"
{
		if (curve.numvc == vcl_size)
		    if (!Reallocate (curve.vcl, xy_pair, vcl_size <<= 1))
			Fatal ("unable to allocate pair array");

		curve.vcl [curve.numvc] [0] = x;
		curve.vcl [curve.numvc ++] [1] = y;
	    ;
    break;}
case 84:
#line 731 "parser.y"
{
		if (curve.numvc == vcl_size)
		    if (!Reallocate (curve.vcl, xy_pair, vcl_size <<= 1))
			Fatal ("unable to allocate pair array");

		curve.vcl [curve.numvc] [0] = x;
		curve.vcl [curve.numvc ++] [1] = y;
	    ;
    break;}
case 85:
#line 741 "parser.y"
{
		vcl_size = 8;
		curve.numvc = 0;
		if (!(curve.vcl = Allocate (xy_pair, vcl_size)))
		    Fatal ("unable to allocate pair array");
	    ;
    break;}
case 86:
#line 752 "parser.y"
{
		x = yyvsp[-4].d;
		y = yyvsp[-2].d;
		z = yyvsp[-1].d;
	    ;
    break;}
case 87:
#line 762 "parser.y"
{
		yyval.d = yyvsp[0].d;
	    ;
    break;}
case 88:
#line 767 "parser.y"
{
		yyval.d = last_z;
	    ;
    break;}
case 89:
#line 775 "parser.y"
{
		x = yyvsp[-3].d;
		y = yyvsp[-1].d;
	    ;
    break;}
case 90:
#line 787 "parser.y"
{
		EmitCode (HaltOp);
		SetIP (0);
		yyval.d = EvalCode (InCore, 0.0);
	    ;
    break;}
case 91:
#line 797 "parser.y"
{
		int ip = GetIP ( );
		SetIP (ip - yyvsp[0].i - 2);
		EmitCode (JmpOp, yyvsp[0].i);
		SetIP (GetIP ( ) - yyvsp[-3].i - 4);
		EmitCode (JzOp, yyvsp[-3].i + 2);
		SetIP (ip);
		yyval.i = yyvsp[-6].i + yyvsp[-4].i + yyvsp[-3].i + yyvsp[-1].i + yyvsp[0].i;
	    ;
    break;}
case 92:
#line 808 "parser.y"
{
		int ip = GetIP ( );
		SetIP (ip - yyvsp[0].i - 3);
		EmitCode (JnzOp, yyvsp[0].i + 1);
		SetIP (ip);
		EmitCode (TestOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + yyvsp[0].i + 1;
	    ;
    break;}
case 93:
#line 818 "parser.y"
{
		int ip = GetIP ( );
		SetIP (ip - yyvsp[0].i - 3);
		EmitCode (JzOp, yyvsp[0].i + 1);
		SetIP (ip);
		EmitCode (TestOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + yyvsp[0].i + 1;
	    ;
    break;}
case 94:
#line 828 "parser.y"
{
		EmitCode (OrOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 95:
#line 834 "parser.y"
{
		EmitCode (XorOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 96:
#line 840 "parser.y"
{
		EmitCode (AndOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 97:
#line 846 "parser.y"
{
		EmitCode (EqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 98:
#line 852 "parser.y"
{
		EmitCode (NeqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 99:
#line 858 "parser.y"
{
		EmitCode (LtOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 100:
#line 864 "parser.y"
{
		EmitCode (GtOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 101:
#line 870 "parser.y"
{
		EmitCode (LteqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 102:
#line 876 "parser.y"
{
		EmitCode (GteqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 103:
#line 882 "parser.y"
{
		EmitCode (LsftOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 104:
#line 888 "parser.y"
{
		EmitCode (RsftOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 105:
#line 894 "parser.y"
{
		EmitCode (AddOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 106:
#line 900 "parser.y"
{
		EmitCode (SubOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 107:
#line 906 "parser.y"
{
		EmitCode (MulOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 108:
#line 912 "parser.y"
{
		EmitCode (DivOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 109:
#line 918 "parser.y"
{
		EmitCode (ModOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 110:
#line 924 "parser.y"
{
		yyval.i = yyvsp[0].i;
	    ;
    break;}
case 111:
#line 929 "parser.y"
{
		EmitCode (NegOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 112:
#line 935 "parser.y"
{
		EmitCode (NotOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 113:
#line 941 "parser.y"
{
		EmitCode (InvOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 114:
#line 947 "parser.y"
{
		yyval.i = yyvsp[-1].i;
	    ;
    break;}
case 115:
#line 952 "parser.y"
{
		EmitCode (PushOp, (double) yyvsp[0].i);
		yyval.i = 2;
	    ;
    break;}
case 116:
#line 958 "parser.y"
{
		EmitCode (PushOp, yyvsp[0].d);
		yyval.i = 2;
	    ;
    break;}
case 118:
#line 969 "parser.y"
{
		EmitCode (SinOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 119:
#line 975 "parser.y"
{
		EmitCode (CosOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 120:
#line 981 "parser.y"
{
		EmitCode (TanOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 121:
#line 987 "parser.y"
{
		EmitCode (PowOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 122:
#line 993 "parser.y"
{
		EmitCode (ExpOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 123:
#line 999 "parser.y"
{
		EmitCode (LnOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 124:
#line 1005 "parser.y"
{
		EmitCode (LogOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 125:
#line 1011 "parser.y"
{
		EmitCode (SqrtOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 126:
#line 1017 "parser.y"
{
		EmitCode (HypotOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 127:
#line 1023 "parser.y"
{
		EmitCode (FloorOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 128:
#line 1029 "parser.y"
{
		EmitCode (CeilOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 129:
#line 1035 "parser.y"
{
		EmitCode (FmodOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 130:
#line 1041 "parser.y"
{
		EmitCode (FabsOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 131:
#line 1050 "parser.y"
{
		EmitCode (JzOp, 0);
		yyval.i = 2;
	    ;
    break;}
case 132:
#line 1059 "parser.y"
{
		EmitCode (JmpOp, 0);
		yyval.i = 2;
	    ;
    break;}
case 133:
#line 1068 "parser.y"
{
		EmitCode (CopyOp);
		EmitCode (JnzOp, 0);
		EmitCode (PopOp);
		yyval.i = 4;
	    ;
    break;}
case 134:
#line 1079 "parser.y"
{
		EmitCode (CopyOp);
		EmitCode (JzOp, 0);
		EmitCode (PopOp);
		yyval.i = 4;
	    ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 442 "/usr/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 1086 "parser.y"
