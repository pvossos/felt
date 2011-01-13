
/*  A Bison parser, made from parser.y
 by  GNU Bison version 1.25
  */

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
#define	TARGET_EQ	311
#define	ALPHA_EQ	312
#define	BOUNDARY_EQ	313
#define	HOLE_EQ	314

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
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		256
#define	YYFLAG		-32768
#define	YYNTBASE	79

#define YYTRANSLATE(x) ((unsigned)(x) <= 314 ? yytranslate[x] : 117)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    24,     2,     2,     2,    22,     9,     2,    77,
    78,    20,    18,    76,    19,     2,    21,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     4,     2,    12,
     2,    13,     3,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    74,     2,    75,     8,     2,     2,     2,     2,     2,     2,
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
    70,    71,    72,    73
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     5,     6,     9,    10,    13,    16,    19,    22,    25,
    26,    28,    30,    32,    34,    36,    39,    41,    44,    45,
    48,    51,    54,    57,    60,    63,    65,    68,    69,    72,
    75,    78,    81,    84,    87,    90,    93,    96,    99,   101,
   104,   105,   108,   111,   114,   117,   120,   123,   126,   129,
   131,   134,   135,   138,   141,   144,   147,   150,   153,   156,
   159,   162,   165,   167,   170,   171,   174,   177,   180,   185,
   190,   192,   194,   196,   198,   200,   202,   204,   208,   211,
   212,   219,   222,   223,   229,   231,   239,   244,   249,   253,
   257,   261,   265,   269,   273,   277,   281,   285,   289,   293,
   297,   301,   305,   309,   313,   316,   319,   322,   325,   329,
   331,   333,   335,   340,   345,   350,   357,   362,   367,   372,
   377,   384,   389,   394,   401,   406,   407,   408,   409
};

static const short yyrhs[] = {    80,
    81,    83,    54,     0,     0,    81,    82,     0,     0,    55,
    27,     0,    56,    27,     0,    58,    26,     0,    57,    26,
     0,    83,    84,     0,     0,    85,     0,    89,     0,    93,
     0,    97,     0,   101,     0,    86,    87,     0,    49,     0,
    87,    88,     0,     0,    59,    26,     0,    60,   107,     0,
    61,   107,     0,    62,   110,     0,    63,   105,     0,    90,
    91,     0,    50,     0,    91,    92,     0,     0,    59,    26,
     0,    60,   107,     0,    61,   107,     0,    64,   110,     0,
    65,   110,     0,    66,   110,     0,    67,   105,     0,    68,
   105,     0,    69,   105,     0,    94,    95,     0,    51,     0,
    95,    96,     0,     0,    59,    26,     0,    60,   109,     0,
    61,   109,     0,    64,   110,     0,    65,   110,     0,    67,
   105,     0,    68,   105,     0,    98,    99,     0,    52,     0,
    99,   100,     0,     0,    59,    26,     0,    60,   107,     0,
    61,   107,     0,    64,   110,     0,    65,   110,     0,    66,
   110,     0,    67,   105,     0,    68,   105,     0,    69,   105,
     0,   102,   103,     0,    53,     0,   103,   104,     0,     0,
    59,    26,     0,    71,   110,     0,    70,    27,     0,    72,
    74,   106,    75,     0,    73,    74,   106,    75,     0,    29,
     0,    31,     0,    30,     0,    32,     0,    33,     0,    34,
     0,    35,     0,   106,    76,   109,     0,   106,   109,     0,
     0,    77,   110,    76,   110,   108,    78,     0,    76,   110,
     0,     0,    77,   110,    76,   110,    78,     0,   111,     0,
   111,     3,   113,   111,     4,   114,   111,     0,   111,     5,
   115,   111,     0,   111,     6,   116,   111,     0,   111,     7,
   111,     0,   111,     8,   111,     0,   111,     9,   111,     0,
   111,    10,   111,     0,   111,    11,   111,     0,   111,    12,
   111,     0,   111,    13,   111,     0,   111,    14,   111,     0,
   111,    15,   111,     0,   111,    16,   111,     0,   111,    17,
   111,     0,   111,    18,   111,     0,   111,    19,   111,     0,
   111,    20,   111,     0,   111,    21,   111,     0,   111,    22,
   111,     0,    18,   111,     0,    19,   111,     0,    24,   111,
     0,    25,   111,     0,    77,   111,    78,     0,    27,     0,
    28,     0,   112,     0,    36,    77,   111,    78,     0,    37,
    77,   111,    78,     0,    38,    77,   111,    78,     0,    39,
    77,   111,    76,   111,    78,     0,    40,    77,   111,    78,
     0,    41,    77,   111,    78,     0,    42,    77,   111,    78,
     0,    43,    77,   111,    78,     0,    44,    77,   111,    76,
   111,    78,     0,    45,    77,   111,    78,     0,    46,    77,
   111,    78,     0,    47,    77,   111,    76,   111,    78,     0,
    48,    77,   111,    78,     0,     0,     0,     0,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   120,   125,   145,   146,   151,   156,   161,   167,   178,   179,
   184,   185,   186,   187,   188,   195,   217,   232,   233,   238,
   244,   251,   258,   263,   273,   295,   309,   310,   315,   321,
   328,   335,   340,   345,   350,   355,   360,   370,   392,   404,
   405,   410,   416,   422,   428,   433,   438,   443,   452,   474,
   491,   492,   497,   503,   510,   517,   522,   527,   532,   537,
   542,   552,   574,   590,   591,   596,   602,   607,   612,   627,
   650,   655,   660,   665,   670,   675,   680,   690,   700,   710,
   721,   731,   736,   744,   756,   766,   777,   787,   797,   803,
   809,   815,   821,   827,   833,   839,   845,   851,   857,   863,
   869,   875,   881,   887,   893,   898,   904,   910,   916,   921,
   927,   933,   938,   944,   950,   956,   962,   968,   974,   980,
   986,   992,   998,  1004,  1010,  1019,  1028,  1037,  1048
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","'?'","':'",
"OR","AND","'|'","'^'","'&'","EQUALS","NEQUAL","'<'","'>'","LT_EQ","GT_EQ","LSHIFT",
"RSHIFT","'+'","'-'","'*'","'/'","'%'","UNARY","'!'","'~'","NAME","INTEGER",
"DOUBLE","LINEAR","SINUSOIDAL","COSINUSOIDAL","LOGARITHMIC","PARABOLIC","REVERSE_LOGARITHMIC",
"REVERSE_PARABOLIC","SIN","COS","TAN","POW","EXP","LOG","LOG10","SQRT","HYPOT",
"FLOOR","CEIL","FMOD","FABS","LINE","GRID","QUADGRID","BRICKGRID","TRIMESH",
"END","NODE_EQ","ELEMENT_EQ","CONSTRAINT_EQ","MATERIAL_EQ","TYPE_EQ","START_EQ",
"END_EQ","NUMBER_EQ","RULE_EQ","X_NUMBER_EQ","Y_NUMBER_EQ","Z_NUMBER_EQ","X_RULE_EQ",
"Y_RULE_EQ","Z_RULE_EQ","TARGET_EQ","ALPHA_EQ","BOUNDARY_EQ","HOLE_EQ","'['",
"']'","','","'('","')'","specification","initialize","start_parameter_list",
"start_parameter","generator_list","generator","line_generator","line_specifier",
"line_parameter_list","line_parameter","grid_generator","grid_specifier","grid_parameter_list",
"grid_parameter","quadgrid_generator","quadgrid_specifier","quadgrid_parameter_list",
"quadgrid_parameter","brickgrid_generator","brickgrid_specifier","brickgrid_parameter_list",
"brickgrid_parameter","trimesh_generator","trimesh_specifier","trimesh_parameter_list",
"trimesh_parameter","rule","pair_list","triple","opt_z_coordinate","pair","constant_expression",
"expression","function","if_action","else_action","or_action","and_action", NULL
};
#endif

static const short yyr1[] = {     0,
    79,    80,    81,    81,    82,    82,    82,    82,    83,    83,
    84,    84,    84,    84,    84,    85,    86,    87,    87,    88,
    88,    88,    88,    88,    89,    90,    91,    91,    92,    92,
    92,    92,    92,    92,    92,    92,    92,    93,    94,    95,
    95,    96,    96,    96,    96,    96,    96,    96,    97,    98,
    99,    99,   100,   100,   100,   100,   100,   100,   100,   100,
   100,   101,   102,   103,   103,   104,   104,   104,   104,   104,
   105,   105,   105,   105,   105,   105,   105,   106,   106,   106,
   107,   108,   108,   109,   110,   111,   111,   111,   111,   111,
   111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
   111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
   111,   111,   112,   112,   112,   112,   112,   112,   112,   112,
   112,   112,   112,   112,   112,   113,   114,   115,   116
};

static const short yyr2[] = {     0,
     4,     0,     2,     0,     2,     2,     2,     2,     2,     0,
     1,     1,     1,     1,     1,     2,     1,     2,     0,     2,
     2,     2,     2,     2,     2,     1,     2,     0,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     1,     2,
     0,     2,     2,     2,     2,     2,     2,     2,     2,     1,
     2,     0,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     1,     2,     0,     2,     2,     2,     4,     4,
     1,     1,     1,     1,     1,     1,     1,     3,     2,     0,
     6,     2,     0,     5,     1,     7,     4,     4,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     2,     2,     2,     2,     3,     1,
     1,     1,     4,     4,     4,     6,     4,     4,     4,     4,
     6,     4,     4,     6,     4,     0,     0,     0,     0
};

static const short yydefact[] = {     2,
     4,    10,     0,     0,     0,     0,     3,     0,     5,     6,
     8,     7,    17,    26,    39,    50,    63,     1,     9,    11,
    19,    12,    28,    13,    41,    14,    52,    15,    65,    16,
    25,    38,    49,    62,     0,     0,     0,     0,     0,    18,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    27,
     0,     0,     0,     0,     0,     0,     0,    40,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
     0,     0,     0,    64,    20,     0,    21,    22,     0,     0,
     0,     0,   110,   111,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    23,    85,
   112,    71,    73,    72,    74,    75,    76,    77,    24,    29,
    30,    31,    32,    33,    34,    35,    36,    37,    42,     0,
    43,    44,    45,    46,    47,    48,    53,    54,    55,    56,
    57,    58,    59,    60,    61,    66,    68,    67,    80,    80,
     0,   105,   106,   107,   108,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   126,
   128,   129,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   109,     0,     0,     0,    89,
    90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
   100,   101,   102,   103,   104,     0,    69,     0,    79,    70,
    83,   113,   114,   115,     0,   117,   118,   119,   120,     0,
   122,   123,     0,   125,     0,    87,    88,     0,    78,     0,
     0,     0,     0,     0,   127,    84,    82,    81,   116,   121,
   124,     0,    86,     0,     0,     0
};

static const short yydefgoto[] = {   254,
     1,     2,     7,     8,    19,    20,    21,    30,    40,    22,
    23,    31,    50,    24,    25,    32,    58,    26,    27,    33,
    68,    28,    29,    34,    74,   109,   180,    77,   241,   219,
    99,   100,   101,   197,   252,   198,   199
};

static const short yypact[] = {-32768,
-32768,   -32,   -11,   -10,    12,    42,-32768,    69,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -54,
   114,   100,   286,   -37,    47,   -56,   -56,   549,   157,-32768,
    51,   -56,   -56,   549,   549,   549,   157,   157,   157,-32768,
    52,    17,    17,   549,   549,   157,   157,-32768,    87,   -56,
   -56,   549,   549,   549,   157,   157,   157,-32768,    88,   102,
   549,    21,    62,-32768,-32768,   549,-32768,-32768,   549,   549,
   549,   549,-32768,-32768,    80,    85,    93,    94,    99,   107,
   116,   117,   118,   155,   175,   207,   271,   549,-32768,   596,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   549,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   101,-32768,-32768,-32768,-32768,   549,   549,   549,   549,   549,
   549,   549,   549,   549,   549,   549,   549,   549,    34,-32768,
-32768,-32768,   549,   549,   549,   549,   549,   549,   549,   549,
   549,   549,   549,   549,   549,   549,   549,   549,   273,   -47,
    -5,   549,   134,   208,   228,   504,   248,   282,   302,   322,
   524,   356,   376,   544,   396,-32768,   549,   549,   549,   672,
   188,   261,    45,    45,   403,   403,   403,   403,   408,   408,
   106,   106,-32768,-32768,-32768,   549,-32768,    17,-32768,-32768,
   280,-32768,-32768,-32768,   549,-32768,-32768,-32768,-32768,   549,
-32768,-32768,   549,-32768,   624,   641,   657,   279,-32768,   549,
   353,   430,   450,   470,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   549,   596,   358,   432,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,    68,   354,    32,-32768,   -49,
   -44,   -67,-32768,-32768,-32768,-32768,-32768
};


#define	YYLAST		694


static const short yytable[] = {   113,
   114,   115,   121,   122,    35,    36,    37,    38,    39,   123,
   124,   142,   143,   144,   145,     9,    10,   130,   131,   132,
    76,    69,     3,     4,     5,     6,   138,   217,   218,   120,
   159,   141,    70,    71,    72,    73,   160,    11,   161,   162,
   163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
   173,   174,   175,   176,   177,   178,   168,   169,   170,   171,
   172,   173,   174,   175,   176,   177,   178,    12,    78,   220,
   218,   120,    75,   111,   112,   179,   110,   119,   183,   184,
   185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
   195,   128,   129,   120,   139,   200,   201,   202,   203,   204,
   205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
   215,   196,   127,   136,   116,   117,   118,    13,    14,    15,
    16,    17,    18,   125,   126,   176,   177,   178,   137,   235,
   236,   237,   133,   134,   135,   140,   160,   221,   161,   162,
   163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
   173,   174,   175,   176,   177,   178,   146,   242,    51,    52,
    53,   147,   243,    54,    55,   244,    56,    57,   239,   148,
   149,   238,    41,    42,    43,   150,   182,    44,    45,    46,
    47,    48,    49,   151,   253,   102,   103,   104,   105,   106,
   107,   108,   152,   153,   154,   247,   165,   166,   167,   168,
   169,   170,   171,   172,   173,   174,   175,   176,   177,   178,
   160,   222,   161,   162,   163,   164,   165,   166,   167,   168,
   169,   170,   171,   172,   173,   174,   175,   176,   177,   178,
   160,   155,   161,   162,   163,   164,   165,   166,   167,   168,
   169,   170,   171,   172,   173,   174,   175,   176,   177,   178,
   160,   156,   161,   162,   163,   164,   165,   166,   167,   168,
   169,   170,   171,   172,   173,   174,   175,   176,   177,   178,
   166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
   176,   177,   178,   157,   160,   223,   161,   162,   163,   164,
   165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
   175,   176,   177,   178,   160,   224,   161,   162,   163,   164,
   165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
   175,   176,   177,   178,   160,   226,   161,   162,   163,   164,
   165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
   175,   176,   177,   178,    59,    60,    61,   158,   216,    62,
    63,    64,    65,    66,    67,   240,   246,   255,   160,   227,
   161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
   171,   172,   173,   174,   175,   176,   177,   178,   160,   228,
   161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
   171,   172,   173,   174,   175,   176,   177,   178,   160,   229,
   161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
   171,   172,   173,   174,   175,   176,   177,   178,   172,   173,
   174,   175,   176,   177,   178,   174,   175,   176,   177,   178,
   248,   256,   160,   231,   161,   162,   163,   164,   165,   166,
   167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
   177,   178,   160,   232,   161,   162,   163,   164,   165,   166,
   167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
   177,   178,   160,   234,   161,   162,   163,   164,   165,   166,
   167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
   177,   178,     0,   181,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   160,   249,   161,   162,
   163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
   173,   174,   175,   176,   177,   178,   160,   250,   161,   162,
   163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
   173,   174,   175,   176,   177,   178,   160,   251,   161,   162,
   163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
   173,   174,   175,   176,   177,   178,    79,    80,     0,     0,
     0,     0,    81,    82,     0,    83,    84,     0,     0,   225,
     0,     0,     0,     0,    85,    86,    87,    88,    89,    90,
    91,    92,    93,    94,    95,    96,    97,     0,   160,   230,
   161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
   171,   172,   173,   174,   175,   176,   177,   178,     0,   233,
     0,     0,     0,     0,     0,    98,   160,   245,   161,   162,
   163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
   173,   174,   175,   176,   177,   178,   162,   163,   164,   165,
   166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
   176,   177,   178,   163,   164,   165,   166,   167,   168,   169,
   170,   171,   172,   173,   174,   175,   176,   177,   178,   164,
   165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
   175,   176,   177,   178
};

static const short yycheck[] = {    44,
    45,    46,    52,    53,    59,    60,    61,    62,    63,    54,
    55,    79,    80,    81,    82,    27,    27,    62,    63,    64,
    77,    59,    55,    56,    57,    58,    71,    75,    76,    77,
    98,    76,    70,    71,    72,    73,     3,    26,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    26,    37,    75,
    76,    77,    26,    42,    43,   120,    26,    26,   146,   147,
   148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
   158,    60,    61,    77,    74,   163,   164,   165,   166,   167,
   168,   169,   170,   171,   172,   173,   174,   175,   176,   177,
   178,    78,    26,    26,    47,    48,    49,    49,    50,    51,
    52,    53,    54,    56,    57,    20,    21,    22,    27,   197,
   198,   199,    65,    66,    67,    74,     3,   182,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    77,   225,    59,    60,
    61,    77,   230,    64,    65,   233,    67,    68,   218,    77,
    77,   216,    59,    60,    61,    77,    76,    64,    65,    66,
    67,    68,    69,    77,   252,    29,    30,    31,    32,    33,
    34,    35,    77,    77,    77,   240,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     3,    78,     5,     6,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     3,    77,     5,     6,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     3,    77,     5,     6,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,    77,     3,    78,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,     3,    78,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,     3,    78,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,    59,    60,    61,    77,    76,    64,
    65,    66,    67,    68,    69,    76,    78,     0,     3,    78,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,     3,    78,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,     3,    78,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    16,    17,
    18,    19,    20,    21,    22,    18,    19,    20,    21,    22,
    78,     0,     3,    78,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,     3,    78,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,     3,    78,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    -1,   140,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,     3,    78,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,     3,    78,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,     3,    78,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    18,    19,    -1,    -1,
    -1,    -1,    24,    25,    -1,    27,    28,    -1,    -1,    76,
    -1,    -1,    -1,    -1,    36,    37,    38,    39,    40,    41,
    42,    43,    44,    45,    46,    47,    48,    -1,     3,    76,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    -1,    76,
    -1,    -1,    -1,    -1,    -1,    77,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

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
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
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

#ifndef YYPARSE_RETURN_TYPE
#define YYPARSE_RETURN_TYPE int
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
YYPARSE_RETURN_TYPE yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
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
__yy_memcpy (char *to, char *from, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 196 "/usr/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

YYPARSE_RETURN_TYPE
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

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
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

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
      __yy_memcpy ((char *)yyss, (char *)yyss1, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1, size * sizeof (*yylsp));
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
  if (yylen > 0)
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
		trimesh.alpha	   = 2.0;
		trimesh.target	   = 100;

		trimesh.definition = last_trimesh_definition;

		curve_size = 2;
		if (!(trimesh.curves = Allocate (Curve, curve_size)))
		    Fatal ("unable to allocate curve array");
	    ;
    break;}
case 66:
#line 597 "parser.y"
{
		trimesh.definition = defnlookup (yyvsp[0].s);
		Deallocate (yyvsp[0].s);
	    ;
    break;}
case 67:
#line 603 "parser.y"
{
		trimesh.alpha = yyvsp[0].d;
	    ;
    break;}
case 68:
#line 608 "parser.y"
{
		trimesh.target = yyvsp[0].i;
	    ;
    break;}
case 69:
#line 613 "parser.y"
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
case 70:
#line 628 "parser.y"
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
case 71:
#line 651 "parser.y"
{
		yyval.i = LinearRule;
	    ;
    break;}
case 72:
#line 656 "parser.y"
{
		yyval.i = CosRule;
	    ;
    break;}
case 73:
#line 661 "parser.y"
{
		yyval.i = SinRule;
	    ;
    break;}
case 74:
#line 666 "parser.y"
{
		yyval.i = LogRule;
	    ;
    break;}
case 75:
#line 671 "parser.y"
{
		yyval.i = ParabolicRule;
	    ;
    break;}
case 76:
#line 676 "parser.y"
{
		yyval.i = RevLogRule;
	    ;
    break;}
case 77:
#line 681 "parser.y"
{
		yyval.i = RevParabolicRule;
	    ;
    break;}
case 78:
#line 691 "parser.y"
{
		if (curve.numvc == vcl_size)
		    if (!Reallocate (curve.vcl, xy_pair, vcl_size <<= 1))
			Fatal ("unable to allocate pair array");

		curve.vcl [curve.numvc] [0] = x;
		curve.vcl [curve.numvc ++] [1] = y;
	    ;
    break;}
case 79:
#line 701 "parser.y"
{
		if (curve.numvc == vcl_size)
		    if (!Reallocate (curve.vcl, xy_pair, vcl_size <<= 1))
			Fatal ("unable to allocate pair array");

		curve.vcl [curve.numvc] [0] = x;
		curve.vcl [curve.numvc ++] [1] = y;
	    ;
    break;}
case 80:
#line 711 "parser.y"
{
		vcl_size = 8;
		curve.numvc = 0;
		if (!(curve.vcl = Allocate (xy_pair, vcl_size)))
		    Fatal ("unable to allocate pair array");
	    ;
    break;}
case 81:
#line 722 "parser.y"
{
		x = yyvsp[-4].d;
		y = yyvsp[-2].d;
		z = yyvsp[-1].d;
	    ;
    break;}
case 82:
#line 732 "parser.y"
{
		yyval.d = yyvsp[0].d;
	    ;
    break;}
case 83:
#line 737 "parser.y"
{
		yyval.d = last_z;
	    ;
    break;}
case 84:
#line 745 "parser.y"
{
		x = yyvsp[-3].d;
		y = yyvsp[-1].d;
	    ;
    break;}
case 85:
#line 757 "parser.y"
{
		EmitCode (HaltOp);
		SetIP (0);
		yyval.d = EvalCode (InCore, 0.0);
	    ;
    break;}
case 86:
#line 767 "parser.y"
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
case 87:
#line 778 "parser.y"
{
		int ip = GetIP ( );
		SetIP (ip - yyvsp[0].i - 3);
		EmitCode (JnzOp, yyvsp[0].i + 1);
		SetIP (ip);
		EmitCode (TestOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + yyvsp[0].i + 1;
	    ;
    break;}
case 88:
#line 788 "parser.y"
{
		int ip = GetIP ( );
		SetIP (ip - yyvsp[0].i - 3);
		EmitCode (JzOp, yyvsp[0].i + 1);
		SetIP (ip);
		EmitCode (TestOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + yyvsp[0].i + 1;
	    ;
    break;}
case 89:
#line 798 "parser.y"
{
		EmitCode (OrOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 90:
#line 804 "parser.y"
{
		EmitCode (XorOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 91:
#line 810 "parser.y"
{
		EmitCode (AndOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 92:
#line 816 "parser.y"
{
		EmitCode (EqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 93:
#line 822 "parser.y"
{
		EmitCode (NeqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 94:
#line 828 "parser.y"
{
		EmitCode (LtOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 95:
#line 834 "parser.y"
{
		EmitCode (GtOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 96:
#line 840 "parser.y"
{
		EmitCode (LteqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 97:
#line 846 "parser.y"
{
		EmitCode (GteqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 98:
#line 852 "parser.y"
{
		EmitCode (LsftOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 99:
#line 858 "parser.y"
{
		EmitCode (RsftOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 100:
#line 864 "parser.y"
{
		EmitCode (AddOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 101:
#line 870 "parser.y"
{
		EmitCode (SubOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 102:
#line 876 "parser.y"
{
		EmitCode (MulOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 103:
#line 882 "parser.y"
{
		EmitCode (DivOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 104:
#line 888 "parser.y"
{
		EmitCode (ModOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 105:
#line 894 "parser.y"
{
		yyval.i = yyvsp[0].i;
	    ;
    break;}
case 106:
#line 899 "parser.y"
{
		EmitCode (NegOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 107:
#line 905 "parser.y"
{
		EmitCode (NotOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 108:
#line 911 "parser.y"
{
		EmitCode (InvOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 109:
#line 917 "parser.y"
{
		yyval.i = yyvsp[-1].i;
	    ;
    break;}
case 110:
#line 922 "parser.y"
{
		EmitCode (PushOp, (double) yyvsp[0].i);
		yyval.i = 2;
	    ;
    break;}
case 111:
#line 928 "parser.y"
{
		EmitCode (PushOp, yyvsp[0].d);
		yyval.i = 2;
	    ;
    break;}
case 113:
#line 939 "parser.y"
{
		EmitCode (SinOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 114:
#line 945 "parser.y"
{
		EmitCode (CosOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 115:
#line 951 "parser.y"
{
		EmitCode (TanOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 116:
#line 957 "parser.y"
{
		EmitCode (PowOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 117:
#line 963 "parser.y"
{
		EmitCode (ExpOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 118:
#line 969 "parser.y"
{
		EmitCode (LnOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 119:
#line 975 "parser.y"
{
		EmitCode (LogOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 120:
#line 981 "parser.y"
{
		EmitCode (SqrtOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 121:
#line 987 "parser.y"
{
		EmitCode (HypotOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 122:
#line 993 "parser.y"
{
		EmitCode (FloorOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 123:
#line 999 "parser.y"
{
		EmitCode (CeilOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 124:
#line 1005 "parser.y"
{
		EmitCode (FmodOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 125:
#line 1011 "parser.y"
{
		EmitCode (FabsOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 126:
#line 1020 "parser.y"
{
		EmitCode (JzOp, 0);
		yyval.i = 2;
	    ;
    break;}
case 127:
#line 1029 "parser.y"
{
		EmitCode (JmpOp, 0);
		yyval.i = 2;
	    ;
    break;}
case 128:
#line 1038 "parser.y"
{
		EmitCode (CopyOp);
		EmitCode (JnzOp, 0);
		EmitCode (PopOp);
		yyval.i = 4;
	    ;
    break;}
case 129:
#line 1049 "parser.y"
{
		EmitCode (CopyOp);
		EmitCode (JzOp, 0);
		EmitCode (PopOp);
		yyval.i = 4;
	    ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 498 "/usr/share/bison.simple"

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
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
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
#line 1056 "parser.y"