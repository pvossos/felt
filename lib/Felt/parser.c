
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
#define	BOOLEAN	270
#define	TIME	271
#define	SIN	272
#define	COS	273
#define	TAN	274
#define	POW	275
#define	EXP	276
#define	LOG	277
#define	LOG10	278
#define	SQRT	279
#define	HYPOT	280
#define	FLOOR	281
#define	CEIL	282
#define	FMOD	283
#define	FABS	284
#define	ANALYSIS_TYPE	285
#define	DIRECTION	286
#define	CONSTRAINT	287
#define	HINGED	288
#define	NODE_DOF	289
#define	MASS_MODE	290
#define	PROBLEM	291
#define	ANALYSIS	292
#define	LOAD_CASES	293
#define	END	294
#define	NODES	295
#define	ELEMENTS	296
#define	MATERIALS	297
#define	LOADS	298
#define	FORCES	299
#define	CONSTRAINTS	300
#define	TITLE_EQ	301
#define	NODES_EQ	302
#define	ELEMENTS_EQ	303
#define	ANALYSIS_EQ	304
#define	X_EQ	305
#define	Y_EQ	306
#define	Z_EQ	307
#define	FORCE_EQ	308
#define	CONSTRAINT_EQ	309
#define	MASS_EQ	310
#define	LOAD_EQ	311
#define	MATERIAL_EQ	312
#define	E_EQ	313
#define	IX_EQ	314
#define	IY_EQ	315
#define	IZ_EQ	316
#define	A_EQ	317
#define	J_EQ	318
#define	G_EQ	319
#define	T_EQ	320
#define	RHO_EQ	321
#define	NU_EQ	322
#define	KAPPA_EQ	323
#define	RK_EQ	324
#define	RM_EQ	325
#define	KX_EQ	326
#define	KY_EQ	327
#define	KZ_EQ	328
#define	C_EQ	329
#define	DIRECTION_EQ	330
#define	VALUES_EQ	331
#define	FX_EQ	332
#define	FY_EQ	333
#define	FZ_EQ	334
#define	MX_EQ	335
#define	MY_EQ	336
#define	MZ_EQ	337
#define	SFX_EQ	338
#define	SFY_EQ	339
#define	SFZ_EQ	340
#define	SMX_EQ	341
#define	SMY_EQ	342
#define	SMZ_EQ	343
#define	TX_EQ	344
#define	TY_EQ	345
#define	TZ_EQ	346
#define	RX_EQ	347
#define	RY_EQ	348
#define	RZ_EQ	349
#define	ITX_EQ	350
#define	ITY_EQ	351
#define	ITZ_EQ	352
#define	IRX_EQ	353
#define	IRY_EQ	354
#define	IRZ_EQ	355
#define	VX_EQ	356
#define	VY_EQ	357
#define	VZ_EQ	358
#define	AX_EQ	359
#define	AY_EQ	360
#define	AZ_EQ	361
#define	ALPHA_EQ	362
#define	BETA_EQ	363
#define	GAMMA_EQ	364
#define	DOFS_EQ	365
#define	MASS_MODE_EQ	366
#define	START_EQ	367
#define	STOP_EQ	368
#define	STEP_EQ	369
#define	GRAVITY_EQ	370
#define	ITERATIONS_EQ	371
#define	TOLERANCE_EQ	372
#define	LOAD_STEPS_EQ	373
#define	RELAXATION_EQ	374
#define	INPUT_RANGE_EQ	375
#define	INPUT_DOF_EQ	376
#define	INPUT_NODE_EQ	377
#define	NODE_FORCES_EQ	378
#define	ELEMENT_LOADS_EQ	379
#define	CANVAS	380
#define	FIGURES	381
#define	NODE_NUM_EQ	382
#define	ELT_NUM_EQ	383
#define	SNAP_EQ	384
#define	GRID_EQ	385
#define	SNAP_SIZE_EQ	386
#define	GRID_SIZE_EQ	387
#define	X_MIN_EQ	388
#define	X_MAX_EQ	389
#define	Y_MIN_EQ	390
#define	Y_MAX_EQ	391
#define	SCALE_EQ	392
#define	X_POS_EQ	393
#define	Y_POS_EQ	394
#define	WIDTH_EQ	395
#define	HEIGHT_EQ	396
#define	NODE_COLOR_EQ	397
#define	ELT_COLOR_EQ	398
#define	LABEL_FONT_EQ	399
#define	TOOL_COLOR_EQ	400
#define	TOOL_FONT_EQ	401
#define	FONT_EQ	402
#define	COLOR_EQ	403
#define	LENGTH_EQ	404
#define	TEXT_EQ	405
#define	POINTS_EQ	406
#define	FIGURE_TYPE	407

#line 1 "parser.y"

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
 * File:	parser.y						*
 *									*
 * Description:	This file contains the yacc specification for the	*
 *		parser for the FElt 2.0 system.				*
 ************************************************************************/

# include <stdio.h>
# include "code.h"
# include "error.h"
# include "objects.h"
# include "problem.h"
# include "allocate.h"

# if !defined (__GNUC__) && !defined (__sparc__)
# define alloca malloc		/* prevents alloca from being called */
# endif

# define VariableExpression 2	/* not 0, 1, or, 'h' */

extern void yyerror ( );
extern int  yylex  ( );
# ifdef NEED_STRDUP
extern char *strdup ( );
# endif

/* Last parameters (default for some parameters is to inherit the last). */

static double		  last_x;		/* last x coordinate	   */
static double		  last_y;		/* last y coordinate	   */
static double		  last_z;		/* last z coordinate	   */
static char		 *last_constraint;	/* name of last constraint */
static char		 *last_material;	/* name of last material   */



/* Current objects (inherited attributes). */

static Item		  found;		/* current object	   */
static Node		  node;			/* current node		   */
static Element		  element;		/* current element	   */
static Material		  material;		/* current material	   */
static Distributed	  load;			/* current load		   */
static Force		  force;		/* current force	   */
static Constraint	  constraint;		/* current constraint	   */
static Definition	  definition;		/* current definition	   */
static LoadCase		  loadcase;		/* current loadcase	   */


/* Dummy strucutures.  If an object is defined twice, the second definition
   is illegal and the current object is set to a dummy object. */

static struct node	  dummy_node;		/* dummy node		   */
static struct element	  dummy_element;	/* dummy element	   */
static struct material	  dummy_material;	/* dummy material	   */
static struct distributed dummy_load;		/* dummy distributed load  */
static struct force	  dummy_force;		/* dummy force		   */
static struct constraint  dummy_constraint;	/* dummy constraint	   */
static struct loadcase    dummy_loadcase;	/* dummy loadcase	   */


/* Temporary arrays. */

static int		  int_array [1024];	/* temporary integer array */
static int		 *int_ptr;		/* pointer into array	   */
static Pair		  pair_array [1024];	/* temporary pair array	   */
static Pair		 *pair_ptr;		/* pointer into array	   */
static CasePair		  case_array [1024];	/* temporary case pair array */
static CasePair		 *case_ptr;		/* pointer into case array */


/* Temporary variables for gravity triplet */

static double		  triple_x;		
static double		  triple_y;
static double		  triple_z;


/* Discrete expression variables. */

static int		  table_error = 0;	/* error indicator	   */
static double		  last_time = 0;	/* last time coordinate	   */
static double		 *table = NULL;		/* table of values	   */
static unsigned		  table_count = 0;	/* count of values	   */
static unsigned		  table_size = 0;	/* size of table	   */


/* Figure list variables. */

static float		  figure_x;		/* current x-coordinate	   */
static float		  figure_y;		/* current y-coordinate	   */
static unsigned		  figure_size;		/* size of figure list	   */
static unsigned		  fig_point_size;	/* size of point list	   */
static FigInfo		 *figure;		/* current figure	   */

#line 117 "parser.y"
typedef union {
    int       i;
    double    d;
    char     *s;
    Pair      p;
    CasePair  cp;
    char      c;
} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		553
#define	YYFLAG		-32768
#define	YYNTBASE	172

#define YYTRANSLATE(x) ((unsigned)(x) <= 407 ? yytranslate[x] : 263)

static const short yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    24,     2,     2,     2,    22,     9,     2,   167,
   168,    20,    18,   171,    19,     2,    21,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     4,     2,    12,
     2,    13,     3,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
   169,     2,   170,     8,     2,     2,     2,     2,     2,     2,
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
    70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
    80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
    90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
   100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
   110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
   120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
   130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
   140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
   150,   151,   152,   153,   154,   155,   156,   157,   158,   159,
   160,   161,   162,   163,   164,   165,   166
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     5,     9,    10,    13,    14,    17,    18,    21,    24,
    27,    30,    32,    35,    36,    38,    40,    42,    44,    46,
    48,    50,    52,    54,    56,    58,    61,    64,    65,    68,
    70,    72,    76,    79,    80,    83,    86,    89,    92,    95,
    98,   100,   103,   105,   108,   109,   112,   114,   116,   120,
   123,   124,   129,   132,   135,   137,   141,   144,   146,   148,
   152,   155,   157,   160,   163,   164,   167,   169,   172,   173,
   176,   179,   182,   185,   188,   191,   194,   197,   200,   203,
   206,   209,   212,   215,   218,   221,   224,   227,   229,   232,
   235,   236,   239,   241,   244,   245,   248,   251,   254,   256,
   260,   263,   265,   271,   276,   279,   282,   283,   286,   288,
   291,   292,   295,   299,   303,   307,   311,   315,   319,   323,
   327,   331,   335,   339,   343,   345,   348,   351,   352,   355,
   357,   360,   361,   364,   368,   372,   376,   380,   384,   388,
   391,   394,   397,   400,   403,   406,   409,   412,   415,   418,
   421,   424,   426,   428,   430,   432,   434,   436,   439,   442,
   443,   446,   448,   451,   452,   455,   458,   460,   464,   467,
   469,   475,   480,   483,   486,   487,   490,   493,   496,   499,
   502,   505,   508,   511,   514,   517,   520,   523,   526,   529,
   534,   539,   542,   545,   547,   551,   554,   555,   559,   562,
   563,   570,   573,   574,   577,   580,   581,   584,   587,   590,
   593,   596,   599,   602,   605,   608,   611,   614,   617,   620,
   623,   626,   629,   632,   635,   638,   641,   644,   646,   649,
   650,   653,   655,   658,   659,   662,   665,   668,   671,   674,
   677,   680,   683,   686,   691,   695,   698,   699,   705,   707,
   709,   712,   716,   719,   721,   727,   728,   730,   738,   743,
   748,   752,   756,   760,   764,   768,   772,   776,   780,   784,
   788,   792,   796,   800,   804,   808,   812,   815,   818,   821,
   824,   828,   830,   832,   834,   836,   841,   846,   851,   858,
   863,   868,   873,   878,   885,   890,   895,   902,   907,   908,
   909,   910
};

static const short yyrhs[] = {   173,
   174,   177,    53,     0,    64,   252,    53,     0,     0,    50,
   175,     0,     0,   175,   176,     0,     0,    60,    26,     0,
    61,    27,     0,    62,    27,     0,    63,    44,     0,     1,
     0,   177,   178,     0,     0,   179,     0,   186,     0,   197,
     0,   203,     0,   211,     0,   217,     0,   233,     0,   225,
     0,   240,     0,   243,     0,    53,     0,    54,   180,     0,
   180,   181,     0,     0,   182,   184,     0,   183,     0,    27,
     0,   167,   256,   168,     0,   184,   185,     0,     0,    64,
   256,     0,    65,   256,     0,    66,   256,     0,    69,   256,
     0,    67,    26,     0,    68,    26,     0,     1,     0,   187,
   188,     0,    55,     0,   188,   189,     0,     0,   190,   192,
     0,   191,     0,    27,     0,   167,   256,   168,     0,   192,
   193,     0,     0,    61,   169,   194,   170,     0,    71,    26,
     0,    70,   196,     0,     1,     0,   194,   171,   195,     0,
   194,   195,     0,   195,     0,   183,     0,   196,   171,    26,
     0,   196,    26,     0,    26,     0,    56,   198,     0,   198,
   199,     0,     0,   200,   201,     0,    26,     0,   201,   202,
     0,     0,   162,    26,     0,    72,   256,     0,    73,   256,
     0,    74,   256,     0,    75,   256,     0,    76,   256,     0,
    77,   256,     0,    78,   256,     0,    79,   256,     0,    80,
   256,     0,    81,   256,     0,    82,   256,     0,    83,   256,
     0,    84,   256,     0,    85,   256,     0,    86,   256,     0,
    87,   256,     0,    88,   256,     0,     1,     0,    57,   204,
     0,   204,   205,     0,     0,   206,   207,     0,    26,     0,
   207,   208,     0,     0,   162,    26,     0,    89,    45,     0,
    90,   209,     0,     1,     0,   209,   171,   210,     0,   209,
   210,     0,   210,     0,   167,   183,   171,   256,   168,     0,
   167,   183,   256,   168,     0,    58,   212,     0,   212,   213,
     0,     0,   214,   215,     0,    26,     0,   215,   216,     0,
     0,   162,    26,     0,    91,   255,   252,     0,    92,   255,
   252,     0,    93,   255,   252,     0,    94,   255,   252,     0,
    95,   255,   252,     0,    96,   255,   252,     0,    97,   255,
   252,     0,    98,   255,   252,     0,    99,   255,   252,     0,
   100,   255,   252,     0,   101,   255,   252,     0,   102,   255,
   252,     0,     1,     0,    59,   218,     0,   218,   219,     0,
     0,   220,   221,     0,    26,     0,   221,   222,     0,     0,
   162,    26,     0,   103,   255,   223,     0,   104,   255,   223,
     0,   105,   255,   223,     0,   106,   255,   224,     0,   107,
   255,   224,     0,   108,   255,   224,     0,   109,   256,     0,
   110,   256,     0,   111,   256,     0,   112,   256,     0,   113,
   256,     0,   114,   256,     0,   115,   256,     0,   116,   256,
     0,   117,   256,     0,   118,   256,     0,   119,   256,     0,
   120,   256,     0,     1,     0,   252,     0,    46,     0,   252,
     0,    46,     0,    47,     0,    52,   226,     0,   226,   227,
     0,     0,   228,   229,     0,    26,     0,   229,   230,     0,
     0,   137,   231,     0,   138,   231,     0,     1,     0,   231,
   171,   232,     0,   231,   232,     0,   232,     0,   167,   183,
   171,    26,   168,     0,   167,   183,    26,   168,     0,    51,
   234,     0,   234,   235,     0,     0,   121,   256,     0,   122,
   256,     0,   123,   256,     0,    83,   256,     0,    84,   256,
     0,   126,   256,     0,   128,   256,     0,   127,   256,     0,
   130,    27,     0,   132,    27,     0,   133,   256,     0,   131,
   256,     0,   135,    48,     0,   136,   183,     0,    61,   169,
   236,   170,     0,   124,   169,   237,   170,     0,   125,    49,
     0,   129,   238,     0,     1,     0,   236,   171,   183,     0,
   236,   183,     0,     0,   237,   171,    48,     0,   237,    48,
     0,     0,   167,   256,   171,   256,   239,   168,     0,   171,
   256,     0,     0,   139,   241,     0,   241,   242,     0,     0,
   141,    29,     0,   142,    29,     0,   143,    29,     0,   144,
    29,     0,   145,   256,     0,   146,   256,     0,   147,   256,
     0,   148,   256,     0,   149,   256,     0,   150,   256,     0,
   152,   256,     0,   153,   256,     0,   154,   256,     0,   155,
   256,     0,   151,   256,     0,   156,    26,     0,   157,    26,
     0,   158,    26,     0,   159,    26,     0,   160,    26,     0,
   244,   245,     0,   140,     0,   245,   246,     0,     0,   247,
   248,     0,   166,     0,   248,   249,     0,     0,    64,   256,
     0,    65,   256,     0,   154,   256,     0,   155,   256,     0,
   126,   256,     0,   163,   256,     0,   164,    26,     0,   162,
    26,     0,   161,    26,     0,   165,   169,   250,   170,     0,
   250,   171,   251,     0,   250,   251,     0,     0,   167,   256,
   171,   256,   168,     0,   257,     0,   253,     0,   253,    18,
     0,   253,   171,   254,     0,   253,   254,     0,   254,     0,
   167,   256,   171,   256,   168,     0,     0,   257,     0,   257,
     3,   259,   257,     4,   260,   257,     0,   257,     5,   261,
   257,     0,   257,     6,   262,   257,     0,   257,     7,   257,
     0,   257,     8,   257,     0,   257,     9,   257,     0,   257,
    10,   257,     0,   257,    11,   257,     0,   257,    12,   257,
     0,   257,    13,   257,     0,   257,    14,   257,     0,   257,
    15,   257,     0,   257,    16,   257,     0,   257,    17,   257,
     0,   257,    18,   257,     0,   257,    19,   257,     0,   257,
    20,   257,     0,   257,    21,   257,     0,   257,    22,   257,
     0,    18,   257,     0,    19,   257,     0,    24,   257,     0,
    25,   257,     0,   167,   257,   168,     0,    27,     0,    28,
     0,    30,     0,   258,     0,    31,   167,   257,   168,     0,
    32,   167,   257,   168,     0,    33,   167,   257,   168,     0,
    34,   167,   257,   171,   257,   168,     0,    35,   167,   257,
   168,     0,    36,   167,   257,   168,     0,    37,   167,   257,
   168,     0,    38,   167,   257,   168,     0,    39,   167,   257,
   171,   257,   168,     0,    40,   167,   257,   168,     0,    41,
   167,   257,   168,     0,    42,   167,   257,   171,   257,   168,
     0,    43,   167,   257,   168,     0,     0,     0,     0,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   192,   193,   198,   212,   213,   218,   219,   224,   230,   235,
   240,   245,   252,   253,   258,   259,   260,   261,   262,   263,
   264,   265,   266,   267,   268,   275,   280,   281,   286,   291,
   319,   320,   326,   327,   332,   337,   342,   347,   352,   358,
   363,   370,   375,   386,   387,   392,   397,   422,   423,   429,
   430,   435,   457,   462,   464,   469,   474,   479,   488,   497,
   510,   523,   536,   541,   542,   547,   552,   566,   567,   572,
   578,   583,   588,   593,   598,   603,   608,   613,   618,   623,
   628,   633,   638,   643,   648,   653,   658,   663,   670,   675,
   676,   681,   686,   701,   702,   707,   713,   718,   739,   744,
   749,   754,   763,   772,   786,   791,   792,   797,   802,   817,
   818,   823,   829,   834,   839,   844,   849,   854,   859,   864,
   869,   874,   879,   884,   889,   896,   901,   902,   907,   912,
   927,   928,   933,   939,   949,   959,   969,   979,   989,   999,
  1004,  1009,  1014,  1019,  1024,  1029,  1034,  1039,  1044,  1049,
  1054,  1059,  1064,  1069,  1077,  1082,  1087,  1097,  1102,  1103,
  1107,  1111,  1125,  1126,  1130,  1155,  1180,  1185,  1190,  1195,
  1204,  1213,  1226,  1231,  1232,  1237,  1242,  1247,  1252,  1257,
  1262,  1267,  1272,  1277,  1282,  1287,  1292,  1297,  1302,  1307,
  1322,  1333,  1338,  1345,  1350,  1355,  1360,  1368,  1373,  1378,
  1390,  1400,  1405,  1415,  1420,  1421,  1426,  1431,  1436,  1441,
  1446,  1451,  1456,  1461,  1466,  1471,  1476,  1481,  1486,  1491,
  1496,  1501,  1507,  1513,  1519,  1525,  1536,  1541,  1549,  1550,
  1555,  1560,  1586,  1587,  1592,  1597,  1602,  1607,  1612,  1617,
  1622,  1628,  1634,  1640,  1645,  1658,  1671,  1684,  1695,  1701,
  1715,  1732,  1733,  1734,  1739,  1760,  1768,  1778,  1789,  1799,
  1809,  1815,  1821,  1827,  1833,  1839,  1845,  1851,  1857,  1863,
  1869,  1875,  1881,  1887,  1893,  1899,  1905,  1910,  1916,  1922,
  1928,  1933,  1939,  1945,  1951,  1956,  1962,  1968,  1974,  1980,
  1986,  1992,  1998,  2004,  2010,  2016,  2022,  2028,  2037,  2046,
  2055,  2066
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","'?'","':'",
"OR","AND","'|'","'^'","'&'","EQUALS","NEQUAL","'<'","'>'","LT_EQ","GT_EQ","LSHIFT",
"RSHIFT","'+'","'-'","'*'","'/'","'%'","UNARY","'!'","'~'","NAME","INTEGER",
"DOUBLE","BOOLEAN","TIME","SIN","COS","TAN","POW","EXP","LOG","LOG10","SQRT",
"HYPOT","FLOOR","CEIL","FMOD","FABS","ANALYSIS_TYPE","DIRECTION","CONSTRAINT",
"HINGED","NODE_DOF","MASS_MODE","PROBLEM","ANALYSIS","LOAD_CASES","END","NODES",
"ELEMENTS","MATERIALS","LOADS","FORCES","CONSTRAINTS","TITLE_EQ","NODES_EQ",
"ELEMENTS_EQ","ANALYSIS_EQ","X_EQ","Y_EQ","Z_EQ","FORCE_EQ","CONSTRAINT_EQ",
"MASS_EQ","LOAD_EQ","MATERIAL_EQ","E_EQ","IX_EQ","IY_EQ","IZ_EQ","A_EQ","J_EQ",
"G_EQ","T_EQ","RHO_EQ","NU_EQ","KAPPA_EQ","RK_EQ","RM_EQ","KX_EQ","KY_EQ","KZ_EQ",
"C_EQ","DIRECTION_EQ","VALUES_EQ","FX_EQ","FY_EQ","FZ_EQ","MX_EQ","MY_EQ","MZ_EQ",
"SFX_EQ","SFY_EQ","SFZ_EQ","SMX_EQ","SMY_EQ","SMZ_EQ","TX_EQ","TY_EQ","TZ_EQ",
"RX_EQ","RY_EQ","RZ_EQ","ITX_EQ","ITY_EQ","ITZ_EQ","IRX_EQ","IRY_EQ","IRZ_EQ",
"VX_EQ","VY_EQ","VZ_EQ","AX_EQ","AY_EQ","AZ_EQ","ALPHA_EQ","BETA_EQ","GAMMA_EQ",
"DOFS_EQ","MASS_MODE_EQ","START_EQ","STOP_EQ","STEP_EQ","GRAVITY_EQ","ITERATIONS_EQ",
"TOLERANCE_EQ","LOAD_STEPS_EQ","RELAXATION_EQ","INPUT_RANGE_EQ","INPUT_DOF_EQ",
"INPUT_NODE_EQ","NODE_FORCES_EQ","ELEMENT_LOADS_EQ","CANVAS","FIGURES","NODE_NUM_EQ",
"ELT_NUM_EQ","SNAP_EQ","GRID_EQ","SNAP_SIZE_EQ","GRID_SIZE_EQ","X_MIN_EQ","X_MAX_EQ",
"Y_MIN_EQ","Y_MAX_EQ","SCALE_EQ","X_POS_EQ","Y_POS_EQ","WIDTH_EQ","HEIGHT_EQ",
"NODE_COLOR_EQ","ELT_COLOR_EQ","LABEL_FONT_EQ","TOOL_COLOR_EQ","TOOL_FONT_EQ",
"FONT_EQ","COLOR_EQ","LENGTH_EQ","TEXT_EQ","POINTS_EQ","FIGURE_TYPE","'('","')'",
"'['","']'","','","specification","initialize","problem_description","problem_parameter_list",
"problem_parameter","section_list","section","node_section","node_definition_list",
"node_definition","node_number","node_number_expression","node_parameter_list",
"node_parameter","element_section","element_header","element_definition_list",
"element_definition","element_number","element_number_expression","element_parameter_list",
"element_parameter","element_node_list","element_node","element_load_list","material_section",
"material_definition_list","material_definition","material_name","material_parameter_list",
"material_parameter","load_section","load_definition_list","load_definition",
"load_name","load_parameter_list","load_parameter","value_pair_list","value_pair",
"force_section","force_definition_list","force_definition","force_name","force_parameter_list",
"force_parameter","constraint_section","constraint_definition_list","constraint_definition",
"constraint_name","constraint_parameter_list","constraint_parameter","translation",
"rotation","loadcase_section","loadcase_definition_list","loadcase_definition",
"loadcase_name","loadcase_parameter_list","loadcase_parameter","loadcase_pair_list",
"loadcase_pair","analysis_section","analysis_parameter_list","analysis_parameter",
"analysis_node_list","analysis_dof_list","triple","opt_z_coordinate","canvas_section",
"canvas_parameter_list","canvas_parameter","figure_section","figure_header",
"figure_definition_list","figure_definition","figure_type","figure_parameter_list",
"figure_parameter","figure_pair_list","figure_pair","variable_expression","discrete_pair_list",
"discrete_pair","enable_copy","constant_expression","expression","function",
"if_action","else_action","or_action","and_action", NULL
};
#endif

static const short yyr1[] = {     0,
   172,   172,   173,   174,   174,   175,   175,   176,   176,   176,
   176,   176,   177,   177,   178,   178,   178,   178,   178,   178,
   178,   178,   178,   178,   178,   179,   180,   180,   181,   182,
   183,   183,   184,   184,   185,   185,   185,   185,   185,   185,
   185,   186,   187,   188,   188,   189,   190,   191,   191,   192,
   192,   193,   193,   193,   193,   194,   194,   194,   195,   196,
   196,   196,   197,   198,   198,   199,   200,   201,   201,   202,
   202,   202,   202,   202,   202,   202,   202,   202,   202,   202,
   202,   202,   202,   202,   202,   202,   202,   202,   203,   204,
   204,   205,   206,   207,   207,   208,   208,   208,   208,   209,
   209,   209,   210,   210,   211,   212,   212,   213,   214,   215,
   215,   216,   216,   216,   216,   216,   216,   216,   216,   216,
   216,   216,   216,   216,   216,   217,   218,   218,   219,   220,
   221,   221,   222,   222,   222,   222,   222,   222,   222,   222,
   222,   222,   222,   222,   222,   222,   222,   222,   222,   222,
   222,   222,   223,   223,   224,   224,   224,   225,   226,   226,
   227,   228,   229,   229,   230,   230,   230,   231,   231,   231,
   232,   232,   233,   234,   234,   235,   235,   235,   235,   235,
   235,   235,   235,   235,   235,   235,   235,   235,   235,   235,
   235,   235,   235,   235,   236,   236,   236,   237,   237,   237,
   238,   239,   239,   240,   241,   241,   242,   242,   242,   242,
   242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
   242,   242,   242,   242,   242,   242,   243,   244,   245,   245,
   246,   247,   248,   248,   249,   249,   249,   249,   249,   249,
   249,   249,   249,   249,   250,   250,   250,   251,   252,   252,
   252,   253,   253,   253,   254,   255,   256,   257,   257,   257,
   257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
   257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
   257,   257,   257,   257,   257,   258,   258,   258,   258,   258,
   258,   258,   258,   258,   258,   258,   258,   258,   259,   260,
   261,   262
};

static const short yyr2[] = {     0,
     4,     3,     0,     2,     0,     2,     0,     2,     2,     2,
     2,     1,     2,     0,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     2,     2,     0,     2,     1,
     1,     3,     2,     0,     2,     2,     2,     2,     2,     2,
     1,     2,     1,     2,     0,     2,     1,     1,     3,     2,
     0,     4,     2,     2,     1,     3,     2,     1,     1,     3,
     2,     1,     2,     2,     0,     2,     1,     2,     0,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     1,     2,     2,
     0,     2,     1,     2,     0,     2,     2,     2,     1,     3,
     2,     1,     5,     4,     2,     2,     0,     2,     1,     2,
     0,     2,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     1,     2,     2,     0,     2,     1,
     2,     0,     2,     3,     3,     3,     3,     3,     3,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     1,     1,     1,     1,     1,     1,     2,     2,     0,
     2,     1,     2,     0,     2,     2,     1,     3,     2,     1,
     5,     4,     2,     2,     0,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     4,
     4,     2,     2,     1,     3,     2,     0,     3,     2,     0,
     6,     2,     0,     2,     2,     0,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     1,     2,     0,
     2,     1,     2,     0,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     4,     3,     2,     0,     5,     1,     1,
     2,     3,     2,     1,     5,     0,     1,     7,     4,     4,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     2,     2,     2,     2,
     3,     1,     1,     1,     1,     4,     4,     4,     6,     4,
     4,     4,     4,     6,     4,     4,     6,     4,     0,     0,
     0,     0
};

static const short yydefact[] = {     3,
     0,     5,     0,     0,     0,     0,   282,   283,   284,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   250,   254,   249,   285,     7,    14,
     0,   277,   278,   279,   280,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   257,
     2,   251,     0,     0,   253,   299,   301,   302,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   281,   257,   252,     0,     0,     0,   261,   262,   263,
   264,   265,   266,   267,   268,   269,   270,   271,   272,   273,
   274,   275,   276,    12,     0,     0,     0,     0,     6,   175,
   160,    25,    28,    43,    65,    91,   107,   128,   206,   228,
    13,    15,    16,    45,    17,    18,    19,    20,    22,    21,
    23,    24,   230,   286,   287,   288,     0,   290,   291,   292,
   293,     0,   295,   296,     0,   298,     0,     0,   259,   260,
     8,     9,    10,    11,     0,   158,    26,    63,    89,   105,
   126,   204,    42,   227,     0,     0,     0,   255,   300,   194,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   174,   162,
   159,   164,    31,     0,    27,    34,    30,    67,    64,    69,
    93,    90,    95,   109,   106,   111,   130,   127,   132,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   205,
    48,     0,    44,    51,    47,   232,   229,   234,   289,   294,
   297,     0,   197,   179,   180,   176,   177,   178,   200,   192,
   181,   183,   182,     0,   193,   184,   187,   185,   186,   188,
   189,     0,     0,     0,     0,     0,     0,     0,   207,   208,
   209,   210,   211,   212,   213,   214,   215,   216,   221,   217,
   218,   219,   220,   222,   223,   224,   225,   226,     0,     0,
   231,   258,     0,     0,     0,   167,     0,     0,   163,    32,
    41,     0,     0,     0,     0,     0,     0,    33,    88,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    68,    99,     0,
     0,     0,    94,   125,   256,   256,   256,   256,   256,   256,
   256,   256,   256,   256,   256,   256,     0,   110,   152,   256,
   256,   256,   256,   256,   256,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   131,    49,
    55,     0,     0,     0,    50,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   233,   190,     0,   196,   199,
   191,     0,     0,     0,   165,   170,   166,    35,    36,    37,
    39,    40,    38,    71,    72,    73,    74,    75,    76,    77,
    78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
    70,    97,     0,    98,   102,    96,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
     0,     0,     0,     0,     0,   140,   141,   142,   143,   144,
   145,   146,   147,   148,   149,   150,   151,   133,     0,    62,
    54,    53,   235,   236,   239,   237,   238,   243,   242,   240,
   241,   247,   195,   198,   203,     0,     0,   169,     0,     0,
   101,   113,   114,   115,   116,   117,   118,   119,   120,   121,
   122,   123,   124,   154,   134,   153,   135,   136,   156,   157,
   137,   155,   138,   139,    59,     0,    58,    61,     0,     0,
     0,     0,     0,     0,   168,     0,     0,   100,    52,     0,
    57,    60,     0,   244,     0,   246,   202,   201,   172,     0,
     0,   104,    56,     0,   245,   171,   103,     0,     0,   248,
     0,     0,     0
};

static const short yydefgoto[] = {   551,
     2,    30,    75,   119,    76,   131,   132,   167,   205,   206,
   515,   274,   318,   133,   134,   173,   243,   244,   245,   300,
   385,   516,   517,   471,   135,   168,   209,   210,   275,   338,
   136,   169,   212,   213,   276,   343,   434,   435,   137,   170,
   215,   216,   277,   358,   138,   171,   218,   219,   278,   379,
   505,   511,   139,   166,   201,   202,   272,   309,   405,   406,
   140,   165,   199,   303,   304,   265,   522,   141,   172,   240,
   142,   143,   174,   247,   248,   301,   396,   520,   536,   506,
    25,    26,   437,    49,    93,    28,    95,   252,    96,    97
};

static const short yypact[] = {   -50,
   230,   -30,   968,   968,   968,   968,-32768,-32768,-32768,  -116,
   -89,   -80,   -63,   -42,   -39,   -36,   -35,   -12,    -6,    -3,
    -2,    -1,   968,    91,   -17,-32768,  1307,-32768,-32768,-32768,
   968,-32768,-32768,-32768,-32768,   968,   968,   968,   968,   968,
   968,   968,   968,   968,   968,   968,   968,   968,   -14,   498,
-32768,-32768,   968,     0,-32768,-32768,-32768,-32768,   968,   968,
   968,   968,   968,   968,   968,   968,   968,   968,   968,   968,
   968,   968,   968,   968,   435,   -10,   498,   550,   575,   595,
    18,   615,   635,   662,   682,    49,   739,   759,   190,   779,
   968,-32768,  1307,-32768,   968,   968,   968,  1050,  1250,   695,
   102,   102,   197,   197,   197,   197,    62,    62,    55,    55,
-32768,-32768,-32768,-32768,   162,   143,   163,   147,-32768,-32768,
-32768,   192,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   968,-32768,-32768,-32768,
-32768,   968,-32768,-32768,   968,-32768,    26,  1208,   357,   407,
-32768,-32768,-32768,-32768,  1118,   198,   -22,   199,   201,   202,
   220,  1149,   -21,    81,   799,   826,   846,-32768,-32768,-32768,
    82,   968,   968,   968,   968,   968,    83,   207,   968,   968,
   968,   107,   223,   968,   266,   968,   246,   -22,-32768,-32768,
-32768,-32768,-32768,   968,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   271,
   272,   274,   275,   968,   968,   968,   968,   968,   968,   968,
   968,   968,   968,   968,   279,   280,   281,   282,   284,-32768,
-32768,   968,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   968,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,   968,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,  1141,   145,   335,   877,  1048,   989,   917,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   146,   412,
   -53,  1307,   -18,   -44,   148,-32768,   149,   149,-32768,-32768,
-32768,   968,   968,   968,   292,   294,   968,-32768,-32768,   968,
   968,   968,   968,   968,   968,   968,   968,   968,   968,   968,
   968,   968,   968,   968,   968,   968,   295,-32768,-32768,   290,
   170,   312,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   313,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   968,   968,   968,   968,   968,
   968,   968,   968,   968,   968,   968,   968,   314,-32768,-32768,
-32768,   172,   316,   317,-32768,   968,   968,   968,   968,   968,
   318,   354,   968,   358,   216,-32768,-32768,   -22,-32768,-32768,
-32768,   347,   968,   -22,  -154,-32768,  -154,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   -22,  -152,-32768,-32768,   230,   230,   230,   230,
   230,   230,   230,   230,   230,   230,   230,   230,-32768,  1122,
  1122,  1122,   852,   852,   852,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -22,-32768,
   -24,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   225,   -23,   149,-32768,   144,   170,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   -11,-32768,-32768,   379,   -82,
   968,   238,   239,   382,-32768,   968,   241,-32768,-32768,   -22,
-32768,-32768,   968,-32768,   243,-32768,-32768,-32768,-32768,   244,
   263,-32768,-32768,   240,-32768,-32768,-32768,   968,   264,-32768,
   434,   437,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -145,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,  -444,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,  -416,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -355,  -348,-32768,-32768,-32768,-32768,-32768,-32768,   132,  -397,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -79,     6,
-32768,    25,   179,   -91,   286,-32768,-32768,-32768,-32768,-32768
};


#define	YYLAST		1329


static const short yytable[] = {   157,
    52,   518,   523,   400,   203,   241,    24,   488,   203,   488,
   386,   387,   404,     1,   433,   203,   487,   491,   490,    29,
    56,   207,    57,    58,    59,    60,    61,    62,    63,    64,
    65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
   120,   121,   122,   123,   124,   125,   126,   127,   128,    55,
    36,    56,   271,    57,    58,    59,    60,    61,    62,    63,
    64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
    74,   531,   388,   528,    72,    73,    74,    37,    94,    70,
    71,    72,    73,    74,   533,   543,    38,   534,   535,   525,
   254,   255,   256,   257,   258,   507,   508,   261,   262,   263,
   389,   390,   267,    39,   269,   513,   514,   391,   392,   393,
   394,   395,   273,    64,    65,    66,    67,    68,    69,    70,
    71,    72,    73,    74,    40,   401,   402,    41,   129,   130,
    42,    43,   283,   284,   285,   286,   287,   288,   289,   290,
   291,   292,   293,    51,   204,   242,   519,   524,   204,    53,
   299,   397,   398,    54,    44,   204,    91,   399,   529,   530,
    45,     3,     4,    46,    47,    48,    53,     5,     6,   162,
     7,     8,   305,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,   161,   147,   163,
   164,    -1,    56,   178,    57,    58,    59,    60,    61,    62,
    63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
    73,    74,    68,    69,    70,    71,    72,    73,    74,   152,
   408,   409,   410,   200,   208,   413,   211,   214,   414,   415,
   416,   417,   418,   419,   420,   421,   422,   423,   424,   425,
   426,   427,   428,   429,   430,   217,   246,     3,     4,   266,
   253,   259,   483,     5,     6,   260,     7,     8,   486,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,   264,   456,   457,   458,   459,   460,   461,
   462,   463,   464,   465,   466,   467,    27,   489,    32,    33,
    34,    35,   268,   270,   473,   474,   475,   476,   477,   279,
   280,   480,   281,   282,   294,   295,   296,   297,    50,   298,
    31,   485,   310,   380,   526,   404,    77,   411,   403,   412,
   431,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    87,    88,    89,    90,   432,   311,   433,   436,   449,   468,
   469,   470,   472,   478,    98,    99,   100,   101,   102,   103,
   104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
   155,   -29,    58,    59,    60,    61,    62,    63,    64,    65,
    66,    67,    68,    69,    70,    71,    72,    73,    74,   479,
   158,   159,   160,   481,   482,   -29,   -29,   -29,   -29,   -29,
   -29,   -29,   -29,   -29,   484,   521,    23,   527,   312,   313,
   314,   315,   316,   317,   532,   538,   539,   540,   542,   533,
   548,   546,   381,    59,    60,    61,    62,    63,    64,    65,
    66,    67,    68,    69,    70,    71,    72,    73,    74,   537,
   547,   550,   175,   552,   541,   114,   553,   176,   -46,   407,
   177,   544,   492,   493,   494,   495,   496,   497,   498,   499,
   500,   501,   502,   503,     0,   545,   549,     0,   512,   512,
   512,     0,   -46,   -46,   -46,   -46,   -46,   -46,   -46,   -46,
   -46,     0,   382,   -29,   -29,     0,     0,     0,     0,     0,
     0,   383,   384,     0,     0,    -4,    -4,    -4,    -4,    -4,
    -4,    -4,    -4,    -4,   115,   116,   117,   118,     0,     0,
    56,   -29,    57,    58,    59,    60,    61,    62,    63,    64,
    65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
     0,     0,     0,     0,   438,   439,   440,   441,   442,   443,
   444,   445,   446,   447,   448,     0,     0,   302,   450,   451,
   452,   453,   454,   455,     0,     0,     0,     0,     0,     0,
   -46,   -46,    56,     0,    57,    58,    59,    60,    61,    62,
    63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
    73,    74,     0,    -4,    -4,     0,     0,    56,   -46,    57,
    58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
    68,    69,    70,    71,    72,    73,    74,    56,     0,    57,
    58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
    68,    69,    70,    71,    72,    73,    74,    56,     0,    57,
    58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
    68,    69,    70,    71,    72,    73,    74,    56,     0,    57,
    58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
    68,    69,    70,    71,    72,    73,    74,     0,     0,     0,
     0,     0,     0,     0,    56,    92,    57,    58,    59,    60,
    61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
    71,    72,    73,    74,    56,     0,    57,    58,    59,    60,
    61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
    71,    72,    73,    74,    62,    63,    64,    65,    66,    67,
    68,    69,    70,    71,    72,    73,    74,   144,     0,     0,
     0,     0,    27,    27,    27,    27,    27,    27,    27,    27,
    27,    27,    27,    27,     0,    27,    27,    27,    27,    27,
    27,    56,   145,    57,    58,    59,    60,    61,    62,    63,
    64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
    74,    56,   146,    57,    58,    59,    60,    61,    62,    63,
    64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
    74,    56,   148,    57,    58,    59,    60,    61,    62,    63,
    64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
    74,    56,   149,    57,    58,    59,    60,    61,    62,    63,
    64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
    74,     0,     0,     0,     0,     0,     0,     0,    56,   150,
    57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
    67,    68,    69,    70,    71,    72,    73,    74,    56,   151,
    57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
    67,    68,    69,    70,    71,    72,    73,    74,     0,     3,
     4,     0,     0,     0,     0,     5,     6,   319,     7,     8,
     0,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    18,    19,    20,    21,    22,     0,     0,   509,   510,     0,
     0,     0,   -66,     0,     0,     0,   153,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   359,     0,     0,
     0,     0,     0,     0,     0,     0,   154,   -66,   -66,   -66,
   -66,   -66,   -66,   -66,   -66,   -66,     0,     0,     0,     0,
     0,     0,  -129,     0,     0,     0,   156,     0,   320,   321,
   322,   323,   324,   325,   326,   327,   328,   329,   330,   331,
   332,   333,   334,   335,   336,     0,   249,  -129,  -129,  -129,
  -129,  -129,  -129,  -129,  -129,  -129,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     3,     4,     0,     0,   344,
     0,     5,     6,   250,     7,     8,     0,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,     0,     0,   251,  -108,   -66,   -66,     0,    23,   360,
   361,   362,   363,   364,   365,   366,   367,   368,   369,   370,
   371,   372,   373,   374,   375,   376,   377,     0,   337,  -108,
  -108,  -108,  -108,  -108,  -108,  -108,  -108,  -108,   339,     0,
     0,     0,     0,     0,     0,  -129,  -129,    60,    61,    62,
    63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
    73,    74,     0,   -92,     0,     0,     0,     0,   378,   345,
   346,   347,   348,   349,   350,   351,   352,   353,   354,   355,
   356,     0,     0,     0,     0,     0,     0,     0,   -92,   -92,
   -92,   -92,   -92,   -92,   -92,   -92,   -92,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   180,     0,
     0,     0,     0,     0,     0,     0,     0,  -108,  -108,     0,
     0,     0,     0,     0,    31,     0,   340,   341,     0,     3,
     4,   306,     0,     0,     0,     5,     6,     0,     7,     8,
   357,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    18,    19,    20,    21,    22,     0,  -161,   504,  -173,  -173,
  -173,  -173,  -173,  -173,  -173,  -173,  -173,     0,   181,     0,
     0,     0,     0,     0,     0,     0,   -92,   -92,     0,     0,
     0,  -161,  -161,  -161,  -161,  -161,  -161,  -161,  -161,  -161,
   182,   183,     0,     0,     0,     0,     0,     0,     0,   342,
    56,   179,    57,    58,    59,    60,    61,    62,    63,    64,
    65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
     0,     0,     0,     0,     0,     0,     0,     0,   184,   185,
   186,   187,   188,   189,   190,   191,   192,   193,   194,   195,
   196,     0,   197,   198,     0,     0,  -173,  -173,    61,    62,
    63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
    73,    74,     0,     0,     0,     0,     0,   307,   308,  -161,
  -161,     0,     0,     0,     0,     0,     0,     0,    23,   220,
   221,   222,   223,   224,   225,   226,   227,   228,   229,   230,
   231,   232,   233,   234,   235,   236,   237,   238,   239,    56,
     0,    57,    58,    59,    60,    61,    62,    63,    64,    65,
    66,    67,    68,    69,    70,    71,    72,    73,    74
};

static const short yycheck[] = {    91,
    18,    26,    26,    48,    27,    27,     1,   405,    27,   407,
    64,    65,   167,    64,   167,    27,   171,   434,   171,    50,
     3,   167,     5,     6,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    51,    52,    53,    54,    55,    56,    57,    58,    59,    25,
   167,     3,   198,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,   516,   126,   490,    20,    21,    22,   167,    54,    18,
    19,    20,    21,    22,   167,   530,   167,   170,   171,   487,
   182,   183,   184,   185,   186,   451,   452,   189,   190,   191,
   154,   155,   194,   167,   196,   454,   455,   161,   162,   163,
   164,   165,   204,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,   167,   170,   171,   167,   139,   140,
   167,   167,   224,   225,   226,   227,   228,   229,   230,   231,
   232,   233,   234,    53,   167,   167,   171,   171,   167,   167,
   242,   170,   171,   171,   167,   167,   171,   303,   170,   171,
   167,    18,    19,   167,   167,   167,   167,    24,    25,    27,
    27,    28,   264,    30,    31,    32,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,    26,   171,    27,
    44,     0,     3,   168,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    16,    17,    18,    19,    20,    21,    22,   171,
   312,   313,   314,    26,    26,   317,    26,    26,   320,   321,
   322,   323,   324,   325,   326,   327,   328,   329,   330,   331,
   332,   333,   334,   335,   336,    26,   166,    18,    19,    27,
   169,   169,   398,    24,    25,    49,    27,    28,   404,    30,
    31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
    41,    42,    43,   167,   366,   367,   368,   369,   370,   371,
   372,   373,   374,   375,   376,   377,     1,   433,     3,     4,
     5,     6,    27,    48,   386,   387,   388,   389,   390,    29,
    29,   393,    29,    29,    26,    26,    26,    26,    23,    26,
   167,   403,   168,   168,   171,   167,    31,    26,   171,    26,
    26,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    45,    46,    47,    48,    45,     1,   167,    26,    26,    26,
   169,    26,    26,    26,    59,    60,    61,    62,    63,    64,
    65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
   171,    27,     6,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    26,
    95,    96,    97,    26,   169,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    48,   171,   167,   489,    64,    65,
    66,    67,    68,    69,    26,   168,   168,    26,   168,   167,
   171,   168,     1,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,   521,
   168,   168,   147,     0,   526,     1,     0,   152,    27,   308,
   155,   533,   437,   438,   439,   440,   441,   442,   443,   444,
   445,   446,   447,   448,    -1,   535,   548,    -1,   453,   454,
   455,    -1,    51,    52,    53,    54,    55,    56,    57,    58,
    59,    -1,    61,   139,   140,    -1,    -1,    -1,    -1,    -1,
    -1,    70,    71,    -1,    -1,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
     3,   167,     5,     6,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    -1,    -1,    -1,    -1,   346,   347,   348,   349,   350,   351,
   352,   353,   354,   355,   356,    -1,    -1,   252,   360,   361,
   362,   363,   364,   365,    -1,    -1,    -1,    -1,    -1,    -1,
   139,   140,     3,    -1,     5,     6,     7,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    -1,   139,   140,    -1,    -1,     3,   167,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,     3,    -1,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,     3,    -1,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,     3,    -1,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,     3,   168,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,     3,    -1,     5,     6,     7,     8,
     9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    19,    20,    21,    22,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,   168,    -1,    -1,
    -1,    -1,   437,   438,   439,   440,   441,   442,   443,   444,
   445,   446,   447,   448,    -1,   450,   451,   452,   453,   454,
   455,     3,   168,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,     3,   168,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,     3,   168,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,     3,   168,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,   168,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,     3,   168,
     5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    -1,    18,
    19,    -1,    -1,    -1,    -1,    24,    25,     1,    27,    28,
    -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
    39,    40,    41,    42,    43,    -1,    -1,    46,    47,    -1,
    -1,    -1,    26,    -1,    -1,    -1,   168,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   168,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,
    -1,    -1,    26,    -1,    -1,    -1,   168,    -1,    72,    73,
    74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
    84,    85,    86,    87,    88,    -1,   168,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    18,    19,    -1,    -1,     1,
    -1,    24,    25,   168,    27,    28,    -1,    30,    31,    32,
    33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
    43,    -1,    -1,   168,    26,   139,   140,    -1,   167,   103,
   104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
   114,   115,   116,   117,   118,   119,   120,    -1,   162,    51,
    52,    53,    54,    55,    56,    57,    58,    59,     1,    -1,
    -1,    -1,    -1,    -1,    -1,   139,   140,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    -1,    26,    -1,    -1,    -1,    -1,   162,    91,
    92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
    53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,   140,    -1,
    -1,    -1,    -1,    -1,   167,    -1,    89,    90,    -1,    18,
    19,     1,    -1,    -1,    -1,    24,    25,    -1,    27,    28,
   162,    30,    31,    32,    33,    34,    35,    36,    37,    38,
    39,    40,    41,    42,    43,    -1,    26,    46,    51,    52,
    53,    54,    55,    56,    57,    58,    59,    -1,    61,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   139,   140,    -1,    -1,
    -1,    51,    52,    53,    54,    55,    56,    57,    58,    59,
    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,
     3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,
   123,   124,   125,   126,   127,   128,   129,   130,   131,   132,
   133,    -1,   135,   136,    -1,    -1,   139,   140,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    -1,    -1,    -1,    -1,    -1,   137,   138,   139,
   140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   141,
   142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
   152,   153,   154,   155,   156,   157,   158,   159,   160,     3,
    -1,     5,     6,     7,     8,     9,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22
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

case 3:
#line 199 "parser.y"
{
		last_x = 0;
		last_y = 0;
		last_z = 0;
		last_material = NULL;
		last_constraint = NULL;
	    ;
    break;}
case 8:
#line 225 "parser.y"
{
		Deallocate (problem.title);
		problem.title = yyvsp[0].s;
	    ;
    break;}
case 9:
#line 231 "parser.y"
{
		problem.num_nodes = yyvsp[0].i;
	    ;
    break;}
case 10:
#line 236 "parser.y"
{
		problem.num_elements = yyvsp[0].i;
	    ;
    break;}
case 11:
#line 241 "parser.y"
{
		problem.mode = yyvsp[0].i;
	    ;
    break;}
case 30:
#line 292 "parser.y"
{
		if (yyvsp[0].i < 1 || yyvsp[0].i > problem.num_nodes) {
		    error ("node number %u is illegal", yyvsp[0].i);
		    node = &dummy_node;
		    break;
		}

		node = CreateNode (yyvsp[0].i);
		found = TreeInsert (problem.node_tree, node);

		if (found != (Item) node) {
		    error ("node number %u is repeated", yyvsp[0].i);
		    DestroyNode (node);
		    node = &dummy_node;
		    break;
		}

		node -> x = last_x;
		node -> y = last_y;
		node -> z = last_z;
		node -> constraint = last_constraint ?
			(Constraint) strdup (last_constraint) : NULL;
	    ;
    break;}
case 32:
#line 321 "parser.y"
{yyval.i = yyvsp[-1].d;;
    break;}
case 35:
#line 333 "parser.y"
{
		node -> x = last_x = yyvsp[0].d;
	    ;
    break;}
case 36:
#line 338 "parser.y"
{
		node -> y = last_y = yyvsp[0].d;
	    ;
    break;}
case 37:
#line 343 "parser.y"
{
		node -> z = last_z = yyvsp[0].d;
	    ;
    break;}
case 38:
#line 348 "parser.y"
{
                node -> m = yyvsp[0].d;
            ;
    break;}
case 39:
#line 353 "parser.y"
{
		Deallocate (node -> force);
		node -> force = (Force) yyvsp[0].s;
	    ;
    break;}
case 40:
#line 359 "parser.y"
{
		node -> constraint = (Constraint) (last_constraint = yyvsp[0].s);
	    ;
    break;}
case 43:
#line 376 "parser.y"
{
		definition = defnlookup (yyvsp[0].s);
		Deallocate (yyvsp[0].s);
		if (!definition)
		    return 1;
	    ;
    break;}
case 47:
#line 398 "parser.y"
{
		if (yyvsp[0].i < 1 || yyvsp[0].i > problem.num_elements) {
		    error ("element number %u is illegal", yyvsp[0].i);
		    element = &dummy_element;
		    break;
		}

		element = CreateElement (yyvsp[0].i, definition);
		found = TreeInsert (problem.element_tree, element);

		if (found != (Item) element) {
		    error ("element number %u is repeated", yyvsp[0].i);
		    DestroyElement (element);
		    element = &dummy_element;
		    break;
		}

		element -> material = last_material ?
			(Material) strdup (last_material) : NULL;
	    ;
    break;}
case 49:
#line 424 "parser.y"
{yyval.i = yyvsp[-1].d;;
    break;}
case 52:
#line 436 "parser.y"
{
		unsigned i;
		unsigned size;
		unsigned number;


		if (element == &dummy_element)
		    break;

		size = int_ptr - int_array;

		if (size != element -> definition -> numnodes) {
		    number = element -> number;
		    error ("incorrect number of nodes for element %u", number);
		    break;
		}

		for (i = 1; i <= size; i ++)
		    element -> node [i] = (Node) int_array [i - 1];
	    ;
    break;}
case 53:
#line 458 "parser.y"
{
		element -> material = (Material) (last_material = yyvsp[0].s);
	    ;
    break;}
case 56:
#line 470 "parser.y"
{
		*int_ptr ++ = yyvsp[0].i;
	    ;
    break;}
case 57:
#line 475 "parser.y"
{
		*int_ptr ++ = yyvsp[0].i;
	    ;
    break;}
case 58:
#line 480 "parser.y"
{
		int_ptr = int_array;
		*int_ptr ++ = yyvsp[0].i;
	    ;
    break;}
case 59:
#line 489 "parser.y"
{
		if (yyvsp[0].i > problem.num_nodes)
		    error ("node number %u is illegal", yyvsp[0].i);
	    ;
    break;}
case 60:
#line 498 "parser.y"
{
		if (element -> numdistributed == 3) {
		    error ("element %u has too many loads", element -> number);
		    break;
		}

		element -> numdistributed ++;
		Deallocate (element -> distributed [element -> numdistributed]);
		element -> distributed [element -> numdistributed] =
		  (Distributed) yyvsp[0].s;
	    ;
    break;}
case 61:
#line 511 "parser.y"
{
		if (element -> numdistributed == 3) {
		    error ("element %u has too many loads", element -> number);
		    break;
		}

		element -> numdistributed ++;
		Deallocate (element -> distributed [element -> numdistributed]);
		element -> distributed [element -> numdistributed] =
		  (Distributed) yyvsp[0].s;
	    ;
    break;}
case 62:
#line 524 "parser.y"
{
		element -> numdistributed = 1;
		Deallocate (element -> distributed [element -> numdistributed]);
		element -> distributed [element -> numdistributed] =
		  (Distributed) yyvsp[0].s;
	    ;
    break;}
case 67:
#line 553 "parser.y"
{
		material = CreateMaterial (yyvsp[0].s);
		found = TreeInsert (problem.material_tree, material);

		if (found != (Item) material) {
		    error ("material %s is previously defined", yyvsp[0].s);
		    DestroyMaterial (material);
		    material = &dummy_material;
		}
	    ;
    break;}
case 70:
#line 573 "parser.y"
{
		Deallocate (material -> color);
                material -> color = yyvsp[0].s;
	    ;
    break;}
case 71:
#line 579 "parser.y"
{
		material -> E = yyvsp[0].d;
	    ;
    break;}
case 72:
#line 584 "parser.y"
{
		material -> Ix = yyvsp[0].d;
	    ;
    break;}
case 73:
#line 589 "parser.y"
{
		material -> Iy = yyvsp[0].d;
	    ;
    break;}
case 74:
#line 594 "parser.y"
{
		material -> Iz = yyvsp[0].d;
	    ;
    break;}
case 75:
#line 599 "parser.y"
{
		material -> A = yyvsp[0].d;
	    ;
    break;}
case 76:
#line 604 "parser.y"
{
		material -> J = yyvsp[0].d;
	    ;
    break;}
case 77:
#line 609 "parser.y"
{
		material -> G = yyvsp[0].d;
	    ;
    break;}
case 78:
#line 614 "parser.y"
{
		material -> t = yyvsp[0].d;
	    ;
    break;}
case 79:
#line 619 "parser.y"
{
		material -> rho = yyvsp[0].d;
	    ;
    break;}
case 80:
#line 624 "parser.y"
{
		material -> nu = yyvsp[0].d;
	    ;
    break;}
case 81:
#line 629 "parser.y"
{
		material -> kappa = yyvsp[0].d;
	    ;
    break;}
case 82:
#line 634 "parser.y"
{
		material -> Rk = yyvsp[0].d;
	    ;
    break;}
case 83:
#line 639 "parser.y"
{
		material -> Rm = yyvsp[0].d;
	    ;
    break;}
case 84:
#line 644 "parser.y"
{
                material -> Kx = yyvsp[0].d;
            ;
    break;}
case 85:
#line 649 "parser.y"
{
                material -> Ky = yyvsp[0].d;
            ;
    break;}
case 86:
#line 654 "parser.y"
{
                material -> Kz = yyvsp[0].d;
            ;
    break;}
case 87:
#line 659 "parser.y"
{
                material -> c = yyvsp[0].d;
            ;
    break;}
case 93:
#line 687 "parser.y"
{
		load = CreateDistributed (yyvsp[0].s, 0);
		found = TreeInsert (problem.distributed_tree, load);

		if (found != (Item) load) {
		    error ("load %s is previously defined", yyvsp[0].s);
		    DestroyDistributed (load);
		    load = &dummy_load;
		}
	    ;
    break;}
case 96:
#line 708 "parser.y"
{
		Deallocate (load -> color);
                load -> color = yyvsp[0].s;
	    ;
    break;}
case 97:
#line 714 "parser.y"
{
		load -> direction = yyvsp[0].i;
	    ;
    break;}
case 98:
#line 719 "parser.y"
{
		unsigned i;
		unsigned size;


		if (load == &dummy_load)
		    break;

		size = pair_ptr - pair_array;

		if (!(load -> value = Allocate (Pair, size)))
		    Fatal ("unable to allocate memory for pairs");

		UnitOffset (load -> value);
		load -> nvalues = size;

		for (i = 1; i <= size; i ++)
		    load -> value [i] = pair_array [i - 1];
	    ;
    break;}
case 100:
#line 745 "parser.y"
{
		*pair_ptr ++ = yyvsp[0].p;
	    ;
    break;}
case 101:
#line 750 "parser.y"
{
		*pair_ptr ++ = yyvsp[0].p;
	    ;
    break;}
case 102:
#line 755 "parser.y"
{
		pair_ptr = pair_array;
		*pair_ptr ++ = yyvsp[0].p;
	    ;
    break;}
case 103:
#line 764 "parser.y"
{
		if (yyvsp[-3].i < 1 || yyvsp[-3].i > problem.num_nodes)
		    error ("node number %u is illegal", yyvsp[-3].i);

		yyval.p.node = yyvsp[-3].i;
		yyval.p.magnitude = yyvsp[-1].d;
	    ;
    break;}
case 104:
#line 773 "parser.y"
{
		if (yyvsp[-2].i < 1 || yyvsp[-2].i > problem.num_nodes)
		    error ("node number %u is illegal", yyvsp[-2].i);

		yyval.p.node = yyvsp[-2].i;
		yyval.p.magnitude = yyvsp[-1].d;
	    ;
    break;}
case 109:
#line 803 "parser.y"
{
		force = CreateForce (yyvsp[0].s);
		found = TreeInsert (problem.force_tree, force);

		if (found != (Item) force) {
		    error ("force %s is previously defined", yyvsp[0].s);
		    DestroyForce (force);
		    force = &dummy_force;
		}
	    ;
    break;}
case 112:
#line 824 "parser.y"
{
		Deallocate (force -> color);
                force -> color = yyvsp[0].s;
	    ;
    break;}
case 113:
#line 830 "parser.y"
{
		AssignForce (force, Fx, InCore, copy_input (0));
	    ;
    break;}
case 114:
#line 835 "parser.y"
{
		AssignForce (force, Fy, InCore, copy_input (0));
	    ;
    break;}
case 115:
#line 840 "parser.y"
{
		AssignForce (force, Fz, InCore, copy_input (0));
	    ;
    break;}
case 116:
#line 845 "parser.y"
{
		AssignForce (force, Mx, InCore, copy_input (0));
	    ;
    break;}
case 117:
#line 850 "parser.y"
{
		AssignForce (force, My, InCore, copy_input (0));
	    ;
    break;}
case 118:
#line 855 "parser.y"
{
		AssignForce (force, Mz, InCore, copy_input (0));
	    ;
    break;}
case 119:
#line 860 "parser.y"
{
		AssignSpectrum (force, Fx, InCore, copy_input (0));
	    ;
    break;}
case 120:
#line 865 "parser.y"
{
		AssignSpectrum (force, Fy, InCore, copy_input (0));
	    ;
    break;}
case 121:
#line 870 "parser.y"
{
		AssignSpectrum (force, Fz, InCore, copy_input (0));
	    ;
    break;}
case 122:
#line 875 "parser.y"
{
		AssignSpectrum (force, Mx, InCore, copy_input (0));
	    ;
    break;}
case 123:
#line 880 "parser.y"
{
		AssignSpectrum (force, My, InCore, copy_input (0));
	    ;
    break;}
case 124:
#line 885 "parser.y"
{
		AssignSpectrum (force, Mz, InCore, copy_input (0));
	    ;
    break;}
case 130:
#line 913 "parser.y"
{
		constraint = CreateConstraint (yyvsp[0].s);
		found = TreeInsert (problem.constraint_tree, constraint);

		if (found != (Item) constraint) {
		    error ("constraint %s is previously defined", yyvsp[0].s);
		    DestroyConstraint (constraint);
		    constraint = &dummy_constraint;
		}
	    ;
    break;}
case 133:
#line 934 "parser.y"
{
		Deallocate (constraint -> color);
                constraint -> color = yyvsp[0].s;
	    ;
    break;}
case 134:
#line 940 "parser.y"
{
                if (yyvsp[0].c == VariableExpression)
                   AssignConstraint (constraint, Tx, InCore, copy_input(0), 1);
                else  {
                   AssignConstraint (constraint, Tx, NULL, NULL, yyvsp[0].c);
		   copy_input (0);
		}
	    ;
    break;}
case 135:
#line 950 "parser.y"
{
                if (yyvsp[0].c == VariableExpression)
                   AssignConstraint (constraint, Ty, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Ty, NULL, NULL, yyvsp[0].c);
		   copy_input (0);
		}
	    ;
    break;}
case 136:
#line 960 "parser.y"
{
                if (yyvsp[0].c == VariableExpression)
                   AssignConstraint (constraint, Tz, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Tz, NULL, NULL, yyvsp[0].c);
		   copy_input (0);
		}
	    ;
    break;}
case 137:
#line 970 "parser.y"
{
                if (yyvsp[0].c == VariableExpression)
                   AssignConstraint (constraint, Rx, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Rx, NULL, NULL, yyvsp[0].c);
		   copy_input (0);
		}
	    ;
    break;}
case 138:
#line 980 "parser.y"
{
                if (yyvsp[0].c == VariableExpression)
                   AssignConstraint (constraint, Ry, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Ry, NULL, NULL, yyvsp[0].c);
		   copy_input (0);
		}
	    ;
    break;}
case 139:
#line 990 "parser.y"
{
                if (yyvsp[0].c == VariableExpression)
                   AssignConstraint (constraint, Rz, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Rz, NULL, NULL, yyvsp[0].c);
		   copy_input (0);
		}
	    ;
    break;}
case 140:
#line 1000 "parser.y"
{
                constraint -> ix [Tx] = yyvsp[0].d;
            ;
    break;}
case 141:
#line 1005 "parser.y"
{
                constraint -> ix [Ty] = yyvsp[0].d;
            ;
    break;}
case 142:
#line 1010 "parser.y"
{
                constraint -> ix [Tz] = yyvsp[0].d;
            ;
    break;}
case 143:
#line 1015 "parser.y"
{
                constraint -> ix [Rx] = yyvsp[0].d;
            ;
    break;}
case 144:
#line 1020 "parser.y"
{
                constraint -> ix [Ry] = yyvsp[0].d;
            ;
    break;}
case 145:
#line 1025 "parser.y"
{
                constraint -> ix [Rz] = yyvsp[0].d;
            ;
    break;}
case 146:
#line 1030 "parser.y"
{
		constraint -> vx [Tx] = yyvsp[0].d;
	    ;
    break;}
case 147:
#line 1035 "parser.y"
{
		constraint -> vx [Ty] = yyvsp[0].d;
	    ;
    break;}
case 148:
#line 1040 "parser.y"
{
		constraint -> vx [Tz] = yyvsp[0].d;
	    ;
    break;}
case 149:
#line 1045 "parser.y"
{
		constraint -> ax [Tx] = yyvsp[0].d;
	    ;
    break;}
case 150:
#line 1050 "parser.y"
{
		constraint -> ax [Ty] = yyvsp[0].d;
	    ;
    break;}
case 151:
#line 1055 "parser.y"
{
		constraint -> ax [Tz] = yyvsp[0].d;
	    ;
    break;}
case 153:
#line 1065 "parser.y"
{
		yyval.c = VariableExpression;
	    ;
    break;}
case 154:
#line 1070 "parser.y"
{
		yyval.c = yyvsp[0].i;
	    ;
    break;}
case 155:
#line 1078 "parser.y"
{
		yyval.c = VariableExpression;
	    ;
    break;}
case 156:
#line 1083 "parser.y"
{
		yyval.c = yyvsp[0].i;
	    ;
    break;}
case 157:
#line 1088 "parser.y"
{
		yyval.c = yyvsp[0].i;
	    ;
    break;}
case 162:
#line 1112 "parser.y"
{
		loadcase = CreateLoadCase (yyvsp[0].s);
		found = TreeInsert (problem.loadcase_tree, loadcase);

		if (found != (Item) loadcase) {
		    error ("loadcase %s is previously defined", yyvsp[0].s);
		    DestroyLoadCase (loadcase);
		    loadcase = &dummy_loadcase;
		}
	    ;
    break;}
case 165:
#line 1131 "parser.y"
{
		unsigned i;
		unsigned size;


		if (loadcase == &dummy_loadcase)
		    break;

		size = case_ptr - case_array;

		if (!(loadcase -> nodes = Allocate (Node, size)) ||
                    !(loadcase -> forces = Allocate (Force, size)))
		    Fatal ("unable to allocate memory for node case pairs");

		UnitOffset (loadcase -> nodes);
		UnitOffset (loadcase -> forces);
		loadcase -> numforces = size;

		for (i = 1; i <= size; i ++) {
		    loadcase -> nodes [i] = (Node) case_array [i - 1].noe;
                    loadcase -> forces [i]   = (Force) case_array [i - 1].fol;
                }
	    ;
    break;}
case 166:
#line 1156 "parser.y"
{
		unsigned i;
		unsigned size;


		if (loadcase == &dummy_loadcase)
		    break;

		size = case_ptr - case_array;

		if (!(loadcase -> elements = Allocate (Element, size)) ||
                    !(loadcase -> loads = Allocate (Distributed, size)))
		    Fatal ("unable to allocate memory for element case pairs");

		UnitOffset (loadcase -> elements);
		UnitOffset (loadcase -> loads);
		loadcase -> numloads = size;

		for (i = 1; i <= size; i ++) {
		    loadcase -> elements [i] = (Element) case_array [i - 1].noe;
                    loadcase -> loads [i]   = (Distributed) case_array [i - 1].fol;
                }
	    ;
    break;}
case 168:
#line 1186 "parser.y"
{
		*case_ptr ++ = yyvsp[0].cp;
	    ;
    break;}
case 169:
#line 1191 "parser.y"
{
		*case_ptr ++ = yyvsp[0].cp;
	    ;
    break;}
case 170:
#line 1196 "parser.y"
{
		case_ptr = case_array;
		*case_ptr ++ = yyvsp[0].cp;
	    ;
    break;}
case 171:
#line 1205 "parser.y"
{
		if (yyvsp[-3].i < 1 || yyvsp[-3].i > problem.num_nodes)
		    error ("node number %u is illegal", yyvsp[-3].i);

		yyval.cp.noe = yyvsp[-3].i;
		yyval.cp.fol = yyvsp[-1].s;
	    ;
    break;}
case 172:
#line 1214 "parser.y"
{
		if (yyvsp[-2].i < 1 || yyvsp[-2].i > problem.num_nodes)
		    error ("node number %u is illegal", yyvsp[-2].i);

		yyval.cp.noe = yyvsp[-2].i;
		yyval.cp.fol = yyvsp[-1].s;
	    ;
    break;}
case 176:
#line 1238 "parser.y"
{
		analysis.alpha = yyvsp[0].d;
	    ;
    break;}
case 177:
#line 1243 "parser.y"
{
		analysis.beta = yyvsp[0].d;
	    ;
    break;}
case 178:
#line 1248 "parser.y"
{
		analysis.gamma = yyvsp[0].d;
	    ;
    break;}
case 179:
#line 1253 "parser.y"
{
                analysis.Rk = yyvsp[0].d;
            ;
    break;}
case 180:
#line 1258 "parser.y"
{
                analysis.Rm = yyvsp[0].d;
            ;
    break;}
case 181:
#line 1263 "parser.y"
{
		analysis.start = yyvsp[0].d;
	    ;
    break;}
case 182:
#line 1268 "parser.y"
{
		analysis.step = yyvsp[0].d;
	    ;
    break;}
case 183:
#line 1273 "parser.y"
{
		analysis.stop = yyvsp[0].d;
	    ;
    break;}
case 184:
#line 1278 "parser.y"
{
		analysis.iterations = yyvsp[0].i;
	    ;
    break;}
case 185:
#line 1283 "parser.y"
{
		analysis.load_steps = yyvsp[0].i;
	    ;
    break;}
case 186:
#line 1288 "parser.y"
{
		analysis.relaxation = yyvsp[0].d;
	    ;
    break;}
case 187:
#line 1293 "parser.y"
{
		analysis.tolerance = yyvsp[0].d;
	    ;
    break;}
case 188:
#line 1298 "parser.y"
{
                analysis.input_dof = yyvsp[0].i;
            ;
    break;}
case 189:
#line 1303 "parser.y"
{
                analysis.input_node = (Node) yyvsp[0].i;
            ;
    break;}
case 190:
#line 1308 "parser.y"
{
		unsigned i;


		analysis.numnodes = int_ptr - int_array;

		if (!(analysis.nodes = Allocate (Node, analysis.numnodes)))
		    Fatal ("unable to allocate memory for analysis nodes");

		UnitOffset (analysis.nodes);
		for (i = 1; i <= analysis.numnodes; i ++)
		    analysis.nodes [i] = (Node) int_array [i - 1];
	    ;
    break;}
case 191:
#line 1323 "parser.y"
{
		int i;


		analysis.numdofs = 0;
		for (i = 1; i <= 6; i ++)
		    if (analysis.dofs [i])
			analysis.dofs [++ analysis.numdofs] = i;
	    ;
    break;}
case 192:
#line 1334 "parser.y"
{
		analysis.mass_mode = yyvsp[0].i;
	    ;
    break;}
case 193:
#line 1339 "parser.y"
{
                analysis.gravity [1] = triple_x;
                analysis.gravity [2] = triple_y;
                analysis.gravity [3] = triple_z;
            ;
    break;}
case 195:
#line 1351 "parser.y"
{
		*int_ptr ++ = yyvsp[0].i;
	    ;
    break;}
case 196:
#line 1356 "parser.y"
{
		*int_ptr ++ = yyvsp[0].i;
	    ;
    break;}
case 197:
#line 1361 "parser.y"
{
		int_ptr = int_array;
	    ;
    break;}
case 198:
#line 1369 "parser.y"
{
		analysis.dofs [yyvsp[0].i] = 1;
	    ;
    break;}
case 199:
#line 1374 "parser.y"
{
		analysis.dofs [yyvsp[0].i] = 1;
	    ;
    break;}
case 200:
#line 1379 "parser.y"
{
		int i;


		for (i = 1; i <= 6; i ++)
		     analysis.dofs [i] = 0;
	    ;
    break;}
case 201:
#line 1391 "parser.y"
{
		triple_x = yyvsp[-4].d;
		triple_y = yyvsp[-2].d;
		triple_z = yyvsp[-1].d;
	    ;
    break;}
case 202:
#line 1401 "parser.y"
{
		yyval.d = yyvsp[0].d;
	    ;
    break;}
case 203:
#line 1406 "parser.y"
{
                yyval.d = 0.0;
            ;
    break;}
case 207:
#line 1427 "parser.y"
{
		appearance.node_numbers = yyvsp[0].i;
	    ;
    break;}
case 208:
#line 1432 "parser.y"
{
		appearance.element_numbers = yyvsp[0].i;
	    ;
    break;}
case 209:
#line 1437 "parser.y"
{
		appearance.snap = yyvsp[0].i;
	    ;
    break;}
case 210:
#line 1442 "parser.y"
{
		appearance.grid = yyvsp[0].i;
	    ;
    break;}
case 211:
#line 1447 "parser.y"
{
		appearance.snap_size = yyvsp[0].d;
	    ;
    break;}
case 212:
#line 1452 "parser.y"
{
		appearance.grid_size = yyvsp[0].d;
	    ;
    break;}
case 213:
#line 1457 "parser.y"
{
		appearance.x_min = yyvsp[0].d;
	    ;
    break;}
case 214:
#line 1462 "parser.y"
{
		appearance.x_max = yyvsp[0].d;
	    ;
    break;}
case 215:
#line 1467 "parser.y"
{
		appearance.y_min = yyvsp[0].d;
	    ;
    break;}
case 216:
#line 1472 "parser.y"
{
		appearance.y_max = yyvsp[0].d;
	    ;
    break;}
case 217:
#line 1477 "parser.y"
{
		appearance.x_pos = yyvsp[0].d;
	    ;
    break;}
case 218:
#line 1482 "parser.y"
{
		appearance.y_pos = yyvsp[0].d;
	    ;
    break;}
case 219:
#line 1487 "parser.y"
{
		appearance.width = yyvsp[0].d;
	    ;
    break;}
case 220:
#line 1492 "parser.y"
{
		appearance.height = yyvsp[0].d;
	    ;
    break;}
case 221:
#line 1497 "parser.y"
{
		appearance.scale = yyvsp[0].d;
	    ;
    break;}
case 222:
#line 1502 "parser.y"
{
		Deallocate (appearance.node_color);
		appearance.node_color = yyvsp[0].s;
	    ;
    break;}
case 223:
#line 1508 "parser.y"
{
		Deallocate (appearance.element_color);
		appearance.element_color = yyvsp[0].s;
	    ;
    break;}
case 224:
#line 1514 "parser.y"
{
		Deallocate (appearance.label_font);
		appearance.label_font = yyvsp[0].s;
	    ;
    break;}
case 225:
#line 1520 "parser.y"
{
		Deallocate (appearance.tool_color);
		appearance.tool_color = yyvsp[0].s;
	    ;
    break;}
case 226:
#line 1526 "parser.y"
{
		Deallocate (appearance.tool_font);
		appearance.tool_font = yyvsp[0].s;
	    ;
    break;}
case 228:
#line 1542 "parser.y"
{
		figure_size = 0;
	    ;
    break;}
case 232:
#line 1561 "parser.y"
{
		if (appearance.num_figures == figure_size) {
		    figure_size = figure_size ? figure_size <<= 1 : 4;
		    if (!Reallocate (appearance.figures, FigInfo, figure_size))
			Fatal ("unable to allocate figure list");
		}

		figure = &appearance.figures [appearance.num_figures ++];
		figure -> type = yyvsp[0].i;
		figure -> x = 0;
		figure -> y = 0;
		figure -> width = 0;
		figure -> height = 0;
		figure -> start = 0;
		figure -> length = 0;
		figure -> num_points = 0;
		figure -> points = NULL;
		figure -> font = NULL;
		figure -> text = NULL;
		figure -> color = NULL;
	    ;
    break;}
case 235:
#line 1593 "parser.y"
{
		figure -> x = yyvsp[0].d;
	    ;
    break;}
case 236:
#line 1598 "parser.y"
{
		figure -> y = yyvsp[0].d;
	    ;
    break;}
case 237:
#line 1603 "parser.y"
{
		figure -> width = yyvsp[0].d;
	    ;
    break;}
case 238:
#line 1608 "parser.y"
{
		figure -> height = yyvsp[0].d;
	    ;
    break;}
case 239:
#line 1613 "parser.y"
{
		figure -> start = yyvsp[0].d;
	    ;
    break;}
case 240:
#line 1618 "parser.y"
{
		figure -> length = yyvsp[0].d;
	    ;
    break;}
case 241:
#line 1623 "parser.y"
{
		Deallocate (figure -> text);
		figure -> text = yyvsp[0].s;
	    ;
    break;}
case 242:
#line 1629 "parser.y"
{
		Deallocate (figure -> color);
		figure -> color = yyvsp[0].s;
	    ;
    break;}
case 243:
#line 1635 "parser.y"
{
		Deallocate (figure -> font);
		figure -> font = yyvsp[0].s;
	    ;
    break;}
case 245:
#line 1646 "parser.y"
{
		if (fig_point_size == figure -> num_points) {
		    fig_point_size <<= 1;
		    Reallocate (figure -> points, FigInfoPair, fig_point_size);
		    if (figure -> points == NULL)
			Fatal ("unable to allocate figure points");
		}

		figure -> points [figure -> num_points].x = figure_x;
		figure -> points [figure -> num_points ++].y = figure_y;
	    ;
    break;}
case 246:
#line 1659 "parser.y"
{
		if (fig_point_size == figure -> num_points) {
		    fig_point_size <<= 1;
		    Reallocate (figure -> points, FigInfoPair, fig_point_size);
		    if (figure -> points == NULL)
			Fatal ("unable to allocate figure points");
		}

		figure -> points [figure -> num_points].x = figure_x;
		figure -> points [figure -> num_points ++].y = figure_y;
	    ;
    break;}
case 247:
#line 1672 "parser.y"
{
		figure -> points = Allocate (FigInfoPair, 2);
		if (figure -> points == NULL)
		    Fatal ("unable to allocate figure points");

		fig_point_size = 2;
		figure -> num_points = 0;
	    ;
    break;}
case 248:
#line 1685 "parser.y"
{
		figure_x = yyvsp[-3].d;
		figure_y = yyvsp[-1].d;
	    ;
    break;}
case 249:
#line 1696 "parser.y"
{
		EmitCode (HaltOp);
		SetIP (0);
	    ;
    break;}
case 250:
#line 1702 "parser.y"
{
		if (table_error)
		    EmitCode (PushOp, 0.0);
		else
		    EmitCode (TableOp, table, table_count);

		EmitCode (HaltOp);
		table_count = 0;
		table_error = 0;
		last_time = 0;
		SetIP (0);
	    ;
    break;}
case 251:
#line 1716 "parser.y"
{
		if (table_error)
		    EmitCode (PushOp, 0.0);
		else
		    EmitCode (CycleOp, table, table_count);

		EmitCode (HaltOp);
		table_count = 0;
		table_error = 0;
		last_time = 0;
		SetIP (0);
	    ;
    break;}
case 255:
#line 1740 "parser.y"
{
		if (yyvsp[-3].d < last_time) {
		    error ("point not in nondecreasing order");
		    table_error = 1;
		    break;
		}

		if (table_count == table_size) {
		    table_size = table_size ? table_size << 1 : 8;
		    if (!Reallocate (table, double, table_size))
			Fatal ("unable to expand table");
		}

		table [table_count ++] = last_time = yyvsp[-3].d;
		table [table_count ++] = yyvsp[-1].d;
	    ;
    break;}
case 256:
#line 1761 "parser.y"
{
		copy_input (1);
	    ;
    break;}
case 257:
#line 1769 "parser.y"
{
		EmitCode (HaltOp);
		SetIP (0);
		yyval.d = EvalCode (InCore, 0.0);
	    ;
    break;}
case 258:
#line 1779 "parser.y"
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
case 259:
#line 1790 "parser.y"
{
		int ip = GetIP ( );
		SetIP (ip - yyvsp[0].i - 3);
		EmitCode (JnzOp, yyvsp[0].i + 1);
		SetIP (ip);
		EmitCode (TestOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + yyvsp[0].i + 1;
	    ;
    break;}
case 260:
#line 1800 "parser.y"
{
		int ip = GetIP ( );
		SetIP (ip - yyvsp[0].i - 3);
		EmitCode (JzOp, yyvsp[0].i + 1);
		SetIP (ip);
		EmitCode (TestOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + yyvsp[0].i + 1;
	    ;
    break;}
case 261:
#line 1810 "parser.y"
{
		EmitCode (OrOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 262:
#line 1816 "parser.y"
{
		EmitCode (XorOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 263:
#line 1822 "parser.y"
{
		EmitCode (AndOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 264:
#line 1828 "parser.y"
{
		EmitCode (EqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 265:
#line 1834 "parser.y"
{
		EmitCode (NeqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 266:
#line 1840 "parser.y"
{
		EmitCode (LtOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 267:
#line 1846 "parser.y"
{
		EmitCode (GtOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 268:
#line 1852 "parser.y"
{
		EmitCode (LteqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 269:
#line 1858 "parser.y"
{
		EmitCode (GteqOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 270:
#line 1864 "parser.y"
{
		EmitCode (LsftOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 271:
#line 1870 "parser.y"
{
		EmitCode (RsftOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 272:
#line 1876 "parser.y"
{
		EmitCode (AddOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 273:
#line 1882 "parser.y"
{
		EmitCode (SubOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 274:
#line 1888 "parser.y"
{
		EmitCode (MulOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 275:
#line 1894 "parser.y"
{
		EmitCode (DivOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 276:
#line 1900 "parser.y"
{
		EmitCode (ModOp);
		yyval.i = yyvsp[-2].i + 1 + yyvsp[0].i;
	    ;
    break;}
case 277:
#line 1906 "parser.y"
{
		yyval.i = yyvsp[0].i;
	    ;
    break;}
case 278:
#line 1911 "parser.y"
{
		EmitCode (NegOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 279:
#line 1917 "parser.y"
{
		EmitCode (NotOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 280:
#line 1923 "parser.y"
{
		EmitCode (InvOp);
		yyval.i = 1 + yyvsp[0].i;
	    ;
    break;}
case 281:
#line 1929 "parser.y"
{
		yyval.i = yyvsp[-1].i;
	    ;
    break;}
case 282:
#line 1934 "parser.y"
{
		EmitCode (PushOp, (double) yyvsp[0].i);
		yyval.i = 2;
	    ;
    break;}
case 283:
#line 1940 "parser.y"
{
		EmitCode (PushOp, yyvsp[0].d);
		yyval.i = 2;
	    ;
    break;}
case 284:
#line 1946 "parser.y"
{
		EmitCode (TimeOp);
		yyval.i = 1;
	    ;
    break;}
case 286:
#line 1957 "parser.y"
{
		EmitCode (SinOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 287:
#line 1963 "parser.y"
{
		EmitCode (CosOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 288:
#line 1969 "parser.y"
{
		EmitCode (TanOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 289:
#line 1975 "parser.y"
{
		EmitCode (PowOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 290:
#line 1981 "parser.y"
{
		EmitCode (ExpOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 291:
#line 1987 "parser.y"
{
		EmitCode (LnOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 292:
#line 1993 "parser.y"
{
		EmitCode (LogOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 293:
#line 1999 "parser.y"
{
		EmitCode (SqrtOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 294:
#line 2005 "parser.y"
{
		EmitCode (HypotOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 295:
#line 2011 "parser.y"
{
		EmitCode (FloorOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 296:
#line 2017 "parser.y"
{
		EmitCode (CeilOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 297:
#line 2023 "parser.y"
{
		EmitCode (FmodOp);
		yyval.i = yyvsp[-3].i + yyvsp[-1].i + 1;
	    ;
    break;}
case 298:
#line 2029 "parser.y"
{
		EmitCode (FabsOp);
		yyval.i = yyvsp[-1].i + 1;
	    ;
    break;}
case 299:
#line 2038 "parser.y"
{
		EmitCode (JzOp, 0);
		yyval.i = 2;
	    ;
    break;}
case 300:
#line 2047 "parser.y"
{
		EmitCode (JmpOp, 0);
		yyval.i = 2;
	    ;
    break;}
case 301:
#line 2056 "parser.y"
{
		EmitCode (CopyOp);
		EmitCode (JnzOp, 0);
		EmitCode (PopOp);
		yyval.i = 4;
	    ;
    break;}
case 302:
#line 2067 "parser.y"
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
#line 2075 "parser.y"


# ifdef YYBYACC
char *felt_suppress_warnings_from_gcc = yysccsid;
# endif
