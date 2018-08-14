/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         felt_yyparse
#define yylex           felt_yylex
#define yyerror         felt_yyerror
#define yydebug         felt_yydebug
#define yynerrs         felt_yynerrs

#define yylval          felt_yylval
#define yychar          felt_yychar

/* Copy the first part of user declarations.  */
#line 1 "parser.y" /* yacc.c:339  */

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
 * Description:	This file contains the yacc specification for the	*
 *		parser for the FElt 2.0 system.				*
 ************************************************************************/

# include <stdio.h>
# include <string.h>
# include "code.h"
# include "error.h"
# include "objects.h"
# include "problem.h"
# include "fe.h"
# include "allocate.h"

# if !defined (__GNUC__) && !defined (__sparc__)
# define alloca malloc		/* prevents alloca from being called */
# endif

# define VariableExpression 2	/* not 0, 1, or, 'h' */

extern "C" void yyerror (const char *msg);
extern "C" int  yylex  (void);

/* Last parameters (default for some parameters is to inherit the last). */

static double		  last_x;		/* last x coordinate	   */
static double		  last_y;		/* last y coordinate	   */
static double		  last_z;		/* last z coordinate	   */
static std::string last_constraint;	/* name of last constraint */
static std::string last_material;	/* name of last material   */



/* Current objects (inherited attributes). */

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

static const Node dummy_node(new node_t);		/* dummy node		   */
static const Element dummy_element(new element_t);	/* dummy element	   */
static const Material dummy_material(new material_t);	/* dummy material	   */
static const Distributed dummy_load(new distributed_t);		/* dummy distributed load  */
static const Force dummy_force(new force_t);		/* dummy force		   */
static const Constraint dummy_constraint(new constraint_t);	/* dummy constraint	   */
static const LoadCase dummy_loadcase(new loadcase_t);	/* dummy loadcase	   */


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
static unsigned		  fig_point_size;	/* size of point list	   */
static FigInfo		 *figure;		/* current figure	   */

#line 187 "parser.cpp" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "parser.hpp".  */
#ifndef YY_FELT_YY_PARSER_HPP_INCLUDED
# define YY_FELT_YY_PARSER_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int felt_yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    OR = 258,
    AND = 259,
    EQUALS = 260,
    NEQUAL = 261,
    LT_EQ = 262,
    GT_EQ = 263,
    LSHIFT = 264,
    RSHIFT = 265,
    UNARY = 266,
    NAME = 267,
    INTEGER = 268,
    DOUBLE = 269,
    BOOLEAN = 270,
    TIME = 271,
    SIN = 272,
    COS = 273,
    TAN = 274,
    POW = 275,
    EXP = 276,
    LOG = 277,
    LOG10 = 278,
    SQRT = 279,
    HYPOT = 280,
    FLOOR = 281,
    CEIL = 282,
    FMOD = 283,
    FABS = 284,
    ANALYSIS_TYPE = 285,
    DIRECTION = 286,
    CONSTRAINT = 287,
    HINGED = 288,
    NODE_DOF = 289,
    MASS_MODE = 290,
    PROBLEM = 291,
    ANALYSIS = 292,
    LOAD_CASES = 293,
    END = 294,
    NODES = 295,
    ELEMENTS = 296,
    MATERIALS = 297,
    LOADS = 298,
    FORCES = 299,
    CONSTRAINTS = 300,
    TITLE_EQ = 301,
    NODES_EQ = 302,
    ELEMENTS_EQ = 303,
    ANALYSIS_EQ = 304,
    X_EQ = 305,
    Y_EQ = 306,
    Z_EQ = 307,
    FORCE_EQ = 308,
    CONSTRAINT_EQ = 309,
    MASS_EQ = 310,
    LOAD_EQ = 311,
    MATERIAL_EQ = 312,
    E_EQ = 313,
    IX_EQ = 314,
    IY_EQ = 315,
    IZ_EQ = 316,
    A_EQ = 317,
    J_EQ = 318,
    G_EQ = 319,
    T_EQ = 320,
    RHO_EQ = 321,
    NU_EQ = 322,
    KAPPA_EQ = 323,
    RK_EQ = 324,
    RM_EQ = 325,
    KX_EQ = 326,
    KY_EQ = 327,
    KZ_EQ = 328,
    C_EQ = 329,
    DIRECTION_EQ = 330,
    VALUES_EQ = 331,
    FX_EQ = 332,
    FY_EQ = 333,
    FZ_EQ = 334,
    MX_EQ = 335,
    MY_EQ = 336,
    MZ_EQ = 337,
    SFX_EQ = 338,
    SFY_EQ = 339,
    SFZ_EQ = 340,
    SMX_EQ = 341,
    SMY_EQ = 342,
    SMZ_EQ = 343,
    TX_EQ = 344,
    TY_EQ = 345,
    TZ_EQ = 346,
    RX_EQ = 347,
    RY_EQ = 348,
    RZ_EQ = 349,
    ITX_EQ = 350,
    ITY_EQ = 351,
    ITZ_EQ = 352,
    IRX_EQ = 353,
    IRY_EQ = 354,
    IRZ_EQ = 355,
    VX_EQ = 356,
    VY_EQ = 357,
    VZ_EQ = 358,
    AX_EQ = 359,
    AY_EQ = 360,
    AZ_EQ = 361,
    ALPHA_EQ = 362,
    BETA_EQ = 363,
    GAMMA_EQ = 364,
    DOFS_EQ = 365,
    MASS_MODE_EQ = 366,
    START_EQ = 367,
    STOP_EQ = 368,
    STEP_EQ = 369,
    GRAVITY_EQ = 370,
    ITERATIONS_EQ = 371,
    TOLERANCE_EQ = 372,
    LOAD_STEPS_EQ = 373,
    RELAXATION_EQ = 374,
    INPUT_RANGE_EQ = 375,
    INPUT_DOF_EQ = 376,
    INPUT_NODE_EQ = 377,
    NODE_FORCES_EQ = 378,
    ELEMENT_LOADS_EQ = 379,
    CANVAS = 380,
    FIGURES = 381,
    NODE_NUM_EQ = 382,
    ELT_NUM_EQ = 383,
    SNAP_EQ = 384,
    GRID_EQ = 385,
    SNAP_SIZE_EQ = 386,
    GRID_SIZE_EQ = 387,
    X_MIN_EQ = 388,
    X_MAX_EQ = 389,
    Y_MIN_EQ = 390,
    Y_MAX_EQ = 391,
    SCALE_EQ = 392,
    X_POS_EQ = 393,
    Y_POS_EQ = 394,
    WIDTH_EQ = 395,
    HEIGHT_EQ = 396,
    NODE_COLOR_EQ = 397,
    ELT_COLOR_EQ = 398,
    LABEL_FONT_EQ = 399,
    TOOL_COLOR_EQ = 400,
    TOOL_FONT_EQ = 401,
    FONT_EQ = 402,
    COLOR_EQ = 403,
    LENGTH_EQ = 404,
    TEXT_EQ = 405,
    POINTS_EQ = 406,
    FIGURE_TYPE = 407
  };
#endif
/* Tokens.  */
#define OR 258
#define AND 259
#define EQUALS 260
#define NEQUAL 261
#define LT_EQ 262
#define GT_EQ 263
#define LSHIFT 264
#define RSHIFT 265
#define UNARY 266
#define NAME 267
#define INTEGER 268
#define DOUBLE 269
#define BOOLEAN 270
#define TIME 271
#define SIN 272
#define COS 273
#define TAN 274
#define POW 275
#define EXP 276
#define LOG 277
#define LOG10 278
#define SQRT 279
#define HYPOT 280
#define FLOOR 281
#define CEIL 282
#define FMOD 283
#define FABS 284
#define ANALYSIS_TYPE 285
#define DIRECTION 286
#define CONSTRAINT 287
#define HINGED 288
#define NODE_DOF 289
#define MASS_MODE 290
#define PROBLEM 291
#define ANALYSIS 292
#define LOAD_CASES 293
#define END 294
#define NODES 295
#define ELEMENTS 296
#define MATERIALS 297
#define LOADS 298
#define FORCES 299
#define CONSTRAINTS 300
#define TITLE_EQ 301
#define NODES_EQ 302
#define ELEMENTS_EQ 303
#define ANALYSIS_EQ 304
#define X_EQ 305
#define Y_EQ 306
#define Z_EQ 307
#define FORCE_EQ 308
#define CONSTRAINT_EQ 309
#define MASS_EQ 310
#define LOAD_EQ 311
#define MATERIAL_EQ 312
#define E_EQ 313
#define IX_EQ 314
#define IY_EQ 315
#define IZ_EQ 316
#define A_EQ 317
#define J_EQ 318
#define G_EQ 319
#define T_EQ 320
#define RHO_EQ 321
#define NU_EQ 322
#define KAPPA_EQ 323
#define RK_EQ 324
#define RM_EQ 325
#define KX_EQ 326
#define KY_EQ 327
#define KZ_EQ 328
#define C_EQ 329
#define DIRECTION_EQ 330
#define VALUES_EQ 331
#define FX_EQ 332
#define FY_EQ 333
#define FZ_EQ 334
#define MX_EQ 335
#define MY_EQ 336
#define MZ_EQ 337
#define SFX_EQ 338
#define SFY_EQ 339
#define SFZ_EQ 340
#define SMX_EQ 341
#define SMY_EQ 342
#define SMZ_EQ 343
#define TX_EQ 344
#define TY_EQ 345
#define TZ_EQ 346
#define RX_EQ 347
#define RY_EQ 348
#define RZ_EQ 349
#define ITX_EQ 350
#define ITY_EQ 351
#define ITZ_EQ 352
#define IRX_EQ 353
#define IRY_EQ 354
#define IRZ_EQ 355
#define VX_EQ 356
#define VY_EQ 357
#define VZ_EQ 358
#define AX_EQ 359
#define AY_EQ 360
#define AZ_EQ 361
#define ALPHA_EQ 362
#define BETA_EQ 363
#define GAMMA_EQ 364
#define DOFS_EQ 365
#define MASS_MODE_EQ 366
#define START_EQ 367
#define STOP_EQ 368
#define STEP_EQ 369
#define GRAVITY_EQ 370
#define ITERATIONS_EQ 371
#define TOLERANCE_EQ 372
#define LOAD_STEPS_EQ 373
#define RELAXATION_EQ 374
#define INPUT_RANGE_EQ 375
#define INPUT_DOF_EQ 376
#define INPUT_NODE_EQ 377
#define NODE_FORCES_EQ 378
#define ELEMENT_LOADS_EQ 379
#define CANVAS 380
#define FIGURES 381
#define NODE_NUM_EQ 382
#define ELT_NUM_EQ 383
#define SNAP_EQ 384
#define GRID_EQ 385
#define SNAP_SIZE_EQ 386
#define GRID_SIZE_EQ 387
#define X_MIN_EQ 388
#define X_MAX_EQ 389
#define Y_MIN_EQ 390
#define Y_MAX_EQ 391
#define SCALE_EQ 392
#define X_POS_EQ 393
#define Y_POS_EQ 394
#define WIDTH_EQ 395
#define HEIGHT_EQ 396
#define NODE_COLOR_EQ 397
#define ELT_COLOR_EQ 398
#define LABEL_FONT_EQ 399
#define TOOL_COLOR_EQ 400
#define TOOL_FONT_EQ 401
#define FONT_EQ 402
#define COLOR_EQ 403
#define LENGTH_EQ 404
#define TEXT_EQ 405
#define POINTS_EQ 406
#define FIGURE_TYPE 407

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 114 "parser.y" /* yacc.c:355  */

    int       i;
    double    d;
    char     *s;
    Pair      p;
    CasePair  cp;
    char      c;

#line 540 "parser.cpp" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE felt_yylval;

int felt_yyparse (void);

#endif /* !YY_FELT_YY_PARSER_HPP_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 557 "parser.cpp" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  30
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1386

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  172
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  92
/* YYNRULES -- Number of rules.  */
#define YYNRULES  303
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  553

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   407

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    24,     2,     2,     2,    22,     9,     2,
     167,   168,    20,    18,   171,    19,     2,    21,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     4,     2,
      12,     2,    13,     3,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   169,     2,   170,     8,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     7,     2,    25,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     1,     2,     5,     6,
      10,    11,    14,    15,    16,    17,    23,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,   155,   156,   157,   158,
     159,   160,   161,   162,   163,   164,   165,   166
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   189,   189,   190,   196,   209,   210,   215,   216,   221,
     227,   232,   237,   242,   249,   250,   255,   256,   257,   258,
     259,   260,   261,   262,   263,   264,   265,   272,   277,   278,
     283,   288,   316,   317,   323,   324,   329,   334,   339,   344,
     349,   355,   362,   369,   374,   385,   386,   391,   396,   420,
     421,   427,   428,   433,   454,   463,   465,   470,   475,   480,
     489,   498,   510,   522,   534,   539,   540,   545,   550,   564,
     565,   570,   576,   581,   586,   591,   596,   601,   606,   611,
     616,   621,   626,   631,   636,   641,   646,   651,   656,   661,
     668,   673,   674,   679,   684,   698,   699,   704,   710,   715,
     732,   737,   742,   747,   756,   765,   779,   784,   785,   790,
     795,   809,   810,   815,   821,   826,   831,   836,   841,   846,
     851,   856,   861,   866,   871,   876,   881,   888,   893,   894,
     899,   904,   919,   920,   925,   931,   941,   951,   961,   971,
     981,   991,   996,  1001,  1006,  1011,  1016,  1021,  1026,  1031,
    1036,  1041,  1046,  1051,  1056,  1061,  1069,  1074,  1079,  1089,
    1094,  1095,  1099,  1103,  1117,  1118,  1122,  1138,  1154,  1159,
    1164,  1169,  1178,  1187,  1200,  1205,  1206,  1211,  1216,  1221,
    1226,  1231,  1236,  1241,  1246,  1251,  1256,  1261,  1266,  1271,
    1276,  1281,  1289,  1300,  1305,  1312,  1317,  1322,  1328,  1335,
    1340,  1346,  1357,  1367,  1373,  1382,  1387,  1388,  1393,  1398,
    1403,  1408,  1413,  1418,  1423,  1428,  1433,  1438,  1443,  1448,
    1453,  1458,  1463,  1468,  1473,  1478,  1483,  1488,  1498,  1503,
    1511,  1512,  1517,  1522,  1542,  1543,  1548,  1553,  1558,  1563,
    1568,  1573,  1578,  1583,  1588,  1593,  1598,  1606,  1615,  1622,
    1633,  1639,  1653,  1670,  1671,  1672,  1677,  1699,  1706,  1716,
    1727,  1737,  1747,  1753,  1759,  1765,  1771,  1777,  1783,  1789,
    1795,  1801,  1807,  1813,  1819,  1825,  1831,  1837,  1843,  1848,
    1854,  1860,  1866,  1871,  1877,  1883,  1889,  1894,  1900,  1906,
    1912,  1918,  1924,  1930,  1936,  1942,  1948,  1954,  1960,  1966,
    1976,  1985,  1994,  2005
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "'?'", "':'", "OR", "AND", "'|'", "'^'",
  "'&'", "EQUALS", "NEQUAL", "'<'", "'>'", "LT_EQ", "GT_EQ", "LSHIFT",
  "RSHIFT", "'+'", "'-'", "'*'", "'/'", "'%'", "UNARY", "'!'", "'~'",
  "NAME", "INTEGER", "DOUBLE", "BOOLEAN", "TIME", "SIN", "COS", "TAN",
  "POW", "EXP", "LOG", "LOG10", "SQRT", "HYPOT", "FLOOR", "CEIL", "FMOD",
  "FABS", "ANALYSIS_TYPE", "DIRECTION", "CONSTRAINT", "HINGED", "NODE_DOF",
  "MASS_MODE", "PROBLEM", "ANALYSIS", "LOAD_CASES", "END", "NODES",
  "ELEMENTS", "MATERIALS", "LOADS", "FORCES", "CONSTRAINTS", "TITLE_EQ",
  "NODES_EQ", "ELEMENTS_EQ", "ANALYSIS_EQ", "X_EQ", "Y_EQ", "Z_EQ",
  "FORCE_EQ", "CONSTRAINT_EQ", "MASS_EQ", "LOAD_EQ", "MATERIAL_EQ", "E_EQ",
  "IX_EQ", "IY_EQ", "IZ_EQ", "A_EQ", "J_EQ", "G_EQ", "T_EQ", "RHO_EQ",
  "NU_EQ", "KAPPA_EQ", "RK_EQ", "RM_EQ", "KX_EQ", "KY_EQ", "KZ_EQ", "C_EQ",
  "DIRECTION_EQ", "VALUES_EQ", "FX_EQ", "FY_EQ", "FZ_EQ", "MX_EQ", "MY_EQ",
  "MZ_EQ", "SFX_EQ", "SFY_EQ", "SFZ_EQ", "SMX_EQ", "SMY_EQ", "SMZ_EQ",
  "TX_EQ", "TY_EQ", "TZ_EQ", "RX_EQ", "RY_EQ", "RZ_EQ", "ITX_EQ", "ITY_EQ",
  "ITZ_EQ", "IRX_EQ", "IRY_EQ", "IRZ_EQ", "VX_EQ", "VY_EQ", "VZ_EQ",
  "AX_EQ", "AY_EQ", "AZ_EQ", "ALPHA_EQ", "BETA_EQ", "GAMMA_EQ", "DOFS_EQ",
  "MASS_MODE_EQ", "START_EQ", "STOP_EQ", "STEP_EQ", "GRAVITY_EQ",
  "ITERATIONS_EQ", "TOLERANCE_EQ", "LOAD_STEPS_EQ", "RELAXATION_EQ",
  "INPUT_RANGE_EQ", "INPUT_DOF_EQ", "INPUT_NODE_EQ", "NODE_FORCES_EQ",
  "ELEMENT_LOADS_EQ", "CANVAS", "FIGURES", "NODE_NUM_EQ", "ELT_NUM_EQ",
  "SNAP_EQ", "GRID_EQ", "SNAP_SIZE_EQ", "GRID_SIZE_EQ", "X_MIN_EQ",
  "X_MAX_EQ", "Y_MIN_EQ", "Y_MAX_EQ", "SCALE_EQ", "X_POS_EQ", "Y_POS_EQ",
  "WIDTH_EQ", "HEIGHT_EQ", "NODE_COLOR_EQ", "ELT_COLOR_EQ",
  "LABEL_FONT_EQ", "TOOL_COLOR_EQ", "TOOL_FONT_EQ", "FONT_EQ", "COLOR_EQ",
  "LENGTH_EQ", "TEXT_EQ", "POINTS_EQ", "FIGURE_TYPE", "'('", "')'", "'['",
  "']'", "','", "$accept", "specification", "initialize",
  "problem_description", "problem_parameter_list", "problem_parameter",
  "section_list", "section", "node_section", "node_definition_list",
  "node_definition", "node_number", "node_number_expression",
  "node_parameter_list", "node_parameter", "element_section",
  "element_header", "element_definition_list", "element_definition",
  "element_number", "element_number_expression", "element_parameter_list",
  "element_parameter", "element_node_list", "element_node",
  "element_load_list", "material_section", "material_definition_list",
  "material_definition", "material_name", "material_parameter_list",
  "material_parameter", "load_section", "load_definition_list",
  "load_definition", "load_name", "load_parameter_list", "load_parameter",
  "value_pair_list", "value_pair", "force_section",
  "force_definition_list", "force_definition", "force_name",
  "force_parameter_list", "force_parameter", "constraint_section",
  "constraint_definition_list", "constraint_definition", "constraint_name",
  "constraint_parameter_list", "constraint_parameter", "translation",
  "rotation", "loadcase_section", "loadcase_definition_list",
  "loadcase_definition", "loadcase_name", "loadcase_parameter_list",
  "loadcase_parameter", "loadcase_pair_list", "loadcase_pair",
  "analysis_section", "analysis_parameter_list", "analysis_parameter",
  "analysis_node_list", "analysis_dof_list", "triple", "opt_z_coordinate",
  "canvas_section", "canvas_parameter_list", "canvas_parameter",
  "figure_section", "figure_header", "figure_definition_list",
  "figure_definition", "figure_type", "figure_parameter_list",
  "figure_parameter", "figure_pair_list", "figure_pair",
  "variable_expression", "discrete_pair_list", "discrete_pair",
  "enable_copy", "constant_expression", "expression", "function",
  "if_action", "else_action", "or_action", "and_action", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,    63,    58,   258,   259,   124,    94,    38,
     260,   261,    60,    62,   262,   263,   264,   265,    43,    45,
      42,    47,    37,   266,    33,   126,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
     281,   282,   283,   284,   285,   286,   287,   288,   289,   290,
     291,   292,   293,   294,   295,   296,   297,   298,   299,   300,
     301,   302,   303,   304,   305,   306,   307,   308,   309,   310,
     311,   312,   313,   314,   315,   316,   317,   318,   319,   320,
     321,   322,   323,   324,   325,   326,   327,   328,   329,   330,
     331,   332,   333,   334,   335,   336,   337,   338,   339,   340,
     341,   342,   343,   344,   345,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,   359,   360,
     361,   362,   363,   364,   365,   366,   367,   368,   369,   370,
     371,   372,   373,   374,   375,   376,   377,   378,   379,   380,
     381,   382,   383,   384,   385,   386,   387,   388,   389,   390,
     391,   392,   393,   394,   395,   396,   397,   398,   399,   400,
     401,   402,   403,   404,   405,   406,   407,    40,    41,    91,
      93,    44
};
# endif

#define YYPACT_NINF -446

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-446)))

#define YYTABLE_NINF -175

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -55,  1137,    14,   -31,  1179,  1179,  1179,  1179,  -446,  -446,
    -446,  -127,   -71,   -63,   -58,   -56,   -46,   -35,   -13,    16,
      18,    19,    20,    22,  1179,    59,   -15,  -446,  1364,  -446,
    -446,  -446,  -446,  1179,  -446,  -446,  -446,  -446,  1179,  1179,
    1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,
    1179,   -51,   405,  -446,  -446,  1179,    23,  -446,  -446,  -446,
    -446,  1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,
    1179,  1179,  1179,  1179,  1179,  1179,  1179,   109,    -9,   405,
     476,   501,   537,    17,   577,   597,   617,   640,    48,   665,
     701,   193,   761,  1179,  -446,  1364,  -446,  1179,  1179,  1179,
     680,   737,   454,   252,   252,    64,    64,    64,    64,    56,
      56,   105,   105,  -446,  -446,  -446,  -446,   165,   168,   170,
     148,  -446,  -446,  -446,   216,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  1179,
    -446,  -446,  -446,  -446,  1179,  -446,  -446,  1179,  -446,    49,
    1273,   360,   517,  -446,  -446,  -446,  -446,  1184,   192,   -23,
     194,   198,   199,   201,  1206,   -14,    62,   781,   804,   829,
    -446,  -446,  -446,    77,  1179,  1179,  1179,  1179,  1179,    78,
     202,  1179,  1179,  1179,    83,   226,  1179,   227,  1179,   207,
     -23,  -446,  -446,  -446,  -446,  -446,  1179,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,   228,   230,   231,   232,  1179,  1179,  1179,  1179,
    1179,  1179,  1179,  1179,  1179,  1179,  1179,   263,   274,   275,
     277,   278,  -446,  -446,  1179,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  1179,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  1179,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  1198,    88,   338,   901,   967,   991,
     803,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,   137,   123,   -47,  1364,   -22,   -42,   135,  -446,   140,
     140,  -446,  -446,  -446,  1179,  1179,  1179,   282,   283,  1179,
    -446,  -446,  1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,
    1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,   284,
    -446,  -446,   268,   147,   289,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,   290,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  1179,  1179,
    1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,  1179,
     291,  -446,  -446,  -446,   149,   293,   295,  -446,  1179,  1179,
    1179,  1179,  1179,   296,   297,  1179,   298,   169,  -446,  -446,
     -23,  -446,  -446,  -446,   292,  1179,   -23,   -70,  -446,   -70,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,   -23,   -65,  -446,  -446,  1137,
    1137,  1137,  1137,  1137,  1137,  1137,  1137,  1137,  1137,  1137,
    1137,  -446,  1108,  1108,  1108,   846,   846,   846,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,   -23,  -446,   -25,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,   171,   -24,   140,
    -446,  1034,   147,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,   -12,  -446,
    -446,   315,   -48,  1179,   175,   176,   319,  -446,  1179,   178,
    -446,  -446,   -23,  -446,  -446,  1179,  -446,   180,  -446,  -446,
    -446,  -446,   215,   219,  -446,  -446,   217,  -446,  -446,  -446,
    1179,   233,  -446
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       4,     0,     0,     6,     0,     0,     0,     0,   283,   284,
     285,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   251,   255,   250,   286,
       1,     8,    15,     0,   278,   279,   280,   281,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   258,     3,   252,     0,     0,   254,   300,   302,
     303,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   282,   258,   253,     0,     0,     0,
     262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
     272,   273,   274,   275,   276,   277,    13,     0,     0,     0,
       0,     7,   176,   161,    26,    29,    44,    66,    92,   108,
     129,   207,   229,    14,    16,    17,    46,    18,    19,    20,
      21,    23,    22,    24,    25,   231,   287,   288,   289,     0,
     291,   292,   293,   294,     0,   296,   297,     0,   299,     0,
       0,   260,   261,     9,    10,    11,    12,     0,   159,    27,
      64,    90,   106,   127,   205,    43,   228,     0,     0,     0,
     256,   301,   195,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   175,   163,   160,   165,    32,     0,    28,    35,    31,
      68,    65,    70,    94,    91,    96,   110,   107,   112,   131,
     128,   133,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   206,    49,     0,    45,    52,    48,   233,   230,
     235,   290,   295,   298,     0,   198,   180,   181,   177,   178,
     179,   201,   193,   182,   184,   183,     0,   194,   185,   188,
     186,   187,   189,   190,     0,     0,     0,     0,     0,     0,
       0,   208,   209,   210,   211,   212,   213,   214,   215,   216,
     217,   222,   218,   219,   220,   221,   223,   224,   225,   226,
     227,     0,     0,   232,   259,     0,     0,     0,   168,     0,
       0,   164,    33,    42,     0,     0,     0,     0,     0,     0,
      34,    89,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,   100,     0,     0,     0,    95,   126,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,     0,
     111,   153,   257,   257,   257,   257,   257,   257,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,    50,    56,     0,     0,     0,    51,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   234,   191,
       0,   197,   200,   192,     0,     0,     0,   166,   171,   167,
      36,    37,    38,    40,    41,    39,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    71,    98,     0,    99,   103,    97,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,     0,     0,     0,     0,     0,     0,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     134,     0,    63,    55,    54,   236,   237,   240,   238,   239,
     244,   243,   241,   242,   248,   196,   199,   204,     0,     0,
     170,     0,     0,   102,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   155,   135,   154,   136,
     137,   157,   158,   138,   156,   139,   140,    60,     0,    59,
      62,     0,     0,     0,     0,     0,     0,   169,     0,     0,
     101,    53,     0,    58,    61,     0,   245,     0,   247,   203,
     202,   173,     0,     0,   105,    57,     0,   246,   172,   104,
       0,     0,   249
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -148,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -445,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -420,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -442,  -368,  -446,  -446,  -446,  -446,  -446,  -446,
      89,  -399,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,  -446,
    -137,     6,  -446,    15,   212,   -93,   287,  -446,  -446,  -446,
    -446,  -446
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,    32,    77,   121,    78,   133,   134,   169,
     207,   208,   517,   276,   320,   135,   136,   175,   245,   246,
     247,   302,   387,   518,   519,   473,   137,   170,   211,   212,
     277,   340,   138,   171,   214,   215,   278,   345,   436,   437,
     139,   172,   217,   218,   279,   360,   140,   173,   220,   221,
     280,   381,   507,   513,   141,   168,   203,   204,   274,   311,
     407,   408,   142,   167,   201,   305,   306,   267,   524,   143,
     174,   242,   144,   145,   176,   249,   250,   303,   398,   522,
     538,   508,    26,    27,   439,    51,    95,    29,    97,   254,
      98,    99
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     159,   520,   525,    54,   205,   205,   402,    25,   490,     1,
     490,   509,   510,   243,    30,   205,   493,   388,   389,    31,
      58,   209,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      38,    57,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    58,   273,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    96,   530,   533,    72,    73,    74,    75,    76,   390,
      70,    71,    72,    73,    74,    75,    76,   545,   515,   516,
     527,   256,   257,   258,   259,   260,    39,   406,   263,   264,
     265,   489,   435,   269,    40,   271,   492,   391,   392,    41,
     116,    42,    53,   275,   393,   394,   395,   396,   397,   535,
      93,    43,   536,   537,   383,    74,    75,    76,   403,   404,
     131,   132,    44,   285,   286,   287,   288,   289,   290,   291,
     292,   293,   294,   295,   206,   206,   521,   526,   399,   400,
     -47,   301,    55,   244,    45,   206,    56,   401,   531,   532,
      -5,    -5,    -5,    -5,    -5,    -5,    -5,    -5,    -5,   117,
     118,   119,   120,   307,   -47,   -47,   -47,   -47,   -47,   -47,
     -47,   -47,   -47,    46,   384,    47,    48,    49,   149,    50,
      55,   163,   166,   385,   386,   164,    58,   165,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    -2,   180,   202,   154,
     210,   410,   411,   412,   213,   216,   415,   219,   248,   416,
     417,   418,   419,   420,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,   431,   432,   255,   261,    -5,    -5,
     266,   262,   485,   268,   270,   272,   312,   281,   488,   282,
     283,   284,   -47,   -47,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,   458,   459,   460,   461,   462,
     463,   464,   465,   466,   467,   468,   469,   491,    28,   296,
     -47,    34,    35,    36,    37,   475,   476,   477,   478,   479,
     297,   298,   482,   299,   300,   382,   405,   406,   413,   414,
     433,    52,   487,   434,   435,   438,   451,   470,   471,   472,
      79,   474,   480,   481,   483,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,   484,   313,
     486,   534,   523,   540,   541,   542,   544,   535,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   157,   -30,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,   548,   160,   161,   162,   549,   550,   -30,
     -30,   -30,   -30,   -30,   -30,   -30,   -30,   -30,   529,   409,
     547,   552,   314,   315,   316,   317,   318,   319,    58,     0,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,     0,     0,
     539,     0,     0,     0,     0,   543,   177,     0,     0,     0,
       0,   178,   546,     0,   179,   494,   495,   496,   497,   498,
     499,   500,   501,   502,   503,   504,   505,   551,     0,     0,
       0,   514,   514,   514,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,   -30,   -30,    58,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,     0,
       0,     0,     0,     0,    58,   -30,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      58,   304,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
     440,   441,   442,   443,   444,   445,   446,   447,   448,   449,
     450,     0,     0,    94,   452,   453,   454,   455,   456,   457,
      58,     0,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      58,     0,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      58,     0,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
       0,     0,     0,    58,   146,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,     0,     0,     0,     0,     0,    58,   147,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,     0,    58,   148,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,     0,     0,    28,    28,    28,    28,
      28,    28,    28,    28,    28,    28,    28,    28,     0,    28,
      28,    28,    28,    28,    28,   150,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
       0,     0,     0,     0,    58,   151,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    58,   152,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,   361,     0,     0,    58,   153,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,     0,     0,  -130,
       0,     0,    58,   155,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,     0,     0,  -130,  -130,  -130,  -130,  -130,  -130,
    -130,  -130,  -130,     0,     4,     5,     0,     0,     0,   156,
       6,     7,     0,     8,     9,     0,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
       0,     0,   511,   512,     0,     0,     0,     0,     0,     0,
       0,     0,   321,     0,     0,     0,   362,   363,   364,   365,
     366,   367,   368,   369,   370,   371,   372,   373,   374,   375,
     376,   377,   378,   379,     0,     0,     0,   -67,     0,   158,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -130,  -130,     0,     0,     0,     0,     0,   251,
       0,     0,   -67,   -67,   -67,   -67,   -67,   -67,   -67,   -67,
     -67,     0,     0,     0,     0,   380,     0,     0,   341,     0,
       0,     0,   252,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,   332,   333,   334,   335,   336,   337,   338,
       0,     0,   346,   -93,     0,     0,     0,   253,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    24,     0,     0,     0,  -109,   -93,   -93,
     -93,   -93,   -93,   -93,   -93,   -93,   -93,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     -67,   -67,  -109,  -109,  -109,  -109,  -109,  -109,  -109,  -109,
    -109,     0,     4,     5,     0,     0,   342,   343,     6,     7,
       0,     8,     9,   339,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,     0,     0,
       0,     0,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   -93,   -93,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     4,     5,     0,   344,
    -109,  -109,     6,     7,     0,     8,     9,     0,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,     0,   359,   506,     4,     5,     0,     0,     0,
       0,     6,     7,     0,     8,     9,     0,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,     0,     0,     0,     0,   182,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     4,     5,   308,
       0,    33,     0,     6,     7,   528,     8,     9,     0,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,     0,  -162,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -174,  -174,  -174,  -174,  -174,
    -174,  -174,  -174,  -174,     0,   183,     0,     0,     0,  -162,
    -162,  -162,  -162,  -162,  -162,  -162,  -162,  -162,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   184,   185,     0,
       0,     0,     0,     0,     0,    24,    58,   181,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,     0,     0,     0,     0,
       0,     0,     0,     0,    24,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197,   198,     0,   199,
     200,     0,     0,  -174,  -174,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   309,   310,  -162,  -162,     0,
       0,     0,     0,     0,     0,     0,    33,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,    58,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76
};

static const yytype_int16 yycheck[] =
{
      93,    26,    26,    18,    27,    27,    48,     1,   407,    64,
     409,   453,   454,    27,     0,    27,   436,    64,    65,    50,
       3,   169,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     167,    26,    51,    52,    53,    54,    55,    56,    57,    58,
      59,     3,   200,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    56,   492,   518,    18,    19,    20,    21,    22,   126,
      16,    17,    18,    19,    20,    21,    22,   532,   456,   457,
     489,   184,   185,   186,   187,   188,   167,   167,   191,   192,
     193,   171,   167,   196,   167,   198,   171,   154,   155,   167,
       1,   167,    53,   206,   161,   162,   163,   164,   165,   167,
     171,   167,   170,   171,     1,    20,    21,    22,   170,   171,
     139,   140,   167,   226,   227,   228,   229,   230,   231,   232,
     233,   234,   235,   236,   167,   167,   171,   171,   170,   171,
      27,   244,   167,   167,   167,   167,   171,   305,   170,   171,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,   266,    51,    52,    53,    54,    55,    56,
      57,    58,    59,   167,    61,   167,   167,   167,   171,   167,
     167,    26,    44,    70,    71,    27,     3,    27,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,     0,   168,    26,   171,
      26,   314,   315,   316,    26,    26,   319,    26,   166,   322,
     323,   324,   325,   326,   327,   328,   329,   330,   331,   332,
     333,   334,   335,   336,   337,   338,   169,   169,   139,   140,
     167,    49,   400,    27,    27,    48,   168,    29,   406,    29,
      29,    29,   139,   140,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,   368,   369,   370,   371,   372,
     373,   374,   375,   376,   377,   378,   379,   435,     1,    26,
     167,     4,     5,     6,     7,   388,   389,   390,   391,   392,
      26,    26,   395,    26,    26,   168,   171,   167,    26,    26,
      26,    24,   405,    45,   167,    26,    26,    26,   169,    26,
      33,    26,    26,    26,    26,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,   169,     1,
      48,    26,   171,   168,   168,    26,   168,   167,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,   171,    27,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,   168,    97,    98,    99,   168,   171,    51,
      52,    53,    54,    55,    56,    57,    58,    59,   491,   310,
     537,   168,    64,    65,    66,    67,    68,    69,     3,    -1,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    -1,    -1,
     523,    -1,    -1,    -1,    -1,   528,   149,    -1,    -1,    -1,
      -1,   154,   535,    -1,   157,   439,   440,   441,   442,   443,
     444,   445,   446,   447,   448,   449,   450,   550,    -1,    -1,
      -1,   455,   456,   457,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,   139,   140,     3,
      -1,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      -1,    -1,    -1,    -1,     3,   167,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
       3,   254,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     348,   349,   350,   351,   352,   353,   354,   355,   356,   357,
     358,    -1,    -1,   168,   362,   363,   364,   365,   366,   367,
       3,    -1,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
       3,    -1,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
       3,    -1,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      -1,    -1,    -1,     3,   168,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    -1,    -1,    -1,    -1,     3,   168,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,     3,   168,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    -1,    -1,   439,   440,   441,   442,
     443,   444,   445,   446,   447,   448,   449,   450,    -1,   452,
     453,   454,   455,   456,   457,   168,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      -1,    -1,    -1,    -1,     3,   168,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,     3,   168,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,     1,    -1,    -1,     3,   168,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    -1,    26,
      -1,    -1,     3,   168,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    -1,    -1,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    -1,    18,    19,    -1,    -1,    -1,   168,
      24,    25,    -1,    27,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    46,    47,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,    -1,    -1,    26,    -1,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   139,   140,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,   162,    -1,    -1,     1,    -1,
      -1,    -1,   168,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      -1,    -1,     1,    26,    -1,    -1,    -1,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,    -1,    26,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     139,   140,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    18,    19,    -1,    -1,    89,    90,    24,    25,
      -1,    27,    28,   162,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    -1,    -1,
      -1,    -1,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   139,   140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    19,    -1,   162,
     139,   140,    24,    25,    -1,    27,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    -1,   162,    46,    18,    19,    -1,    -1,    -1,
      -1,    24,    25,    -1,    27,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    19,     1,
      -1,   167,    -1,    24,    25,   171,    27,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    -1,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    -1,    61,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    84,    -1,
      -1,    -1,    -1,    -1,    -1,   167,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    -1,    -1,   139,   140,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,   138,   139,   140,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,   155,   156,   157,   158,   159,   160,     3,    -1,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,    64,   173,   174,    18,    19,    24,    25,    27,    28,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,   167,   253,   254,   255,   258,   259,
       0,    50,   175,   167,   258,   258,   258,   258,   167,   167,
     167,   167,   167,   167,   167,   167,   167,   167,   167,   167,
     167,   257,   258,    53,    18,   167,   171,   255,     3,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,   176,   178,   258,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   258,   258,   171,   168,   258,   255,   260,   262,   263,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   258,   258,   258,   258,   258,     1,    60,    61,    62,
      63,   177,    51,    52,    53,    54,    55,    56,    57,    58,
      59,   139,   140,   179,   180,   187,   188,   198,   204,   212,
     218,   226,   234,   241,   244,   245,   168,   168,   168,   171,
     168,   168,   168,   168,   171,   168,   168,   171,   168,   257,
     258,   258,   258,    26,    27,    27,    44,   235,   227,   181,
     199,   205,   213,   219,   242,   189,   246,   258,   258,   258,
     168,     4,     1,    61,    83,    84,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   135,
     136,   236,    26,   228,   229,    27,   167,   182,   183,   184,
      26,   200,   201,    26,   206,   207,    26,   214,   215,    26,
     220,   221,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,   155,   156,   157,   158,
     159,   160,   243,    27,   167,   190,   191,   192,   166,   247,
     248,   168,   168,   168,   261,   169,   257,   257,   257,   257,
     257,   169,    49,   257,   257,   257,   167,   239,    27,   257,
      27,   257,    48,   184,   230,   257,   185,   202,   208,   216,
     222,    29,    29,    29,    29,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,    26,    26,    26,    26,
      26,   257,   193,   249,   258,   237,   238,   257,     1,   137,
     138,   231,   168,     1,    64,    65,    66,    67,    68,    69,
     186,     1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,   162,
     203,     1,    89,    90,   162,   209,     1,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   162,
     217,     1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     162,   223,   168,     1,    61,    70,    71,   194,    64,    65,
     126,   154,   155,   161,   162,   163,   164,   165,   250,   170,
     171,   184,    48,   170,   171,   171,   167,   232,   233,   232,
     257,   257,   257,    26,    26,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,    26,    45,   167,   210,   211,    26,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   256,   256,
     256,    26,   256,   256,   256,   256,   256,   256,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
      26,   169,    26,   197,    26,   257,   257,   257,   257,   257,
      26,    26,   257,    26,   169,   184,    48,   257,   184,   171,
     233,   184,   171,   211,   253,   253,   253,   253,   253,   253,
     253,   253,   253,   253,   253,   253,    46,   224,   253,   224,
     224,    46,    47,   225,   253,   225,   225,   184,   195,   196,
      26,   171,   251,   171,   240,    26,   171,   233,   171,   257,
     211,   170,   171,   196,    26,   167,   170,   171,   252,   257,
     168,   168,    26,   257,   168,   196,   257,   252,   168,   168,
     171,   257,   168
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   172,   173,   173,   174,   175,   175,   176,   176,   177,
     177,   177,   177,   177,   178,   178,   179,   179,   179,   179,
     179,   179,   179,   179,   179,   179,   179,   180,   181,   181,
     182,   183,   184,   184,   185,   185,   186,   186,   186,   186,
     186,   186,   186,   187,   188,   189,   189,   190,   191,   192,
     192,   193,   193,   194,   194,   194,   194,   195,   195,   195,
     196,   197,   197,   197,   198,   199,   199,   200,   201,   202,
     202,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     204,   205,   205,   206,   207,   208,   208,   209,   209,   209,
     209,   210,   210,   210,   211,   211,   212,   213,   213,   214,
     215,   216,   216,   217,   217,   217,   217,   217,   217,   217,
     217,   217,   217,   217,   217,   217,   217,   218,   219,   219,
     220,   221,   222,   222,   223,   223,   223,   223,   223,   223,
     223,   223,   223,   223,   223,   223,   223,   223,   223,   223,
     223,   223,   223,   223,   224,   224,   225,   225,   225,   226,
     227,   227,   228,   229,   230,   230,   231,   231,   231,   232,
     232,   232,   233,   233,   234,   235,   235,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   237,   237,   237,   238,
     238,   238,   239,   240,   240,   241,   242,   242,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   244,   245,
     246,   246,   247,   248,   249,   249,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   251,   251,   251,   252,
     253,   253,   253,   254,   254,   254,   255,   256,   257,   258,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   258,   258,   258,   258,   258,   258,   259,   259,   259,
     259,   259,   259,   259,   259,   259,   259,   259,   259,   259,
     260,   261,   262,   263
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     4,     3,     0,     2,     0,     2,     0,     2,
       2,     2,     2,     1,     2,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     2,     0,
       2,     1,     1,     3,     2,     0,     2,     2,     2,     2,
       2,     2,     1,     2,     1,     2,     0,     2,     1,     1,
       3,     2,     0,     4,     2,     2,     1,     3,     2,     1,
       1,     3,     2,     1,     2,     2,     0,     2,     1,     2,
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       2,     2,     0,     2,     1,     2,     0,     2,     2,     2,
       1,     3,     2,     1,     5,     4,     2,     2,     0,     2,
       1,     2,     0,     2,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     1,     2,     2,     0,
       2,     1,     2,     0,     2,     3,     3,     3,     3,     3,
       3,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     1,     1,     1,     1,     1,     1,     2,
       2,     0,     2,     1,     2,     0,     2,     2,     1,     3,
       2,     1,     5,     4,     2,     2,     0,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     4,     4,     2,     2,     1,     3,     2,     0,     3,
       2,     0,     6,     2,     0,     2,     2,     0,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       2,     0,     2,     1,     2,     0,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     4,     3,     2,     0,     5,
       1,     1,     2,     3,     2,     1,     5,     0,     1,     7,
       4,     4,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     2,     2,
       2,     2,     3,     1,     1,     1,     1,     4,     4,     4,
       6,     4,     4,     4,     4,     6,     4,     4,     6,     4,
       0,     0,     0,     0
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:
#line 196 "parser.y" /* yacc.c:1646  */
    {
		last_x = 0;
		last_y = 0;
		last_z = 0;
		last_material = "";
		last_constraint = "";
	    }
#line 2262 "parser.cpp" /* yacc.c:1646  */
    break;

  case 9:
#line 222 "parser.y" /* yacc.c:1646  */
    {
		Deallocate (problem.title);
		problem.title = (yyvsp[0].s);
	    }
#line 2271 "parser.cpp" /* yacc.c:1646  */
    break;

  case 10:
#line 228 "parser.y" /* yacc.c:1646  */
    {
             problem.nodes.resize((yyvsp[0].i));
	    }
#line 2279 "parser.cpp" /* yacc.c:1646  */
    break;

  case 11:
#line 233 "parser.y" /* yacc.c:1646  */
    {
             problem.elements.resize((yyvsp[0].i));
	    }
#line 2287 "parser.cpp" /* yacc.c:1646  */
    break;

  case 12:
#line 238 "parser.y" /* yacc.c:1646  */
    {
		problem.mode = (AnalysisType) (yyvsp[0].i);
	    }
#line 2295 "parser.cpp" /* yacc.c:1646  */
    break;

  case 31:
#line 289 "parser.y" /* yacc.c:1646  */
    {
             if ((yyvsp[0].i) < 1 || (yyvsp[0].i) > problem.nodes.size()) {
                  error ("node number %u is illegal", (yyvsp[0].i));
                  node = dummy_node;
                  break;
             }
             
             node.reset(new node_t((yyvsp[0].i)));

		if (!problem.node_set.insert(node).second) {
		    error ("node number %u is repeated", (yyvsp[0].i));
		    node = dummy_node;
		    break;
		}

		node -> x = last_x;
		node -> y = last_y;
		node -> z = last_z;
        if (!last_constraint.empty())
             node->constraint.reset(new constraint_t(last_constraint.c_str()));
        else
             node->constraint.reset();
	    }
#line 2323 "parser.cpp" /* yacc.c:1646  */
    break;

  case 33:
#line 318 "parser.y" /* yacc.c:1646  */
    {(yyval.i) = (yyvsp[-1].d);}
#line 2329 "parser.cpp" /* yacc.c:1646  */
    break;

  case 36:
#line 330 "parser.y" /* yacc.c:1646  */
    {
		node -> x = last_x = (yyvsp[0].d);
	    }
#line 2337 "parser.cpp" /* yacc.c:1646  */
    break;

  case 37:
#line 335 "parser.y" /* yacc.c:1646  */
    {
		node -> y = last_y = (yyvsp[0].d);
	    }
#line 2345 "parser.cpp" /* yacc.c:1646  */
    break;

  case 38:
#line 340 "parser.y" /* yacc.c:1646  */
    {
		node -> z = last_z = (yyvsp[0].d);
	    }
#line 2353 "parser.cpp" /* yacc.c:1646  */
    break;

  case 39:
#line 345 "parser.y" /* yacc.c:1646  */
    {
                node -> m = (yyvsp[0].d);
            }
#line 2361 "parser.cpp" /* yacc.c:1646  */
    break;

  case 40:
#line 350 "parser.y" /* yacc.c:1646  */
    {
         node -> force.reset(new force_t((yyvsp[0].s)));
         free((yyvsp[0].s));
    }
#line 2370 "parser.cpp" /* yacc.c:1646  */
    break;

  case 41:
#line 356 "parser.y" /* yacc.c:1646  */
    {
         last_constraint = (yyvsp[0].s);
         free((yyvsp[0].s));
         node -> constraint.reset(new constraint_t(last_constraint.c_str()));
    }
#line 2380 "parser.cpp" /* yacc.c:1646  */
    break;

  case 44:
#line 375 "parser.y" /* yacc.c:1646  */
    {
		definition = defnlookup ((yyvsp[0].s));
		Deallocate ((yyvsp[0].s));
		if (!definition)
		    return 1;
	    }
#line 2391 "parser.cpp" /* yacc.c:1646  */
    break;

  case 48:
#line 397 "parser.y" /* yacc.c:1646  */
    {
		if ((yyvsp[0].i) < 1 || (yyvsp[0].i) > problem.elements.size()) {
		    error ("element number %u is illegal", (yyvsp[0].i));
		    element = dummy_element;
		    break;
		}

		element.reset(new element_t((yyvsp[0].i), definition));

        if (!problem.element_set.insert(element).second) {
             error ("element number %u is repeated", (yyvsp[0].i));
             element = dummy_element;
             break;
        } 

        if (!element->material)
             element -> material.reset(new material_t);
		element -> material->name = last_material;
	    }
#line 2415 "parser.cpp" /* yacc.c:1646  */
    break;

  case 50:
#line 422 "parser.y" /* yacc.c:1646  */
    {(yyval.i) = (yyvsp[-1].d);}
#line 2421 "parser.cpp" /* yacc.c:1646  */
    break;

  case 53:
#line 434 "parser.y" /* yacc.c:1646  */
    {
             if (element == dummy_element)
                  break;

		unsigned size = int_ptr - int_array;

		if (size != element -> definition -> numnodes) {
             unsigned number = element -> number;
             error ("incorrect number of nodes for element %u", number);
             break;
		}

		for (unsigned i = 1; i <= size; i ++) {
             int nn = int_array[i-1];
             if (nn != 0)
                  element -> node [i].reset(new node_t(nn));
        }
        
	    }
#line 2445 "parser.cpp" /* yacc.c:1646  */
    break;

  case 54:
#line 455 "parser.y" /* yacc.c:1646  */
    {
             last_material = (yyvsp[0].s);
             free((yyvsp[0].s));
             if (!element->material)
                  element->material.reset(new material_t);
             element -> material -> name = last_material;
	    }
#line 2457 "parser.cpp" /* yacc.c:1646  */
    break;

  case 57:
#line 471 "parser.y" /* yacc.c:1646  */
    {
		*int_ptr ++ = (yyvsp[0].i);
	    }
#line 2465 "parser.cpp" /* yacc.c:1646  */
    break;

  case 58:
#line 476 "parser.y" /* yacc.c:1646  */
    {
		*int_ptr ++ = (yyvsp[0].i);
	    }
#line 2473 "parser.cpp" /* yacc.c:1646  */
    break;

  case 59:
#line 481 "parser.y" /* yacc.c:1646  */
    {
		int_ptr = int_array;
		*int_ptr ++ = (yyvsp[0].i);
	    }
#line 2482 "parser.cpp" /* yacc.c:1646  */
    break;

  case 60:
#line 490 "parser.y" /* yacc.c:1646  */
    {
             if ((yyvsp[0].i) > problem.nodes.size())
                  error ("node number %u is illegal", (yyvsp[0].i));
	    }
#line 2491 "parser.cpp" /* yacc.c:1646  */
    break;

  case 61:
#line 499 "parser.y" /* yacc.c:1646  */
    {
		if (element -> numdistributed == 3) {
		    error ("element %u has too many loads", element -> number);
		    break;
		}

		element -> numdistributed ++;
		element -> distributed [element -> numdistributed].reset(new distributed_t((yyvsp[0].s)));
        free((yyvsp[0].s));
	    }
#line 2506 "parser.cpp" /* yacc.c:1646  */
    break;

  case 62:
#line 511 "parser.y" /* yacc.c:1646  */
    {
		if (element -> numdistributed == 3) {
		    error ("element %u has too many loads", element -> number);
		    break;
		}

		element -> numdistributed ++;
		element -> distributed [element -> numdistributed].reset(new distributed_t((yyvsp[0].s)));
        free((yyvsp[0].s));
	    }
#line 2521 "parser.cpp" /* yacc.c:1646  */
    break;

  case 63:
#line 523 "parser.y" /* yacc.c:1646  */
    {
		element -> numdistributed = 1;
		element -> distributed [element -> numdistributed].reset(new distributed_t((yyvsp[0].s)));
        free((yyvsp[0].s));
	    }
#line 2531 "parser.cpp" /* yacc.c:1646  */
    break;

  case 68:
#line 551 "parser.y" /* yacc.c:1646  */
    {
             material.reset(new material_t((yyvsp[0].s)));
             
             if (problem.material_set.count(material) > 0) {
                  error ("material %s is previously defined", (yyvsp[0].s));
                  material = dummy_material;
             } else 
                  problem.material_set.insert(material);
             free((yyvsp[0].s));
	    }
#line 2546 "parser.cpp" /* yacc.c:1646  */
    break;

  case 71:
#line 571 "parser.y" /* yacc.c:1646  */
    {
             material -> color = (yyvsp[0].s);
             free((yyvsp[0].s));
	    }
#line 2555 "parser.cpp" /* yacc.c:1646  */
    break;

  case 72:
#line 577 "parser.y" /* yacc.c:1646  */
    {
		material -> E = (yyvsp[0].d);
	    }
#line 2563 "parser.cpp" /* yacc.c:1646  */
    break;

  case 73:
#line 582 "parser.y" /* yacc.c:1646  */
    {
		material -> Ix = (yyvsp[0].d);
	    }
#line 2571 "parser.cpp" /* yacc.c:1646  */
    break;

  case 74:
#line 587 "parser.y" /* yacc.c:1646  */
    {
		material -> Iy = (yyvsp[0].d);
	    }
#line 2579 "parser.cpp" /* yacc.c:1646  */
    break;

  case 75:
#line 592 "parser.y" /* yacc.c:1646  */
    {
		material -> Iz = (yyvsp[0].d);
	    }
#line 2587 "parser.cpp" /* yacc.c:1646  */
    break;

  case 76:
#line 597 "parser.y" /* yacc.c:1646  */
    {
		material -> A = (yyvsp[0].d);
	    }
#line 2595 "parser.cpp" /* yacc.c:1646  */
    break;

  case 77:
#line 602 "parser.y" /* yacc.c:1646  */
    {
		material -> J = (yyvsp[0].d);
	    }
#line 2603 "parser.cpp" /* yacc.c:1646  */
    break;

  case 78:
#line 607 "parser.y" /* yacc.c:1646  */
    {
		material -> G = (yyvsp[0].d);
	    }
#line 2611 "parser.cpp" /* yacc.c:1646  */
    break;

  case 79:
#line 612 "parser.y" /* yacc.c:1646  */
    {
		material -> t = (yyvsp[0].d);
	    }
#line 2619 "parser.cpp" /* yacc.c:1646  */
    break;

  case 80:
#line 617 "parser.y" /* yacc.c:1646  */
    {
		material -> rho = (yyvsp[0].d);
	    }
#line 2627 "parser.cpp" /* yacc.c:1646  */
    break;

  case 81:
#line 622 "parser.y" /* yacc.c:1646  */
    {
		material -> nu = (yyvsp[0].d);
	    }
#line 2635 "parser.cpp" /* yacc.c:1646  */
    break;

  case 82:
#line 627 "parser.y" /* yacc.c:1646  */
    {
		material -> kappa = (yyvsp[0].d);
	    }
#line 2643 "parser.cpp" /* yacc.c:1646  */
    break;

  case 83:
#line 632 "parser.y" /* yacc.c:1646  */
    {
		material -> Rk = (yyvsp[0].d);
	    }
#line 2651 "parser.cpp" /* yacc.c:1646  */
    break;

  case 84:
#line 637 "parser.y" /* yacc.c:1646  */
    {
		material -> Rm = (yyvsp[0].d);
	    }
#line 2659 "parser.cpp" /* yacc.c:1646  */
    break;

  case 85:
#line 642 "parser.y" /* yacc.c:1646  */
    {
                material -> Kx = (yyvsp[0].d);
            }
#line 2667 "parser.cpp" /* yacc.c:1646  */
    break;

  case 86:
#line 647 "parser.y" /* yacc.c:1646  */
    {
                material -> Ky = (yyvsp[0].d);
            }
#line 2675 "parser.cpp" /* yacc.c:1646  */
    break;

  case 87:
#line 652 "parser.y" /* yacc.c:1646  */
    {
                material -> Kz = (yyvsp[0].d);
            }
#line 2683 "parser.cpp" /* yacc.c:1646  */
    break;

  case 88:
#line 657 "parser.y" /* yacc.c:1646  */
    {
                material -> c = (yyvsp[0].d);
            }
#line 2691 "parser.cpp" /* yacc.c:1646  */
    break;

  case 94:
#line 685 "parser.y" /* yacc.c:1646  */
    {
             load.reset(new distributed_t((yyvsp[0].s), 0));
             
             if (!problem.distributed_set.insert(load).second) {
                  error ("load %s is previously defined", (yyvsp[0].s));
                  load = dummy_load;
             }
             free((yyvsp[0].s));
	    }
#line 2705 "parser.cpp" /* yacc.c:1646  */
    break;

  case 97:
#line 705 "parser.y" /* yacc.c:1646  */
    {
             load -> color = (yyvsp[0].s);
             free((yyvsp[0].s));
	    }
#line 2714 "parser.cpp" /* yacc.c:1646  */
    break;

  case 98:
#line 711 "parser.y" /* yacc.c:1646  */
    {
		load -> direction = (Direction) (yyvsp[0].i);
	    }
#line 2722 "parser.cpp" /* yacc.c:1646  */
    break;

  case 99:
#line 716 "parser.y" /* yacc.c:1646  */
    {
		unsigned i;
		unsigned size;


		if (load == dummy_load)
		    break;

		size = pair_ptr - pair_array;

        load -> value.resize(size);

		for (i = 1; i <= size; i ++)
		    load -> value [i] = pair_array [i - 1];
	    }
#line 2742 "parser.cpp" /* yacc.c:1646  */
    break;

  case 101:
#line 738 "parser.y" /* yacc.c:1646  */
    {
		*pair_ptr ++ = (yyvsp[0].p);
	    }
#line 2750 "parser.cpp" /* yacc.c:1646  */
    break;

  case 102:
#line 743 "parser.y" /* yacc.c:1646  */
    {
		*pair_ptr ++ = (yyvsp[0].p);
	    }
#line 2758 "parser.cpp" /* yacc.c:1646  */
    break;

  case 103:
#line 748 "parser.y" /* yacc.c:1646  */
    {
		pair_ptr = pair_array;
		*pair_ptr ++ = (yyvsp[0].p);
	    }
#line 2767 "parser.cpp" /* yacc.c:1646  */
    break;

  case 104:
#line 757 "parser.y" /* yacc.c:1646  */
    {
             if ((yyvsp[-3].i) < 1 || (yyvsp[-3].i) > problem.nodes.size())
                  error ("node number %u is illegal", (yyvsp[-3].i));

		(yyval.p).node = (yyvsp[-3].i);
		(yyval.p).magnitude = (yyvsp[-1].d);
	    }
#line 2779 "parser.cpp" /* yacc.c:1646  */
    break;

  case 105:
#line 766 "parser.y" /* yacc.c:1646  */
    {
             if ((yyvsp[-2].i) < 1 || (yyvsp[-2].i) > problem.nodes.size())
                  error ("node number %u is illegal", (yyvsp[-2].i));
             
		(yyval.p).node = (yyvsp[-2].i);
		(yyval.p).magnitude = (yyvsp[-1].d);
	    }
#line 2791 "parser.cpp" /* yacc.c:1646  */
    break;

  case 110:
#line 796 "parser.y" /* yacc.c:1646  */
    {
             force.reset(new force_t((yyvsp[0].s)));
             
             if (!problem.force_set.insert(force).second) {
                  error ("force %s is previously defined", (yyvsp[0].s));
                  force = dummy_force;
             }
             free((yyvsp[0].s));
	    }
#line 2805 "parser.cpp" /* yacc.c:1646  */
    break;

  case 113:
#line 816 "parser.y" /* yacc.c:1646  */
    {
             force -> color = (yyvsp[0].s);
             free((yyvsp[0].s));
	    }
#line 2814 "parser.cpp" /* yacc.c:1646  */
    break;

  case 114:
#line 822 "parser.y" /* yacc.c:1646  */
    {
		AssignForce (force, Fx, InCore, copy_input (0));
	    }
#line 2822 "parser.cpp" /* yacc.c:1646  */
    break;

  case 115:
#line 827 "parser.y" /* yacc.c:1646  */
    {
		AssignForce (force, Fy, InCore, copy_input (0));
	    }
#line 2830 "parser.cpp" /* yacc.c:1646  */
    break;

  case 116:
#line 832 "parser.y" /* yacc.c:1646  */
    {
		AssignForce (force, Fz, InCore, copy_input (0));
	    }
#line 2838 "parser.cpp" /* yacc.c:1646  */
    break;

  case 117:
#line 837 "parser.y" /* yacc.c:1646  */
    {
		AssignForce (force, Mx, InCore, copy_input (0));
	    }
#line 2846 "parser.cpp" /* yacc.c:1646  */
    break;

  case 118:
#line 842 "parser.y" /* yacc.c:1646  */
    {
		AssignForce (force, My, InCore, copy_input (0));
	    }
#line 2854 "parser.cpp" /* yacc.c:1646  */
    break;

  case 119:
#line 847 "parser.y" /* yacc.c:1646  */
    {
		AssignForce (force, Mz, InCore, copy_input (0));
	    }
#line 2862 "parser.cpp" /* yacc.c:1646  */
    break;

  case 120:
#line 852 "parser.y" /* yacc.c:1646  */
    {
		AssignSpectrum (force, Fx, InCore, copy_input (0));
	    }
#line 2870 "parser.cpp" /* yacc.c:1646  */
    break;

  case 121:
#line 857 "parser.y" /* yacc.c:1646  */
    {
		AssignSpectrum (force, Fy, InCore, copy_input (0));
	    }
#line 2878 "parser.cpp" /* yacc.c:1646  */
    break;

  case 122:
#line 862 "parser.y" /* yacc.c:1646  */
    {
		AssignSpectrum (force, Fz, InCore, copy_input (0));
	    }
#line 2886 "parser.cpp" /* yacc.c:1646  */
    break;

  case 123:
#line 867 "parser.y" /* yacc.c:1646  */
    {
		AssignSpectrum (force, Mx, InCore, copy_input (0));
	    }
#line 2894 "parser.cpp" /* yacc.c:1646  */
    break;

  case 124:
#line 872 "parser.y" /* yacc.c:1646  */
    {
		AssignSpectrum (force, My, InCore, copy_input (0));
	    }
#line 2902 "parser.cpp" /* yacc.c:1646  */
    break;

  case 125:
#line 877 "parser.y" /* yacc.c:1646  */
    {
		AssignSpectrum (force, Mz, InCore, copy_input (0));
	    }
#line 2910 "parser.cpp" /* yacc.c:1646  */
    break;

  case 131:
#line 905 "parser.y" /* yacc.c:1646  */
    {
             constraint.reset(new constraint_t((yyvsp[0].s)));
             
             if (!problem.constraint_set.insert(constraint).second) {
                  error ("constraint %s is previously defined", (yyvsp[0].s));
                  constraint = dummy_constraint;
             }

             free((yyvsp[0].s));
	    }
#line 2925 "parser.cpp" /* yacc.c:1646  */
    break;

  case 134:
#line 926 "parser.y" /* yacc.c:1646  */
    {
             constraint -> color = (yyvsp[0].s);
             free((yyvsp[0].s));
	    }
#line 2934 "parser.cpp" /* yacc.c:1646  */
    break;

  case 135:
#line 932 "parser.y" /* yacc.c:1646  */
    {
                if ((yyvsp[0].c) == VariableExpression)
                   AssignConstraint (constraint, Tx, InCore, copy_input(0), 1);
                else  {
                   AssignConstraint (constraint, Tx, NULL, NULL, (yyvsp[0].c));
		   copy_input (0);
		}
	    }
#line 2947 "parser.cpp" /* yacc.c:1646  */
    break;

  case 136:
#line 942 "parser.y" /* yacc.c:1646  */
    {
                if ((yyvsp[0].c) == VariableExpression)
                   AssignConstraint (constraint, Ty, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Ty, NULL, NULL, (yyvsp[0].c));
		   copy_input (0);
		}
	    }
#line 2960 "parser.cpp" /* yacc.c:1646  */
    break;

  case 137:
#line 952 "parser.y" /* yacc.c:1646  */
    {
                if ((yyvsp[0].c) == VariableExpression)
                   AssignConstraint (constraint, Tz, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Tz, NULL, NULL, (yyvsp[0].c));
		   copy_input (0);
		}
	    }
#line 2973 "parser.cpp" /* yacc.c:1646  */
    break;

  case 138:
#line 962 "parser.y" /* yacc.c:1646  */
    {
                if ((yyvsp[0].c) == VariableExpression)
                   AssignConstraint (constraint, Rx, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Rx, NULL, NULL, (yyvsp[0].c));
		   copy_input (0);
		}
	    }
#line 2986 "parser.cpp" /* yacc.c:1646  */
    break;

  case 139:
#line 972 "parser.y" /* yacc.c:1646  */
    {
                if ((yyvsp[0].c) == VariableExpression)
                   AssignConstraint (constraint, Ry, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Ry, NULL, NULL, (yyvsp[0].c));
		   copy_input (0);
		}
	    }
#line 2999 "parser.cpp" /* yacc.c:1646  */
    break;

  case 140:
#line 982 "parser.y" /* yacc.c:1646  */
    {
                if ((yyvsp[0].c) == VariableExpression)
                   AssignConstraint (constraint, Rz, InCore, copy_input(0), 1);
                else {
                   AssignConstraint (constraint, Rz, NULL, NULL, (yyvsp[0].c));
		   copy_input (0);
		}
	    }
#line 3012 "parser.cpp" /* yacc.c:1646  */
    break;

  case 141:
#line 992 "parser.y" /* yacc.c:1646  */
    {
                constraint -> ix [Tx] = (yyvsp[0].d);
            }
#line 3020 "parser.cpp" /* yacc.c:1646  */
    break;

  case 142:
#line 997 "parser.y" /* yacc.c:1646  */
    {
                constraint -> ix [Ty] = (yyvsp[0].d);
            }
#line 3028 "parser.cpp" /* yacc.c:1646  */
    break;

  case 143:
#line 1002 "parser.y" /* yacc.c:1646  */
    {
                constraint -> ix [Tz] = (yyvsp[0].d);
            }
#line 3036 "parser.cpp" /* yacc.c:1646  */
    break;

  case 144:
#line 1007 "parser.y" /* yacc.c:1646  */
    {
                constraint -> ix [Rx] = (yyvsp[0].d);
            }
#line 3044 "parser.cpp" /* yacc.c:1646  */
    break;

  case 145:
#line 1012 "parser.y" /* yacc.c:1646  */
    {
                constraint -> ix [Ry] = (yyvsp[0].d);
            }
#line 3052 "parser.cpp" /* yacc.c:1646  */
    break;

  case 146:
#line 1017 "parser.y" /* yacc.c:1646  */
    {
                constraint -> ix [Rz] = (yyvsp[0].d);
            }
#line 3060 "parser.cpp" /* yacc.c:1646  */
    break;

  case 147:
#line 1022 "parser.y" /* yacc.c:1646  */
    {
		constraint -> vx [Tx] = (yyvsp[0].d);
	    }
#line 3068 "parser.cpp" /* yacc.c:1646  */
    break;

  case 148:
#line 1027 "parser.y" /* yacc.c:1646  */
    {
		constraint -> vx [Ty] = (yyvsp[0].d);
	    }
#line 3076 "parser.cpp" /* yacc.c:1646  */
    break;

  case 149:
#line 1032 "parser.y" /* yacc.c:1646  */
    {
		constraint -> vx [Tz] = (yyvsp[0].d);
	    }
#line 3084 "parser.cpp" /* yacc.c:1646  */
    break;

  case 150:
#line 1037 "parser.y" /* yacc.c:1646  */
    {
		constraint -> ax [Tx] = (yyvsp[0].d);
	    }
#line 3092 "parser.cpp" /* yacc.c:1646  */
    break;

  case 151:
#line 1042 "parser.y" /* yacc.c:1646  */
    {
		constraint -> ax [Ty] = (yyvsp[0].d);
	    }
#line 3100 "parser.cpp" /* yacc.c:1646  */
    break;

  case 152:
#line 1047 "parser.y" /* yacc.c:1646  */
    {
		constraint -> ax [Tz] = (yyvsp[0].d);
	    }
#line 3108 "parser.cpp" /* yacc.c:1646  */
    break;

  case 154:
#line 1057 "parser.y" /* yacc.c:1646  */
    {
		(yyval.c) = VariableExpression;
	    }
#line 3116 "parser.cpp" /* yacc.c:1646  */
    break;

  case 155:
#line 1062 "parser.y" /* yacc.c:1646  */
    {
		(yyval.c) = (yyvsp[0].i);
	    }
#line 3124 "parser.cpp" /* yacc.c:1646  */
    break;

  case 156:
#line 1070 "parser.y" /* yacc.c:1646  */
    {
		(yyval.c) = VariableExpression;
	    }
#line 3132 "parser.cpp" /* yacc.c:1646  */
    break;

  case 157:
#line 1075 "parser.y" /* yacc.c:1646  */
    {
		(yyval.c) = (yyvsp[0].i);
	    }
#line 3140 "parser.cpp" /* yacc.c:1646  */
    break;

  case 158:
#line 1080 "parser.y" /* yacc.c:1646  */
    {
		(yyval.c) = (yyvsp[0].i);
	    }
#line 3148 "parser.cpp" /* yacc.c:1646  */
    break;

  case 163:
#line 1104 "parser.y" /* yacc.c:1646  */
    {
             loadcase.reset(new loadcase_t((yyvsp[0].s)));
             
             if (!problem.loadcase_set.insert(loadcase).second) {
                  error ("loadcase %s is previously defined", (yyvsp[0].s));
                  loadcase = dummy_loadcase;
             }
             
             free((yyvsp[0].s));
        }
#line 3163 "parser.cpp" /* yacc.c:1646  */
    break;

  case 166:
#line 1123 "parser.y" /* yacc.c:1646  */
    {
             if (loadcase == dummy_loadcase)
                  break;
             
             unsigned size = case_ptr - case_array;
             
             loadcase->nodes.resize(size);
             loadcase->forces.resize(size);
             
             for (unsigned i = 1; i <= size; i ++) {
                  loadcase -> nodes [i].reset(new node_t(case_array [i - 1].noe));
                  loadcase -> forces [i].reset(new force_t(case_array [i - 1].fol));
             }
	    }
#line 3182 "parser.cpp" /* yacc.c:1646  */
    break;

  case 167:
#line 1139 "parser.y" /* yacc.c:1646  */
    {
             if (loadcase == dummy_loadcase)
                  break;

             unsigned size = case_ptr - case_array;
             
             loadcase->elements.resize(size);
             loadcase->loads.resize(size);
             
             for (unsigned i = 1; i <= size; i ++) {
                  loadcase -> elements [i].reset(new element_t(case_array [i - 1].noe));
                  loadcase -> loads [i].reset(new distributed_t(case_array [i - 1].fol));
             }
	    }
#line 3201 "parser.cpp" /* yacc.c:1646  */
    break;

  case 169:
#line 1160 "parser.y" /* yacc.c:1646  */
    {
		*case_ptr ++ = (yyvsp[0].cp);
	    }
#line 3209 "parser.cpp" /* yacc.c:1646  */
    break;

  case 170:
#line 1165 "parser.y" /* yacc.c:1646  */
    {
		*case_ptr ++ = (yyvsp[0].cp);
	    }
#line 3217 "parser.cpp" /* yacc.c:1646  */
    break;

  case 171:
#line 1170 "parser.y" /* yacc.c:1646  */
    {
		case_ptr = case_array;
		*case_ptr ++ = (yyvsp[0].cp);
	    }
#line 3226 "parser.cpp" /* yacc.c:1646  */
    break;

  case 172:
#line 1179 "parser.y" /* yacc.c:1646  */
    {
             if ((yyvsp[-3].i) < 1 || (yyvsp[-3].i) > problem.nodes.size())
                  error ("node number %u is illegal", (yyvsp[-3].i));

		(yyval.cp).noe = (yyvsp[-3].i);
		(yyval.cp).fol = (yyvsp[-1].s);
	    }
#line 3238 "parser.cpp" /* yacc.c:1646  */
    break;

  case 173:
#line 1188 "parser.y" /* yacc.c:1646  */
    {
             if ((yyvsp[-2].i) < 1 || (yyvsp[-2].i) > problem.nodes.size())
                  error ("node number %u is illegal", (yyvsp[-2].i));

		(yyval.cp).noe = (yyvsp[-2].i);
		(yyval.cp).fol = (yyvsp[-1].s);
	    }
#line 3250 "parser.cpp" /* yacc.c:1646  */
    break;

  case 177:
#line 1212 "parser.y" /* yacc.c:1646  */
    {
		analysis.alpha = (yyvsp[0].d);
	    }
#line 3258 "parser.cpp" /* yacc.c:1646  */
    break;

  case 178:
#line 1217 "parser.y" /* yacc.c:1646  */
    {
		analysis.beta = (yyvsp[0].d);
	    }
#line 3266 "parser.cpp" /* yacc.c:1646  */
    break;

  case 179:
#line 1222 "parser.y" /* yacc.c:1646  */
    {
		analysis.gamma = (yyvsp[0].d);
	    }
#line 3274 "parser.cpp" /* yacc.c:1646  */
    break;

  case 180:
#line 1227 "parser.y" /* yacc.c:1646  */
    {
                analysis.Rk = (yyvsp[0].d);
            }
#line 3282 "parser.cpp" /* yacc.c:1646  */
    break;

  case 181:
#line 1232 "parser.y" /* yacc.c:1646  */
    {
                analysis.Rm = (yyvsp[0].d);
            }
#line 3290 "parser.cpp" /* yacc.c:1646  */
    break;

  case 182:
#line 1237 "parser.y" /* yacc.c:1646  */
    {
		analysis.start = (yyvsp[0].d);
	    }
#line 3298 "parser.cpp" /* yacc.c:1646  */
    break;

  case 183:
#line 1242 "parser.y" /* yacc.c:1646  */
    {
		analysis.step = (yyvsp[0].d);
	    }
#line 3306 "parser.cpp" /* yacc.c:1646  */
    break;

  case 184:
#line 1247 "parser.y" /* yacc.c:1646  */
    {
		analysis.stop = (yyvsp[0].d);
	    }
#line 3314 "parser.cpp" /* yacc.c:1646  */
    break;

  case 185:
#line 1252 "parser.y" /* yacc.c:1646  */
    {
		analysis.iterations = (yyvsp[0].i);
	    }
#line 3322 "parser.cpp" /* yacc.c:1646  */
    break;

  case 186:
#line 1257 "parser.y" /* yacc.c:1646  */
    {
		analysis.load_steps = (yyvsp[0].i);
	    }
#line 3330 "parser.cpp" /* yacc.c:1646  */
    break;

  case 187:
#line 1262 "parser.y" /* yacc.c:1646  */
    {
		analysis.relaxation = (yyvsp[0].d);
	    }
#line 3338 "parser.cpp" /* yacc.c:1646  */
    break;

  case 188:
#line 1267 "parser.y" /* yacc.c:1646  */
    {
		analysis.tolerance = (yyvsp[0].d);
	    }
#line 3346 "parser.cpp" /* yacc.c:1646  */
    break;

  case 189:
#line 1272 "parser.y" /* yacc.c:1646  */
    {
                analysis.input_dof = (yyvsp[0].i);
            }
#line 3354 "parser.cpp" /* yacc.c:1646  */
    break;

  case 190:
#line 1277 "parser.y" /* yacc.c:1646  */
    {
                 analysis.input_node.reset(new node_t((yyvsp[0].i)));
            }
#line 3362 "parser.cpp" /* yacc.c:1646  */
    break;

  case 191:
#line 1282 "parser.y" /* yacc.c:1646  */
    {
             analysis.nodes.resize(int_ptr - int_array);
             
             for (unsigned i = 1; i <= analysis.nodes.size(); i ++)
                  analysis.nodes [i].reset(new node_t(int_array [i - 1]));
	    }
#line 3373 "parser.cpp" /* yacc.c:1646  */
    break;

  case 192:
#line 1290 "parser.y" /* yacc.c:1646  */
    {
		int i;


		analysis.numdofs = 0;
		for (i = 1; i <= 6; i ++)
		    if (analysis.dofs [i])
			analysis.dofs [++ analysis.numdofs] = i;
	    }
#line 3387 "parser.cpp" /* yacc.c:1646  */
    break;

  case 193:
#line 1301 "parser.y" /* yacc.c:1646  */
    {
		analysis.mass_mode = (yyvsp[0].i);
	    }
#line 3395 "parser.cpp" /* yacc.c:1646  */
    break;

  case 194:
#line 1306 "parser.y" /* yacc.c:1646  */
    {
                analysis.gravity [1] = triple_x;
                analysis.gravity [2] = triple_y;
                analysis.gravity [3] = triple_z;
            }
#line 3405 "parser.cpp" /* yacc.c:1646  */
    break;

  case 196:
#line 1318 "parser.y" /* yacc.c:1646  */
    {
		*int_ptr ++ = (yyvsp[0].i);
	    }
#line 3413 "parser.cpp" /* yacc.c:1646  */
    break;

  case 197:
#line 1323 "parser.y" /* yacc.c:1646  */
    {
		*int_ptr ++ = (yyvsp[0].i);
	    }
#line 3421 "parser.cpp" /* yacc.c:1646  */
    break;

  case 198:
#line 1328 "parser.y" /* yacc.c:1646  */
    {
		int_ptr = int_array;
	    }
#line 3429 "parser.cpp" /* yacc.c:1646  */
    break;

  case 199:
#line 1336 "parser.y" /* yacc.c:1646  */
    {
		analysis.dofs [(yyvsp[0].i)] = 1;
	    }
#line 3437 "parser.cpp" /* yacc.c:1646  */
    break;

  case 200:
#line 1341 "parser.y" /* yacc.c:1646  */
    {
		analysis.dofs [(yyvsp[0].i)] = 1;
	    }
#line 3445 "parser.cpp" /* yacc.c:1646  */
    break;

  case 201:
#line 1346 "parser.y" /* yacc.c:1646  */
    {
		int i;


		for (i = 1; i <= 6; i ++)
		     analysis.dofs [i] = 0;
	    }
#line 3457 "parser.cpp" /* yacc.c:1646  */
    break;

  case 202:
#line 1358 "parser.y" /* yacc.c:1646  */
    {
		triple_x = (yyvsp[-4].d);
		triple_y = (yyvsp[-2].d);
		triple_z = (yyvsp[-1].d);
	    }
#line 3467 "parser.cpp" /* yacc.c:1646  */
    break;

  case 203:
#line 1368 "parser.y" /* yacc.c:1646  */
    {
		(yyval.d) = (yyvsp[0].d);
	    }
#line 3475 "parser.cpp" /* yacc.c:1646  */
    break;

  case 204:
#line 1373 "parser.y" /* yacc.c:1646  */
    {
                (yyval.d) = 0.0;
            }
#line 3483 "parser.cpp" /* yacc.c:1646  */
    break;

  case 208:
#line 1394 "parser.y" /* yacc.c:1646  */
    {
		appearance.node_numbers = (yyvsp[0].i);
	    }
#line 3491 "parser.cpp" /* yacc.c:1646  */
    break;

  case 209:
#line 1399 "parser.y" /* yacc.c:1646  */
    {
		appearance.element_numbers = (yyvsp[0].i);
	    }
#line 3499 "parser.cpp" /* yacc.c:1646  */
    break;

  case 210:
#line 1404 "parser.y" /* yacc.c:1646  */
    {
		appearance.snap = (yyvsp[0].i);
	    }
#line 3507 "parser.cpp" /* yacc.c:1646  */
    break;

  case 211:
#line 1409 "parser.y" /* yacc.c:1646  */
    {
		appearance.grid = (yyvsp[0].i);
	    }
#line 3515 "parser.cpp" /* yacc.c:1646  */
    break;

  case 212:
#line 1414 "parser.y" /* yacc.c:1646  */
    {
		appearance.snap_size = (yyvsp[0].d);
	    }
#line 3523 "parser.cpp" /* yacc.c:1646  */
    break;

  case 213:
#line 1419 "parser.y" /* yacc.c:1646  */
    {
		appearance.grid_size = (yyvsp[0].d);
	    }
#line 3531 "parser.cpp" /* yacc.c:1646  */
    break;

  case 214:
#line 1424 "parser.y" /* yacc.c:1646  */
    {
		appearance.x_min = (yyvsp[0].d);
	    }
#line 3539 "parser.cpp" /* yacc.c:1646  */
    break;

  case 215:
#line 1429 "parser.y" /* yacc.c:1646  */
    {
		appearance.x_max = (yyvsp[0].d);
	    }
#line 3547 "parser.cpp" /* yacc.c:1646  */
    break;

  case 216:
#line 1434 "parser.y" /* yacc.c:1646  */
    {
		appearance.y_min = (yyvsp[0].d);
	    }
#line 3555 "parser.cpp" /* yacc.c:1646  */
    break;

  case 217:
#line 1439 "parser.y" /* yacc.c:1646  */
    {
		appearance.y_max = (yyvsp[0].d);
	    }
#line 3563 "parser.cpp" /* yacc.c:1646  */
    break;

  case 218:
#line 1444 "parser.y" /* yacc.c:1646  */
    {
		appearance.x_pos = (yyvsp[0].d);
	    }
#line 3571 "parser.cpp" /* yacc.c:1646  */
    break;

  case 219:
#line 1449 "parser.y" /* yacc.c:1646  */
    {
		appearance.y_pos = (yyvsp[0].d);
	    }
#line 3579 "parser.cpp" /* yacc.c:1646  */
    break;

  case 220:
#line 1454 "parser.y" /* yacc.c:1646  */
    {
		appearance.width = (yyvsp[0].d);
	    }
#line 3587 "parser.cpp" /* yacc.c:1646  */
    break;

  case 221:
#line 1459 "parser.y" /* yacc.c:1646  */
    {
		appearance.height = (yyvsp[0].d);
	    }
#line 3595 "parser.cpp" /* yacc.c:1646  */
    break;

  case 222:
#line 1464 "parser.y" /* yacc.c:1646  */
    {
		appearance.scale = (yyvsp[0].d);
	    }
#line 3603 "parser.cpp" /* yacc.c:1646  */
    break;

  case 223:
#line 1469 "parser.y" /* yacc.c:1646  */
    {
             appearance.node_color = (yyvsp[0].s);
	    }
#line 3611 "parser.cpp" /* yacc.c:1646  */
    break;

  case 224:
#line 1474 "parser.y" /* yacc.c:1646  */
    {
             appearance.element_color = (yyvsp[0].s);
	    }
#line 3619 "parser.cpp" /* yacc.c:1646  */
    break;

  case 225:
#line 1479 "parser.y" /* yacc.c:1646  */
    {
             appearance.label_font = (yyvsp[0].s);
	    }
#line 3627 "parser.cpp" /* yacc.c:1646  */
    break;

  case 226:
#line 1484 "parser.y" /* yacc.c:1646  */
    {
             appearance.tool_color = (yyvsp[0].s);
	    }
#line 3635 "parser.cpp" /* yacc.c:1646  */
    break;

  case 227:
#line 1489 "parser.y" /* yacc.c:1646  */
    {
             appearance.tool_font = (yyvsp[0].s);
	    }
#line 3643 "parser.cpp" /* yacc.c:1646  */
    break;

  case 229:
#line 1504 "parser.y" /* yacc.c:1646  */
    {
             /* figure_size = 0; */
	    }
#line 3651 "parser.cpp" /* yacc.c:1646  */
    break;

  case 233:
#line 1523 "parser.y" /* yacc.c:1646  */
    {
             FigInfo figure;
             figure.type = (yyvsp[0].i);
             figure.x = 0;
             figure.y = 0;
             figure.width = 0;
             figure.height = 0;
             figure.start = 0;
             figure.length = 0;
             figure.points.clear();
             figure.font.clear();
             figure.text.clear();
             figure.color.clear();
             appearance.figures.push_back(figure);
	    }
#line 3671 "parser.cpp" /* yacc.c:1646  */
    break;

  case 236:
#line 1549 "parser.y" /* yacc.c:1646  */
    {
		figure -> x = (yyvsp[0].d);
	    }
#line 3679 "parser.cpp" /* yacc.c:1646  */
    break;

  case 237:
#line 1554 "parser.y" /* yacc.c:1646  */
    {
		figure -> y = (yyvsp[0].d);
	    }
#line 3687 "parser.cpp" /* yacc.c:1646  */
    break;

  case 238:
#line 1559 "parser.y" /* yacc.c:1646  */
    {
		figure -> width = (yyvsp[0].d);
	    }
#line 3695 "parser.cpp" /* yacc.c:1646  */
    break;

  case 239:
#line 1564 "parser.y" /* yacc.c:1646  */
    {
		figure -> height = (yyvsp[0].d);
	    }
#line 3703 "parser.cpp" /* yacc.c:1646  */
    break;

  case 240:
#line 1569 "parser.y" /* yacc.c:1646  */
    {
		figure -> start = (yyvsp[0].d);
	    }
#line 3711 "parser.cpp" /* yacc.c:1646  */
    break;

  case 241:
#line 1574 "parser.y" /* yacc.c:1646  */
    {
		figure -> length = (yyvsp[0].d);
	    }
#line 3719 "parser.cpp" /* yacc.c:1646  */
    break;

  case 242:
#line 1579 "parser.y" /* yacc.c:1646  */
    {
             figure -> text = (yyvsp[0].s);
	    }
#line 3727 "parser.cpp" /* yacc.c:1646  */
    break;

  case 243:
#line 1584 "parser.y" /* yacc.c:1646  */
    {
             figure -> color = (yyvsp[0].s);
	    }
#line 3735 "parser.cpp" /* yacc.c:1646  */
    break;

  case 244:
#line 1589 "parser.y" /* yacc.c:1646  */
    {
             figure -> font = (yyvsp[0].s);
	    }
#line 3743 "parser.cpp" /* yacc.c:1646  */
    break;

  case 246:
#line 1599 "parser.y" /* yacc.c:1646  */
    {
             FigInfoPair fip;
             fip.x = figure_x;
             fip.y = figure_y;
             figure->points.push_back(fip);
	    }
#line 3754 "parser.cpp" /* yacc.c:1646  */
    break;

  case 247:
#line 1607 "parser.y" /* yacc.c:1646  */
    {
             FigInfoPair fip;
             fip.x = figure_x;
             fip.y = figure_y;
             figure->points.push_back(fip);
	    }
#line 3765 "parser.cpp" /* yacc.c:1646  */
    break;

  case 248:
#line 1615 "parser.y" /* yacc.c:1646  */
    {
             /* NO-OP */
	    }
#line 3773 "parser.cpp" /* yacc.c:1646  */
    break;

  case 249:
#line 1623 "parser.y" /* yacc.c:1646  */
    {
		figure_x = (yyvsp[-3].d);
		figure_y = (yyvsp[-1].d);
	    }
#line 3782 "parser.cpp" /* yacc.c:1646  */
    break;

  case 250:
#line 1634 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (HaltOp);
		SetIP (0);
	    }
#line 3791 "parser.cpp" /* yacc.c:1646  */
    break;

  case 251:
#line 1640 "parser.y" /* yacc.c:1646  */
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
	    }
#line 3808 "parser.cpp" /* yacc.c:1646  */
    break;

  case 252:
#line 1654 "parser.y" /* yacc.c:1646  */
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
	    }
#line 3825 "parser.cpp" /* yacc.c:1646  */
    break;

  case 256:
#line 1678 "parser.y" /* yacc.c:1646  */
    {
		if ((yyvsp[-3].d) < last_time) {
		    error ("point not in nondecreasing order");
		    table_error = 1;
		    break;
		}

		if (table_count == table_size) {
		    table_size = table_size ? table_size << 1 : 8;
		    if (!Reallocate (table, double, table_size))
			Fatal ("unable to expand table");
		}

		table [table_count ++] = last_time = (yyvsp[-3].d);
		table [table_count ++] = (yyvsp[-1].d);
	    }
#line 3846 "parser.cpp" /* yacc.c:1646  */
    break;

  case 257:
#line 1699 "parser.y" /* yacc.c:1646  */
    {
		copy_input (1);
	    }
#line 3854 "parser.cpp" /* yacc.c:1646  */
    break;

  case 258:
#line 1707 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (HaltOp);
		SetIP (0);
		(yyval.d) = EvalCode (InCore, 0.0);
	    }
#line 3864 "parser.cpp" /* yacc.c:1646  */
    break;

  case 259:
#line 1717 "parser.y" /* yacc.c:1646  */
    {
		int ip = GetIP ( );
		SetIP (ip - (yyvsp[0].i) - 2);
		EmitCode (JmpOp, (yyvsp[0].i));
		SetIP (GetIP ( ) - (yyvsp[-3].i) - 4);
		EmitCode (JzOp, (yyvsp[-3].i) + 2);
		SetIP (ip);
		(yyval.i) = (yyvsp[-6].i) + (yyvsp[-4].i) + (yyvsp[-3].i) + (yyvsp[-1].i) + (yyvsp[0].i);
	    }
#line 3878 "parser.cpp" /* yacc.c:1646  */
    break;

  case 260:
#line 1728 "parser.y" /* yacc.c:1646  */
    {
		int ip = GetIP ( );
		SetIP (ip - (yyvsp[0].i) - 3);
		EmitCode (JnzOp, (yyvsp[0].i) + 1);
		SetIP (ip);
		EmitCode (TestOp);
		(yyval.i) = (yyvsp[-3].i) + (yyvsp[-1].i) + (yyvsp[0].i) + 1;
	    }
#line 3891 "parser.cpp" /* yacc.c:1646  */
    break;

  case 261:
#line 1738 "parser.y" /* yacc.c:1646  */
    {
		int ip = GetIP ( );
		SetIP (ip - (yyvsp[0].i) - 3);
		EmitCode (JzOp, (yyvsp[0].i) + 1);
		SetIP (ip);
		EmitCode (TestOp);
		(yyval.i) = (yyvsp[-3].i) + (yyvsp[-1].i) + (yyvsp[0].i) + 1;
	    }
#line 3904 "parser.cpp" /* yacc.c:1646  */
    break;

  case 262:
#line 1748 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (OrOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3913 "parser.cpp" /* yacc.c:1646  */
    break;

  case 263:
#line 1754 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (XorOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3922 "parser.cpp" /* yacc.c:1646  */
    break;

  case 264:
#line 1760 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (AndOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3931 "parser.cpp" /* yacc.c:1646  */
    break;

  case 265:
#line 1766 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (EqOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3940 "parser.cpp" /* yacc.c:1646  */
    break;

  case 266:
#line 1772 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (NeqOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3949 "parser.cpp" /* yacc.c:1646  */
    break;

  case 267:
#line 1778 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (LtOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3958 "parser.cpp" /* yacc.c:1646  */
    break;

  case 268:
#line 1784 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (GtOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3967 "parser.cpp" /* yacc.c:1646  */
    break;

  case 269:
#line 1790 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (LteqOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3976 "parser.cpp" /* yacc.c:1646  */
    break;

  case 270:
#line 1796 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (GteqOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3985 "parser.cpp" /* yacc.c:1646  */
    break;

  case 271:
#line 1802 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (LsftOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 3994 "parser.cpp" /* yacc.c:1646  */
    break;

  case 272:
#line 1808 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (RsftOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 4003 "parser.cpp" /* yacc.c:1646  */
    break;

  case 273:
#line 1814 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (AddOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 4012 "parser.cpp" /* yacc.c:1646  */
    break;

  case 274:
#line 1820 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (SubOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 4021 "parser.cpp" /* yacc.c:1646  */
    break;

  case 275:
#line 1826 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (MulOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 4030 "parser.cpp" /* yacc.c:1646  */
    break;

  case 276:
#line 1832 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (DivOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 4039 "parser.cpp" /* yacc.c:1646  */
    break;

  case 277:
#line 1838 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (ModOp);
		(yyval.i) = (yyvsp[-2].i) + 1 + (yyvsp[0].i);
	    }
#line 4048 "parser.cpp" /* yacc.c:1646  */
    break;

  case 278:
#line 1844 "parser.y" /* yacc.c:1646  */
    {
		(yyval.i) = (yyvsp[0].i);
	    }
#line 4056 "parser.cpp" /* yacc.c:1646  */
    break;

  case 279:
#line 1849 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (NegOp);
		(yyval.i) = 1 + (yyvsp[0].i);
	    }
#line 4065 "parser.cpp" /* yacc.c:1646  */
    break;

  case 280:
#line 1855 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (NotOp);
		(yyval.i) = 1 + (yyvsp[0].i);
	    }
#line 4074 "parser.cpp" /* yacc.c:1646  */
    break;

  case 281:
#line 1861 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (InvOp);
		(yyval.i) = 1 + (yyvsp[0].i);
	    }
#line 4083 "parser.cpp" /* yacc.c:1646  */
    break;

  case 282:
#line 1867 "parser.y" /* yacc.c:1646  */
    {
		(yyval.i) = (yyvsp[-1].i);
	    }
#line 4091 "parser.cpp" /* yacc.c:1646  */
    break;

  case 283:
#line 1872 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (PushOp, (double) (yyvsp[0].i));
		(yyval.i) = 2;
	    }
#line 4100 "parser.cpp" /* yacc.c:1646  */
    break;

  case 284:
#line 1878 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (PushOp, (yyvsp[0].d));
		(yyval.i) = 2;
	    }
#line 4109 "parser.cpp" /* yacc.c:1646  */
    break;

  case 285:
#line 1884 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (TimeOp);
		(yyval.i) = 1;
	    }
#line 4118 "parser.cpp" /* yacc.c:1646  */
    break;

  case 287:
#line 1895 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (SinOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4127 "parser.cpp" /* yacc.c:1646  */
    break;

  case 288:
#line 1901 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (CosOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4136 "parser.cpp" /* yacc.c:1646  */
    break;

  case 289:
#line 1907 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (TanOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4145 "parser.cpp" /* yacc.c:1646  */
    break;

  case 290:
#line 1913 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (PowOp);
		(yyval.i) = (yyvsp[-3].i) + (yyvsp[-1].i) + 1;
	    }
#line 4154 "parser.cpp" /* yacc.c:1646  */
    break;

  case 291:
#line 1919 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (ExpOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4163 "parser.cpp" /* yacc.c:1646  */
    break;

  case 292:
#line 1925 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (LnOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4172 "parser.cpp" /* yacc.c:1646  */
    break;

  case 293:
#line 1931 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (LogOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4181 "parser.cpp" /* yacc.c:1646  */
    break;

  case 294:
#line 1937 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (SqrtOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4190 "parser.cpp" /* yacc.c:1646  */
    break;

  case 295:
#line 1943 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (HypotOp);
		(yyval.i) = (yyvsp[-3].i) + (yyvsp[-1].i) + 1;
	    }
#line 4199 "parser.cpp" /* yacc.c:1646  */
    break;

  case 296:
#line 1949 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (FloorOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4208 "parser.cpp" /* yacc.c:1646  */
    break;

  case 297:
#line 1955 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (CeilOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4217 "parser.cpp" /* yacc.c:1646  */
    break;

  case 298:
#line 1961 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (FmodOp);
		(yyval.i) = (yyvsp[-3].i) + (yyvsp[-1].i) + 1;
	    }
#line 4226 "parser.cpp" /* yacc.c:1646  */
    break;

  case 299:
#line 1967 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (FabsOp);
		(yyval.i) = (yyvsp[-1].i) + 1;
	    }
#line 4235 "parser.cpp" /* yacc.c:1646  */
    break;

  case 300:
#line 1976 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (JzOp, 0);
		(yyval.i) = 2;
	    }
#line 4244 "parser.cpp" /* yacc.c:1646  */
    break;

  case 301:
#line 1985 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (JmpOp, 0);
		(yyval.i) = 2;
	    }
#line 4253 "parser.cpp" /* yacc.c:1646  */
    break;

  case 302:
#line 1994 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (CopyOp);
		EmitCode (JnzOp, 0);
		EmitCode (PopOp);
		(yyval.i) = 4;
	    }
#line 4264 "parser.cpp" /* yacc.c:1646  */
    break;

  case 303:
#line 2005 "parser.y" /* yacc.c:1646  */
    {
		EmitCode (CopyOp);
		EmitCode (JzOp, 0);
		EmitCode (PopOp);
		(yyval.i) = 4;
	    }
#line 4275 "parser.cpp" /* yacc.c:1646  */
    break;


#line 4279 "parser.cpp" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 2013 "parser.y" /* yacc.c:1906  */


# ifdef YYBYACC
char *felt_suppress_warnings_from_gcc = yysccsid;
# endif
