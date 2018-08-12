/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

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
#line 114 "parser.y" /* yacc.c:1909  */

    int       i;
    double    d;
    char     *s;
    Pair      p;
    CasePair  cp;
    char      c;

#line 367 "parser.hpp" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE felt_yylval;

int felt_yyparse (void);

#endif /* !YY_FELT_YY_PARSER_HPP_INCLUDED  */
