typedef union {
    int    i;
    double d;
    char  *s;
} YYSTYPE;
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


extern YYSTYPE yylval;
