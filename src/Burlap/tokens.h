typedef union {
    int		ival;
    Address	addr;
    char       *sval;
    descriptor *desc;
    bfloc	loc;
    void       *ptr;
} BFSTYPE;
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

