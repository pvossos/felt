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
 * File:	lexer.h							*
 *									*
 * Description:	This file contains the public function declarations for	*
 *		the lexical analyzer.  Note that these declarations	*
 *		already reflect the changing of "yy" to "bf".		*
 ************************************************************************/

# ifndef _LEXER_H
# define _LEXER_H

typedef struct {
    unsigned short line;		/* line number of instruction */
    unsigned short file;		/* file number of instruction */
} burlap_yyloc;

/*#include "y.tab.h"*/

extern int   line_num;			/* line number during parsing */
extern int   file_num;			/* file number during parsing */
extern char *file_name;			/* file name during parsing   */
extern int   interactive;		/* interactive lexer	      */

/*int burlap_yylex(YYSTYPE *yylval);*/

void burlap_yyerror(const char *message);

int burlap_yyinclude(char *file);

# endif /* _LEXER_H */
