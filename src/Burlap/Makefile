#    This file is part of the FElt finite element analysis package.
#    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

CFLAGS	= $(CCOPTS) -I. -I$(TOPDIR)/include -DEXECDEBUG
LEX	= flex
LDFLAGS	= $(LDOPTS)
LIBS	= $(LIBFELT) $(LIBELT) $(LIBMTX) $(READLIBS) -lm
OBJS	= apply.o arithmetic.o array.o assignment.o burlap.o\
	  codegen.o coerce.o control.o debug.o descriptor.o error.o\
	  execute.o fefunc.o felt.o field.o function.o globals.o help.o\
	  interactive.o lexer.o literal.o location.o loop.o mathfunc.o\
	  matrixfunc.o miscfunc.o parser.o pathsearch.o predicate.o\
	  relational.o symbol.o trap.o vector.o write.o xmalloc.o
PROG	= burlap
SRCS	= lexer.c parser.c
YACC	= bison
YFLAGS	= -d -y

CURDIR	= ./src/Burlap
TOPDIR	= ../..

include $(TOPDIR)/etc/Makefile.bin


interactive.o:	interactive.c
		$(CC) $(CFLAGS) $(READLINE) -c interactive.c

parser.c:	parser.y
		$(YACC) $(YFLAGS) parser.y
		sed -e s/yy/bf/g -e s/YY/BF/g y.tab.c > parser.c
		sed -e s/yy/bf/g -e s/YY/BF/g y.tab.h > tokens.h
		rm -f y.tab.c y.tab.h

lexer.c:	lexer.l
		$(LEX) $(LFLAGS) lexer.l
		sed -e s/yy/bf/g -e s/YY/BF/g lex.yy.c > lexer.c
		rm -f lex.yy.c

help.o:		help.c helptab.h
