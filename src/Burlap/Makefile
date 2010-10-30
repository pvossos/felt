#    This file is part of the FElt finite element analysis package.
#    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson
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
CXXFLAGS = $(CXXOPTS) -I. -I$(TOPDIR)/include -DEXECDEBUG
LEX	= flex
LFLAGS  = -i -Pburlap_yy
LDFLAGS	= $(LDOPTS)
LIBS	= $(LIBFELT) $(LIBELT) $(LIBMTX) $(READLIBS) -lm
OBJS	= apply.o arithmetic.o array.o assignment.o burlap.o\
	  codegen.o coerce.o control.o debug.o descriptor.o error.o\
	  execute.o fefunc.o felt.o field.o function.o globals.o help.o\
	  interactive.o lexer.o literal.o location.o loop.o mathfunc.o\
	  matrixfunc.o miscfunc.o parser.o pathsearch.o predicate.o\
	  relational.o symbol.o trap.o vector.o write.o
PROG	= burlap
SRCS	= lexer.c parser.c
YACC	= bison
YFLAGS	= -d -y -pburlap_yy

CURDIR	= ./src/Burlap
TOPDIR	= ../..

include $(TOPDIR)/etc/Makefile.bin


interactive.o:	interactive.c
		$(CC) $(CFLAGS) $(READLINE) -c interactive.c

help.o:		help.c helptab.h
