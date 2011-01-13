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

CFLAGS	= $(CCOPTS) -I$(TOPDIR)/include
LEX	= flex
LFLAGS	= -i
LIB	= libfelt.a
OBJS	= Tree.o code.o definition.o detail.o draw.o fe.o file.o initialize.o\
	  lexer.o modal.o nonlinear.o objects.o parser.o problem.o renumber.o\
	  results.o rosenbrock.o spectral.o transient.o
SRCS	= lexer.c parser.c
YACC	= bison
YFLAGS	= -d -y

CURDIR	= ./lib/Felt
TOPDIR	= ../..

include $(TOPDIR)/etc/Makefile.lib


problem.o:	problem.c
		$(CC) $(CFLAGS) -DLIBDIR=\"$(DESTLIB)\" -DCPP=\"$(CPP)\" -c problem.c

initialize.o:	initialize.c
		$(CC) $(CFLAGS) $(CONTRIBFLAGS) -c initialize.c
