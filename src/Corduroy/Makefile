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
CXXFLAGS = $(CXXOPTS) -I$(TOPDIR)/include
LEX	= flex
LDFLAGS	= $(LDOPTS)
LFLAGS	= -i -Pcord_yy
LIBS	= $(LIBGEN) $(LIBFELT) $(LIBELT) $(LIBMTX) -lm
OBJS	= corduroy.o debug.o error.o generator.o lexer.o parser.o
PROG	= corduroy
SRCS	= lexer.c parser.c
YACC	= bison
YFLAGS	= -d -y -pcord_yy

CURDIR	= ./src/Corduroy
TOPDIR	= ../..

include $(TOPDIR)/etc/Makefile.bin

generator.o:	generator.cpp
		$(CXX) $(CXXFLAGS) -DLIBDIR=\"$(DESTLIB)\" -DCPP=\"$(CPP)\" -c generator.cpp

