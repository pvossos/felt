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

CFLAGS	= $(CCOPTS) -I$(X11INC) -I$(TOPDIR)/include
LDFLAGS	= $(LDOPTS) -L$(X11LIB)
LIBS	= $(LIBWIDGETS) $(LIBGEN) $(LIBFELT) $(LIBELT) $(LIBMTX)\
          -lGLw -lGLU -lGL -lXaw3d -lXmu -lXt -lSM -lICE -lXext -lX11 -lm 
#	  /usr/X11R6/lib/libXaw3d.a /usr/X11R6/lib/libXmu.so.6.0 \
#	  /usr/X11R6/lib/libXt.so.6.0 /usr/X11R6/lib/libSM.so.6.0 \
#	  /usr/X11R6/lib/libICE.so.6.0 /usr/X11R6/lib/libXext.so.6.0 \
#	  /usr/X11R6/lib/libX11.so.6.0 -lm
OBJS	= analysis.o animate.o apply.o callbacks.o canvas.o\
	  colors.o constraint.o contour.o draw3d.o drawingshell.o\
	  element.o elementlist.o elementop.o eps.o error.o file.o\
	  filedialog.o force.o generate.o globals.o graph.o grid.o group.o\
          load.o loadcase.o material.o modal.o node.o nodeop.o opengl.o\
          outputdialog.o panel.o postscript.o ppm.o pslib.o solution.o\
          solve.o structure.o text_entry.o text_output.o tools.o trimesh.o\
          velvet.o vfe.o wireframe.o 
PROG	= velvet

CURDIR	= ./src/Velvet
TOPDIR	= ../..

include $(TOPDIR)/etc/Makefile.bin

velvet.o:	velvet.ad.h interface.h

install::;	@$(INSTALL) -m 0644 Velvet.ad $(X11DEF) 2> /dev/null || true
