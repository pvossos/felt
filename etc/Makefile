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

ROOT	= FElt-3.06
TAR	= tar

CURDIR	= ./etc
TOPDIR	= ..

include $(TOPDIR)/etc/Makefile.conf

all::;

clean::;

clobber::;

install::;	@$(MKDIR) $(DESTLIB)
		@for f in Defaults/*.defaults Translations/*.trn; do \
		$(INSTALL) -m 0644 $$f $(DESTLIB); done

depend::;

srcdist::;	@cd $(TOPDIR)/..; rm -f felt.tar.gz; \
		find $(ROOT) -name Loom -prune -o -name core -prune \
                -o -name "*.[oa]" -prune \
		-o -perm 755 -type f ! -name configure -prune -o -type f \
		-print | xargs $(TAR) rvf felt.tar; gzip --best felt.tar

fulldist::;	@cd $(TOPDIR)/..; rm -f felt.tar.gz; \
		find $(ROOT) -name core -prune -o -name "*.[oa]" -prune \
		-o -perm 755 -type f ! -name configure -prune -o -type f \
		-print | xargs $(TAR) rvf felt.tar; gzip --best felt.tar

bindist::;	@cd $(TOPDIR)/..; rm -f felt-bin.tar.gz; \
		sed -e s/ROOT/$(ROOT)/ $(ROOT)/etc/bindist | \
		xargs $(TAR) rvf felt-bin.tar; gzip --best felt-bin.tar

dosdist::;	@cd $(TOPDIR); rm -f ../felt-dos.zip; \
		mv lib/Felt/y.tab.h lib/Felt/yxtab.h; \
		mv src/Corduroy/y.tab.h src/Corduroy/yxtab.h; \
		find etc lib/Felt lib/Elements lib/Matrix lib/Generate \
		lib/Geompack src/Felt src/Corduroy src/Patchwork src/Burlap \
		src/Yardstick src/Loom include examples CHANGELOG FAQ README \
		README.old TODO COPYING -name core -prune -o -name "*.[oaylf]" \
		-prune -o -perm 755 -type f -prune \
		-o -type f -print | xargs zip ../felt-dos.zip; \
		mv src/Corduroy/yxtab.h src/Corduroy/y.tab.h; \
		mv lib/Felt/yxtab.h lib/Felt/y.tab.h
