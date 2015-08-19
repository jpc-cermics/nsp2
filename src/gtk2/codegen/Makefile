.SUFFIXES: .defs .diff .idiff .c $(SUFFIXES)
SHELL = /bin/sh

ALL=  pango.c atk.c gdk.c gtk.c webkit.c gvalue.c girepository.c

DIFF=$(patsubst %.c,%.diff,$(ALL))

PROG=../../../bin/nspbind -nw

all:	$(ALL)
	cd ../; make -j all

gvalue.c: gvalue.defs
	@echo "generating $*.c"
	@$(PROG) -cp "codegen/" -p $* -o $*.c $*
	@mv gvalue.c ../
	@mv *.h ../../include/nsp/gtk/

girepository.c: girepository.defs
	@echo "generating $*.c"
	@$(PROG) -cp "codegen/" -p $* -o $*.c $*
	@mv girepository.c ../
	@mv girepository.h ../../include/nsp/gtk/
	@mv gibaseinfo.h ../../include/nsp/

clean:
	rm -f *.c *.h

pango.c: pango.defs pango.override pango-types.defs
atk.c: atk.defs atk.override atk-types.defs
gdk.c: gdk.defs gdk.override gdk-types.defs
gtk.c: gtk.defs gtk.override gtk-types.defs
webkit.c: webkit.defs webkit.override
gvalue.c: gvalue.defs gvalue.override


%.c: %.defs
	@echo "generating $*.c"
	@$(PROG) -cp "codegen/" -p $* -o $*.c $*
	@mv $*.c ../
	@mv *.h ../../include/nsp/gtk/

