.SUFFIXES: .defs .diff .idiff .c $(SUFFIXES)
SHELL = /bin/sh

ALL=  pango.c atk.c gdk.c gtk.c webkit.c gvalue.c girepository.c

DIFF=$(patsubst %.c,%.diff,$(ALL))

PROG=../../../bin/nspbind -nw

all:	$(ALL)

%.c: %.defs
	@echo "generating $*.c"
	@$(PROG) -p $* -o /tmp/$*.c $*
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/$*.c+$*.c+g" /tmp/$*.c > $*.c
	@mv *.h ../../include/nsp/gtk/
	@rm -f /tmp/$*.c

gvalue.c: gvalue.defs
	@echo "generating gvalue.c"
	@$(PROG) -p gvalue -o /tmp/gvalue.c gvalue
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/gvalue.c+gvalue.c+g" /tmp/gvalue.c > gvalue.c
	@mv *.h ../../include/nsp/
	@rm -f /tmp/gvalue.c

girepository.c: girepository.defs
	@echo "generating girepository.c"
	@$(PROG) -p girepository -o /tmp/girepository.c girepository
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/girepository.c+girepository.c+g" /tmp/girepository.c > girepository.c
	@mv girepository.h ../../include/nsp/gtk/
	@mv gibaseinfo.h ../../include/nsp/
	@rm -f /tmp/girepository.c

clean:
	rm -f *.c *.h

pango.c: pango.defs pango.override pango-types.defs
atk.c: atk.defs atk.override atk-types.defs
gdk.c: gdk.defs gdk.override gdk-types.defs
gtk.c: gtk.defs gtk.override gtk-types.defs
webkit.c: webkit.defs webkit.override
