.SUFFIXES: .defs .diff .idiff .c $(SUFFIXES)
SHELL = /bin/sh

ALL=  pango.c atk.c gdk.c gtk.c webkit.c 

DIFF=$(patsubst %.c,%.diff,$(ALL))

PROG=../bin/nspbind -nw 
SRC=/usr/local/src/nsp2-ref/src/types-test/
GTK=/usr/local/src/nsp2-ref/src/gtk2/codegen/
ISRC=/usr/local/src/nsp2-ref/src/include/nsp/gtk/

all:	$(ALL)

pango.c: pango.defs pango.override ../bin/nspbind
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/pango.c+pango.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c

atk.c: atk.defs atk.override ../bin/nspbind
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/atk.c+atk.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c

gdk.c: gdk.defs gdk.override ../bin/nspbind
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/gdk.c+gdk.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c

gtk.c: gtk.defs gtk.override ../bin/nspbind
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/gtk.c+gtk.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c

webkit.c: webkit.defs webkit.override ../bin/nspbind
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/webkit.c+webkit.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c


%.diff: %.c	
	@echo "-------- diff $*.c -------------------"
	@diff -rw $*.c $(GTK); $$status 

%.idiff: %.h	
	@echo "-------- diff $*.h -------------------"
	@diff -rw $*.h $(ISRC); $$status 

%.c: %.defs
	$(PROG) -o $*.c $*

diff: $(DIFF)

HSRC=$(wildcard *.h)
IDIFF=$(patsubst %.h,%.idiff,$(HSRC))

idiff: $(IDIFF)

clean:
	rm -f *.c *.h


















