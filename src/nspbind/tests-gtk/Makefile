.SUFFIXES: .defs .diff .idiff .c $(SUFFIXES)
SHELL = /bin/sh

ALL=  pango.c atk.c gdk.c gtk.c

DIFF=$(patsubst %.c,%.diff,$(ALL))
IDIFF=$(patsubst %.c,%.idiff,$(ALL))

PROG=../bin/simport 
SRC=/usr/local/src/nsp2-ref/src/types-test/
GTK=/usr/local/src/nsp2-ref/src/gtk2/codegen/
ISRC=/usr/local/src/nsp2-ref/src/include/nsp/

all:	$(ALL)

pango.c: pango.defs pango.override ../bin/simport.bin
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/pango.c+pango.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c

atk.c: atk.defs atk.override ../bin/simport.bin
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/atk.c+atk.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c

gdk.c: gdk.defs gdk.override ../bin/simport.bin
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/gdk.c+gdk.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c

gtk.c: gtk.defs gtk.override ../bin/simport.bin
	$(PROG) -p $* -o /tmp/$*.c $* 
	@sed -e "s+codegen/++g" \
	    -e "s+/tmp/gtk.c+gtk.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c


%.diff: %.c	
	@echo "-------- diff $*.c -------------------"
	@diff -rw $*.c $(GTK); $$status 

%.idiff: %.c	
	@echo "-------- diff $*.h -------------------"
	@diff -rw $*.h $(ISRC); $$status 

%.c: %.defs
	$(PROG) -o $*.c $*

diff: $(DIFF)

idiff: $(IDIFF)

clean:
	rm -f *.c *.h

















