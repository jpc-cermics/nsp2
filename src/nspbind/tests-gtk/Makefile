.SUFFIXES: .defs .cdiff .hdiff .diff .c $(SUFFIXES)
SHELL = /bin/sh

ALL=  pango.diff atk.diff gdk.diff gtk.diff webkit.diff

PROG=../../../bin/nspbind -nw
GTK=../../gtk2/codegen/
ISRC=../../include/nsp/gtk/

all:
	cp $(GTK)/*.defs .
	cp $(GTK)/*.override .
	make diffs

diffs:	$(ALL)

%.cdiff: %.c
	@echo "-------- diff $*.c -------------------"
	@diff -rw $*.c $(GTK); $$status

%.hdiff: %.h
	@echo "-------- diff $*.h -------------------"
	@diff -rw $*.h $(ISRC); $$status

%.diff: %.defs
	@echo "-------- gen $*.c --------------------"
	@$(PROG) -p $* -o $*.c $* -cp ""
	@mv *.h headers/
	@make -s $*.cdiff
	@make headers
	@$(RM) headers/*.h

HEADERS=$(wildcard headers/*.h)
H_DIFF=$(patsubst %.h,%.hdiff,$(HEADERS))

headers: $(H_DIFF)

clean:
	@$(RM) *.c *.h *.defs *.override
