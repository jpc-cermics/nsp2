.SUFFIXES: .defs .cdiff .hdiff .diff .c $(SUFFIXES)
SHELL = /bin/sh

ALL=  pango.diff atk.diff gdk.diff gtk.diff webkit.diff

PROG=../../../bin/nspbind -nw
GTK=../../gtk3/
ISRC=../../include/nsp/gtk/

all:
	cp $(GTK)codegen-3.0/*.defs .
	cp $(GTK)codegen-3.0/*.override .
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
	@$(PROG) -p $* -o $*.c $* -cp "codegen-3.0/"
	@mv *.h headers/
	@make -s $*.cdiff
	@make headers
	@$(RM) headers/*.h

HEADERS=$(wildcard headers/*.h)
H_DIFF=$(patsubst %.h,%.hdiff,$(HEADERS))

headers: $(H_DIFF)

clean:
	@$(RM) *.c *.h *.defs *.override
