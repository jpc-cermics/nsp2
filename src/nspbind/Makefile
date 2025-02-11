SHELL=/bin/sh

all:
	dune build

clean:

distclean: clean
	$(RM) -fr _build

install:
	cp _build/default/main_gen.exe ../../bin/nspbind
