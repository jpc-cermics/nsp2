
all::	
	export TEXINPUTS=::`pwd`/../src// ; \
	latex -interaction=batchmode manual 
	makeindex -s index.isty manual.idx
	export TEXINPUTS=::`pwd`/../src// ; \
	latex -interaction=batchmode manual
	dvips -Ppdf manual
	ps2pdf manual.ps

all-debug:
	export TEXINPUTS=::`pwd`/../src// ; latex manual 
	makeindex -s index.isty manual.idx
	export TEXINPUTS=::`pwd`/../src// ; \
		latex -interaction=batchmode manual
	dvips -Ppdf manual
	ps2pdf manual.ps

distclean::
	@$(RM) manual.aux  manual.dvi manual.ps manual.pdf 
	@$(RM) manual.ilg manual.out manual.ind
	@$(RM) manual.idx  manual.log manual.toc *.aux


