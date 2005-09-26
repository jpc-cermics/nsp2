
all::	
	latex -interaction=batchmode manual 
	makeindex -s index.isty manual.idx
	latex -interaction=batchmode manual
	dvips -Ppdf manual
	ps2pdf manual.ps

all-debug:
	latex manual 
	makeindex -s index.isty manual.idx
	latex -interaction=batchmode manual
	dvips -Ppdf manual
	ps2pdf manual.ps

distclean::
	@$(RM) 
	@$(RM) manual.aux
	@$(RM) manual.dvi
	@$(RM) manual.idx
	@$(RM) manual.ilg
	@$(RM) manual.ind
	@$(RM) manual.log
	@$(RM) manual.out
	@$(RM) manual.pdf
	@$(RM) manual.ps
	@$(RM) manual.toc
