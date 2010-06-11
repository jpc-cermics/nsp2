SHELL=/bin/sh

all:: manual-1

MANPAGE=model/model.tex

manual::
	export TEXINPUTS=`pwd`/../../src//:: ; \
	htlatex ../manual 'manual,xhtml,3,section+,next,notoc*' '-cunihtf' '' '\def\manname{$(MANPAGE)}'
	$(RM) -f  manual.css
	cp ../../html/manual.css manual.css

manual-1:
	@echo "running htlatex"
	@export TEXINPUTS=`pwd`/../../src//:: ; \
	htlatex ../manual 'manual,xhtml,uni-html4,3,section+,next,notoc*' '-cunihtf' '' '\def\manname{$(MANPAGE)}'
	cp ../../html/manual.css manual.css

index: 
	tex '\def\filename{{manual}{idx}{4dx}{ind}} \input  idxmake.4ht' 
	makeindex -o manual.ind manual.4dx 

distclean::
	@$(RM) manualch*.html manualli*.html manualse*.html 
	@$(RM) idxmake.dvi
	@$(RM) idxmake.log
	@$(RM) manual.4ct
	@$(RM) manual.4dx
	@$(RM) manual.4ix
	@$(RM) manual.4tc
	@$(RM) manual.aux
	@$(RM) manual.css
	@$(RM) manual.dvi
	@$(RM) manual.html
	@$(RM) manual.idv
	@$(RM) manual.idx
	@$(RM) manual.ilg
	@$(RM) manual.ind
	@$(RM) manual.lg
	@$(RM) manual.log
	@$(RM) manual.tmp
	@$(RM) manual.xref
	@$(RM) *.aux
	@$(RM) manual*.png

clean	::
