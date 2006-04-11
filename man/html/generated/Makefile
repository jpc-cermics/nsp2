
SHELL = /bin/sh

# voir http://john.regehr.org/latex/


all:: manual index manual manualindex 

# tlatex ../manual 'xhtml,3,sec-filename,next,notoc*'

manual::
	export TEXINPUTS=`pwd`/../../src//:: ; \
	htlatex ../manual 'xhtml,uni-html4,3,section+,next,notoc*' "-cunihtf -utf8"
	cp ../manual.css manual.css

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

# a link for the index.

manualindex	::
	$(RM)  manualindex.html
	ln -f -s `grep  -l '<title>Index</title>' *.html` manualindex.html
