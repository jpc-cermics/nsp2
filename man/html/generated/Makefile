SHELL=/bin/bash

# voir http://john.regehr.org/latex/
# unicode.4hf is a copy of 
# /usr/share/texmf/tex4ht/ht-fonts/mozilla/charset/unicode.4hf
# with a few changes at the end to allow copy/paste to nsp 
# 

# I have changed 'manual,xhtml,...' to 'manual,html,...'
# Some closing tags are missing in the generated code
# and it blocks the display when using xhtml while html is more permissive.
# July 2022

manual:
	./htlatex ../manual 'manual,html,3,section+,next,notoc*' ' -cunihtf' ''
	@cp ../manual.css manual.css

all:: manual-silent index-silent manual2-silent  manualindex 

manual-silent manual2-silent:
	@echo -n "running htlatex..."
	@./htlatex ../manual 'manual,html,3,section+,next,notoc*' ' -cunihtf' '' >& /dev/null
	@cp ../manual.css manual.css
	@echo "done"

index-silent: 
	@echo -n "make index..."
	@tex '\def\filename{{manual}{idx}{4dx}{ind}} \input  idxmake.4ht' >& /dev/null
	@makeindex -o manual.ind manual.4dx >& /dev/null
	@echo "done"

index:
	@echo "make index"
	@tex '\def\filename{{manual}{idx}{4dx}{ind}} \input  idxmake.4ht' 
	@makeindex -o manual.ind manual.4dx >& /dev/null

# a link for the index.

manualindex:
	@echo "link for index"	
	@$(RM)  manualindex.html
	@ln -f -s `grep  -l '<title>Index</title>' *.html` manualindex.html

distclean::
	@$(RM) manualch*.html manualli*.html manualse*.html manualindex.html
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
	@$(RM) -fr _mintednsp-manual _minted-manual
clean	:: 
