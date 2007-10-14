SHELL=/bin/bash

# voir http://john.regehr.org/latex/

all:: manual-silent index-silent manual2-silent  manualindex 

# tlatex ../manual 'xhtml,3,sec-filename,next,notoc*'
# htlatex ../manual 'xhtml,uni-html4,3,section+,next,notoc*' "-cunihtf -utf8"

manual2-silent manual-silent:
	@echo -n "running htlatex..."
	@export TEXINPUTS=`pwd`/../../src//:: ; \
	htlatex ../manual 'xhtml,uni-html4,3,section+,next,notoc*'  >& /dev/null
	@cp ../manual.css manual.css
	@echo "done"

manual:
	@echo "running htlatex"
	@export TEXINPUTS=`pwd`/../../src//:: ; \
	htlatex ../manual 'xhtml,uni-html4,3,section+,next,notoc*' 
	@cp ../manual.css manual.css

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

clean	:: 
