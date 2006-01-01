
all:: manual 

MANPAGE=../src/model/model.tex

manual::
	htlatex ../manual 'xhtml,3,notoc*,sec-filename' '' '' '\def\manname{$(MANPAGE)}'
	$(RM) -f  manual.css
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
