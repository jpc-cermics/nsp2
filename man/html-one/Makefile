
all:: manual

MANPAGE=../src/model/model.tex

manual::
	cd generated ; make manual MANPAGE=$(MANPAGE)

index: 
	cd generated ; make index 

distclean::
	cd generated ; make distclean

clean:: 


show:
	firefox file://`pwd`/generated/manual.html
