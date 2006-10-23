
all:: manual

# give the man page as a relative path from 
# ../src 

MANPAGE=model/model.tex

manual::
	cd generated ; make manual MANPAGE=$(MANPAGE)

index: 
	cd generated ; make index 

distclean::
	cd generated ; make distclean

clean:: 


show:
	firefox file://`pwd`/generated/manual.html
