all: scan mktmpl mkdb mkhtml 

scan :
	gtkdoc-scan --module=nsp --source-dir=../include/nsp --ignore-headers="blas.h gsort.h gsort-double.h gsort-int.h gsort-gen.h gsort-string.h gsort-p.h gtk TAGS all-gtk.h all-pango.h all-gdk.h"

mktmpl:
	gtkdoc-mktmpl --module=nsp

mkdb:
	gtkdoc-mkdb --module=nsp --source-dir=../Newstack --sgml-mode --output-format=xml

mkhtml:
	cd html; gtkdoc-mkhtml nsp ../nsp-docs.sgml

clean:
	\rm -f -r html/* xml *.stamp tmpl *.txt 

distclean:
	\rm -f -r html/* xml *.stamp tmpl *.txt 
