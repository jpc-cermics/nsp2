all: scan mktmpl mkdb mkhtml 

scan :
	gtkdoc-scan --module=nspdoc --source-dir=../include/nsp --ignore-headers="blas.h gsort.h gsort-double.h gsort-int.h gsort-gen.h gsort-string.h gsort-p.h gtk TAGS all-gtk.h all-pango.h all-gdk.h"

mktmpl:
	gtkdoc-mktmpl --module=nspdoc

mkdb:
	gtkdoc-mkdb --module=nspdoc --source-dir=../Newstack --source-dir=../types  --sgml-mode --output-format=xml

mkhtml:
	cd html; gtkdoc-mkhtml nspdoc ../nspdoc-docs.sgml

clean:
	\rm -f -r html/* xml *.stamp 

distclean:
	\rm -f -r html/* xml *.stamp 

