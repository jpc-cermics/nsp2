all: scan mktmpl mkdb mkhtml 

# gsort.h gsort-double.h gsort-int.h gsort-gen.h gsort-string.h gsort-p.h

scan :
	./gtkdoc-scan --module=nspdoc --source-dir=../include/nsp --source-dir=../librand --source-dir=../include/mex \
	--source-dir=./ --ignore-headers="blas.h  gtk TAGS all-gtk.h all-pango.h all-gdk.h integ.h machine-h.vc lapack-c.h machine.h.in graphics"

mktmpl:
	gtkdoc-mktmpl --module=nspdoc

mkdb:
	gtkdoc-mkdb --module=nspdoc --source-dir=../include/nsp --source-dir=../objects --source-dir=../zcalelm \
	--source-dir=../libapprox  --source-dir=../librand --source-dir=../functions --source-dir=../types-test --source-dir=../lapack \
	--source-dir=../gobjects 	--source-dir=../interp --source-dir=../gtk2  \
	--sgml-mode --output-format=xml

mkhtml:
	cd html; gtkdoc-mkhtml nspdoc ../nspdoc-docs.sgml

clean:
	\rm -f -r html/*.html xml *.stamp 

distclean: clean


