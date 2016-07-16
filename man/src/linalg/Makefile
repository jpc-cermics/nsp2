
DIR=../../html-one/generated/
LDIR=linalg

DIA = $(patsubst %.tex,%.dia,$(wildcard *.tex))

all: $(DIA)

%.dia: %.tex
	cd $(DIR); make distclean
	cd $(DIR); make manual MANPAGE=$(LDIR)/$*.tex
	firefox file://`pwd`/$(DIR)/manual.html
	touch $*.dia

clean:
	$(RM) *.dia




