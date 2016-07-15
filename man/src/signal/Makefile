
DIR=../../html-one/generated/
LDIR=signal

DIA = $(patsubst %.tex,%.dia,$(wildcard *.tex))

all: $(DIA)

%.dia: %.tex
	cd $(DIR); make manual MANPAGE=$(LDIR)/$*.tex
	firefox file://`pwd`/$(DIR)/manual.html

clean:
	$(RM) *.dia




