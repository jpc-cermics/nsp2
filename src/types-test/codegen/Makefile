.SUFFIXES: .defs .c $(SUFFIXES)
SHELL = /bin/sh

SCIDIR=../../../
SCIDIR1=..\..\..\

LIBRARY = nsp-cla.a 

OBJSC = classa.o classb.o 
OBJSF=

include $(SCIDIR)/Makefile.incl

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

PYTHON=python 
ALL=  ../classa.c ../classb.c ../classaref.c ../classbref.c \
	../polyline.c ../graphic.c ../axes.c ../figure.c ../curve.c 

all:: $(ALL)

clean :: 
	$(RM) $(ALL)

.defs.c:
	($(PYTHON) codegen/codegen.py \
	    --override $*.override \
	    --prefix $* $*.defs) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c

../classa.c : classa.defs classa.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override classa.override \
	    --prefix ClassA classa.defs) > $*.c \
	&& cp classa.h ../../include/nsp 

../classb.c : classb.defs classb.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override classb.override \
	    --prefix ClassB classb.defs) > $*.c \
	&& cp classb.h ../../include/nsp 

../classaref.c : classaref.defs classaref.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override classaref.override \
	    --prefix ClassARef classaref.defs) > $*.c \
	&& cp classaref.h ../../include/nsp 

../classbref.c : classbref.defs classbref.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override classbref.override \
	    --prefix ClassBRef classbref.defs) > $*.c \
	&& cp classbref.h ../../include/nsp 


../polyline.c : polyline.defs polyline.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override polyline.override \
	    --prefix Polyline polyline.defs) > $*.c \
	&& cp polyline.h ../../include/nsp 

../graphic.c : graphic.defs graphic.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override graphic.override \
	    --prefix Graphic graphic.defs) > $*.c \
	&& cp graphic.h ../../include/nsp 

../axes.c : axes.defs axes.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override axes.override \
	    --prefix Compound axes.defs) > $*.c \
	&& cp axes.h ../../include/nsp 

../curve.c : curve.defs curve.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override curve.override \
	    --prefix Curve curve.defs) > $*.c \
	&& cp curve.h ../../include/nsp 

../figure.c : figure.defs figure.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override figure.override \
	    --prefix Curve figure.defs) > $*.c \
	&& cp figure.h ../../include/nsp 


