.SUFFIXES: .defs .diff .idiff .c $(SUFFIXES)
SHELL = /bin/sh

DIFFS=  classa.diff classb.diff classaref.diff classbref.diff \
	polyline.diff graphic.diff axes.diff figure.diff curve.diff \
	qcurve.diff \
	groot.diff gmatrix.diff vfield.diff contour.diff grstring.diff \
	grrect.diff grarc.diff arrows.diff segments.diff surf.diff \
	box3d.diff objs3d.diff points3d.diff polyhedron.diff polyline3d.diff \
	spolyhedron.diff string3d.diff  compound.diff gmatrix1.diff \
	fec.diff  contour3d.diff block.diff link.diff connector.diff \
	diagram.diff grimage.diff epoints.diff sharedlib.diff \
	hm.diff ast.diff astv.diff bvar.diff bson.diff

EXE=../../../bin/nspbind
SRC_REF_DIR=../../types-test
H_REF_DIR=../../include/nsp

all: fill
	make targets

fill:
	@cp $(SRC_REF_DIR)/codegen/*.defs .
	@cp $(SRC_REF_DIR)/codegen/*.override .

targets: $(DIFFS)

%.diff: %.defs
	@echo "-------- gen $*.c --------------------"
	@$(EXE) -o $*.c $*
	@echo "-------- diff $*.c -------------------"
	@diff -rw $*.c $(SRC_REF_DIR)/$*.c ; $$status
	@echo "-------- diff $*.h -------------------"
	@diff -rw $*.h $(H_REF_DIR)/$*.h; $$status

clean:
	@$(RM) *.c *.h *.diff *.idiff *.defs *.override
