TNAME=ClassB
TDOWNCASE=classb
TSHORT=clb
TASSTRING=ClassB
NULL=NULLCLB
PARENT=ClassA
PARENTDOWNCASE=classa
PARENTINC=classa.h
FIELDS=int classb_color;\n  int classb_thickness;\n  NspMatrix *classb_val;

all: job job_classa

job : $(TDOWNCASE).c $(TDOWNCASE).h

$(TDOWNCASE).c : model-c Makefile
	sed -e "s/@TYPENAME@/$(TNAME)/g" \
	    -e "s/@TYPEDOWNCASE@/$(TDOWNCASE)/g" \
	    -e "s/@TYPEASSTRING@/$(TASSTRING)/g" \
	    -e "s/@NULL@/$(NULL)/g" \
	    -e "s/@TYPESHORT@/$(TSHORT)/g" \
	    -e "s/@PARENTDOWNCASE@/$(PARENTDOWNCASE)/g" \
	    -e "s/@PARENT@/$(PARENT)/g" \
	    model-c > $(TDOWNCASE).c 

$(TDOWNCASE).h : model-h Makefile
	sed -e "s/@TYPENAME@/$(TNAME)/g" \
	    -e "s/@TYPEDOWNCASE@/$(TDOWNCASE)/g" \
	    -e "s/@TYPEASSTRING@/$(TASSTRING)/g" \
	    -e "s/@NULL@/$(NULL)/g" \
	    -e "s/@TYPESHORT@/$(TSHORT)/g" \
	    -e "s+@FIELDS@+$(FIELDS)+g" \
	    -e "s/@PARENTDOWNCASE@/$(PARENTDOWNCASE)/g" \
	    -e "s/@PARENT@/$(PARENT)/g" \
	    -e "s+@PARENTINC@+$(PARENTINC)+g" \
	    model-h > $(TDOWNCASE).h

job_classa: 
	make job  TNAME=ClassA TDOWNCASE=classa TSHORT=cla \
		TASSTRING=ClassA NULL=NULLCLA \
		PARENT=Object FIELDS="int classa_color;\n  int classa_thickness;\n  NspMatrix *classa_val;"\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h

job_classnone: 
	make job  TNAME=None TDOWNCASE=none TSHORT=none \
		TASSTRING=None NULL=NULLNONE \
		PARENT=Object FIELDS=""\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h

job_classpango: 
	make job  TNAME=PangoAttribute TDOWNCASE=pangoattribute TSHORT=pangoattribute \
		TASSTRING=PangoAttribute NULL=NULLPATTR \
		PARENT=Object FIELDS="PangoAttribute *attr;"\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h

job_classgdkatom: 
	make job TNAME=GdkAtom TDOWNCASE=gdkatom TSHORT=gdkatom \
		TASSTRING=GdkAtom NULL=NULLGDKATOM \
		PARENT=Object FIELDS="  gchar *name;\n  GdkAtom atom;"\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h

job_classtype: 
	make job TNAME=Type TDOWNCASE=type TSHORT=type \
		TASSTRING=Type NULL=NULLTYPE \
		PARENT=Object FIELDS="  NspTypeBase *type;"\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h

job_classc: 
	make -f Makefile-sub job_classc 


job_hash: 
	make job TNAME=Hash TDOWNCASE=hash TSHORT=h \
		TASSTRING=Hash NULL=NULLHASH \
		PARENT=Object FIELDS="  unsigned int hsize,filled;\n  void  *htable;"\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h


job_gobject: 
	make job TNAME=GObject TDOWNCASE=gobject TSHORT=gobj \
		TASSTRING=GObject NULL=NULLGOBJECT \
		PARENT=Object FIELDS="  GObject *obj;\n GSList *closures;\n  NspTypeBase *obj_type;"\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h

job_mat: 
	make job  TNAME=Matrix TDOWNCASE=matrix TSHORT=m \
		TASSTRING=Mat NULL=NULLMAT \
		PARENT=Object FIELDS="pipo"\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h


job_gboxed: 
	make job  TNAME=GBoxed TDOWNCASE=gboxed TSHORT=gb \
		TASSTRING=GBoxed NULL=NULLGBOXED \
		PARENT=Object FIELDS="pipo"\
		PARENTDOWNCASE=object PARENTINC=nsp/object.h

job_mod:
	make job  TNAME=Module TDOWNCASE=module TSHORT=mo \
		TASSTRING=Module NULL=NULLMODULE \
		PARENT=Object FIELDS="char *path ; /* absolute XXX path of the module */\n\tchar *mname;  /* module name */\n\tNspHash *T;      /* hash table to store module objects of type me (module elts) */\n\tNspList *L;       /* List of submodules */\n\tint flag;     /* are we a copy ? */;" \
		PARENTDOWNCASE=object PARENTINC=nsp/object.h 

job_me:
	make job  TNAME=ModuleElt TDOWNCASE=modulelt TSHORT=me \
		TASSTRING=ModuleElt NULL=NULLME \
		PARENT=Object FIELDS="char *path ;\n\tchar *module;" \
		PARENTDOWNCASE=object PARENTINC=nsp/object.h 

clean : distclean 

distclean:
	$(RM) *.c *.h










