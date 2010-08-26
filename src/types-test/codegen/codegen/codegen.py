import sys, os, string
import getopt, traceback, keyword
import defsparser, argtypes, override

def exc_info():
    #traceback.print_exc()
    etype, value, tb = sys.exc_info()
    ret = ""
    try:
        sval = str(value)
        if etype == KeyError:
            ret = "No ArgType for %s" % (sval,)
        else:
            ret = sval
    finally:
        del etype, value, tb
    return ret

def fixname(name):
    if keyword.iskeyword(name):
	return name + '_'
    return name

class FileOutput:
    '''Simple wrapper for file object, that makes writing #line
    statements easier.''' # "
    def __init__(self, fp, filename=None):
        self.fp = fp
        self.lineno = 1
        if filename:
            self.filename = filename
        else:
            self.filename = self.fp.name
    # handle writing to the file, and keep track of the line number ...
    def write(self, str):
        self.fp.write(str)
        self.lineno = self.lineno + string.count(str, '\n')
    def writelines(self, sequence):
        for line in sequence:
            self.write(line)
    def close(self):
        self.fp.close()
    def flush(self):
        self.fp.flush()

    def setline(self, linenum, filename):
        '''writes out a #line statement, for use by the C
        preprocessor.''' # "
        self.write('#line %d "%s"\n' % (linenum, filename))
    def resetline(self):
        '''resets line numbering to the original file'''
        self.setline(self.lineno + 1, self.filename)

class Wrapper:
    var_loc1 = 'H'
    var_loc2 = 'Q'

    type_tmpl_1_0 = \
              '\n'  \
              '#define  %(typename)s_Private \n'  \
              '#include <nsp/objects.h>\n'  \
              '#include <nsp/%(typename_dc)s.h>\n'  \
              '#include <nsp/interf.h>\n'  \
              '\n'  \
              '/* \n' \
              ' * %(typename)s inherits from %(parent)s \n%(implements)s' \
              ' */\n' \
              '\n' \
              'int nsp_type_%(typename_dc)s_id=0;\n' \
              'NspType%(typename_nn)s *nsp_type_%(typename_dc)s=NULL;\n' \
              '\n' \
              '/*\n' \
              ' * Type object for %(typename)s \n' \
              ' * all the instance of NspType%(typename_nn)s share the same id. \n' \
              ' * nsp_type_%(typename_dc)s: is an instance of NspType%(typename_nn)s \n' \
              ' *    used for objects of %(typename)s type (i.e built with new_%(typename_dc)s) \n' \
              ' * other instances are used for derived classes \n' \
              ' */\n' \
              'NspType%(typename_nn)s *new_type_%(typename_dc)s(type_mode mode)\n'  \
              '{\n'  
    type_tmpl_1_0_1 = \
              '  NspType%(typename_nn)s *type= NULL;\n'  \
              '  NspTypeObject *top;\n'  \
              '  if (  nsp_type_%(typename_dc)s != 0 && mode == T_BASE )\n'  \
              '    {\n'  \
              '      /* initialization performed and T_BASE requested */\n'  \
              '      return nsp_type_%(typename_dc)s;\n'  \
              '    }\n'  \
              '  if (( type =  malloc(sizeof(NspType%(typename_nn)s))) == NULL) return NULL;\n'  \
              '  type->interface = NULL;\n'  \
              '  type->surtype = (NspTypeBase *) new_type_%(parent_dc)s(T_DERIVED);\n'  \
              '  if ( type->surtype == NULL) return NULL;\n'  \
              '  type->attrs = %(typename_dc)s_attrs;\n' \
              '  type->get_attrs = (attrs_func *) %(tp_getattr)s;\n'  \
              '  type->set_attrs = (attrs_func *) %(tp_setattr)s;\n'  \
              '  type->methods = %(typename_dc)s_get_methods;\n'  \
              '  type->new = (new_func *) new_%(typename_dc)s;\n'  \
              '\n'  \
              '\n'  \
              '  top = NSP_TYPE_OBJECT(type->surtype);\n'  \
              '  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);\n'  \
              '\n'  \
              '  /* object methods redefined for %(typename_dc)s */ \n'  \
              '\n'  \
              '  top->pr = (print_func *) nsp_%(typename_dc)s_print;\n' \
              '  top->dealloc = (dealloc_func *) nsp_%(typename_dc)s_destroy;\n' \
              '  top->copy  =  (copy_func *) nsp_%(typename_dc)s_copy;\n' \
              '  top->size  = (size_func *) nsp_%(typename_dc)s_size;\n' \
              '  top->s_type =  (s_type_func *) nsp_%(typename_dc)s_type_as_string;\n' \
              '  top->sh_type = (sh_type_func *) nsp_%(typename_dc)s_type_short_string;\n' \
              '  top->info = (info_func *) nsp_%(typename_dc)s_info;\n' \
              '  /* top->is_true = (is_true_func  *) nsp_%(typename_dc)s_is_true; */\n' \
              '  /* top->loop =(loop_func *) nsp_%(typename_dc)s_loop;*/\n' \
              '  top->path_extract = (path_func *)  object_path_extract;\n' \
              '  top->get_from_obj = (get_from_obj_func *) nsp_%(typename_dc)s_object;\n' \
              '  top->eq  = (eq_func *) nsp_%(typename_dc)s_eq;\n' \
              '  top->neq  = (eq_func *) nsp_%(typename_dc)s_neq;\n' \
              '  top->save  = (save_func *) nsp_%(typename_dc)s_xdr_save;\n' \
              '  top->load  = (load_func *) nsp_%(typename_dc)s_xdr_load;\n' \
              '  top->create = (create_func*) int_%(typename_dc)s_create;\n' \
              '  top->latex = (print_func *) nsp_%(typename_dc)s_latex;\n' \
              '  top->full_copy = (copy_func *) nsp_%(typename_dc)s_full_copy;\n' \
              '\n'  \
              '  /* specific methods for %(typename_dc)s */\n'  \
              '\n'  \
              '  type->init = (init_func *) init_%(typename_dc)s;\n'  \
              '\n'  \

    type_tmpl_1_0_2 = \
              '  /* \n' \
              '   * %(typename)s interfaces can be added here \n' \
              '   * type->interface = (NspTypeBase *) new_type_b();\n' \
              '   * type->interface->interface = (NspTypeBase *) new_type_C()\n' \
              '   * ....\n' \
              '   */\n' 

    type_tmpl_1_1 = \
              '  if ( nsp_type_%(typename_dc)s_id == 0 ) \n'  \
              '    {\n'  \
              '      /* \n'  \
              '       * the first time we get here we initialize the type id and\n'  \
              '       * an instance of NspType%(typename_nn)s called nsp_type_%(typename_dc)s\n'  \
              '       */\n'  \
              '      type->id =  nsp_type_%(typename_dc)s_id = nsp_new_type_id();\n'  \
              '      nsp_type_%(typename_dc)s = type;\n'  \
              '      if ( nsp_register_type(nsp_type_%(typename_dc)s) == FALSE) return NULL;\n'  \
              '      return ( mode == T_BASE ) ? type : new_type_%(typename_dc)s(mode);\n'  \
              '    }\n'  \
              '  else \n'  \
              '    {\n' \
              '      type->id = nsp_type_%(typename_dc)s_id;\n'  \
              '      return type;\n'  \
              '    }\n' \
              '}\n'  \
              '\n'  \
              '/*\n'  \
              ' * initialize %(typename)s instances \n'  \
              ' * locally and by calling initializer on parent class \n'  \
              ' */\n'  \
              '\n'  \
              'static int init_%(typename_dc)s(%(typename)s *Obj,NspType%(typename_nn)s *type)\n'  \
              '{\n'  \
              '  /* initialize the surtype */ \n'  \
              '  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;\n'  \
              '  Obj->type = type;\n'  \
              '  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;\n'  \
              '  /* specific */\n'  \
              '%(fields_init)s' \
              '  return OK;\n'  \
              '}\n'  \
              '\n'  \
              '/*\n'  \
              ' * new instance of %(typename)s \n'  \
              ' */\n'  \
              '\n'  \
              '%(typename)s *new_%(typename_dc)s() \n'  \
              '{\n'  \
              '  %(typename)s *loc;\n'  \
              '  /* type must exists */\n'  \
              '  nsp_type_%(typename_dc)s = new_type_%(typename_dc)s(T_BASE);\n'  \
              '  if ( (loc = malloc(sizeof(%(typename)s)))== NULL%(typename_uc)s) return loc;\n'  \
              '  /* initialize object */\n'  \
              '  if ( init_%(typename_dc)s(loc,nsp_type_%(typename_dc)s) == FAIL) return NULL%(typename_uc)s;\n'  \
              '  return loc;\n'  \
              '}\n'  \
              '\n'  \
              '/*----------------------------------------------\n'  \
              ' * Object method redefined for %(typename)s \n'  \
              ' *-----------------------------------------------*/\n'  \
              '/*\n' \
              ' * size \n' \
              ' */\n' \
              '\n' \
              'static int nsp_%(typename_dc)s_size(%(typename)s *Mat, int flag)\n' \
              '{\n' \
              '  return 0;\n' \
              '}\n' \
              '\n'  \
              '/*\n'  \
              ' * type as string \n'  \
              ' */\n'  \
              '\n'  \
              'static char %(typename_dc)s_type_name[]="%(typename_nn)s";\n'  \
              'static char %(typename_dc)s_short_type_name[]="%(typename_dc)s";\n'  \
              '\n'  \
              'static char *nsp_%(typename_dc)s_type_as_string(void)\n'  \
              '{\n'  \
              '  return(%(typename_dc)s_type_name);\n'  \
              '}\n'  \
              '\n'  \
              'static char *nsp_%(typename_dc)s_type_short_string(NspObject *v)\n'  \
              '{\n'  \
              '  return(%(typename_dc)s_short_type_name);\n'  \
              '}\n'  \
              '\n' 

    type_tmpl_1_1_1 = \
              '/*\n' \
              ' * A == B \n' \
              ' */\n' \
              '\n' \
              'static int nsp_%(typename_dc)s_eq(%(typename)s *A, NspObject *B)\n' \
              '{\n' \
              '  %(typename)s *loc = (%(typename)s *) B;\n' \
              '  if ( check_cast(B,nsp_type_%(typename_dc)s_id) == FALSE) return FALSE ;\n' \
              '%(fields_equal)s' \
              '  return TRUE;\n' \
              '}\n' \
              '\n' \
              '/*\n' \
              ' * A != B \n' \
              ' */\n' \
              '\n' \
              'static int nsp_%(typename_dc)s_neq(%(typename)s *A, NspObject *B)\n' \
              '{\n' \
              '  return ( nsp_%(typename_dc)s_eq(A,B) == TRUE ) ? FALSE : TRUE;\n' \
              '}\n' \
              '\n' \
              '/*\n' \
              ' * save \n' \
              ' */\n' \
              '\n' 

    type_tmpl_save = \
              'int nsp_%(typename_dc)s_xdr_save(XDR *xdrs, %(typename)s *M)\n' \
              '{\n' \
              '  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/\n' \
              '  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ \n' \
              '  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;\n' \
              '  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_%(typename_dc)s)) == FAIL) return FAIL;\n' \
              '  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;\n' \
              '%(fields_save)s' \
              '  return OK;\n' \
              '}\n' \
              '\n' 

    type_tmpl_load_1 = \
        '/*\n' \
        ' * load \n' \
        ' */\n' \
        '\n' \
        '%(typename)s  *nsp_%(typename_dc)s_xdr_load_partial(XDR *xdrs, %(typename)s *M)\n' \
        '{\n' \
        '%(fields_load)s' \
        ' return M;\n' \
        '}\n\n' \
        'static %(typename)s  *nsp_%(typename_dc)s_xdr_load(XDR *xdrs)\n' \
        '{\n' \
        '  %(typename)s *H = NULL;\n' \
        '  char name[NAME_MAXL];\n' \
        '  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL%(typename_uc)s;\n' \
        '  if ((H  = nsp_%(typename_dc)s_create_void(name,(NspTypeBase *) nsp_type_%(typename_dc)s))== NULL%(typename_uc)s) return H;\n' \
        '  if ( nsp_%(typename_dc)s_create_partial(H) == FAIL) return NULL%(typename_uc)s;\n' \
        '  if ((H  = nsp_%(typename_dc)s_xdr_load_partial(xdrs,H))== NULL%(typename_uc)s) return H;\n' \
        '  if ( nsp_%(typename_dc)s_check_values(H) == FAIL) return NULL%(typename_uc)s;\n'

    type_tmpl_load_2 = \
        '  return H;\n}\n\n' 

    type_tmpl_delete = \
              '/*\n' \
              ' * delete \n' \
              ' */\n' \
              '\n' \
              'void nsp_%(typename_dc)s_destroy_partial(%(typename)s *H)\n' \
              '{\n' \
              '%(fields_free1)s' 

    type_tmpl_1_1_1_1 = \
              '%(fields_free2)s' \
              '}\n\n' \
              'void nsp_%(typename_dc)s_destroy(%(typename)s *H)\n' \
              '{\n' \
              '  nsp_object_destroy_name(NSP_OBJECT(H));\n' \
              '  nsp_%(typename_dc)s_destroy_partial(H);\n' \
              '  FREE(H);\n' \
              '}\n' \
              '\n' \
              '/*\n' \
              ' * info \n' \
              ' */\n' \
              '\n' \
              'int nsp_%(typename_dc)s_info(%(typename)s *M,int indent,const char *name,int rec_level)\n' \
              '{\n' \
              '  const char *pname;\n' \
              '  if ( M == NULL%(typename_uc)s) \n' \
              '    {\n' \
              '      Sciprintf("Null Pointer %(typename)s \\n");\n' \
              '      return TRUE;\n' \
              '    }\n' \
              '  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;\n' \
              '  Sciprintf1(indent,"%%s\\t=\\t\\t%%s\\n", (pname==NULL) ? "" : pname,\n' \
              '             nsp_%(typename_dc)s_type_short_string(NSP_OBJECT(M)));\n' \
              '  return TRUE;\n' \
              '}\n' \
              '\n' \
              '/*\n' \
              ' * print \n' \
              ' */\n' \
              '\n' \
              'int nsp_%(typename_dc)s_print(%(typename)s *M, int indent,const char *name, int rec_level)\n' \
              '{\n' \
              '  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;\n' \
              '  if ( M == NULL%(typename_uc)s) \n' \
              '    {\n' \
              '      Sciprintf("Null Pointer %(typename)s \\n");\n' \
              '      return TRUE;\n' \
              '    }\n' \
              '  if (user_pref.pr_as_read_syntax) \n' \
              '    { \n' \
              '      Sciprintf1(indent,"%%s=TO_BE_DONE();\\n",pname);\n' \
              '    } \n' \
              '  else \n' \
              '    { \n' \
              '      if ( user_pref.pr_depth  <= rec_level -1 ) \n' \
              '        {\n' \
              '          nsp_%(typename_dc)s_info(M,indent,pname,rec_level);\n' \
              '          return TRUE;\n' \
              '        }\n' \
              '      Sciprintf1(indent,"%%s\\t=\\t\\t%%s %(ref_count)s\\n",pname, nsp_%(typename_dc)s_type_short_string(NSP_OBJECT(M)) %(ref_count_ref)s);\n' \
              '      Sciprintf1(indent+1,"{\\n");\n' \
              '%(fields_print)s' \
              '      Sciprintf1(indent+1,"}\\n");\n' \
              '    }\n' \
              '  return TRUE;\n' \
              '}\n\n' \
              '/*\n' \
              ' * latex print \n' \
              ' */\n\n' \
              'int nsp_%(typename_dc)s_latex(%(typename)s *M, int indent,const char *name, int rec_level)\n' \
              '{\n' \
              '  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;\n' \
              '  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\002latex:\\\\[");\n' \
              '  Sciprintf1(indent,"%%s\\t=\\t\\t%%s\\n",pname, nsp_%(typename_dc)s_type_short_string(NSP_OBJECT(M)));\n' \
              '  Sciprintf1(indent+1,"{\\n");\n' \
              '%(fields_latex)s' \
              '  Sciprintf1(indent+1,"}\\n");\n' \
              '  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\\\]\\005");\n' \
              '  return TRUE;\n' \
              '}\n' 

    
    type_tmpl_interface_util = \
              '/*-----------------------------------------------------\n'  \
              ' * a set of functions used when writing interfaces \n'  \
              ' * for %(typename)s objects \n'  \
              ' * Note that some of these functions could become MACROS\n'  \
              ' *-----------------------------------------------------*/\n'  \
              '\n'  \
              '%(typename)s   *nsp_%(typename_dc)s_object(NspObject *O)\n'  \
              '{\n'  \
              '  /* Follow pointer */\n'  \
              '  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;\n'  \
              '  /* Check type */\n'  \
              '  if ( %(interface_1)s (O,nsp_type_%(typename_dc)s_id) == TRUE ) return ((%(typename)s *) O);\n'  \
              '  else \n'  \
              '    Scierror("Error:\tArgument should be a %%s\\n",type_get_name(nsp_type_%(typename_dc)s));\n'  \
              '  return NULL;\n'  \
              '}\n'  \
              '\n'  \
              'int Is%(typename_nn)sObj(Stack stack, int i)\n'  \
              '{\n'  \
              '  return %(interface_2)s(NthObj(i),nsp_type_%(typename_dc)s_id);\n'  \
              '}\n'  \
              '\n'  \
              'int Is%(typename_nn)s(NspObject *O)\n'  \
              '{\n'  \
              '  return %(interface_2)s(O,nsp_type_%(typename_dc)s_id);\n'  \
              '}\n'  \
              '\n'  \
              '%(typename)s  *Get%(typename_nn)sCopy(Stack stack, int i)\n'  \
              '{\n'  \
              '  if (  Get%(typename_nn)s(stack,i) == NULL ) return NULL;\n'  \
              '  return MaybeObjCopy(&NthObj(i));\n'  \
              '}\n'  \
              '\n'  \
              '%(typename)s  *Get%(typename_nn)s(Stack stack, int i)\n'  \
              '{\n'  \
              '  %(typename)s *M;\n'  \
              '  if (( M = nsp_%(typename_dc)s_object(NthObj(i))) == NULL%(typename_uc)s)\n'  \
              '     ArgMessage(stack,i);\n'  \
              '  return M;\n'  \
              '}\n'  \
              '\n'  

    # This is the part used for the generation of the header associated to the class
    # 
    type_header_01 = \
        '/* -*- Mode: C -*- */\n' \
        '#ifndef NSP_INC_%(typename)s\n' \
        '#define NSP_INC_%(typename)s\n' \
        '\n' \
        '/*\n' \
        ' * This Software is GPL (Copyright ENPC 1998-2009) \n' \
        ' * Jean-Philippe Chancelier Enpc/Cermics         \n' \
        ' */\n\n' 
    type_header_1 = \
        '/* %(typename)s */\n' \
        '\n' \
        '#include <nsp/%(parent_dc)s.h>\n' \
        '\n' \
        '/*\n' \
        ' * %(typename)s inherits from %(parent)s\n' \
        ' */\n' \
        '\n' \
        'typedef struct _%(typename)s %(typename)s ;\n' \
        'typedef struct _NspType%(typename_nn)s NspType%(typename_nn)s ;\n' \
        '\n' 
    type_header_2 = \
        '\n' \
        'struct _NspType%(typename_nn)s {\n' \
        '  /*< private >*/\n' \
        '  NSP_TYPE_OBJECT__\n' \
        '  /*< public >*/\n' 
    type_header_3 = \
        '};\n' \
        '\n' \
        '%(fields_ref)s' \
        'struct _%(typename)s {\n' \
        '  /*< private >*/\n' \
        '  Nsp%(parent)s father;\n' \
        '  NspType%(typename_nn)s*type;\n' \
        '  /*< public >*/\n' \
        '  %(fields)s' \
        '};\n' \
        '\n' \
        'extern int nsp_type_%(typename_dc)s_id;\n' \
        'extern NspType%(typename_nn)s *nsp_type_%(typename_dc)s;\n' \
        '\n' \
        '/* type instances for %(parent_dc)s */\n' \
        '\n' \
        'NspType%(typename_nn)s *new_type_%(typename_dc)s(type_mode mode);\n' \
        '\n' \
        '/* instance for %(typename)s */\n' \
        '\n' \
        '%(typename)s *new_%(typename_dc)s();\n' \
        '\n' \
        '/*\n' \
        ' * Object methods redefined for %(typename_dc)s \n' \
        ' */\n' \
        '\n' \
        '\n' \
        '#define NULL%(typename_uc)s (%(typename)s*) 0\n' \
        '\n' \
        'extern %(typename)s *nsp_%(typename_dc)s_create(char *name,%(fields_list)s,NspTypeBase *type);\n' \
        'extern %(typename)s *nsp_%(typename_dc)s_create_default(char *name);\n' \
        '\n' \
        '/* from %(typename)sObj.c */\n' \
        '\n' \
        'extern %(typename)s *nsp_%(typename_dc)s_copy(%(typename)s *H);\n' \
        'extern void nsp_%(typename_dc)s_destroy(%(typename)s *H);\n' \
        'extern int nsp_%(typename_dc)s_info(%(typename)s *H, int indent,const char *name, int rec_level);\n' \
        'extern int nsp_%(typename_dc)s_print(%(typename)s *H, int indent,const char *name, int rec_level);\n' \
        'extern int nsp_%(typename_dc)s_latex(%(typename)s *H, int indent,const char *name, int rec_level);\n' \
        'extern %(typename)s *nsp_%(typename_dc)s_object (NspObject *O);\n' \
        'extern int Is%(typename_nn)sObj (Stack stack, int i);\n' \
        'extern int Is%(typename_nn)s(NspObject *O);\n' \
        'extern %(typename)s *Get%(typename_nn)sCopy (Stack stack, int i);\n' \
        'extern %(typename)s *Get%(typename_nn)s (Stack stack, int i);\n' \
        'extern int nsp_%(typename_dc)s_create_partial(%(typename)s *H);\n' \
        'extern void nsp_%(typename_dc)s_destroy_partial(%(typename)s *H);\n' \
        'extern %(typename)s * nsp_%(typename_dc)s_copy_partial(%(typename)s *H,%(typename)s *self);\n' \
        'extern %(typename)s * nsp_%(typename_dc)s_full_copy_partial(%(typename)s *H,%(typename)s *self);\n' \
        'extern %(typename)s * nsp_%(typename_dc)s_full_copy(%(typename)s *self);\n' \
        'extern int nsp_%(typename_dc)s_check_values(%(typename)s *H);\n' \
        'extern int int_%(typename_dc)s_create(Stack stack, int rhs, int opt, int lhs);\n' \
        'extern %(typename)s *nsp_%(typename_dc)s_xdr_load_partial(XDR *xdrs, %(typename)s *M);\n' \
        'extern int nsp_%(typename_dc)s_xdr_save(XDR  *xdrs, %(typename)s *M);\n' \
        '\n' 

    type_header_31 = \
        '#endif /* NSP_INC_%(typename)s */ \n\n' \
        '#ifdef %(typename)s_Private \n' \
        'static int init_%(typename_dc)s(%(typename)s *o,NspType%(typename_nn)s *type);\n' \
        'static int nsp_%(typename_dc)s_size(%(typename)s *Mat, int flag);\n' \
        'static char *nsp_%(typename_dc)s_type_as_string(void);\n' \
        'static char *nsp_%(typename_dc)s_type_short_string(NspObject *v);\n' \
        'static int nsp_%(typename_dc)s_eq(%(typename)s *A, NspObject *B);\n' \
        'static int nsp_%(typename_dc)s_neq(%(typename)s *A, NspObject *B);\n' \
        'static %(typename)s *nsp_%(typename_dc)s_xdr_load(XDR *xdrs);\n' \
        'static AttrTab %(typename_dc)s_attrs[];\n' \
        'static NspMethods *%(typename_dc)s_get_methods(void);\n' \
        '/* static int int_%(typename_dc)s_create(Stack stack, int rhs, int opt, int lhs);*/ \n' \
        'static %(typename)s *nsp_%(typename_dc)s_create_void(char *name,NspTypeBase *type);\n' 
    
    type_header_4 = \
        '#endif /* %(typename)s_Private */\n\n'

    slots_list = ['tp_getattr', 'tp_setattr' ]

    getter_tmpl = \
        'static NspObject *%(funcname)s(void *self,const char *attr)\n' \
        '{\n' \
        '%(varlist)s' \
        '  ret = %(field)s;\n' \
        '%(attrcodeafter)s\n' \
        '}\n\n'

    getterobj_tmpl = \
        'static NspObject *%(funcname)s(void *self,const char *attr, int *copy)\n' \
        '{\n' \
        '%(varlist)s' \
        '  *copy = FALSE;\n'\
        '  ret = %(field)s;\n' \
        '%(attrcodeafter)s\n' \
        '}\n\n'

    setter_tmpl = \
        'static int %(funcname)s(void *self,const char *attr, NspObject *O)\n' \
        '{\n' \
        '%(varlist)s' \
        '%(attr_set_code)s' \
        '  return OK;\n' \
        '}\n\n'

    parse_tmpl = \
        '  if ( GetArgs(stack,rhs,opt,T,%(parselist)s) == FAIL) return RET_BUG;\n'
    
    deprecated_tmpl = \
        '  Scierror("%%s: deprecated %(deprecationmsg)s",stack.fname); return RET_BUG;\n'
    
    methdef_tmpl = '  {"%(name)s",(nsp_method *) %(cname)s},\n'
    funcdef_tmpl = '  {"%(name)s", %(cname)s},\n'

    noconstructor = '' \
    # this is used when a default constructor giving a Warning must be used 

    function_tmpl = \
        'int _wrap_%(cname_1)s(Stack stack, int rhs, int opt, int lhs) /* %(name)s */\n' \
        '{\n' \
        '%(varlist)s' \
        '%(parseargs)s' \
        '%(codebefore)s' \
        '    %(setreturn)s%(cname)s(%(arglist)s);\n' \
        '%(codeafter)s\n' \
        '}\n\n'

    # template for method calls
    constructor_tmpl = None
    method_tmpl = None

    def __init__(self, parser, objinfo, overrides, fp=FileOutput(sys.stdout),byref=None):
        self.parser = parser
        self.objinfo = objinfo
        self.overrides = overrides
        self.fp = fp
        self.byref = byref

    # used to build the attribute interface names
    # 

    def get_lower_name(self):
        # return string.lower(string.replace(self.objinfo.typecode,'_TYPE_', '_', 1))
        return string.lower(self.objinfo.name)

    def get_lower_c_name(self):
        # return string.lower(string.replace(self.objinfo.typecode,'_TYPE_', '_', 1))
        return string.lower(self.objinfo.c_name)
    
    def get_field_accessor(self, fieldname, fieldtype):
        raise NotImplementedError

    
    def get_initial_class_substdict(self): return { 'interface_1' : 'check_cast',
                                                    'interface_2' : 'nsp_object_type'}

    def get_initial_constructor_substdict(self):
        return { 'name': '%s.__init__' % self.objinfo.c_name,
                 'errorreturn': '-1' }
    def get_initial_method_substdict(self, method):
        return { 'name': '%s.%s' % (self.objinfo.c_name, method.name) }

    def write_class(self):
        self.fp.write('\n/* ----------- ' + self.objinfo.c_name + ' ----------- */\n\n')
        substdict = self.get_initial_class_substdict()
        # name used for local variables
        substdict['var_loc1'] = self.var_loc1
        substdict['var_loc2'] = self.var_loc2

        if self.overrides.modulename:
            classname = '%s.%s' % (self.overrides.modulename,self.objinfo.name)
        else:
            classname = self.objinfo.name

        substdict['classname'] = classname 
        typename = self.objinfo.c_name
        typename_nn = self.objinfo.name
        substdict['typecode'] =  self.objinfo.typecode
        substdict['typename'] =  typename
        # typename but not the c_name 
        substdict['typename_nn'] =  typename_nn
        substdict['typename_dc'] = string.lower(self.objinfo.name)
        substdict['typename_uc'] = string.upper(self.objinfo.name)
        substdict['parent'] = self.objinfo.parent
        substdict['parent_dc'] = string.lower(self.objinfo.parent)
        substdict['tp_getattr_def'] = 'int_get_attribute';
        substdict['tp_setattr_def'] = 'int_set_attribute';


        # check if some slots are overriden and add them in substdict
        # tp_setattr and tp_getattr 

        for slot in self.slots_list:
            if substdict.has_key(slot) and substdict[slot] != '0':
                continue
            slotname = '%s.%s' % (self.objinfo.c_name, slot)
            slotfunc = '_wrap_%s_%s' % (self.get_lower_c_name(), slot)
            if slot[:6] == 'tp_as_':
                slotfunc = '&' + slotfunc
            if self.overrides.slot_is_overriden(slotname):
                substdict[slot] = slotfunc
                self.fp.write('static int %s(Stack stack, int rhs, int opt, int lhs);\n' %substdict[slot]);
            else:
                if substdict.has_key(slot+'_def'): 
                    substdict[slot] = substdict[slot+'_def']
                else:
                    substdict[slot] = '0'

        # this can be used to change the tp_setattr and tp_getattr functions.
        # if self.objinfo.is_interface == 't' :

        # insert the comment for implements in dictionary 
        str = ''
        substdict['implements'] = ''
        if len(self.objinfo.implements) != 0 :
            str = ' * and implements '
            for interf in self.objinfo.implements:
                str = '%s %s' % (str, interf )
            str = str + '\n'
            substdict['implements'] = str 

        # insert the type defintion 

        self.fp.write(self.type_tmpl_1_0 % substdict)
        # insert declaration for implemented interfaces 
        if len(self.objinfo.implements) != 0 :
            for interf in self.objinfo.implements:
                self.fp.write('  NspType%s *t_%s;\n' % (interf,string.lower(interf)));
        self.fp.write(self.type_tmpl_1_0_1 % substdict)
        # insert type override code 
        if self.overrides.part_type_is_overriden(typename_nn):
            lineno, filename = self.overrides.getstartline(typename_nn)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_type(typename_nn))
            self.fp.resetline()

        self.fp.write(self.type_tmpl_1_0_2 % substdict)
        # insert the implemented interfaces
        if len(self.objinfo.implements) != 0 :
            ti = 'type->interface'
            for interf in self.objinfo.implements:
                l_interf = string.lower(interf);
                self.fp.write('  t_%s = new_type_%s(T_DERIVED);\n' % (l_interf,l_interf))
                self.fp.write('  %s = (NspTypeBase *) t_%s;\n' % (ti,l_interf))
                #self.fp.write('  nsp_initialize_interface_%s(t_%s);\n' % (l_interf,l_interf))
                ti = ti + '->interface'
        # insert override code for implemented interfaces 
        if self.overrides.part_implements_is_overriden(typename_nn):
            slot = '%s_implements' % (typename_nn)
            lineno, filename = self.overrides.getstartline(slot)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_implements(typename_nn))
            self.fp.resetline()

        # insert fields related code in subst dictionary
        # fields_copy: used in create function 
        substdict['fields_copy'] = self.build_copy_fields('H','','nsp_object_copy')
        # for create default
        substdict['fields_copy_default'] = self.build_copy_fields_default('H')
        # fields_copy: used in copy function 
        substdict['fields_copy_self'] = self.build_copy_fields('H','self','nsp_object_copy')
        substdict['fields_full_copy_self'] = self.build_copy_fields('H','self','nsp_object_full_copy')
        # fields_copy: used in full copy function 
        substdict['fields_full_copy_partial_code'] = self.build_fields_full_copy_partial_code(substdict,'H','self','nsp_object_full_copy')
        # give the sequence of parameters to be inserted in _create function
        substdict['fields_list'] = self.build_list_fields('')
        # for the save function 
        substdict['fields_save'] = self.build_save_fields('M')
        # for the load function 
        substdict['fields_load'] = self.build_load_fields('M')
        substdict['fields_info'] = self.build_info_fields('M')
        substdict['fields_print'] = self.build_print_fields('M','print')
        substdict['fields_latex'] = self.build_print_fields('M','latex')
        substdict['fields_init'] = self.build_init_fields('Obj')
        # used in int_xxx_create interface 
        substdict['fields_from_attributes'] = self.build_fields_from_attributes('H')
        substdict['fields_defval'] = self.build_defval_fields('H')
        substdict['fields_free1'] = self.build_fields_free1('H')
        substdict['fields_free2'] = self.build_fields_free2('H')
        substdict['fields_equal'] = self.build_equal_fields('H')
        substdict['create_partial'] = self.build_create_partial('H')
        substdict['copy_partial'] = self.build_copy_partial('H','')
        substdict['full_copy_partial'] = self.build_copy_partial('H','full_')
        substdict['full_copy_code'] = self.build_full_copy_code(substdict,'H')
        # fields for declaration 
        substdict['fields'] = self.build_fields()
        # methods to be inserted in the class declaration 
        substdict['internal_methods'] = self.build_internal_methods()
        # prototypes for the class methods 
        substdict['internal_methods_proto'] = self.build_internal_methods_protos()
        substdict['fields_ref'] = self.build_fields_ref()
        if self.byref != 't' :
            substdict['ref_count']= ''
            substdict['ref_count_ref']= ''
        else:
            # if we are a by ref object print the ref counter 
            substdict['ref_count']= '(nref=%d)'
            substdict['ref_count_ref']= ',M->obj->ref_count'
        # insert the end of type defintion 
        self.fp.write(self.type_tmpl_1_1 % substdict)
        self.fp.write(self.type_tmpl_1_1_1 % substdict)

        # insert override code for save_load 
        if self.overrides.part_save_load_is_overriden(typename_nn):
            slot = '%s_save_load' % (typename_nn)
            lineno, filename = self.overrides.getstartline(slot)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_save_load(typename_nn))
            self.fp.resetline()
        else:
            # insert the code for save
            self.fp.write(self.type_tmpl_save % substdict)
            # insert the code for load 
            self.fp.write(self.type_tmpl_load_1 % substdict)
            substdict['ret']= 'NULL'
            self.build_int_create_final('H',typename_nn,substdict)
            self.fp.write(self.type_tmpl_load_2 % substdict)

        # destroy code 
        self.fp.write(self.type_tmpl_delete % substdict)
        
        if self.overrides.part_destroy_is_overriden(typename_nn):
            stn = 'destroy_%s' % typename_nn
            lineno, filename = self.overrides.getstartline(stn)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_destroy(typename_nn))
            self.fp.resetline()
            
        self.fp.write(self.type_tmpl_1_1_1_1 % substdict)
        # insert the end of type definition 
        # i.e a set of functions used for writing interfaces 
        self.fp.write(self.type_tmpl_interface_util % substdict)

        # code for create 
        self.fp.write(self.type_tmpl_create_header % substdict)
        if self.overrides.part_create_is_overriden(typename_nn):
            slot = '%s_create' % (typename_nn)
            lineno, filename = self.overrides.getstartline(slot)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_create(typename_nn))
            self.fp.resetline()
        else:
            # insert the code for create
            self.fp.write(self.type_tmpl_create_0 % substdict)
            substdict['ret']= 'NULL'
            self.build_int_create_final('H',typename_nn,substdict)
            self.fp.write(self.type_tmpl_create_1 % substdict)

        # code for copy 
        self.fp.write(self.type_tmpl_copy_1 % substdict)
        substdict['ret']= 'NULL'
        self.build_int_create_final('H',typename_nn,substdict)
        self.fp.write(self.type_tmpl_copy_2 % substdict)

        # write the int_create inteface 
        if self.overrides.part_intcreate_is_overriden(typename_nn):
            slot = '%s_intcreate' % (typename_nn)
            lineno, filename = self.overrides.getstartline(slot)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_intcreate(typename_nn))
            self.fp.resetline()
        else:
            self.fp.write(self.type_tmpl_intcreate % substdict)
            substdict['ret']= 'RET_BUG'
            self.build_int_create_final('H',typename_nn,substdict)
            self.fp.write(self.type_tmpl_intcreate_last % substdict)

        # write a header file for class object
        outheadername = './' + string.lower(self.objinfo.name) + '.h'
        self.fhp = FileOutput(open(outheadername, "w"),outheadername)
        self.fhp.write(self.type_header_01 %substdict)
        # include start code from override 
        hname = self.objinfo.name+'.include_start'
        if self.overrides.include_start_is_overriden(hname):
            lineno, filename = self.overrides.getstartline(hname)
            self.fhp.setline(lineno,'codegen/'+ filename)
            self.fhp.write(self.overrides.include_start_override(hname))
            self.fhp.resetline()
        # 
        self.fhp.write(self.type_header_1 %substdict)
        self.fhp.write('%(internal_methods_proto)s' %substdict)
        self.fhp.resetline()
        self.fhp.write(self.type_header_2 %substdict)
        self.fhp.write(' %(internal_methods)s\n' %substdict)
        self.fhp.resetline()
        self.fhp.write(self.type_header_3 %substdict)
        # insert part from .override 
        hname = self.objinfo.name+'.include_public'
        if self.overrides.include_public_is_overriden(hname):
            lineno, filename = self.overrides.getstartline(hname)
            self.fhp.setline(lineno,'codegen/'+ filename)
            self.fhp.write(self.overrides.include_public_override(hname))
            self.fhp.resetline()
        self.fhp.write(self.type_header_31 %substdict)
        hname = self.objinfo.name+'.include_private'
        if self.overrides.include_private_is_overriden(hname):
            lineno, filename = self.overrides.getstartline(hname)
            self.fhp.setline(lineno,'codegen/'+ filename)
            self.fhp.write(self.overrides.include_private_override(hname))
            self.fhp.resetline()
        self.fhp.write(self.type_header_4 %substdict)

        self.fhp.close() 
        
        substdict['tp_init'] = self.write_constructor()

        self.fp.write( '/*-------------------------------------------\n')
        self.fp.write( ' * Methods\n')
        self.fp.write( ' *-------------------------------------------*/\n')

        substdict['tp_methods'] = self.write_methods()
        substdict['tp_getset'] = self.write_getsets()

        # handle slots ...
        for slot in self.slots_list:
            #if substdict.has_key(slot) and substdict[slot] != '0':
            #    continue
            slotname = '%s.%s' % (self.objinfo.c_name, slot)
            slotfunc = '_wrap_%s_%s' % (self.get_lower_c_name(), slot)
            if slot[:6] == 'tp_as_':
                slotfunc = '&' + slotfunc
            if self.overrides.slot_is_overriden(slotname):
                lineno, filename = self.overrides.getstartline(slotname)
                self.fp.setline(lineno,'codegen/'+ filename)
                self.fp.write(self.overrides.slot_override(slotname))
                self.fp.resetline()
                self.fp.write('\n\n')
                substdict[slot] = slotfunc
            else:
                substdict[slot] = '0'


    def write_struct(self):
        # for struct fields which are not embeded in Nsp class
        self.fp.write('\n/* ----------- ' + self.objinfo.c_name + ' ----------- */\n\n')
        substdict = self.get_initial_class_substdict()
        # name used for local variables
        substdict['var_loc1'] = self.var_loc1
        substdict['var_loc2'] = self.var_loc2

        if self.overrides.modulename:
            classname = '%s.%s' % (self.overrides.modulename,self.objinfo.name)
        else:
            classname = self.objinfo.name

        substdict['classname'] = classname 
        typename = self.objinfo.c_name
        typename_nn = self.objinfo.name
        substdict['typecode'] =  self.objinfo.typecode
        substdict['typename'] =  typename
        # typename but not the c_name 
        substdict['typename_nn'] =  typename_nn
        substdict['typename_dc'] = string.lower(self.objinfo.name)
        substdict['typename_uc'] = string.upper(self.objinfo.name)
        substdict['parent'] = self.objinfo.parent
        substdict['parent_dc'] = string.lower(self.objinfo.parent)
        substdict['tp_getattr_def'] = 'int_get_attribute';
        substdict['tp_setattr_def'] = 'int_set_attribute';

        # check if some slots are overriden and add them in substdict
        # tp_setattr and tp_getattr 
        
        for slot in self.slots_list:
            if substdict.has_key(slot) and substdict[slot] != '0':
                continue
            slotname = '%s.%s' % (self.objinfo.c_name, slot)
            slotfunc = '_wrap_%s_%s' % (self.get_lower_c_name(), slot)
            if slot[:6] == 'tp_as_':
                slotfunc = '&' + slotfunc
            if self.overrides.slot_is_overriden(slotname):
                substdict[slot] = slotfunc
                self.fp.write('static int %s(Stack stack, int rhs, int opt, int lhs);\n' %substdict[slot]);
            else:
                if substdict.has_key(slot+'_def'): 
                    substdict[slot] = substdict[slot+'_def']
                else:
                    substdict[slot] = '0'

        # this can be used to change the tp_setattr and tp_getattr functions.
        # if self.objinfo.is_interface == 't' :
        str = ''
        substdict['implements'] = ''
        # insert fields related code in subst dictionary
        # fields_copy: used in create function 
        substdict['fields_copy'] = self.build_copy_fields('H','','nsp_object_copy')
        # for create default
        substdict['fields_copy_default'] = self.build_copy_fields_default('H')
        # fields_copy: used in copy function 
        substdict['fields_copy_self'] = self.build_copy_fields('H','self','nsp_object_copy')
        substdict['fields_full_copy_self'] = self.build_copy_fields('H','self','nsp_object_full_copy')
        # fields_copy: used in full copy function 
        substdict['fields_full_copy_partial_code'] = self.build_fields_full_copy_partial_code(substdict,'H','self','nsp_object_copy')
        # give the sequence of parameters to be inserted in _create function
        substdict['fields_list'] = self.build_list_fields('')
        # for the save function 
        substdict['fields_save'] = self.build_save_fields('M')
        # for the load function 
        substdict['fields_load'] = self.build_load_fields('M')
        substdict['fields_info'] = self.build_info_fields('M')
        substdict['fields_print'] = self.build_print_fields('M','print')
        substdict['fields_latex'] = self.build_print_fields('M','latex')
        substdict['fields_init'] = self.build_init_fields('Obj')
        # used in int_xxx_create interface 
        substdict['fields_from_attributes'] = self.build_fields_from_attributes('H')
        substdict['fields_defval'] = self.build_defval_fields('H')
        substdict['fields_free1'] = self.build_fields_free1('H')
        substdict['fields_free2'] = self.build_fields_free2('H')
        substdict['fields_equal'] = self.build_equal_fields('H')
        substdict['create_partial'] = self.build_create_partial('H')
        substdict['copy_partial'] = self.build_copy_partial('H','')
        substdict['full_copy_partial'] = self.build_copy_partial('H','full_')
        substdict['full_copy_code'] = self.build_full_copy_code(substdict,'H')
        # fields for declaration 
        substdict['fields'] = self.build_fields()
        # methods to be inserted in the class declaration 
        substdict['internal_methods'] = self.build_internal_methods()
        # prototypes for the class methods 
        substdict['internal_methods_proto'] = self.build_internal_methods_protos()
        substdict['fields_ref'] = self.build_fields_ref()
        if self.byref != 't' :
            substdict['ref_count']= ''
            substdict['ref_count_ref']= ''
        else:
            # if we are a by ref object print the ref counter 
            substdict['ref_count']= '(nref=%d)'
            substdict['ref_count_ref']= ',M->obj->ref_count'
        # insert the end of type defintion 
        # insert override code for save_load 
        if self.overrides.part_save_load_is_overriden(typename_nn):
            slot = '%s_save_load' % (typename_nn)
            lineno, filename = self.overrides.getstartline(slot)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_save_load(typename_nn))
            self.fp.resetline()
        else:
            # insert the code for save
            self.fp.write(self.type_tmpl_save % substdict)
            # insert the code for load 
            self.fp.write(self.type_tmpl_load_1 % substdict)
            substdict['ret']= 'NULL'
            self.build_int_create_final('H',typename_nn,substdict)
            self.fp.write(self.type_tmpl_load_2 % substdict)

        # destroy code 
        self.fp.write(self.type_tmpl_delete % substdict)

        if self.overrides.part_destroy_is_overriden(typename_nn):
            stn = 'destroy_%s' % typename_nn
            lineno, filename = self.overrides.getstartline(stn)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_destroy(typename_nn))
            self.fp.resetline()

        self.fp.write(self.type_tmpl_1_1_1_1 % substdict)
        # insert the end of type definition 
        # i.e a set of functions used for writing interfaces 
        self.fp.write(self.type_tmpl_interface_util % substdict)
        # code for create 

        self.fp.write(self.type_tmpl_create_header % substdict)
        if self.overrides.part_create_is_overriden(typename_nn):
            slot = '%s_create' % (typename_nn)
            lineno, filename = self.overrides.getstartline(slot)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_create(typename_nn))
            self.fp.resetline()
        else:
            # insert the code for create
            self.fp.write(self.type_tmpl_create_0 % substdict)
            substdict['ret']= 'NULL'
            self.build_int_create_final('H',typename_nn,substdict)
            self.fp.write(self.type_tmpl_create_1 % substdict)

        # code for copy 
        self.fp.write(self.type_tmpl_copy_1 % substdict)
        substdict['ret']= 'NULL'
        self.build_int_create_final('H',typename_nn,substdict)
        self.fp.write(self.type_tmpl_copy_2 % substdict)

        # write the int_create inteface 
        if self.overrides.part_intcreate_is_overriden(typename_nn):
            slot = '%s_intcreate' % (typename_nn)
            lineno, filename = self.overrides.getstartline(slot)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_intcreate(typename_nn))
            self.fp.resetline()
        else:
            self.fp.write(self.type_tmpl_intcreate % substdict)
            substdict['ret']= 'RET_BUG'
            self.build_int_create_final('H',typename_nn,substdict)
            self.fp.write(self.type_tmpl_intcreate_last % substdict)

        # write a header file for class object
        outheadername = './' + string.lower(self.objinfo.name) + '.h'
        self.fhp = FileOutput(open(outheadername, "w"),outheadername)
        self.fhp.write(self.type_header_01 %substdict)
        # include start code from override 
        hname = self.objinfo.name+'.include_start'
        if self.overrides.include_start_is_overriden(hname):
            lineno, filename = self.overrides.getstartline(hname)
            self.fhp.setline(lineno,'codegen/'+ filename)
            self.fhp.write(self.overrides.include_start_override(hname))
            self.fhp.resetline()
        # 
        self.fhp.write(self.type_header_1 %substdict)
        self.fhp.write('%(internal_methods_proto)s' %substdict)
        self.fhp.resetline()
        self.fhp.write(self.type_header_2 %substdict)
        self.fhp.write(' %(internal_methods)s\n' %substdict)
        self.fhp.resetline()
        self.fhp.write(self.type_header_3 %substdict)
        # insert part from .override 
        hname = self.objinfo.name+'.include_public'
        if self.overrides.include_public_is_overriden(hname):
            lineno, filename = self.overrides.getstartline(hname)
            self.fhp.setline(lineno,'codegen/'+ filename)
            self.fhp.write(self.overrides.include_public_override(hname))
            self.fhp.resetline()
        self.fhp.write(self.type_header_31 %substdict)
        hname = self.objinfo.name+'.include_private'
        if self.overrides.include_private_is_overriden(hname):
            lineno, filename = self.overrides.getstartline(hname)
            self.fhp.setline(lineno,'codegen/'+ filename)
            self.fhp.write(self.overrides.include_private_override(hname))
            self.fhp.resetline()
        self.fhp.write(self.type_header_4 %substdict)

        self.fhp.close() 
        
        substdict['tp_init'] = self.write_constructor()

        self.fp.write( '/*-------------------------------------------\n')
        self.fp.write( ' * Methods\n')
        self.fp.write( ' *-------------------------------------------*/\n')

        substdict['tp_methods'] = self.write_methods()
        substdict['tp_getset'] = self.write_getsets()

        # handle slots ...
        for slot in self.slots_list:
            #if substdict.has_key(slot) and substdict[slot] != '0':
            #    continue
            slotname = '%s.%s' % (self.objinfo.c_name, slot)
            slotfunc = '_wrap_%s_%s' % (self.get_lower_c_name(), slot)
            if slot[:6] == 'tp_as_':
                slotfunc = '&' + slotfunc
            if self.overrides.slot_is_overriden(slotname):
                lineno, filename = self.overrides.getstartline(slotname)
                self.fp.setline(lineno,'codegen/'+ filename)
                self.fp.write(self.overrides.slot_override(slotname))
                self.fp.resetline()
                self.fp.write('\n\n')
                substdict[slot] = slotfunc
            else:
                substdict[slot] = '0'

    def build_fields(self):
        if self.byref == 't' :
            str =  'nsp_%s *obj;\n' % string.lower(self.objinfo.name)
            return str
        lower_name = self.get_lower_c_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt, pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str + handler.attr_write_field_declaration(ftype,fname,opt,pdef,psize,pcheck) 
        return str 

    def build_internal_methods(self):
        return  self.overrides.override_internal_methods

    def build_internal_methods_protos(self):
        return  self.overrides.override_internal_methods_protos

    def build_fields_ref(self):
        if self.byref != 't' :
            return ''
        cn = self.get_lower_name()
        str = 'typedef struct _nsp_%s nsp_%s;\n' \
              'struct _nsp_%s {\n'  % (cn,cn,cn)
        if not self.objinfo.fields:
            return str + '};\n' 
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str + handler.attr_write_field_declaration(ftype,fname,opt,pdef,psize,pcheck) 
        str = str + '  int ref_count;\n};\n\n' 
        return str

    def build_list_fields(self,flag):
        lower_name = self.get_lower_c_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            if str:
                if str[-1] != ',':
                    str = str + ',' 
            str = str + handler.attr_write_create_call(ftype,fname,opt,pdef,psize,pcheck,flag) 
        return str


    def build_copy_fields(self,left_varname,right_varname,f_copy_name):
        # used in copy function and in create function 
        # when right_varname == 'self' we are in copy function and else 
        # in create function. 
        # if full is 't' then we make a full copy even if we are in
        # 'self' mode 
        # f_copy_name is the name of the function used to copy objects
        # i.e nsp_object_copy or nsp_object_full_copy
        lower_name = self.get_lower_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        if self.byref == 't':
            if right_varname == 'self':
                str = '  H->obj = self->obj; self->obj->ref_count++;\n' 
                return str
            else:
                # used in type_create functions
                left_varname = left_varname + '->obj' 
                str = '  if ((H->obj = calloc(1,sizeof(nsp_%s))) == NULL) return NULL;\n' \
                      '  H->obj->ref_count=1;\n' % (lower_name)
                str = '  if ( nsp_%s_create_partial(H) == FAIL) return NULL%s;\n' % (lower_name,string.upper(lower_name)) 

        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str + handler.attr_write_copy(ftype,fname,left_varname,right_varname,f_copy_name,self.byref, pdef , psize, pcheck)
        return str
    
    
    def build_copy_fields_default(self,left_varname):
        # used in copy function and in create function 
        # when right_varname == 'self' we are in copy function and else 
        # in create function. 
        # if full is 't' then we make a full copy even if we are in
        # 'self' mode 
        lower_name = self.get_lower_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        if self.byref == 't':
            # used in type_create functions
            left_varname = left_varname + '->obj' 
            str = '  if ((H->obj = calloc(1,sizeof(nsp_%s))) == NULL) return NULL;\n' \
                '  H->obj->ref_count=1;\n' % (lower_name)
            str = '  if ( nsp_%s_create_partial(H) == FAIL) return NULL%s;\n' % (lower_name,string.upper(lower_name)) 
        return str


    def build_fields_full_copy_partial_code(self,substdic, left_varname,right_varname,f_copy_name):
        # generate a full_copy_partial function which is useful 
        # for full_copy of byref objects.
        lower_name = self.get_lower_name()
        if self.byref != 't':
            # if object is not by reference then full copy is similar to copy 
            # except that the full_copy object functions should be used
            return '%(typename)s *nsp_%(typename_dc)s_full_copy_partial(%(typename)s *H,%(typename)s *self)\n'  \
                '{\n'  \
                '%(fields_full_copy_self)s' \
                '  return H;\n' \
                '}\n\n'  % substdic 
        right_varname = right_varname + '->obj'
        # no overrides for the whole function.  If no fields, don't write a func
        str = '%(typename)s *nsp_%(typename_dc)s_full_copy_partial(%(typename)s *H,%(typename)s *self)\n' \
            '{\n'  % substdic 

        if self.byref == 't':
            # used in type_create functions
            left_varname = left_varname + '->obj' 
            str = str + '  if ((H->obj = calloc(1,sizeof(nsp_%s))) == NULL) return NULL%s;\n' \
                '  H->obj->ref_count=1;\n' % (lower_name,string.upper(lower_name))

        if self.objinfo.fields:
            for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
                handler = argtypes.matcher.get(ftype)
                str = str + handler.attr_write_copy(ftype,fname,left_varname,right_varname,f_copy_name,self.byref,pdef,psize,pcheck)
        str = str + '  return H;\n' \
            '}\n\n'  
        return str

    def build_save_fields(self,varname):
        lower_name = self.get_lower_c_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        if self.byref == 't' :
            varname = varname +'->obj'
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            if opt != 'hidden' :
                str = str + handler.attr_write_save(ftype,fname,varname,self.byref, pdef , psize, pcheck)
        father = self.objinfo.parent
        if father != 'Object':
            str = str + '  if ( nsp_%s_xdr_save(xdrs, (%s *) M)== FAIL) return FAIL;\n' % (string.lower(father),'Nsp'+father)
        return str

    def build_info_fields(self,varname):
        lower_name = self.get_lower_c_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        if self.byref == 't' :
            varname = varname +'->obj'
        
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str + handler.attr_write_info(ftype, fname,varname,self.byref)
        return str

    def build_print_fields(self,varname,print_mode):
        lower_name = self.get_lower_c_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        father = self.objinfo.parent
        
        if self.byref == 't' :
            varname = varname +'->obj'

        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str + handler.attr_write_print(ftype, fname,varname,self.byref,print_mode, pdef , psize, pcheck)
        if father != 'Object':
            str = str + '  nsp_%s_%s((%s *) M,indent+2,NULL,rec_level);\n'  % (string.lower(father),print_mode,'Nsp'+father)
        return str

    def build_init_fields(self,varname):
        # initialize the fields to default value but only for 
        # object that are not byref 
        lower_name = self.get_lower_c_name()
        str = ''
        if self.byref == 't' :
            return '  '+varname+'->obj = NULL;\n'
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str 
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str + handler.attr_write_init(ftype, fname,varname, self.byref, pdef, psize, pcheck )
        return str 
    
    def build_load_fields(self,varname):
        lower_name = self.get_lower_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        father = self.objinfo.parent
        if father != 'Object':
            str = str + '  int fid;\n  char name[NAME_MAXL];\n'
            
        if self.byref == 't' :
            #str = str + '  if ((%s->obj = calloc(1,sizeof(nsp_%s))) == NULL) return NULL;\n' % (varname,lower_name)
            str = str + '  %s->obj->ref_count=1;\n' % (varname)
            varname = varname +'->obj'

        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            if opt != 'hidden' :
                str = str + handler.attr_write_load(ftype,fname,varname,self.byref, pdef, psize, pcheck)

        if father != 'Object':
            str = str + '  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;\n'
            str = str + '  if ( fid == nsp_dynamic_id)\n'
            str = str + '    {\n'
            str = str + '     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;\n'
            str = str + '    }\n'
            str = str + '  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;\n'
            str = str + '  if ( nsp_%s_xdr_load_partial(xdrs,(%s *)M) == NULL) return NULL;\n' \
                %  (string.lower(father),'Nsp'+father)
        return str

    def build_fields_from_attributes(self,varname):
        lower_name = self.get_lower_name()
        # called in create interface 
        str = ''
        if self.byref == 't' :
            varname = varname + '->obj'
            str = '  if ( nsp_%s_create_partial(H) == FAIL) return RET_BUG;\n' % (lower_name) 
        str = str + '  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;\n'
        if not self.objinfo.fields:
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            # str = str + handler.attr_check_null(ftype, fname,varname,self.byref)
        return str

    def build_defval_fields(self,varname):
        lower_name = self.get_lower_c_name()
        # no overrides for the whole function.  If no fields, don't write a func
        str = ''
        father = self.objinfo.parent
        if self.byref == 't' :
            varname = varname + '->obj'
            
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str +  handler.attr_write_defval(ftype,fname,varname,self.byref, pdef , psize, pcheck)
        if father != 'Object':
            str = str + '  nsp_%s_check_values((%s *) H);\n'  % (string.lower(father),'Nsp'+father)
        return str
    
    def build_fields_free1(self,varname):
        lower_name = self.get_lower_c_name()
        # build the code used to free attribute of an object 
        str = '' 
        father = self.objinfo.parent
        if self.byref == 't':
            if father != 'Object':
                str = '  nsp_%s_destroy_partial((%s *) H);\n'  % (string.lower(father),'Nsp'+father)
            str = str +  '  %s->obj->ref_count--;\n' \
                '  if ( %s->obj->ref_count == 0 )\n   {\n' % (varname,varname)
            varname = varname +'->obj'
        else:
            if father != 'Object':
                str = str + '  nsp_%s_destroy_partial((%s *) H);\n'  % (string.lower(father),'Nsp'+father)
        return str

    def build_fields_free2(self,varname):
        lower_name = self.get_lower_c_name()
        # build the code used to free attribute of an object 
        str = '' 
        father = self.objinfo.parent
        if self.byref == 't':
            varname = varname +'->obj'
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str + handler.attr_free_fields(ftype,fname,varname,self.byref)
        if self.byref == 't':
            str = str + '    FREE(%s);\n   }\n'  % (varname)
        return str

    def build_int_create_final(self,varname,typename_nn,substdict):
        # code to be called before returning in the _create 
        # interface By defaut this code is 
        # empty but can be filled in override code 
        if self.overrides.part_int_create_final_is_overriden(typename_nn):
            stn = 'int_create_final_%s' % typename_nn
            lineno, filename = self.overrides.getstartline(stn)
            self.fp.setline(lineno,'codegen/'+ filename)
            self.fp.write(self.overrides.get_override_int_create_final(typename_nn) % substdict)
            self.fp.resetline()
            # return self.overrides.override_int_create_final
        
    def build_create_partial(self,varname):
        # used when creating a new instance 
        # only useful for by ref objects 
        lower_name = self.get_lower_name()
        str = '' 
        father = self.objinfo.parent
        if self.byref == 't':
            if father != 'Object':
                str = '  if ( nsp_%s_create_partial((%s *) H)== FAIL) return FAIL;\n' % (string.lower(father),'Nsp'+father)
            str = str +  '  if((H->obj = calloc(1,sizeof(nsp_%s)))== NULL ) return FAIL;\n' \
                '  H->obj->ref_count=1;\n' % (lower_name)  
            if not self.objinfo.fields:
                return str 
            for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
                handler = argtypes.matcher.get(ftype)
                str = str + handler.attr_write_init(ftype,fname,varname+'->obj', self.byref, pdef, psize, pcheck )
        return str
            
    def build_copy_partial(self,varname,full):
        # no overrides for the whole function.  If no fields, don't write a func
        lower_name = self.get_lower_name()
        str = '' 
        father = self.objinfo.parent
        if father != 'Object':
            str = '  if ( nsp_%s_%scopy_partial((%s *) H,(%s *) self ) == NULL) return NULL%s;\n' \
                % (string.lower(father),full,'Nsp'+father,'Nsp'+father,string.upper(lower_name))
        str = str + '  if ( nsp_%s_%scopy_partial(H,self)== NULL) return NULL%s;\n' \
            % (lower_name,full,string.upper(lower_name)) 
        return str


    def build_full_copy_code(self,substdict,varname):
        # This function generates the code for full_copy of an object 
        # at the end we insert a part which can be inserted from override 
        # override_int_create_final 
        if self.byref != 't' :
            # here full_copy is similar to copy except that full_copy_partial 
            # is used
            str = '%(typename)s *nsp_%(typename_dc)s_full_copy(%(typename)s *self)\n'  \
                '{\n'  \
                '  %(typename)s *H  =nsp_%(typename_dc)s_create_void(NVOID,(NspTypeBase *) nsp_type_%(typename_dc)s);\n'  \
                '  if ( H ==  NULL%(typename_uc)s) return NULL%(typename_uc)s;\n' \
                '%(full_copy_partial)s\n'   % substdict 
            return str
        
        str = '%(typename)s *nsp_%(typename_dc)s_full_copy(%(typename)s *self)\n'  \
            '{\n'  \
            '  %(typename)s *H  =nsp_%(typename_dc)s_create_void(NVOID,(NspTypeBase *) nsp_type_%(typename_dc)s);\n'  \
            '  if ( H ==  NULL%(typename_uc)s) return NULL%(typename_uc)s;\n' % substdict 
        lower_name = self.get_lower_name()
        father = self.objinfo.parent
        if father != 'Object':
            str = str + '  if ( nsp_%s_full_copy_partial((%s *) H,(%s *) self ) == NULL) return NULL%s;\n' \
                % (string.lower(father),'Nsp'+father,'Nsp'+father,string.upper(lower_name))
        str = str + '  if ( nsp_%s_full_copy_partial(H,self)== NULL) return NULL%s;\n' \
            % (lower_name,string.upper(lower_name)) 
        return str 

    def build_equal_fields(self,varname):
        lower_name = self.get_lower_c_name()
        # no overrides for the whole function.  If no fields, don't write a func
        if self.byref == 't' :
            str = '  if ( A->obj == loc->obj ) return TRUE;\n'
        else:
            str = ''
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.c_name)
            return str
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            handler = argtypes.matcher.get(ftype)
            str = str + handler.attr_equal_fields(ftype,fname,varname,self.byref, pdef, psize, pcheck)
        return str

    def write_function_wrapper(self, function_obj, template,
                               handle_return=0, is_method=0, kwargs_needed=0,
                               substdict=None):
        '''This function is the guts of all functions that generate
        wrappers for functions, methods and constructors.'''
        if not substdict: substdict = {}
        
        info = argtypes.WrapperInfo()

        substdict.setdefault('errorreturn', 'NULL')

        # for methods, we want the leading comma
        if is_method:
            info.arglist.append('')
## 	    info.varlist.add('NspObject *', 'ob' )
##             info.add_parselist('obj', ['&ob'], ['ob'])

        if function_obj.varargs:
            raise ValueError, "varargs functions not supported"
        pos = 0

        for ptype, pname, pdflt, pnull, psize in function_obj.params:
            if pdflt and '|' not in info.parsestr:
                info.add_parselist('|', [], [])
            handler = argtypes.matcher.get(ptype)
            pos = pos+1
            handler.write_param('',ptype, pname, pdflt, pnull, psize, info, pos,self.byref)

        substdict['setreturn'] = ''
        if handle_return:
            if function_obj.ret not in ('none', None):
                substdict['setreturn'] = 'ret = '
            handler = argtypes.matcher.get(function_obj.ret)
            handler.write_return(function_obj.ret,
                                 function_obj.caller_owns_return, info)
            
        if function_obj.deprecated != None:
            deprecated = self.deprecated_tmpl % {
                'deprecationmsg': function_obj.deprecated,
                'errorreturn': substdict['errorreturn'] }
        else:
            deprecated = ''


        # if name isn't set, set it to function_obj.name
        substdict.setdefault('name', function_obj.name)

        if self.objinfo:
            substdict['typename'] = self.objinfo.c_name
        substdict['cname'] = function_obj.c_name
        cname1 = function_obj.c_name.replace("C2F(", "",1)
        cname1 = cname1.replace(")", "",1)
        substdict['cname_1'] = cname1
        substdict['varlist'] = info.get_varlist()
        substdict['typecodes'] = info.parsestr
        substdict['parselist'] = info.get_parselist()
        substdict['arglist'] = info.get_arglist()
        substdict['codebefore'] = deprecated + \
            string.replace(info.get_codebefore(),
            'return NULL', 'return ' + substdict['errorreturn'])
        substdict['codeafter'] = string.replace(info.get_codeafter(),
            'return NULL', 'return ' + substdict['errorreturn'])

        if info.parsestr or kwargs_needed:
            if info.get_parselist() == '': 
                substdict['parseargs'] = '  CheckRhs(0,0);\n'
            else:
                substdict['parseargs'] = self.parse_tmpl % substdict
            substdict['extraparams'] = ', NspObject *args, NspObject *kwargs'
            flags = 'METH_VARARGS|METH_KEYWORDS'

            # prepend the keyword list to the variable list
            substdict['varlist'] = info.get_tylist() + info.get_kwlist() + substdict['varlist']
        else:
            substdict['varlist'] = info.get_tylist() + substdict['varlist']
            substdict['parseargs'] = ''
            substdict['extraparams'] = ''
            flags = 'METH_NOARGS'
            
        return  template % substdict, flags

    def write_constructor(self):
        initfunc = '0'
        self.constructor_func = ''
        constructor = self.parser.find_constructor(self.objinfo,self.overrides)
        if constructor:
            try:
                if self.overrides.is_overriden(constructor.c_name):
                    lineno, filename = self.overrides.getstartline(
                        constructor.c_name)
                    self.fp.setline(lineno, 'codegen/'+ filename)
                    self.fp.write(self.overrides.override(constructor.c_name))
                    self.fp.resetline()
                    self.fp.write('\n\n')
                else:
                    # write constructor from template ...
                    code = self.write_function_wrapper(constructor,
                        self.constructor_tmpl,
                        handle_return=0, is_method=0, kwargs_needed=1,
                        substdict=self.get_initial_constructor_substdict())[0]
                    self.fp.write(code)
                initfunc = '_wrap_' + constructor.c_name
                self.constructor_func = string.lower(self.objinfo.c_name) + '_new'
            except:
                sys.stderr.write('Could not write constructor for %s: %s\n' 
                                 % (self.objinfo.c_name, exc_info()))
                # this is a hack ...
                if not hasattr(self.overrides, 'no_constructor_written'):
                    self.fp.write(self.noconstructor)
                    self.overrides.no_constructor_written = 1
                initfunc = 'nspgobject_no_constructor'
                self.constructor_func = ''
        else:
            # this is a hack ...
            if not hasattr(self.overrides, 'no_constructor_written'):
                self.fp.write(self.noconstructor)
                self.overrides.no_constructor_written = 1
            initfunc = 'nspgobject_no_constructor'
            self.constructor_func = ''
        return initfunc

    def write_methods(self):
        methods = []
        for meth in self.parser.find_methods(self.objinfo):
            # take care that method names refer to the c_name 
            if self.overrides.is_ignored(meth.c_name):
                continue

            meth_name1 = self.objinfo.name + '.' + meth.c_name
            #self.fp.write('XXX dans write_methods pour %s\n' % meth_name1)

            try:
                methflags = 'METH_VARARGS'
                if self.overrides.is_overriden(meth.c_name):
                    # overriden by its name 
                    if not self.overrides.is_already_included(meth.c_name):
                        lineno, filename = self.overrides.getstartline(meth.c_name)
                        self.fp.setline(lineno,'codegen/'+ filename)
                        self.fp.write(self.overrides.override(meth.c_name))
                        self.fp.resetline()
                        self.fp.write('\n\n')
                    if self.overrides.wants_kwargs(meth.c_name):
                        methflags = methflags + '|METH_KEYWORDS'
                    elif self.overrides.wants_noargs(meth.c_name):
                        methflags = 'METH_NOARGS'
                elif self.overrides.is_overriden(meth_name1):
                    # overriden by its name given as class.name
                    if not self.overrides.is_already_included(meth_name1):
                        lineno, filename = self.overrides.getstartline(meth_name1)
                        self.fp.setline(lineno,'codegen/'+ filename)
                        self.fp.write(self.overrides.override(meth_name1))
                        self.fp.resetline()
                        self.fp.write('\n\n')
                    if self.overrides.wants_kwargs(meth_name1):
                        methflags = methflags + '|METH_KEYWORDS'
                    elif self.overrides.wants_noargs(meth_name1):
                        methflags = 'METH_NOARGS'
                else:
                    # write constructor from template ...
                    code, methflags = self.write_function_wrapper(meth,
                        self.method_tmpl, handle_return=1, is_method=1,
                        substdict=self.get_initial_method_substdict(meth))
                    self.fp.write(code)
                
                methods.append(self.methdef_tmpl %
                               { 'name':  fixname(meth.name),
                                 'cname': '_wrap_' + meth.c_name,
                                 'flags': methflags})
            
            except:
                sys.stderr.write('Could not write method %s.%s: %s\n'
                                % (self.objinfo.c_name, meth.name, exc_info()))

        lower_name = string.lower(self.objinfo.name)
        if methods:
            methoddefs = '%s_methods' % lower_name
            # write the PyMethodDef structure
            methods.append('  { NULL, NULL}\n')
            self.fp.write('static NspMethods %s[] = {\n' % methoddefs)
            self.fp.write(string.join(methods, ''))
            self.fp.write('};\n\n')
            self.fp.write('static NspMethods *%s_get_methods(void) { return %s;};\n' % (lower_name, methoddefs))
        else:
            methoddefs = '%s_methods' % self.objinfo.c_name
            self.fp.write('static NspMethods *%s_get_methods(void) { return NULL;};\n' % lower_name)
            methoddefs = 'NULL'
        return methoddefs
    
    def write_getsets(self):
        lower_name = self.get_lower_name()
        getsets_name = lower_name + '_attrs'
        getterprefix = '_wrap_' + lower_name + '_get_'
        setterprefix = '_wrap_' + lower_name + '_set_'

        type_tmpl_2 = \
                    '/*-------------------------------------------\n'  \
                    ' * Attributes\n'  \
                    ' *-------------------------------------------*/\n\n'  \

        self.fp.write(type_tmpl_2)

        # no overrides for the whole function.  If no fields, don't write a func
        if not self.objinfo.fields:
            lower_name1 = string.lower(self.objinfo.name)
            self.fp.write('static AttrTab %s_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;\n' % lower_name1)
            # self.fp.write('static AttrTab *%s_get_attrs_table(void) { return NULL;};\n' % lower_name1)
            return '0'
        getsets = []
        for ftype, fname, opt , pdef, psize, pcheck in self.objinfo.fields:
            gettername = 'int_get_failed'
            settername = 'int_set_failed'
            getterobjectname = 'int_get_object_failed'
            setterobjectname = 'int_set_object_failed'
            attrname = self.objinfo.name + '.' + fname
            #self.fp.write('override attribute ? %s\n' % attrname)
            if self.overrides.attr_is_overriden(attrname):
                #self.fp.write('yes for %s\n' % attrname)
                lineno, filename = self.overrides.getstartline(attrname)
                code = self.overrides.attr_override(attrname)
                self.fp.setline(lineno, 'codegen/'+ filename)
                self.fp.write(code)
                self.fp.resetline()
                if string.find(code, getterprefix + fname) >= 0:
                    gettername = getterprefix + fname
                if string.find(code, setterprefix + fname) >= 0:
                    settername = setterprefix + fname
                if string.find(code, getterprefix + 'obj_' + fname) >= 0:
                    getterobjectname = getterprefix + 'obj_' + fname
                if string.find(code, setterprefix + 'obj_' + fname) >= 0:
                    setterobjectname = setterprefix + 'obj_' + fname

            if opt != 'hidden' and gettername == 'int_get_failed':
                try:
                    funcname = getterprefix + fname
                    info = argtypes.WrapperInfo()
                    handler = argtypes.matcher.get(ftype)
                    # we need the object type in write_return 
                    # and the attribute name and byref flag 
                    handler.c_name = self.objinfo.c_name;
                    handler.fname = fname;
                    handler.byref = self.byref;
                    # for attributes, we don't own the "return value"
                    handler.attr_write_return(ftype, 0, info,  pdef, psize, pcheck )
                    # for attributes get we return an NspObject
                    # info.varlist.add('NspObject*', 'nsp_ret')
                    self.fp.write(self.getter_tmpl %
                                  { 'funcname': funcname,
                                    'varlist': info.varlist,
                                    'field': self.get_field_accessor(fname,''),
                                    'attrcodeafter': info.get_attrcodeafter() })
                    gettername = funcname
                except:
                    sys.stderr.write("Could not write getter for %s.%s: %s\n"
                                     % (self.objinfo.name, fname, exc_info()))

            if opt != 'hidden' and getterobjectname == 'int_get_object_failed':
                try:
                    # check if we can use obj.val(xxx) = yyy in get operation 
                    if info.setobj == 't':
                        getterobjectname = getterprefix + 'obj_' + fname 
                        self.fp.write(self.getterobj_tmpl %
                                      { 'funcname': getterobjectname,
                                        'varlist': info.varlist,
                                        'field': self.get_field_accessor(fname,ftype),
                                        'attrcodeafter': info.get_attrcodeafter() })
                        
                except:
                    sys.stderr.write("Could not write getterobj for %s.%s: %s\n"
                                     % (self.objinfo.name, fname, exc_info()))

            if   opt != 'hidden' and settername == 'int_set_failed':
                try:
                    funcname = setterprefix + fname
                    info = argtypes.WrapperInfo()
                    handler = argtypes.matcher.get(ftype)
                    # handler.write_param(upinfo,ptype, pname, pdflt, pnull, psize, info, pos)
                    handler.attr_write_set(self.objinfo.c_name,ftype,fname ,'','',psize,info, 4,self.byref)
                    # a mettre a la fin '  %(field)s = %(fname)s;\n' 
                    # ftype unused in set operation 
                    self.fp.write(self.setter_tmpl %
                                  { 'funcname': funcname,
                                    'varlist': info.varlist,
                                    'field': self.get_field_accessor(fname,''),
                                    'fname': fname,
                                    'attr_set_code': info.get_attrcodebefore() })
                    settername = funcname
                except:
                    sys.stderr.write("Could not write setter for %s.%s: %s\n"
                                     % (self.objinfo.name, fname, exc_info()))
            if opt != 'hidden' and (gettername != 'int_get_failed' or settername != 'int_set_failed'):
                getsets.append('  { "%s", (attr_get_function *)%s, (attr_set_function *)%s,(attr_get_object_function *)%s, (attr_set_object_function *)%s },\n' %
                               (fixname(fname), gettername, settername,getterobjectname, setterobjectname))

        lower_name1 = string.lower(self.objinfo.name)
        if not getsets:
            # self.fp.write('static AttrTab *%s_get_attrs_table(void) { return NULL;};\n' % lower_name1)
            self.fp.write('static AttrTab %s_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;\n' % lower_name1)
            return '0'
        else: 
            self.fp.write('static AttrTab %s_attrs[] = {\n' % lower_name1)
            for getset in getsets:
                self.fp.write(getset)
            self.fp.write('  { NULL,NULL,NULL,NULL,NULL },\n')
            self.fp.write('};\n\n')
            lower_name1 = string.lower(self.objinfo.name)
            #self.fp.write('static AttrTab *%s_get_attrs_table(void) { return %s;};\n' % (lower_name1, getsets_name))
            return getsets_name
        
    def write_functions(self, prefix,functions ):
        type_tmpl_2 = \
                    '/*-------------------------------------------\n'  \
                    ' * functions \n'  \
                    ' *-------------------------------------------*/\n'  \

        self.fp.write(type_tmpl_2)
        
        # functions = []
        for func in self.parser.find_functions():
            # sys.stderr.write('XXXCode for function %s (C: %s)\n' % (func.name, func.c_name))
            if self.overrides.is_ignored(func.c_name):
                continue
            try:
                methflags = 'METH_VARARGS'
                if self.overrides.is_overriden(func.c_name):
                    lineno, filename = self.overrides.getstartline(func.c_name)
                    self.fp.setline(lineno, 'codegen/'+ filename)
                    self.fp.write(self.overrides.override(func.c_name))
                    self.fp.resetline()
                    self.fp.write('\n\n')
                    if self.overrides.wants_kwargs(func.c_name):
                        methflags = methflags + '|METH_KEYWORDS'
                    elif self.overrides.wants_noargs(func.c_name):
                        methflags = 'METH_NOARGS'
                else:
                    # write constructor from template ...
                    # sys.stderr.write('XXXCode for function %s (C: %s)\n' % (func.name, func.c_name))
                    code, methflags = self.write_function_wrapper(func,
                        self.function_tmpl, handle_return=1, is_method=0)
                    self.fp.write(code)
                cname1 = func.c_name.replace("C2F(", "",1)
                cname1 = cname1.replace(")", "",1)
                functions.append(self.funcdef_tmpl %
                                 { 'name':  func.name, # XXX use c_name and not name 
                                   'cname': '_wrap_' + cname1, 
                                   'flags': methflags })
            except:
                sys.stderr.write('Could not write function %s: %s\n'
                                 % (func.name, exc_info()))

        substdict = self.get_initial_class_substdict()

        if self.objinfo:
            substdict['typename'] = self.objinfo.c_name
        else:
            substdict['typename'] = prefix
        substdict['typename_dc'] = string.lower(prefix)
        substdict['typename_uc'] = 'ZZ'+string.upper(prefix)

        functions.append('  { "%(typename_dc)s_create", int_%(typename_dc)s_create},\n' % substdict)
        functions.append('  { NULL, NULL}\n')


        type_tmpl_3 = \
              '/*----------------------------------------------------\n'  \
              ' * Interface \n'  \
              ' * i.e a set of function which are accessible at nsp level\n'  \
              ' *----------------------------------------------------*/\n'  \
              '\n'  \
              'static OpTab %(typename)s_func[]={\n'  \

        self.fp.write(type_tmpl_3 % substdict)
        self.fp.write(string.join(functions, ''))
                
        type_tmpl_4 = \
              '};\n'  \
              '\n'  \
              '/* call ith function in the %(typename)s interface */\n'  \
              '\n'  \
              'int %(typename)s_Interf(int i, Stack stack, int rhs, int opt, int lhs)\n'  \
              '{\n'  \
              '  return (*(%(typename)s_func[i].fonc))(stack,rhs,opt,lhs);\n'  \
              '}\n'  \
              '\n'  \
              '/* used to walk through the interface table \n'  \
              '    (for adding or removing functions) */\n'  \
              '\n'  \
              'void %(typename)s_Interf_Info(int i, char **fname, function (**f))\n'  \
              '{\n'  \
              '  *fname = %(typename)s_func[i].name;\n'  \
              '  *f = %(typename)s_func[i].fonc;\n'  \
              '}\n'

        self.fp.write(type_tmpl_4 % substdict)

class NspObjectWrapper(Wrapper):

    constructor_tmpl = \
        'static int\n' \
        '_wrap_%(typename_dc)s_new(Stack stack, int rhs, int opt, int lhs)\n' \
        '{\n' \
        '%(varlist)s' \
        '  GObject *ret; NspObject *nsp_ret;\n' \
        '%(parseargs)s' \
        '%(codebefore)s' \
        '  if ((ret = (GObject *)%(cname)s(%(arglist)s))== NULL) return RET_BUG;\n' \
        '%(codeafter)s\n' \
        '  nsp_type_%(typename_dc)s = new_type_%(typename_dc)s(T_BASE);\n' \
        '%(aftercreate)s' \
        '  nsp_ret = (NspObject *) ZZZgobject_create(NVOID,ret,(NspTypeBase *) nsp_type_%(typename_dc)s );\n' \
        '  if ( nsp_ret == NULL) return RET_BUG;\n' \
        '  MoveObj(stack,1,nsp_ret);\n' \
        '  return 1;\n' \
        '}\n\n'
    
    method_tmpl = \
        'static int _wrap_%(cname)s(%(typename)s *self,Stack stack,int rhs,int opt,int lhs)\n'\
        '{\n' \
        '%(varlist)s' \
        '%(parseargs)s' \
        '%(codebefore)s' \
        '  %(setreturn)s%(cname)s(self%(arglist)s);\n' \
        '%(codeafter)s\n' \
        '}\n\n'

    type_tmpl_create_header = \
        '/*-----------------------------------------------------\n' \
        ' * constructor \n' \
        ' * if type is non NULL it is a subtype which can be used to \n' \
        ' * create a %(typename)s instance \n' \
        ' *-----------------------------------------------------*/\n' 

    type_tmpl_create_0 = \
        '\n' \
        'static %(typename)s *nsp_%(typename_dc)s_create_void(char *name,NspTypeBase *type)\n' \
        '{\n' \
        ' %(typename)s *H  = (type == NULL) ? new_%(typename_dc)s() : type->new();\n' \
        ' if ( H ==  NULL%(typename_uc)s)\n' \
        '  {\n' \
        '   Sciprintf("No more memory\\n");\n' \
        '   return NULL%(typename_uc)s;\n' \
        '  }\n' \
        ' if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULL%(typename_uc)s;\n' \
        ' NSP_OBJECT(H)->ret_pos = -1 ;\n' \
        ' return H;\n' \
        '}\n\n' \
        'int nsp_%(typename_dc)s_create_partial(%(typename)s *H)\n' \
        '{\n' \
        '%(create_partial)s' \
        '  return OK;\n' \
        '}\n\n' \
        'int nsp_%(typename_dc)s_check_values(%(typename)s *H)\n' \
        '{\n' \
        '%(fields_defval)s' \
        '  return OK;\n' \
        '}\n' \
        '\n' \
        '%(typename)s *nsp_%(typename_dc)s_create(char *name,%(fields_list)s,NspTypeBase *type)\n' \
        '{\n' \
        '  %(typename)s *H  = nsp_%(typename_dc)s_create_void(name,type);\n' \
        '  if ( H ==  NULL%(typename_uc)s) return NULL%(typename_uc)s;\n' \
        '%(fields_copy)s' \
        '  if ( nsp_%(typename_dc)s_check_values(H) == FAIL) return NULL%(typename_uc)s;\n' 

    type_tmpl_create_1 = \
        '  return H;\n' \
        '}\n' \
        '\n' \
        '\n' \
        '%(typename)s *nsp_%(typename_dc)s_create_default(char *name)\n' \
        '{\n' \
        ' %(typename)s *H  = nsp_%(typename_dc)s_create_void(name,NULL);\n' \
        ' if ( H ==  NULL%(typename_uc)s) return NULL%(typename_uc)s;\n' \
        '%(fields_copy_default)s' \
        ' if ( nsp_%(typename_dc)s_check_values(H) == FAIL) return NULL%(typename_uc)s;\n' \
        ' return H;\n' \
        '}\n' \
        '\n' 

    type_tmpl_copy_1 = \
        '/*\n'  \
        ' * copy for gobject derived class  \n'  \
        ' */\n'  \
        '\n'  \
        '%(typename)s *nsp_%(typename_dc)s_copy_partial(%(typename)s *H,%(typename)s *self)\n'  \
        '{\n'  \
        '%(fields_copy_self)s' \
        '  return H;\n' \
        '}\n\n'  \
        '%(typename)s *nsp_%(typename_dc)s_copy(%(typename)s *self)\n'  \
        '{\n'  \
        '  %(typename)s *H  =nsp_%(typename_dc)s_create_void(NVOID,(NspTypeBase *) nsp_type_%(typename_dc)s);\n'  \
        '  if ( H ==  NULL%(typename_uc)s) return NULL%(typename_uc)s;\n' \
        '%(copy_partial)s\n' \
        '  return H;\n' \
        '}\n'  \
        '/*\n'  \
        ' * full copy for gobject derived class\n'  \
        ' */\n'  \
        '\n'  \
        '%(fields_full_copy_partial_code)s' \
        '%(full_copy_code)s' 

    type_tmpl_copy_2 = \
        '  return H;\n' + '}\n' \
        '\n'  \
        '/*-------------------------------------------------------------------\n'  \
        ' * wrappers for the %(typename)s\n'  \
        ' * i.e functions at Nsp level \n'  \
        ' *-------------------------------------------------------------------*/\n'  \
        '\n'  

    type_tmpl_intcreate = \
        'int int_%(typename_dc)s_create(Stack stack, int rhs, int opt, int lhs)\n'  \
        '{\n'  \
        '  %(typename)s *H;\n'  \
        '  CheckStdRhs(0,0);\n'  \
        '  /* want to be sure that type %(typename_dc)s is initialized */\n'  \
        '  nsp_type_%(typename_dc)s = new_type_%(typename_dc)s(T_BASE);\n'  \
        '  if(( H = nsp_%(typename_dc)s_create_void(NVOID,(NspTypeBase *) nsp_type_%(typename_dc)s)) == NULL%(typename_uc)s) return RET_BUG;\n' \
        '  /* then we use optional arguments to fill attributes */\n' \
        '%(fields_from_attributes)s' \
        ' if ( nsp_%(typename_dc)s_check_values(H) == FAIL) return RET_BUG;\n' 

    type_tmpl_intcreate_last = \
        '  MoveObj(stack,1,(NspObject  *) H);\n'  \
        '  return 1;\n'  \
        '} \n'  \
        '\n'


    def __init__(self, parser, objinfo, overrides, fp=FileOutput(sys.stdout),byref=None):
        Wrapper.__init__(self, parser, objinfo, overrides, fp,byref)
        if self.objinfo:
            self.castmacro = string.replace(self.objinfo.typecode, '_TYPE_', '_', 1)

    def get_initial_class_substdict(self):
        return {'interface_1' : 'check_cast',
                'interface_2' : 'nsp_object_type',
                'tp_basicsize'      : 'NspGObject',
                'tp_weaklistoffset' : 'offsetof(PyGObject, weakreflist)',
                'tp_dictoffset'     : 'offsetof(PyGObject, inst_dict)' }
    
    def get_field_accessor(self, fieldname, fieldtype):
        # castmacro = 'NSP_' + self.objinfo.typecode
        # castmacro = self.objinfo.typecode
        castmacro = self.objinfo.c_name
        if self.byref == 't' :
            fieldname =  'obj->' +             fieldname
            
        if fieldtype:
            return '((%s) ((%s *) self)->%s)' % (fieldtype, castmacro, fieldname)
        else:
            return '((%s *) self)->%s' % (castmacro, fieldname)
                
    def get_initial_constructor_substdict(self):
        substdict = Wrapper.get_initial_constructor_substdict(self)
        typename = self.objinfo.c_name 
        substdict['typename'] =  typename
        substdict['typename_dc'] = string.lower(typename)
        if argtypes.matcher.object_is_a(self.objinfo.c_name, 'GtkWindow'):
            substdict['aftercreate'] = "    /* g_xxxx_object_ref(ret); XXXwe don't own the first reference of windows */\n"
        elif argtypes.matcher.object_is_a(self.objinfo.c_name, 'GtkInvisible'):
            substdict['aftercreate'] = "    /* g_xxxx_object_ref(ret); XXXwe don't own the first reference of invisibles */\n"
        else:
            substdict['aftercreate'] = ''
        return substdict

    # used to cast objects 

    def get_initial_method_substdict(self, method):
        substdict = Wrapper.get_initial_method_substdict(self, method)
        substdict['cast'] = string.replace(self.objinfo.typecode, '_TYPE_', '_', 1)
        return substdict


class NspInterfaceWrapper(NspObjectWrapper):
    
    def get_initial_class_substdict(self):
        return { 'interface_1' : 'check_implements',
                 'interface_2' : 'nsp_object_implements',
                 'tp_basicsize'      : 'PyGObject',
                 'tp_weaklistoffset' : '0',
                 'tp_dictoffset'     : '0'}

    def write_constructor(self):
        # interfaces have no constructors ...
        self.constructor_func = ''
        return '0'
    def write_getsets(self):
        # interfaces have no fields ...
        lower_name1 = string.lower(self.objinfo.c_name)
        self.fp.write('static AttrTab %s_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;\n' % lower_name1)
        return '0'


def write_enums(parser, prefix, fp=sys.stdout):
    if not parser.enums:
        return
    fp.write('\n/* ----------- enums and flags ----------- */\n\n')
    fp.write('void\n' + prefix + '_add_constants(NspObject *module, const gchar *strip_prefix)\n{\n')
    for enum in parser.enums:
        if enum.typecode is None:
            for nick, value in enum.values:
                fp.write('/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("%s", strip_prefix), %s);*/\n'
                         % (value, value))
        else:
            if enum.deftype == 'enum':
                fp.write('  nsp_enum_add_constants((NspHash *) module, %s, strip_prefix);\n'
                         % (enum.typecode,))
            else:
                fp.write('  nsp_flags_add_constants((NspHash *)module, %s, strip_prefix);\n'
                         % (enum.typecode,))
    fp.write('}\n\n')

def write_source(parser, overrides, prefix, fp=FileOutput(sys.stdout)):
    fp.write('/* -*- Mode: C -*- */\n\n')
    fp.write('/* This file is generated, please do not edit */\n')

    fp.write('/* Nsp\n') 
    tag = overrides.get_copyright()
    if tag == '' :
        tag = ' * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics\n'
        fp.write( tag) 
    else:
        fp.write( tag) 
        fp.resetline()

    type_tmpl_copyright = \
        ' *\n' \
        ' * This library is free software; you can redistribute it and/or\n' \
        ' * modify it under the terms of the GNU General Public\n' \
        ' * License as published by the Free Software Foundation; either\n' \
        ' * version 2 of the License, or (at your option) any later version.\n' \
        ' *\n' \
        ' * This library is distributed in the hope that it will be useful,\n' \
        ' * but WITHOUT ANY WARRANTY; without even the implied warranty of\n' \
        ' * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n' \
        ' * General Public License for more details.\n' \
        ' *\n' \
        ' * You should have received a copy of the GNU General Public\n' \
        ' * License along with this library; if not, write to the\n' \
        ' * Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n' \
        ' * Boston, MA 02111-1307, USA.\n' \
        ' */\n' 
    fp.write( type_tmpl_copyright) 
    fp.write('\n\n')
    #fp.write('#include <nsp/object.h>\n')
    #fp.write('#include <gtk/gtk.h>\n')
    for module, pyname, cname in overrides.get_imports():
        fp.write('#include "%s.h"\n' % string.lower(cname))
    fp.write('\n\n')
    fp.write(overrides.get_headers())
    fp.resetline()
    #     fp.write('/* ---------- forward type declarations ---------- */\n')
    #     for obj in parser.boxes:
    #         fp.write('#define %s_Private\n#include "nsp/%s.h"\n' % (obj.c_name, string.lower(obj.c_name)))
    #     for obj in parser.objects:
    #         fp.write('#define %s_Private\n#include "nsp/%s.h"\n' % (obj.c_name, string.lower(obj.c_name)))
    #     for interface in parser.interfaces:
    #         fp.write('#define %s_Private\n#include "nsp/%s.h"\n' % (obj.c_name, string.lower(obj.c_name)))
    #     fp.write('\n')

    # used to collect constructors 
    functions =[]
    
    for interface in parser.interfaces:
        wrapper = NspInterfaceWrapper(parser, interface, overrides, fp)
        wrapper.write_class()
        fp.write('\n')
        
    for obj in parser.objects:
        if obj.byref == 't':
            wrapper = NspObjectWrapper(parser, obj, overrides, fp,obj.byref)
        else:
            wrapper = NspObjectWrapper(parser, obj, overrides, fp,obj.byref)
        wrapper.write_class()
        constructor = wrapper.constructor_func
        if constructor != '':
            functions.append(wrapper.funcdef_tmpl %
                             { 'name':  constructor,
                               'cname': '_wrap_' + constructor })
        fp.write('\n')

    # structs 
    # voir si on veut generer du code pour cela XXX

#     for obj in parser.structs:
#         wrapper = NspObjectWrapper(parser, obj, overrides, fp, 'f')
#         wrapper.write_struct()
#         constructor = wrapper.constructor_func
#         if constructor != '':
#             functions.append(wrapper.funcdef_tmpl %
#                              { 'name':  constructor,
#                                'cname': '_wrap_' + constructor })
#         fp.write('\n')

    # functions 
    # 
    wrapper = Wrapper(parser, None, overrides, fp)
    wrapper.write_functions(prefix,functions)

    # enums 
    write_enums(parser, prefix, fp)

#     fp.write('/* intialise stuff extension classes */\n')
#     fp.write('/* void\n' + prefix + '_register_classes(NspObject *d)\n{\n')
#     imports = overrides.get_imports()[:]
#     if imports:
#         bymod = {}
#         for module, pyname, cname in imports:
#             bymod.setdefault(module, []).append((pyname, cname))
#         fp.write('  NspObject *module;\n\n')
#         for module in bymod:
#             fp.write('  if ((module = PyImport_ImportModule("%s")) != NULL) {\n' % module)
#             fp.write('      NspObject *moddict = PyModule_GetDict(module);\n\n')
#             for pyname, cname in bymod[module]:
#                 fp.write('      _%s = (PyTypeObject *)PyDict_GetItemString(moddict, "%s");\n' % (cname, pyname))
#             fp.write('  } else {\n')
#             fp.write('      Py_FatalError("could not import %s");\n' %module)
#             fp.write('      return;\n')
#             fp.write('  }\n')
#         fp.write('\n')
#     fp.write(overrides.get_init() + '\n')
#     fp.resetline()

#     for boxed in parser.boxes:
#         fp.write('  nspg_register_boxed(d, "' + boxed.name +
#                  '", ' + boxed.typecode + ', &Nsp' + boxed.c_name + '_Type);\n')
#     for pointer in parser.pointers:
#         fp.write('  nspg_register_pointer(d, "' + pointer.name +
#                  '", ' + pointer.typecode + ', &Nsp' + pointer.c_name + '_Type);\n')
#     for interface in parser.interfaces:
#         fp.write('  nspg_register_interface(d, "' + interface.name +
#                  '", '+ interface.typecode + ', &Nsp' + interface.c_name +
#                  '_Type);\n')
#     objects = parser.objects[:]
#     pos = 0
#     while pos < len(objects):
#         parent = objects[pos].parent
#         for i in range(pos+1, len(objects)):
#             if objects[i].c_name == parent:
#                 objects.insert(i+1, objects[pos])
#                 del objects[pos]
#                 break
#         else:
#             pos = pos + 1
#     for obj in objects:
#         bases = []
#         if obj.parent != None:
#             bases.append(obj.parent)
#         bases = bases + obj.implements
#         if bases:
#             fp.write('  nspgobject_register_class(d, "' + obj.c_name +
#                      '", ' + obj.typecode + ', &Nsp' + obj.c_name +
#                      '_Type, Nsp_BuildValue("(' + 'O' * len(bases) + ')", ' +
#                      string.join(map(lambda s: '&Nsp'+s+'_Type', bases), ', ') +
#                      '));\n')
#         else:
#             fp.write('  nspgobject_register_class(d, "' + obj.c_name +
#                      '", ' + obj.typecode + ', &Nsp' + obj.c_name +
#                      '_Type, NULL);\n')
#     fp.write('}\n*/\n')

    # code added verbatim at the end 
    fp.write(overrides.get_last() + '\n')
    fp.resetline()


def register_types(parser):
    for obj in parser.interfaces:
        argtypes.matcher.register_object(obj.c_name,obj.name, None, obj.typecode)
    for boxed in parser.boxes:
        argtypes.matcher.register_boxed(boxed.c_name, boxed.typecode)
    for pointer in parser.pointers:
        argtypes.matcher.register_pointer(pointer.c_name, pointer.typecode)
    for struct in parser.structs:
        argtypes.matcher.register_struct(struct.c_name, struct.typecode)
    for obj in parser.objects:
        argtypes.matcher.register_object(obj.c_name,obj.name, obj.parent, obj.typecode)
    for enum in parser.enums:
	if enum.deftype == 'flags':
	    argtypes.matcher.register_flag(enum.c_name, enum.typecode)
	else:
	    argtypes.matcher.register_enum(enum.c_name, enum.typecode)

def main():
    o = override.Overrides()
    prefix = 'nspgtk'
    outfilename = None
    errorfilename = None
    opts, args = getopt.getopt(sys.argv[1:], "o:p:r:t:",
                        ["override=", "prefix=", "register=", "outfilename=",
                         "load-types=", "errorfilename="])
    for opt, arg in opts:
        if opt in ('-o', '--override'):
            o = override.Overrides(arg)
        elif opt in ('-p', '--prefix'):
            prefix = arg
        elif opt in ('-r', '--register'):
            p = defsparser.DefsParser(arg)
            p.startParsing()
            register_types(p)
            del p
        elif opt == '--outfilename':
            outfilename = arg
        elif opt == '--errorfilename':
            errorfilename = arg
        elif opt in ('-t', '--load-types'):
            globals = {}
            execfile(arg, globals)
    if len(args) < 1:
        sys.stderr.write(
            'usage: codegen.py [-o overridesfile] [-p prefix] defsfile\n')
        sys.exit(1)
    if errorfilename:
        sys.stderr = open(errorfilename, "w")
    p = defsparser.DefsParser(args[0])
    if not outfilename:
        outfilename = os.path.splitext(args[0])[0] + '.c'
    p.startParsing()
    register_types(p)
    write_source(p, o, prefix, FileOutput(sys.stdout, outfilename))

if __name__ == '__main__':
    main()
