/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "classbref.override"

#line 14 "classbref.c"

/* ----------- ClassBRef ----------- */


#define  ClassBRef_Private 
#include "nsp/object.h"
#include "nsp/classbref.h"
#include "nsp/interf.h"

/* 
 * NspClassBRef inherits from NspClassARef 
 */

int nsp_type_classbref_id=0;
NspTypeClassBRef *nsp_type_classbref=NULL;

/*
 * Type object for ClassBRef 
 * all the instance of NspTypeClassBRef share the same id. 
 * nsp_type_classbref: is an instance of NspTypeClassBRef 
 *    used for objects of NspClassBRef type (i.e built with new_classbref) 
 * other instances are used for derived classes 
 */
NspTypeClassBRef *new_type_classbref(type_mode mode)
{
  NspTypeClassBRef *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_classbref != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_classbref;
    }
  if ((type =  malloc(sizeof(NspTypeClassBRef))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_classaref(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = classbref_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = classbref_get_methods; 
  type->new = (new_func *) new_classbref;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for classbref */ 

  top->pr = (print_func *) nsp_classbref_print;                  
  top->dealloc = (dealloc_func *) nsp_classbref_destroy;
  top->copy  =  (copy_func *) nsp_classbref_copy;                 
  top->size  = (size_func *) nsp_classbref_size;                
  top->s_type =  (s_type_func *) nsp_classbref_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_classbref_type_short_string;
  top->info = (info_func *) nsp_classbref_info ;                  
  /* top->is_true = (is_true_func  *) nsp_classbref_is_true; */
  /* top->loop =(loop_func *) nsp_classbref_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_classbref_object;
  top->eq  = (eq_func *) nsp_classbref_eq;
  top->neq  = (eq_func *) nsp_classbref_neq;
  top->save  = (save_func *) nsp_classbref_xdr_save;
  top->load  = (load_func *) nsp_classbref_xdr_load;
  top->create = (create_func*) int_classbref_create;
  top->latex = (print_func *) nsp_classbref_latex;
  
  /* specific methods for classbref */
      
  type->init = (init_func *) init_classbref;

  /* 
   * ClassBRef interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_classbref_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeClassBRef called nsp_type_classbref
       */
      type->id =  nsp_type_classbref_id = nsp_new_type_id();
      nsp_type_classbref = type;
      if ( nsp_register_type(nsp_type_classbref) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_classbref(mode);
    }
  else 
    {
       type->id = nsp_type_classbref_id;
       return type;
    }
}

/*
 * initialize ClassBRef instances 
 * locally and by calling initializer on parent class 
 */

static int init_classbref(NspClassBRef *Obj,NspTypeClassBRef *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of ClassBRef 
 */

NspClassBRef *new_classbref() 
{
  NspClassBRef *loc; 
  /* type must exists */
  nsp_type_classbref = new_type_classbref(T_BASE);
  if ( (loc = malloc(sizeof(NspClassBRef)))== NULLCLASSBREF) return loc;
  /* initialize object */
  if ( init_classbref(loc,nsp_type_classbref) == FAIL) return NULLCLASSBREF;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ClassBRef 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_classbref_size(NspClassBRef *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char classbref_type_name[]="ClassBRef";
static char classbref_short_type_name[]="classbref";

static char *nsp_classbref_type_as_string(void)
{
  return(classbref_type_name);
}

static char *nsp_classbref_type_short_string(NspObject *v)
{
  return(classbref_short_type_name);
}

/*
 * A == B 
 */

static int nsp_classbref_eq(NspClassBRef *A, NspObject *B)
{
  NspClassBRef *loc = (NspClassBRef *) B;
  if ( check_cast(B,nsp_type_classbref_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->clb_color != loc->obj->clb_color) return FALSE;
  if ( A->obj->clb_thickness != loc->obj->clb_thickness) return FALSE;
  if ( NSP_OBJECT(A->obj->clb_val)->type->eq(A->obj->clb_val,loc->obj->clb_val) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_classbref_neq(NspClassBRef *A, NspObject *B)
{
  return ( nsp_classbref_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_classbref_xdr_save(XDR *xdrs, NspClassBRef *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->clb_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->clb_thickness) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->clb_val)) == FAIL) return FAIL;
  if ( nsp_classaref_xdr_save(xdrs, (NspClassARef *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspClassBRef  *nsp_classbref_xdr_load_partial(XDR *xdrs, NspClassBRef *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = malloc(sizeof(nsp_classbref))) == NULL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->clb_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->clb_thickness) == FAIL) return NULL;
  if ((M->obj->clb_val =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_classaref_xdr_load_partial(xdrs,(NspClassARef *)M) == NULL) return NULL;
 return M;
}

static NspClassBRef  *nsp_classbref_xdr_load(XDR *xdrs)
{
  NspClassBRef *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCLASSBREF;
  if ((M  = nsp_classbref_create_void(name,(NspTypeBase *) nsp_type_classbref))== NULLCLASSBREF) return M;
  return nsp_classbref_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_classbref_destroy_partial(NspClassBRef *H)
{
  nsp_classaref_destroy_partial((NspClassARef *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_matrix_destroy(H->obj->clb_val);
    FREE(H->obj);
   }
}

void nsp_classbref_destroy(NspClassBRef *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_classbref_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_classbref_info(NspClassBRef *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCLASSBREF) 
    {
      Sciprintf("Null Pointer ClassBRef \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_classbref_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_classbref_print(NspClassBRef *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCLASSBREF) 
    {
      Sciprintf("Null Pointer ClassBRef \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_classbref_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classbref_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        Sciprintf1(indent+2,"clb_color=%d\n",M->obj->clb_color);
  Sciprintf1(indent+2,"clb_thickness=%d\n",M->obj->clb_thickness);
  if ( M->obj->clb_val != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->clb_val),indent+2,"clb_val",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_classaref_print((NspClassARef *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_classbref_latex(NspClassBRef *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classbref_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    Sciprintf1(indent+2,"clb_color=%d\n",M->obj->clb_color);
  Sciprintf1(indent+2,"clb_thickness=%d\n",M->obj->clb_thickness);
  if ( M->obj->clb_val != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->clb_val),indent+2,"clb_val",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_classaref_latex((NspClassARef *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassBRef objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspClassBRef   *nsp_classbref_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_classbref_id) == TRUE ) return ((NspClassBRef *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_classbref));
  return NULL;
}

int IsClassBRefObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_classbref_id);
}

int IsClassBRef(NspObject *O)
{
  return nsp_object_type(O,nsp_type_classbref_id);
}

NspClassBRef  *GetClassBRefCopy(Stack stack, int i)
{
  if (  GetClassBRef(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspClassBRef  *GetClassBRef(Stack stack, int i)
{
  NspClassBRef *M;
  if (( M = nsp_classbref_object(NthObj(i))) == NULLCLASSBREF)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspClassBRef *nsp_classbref_create_void(char *name,NspTypeBase *type)
{
 NspClassBRef *H  = (type == NULL) ? new_classbref() : type->new();
 if ( H ==  NULLCLASSBREF)
  {
   Sciprintf("No more memory\n");
   return NULLCLASSBREF;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCLASSBREF;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_classbref_create_partial(NspClassBRef *H)
{
  if ( nsp_classaref_create_partial((NspClassARef *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_classbref)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  return OK;
}

int nsp_classbref_check_values(NspClassBRef *H)
{
  if ( H->obj->clb_val == NULLMAT) 
    {
     if (( H->obj->clb_val = nsp_matrix_create("clb_val",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  nsp_classaref_check_values((NspClassARef *) H);
  return OK;
}

NspClassBRef *nsp_classbref_create(char *name,int clb_color,int clb_thickness,NspMatrix* clb_val,NspTypeBase *type)
{
 NspClassBRef *H  = nsp_classbref_create_void(name,type);
 if ( H ==  NULLCLASSBREF) return NULLCLASSBREF;
  if ( nsp_classbref_create_partial(H) == FAIL) return NULLCLASSBREF;
  H->obj->clb_color=clb_color;
  H->obj->clb_thickness=clb_thickness;
  if ( clb_val == NULL )
    { H->obj->clb_val = NULL;}
  else
    {
      if ((H->obj->clb_val = (NspMatrix *)  nsp_object_copy_and_name("clb_val",NSP_OBJECT(clb_val))) == NULLMAT) return NULL;
    }
 if ( nsp_classbref_check_values(H) == FAIL) return NULLCLASSBREF;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspClassBRef *nsp_classbref_copy_partial(NspClassBRef *H,NspClassBRef *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspClassBRef *nsp_classbref_copy(NspClassBRef *self)
{
  NspClassBRef *H  =nsp_classbref_create_void(NVOID,(NspTypeBase *) nsp_type_classbref);
  if ( H ==  NULLCLASSBREF) return NULLCLASSBREF;
  if ( nsp_classaref_copy_partial((NspClassARef *) H,(NspClassARef *) self ) == NULL) return NULLCLASSBREF;
  if ( nsp_classbref_copy_partial(H,self)== NULL) return NULLCLASSBREF;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the ClassBRef
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_classbref_create(Stack stack, int rhs, int opt, int lhs)
{
  NspClassBRef *H;
  CheckStdRhs(0,0);
  /* want to be sure that type classbref is initialized */
  nsp_type_classbref = new_type_classbref(T_BASE);
  if(( H = nsp_classbref_create_void(NVOID,(NspTypeBase *) nsp_type_classbref)) == NULLCLASSBREF) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_classbref_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_classbref_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

#line 19 "classbref.override"
static int _wrap_classb_color_change(NspClassBRef *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int color;
  if ( GetArgs(stack,rhs,opt,T,&color) == FAIL) return RET_BUG;
  self->obj->clb_color = color;
  return 0;
}
#line 476 "classbref.c"


#line 29 "classbref.override"
static int _wrap_classb_color_show(NspClassBRef *self,Stack stack,int rhs,int opt,int lhs)
{
  Sciprintf("color: %d\n",self->obj->clb_color);
  return 0;
}


#line 487 "classbref.c"


static NspMethods classbref_methods[] = {
  {"classb_color_change",(nsp_method *) _wrap_classb_color_change},
  {"classb_color_show",(nsp_method *) _wrap_classb_color_show},
  { NULL, NULL}
};

static NspMethods *classbref_get_methods(void) { return classbref_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_classbref_get_clb_color(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspClassBRef *) self)->obj->clb_color);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_classbref_set_clb_color(void *self, char *attr, NspObject *O)
{
  int clb_color;

  if ( IntScalar(O,&clb_color) == FAIL) return FAIL;
  ((NspClassBRef *) self)->obj->clb_color = clb_color;
  return OK;
}

static NspObject *_wrap_classbref_get_clb_thickness(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspClassBRef *) self)->obj->clb_thickness);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_classbref_set_clb_thickness(void *self, char *attr, NspObject *O)
{
  int clb_thickness;

  if ( IntScalar(O,&clb_thickness) == FAIL) return FAIL;
  ((NspClassBRef *) self)->obj->clb_thickness = clb_thickness;
  return OK;
}

static NspObject *_wrap_classbref_get_clb_val(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspClassBRef *) self)->obj->clb_val);
  return (NspObject *) ret;
}

static NspObject *_wrap_classbref_get_obj_clb_val(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspClassBRef *) self)->obj->clb_val);
  return (NspObject *) ret;
}

static int _wrap_classbref_set_clb_val(void *self, char *attr, NspObject *O)
{
  NspMatrix *clb_val;

  if ( ! IsMat(O) ) return FAIL;
  if ((clb_val = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspClassBRef *) self)->obj->clb_val != NULL ) 
    nsp_matrix_destroy(((NspClassBRef *) self)->obj->clb_val);
  ((NspClassBRef *) self)->obj->clb_val = clb_val;
  return OK;
}

static AttrTab classbref_attrs[] = {
  { "clb_color", (attr_get_function *)_wrap_classbref_get_clb_color, (attr_set_function *)_wrap_classbref_set_clb_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "clb_thickness", (attr_get_function *)_wrap_classbref_get_clb_thickness, (attr_set_function *)_wrap_classbref_set_clb_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "clb_val", (attr_get_function *)_wrap_classbref_get_clb_val, (attr_set_function *)_wrap_classbref_set_clb_val,(attr_get_object_function *)_wrap_classbref_get_obj_clb_val, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ClassBRef_func[]={
  { "classbref_create", int_classbref_create},
  { NULL, NULL}
};

/* call ith function in the ClassBRef interface */

int ClassBRef_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(ClassBRef_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void ClassBRef_Interf_Info(int i, char **fname, function (**f))
{
  *fname = ClassBRef_func[i].name;
  *f = ClassBRef_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
ClassBRef_register_classes(NspObject *d)
{

#line 7 "classbref.override"

/ * init * /


#line 610 "classbref.c"
  nspgobject_register_class(d, "ClassBRef", ClassBRef, &NspClassBRef_Type, Nsp_BuildValue("(O)", &NspClassARef_Type));
}
*/

#line 615 "classbref.c"
