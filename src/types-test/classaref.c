/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "classaref.override"

#line 14 "classaref.c"

/* ----------- ClassARef ----------- */


#define  ClassARef_Private 
#include "nsp/object.h"
#include "nsp/classaref.h"
#include "nsp/interf.h"

/* 
 * NspClassARef inherits from NspObject 
 */

int nsp_type_classaref_id=0;
NspTypeClassARef *nsp_type_classaref=NULL;

/*
 * Type object for ClassARef 
 * all the instance of NspTypeClassARef share the same id. 
 * nsp_type_classaref: is an instance of NspTypeClassARef 
 *    used for objects of NspClassARef type (i.e built with new_classaref) 
 * other instances are used for derived classes 
 */
NspTypeClassARef *new_type_classaref(type_mode mode)
{
  NspTypeClassARef *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_classaref != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_classaref;
    }
  if ((type =  malloc(sizeof(NspTypeClassARef))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = classaref_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = classaref_get_methods; 
  type->new = (new_func *) new_classaref;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for classaref */ 

  top->pr = (print_func *) nsp_classaref_print;                  
  top->dealloc = (dealloc_func *) nsp_classaref_destroy;
  top->copy  =  (copy_func *) nsp_classaref_copy;                 
  top->size  = (size_func *) nsp_classaref_size;                
  top->s_type =  (s_type_func *) nsp_classaref_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_classaref_type_short_string;
  top->info = (info_func *) nsp_classaref_info ;                  
  /* top->is_true = (is_true_func  *) nsp_classaref_is_true; */
  /* top->loop =(loop_func *) nsp_classaref_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_classaref_object;
  top->eq  = (eq_func *) nsp_classaref_eq;
  top->neq  = (eq_func *) nsp_classaref_neq;
  top->save  = (save_func *) nsp_classaref_xdr_save;
  top->load  = (load_func *) nsp_classaref_xdr_load;
  top->create = (create_func*) int_classaref_create;
  top->latex = (print_func *) nsp_classaref_latex;
  
  /* specific methods for classaref */
      
  type->init = (init_func *) init_classaref;

  /* 
   * ClassARef interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_classaref_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeClassARef called nsp_type_classaref
       */
      type->id =  nsp_type_classaref_id = nsp_new_type_id();
      nsp_type_classaref = type;
      if ( nsp_register_type(nsp_type_classaref) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_classaref(mode);
    }
  else 
    {
       type->id = nsp_type_classaref_id;
       return type;
    }
}

/*
 * initialize ClassARef instances 
 * locally and by calling initializer on parent class 
 */

static int init_classaref(NspClassARef *Obj,NspTypeClassARef *type)
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
 * new instance of ClassARef 
 */

NspClassARef *new_classaref() 
{
  NspClassARef *loc; 
  /* type must exists */
  nsp_type_classaref = new_type_classaref(T_BASE);
  if ( (loc = malloc(sizeof(NspClassARef)))== NULLCLASSAREF) return loc;
  /* initialize object */
  if ( init_classaref(loc,nsp_type_classaref) == FAIL) return NULLCLASSAREF;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ClassARef 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_classaref_size(NspClassARef *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char classaref_type_name[]="ClassARef";
static char classaref_short_type_name[]="classaref";

static char *nsp_classaref_type_as_string(void)
{
  return(classaref_type_name);
}

static char *nsp_classaref_type_short_string(NspObject *v)
{
  return(classaref_short_type_name);
}

/*
 * A == B 
 */

static int nsp_classaref_eq(NspClassARef *A, NspObject *B)
{
  NspClassARef *loc = (NspClassARef *) B;
  if ( check_cast(B,nsp_type_classaref_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->cla_color != loc->obj->cla_color) return FALSE;
  if ( A->obj->cla_thickness != loc->obj->cla_thickness) return FALSE;
  if ( NSP_OBJECT(A->obj->cla_val)->type->eq(A->obj->cla_val,loc->obj->cla_val) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_classaref_neq(NspClassARef *A, NspObject *B)
{
  return ( nsp_classaref_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_classaref_xdr_save(XDR *xdrs, NspClassARef *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->cla_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->cla_thickness) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->cla_val)) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspClassARef  *nsp_classaref_xdr_load_partial(XDR *xdrs, NspClassARef *M)
{
  if ((M->obj = malloc(sizeof(nsp_classaref))) == NULL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->cla_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->cla_thickness) == FAIL) return NULL;
  if ((M->obj->cla_val =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
 return M;
}

static NspClassARef  *nsp_classaref_xdr_load(XDR *xdrs)
{
  NspClassARef *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCLASSAREF;
  if ((M  = nsp_classaref_create_void(name,(NspTypeBase *) nsp_type_classaref))== NULLCLASSAREF) return M;
  return nsp_classaref_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_classaref_destroy_partial(NspClassARef *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_matrix_destroy(H->obj->cla_val);
    FREE(H->obj);
   }
}

void nsp_classaref_destroy(NspClassARef *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_classaref_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

void nsp_classaref_info(NspClassARef *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCLASSAREF) 
    {
      Sciprintf("Null Pointer ClassARef \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_classaref_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_classaref_print(NspClassARef *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCLASSAREF) 
    {
      Sciprintf("Null Pointer ClassARef \n");
      return;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_classaref_info(M,indent,pname,rec_level);
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classaref_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        Sciprintf1(indent+2,"cla_color=%d\n",M->obj->cla_color);
  Sciprintf1(indent+2,"cla_thickness=%d\n",M->obj->cla_thickness);
  if ( M->obj->cla_val != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->cla_val),indent+2,"cla_val",rec_level+1);
      Sciprintf1(indent+1,"}\n");
    }
}

/*
 * latex print 
 */

void nsp_classaref_latex(NspClassARef *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classaref_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    Sciprintf1(indent+2,"cla_color=%d\n",M->obj->cla_color);
  Sciprintf1(indent+2,"cla_thickness=%d\n",M->obj->cla_thickness);
  if ( M->obj->cla_val != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->cla_val),indent+2,"cla_val",rec_level+1);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassARef objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspClassARef   *nsp_classaref_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_classaref_id) == TRUE ) return ((NspClassARef *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_classaref));
  return NULL;
}

int IsClassARefObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_classaref_id);
}

int IsClassARef(NspObject *O)
{
  return nsp_object_type(O,nsp_type_classaref_id);
}

NspClassARef  *GetClassARefCopy(Stack stack, int i)
{
  if (  GetClassARef(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspClassARef  *GetClassARef(Stack stack, int i)
{
  NspClassARef *M;
  if (( M = nsp_classaref_object(NthObj(i))) == NULLCLASSAREF)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspClassARef *nsp_classaref_create_void(char *name,NspTypeBase *type)
{
 NspClassARef *H  = (type == NULL) ? new_classaref() : type->new();
 if ( H ==  NULLCLASSAREF)
  {
   Sciprintf("No more memory\n");
   return NULLCLASSAREF;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCLASSAREF;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_classaref_create_partial(NspClassARef *H)
{
  if((H->obj = calloc(1,sizeof(nsp_classaref)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  return OK;
}

int nsp_classaref_check_values(NspClassARef *H)
{
  if ( H->obj->cla_val == NULLMAT) 
    {
     if (( H->obj->cla_val = nsp_matrix_create("cla_val",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  return OK;
}

NspClassARef *nsp_classaref_create(char *name,int cla_color,int cla_thickness,NspMatrix* cla_val,NspTypeBase *type)
{
 NspClassARef *H  = nsp_classaref_create_void(name,type);
 if ( H ==  NULLCLASSAREF) return NULLCLASSAREF;
  if ( nsp_classaref_create_partial(H) == FAIL) return NULLCLASSAREF;
  H->obj->cla_color=cla_color;
  H->obj->cla_thickness=cla_thickness;
  if ( cla_val == NULL )
    { H->obj->cla_val = NULL;}
  else
    {
      if ((H->obj->cla_val = (NspMatrix *)  nsp_object_copy_and_name("cla_val",NSP_OBJECT(cla_val))) == NULLMAT) return NULL;
    }
 if ( nsp_classaref_check_values(H) == FAIL) return NULLCLASSAREF;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspClassARef *nsp_classaref_copy_partial(NspClassARef *H,NspClassARef *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspClassARef *nsp_classaref_copy(NspClassARef *self)
{
  NspClassARef *H  =nsp_classaref_create_void(NVOID,(NspTypeBase *) nsp_type_classaref);
  if ( H ==  NULLCLASSAREF) return NULLCLASSAREF;
  if ( nsp_classaref_copy_partial(H,self)== NULL) return NULLCLASSAREF;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the ClassARef
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_classaref_create(Stack stack, int rhs, int opt, int lhs)
{
  NspClassARef *H;
  CheckStdRhs(0,0);
  /* want to be sure that type classaref is initialized */
  nsp_type_classaref = new_type_classaref(T_BASE);
  if(( H = nsp_classaref_create_void(NVOID,(NspTypeBase *) nsp_type_classaref)) == NULLCLASSAREF) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_classaref_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_classaref_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

#line 20 "classaref.override"
static int _wrap_classa_color_change(NspClassARef *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int color;
  if ( GetArgs(stack,rhs,opt,T,&color) == FAIL) return RET_BUG;
  self->obj->cla_color = color;
  return 0;
}
#line 459 "classaref.c"


#line 30 "classaref.override"
static int _wrap_classa_color_show(NspClassARef *self,Stack stack,int rhs,int opt,int lhs)
{
  Sciprintf("color: %d\n",self->obj->cla_color);
  return 0;
}
#line 468 "classaref.c"


static NspMethods classaref_methods[] = {
  {"classa_color_change",(nsp_method *) _wrap_classa_color_change},
  {"classa_color_show",(nsp_method *) _wrap_classa_color_show},
  { NULL, NULL}
};

static NspMethods *classaref_get_methods(void) { return classaref_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_classaref_get_cla_color(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspClassARef *) self)->obj->cla_color);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_classaref_set_cla_color(void *self, char *attr, NspObject *O)
{
  int cla_color;

  if ( IntScalar(O,&cla_color) == FAIL) return FAIL;
  ((NspClassARef *) self)->obj->cla_color = cla_color;
  return OK;
}

static NspObject *_wrap_classaref_get_cla_thickness(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspClassARef *) self)->obj->cla_thickness);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_classaref_set_cla_thickness(void *self, char *attr, NspObject *O)
{
  int cla_thickness;

  if ( IntScalar(O,&cla_thickness) == FAIL) return FAIL;
  ((NspClassARef *) self)->obj->cla_thickness = cla_thickness;
  return OK;
}

static NspObject *_wrap_classaref_get_cla_val(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspClassARef *) self)->obj->cla_val);
  return (NspObject *) ret;
}

static NspObject *_wrap_classaref_get_obj_cla_val(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspClassARef *) self)->obj->cla_val);
  return (NspObject *) ret;
}

static int _wrap_classaref_set_cla_val(void *self, char *attr, NspObject *O)
{
  NspMatrix *cla_val;

  if ( ! IsMat(O) ) return FAIL;
  if ((cla_val = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspClassARef *) self)->obj->cla_val != NULL ) 
    nsp_matrix_destroy(((NspClassARef *) self)->obj->cla_val);
  ((NspClassARef *) self)->obj->cla_val = cla_val;
  return OK;
}

static AttrTab classaref_attrs[] = {
  { "cla_color", (attr_get_function *)_wrap_classaref_get_cla_color, (attr_set_function *)_wrap_classaref_set_cla_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "cla_thickness", (attr_get_function *)_wrap_classaref_get_cla_thickness, (attr_set_function *)_wrap_classaref_set_cla_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "cla_val", (attr_get_function *)_wrap_classaref_get_cla_val, (attr_set_function *)_wrap_classaref_set_cla_val,(attr_get_object_function *)_wrap_classaref_get_obj_cla_val, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 43 "classaref.override"
/* XXXX : the generated code should be corrected */
static int clareftest(NspClassARef *A)
{
  nsp_object_print((NspObject *) A,0,NULL,0);
  return TRUE;
}

static int _wrap_clareftest(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,t_end};
  int ret;
  NspObject *A;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_classaref, &A) == FAIL) return RET_BUG;
  ret = clareftest((NspClassARef *) A);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}
#line 574 "classaref.c"


#line 37 "classaref.override"
static int _wrap_setrowscols_classaref(Stack stack,int rhs,int opt,int lhs)
{
  return int_set_attribute(stack,rhs,opt,lhs);
}
#line 582 "classaref.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ClassARef_func[]={
  {"clareftest", _wrap_clareftest},
  {"setrowscols_classaref", _wrap_setrowscols_classaref},
  { "classaref_create", int_classaref_create},
  { NULL, NULL}
};

/* call ith function in the ClassARef interface */

int ClassARef_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(ClassARef_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void ClassARef_Interf_Info(int i, char **fname, function (**f))
{
  *fname = ClassARef_func[i].name;
  *f = ClassARef_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
ClassARef_register_classes(NspObject *d)
{

#line 7 "classaref.override"

/ * init code  * /


#line 622 "classaref.c"
  nspgobject_register_class(d, "ClassARef", ClassARef, &NspClassARef_Type, Nsp_BuildValue("(O)", &NspObject_Type));
}
*/

#line 627 "classaref.c"
