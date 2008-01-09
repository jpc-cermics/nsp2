/* -*- Mode: C -*- */

/* generated file */

#line 4 "classb.override"

#line 8 "classb.c"


#include <nsp/object.h>
#include <gtk/gtk.h>


/* ---------- forward type declarations ---------- */
#define ClassB_Private
#include "nsp/classb.h"


/* ----------- ClassB ----------- */


#define  ClassB_Private 
#include "nsp/object.h"
#include "nsp/classb.h"
#include "nsp/interf.h"

/* 
 * NspClassB inherits from NspClassA 
 */

int nsp_type_classb_id=0;
NspTypeClassB *nsp_type_classb=NULL;

/*
 * Type object for ClassB 
 * all the instance of NspTypeClassB share the same id. 
 * nsp_type_classb: is an instance of NspTypeClassB 
 *    used for objects of NspClassB type (i.e built with new_classb) 
 * other instances are used for derived classes 
 */
NspTypeClassB *new_type_classb(type_mode mode)
{
  NspTypeClassB *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_classb != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_classb;
    }
  if ((type =  malloc(sizeof(NspTypeClassB))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_classa(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = classb_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = classb_get_methods; 
  type->new = (new_func *) new_classb;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for classb */ 

  top->pr = (print_func *) nsp_classb_print;                  
  top->dealloc = (dealloc_func *) nsp_classb_destroy;
  top->copy  =  (copy_func *) nsp_classb_copy;                 
  top->size  = (size_func *) nsp_classb_size;                
  top->s_type =  (s_type_func *) nsp_classb_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_classb_type_short_string;
  top->info = (info_func *) nsp_classb_info ;                  
  /* top->is_true = (is_true_func  *) nsp_classb_is_true; */
  /* top->loop =(loop_func *) nsp_classb_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_classb_object;
  top->eq  = (eq_func *) nsp_classb_eq;
  top->neq  = (eq_func *) nsp_classb_neq;
  top->save  = (save_func *) nsp_classb_xdr_save;
  top->load  = (load_func *) nsp_classb_xdr_load;
  top->create = (create_func*) int_classb_create;
  top->latex = (print_func *) nsp_classb_latex_print;
  
  /* specific methods for classb */
      
  type->init = (init_func *) init_classb;

/* 
 * ClassB interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_classb_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeClassB called nsp_type_classb
       */
      type->id =  nsp_type_classb_id = nsp_new_type_id();
      nsp_type_classb = type;
      if ( nsp_register_type(nsp_type_classb) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_classb(mode);
    }
  else 
    {
       type->id = nsp_type_classb_id;
       return type;
    }
}

/*
 * initialize ClassB instances 
 * locally and by calling initializer on parent class 
 */

static int init_classb(NspClassB *Obj,NspTypeClassB *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 Obj->clb_color = 0;
 Obj->clb_thickness = 0;
  Obj->clb_val = NULLMAT;
  return OK;
}

/*
 * new instance of ClassB 
 */

NspClassB *new_classb() 
{
  NspClassB *loc; 
  /* type must exists */
  nsp_type_classb = new_type_classb(T_BASE);
  if ( (loc = malloc(sizeof(NspClassB)))== NULLCLASSB) return loc;
  /* initialize object */
  if ( init_classb(loc,nsp_type_classb) == FAIL) return NULLCLASSB;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ClassB 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_classb_size(NspClassB *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char classb_type_name[]="ClassB";
static char classb_short_type_name[]="classb";

static char *nsp_classb_type_as_string(void)
{
  return(classb_type_name);
}

static char *nsp_classb_type_short_string(NspObject *v)
{
  return(classb_short_type_name);
}

/*
 * A == B 
 */

static int nsp_classb_eq(NspClassB *A, NspObject *B)
{
  NspClassB *loc = (NspClassB *) B;
  if ( check_cast(B,nsp_type_classb_id) == FALSE) return FALSE ;
  if ( A->clb_color != loc->clb_color) return FALSE;
  if ( A->clb_thickness != loc->clb_thickness) return FALSE;
  if ( NSP_OBJECT(A->clb_val)->type->eq(A->clb_val,loc->clb_val) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_classb_neq(NspClassB *A, NspObject *B)
{
  return ( nsp_classb_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_classb_xdr_save(XDR *xdrs, NspClassB *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->clb_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->clb_thickness) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->clb_val)) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspClassB  *nsp_classb_xdr_load(XDR *xdrs)
{
  NspClassB *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCLASSB;
  if ((M  = nsp_classb_create_void(name,(NspTypeBase *) nsp_type_classb))== NULLCLASSB) return M;
  if (nsp_xdr_load_i(xdrs, &M->clb_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->clb_thickness) == FAIL) return NULL;
  if ((M->clb_val =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
 return M;
}

/*
 * delete 
 */

void nsp_classb_destroy_partial(NspClassB *H)
{
  nsp_matrix_destroy(H->clb_val);
}

void nsp_classb_destroy(NspClassB *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_classb_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

void nsp_classb_info(NspClassB *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCLASSB) 
    {
      Sciprintf("Null Pointer ClassB \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_classb_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_classb_print(NspClassB *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCLASSB) 
    {
      Sciprintf("Null Pointer ClassB \n");
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
          nsp_classb_info(M,indent,pname,rec_level);
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classb_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        Sciprintf1(indent+2,"clb_color=%d\n",M->clb_color);
  Sciprintf1(indent+2,"clb_thickness=%d\n",M->clb_thickness);
  nsp_object_print(NSP_OBJECT(M->clb_val),indent+2,"clb_val",rec_level+1);
      Sciprintf1(indent+1,"}\n");
    }
}

/*
 * latex print 
 */

void nsp_classb_latex_print(NspClassB *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classb_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    Sciprintf1(indent+2,"clb_color=%d\n",M->clb_color);
  Sciprintf1(indent+2,"clb_thickness=%d\n",M->clb_thickness);
  nsp_object_print(NSP_OBJECT(M->clb_val),indent+2,"clb_val",rec_level+1);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassB objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspClassB   *nsp_classb_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_classb_id) == TRUE ) return ((NspClassB *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_classb));
  return NULL;
}

int IsClassBObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_classb_id);
}

int IsClassB(NspObject *O)
{
  return nsp_object_type(O,nsp_type_classb_id);
}

NspClassB  *GetClassBCopy(Stack stack, int i)
{
  if (  GetClassB(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspClassB  *GetClassB(Stack stack, int i)
{
  NspClassB *M;
  if (( M = nsp_classb_object(NthObj(i))) == NULLCLASSB)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspClassB *nsp_classb_create_void(char *name,NspTypeBase *type)
{
 NspClassB *H  = (type == NULL) ? new_classb() : type->new();
 if ( H ==  NULLCLASSB)
  {
   Sciprintf("No more memory\n");
   return NULLCLASSB;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCLASSB;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_classb_create_partial(NspClassB *H)
{
  return OK;
}

NspClassB *nsp_classb_create(char *name,int clb_color,int clb_thickness,NspMatrix* clb_val,NspTypeBase *type)
{
 NspClassB *H  = nsp_classb_create_void(name,type);
 if ( H ==  NULLCLASSB) return NULLCLASSB;
  H->clb_color=clb_color;
  H->clb_thickness=clb_thickness;
  if ((H->clb_val = (NspMatrix *)  nsp_object_copy_and_name("clb_val",NSP_OBJECT(clb_val))) == NULLMAT) return NULL;
 return H;
}

/*
 * copy for gobject derived class  
 */

void nsp_classb_copy_partial(NspClassB *H,NspClassB *self)
{
}

NspClassB *nsp_classb_copy(NspClassB *self)
{
  NspClassB *H  =nsp_classb_create_void(NVOID,(NspTypeBase *) nsp_type_classb);
  if ( H ==  NULLCLASSB) return NULLCLASSB;
  H->clb_color=self->clb_color;
  H->clb_thickness=self->clb_thickness;
  if ((H->clb_val = (NspMatrix *) nsp_object_copy_and_name("clb_val",NSP_OBJECT(self->clb_val))) == NULLMAT) return NULL;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the ClassB
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_classb_create(Stack stack, int rhs, int opt, int lhs)
{
  NspClassB *H;
  CheckStdRhs(0,0);
  /* want to be sure that type classb is initialized */
  nsp_type_classb = new_type_classb(T_BASE);
  if(( H = nsp_classb_create_void(NVOID,(NspTypeBase *) nsp_type_classb)) == NULLCLASSB) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
  if ( H->clb_val == NULLMAT) {Scierror("Error: field clb_val is to be set\n");return RET_BUG;}
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

#line 19 "classb.override"
static int _wrap_classb_color_change(NspClassB *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int color;
  if ( GetArgs(stack,rhs,opt,T,&color) == FAIL) return RET_BUG;
  self->clb_color = color;
  return 0;
}
#line 430 "classb.c"


#line 29 "classb.override"
static int _wrap_classb_color_show(NspClassB *self,Stack stack,int rhs,int opt,int lhs)
{
  Sciprintf("color: %d\n",self->clb_color);
  return 0;
}


#line 441 "classb.c"


static NspMethods classb_methods[] = {
  {"color_change",(nsp_method *) _wrap_classb_color_change},
  {"color_show",(nsp_method *) _wrap_classb_color_show},
  { NULL, NULL}
};

static NspMethods *classb_get_methods(void) { return classb_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_classb_get_clb_color(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspClassB *) self)->clb_color);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_classb_set_clb_color(void *self, char *attr, NspObject *O)
{
  int clb_color;

  if ( IntScalar(O,&clb_color) == FAIL) return FAIL;
  ((NspClassB *) self)->clb_color = clb_color;
  return OK;
}

static NspObject *_wrap_classb_get_clb_thickness(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspClassB *) self)->clb_thickness);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_classb_set_clb_thickness(void *self, char *attr, NspObject *O)
{
  int clb_thickness;

  if ( IntScalar(O,&clb_thickness) == FAIL) return FAIL;
  ((NspClassB *) self)->clb_thickness = clb_thickness;
  return OK;
}

static NspObject *_wrap_classb_get_clb_val(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspClassB *) self)->clb_val);
  return (NspObject *) ret;
}

static int _wrap_classb_set_clb_val(void *self, char *attr, NspObject *O)
{
  NspMatrix *clb_val;

  if ( ! IsMat(O) ) return FAIL;
  if ((clb_val = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspClassB *) self)->clb_val != NULL ) 
    nsp_object_destroy((NspObject **) &((NspClassB *) self)->clb_val);
  ((NspClassB *) self)->clb_val = clb_val;
  return OK;
}

static AttrTab classb_attrs[] = {
  { "clb_color", (attr_get_function *)_wrap_classb_get_clb_color, (attr_set_function *)_wrap_classb_set_clb_color,(attr_get_object_function *)int_get_object_failed },
  { "clb_thickness", (attr_get_function *)_wrap_classb_get_clb_thickness, (attr_set_function *)_wrap_classb_set_clb_thickness,(attr_get_object_function *)int_get_object_failed },
  { "clb_val", (attr_get_function *)_wrap_classb_get_clb_val, (attr_set_function *)_wrap_classb_set_clb_val,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ClassB_func[]={
  { "classb_create", int_classb_create},
  { NULL, NULL}
};

/* call ith function in the ClassB interface */

int ClassB_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(ClassB_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void ClassB_Interf_Info(int i, char **fname, function (**f))
{
  *fname = ClassB_func[i].name;
  *f = ClassB_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
ClassB_register_classes(NspObject *d)
{

#line 7 "classb.override"

/ * init * /


#line 555 "classb.c"
  nspgobject_register_class(d, "ClassB", ClassB, &PyClassB_Type, Py_BuildValue("(O)", &PyClassA_Type));
}
*/
