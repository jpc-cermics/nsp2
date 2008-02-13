/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "classa.override"

#line 14 "classa.c"

/* ----------- ClassA ----------- */


#define  ClassA_Private 
#include "nsp/object.h"
#include "nsp/classa.h"
#include "nsp/interf.h"

/* 
 * NspClassA inherits from NspObject 
 */

int nsp_type_classa_id=0;
NspTypeClassA *nsp_type_classa=NULL;

/*
 * Type object for ClassA 
 * all the instance of NspTypeClassA share the same id. 
 * nsp_type_classa: is an instance of NspTypeClassA 
 *    used for objects of NspClassA type (i.e built with new_classa) 
 * other instances are used for derived classes 
 */
NspTypeClassA *new_type_classa(type_mode mode)
{
  NspTypeClassA *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_classa != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_classa;
    }
  if ((type =  malloc(sizeof(NspTypeClassA))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = classa_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = classa_get_methods; 
  type->new = (new_func *) new_classa;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for classa */ 

  top->pr = (print_func *) nsp_classa_print;                  
  top->dealloc = (dealloc_func *) nsp_classa_destroy;
  top->copy  =  (copy_func *) nsp_classa_copy;                 
  top->size  = (size_func *) nsp_classa_size;                
  top->s_type =  (s_type_func *) nsp_classa_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_classa_type_short_string;
  top->info = (info_func *) nsp_classa_info ;                  
  /* top->is_true = (is_true_func  *) nsp_classa_is_true; */
  /* top->loop =(loop_func *) nsp_classa_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_classa_object;
  top->eq  = (eq_func *) nsp_classa_eq;
  top->neq  = (eq_func *) nsp_classa_neq;
  top->save  = (save_func *) nsp_classa_xdr_save;
  top->load  = (load_func *) nsp_classa_xdr_load;
  top->create = (create_func*) int_classa_create;
  top->latex = (print_func *) nsp_classa_latex;
  
  /* specific methods for classa */
      
  type->init = (init_func *) init_classa;

  /* 
   * ClassA interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_classa_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeClassA called nsp_type_classa
       */
      type->id =  nsp_type_classa_id = nsp_new_type_id();
      nsp_type_classa = type;
      if ( nsp_register_type(nsp_type_classa) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_classa(mode);
    }
  else 
    {
       type->id = nsp_type_classa_id;
       return type;
    }
}

/*
 * initialize ClassA instances 
 * locally and by calling initializer on parent class 
 */

static int init_classa(NspClassA *Obj,NspTypeClassA *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->cla_color = 0;
  Obj->cla_thickness = 0;
  Obj->cla_val = NULLMAT;
  Obj->cla_bval = NULLBMAT;
  Obj->cla_lval = NULLLIST;
  return OK;
}

/*
 * new instance of ClassA 
 */

NspClassA *new_classa() 
{
  NspClassA *loc; 
  /* type must exists */
  nsp_type_classa = new_type_classa(T_BASE);
  if ( (loc = malloc(sizeof(NspClassA)))== NULLCLASSA) return loc;
  /* initialize object */
  if ( init_classa(loc,nsp_type_classa) == FAIL) return NULLCLASSA;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ClassA 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_classa_size(NspClassA *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char classa_type_name[]="ClassA";
static char classa_short_type_name[]="classa";

static char *nsp_classa_type_as_string(void)
{
  return(classa_type_name);
}

static char *nsp_classa_type_short_string(NspObject *v)
{
  return(classa_short_type_name);
}

/*
 * A == B 
 */

static int nsp_classa_eq(NspClassA *A, NspObject *B)
{
  NspClassA *loc = (NspClassA *) B;
  if ( check_cast(B,nsp_type_classa_id) == FALSE) return FALSE ;
  if ( A->cla_color != loc->cla_color) return FALSE;
  if ( A->cla_thickness != loc->cla_thickness) return FALSE;
  if ( NSP_OBJECT(A->cla_val)->type->eq(A->cla_val,loc->cla_val) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->cla_bval)->type->eq(A->cla_bval,loc->cla_bval) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->cla_lval)->type->eq(A->cla_lval,loc->cla_lval) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_classa_neq(NspClassA *A, NspObject *B)
{
  return ( nsp_classa_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_classa_xdr_save(XDR *xdrs, NspClassA *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->cla_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->cla_thickness) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->cla_val)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->cla_bval)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->cla_lval)) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspClassA  *nsp_classa_xdr_load_partial(XDR *xdrs, NspClassA *M)
{
  if (nsp_xdr_load_i(xdrs, &M->cla_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->cla_thickness) == FAIL) return NULL;
  if ((M->cla_val =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->cla_bval =(NspBMatrix *) nsp_object_xdr_load(xdrs))== NULLBMAT) return NULL;
  if ((M->cla_lval =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
 return M;
}

static NspClassA  *nsp_classa_xdr_load(XDR *xdrs)
{
  NspClassA *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCLASSA;
  if ((M  = nsp_classa_create_void(name,(NspTypeBase *) nsp_type_classa))== NULLCLASSA) return M;
  return nsp_classa_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_classa_destroy_partial(NspClassA *H)
{
  nsp_matrix_destroy(H->cla_val);
  nsp_bmatrix_destroy(H->cla_bval);
  nsp_list_destroy(H->cla_lval);
}

void nsp_classa_destroy(NspClassA *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_classa_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

void nsp_classa_info(NspClassA *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCLASSA) 
    {
      Sciprintf("Null Pointer ClassA \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_classa_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_classa_print(NspClassA *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCLASSA) 
    {
      Sciprintf("Null Pointer ClassA \n");
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
          nsp_classa_info(M,indent,pname,rec_level);
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classa_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        Sciprintf1(indent+2,"cla_color=%d\n",M->cla_color);
  Sciprintf1(indent+2,"cla_thickness=%d\n",M->cla_thickness);
  if ( M->cla_val != NULL)
    nsp_object_print(NSP_OBJECT(M->cla_val),indent+2,"cla_val",rec_level+1);
  if ( M->cla_bval != NULL)
    nsp_object_print(NSP_OBJECT(M->cla_bval),indent+2,"cla_bval",rec_level+1);
  if ( M->cla_lval != NULL)
    nsp_object_print(NSP_OBJECT(M->cla_lval),indent+2,"cla_lval",rec_level+1);
      Sciprintf1(indent+1,"}\n");
    }
}

/*
 * latex print 
 */

void nsp_classa_latex(NspClassA *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classa_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    Sciprintf1(indent+2,"cla_color=%d\n",M->cla_color);
  Sciprintf1(indent+2,"cla_thickness=%d\n",M->cla_thickness);
  if ( M->cla_val != NULL)
    nsp_object_latex(NSP_OBJECT(M->cla_val),indent+2,"cla_val",rec_level+1);
  if ( M->cla_bval != NULL)
    nsp_object_latex(NSP_OBJECT(M->cla_bval),indent+2,"cla_bval",rec_level+1);
  if ( M->cla_lval != NULL)
    nsp_object_latex(NSP_OBJECT(M->cla_lval),indent+2,"cla_lval",rec_level+1);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassA objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspClassA   *nsp_classa_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_classa_id) == TRUE ) return ((NspClassA *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_classa));
  return NULL;
}

int IsClassAObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_classa_id);
}

int IsClassA(NspObject *O)
{
  return nsp_object_type(O,nsp_type_classa_id);
}

NspClassA  *GetClassACopy(Stack stack, int i)
{
  if (  GetClassA(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspClassA  *GetClassA(Stack stack, int i)
{
  NspClassA *M;
  if (( M = nsp_classa_object(NthObj(i))) == NULLCLASSA)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspClassA *nsp_classa_create_void(char *name,NspTypeBase *type)
{
 NspClassA *H  = (type == NULL) ? new_classa() : type->new();
 if ( H ==  NULLCLASSA)
  {
   Sciprintf("No more memory\n");
   return NULLCLASSA;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCLASSA;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_classa_create_partial(NspClassA *H)
{
  return OK;
}

int nsp_classa_check_values(NspClassA *H)
{
  if ( H->cla_val == NULLMAT) 
    {
     if (( H->cla_val = nsp_matrix_create("cla_val",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->cla_bval == NULLBMAT) 
    {
     if (( H->cla_bval = nsp_bmatrix_create("cla_bval",0,0)) == NULLBMAT)
       return FAIL;
    }
  if ( H->cla_lval == NULLLIST) 
    {
     if (( H->cla_lval = nsp_list_create("cla_lval")) == NULLLIST)
       return FAIL;
    }
  return OK;
}

NspClassA *nsp_classa_create(char *name,int cla_color,int cla_thickness,NspMatrix* cla_val,NspBMatrix* cla_bval,NspList* cla_lval,NspTypeBase *type)
{
 NspClassA *H  = nsp_classa_create_void(name,type);
 if ( H ==  NULLCLASSA) return NULLCLASSA;
  H->cla_color=cla_color;
  H->cla_thickness=cla_thickness;
  if ( cla_val == NULL )
    { H->cla_val = NULL;}
  else
    {
      if ((H->cla_val = (NspMatrix *)  nsp_object_copy_and_name("cla_val",NSP_OBJECT(cla_val))) == NULLMAT) return NULL;
    }
  if ( cla_bval == NULL )
    { H->cla_bval = NULL;}
  else
    {
      if ((H->cla_bval = (NspBMatrix *)  nsp_object_copy_and_name("cla_bval",NSP_OBJECT(cla_bval))) == NULLBMAT) return NULL;
    }
  if ( cla_lval == NULL )
    { H->cla_lval = NULL;}
  else
    {
      if ((H->cla_lval = (NspList *)  nsp_object_copy_and_name("cla_lval",NSP_OBJECT(cla_lval))) == NULLLIST) return NULL;
    }
 if ( nsp_classa_check_values(H) == FAIL) return NULLCLASSA;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspClassA *nsp_classa_copy_partial(NspClassA *H,NspClassA *self)
{
  H->cla_color=self->cla_color;
  H->cla_thickness=self->cla_thickness;
  if ( self->cla_val == NULL )
    { H->cla_val = NULL;}
  else
    {
      if ((H->cla_val = (NspMatrix *) nsp_object_copy_and_name("cla_val",NSP_OBJECT(self->cla_val))) == NULLMAT) return NULL;
    }
  if ( self->cla_bval == NULL )
    { H->cla_bval = NULL;}
  else
    {
      if ((H->cla_bval = (NspBMatrix *) nsp_object_copy_and_name("cla_bval",NSP_OBJECT(self->cla_bval))) == NULLBMAT) return NULL;
    }
  if ( self->cla_lval == NULL )
    { H->cla_lval = NULL;}
  else
    {
      if ((H->cla_lval = (NspList *) nsp_object_copy_and_name("cla_lval",NSP_OBJECT(self->cla_lval))) == NULLLIST) return NULL;
    }
  return H;
}

NspClassA *nsp_classa_copy(NspClassA *self)
{
  NspClassA *H  =nsp_classa_create_void(NVOID,(NspTypeBase *) nsp_type_classa);
  if ( H ==  NULLCLASSA) return NULLCLASSA;
  if ( nsp_classa_copy_partial(H,self)== NULL) return NULLCLASSA;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the ClassA
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_classa_create(Stack stack, int rhs, int opt, int lhs)
{
  NspClassA *H;
  CheckStdRhs(0,0);
  /* want to be sure that type classa is initialized */
  nsp_type_classa = new_type_classa(T_BASE);
  if(( H = nsp_classa_create_void(NVOID,(NspTypeBase *) nsp_type_classa)) == NULLCLASSA) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_classa_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

#line 20 "classa.override"
static int _wrap_classa_color_change(NspClassA *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int color;
  if ( GetArgs(stack,rhs,opt,T,&color) == FAIL) return RET_BUG;
  self->cla_color = color;
  return 0;
}
#line 509 "classa.c"


#line 30 "classa.override"
static int _wrap_classa_color_show(NspClassA *self,Stack stack,int rhs,int opt,int lhs)
{
  Sciprintf("color: %d\n",self->cla_color);
  return 0;
}
#line 518 "classa.c"


static NspMethods classa_methods[] = {
  {"classa_color_change",(nsp_method *) _wrap_classa_color_change},
  {"classa_color_show",(nsp_method *) _wrap_classa_color_show},
  { NULL, NULL}
};

static NspMethods *classa_get_methods(void) { return classa_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_classa_get_cla_color(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspClassA *) self)->cla_color);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_classa_set_cla_color(void *self, char *attr, NspObject *O)
{
  int cla_color;

  if ( IntScalar(O,&cla_color) == FAIL) return FAIL;
  ((NspClassA *) self)->cla_color = cla_color;
  return OK;
}

static NspObject *_wrap_classa_get_cla_thickness(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspClassA *) self)->cla_thickness);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_classa_set_cla_thickness(void *self, char *attr, NspObject *O)
{
  int cla_thickness;

  if ( IntScalar(O,&cla_thickness) == FAIL) return FAIL;
  ((NspClassA *) self)->cla_thickness = cla_thickness;
  return OK;
}

static NspObject *_wrap_classa_get_cla_val(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspClassA *) self)->cla_val);
  return (NspObject *) ret;
}

static NspObject *_wrap_classa_get_cla_val_obj(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspClassA *) self)->cla_val);
  return (NspObject *) ret;
}

static int _wrap_classa_set_cla_val(void *self, char *attr, NspObject *O)
{
  NspMatrix *cla_val;

  if ( ! IsMat(O) ) return FAIL;
  if ((cla_val = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspClassA *) self)->cla_val != NULL ) 
    nsp_matrix_destroy(((NspClassA *) self)->cla_val);
  ((NspClassA *) self)->cla_val = cla_val;
  return OK;
}

static NspObject *_wrap_classa_get_cla_bval(void *self,char *attr)
{
  NspBMatrix *ret;

  ret = ((NspBMatrix*) ((NspClassA *) self)->cla_bval);
  return (NspObject *) ret;
}

static NspObject *_wrap_classa_get_cla_bval_obj(void *self,char *attr)
{
  NspBMatrix *ret;

  ret = ((NspBMatrix*) ((NspClassA *) self)->cla_bval);
  return (NspObject *) ret;
}

static int _wrap_classa_set_cla_bval(void *self, char *attr, NspObject *O)
{
  NspBMatrix *cla_bval;

  if ( ! IsBMat(O) ) return FAIL;
  if ((cla_bval = (NspBMatrix *) nsp_object_copy_and_name(attr,O)) == NULLBMAT) return FAIL;
  if (((NspClassA *) self)->cla_bval != NULL ) 
    nsp_bmatrix_destroy(((NspClassA *) self)->cla_bval);
  ((NspClassA *) self)->cla_bval = cla_bval;
  return OK;
}

static NspObject *_wrap_classa_get_cla_lval(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspList*) ((NspClassA *) self)->cla_lval);
  return (NspObject *) ret;
}


static int _wrap_classa_set_cla_lval(void *self, char *attr, NspObject *O)
{
  NspList *cla_lval;

  if ( ! IsList(O) ) return FAIL;
  if ((cla_lval = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspClassA *) self)->cla_lval != NULL ) 
    nsp_list_destroy(((NspClassA *) self)->cla_lval);
  ((NspClassA *) self)->cla_lval = cla_lval;
  return OK;
}

/* we use here copy = TRUE
 * thus 
 */

static NspObject *_wrap_classa_get_cla_lval_obj(void *self,char *attr, int *copy)
{
  NspList *ret;
  *copy = TRUE;
  ret = ((NspList*) ((NspClassA *) self)->cla_lval);
  return (NspObject *) ret;
}

static int _wrap_classa_set_cla_lval_obj(void *self,NspObject *val)
{
  if (((NspClassA *) self)->cla_lval != NULL ) 
    nsp_list_destroy(((NspClassA *) self)->cla_lval);
  ((NspClassA *) self)->cla_lval = (NspList *) val;
  return FAIL;
}

/* (attr_get_object_function *)int_get_object_failed,(attr_set_object_function *)int_set_object_failed },*/
static AttrTab classa_attrs[] = {
  { "cla_color", (attr_get_function *)_wrap_classa_get_cla_color, (attr_set_function *)_wrap_classa_set_cla_color,NULL,NULL},
  { "cla_thickness", (attr_get_function *)_wrap_classa_get_cla_thickness, (attr_set_function *)_wrap_classa_set_cla_thickness,(attr_get_object_function *)int_get_object_failed ,(attr_set_object_function *)int_set_object_failed},
  { "cla_val", (attr_get_function *)_wrap_classa_get_cla_val, (attr_set_function *)_wrap_classa_set_cla_val,(attr_get_object_function *)_wrap_classa_get_cla_val_obj,(attr_set_object_function *)int_set_object_failed },
  { "cla_bval", (attr_get_function *)_wrap_classa_get_cla_bval, (attr_set_function *)_wrap_classa_set_cla_bval,(attr_get_object_function *)_wrap_classa_get_cla_bval_obj,(attr_set_object_function *) int_set_object_failed },
  { "cla_lval", (attr_get_function *)_wrap_classa_get_cla_lval, (attr_set_function *)_wrap_classa_set_cla_lval,(attr_get_object_function *)_wrap_classa_get_cla_lval_obj, _wrap_classa_set_cla_lval_obj },
  { NULL,NULL,NULL,NULL, NULL  },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 43 "classa.override"
/* XXXX : the generated code should be corrected */
static int clatest(NspClassA *A)
{
  nsp_object_print((NspObject *) A,0,NULL,0);
  return TRUE;
}

static int _wrap_clatest(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,t_end};
  int ret;
  NspObject *A;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_classa, &A) == FAIL) return RET_BUG;
  ret = clatest((NspClassA *) A);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}
#line 681 "classa.c"


#line 37 "classa.override"
static int _wrap_setrowscols_classa(Stack stack,int rhs,int opt,int lhs)
{
  return int_set_attribute(stack,rhs,opt,lhs);
}
#line 689 "classa.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ClassA_func[]={
  {"clatest", _wrap_clatest},
  {"setrowscols_classa", _wrap_setrowscols_classa},
  { "classa_create", int_classa_create},
  { NULL, NULL}
};

/* call ith function in the ClassA interface */

int ClassA_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(ClassA_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void ClassA_Interf_Info(int i, char **fname, function (**f))
{
  *fname = ClassA_func[i].name;
  *f = ClassA_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
ClassA_register_classes(NspObject *d)
{

#line 7 "classa.override"

/ * init code  * /


#line 729 "classa.c"
  nspgobject_register_class(d, "ClassA", ClassA, &NspClassA_Type, Nsp_BuildValue("(O)", &NspObject_Type));
}
*/

#line 734 "classa.c"
