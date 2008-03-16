/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "axes.override"
#include "nsp/axes.h"
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_axes(BCG *Xgc,NspGraphic *Obj);
static void nsp_translate_axes(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_axes(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_axes(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_axes(BCG *Xgc,NspGraphic *o,double *bounds);



#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 28 "axes.c"

/* ----------- Axes ----------- */


#define  Axes_Private 
#include "nsp/object.h"
#include "nsp/axes.h"
#include "nsp/interf.h"

/* 
 * NspAxes inherits from NspGraphic 
 */

int nsp_type_axes_id=0;
NspTypeAxes *nsp_type_axes=NULL;

/*
 * Type object for Axes 
 * all the instance of NspTypeAxes share the same id. 
 * nsp_type_axes: is an instance of NspTypeAxes 
 *    used for objects of NspAxes type (i.e built with new_axes) 
 * other instances are used for derived classes 
 */
NspTypeAxes *new_type_axes(type_mode mode)
{
  NspTypeAxes *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_axes != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_axes;
    }
  if ((type =  malloc(sizeof(NspTypeAxes))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = axes_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = axes_get_methods; 
  type->new = (new_func *) new_axes;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for axes */ 

  top->pr = (print_func *) nsp_axes_print;                  
  top->dealloc = (dealloc_func *) nsp_axes_destroy;
  top->copy  =  (copy_func *) nsp_axes_copy;                 
  top->size  = (size_func *) nsp_axes_size;                
  top->s_type =  (s_type_func *) nsp_axes_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_axes_type_short_string;
  top->info = (info_func *) nsp_axes_info ;                  
  /* top->is_true = (is_true_func  *) nsp_axes_is_true; */
  /* top->loop =(loop_func *) nsp_axes_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_axes_object;
  top->eq  = (eq_func *) nsp_axes_eq;
  top->neq  = (eq_func *) nsp_axes_neq;
  top->save  = (save_func *) nsp_axes_xdr_save;
  top->load  = (load_func *) nsp_axes_xdr_load;
  top->create = (create_func*) int_axes_create;
  top->latex = (print_func *) nsp_axes_latex;
  
  /* specific methods for axes */
      
  type->init = (init_func *) init_axes;

#line 26 "axes.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_axes;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_axes ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_axes  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_axes  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_axes  ;

#line 107 "axes.c"
  /* 
   * Axes interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_axes_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAxes called nsp_type_axes
       */
      type->id =  nsp_type_axes_id = nsp_new_type_id();
      nsp_type_axes = type;
      if ( nsp_register_type(nsp_type_axes) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_axes(mode);
    }
  else 
    {
       type->id = nsp_type_axes_id;
       return type;
    }
}

/*
 * initialize Axes instances 
 * locally and by calling initializer on parent class 
 */

static int init_axes(NspAxes *Obj,NspTypeAxes *type)
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
 * new instance of Axes 
 */

NspAxes *new_axes() 
{
  NspAxes *loc; 
  /* type must exists */
  nsp_type_axes = new_type_axes(T_BASE);
  if ( (loc = malloc(sizeof(NspAxes)))== NULLAXES) return loc;
  /* initialize object */
  if ( init_axes(loc,nsp_type_axes) == FAIL) return NULLAXES;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Axes 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_axes_size(NspAxes *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char axes_type_name[]="Axes";
static char axes_short_type_name[]="axes";

static char *nsp_axes_type_as_string(void)
{
  return(axes_type_name);
}

static char *nsp_axes_type_short_string(NspObject *v)
{
  return(axes_short_type_name);
}

/*
 * A == B 
 */

static int nsp_axes_eq(NspAxes *A, NspObject *B)
{
  NspAxes *loc = (NspAxes *) B;
  if ( check_cast(B,nsp_type_axes_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->frect)->type->eq(A->obj->frect,loc->obj->frect) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->wrect)->type->eq(A->obj->wrect,loc->obj->wrect) == FALSE ) return FALSE;
  if ( A->obj->top != loc->obj->top) return FALSE;
  if ( NSP_OBJECT(A->obj->elts_bounds)->type->eq(A->obj->elts_bounds,loc->obj->elts_bounds) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->elts)->type->eq(A->obj->elts,loc->obj->elts) == FALSE ) return FALSE;
  if ( A->obj->alpha != loc->obj->alpha) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_axes_neq(NspAxes *A, NspObject *B)
{
  return ( nsp_axes_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_axes_xdr_save(XDR *xdrs, NspAxes *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->frect)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->wrect)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->top) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->elts_bounds)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->elts)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->alpha) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAxes  *nsp_axes_xdr_load_partial(XDR *xdrs, NspAxes *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = malloc(sizeof(nsp_axes))) == NULL) return NULL;
  if ((M->obj->frect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->wrect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->top) == FAIL) return NULL;
  if ((M->obj->elts_bounds =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->elts =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->alpha) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspAxes  *nsp_axes_xdr_load(XDR *xdrs)
{
  NspAxes *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLAXES;
  if ((M  = nsp_axes_create_void(name,(NspTypeBase *) nsp_type_axes))== NULLAXES) return M;
  return nsp_axes_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_axes_destroy_partial(NspAxes *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_matrix_destroy(H->obj->frect);
  nsp_matrix_destroy(H->obj->wrect);
  nsp_matrix_destroy(H->obj->elts_bounds);
  nsp_list_destroy(H->obj->elts);
    FREE(H->obj);
   }
}

void nsp_axes_destroy(NspAxes *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_axes_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

void nsp_axes_info(NspAxes *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLAXES) 
    {
      Sciprintf("Null Pointer Axes \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_axes_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_axes_print(NspAxes *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLAXES) 
    {
      Sciprintf("Null Pointer Axes \n");
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
          nsp_axes_info(M,indent,pname,rec_level);
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_axes_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        if ( M->obj->frect != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1);
  if ( M->obj->wrect != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1);
  Sciprintf1(indent+2,"top=%d\n",M->obj->top);
  if ( M->obj->elts_bounds != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->elts_bounds),indent+2,"elts_bounds",rec_level+1);
  if ( M->obj->elts != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->elts),indent+2,"elts",rec_level+1);
  Sciprintf1(indent+2,"alpha=%f\n",M->obj->alpha);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
}

/*
 * latex print 
 */

void nsp_axes_latex(NspAxes *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_axes_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    if ( M->obj->frect != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1);
  if ( M->obj->wrect != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1);
  Sciprintf1(indent+2,"top=%d\n",M->obj->top);
  if ( M->obj->elts_bounds != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->elts_bounds),indent+2,"elts_bounds",rec_level+1);
  if ( M->obj->elts != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->elts),indent+2,"elts",rec_level+1);
  Sciprintf1(indent+2,"alpha=%f\n",M->obj->alpha);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Axes objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAxes   *nsp_axes_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_axes_id) == TRUE ) return ((NspAxes *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_axes));
  return NULL;
}

int IsAxesObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_axes_id);
}

int IsAxes(NspObject *O)
{
  return nsp_object_type(O,nsp_type_axes_id);
}

NspAxes  *GetAxesCopy(Stack stack, int i)
{
  if (  GetAxes(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAxes  *GetAxes(Stack stack, int i)
{
  NspAxes *M;
  if (( M = nsp_axes_object(NthObj(i))) == NULLAXES)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspAxes *nsp_axes_create_void(char *name,NspTypeBase *type)
{
 NspAxes *H  = (type == NULL) ? new_axes() : type->new();
 if ( H ==  NULLAXES)
  {
   Sciprintf("No more memory\n");
   return NULLAXES;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLAXES;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_axes_create_partial(NspAxes *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_axes)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  return OK;
}

int nsp_axes_check_values(NspAxes *H)
{
  if ( H->obj->frect == NULLMAT) 
    {
     if (( H->obj->frect = nsp_matrix_create("frect",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->obj->wrect == NULLMAT) 
    {
     if (( H->obj->wrect = nsp_matrix_create("wrect",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->obj->elts_bounds == NULLMAT) 
    {
     if (( H->obj->elts_bounds = nsp_matrix_create("elts_bounds",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  if ( H->obj->elts == NULLLIST) 
    {
     if (( H->obj->elts = nsp_list_create("elts")) == NULLLIST)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspAxes *nsp_axes_create(char *name,NspMatrix* frect,NspMatrix* wrect,gboolean top,NspMatrix* elts_bounds,NspList* elts,double alpha,NspTypeBase *type)
{
 NspAxes *H  = nsp_axes_create_void(name,type);
 if ( H ==  NULLAXES) return NULLAXES;
  if ( nsp_axes_create_partial(H) == FAIL) return NULLAXES;
  if ( frect == NULL )
    { H->obj->frect = NULL;}
  else
    {
      if ((H->obj->frect = (NspMatrix *)  nsp_object_copy_and_name("frect",NSP_OBJECT(frect))) == NULLMAT) return NULL;
    }
  if ( wrect == NULL )
    { H->obj->wrect = NULL;}
  else
    {
      if ((H->obj->wrect = (NspMatrix *)  nsp_object_copy_and_name("wrect",NSP_OBJECT(wrect))) == NULLMAT) return NULL;
    }
  H->obj->top=top;
  if ( elts_bounds == NULL )
    { H->obj->elts_bounds = NULL;}
  else
    {
      if ((H->obj->elts_bounds = (NspMatrix *)  nsp_object_copy_and_name("elts_bounds",NSP_OBJECT(elts_bounds))) == NULLMAT) return NULL;
    }
  if ( elts == NULL )
    { H->obj->elts = NULL;}
  else
    {
      if ((H->obj->elts = (NspList *)  nsp_object_copy_and_name("elts",NSP_OBJECT(elts))) == NULLLIST) return NULL;
    }
  H->obj->alpha=alpha;
 if ( nsp_axes_check_values(H) == FAIL) return NULLAXES;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAxes *nsp_axes_copy_partial(NspAxes *H,NspAxes *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspAxes *nsp_axes_copy(NspAxes *self)
{
  NspAxes *H  =nsp_axes_create_void(NVOID,(NspTypeBase *) nsp_type_axes);
  if ( H ==  NULLAXES) return NULLAXES;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLAXES;
  if ( nsp_axes_copy_partial(H,self)== NULL) return NULLAXES;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Axes
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_axes_create(Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *H;
  CheckStdRhs(0,0);
  /* want to be sure that type axes is initialized */
  nsp_type_axes = new_type_axes(T_BASE);
  if(( H = nsp_axes_create_void(NVOID,(NspTypeBase *) nsp_type_axes)) == NULLAXES) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_axes_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_axes_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *axes_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_axes_get_frect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspAxes *) self)->obj->frect);
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_frect(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAxes *) self)->obj->frect);
  return (NspObject *) ret;
}

static int _wrap_axes_set_frect(void *self, char *attr, NspObject *O)
{
  NspMatrix *frect;

  if ( ! IsMat(O) ) return FAIL;
  if ((frect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAxes *) self)->obj->frect != NULL ) 
    nsp_matrix_destroy(((NspAxes *) self)->obj->frect);
  ((NspAxes *) self)->obj->frect = frect;
  return OK;
}

static NspObject *_wrap_axes_get_wrect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspAxes *) self)->obj->wrect);
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_wrect(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAxes *) self)->obj->wrect);
  return (NspObject *) ret;
}

static int _wrap_axes_set_wrect(void *self, char *attr, NspObject *O)
{
  NspMatrix *wrect;

  if ( ! IsMat(O) ) return FAIL;
  if ((wrect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAxes *) self)->obj->wrect != NULL ) 
    nsp_matrix_destroy(((NspAxes *) self)->obj->wrect);
  ((NspAxes *) self)->obj->wrect = wrect;
  return OK;
}

static NspObject *_wrap_axes_get_top(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((gboolean) ((NspAxes *) self)->obj->top);
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_axes_set_top(void *self, char *attr, NspObject *O)
{
  int top;

  if ( BoolScalar(O,&top) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->top = top;
  return OK;
}

static NspObject *_wrap_axes_get_elts_bounds(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspAxes *) self)->obj->elts_bounds);
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_elts_bounds(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAxes *) self)->obj->elts_bounds);
  return (NspObject *) ret;
}

static int _wrap_axes_set_elts_bounds(void *self, char *attr, NspObject *O)
{
  NspMatrix *elts_bounds;

  if ( ! IsMat(O) ) return FAIL;
  if ((elts_bounds = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAxes *) self)->obj->elts_bounds != NULL ) 
    nsp_matrix_destroy(((NspAxes *) self)->obj->elts_bounds);
  ((NspAxes *) self)->obj->elts_bounds = elts_bounds;
  return OK;
}

static NspObject *_wrap_axes_get_elts(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspList*) ((NspAxes *) self)->obj->elts);
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_elts(void *self,char *attr, int *copy)
{
  NspList *ret;

  *copy = FALSE;
  ret = ((NspList*) ((NspAxes *) self)->obj->elts);
  return (NspObject *) ret;
}

static int _wrap_axes_set_elts(void *self, char *attr, NspObject *O)
{
  NspList *elts;

  if ( ! IsList(O) ) return FAIL;
  if ((elts = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspAxes *) self)->obj->elts != NULL ) 
    nsp_list_destroy(((NspAxes *) self)->obj->elts);
  ((NspAxes *) self)->obj->elts = elts;
  return OK;
}

#line 69 "axes.override"
/* override set alpha */
static int _wrap_axes_set_alpha(void *self, char *attr, NspObject *O)
{
  double alpha;
  BCG *Xgc;
  if ( DoubleScalar(O,&alpha) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->alpha = alpha;
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->force_redraw(Xgc);
  return OK;
}


#line 696 "axes.c"
static NspObject *_wrap_axes_get_alpha(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((double) ((NspAxes *) self)->obj->alpha);
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static AttrTab axes_attrs[] = {
  { "frect", (attr_get_function *)_wrap_axes_get_frect, (attr_set_function *)_wrap_axes_set_frect,(attr_get_object_function *)_wrap_axes_get_obj_frect, (attr_set_object_function *)int_set_object_failed },
  { "wrect", (attr_get_function *)_wrap_axes_get_wrect, (attr_set_function *)_wrap_axes_set_wrect,(attr_get_object_function *)_wrap_axes_get_obj_wrect, (attr_set_object_function *)int_set_object_failed },
  { "top", (attr_get_function *)_wrap_axes_get_top, (attr_set_function *)_wrap_axes_set_top,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "elts_bounds", (attr_get_function *)_wrap_axes_get_elts_bounds, (attr_set_function *)_wrap_axes_set_elts_bounds,(attr_get_object_function *)_wrap_axes_get_obj_elts_bounds, (attr_set_object_function *)int_set_object_failed },
  { "elts", (attr_get_function *)_wrap_axes_get_elts, (attr_set_function *)_wrap_axes_set_elts,(attr_get_object_function *)_wrap_axes_get_obj_elts, (attr_set_object_function *)int_set_object_failed },
  { "alpha", (attr_get_function *)_wrap_axes_get_alpha, (attr_set_function *)_wrap_axes_set_alpha,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 43 "axes.override"
int _wrap_axes_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 733 "axes.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab axes_func[]={
  {"axes_attach", _wrap_axes_attach},
  { "axes_create", int_axes_create},
  { NULL, NULL}
};

/* call ith function in the axes interface */

int axes_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(axes_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void axes_Interf_Info(int i, char **fname, function (**f))
{
  *fname = axes_func[i].name;
  *f = axes_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
axes_register_classes(NspObject *d)
{

#line 21 "axes.override"

Init portion 


#line 772 "axes.c"
  nspgobject_register_class(d, "Axes", Axes, &NspAxes_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 84 "axes.override"

/* inserted verbatim at the end */
static void nsp_axes_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,int *aaint,int isomode,
					     int auto_axes, char *xf);

static void nsp_axes_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds);

static void nsp_draw_axes(BCG *Xgc,NspGraphic *Obj)
{
  char xf[]="onn";
  char strflag[]="151";
  double WRect[4],*wrect1,WRect1[4], FRect[4], ARect[4], inside_bounds[4];
  char logscale[2];
  int aaint[4]={10,2,10,2};
  Cell *cloc;
  NspList *L;
  NspAxes *P = (NspAxes *) Obj;
  /* draw elements */
  L = P->obj->elts;
  cloc = L->first ;
  /* we change the scale according to the axes */
  getscale2d(Xgc,WRect,FRect,logscale,ARect);
  if ( P->obj->top == TRUE ) 
    {
      double arect[4]={0.125,0.125,0.125,0.125};
      /* wrect is the window proportion to use inside the 
       * graphic window 
       */
      set_scale(Xgc,"fTffft",P->obj->wrect->R,NULL,NULL,NULL,arect);
      wrect1= P->obj->wrect->R;
    }
  else 
    {
      /* This is not a top level axes, we draw its enclosing rectangle 
       * if alpha is non nul we should draw a rotated rectangle
       */
      Xgc->graphic_engine->scale->drawrectangle(Xgc,P->obj->wrect->R);
      /* wrect is [left,up,w,h] */
      WRect1[0]= (P->obj->wrect->R[0]-FRect[0])/(FRect[2]-FRect[0]);
      WRect1[1]= 1- (P->obj->wrect->R[1]-FRect[1])/(FRect[3]-FRect[1]);
      WRect1[2]= (P->obj->wrect->R[2])/(FRect[2]-FRect[0]);
      WRect1[3]= (P->obj->wrect->R[3])/(FRect[3]-FRect[1]);
      wrect1 = WRect1;
      Xgc->scales->cosa= cos( P->obj->alpha);
      Xgc->scales->sina= sin( P->obj->alpha);
    }
  /* we directly change the default scale because we do not want 
   * to register all the scales that will be generated by set_scale 
   * thus we use T in flag[1].
   */
  /* set_scale(Xgc,"fTtfff",WRect1,P->obj->frect->R,NULL,NULL,NULL); */
  nsp_axes_compute_inside_bounds(Xgc,Obj,inside_bounds);
  nsp_axes_update_frame_bounds(Xgc,wrect1,
				   TRUE ? inside_bounds : P->obj->frect->R,
				   aaint,
				   TRUE,
				   TRUE,
				   xf);
  axis_draw(Xgc,strflag);
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G);
	}
      cloc = cloc->next;
    }
  /* scale back */
  set_scale(Xgc,"fTtfff",WRect,FRect,NULL,NULL,NULL);
  if (  P->obj->top != TRUE )
    {
      Xgc->scales->cosa=1.0;
      Xgc->scales->sina=0.0;
    }
}



/* compute the bounds of the set of objects countained in the 
 * axes 
 */




static void nsp_axes_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  double l_bounds[4];
  Cell *cloc;
  NspList *L;
  NspAxes *P = (NspAxes *) Obj;
  L = P->obj->elts;
  cloc = L->first ;
  
  if ( cloc == NULLCELL) 
    {
      bounds[0]=bounds[1]=0;
      bounds[2]=bounds[3]=0;
      return;
    }
  
  bounds[0]=bounds[1]=LARGEST_REAL;
  bounds[2]=bounds[3]=-LARGEST_REAL;

  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->bounds(Xgc,G,l_bounds);
	  if ( l_bounds[0] < bounds[0] ) 
	    bounds[0]= l_bounds[0];
	  if (  l_bounds[2] > bounds[2])
	    bounds[2]= l_bounds[2];
	  if ( l_bounds[1] < bounds[1] ) 
	    bounds[1]= l_bounds[1];
	  if (  l_bounds[3] > bounds[3])
	    bounds[3]= l_bounds[3];
	}
      cloc = cloc->next;
    }
}


void nsp_axes_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,int *aaint,int isomode,int auto_axes, char *xf)
{
  double FRect1[4];
  int Xdec[3],Ydec[3],i;
  double xmin=0.0,xmax=10.0,ymin= 0.0,ymax= 10.0;
  
  xmin=frect[0];ymin=frect[1];xmax=frect[2];ymax=frect[3];
  
  /*
   * modify computed min,max if isoview requested 
   */
  
  if ( isomode == TRUE ) 
    {
      /* code by S. Mottelet 11/7/2000 */
      double hx=xmax-xmin,hy=ymax-ymin,hx1,hy1, dwdim[2];
      double ARect[4]={0,0,0,0}; /* XXXX to be modified */
      int wdim[2];
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      dwdim[0]=linint((double)wdim[0] * (wrect[2]*(1.0-ARect[0]-ARect[1])));  /* add corrections for margins */
      dwdim[1]=linint((double)wdim[1] * (wrect[3]*(1.0-ARect[2]-ARect[3])));  /* add corrections for margins */
      if ( hx/dwdim[0] < hy/dwdim[1] ) 
	{
	  hx1=dwdim[0]*hy/dwdim[1];
	  xmin=xmin-(hx1-hx)/2.0;
	  xmax=xmax+(hx1-hx)/2.0;
	}
      else 
	{
	  hy1=dwdim[1]*hx/dwdim[0];
	  ymin=ymin-(hy1-hy)/2.0;
	  ymax=ymax+(hy1-hy)/2.0;
	}
    }
    
  /* Changing min,max and aaint if using log scaling X axis */
  if ((int)strlen(xf) >= 2 && xf[1]=='l' ) 
    {
      /* xaxis */
      if ( xmin >  0)
	{
	  xmax=ceil(log10(xmax));  xmin=floor(log10(xmin));
	}
      else 
	{
	  Scistring("Warning: Can't use Log on X-axis xmin is negative \n");
	  xmax= 1; xmin= 0;
	}
      aaint[0]=1;aaint[1]=inint(xmax-xmin);
    }

  /* Changing ymin,ymax and aaint if using log scaling Y axis */
  if ((int)strlen(xf) >=3  && xf[2]=='l' ) 
    {
      /* y axis */
      if ( ymin > 0 ) 
	{
	  ymax= ceil(log10(ymax)); ymin= floor(log10(ymin));
	}
      else 
	{
	  Scistring(" Can't use Log on y-axis ymin is negative \n");
	  ymax= 1; ymin= 0;
	}
      aaint[2]=1;aaint[3]=inint(ymax-ymin);
    }
  
  /* FRect1 gives the plotting boundaries xmin,ymin,xmax,ymax */
  FRect1[0]=xmin;FRect1[1]=ymin;FRect1[2]=xmax;FRect1[3]=ymax;
  /* interval too small */
  
  if ( Abs(FRect1[0]- FRect1[2]) < 1.e-8 ) 
    {
      FRect1[0] -= 1.e-8;
      FRect1[2] += 1.e-8;
    }
  if ( Abs(FRect1[1]- FRect1[3]) < 1.e-8 ) 
    {
      FRect1[1] -= 1.e-8;
      FRect1[3] += 1.e-8;
    }
  
  /* pretty axes */
  if ( auto_axes == TRUE ) 
    {
      double FRect2[4];
      int i;
      for (i=0; i< 4 ;i++) FRect2[i]=FRect1[i];
      /* change graduation */
      Gr_Rescale_new(&xf[1],FRect2,Xdec,Ydec,&(aaint[0]),&(aaint[2]));
    }
  
  /* Update the current scale */
  
  set_scale(Xgc,"tTtttf",wrect,FRect1,aaint,xf+1,NULL);
  
  /* store information about graduation in xtics */
  
  if ( auto_axes )
    {
      for (i=0; i < 3 ; i++ ) Xgc->scales->xtics[i] = Xdec[i];
      for (i=0; i < 3 ; i++ ) Xgc->scales->ytics[i] = Ydec[i];
      Xgc->scales->xtics[3] = aaint[1];
      Xgc->scales->ytics[3] = aaint[3];
    }
  else 
    {
      Xgc->scales->xtics[0] = xmin;
      Xgc->scales->xtics[1] = xmax;
      Xgc->scales->xtics[2] = 0.0;
      Xgc->scales->xtics[3] = aaint[1];

      Xgc->scales->ytics[0] = ymin;
      Xgc->scales->ytics[1] = ymax;
      Xgc->scales->ytics[2] = 0.0;
      Xgc->scales->ytics[3] = aaint[3];
    }
  
  /* Changing back min,max and aaint if using log scaling X axis */
  if ((int)strlen(xf) >= 2 && xf[1]=='l' ) 
    {
      FRect1[0]=exp10(xmin);FRect1[2]=exp10(xmax);
    }
  /* Changing ymin,ymax and aaint if using log scaling Y axis */
  if ((int)strlen(xf) >=3  && xf[2]=='l' )
    {
      FRect1[1]= exp10(ymin);FRect1[3]= exp10(ymax);
    }

#ifdef WITH_GTKGLEXT 
  /* transmit info to opengl */
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_2dview(Xgc);
    }
#endif
  
}


static void nsp_translate_axes(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE) return ;
  P->obj->wrect->R[0] += tr[0];
  P->obj->wrect->R[1] += tr[1];
}

static void nsp_rotate_axes(BCG *Xgc,NspGraphic *Obj,double *R)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE) return ;
  Sciprintf("we should get a double here for alpha\n");
}

static void nsp_scale_axes(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE) return ;
  P->obj->wrect->R[2] *= alpha[0];
  P->obj->wrect->R[3] *= alpha[1];
}

/* compute in bounds the enclosing rectangle of axes 
 *
 */

static void nsp_getbounds_axes(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE) return ;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  bounds[0]=P->obj->wrect->R[0];/* xmin */
  bounds[1]=P->obj->wrect->R[1]-P->obj->wrect->R[3];/* ymin */
  bounds[2]=P->obj->wrect->R[0]+P->obj->wrect->R[2];/* xmax */
  bounds[3]=P->obj->wrect->R[1];/* ymax */
}


#line 1082 "axes.c"
