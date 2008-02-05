/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>




#include "nsp/compound.h"
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_compound(BCG *Xgc,NspGraphic *Obj);

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 


/* ----------- Compound ----------- */


#define  Compound_Private 
#include "nsp/object.h"
#include "nsp/compound.h"
#include "nsp/interf.h"

/* 
 * NspCompound inherits from NspGraphic 
 */

int nsp_type_compound_id=0;
NspTypeCompound *nsp_type_compound=NULL;

/*
 * Type object for Compound 
 * all the instance of NspTypeCompound share the same id. 
 * nsp_type_compound: is an instance of NspTypeCompound 
 *    used for objects of NspCompound type (i.e built with new_compound) 
 * other instances are used for derived classes 
 */
NspTypeCompound *new_type_compound(type_mode mode)
{
  NspTypeCompound *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_compound != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_compound;
    }
  if ((type =  malloc(sizeof(NspTypeCompound))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = compound_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = compound_get_methods; 
  type->new = (new_func *) new_compound;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for compound */ 

  top->pr = (print_func *) nsp_compound_print;                  
  top->dealloc = (dealloc_func *) nsp_compound_destroy;
  top->copy  =  (copy_func *) nsp_compound_copy;                 
  top->size  = (size_func *) nsp_compound_size;                
  top->s_type =  (s_type_func *) nsp_compound_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_compound_type_short_string;
  top->info = (info_func *) nsp_compound_info ;                  
  /* top->is_true = (is_true_func  *) nsp_compound_is_true; */
  /* top->loop =(loop_func *) nsp_compound_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_compound_object;
  top->eq  = (eq_func *) nsp_compound_eq;
  top->neq  = (eq_func *) nsp_compound_neq;
  top->save  = (save_func *) nsp_compound_xdr_save;
  top->load  = (load_func *) nsp_compound_xdr_load;
  top->create = (create_func*) int_compound_create;
  top->latex = (print_func *) nsp_compound_latex;
  
  /* specific methods for compound */
      
  type->init = (init_func *) init_compound;


  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_compound;

  /* 
   * Compound interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_compound_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCompound called nsp_type_compound
       */
      type->id =  nsp_type_compound_id = nsp_new_type_id();
      nsp_type_compound = type;
      if ( nsp_register_type(nsp_type_compound) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_compound(mode);
    }
  else 
    {
       type->id = nsp_type_compound_id;
       return type;
    }
}

/*
 * initialize Compound instances 
 * locally and by calling initializer on parent class 
 */

static int init_compound(NspCompound *Obj,NspTypeCompound *type)
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
 * new instance of Compound 
 */

NspCompound *new_compound() 
{
  NspCompound *loc; 
  /* type must exists */
  nsp_type_compound = new_type_compound(T_BASE);
  if ( (loc = malloc(sizeof(NspCompound)))== NULLCOMPOUND) return loc;
  /* initialize object */
  if ( init_compound(loc,nsp_type_compound) == FAIL) return NULLCOMPOUND;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Compound 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_compound_size(NspCompound *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char compound_type_name[]="Compound";
static char compound_short_type_name[]="compound";

static char *nsp_compound_type_as_string(void)
{
  return(compound_type_name);
}

static char *nsp_compound_type_short_string(NspObject *v)
{
  return(compound_short_type_name);
}

/*
 * A == B 
 */

static int nsp_compound_eq(NspCompound *A, NspObject *B)
{
  NspCompound *loc = (NspCompound *) B;
  if ( check_cast(B,nsp_type_compound_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->frect)->type->eq(A->obj->frect,loc->obj->frect) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->wrect)->type->eq(A->obj->wrect,loc->obj->wrect) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->elts_bounds)->type->eq(A->obj->elts_bounds,loc->obj->elts_bounds) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->elts)->type->eq(A->obj->elts,loc->obj->elts) == FALSE ) return FALSE;
  if ( A->obj->alpha != loc->obj->alpha) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_compound_neq(NspCompound *A, NspObject *B)
{
  return ( nsp_compound_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_compound_xdr_save(XDR *xdrs, NspCompound *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->frect)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->wrect)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->elts_bounds)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->elts)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->alpha) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspCompound  *nsp_compound_xdr_load_partial(XDR *xdrs, NspCompound *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = malloc(sizeof(nsp_compound))) == NULL) return NULL;
  if ((M->obj->frect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->wrect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->elts_bounds =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->elts =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->alpha) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspCompound  *nsp_compound_xdr_load(XDR *xdrs)
{
  NspCompound *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCOMPOUND;
  if ((M  = nsp_compound_create_void(name,(NspTypeBase *) nsp_type_compound))== NULLCOMPOUND) return M;
  return nsp_compound_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_compound_destroy_partial(NspCompound *H)
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

void nsp_compound_destroy(NspCompound *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_compound_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

void nsp_compound_info(NspCompound *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCOMPOUND) 
    {
      Sciprintf("Null Pointer Compound \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_compound_type_short_string(NSP_OBJECT(M)))
;}

/*
 * print 
 */

void nsp_compound_print(NspCompound *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCOMPOUND) 
    {
      Sciprintf("Null Pointer Compound \n");
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
          nsp_compound_info(M,indent,pname,rec_level);
          return;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_compound_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
        if ( M->obj->frect != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1);
  if ( M->obj->wrect != NULL)
    nsp_object_print(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1);
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

void nsp_compound_latex(NspCompound *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_compound_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
    if ( M->obj->frect != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1);
  if ( M->obj->wrect != NULL)
    nsp_object_latex(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1);
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
 * for Compound objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspCompound   *nsp_compound_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_compound_id) == TRUE ) return ((NspCompound *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_compound));
  return NULL;
}

int IsCompoundObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_compound_id);
}

int IsCompound(NspObject *O)
{
  return nsp_object_type(O,nsp_type_compound_id);
}

NspCompound  *GetCompoundCopy(Stack stack, int i)
{
  if (  GetCompound(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspCompound  *GetCompound(Stack stack, int i)
{
  NspCompound *M;
  if (( M = nsp_compound_object(NthObj(i))) == NULLCOMPOUND)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspCompound *nsp_compound_create_void(char *name,NspTypeBase *type)
{
 NspCompound *H  = (type == NULL) ? new_compound() : type->new();
 if ( H ==  NULLCOMPOUND)
  {
   Sciprintf("No more memory\n");
   return NULLCOMPOUND;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCOMPOUND;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_compound_create_partial(NspCompound *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_compound)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  return OK;
}

int nsp_compound_check_values(NspCompound *H)
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

NspCompound *nsp_compound_create(char *name,NspMatrix* frect,NspMatrix* wrect,NspMatrix* elts_bounds,NspList* elts,double alpha,NspTypeBase *type)
{
 NspCompound *H  = nsp_compound_create_void(name,type);
 if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_compound_create_partial(H) == FAIL) return NULLCOMPOUND;
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
 if ( nsp_compound_check_values(H) == FAIL) return NULLCOMPOUND;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspCompound *nsp_compound_copy_partial(NspCompound *H,NspCompound *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspCompound *nsp_compound_copy(NspCompound *self)
{
  NspCompound *H  =nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound);
  if ( H ==  NULLCOMPOUND) return NULLCOMPOUND;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCOMPOUND;
  if ( nsp_compound_copy_partial(H,self)== NULL) return NULLCOMPOUND;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Compound
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_compound_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCompound *H;
  CheckStdRhs(0,0);
  /* want to be sure that type compound is initialized */
  nsp_type_compound = new_type_compound(T_BASE);
  if(( H = nsp_compound_create_void(NVOID,(NspTypeBase *) nsp_type_compound)) == NULLCOMPOUND) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_compound_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_compound_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *compound_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_compound_get_frect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->frect);
  return (NspObject *) ret;
}

static NspObject *_wrap_compound_get_frect_obj(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->frect);
  return (NspObject *) ret;
}

static int _wrap_compound_set_frect(void *self, char *attr, NspObject *O)
{
  NspMatrix *frect;

  if ( ! IsMat(O) ) return FAIL;
  if ((frect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspCompound *) self)->obj->frect != NULL ) 
    nsp_matrix_destroy(((NspCompound *) self)->obj->frect);
  ((NspCompound *) self)->obj->frect = frect;
  return OK;
}

static NspObject *_wrap_compound_get_wrect(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->wrect);
  return (NspObject *) ret;
}

static NspObject *_wrap_compound_get_wrect_obj(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->wrect);
  return (NspObject *) ret;
}

static int _wrap_compound_set_wrect(void *self, char *attr, NspObject *O)
{
  NspMatrix *wrect;

  if ( ! IsMat(O) ) return FAIL;
  if ((wrect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspCompound *) self)->obj->wrect != NULL ) 
    nsp_matrix_destroy(((NspCompound *) self)->obj->wrect);
  ((NspCompound *) self)->obj->wrect = wrect;
  return OK;
}

static NspObject *_wrap_compound_get_elts_bounds(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->elts_bounds);
  return (NspObject *) ret;
}

static NspObject *_wrap_compound_get_elts_bounds_obj(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspMatrix*) ((NspCompound *) self)->obj->elts_bounds);
  return (NspObject *) ret;
}

static int _wrap_compound_set_elts_bounds(void *self, char *attr, NspObject *O)
{
  NspMatrix *elts_bounds;

  if ( ! IsMat(O) ) return FAIL;
  if ((elts_bounds = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspCompound *) self)->obj->elts_bounds != NULL ) 
    nsp_matrix_destroy(((NspCompound *) self)->obj->elts_bounds);
  ((NspCompound *) self)->obj->elts_bounds = elts_bounds;
  return OK;
}

static NspObject *_wrap_compound_get_elts(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspList*) ((NspCompound *) self)->obj->elts);
  return (NspObject *) ret;
}

static NspObject *_wrap_compound_get_elts_obj(void *self,char *attr)
{
  NspList *ret;

  ret = ((NspList*) ((NspCompound *) self)->obj->elts);
  return (NspObject *) ret;
}

static int _wrap_compound_set_elts(void *self, char *attr, NspObject *O)
{
  NspList *elts;

  if ( ! IsList(O) ) return FAIL;
  if ((elts = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspCompound *) self)->obj->elts != NULL ) 
    nsp_list_destroy(((NspCompound *) self)->obj->elts);
  ((NspCompound *) self)->obj->elts = elts;
  return OK;
}

static NspObject *_wrap_compound_get_alpha(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((double) ((NspCompound *) self)->obj->alpha);
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_compound_set_alpha(void *self, char *attr, NspObject *O)
{
  double alpha;
  BCG *Xgc;
  if ( DoubleScalar(O,&alpha) == FAIL) return FAIL;
  ((NspCompound *) self)->obj->alpha = alpha;
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->force_redraw(Xgc);
  return OK;
}

static AttrTab compound_attrs[] = {
  { "frect", (attr_get_function *)_wrap_compound_get_frect, (attr_set_function *)_wrap_compound_set_frect,(attr_get_object_function *)_wrap_compound_get_frect_obj },
  { "wrect", (attr_get_function *)_wrap_compound_get_wrect, (attr_set_function *)_wrap_compound_set_wrect,(attr_get_object_function *)_wrap_compound_get_wrect_obj },
  { "elts_bounds", (attr_get_function *)_wrap_compound_get_elts_bounds, (attr_set_function *)_wrap_compound_set_elts_bounds,(attr_get_object_function *)_wrap_compound_get_elts_bounds_obj },
  { "elts", (attr_get_function *)_wrap_compound_get_elts, (attr_set_function *)_wrap_compound_set_elts,(attr_get_object_function *)_wrap_compound_get_elts_obj },
  { "alpha", (attr_get_function *)_wrap_compound_get_alpha, (attr_set_function *)_wrap_compound_set_alpha,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_compound_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}



/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Compound_func[]={
  {"compound_attach", _wrap_compound_attach},
  { "compound_create", int_compound_create},
  { NULL, NULL}
};

/* call ith function in the Compound interface */

int Compound_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Compound_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Compound_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Compound_func[i].name;
  *f = Compound_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Compound_register_classes(NspObject *d)
{


Init portion 



  nspgobject_register_class(d, "Compound", Compound, &NspCompound_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/


/* inserted verbatim at the end */

static void nsp_compound_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,int *aaint,int isomode,
					     int auto_axes, char *xf);

static void nsp_compound_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds);

static void nsp_draw_compound(BCG *Xgc,NspGraphic *Obj)
{
  char xf[]="onn";
  char strflag[]="151";
  double WRect[4],WRect1[4], FRect[4], ARect[4], inside_bounds[4];
  char logscale[2];
  int aaint[4]={10,2,10,2};
  Cell *cloc;
  NspList *L;
  NspCompound *P = (NspCompound *) Obj;
  Xgc->graphic_engine->scale->drawrectangle(Xgc,P->obj->wrect->R);
  /* draw elements */
  L = P->obj->elts;
  cloc = L->first ;
  /* we change the scale according to the compound */
  getscale2d(Xgc,WRect,FRect,logscale,ARect);
  /* wrect is [left,up,w,h] */
  WRect1[0]= (P->obj->wrect->R[0]-FRect[0])/(FRect[2]-FRect[0]);
  WRect1[1]= 1- (P->obj->wrect->R[1]-FRect[1])/(FRect[3]-FRect[1]);
  WRect1[2]= (P->obj->wrect->R[2])/(FRect[2]-FRect[0]);
  WRect1[3]= (P->obj->wrect->R[3])/(FRect[3]-FRect[1]);
  Xgc->scales->cosa= cos( P->obj->alpha);
  Xgc->scales->sina= sin( P->obj->alpha);

  /* we directly change the default scale because we do not want 
   * to register all the scales that will be generated by set_scale 
   * thus we use T in flag[1].
   */
  /* set_scale(Xgc,"fTtfff",WRect1,P->obj->frect->R,NULL,NULL,NULL); */
  nsp_compound_compute_inside_bounds(Xgc,Obj,inside_bounds);
  nsp_compound_update_frame_bounds(Xgc,WRect1,
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
  Xgc->scales->cosa=1.0;
  Xgc->scales->sina=0.0;
}

/* compute the bounds of the set of objects countained in the 
 * compound 
 */

static void nsp_compound_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  double l_bounds[4];
  Cell *cloc;
  NspList *L;
  NspCompound *P = (NspCompound *) Obj;
  L = P->obj->elts;
  cloc = L->first ;
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
	  else if (  l_bounds[2] > bounds[2])
	    bounds[2]= l_bounds[2];
	  if ( l_bounds[1] < bounds[1] ) 
	    bounds[1]= l_bounds[1];
	  else if (  l_bounds[3] > bounds[3])
	    bounds[3]= l_bounds[3];
	}
      cloc = cloc->next;
    }
}

void nsp_compound_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,int *aaint,int isomode,int auto_axes, char *xf)
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


/*
  
  */


