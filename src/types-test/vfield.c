/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/vfield.override"
#include <nsp/axes.h>
#include <nsp/figure.h> 
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_vfield(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_vfield(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_vfield(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_vfield(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_vfield(BCG *Xgc,NspGraphic *o,double *bounds);

extern void nsp_figure_force_redraw(nsp_figure *F);

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 29 "vfield.c"

/* ----------- NspVField ----------- */


#define  NspVField_Private 
#include "nsp/object.h"
#include "nsp/vfield.h"
#include "nsp/interf.h"

/* 
 * NspVField inherits from Graphic 
 */

int nsp_type_vfield_id=0;
NspTypeNspVField *nsp_type_vfield=NULL;

/*
 * Type object for NspVField 
 * all the instance of NspTypeNspVField share the same id. 
 * nsp_type_vfield: is an instance of NspTypeNspVField 
 *    used for objects of NspVField type (i.e built with new_vfield) 
 * other instances are used for derived classes 
 */
NspTypeNspVField *new_type_vfield(type_mode mode)
{
  NspTypeNspVField *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_vfield != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_vfield;
    }
  if ((type =  malloc(sizeof(NspTypeNspVField))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = vfield_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = vfield_get_methods; 
  type->new = (new_func *) new_vfield;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for vfield */ 

  top->pr = (print_func *) nsp_vfield_print;                  
  top->dealloc = (dealloc_func *) nsp_vfield_destroy;
  top->copy  =  (copy_func *) nsp_vfield_copy;                 
  top->size  = (size_func *) nsp_vfield_size;                
  top->s_type =  (s_type_func *) nsp_vfield_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_vfield_type_short_string;
  top->info = (info_func *) nsp_vfield_info ;                  
  /* top->is_true = (is_true_func  *) nsp_vfield_is_true; */
  /* top->loop =(loop_func *) nsp_vfield_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_vfield_object;
  top->eq  = (eq_func *) nsp_vfield_eq;
  top->neq  = (eq_func *) nsp_vfield_neq;
  top->save  = (save_func *) nsp_vfield_xdr_save;
  top->load  = (load_func *) nsp_vfield_xdr_load;
  top->create = (create_func*) int_vfield_create;
  top->latex = (print_func *) nsp_vfield_latex;
  
  /* specific methods for vfield */
      
  type->init = (init_func *) init_vfield;

#line 27 "codegen/vfield.override"
  /* inserted verbatim in the type definition */
  ((NspTypeNspGraphic *) type->surtype)->draw = nsp_draw_vfield;
  ((NspTypeNspGraphic *) type->surtype)->translate =nsp_translate_vfield ;
  ((NspTypeNspGraphic *) type->surtype)->rotate =nsp_rotate_vfield  ;
  ((NspTypeNspGraphic *) type->surtype)->scale =nsp_scale_vfield  ;
  ((NspTypeNspGraphic *) type->surtype)->bounds =nsp_getbounds_vfield  ;
  /* next method are defined in NspGraphic and need not be chnaged here for GMatrix */
  /* ((NspTypeNspGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeNspGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 111 "vfield.c"
  /* 
   * NspVField interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_vfield_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeNspVField called nsp_type_vfield
       */
      type->id =  nsp_type_vfield_id = nsp_new_type_id();
      nsp_type_vfield = type;
      if ( nsp_register_type(nsp_type_vfield) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_vfield(mode);
    }
  else 
    {
       type->id = nsp_type_vfield_id;
       return type;
    }
}

/*
 * initialize NspVField instances 
 * locally and by calling initializer on parent class 
 */

static int init_vfield(NspVField *Obj,NspTypeNspVField *type)
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
 * new instance of NspVField 
 */

NspVField *new_vfield() 
{
  NspVField *loc; 
  /* type must exists */
  nsp_type_vfield = new_type_vfield(T_BASE);
  if ( (loc = malloc(sizeof(NspVField)))== NULLVFIELD) return loc;
  /* initialize object */
  if ( init_vfield(loc,nsp_type_vfield) == FAIL) return NULLVFIELD;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspVField 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_vfield_size(NspVField *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char vfield_type_name[]="NspVField";
static char vfield_short_type_name[]="vfield";

static char *nsp_vfield_type_as_string(void)
{
  return(vfield_type_name);
}

static char *nsp_vfield_type_short_string(NspObject *v)
{
  return(vfield_short_type_name);
}

/*
 * A == B 
 */

static int nsp_vfield_eq(NspVField *A, NspObject *B)
{
  NspVField *loc = (NspVField *) B;
  if ( check_cast(B,nsp_type_vfield_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->fx)->type->eq(A->obj->fx,loc->obj->fx) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->fy)->type->eq(A->obj->fy,loc->obj->fy) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( A->obj->colored != loc->obj->colored) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_vfield_neq(NspVField *A, NspObject *B)
{
  return ( nsp_vfield_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_vfield_xdr_save(XDR *xdrs, NspVField *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
   if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_vfield)) == FAIL) return FAIL; 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->fx)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->fy)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->colored) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspVField  *nsp_vfield_xdr_load_partial(XDR *xdrs, NspVField *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_vfield))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->fx =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->fy =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->colored) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspVField  *nsp_vfield_xdr_load(XDR *xdrs)
{
  NspVField *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLVFIELD;
  if ((H  = nsp_vfield_create_void(name,(NspTypeBase *) nsp_type_vfield))== NULLVFIELD) return H;
  if ((H  = nsp_vfield_xdr_load_partial(xdrs,H))== NULLVFIELD) return H;
  if ( nsp_vfield_check_values(H) == FAIL) return NULLVFIELD;
#line 275 "vfield.c"
  return H;
}

/*
 * delete 
 */

void nsp_vfield_destroy_partial(NspVField *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 289 "vfield.c"
    nsp_matrix_destroy(H->obj->fx);
    nsp_matrix_destroy(H->obj->fy);
    nsp_matrix_destroy(H->obj->x);
    nsp_matrix_destroy(H->obj->y);
    FREE(H->obj);
   }
}

void nsp_vfield_destroy(NspVField *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_vfield_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_vfield_info(NspVField *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLVFIELD) 
    {
      Sciprintf("Null Pointer NspVField \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_vfield_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_vfield_print(NspVField *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLVFIELD) 
    {
      Sciprintf("Null Pointer NspVField \n");
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
          nsp_vfield_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_vfield_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->fx != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->fx),indent+2,"fx",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->fy != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->fy),indent+2,"fy",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"colored	= %s\n", ( M->obj->colored == TRUE) ? "T" : "F" );
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_vfield_latex(NspVField *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_vfield_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->fx != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->fx),indent+2,"fx",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->fy != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->fy),indent+2,"fy",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"colored	= %s\n", ( M->obj->colored == TRUE) ? "T" : "F" );
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspVField objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspVField   *nsp_vfield_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_vfield_id) == TRUE ) return ((NspVField *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_vfield));
  return NULL;
}

int IsVFieldObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_vfield_id);
}

int IsVField(NspObject *O)
{
  return nsp_object_type(O,nsp_type_vfield_id);
}

NspVField  *GetVFieldCopy(Stack stack, int i)
{
  if (  GetVField(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspVField  *GetVField(Stack stack, int i)
{
  NspVField *M;
  if (( M = nsp_vfield_object(NthObj(i))) == NULLVFIELD)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspVField *nsp_vfield_create_void(char *name,NspTypeBase *type)
{
 NspVField *H  = (type == NULL) ? new_vfield() : type->new();
 if ( H ==  NULLVFIELD)
  {
   Sciprintf("No more memory\n");
   return NULLVFIELD;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLVFIELD;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_vfield_create_partial(NspVField *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_vfield)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->fx = NULLMAT;
  H->obj->fy = NULLMAT;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->colored = TRUE;
  return OK;
}

int nsp_vfield_check_values(NspVField *H)
{
  if ( H->obj->fx == NULLMAT) 
    {
       if (( H->obj->fx = nsp_matrix_create("fx",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->fy == NULLMAT) 
    {
       if (( H->obj->fy = nsp_matrix_create("fy",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->x == NULLMAT) 
    {
       if (( H->obj->x = nsp_matrix_create("x",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->y == NULLMAT) 
    {
       if (( H->obj->y = nsp_matrix_create("y",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspVField *nsp_vfield_create(char *name,NspMatrix* fx,NspMatrix* fy,NspMatrix* x,NspMatrix* y,gboolean colored,NspTypeBase *type)
{
 NspVField *H  = nsp_vfield_create_void(name,type);
 if ( H ==  NULLVFIELD) return NULLVFIELD;
  if ( nsp_vfield_create_partial(H) == FAIL) return NULLVFIELD;
  H->obj->fx= fx;
  H->obj->fy= fy;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->colored=colored;
 if ( nsp_vfield_check_values(H) == FAIL) return NULLVFIELD;
 return H;
}


NspVField *nsp_vfield_create_default(char *name)
{
 NspVField *H  = nsp_vfield_create_void(name,NULL);
 if ( H ==  NULLVFIELD) return NULLVFIELD;
  if ( nsp_vfield_create_partial(H) == FAIL) return NULLVFIELD;
 if ( nsp_vfield_check_values(H) == FAIL) return NULLVFIELD;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspVField *nsp_vfield_copy_partial(NspVField *H,NspVField *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspVField *nsp_vfield_copy(NspVField *self)
{
  NspVField *H  =nsp_vfield_create_void(NVOID,(NspTypeBase *) nsp_type_vfield);
  if ( H ==  NULLVFIELD) return NULLVFIELD;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLVFIELD;
  if ( nsp_vfield_copy_partial(H,self)== NULL) return NULLVFIELD;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspVField *nsp_vfield_full_copy_partial(NspVField *H,NspVField *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_vfield))) == NULL) return NULLVFIELD;
  H->obj->ref_count=1;
  if ( self->obj->fx == NULL )
    { H->obj->fx = NULL;}
  else
    {
      if ((H->obj->fx = (NspMatrix *) nsp_object_copy_and_name("fx",NSP_OBJECT(self->obj->fx))) == NULLMAT) return NULL;
    }
  if ( self->obj->fy == NULL )
    { H->obj->fy = NULL;}
  else
    {
      if ((H->obj->fy = (NspMatrix *) nsp_object_copy_and_name("fy",NSP_OBJECT(self->obj->fy))) == NULLMAT) return NULL;
    }
  if ( self->obj->x == NULL )
    { H->obj->x = NULL;}
  else
    {
      if ((H->obj->x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(self->obj->x))) == NULLMAT) return NULL;
    }
  if ( self->obj->y == NULL )
    { H->obj->y = NULL;}
  else
    {
      if ((H->obj->y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(self->obj->y))) == NULLMAT) return NULL;
    }
  H->obj->colored=self->obj->colored;
  return H;
}

NspVField *nsp_vfield_full_copy(NspVField *self)
{
  NspVField *H  =nsp_vfield_create_void(NVOID,(NspTypeBase *) nsp_type_vfield);
  if ( H ==  NULLVFIELD) return NULLVFIELD;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLVFIELD;
  if ( nsp_vfield_full_copy_partial(H,self)== NULL) return NULLVFIELD;
#line 583 "vfield.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspVField
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_vfield_create(Stack stack, int rhs, int opt, int lhs)
{
  NspVField *H;
  CheckStdRhs(0,0);
  /* want to be sure that type vfield is initialized */
  nsp_type_vfield = new_type_vfield(T_BASE);
  if(( H = nsp_vfield_create_void(NVOID,(NspTypeBase *) nsp_type_vfield)) == NULLVFIELD) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_vfield_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_vfield_check_values(H) == FAIL) return RET_BUG;
#line 603 "vfield.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *vfield_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_vfield_get_fx(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspVField *) self)->obj->fx;
  return (NspObject *) ret;
}

static NspObject *_wrap_vfield_get_obj_fx(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspVField *) self)->obj->fx);
  return (NspObject *) ret;
}

static int _wrap_vfield_set_fx(void *self, char *attr, NspObject *O)
{
  NspMatrix *fx;

  if ( ! IsMat(O) ) return FAIL;
  if ((fx = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspVField *) self)->obj->fx != NULL ) 
    nsp_matrix_destroy(((NspVField *) self)->obj->fx);
  ((NspVField *) self)->obj->fx= fx;
  return OK;
}

static NspObject *_wrap_vfield_get_fy(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspVField *) self)->obj->fy;
  return (NspObject *) ret;
}

static NspObject *_wrap_vfield_get_obj_fy(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspVField *) self)->obj->fy);
  return (NspObject *) ret;
}

static int _wrap_vfield_set_fy(void *self, char *attr, NspObject *O)
{
  NspMatrix *fy;

  if ( ! IsMat(O) ) return FAIL;
  if ((fy = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspVField *) self)->obj->fy != NULL ) 
    nsp_matrix_destroy(((NspVField *) self)->obj->fy);
  ((NspVField *) self)->obj->fy= fy;
  return OK;
}

static NspObject *_wrap_vfield_get_x(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspVField *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_vfield_get_obj_x(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspVField *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_vfield_set_x(void *self, char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspVField *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspVField *) self)->obj->x);
  ((NspVField *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_vfield_get_y(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspVField *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_vfield_get_obj_y(void *self,char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspVField *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_vfield_set_y(void *self, char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspVField *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspVField *) self)->obj->y);
  ((NspVField *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_vfield_get_colored(void *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspVField *) self)->obj->colored;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_vfield_set_colored(void *self, char *attr, NspObject *O)
{
  int colored;

  if ( BoolScalar(O,&colored) == FAIL) return FAIL;
  ((NspVField *) self)->obj->colored= colored;
  return OK;
}

static AttrTab vfield_attrs[] = {
  { "fx", (attr_get_function *)_wrap_vfield_get_fx, (attr_set_function *)_wrap_vfield_set_fx,(attr_get_object_function *)_wrap_vfield_get_obj_fx, (attr_set_object_function *)int_set_object_failed },
  { "fy", (attr_get_function *)_wrap_vfield_get_fy, (attr_set_function *)_wrap_vfield_set_fy,(attr_get_object_function *)_wrap_vfield_get_obj_fy, (attr_set_object_function *)int_set_object_failed },
  { "x", (attr_get_function *)_wrap_vfield_get_x, (attr_set_function *)_wrap_vfield_set_x,(attr_get_object_function *)_wrap_vfield_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_vfield_get_y, (attr_set_function *)_wrap_vfield_set_y,(attr_get_object_function *)_wrap_vfield_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "colored", (attr_get_function *)_wrap_vfield_get_colored, (attr_set_function *)_wrap_vfield_set_colored,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 48 "codegen/vfield.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_vfield(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 770 "vfield.c"


#line 58 "codegen/vfield.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_vfield(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 782 "vfield.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab VField_func[]={
  {"extractelts_vfield", _wrap_nsp_extractelts_vfield},
  {"setrowscols_vfield", _wrap_nsp_setrowscols_vfield},
  { "vfield_create", int_vfield_create},
  { NULL, NULL}
};

/* call ith function in the VField interface */

int VField_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(VField_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void VField_Interf_Info(int i, char **fname, function (**f))
{
  *fname = VField_func[i].name;
  *f = VField_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
VField_register_classes(NspObject *d)
{

#line 22 "codegen/vfield.override"

Init portion 


#line 822 "vfield.c"
  nspgobject_register_class(d, "NspVField", VField, &NspNspVField_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 68 "codegen/vfield.override"

static void nsp_draw_vfield(BCG *Xgc,NspGraphic *Obj, void *data)
{
  double arfact = 1.0;
  NspVField *P = (NspVField *) Obj;
  double *x= P->obj->x->R; 
  double *y= P->obj->y->R; 
  double *fx= P->obj->fx->R; 
  double *fy= P->obj->fy->R; 
  int n1 = P->obj->x->mn;
  int n2 = P->obj->y->mn;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  if ( P->obj->x->mn  == 0 || P->obj->y->mn  == 0 ) return;
  nsp_draw_vfield_generic(Xgc,"champ",P->obj->colored,x,y,fx,fy,n1,n2,TRUE,NULL,NULL,&arfact);
}


static void nsp_translate_vfield(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  NspVField *P = (NspVField *) Obj;
  int i;
  for ( i = 0 ; i < P->obj->x->mn ; i++) 
    P->obj->x->R[i] += tr[0];
  for ( i = 0 ; i < P->obj->y->mn ; i++) 
    P->obj->x->R[i] += tr[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_rotate_vfield(BCG *Xgc,NspGraphic *Obj,double *R)
{
  /* NspVField *P = (NspVField *) Obj; */
  Sciprintf("we should get a double here for alpha\n");
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_vfield(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  int i;
  NspVField *P = (NspVField *) Obj;
  for ( i = 0 ; i < P->obj->x->mn ; i++) 
    P->obj->x->R[i] *= alpha[0];
  for ( i = 0 ; i < P->obj->y->mn ; i++) 
    P->obj->x->R[i] *= alpha[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of vfield 
 *
 */

static void nsp_getbounds_vfield (BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspVField *P = (NspVField *) Obj;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  if ( P->obj->x->mn != 0 ) 
    {
      bounds[0]=P->obj->x->R[0]; /* xmin */
      bounds[2]=P->obj->x->R[P->obj->x->mn-1];/* xmax */
    }
  if ( P->obj->y->mn != 0 ) 
    {
      bounds[1]=P->obj->y->R[0] ; /* ymin */
      bounds[3]=P->obj->y->R[P->obj->y->mn-1];/* ymax */
    }
}


#line 895 "vfield.c"
