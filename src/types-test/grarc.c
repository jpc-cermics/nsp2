/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "codegen/grarc.override"
#include "nsp/grarc.h"
#include <nsp/figure.h> 
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_grarc(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_grarc(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_grarc(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_grarc(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_grarc(BCG *Xgc,NspGraphic *o,double *bounds);

extern void nsp_figure_force_redraw(nsp_figure *F);

#line 25 "grarc.c"

/* ----------- GrArc ----------- */


#define  GrArc_Private 
#include "nsp/object.h"
#include "nsp/grarc.h"
#include "nsp/interf.h"

/* 
 * NspGrArc inherits from NspGraphic 
 */

int nsp_type_grarc_id=0;
NspTypeGrArc *nsp_type_grarc=NULL;

/*
 * Type object for GrArc 
 * all the instance of NspTypeGrArc share the same id. 
 * nsp_type_grarc: is an instance of NspTypeGrArc 
 *    used for objects of NspGrArc type (i.e built with new_grarc) 
 * other instances are used for derived classes 
 */
NspTypeGrArc *new_type_grarc(type_mode mode)
{
  NspTypeGrArc *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_grarc != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_grarc;
    }
  if ((type =  malloc(sizeof(NspTypeGrArc))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = grarc_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = grarc_get_methods; 
  type->new = (new_func *) new_grarc;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for grarc */ 

  top->pr = (print_func *) nsp_grarc_print;                  
  top->dealloc = (dealloc_func *) nsp_grarc_destroy;
  top->copy  =  (copy_func *) nsp_grarc_copy;                 
  top->size  = (size_func *) nsp_grarc_size;                
  top->s_type =  (s_type_func *) nsp_grarc_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_grarc_type_short_string;
  top->info = (info_func *) nsp_grarc_info ;                  
  /* top->is_true = (is_true_func  *) nsp_grarc_is_true; */
  /* top->loop =(loop_func *) nsp_grarc_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_grarc_object;
  top->eq  = (eq_func *) nsp_grarc_eq;
  top->neq  = (eq_func *) nsp_grarc_neq;
  top->save  = (save_func *) nsp_grarc_xdr_save;
  top->load  = (load_func *) nsp_grarc_xdr_load;
  top->create = (create_func*) int_grarc_create;
  top->latex = (print_func *) nsp_grarc_latex;
  
  /* specific methods for grarc */
      
  type->init = (init_func *) init_grarc;

#line 23 "codegen/grarc.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_grarc;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_grarc ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_grarc  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_grarc  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_grarc  ;
  ((NspTypeGraphic *) type->surtype)->full_copy = (full_copy_func *) nsp_grarc_full_copy ;
  /* next method are defined in NspGraphic and need not be chnaged here for GrArc */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 110 "grarc.c"
  /* 
   * GrArc interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_grarc_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGrArc called nsp_type_grarc
       */
      type->id =  nsp_type_grarc_id = nsp_new_type_id();
      nsp_type_grarc = type;
      if ( nsp_register_type(nsp_type_grarc) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_grarc(mode);
    }
  else 
    {
       type->id = nsp_type_grarc_id;
       return type;
    }
}

/*
 * initialize GrArc instances 
 * locally and by calling initializer on parent class 
 */

static int init_grarc(NspGrArc *Obj,NspTypeGrArc *type)
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
 * new instance of GrArc 
 */

NspGrArc *new_grarc() 
{
  NspGrArc *loc; 
  /* type must exists */
  nsp_type_grarc = new_type_grarc(T_BASE);
  if ( (loc = malloc(sizeof(NspGrArc)))== NULLGRARC) return loc;
  /* initialize object */
  if ( init_grarc(loc,nsp_type_grarc) == FAIL) return NULLGRARC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GrArc 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_grarc_size(NspGrArc *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char grarc_type_name[]="GrArc";
static char grarc_short_type_name[]="grarc";

static char *nsp_grarc_type_as_string(void)
{
  return(grarc_type_name);
}

static char *nsp_grarc_type_short_string(NspObject *v)
{
  return(grarc_short_type_name);
}

/*
 * A == B 
 */

static int nsp_grarc_eq(NspGrArc *A, NspObject *B)
{
  NspGrArc *loc = (NspGrArc *) B;
  if ( check_cast(B,nsp_type_grarc_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->x != loc->obj->x) return FALSE;
  if ( A->obj->y != loc->obj->y) return FALSE;
  if ( A->obj->w != loc->obj->w) return FALSE;
  if ( A->obj->h != loc->obj->h) return FALSE;
  if ( A->obj->a1 != loc->obj->a1) return FALSE;
  if ( A->obj->a2 != loc->obj->a2) return FALSE;
  if ( A->obj->fill_color != loc->obj->fill_color) return FALSE;
  if ( A->obj->thickness != loc->obj->thickness) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_grarc_neq(NspGrArc *A, NspObject *B)
{
  return ( nsp_grarc_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_grarc_xdr_save(XDR *xdrs, NspGrArc *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
   if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_grarc)) == FAIL) return FAIL; 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->x) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->y) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->w) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->h) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->a1) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->a2) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->fill_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->thickness) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGrArc  *nsp_grarc_xdr_load_partial(XDR *xdrs, NspGrArc *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_grarc))) == NULL) return NULL;
  M->obj->ref_count=1;
  if (nsp_xdr_load_d(xdrs, &M->obj->x) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->y) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->w) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->h) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->a1) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->a2) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->fill_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->thickness) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspGrArc  *nsp_grarc_xdr_load(XDR *xdrs)
{
  NspGrArc *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRARC;
  if ((H  = nsp_grarc_create_void(name,(NspTypeBase *) nsp_type_grarc))== NULLGRARC) return H;
  if ((H  = nsp_grarc_xdr_load_partial(xdrs,H))== NULLGRARC) return H;
  if ( nsp_grarc_check_values(H) == FAIL) return NULLGRARC;
#line 286 "grarc.c"
  return H;
}

/*
 * delete 
 */

void nsp_grarc_destroy_partial(NspGrArc *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 300 "grarc.c"
    FREE(H->obj);
   }
}

void nsp_grarc_destroy(NspGrArc *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_grarc_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_grarc_info(NspGrArc *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRARC) 
    {
      Sciprintf("Null Pointer GrArc \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_grarc_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_grarc_print(NspGrArc *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRARC) 
    {
      Sciprintf("Null Pointer GrArc \n");
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
          nsp_grarc_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_grarc_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n",M->obj->x);
  Sciprintf1(indent+2,"y=%f\n",M->obj->y);
  Sciprintf1(indent+2,"w=%f\n",M->obj->w);
  Sciprintf1(indent+2,"h=%f\n",M->obj->h);
  Sciprintf1(indent+2,"a1=%f\n",M->obj->a1);
  Sciprintf1(indent+2,"a2=%f\n",M->obj->a2);
  Sciprintf1(indent+2,"fill_color=%d\n",M->obj->fill_color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_grarc_latex(NspGrArc *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_grarc_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n",M->obj->x);
  Sciprintf1(indent+2,"y=%f\n",M->obj->y);
  Sciprintf1(indent+2,"w=%f\n",M->obj->w);
  Sciprintf1(indent+2,"h=%f\n",M->obj->h);
  Sciprintf1(indent+2,"a1=%f\n",M->obj->a1);
  Sciprintf1(indent+2,"a2=%f\n",M->obj->a2);
  Sciprintf1(indent+2,"fill_color=%d\n",M->obj->fill_color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GrArc objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspGrArc   *nsp_grarc_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_grarc_id) == TRUE ) return ((NspGrArc *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_grarc));
  return NULL;
}

int IsGrArcObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_grarc_id);
}

int IsGrArc(NspObject *O)
{
  return nsp_object_type(O,nsp_type_grarc_id);
}

NspGrArc  *GetGrArcCopy(Stack stack, int i)
{
  if (  GetGrArc(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGrArc  *GetGrArc(Stack stack, int i)
{
  NspGrArc *M;
  if (( M = nsp_grarc_object(NthObj(i))) == NULLGRARC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspGrArc *nsp_grarc_create_void(char *name,NspTypeBase *type)
{
 NspGrArc *H  = (type == NULL) ? new_grarc() : type->new();
 if ( H ==  NULLGRARC)
  {
   Sciprintf("No more memory\n");
   return NULLGRARC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRARC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_grarc_create_partial(NspGrArc *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_grarc)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = 0.0;
  H->obj->y = 0.0;
  H->obj->w = 0.0;
  H->obj->h = 0.0;
  H->obj->a1 = 0.0;
  H->obj->a2 = 0.0;
  H->obj->fill_color = -1;
  H->obj->thickness = 0;
  H->obj->color = 0;
  return OK;
}

int nsp_grarc_check_values(NspGrArc *H)
{
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspGrArc *nsp_grarc_create(char *name,double x,double y,double w,double h,double a1,double a2,int fill_color,int thickness,int color,NspTypeBase *type)
{
 NspGrArc *H  = nsp_grarc_create_void(name,type);
 if ( H ==  NULLGRARC) return NULLGRARC;
  if ( nsp_grarc_create_partial(H) == FAIL) return NULLGRARC;
  H->obj->x=x;
  H->obj->y=y;
  H->obj->w=w;
  H->obj->h=h;
  H->obj->a1=a1;
  H->obj->a2=a2;
  H->obj->fill_color=fill_color;
  H->obj->thickness=thickness;
  H->obj->color=color;
 if ( nsp_grarc_check_values(H) == FAIL) return NULLGRARC;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGrArc *nsp_grarc_copy_partial(NspGrArc *H,NspGrArc *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGrArc *nsp_grarc_copy(NspGrArc *self)
{
  NspGrArc *H  =nsp_grarc_create_void(NVOID,(NspTypeBase *) nsp_type_grarc);
  if ( H ==  NULLGRARC) return NULLGRARC;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRARC;
  if ( nsp_grarc_copy_partial(H,self)== NULL) return NULLGRARC;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspGrArc *nsp_grarc_full_copy_partial(NspGrArc *H,NspGrArc *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_grarc))) == NULL) return NULLGRARC;
  H->obj->ref_count=1;
  H->obj->x=self->obj->x;
  H->obj->y=self->obj->y;
  H->obj->w=self->obj->w;
  H->obj->h=self->obj->h;
  H->obj->a1=self->obj->a1;
  H->obj->a2=self->obj->a2;
  H->obj->fill_color=self->obj->fill_color;
  H->obj->thickness=self->obj->thickness;
  H->obj->color=self->obj->color;
  return H;
}

NspGrArc *nsp_grarc_full_copy(NspGrArc *self)
{
  NspGrArc *H  =nsp_grarc_create_void(NVOID,(NspTypeBase *) nsp_type_grarc);
  if ( H ==  NULLGRARC) return NULLGRARC;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRARC;
  if ( nsp_grarc_full_copy_partial(H,self)== NULL) return NULLGRARC;
#line 540 "grarc.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the GrArc
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_grarc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGrArc *H;
  CheckStdRhs(0,0);
  /* want to be sure that type grarc is initialized */
  nsp_type_grarc = new_type_grarc(T_BASE);
  if(( H = nsp_grarc_create_void(NVOID,(NspTypeBase *) nsp_type_grarc)) == NULLGRARC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_grarc_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_grarc_check_values(H) == FAIL) return RET_BUG;
#line 560 "grarc.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_nsp_grarc_full_copy(NspGrArc *self,Stack stack,int rhs,int opt,int lhs)
{
  NspGrArc *ret;

  ret = nsp_grarc_full_copy(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static NspMethods grarc_methods[] = {
  {"full_copy",(nsp_method *) _wrap_nsp_grarc_full_copy},
  { NULL, NULL}
};

static NspMethods *grarc_get_methods(void) { return grarc_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_grarc_get_x(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrArc *) self)->obj->x;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_x(void *self, char *attr, NspObject *O)
{
  double x;

  if ( DoubleScalar(O,&x) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_grarc_get_y(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrArc *) self)->obj->y;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_y(void *self, char *attr, NspObject *O)
{
  double y;

  if ( DoubleScalar(O,&y) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_grarc_get_w(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrArc *) self)->obj->w;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_w(void *self, char *attr, NspObject *O)
{
  double w;

  if ( DoubleScalar(O,&w) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->w= w;
  return OK;
}

static NspObject *_wrap_grarc_get_h(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrArc *) self)->obj->h;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_h(void *self, char *attr, NspObject *O)
{
  double h;

  if ( DoubleScalar(O,&h) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->h= h;
  return OK;
}

static NspObject *_wrap_grarc_get_a1(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrArc *) self)->obj->a1;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_a1(void *self, char *attr, NspObject *O)
{
  double a1;

  if ( DoubleScalar(O,&a1) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->a1= a1;
  return OK;
}

static NspObject *_wrap_grarc_get_a2(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrArc *) self)->obj->a2;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_a2(void *self, char *attr, NspObject *O)
{
  double a2;

  if ( DoubleScalar(O,&a2) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->a2= a2;
  return OK;
}

static NspObject *_wrap_grarc_get_fill_color(void *self,char *attr)
{
  int ret;

  ret = ((NspGrArc *) self)->obj->fill_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grarc_set_fill_color(void *self, char *attr, NspObject *O)
{
  int fill_color;

  if ( IntScalar(O,&fill_color) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->fill_color= fill_color;
  return OK;
}

static NspObject *_wrap_grarc_get_thickness(void *self,char *attr)
{
  int ret;

  ret = ((NspGrArc *) self)->obj->thickness;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grarc_set_thickness(void *self, char *attr, NspObject *O)
{
  int thickness;

  if ( IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->thickness= thickness;
  return OK;
}

static NspObject *_wrap_grarc_get_color(void *self,char *attr)
{
  int ret;

  ret = ((NspGrArc *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grarc_set_color(void *self, char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->color= color;
  return OK;
}

static AttrTab grarc_attrs[] = {
  { "x", (attr_get_function *)_wrap_grarc_get_x, (attr_set_function *)_wrap_grarc_set_x,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_grarc_get_y, (attr_set_function *)_wrap_grarc_set_y,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "w", (attr_get_function *)_wrap_grarc_get_w, (attr_set_function *)_wrap_grarc_set_w,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "h", (attr_get_function *)_wrap_grarc_get_h, (attr_set_function *)_wrap_grarc_set_h,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "a1", (attr_get_function *)_wrap_grarc_get_a1, (attr_set_function *)_wrap_grarc_set_a1,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "a2", (attr_get_function *)_wrap_grarc_get_a2, (attr_set_function *)_wrap_grarc_set_a2,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "fill_color", (attr_get_function *)_wrap_grarc_get_fill_color, (attr_set_function *)_wrap_grarc_set_fill_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "thickness", (attr_get_function *)_wrap_grarc_get_thickness, (attr_set_function *)_wrap_grarc_set_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "color", (attr_get_function *)_wrap_grarc_get_color, (attr_set_function *)_wrap_grarc_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 46 "codegen/grarc.override"
int _wrap_grarc_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 779 "grarc.c"


#line 89 "codegen/grarc.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_grarc(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 791 "grarc.c"


#line 99 "codegen/grarc.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_grarc(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 804 "grarc.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GrArc_func[]={
  {"grarc_attach", _wrap_grarc_attach},
  {"extractelts_grarc", _wrap_nsp_extractelts_grarc},
  {"setrowscols_grarc", _wrap_nsp_setrowscols_grarc},
  { "grarc_create", int_grarc_create},
  { NULL, NULL}
};

/* call ith function in the GrArc interface */

int GrArc_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GrArc_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GrArc_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GrArc_func[i].name;
  *f = GrArc_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
GrArc_register_classes(NspObject *d)
{

#line 18 "codegen/grarc.override"

Init portion 


#line 845 "grarc.c"
  nspgobject_register_class(d, "GrArc", GrArc, &NspGrArc_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 110 "codegen/grarc.override"

/* inserted verbatim at the end */

static void nsp_draw_grarc(BCG *Xgc,NspGraphic *Obj, void *data)
{
  double val[6];
  int ccolor=-1,cthick=-1;
  NspGrArc *P = (NspGrArc *) Obj;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  ccolor = Xgc->graphic_engine->xget_pattern(Xgc); 
  val[0]= P->obj->x;
  val[1]= P->obj->y;
  val[2]= P->obj->w;
  val[3]= P->obj->h;
  val[4]= P->obj->a1;
  val[5]= P->obj->a2;

  
  if ( P->obj->fill_color != -2 ) 
    {
      /* fill the arc */ 
      if (  P->obj->fill_color != -1) 
	Xgc->graphic_engine->scale->xset_pattern(Xgc,P->obj->fill_color);
      Xgc->graphic_engine->scale->fillarc(Xgc,val);
      if (  P->obj->fill_color != -1) 
	Xgc->graphic_engine->scale->xset_pattern(Xgc,ccolor);
    }
  
  if ( P->obj->color != -2 ) 
    {
      /* draw the arc */ 
      if ( P->obj->color != -1 ) 
	Xgc->graphic_engine->scale->xset_pattern(Xgc,P->obj->color);
      if ( P->obj->thickness != -1 ) 
	{
	  cthick = Xgc->graphic_engine->xget_thickness(Xgc); 
	  Xgc->graphic_engine->scale->xset_thickness(Xgc,P->obj->thickness);
	}
      Xgc->graphic_engine->scale->drawarc(Xgc,val);
      /* reset to default values */
      if ( P->obj->color != -1 ) 
	Xgc->graphic_engine->scale->xset_pattern(Xgc,ccolor);
      if ( P->obj->thickness != -1 ) 
	Xgc->graphic_engine->scale->xset_thickness(Xgc,cthick);
    }
}

static void nsp_translate_grarc(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  NspGrArc *P = (NspGrArc *) Obj;
  P->obj->x += tr[0];
  P->obj->y += tr[1];
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_grarc(BCG *Xgc,NspGraphic *Obj,double *R)
{
  NspGrArc *P = (NspGrArc *) Obj;
  double x1;
  x1 = R[0]*(P->obj->x) -R[1]*(P->obj->y);
  P->obj->y = R[1]*(P->obj->x) +R[0]*(P->obj->y);
  P->obj->x = x1;
  /* Il faut aussi changer l'angle */
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_grarc(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  NspGrArc *P = (NspGrArc *) Obj;
  P->obj->x *= alpha[0];
  P->obj->y *= alpha[1];
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of grarc 
 *
 */

static void nsp_getbounds_grarc(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  NspGrArc *P = (NspGrArc *) Obj;
  bounds[0]=P->obj->x;/* xmin */
  bounds[1]=P->obj->y-P->obj->w;/* ymin */
  bounds[2]=P->obj->x+P->obj->w;/* xmax */
  bounds[3]=P->obj->y;/* ymax */
}


#line 940 "grarc.c"
