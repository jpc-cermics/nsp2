/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 4 "curve.override"
#include "nsp/curve.h"
extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
static void nsp_draw_curve(BCG *Xgc,NspGraphic *Obj);
static void nsp_translate_curve(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_curve(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_curve(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_curve(BCG *Xgc,NspGraphic *o,double *bounds);

#line 22 "curve.c"

/* ----------- Curve ----------- */


#define  Curve_Private 
#include "nsp/object.h"
#include "nsp/curve.h"
#include "nsp/interf.h"

/* 
 * NspCurve inherits from NspGraphic 
 */

int nsp_type_curve_id=0;
NspTypeCurve *nsp_type_curve=NULL;

/*
 * Type object for Curve 
 * all the instance of NspTypeCurve share the same id. 
 * nsp_type_curve: is an instance of NspTypeCurve 
 *    used for objects of NspCurve type (i.e built with new_curve) 
 * other instances are used for derived classes 
 */
NspTypeCurve *new_type_curve(type_mode mode)
{
  NspTypeCurve *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_curve != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_curve;
    }
  if ((type =  malloc(sizeof(NspTypeCurve))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = curve_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = curve_get_methods; 
  type->new = (new_func *) new_curve;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for curve */ 

  top->pr = (print_func *) nsp_curve_print;                  
  top->dealloc = (dealloc_func *) nsp_curve_destroy;
  top->copy  =  (copy_func *) nsp_curve_copy;                 
  top->size  = (size_func *) nsp_curve_size;                
  top->s_type =  (s_type_func *) nsp_curve_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_curve_type_short_string;
  top->info = (info_func *) nsp_curve_info ;                  
  /* top->is_true = (is_true_func  *) nsp_curve_is_true; */
  /* top->loop =(loop_func *) nsp_curve_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_curve_object;
  top->eq  = (eq_func *) nsp_curve_eq;
  top->neq  = (eq_func *) nsp_curve_neq;
  top->save  = (save_func *) nsp_curve_xdr_save;
  top->load  = (load_func *) nsp_curve_xdr_load;
  top->create = (create_func*) int_curve_create;
  top->latex = (print_func *) nsp_curve_latex;
  
  /* specific methods for curve */
      
  type->init = (init_func *) init_curve;

#line 20 "curve.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_curve;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_curve ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_curve  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_curve  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_curve  ;

#line 101 "curve.c"
  /* 
   * Curve interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_curve_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCurve called nsp_type_curve
       */
      type->id =  nsp_type_curve_id = nsp_new_type_id();
      nsp_type_curve = type;
      if ( nsp_register_type(nsp_type_curve) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_curve(mode);
    }
  else 
    {
       type->id = nsp_type_curve_id;
       return type;
    }
}

/*
 * initialize Curve instances 
 * locally and by calling initializer on parent class 
 */

static int init_curve(NspCurve *Obj,NspTypeCurve *type)
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
 * new instance of Curve 
 */

NspCurve *new_curve() 
{
  NspCurve *loc; 
  /* type must exists */
  nsp_type_curve = new_type_curve(T_BASE);
  if ( (loc = malloc(sizeof(NspCurve)))== NULLCURVE) return loc;
  /* initialize object */
  if ( init_curve(loc,nsp_type_curve) == FAIL) return NULLCURVE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Curve 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_curve_size(NspCurve *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char curve_type_name[]="Curve";
static char curve_short_type_name[]="curve";

static char *nsp_curve_type_as_string(void)
{
  return(curve_type_name);
}

static char *nsp_curve_type_short_string(NspObject *v)
{
  return(curve_short_type_name);
}

/*
 * A == B 
 */

static int nsp_curve_eq(NspCurve *A, NspObject *B)
{
  NspCurve *loc = (NspCurve *) B;
  if ( check_cast(B,nsp_type_curve_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->mark != loc->obj->mark) return FALSE;
  if ( A->obj->width != loc->obj->width) return FALSE;
  if ( A->obj->style != loc->obj->style) return FALSE;
  if ( A->obj->mode != loc->obj->mode) return FALSE;
  if ( NSP_OBJECT(A->obj->Pts)->type->eq(A->obj->Pts,loc->obj->Pts) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_curve_neq(NspCurve *A, NspObject *B)
{
  return ( nsp_curve_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_curve_xdr_save(XDR *xdrs, NspCurve *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->width) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->style) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mode) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Pts)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspCurve  *nsp_curve_xdr_load_partial(XDR *xdrs, NspCurve *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = malloc(sizeof(nsp_curve))) == NULL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->width) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->style) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mode) == FAIL) return NULL;
  if ((M->obj->Pts =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspCurve  *nsp_curve_xdr_load(XDR *xdrs)
{
  NspCurve *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCURVE;
  if ((M  = nsp_curve_create_void(name,(NspTypeBase *) nsp_type_curve))== NULLCURVE) return M;
  return nsp_curve_xdr_load_partial(xdrs,M);
}

/*
 * delete 
 */

void nsp_curve_destroy_partial(NspCurve *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    nsp_matrix_destroy(H->obj->Pts);
    FREE(H->obj);
   }
}

void nsp_curve_destroy(NspCurve *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_curve_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_curve_info(NspCurve *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCURVE) 
    {
      Sciprintf("Null Pointer Curve \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_curve_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_curve_print(NspCurve *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCURVE) 
    {
      Sciprintf("Null Pointer Curve \n");
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
          nsp_curve_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_curve_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"mark=%d\n",M->obj->mark);
  Sciprintf1(indent+2,"width=%f\n",M->obj->width);
  Sciprintf1(indent+2,"style=%d\n",M->obj->style);
  Sciprintf1(indent+2,"mode=%d\n",M->obj->mode);
  if ( M->obj->Pts != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Pts),indent+2,"Pts",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_curve_latex(NspCurve *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_curve_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"mark=%d\n",M->obj->mark);
  Sciprintf1(indent+2,"width=%f\n",M->obj->width);
  Sciprintf1(indent+2,"style=%d\n",M->obj->style);
  Sciprintf1(indent+2,"mode=%d\n",M->obj->mode);
  if ( M->obj->Pts != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Pts),indent+2,"Pts",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Curve objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspCurve   *nsp_curve_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_curve_id) == TRUE ) return ((NspCurve *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_curve));
  return NULL;
}

int IsCurveObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_curve_id);
}

int IsCurve(NspObject *O)
{
  return nsp_object_type(O,nsp_type_curve_id);
}

NspCurve  *GetCurveCopy(Stack stack, int i)
{
  if (  GetCurve(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspCurve  *GetCurve(Stack stack, int i)
{
  NspCurve *M;
  if (( M = nsp_curve_object(NthObj(i))) == NULLCURVE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspCurve *nsp_curve_create_void(char *name,NspTypeBase *type)
{
 NspCurve *H  = (type == NULL) ? new_curve() : type->new();
 if ( H ==  NULLCURVE)
  {
   Sciprintf("No more memory\n");
   return NULLCURVE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCURVE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_curve_create_partial(NspCurve *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_curve)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  return OK;
}

int nsp_curve_check_values(NspCurve *H)
{
  if ( H->obj->Pts == NULLMAT) 
    {
     if (( H->obj->Pts = nsp_matrix_create("Pts",'r',0,0)) == NULLMAT)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspCurve *nsp_curve_create(char *name,int color,int mark,double width,int style,int mode,NspMatrix* Pts,NspTypeBase *type)
{
 NspCurve *H  = nsp_curve_create_void(name,type);
 if ( H ==  NULLCURVE) return NULLCURVE;
  if ( nsp_curve_create_partial(H) == FAIL) return NULLCURVE;
  H->obj->color=color;
  H->obj->mark=mark;
  H->obj->width=width;
  H->obj->style=style;
  H->obj->mode=mode;
  if ( Pts == NULL )
    { H->obj->Pts = NULL;}
  else
    {
      if ((H->obj->Pts = (NspMatrix *)  nsp_object_copy_and_name("Pts",NSP_OBJECT(Pts))) == NULLMAT) return NULL;
    }
 if ( nsp_curve_check_values(H) == FAIL) return NULLCURVE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspCurve *nsp_curve_copy_partial(NspCurve *H,NspCurve *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspCurve *nsp_curve_copy(NspCurve *self)
{
  NspCurve *H  =nsp_curve_create_void(NVOID,(NspTypeBase *) nsp_type_curve);
  if ( H ==  NULLCURVE) return NULLCURVE;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCURVE;
  if ( nsp_curve_copy_partial(H,self)== NULL) return NULLCURVE;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Curve
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_curve_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCurve *H;
  CheckStdRhs(0,0);
  /* want to be sure that type curve is initialized */
  nsp_type_curve = new_type_curve(T_BASE);
  if(( H = nsp_curve_create_void(NVOID,(NspTypeBase *) nsp_type_curve)) == NULLCURVE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_curve_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_curve_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *curve_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_curve_get_color(void *self,char *attr)
{
  int ret;

  ret = ((NspCurve *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_curve_set_color(void *self, char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  return OK;
}

static NspObject *_wrap_curve_get_mark(void *self,char *attr)
{
  int ret;

  ret = ((NspCurve *) self)->obj->mark;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_curve_set_mark(void *self, char *attr, NspObject *O)
{
  int mark;

  if ( IntScalar(O,&mark) == FAIL) return FAIL;
  return OK;
}

static NspObject *_wrap_curve_get_width(void *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspCurve *) self)->obj->width;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_curve_set_width(void *self, char *attr, NspObject *O)
{
  double width;

  if ( DoubleScalar(O,&width) == FAIL) return FAIL;
  return OK;
}

static NspObject *_wrap_curve_get_style(void *self,char *attr)
{
  int ret;

  ret = ((NspCurve *) self)->obj->style;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_curve_set_style(void *self, char *attr, NspObject *O)
{
  int style;

  if ( IntScalar(O,&style) == FAIL) return FAIL;
  return OK;
}

#line 50 "curve.override"
/* override set alpha */
static int _wrap_curve_set_mode(void *self, char *attr, NspObject *O)
{
  int mode;
  BCG *Xgc;
  if ( IntScalar(O,&mode) == FAIL) return FAIL;
  ((NspCurve *) self)->obj->mode = mode;
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->force_redraw(Xgc);
  return OK;
}

#line 586 "curve.c"
static NspObject *_wrap_curve_get_mode(void *self,char *attr)
{
  int ret;

  ret = ((NspCurve *) self)->obj->mode;
  return nsp_new_double_obj((double) ret);
}

#line 64 "curve.override"

/* overriden to check dimensions when changing values.
 */

static NspObject *_wrap_curve_get_obj_Pts(void *self,char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = TRUE; 
  ret = ((NspMatrix*) ((NspCurve *) self)->obj->Pts);
  return (NspObject *) ret;
}

static int _wrap_curve_set_obj_Pts(void *self,NspObject *val)
{
  NspMatrix *M= (NspMatrix *) val ; 
  NspCurve *poly = self ;
  if ( M->rc_type != 'r' || M->n != 2 )
    {
      Scierror("Error: curve field Pts should be real an mx2 sized\n");
      return FAIL;
    }
  /* before replacing the field we check that dimensions are correct */
  if ( poly->obj->Pts != NULL )
    nsp_matrix_destroy(poly->obj->Pts);
  poly->obj->Pts = (NspMatrix *) val ;
  return OK;
}



#line 626 "curve.c"
static NspObject *_wrap_curve_get_Pts(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspCurve *) self)->obj->Pts;
  return (NspObject *) ret;
}

static int _wrap_curve_set_Pts(void *self, char *attr, NspObject *O)
{
  NspMatrix *Pts;

  if ( ! IsMat(O) ) return FAIL;
  if ((Pts = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspCurve *) self)->obj->Pts != NULL ) 
    nsp_matrix_destroy(((NspCurve *) self)->obj->Pts);
  return OK;
}

static AttrTab curve_attrs[] = {
  { "color", (attr_get_function *)_wrap_curve_get_color, (attr_set_function *)_wrap_curve_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mark", (attr_get_function *)_wrap_curve_get_mark, (attr_set_function *)_wrap_curve_set_mark,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "width", (attr_get_function *)_wrap_curve_get_width, (attr_set_function *)_wrap_curve_set_width,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "style", (attr_get_function *)_wrap_curve_get_style, (attr_set_function *)_wrap_curve_set_style,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mode", (attr_get_function *)_wrap_curve_get_mode, (attr_set_function *)_wrap_curve_set_mode,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "Pts", (attr_get_function *)_wrap_curve_get_Pts, (attr_set_function *)_wrap_curve_set_Pts,(attr_get_object_function *)_wrap_curve_get_obj_Pts, (attr_set_object_function *)_wrap_curve_set_obj_Pts },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 37 "curve.override"
int _wrap_curve_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  store_graphic_object(Xgc, pl);
  return 0;
}

#line 672 "curve.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Curve_func[]={
  {"curve_attach", _wrap_curve_attach},
  { "curve_create", int_curve_create},
  { NULL, NULL}
};

/* call ith function in the Curve interface */

int Curve_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Curve_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Curve_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Curve_func[i].name;
  *f = Curve_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Curve_register_classes(NspObject *d)
{

#line 15 "curve.override"

Init portion 


#line 711 "curve.c"
  nspgobject_register_class(d, "Curve", Curve, &NspCurve_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 96 "curve.override"

/* inserted verbatim at the end */
/* 
    '("int" "color"); curve color 
    '("int" "mark") ; mark to be used 
    '("double" "width"); line width 
    '("int" "style"); line style 
    '("int" "mode"); mode: std, step, stem, arrow.
    '("NspMatrix*" "Pts")
*/

typedef enum { curve_std, curve_stairs, curve_stem , curve_arrow} nsp_curve_mode ; 

static void nsp_draw_curve(BCG *Xgc,NspGraphic *Obj)
{
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  int c_width = Xgc->graphic_engine->xget_thickness(Xgc);
  int c_color = Xgc->graphic_engine->xget_pattern(Xgc);
  if ( P->obj->Pts->m == 0) return;
  Xgc->graphic_engine->xset_thickness(Xgc,P->obj->width);
  Xgc->graphic_engine->xset_pattern(Xgc,P->obj->color);
  /*XXX: we should not be in Rec mode here */
  switch ( P->obj->mode ) 
    {
    case curve_std: 
      Xgc->graphic_engine->scale->drawpolyline(Xgc,M->R,M->R+M->m,M->m,0);
      break;
    case curve_stairs:
      {
	double *xm=NULL,*ym=NULL;
	int n= 2*M->m,i;
	xm = graphic_alloc(0,n,sizeof(double));
	ym = graphic_alloc(1,n,sizeof(double));
	if ( xm == 0 || ym == 0) 
	  {
	    Sciprintf("Error: cannot allocated points for drawing\n");
	    return;
	  }
	for ( i=0 ; i < M->m -1 ; i++) 
	  {
	    xm[2*i]= M->R[i];
	    ym[2*i]= M->R[i+M->m];
	    xm[2*i+1]= M->R[i+1];
	    ym[2*i+1]= ym[2*i];
	  }
	xm[2*(M->m-1)] = M->R[M->m-1];
	ym[2*(M->m-1)] = M->R[M->m-1+M->m];
	Xgc->graphic_engine->scale->drawpolyline(Xgc,xm,ym,2*M->m-1,0); 
      }
      break;
    case curve_stem:
      {
	double *xm=NULL,*ym=NULL;
	int n= 2*M->m,i;
	xm = graphic_alloc(0,n,sizeof(double));
	ym = graphic_alloc(1,n,sizeof(double));
	if ( xm == 0 || ym == 0) 
	  {
	    Sciprintf("Error: cannot allocated points for drawing\n");
	    return;
	  }
	for ( i=0 ; i < M->m ; i++) 
	  {
	    xm[2*i]= M->R[i];
	    ym[2*i]= 0;
	    xm[2*i+1]= M->R[i];
	    ym[2*i+1]= M->R[i+M->m];
	  }
	for ( i = 0 ; i < M->m ; i++)
	  {
	    int iflag=0;
	    Xgc->graphic_engine->scale->drawsegments(Xgc,xm,ym,2*M->m,&P->obj->color,iflag);
	  }
      }
      break;
    case curve_arrow: 
      break;
    }
  Xgc->graphic_engine->xset_thickness(Xgc,c_width);
  Xgc->graphic_engine->xset_pattern(Xgc,c_color);
}

static void nsp_translate_curve(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  int i; 
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m;
  for ( i=0; i < M->m ; i++) 
    {
      *(x++) += tr[0];
      *(y++) += tr[1];
    }
}

static void nsp_rotate_curve(BCG *Xgc,NspGraphic *Obj,double *R)
{
  int i;
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m,x1,y1;
  for ( i=0; i < M->m ; i++) 
    {
      x1 = R[0]*(*x) -R[1]*(*y);
      y1 = R[1]*(*x) +R[0]*(*y);
      *(x++) =x1;
      *(y++) =y1;
    }
}

static void nsp_scale_curve(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  int i;
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m;
  for ( i=0; i < M->m ; i++) 
    {
      *(x++) *= alpha[0];
      *(y++) *= alpha[1];
    }
}

/* compute in bounds the enclosing rectangle of curve 
 *
 */

static void nsp_getbounds_curve(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  int i;
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m, dval;
  bounds[0]=*x;/* xmin */
  bounds[1]=*y;/* ymin */
  bounds[2]=*x;/* xmax */
  bounds[3]=*y;/* ymax */
  for (i = 1; i < M->m; i++)
    {
      dval = x[i];
      if ( dval > bounds[2] )
	bounds[2] = dval;
      else if ( dval < bounds[0] )
	bounds[0] = dval;
      dval = y[i];
      if ( dval > bounds[3] )
	bounds[3] = dval;
      else if ( dval < bounds[1] )
	bounds[1] = dval;
    }
}


#line 871 "curve.c"
