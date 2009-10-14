/* -*- Mode: C -*- */

/* generated file */


#include <nsp/object.h>
#include <gtk/gtk.h>



#line 30 "codegen/qcurve.override"
#include <nsp/figuredata.h> 
#include <nsp/figure.h>
#include <nsp/qcurve.h>
#include <nsp/axes.h> 

#line 18 "qcurve.c"

/* ----------- NspQcurve ----------- */


#define  NspQcurve_Private 
#include <nsp/object.h>
#include <nsp/qcurve.h>
#include <nsp/interf.h>

/* 
 * NspQcurve inherits from Graphic 
 */

int nsp_type_qcurve_id=0;
NspTypeQcurve *nsp_type_qcurve=NULL;

/*
 * Type object for NspQcurve 
 * all the instance of NspTypeQcurve share the same id. 
 * nsp_type_qcurve: is an instance of NspTypeQcurve 
 *    used for objects of NspQcurve type (i.e built with new_qcurve) 
 * other instances are used for derived classes 
 */
NspTypeQcurve *new_type_qcurve(type_mode mode)
{
  NspTypeQcurve *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_qcurve != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_qcurve;
    }
  if (( type =  malloc(sizeof(NspTypeQcurve))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = qcurve_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = qcurve_get_methods;
  type->new = (new_func *) new_qcurve;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for qcurve */ 

  top->pr = (print_func *) nsp_qcurve_print;
  top->dealloc = (dealloc_func *) nsp_qcurve_destroy;
  top->copy  =  (copy_func *) nsp_qcurve_copy;
  top->size  = (size_func *) nsp_qcurve_size;
  top->s_type =  (s_type_func *) nsp_qcurve_type_as_string;
  top->sh_type = (sh_type_func *) nsp_qcurve_type_short_string;
  top->info = (info_func *) nsp_qcurve_info;
  /* top->is_true = (is_true_func  *) nsp_qcurve_is_true; */
  /* top->loop =(loop_func *) nsp_qcurve_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_qcurve_object;
  top->eq  = (eq_func *) nsp_qcurve_eq;
  top->neq  = (eq_func *) nsp_qcurve_neq;
  top->save  = (save_func *) nsp_qcurve_xdr_save;
  top->load  = (load_func *) nsp_qcurve_xdr_load;
  top->create = (create_func*) int_qcurve_create;
  top->latex = (print_func *) nsp_qcurve_latex;

  /* specific methods for qcurve */

  type->init = (init_func *) init_qcurve;

#line 42 "codegen/qcurve.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_qcurve;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_qcurve ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_qcurve  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_qcurve  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_qcurve  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Qqcurve */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 100 "qcurve.c"
  /* 
   * NspQcurve interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_qcurve_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeQcurve called nsp_type_qcurve
       */
      type->id =  nsp_type_qcurve_id = nsp_new_type_id();
      nsp_type_qcurve = type;
      if ( nsp_register_type(nsp_type_qcurve) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_qcurve(mode);
    }
  else 
    {
      type->id = nsp_type_qcurve_id;
      return type;
    }
}

/*
 * initialize NspQcurve instances 
 * locally and by calling initializer on parent class 
 */

static int init_qcurve(NspQcurve *Obj,NspTypeQcurve *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of NspQcurve 
 */

NspQcurve *new_qcurve() 
{
  NspQcurve *loc;
  /* type must exists */
  nsp_type_qcurve = new_type_qcurve(T_BASE);
  if ( (loc = malloc(sizeof(NspQcurve)))== NULLQCURVE) return loc;
  /* initialize object */
  if ( init_qcurve(loc,nsp_type_qcurve) == FAIL) return NULLQCURVE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspQcurve 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_qcurve_size(NspQcurve *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char qcurve_type_name[]="Qcurve";
static char qcurve_short_type_name[]="qcurve";

static char *nsp_qcurve_type_as_string(void)
{
  return(qcurve_type_name);
}

static char *nsp_qcurve_type_short_string(NspObject *v)
{
  return(qcurve_short_type_name);
}

/*
 * A == B 
 */

static int nsp_qcurve_eq(NspQcurve *A, NspObject *B)
{
  NspQcurve *loc = (NspQcurve *) B;
  if ( check_cast(B,nsp_type_qcurve_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->mark != loc->obj->mark) return FALSE;
  if ( A->obj->width != loc->obj->width) return FALSE;
  if ( A->obj->style != loc->obj->style) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->mode != loc->obj->mode) return FALSE;
  if ( NSP_OBJECT(A->obj->Pts)->type->eq(A->obj->Pts,loc->obj->Pts) == FALSE ) return FALSE;
  if ( strcmp(A->obj->legend,loc->obj->legend) != 0) return FALSE;
  if ( A->obj->start != loc->obj->start) return FALSE;
  if ( A->obj->last != loc->obj->last) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_qcurve_neq(NspQcurve *A, NspObject *B)
{
  return ( nsp_qcurve_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_qcurve_xdr_save(XDR *xdrs, NspQcurve *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_qcurve)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->width) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->style) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mode) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Pts)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->legend) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspQcurve  *nsp_qcurve_xdr_load_partial(XDR *xdrs, NspQcurve *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_qcurve))) == NULL) return NULL;
  M->obj->ref_count=1;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->width) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->style) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mode) == FAIL) return NULL;
  if ((M->obj->Pts =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->legend)) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspQcurve  *nsp_qcurve_xdr_load(XDR *xdrs)
{
  NspQcurve *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLQCURVE;
  if ((H  = nsp_qcurve_create_void(name,(NspTypeBase *) nsp_type_qcurve))== NULLQCURVE) return H;
  if ((H  = nsp_qcurve_xdr_load_partial(xdrs,H))== NULLQCURVE) return H;
  if ( nsp_qcurve_check_values(H) == FAIL) return NULLQCURVE;
#line 272 "qcurve.c"
  return H;
}

/*
 * delete 
 */

void nsp_qcurve_destroy_partial(NspQcurve *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 286 "qcurve.c"
    nsp_matrix_destroy(H->obj->Pts);
  nsp_string_destroy(&(H->obj->legend));
    FREE(H->obj);
   }
}

void nsp_qcurve_destroy(NspQcurve *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_qcurve_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_qcurve_info(NspQcurve *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLQCURVE) 
    {
      Sciprintf("Null Pointer NspQcurve \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_qcurve_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_qcurve_print(NspQcurve *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLQCURVE) 
    {
      Sciprintf("Null Pointer NspQcurve \n");
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
          nsp_qcurve_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_qcurve_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"mark=%d\n",M->obj->mark);
  Sciprintf1(indent+2,"width=%d\n",M->obj->width);
  Sciprintf1(indent+2,"style=%d\n",M->obj->style);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"mode=%d\n",M->obj->mode);
  if ( M->obj->Pts != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Pts),indent+2,"Pts",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"legend=%s\n",M->obj->legend);
  Sciprintf1(indent+2,"start=%d\n",M->obj->start);
  Sciprintf1(indent+2,"last=%d\n",M->obj->last);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_qcurve_latex(NspQcurve *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_qcurve_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"mark=%d\n",M->obj->mark);
  Sciprintf1(indent+2,"width=%d\n",M->obj->width);
  Sciprintf1(indent+2,"style=%d\n",M->obj->style);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"mode=%d\n",M->obj->mode);
  if ( M->obj->Pts != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Pts),indent+2,"Pts",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"legend=%s\n",M->obj->legend);
  Sciprintf1(indent+2,"start=%d\n",M->obj->start);
  Sciprintf1(indent+2,"last=%d\n",M->obj->last);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspQcurve objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspQcurve   *nsp_qcurve_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_qcurve_id) == TRUE ) return ((NspQcurve *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_qcurve));
  return NULL;
}

int IsQcurveObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_qcurve_id);
}

int IsQcurve(NspObject *O)
{
  return nsp_object_type(O,nsp_type_qcurve_id);
}

NspQcurve  *GetQcurveCopy(Stack stack, int i)
{
  if (  GetQcurve(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspQcurve  *GetQcurve(Stack stack, int i)
{
  NspQcurve *M;
  if (( M = nsp_qcurve_object(NthObj(i))) == NULLQCURVE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspQcurve instance 
 *-----------------------------------------------------*/

static NspQcurve *nsp_qcurve_create_void(char *name,NspTypeBase *type)
{
 NspQcurve *H  = (type == NULL) ? new_qcurve() : type->new();
 if ( H ==  NULLQCURVE)
  {
   Sciprintf("No more memory\n");
   return NULLQCURVE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLQCURVE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_qcurve_create_partial(NspQcurve *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_qcurve)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->mark = -1;
  H->obj->width = -1;
  H->obj->style = 0;
  H->obj->color = 0;
  H->obj->mode = 0;
  H->obj->Pts = NULLMAT;
  H->obj->legend = NULL;
  H->obj->start = -1;
  H->obj->last = -1;
  return OK;
}

int nsp_qcurve_check_values(NspQcurve *H)
{
  if ( H->obj->Pts == NULLMAT) 
    {
       if (( H->obj->Pts = nsp_matrix_create("Pts",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->legend == NULL) 
    {
     if (( H->obj->legend = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspQcurve *nsp_qcurve_create(char *name,int mark,int width,int style,int color,int mode,NspMatrix* Pts,char* legend,int start,int last,NspTypeBase *type)
{
 NspQcurve *H  = nsp_qcurve_create_void(name,type);
 if ( H ==  NULLQCURVE) return NULLQCURVE;
  if ( nsp_qcurve_create_partial(H) == FAIL) return NULLQCURVE;
  H->obj->mark=mark;
  H->obj->width=width;
  H->obj->style=style;
  H->obj->color=color;
  H->obj->mode=mode;
  H->obj->Pts= Pts;
  H->obj->legend = legend;
  H->obj->start=start;
  H->obj->last=last;
 if ( nsp_qcurve_check_values(H) == FAIL) return NULLQCURVE;
 return H;
}


NspQcurve *nsp_qcurve_create_default(char *name)
{
 NspQcurve *H  = nsp_qcurve_create_void(name,NULL);
 if ( H ==  NULLQCURVE) return NULLQCURVE;
  if ( nsp_qcurve_create_partial(H) == FAIL) return NULLQCURVE;
 if ( nsp_qcurve_check_values(H) == FAIL) return NULLQCURVE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspQcurve *nsp_qcurve_copy_partial(NspQcurve *H,NspQcurve *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspQcurve *nsp_qcurve_copy(NspQcurve *self)
{
  NspQcurve *H  =nsp_qcurve_create_void(NVOID,(NspTypeBase *) nsp_type_qcurve);
  if ( H ==  NULLQCURVE) return NULLQCURVE;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLQCURVE;
  if ( nsp_qcurve_copy_partial(H,self)== NULL) return NULLQCURVE;

  return H;
}
/*
 * full copy for gobject derived class  
 */

NspQcurve *nsp_qcurve_full_copy_partial(NspQcurve *H,NspQcurve *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_qcurve))) == NULL) return NULLQCURVE;
  H->obj->ref_count=1;
  H->obj->mark=self->obj->mark;
  H->obj->width=self->obj->width;
  H->obj->style=self->obj->style;
  H->obj->color=self->obj->color;
  H->obj->mode=self->obj->mode;
  if ( self->obj->Pts == NULL )
    { H->obj->Pts = NULL;}
  else
    {
      if ((H->obj->Pts = (NspMatrix *) nsp_object_copy_and_name("Pts",NSP_OBJECT(self->obj->Pts))) == NULLMAT) return NULL;
    }
  if ((H->obj->legend = nsp_string_copy(self->obj->legend)) == NULL) return NULL;
  H->obj->start=self->obj->start;
  H->obj->last=self->obj->last;
  return H;
}

NspQcurve *nsp_qcurve_full_copy(NspQcurve *self)
{
  NspQcurve *H  =nsp_qcurve_create_void(NVOID,(NspTypeBase *) nsp_type_qcurve);
  if ( H ==  NULLQCURVE) return NULLQCURVE;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLQCURVE;
  if ( nsp_qcurve_full_copy_partial(H,self)== NULL) return NULLQCURVE;
#line 558 "qcurve.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspQcurve
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_qcurve_create(Stack stack, int rhs, int opt, int lhs)
{
  NspQcurve *H;
  CheckStdRhs(0,0);
  /* want to be sure that type qcurve is initialized */
  nsp_type_qcurve = new_type_qcurve(T_BASE);
  if(( H = nsp_qcurve_create_void(NVOID,(NspTypeBase *) nsp_type_qcurve)) == NULLQCURVE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_qcurve_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_qcurve_check_values(H) == FAIL) return RET_BUG;
#line 578 "qcurve.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_nsp_qcurve_addPts(NspQcurve *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {mat,t_end};
  NspMatrix *pts;

  if ( GetArgs(stack,rhs,opt,T,&pts) == FAIL) return RET_BUG;
/* pts << 1 */
  nsp_qcurve_addPts(self, pts);
  return 0;
}

static int _wrap_nsp_qcurve_clear(NspQcurve *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_qcurve_clear(self);
  return 0;
}

static NspMethods qcurve_methods[] = {
  {"add_points",(nsp_method *) _wrap_nsp_qcurve_addPts},
  {"clear",(nsp_method *) _wrap_nsp_qcurve_clear},
  { NULL, NULL}
};

static NspMethods *qcurve_get_methods(void) { return qcurve_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_qcurve_get_mark(void *self,char *attr)
{
  int ret;

  ret = ((NspQcurve *) self)->obj->mark;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_qcurve_set_mark(void *self, char *attr, NspObject *O)
{
  int mark;

  if ( IntScalar(O,&mark) == FAIL) return FAIL;
  ((NspQcurve *) self)->obj->mark= mark;
  return OK;
}

static NspObject *_wrap_qcurve_get_width(void *self,char *attr)
{
  int ret;

  ret = ((NspQcurve *) self)->obj->width;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_qcurve_set_width(void *self, char *attr, NspObject *O)
{
  int width;

  if ( IntScalar(O,&width) == FAIL) return FAIL;
  ((NspQcurve *) self)->obj->width= width;
  return OK;
}

static NspObject *_wrap_qcurve_get_style(void *self,char *attr)
{
  int ret;

  ret = ((NspQcurve *) self)->obj->style;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_qcurve_set_style(void *self, char *attr, NspObject *O)
{
  int style;

  if ( IntScalar(O,&style) == FAIL) return FAIL;
  ((NspQcurve *) self)->obj->style= style;
  return OK;
}

static NspObject *_wrap_qcurve_get_color(void *self,char *attr)
{
  int ret;

  ret = ((NspQcurve *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_qcurve_set_color(void *self, char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspQcurve *) self)->obj->color= color;
  return OK;
}

#line 75 "codegen/qcurve.override"
/* override set alpha */
static int _wrap_qcurve_set_mode(void *self, char *attr, NspObject *O)
{
  int mode;
  if ( IntScalar(O,&mode) == FAIL) return FAIL;
  if ( ((NspQcurve *) self)->obj->mode !=  mode)
    {
      ((NspQcurve *) self)->obj->mode =  mode;
      nsp_figure_force_redraw(((NspGraphic *) self)->obj->Fig);
    }
  return OK;
}

#line 693 "qcurve.c"
static NspObject *_wrap_qcurve_get_mode(void *self,char *attr)
{
  int ret;

  ret = ((NspQcurve *) self)->obj->mode;
  return nsp_new_double_obj((double) ret);
}

#line 90 "codegen/qcurve.override"

/* overriden to check dimensions when changing values.
 */

static NspObject *_wrap_qcurve_get_obj_Pts(void *self,char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = TRUE; 
  ret = ((NspMatrix*) ((NspQcurve *) self)->obj->Pts);
  return (NspObject *) ret;
}

static int _wrap_qcurve_set_obj_Pts(void *self,NspObject *val)
{
  NspMatrix *M= (NspMatrix *) val ; 
  NspQcurve *poly = self ;
  if ( M->rc_type != 'r' || M->n != 2 )
    {
      Scierror("Error: qcurve field Pts should be real an mx2 sized\n");
      return FAIL;
    }
  /* before replacing the field we check that dimensions are correct */
  if ( poly->obj->Pts != NULL )
    nsp_matrix_destroy(poly->obj->Pts);
  poly->obj->Pts = (NspMatrix *) val ;
  return OK;
}

#line 731 "qcurve.c"
static NspObject *_wrap_qcurve_get_Pts(void *self,char *attr)
{
  NspMatrix *ret;

  ret = ((NspQcurve *) self)->obj->Pts;
  return (NspObject *) ret;
}

static int _wrap_qcurve_set_Pts(void *self, char *attr, NspObject *O)
{
  NspMatrix *Pts;

  if ( ! IsMat(O) ) return FAIL;
  if ((Pts = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspQcurve *) self)->obj->Pts != NULL ) 
    nsp_matrix_destroy(((NspQcurve *) self)->obj->Pts);
  ((NspQcurve *) self)->obj->Pts= Pts;
  return OK;
}

static NspObject *_wrap_qcurve_get_legend(void *self,char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspQcurve *) self)->obj->legend;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_qcurve_set_legend(void *self, char *attr, NspObject *O)
{
  char *legend;

  if ((legend = nsp_string_object(O))==NULL) return FAIL;
  if ((legend = nsp_string_copy(legend)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspQcurve *) self)->obj->legend);
  ((NspQcurve *) self)->obj->legend= legend;
  return OK;
}

static AttrTab qcurve_attrs[] = {
  { "mark", (attr_get_function *)_wrap_qcurve_get_mark, (attr_set_function *)_wrap_qcurve_set_mark,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "width", (attr_get_function *)_wrap_qcurve_get_width, (attr_set_function *)_wrap_qcurve_set_width,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "style", (attr_get_function *)_wrap_qcurve_get_style, (attr_set_function *)_wrap_qcurve_set_style,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "color", (attr_get_function *)_wrap_qcurve_get_color, (attr_set_function *)_wrap_qcurve_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mode", (attr_get_function *)_wrap_qcurve_get_mode, (attr_set_function *)_wrap_qcurve_set_mode,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "Pts", (attr_get_function *)_wrap_qcurve_get_Pts, (attr_set_function *)_wrap_qcurve_set_Pts,(attr_get_object_function *)_wrap_qcurve_get_obj_Pts, (attr_set_object_function *)_wrap_qcurve_set_obj_Pts },
  { "legend", (attr_get_function *)_wrap_qcurve_get_legend, (attr_set_function *)_wrap_qcurve_set_legend,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 62 "codegen/qcurve.override"
int _wrap_qcurve_attach(Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *pl = NULL;
  BCG *Xgc;
  int_types T[] = {obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&pl) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  tape_store_graphic_object(Xgc, pl);
  return 0;
}

#line 800 "qcurve.c"


#line 120 "codegen/qcurve.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_qcurve(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 812 "qcurve.c"


#line 130 "codegen/qcurve.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_qcurve(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 825 "qcurve.c"


int _wrap_oscillo_test(Stack stack, int rhs, int opt, int lhs) /* oscillo */
{
    oscillo_test();
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Qcurve_func[]={
  {"qcurve_attach", _wrap_qcurve_attach},
  {"extractelts_qcurve", _wrap_nsp_extractelts_qcurve},
  {"setrowscols_qcurve", _wrap_nsp_setrowscols_qcurve},
  {"oscillo", _wrap_oscillo_test},
  { "qcurve_create", int_qcurve_create},
  { NULL, NULL}
};

/* call ith function in the Qcurve interface */

int Qcurve_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Qcurve_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Qcurve_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Qcurve_func[i].name;
  *f = Qcurve_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
Qcurve_register_classes(NspObject *d)
{

#line 37 "codegen/qcurve.override"

Init portion 


#line 873 "qcurve.c"
  nspgobject_register_class(d, "NspQcurve", Qcurve, &NspNspQcurve_Type, Nsp_BuildValue("(O)", &NspGraphic_Type));
}
*/

#line 141 "codegen/qcurve.override"

/* inserted verbatim at the end */
/* 
    '("int" "color"); curve color 
    '("int" "mark") ; mark to be used 
    '("double" "width"); line width 
    '("int" "style"); line style 
    '("int" "mode"); mode: std, step, stem, arrow.
    '("NspMatrix*" "Pts")
*/

typedef enum { qcurve_std, qcurve_stairs, qcurve_stem , qcurve_arrow} nsp_qcurve_mode ; 

static void nsp_draw_qcurve(BCG *Xgc,NspGraphic *Obj, void *data)
{
  int cmark=-1;
  int xmark[2];
  NspQcurve *P = (NspQcurve *) Obj;
  int c_width = Xgc->graphic_engine->xget_thickness(Xgc);
  int c_color = Xgc->graphic_engine->xget_pattern(Xgc);
  if ( P->obj->Pts->m == 0) return;
  if ( P->obj->start == -1) return;

  Xgc->graphic_engine->xset_thickness(Xgc,P->obj->width);
  if ( P->obj->mark >= 0 ) 
    {
      /* use a mark */
      Xgc->graphic_engine->xget_mark(Xgc,xmark); 
      cmark=xmark[0];
      Xgc->graphic_engine->xset_mark(Xgc, P->obj->mark,xmark[1]);
    }
  if ( P->obj->color != -1 ) 
    Xgc->graphic_engine->xset_pattern(Xgc, P->obj->color);

  Sciprintf("Avec color %d et mark %d\n", P->obj->color, P->obj->mark);
  /*XXX: we should not be in Rec mode here */
  switch ( P->obj->mode ) 
    {
    case qcurve_std:
    default:
      {
	int n = nsp_qcurve_get_len(P);
	double *xm=NULL,*ym=NULL;
	if ( n == 0) break;
	xm = graphic_alloc(0,n,sizeof(double));
	ym = graphic_alloc(1,n,sizeof(double));
	if ( xm == 0 || ym == 0) 
	  {
	    Sciprintf("Error: cannot allocate points for drawing\n");
	    break;
	  }
	nsp_qcurve_get_xy(P,xm,ym);
	if ( P->obj->mark >= 0 )
	  Xgc->graphic_engine->scale->drawpolymark(Xgc,xm,ym,n);
	else
	  Xgc->graphic_engine->scale->drawpolyline(Xgc,xm,ym,n,0);
	break;
      }

    }
  Xgc->graphic_engine->xset_thickness(Xgc,c_width);
  Xgc->graphic_engine->xset_pattern(Xgc,c_color);
  if ( P->obj->mark >= 0 ) 
    {
      Xgc->graphic_engine->xset_mark(Xgc,cmark,xmark[1]);
    }
}

static void nsp_translate_qcurve(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  int i; 
  NspQcurve *P = (NspQcurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m;
  for ( i=0; i < M->m ; i++) 
    {
      *(x++) += tr[0];
      *(y++) += tr[1];
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_rotate_qcurve(BCG *Xgc,NspGraphic *Obj,double *R)
{
  int i;
  NspQcurve *P = (NspQcurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m,x1,y1;
  for ( i=0; i < M->m ; i++) 
    {
      x1 = R[0]*(*x) -R[1]*(*y);
      y1 = R[1]*(*x) +R[0]*(*y);
      *(x++) =x1;
      *(y++) =y1;
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_qcurve(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  int i;
  NspQcurve *P = (NspQcurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m;
  for ( i=0; i < M->m ; i++) 
    {
      *(x++) *= alpha[0];
      *(y++) *= alpha[1];
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of qcurve 
 *
 */

static int nsp_getbounds_qcurve(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  int pos,max;
  NspQcurve *C = (NspQcurve *) Obj;
  NspMatrix *M = C->obj->Pts;
  double *x=M->R,*y= M->R+M->m, dval;

  if ( M->mn == 0) return FALSE;
  if ( C->obj->start == -1) return FALSE;
  
  pos = C->obj->start;
  
  bounds[0]=x[pos];/* xmin */
  bounds[1]=y[pos];/* ymin */
  bounds[2]=x[pos];/* xmax */
  bounds[3]=y[pos];/* ymax */
  
  if ( pos <= C->obj->last )
    max = C->obj->last;
  else 
    max = M->m -1 ;
  while ( pos <= max )
    {
      dval = x[pos];
      if ( dval > bounds[2] )
	bounds[2] = dval;
      else if ( dval < bounds[0] )
	bounds[0] = dval;
      dval = y[pos];
      if ( dval > bounds[3] )
	bounds[3] = dval;
      else if ( dval < bounds[1] )
	bounds[1] = dval;
      pos++;
    }
  if ( C->obj->last < C->obj->start )
    {
      pos = 0;
      while ( pos <= C->obj->last )
	{
	  dval = x[pos];
	  if ( dval > bounds[2] )
	    bounds[2] = dval;
	  else if ( dval < bounds[0] )
	    bounds[0] = dval;
	  dval = y[pos];
	  if ( dval > bounds[3] )
	    bounds[3] = dval;
	  else if ( dval < bounds[1] )
	    bounds[1] = dval;
	  pos++;
	}
    }
  return TRUE;
}

static void nsp_qcurve_addPts(NspQcurve *C,NspMatrix *Pts)
{
  /* XXX : check if matrix is real and with two columns 
   */
  return nsp_qcurve_addpt(C,Pts->R,Pts->R+Pts->m,Pts->m);
}

static void nsp_qcurve_addpt(NspQcurve *C,double *x,double *y,int n)
{
  NspMatrix *M = C->obj->Pts;
  double *qx=M->R,*qy= M->R+M->m;
  int i,pos;
  /* add n points to the qcurvebuffer 
   */
  if ( M->mn == 0 ) return ;
  if ( C->obj->start == -1 )
    {
      /* initialize */
      C->obj->start = 0;
    }
  for ( i = 0 ; i < n ; i++)
    {
      /* insert after last */
      if ( C->obj->last == -1 )
	{
	  C->obj->last = 0;
	}
      else
	{
	  C->obj->last++;
	  if ( C->obj->last >= M->m )C->obj->last = 0;
	  if ( C->obj->last == C->obj->start)
	    {
	      C->obj->start++;
	      if ( C->obj->start >= M->m ) C->obj->start = 0;
	    }
	}
      /* now insert at  C->obj->last */
      pos = C->obj->last;
      qx[pos]= x[i];
      qy[pos]= y[i];
    }
}

static void nsp_qcurve_clear(NspQcurve *C)
{
  C->obj->start = -1;
}

static int nsp_qcurve_get_len(NspQcurve *C)
{
  return ( C->obj->start <= C->obj->last) 
    ? C->obj->last - C->obj->start + 1
    : C->obj->Pts->m - C->obj->start + C->obj->last +1;
}

static void nsp_qcurve_get_xy(NspQcurve *C,double *cx,double *cy)
{
  int i=0 ,pos,max;
  NspMatrix *M = C->obj->Pts;
  double *x=M->R,*y= M->R+M->m;
  pos = C->obj->start;
  max =( pos <= C->obj->last )
    ? C->obj->last :  M->m -1 ;
  while ( pos <= max )
    {
      cx[i]  = x[pos];
      cy[i]  = y[pos];
      pos++;i++;
    }
  if ( C->obj->last < C->obj->start )
    {
      pos = 0;
      while ( pos <= C->obj->last )
	{
	  cx[i]  = x[pos];
	  cy[i]  = y[pos];
	  pos++;i++;
	}
    }
}

/* set up an oscillo 
 * 
 */

NspFigure *nsp_oscillo_obj(int win,int ncurves,int style[],int bufsize,int yfree,double ymin,double ymax,NspList **Lc)
{
  NspAxes *axe;
  BCG *Xgc;
  char *curve_l=NULL;
  int i,l;
  /*
   * set current window
   */
  if ((Xgc = window_list_get_first()) != NULL) 
    Xgc->graphic_engine->xset_curwin(Max(win,0),TRUE);
  else 
    Xgc= set_graphic_window_new(Max(win,0));

  /*
   * Gc of new window 
   */
  if ((Xgc = window_list_get_first())== NULL) return NULL;
  if ((axe=  nsp_check_for_axes(Xgc,NULL)) == NULL) return NULL;

  /* clean previous plots 
   */ 

  l =  nsp_list_length(axe->obj->children);
  for ( i = 0 ; i < l  ; i++)
    nsp_list_remove_first(axe->obj->children);

  /* create a set of qcurves and insert them in axe */
  for ( i = 0 ; i < ncurves ; i++) 
    {
      int mark=-1;
      NspQcurve *curve;
      NspMatrix *Pts = nsp_matrix_create("Pts",'r',Max(bufsize,0),2); 
      if ( Pts == NULL) return NULL;
      if ( style[i] <= 0 ) mark = -style[i];
      curve= nsp_qcurve_create("curve",mark,0,0,( style[i] > 0 ) ?  style[i] : -1,
			       qcurve_std,Pts,curve_l,-1,-1,NULL);
      if ( curve == NULL) return NULL;
      /* insert the new curve */
      if ( nsp_list_end_insert( axe->obj->children,(NspObject *)curve )== FAIL)
	return NULL;
    }
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  /* updates the axes scale information */
  nsp_strf_axes(Xgc, axe , NULL, '2');
  axe->obj->iso = FALSE;
  axe->obj->fixed = FALSE;
  nsp_figure_force_redraw(((NspGraphic *) axe)->obj->Fig);
  if ( Lc != NULL) *Lc = axe->obj->children;
  return ((NspGraphic *) axe)->obj->Fig;
}

void  nsp_oscillo_add_point(NspList *L,double t,double *y, int n)
{
  int count =0;
  Cell *Loc = L->first;
  while ( Loc != NULLCELL ) 
    {
      if ( Loc->O != NULLOBJ )
	{ 
	  NspQcurve *curve =(NspQcurve *) Loc->O;
	  if ( count >= n ) return;
	  nsp_qcurve_addpt(curve,&t,&y[count],1);
	  count++;
	  return;
	}
      Loc = Loc->next;
    }
}

static void oscillo_test()
{
  int style[]={-1,2,3};
  nsp_oscillo_obj(123,3,style,100,TRUE,0,0,NULL);
}


     

#line 1216 "qcurve.c"
