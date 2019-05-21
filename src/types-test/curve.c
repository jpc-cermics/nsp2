/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */





#line 34 "codegen/curve.override"
#include <gdk/gdk.h>
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/curve.h>
#include <nsp/figuredata.h>
#include <nsp/figure.h>

#line 36 "curve.c"

/* -----------NspCurve ----------- */


#define  NspCurve_Private 
#include <nsp/objects.h>
#include <nsp/curve.h>
#include <nsp/interf.h>

/* 
 * NspCurve inherits from Graphic 
 */

int nsp_type_curve_id=0;
NspTypeCurve *nsp_type_curve=NULL;

/*
 * Type object for NspCurve 
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
  if (( type =  malloc(sizeof(NspTypeCurve))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = curve_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = curve_get_methods;
  type->gtk_methods = FALSE;
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
  top->info = (info_func *) nsp_curve_info;
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
  top->full_copy = (copy_func *) nsp_curve_full_copy;

  /* specific methods for curve */

  type->init = (init_func *) init_curve;

#line 48 "codegen/curve.override"
  /* inserted verbatim in the type definition */
  type->gtk_methods = TRUE;
  /* here we override the method or its father class i.e Graphic */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_curve;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_curve ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_curve  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_curve  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_curve  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Curve */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */

#line 122 "curve.c"
  /* 
   * NspCurve interfaces can be added here 
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
 * initialize NspCurve instances 
 * locally and by calling initializer on parent class 
 */

static int init_curve(NspCurve *Obj,NspTypeCurve *type)
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
 * new instance of NspCurve 
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
 * Object method redefined for NspCurve 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_curve_size(NspCurve *Mat, int flag)
{
  return 1;
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
  if ( A->obj->mark != loc->obj->mark) return FALSE;
  if ( A->obj->mark_size != loc->obj->mark_size) return FALSE;
  if ( A->obj->mark_color != loc->obj->mark_color) return FALSE;
  if ( A->obj->width != loc->obj->width) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->mode != loc->obj->mode) return FALSE;
  if ( NSP_OBJECT(A->obj->Pts)->type->eq(A->obj->Pts,loc->obj->Pts) == FALSE ) return FALSE;
  if ( strcmp(A->obj->legend,loc->obj->legend) != 0) return FALSE;
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
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_curve)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark_size) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->width) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mode) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Pts)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->legend) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspCurve  *nsp_curve_xdr_load_partial(XDR *xdrs, NspCurve *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark_size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->width) == FAIL) return NULL;
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
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic * )M) == NULL) return NULL;
 return M;
}

static NspCurve  *nsp_curve_xdr_load(XDR *xdrs)
{
  NspCurve *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCURVE;
  if ((H  = nsp_curve_create_void(name,(NspTypeBase *) nsp_type_curve))== NULLCURVE) return H;
  if ( nsp_curve_create_partial(H) == FAIL) return NULLCURVE;
  if ((H  = nsp_curve_xdr_load_partial(xdrs,H))== NULLCURVE) return H;
  if ( nsp_curve_check_values(H) == FAIL) return NULLCURVE;
  return H;
}

/*
 * delete 
 */

void nsp_curve_destroy_partial(NspCurve *H)
{
  nsp_graphic_destroy_partial((NspGraphic * ) H);
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->Pts != NULL ) 
      nsp_matrix_destroy(H->obj->Pts);
  nsp_string_destroy(&(H->obj->legend));
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
      Sciprintf("Null Pointer NspCurve \n");
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
      Sciprintf("Null Pointer NspCurve \n");
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
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_curve_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"mark=%d\n", M->obj->mark);
  Sciprintf1(indent+2,"mark_size=%d\n", M->obj->mark_size);
  Sciprintf1(indent+2,"mark_color=%d\n", M->obj->mark_color);
  Sciprintf1(indent+2,"width=%d\n", M->obj->width);
  Sciprintf1(indent+2,"color=%d\n", M->obj->color);
  Sciprintf1(indent+2,"mode=%d\n", M->obj->mode);
  if ( M->obj->Pts != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Pts),indent+2,"Pts", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"legend=%s\n",M->obj->legend);
  nsp_graphic_print((NspGraphic * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_curve_latex(NspCurve *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_curve_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|mark|= \\numprint{%d}\n",M->obj->mark);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|mark_size|= \\numprint{%d}\n",M->obj->mark_size);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|mark_color|= \\numprint{%d}\n",M->obj->mark_color);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|width|= \\numprint{%d}\n",M->obj->width);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|color|= \\numprint{%d}\n",M->obj->color);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|mode|= \\numprint{%d}\n",M->obj->mode);
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->Pts != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Pts),FALSE,"Pts", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|legend|=\\verb@\"%s\"@\n",(M->obj->legend==NULL) ? "NULL": M->obj->legend);
  Sciprintf1(2,"\\\\\n");
  nsp_graphic_latex((NspGraphic * ) M, FALSE,NULL,rec_level);
  Sciprintf1(indent+1,"\n");
  Sciprintf("\\end{array}\n");

  Sciprintf("\\right.\n");

  if ( use_math ) Sciprintf("\\end{equation*}\n");

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspCurve objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspCurve   *nsp_curve_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_curve_id)  == TRUE  ) return ((NspCurve *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_curve));
  return NULL;
}

int IsCurveObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_curve_id);
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
 * create a NspCurve instance 
 *-----------------------------------------------------*/

static NspCurve *nsp_curve_create_void(const char *name,NspTypeBase *type)
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
  if ( nsp_graphic_create_partial((NspGraphic * ) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_curve)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->mark = -1;
  H->obj->mark_size = -1;
  H->obj->mark_color = -1;
  H->obj->width = -1;
  H->obj->color = 0;
  H->obj->mode = 0;
  H->obj->Pts = NULLMAT;
  H->obj->legend = NULL;
  return OK;
}

int nsp_curve_check_values(NspCurve *H)
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
  nsp_graphic_check_values((NspGraphic * ) H);
  return OK;
}

NspCurve *nsp_curve_create(const char *name,int mark,int mark_size,int mark_color,int width,int color,int mode,NspMatrix* Pts,char* legend,NspTypeBase *type)
{
  NspCurve *H  = nsp_curve_create_void(name,type);
  if ( H ==  NULLCURVE) return NULLCURVE;
  if ( nsp_curve_create_partial(H) == FAIL) return NULLCURVE;
  H->obj->mark=mark;
  H->obj->mark_size=mark_size;
  H->obj->mark_color=mark_color;
  H->obj->width=width;
  H->obj->color=color;
  H->obj->mode=mode;
  H->obj->Pts= Pts;
  H->obj->legend = legend;
  if ( nsp_curve_check_values(H) == FAIL) return NULLCURVE;
  return H;
}


NspCurve *nsp_curve_create_default(const char *name)
{
 NspCurve *H  = nsp_curve_create_void(name,NULL);
 if ( H ==  NULLCURVE) return NULLCURVE;
  if ( nsp_curve_create_partial(H) == FAIL) return NULLCURVE;
  if ( nsp_curve_check_values(H) == FAIL) return NULLCURVE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspCurve *nsp_curve_copy_partial(NspCurve *H,NspCurve *self)
{
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLCURVE;
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspCurve *nsp_curve_copy(NspCurve *self)
{
  NspCurve *H  =nsp_curve_create_void(NVOID,(NspTypeBase *) nsp_type_curve);
  if ( H ==  NULLCURVE) return NULLCURVE;
  if ( nsp_curve_copy_partial(H,self)== NULL) return NULLCURVE;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspCurve *nsp_curve_full_copy_partial(NspCurve *H,NspCurve *self)
{
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLCURVE;
  if ((H->obj = calloc(1,sizeof(nsp_curve))) == NULL) return NULLCURVE;
  H->obj->ref_count=1;
  H->obj->mark=self->obj->mark;
  H->obj->mark_size=self->obj->mark_size;
  H->obj->mark_color=self->obj->mark_color;
  H->obj->width=self->obj->width;
  H->obj->color=self->obj->color;
  H->obj->mode=self->obj->mode;
  if ( self->obj->Pts == NULL )
    { H->obj->Pts = NULL;}
  else
    {
      if ((H->obj->Pts = (NspMatrix *) nsp_object_full_copy_and_name("Pts", NSP_OBJECT(self->obj->Pts))) == NULLMAT) return NULL;
    }
  if ((H->obj->legend = nsp_string_copy(self->obj->legend)) == NULL) return NULL;
  return H;
}

NspCurve *nsp_curve_full_copy(NspCurve *self)
{
  NspCurve *H  =nsp_curve_create_void(NVOID,(NspTypeBase *) nsp_type_curve);
  if ( H ==  NULLCURVE) return NULLCURVE;
  if ( nsp_curve_full_copy_partial(H,self)== NULL) return NULLCURVE;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspCurve
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
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_curve_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *curve_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_curve_get_mark(void *self,const char *attr)
{
  int ret;
  ret = ((NspCurve *) self)->obj->mark;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_curve_set_mark(void *self,const char *attr, NspObject *O)
{
  int mark;
  if ( IntScalar(O,&mark) == FAIL) return FAIL;
  ((NspCurve *) self)->obj->mark= mark;
  return OK;
}

static NspObject *_wrap_curve_get_mark_size(void *self,const char *attr)
{
  int ret;
  ret = ((NspCurve *) self)->obj->mark_size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_curve_set_mark_size(void *self,const char *attr, NspObject *O)
{
  int mark_size;
  if ( IntScalar(O,&mark_size) == FAIL) return FAIL;
  ((NspCurve *) self)->obj->mark_size= mark_size;
  return OK;
}

static NspObject *_wrap_curve_get_mark_color(void *self,const char *attr)
{
  int ret;
  ret = ((NspCurve *) self)->obj->mark_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_curve_set_mark_color(void *self,const char *attr, NspObject *O)
{
  int mark_color;
  if ( IntScalar(O,&mark_color) == FAIL) return FAIL;
  ((NspCurve *) self)->obj->mark_color= mark_color;
  return OK;
}

static NspObject *_wrap_curve_get_width(void *self,const char *attr)
{
  int ret;
  ret = ((NspCurve *) self)->obj->width;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_curve_set_width(void *self,const char *attr, NspObject *O)
{
  int width;
  if ( IntScalar(O,&width) == FAIL) return FAIL;
  ((NspCurve *) self)->obj->width= width;
  return OK;
}

static NspObject *_wrap_curve_get_color(void *self,const char *attr)
{
  int ret;
  ret = ((NspCurve *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_curve_set_color(void *self,const char *attr, NspObject *O)
{
  int color;
  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspCurve *) self)->obj->color= color;
  return OK;
}

#line 70 "codegen/curve.override"
/* override set alpha */
static int _wrap_curve_set_mode(void *self, char *attr, NspObject *O)
{
  int mode;
  if ( IntScalar(O,&mode) == FAIL) return FAIL;
  if ( ((NspCurve *) self)->obj->mode !=  mode)
    {
      ((NspCurve *) self)->obj->mode =  mode;
      nsp_graphic_invalidate((NspGraphic *) self);
    }
  return OK;
}

#line 719 "curve.c"
static NspObject *_wrap_curve_get_mode(void *self,const char *attr)
{
  int ret;
  ret = ((NspCurve *) self)->obj->mode;
  return nsp_new_double_obj((double) ret);
}

#line 85 "codegen/curve.override"

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

#line 756 "curve.c"
static NspObject *_wrap_curve_get_Pts(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspCurve *) self)->obj->Pts;
  return (NspObject *) ret;
}

static int _wrap_curve_set_Pts(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Pts;
  if ( ! IsMat(O) ) return FAIL;
  if ((Pts = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspCurve *) self)->obj->Pts != NULL ) 
    nsp_matrix_destroy(((NspCurve *) self)->obj->Pts);
  ((NspCurve *) self)->obj->Pts= Pts;
  return OK;
}

static NspObject *_wrap_curve_get_legend(void *self,const char *attr)
{
  NspObject *nsp_ret;
  const gchar *ret;
  ret = ((NspCurve *) self)->obj->legend;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_curve_set_legend(void *self,const char *attr, NspObject *O)
{
  char *legend;
  if ((legend = nsp_string_object(O))==NULL) return FAIL;
  if ((legend = nsp_string_copy(legend)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspCurve *) self)->obj->legend);
  ((NspCurve *) self)->obj->legend= legend;
  return OK;
}

static AttrTab curve_attrs[] = {
  { "mark", (attr_get_function * )_wrap_curve_get_mark, (attr_set_function * )_wrap_curve_set_mark, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "mark_size", (attr_get_function * )_wrap_curve_get_mark_size, (attr_set_function * )_wrap_curve_set_mark_size, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "mark_color", (attr_get_function * )_wrap_curve_get_mark_color, (attr_set_function * )_wrap_curve_set_mark_color, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "width", (attr_get_function * )_wrap_curve_get_width, (attr_set_function * )_wrap_curve_set_width, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "color", (attr_get_function * )_wrap_curve_get_color, (attr_set_function * )_wrap_curve_set_color, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "mode", (attr_get_function * )_wrap_curve_get_mode, (attr_set_function * )_wrap_curve_set_mode, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "Pts", (attr_get_function * )_wrap_curve_get_Pts, (attr_set_function * )_wrap_curve_set_Pts, (attr_get_object_function * )_wrap_curve_get_obj_Pts, (attr_set_object_function * )_wrap_curve_set_obj_Pts },
  { "legend", (attr_get_function * )_wrap_curve_get_legend, (attr_set_function * )_wrap_curve_set_legend, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 115 "codegen/curve.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_curve(Stack stack, int rhs, int opt, int lhs)
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 819 "curve.c"


#line 125 "codegen/curve.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_curve(Stack stack, int rhs, int opt, int lhs)
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 832 "curve.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Curve_func[]={
  { "extractelts_curve", _wrap_nsp_extractelts_curve},
  { "setrowscols_curve", _wrap_nsp_setrowscols_curve},
  { "curve_create", int_curve_create},
  { NULL, NULL}
};

/* call ith function in the Curve interface */

int Curve_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Curve_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Curve_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Curve_func[i].name;
  *f = Curve_func[i].fonc;
}
void nsp_initialize_Curve_types(void)
{
  new_type_curve(T_BASE);
}

#line 136 "codegen/curve.override"

/* inserted verbatim at the end */

/* nsp_draw_curve: drawing function for a curve 
 * WIP: dashed line style to be used 
 *      make a colored line with colors along the curve 
 *      or arrow color along the line 
 */

static void nsp_draw_curve(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  NspCurve *P = (NspCurve *) Obj;
  int xmark[2]={-1,-1},mode,obj_color=P->obj->color ;
  NspMatrix *M = P->obj->Pts;
  int c_width =  Xgc->graphic_engine->xget_thickness(Xgc);
  int c_color = Xgc->graphic_engine->xget_color(Xgc);

  if (Obj->obj->show == FALSE ) return;

  /* check if the block is inside drawing rectangle
   */

  if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
    {
      return ;
    }

  if ( P->obj->Pts->m == 0) return;
  Xgc->graphic_engine->xset_thickness(Xgc,P->obj->width);
  if ( P->obj->mark >= -1 )
    {
      /* we will use a mark */
      Xgc->graphic_engine->xget_mark(Xgc,xmark);
      if ( P->obj->mark_size >= 0 )
	Xgc->graphic_engine->xset_mark(Xgc, P->obj->mark,P->obj->mark_size);
      else
	Xgc->graphic_engine->xset_mark(Xgc, P->obj->mark,xmark[1]);
    }
  mode = P->obj->mode;
 more :
  switch ( mode )
    {
    case curve_std:
      if ( P->obj->color >= -1 )
	{
	  if ( P->obj->color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, P->obj->color);
	  Xgc->graphic_engine->scale->drawpolyline(Xgc,M->R,M->R+M->m,M->m,0);
	  if ( P->obj->color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, c_color);
	}
      if ( P->obj->mark >= -1 )
	{
	  if ( P->obj->mark_color >= 0) Xgc->graphic_engine->xset_color(Xgc, P->obj->mark_color);
	  Xgc->graphic_engine->scale->drawpolymark(Xgc,M->R,M->R+M->m,M->m);
	  if ( P->obj->mark_color >= 0) Xgc->graphic_engine->xset_color(Xgc, c_color);
	}
      break;
    case curve_stairs:
      /* will just draw */
      nsp_curve_stairs_fill_basic(Xgc,P,M);
      break;
    case curve_stairs_fill:
      /* draw and fill with special algo for opengl */
      nsp_curve_stairs_fill(Xgc,P,M);
      if ( P->obj->mode == curve_stairs_fill ) { mode = curve_stem; obj_color=-1;goto more;};
      break;
    case curve_stem:
      {
	double xm[2],vx[2],ym[2],vy[2];
	int i;
	if ( M->m == 0) break;
	if ( obj_color >= 0) Xgc->graphic_engine->xset_color(Xgc, obj_color);
	for ( i=0 ; i < M->m ; i++)
	  {
	    xm[0]= M->R[i];
	    ym[0]= 0;
	    xm[1]= M->R[i];
	    ym[1]= M->R[i+M->m];
	    scale_double_to_pixels(Xgc->scales,xm,ym,vx,vy,2);
	    Xgc->graphic_engine->drawline(Xgc,vx[0],vy[0],vx[1],vy[1]);
	  }
	if ( obj_color >= 0) Xgc->graphic_engine->xset_color(Xgc, c_color);
      }
      break;
    case curve_arrow:
      {
	double *xm=NULL,*ym=NULL,arsize1,arsize2,arsize;
	int n= 2*M->m,i;
	if ( n == 0 ) break;
	xm = graphic_alloc(0,n,sizeof(double));
	ym = graphic_alloc(1,n,sizeof(double));
	if ( xm == 0 || ym == 0)
	  {
	    Sciprintf("Error: cannot allocate points for drawing\n");
	    return;
	  }
	/* size of arrow */
	arsize1= Abs(Xgc->scales->frect[2]-Xgc->scales->frect[0])/40.;
	arsize2= Abs(Xgc->scales->frect[3]-Xgc->scales->frect[1])/40.;
	arsize=  Min(arsize1, arsize2);
	for ( i=0 ; i < M->m -1 ; i++)
	  {
	    xm[2*i]= M->R[i];
	    ym[2*i]= M->R[i+M->m] ;
	    xm[2*i+1]= M->R[i+1];
	    ym[2*i+1]= M->R[i+M->m+1];
	  }
	Xgc->graphic_engine->scale->drawarrows(Xgc,xm,ym,n,arsize,&P->obj->color,0);
      }
      break;
    case curve_fill:
      nsp_curve_fill(Xgc,P,M);
      break;
    }

  Xgc->graphic_engine->xset_thickness(Xgc,c_width);
  Xgc->graphic_engine->xset_color(Xgc,c_color);
  if ( P->obj->mark >= 0)
    {
      Xgc->graphic_engine->xset_mark(Xgc,xmark[0],xmark[1]);
    }
}

static void nsp_translate_curve(NspGraphic *Obj,const double *tr)
{
  int i;
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < M->m ; i++)
    {
      *(x++) += tr[0];
      *(y++) += tr[1];
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);

}

static void nsp_rotate_curve(NspGraphic *Obj,double *R)
{
  int i;
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m,x1,y1;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < M->m ; i++)
    {
      x1 = R[0]*(*x) -R[1]*(*y);
      y1 = R[1]*(*x) +R[0]*(*y);
      *(x++) =x1;
      *(y++) =y1;
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_curve(NspGraphic *Obj,double *alpha)
{
  int i;
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < M->m ; i++)
    {
      *(x++) *= alpha[0];
      *(y++) *= alpha[1];
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of curve
 *
 */

static int nsp_getbounds_curve(NspGraphic *Obj,double *bounds)
{
  NspCurve *P = (NspCurve *) Obj;
  NspMatrix *M = P->obj->Pts;
  double *x=M->R,*y= M->R+M->m;
  if ( M->mn == 0) return FALSE;
  bounds[0]= Mini(x,M->m);
  bounds[1]= Mini(y,M->m);
  bounds[2]= Maxi(x,M->m);
  bounds[3]= Maxi(y,M->m);
  return TRUE;
}

/*  */
extern Gengine GL_gengine;

/* try to partially solve opengl pbs  */



static void nsp_curve_fill(BCG *Xgc,NspCurve *C,NspMatrix *M)
{
#ifdef  WITH_OPENGL
  if ( Xgc->graphic_engine == &GL_gengine )
    /* if we are using OpenGl we need to detect the convex parts */
    {
      nsp_curve_fill_ext(Xgc,C,M);
    }
  else
    {
      nsp_curve_fill_basic(Xgc,C,M);
    }
#else
  nsp_curve_fill_basic(Xgc,C,M);
#endif
}

static void nsp_curve_fill_basic(BCG *Xgc,NspCurve *C,NspMatrix *M)
{
  double *xm=NULL,*ym=NULL;
  int n= M->m+2;
  if (M->m == 0) return ;
  xm = graphic_alloc(0,n,sizeof(double));
  ym = graphic_alloc(1,n,sizeof(double));
  if ( xm == 0 || ym == 0)
    {
      Sciprintf("Error: cannot allocate points for drawing\n");
      return;
    }
  memcpy(xm,M->R,M->m*sizeof(double));
  memcpy(ym,M->R+M->m,M->m*sizeof(double));
  xm[M->m]= M->R[M->m-1];
  ym[M->m]=0.0;
  xm[M->m+1]= M->R[0];
  ym[M->m+1]=0.0;
  if ( C->obj->color >= -1 )
    {
      int c_color=0;
      if ( C->obj->color >= 0 ) c_color= Xgc->graphic_engine->xset_color(Xgc, C->obj->color);
      Xgc->graphic_engine->scale->fillpolyline(Xgc,xm,ym,n,0);
      if ( C->obj->color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, c_color);
    }
  Xgc->graphic_engine->scale->drawpolyline(Xgc,xm,ym,n,0);
}

#ifdef  WITH_OPENGL
static void nsp_curve_fill_ext(BCG *Xgc,NspCurve *C,NspMatrix *M)
{
  double xi ;
  int c_color = Xgc->graphic_engine->xget_color(Xgc);
  int start=0;
  if (M->m == 0) return;
  xi=M->R[0];
  while (1)
    {
      start=nsp_curve_fill_part(Xgc,C,M,start,&xi);
      if ( start >= M->m) break;
    }
  if ( C->obj->color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, c_color);
}

static int nsp_curve_fill_part(BCG *Xgc,NspCurve *C, NspMatrix *M, int start,double *xi)
{
  int p,P,sign;
  double *xm=NULL,*ym=NULL;
  int n= M->m+2;
  xm = graphic_alloc(0,n,sizeof(double));
  ym = graphic_alloc(1,n,sizeof(double));
  if ( xm == 0 || ym == 0)
    {
      Sciprintf("Error: cannot allocate points for drawing\n");
      return M->m;
    }
  /* first point y=0 */
  p = 0; P = start;
  xm[p]= *xi;
  ym[p]= 0.0;
  p++;
  while (1)
    {
      if ( M->R[P+M->m] != 0.0 ) break;
      P++;
      if ( P >= M->m) return P;
    }
  /* we are at the first non null point */
  sign = (M->R[P+M->m]) >= 0;
  while (1)
    {
      /* accumulate points until sign changes */
      xm[p]= M->R[P];
      ym[p]= M->R[P+M->m];
      if ( ((ym[p]) >= 0) != sign )
	{
	  double alpha = (0- ym[p-1])/(ym[p]-ym[p-1]);
	  *xi = (1-alpha)*xm[p-1] + alpha*xm[p];
	  xm[p]=*xi;
	  ym[p]=0.0;
	  p++;
	  break;
	}
      p++;P++;
      if ( P >= M->m)
	{
	  /* we need a last point */
	  xm[p]= xm[p-1];
	  ym[p]= 0.0;
	  p++;
	  break;
	}
    }
  /* */
  if ( C->obj->color >= -1 )
    {
      int c_color=0;
      if ( C->obj->color >= 0 ) c_color= Xgc->graphic_engine->xset_color(Xgc, C->obj->color);
      Xgc->graphic_engine->scale->fillpolyline(Xgc,xm,ym,p,1);
      if ( C->obj->color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, c_color);
    }
  Xgc->graphic_engine->scale->drawpolyline(Xgc,xm,ym,p,0);
  return P;
}

static NspMatrix *nsp_curve_stairs_alloc(NspMatrix *M)
{
  double *xm,*ym;
  int i, n= 2*M->m -1;
  NspMatrix *loc;
  if ((loc = nsp_matrix_create(NVOID,'r',n,2)) == NULLMAT)
    return NULLMAT;
  xm= loc->R; ym= loc->R + loc->m;
  for ( i=0 ; i < M->m -1 ; i++)
    {
      xm[2*i]= M->R[i];
      ym[2*i]= M->R[i+M->m];
      xm[2*i+1]= M->R[i+1];
      ym[2*i+1]= ym[2*i];
    }
  xm[2*(M->m-1)] = M->R[M->m-1];
  ym[2*(M->m-1)] = M->R[M->m-1+M->m];
  return loc;
}
#endif 




static void nsp_curve_stairs_fill(BCG *Xgc,NspCurve *P,NspMatrix *M)
{
#ifdef  WITH_OPENGL
  if ( Xgc->graphic_engine == &GL_gengine )
    /* if we are using OpenGl we need to detect the convex parts */
    {
      NspMatrix *loc;
      if ((loc = nsp_curve_stairs_alloc(M))== NULL) return;
      nsp_curve_fill_ext(Xgc,P,loc);
      nsp_matrix_destroy(loc);
    }
  else
    {
      nsp_curve_stairs_fill_basic(Xgc,P,M);
    }
#else
  nsp_curve_stairs_fill_basic(Xgc,P,M);
#endif
}

static void nsp_curve_stairs_fill_basic(BCG *Xgc,NspCurve *P,NspMatrix *M)
{
  int c_color = Xgc->graphic_engine->xget_color(Xgc);
  double *xm=NULL,*ym=NULL;
  int n= ( P->obj->mode == curve_stairs ) ? 2*M->m -1 : 2*M->m+1 ,i;
  /* stroke color */
  int color= ( P->obj->mode == curve_stairs ) ? P->obj->color : -1;
  xm = graphic_alloc(0,n,sizeof(double));
  ym = graphic_alloc(1,n,sizeof(double));
  if ( xm == 0 || ym == 0)
    {
      Sciprintf("Error: cannot allocate points for drawing\n");
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
  if ( P->obj->mode == curve_stairs_fill )
    {
      /* fill the stairs */
      xm[2*(M->m)-1]= M->R[M->m-1];
      ym[2*(M->m)-1]= 0.0;
      xm[2*(M->m)]= M->R[0];
      ym[2*(M->m)]=0.0;
      if ( P->obj->color >= -1 )
	{
	  if ( P->obj->color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, P->obj->color);
	  Xgc->graphic_engine->scale->fillpolyline(Xgc,xm,ym,n,0);
	  if ( P->obj->color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, c_color);
	}
    }

  if ( color >= -1 )
    {
      if ( color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, color);
      Xgc->graphic_engine->scale->drawpolyline(Xgc,xm,ym,2*M->m-1,0);
      if ( color >= 0 ) Xgc->graphic_engine->xset_color(Xgc, c_color);
    }
  if ( P->obj->mark >= -1 )
    {
      if ( P->obj->mark_color >= 0) Xgc->graphic_engine->xset_color(Xgc, P->obj->mark_color);
      Xgc->graphic_engine->scale->drawpolymark(Xgc,xm,ym,2*M->m-1);
      if ( P->obj->mark_color >= 0) Xgc->graphic_engine->xset_color(Xgc, c_color);
    }
}

#line 1280 "curve.c"
