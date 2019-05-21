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





#line 69 "codegen/axes.override"
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/objs3d.h>
#include <nsp/curve.h>

#ifdef  WITH_OPENGL
extern Gengine GL_gengine;
#endif

#line 38 "axes.c"

/* -----------NspAxes ----------- */


#define  NspAxes_Private 
#include <nsp/objects.h>
#include <nsp/axes.h>
#include <nsp/interf.h>

/* 
 * NspAxes inherits from Graphic 
 */

int nsp_type_axes_id=0;
NspTypeAxes *nsp_type_axes=NULL;

/*
 * Type object for NspAxes 
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
  if (( type =  malloc(sizeof(NspTypeAxes))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = axes_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = axes_get_methods;
  type->gtk_methods = FALSE;
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
  top->info = (info_func *) nsp_axes_info;
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
  top->full_copy = (copy_func *) nsp_axes_full_copy;

  /* specific methods for axes */

  type->init = (init_func *) init_axes;

#line 85 "codegen/axes.override"
  /* inserted verbatim in the type definition */
  type->gtk_methods = TRUE;
  /* here we override the method or its father class i.e Graphic */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_axes;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_axes ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_axes  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_axes  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_axes  ;
  ((NspTypeGraphic *) type->surtype)->link_figure = nsp_axes_link_figure;
  ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_axes_unlink_figure;
  ((NspTypeGraphic *) type->surtype)->children = (children_func *) nsp_axes_children ;
  ((NspTypeGraphic *) type->surtype)->invalidate = nsp_axes_invalidate;

#line 125 "axes.c"
  /* 
   * NspAxes interfaces can be added here 
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
 * initialize NspAxes instances 
 * locally and by calling initializer on parent class 
 */

static int init_axes(NspAxes *Obj,NspTypeAxes *type)
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
 * new instance of NspAxes 
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
 * Object method redefined for NspAxes 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_axes_size(NspAxes *Mat, int flag)
{
  return 1;
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
  if ( nsp_eq_nsp_gcscale(&A->obj->scale,&loc->obj->scale)== FALSE) return FALSE;
  if ( NSP_OBJECT(A->obj->wrect)->type->eq(A->obj->wrect,loc->obj->wrect) == FALSE ) return FALSE;
  if ( A->obj->rho != loc->obj->rho) return FALSE;
  if ( A->obj->top != loc->obj->top) return FALSE;
  if ( NSP_OBJECT(A->obj->bounds)->type->eq(A->obj->bounds,loc->obj->bounds) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->arect)->type->eq(A->obj->arect,loc->obj->arect) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->frect)->type->eq(A->obj->frect,loc->obj->frect) == FALSE ) return FALSE;
  if ( strcmp(A->obj->title,loc->obj->title) != 0) return FALSE;
  if ( strcmp(A->obj->x,loc->obj->x) != 0) return FALSE;
  if ( strcmp(A->obj->y,loc->obj->y) != 0) return FALSE;
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  if ( A->obj->fixed != loc->obj->fixed) return FALSE;
  if ( A->obj->iso != loc->obj->iso) return FALSE;
  if ( A->obj->auto_axis != loc->obj->auto_axis) return FALSE;
  if ( A->obj->grid != loc->obj->grid) return FALSE;
  if ( A->obj->axes != loc->obj->axes) return FALSE;
  if ( A->obj->xlog != loc->obj->xlog) return FALSE;
  if ( A->obj->ylog != loc->obj->ylog) return FALSE;
  if ( A->obj->lpos != loc->obj->lpos) return FALSE;
  if ( NSP_OBJECT(A->obj->rect)->type->eq(A->obj->rect,loc->obj->rect) == FALSE ) return FALSE;
  if ( A->obj->zoom != loc->obj->zoom) return FALSE;
  if ( NSP_OBJECT(A->obj->zrect)->type->eq(A->obj->zrect,loc->obj->zrect) == FALSE ) return FALSE;
  if ( A->obj->clip != loc->obj->clip) return FALSE;
  if ( A->obj->line_width != loc->obj->line_width) return FALSE;
  if ( A->obj->font_size != loc->obj->font_size) return FALSE;
  if ( A->obj->background != loc->obj->background) return FALSE;
  if ( NSP_OBJECT(A->obj->nax)->type->eq(A->obj->nax,loc->obj->nax) == FALSE ) return FALSE;
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
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_axes)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->wrect)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->rho) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->top) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->arect)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->frect)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->title) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->x) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->y) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->fixed) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->iso) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->auto_axis) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->grid) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->axes) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->xlog) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->ylog) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->lpos) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->rect)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->zoom) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->clip) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->line_width) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->font_size) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->background) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->nax)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAxes  *nsp_axes_xdr_load_partial(XDR *xdrs, NspAxes *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ((M->obj->wrect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->rho) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->top) == FAIL) return NULL;
  if ((M->obj->arect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->frect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->title)) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->x)) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->y)) == FAIL) return NULL;
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->fixed) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->iso) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->auto_axis) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->grid) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->axes) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->xlog) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->ylog) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->lpos) == FAIL) return NULL;
  if ((M->obj->rect =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->zoom) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->clip) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->line_width) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->font_size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->background) == FAIL) return NULL;
  if ((M->obj->nax =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic * )M) == NULL) return NULL;
 return M;
}

static NspAxes  *nsp_axes_xdr_load(XDR *xdrs)
{
  NspAxes *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLAXES;
  if ((H  = nsp_axes_create_void(name,(NspTypeBase *) nsp_type_axes))== NULLAXES) return H;
  if ( nsp_axes_create_partial(H) == FAIL) return NULLAXES;
  if ((H  = nsp_axes_xdr_load_partial(xdrs,H))== NULLAXES) return H;
  if ( nsp_axes_check_values(H) == FAIL) return NULLAXES;
  return H;
}

/*
 * delete 
 */

void nsp_axes_destroy_partial(NspAxes *H)
{
  nsp_graphic_destroy_partial((NspGraphic * ) H);
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_destroy_nsp_gcscale(&H->obj->scale,H); 
    if ( H->obj->wrect != NULL ) 
      nsp_matrix_destroy(H->obj->wrect);
    if ( H->obj->bounds != NULL ) 
      nsp_matrix_destroy(H->obj->bounds);
    if ( H->obj->arect != NULL ) 
      nsp_matrix_destroy(H->obj->arect);
    if ( H->obj->frect != NULL ) 
      nsp_matrix_destroy(H->obj->frect);
  nsp_string_destroy(&(H->obj->title));
  nsp_string_destroy(&(H->obj->x));
  nsp_string_destroy(&(H->obj->y));
    if ( H->obj->children != NULL ) 
      nsp_list_destroy(H->obj->children);
    if ( H->obj->rect != NULL ) 
      nsp_matrix_destroy(H->obj->rect);
    if ( H->obj->zrect != NULL ) 
      nsp_matrix_destroy(H->obj->zrect);
    if ( H->obj->nax != NULL ) 
      nsp_matrix_destroy(H->obj->nax);
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

int nsp_axes_info(NspAxes *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLAXES) 
    {
      Sciprintf("Null Pointer NspAxes \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_axes_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_axes_print(NspAxes *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLAXES) 
    {
      Sciprintf("Null Pointer NspAxes \n");
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
          nsp_axes_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_axes_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  nsp_print_nsp_gcscale(indent+2,&M->obj->scale,M);
  if ( M->obj->wrect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->wrect),indent+2,"wrect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"rho=%f\n", M->obj->rho);
  Sciprintf1(indent+2,"top	= %s\n", ( M->obj->top == TRUE) ? "T" : "F" );
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->bounds),indent+2,"bounds", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->arect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->arect),indent+2,"arect", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->frect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->frect),indent+2,"frect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"title=%s\n",M->obj->title);
  Sciprintf1(indent+2,"x=%s\n",M->obj->x);
  Sciprintf1(indent+2,"y=%s\n",M->obj->y);
  if ( M->obj->children != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"fixed	= %s\n", ( M->obj->fixed == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"iso	= %s\n", ( M->obj->iso == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"auto_axis	= %s\n", ( M->obj->auto_axis == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"grid=%d\n", M->obj->grid);
  Sciprintf1(indent+2,"axes=%d\n", M->obj->axes);
  Sciprintf1(indent+2,"xlog	= %s\n", ( M->obj->xlog == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"ylog	= %s\n", ( M->obj->ylog == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"lpos=%d\n", M->obj->lpos);
  if ( M->obj->rect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->rect),indent+2,"rect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"zoom	= %s\n", ( M->obj->zoom == TRUE) ? "T" : "F" );
  if ( M->obj->zrect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->zrect),indent+2,"zrect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"clip	= %s\n", ( M->obj->clip == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"line_width=%d\n", M->obj->line_width);
  Sciprintf1(indent+2,"font_size=%d\n", M->obj->font_size);
  Sciprintf1(indent+2,"background=%d\n", M->obj->background);
  if ( M->obj->nax != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->nax),indent+2,"nax", rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_axes_latex(NspAxes *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_axes_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  nsp_print_nsp_gcscale(indent+2,&M->obj->scale,M);
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->wrect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->wrect),FALSE,"wrect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|rho| = \\numprint{%f}\n", M->obj->rho);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|top|= %s\n",( M->obj->top == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->bounds),FALSE,"bounds", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->arect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->arect),FALSE,"arect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->frect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->frect),FALSE,"frect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|title|=\\verb@\"%s\"@\n",(M->obj->title==NULL) ? "NULL": M->obj->title);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|x|=\\verb@\"%s\"@\n",(M->obj->x==NULL) ? "NULL": M->obj->x);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|y|=\\verb@\"%s\"@\n",(M->obj->y==NULL) ? "NULL": M->obj->y);
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->children != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->children),FALSE,"children", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|fixed|= %s\n",( M->obj->fixed == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|iso|= %s\n",( M->obj->iso == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|auto_axis|= %s\n",( M->obj->auto_axis == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|grid|= \\numprint{%d}\n",M->obj->grid);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|axes|= \\numprint{%d}\n",M->obj->axes);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|xlog|= %s\n",( M->obj->xlog == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|ylog|= %s\n",( M->obj->ylog == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|lpos|= \\numprint{%d}\n",M->obj->lpos);
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->rect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->rect),FALSE,"rect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|zoom|= %s\n",( M->obj->zoom == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->zrect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->zrect),FALSE,"zrect", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|clip|= %s\n",( M->obj->clip == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|line_width|= \\numprint{%d}\n",M->obj->line_width);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|font_size|= \\numprint{%d}\n",M->obj->font_size);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|background|= \\numprint{%d}\n",M->obj->background);
  Sciprintf1(2,"\\\\\n");
  if ( M->obj->nax != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->nax),FALSE,"nax", rec_level+1)== FALSE ) return FALSE ;
    }
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
 * for NspAxes objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAxes   *nsp_axes_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_axes_id)  == TRUE  ) return ((NspAxes *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_axes));
  return NULL;
}

int IsAxesObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_axes_id);
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
 * create a NspAxes instance 
 *-----------------------------------------------------*/

static NspAxes *nsp_axes_create_void(const char *name,NspTypeBase *type)
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
  if ( nsp_graphic_create_partial((NspGraphic * ) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_axes)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  nsp_init_nsp_gcscale(&H->obj->scale);
  H->obj->wrect = NULLMAT;
  H->obj->rho = 0.0;
  H->obj->top = TRUE;
  H->obj->bounds = NULLMAT;
  H->obj->arect = NULLMAT;
  H->obj->frect = NULLMAT;
  H->obj->title = NULL;
  H->obj->x = NULL;
  H->obj->y = NULL;
  H->obj->children = NULLLIST;
  H->obj->fixed = FALSE;
  H->obj->iso = FALSE;
  H->obj->auto_axis = TRUE;
  H->obj->grid = -1;
  H->obj->axes = 1;
  H->obj->xlog = FALSE;
  H->obj->ylog = FALSE;
  H->obj->lpos = 1;
  H->obj->rect = NULLMAT;
  H->obj->zoom = FALSE;
  H->obj->zrect = NULLMAT;
  H->obj->clip = TRUE;
  H->obj->line_width = -1;
  H->obj->font_size = -1;
  H->obj->background = -1;
  H->obj->nax = NULLMAT;
  return OK;
}

int nsp_axes_check_values(NspAxes *H)
{
  if ( nsp_check_nsp_gcscale(&H->obj->scale,H) == FAIL ) return FAIL;
  if ( H->obj->wrect == NULLMAT) 
    {
     double x_def[4]={0,0,1,1};
     if (( H->obj->wrect = nsp_matrix_create("wrect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->wrect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->bounds == NULLMAT) 
    {
     double x_def[4]={0,0,0,0};
     if (( H->obj->bounds = nsp_matrix_create("bounds",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->bounds->R,x_def,4*sizeof(double));
  }
  if ( H->obj->arect == NULLMAT) 
    {
     double x_def[4]={1/8.0,1/8.0,1/8.0,1/8.0};
     if (( H->obj->arect = nsp_matrix_create("arect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->arect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->frect == NULLMAT) 
    {
     double x_def[4]={0,0,1,1};
     if (( H->obj->frect = nsp_matrix_create("frect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->frect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->title == NULL) 
    {
  if (( H->obj->title = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->x == NULL) 
    {
  if (( H->obj->x = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->y == NULL) 
    {
  if (( H->obj->y = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->obj->children == NULLLIST) 
    {
     if (( H->obj->children = nsp_list_create("children")) == NULLLIST)
       return FAIL;
    }
  if ( H->obj->rect == NULLMAT) 
    {
     double x_def[4]={0,0,1,1};
     if (( H->obj->rect = nsp_matrix_create("rect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->rect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->zrect == NULLMAT) 
    {
     double x_def[4]={0,0,1,1};
     if (( H->obj->zrect = nsp_matrix_create("zrect",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->zrect->R,x_def,4*sizeof(double));
  }
  if ( H->obj->nax == NULLMAT) 
    {
     double x_def[4]={2,10,2,10};
     if (( H->obj->nax = nsp_matrix_create("nax",'r',1,4)) == NULLMAT)
       return FAIL;
      memcpy(H->obj->nax->R,x_def,4*sizeof(double));
  }
  nsp_graphic_check_values((NspGraphic * ) H);
  return OK;
}

NspAxes *nsp_axes_create(const char *name,nsp_gcscale scale,NspMatrix* wrect,double rho,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,char* x,char* y,NspList* children,gboolean fixed,gboolean iso,gboolean auto_axis,int grid,int axes,gboolean xlog,gboolean ylog,int lpos,NspMatrix* rect,gboolean zoom,NspMatrix* zrect,gboolean clip,int line_width,int font_size,int background,NspMatrix* nax,NspTypeBase *type)
{
  NspAxes *H  = nsp_axes_create_void(name,type);
  if ( H ==  NULLAXES) return NULLAXES;
  if ( nsp_axes_create_partial(H) == FAIL) return NULLAXES;
  H->obj->scale = scale;
  H->obj->wrect= wrect;
  H->obj->rho=rho;
  H->obj->top=top;
  H->obj->bounds= bounds;
  H->obj->arect= arect;
  H->obj->frect= frect;
  H->obj->title = title;
  H->obj->x = x;
  H->obj->y = y;
  H->obj->children= children;
  H->obj->fixed=fixed;
  H->obj->iso=iso;
  H->obj->auto_axis=auto_axis;
  H->obj->grid=grid;
  H->obj->axes=axes;
  H->obj->xlog=xlog;
  H->obj->ylog=ylog;
  H->obj->lpos=lpos;
  H->obj->rect= rect;
  H->obj->zoom=zoom;
  H->obj->zrect= zrect;
  H->obj->clip=clip;
  H->obj->line_width=line_width;
  H->obj->font_size=font_size;
  H->obj->background=background;
  H->obj->nax= nax;
  if ( nsp_axes_check_values(H) == FAIL) return NULLAXES;
  return H;
}


NspAxes *nsp_axes_create_default(const char *name)
{
 NspAxes *H  = nsp_axes_create_void(name,NULL);
 if ( H ==  NULLAXES) return NULLAXES;
  if ( nsp_axes_create_partial(H) == FAIL) return NULLAXES;
  if ( nsp_axes_check_values(H) == FAIL) return NULLAXES;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAxes *nsp_axes_copy_partial(NspAxes *H,NspAxes *self)
{
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLAXES;
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspAxes *nsp_axes_copy(NspAxes *self)
{
  NspAxes *H  =nsp_axes_create_void(NVOID,(NspTypeBase *) nsp_type_axes);
  if ( H ==  NULLAXES) return NULLAXES;
  if ( nsp_axes_copy_partial(H,self)== NULL) return NULLAXES;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspAxes *nsp_axes_full_copy_partial(NspAxes *H,NspAxes *self)
{
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLAXES;
  if ((H->obj = calloc(1,sizeof(nsp_axes))) == NULL) return NULLAXES;
  H->obj->ref_count=1;
  if( nsp_nsp_gcscale_full_copy(H,&H->obj->scale,self)== FAIL) return NULL;
  if ( self->obj->wrect == NULL )
    { H->obj->wrect = NULL;}
  else
    {
      if ((H->obj->wrect = (NspMatrix *) nsp_object_full_copy_and_name("wrect", NSP_OBJECT(self->obj->wrect))) == NULLMAT) return NULL;
    }
  H->obj->rho=self->obj->rho;
  H->obj->top=self->obj->top;
  if ( self->obj->bounds == NULL )
    { H->obj->bounds = NULL;}
  else
    {
      if ((H->obj->bounds = (NspMatrix *) nsp_object_full_copy_and_name("bounds", NSP_OBJECT(self->obj->bounds))) == NULLMAT) return NULL;
    }
  if ( self->obj->arect == NULL )
    { H->obj->arect = NULL;}
  else
    {
      if ((H->obj->arect = (NspMatrix *) nsp_object_full_copy_and_name("arect", NSP_OBJECT(self->obj->arect))) == NULLMAT) return NULL;
    }
  if ( self->obj->frect == NULL )
    { H->obj->frect = NULL;}
  else
    {
      if ((H->obj->frect = (NspMatrix *) nsp_object_full_copy_and_name("frect", NSP_OBJECT(self->obj->frect))) == NULLMAT) return NULL;
    }
  if ((H->obj->title = nsp_string_copy(self->obj->title)) == NULL) return NULL;
  if ((H->obj->x = nsp_string_copy(self->obj->x)) == NULL) return NULL;
  if ((H->obj->y = nsp_string_copy(self->obj->y)) == NULL) return NULL;
  if ( self->obj->children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *) nsp_object_full_copy_and_name("children", NSP_OBJECT(self->obj->children))) == NULLLIST) return NULL;
    }
  H->obj->fixed=self->obj->fixed;
  H->obj->iso=self->obj->iso;
  H->obj->auto_axis=self->obj->auto_axis;
  H->obj->grid=self->obj->grid;
  H->obj->axes=self->obj->axes;
  H->obj->xlog=self->obj->xlog;
  H->obj->ylog=self->obj->ylog;
  H->obj->lpos=self->obj->lpos;
  if ( self->obj->rect == NULL )
    { H->obj->rect = NULL;}
  else
    {
      if ((H->obj->rect = (NspMatrix *) nsp_object_full_copy_and_name("rect", NSP_OBJECT(self->obj->rect))) == NULLMAT) return NULL;
    }
  H->obj->zoom=self->obj->zoom;
  if ( self->obj->zrect == NULL )
    { H->obj->zrect = NULL;}
  else
    {
      if ((H->obj->zrect = (NspMatrix *) nsp_object_full_copy_and_name("zrect", NSP_OBJECT(self->obj->zrect))) == NULLMAT) return NULL;
    }
  H->obj->clip=self->obj->clip;
  H->obj->line_width=self->obj->line_width;
  H->obj->font_size=self->obj->font_size;
  H->obj->background=self->obj->background;
  if ( self->obj->nax == NULL )
    { H->obj->nax = NULL;}
  else
    {
      if ((H->obj->nax = (NspMatrix *) nsp_object_full_copy_and_name("nax", NSP_OBJECT(self->obj->nax))) == NULLMAT) return NULL;
    }
  return H;
}

NspAxes *nsp_axes_full_copy(NspAxes *self)
{
  NspAxes *H  =nsp_axes_create_void(NVOID,(NspTypeBase *) nsp_type_axes);
  if ( H ==  NULLAXES) return NULLAXES;
  if ( nsp_axes_full_copy_partial(H,self)== NULL) return NULLAXES;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspAxes
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
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_axes_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *axes_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_axes_get_wrect(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspAxes *) self)->obj->wrect;
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_wrect(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAxes *) self)->obj->wrect);
  return (NspObject *) ret;
}

static int _wrap_axes_set_wrect(void *self,const char *attr, NspObject *O)
{
  NspMatrix *wrect;
  if ( ! IsMat(O) ) return FAIL;
  if ((wrect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAxes *) self)->obj->wrect != NULL ) 
    nsp_matrix_destroy(((NspAxes *) self)->obj->wrect);
  ((NspAxes *) self)->obj->wrect= wrect;
  return OK;
}

#line 108 "codegen/axes.override"
/* override set rho */
static int _wrap_axes_set_rho(void *self, char *attr, NspObject *O)
{
  double rho;
  if ( DoubleScalar(O,&rho) == FAIL) return FAIL;

  if ( ((NspAxes *) self)->obj->rho != rho)
    {
      ((NspAxes *) self)->obj->rho = rho;
      nsp_axes_invalidate((NspGraphic *) self);
    }
  return OK;
}

#line 978 "axes.c"
static NspObject *_wrap_axes_get_rho(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->rho;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_axes_get_top(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->top;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
return nsp_ret;
}

static int _wrap_axes_set_top(void *self,const char *attr, NspObject *O)
{
  int top;
  if ( BoolScalar(O,&top) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->top= top;
  return OK;
}

static NspObject *_wrap_axes_get_arect(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspAxes *) self)->obj->arect;
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_arect(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAxes *) self)->obj->arect);
  return (NspObject *) ret;
}

static int _wrap_axes_set_arect(void *self,const char *attr, NspObject *O)
{
  NspMatrix *arect;
  if ( ! IsMat(O) ) return FAIL;
  if ((arect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAxes *) self)->obj->arect != NULL ) 
    nsp_matrix_destroy(((NspAxes *) self)->obj->arect);
  ((NspAxes *) self)->obj->arect= arect;
  return OK;
}

static NspObject *_wrap_axes_get_frect(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspAxes *) self)->obj->frect;
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_frect(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAxes *) self)->obj->frect);
  return (NspObject *) ret;
}

static int _wrap_axes_set_frect(void *self,const char *attr, NspObject *O)
{
  NspMatrix *frect;
  if ( ! IsMat(O) ) return FAIL;
  if ((frect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAxes *) self)->obj->frect != NULL ) 
    nsp_matrix_destroy(((NspAxes *) self)->obj->frect);
  ((NspAxes *) self)->obj->frect= frect;
  return OK;
}

static NspObject *_wrap_axes_get_title(void *self,const char *attr)
{
  NspObject *nsp_ret;
  const gchar *ret;
  ret = ((NspAxes *) self)->obj->title;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_axes_set_title(void *self,const char *attr, NspObject *O)
{
  char *title;
  if ((title = nsp_string_object(O))==NULL) return FAIL;
  if ((title = nsp_string_copy(title)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspAxes *) self)->obj->title);
  ((NspAxes *) self)->obj->title= title;
  return OK;
}

static NspObject *_wrap_axes_get_x(void *self,const char *attr)
{
  NspObject *nsp_ret;
  const gchar *ret;
  ret = ((NspAxes *) self)->obj->x;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_axes_set_x(void *self,const char *attr, NspObject *O)
{
  char *x;
  if ((x = nsp_string_object(O))==NULL) return FAIL;
  if ((x = nsp_string_copy(x)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspAxes *) self)->obj->x);
  ((NspAxes *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_axes_get_y(void *self,const char *attr)
{
  NspObject *nsp_ret;
  const gchar *ret;
  ret = ((NspAxes *) self)->obj->y;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_axes_set_y(void *self,const char *attr, NspObject *O)
{
  char *y;
  if ((y = nsp_string_object(O))==NULL) return FAIL;
  if ((y = nsp_string_copy(y)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspAxes *) self)->obj->y);
  ((NspAxes *) self)->obj->y= y;
  return OK;
}

#line 124 "codegen/axes.override"

/* here we override get_obj  and set_obj
 * we want a get to be followed by a set to check that
 * inserted value is correct thus we use copy = TRUE.
 */

static NspObject *_wrap_axes_get_obj_children(void *self,char *attr, int *copy)
{
  NspList *ret;
  *copy = TRUE;
  ret = ((NspList*) ((NspAxes *) self)->obj->children);
  return (NspObject *) ret;
}

/* in this function we can check that val is correct before
 * setting the field with val. return FAIL if val is incorrect.
 */

static int _wrap_axes_set_obj_children(void *self,NspObject *val)
{
  if ( ! IsList(val) ) return FAIL;
  if ( nsp_list_check_figure((NspList *) val, ((NspGraphic *) self)->obj->Fig) == FAIL) return FAIL;
  if (((NspAxes *) self)->obj->children != NULL )
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL)
	nsp_list_unlink_figure(((NspAxes *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspAxes *) self)->obj->children);
    }
  ((NspAxes *) self)->obj->children =  (NspList *) val;
  if ( ((NspGraphic *) self)->obj->Fig != NULL)
    nsp_list_link_figure((NspList *) val,((NspGraphic *) self)->obj->Fig,((NspAxes *) self)->obj );
  /* some objects need an Xgc for computing their bounds */
  nsp_axes_compute_inside_bounds(self,((NspAxes *) self)->obj->bounds->R);
  return OK;
}

static int _wrap_axes_set_children(void *self, char *attr, NspObject *O)
{
  NspList *children;
  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspAxes *) self)->obj->children != NULL )
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL)
	nsp_list_unlink_figure(((NspAxes *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspAxes *) self)->obj->children);
    }
  ((NspAxes *) self)->obj->children= children;
  if ( ((NspGraphic *) self)->obj->Fig != NULL)
    nsp_list_link_figure((NspList *) O,((NspGraphic *) self)->obj->Fig,((NspAxes *) self)->obj);
  nsp_axes_compute_inside_bounds(self,((NspAxes *) self)->obj->bounds->R);
  return OK;
}


#line 1170 "axes.c"
static NspObject *_wrap_axes_get_children(void *self,const char *attr)
{
  NspList *ret;
  ret = ((NspAxes *) self)->obj->children;
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_fixed(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->fixed;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
return nsp_ret;
}

static int _wrap_axes_set_fixed(void *self,const char *attr, NspObject *O)
{
  int fixed;
  if ( BoolScalar(O,&fixed) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->fixed= fixed;
  return OK;
}

static NspObject *_wrap_axes_get_iso(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->iso;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
return nsp_ret;
}

static int _wrap_axes_set_iso(void *self,const char *attr, NspObject *O)
{
  int iso;
  if ( BoolScalar(O,&iso) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->iso= iso;
  return OK;
}

static NspObject *_wrap_axes_get_auto_axis(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->auto_axis;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
return nsp_ret;
}

static int _wrap_axes_set_auto_axis(void *self,const char *attr, NspObject *O)
{
  int auto_axis;
  if ( BoolScalar(O,&auto_axis) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->auto_axis= auto_axis;
  return OK;
}

static NspObject *_wrap_axes_get_grid(void *self,const char *attr)
{
  int ret;
  ret = ((NspAxes *) self)->obj->grid;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_axes_set_grid(void *self,const char *attr, NspObject *O)
{
  int grid;
  if ( IntScalar(O,&grid) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->grid= grid;
  return OK;
}

static NspObject *_wrap_axes_get_axes(void *self,const char *attr)
{
  int ret;
  ret = ((NspAxes *) self)->obj->axes;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_axes_set_axes(void *self,const char *attr, NspObject *O)
{
  int axes;
  if ( IntScalar(O,&axes) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->axes= axes;
  return OK;
}

static NspObject *_wrap_axes_get_xlog(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->xlog;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
return nsp_ret;
}

static int _wrap_axes_set_xlog(void *self,const char *attr, NspObject *O)
{
  int xlog;
  if ( BoolScalar(O,&xlog) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->xlog= xlog;
  return OK;
}

static NspObject *_wrap_axes_get_ylog(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->ylog;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
return nsp_ret;
}

static int _wrap_axes_set_ylog(void *self,const char *attr, NspObject *O)
{
  int ylog;
  if ( BoolScalar(O,&ylog) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->ylog= ylog;
  return OK;
}

static NspObject *_wrap_axes_get_lpos(void *self,const char *attr)
{
  int ret;
  ret = ((NspAxes *) self)->obj->lpos;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_axes_set_lpos(void *self,const char *attr, NspObject *O)
{
  int lpos;
  if ( IntScalar(O,&lpos) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->lpos= lpos;
  return OK;
}

static NspObject *_wrap_axes_get_rect(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspAxes *) self)->obj->rect;
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_rect(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAxes *) self)->obj->rect);
  return (NspObject *) ret;
}

static int _wrap_axes_set_rect(void *self,const char *attr, NspObject *O)
{
  NspMatrix *rect;
  if ( ! IsMat(O) ) return FAIL;
  if ((rect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAxes *) self)->obj->rect != NULL ) 
    nsp_matrix_destroy(((NspAxes *) self)->obj->rect);
  ((NspAxes *) self)->obj->rect= rect;
  return OK;
}

static NspObject *_wrap_axes_get_zoom(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->zoom;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
return nsp_ret;
}

static int _wrap_axes_set_zoom(void *self,const char *attr, NspObject *O)
{
  int zoom;
  if ( BoolScalar(O,&zoom) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->zoom= zoom;
  return OK;
}

static NspObject *_wrap_axes_get_clip(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;
  ret = ((NspAxes *) self)->obj->clip;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
return nsp_ret;
}

static int _wrap_axes_set_clip(void *self,const char *attr, NspObject *O)
{
  int clip;
  if ( BoolScalar(O,&clip) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->clip= clip;
  return OK;
}

static NspObject *_wrap_axes_get_line_width(void *self,const char *attr)
{
  int ret;
  ret = ((NspAxes *) self)->obj->line_width;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_axes_set_line_width(void *self,const char *attr, NspObject *O)
{
  int line_width;
  if ( IntScalar(O,&line_width) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->line_width= line_width;
  return OK;
}

static NspObject *_wrap_axes_get_font_size(void *self,const char *attr)
{
  int ret;
  ret = ((NspAxes *) self)->obj->font_size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_axes_set_font_size(void *self,const char *attr, NspObject *O)
{
  int font_size;
  if ( IntScalar(O,&font_size) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->font_size= font_size;
  return OK;
}

static NspObject *_wrap_axes_get_background(void *self,const char *attr)
{
  int ret;
  ret = ((NspAxes *) self)->obj->background;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_axes_set_background(void *self,const char *attr, NspObject *O)
{
  int background;
  if ( IntScalar(O,&background) == FAIL) return FAIL;
  ((NspAxes *) self)->obj->background= background;
  return OK;
}

static NspObject *_wrap_axes_get_nax(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspAxes *) self)->obj->nax;
  return (NspObject *) ret;
}

static NspObject *_wrap_axes_get_obj_nax(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspAxes *) self)->obj->nax);
  return (NspObject *) ret;
}

static int _wrap_axes_set_nax(void *self,const char *attr, NspObject *O)
{
  NspMatrix *nax;
  if ( ! IsMat(O) ) return FAIL;
  if ((nax = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspAxes *) self)->obj->nax != NULL ) 
    nsp_matrix_destroy(((NspAxes *) self)->obj->nax);
  ((NspAxes *) self)->obj->nax= nax;
  return OK;
}

static AttrTab axes_attrs[] = {
  { "wrect", (attr_get_function * )_wrap_axes_get_wrect, (attr_set_function * )_wrap_axes_set_wrect, (attr_get_object_function * )_wrap_axes_get_obj_wrect, (attr_set_object_function * )int_set_object_failed },
  { "rho", (attr_get_function * )_wrap_axes_get_rho, (attr_set_function * )_wrap_axes_set_rho, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "top", (attr_get_function * )_wrap_axes_get_top, (attr_set_function * )_wrap_axes_set_top, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "arect", (attr_get_function * )_wrap_axes_get_arect, (attr_set_function * )_wrap_axes_set_arect, (attr_get_object_function * )_wrap_axes_get_obj_arect, (attr_set_object_function * )int_set_object_failed },
  { "frect", (attr_get_function * )_wrap_axes_get_frect, (attr_set_function * )_wrap_axes_set_frect, (attr_get_object_function * )_wrap_axes_get_obj_frect, (attr_set_object_function * )int_set_object_failed },
  { "title", (attr_get_function * )_wrap_axes_get_title, (attr_set_function * )_wrap_axes_set_title, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "x", (attr_get_function * )_wrap_axes_get_x, (attr_set_function * )_wrap_axes_set_x, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "y", (attr_get_function * )_wrap_axes_get_y, (attr_set_function * )_wrap_axes_set_y, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "children", (attr_get_function * )_wrap_axes_get_children, (attr_set_function * )_wrap_axes_set_children, (attr_get_object_function * )_wrap_axes_get_obj_children, (attr_set_object_function * )_wrap_axes_set_obj_children },
  { "fixed", (attr_get_function * )_wrap_axes_get_fixed, (attr_set_function * )_wrap_axes_set_fixed, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "iso", (attr_get_function * )_wrap_axes_get_iso, (attr_set_function * )_wrap_axes_set_iso, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "auto_axis", (attr_get_function * )_wrap_axes_get_auto_axis, (attr_set_function * )_wrap_axes_set_auto_axis, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "grid", (attr_get_function * )_wrap_axes_get_grid, (attr_set_function * )_wrap_axes_set_grid, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "axes", (attr_get_function * )_wrap_axes_get_axes, (attr_set_function * )_wrap_axes_set_axes, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "xlog", (attr_get_function * )_wrap_axes_get_xlog, (attr_set_function * )_wrap_axes_set_xlog, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "ylog", (attr_get_function * )_wrap_axes_get_ylog, (attr_set_function * )_wrap_axes_set_ylog, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "lpos", (attr_get_function * )_wrap_axes_get_lpos, (attr_set_function * )_wrap_axes_set_lpos, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "rect", (attr_get_function * )_wrap_axes_get_rect, (attr_set_function * )_wrap_axes_set_rect, (attr_get_object_function * )_wrap_axes_get_obj_rect, (attr_set_object_function * )int_set_object_failed },
  { "zoom", (attr_get_function * )_wrap_axes_get_zoom, (attr_set_function * )_wrap_axes_set_zoom, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "clip", (attr_get_function * )_wrap_axes_get_clip, (attr_set_function * )_wrap_axes_set_clip, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "line_width", (attr_get_function * )_wrap_axes_get_line_width, (attr_set_function * )_wrap_axes_set_line_width, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "font_size", (attr_get_function * )_wrap_axes_get_font_size, (attr_set_function * )_wrap_axes_set_font_size, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "background", (attr_get_function * )_wrap_axes_get_background, (attr_set_function * )_wrap_axes_set_background, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "nax", (attr_get_function * )_wrap_axes_get_nax, (attr_set_function * )_wrap_axes_set_nax, (attr_get_object_function * )_wrap_axes_get_obj_nax, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 181 "codegen/axes.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_axes(Stack stack, int rhs, int opt, int lhs)
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 1480 "axes.c"


#line 191 "codegen/axes.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_axes(Stack stack, int rhs, int opt, int lhs)
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 1493 "axes.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Axes_func[]={
  { "extractelts_axes", _wrap_nsp_extractelts_axes},
  { "setrowscols_axes", _wrap_nsp_setrowscols_axes},
  { "axes_create", int_axes_create},
  { NULL, NULL}
};

/* call ith function in the Axes interface */

int Axes_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Axes_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Axes_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Axes_func[i].name;
  *f = Axes_func[i].fonc;
}
void nsp_initialize_Axes_types(void)
{
  new_type_axes(T_BASE);
}

#line 202 "codegen/axes.override"

/* inserted verbatim at the end */

/**
 * nsp_axes_remove_children:
 * @F:
 *
 *
 *
 * Returns:
 **/

extern int nsp_axes_remove_children(NspAxes *A)
{
  if (A->obj->children != NULL )
    {
      int i,l;
      NspGraphic *G= (NspGraphic *) A;
      /* unlink children */
      nsp_list_unlink_figure(A->obj->children, G->obj->Fig);
      l = nsp_list_length(A->obj->children);
      for (i=1; i <= l ; i++)
	nsp_list_remove_first(A->obj->children);
    }
  return OK;
}


static int nsp_axes_legends(BCG *Xgc,NspAxes *axe);

int nsp_fontsize_string_in_box(BCG *Xgc, double iw, double ih, int fsize, const char *str);

static void nsp_draw_axes(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  nsp_gcscale scale_keep = *Xgc->scales;
  int lw=-1, font[2], nax[4], i,zfont[2];
  GdkRectangle clip, clip_axe , r2, rect_a;
  char xf[]="onn";
  double wrect1[4], inside_bounds[4];
  Cell *cloc;
  NspList *L;
  NspAxes *P = (NspAxes *) Obj;
  if ( ((NspGraphic *) P)->obj->show == FALSE ) return;

  if ( rect != NULL) rect_a = *rect;

  /* get the axe bounding rectangle */

  if ( P->obj->top == TRUE )
    {
      /* top level axe
       * P->obj->wrect gives the position in window as proportions
       */
      int wdim[2];
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      clip_axe.x=P->obj->wrect->R[0]*wdim[0];
      clip_axe.y=P->obj->wrect->R[1]*wdim[1];
      clip_axe.width=P->obj->wrect->R[2]*wdim[0];
      clip_axe.height=P->obj->wrect->R[3]*wdim[1];
    }
  else
    {
      /* non top level axe
       * P->obj->wrect gives the position in father (up,left,w,h)
       */
      nsp_axes *A = ((NspGraphic *) P)->obj->Axe;
      scale_f2i(&A->scale,P->obj->wrect->R,P->obj->wrect->R+1,&clip_axe.x,&clip_axe.y,1);
      length_scale_f2i(&A->scale,P->obj->wrect->R+2,P->obj->wrect->R+3,
		       &clip_axe.width,&clip_axe.height,1);
    }

  if ( rect != NULL)
    {
      /*
       * check if we are in the draw zone given by rect
       */
      if ( ! gdk_rectangle_intersect(&rect_a,&clip_axe,&r2)) return;
    }

  if ( P->obj->top == TRUE )
    {
      /* This is a top level axes, wrect gives the axes position in the
       * enclosing graphic window.
       */
      set_scale(Xgc->scales,NULL,P->obj->wrect->R,NULL,NULL,NULL,P->obj->arect->R);
      memcpy(wrect1,P->obj->wrect->R,4*sizeof(double));
    }
  else
    {
      nsp_axes *A = ((NspGraphic *) P)->obj->Axe;
      double *ARect = A->arect->R, *FRect = A->frect->R;
      /* This is not a top level axes, we draw its enclosing rectangle
       * if alpha is non nul we should draw a rotated rectangle
       */
      Xgc->graphic_engine->scale->drawrectangle(Xgc,P->obj->wrect->R);
      /* wrect->R is [left,up,w,h]
       * we need to compute wrect->R in term on window/proportions
       */
      wrect1[0]= ARect[0]+(1-ARect[0]-ARect[2])*(P->obj->wrect->R[0]-FRect[0])/(FRect[2]-FRect[0]);
      wrect1[1]= ARect[1]+(1-ARect[1]-ARect[3])*(1- (P->obj->wrect->R[1]-FRect[1])/(FRect[3]-FRect[1]));
      wrect1[2]= (1-ARect[0]-ARect[2])*(P->obj->wrect->R[2])/(FRect[2]-FRect[0]);
      wrect1[3]= (1-ARect[1]-ARect[3])*(P->obj->wrect->R[3])/(FRect[3]-FRect[1]);
      Xgc->scales->cosa= cos( P->obj->rho);
      Xgc->scales->sina= sin( P->obj->rho);
    }

  /* update frect
   *
   */

  if ( P->obj->fixed == FALSE )
    {
      /* actualize the inside bounds with objects
       * this should not be done systematically
       */
      nsp_axes_compute_inside_bounds(Obj,inside_bounds);
      /*
       * update frect with inside_bounds
       */
      memcpy(P->obj->frect->R,inside_bounds,4*sizeof(double));
    }
  else
    {
      /* bounds of the plot are given by rect 
       * except if rect contains infinite values 
       */
      if ( isinf(P->obj->rect->R[0]) ||
	   isinf(P->obj->rect->R[1]) ||
	   isinf(P->obj->rect->R[2]) ||
	   isinf(P->obj->rect->R[3]) )
	{
	  double frect[4];
	  int i;
	  nsp_axes_compute_inside_bounds(Obj,frect);
	  for ( i = 0 ; i < 4; i++)
	    if (! isinf(P->obj->rect->R[i])) frect[i]= P->obj->rect->R[i];
	  memcpy(P->obj->frect->R, frect,4*sizeof(double));
	}
      else
	{
	  memcpy(P->obj->frect->R,P->obj->rect->R,4*sizeof(double));
	}
    }
  if ( P->obj->zoom == TRUE )
    {
      /* when zooming zrect is used
       */
      memcpy(P->obj->frect->R,P->obj->zrect->R,4*sizeof(double));
    }

  if ( P->obj->xlog == TRUE ) xf[1]= 'l';
  if ( P->obj->ylog == TRUE ) xf[2]= 'l';

  for ( i= 0; i <4 ; i++) nax[i]=P->obj->nax->R[i];

  nsp_axes_update_frame_bounds(Xgc,wrect1,
			       P->obj->frect->R,
			       P->obj->arect->R,
			       nax,
			       P->obj->iso,
			       P->obj->auto_axis,
			       xf);
  /* save the scales */

  P->obj->scale = *Xgc->scales;
  nsp_send_scale_2D_to_opengl(Xgc);

  /* clip the enclosing rectangle of the  axe
   */

  clip = clip_axe;
  if ( rect != NULL ) gdk_rectangle_intersect( &rect_a, &clip, &clip);

  {
    GdkRectangle clip1= clip;
    Xgc->graphic_engine->xset_clip(Xgc, &clip1);
  }

  Xgc->graphic_engine->xget_font(Xgc,zfont, FALSE);

  if (  P->obj->font_size != -1)
    {
      Xgc->graphic_engine->xget_font(Xgc,font, FALSE);
      Xgc->graphic_engine->xset_font(Xgc,font[0], P->obj->font_size, FALSE);
    }

  if ( P->obj->line_width != -1 )
    {
      lw = Xgc->graphic_engine->xset_thickness(Xgc, P->obj->line_width);
    }

  /* draw axes, ticks: we call this function twice 
   * first pass to draw the background 
   * second pass for the foreground 
   */
  nsp_axis_draw(Xgc,P->obj->axes+'0', (P->obj->auto_axis) ? '5': '1',
		P->obj->grid, P->obj->background,TRUE);

  /* title if present */
  nsp_graphic_titles(Xgc,P->obj->title,P->obj->x,P->obj->y);

  if (  P->obj->font_size != -1)
    {
      Xgc->graphic_engine->xset_font(Xgc,font[0],font[1], FALSE);
    }

  if ( P->obj->line_width != -1 )
    {
      Xgc->graphic_engine->xset_thickness(Xgc, lw);
    }

  /* clip the inside rectangle of the  axe
   * Note that clipping is wrong when an axe is rotated
   * since clipping only works with rectangles
   */

  clip = (  P->obj->clip == TRUE ) ?  Xgc->scales->Irect : clip_axe;
  if ( rect != NULL ) gdk_rectangle_intersect(&rect_a,&clip,&clip);

  {
    GdkRectangle clip1= clip;
    Xgc->graphic_engine->xset_clip(Xgc, &clip1);
  }


  /* draw elements
   * we can limit the drawing to clip
   */

  L = P->obj->children;
  cloc = L->first ;
  while ( cloc != NULLCELL )
    {
      if ( cloc->O != NULLOBJ )
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G,&clip,data);
	}
      cloc = cloc->next;
    }

  /* insert the legends at last since it must be above curves */

  clip = clip_axe;
  if ( rect != NULL ) gdk_rectangle_intersect( &rect_a, &clip, &clip);

  {
    GdkRectangle clip1= clip;
    Xgc->graphic_engine->xset_clip(Xgc, &clip1);
  }

  Xgc->graphic_engine->xset_font(Xgc,zfont[0],zfont[1], FALSE);

  if (  P->obj->font_size != -1)
    {
      Xgc->graphic_engine->xset_font(Xgc,font[0], P->obj->font_size, FALSE);
    }

  if ( P->obj->line_width != -1 )
    {
      lw = Xgc->graphic_engine->xset_thickness(Xgc, P->obj->line_width);
    }

  /* legends */
  nsp_axes_legends(Xgc,P);

  /* draw axes, ticks */
  nsp_axis_draw(Xgc,P->obj->axes+'0', (P->obj->auto_axis) ? '5': '1',
		P->obj->grid, P->obj->background, FALSE);

  if (  P->obj->font_size != -1)
    {
      Xgc->graphic_engine->xset_font(Xgc,font[0],font[1], FALSE);
    }

  if ( P->obj->line_width != -1 )
    {
      Xgc->graphic_engine->xset_thickness(Xgc, lw);
    }
  /* end of legends */

  /* back to previous clip zone */

  if ( rect != NULL )
    {
      GdkRectangle rect1= *rect;
      Xgc->graphic_engine->xset_clip(Xgc, &rect1);
    }
  else
    {
      Xgc->graphic_engine->xset_unclip(Xgc);
    }

  /* scale back */
  *Xgc->scales = scale_keep;
}



/* to be synchronized with the above function
 *
 */

void nsp_axes_i2f(nsp_axes *A,int x,int y,double pt[2])
{
  scale_i2f(&A->scale,pt,pt+1,&x,&y,1);
}

/* draw legends from information contained in axe
 */

static int nsp_axes_legends(BCG *Xgc,NspAxes *axe)
{
#define NC 256
  int cu_mark[NC];
  int cu_mark_size[NC];
  int cu_mark_color[NC];
  int cu_width[NC];
  int cu_color[NC];
  int count=0,legend_pos=1;
  NspSMatrix *legends=NULL;
  /* get and collect the legends */
  NspList *L = axe->obj->children;
  Cell *cloc = L->first ;
  legend_pos = axe->obj->lpos;
  legends = nsp_smatrix_create_with_length(NVOID,0,0,-1);
  while ( cloc != NULLCELL )
    {
      if ( cloc->O != NULLOBJ && IsCurve(cloc->O) )
	{
	  NspCurve *cv = (NspCurve *) cloc->O;
	  if (cv->obj->legend[0] != '\0' )
	    {
	      nsp_row_smatrix_append_string(legends,cv->obj->legend);
	      cu_mark[count]= cv->obj->mark;
	      cu_mark_size[count]= cv->obj->mark_size;
	      cu_mark_color[count]= cv->obj->mark_color;
	      cu_width[count]= cv->obj->width;
	      cu_color[count]= cv->obj->color;
	      count++;
	      if (count >= NC ) break;
	    }
	}
      cloc = cloc->next;
    }
  if ( count != 0)
    {
      nsp_legends(Xgc,legend_pos,legends->mn,
		  cu_mark,cu_mark_size,cu_mark_color,cu_width,cu_color,
		  legends->S,"@");
    }
  if ( legends != NULL) nsp_smatrix_destroy(legends);
  return OK;
}


/* compute the bounds of the set of objects countained in the
 * axes
 */

static void nsp_axes_compute_inside_bounds(NspGraphic *Obj,double *bounds)
{
  NspAxes *P = (NspAxes *) Obj;
  nsp_grlist_compute_inside_bounds(P->obj->children,bounds);
}


void nsp_axes_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,double *arect,
				  int *aaint,int isomode,int auto_axes, char *xf)
{
  double FRect1[4];
  int Xdec[3],Ydec[3],i;
  double xmin=0.0,xmax=10.0,ymin= 0.0,ymax= 10.0;
  int wdim[2];
  xmin=Min(frect[0],frect[2]);
  ymin=Min(frect[1],frect[3]);
  xmax=Max(frect[0],frect[2]);
  ymax=Max(frect[1],frect[3]);

  Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);

  /*
   * modify computed min,max if isoview requested
   */

  if ( isomode == TRUE )
    {
      /* code by S. Mottelet 11/7/2000 */
      double hx=xmax-xmin,hy=ymax-ymin,hx1,hy1, dwdim[2];
      dwdim[0]=linint((double)wdim[0] * (wrect[2]*(1.0-arect[0]-arect[1])));
      dwdim[1]=linint((double)wdim[1] * (wrect[3]*(1.0-arect[2]-arect[3])));
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
	  Sciprintf("Warning: Can't use Log on X-axis xmin is negative \n");
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
	  Sciprintf(" Can't use Log on y-axis ymin is negative \n");
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
      for (i=0; i< 4 ;i++) FRect2[i]=FRect1[i];
      /* change graduation */
      gr_rescale_new(&xf[1],FRect2,Xdec,Ydec,&(aaint[0]),&(aaint[2]));
    }

  /* Update the current scale */

  set_scale(Xgc->scales,wdim,wrect,FRect1,aaint,xf+1,arect);

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

#ifdef WITH_OPENGL
  /* transmit info to opengl */
  if ( Xgc->graphic_engine == &GL_gengine )
    {
      nsp_ogl_set_2dview(Xgc);
    }
#endif

}


static void nsp_translate_axes(NspGraphic *Obj,const double *tr)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE) return ;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  /* only valid for non top axes */
  P->obj->wrect->R[0] += tr[0];
  P->obj->wrect->R[1] += tr[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_axes(NspGraphic *Obj,double *R)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE) return ;
  Sciprintf("we should get a double here for rho\n");
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_axes(NspGraphic *Obj,double *alpha)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE) return ;
  P->obj->wrect->R[2] *= alpha[0];
  P->obj->wrect->R[3] *= alpha[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of axes
 *
 */

static int nsp_getbounds_axes(NspGraphic *Obj,double *bounds)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE)
    {
      nsp_figure *Fig = (((NspGraphic *) Obj)->obj->Fig);
      BCG *Xgc= Fig->Xgc;
      /* tolevel axe: we need the window dimension */
      int wdim[2];
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      bounds[0]= P->obj->wrect->R[0]*wdim[0];
      bounds[1]= (P->obj->wrect->R[1]- P->obj->wrect->R[3])*wdim[1];
      bounds[2]= (P->obj->wrect->R[0]+P->obj->wrect->R[2])*wdim[0];
      bounds[3]= P->obj->wrect->R[1]*wdim[1];
    }
  else
    {
      /* get the bound in parent i.e given by wrect : upper-left w,h */
      bounds[0]=P->obj->wrect->R[0]; /* xmin */
      bounds[1]=P->obj->wrect->R[1]-P->obj->wrect->R[3];/* ymin */
      bounds[2]=P->obj->wrect->R[0]+P->obj->wrect->R[2];/* xmax */
      bounds[3]=P->obj->wrect->R[1];/* ymax */
    }
  return TRUE;
}

static void nsp_axes_link_figure(NspGraphic *G, void *F, void *A)
{
  /* link toplevel */
  if ( ((NspAxes *) G)->obj->top == TRUE)
    nsp_graphic_link_figure(G, F, ((NspAxes *) G)->obj);
  else
    nsp_graphic_link_figure(G, F, A);
  /* link children */
  nsp_list_link_figure(((NspAxes *) G)->obj->children,F, ((NspAxes *) G)->obj);
}


static void nsp_axes_unlink_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_unlink_figure(G,F);
  /* link children */
  nsp_list_unlink_figure(((NspAxes *) G)->obj->children,F);
}

static NspList *nsp_axes_children(NspGraphic *Obj)
{
  return  ((NspAxes *) Obj)->obj->children;
}


/* set up the bounds of axes according to objects
 * already inserted and rect. rect can be NULL when unused.
 *
 */

void nsp_strf_axes(NspAxes *A,double *rect, char scale)
{
  NspGraphic *G= (NspGraphic *) A;
  /* use strf argument to change the axes */
  switch (scale)
    {
    case '0': /* no computation, the plot use the previus (or default) scale */
      break;
    case '1': /* from the rect arg */
      memcpy(A->obj->rect->R,rect,4*sizeof(double));
      A->obj->iso = FALSE;
      A->obj->auto_axis = FALSE;
      A->obj->fixed = TRUE;
      break;
    case '2': /* from the min/max of all the datas */
      nsp_axes_compute_inside_bounds(G,A->obj->bounds->R);
      A->obj->iso = FALSE;
      A->obj->auto_axis = FALSE;
      A->obj->fixed = FALSE;
      break;
    case '3': /* built for an isometric scale from the rect arg */
      memcpy(A->obj->rect->R,rect,4*sizeof(double));
      A->obj->iso = TRUE;
      A->obj->auto_axis = TRUE;
      A->obj->fixed = TRUE;
      break;
    case '4': /* built for an isometric plot from the min/max of the x, y datas */
      nsp_axes_compute_inside_bounds(G,A->obj->bounds->R);
      A->obj->iso = TRUE;
      A->obj->auto_axis = TRUE;
      A->obj->fixed = FALSE;
      break;
    case '5': /* case 3 + enlarged for pretty axes from the rect arg */
      memcpy(A->obj->rect->R,rect,4*sizeof(double));
      A->obj->auto_axis = TRUE;
      A->obj->iso = FALSE;
      A->obj->fixed = TRUE;
      break;
    case '6': /* enlarged for pretty axes from the min/max of the x, y datas */
      nsp_axes_compute_inside_bounds(G,A->obj->bounds->R);
      A->obj->auto_axis = TRUE;
      A->obj->iso = TRUE;
      A->obj->fixed = FALSE;
      break;
    case '7': /* backward compatibility  */
      memcpy(A->obj->rect->R,rect,4*sizeof(double));
      A->obj->iso = FALSE;
      A->obj->auto_axis = FALSE;
      A->obj->fixed = TRUE;
      break;
    case '8': /* backward compatibility  */
      nsp_axes_compute_inside_bounds(G,A->obj->bounds->R);
      A->obj->auto_axis = TRUE;
      A->obj->iso = FALSE;
      A->obj->fixed = FALSE;
      break;
    }
  if ( A->obj->fixed )
    memcpy(A->obj->frect->R,A->obj->rect->R,4*sizeof(double));
  else
    memcpy(A->obj->frect->R,A->obj->bounds->R,4*sizeof(double));
}

void nsp_strf_axes_new_deprecated(NspAxes *A,double *rect, char scale,int auto_axis,int iso)
{
  NspGraphic *G= (NspGraphic *) A;
  A->obj->iso = iso;
  A->obj->auto_axis = auto_axis;
  /* use strf argument to change the axes */
  switch (scale)
    {
    case '0': /* no computation, the plot use the previus (or default) scale */
      break;
    case '1':
    case '3':
    case '5':
    case '7':
      memcpy(A->obj->rect->R,rect,4*sizeof(double));
      A->obj->fixed = TRUE;
      break;
    case '2': /* from the min/max of all the datas */
    case '4':
    case '6':
    case '8':
      nsp_axes_compute_inside_bounds(G,A->obj->bounds->R);
      A->obj->fixed = FALSE;
    }
  if ( A->obj->fixed )
    memcpy(A->obj->frect->R,A->obj->rect->R,4*sizeof(double));
  else
    memcpy(A->obj->frect->R,A->obj->bounds->R,4*sizeof(double));
}


/**
 * nsp_figure_zoom:
 * @Xgc:
 * @box:
 *
 * select the axes to be zoomed by considering the
 * center of the given @box. Then set the zoom
 * scales on the axe and invalidate the axe.
 *
 **/

void nsp_figure_zoom(BCG *Xgc,int *box)
{

  NspObject *Obj1;
  int pt[2]={ (box[0]+box[2])/2, (box[1]+box[3])/2};
  Obj1=nsp_check_pt_axes_or_objs3d(Xgc,pt);
  if ( Obj1 == NULL) return ;
  if ( IsAxes(Obj1) )
    {
      NspAxes *A = (NspAxes *) Obj1;
      NspGraphic *G = (NspGraphic *) Obj1;
      double pt1[2],pt2[2];
      /* Sciprintf("Found an axes to be zoomed\n"); */
      nsp_axes_i2f(A->obj,box[0],box[1], pt1);
      nsp_axes_i2f(A->obj,box[2],box[3], pt2);
      A->obj->zoom=TRUE;
      A->obj->zrect->R[0]=pt1[0]; /* xmin */
      A->obj->zrect->R[1]=pt2[1]; /* ymin */
      A->obj->zrect->R[2]=pt2[0]; /* xmax */
      A->obj->zrect->R[3]=pt1[1]; /* ymax */
      G->type->invalidate(G);
    }
  else if ( IsObjs3d(Obj1))
    {
      /* Sciprintf("Found a 3dobj to be zoomed\n"); */
    }
}


/**
 * nsp_figure_unzoom:
 * @Obj: a #NspGraphic
 *
 * change the zoom flag for all the axes
 * found in figure @Obj and invalidate the
 * axes if necessary.
 *
 **/

void nsp_figure_unzoom(NspGraphic *Obj)
{
  NspList *L;
  Cell *cloc;
  NspFigure *F= (NspFigure *) Obj;
  if ( Obj == NULL ||  !IsFigure((NspObject *) Obj))  return ;

  L= F->obj->children;
  cloc = L->first ;
  while ( cloc != NULLCELL )
    {
      if ( cloc->O != NULLOBJ )
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  if ( IsAxes(NSP_OBJECT(G)))
	    {
	      NspAxes *A = (NspAxes *) G;
	      if ( A->obj->zoom == TRUE)
		{
		  A->obj->zoom= FALSE;
		  G->type->invalidate(G);
		}

	    }
	  else if ( IsObjs3d(NSP_OBJECT(G)))
	    {
	      /* NspObjs3d *A = (NspObjs3d *) G; */

	    }
	}
      cloc = cloc->next;
    }
}


/*
 * here we compute new axis graduation
 * but without changing FRect. The computed graduation does not
 * necessary start at FRect boundaries but inside.
 */


static void gr_rescale_new(char *logf, double *FRectI, int *Xdec, int *Ydec, int *xnax, int *ynax)
{
  int i;
  double FRectO[4];
  double xtest;
  if (logf[0] == 'n')
    {
      /* avoid infinite loop with nans */
      if ( isnan(FRectI[0]) || isnan(FRectI[2]) ) { FRectI[0]=-1;FRectI[1]=1;}

      if ( FRectI[0]*FRectI[2] < 0 )
	{
	  double xmin,xmax;
	  /* if zero is inside frect we try to find a graduation with zero inside */
	  xmin = Min(FRectI[0],-FRectI[2]);
	  xmax = Max(FRectI[2],-FRectI[0]);
	  graduate(&xmin,&xmax,FRectO,FRectO+2,xnax,xnax+1,Xdec,Xdec+1,Xdec+2);
	}
      else
	{
	  graduate(FRectI,FRectI+2,FRectO,FRectO+2,xnax,xnax+1,Xdec,Xdec+1,Xdec+2);
	}
      /* we do not change FRectI but change Xdec to eliminate points outside FRectI
       * The problem is that proceding that way we can obtain just one point.
       */
      i=0;
      while (1) {
	xtest= exp10((double)Xdec[2])*(Xdec[0] + i*(Xdec[1]-Xdec[0])/xnax[1]);
	if ( xtest >= FRectI[0] ) break;
	i++;
      }
      Xdec[0] += i*(Xdec[1]-Xdec[0])/xnax[1];
      xnax[1] -= i;
      /* eliminate extra values at the end  */
      i=0;
      while (1)
	{
	  xtest = exp10((double)Xdec[2])*(Xdec[0]+(xnax[1]-i)*(Xdec[1]-Xdec[0])/xnax[1]);
	  if ( xtest <=  FRectI[2] ) break;
	  i++;
	}
      Xdec[1] -= i*(Xdec[1]-Xdec[0])/xnax[1];
      xnax[1] -= i;
    }
  else
    {
      /* logscale */
      Xdec[0]=inint(FRectI[0]);
      Xdec[1]=inint(FRectI[2]);
      Xdec[2]=0;
    }
  if (logf[1] == 'n')
    {
      /* avoid infinite loop with nans */
      if ( isnan(FRectI[1]) || isnan(FRectI[3]) ) { FRectI[1]=-1;FRectI[3]=1;}

      if ( FRectI[1]*FRectI[3] < 0 )
	{
	  double ymin,ymax;
	  /* if zero is inside frect we try to find a graduation with zero inside */
	  ymin = Min(FRectI[1],-FRectI[3]);
	  ymax = Max(FRectI[3],-FRectI[1]);
	  graduate(&ymin,&ymax,FRectO+1,FRectO+3,ynax,ynax+1,Ydec,Ydec+1,Ydec+2);
	}
      else
	{
	  graduate(FRectI+1,FRectI+3,FRectO+1,FRectO+3,ynax,ynax+1,Ydec,Ydec+1,Ydec+2);
	}

      /* we do not change FRectI but change Xdec to eliminate points outside FRectI
       * The problem is that proceding that way we can obtain just one point.
       */
      i=0;
      while (1)
	{
	  xtest = exp10(Ydec[2])*(Ydec[0] + i*(Ydec[1]-Ydec[0])/ynax[1]);
	  if ( xtest >= FRectI[1] ) break;
	  i++;
	}
      Ydec[0] += i*(Ydec[1]-Ydec[0])/ynax[1];
      ynax[1] -= i;
      /* eliminate extra values at the end  */
      i=0;
      while (1)
	{
	  xtest = exp10(Ydec[2])*(Ydec[0]+(ynax[1]-i)*(Ydec[1]-Ydec[0])/ynax[1]);
	  if ( xtest <=  FRectI[3] ) break;
	  i++;
	}
      Ydec[1] -= i*(Ydec[1]-Ydec[0])/ynax[1];
      ynax[1] -= i;
    }
  else
    {
      /* logscale */
      Ydec[0]=inint(FRectI[1]);Ydec[1]=inint(FRectI[3]);Ydec[2]=0;
    }
}


/* requested for nsp_gcscale
 *
 */

static void  nsp_destroy_nsp_gcscale(nsp_gcscale *scales,NspAxes *H)
{
  return;
}


static int nsp_print_nsp_gcscale(int indent,nsp_gcscale *locks,NspAxes *M)
{
  return OK;
}

static int nsp_check_nsp_gcscale(nsp_gcscale *locks,NspAxes *M)
{
  return OK;
}

static int nsp_nsp_gcscale_full_copy(NspAxes *C,nsp_gcscale *scale,NspAxes *M)
{
  C->obj->scale = *scale;
  return OK;
}

static int nsp_eq_nsp_gcscale(nsp_gcscale *scale1, nsp_gcscale *scale2)
{
  /* XXX */
  return TRUE;
}

static void nsp_init_nsp_gcscale(nsp_gcscale *scale)
{
  nsp_scale_default(scale);
}

/**
 * nsp_axes_insert_child:
 * @A: a #NspAxes
 * @G: a #NspGraphic
 *
 * inserts @G in the given axe @A. The bounds
 * of the axes are updated acordingly and an
 * invalidate operation is raised on @G if @invalidate
 * is true.
 *
 * Returns: %OK or %FAIL
 **/

int nsp_axes_insert_child(NspAxes *A, NspGraphic *G, int invalidate)
{
  if ( nsp_list_end_insert(A->obj->children,(NspObject *) G )== FAIL)
    return FAIL;
  /* call the link_figure method */
  G->type->link_figure( G,((NspGraphic *) A)->obj->Fig,A->obj);
  /* updates the bounds of the axe */
  nsp_axes_compute_inside_bounds((NspGraphic *) A,A->obj->bounds->R);
  /* raise an invalidate operation */
  if ( invalidate ) nsp_graphic_invalidate((NspGraphic *) G);
  return OK;
}


/**
 * nsp_axes_invalidate:
 * @G:
 *
 * invalidate the drawing region associated to an axes object.
 *
 **/

void nsp_axes_invalidate(NspGraphic *G)
{
  NspAxes *P = (NspAxes *) G;
  if ( P->obj->top == TRUE)
    {
      gint rect[4]; /* like a GdkRectangle */
      int wdim[2];
      nsp_figure *F = G->obj->Fig;
      BCG *Xgc;
      if ( F == NULL ) return ;
      if ((Xgc= F->Xgc) == NULL) return ;
      if ( F->draw_now== FALSE) return;
      if ( G->obj->show == FALSE ) return;
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      rect[0]= P->obj->wrect->R[0]*wdim[0];
      rect[1]= P->obj->wrect->R[1]*wdim[1];
      rect[2]= P->obj->wrect->R[2]*wdim[0];
      rect[3]= P->obj->wrect->R[3]*wdim[1];
      Xgc->graphic_engine->invalidate(Xgc,rect);
    }
  else
    {
      return nsp_graphic_invalidate(G);
    }
}





void nsp_draw_colorbar(BCG *Xgc,nsp_axes *P,double vmin , double vmax, int *colminmax)
{
  double grads[20], axrect[4], hr, rect[4], rrect[4];
  int ntags,start, color,i,cpat, wdim[2];
  int nb_colors;
  int last = Xgc->graphic_engine->xget_last(Xgc);
  int clip[5];
  GdkRectangle rclip;

  Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
  nb_colors = colminmax[1] - colminmax[0]+1;

  /* frame rectangle */
  axrect[0]=P->wrect->R[0]*wdim[0];
  axrect[1]=P->wrect->R[1]*wdim[1];
  axrect[2]=P->wrect->R[2]*wdim[0];
  axrect[3]=P->wrect->R[3]*wdim[1];
  /* right margin rectangle */
  rrect[0]= axrect[0]+axrect[2]*(1-P->arect->R[1]);
  rrect[1]= axrect[1]+axrect[3]*(P->arect->R[2]);
  rrect[2]= axrect[2]*(P->arect->R[1]);
  rrect[3]= axrect[3]*(1 - P->arect->R[2] -P->arect->R[3]);

  Xgc->graphic_engine->xget_clip(Xgc,clip);
  rclip.x = rrect[0];
  rclip.y = rrect[1]-10;
  rclip.width = rrect[2];
  rclip.height = rrect[3]+20;
  Xgc->graphic_engine->xset_clip(Xgc,&rclip);

  /* colorbar rectangle */
  cpat = Xgc->graphic_engine->xset_color(Xgc,last+1);
  rect[0]=rrect[0]+4;
  rect[1]=rrect[1];
  rect[2]=rrect[2]/4;
  hr= rrect[3]/(double) nb_colors;
  rect[3]=hr;
  color= colminmax[1];
  for (i = 0 ; i < nb_colors ; i++)
    {
      rect[1] = rrect[1]+ i*hr ;
      rect[3] = rrect[1]+ (i+1)*hr - rect[1];
      Xgc->graphic_engine->xset_color(Xgc,color);
      Xgc->graphic_engine->fillrectangle(Xgc,rect);
      color--;
    }

  /* switch to black */
  Xgc->graphic_engine->xset_color(Xgc,last+1);
  rect[1]=rrect[1];
  rect[3]=(nb_colors)*hr;
  Xgc->graphic_engine->drawrectangle(Xgc,rect);

  ntags = getticks(vmin,vmax,grads,&start);

  for ( i = 0 ; i < ntags ; i++ )
    {
      double srect[4];
      int y;
      double uval;
      char str[256];
      uval = grads[start+i];
      sprintf(str,"%g",uval);
      Xgc->graphic_engine->boundingbox(Xgc,str,0,0,srect);
      y = rect[1] + rect[3]*(1-((uval-vmin)/(vmax - vmin)));
      Xgc->graphic_engine->drawline(Xgc,rect[0]+rect[2],y,rect[0]+rect[2]+5,y);
      y += srect[3]/2;
      Xgc->graphic_engine->displaystring(Xgc,str,rect[0]+rect[2]+8,y,FALSE,0,
					 GR_STR_XLEFT, GR_STR_YBOTTOM);
    }

  if ( clip[0]== 1 )
    {
      rclip.x = clip[1];
      rclip.y = clip[2];
      rclip.width = clip[3];
      rclip.height = clip[4];
      Xgc->graphic_engine->xset_clip(Xgc,&rclip);
    }

  /* back to current value*/
  Xgc->graphic_engine->xset_color(Xgc,cpat);
}


static int getticks(double xmin,double xmax,double *grads,int *start)
{
  int ngrads, n1, n2;
  gr_compute_ticks(&xmin, &xmax, grads, &ngrads);
  n1 = 0; n2 = ngrads-1;
  if ( grads[n1] < xmin ) n1++;
  if ( grads[n2] > xmax ) n2--;
  ngrads = n2 - n1 + 1;
  *start = n1;
  return ngrads;
}

#line 2612 "axes.c"
