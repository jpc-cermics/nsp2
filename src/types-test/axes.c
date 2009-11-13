/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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





#line 61 "codegen/axes.override"
#include <nsp/objs3d.h>
#include <nsp/curve.h>

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 36 "axes.c"

/* ----------- NspAxes ----------- */


#define  NspAxes_Private 
#include <nsp/object.h>
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

#line 75 "codegen/axes.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_axes;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_axes ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_axes  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_axes  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_axes  ;
  ((NspTypeGraphic *) type->surtype)->link_figure = nsp_axes_link_figure; 
  ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_axes_unlink_figure; 
  ((NspTypeGraphic *) type->surtype)->children = (children_func *) nsp_axes_children ;
  ((NspTypeGraphic *) type->surtype)->invalidate = nsp_axes_invalidate;

#line 120 "axes.c"
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
  if ( nsp_eq_nsp_gcscale(&A->obj->scale,&loc->obj->scale)== FALSE) return FALSE;
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
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
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
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
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
  nsp_destroy_nsp_gcscale(&H->obj->scale,H); 
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
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_axes_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->wrect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"rho=%f\n",M->obj->rho);
  Sciprintf1(indent+2,"top	= %s\n", ( M->obj->top == TRUE) ? "T" : "F" );
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->bounds),indent+2,"bounds",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->arect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->arect),indent+2,"arect",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->frect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"title=%s\n",M->obj->title);
  Sciprintf1(indent+2,"x=%s\n",M->obj->x);
  Sciprintf1(indent+2,"y=%s\n",M->obj->y);
  if ( M->obj->children != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"fixed	= %s\n", ( M->obj->fixed == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"iso	= %s\n", ( M->obj->iso == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"auto_axis	= %s\n", ( M->obj->auto_axis == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"grid=%d\n",M->obj->grid);
  Sciprintf1(indent+2,"axes=%d\n",M->obj->axes);
  Sciprintf1(indent+2,"xlog	= %s\n", ( M->obj->xlog == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"ylog	= %s\n", ( M->obj->ylog == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"lpos=%d\n",M->obj->lpos);
  if ( M->obj->rect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->rect),indent+2,"rect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"zoom	= %s\n", ( M->obj->zoom == TRUE) ? "T" : "F" );
  if ( M->obj->zrect != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->zrect),indent+2,"zrect",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_print_nsp_gcscale(indent+2,&M->obj->scale,M);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_axes_latex(NspAxes *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_axes_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->wrect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->wrect),indent+2,"wrect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"rho=%f\n",M->obj->rho);
  Sciprintf1(indent+2,"top	= %s\n", ( M->obj->top == TRUE) ? "T" : "F" );
  if ( M->obj->bounds != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->bounds),indent+2,"bounds",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->arect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->arect),indent+2,"arect",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->frect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->frect),indent+2,"frect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"title=%s\n",M->obj->title);
  Sciprintf1(indent+2,"x=%s\n",M->obj->x);
  Sciprintf1(indent+2,"y=%s\n",M->obj->y);
  if ( M->obj->children != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"fixed	= %s\n", ( M->obj->fixed == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"iso	= %s\n", ( M->obj->iso == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"auto_axis	= %s\n", ( M->obj->auto_axis == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"grid=%d\n",M->obj->grid);
  Sciprintf1(indent+2,"axes=%d\n",M->obj->axes);
  Sciprintf1(indent+2,"xlog	= %s\n", ( M->obj->xlog == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"ylog	= %s\n", ( M->obj->ylog == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"lpos=%d\n",M->obj->lpos);
  if ( M->obj->rect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->rect),indent+2,"rect",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"zoom	= %s\n", ( M->obj->zoom == TRUE) ? "T" : "F" );
  if ( M->obj->zrect != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->zrect),indent+2,"zrect",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_print_nsp_gcscale(indent+2,&M->obj->scale,M);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
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
  if ( check_cast (O,nsp_type_axes_id) == TRUE ) return ((NspAxes *) O);
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
  H->obj->iso = TRUE;
  H->obj->auto_axis = TRUE;
  H->obj->grid = -1;
  H->obj->axes = 1;
  H->obj->xlog = FALSE;
  H->obj->ylog = FALSE;
  H->obj->lpos = 1;
  H->obj->rect = NULLMAT;
  H->obj->zoom = FALSE;
  H->obj->zrect = NULLMAT;
  nsp_init_nsp_gcscale(&H->obj->scale);
  return OK;
}

int nsp_axes_check_values(NspAxes *H)
{
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
  if ( nsp_check_nsp_gcscale(&H->obj->scale,H) == FAIL ) return FAIL;
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspAxes *nsp_axes_create(char *name,NspMatrix* wrect,double rho,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,char* x,char* y,NspList* children,gboolean fixed,gboolean iso,gboolean auto_axis,int grid,int axes,gboolean xlog,gboolean ylog,int lpos,NspMatrix* rect,gboolean zoom,NspMatrix* zrect,nsp_gcscale scale,NspTypeBase *type)
{
  NspAxes *H  = nsp_axes_create_void(name,type);
  if ( H ==  NULLAXES) return NULLAXES;
  if ( nsp_axes_create_partial(H) == FAIL) return NULLAXES;
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
  H->obj->scale = scale;
  if ( nsp_axes_check_values(H) == FAIL) return NULLAXES;
  return H;
}


NspAxes *nsp_axes_create_default(char *name)
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
/*
 * full copy for gobject derived class
 */

NspAxes *nsp_axes_full_copy_partial(NspAxes *H,NspAxes *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_axes))) == NULL) return NULLAXES;
  H->obj->ref_count=1;
  if ( self->obj->wrect == NULL )
    { H->obj->wrect = NULL;}
  else
    {
      if ((H->obj->wrect = (NspMatrix *) nsp_object_full_copy_and_name("wrect",NSP_OBJECT(self->obj->wrect))) == NULLMAT) return NULL;
    }
  H->obj->rho=self->obj->rho;
  H->obj->top=self->obj->top;
  if ( self->obj->bounds == NULL )
    { H->obj->bounds = NULL;}
  else
    {
      if ((H->obj->bounds = (NspMatrix *) nsp_object_full_copy_and_name("bounds",NSP_OBJECT(self->obj->bounds))) == NULLMAT) return NULL;
    }
  if ( self->obj->arect == NULL )
    { H->obj->arect = NULL;}
  else
    {
      if ((H->obj->arect = (NspMatrix *) nsp_object_full_copy_and_name("arect",NSP_OBJECT(self->obj->arect))) == NULLMAT) return NULL;
    }
  if ( self->obj->frect == NULL )
    { H->obj->frect = NULL;}
  else
    {
      if ((H->obj->frect = (NspMatrix *) nsp_object_full_copy_and_name("frect",NSP_OBJECT(self->obj->frect))) == NULLMAT) return NULL;
    }
  if ((H->obj->title = nsp_string_copy(self->obj->title)) == NULL) return NULL;
  if ((H->obj->x = nsp_string_copy(self->obj->x)) == NULL) return NULL;
  if ((H->obj->y = nsp_string_copy(self->obj->y)) == NULL) return NULL;
  if ( self->obj->children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *) nsp_object_full_copy_and_name("children",NSP_OBJECT(self->obj->children))) == NULLLIST) return NULL;
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
      if ((H->obj->rect = (NspMatrix *) nsp_object_full_copy_and_name("rect",NSP_OBJECT(self->obj->rect))) == NULLMAT) return NULL;
    }
  H->obj->zoom=self->obj->zoom;
  if ( self->obj->zrect == NULL )
    { H->obj->zrect = NULL;}
  else
    {
      if ((H->obj->zrect = (NspMatrix *) nsp_object_full_copy_and_name("zrect",NSP_OBJECT(self->obj->zrect))) == NULLMAT) return NULL;
    }
  if( nsp_nsp_gcscale_full_copy(H,&H->obj->scale,self)== FAIL) return NULL;
  return H;
}

NspAxes *nsp_axes_full_copy(NspAxes *self)
{
  NspAxes *H  =nsp_axes_create_void(NVOID,(NspTypeBase *) nsp_type_axes);
  if ( H ==  NULLAXES) return NULLAXES;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLAXES;
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
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
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

#line 96 "codegen/axes.override"
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

#line 875 "axes.c"
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
  const gchar *ret;
  NspObject *nsp_ret;

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
  const gchar *ret;
  NspObject *nsp_ret;

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
  const gchar *ret;
  NspObject *nsp_ret;

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

#line 112 "codegen/axes.override"

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
  nsp_axes_compute_inside_bounds(NULL,self,((NspAxes *) self)->obj->bounds->R);
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) val,((NspGraphic *) self)->obj->Fig,((NspAxes *) self)->obj );
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
  nsp_axes_compute_inside_bounds(NULL,self,((NspAxes *) self)->obj->bounds->R);
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) O,((NspGraphic *) self)->obj->Fig,((NspAxes *) self)->obj);
  return OK;
}


#line 1081 "axes.c"
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

static AttrTab axes_attrs[] = {
  { "wrect", (attr_get_function *)_wrap_axes_get_wrect, (attr_set_function *)_wrap_axes_set_wrect,(attr_get_object_function *)_wrap_axes_get_obj_wrect, (attr_set_object_function *)int_set_object_failed },
  { "rho", (attr_get_function *)_wrap_axes_get_rho, (attr_set_function *)_wrap_axes_set_rho,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "top", (attr_get_function *)_wrap_axes_get_top, (attr_set_function *)_wrap_axes_set_top,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "arect", (attr_get_function *)_wrap_axes_get_arect, (attr_set_function *)_wrap_axes_set_arect,(attr_get_object_function *)_wrap_axes_get_obj_arect, (attr_set_object_function *)int_set_object_failed },
  { "frect", (attr_get_function *)_wrap_axes_get_frect, (attr_set_function *)_wrap_axes_set_frect,(attr_get_object_function *)_wrap_axes_get_obj_frect, (attr_set_object_function *)int_set_object_failed },
  { "title", (attr_get_function *)_wrap_axes_get_title, (attr_set_function *)_wrap_axes_set_title,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "x", (attr_get_function *)_wrap_axes_get_x, (attr_set_function *)_wrap_axes_set_x,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_axes_get_y, (attr_set_function *)_wrap_axes_set_y,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "children", (attr_get_function *)_wrap_axes_get_children, (attr_set_function *)_wrap_axes_set_children,(attr_get_object_function *)_wrap_axes_get_obj_children, (attr_set_object_function *)_wrap_axes_set_obj_children },
  { "fixed", (attr_get_function *)_wrap_axes_get_fixed, (attr_set_function *)_wrap_axes_set_fixed,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "iso", (attr_get_function *)_wrap_axes_get_iso, (attr_set_function *)_wrap_axes_set_iso,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "auto_axis", (attr_get_function *)_wrap_axes_get_auto_axis, (attr_set_function *)_wrap_axes_set_auto_axis,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "grid", (attr_get_function *)_wrap_axes_get_grid, (attr_set_function *)_wrap_axes_set_grid,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "axes", (attr_get_function *)_wrap_axes_get_axes, (attr_set_function *)_wrap_axes_set_axes,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "xlog", (attr_get_function *)_wrap_axes_get_xlog, (attr_set_function *)_wrap_axes_set_xlog,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "ylog", (attr_get_function *)_wrap_axes_get_ylog, (attr_set_function *)_wrap_axes_set_ylog,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "lpos", (attr_get_function *)_wrap_axes_get_lpos, (attr_set_function *)_wrap_axes_set_lpos,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "rect", (attr_get_function *)_wrap_axes_get_rect, (attr_set_function *)_wrap_axes_set_rect,(attr_get_object_function *)_wrap_axes_get_obj_rect, (attr_set_object_function *)int_set_object_failed },
  { "zoom", (attr_get_function *)_wrap_axes_get_zoom, (attr_set_function *)_wrap_axes_set_zoom,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 168 "codegen/axes.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_axes(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 1320 "axes.c"


#line 178 "codegen/axes.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_axes(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 1333 "axes.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Axes_func[]={
  {"extractelts_axes", _wrap_nsp_extractelts_axes},
  {"setrowscols_axes", _wrap_nsp_setrowscols_axes},
  { "axes_create", int_axes_create},
  { NULL, NULL}
};

/* call ith function in the Axes interface */

int Axes_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Axes_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Axes_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Axes_func[i].name;
  *f = Axes_func[i].fonc;
}

#line 189 "codegen/axes.override"

/* inserted verbatim at the end */
void nsp_axes_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,double *arect,
				  int *aaint,int isomode, int auto_axes, char *xf);
static int nsp_axes_legends(BCG *Xgc,NspAxes *axe);

static void nsp_draw_axes(BCG *Xgc,NspGraphic *Obj, GdkRectangle *rect,void *data)
{
  GdkRectangle *r = rect, clip;
  char xf[]="onn";
  double WRect[4],*wrect1,WRect1[4], FRect[4], ARect[4], inside_bounds[4];
  char logscale[2];
  int aaint[4]={10,2,10,2};
  Cell *cloc;
  NspList *L;
  NspAxes *P = (NspAxes *) Obj;
  if ( ((NspGraphic *) P)->obj->show == FALSE ) return;

  if ( rect != NULL) 
    {
      /*
       * check if we are in the draw zone given by data.
       */
      if ( P->obj->top == TRUE ) 
	{
	  GdkRectangle r1;
	  int wdim[2];
	  Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
	  r1.x=P->obj->wrect->R[0]*wdim[0];
	  r1.y=P->obj->wrect->R[1]*wdim[1];
	  r1.width=P->obj->wrect->R[2]*wdim[0];
	  r1.height=P->obj->wrect->R[3]*wdim[1];
	  if ( ! gdk_rectangle_intersect(r,&r1,NULL))
	    {
	      /*
	      Sciprintf("No need to draw one axes [%d,%d,%d,%d] draw=[%d,%d,%d,%d]\n",
			r1.x,r1.y,r1.width,r1.height,
			r->x,r->y,r->width,r->height
			);
	      */
	      return;
	    }
	  else
	    {
	      /*
	      Sciprintf("Drawing axes\n");
	      */
	    }
	}
      else
	{
	  /* XXXX
	    Sciprintf("draw axes for non to level to be done \n");
	  */
	}
    }

  /* draw elements */

  L = P->obj->children;
  cloc = L->first ;
  /* we change the scale according to the axes */
  getscale2d(Xgc,WRect,FRect,logscale,ARect);
  if ( P->obj->top == TRUE ) 
    {
      /* This is a top level axes, wrect gives the axes position in the 
       * enclosing graphic window. 
       */
      set_scale(Xgc,"fTffft",P->obj->wrect->R,NULL,NULL,NULL,P->obj->arect->R);
      wrect1= P->obj->wrect->R;
    }
  else 
    {
      /* This is not a top level axes, we draw its enclosing rectangle 
       * if alpha is non nul we should draw a rotated rectangle
       */
      Xgc->graphic_engine->scale->drawrectangle(Xgc,P->obj->wrect->R);
      /* wrect->R is [left,up,w,h] 
       * we need to compute wrect->R in term on window/proportions 
       */
      WRect1[0]= ARect[0]+(1-ARect[0]-ARect[2])*(P->obj->wrect->R[0]-FRect[0])/(FRect[2]-FRect[0]);
      WRect1[1]= ARect[1]+(1-ARect[1]-ARect[3])*(1- (P->obj->wrect->R[1]-FRect[1])/(FRect[3]-FRect[1]));
      WRect1[2]= (1-ARect[0]-ARect[2])*(P->obj->wrect->R[2])/(FRect[2]-FRect[0]);
      WRect1[3]= (1-ARect[1]-ARect[3])*(P->obj->wrect->R[3])/(FRect[3]-FRect[1]);
      wrect1 = WRect1;
      Xgc->scales->cosa= cos( P->obj->rho);
      Xgc->scales->sina= sin( P->obj->rho);
    }

  /* we directly change the default scale because we do not want 
   * to register all the scales that will be generated by set_scale 
   * thus we use T in flag[1].
   */


  if ( P->obj->fixed == FALSE ) 
    {
      /* actualize the inside bounds with objects 
       * this should not be done systematically 
       */
      nsp_axes_compute_inside_bounds(Xgc,Obj,inside_bounds);
      memcpy(P->obj->frect->R,inside_bounds,4*sizeof(double));
    }
  else
    {
      /* bounds of the plot are given by rect 
       */
      memcpy(P->obj->frect->R,P->obj->rect->R,4*sizeof(double));
    }

  if ( P->obj->zoom == TRUE )
    {
      /* when zooming zrect is used 
       */
      memcpy(P->obj->frect->R,P->obj->zrect->R,4*sizeof(double));
    }
  
  if ( P->obj->xlog == TRUE ) xf[1]= 'l';
  if ( P->obj->ylog == TRUE ) xf[2]= 'l';
  
  nsp_axes_update_frame_bounds(Xgc,wrect1,
			       P->obj->frect->R,
			       P->obj->arect->R,
			       aaint,
			       P->obj->iso,
			       P->obj->auto_axis,
			       xf);
#if 0
  {
    int clipz[5];
    Xgc->graphic_engine->xget_clip(Xgc,(int *) &clipz);
    Sciprintf("avant de dessiner les axes: clip=[%d,%d,%d,%d]\n",
	      clipz[1],clipz[2],clipz[3],clipz[4]);
    Sciprintf("avant de dessiner les axes: rect=[%d,%d,%d,%d]\n",
	      rect->x,rect->y, rect->width,rect->height);
  }
#endif 

  axis_draw(Xgc,'1', 
	    (P->obj->auto_axis) ? '5': '1',
	    P->obj->grid);

  /* save the scales */
  P->obj->scale = *Xgc->scales;

  /* clip the axes 
   * Note that clipping is wrong when an axe is rotated 
   * since clipping only works with rectangles 
   */

  Sciprintf("Axes with t=%d\n", P->obj->top);

  if ( P->obj->top == TRUE )
    {
      if ( rect != NULL ) 
	{
	  gdk_rectangle_intersect( rect, (GdkRectangle *) Xgc->scales->WIRect1, &clip);
	  Xgc->graphic_engine->xset_clip(Xgc,(int *) &clip);
	  /*
	    Sciprintf("apres les  axes intersection de clip=[%d,%d,%d,%d]\n",
	    clip.x,clip.y,clip.width,clip.height);
	  */
	}
      else
	{
	  Xgc->graphic_engine->xset_clip(Xgc,(int *) Xgc->scales->WIRect1);
	}
    }
  
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  G->type->draw(Xgc,G,rect,data);
	}
      cloc = cloc->next;
    }

  if ( P->obj->top == TRUE )
    {
      /* back to previous clip zone */
      if ( rect != NULL ) 
	{
	  Xgc->graphic_engine->xset_clip(Xgc,(int *) &rect);
	}
      else
	{
	  Xgc->graphic_engine->xset_unclip(Xgc);
	}
    }
  
  /* legends */
  nsp_axes_legends(Xgc,P);
  /* title if present */
  if ( P->obj->title[0] != '\0') 
    Xgc->graphic_engine->scale->displaystringa(Xgc,P->obj->title,1);
  if ( P->obj->x[0] != '\0') 
    Xgc->graphic_engine->scale->displaystringa(Xgc,P->obj->x,2);
  if ( P->obj->y[0] != '\0') 
    Xgc->graphic_engine->scale->displaystringa(Xgc,P->obj->y,3);
  
  /* scale back */
  set_scale(Xgc,"fTtfft",WRect,FRect,NULL,NULL,ARect);
  if (  P->obj->top != TRUE )
    {
      Xgc->scales->cosa=1.0;
      Xgc->scales->sina=0.0;
    }
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
  int style[56],count=0,legend_pos=1;
  NspSMatrix *legends=NULL,*legend=NULL;
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
	      style[count++]= cv->obj->color;
	      if (count >= 56 ) break; 
	    }
	}
      cloc = cloc->next;
    }
  if ( count != 0) 
    {
      legend = nsp_smatrix_row_concat(legends,"@",1);
      if (legend != NULL) 
	nsp_legends(Xgc,legend_pos,legends->mn,style,legend->S[0],"@"); 
    }
  if ( legend != NULL) nsp_smatrix_destroy(legend);
  if ( legends != NULL) nsp_smatrix_destroy(legends);
  return OK;
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
  L = P->obj->children;
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
	  if ( G->type->bounds(G,l_bounds) == TRUE ) 
	    {
	      if ( l_bounds[0] < bounds[0] ) 
		bounds[0]= l_bounds[0];
	      if (  l_bounds[2] > bounds[2])
		bounds[2]= l_bounds[2];
	      if ( l_bounds[1] < bounds[1] ) 
		bounds[1]= l_bounds[1];
	      if (  l_bounds[3] > bounds[3])
		bounds[3]= l_bounds[3];
	    }
	}
      cloc = cloc->next;
    }
}


void nsp_axes_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,double *arect,
				  int *aaint,int isomode,int auto_axes, char *xf)
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
      int wdim[2];
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      dwdim[0]=linint((double)wdim[0] * (wrect[2]*(1.0-arect[0]-arect[1])));  /* add corrections for margins */
      dwdim[1]=linint((double)wdim[1] * (wrect[3]*(1.0-arect[2]-arect[3])));  /* add corrections for margins */
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
      gr_rescale_new(&xf[1],FRect2,Xdec,Ydec,&(aaint[0]),&(aaint[2]));
    }
  
  /* Update the current scale */
  
  set_scale(Xgc,"tTtttt",wrect,FRect1,aaint,xf+1,arect);
  
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


static void nsp_translate_axes(NspGraphic *Obj,const double *tr)
{
  NspAxes *P = (NspAxes *) Obj;
  if ( P->obj->top == TRUE) return ;
  if (((NspGraphic *) Obj)->obj->show == TRUE)
    nsp_graphic_invalidate((NspGraphic *) Obj);
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
  nsp_graphic_link_figure(G, F, ((NspAxes *) G)->obj);
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

void nsp_strf_axes(BCG *Xgc,NspAxes *A,double *rect, char scale)
{
  NspGraphic *G= (NspGraphic *) A;
  /* use strf argument to change the axes */
  switch (scale) 
    {
    case '0': /* no computation, the plot use the previus (or default) scale */
      break;
    case '1': /* from the rect arg */
      A->obj->iso = FALSE;
      A->obj->auto_axis = FALSE;
      break;
    case '7': /* backward compatibility  */
      memcpy(A->obj->rect->R,rect,4*sizeof(double));
      A->obj->iso = FALSE;
      A->obj->auto_axis = FALSE;
      A->obj->fixed = TRUE;
      break;
    case '5': /* enlarged for pretty axes from the rect arg */
      memcpy(A->obj->rect->R,rect,4*sizeof(double));
      A->obj->auto_axis = TRUE;
      A->obj->iso = FALSE;
      A->obj->fixed = TRUE; 
      break;
    case '3': /* built for an isometric scale from the rect arg */
      memcpy(A->obj->rect->R,rect,4*sizeof(double));
      A->obj->iso = TRUE;
      A->obj->auto_axis = TRUE;
      A->obj->fixed = TRUE; 
      break;
    case '2': /* from the min/max of the x, y datas */
      /* could be stored in a cache */
      nsp_axes_compute_inside_bounds(Xgc,G,A->obj->bounds->R);
      A->obj->iso = FALSE;
      A->obj->auto_axis = FALSE;
      A->obj->fixed = FALSE; 
      break;
    case '8': /* backward compatibility  */
      nsp_axes_compute_inside_bounds(Xgc,G,A->obj->bounds->R);
      A->obj->auto_axis = TRUE;
      A->obj->iso = FALSE;
      A->obj->fixed = FALSE; 
      break;
    case '6': /* enlarged for pretty axes from the min/max of the x, y datas */
      nsp_axes_compute_inside_bounds(Xgc,G,A->obj->bounds->R);
      A->obj->auto_axis = TRUE;
      A->obj->iso = TRUE;
      A->obj->fixed = FALSE; 
      break;
    case '4': /* built for an isometric plot from the min/max of the x, y datas */
      nsp_axes_compute_inside_bounds(Xgc,G,A->obj->bounds->R);
      A->obj->iso = TRUE;
      A->obj->auto_axis = TRUE;
      A->obj->fixed = FALSE; 
      break;
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
 * invalidate operation is raised using the 
 * graphic object. 
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_axes_insert_child(NspAxes *A, NspGraphic *G)
{
  if ( nsp_list_end_insert(A->obj->children,(NspObject *) G )== FAIL)
    return FAIL;
  /* call the link_figure method */
  G->type->link_figure( G,((NspGraphic *) A)->obj->Fig,A->obj);
  /* updates the bounds of the axe */
  nsp_axes_compute_inside_bounds(NULL,(NspGraphic *) A,A->obj->bounds->R);
  /* raise an invalidate operation */
  nsp_graphic_invalidate((NspGraphic *) G);
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

#line 2243 "axes.c"
