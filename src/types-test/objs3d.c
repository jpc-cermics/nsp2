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





#line 51 "codegen/objs3d.override"
#include <gtk/gtk.h>
#include <nsp/object.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h>
#include <nsp/curve.h>
#include <nsp/polyhedron.h>

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 40 "objs3d.c"

/* ----------- NspObjs3d ----------- */


#define  NspObjs3d_Private 
#include <nsp/object.h>
#include <nsp/objs3d.h>
#include <nsp/interf.h>

/* 
 * NspObjs3d inherits from Graphic 
 */

int nsp_type_objs3d_id=0;
NspTypeObjs3d *nsp_type_objs3d=NULL;

/*
 * Type object for NspObjs3d 
 * all the instance of NspTypeObjs3d share the same id. 
 * nsp_type_objs3d: is an instance of NspTypeObjs3d 
 *    used for objects of NspObjs3d type (i.e built with new_objs3d) 
 * other instances are used for derived classes 
 */
NspTypeObjs3d *new_type_objs3d(type_mode mode)
{
  NspTypeObjs3d *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_objs3d != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_objs3d;
    }
  if (( type =  malloc(sizeof(NspTypeObjs3d))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = objs3d_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = objs3d_get_methods;
  type->new = (new_func *) new_objs3d;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for objs3d */ 

  top->pr = (print_func *) nsp_objs3d_print;
  top->dealloc = (dealloc_func *) nsp_objs3d_destroy;
  top->copy  =  (copy_func *) nsp_objs3d_copy;
  top->size  = (size_func *) nsp_objs3d_size;
  top->s_type =  (s_type_func *) nsp_objs3d_type_as_string;
  top->sh_type = (sh_type_func *) nsp_objs3d_type_short_string;
  top->info = (info_func *) nsp_objs3d_info;
  /* top->is_true = (is_true_func  *) nsp_objs3d_is_true; */
  /* top->loop =(loop_func *) nsp_objs3d_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_objs3d_object;
  top->eq  = (eq_func *) nsp_objs3d_eq;
  top->neq  = (eq_func *) nsp_objs3d_neq;
  top->save  = (save_func *) nsp_objs3d_xdr_save;
  top->load  = (load_func *) nsp_objs3d_xdr_load;
  top->create = (create_func*) int_objs3d_create;
  top->latex = (print_func *) nsp_objs3d_latex;
  top->full_copy = (copy_func *) nsp_objs3d_full_copy;

  /* specific methods for objs3d */

  type->init = (init_func *) init_objs3d;

#line 69 "codegen/objs3d.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_objs3d;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_objs3d ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_objs3d  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_objs3d  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_objs3d  ;
  ((NspTypeGraphic *) type->surtype)->link_figure = nsp_objs3d_link_figure; 
  ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_objs3d_unlink_figure; 
  ((NspTypeGraphic *) type->surtype)->children = (children_func *) nsp_objs3d_children ;
  ((NspTypeGraphic *) type->surtype)->invalidate = nsp_objs3d_invalidate;

#line 124 "objs3d.c"
  /* 
   * NspObjs3d interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_objs3d_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeObjs3d called nsp_type_objs3d
       */
      type->id =  nsp_type_objs3d_id = nsp_new_type_id();
      nsp_type_objs3d = type;
      if ( nsp_register_type(nsp_type_objs3d) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_objs3d(mode);
    }
  else 
    {
      type->id = nsp_type_objs3d_id;
      return type;
    }
}

/*
 * initialize NspObjs3d instances 
 * locally and by calling initializer on parent class 
 */

static int init_objs3d(NspObjs3d *Obj,NspTypeObjs3d *type)
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
 * new instance of NspObjs3d 
 */

NspObjs3d *new_objs3d() 
{
  NspObjs3d *loc;
  /* type must exists */
  nsp_type_objs3d = new_type_objs3d(T_BASE);
  if ( (loc = malloc(sizeof(NspObjs3d)))== NULLOBJS3D) return loc;
  /* initialize object */
  if ( init_objs3d(loc,nsp_type_objs3d) == FAIL) return NULLOBJS3D;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspObjs3d 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_objs3d_size(NspObjs3d *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char objs3d_type_name[]="Objs3d";
static char objs3d_short_type_name[]="objs3d";

static char *nsp_objs3d_type_as_string(void)
{
  return(objs3d_type_name);
}

static char *nsp_objs3d_type_short_string(NspObject *v)
{
  return(objs3d_short_type_name);
}

/*
 * A == B 
 */

static int nsp_objs3d_eq(NspObjs3d *A, NspObject *B)
{
  NspObjs3d *loc = (NspObjs3d *) B;
  if ( check_cast(B,nsp_type_objs3d_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->wrect)->type->eq(A->obj->wrect,loc->obj->wrect) == FALSE ) return FALSE;
  if ( A->obj->rho != loc->obj->rho) return FALSE;
  if ( A->obj->top != loc->obj->top) return FALSE;
  if ( NSP_OBJECT(A->obj->bounds)->type->eq(A->obj->bounds,loc->obj->bounds) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->arect)->type->eq(A->obj->arect,loc->obj->arect) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->frect)->type->eq(A->obj->frect,loc->obj->frect) == FALSE ) return FALSE;
  if ( strcmp(A->obj->title,loc->obj->title) != 0) return FALSE;
  if ( NSP_OBJECT(A->obj->children)->type->eq(A->obj->children,loc->obj->children) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->colormap)->type->eq(A->obj->colormap,loc->obj->colormap) == FALSE ) return FALSE;
  if ( A->obj->alpha != loc->obj->alpha) return FALSE;
  if ( A->obj->theta != loc->obj->theta) return FALSE;
  if ( A->obj->with_box != loc->obj->with_box) return FALSE;
  if ( A->obj->box_color != loc->obj->box_color) return FALSE;
  if ( A->obj->box_style != loc->obj->box_style) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_objs3d_neq(NspObjs3d *A, NspObject *B)
{
  return ( nsp_objs3d_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_objs3d_xdr_save(XDR *xdrs, NspObjs3d *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_objs3d)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->wrect)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->rho) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->top) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->arect)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->frect)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->title) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->children)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->colormap)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->alpha) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->theta) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->with_box) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->box_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->box_style) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspObjs3d  *nsp_objs3d_xdr_load_partial(XDR *xdrs, NspObjs3d *M)
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
  if ((M->obj->children =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
  if ((M->obj->colormap =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->alpha) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->theta) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->with_box) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->box_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->box_style) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspObjs3d  *nsp_objs3d_xdr_load(XDR *xdrs)
{
  NspObjs3d *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLOBJS3D;
  if ((H  = nsp_objs3d_create_void(name,(NspTypeBase *) nsp_type_objs3d))== NULLOBJS3D) return H;
  if ( nsp_objs3d_create_partial(H) == FAIL) return NULLOBJS3D;
  if ((H  = nsp_objs3d_xdr_load_partial(xdrs,H))== NULLOBJS3D) return H;
  if ( nsp_objs3d_check_values(H) == FAIL) return NULLOBJS3D;
  return H;
}

/*
 * delete 
 */

void nsp_objs3d_destroy_partial(NspObjs3d *H)
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
    if ( H->obj->children != NULL ) 
      nsp_list_destroy(H->obj->children);
    if ( H->obj->colormap != NULL ) 
      nsp_matrix_destroy(H->obj->colormap);
    FREE(H->obj);
   }
}

void nsp_objs3d_destroy(NspObjs3d *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_objs3d_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_objs3d_info(NspObjs3d *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLOBJS3D) 
    {
      Sciprintf("Null Pointer NspObjs3d \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_objs3d_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_objs3d_print(NspObjs3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLOBJS3D) 
    {
      Sciprintf("Null Pointer NspObjs3d \n");
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
          nsp_objs3d_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_objs3d_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
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
  if ( M->obj->children != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->colormap != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->colormap),indent+2,"colormap",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"alpha=%f\n",M->obj->alpha);
  Sciprintf1(indent+2,"theta=%f\n",M->obj->theta);
  Sciprintf1(indent+2,"with_box	= %s\n", ( M->obj->with_box == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"box_color=%d\n",M->obj->box_color);
  Sciprintf1(indent+2,"box_style=%d\n",M->obj->box_style);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_objs3d_latex(NspObjs3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_objs3d_type_short_string(NSP_OBJECT(M)));
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
  if ( M->obj->children != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->children),indent+2,"children",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->colormap != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->colormap),indent+2,"colormap",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"alpha=%f\n",M->obj->alpha);
  Sciprintf1(indent+2,"theta=%f\n",M->obj->theta);
  Sciprintf1(indent+2,"with_box	= %s\n", ( M->obj->with_box == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"box_color=%d\n",M->obj->box_color);
  Sciprintf1(indent+2,"box_style=%d\n",M->obj->box_style);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspObjs3d objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspObjs3d   *nsp_objs3d_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_objs3d_id) == TRUE ) return ((NspObjs3d *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_objs3d));
  return NULL;
}

int IsObjs3dObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_objs3d_id);
}

int IsObjs3d(NspObject *O)
{
  return nsp_object_type(O,nsp_type_objs3d_id);
}

NspObjs3d  *GetObjs3dCopy(Stack stack, int i)
{
  if (  GetObjs3d(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspObjs3d  *GetObjs3d(Stack stack, int i)
{
  NspObjs3d *M;
  if (( M = nsp_objs3d_object(NthObj(i))) == NULLOBJS3D)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspObjs3d instance 
 *-----------------------------------------------------*/

static NspObjs3d *nsp_objs3d_create_void(char *name,NspTypeBase *type)
{
 NspObjs3d *H  = (type == NULL) ? new_objs3d() : type->new();
 if ( H ==  NULLOBJS3D)
  {
   Sciprintf("No more memory\n");
   return NULLOBJS3D;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLOBJS3D;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_objs3d_create_partial(NspObjs3d *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_objs3d)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->wrect = NULLMAT;
  H->obj->rho = 0.0;
  H->obj->top = TRUE;
  H->obj->bounds = NULLMAT;
  H->obj->arect = NULLMAT;
  H->obj->frect = NULLMAT;
  H->obj->title = NULL;
  H->obj->children = NULLLIST;
  H->obj->colormap = NULLMAT;
  H->obj->alpha = 35;
  H->obj->theta = 45;
  H->obj->with_box = TRUE;
  H->obj->box_color = -1;
  H->obj->box_style = 0;
  return OK;
}

int nsp_objs3d_check_values(NspObjs3d *H)
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
       if (( H->obj->bounds = nsp_matrix_create("bounds",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->arect == NULLMAT) 
    {
     double x_def[4]={1./8.,1./8.,1./8.,1./8.};
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
  if ( H->obj->children == NULLLIST) 
    {
     if (( H->obj->children = nsp_list_create("children")) == NULLLIST)
       return FAIL;
    }
  if ( H->obj->colormap == NULLMAT) 
    {
       if (( H->obj->colormap = nsp_matrix_create("colormap",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspObjs3d *nsp_objs3d_create(char *name,NspMatrix* wrect,double rho,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,NspList* children,NspMatrix* colormap,double alpha,double theta,gboolean with_box,int box_color,int box_style,NspTypeBase *type)
{
  NspObjs3d *H  = nsp_objs3d_create_void(name,type);
  if ( H ==  NULLOBJS3D) return NULLOBJS3D;
  if ( nsp_objs3d_create_partial(H) == FAIL) return NULLOBJS3D;
  H->obj->wrect= wrect;
  H->obj->rho=rho;
  H->obj->top=top;
  H->obj->bounds= bounds;
  H->obj->arect= arect;
  H->obj->frect= frect;
  H->obj->title = title;
  H->obj->children= children;
  H->obj->colormap= colormap;
  H->obj->alpha=alpha;
  H->obj->theta=theta;
  H->obj->with_box=with_box;
  H->obj->box_color=box_color;
  H->obj->box_style=box_style;
  if ( nsp_objs3d_check_values(H) == FAIL) return NULLOBJS3D;
  return H;
}


NspObjs3d *nsp_objs3d_create_default(char *name)
{
 NspObjs3d *H  = nsp_objs3d_create_void(name,NULL);
 if ( H ==  NULLOBJS3D) return NULLOBJS3D;
  if ( nsp_objs3d_create_partial(H) == FAIL) return NULLOBJS3D;
 if ( nsp_objs3d_check_values(H) == FAIL) return NULLOBJS3D;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspObjs3d *nsp_objs3d_copy_partial(NspObjs3d *H,NspObjs3d *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspObjs3d *nsp_objs3d_copy(NspObjs3d *self)
{
  NspObjs3d *H  =nsp_objs3d_create_void(NVOID,(NspTypeBase *) nsp_type_objs3d);
  if ( H ==  NULLOBJS3D) return NULLOBJS3D;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLOBJS3D;
  if ( nsp_objs3d_copy_partial(H,self)== NULL) return NULLOBJS3D;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspObjs3d *nsp_objs3d_full_copy_partial(NspObjs3d *H,NspObjs3d *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_objs3d))) == NULL) return NULLOBJS3D;
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
  if ( self->obj->children == NULL )
    { H->obj->children = NULL;}
  else
    {
      if ((H->obj->children = (NspList *) nsp_object_full_copy_and_name("children",NSP_OBJECT(self->obj->children))) == NULLLIST) return NULL;
    }
  if ( self->obj->colormap == NULL )
    { H->obj->colormap = NULL;}
  else
    {
      if ((H->obj->colormap = (NspMatrix *) nsp_object_full_copy_and_name("colormap",NSP_OBJECT(self->obj->colormap))) == NULLMAT) return NULL;
    }
  H->obj->alpha=self->obj->alpha;
  H->obj->theta=self->obj->theta;
  H->obj->with_box=self->obj->with_box;
  H->obj->box_color=self->obj->box_color;
  H->obj->box_style=self->obj->box_style;
  return H;
}

NspObjs3d *nsp_objs3d_full_copy(NspObjs3d *self)
{
  NspObjs3d *H  =nsp_objs3d_create_void(NVOID,(NspTypeBase *) nsp_type_objs3d);
  if ( H ==  NULLOBJS3D) return NULLOBJS3D;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLOBJS3D;
  if ( nsp_objs3d_full_copy_partial(H,self)== NULL) return NULLOBJS3D;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspObjs3d
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_objs3d_create(Stack stack, int rhs, int opt, int lhs)
{
  NspObjs3d *H;
  CheckStdRhs(0,0);
  /* want to be sure that type objs3d is initialized */
  nsp_type_objs3d = new_type_objs3d(T_BASE);
  if(( H = nsp_objs3d_create_void(NVOID,(NspTypeBase *) nsp_type_objs3d)) == NULLOBJS3D) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_objs3d_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_objs3d_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *objs3d_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_objs3d_get_wrect(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspObjs3d *) self)->obj->wrect;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_obj_wrect(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspObjs3d *) self)->obj->wrect);
  return (NspObject *) ret;
}

static int _wrap_objs3d_set_wrect(void *self,const char *attr, NspObject *O)
{
  NspMatrix *wrect;

  if ( ! IsMat(O) ) return FAIL;
  if ((wrect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspObjs3d *) self)->obj->wrect != NULL ) 
    nsp_matrix_destroy(((NspObjs3d *) self)->obj->wrect);
  ((NspObjs3d *) self)->obj->wrect= wrect;
  return OK;
}

#line 90 "codegen/objs3d.override"
/* override set rho */
static int _wrap_objs3d_set_rho(void *self, char *attr, NspObject *O)
{
  double rho;
  if ( DoubleScalar(O,&rho) == FAIL) return FAIL;

  if ( ((NspObjs3d *) self)->obj->rho != rho) 
    {
      ((NspObjs3d *) self)->obj->rho = rho;
      nsp_objs3d_invalidate((NspGraphic *) self);
    }
  return OK;
}

#line 785 "objs3d.c"
static NspObject *_wrap_objs3d_get_rho(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->rho;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_objs3d_get_top(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->top;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_objs3d_set_top(void *self,const char *attr, NspObject *O)
{
  int top;

  if ( BoolScalar(O,&top) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->top= top;
  return OK;
}

static NspObject *_wrap_objs3d_get_arect(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspObjs3d *) self)->obj->arect;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_obj_arect(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspObjs3d *) self)->obj->arect);
  return (NspObject *) ret;
}

static int _wrap_objs3d_set_arect(void *self,const char *attr, NspObject *O)
{
  NspMatrix *arect;

  if ( ! IsMat(O) ) return FAIL;
  if ((arect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspObjs3d *) self)->obj->arect != NULL ) 
    nsp_matrix_destroy(((NspObjs3d *) self)->obj->arect);
  ((NspObjs3d *) self)->obj->arect= arect;
  return OK;
}

static NspObject *_wrap_objs3d_get_frect(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspObjs3d *) self)->obj->frect;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_obj_frect(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspObjs3d *) self)->obj->frect);
  return (NspObject *) ret;
}

static int _wrap_objs3d_set_frect(void *self,const char *attr, NspObject *O)
{
  NspMatrix *frect;

  if ( ! IsMat(O) ) return FAIL;
  if ((frect = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspObjs3d *) self)->obj->frect != NULL ) 
    nsp_matrix_destroy(((NspObjs3d *) self)->obj->frect);
  ((NspObjs3d *) self)->obj->frect= frect;
  return OK;
}

static NspObject *_wrap_objs3d_get_title(void *self,const char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->title;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_objs3d_set_title(void *self,const char *attr, NspObject *O)
{
  char *title;

  if ((title = nsp_string_object(O))==NULL) return FAIL;
  if ((title = nsp_string_copy(title)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspObjs3d *) self)->obj->title);
  ((NspObjs3d *) self)->obj->title= title;
  return OK;
}

#line 106 "codegen/objs3d.override"

/* here we override get_obj  and set_obj 
 * we want get to be followed by a set to check that 
 * inserted value is correct thus we use copy = TRUE.
 */

static NspObject *_wrap_objs3d_get_obj_children(void *self,char *attr, int *copy)
{
  NspList *ret;
  *copy = TRUE; 
  ret = ((NspList*) ((NspObjs3d *) self)->obj->children);
  return (NspObject *) ret;
}

/* in this function we can check that val is correct before 
 * setting the field with val. return FAIL if val is incorrect.
 */

static int _wrap_objs3d_set_obj_children(void *self,NspObject *val)
{
  double inside_bounds[6];
  if ( ! IsList(val) ) return FAIL;
  if ( nsp_list_check_figure((NspList *) val, ((NspGraphic *) self)->obj->Fig) == FAIL) return FAIL;
  if (((NspObjs3d *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspObjs3d *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspObjs3d *) self)->obj->children);
    }
  ((NspObjs3d *) self)->obj->children =  (NspList *) val;
  nsp_objs3d_compute_inside_bounds(NULL,self,inside_bounds);
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) val,((NspGraphic *) self)->obj->Fig,((NspObjs3d *) self)->obj);
  return OK;
}

static int _wrap_objs3d_set_children(void *self, char *attr, NspObject *O)
{
  double inside_bounds[6];
  NspList *children;
  if ( ! IsList(O) ) return FAIL;
  if ((children = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspObjs3d *) self)->obj->children != NULL ) 
    {
      if ( ((NspGraphic *) self)->obj->Fig != NULL) 
	nsp_list_unlink_figure(((NspObjs3d *) self)->obj->children,((NspGraphic *) self)->obj->Fig);
      nsp_list_destroy(((NspObjs3d *) self)->obj->children);
    }
  ((NspObjs3d *) self)->obj->children= children;
  nsp_objs3d_compute_inside_bounds(NULL,self,inside_bounds);
  if ( ((NspGraphic *) self)->obj->Fig != NULL) 
    nsp_list_link_figure((NspList *) O,((NspGraphic *) self)->obj->Fig, ((NspObjs3d *) self)->obj);
  return OK;
}


#line 951 "objs3d.c"
static NspObject *_wrap_objs3d_get_children(void *self,const char *attr)
{
  NspList *ret;

  ret = ((NspObjs3d *) self)->obj->children;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_colormap(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspObjs3d *) self)->obj->colormap;
  return (NspObject *) ret;
}

static NspObject *_wrap_objs3d_get_obj_colormap(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspObjs3d *) self)->obj->colormap);
  return (NspObject *) ret;
}

static int _wrap_objs3d_set_colormap(void *self,const char *attr, NspObject *O)
{
  NspMatrix *colormap;

  if ( ! IsMat(O) ) return FAIL;
  if ((colormap = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspObjs3d *) self)->obj->colormap != NULL ) 
    nsp_matrix_destroy(((NspObjs3d *) self)->obj->colormap);
  ((NspObjs3d *) self)->obj->colormap= colormap;
  return OK;
}

static NspObject *_wrap_objs3d_get_alpha(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->alpha;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_objs3d_set_alpha(void *self,const char *attr, NspObject *O)
{
  double alpha;

  if ( DoubleScalar(O,&alpha) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->alpha= alpha;
  return OK;
}

static NspObject *_wrap_objs3d_get_theta(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->theta;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_objs3d_set_theta(void *self,const char *attr, NspObject *O)
{
  double theta;

  if ( DoubleScalar(O,&theta) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->theta= theta;
  return OK;
}

static NspObject *_wrap_objs3d_get_with_box(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspObjs3d *) self)->obj->with_box;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_objs3d_set_with_box(void *self,const char *attr, NspObject *O)
{
  int with_box;

  if ( BoolScalar(O,&with_box) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->with_box= with_box;
  return OK;
}

static NspObject *_wrap_objs3d_get_box_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspObjs3d *) self)->obj->box_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_objs3d_set_box_color(void *self,const char *attr, NspObject *O)
{
  int box_color;

  if ( IntScalar(O,&box_color) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->box_color= box_color;
  return OK;
}

static NspObject *_wrap_objs3d_get_box_style(void *self,const char *attr)
{
  int ret;

  ret = ((NspObjs3d *) self)->obj->box_style;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_objs3d_set_box_style(void *self,const char *attr, NspObject *O)
{
  int box_style;

  if ( IntScalar(O,&box_style) == FAIL) return FAIL;
  ((NspObjs3d *) self)->obj->box_style= box_style;
  return OK;
}

static AttrTab objs3d_attrs[] = {
  { "wrect", (attr_get_function *)_wrap_objs3d_get_wrect, (attr_set_function *)_wrap_objs3d_set_wrect,(attr_get_object_function *)_wrap_objs3d_get_obj_wrect, (attr_set_object_function *)int_set_object_failed },
  { "rho", (attr_get_function *)_wrap_objs3d_get_rho, (attr_set_function *)_wrap_objs3d_set_rho,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "top", (attr_get_function *)_wrap_objs3d_get_top, (attr_set_function *)_wrap_objs3d_set_top,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "arect", (attr_get_function *)_wrap_objs3d_get_arect, (attr_set_function *)_wrap_objs3d_set_arect,(attr_get_object_function *)_wrap_objs3d_get_obj_arect, (attr_set_object_function *)int_set_object_failed },
  { "frect", (attr_get_function *)_wrap_objs3d_get_frect, (attr_set_function *)_wrap_objs3d_set_frect,(attr_get_object_function *)_wrap_objs3d_get_obj_frect, (attr_set_object_function *)int_set_object_failed },
  { "title", (attr_get_function *)_wrap_objs3d_get_title, (attr_set_function *)_wrap_objs3d_set_title,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "children", (attr_get_function *)_wrap_objs3d_get_children, (attr_set_function *)_wrap_objs3d_set_children,(attr_get_object_function *)_wrap_objs3d_get_obj_children, (attr_set_object_function *)_wrap_objs3d_set_obj_children },
  { "colormap", (attr_get_function *)_wrap_objs3d_get_colormap, (attr_set_function *)_wrap_objs3d_set_colormap,(attr_get_object_function *)_wrap_objs3d_get_obj_colormap, (attr_set_object_function *)int_set_object_failed },
  { "alpha", (attr_get_function *)_wrap_objs3d_get_alpha, (attr_set_function *)_wrap_objs3d_set_alpha,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "theta", (attr_get_function *)_wrap_objs3d_get_theta, (attr_set_function *)_wrap_objs3d_set_theta,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "with_box", (attr_get_function *)_wrap_objs3d_get_with_box, (attr_set_function *)_wrap_objs3d_set_with_box,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "box_color", (attr_get_function *)_wrap_objs3d_get_box_color, (attr_set_function *)_wrap_objs3d_set_box_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "box_style", (attr_get_function *)_wrap_objs3d_get_box_style, (attr_set_function *)_wrap_objs3d_set_box_style,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 164 "codegen/objs3d.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_objs3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 1110 "objs3d.c"


#line 174 "codegen/objs3d.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_objs3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 1122 "objs3d.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Objs3d_func[]={
  {"extractelts_objs3d", _wrap_nsp_extractelts_objs3d},
  {"setrowscols_objs3d", _wrap_nsp_setrowscols_objs3d},
  { "objs3d_create", int_objs3d_create},
  { NULL, NULL}
};

/* call ith function in the Objs3d interface */

int Objs3d_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Objs3d_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Objs3d_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Objs3d_func[i].name;
  *f = Objs3d_func[i].fonc;
}

#line 184 "codegen/objs3d.override"

/* inserted verbatim at the end */

static void nsp_draw_objs3d(BCG *Xgc,NspGraphic *Obj, GdkRectangle *rect,void *data)
{
  int flag[]={1,2,4}, rep;
  char legend[]="X@Y@Z";
  GdkRectangle *r = data;
  char xf[]="onn";
  char strflag[]="151";
  double WRect[4],*wrect1,WRect1[4], FRect[4], ARect[4], inside_bounds[6];
  char logscale[2];
  int aaint[4]={10,2,10,2};
  Cell *cloc;
  NspList *L;
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( Obj->obj->show == FALSE ) return;

  /* check if the block is inside drawing rectangle
   */

  /*y
   * check if we are in the draw zone 
   */
  if ( data != NULL) 
    {
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
	      Sciprintf("No need to draw one objs3d [%d,%d,%d,%d] draw=[%d,%d,%d,%d]\n",
			r1.x,r1.y,r1.width,r1.height,
			r->x,r->y,r->width,r->height
			);
	      return;
	    }
	  else
	    {
	      Sciprintf("Drawing objs3d\n");
	    }
	}
      else
	{
	  Sciprintf("draw objs3d for non top level to be done \n");
	}
    }
  
  /* draw elements */
  L = P->obj->children;
  cloc = L->first ;
  /* we change the scale according to the objs3d */
  getscale2d(Xgc,WRect,FRect,logscale,ARect);
  if ( P->obj->top == TRUE ) 
    {
      /* This is a top level objs3d, wrect gives the objs3d position in the 
       * enclosing graphic window. 
       */
      if ( Xgc->scales->scale_3drot_flag != 1) 
	set_scale(Xgc,"fTffft",P->obj->wrect->R,NULL,NULL,NULL,P->obj->arect->R);
      wrect1= P->obj->wrect->R;
    }
  else 
    {
      /* This is not a top level objs3d, we draw its enclosing rectangle 
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
  /* set_scale(Xgc,"fTtfff",WRect1,P->obj->frect->R,NULL,NULL,NULL); */
  
  if ( Xgc->scales->scale_3drot_flag != 1) 
    nsp_axes_update_frame_bounds(Xgc,wrect1,P->obj->frect->R,
				 P->obj->arect->R,
				 aaint,
				 TRUE,
				 TRUE,
				 xf);
  
  nsp_objs3d_compute_inside_bounds(Xgc,Obj,inside_bounds);
  axis_draw(Xgc, (strlen(strflag) >= 3) ? strflag[2] : '1', 
	    (strlen(strflag) >= 2) ? strflag[1] : '6', -1);

  frame_clip_on(Xgc);
  
  rep = Xgc->graphic_engine->xpush_colormap(Xgc,P->obj->colormap);

  nsp_draw_objs3d_s2(Xgc,P,P->obj->theta,P->obj->alpha,legend,flag,inside_bounds,
		     P->obj->with_box,P->obj->box_color,P->obj->box_style);

  if ( rep == OK)  Xgc->graphic_engine->xpop_colormap(Xgc);

  /* Note that clipping is wrong when an axe is rotated 
   * since clipping only works with rectangles 
   */
  frame_clip_off(Xgc);
  /* title if present */
  if ( P->obj->title[0] != '\0') 
    Xgc->graphic_engine->scale->displaystringa(Xgc,P->obj->title,1);
  /* scale back */
  /* set_scale(Xgc,"fTtfft",WRect,FRect,NULL,NULL,ARect); */
  if (  P->obj->top != TRUE )
    {
      Xgc->scales->cosa=1.0;
      Xgc->scales->sina=0.0;
    }
}



/* compute the bounds of the set of objects countained in the 
 * objs3d 
 */

static void nsp_objs3d_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  int i;
  double l_bounds[6];
  Cell *cloc;
  NspList *L;
  NspObjs3d *P = (NspObjs3d *) Obj;
  L = P->obj->children;
  cloc = L->first ;
  
  if ( cloc == NULLCELL) 
    {
      bounds[0]=bounds[1]=bounds[2]=bounds[3]=bounds[4]=bounds[5]=0;
      return;
    }
  
  bounds[0]=bounds[2]=bounds[4]=LARGEST_REAL;
  bounds[1]=bounds[3]=bounds[5]=-LARGEST_REAL;

  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  if ( G->type->bounds(G,l_bounds) == TRUE )
	    for ( i = 0 ; i < 3 ; i++) 
	      {
		if ( l_bounds[2*i] < bounds[2*i] )   bounds[2*i]= l_bounds[2*i];
		if ( l_bounds[2*i+1] > bounds[2*i+1])   bounds[2*i+1]= l_bounds[2*i+1];
	      }
	}
      cloc = cloc->next;
    }
}

static void nsp_translate_objs3d(NspGraphic *Obj,const double *tr)
{
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( P->obj->top == TRUE) return ;
  P->obj->wrect->R[0] += tr[0];
  P->obj->wrect->R[1] += tr[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_objs3d(NspGraphic *Obj,double *R)
{
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( P->obj->top == TRUE) return ;
  Sciprintf("we should get a double here for rho\n");
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_objs3d(NspGraphic *Obj,double *alpha)
{
  NspObjs3d *P = (NspObjs3d *) Obj;
  if ( P->obj->top == TRUE) return ;
  P->obj->wrect->R[2] *= alpha[0];
  P->obj->wrect->R[3] *= alpha[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of objs3d 
 *
 */

static int nsp_getbounds_objs3d(NspGraphic *Obj,double *bounds)
{
  NspObjs3d *P = (NspObjs3d *) Obj;
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

static void nsp_objs3d_link_figure(NspGraphic *G, void *F, void *A)
{
  /* link toplevel, take care to use nsp_graphic field */
  nsp_graphic_link_figure(G, F, ((NspObjs3d *) G)->obj);
  /* link children */
  nsp_list_link_figure(((NspObjs3d *) G)->obj->children,F,  ((NspObjs3d *) G)->obj);
}


static void nsp_objs3d_unlink_figure(NspGraphic *G, void *F)
{
  /* link toplevel */
  nsp_graphic_unlink_figure(G, F);
  /* link children */
  nsp_list_unlink_figure(((NspObjs3d *) G)->obj->children,F);
}

static NspList *nsp_objs3d_children(NspGraphic *Obj)
{
  return  ((NspObjs3d *) Obj)->obj->children;
}


static void nsp_draw_3d_obj_ogl( BCG *Xgc,NspObjs3d *,double theta,double alpha,const char *legend,
				 int *flag,double *ebox,int with_box,int box_color,int box_style);




/* Author: Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> */

static void nsp_draw_objs3d_s2( BCG *Xgc,NspObjs3d *Obj,double theta,double alpha,const char *legend,
				int *flag,double *ebox,int with_box,int box_color,int box_style)
{
  NspObject **objs_array= NULL;
  Cell *cloc;
  NspList *Children;
  nsp_box_3d box;
  double x[2],y[2],zz[2],zzmin,zzmax;
  int two=2;
  double Box[6]={ebox[0],ebox[2],ebox[4],ebox[1],ebox[3],ebox[5]};
  int nf=0,nbObj;
  int i, j, k, n, *p;
  HFstruct *HF;
  double lim[3], *z;
  Plot3dBox *B=NULL;
  int flagx;
  /* should be shared */
  int foreground_color;
  int background_color;

#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_draw_3d_obj_ogl(Xgc,Obj,theta,alpha,legend,flag,ebox,with_box,box_color,box_style);
      nsp_ogl_set_2dview(Xgc);
      return; 
    }
#endif
    
  flagx = Xgc->graphic_engine->xget_last(Xgc);
  /* XXX */
  foreground_color = flagx+1;
  background_color = flagx+2;

  /* allocate a structure for drawing purpose 
   * The unchanged values are kept in Lobj
   */

  /* Obj = (Obj3d *)obj3d_from_list(SciStack,Lobj,TRUE,&err,&nf,&nbObj) ; */
  Children = nsp_objs3d_children((NspGraphic *) Obj);
  nbObj = nsp_list_length(Children);
  /* we have to loop here to collect the number of faces */
  cloc = Children->first ;
  nf = 0;

  while ( cloc != NULLCELL ) 
    {
      NspGraphic *G = (NspGraphic *) cloc->O;
      nf += G->type->n_faces(Xgc,G);
      cloc = cloc->next;
    }

  x[0]= Box[0];y[0]= Box[1];zz[0]= Box[2];
  x[1]= Box[3];y[1]= Box[4];zz[1]= Box[5];
  
  nsp_plot3d_update_bounds(Xgc,"box",x,y,zz,&two,&two,&theta,&alpha,legend,&flag[1],
			   ebox,&zzmin,&zzmax,param3d_t);
  SetEch3d1(Xgc,&box,ebox,theta,alpha,(long)(flag[1]+1)/2);

#ifdef WITH_GTKGLEXT 
  /* transmit info to opengl pretending we are doing 2d !!! */
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_2dview(Xgc);
    }
#endif
  
  lim[0] = 1.e+10;
  lim[1] = 1.e+10;
  lim[2] = - 1.e+10;

  /* fabrication de la boite et calcul de ses coordonnees ds le repere local */
  if ( with_box == TRUE ) B = make_box(Xgc,Box, BTRUE, box_style,box_color, lim);
 
#ifdef ALLOC_OBJ
  HF= malloc( nf * sizeof(HFstruct) );
  z = malloc( nf * sizeof(double) );
  p = malloc( nf * sizeof(int) );
  /* just to accelerate next step */
  objs_array = malloc( nbObj*sizeof(NspObject *));
#else 
  HF= graphic_alloc(7, nf,  sizeof(HFstruct) );
  z = graphic_alloc(8, nf,  sizeof(double) );
  p = graphic_alloc(9, nf,  sizeof(int) );
  objs_array = graphic_alloc(10, nbObj,sizeof(NspObject *));
#endif 


  /* just to accelerate next step */
  objs_array = malloc( nbObj*sizeof(NspObject *));

  cloc = Children->first ;
  n=0;
  while ( cloc != NULLCELL ) 
    {
      objs_array[n++]= cloc->O; 
      cloc = cloc->next;
    }
  
  /* step 1 : for each object :
   *            a/ get the coordinates in the local repair
   *               and determines the pos of each point within the pyramidal
   *               visible region (IN, OUT_XY, OUT_Z)
   *            b/ then add the visible parts (faces, segments, points) in the z 
   *               and HF arrays for the hidden face algorithm (only partial visible 
   *               parts without any OUT_Z point are included)
   */
  n = 0;k=0;
  cloc = Children->first ;
  nf = 0;
  while ( cloc != NULLCELL ) 
    {
      NspGraphic *G = (NspGraphic *) cloc->O;
      G->type->zmean(Xgc,G,z,HF,&n,k,lim);
      k++;
      cloc = cloc->next;
    }
  /*  step 3 : sort of all the a priori visible "faces" (faces, segments, points) */
  nsp_obj3d_dsortc(z, &n, p);

  /* step 4 : drawing of each faces */
  if ( with_box == TRUE  ) nsp_obj3d_draw_box(Xgc,B);

  for (i = n -1 ; i >= 0 ; i--)
    {
      k = HF[p[i]].num_obj;  /* numero de l'objet correspondant a cette "face" */
      j = HF[p[i]].num_in_obj; /* son numéro de face dans l'objet en question */
      /* dessin partiel de l'objet en utilisant la face j */
      /* XXX OBJ3D(Obj[k].obj)->draw_partial(Xgc,Obj[k].obj,j); */
      ((NspGraphic *) objs_array[k])->type->draw(Xgc,(NspGraphic *) objs_array[k],NULL,&j);
    }
  if ( with_box == TRUE  &&  B->box_style == SCILAB )  nsp_obj3d_draw_near_box_segments(Xgc,B);
  if ( with_box == TRUE ) nsp_obj3d_free_box(B);

#ifdef ALLOC_OBJ
  free(HF);
  free(z);
  free(p);
#endif 
}

#ifdef  WITH_GTKGLEXT 

static void nsp_draw_3d_obj_ogl( BCG *Xgc,NspObjs3d *Obj,double theta,double alpha,const char *legend,
				 int *flag,double *ebox,int with_box,int box_color,int box_style)
{
  Cell *cloc;
  NspList *Children;
  /* Stack stack;*/ /* just used for messages i.e NspFname(stack) */
  nsp_box_3d box;
  double x[2],y[2],zz[2],zzmin,zzmax;
  double Box[6]={ebox[0],ebox[2],ebox[4],ebox[1],ebox[3],ebox[5]};
  int flagx, nf=0, two=2;
  double lim[3];
  Plot3dBox *B;
  /* should be shared */
  int foreground_color;
  int background_color;

  /* NspFname(stack) ="drawobj"; */
  flagx = Xgc->graphic_engine->xget_last(Xgc);
  foreground_color = flagx+1;
  background_color = flagx+2;

  /* allocate a structure for drawing purpose 
   * The unchanged values are kept in Lobj
   */
  
  x[0]= Box[0];y[0]= Box[1];zz[0]= Box[2];
  x[1]= Box[3];y[1]= Box[4];zz[1]= Box[5];

  nsp_plot3d_update_bounds(Xgc,"box",x,y,zz,&two,&two,&theta,&alpha,legend,&flag[1],
			   ebox,&zzmin,&zzmax,param3d_t);
  SetEch3d1(Xgc,&box,ebox,theta,alpha,(long)(flag[1]+1)/2);
  
  lim[0] = 1.e+10;
  lim[1] = 1.e+10;
  lim[2] = - 1.e+10;

  /* fabrication de la boite et calcul de ses coordonnees ds le repere local */
  if ( with_box == TRUE  )
    {
      B = make_box(Xgc,Box, BTRUE, box_style,box_color,lim);
      nsp_obj3d_draw_box(Xgc,B);
      if (B->box_style == SCILAB ) nsp_obj3d_draw_near_box_segments(Xgc,B);
      nsp_obj3d_free_box(B);
    }

  Children = nsp_objs3d_children((NspGraphic *) Obj);
  cloc = Children->first ;
  nf = 0;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspGraphic *G= (NspGraphic *) cloc->O;
	  nsp_ogl_set_3dview(Xgc);
	  G->type->draw(Xgc,G,NULL,NULL);
	}
      cloc = cloc->next;
    }
}
#endif 

/* Obj is a Figure 
 *
 */

void nsp_figure_change3d_orientation(BCG *Xgc,NspGraphic *Obj,double theta, double alpha,int *pt)
{
  NspObjs3d *Obj3d;
  Obj3d = (NspObjs3d *) nsp_check_pt_axes_or_objs3d(Xgc,pt);
  if ( Obj3d == NULL) return;
  if ( !IsObjs3d(NSP_OBJECT(Obj3d))) return;
  Obj3d->obj->alpha = alpha;
  Obj3d->obj->theta = theta;
  nsp_objs3d_invalidate((NspGraphic *) Obj3d);
}




/* 
 *  Compute for each point the local coordinate (in the visualisation repair)
 *  then applies the perpective transform and test if the point is inside the
 *  troncated vision pyramide :  | loc_x | <= lim[0]
 *                               | loc_y | <= lim[1]
 *                               | loc_z | >= lim[2] 
 */

void apply_transforms_new1(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord)
{
  int i, k=0;
  double facteur;
  for (i = 0; i < 3*ncoord ; i += 3)
    {
      /* take care that Coord and M can point to the same location 
       * thus we have to copy
       */
      double v[3];
      v[0] = M[i];v[1] = M[i+1]; v[2] = M[i+2]; 
      Coord[i]   = TRX(Xgc->scales,v[0],v[1],v[2]);
      Coord[i+1] = TRY(Xgc->scales,v[0],v[1],v[2]);
      Coord[i+2] = TRZ(Xgc->scales,v[0],v[1],v[2]);
      if ( Coord[i+2] < lim[2] )  
	{
	  pos[k] = OUT_Z; /* dans ce cas on applique pas la perspective */
	}
      else
	{
	  /* on applique la perspective */
	  facteur = 1.0/Coord[i+2];
	  facteur = 1.0;
	  Coord[i]   = facteur*Coord[i];
	  Coord[i+1] = facteur*Coord[i+1];
	  /* le point est-il dans le rectangle de visu ? */
	  if ( fabs(Coord[i]) > lim[0] || fabs(Coord[i+1]) > lim[1] ) 
	    pos[k] = OUT_XY;
	  else
	    pos[k] = VIN;
	}
      k++;
    }
}

/* similar but with a transposed Coord
 *
 */

void apply_transforms_new(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord)
{
  int i, k=0;
  double facteur;
  for (i = 0; i < ncoord ; i++ )
    {
      /* take care that Coord and M can point to the same location 
       * thus we have to copy
       */
      double v[3];
      v[0] = M[i];v[1] = M[i+ncoord]; v[2] = M[i+2*ncoord]; 
      Coord[i]   = TRX(Xgc->scales,v[0],v[1],v[2]);
      Coord[i+ncoord] = TRY(Xgc->scales,v[0],v[1],v[2]);
      Coord[i+2*ncoord] = TRZ(Xgc->scales,v[0],v[1],v[2]);
      if ( Coord[i+2*ncoord] < lim[2] )  
	{
	  pos[k] = OUT_Z; /* dans ce cas on applique pas la perspective */
	}
      else
	{
	  /* on applique la perspective */
	  facteur = 1.0/Coord[i+2*ncoord];
	  facteur = 1.0;
	  Coord[i]   = facteur*Coord[i];
	  Coord[i+ncoord] = facteur*Coord[i+ncoord];
	  /* le point est-il dans le rectangle de visu ? */
	  if ( fabs(Coord[i]) > lim[0] || fabs(Coord[i+ncoord]) > lim[1] ) 
	    pos[k] = OUT_XY;
	  else
	    pos[k] = VIN;
	}
      k++;
    }
}


/* A set of functions for 3D box which could be moved 
 *
 */


static int select_box_vertex(const double coord[]);
static void compute_ticks(double *vmin, double *vmax, double **Ticks, int *Nb_ticks);
static int build_ticks_segment(Plot3dBox *B, double xmin, double xmax, 
			       double ymin, double ymax, double zmin, double zmax);
static void build_xtick_seg(double *coord, int *num_sg, double x, double axe[], double sens[]);
static void build_ytick_seg(double *coord, int *num_sg, double y, double axe[], double sens[]);
static void build_ztick_seg(double *coord, int *num_sg, double z, double axe[], double sens[]);
static int  build_box_others_segment(Plot3dBox *B, double xmin, double xmax, double ymin, double ymax, double zmin, double zmax);
static void build_x_seg(double *coord, int *num_sg, double xmin, double xmax, double y, double z);
static void build_y_seg(double *coord, int *num_sg, double x, double ymin, double ymax, double z);
static void build_z_seg(double *coord, int *num_sg, double x, double y, double zmin, double zmax);

static void draw_far_box_segments(BCG *Xgc,Plot3dBox *B);
static void draw_tick(BCG *Xgc,Plot3dBox *B,double val,const double coord[]);
static void draw_segment_bis(BCG *Xgc,double coord[], int ns, int color);
static void draw_segment(BCG *Xgc,double coord[], int ia, int ib, int color);
static void draw_justified_string(BCG *Xgc,char *str, double xx, double yy, int xj, int yj);
static void draw_box_face(BCG *Xgc,Plot3dBox *B, int j);

static int ticks_font_type = 2;
static int ticks_font_size = 1;
static int foreground_color = 1;
static const int box_segments[24] = {0,2,  0,5,  0,6,  1,3,  1,4,  1,7,  2,4,  2,7,  3,5,  3,6,  4,6,  5,7};
static const int box_faces[24] = {0,5,3,6,  0,6,4,2,  0,2,7,5,  1,7,2,4,  1,3,5,7,  1,4,6,3};


static Plot3dBox* make_box(BCG *Xgc,double Box[], GBoolean with_ticks, BoxStyle box_style,int box_color, double lim[])
{
  /*                      s0    s1    s2    s3    s4    s5    s6    s7    s8    s9    s10   s11  */

#ifdef WITH_GTKGLEXT 
  double coord[24];
#endif
  Plot3dBox *B;
  double xmin, ymin, zmin, xmax, ymax, zmax;

  B = malloc(sizeof(Plot3dBox));

  xmin = Box[0]; ymin = Box[1]; zmin = Box[2];
  xmax = Box[3]; ymax = Box[4]; zmax = Box[5];
  
  if ( with_ticks )
    {
      B->with_ticks = BTRUE;
      compute_ticks(&xmin, &xmax, &(B->xticks), &(B->nb_xticks));
      compute_ticks(&ymin, &ymax, &(B->yticks), &(B->nb_yticks));
      if ( 1) /* -0.95 <= P[8]  &&  P[8] <= 0.95 ) */
	compute_ticks(&zmin, &zmax, &(B->zticks), &(B->nb_zticks));
      else
	{ B->nb_zticks = 0; B->zticks = NULL; }
      B->nb_xyz_ticks = B->nb_xticks + B->nb_yticks + B->nb_zticks;
    }
  else
    B->with_ticks = BFALSE;
  
  B->box_style = box_style;
  B->segment = box_segments;
  B->face = box_faces;

  if ( box_color == -1 )
    {

      /* last +3 is a light gray */
      box_color = Xgc->graphic_engine->xget_last(Xgc)+3;
    }

  B->color = box_color;
  
  B->coord[0]  = xmin; B->coord[1]  = ymin; B->coord[2]  = zmin; //1
  B->coord[3]  = xmax; B->coord[4]  = ymax; B->coord[5]  = zmax; //2
  B->coord[6]  = xmin; B->coord[7]  = ymin; B->coord[8]  = zmax; //3
  B->coord[9]  = xmax; B->coord[10] = ymax; B->coord[11] = zmin; //4
  B->coord[12] = xmax; B->coord[13] = ymin; B->coord[14] = zmax; //5
  B->coord[15] = xmin; B->coord[16] = ymax; B->coord[17] = zmin; //6
  B->coord[18] = xmax; B->coord[19] = ymin; B->coord[20] = zmin; //7
  B->coord[21] = xmin; B->coord[22] = ymax; B->coord[23] = zmax; //8

#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      /* in open_gl we do not want to change coordinates */
      apply_transforms_new1(Xgc,coord,B->coord, B->pos, lim, 8);
      B->inear = select_box_vertex(coord);       
    }
  else 
#endif
    {
      apply_transforms_new1(Xgc,B->coord,B->coord, B->pos, lim, 8);
      B->inear = select_box_vertex(B->coord);      
    }
  
  if ( B->with_ticks )
    {
      build_ticks_segment(B, xmin, xmax, ymin, ymax, zmin, zmax);
      B->ticks_pos = malloc(2*(B->nb_xyz_ticks)*sizeof(VisionPos));
#ifdef WITH_GTKGLEXT 
      if ( Xgc->graphic_engine != &GL_gengine ) 
#endif	
	apply_transforms_new1(Xgc, B->ticks_coord,B->ticks_coord, B->ticks_pos, lim, 2*(B->nb_xyz_ticks)); 
    }
  if ( B->with_ticks  &&  B->box_style == MATLAB )
    {
      build_box_others_segment(B, xmin, xmax, ymin, ymax, zmin, zmax);
      B->others_pos = malloc(4*(B->nb_xyz_ticks)*sizeof(VisionPos));
#ifdef WITH_GTKGLEXT 
      if ( Xgc->graphic_engine != &GL_gengine ) 
#endif	
	apply_transforms_new1(Xgc, B->others_coord,B->others_coord, B->others_pos, lim, 4*(B->nb_xyz_ticks));
    }      
  return ( B );
}

static int  build_ticks_segment(Plot3dBox *B, double xmin, double xmax, 
				double ymin, double ymax, double zmin, double zmax)
{
  int i, sg=0;
  double d, e, axe_x[2], axe_y[2], axe_z[2], sens_x[2], sens_y[2], sens_z[2];

  d = (xmax-xmin + ymax-ymin + zmax-zmin)/100; /* a voir */
  e = d/sqrt(2.0);
  
  B->ticks_coord = malloc( 6*(B->nb_xyz_ticks)*sizeof(double) );
  if ( B->ticks_coord == NULL) return FAIL;
  switch(B->inear)
    {
    default:
    case(0):
      axe_x[0] = ymin; axe_x[1] = zmax; sens_x[0] =-d; sens_x[1] = 0;
      axe_y[0] = xmin; axe_y[1] = zmax; sens_y[0] =-d; sens_y[1] = 0;
      axe_z[0] = xmax; axe_z[1] = ymin; sens_z[0] = d; sens_z[1] = 0;
      sens_z[0] = e; sens_z[1] =-e;
      break;
    case(1):
      axe_x[0] = ymax; axe_x[1] = zmin; sens_x[0] = d; sens_x[1] = 0;
      axe_y[0] = xmax; axe_y[1] = zmin; sens_y[0] = d; sens_y[1] = 0;
      axe_z[0] = xmax; axe_z[1] = ymin; sens_z[0] = 0; sens_z[1] =-d;
      sens_z[0] = e; sens_z[1] =-e;
      break;
    case(2):
      axe_x[0] = ymin; axe_x[1] = zmin; sens_x[0] =-d; sens_x[1] = 0;
      axe_y[0] = xmin; axe_y[1] = zmin; sens_y[0] =-d; sens_y[1] = 0;
      axe_z[0] = xmin; axe_z[1] = ymax; sens_z[0] = 0; sens_z[1] = d;
      sens_z[0] =-e; sens_z[1] = e;
      break;
    case(3):
      axe_x[0] = ymax; axe_x[1] = zmax; sens_x[0] = d; sens_x[1] = 0;
      axe_y[0] = xmax; axe_y[1] = zmax; sens_y[0] = d; sens_y[1] = 0;
      axe_z[0] = xmin; axe_z[1] = ymax; sens_z[0] =-d; sens_z[1] = 0;
      sens_z[0] =-e; sens_z[1] = e;
      break;
    case(4):
      axe_x[0] = ymin; axe_x[1] = zmin; sens_x[0] =-d; sens_x[1] = 0;
      axe_y[0] = xmax; axe_y[1] = zmin; sens_y[0] = d; sens_y[1] = 0;
      axe_z[0] = xmin; axe_z[1] = ymin; sens_z[0] =-d; sens_z[1] = 0;
      sens_z[0] =-e; sens_z[1] =-e;
      break;
    case(5):
      axe_x[0] = ymax; axe_x[1] = zmax; sens_x[0] = d; sens_x[1] = 0;
      axe_y[0] = xmin; axe_y[1] = zmax; sens_y[0] =-d; sens_y[1] = 0;
      axe_z[0] = xmin; axe_z[1] = ymin; sens_z[0] = 0; sens_z[1] =-d;
      sens_z[0] =-e; sens_z[1] =-e;
      break;
    case(6):
      axe_x[0] = ymin; axe_x[1] = zmax; sens_x[0] =-d; sens_x[1] = 0;
      axe_y[0] = xmax; axe_y[1] = zmax; sens_y[0] = d; sens_y[1] = 0;
      axe_z[0] = xmax; axe_z[1] = ymax; sens_z[0] = 0; sens_z[1] = d;
      sens_z[0] = e; sens_z[1] = e;
      break;
    case(7):
      axe_x[0] = ymax; axe_x[1] = zmin; sens_x[0] = d; sens_x[1] = 0;
      axe_y[0] = xmin; axe_y[1] = zmin; sens_y[0] =-d; sens_y[1] = 0;
      axe_z[0] = xmax; axe_z[1] = ymax; sens_z[0] = d; sens_z[1] = 0;
      sens_z[0] = e; sens_z[1] = e;
      break;
    }

  for ( i = 0 ; i < B->nb_xticks ; i++)
    build_xtick_seg(B->ticks_coord, &sg, B->xticks[i], axe_x, sens_x);
  for ( i = 0 ; i < B->nb_yticks ; i++)
    build_ytick_seg(B->ticks_coord, &sg, B->yticks[i], axe_y, sens_y);
  for ( i = 0 ; i < B->nb_zticks ; i++)
    build_ztick_seg(B->ticks_coord, &sg, B->zticks[i], axe_z, sens_z);
  return OK;
}

static void build_xtick_seg(double *coord, int *num_sg, double x, double axe[], double sens[])
{
  int sg = *num_sg;
  coord[6*sg]   = x; coord[6*sg+1] = axe[0];         coord[6*sg+2] = axe[1];
  coord[6*sg+3] = x; coord[6*sg+4] = axe[0]+sens[0]; coord[6*sg+5] = axe[1]+sens[1];
  (*num_sg)++;
}

static void build_ytick_seg(double *coord, int *num_sg, double y, double axe[], double sens[])
{
  int sg = *num_sg;
  coord[6*sg]   = axe[0];         coord[6*sg+1] = y; coord[6*sg+2] = axe[1];
  coord[6*sg+3] = axe[0]+sens[0]; coord[6*sg+4] = y; coord[6*sg+5] = axe[1]+sens[1];
  (*num_sg)++;
}

static void build_ztick_seg(double *coord, int *num_sg, double z, double axe[], double sens[])
{
  int sg = *num_sg;
  coord[6*sg]   = axe[0];         coord[6*sg+1] = axe[1];         coord[6*sg+2] = z;
  coord[6*sg+3] = axe[0]+sens[0]; coord[6*sg+4] = axe[1]+sens[1]; coord[6*sg+5] = z;
  (*num_sg)++;
}

static int build_box_others_segment(Plot3dBox *B, double xmin, double xmax, 
				    double ymin, double ymax, double zmin, double zmax)
{
  /* compute the 3 faces where these segments are drawn */
  int f[3], i, j, k, nf=0, sg=0;
  double x, y, z;
  GBoolean GOK;

  for ( k = 0 ; k < 6 ; k++ )
    {
      GOK = BTRUE;
      for ( j = 0; j < 4 && GOK ; j++ )
	GOK = B->face[4*k+j] != B->inear;
      if ( GOK ) { f[nf] = k; nf++; };
    }

  /* face:   0 <-> z=zmin, 1 <-> y=ymin, 2 <-> x=xmin
   *	     3 <-> z=zmax, 4 <-> y=ymax, 3 <-> x=xmax
   */
  B->others_coord = malloc( 12*(B->nb_xyz_ticks)*sizeof(double) );
  if ( B->others_coord == NULL) return FAIL;
  for ( nf = 0 ; nf < 3 ; nf++ )
    {
      if ( f[nf] == 0 || f[nf] == 3 )     /* z = cte */
	{
	  if ( f[nf] == 0 ) z = zmin; else z = zmax;
	  for ( i = 0 ; i < B->nb_xticks ; i++)
	    build_y_seg(B->others_coord, &sg, B->xticks[i], ymin, ymax, z);
 	  for ( i = 0 ; i < B->nb_yticks ; i++)
	    build_x_seg(B->others_coord, &sg, xmin, xmax, B->yticks[i], z);
	}
      else if ( f[nf] == 1 || f[nf] == 4 )  /* y = cte */
	{
	  if ( f[nf] == 1 ) y = ymin; else y = ymax;
	  for ( i = 0 ; i < B->nb_xticks ; i++)
	    build_z_seg(B->others_coord, &sg, B->xticks[i], y, zmin, zmax);
	  for ( i = 0 ; i < B->nb_zticks ; i++)
	    build_x_seg(B->others_coord, &sg, xmin, xmax, y, B->zticks[i]);
	}
      else if ( f[nf] == 2 || f[nf] == 5 ) /* x = cte */
	{
	  if ( f[nf] == 2 ) x = xmin; else x = xmax;
	  for ( i = 0 ; i < B->nb_yticks ; i++)
	    build_z_seg(B->others_coord, &sg, x, B->yticks[i], zmin, zmax);
	  for ( i = 0 ; i < B->nb_zticks ; i++)
	    build_y_seg(B->others_coord, &sg, x, ymin, ymax, B->zticks[i]);
	}
    }
  return OK;
}

static void build_x_seg(double *coord, int *num_sg, double xmin, double xmax, double y, double z)
{
  int sg = *num_sg;
  coord[6*sg]   = xmin; coord[6*sg+1] = y; coord[6*sg+2] = z;
  coord[6*sg+3] = xmax; coord[6*sg+4] = y; coord[6*sg+5] = z;
  (*num_sg)++;
}

static void build_y_seg(double *coord, int *num_sg, double x, double ymin, double ymax, double z)
{
  int sg = *num_sg;
  coord[6*sg]   = x; coord[6*sg+1] = ymin; coord[6*sg+2] = z;
  coord[6*sg+3] = x; coord[6*sg+4] = ymax; coord[6*sg+5] = z;
  (*num_sg)++;
}

static void build_z_seg(double *coord, int *num_sg, double x, double y, double zmin, double zmax)
{
  int sg = *num_sg;
  coord[6*sg]   = x; coord[6*sg+1] = y; coord[6*sg+2] = zmin;
  coord[6*sg+3] = x; coord[6*sg+4] = y; coord[6*sg+5] = zmax;
  (*num_sg)++;
}


static void compute_ticks(double *vmin, double *vmax, double **Ticks, int *Nb_ticks)
{ 
  int i, j, first, last, inc=1, nb_grad, nb_ticks;
  double work[20], *ticks;
  
  gr_compute_ticks(vmin, vmax, work, &nb_grad);
  if ( nb_grad <= 2 )
    {
      nb_ticks = 2; work[0] = *vmin; work[1] = *vmax;
      first = 0;
    }
  else
    {
      if ( work[0] < *vmin ) 
	first = 1; 
      else 
	first = 0;
      if ( work[nb_grad-1] > *vmax ) 
	last = nb_grad-2; 
      else 
	last = nb_grad-1;
      
      nb_ticks = last - first + 1;
      
      if ( nb_ticks < 2 )
	{
	  nb_ticks = 2; work[0] = *vmin; work[1] = *vmax;
	  first = 0;
	}
      else if ( nb_ticks > 8 ) 
	{
	  nb_ticks = (nb_ticks+1)/2; inc = 2; 
	}
    }
  
  ticks = malloc(nb_ticks*sizeof(double));
  for ( i = 0, j = first ; i < nb_ticks ; i++, j+= inc )
    ticks[i] = work[j];

  *Nb_ticks = nb_ticks;
  *Ticks = ticks;

  return;
}

static int select_box_vertex(const double coord[])
{
  /* selectionne le sommet le plus proche de la camera */
  int k, imax = 0 ;
  double zmax = coord[2];

  for ( k = 1 ; k < 8 ; k++ )
    if ( coord[3*k+2] > zmax )
      {
	imax = k;
	zmax = coord[3*k+2];
      };
  return ( imax );
}



static void nsp_obj3d_draw_box(BCG *Xgc,Plot3dBox *B)
{
  int k, j, b0;
  GBoolean GOK;
  if ( B->box_style == SCILAB )
    draw_far_box_segments(Xgc,B);
  else
    for ( k = 0 ; k < 6 ; k++ )
      {
	GOK = BTRUE;
	for ( j = 0; j < 4 && GOK ; j++ )
	  {
	    int face =  B->face[4*k+j];
	    GOK = face != B->inear  &&  B->pos[face] != OUT_Z;
	  }
	if ( GOK )
	  draw_box_face(Xgc,B, k);
      }

  if ( B->with_ticks )
    {
      for ( j = 0 ; j < B->nb_xyz_ticks ; j++)
	draw_segment_bis(Xgc,B->ticks_coord, j, foreground_color);

      Xgc->graphic_engine->xset_font(Xgc,(ticks_font_type),(ticks_font_size));
      for ( j = 0 ; j < B->nb_xticks ; j++)
	draw_tick(Xgc,B,B->xticks[j], &(B->ticks_coord[6*j]));
      b0 = 6*B->nb_xticks;
      for ( j = 0 ; j < B->nb_yticks ; j++)
	draw_tick(Xgc,B,B->yticks[j], &(B->ticks_coord[6*j+b0]));
      b0 += 6*B->nb_yticks;
      for ( j = 0 ; j < B->nb_zticks ; j++)
	draw_tick(Xgc,B,B->zticks[j], &(B->ticks_coord[6*j+b0]));

      if ( B->box_style == MATLAB )
	{
	  Xgc->graphic_engine->xset_line_style(Xgc,2);
	  for ( j = 0 ; j < 2*(B->nb_xyz_ticks) ; j++)
	    draw_segment_bis(Xgc,B->others_coord, j, foreground_color);
	  Xgc->graphic_engine->xset_line_style(Xgc,1);
	}
    }
}

static void draw_tick(BCG *Xgc,Plot3dBox *B,double val,const double coord[])
{
  double xt, yt, vxt, vyt, normv, lim = 0.7071068;
  int xj, yj;
  char buf[60];

#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      const double lim[] ={ 1.e+10,  1.e+10, - 1.e+10};
      /* we move to 2d scale */
      double Tcoord[6];
      apply_transforms_new1(Xgc,Tcoord,coord, B->pos,lim,2); 
      vxt = Tcoord[3] - Tcoord[0];
      vyt = Tcoord[4] - Tcoord[1];
      xt = Tcoord[3] + 0.6*vxt;
      yt = Tcoord[4] + 0.6*vyt;
    } 
  else 
#endif 
    {
      vxt = coord[3] - coord[0];
      vyt = coord[4] - coord[1];
      xt = coord[3] + 0.6*vxt;
      yt = coord[4] + 0.6*vyt;
    }
  normv = sqrt( vxt*vxt + vyt*vyt );
  vxt = vxt/normv;
  vyt = vyt/normv;
  if ( vxt >= lim )
    { xj = LEFT; yj = CENTER; }
  else if ( vxt <= -lim )
    { xj = RIGHT; yj = CENTER; }
  else if ( vyt >= lim )
    { xj = CENTER; yj = UP; }
  else
    { xj = CENTER; yj = DOWN; }
  sprintf(buf, "%g", val);
  xt = XScale(Xgc->scales,xt);
  yt = YScale(Xgc->scales,yt);
#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_2dview(Xgc);
      draw_justified_string(Xgc,buf, xt, yt, xj, yj);
      nsp_ogl_set_3dview(Xgc);
      return ;
    }
#endif 
  draw_justified_string(Xgc,buf, xt, yt, xj, yj);
}

static void draw_far_box_segments(BCG *Xgc,Plot3dBox *B)
{
  /* dessine les segments n'ayant pas le point inear comme sommet */
  int k, ia, ib;

  for ( k = 0 ; k < 12 ; k++ )
    {
      ia = B->segment[2*k]; ib = B->segment[2*k+1];
      if ( ia != B->inear  &&  ib != B->inear )
	draw_segment(Xgc,B->coord, ia, ib, foreground_color);
    };
}

static void nsp_obj3d_draw_near_box_segments(BCG *Xgc,Plot3dBox *B)
{
  /* dessine les segments ayant le point inear comme sommet */
  int k, ia, ib;
  if (B->pos[B->inear] == OUT_Z )
    return;
  Xgc->graphic_engine->xset_line_style(Xgc,2); 
  for ( k = 0 ; k < 12 ; k++ )
    {
      ia = B->segment[2*k]; ib = B->segment[2*k+1];
      if ( ia == B->inear ||  ib == B->inear )
	draw_segment(Xgc,B->coord, ia, ib, foreground_color);
    };
  Xgc->graphic_engine->xset_line_style(Xgc,1); 
}

static void draw_segment(BCG *Xgc,double coord[], int ia, int ib, int color)
{
  int x[2], y[2], n=2, flag=0;
#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      double xd[2], yd[2],zd[2];
      int n=2, flag=0;
      xd[0] = coord[3*ia];
      yd[0] = coord[3*ia+1];
      zd[0] = coord[3*ia+2];
      xd[1] = coord[3*ib];
      yd[1] = coord[3*ib+1];
      zd[1] = coord[3*ib+2];
      drawsegments3D(Xgc, xd, yd ,zd, n, &color, flag);
      return; 
    }
#endif
  x[0] = XScale(Xgc->scales,coord[3*ia]);
  y[0] = YScale(Xgc->scales,coord[3*ia+1]);
  x[1] = XScale(Xgc->scales,coord[3*ib]);
  y[1] = YScale(Xgc->scales,coord[3*ib+1]);
  Xgc->graphic_engine->drawsegments(Xgc, x, y , n, &color, flag);
}

static void draw_segment_bis(BCG *Xgc,double coord[], int ns, int color)
{
  int x[2], y[2], n=2, flag=0;
#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      double x[2], y[2], z[2]; 
      int n=2, flag=0;
      x[0] = coord[6*ns];
      y[0] = coord[6*ns+1];
      z[0] = coord[6*ns+2];
      x[1] = coord[6*ns+3];
      y[1] = coord[6*ns+4];
      z[1] = coord[6*ns+5];
      drawsegments3D(Xgc, x, y ,z, n, &color, flag);
      return;
    }
#endif
  x[0] = XScale(Xgc->scales,coord[6*ns]);
  y[0] = YScale(Xgc->scales,coord[6*ns+1]);
  x[1] = XScale(Xgc->scales,coord[6*ns+3]);
  y[1] = YScale(Xgc->scales,coord[6*ns+4]);
  Xgc->graphic_engine->drawsegments(Xgc, x, y , n, &color, flag);
}

static void draw_justified_string(BCG *Xgc,char *str, double x, double y, int xj, int yj)
{
  int flag=0, rect[4], w, h;
  double angle=0.0; 
  Xgc->graphic_engine->boundingbox(Xgc,str,x,y, rect);
  w = rect[2]; h = rect[3];
  if ( xj == CENTER ) 
    x -= w/2;
  else if ( xj == RIGHT )
    x -= w;
  if ( yj == CENTER )
    y += h/2;
  else if ( yj == DOWN )
    y += h;
  Xgc->graphic_engine->displaystring(Xgc,str,x,y, flag,angle);
}

static void draw_box_face(BCG *Xgc,Plot3dBox *B, int j)
{
  int x[4], y[4], i, numpt, np=1, m=4;
  const int *current_vertex;

#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      double xd[4],yd[4],zd[4];
      current_vertex = &(B->face[4*j]);
      for (i = 0 ; i < 4 ; i++)
	{
	  numpt = current_vertex[i];
	  xd[i] = B->coord[3*numpt];
	  yd[i] = B->coord[3*numpt+1];
	  zd[i] = B->coord[3*numpt+2];
	}
      Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
      fillpolylines3D(Xgc, xd, yd,zd, &B->color, np, m);
      return;
    }
#endif
  current_vertex = &(B->face[4*j]);
  for (i = 0 ; i < 4 ; i++)
    {
      numpt = current_vertex[i];
      x[i] = XScale(Xgc->scales,B->coord[3*numpt]);
      y[i] = YScale(Xgc->scales,B->coord[3*numpt+1]);
    }
  Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
  Xgc->graphic_engine->fillpolylines(Xgc, x, y,&B->color, np, m);
}

static void nsp_obj3d_free_box(Plot3dBox *B)
{
  if ( B->with_ticks )
    {
      free(B->xticks);
      free(B->yticks);
      free(B->zticks);
      free(B->ticks_coord);
      free(B->ticks_pos);
      if ( B->box_style == MATLAB )
	{
	  free(B->others_coord);
	  free(B->others_pos);
	}
    }
  free(B);
}



#define SWAP(i,j)  temp = x[i]; x[i] = x[j]; x[j] = temp; \
                   itemp = p[i]; p[i] = p[j]; p[j] = itemp

#define SWITCH_VALUE 20

#define POP_segment(ia,ib) la--; if ( la >= 0 ) { ia = ileft[la]; ib = iright[la]; }
#define PUSH_segment(ia,ib) ileft[la] = (ia); iright[la] = (ib); la++


static void nsp_obj3d_dsortc(double x[], int *n, int p[])
{
  /*
   *     PURPOSE
   *        sort the double precision array x(1..n) in decreasing order
   *        and computes (if perm == 1) the permutation p of the sort :
   *
   *               x_sorted(i) = x(p(i))  1<=i<=n
   *
   *     AUTHOR
   *        B. Pincon (trying to accelerate the initial scilab dsort.f)
   *
   *     NOTES
   *        (i) n must be less than 2**(25) ! due to lengh of work space (ileft, iright)
   *        (ii) quicksort is used with Sedgewick tricks
   */

  int ileft[25], iright[25]; /* to store parts (segments) of the array which stay to sort */
  int i, ia, ib, im, la, j, itemp;
  double temp, pivot;

  for ( i = 0; i < *n ; i++) p[i] = i;

  if ( *n == 1 ) return;

  ia = 0; ib = *n-1;  /* ia..ib is the current part (segment) of the array to sort */
  la = 0;

  while (la >= 0)   /* la >= 0  <=> stay one or some segments to sort */
    {
      if ( ib-ia < SWITCH_VALUE ) /* segment is short enough => insertion sort */
	{
	  for ( i = ia+1 ; i <= ib ; i++ )
	    {
	      j = i;
	      while ( j > ia  &&  x[j] > x[j-1] )
		{
		  SWAP(j,j-1);
		  j--;
		}
	    };
	  POP_segment(ia,ib);  /* get the next segment to sort if any */
	}
      else    /* quicksort */
	{
	  im = (ia+ib)/2;
	  SWAP(ia, im);
	  i = ia+1; j = ib;
	  if (x[i] < x[j])  { SWAP(i, j); }
	  if (x[ia] < x[j]) { SWAP(ia, j); }
	  else if (x[i] < x[ia]) { SWAP(ia, i); }
	  pivot = x[ia];
          /* at this point we have  x[i=ia+1] >= pivot (=x[ia]) >= x[j=ib]  */
	  while (1)
	    {
	      do i++;  while ( x[i] > pivot );
	      do j--;  while ( x[j] < pivot );
	      if (i >= j) break;
	      SWAP(i, j);
	    }
	  SWAP(ia, j);

	  /*  store the longer subdivision in workspace and    */
          /*  update the current segment to be sorted [ia..ib] */
	  if ( j-ia > ib-j )
	    { PUSH_segment(ia,j-1); ia = j+1; }
	  else
	    { PUSH_segment(j+1,ib); ib = j-1; }
	  if ( ib-ia <= 0)
	    { POP_segment(ia,ib); }
	}
    }
}



static void nsp_plot3d_update_bounds(BCG *Xgc,char *name, double *x, double *y, double *z, int *p, int *q, 
			      double *teta, double *alpha,const char *legend, int *flag, double *bbox,double *zmin,
			      double *zmax,nsp_plot3d_type type3d)
{
  int redraw = FALSE;
  int i;
  if (*flag!=0 && *flag!=1 && *flag!=3 && *flag!=5 && *flag != 7 )
    {
      switch (type3d) 
	{
	case plot3d_t :
	  bbox[0]=x[0];bbox[1]=x[*p-1];
	  bbox[2]=y[0];bbox[3]=y[*q-1];
	  *zmin=bbox[4]=(double) Mini(z,*p*(*q)); 
	  *zmax=bbox[5]=(double) Maxi(z,*p*(*q));
	  break;
	case facettes_t: 
	  bbox[0]=(double) Mini(x,*p*(*q));
	  bbox[1]=(double) Maxi(x,*p*(*q));
	  bbox[2]=(double) Mini(y,*p*(*q)); 
	  bbox[3]=(double) Maxi(y,*p*(*q));
	  *zmin=bbox[4]=(double) Mini(z,*p*(*q)); 
	  *zmax=bbox[5]=(double) Maxi(z,*p*(*q));
	  break;
	case param3d_t: 
	  bbox[0]=(double) Mini(x,*p);bbox[1]=(double) Maxi(x,*p);
	  bbox[2]=(double) Mini(y,*p);bbox[3]=(double) Maxi(y,*p);
	  bbox[4]=(double) Mini(z,*p);bbox[5]=(double) Maxi(z,*p);
	  break;
	}
    }
  if ( *flag==1 || *flag==3 || *flag==5 || *flag == 7 ) 
    {
      *zmin=bbox[4];
      *zmax=bbox[5];
    }

  if ( Xgc->scales->scale_flag3d != 0 ) 
    {
      if (*flag == 7 || *flag == 8 )
	{
	  for ( i= 0 ; i < 6 ; i +=2 ) bbox[i]=Min(Xgc->scales->bbox1[i],bbox[i]);
	  for ( i= 1 ; i < 6 ; i +=2 ) bbox[i]=Max(Xgc->scales->bbox1[i],bbox[i]);
	  *zmin=bbox[4];
	  *zmax=bbox[5];

	  if ( bbox[0] < Xgc->scales->bbox1[0] 
	       || bbox[1] > Xgc->scales->bbox1[1] 
	       || bbox[2] < Xgc->scales->bbox1[2] 
	       || bbox[3] > Xgc->scales->bbox1[3] 
	       || bbox[4] < Xgc->scales->bbox1[4] 
	       || bbox[5] > Xgc->scales->bbox1[5] )
	    redraw = TRUE;
	  /* changing flag to the mode used by other recorded 3d plots */
	  *flag=2*Xgc->scales->metric3d;
	  if ( Xgc->scales->theta != *teta ||  Xgc->scales->alpha != *alpha ) 
	    redraw = TRUE;
	}

    }
  else 
    {
      if (*flag == 7 || *flag == 8 )
	{
	  /* we have used a superpose mode and there's no previous 
	   * 3d graphics, we switch to default 
	   */
	  *flag= 1;
	}
    }
  /* switch to mode with ebox to accelerate replot */
  if ( *flag==2 || *flag==4 || *flag==6 || *flag == 8 ) 
    (*flag)--;

  /* Redraw other graphics */
  if ( redraw == TRUE )
    {
      /* just change bbox not flag */
      static int iflag[]={0,0,0,1};
      if ( Xgc->graphic_engine->xget_recording(Xgc) == FALSE ) 
	{
	  Xgc->graphic_engine->xinfo(Xgc,"Auto rescale only works when recording is on " );
	  return;
	}
      Xgc->graphic_engine->clearwindow(Xgc);    
      /* redraw 3d with new bbox */
      tape_replay_new_angles(Xgc,Xgc->CurWindow,iflag,NULL,teta,alpha,bbox);
    }
}



/*-------------------------------------------------------------------
 * functions for 3D scales 
 * flag = 0: use current scale 
 * flag = 1: standard scale (the graphic is expanded).
 * flag = 2: iso mode but scale is such that it can contain the 
 *           sphere circumscribing the box
 * flag = 3: iso mode (the graphic is expanded).
 *-------------------------------------------------------------------*/

static void SetEch3d1(BCG *Xgc, nsp_box_3d *box,const double *bbox, double Teta, double Alpha, int flag)
{
  double xmmin=0.0,ymmax=1.0,xmmax=1.0,ymmin=0.0,FRect[4],WRect[4],ARect[4];
  double R,xo,yo,zo,dx,dy,dz,hx,hy,hx1,hy1;
  double cost,sint,cosa,sina;
  int ib, i, aaint[]={2,10,2,10},wdim[2], wmax=0,hmax=0;
  char logf[2];

  Xgc->scales->scale_flag3d = 1;
  Xgc->scales->alpha = Alpha;
  Xgc->scales->theta = Teta;
  cost=cos((Teta)*M_PI/180.0);
  sint=sin((Teta)*M_PI/180.0);
  cosa=cos((Alpha)*M_PI/180.0);
  sina=sin((Alpha)*M_PI/180.0);
  Xgc->scales->m[0][0]= -sint    ;    Xgc->scales->m[0][1]= cost      ;    Xgc->scales->m[0][2]= 0;
  Xgc->scales->m[1][0]= -cost*cosa;   Xgc->scales->m[1][1]= -sint*cosa;    Xgc->scales->m[1][2]= sina;
  Xgc->scales->m[2][0]=  cost*sina;   Xgc->scales->m[2][1]= sint*sina;     Xgc->scales->m[2][2]= cosa;

  /* in (xbox[8],ybox[8],zbox[8]) are stored the coordinates of the bounding box 
   * which contains the surface stored clockwise (one level then the other)
   *       Z
   *       |
   *      4 ----- 5        
   *       /    /|         
   *     7----6  |__________ Y         
   *      | 0 | / 1        
   *     3----- 2          
   *    /
   *    X
   */
  for (ib=0;ib<6 ;ib++) 
    { 
      if (flag==0) 
	box->bbox[ib]=Xgc->scales->bbox1[ib];
      else 
	box->bbox[ib]=Xgc->scales->bbox1[ib]=bbox[ib];
    }

  Xgc->scales->c[0]=( box->bbox[0]+box->bbox[1])/2.0;
  Xgc->scales->c[1]=( box->bbox[2]+box->bbox[3])/2.0; 
  Xgc->scales->c[2]=( box->bbox[4]+box->bbox[5])/2.0;
  
  box->x_r[0]=box->bbox[0]; box->y_r[0]=box->bbox[2];  box->z_r[0]=box->bbox[4];
  box->x_r[1]=box->bbox[0]; box->y_r[1]=box->bbox[3];  box->z_r[1]=box->bbox[4];
  box->x_r[2]=box->bbox[1]; box->y_r[2]=box->bbox[3];  box->z_r[2]=box->bbox[4];
  box->x_r[3]=box->bbox[1]; box->y_r[3]=box->bbox[2];  box->z_r[3]=box->bbox[4];
  box->x_r[4]=box->bbox[0]; box->y_r[4]=box->bbox[2];  box->z_r[4]=box->bbox[5];
  box->x_r[5]=box->bbox[0]; box->y_r[5]=box->bbox[3];  box->z_r[5]=box->bbox[5];
  box->x_r[6]=box->bbox[1]; box->y_r[6]=box->bbox[3];  box->z_r[6]=box->bbox[5];
  box->x_r[7]=box->bbox[1]; box->y_r[7]=box->bbox[2];  box->z_r[7]=box->bbox[5];

  for ( i = 0 ; i < 8 ; i++) 
    {
      box->x[i]=TRX(Xgc->scales,box->x_r[i],box->y_r[i],box->z_r[i]);
      box->y[i]=TRY(Xgc->scales,box->x_r[i],box->y_r[i],box->z_r[i]);
      box->z[i]=TRZ(Xgc->scales,box->x_r[i],box->y_r[i],box->z_r[i]);
    }

  /* Calcul des echelles en fonction de la taille du dessin **/
  if ( flag == 1 || flag == 3 )
    {
      xmmin=  (double) Mini(box->x,8L);xmmax= (double) Maxi(box->x,8L);
      ymmax=  (double) - Mini(box->y,8L);
      ymmin=  (double) - Maxi(box->y,8L);
    }
  
  /* code added by es: isoview scaling */
  if ( flag == 2 || flag == 3 )
    {
      /* get current window size */
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      /* FIXME: getscale2d is useless here just use 
       * WRect[i] <-> Xgc->scales->subwin_rect[i];
       */
      getscale2d(Xgc,WRect,FRect,logf,ARect);
      wmax=linint((double)wdim[0] * WRect[2]);
      hmax=linint((double)wdim[1] * WRect[3]); 
    }
  if ( flag == 2 )
    {
      /* radius and center of the sphere circumscribing the box */
      dx=box->bbox[1]-box->bbox[0]; dy=box->bbox[3]-box->bbox[2]; dz=box->bbox[5]-box->bbox[4];
      R= (double) sqrt(dx*dx + dy*dy + dz*dz)/2;
      xo= (double) (box->x[0]+box->x[6])/2 ;
      yo= (double) (box->y[0]+box->y[6])/2 ;
      zo= (double) (box->z[0]+box->z[6])/2 ;
      xmmin=  (double) xo - R ;
      xmmax=  (double) xo + R ;
      ymmax=  (double) -yo + R ;
      ymmin=  (double) -yo - R ;
    }
  if (flag==2 || flag==3)
    {
      /* adjust limits (code adapted from Plo2d.c & Stephane's patch) */
      hx=xmmax-xmmin;
      hy=ymmax-ymmin;
      if ( hx/(double)wmax  < hy/(double)hmax ) 
        {
          hx1=wmax*hy/hmax;
          xmmin=xmmin-(hx1-hx)/2.0;
          xmmax=xmmax+(hx1-hx)/2.0;
        }
      else 
        {
          hy1=hmax*hx/wmax;
          ymmin=ymmin-(hy1-hy)/2.0;
          ymmax=ymmax+(hy1-hy)/2.0;
        }
    }
  if (flag !=0 && Xgc->scales->scale_3drot_flag  == 0 )
    {
      FRect[0]=xmmin;FRect[1]= -ymmax;FRect[2]=xmmax;FRect[3]= -ymmin;
      set_scale(Xgc,"tftttf",NULL,FRect,aaint,"nn",NULL);
      Xgc->scales->metric3d=flag; /* the metric mode is stored into the list of Scales */
      /* this is used by opengl for zmin zmax and depth */
      Xgc->scales->zfrect[0]= (double) Mini(box->z,8L);
      Xgc->scales->zfrect[1]= (double) Maxi(box->z,8L);
    }
  
  /* end of code added by es */
#ifdef WITH_GTKGLEXT 
  /* transmit info to opengl */
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_3dview(Xgc);
    }
#endif
}

int nsp_geom3d_new(BCG *Xgc,double *x, double *y, double *z, int *n)
{
  int j;
  for ( j =0 ; j < (*n) ; j++)	 
    {
      double x1,y1;
      x1=TRX(Xgc->scales,x[j],y[j],z[j]);
      y1=TRY(Xgc->scales,x[j],y[j],z[j]);
      z[j]=TRZ(Xgc->scales,x[j],y[j],z[j]);
      x[j]=x1;
      y[j]=y1;
    }
  return(0);
}


/*
 *
 * Interactive change of view angle 
 * with full redraw when the mouse moves 
 * The process is initiated by a click and 
 * stopped when the mouse is released 
 */

/* #define ROTATION_NORESCALE */

void nsp_3d_rotation(BCG *Xgc)
{
  int box_only = FALSE;
  double theta,alpha, theta_dir;
  int flag[3],pixmode,pt[2];
  int iflag[]={0,0,0,0};
  int xc,yc;
  double theta0,alpha0;
  int ibutton,imask,iwait=FALSE,istr=0;
  double x0,y0,x,y,bbox[4];
  /* FIXME */
  if ( tape_check_recorded_3D(Xgc,Xgc->CurWindow) == FAIL) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"No 3d recorded plots in your graphic window");
      /* XXX continue for new graphics  */
    }
  if ( Xgc->graphic_engine->xget_recording(Xgc) == FALSE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"3d rotation is not possible when recording is not on" );
      return;
    }
  Xgc->graphic_engine->xset_win_protect(Xgc,TRUE); /* protect against window kill */
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  nsp_set_cursor(Xgc,GDK_EXCHANGE);
  Xgc->graphic_engine->xclick(Xgc,"one",&ibutton,&imask,&xc,&yc,iwait,FALSE,FALSE,FALSE,istr);
  theta0 = theta = Xgc->scales->theta ;
  alpha0 = alpha = Xgc->scales->alpha ;
  pt[0]=x0 = x = xc;
  pt[1]=y0 = y = yc;
  ibutton=-1;
  while ( ibutton == -1 ) 
    {
      /* */
      theta_dir= ( sin(M_PI*alpha0/180.0) >= 0.0 ) ? 1.0 : -1.0;
      alpha= alpha0 - (y-y0)/2.0;
      theta= theta0 - theta_dir*(x-x0)/2.0;
      x0=x;y0=y;alpha0=alpha;theta0=theta;
#ifdef ROTATION_NORESCALE
      Xgc->scales->scale_3drot_flag = 1;
#endif 
      Xgc->graphic_engine->xinfo(Xgc,"alpha=%.2f,theta=%.2f",alpha,theta); 
      if ( box_only == TRUE) 
	{
	  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
	  Xgc->graphic_engine->clearwindow(Xgc);    
	  /* dbox(Xgc,theta,alpha); */
	  if ( pixmode == 1) Xgc->graphic_engine->xset_show(Xgc);
	  Xgc->graphic_engine->xset_recording(Xgc,TRUE);
	}
      else 
	{
	  /* just changes the angles in recorded plots 
	   * and invalidate the objs3d 
	   */
	  tape_new_angles_plots(Xgc,Xgc->CurWindow,&theta,&alpha,iflag,flag,bbox,pt);
	}
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&xc, &yc,FALSE,TRUE,TRUE,FALSE);
      x=xc;
      y=yc;
    }
  nsp_set_cursor(Xgc,-1);
  Xgc->scales->scale_3drot_flag = 0;
  Xgc->graphic_engine->xset_win_protect(Xgc,FALSE); /* protect against window kill */
  tape_new_angles_plots(Xgc,Xgc->CurWindow,&theta,&alpha,iflag,flag,bbox,pt);
}




/* invalidate the drawing region associated to an axes object. 
 */

void nsp_objs3d_invalidate(NspGraphic *G)
{
  NspObjs3d *P = (NspObjs3d *) G;
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

#line 2752 "objs3d.c"
