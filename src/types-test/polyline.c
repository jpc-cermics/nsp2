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





#line 19 "codegen/polyline.override"
#include <gdk/gdk.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 

#line 33 "polyline.c"

/* ----------- NspPolyline ----------- */


#define  NspPolyline_Private 
#include <nsp/object.h>
#include <nsp/polyline.h>
#include <nsp/interf.h>

/* 
 * NspPolyline inherits from Graphic 
 */

int nsp_type_polyline_id=0;
NspTypePolyline *nsp_type_polyline=NULL;

/*
 * Type object for NspPolyline 
 * all the instance of NspTypePolyline share the same id. 
 * nsp_type_polyline: is an instance of NspTypePolyline 
 *    used for objects of NspPolyline type (i.e built with new_polyline) 
 * other instances are used for derived classes 
 */
NspTypePolyline *new_type_polyline(type_mode mode)
{
  NspTypePolyline *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_polyline != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_polyline;
    }
  if (( type =  malloc(sizeof(NspTypePolyline))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = polyline_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = polyline_get_methods;
  type->new = (new_func *) new_polyline;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for polyline */ 

  top->pr = (print_func *) nsp_polyline_print;
  top->dealloc = (dealloc_func *) nsp_polyline_destroy;
  top->copy  =  (copy_func *) nsp_polyline_copy;
  top->size  = (size_func *) nsp_polyline_size;
  top->s_type =  (s_type_func *) nsp_polyline_type_as_string;
  top->sh_type = (sh_type_func *) nsp_polyline_type_short_string;
  top->info = (info_func *) nsp_polyline_info;
  /* top->is_true = (is_true_func  *) nsp_polyline_is_true; */
  /* top->loop =(loop_func *) nsp_polyline_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_polyline_object;
  top->eq  = (eq_func *) nsp_polyline_eq;
  top->neq  = (eq_func *) nsp_polyline_neq;
  top->save  = (save_func *) nsp_polyline_xdr_save;
  top->load  = (load_func *) nsp_polyline_xdr_load;
  top->create = (create_func*) int_polyline_create;
  top->latex = (print_func *) nsp_polyline_latex;
  top->full_copy = (copy_func *) nsp_polyline_full_copy;

  /* specific methods for polyline */

  type->init = (init_func *) init_polyline;

#line 30 "codegen/polyline.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_polyline;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_polyline ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_polyline  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_polyline  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_polyline  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Polyline */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 118 "polyline.c"
  /* 
   * NspPolyline interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_polyline_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePolyline called nsp_type_polyline
       */
      type->id =  nsp_type_polyline_id = nsp_new_type_id();
      nsp_type_polyline = type;
      if ( nsp_register_type(nsp_type_polyline) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_polyline(mode);
    }
  else 
    {
      type->id = nsp_type_polyline_id;
      return type;
    }
}

/*
 * initialize NspPolyline instances 
 * locally and by calling initializer on parent class 
 */

static int init_polyline(NspPolyline *Obj,NspTypePolyline *type)
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
 * new instance of NspPolyline 
 */

NspPolyline *new_polyline() 
{
  NspPolyline *loc;
  /* type must exists */
  nsp_type_polyline = new_type_polyline(T_BASE);
  if ( (loc = malloc(sizeof(NspPolyline)))== NULLPOLYLINE) return loc;
  /* initialize object */
  if ( init_polyline(loc,nsp_type_polyline) == FAIL) return NULLPOLYLINE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspPolyline 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_polyline_size(NspPolyline *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char polyline_type_name[]="Polyline";
static char polyline_short_type_name[]="polyline";

static char *nsp_polyline_type_as_string(void)
{
  return(polyline_type_name);
}

static char *nsp_polyline_type_short_string(NspObject *v)
{
  return(polyline_short_type_name);
}

/*
 * A == B 
 */

static int nsp_polyline_eq(NspPolyline *A, NspObject *B)
{
  NspPolyline *loc = (NspPolyline *) B;
  if ( check_cast(B,nsp_type_polyline_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( A->obj->close != loc->obj->close) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->mark != loc->obj->mark) return FALSE;
  if ( A->obj->mark_size != loc->obj->mark_size) return FALSE;
  if ( A->obj->fill_color != loc->obj->fill_color) return FALSE;
  if ( A->obj->thickness != loc->obj->thickness) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_polyline_neq(NspPolyline *A, NspObject *B)
{
  return ( nsp_polyline_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_polyline_xdr_save(XDR *xdrs, NspPolyline *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_polyline)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->close) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark_size) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->fill_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->thickness) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspPolyline  *nsp_polyline_xdr_load_partial(XDR *xdrs, NspPolyline *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->close) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark_size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->fill_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->thickness) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspPolyline  *nsp_polyline_xdr_load(XDR *xdrs)
{
  NspPolyline *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPOLYLINE;
  if ((H  = nsp_polyline_create_void(name,(NspTypeBase *) nsp_type_polyline))== NULLPOLYLINE) return H;
  if ( nsp_polyline_create_partial(H) == FAIL) return NULLPOLYLINE;
  if ((H  = nsp_polyline_xdr_load_partial(xdrs,H))== NULLPOLYLINE) return H;
  if ( nsp_polyline_check_values(H) == FAIL) return NULLPOLYLINE;
  return H;
}

/*
 * delete 
 */

void nsp_polyline_destroy_partial(NspPolyline *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->x != NULL ) 
      nsp_matrix_destroy(H->obj->x);
    if ( H->obj->y != NULL ) 
      nsp_matrix_destroy(H->obj->y);
    FREE(H->obj);
   }
}

void nsp_polyline_destroy(NspPolyline *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_polyline_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_polyline_info(NspPolyline *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLPOLYLINE) 
    {
      Sciprintf("Null Pointer NspPolyline \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_polyline_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_polyline_print(NspPolyline *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLPOLYLINE) 
    {
      Sciprintf("Null Pointer NspPolyline \n");
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
          nsp_polyline_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_polyline_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"close	= %s\n", ( M->obj->close == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"mark=%d\n",M->obj->mark);
  Sciprintf1(indent+2,"mark_size=%d\n",M->obj->mark_size);
  Sciprintf1(indent+2,"fill_color=%d\n",M->obj->fill_color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_polyline_latex(NspPolyline *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_polyline_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"close	= %s\n", ( M->obj->close == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"mark=%d\n",M->obj->mark);
  Sciprintf1(indent+2,"mark_size=%d\n",M->obj->mark_size);
  Sciprintf1(indent+2,"fill_color=%d\n",M->obj->fill_color);
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPolyline objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspPolyline   *nsp_polyline_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_polyline_id) == TRUE ) return ((NspPolyline *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_polyline));
  return NULL;
}

int IsPolylineObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_polyline_id);
}

int IsPolyline(NspObject *O)
{
  return nsp_object_type(O,nsp_type_polyline_id);
}

NspPolyline  *GetPolylineCopy(Stack stack, int i)
{
  if (  GetPolyline(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPolyline  *GetPolyline(Stack stack, int i)
{
  NspPolyline *M;
  if (( M = nsp_polyline_object(NthObj(i))) == NULLPOLYLINE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspPolyline instance 
 *-----------------------------------------------------*/

static NspPolyline *nsp_polyline_create_void(char *name,NspTypeBase *type)
{
 NspPolyline *H  = (type == NULL) ? new_polyline() : type->new();
 if ( H ==  NULLPOLYLINE)
  {
   Sciprintf("No more memory\n");
   return NULLPOLYLINE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLPOLYLINE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_polyline_create_partial(NspPolyline *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_polyline)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->close = FALSE;
  H->obj->color = -1;
  H->obj->mark = -1;
  H->obj->mark_size = -1;
  H->obj->fill_color = -1;
  H->obj->thickness = 0;
  return OK;
}

int nsp_polyline_check_values(NspPolyline *H)
{
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

NspPolyline *nsp_polyline_create(char *name,NspMatrix* x,NspMatrix* y,gboolean close,int color,int mark,int mark_size,int fill_color,int thickness,NspTypeBase *type)
{
  NspPolyline *H  = nsp_polyline_create_void(name,type);
  if ( H ==  NULLPOLYLINE) return NULLPOLYLINE;
  if ( nsp_polyline_create_partial(H) == FAIL) return NULLPOLYLINE;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->close=close;
  H->obj->color=color;
  H->obj->mark=mark;
  H->obj->mark_size=mark_size;
  H->obj->fill_color=fill_color;
  H->obj->thickness=thickness;
  if ( nsp_polyline_check_values(H) == FAIL) return NULLPOLYLINE;
  return H;
}


NspPolyline *nsp_polyline_create_default(char *name)
{
 NspPolyline *H  = nsp_polyline_create_void(name,NULL);
 if ( H ==  NULLPOLYLINE) return NULLPOLYLINE;
  if ( nsp_polyline_create_partial(H) == FAIL) return NULLPOLYLINE;
 if ( nsp_polyline_check_values(H) == FAIL) return NULLPOLYLINE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspPolyline *nsp_polyline_copy_partial(NspPolyline *H,NspPolyline *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspPolyline *nsp_polyline_copy(NspPolyline *self)
{
  NspPolyline *H  =nsp_polyline_create_void(NVOID,(NspTypeBase *) nsp_type_polyline);
  if ( H ==  NULLPOLYLINE) return NULLPOLYLINE;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOLYLINE;
  if ( nsp_polyline_copy_partial(H,self)== NULL) return NULLPOLYLINE;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspPolyline *nsp_polyline_full_copy_partial(NspPolyline *H,NspPolyline *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_polyline))) == NULL) return NULLPOLYLINE;
  H->obj->ref_count=1;
  if ( self->obj->x == NULL )
    { H->obj->x = NULL;}
  else
    {
      if ((H->obj->x = (NspMatrix *) nsp_object_full_copy_and_name("x",NSP_OBJECT(self->obj->x))) == NULLMAT) return NULL;
    }
  if ( self->obj->y == NULL )
    { H->obj->y = NULL;}
  else
    {
      if ((H->obj->y = (NspMatrix *) nsp_object_full_copy_and_name("y",NSP_OBJECT(self->obj->y))) == NULLMAT) return NULL;
    }
  H->obj->close=self->obj->close;
  H->obj->color=self->obj->color;
  H->obj->mark=self->obj->mark;
  H->obj->mark_size=self->obj->mark_size;
  H->obj->fill_color=self->obj->fill_color;
  H->obj->thickness=self->obj->thickness;
  return H;
}

NspPolyline *nsp_polyline_full_copy(NspPolyline *self)
{
  NspPolyline *H  =nsp_polyline_create_void(NVOID,(NspTypeBase *) nsp_type_polyline);
  if ( H ==  NULLPOLYLINE) return NULLPOLYLINE;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOLYLINE;
  if ( nsp_polyline_full_copy_partial(H,self)== NULL) return NULLPOLYLINE;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspPolyline
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_polyline_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPolyline *H;
  CheckStdRhs(0,0);
  /* want to be sure that type polyline is initialized */
  nsp_type_polyline = new_type_polyline(T_BASE);
  if(( H = nsp_polyline_create_void(NVOID,(NspTypeBase *) nsp_type_polyline)) == NULLPOLYLINE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_polyline_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_polyline_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *polyline_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_polyline_get_x(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspPolyline *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_polyline_get_obj_x(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPolyline *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_polyline_set_x(void *self,const char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyline *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspPolyline *) self)->obj->x);
  ((NspPolyline *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_polyline_get_y(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspPolyline *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_polyline_get_obj_y(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPolyline *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_polyline_set_y(void *self,const char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyline *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspPolyline *) self)->obj->y);
  ((NspPolyline *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_polyline_get_close(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspPolyline *) self)->obj->close;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_polyline_set_close(void *self,const char *attr, NspObject *O)
{
  int close;

  if ( BoolScalar(O,&close) == FAIL) return FAIL;
  ((NspPolyline *) self)->obj->close= close;
  return OK;
}

static NspObject *_wrap_polyline_get_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspPolyline *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_polyline_set_color(void *self,const char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspPolyline *) self)->obj->color= color;
  return OK;
}

static NspObject *_wrap_polyline_get_mark(void *self,const char *attr)
{
  int ret;

  ret = ((NspPolyline *) self)->obj->mark;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_polyline_set_mark(void *self,const char *attr, NspObject *O)
{
  int mark;

  if ( IntScalar(O,&mark) == FAIL) return FAIL;
  ((NspPolyline *) self)->obj->mark= mark;
  return OK;
}

static NspObject *_wrap_polyline_get_mark_size(void *self,const char *attr)
{
  int ret;

  ret = ((NspPolyline *) self)->obj->mark_size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_polyline_set_mark_size(void *self,const char *attr, NspObject *O)
{
  int mark_size;

  if ( IntScalar(O,&mark_size) == FAIL) return FAIL;
  ((NspPolyline *) self)->obj->mark_size= mark_size;
  return OK;
}

static NspObject *_wrap_polyline_get_fill_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspPolyline *) self)->obj->fill_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_polyline_set_fill_color(void *self,const char *attr, NspObject *O)
{
  int fill_color;

  if ( IntScalar(O,&fill_color) == FAIL) return FAIL;
  ((NspPolyline *) self)->obj->fill_color= fill_color;
  return OK;
}

static NspObject *_wrap_polyline_get_thickness(void *self,const char *attr)
{
  int ret;

  ret = ((NspPolyline *) self)->obj->thickness;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_polyline_set_thickness(void *self,const char *attr, NspObject *O)
{
  int thickness;

  if ( IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspPolyline *) self)->obj->thickness= thickness;
  return OK;
}

static AttrTab polyline_attrs[] = {
  { "x", (attr_get_function *)_wrap_polyline_get_x, (attr_set_function *)_wrap_polyline_set_x,(attr_get_object_function *)_wrap_polyline_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_polyline_get_y, (attr_set_function *)_wrap_polyline_set_y,(attr_get_object_function *)_wrap_polyline_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "close", (attr_get_function *)_wrap_polyline_get_close, (attr_set_function *)_wrap_polyline_set_close,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "color", (attr_get_function *)_wrap_polyline_get_color, (attr_set_function *)_wrap_polyline_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mark", (attr_get_function *)_wrap_polyline_get_mark, (attr_set_function *)_wrap_polyline_set_mark,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mark_size", (attr_get_function *)_wrap_polyline_get_mark_size, (attr_set_function *)_wrap_polyline_set_mark_size,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "fill_color", (attr_get_function *)_wrap_polyline_get_fill_color, (attr_set_function *)_wrap_polyline_set_fill_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "thickness", (attr_get_function *)_wrap_polyline_get_thickness, (attr_set_function *)_wrap_polyline_set_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 52 "codegen/polyline.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_polyline(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 800 "polyline.c"


#line 62 "codegen/polyline.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_polyline(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 813 "polyline.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Polyline_func[]={
  {"extractelts_polyline", _wrap_nsp_extractelts_polyline},
  {"setrowscols_polyline", _wrap_nsp_setrowscols_polyline},
  { "polyline_create", int_polyline_create},
  { NULL, NULL}
};

/* call ith function in the Polyline interface */

int Polyline_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Polyline_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Polyline_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Polyline_func[i].name;
  *f = Polyline_func[i].fonc;
}

#line 73 "codegen/polyline.override"

/* inserted verbatim at the end */


static void nsp_draw_polyline(BCG *Xgc,NspGraphic *Obj, GdkRectangle *rect,void *data)
{
  int xmark[2];
  int ccolor=-1,cmark=-1,cthick=-1;
  NspPolyline *P = (NspPolyline *) Obj;
  int *xm=NULL,*ym=NULL;

  if (((NspGraphic *) P)->obj->show == FALSE ) return;
  if ( P->obj->x->mn == 0) return ;

  if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
    {
      return ;
    }

  
  xm= graphic_alloc(6,P->obj->x->mn,sizeof(int));
  ym= graphic_alloc(7,P->obj->x->mn,sizeof(int));
  if ( xm  ==  0 || ym  ==  0 ) return;
  scale_f2i(Xgc->scales,P->obj->x->R,P->obj->y->R,xm,ym,P->obj->x->mn);
  /* fill polyline */
  if ( P->obj->fill_color != -2 )
    {
      /* set the fill color */
      if ( P->obj->fill_color != -1 ) 
	{
	  ccolor = Xgc->graphic_engine->xget_pattern(Xgc); 
	  Xgc->graphic_engine->xset_pattern(Xgc,P->obj->fill_color);
	}
      /* fill */
      Xgc->graphic_engine->fillpolyline(Xgc,xm,ym,P->obj->x->mn,P->obj->close);
      /* reset color */
      if ( P->obj->fill_color != -1 ) 
	Xgc->graphic_engine->xset_pattern(Xgc,ccolor);
    }
  /* draw polyline */
  if ( P->obj->color != -2 ) 
    {
      /* we will draw polyline */
      if ( P->obj->thickness != -1 ) 	
	{
	  cthick = Xgc->graphic_engine->xget_thickness(Xgc); 
	  Xgc->graphic_engine->xset_thickness(Xgc,P->obj->thickness);
	}
      if ( P->obj->color != -1 ) 
	{
	  ccolor = Xgc->graphic_engine->xget_pattern(Xgc); 
	  Xgc->graphic_engine->xset_pattern(Xgc,P->obj->color);
	}
      Xgc->graphic_engine->drawpolyline(Xgc,xm,ym,P->obj->x->mn,P->obj->close);
      if ( P->obj->thickness != -1 ) 
	Xgc->graphic_engine->xset_thickness(Xgc,cthick);
      if ( P->obj->color != -1 )
	Xgc->graphic_engine->xset_pattern(Xgc,ccolor);
    }
  /* draw polymark */
  if ( P->obj->mark != -2 ) 
    {
      /* we will draw marks */
      if ( P->obj->mark != -1 ) 
	{
	  Xgc->graphic_engine->xget_mark(Xgc,xmark); 
	  cmark=xmark[0];
	  Xgc->graphic_engine->xset_mark(Xgc, P->obj->mark,xmark[1]);
	}
      Xgc->graphic_engine->drawpolymark(Xgc,xm,ym,P->obj->x->mn);
      if ( P->obj->mark != -1 ) 
	Xgc->graphic_engine->xset_mark(Xgc,cmark,xmark[1]);
    }
}

static void nsp_translate_polyline(NspGraphic *Obj,const double *tr)
{
  int i; 
  NspPolyline *P = (NspPolyline *) Obj;
  double *x=P->obj->x->R,*y= P->obj->y->R;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) += tr[0];
      *(y++) += tr[1];
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_polyline(NspGraphic *Obj,double *R)
{
  int i;
  NspPolyline *P = (NspPolyline *) Obj;
  double *x= P->obj->x->R,*y= P->obj->y->R,x1,y1;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      x1 = R[0]*(*x) -R[1]*(*y);
      y1 = R[1]*(*x) +R[0]*(*y);
      *(x++) =x1;
      *(y++) =y1;
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_polyline(NspGraphic *Obj,double *alpha)
{
  int i;
  NspPolyline *P = (NspPolyline *) Obj;
  double *x= P->obj->x->R,*y= P->obj->y->R;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) *= alpha[0];
      *(y++) *= alpha[1];
    }
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of polyline 
 *
 */

static int nsp_getbounds_polyline(NspGraphic *Obj,double *bounds)
{
  int i;
  NspPolyline *P = (NspPolyline *) Obj;
  double *x= P->obj->x->R,*y= P->obj->y->R,dval;
  if ( P->obj->x->mn == 0) return FALSE;
  bounds[0]=*x;/* xmin */
  bounds[1]=*y;/* ymin */
  bounds[2]=*x;/* xmax */
  bounds[3]=*y;/* ymax */
  for (i = 1; i < P->obj->x->mn; i++)
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
  return TRUE;
}


#line 995 "polyline.c"
