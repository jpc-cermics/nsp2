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





#line 23 "codegen/gpixbuf.override"
#include <gdk/gdk.h>
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/figuredata.h>
#include <nsp/figure.h>
#include <nsp/axes.h>

#ifdef  WITH_OPENGL
extern Gengine GL_gengine;
#endif

#line 40 "gpixbuf.c"

/* -----------NspGPixbuf ----------- */


#define  NspGPixbuf_Private 
#include <nsp/objects.h>
#include <nsp/gpixbuf.h>
#include <nsp/interf.h>

/* 
 * NspGPixbuf inherits from Graphic 
 */

int nsp_type_gpixbuf_id=0;
NspTypeGPixbuf *nsp_type_gpixbuf=NULL;

/*
 * Type object for NspGPixbuf 
 * all the instance of NspTypeGPixbuf share the same id. 
 * nsp_type_gpixbuf: is an instance of NspTypeGPixbuf 
 *    used for objects of NspGPixbuf type (i.e built with new_gpixbuf) 
 * other instances are used for derived classes 
 */
NspTypeGPixbuf *new_type_gpixbuf(type_mode mode)
{
  NspTypeGPixbuf *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gpixbuf != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gpixbuf;
    }
  if (( type =  malloc(sizeof(NspTypeGPixbuf))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gpixbuf_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gpixbuf_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_gpixbuf;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gpixbuf */ 

  top->pr = (print_func *) nsp_gpixbuf_print;
  top->dealloc = (dealloc_func *) nsp_gpixbuf_destroy;
  top->copy  =  (copy_func *) nsp_gpixbuf_copy;
  top->size  = (size_func *) nsp_gpixbuf_size;
  top->s_type =  (s_type_func *) nsp_gpixbuf_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gpixbuf_type_short_string;
  top->info = (info_func *) nsp_gpixbuf_info;
  /* top->is_true = (is_true_func  *) nsp_gpixbuf_is_true; */
  /* top->loop =(loop_func *) nsp_gpixbuf_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gpixbuf_object;
  top->eq  = (eq_func *) nsp_gpixbuf_eq;
  top->neq  = (eq_func *) nsp_gpixbuf_neq;
  top->save  = (save_func *) nsp_gpixbuf_xdr_save;
  top->load  = (load_func *) nsp_gpixbuf_xdr_load;
  top->create = (create_func*) int_gpixbuf_create;
  top->latex = (print_func *) nsp_gpixbuf_latex;
  top->full_copy = (copy_func *) nsp_gpixbuf_full_copy;

  /* specific methods for gpixbuf */

  type->init = (init_func *) init_gpixbuf;

#line 41 "codegen/gpixbuf.override"
  /* inserted verbatim in the type definition */
  type->gtk_methods = TRUE;
  /* here we override the method or its father class i.e Graphic */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_gpixbuf;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_gpixbuf ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_gpixbuf  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_gpixbuf  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_gpixbuf  ;
  /* next method are defined in NspGraphic and need not be chnaged here for GPixbuf */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */

#line 126 "gpixbuf.c"
  /* 
   * NspGPixbuf interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gpixbuf_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGPixbuf called nsp_type_gpixbuf
       */
      type->id =  nsp_type_gpixbuf_id = nsp_new_type_id();
      nsp_type_gpixbuf = type;
      if ( nsp_register_type(nsp_type_gpixbuf) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gpixbuf(mode);
    }
  else 
    {
      type->id = nsp_type_gpixbuf_id;
      return type;
    }
}

/*
 * initialize NspGPixbuf instances 
 * locally and by calling initializer on parent class 
 */

static int init_gpixbuf(NspGPixbuf *Obj,NspTypeGPixbuf *type)
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
 * new instance of NspGPixbuf 
 */

NspGPixbuf *new_gpixbuf() 
{
  NspGPixbuf *loc;
  /* type must exists */
  nsp_type_gpixbuf = new_type_gpixbuf(T_BASE);
  if ( (loc = malloc(sizeof(NspGPixbuf)))== NULLGPIXBUF) return loc;
  /* initialize object */
  if ( init_gpixbuf(loc,nsp_type_gpixbuf) == FAIL) return NULLGPIXBUF;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGPixbuf 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gpixbuf_size(NspGPixbuf *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char gpixbuf_type_name[]="GPixbuf";
static char gpixbuf_short_type_name[]="gpixbuf";

static char *nsp_gpixbuf_type_as_string(void)
{
  return(gpixbuf_type_name);
}

static char *nsp_gpixbuf_type_short_string(NspObject *v)
{
  return(gpixbuf_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gpixbuf_eq(NspGPixbuf *A, NspObject *B)
{
  NspGPixbuf *loc = (NspGPixbuf *) B;
  if ( check_cast(B,nsp_type_gpixbuf_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->src_x != loc->obj->src_x) return FALSE;
  if ( A->obj->src_y != loc->obj->src_y) return FALSE;
  if ( A->obj->dest_x != loc->obj->dest_x) return FALSE;
  if ( A->obj->dest_y != loc->obj->dest_y) return FALSE;
  if ( A->obj->width != loc->obj->width) return FALSE;
  if ( A->obj->height != loc->obj->height) return FALSE;
  if ( A->obj->pixbuf != loc->obj->pixbuf) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_gpixbuf_neq(NspGPixbuf *A, NspObject *B)
{
  return ( nsp_gpixbuf_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gpixbuf_xdr_save(XDR *xdrs, NspGPixbuf *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gpixbuf)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->src_x) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->src_y) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->dest_x) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->dest_y) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->width) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->height) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGPixbuf  *nsp_gpixbuf_xdr_load_partial(XDR *xdrs, NspGPixbuf *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if (nsp_xdr_load_i(xdrs, &M->obj->src_x) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->src_y) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->dest_x) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->dest_y) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->width) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->height) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic * )M) == NULL) return NULL;
 return M;
}

static NspGPixbuf  *nsp_gpixbuf_xdr_load(XDR *xdrs)
{
  NspGPixbuf *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGPIXBUF;
  if ((H  = nsp_gpixbuf_create_void(name,(NspTypeBase *) nsp_type_gpixbuf))== NULLGPIXBUF) return H;
  if ( nsp_gpixbuf_create_partial(H) == FAIL) return NULLGPIXBUF;
  if ((H  = nsp_gpixbuf_xdr_load_partial(xdrs,H))== NULLGPIXBUF) return H;
  if ( nsp_gpixbuf_check_values(H) == FAIL) return NULLGPIXBUF;
  return H;
}

/*
 * delete 
 */

void nsp_gpixbuf_destroy_partial(NspGPixbuf *H)
{
  nsp_graphic_destroy_partial((NspGraphic * ) H);
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 55 "codegen/gpixbuf.override"
  /* verbatim in destroy */
  gobject_destroy(H->obj->pixbuf);

#line 311 "gpixbuf.c"
    FREE(H->obj);
   }
}

void nsp_gpixbuf_destroy(NspGPixbuf *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gpixbuf_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gpixbuf_info(NspGPixbuf *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGPIXBUF) 
    {
      Sciprintf("Null Pointer NspGPixbuf \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gpixbuf_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gpixbuf_print(NspGPixbuf *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGPIXBUF) 
    {
      Sciprintf("Null Pointer NspGPixbuf \n");
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
          nsp_gpixbuf_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_gpixbuf_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"src_x=%d\n", M->obj->src_x);
  Sciprintf1(indent+2,"src_y=%d\n", M->obj->src_y);
  Sciprintf1(indent+2,"dest_x=%f\n", M->obj->dest_x);
  Sciprintf1(indent+2,"dest_y=%f\n", M->obj->dest_y);
  Sciprintf1(indent+2,"width=%f\n", M->obj->width);
  Sciprintf1(indent+2,"height=%f\n", M->obj->height);
  Sciprintf1(indent+2,"pixbuf=0x%x\n", M->obj->pixbuf);
  nsp_graphic_print((NspGraphic * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gpixbuf_latex(NspGPixbuf *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gpixbuf_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|src_x|= \\numprint{%d}\n",M->obj->src_x);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|src_y|= \\numprint{%d}\n",M->obj->src_y);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|dest_x| = \\numprint{%f}\n", M->obj->dest_x);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|dest_y| = \\numprint{%f}\n", M->obj->dest_y);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|width| = \\numprint{%f}\n", M->obj->width);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|height| = \\numprint{%f}\n", M->obj->height);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|pixbuf|= \\verb@0x%x@\n",M->obj->pixbuf);
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
 * for NspGPixbuf objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGPixbuf   *nsp_gpixbuf_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gpixbuf_id)  == TRUE  ) return ((NspGPixbuf *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gpixbuf));
  return NULL;
}

int IsGPixbufObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gpixbuf_id);
}

int IsGPixbuf(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gpixbuf_id);
}

NspGPixbuf  *GetGPixbufCopy(Stack stack, int i)
{
  if (  GetGPixbuf(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGPixbuf  *GetGPixbuf(Stack stack, int i)
{
  NspGPixbuf *M;
  if (( M = nsp_gpixbuf_object(NthObj(i))) == NULLGPIXBUF)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGPixbuf instance 
 *-----------------------------------------------------*/

static NspGPixbuf *nsp_gpixbuf_create_void(const char *name,NspTypeBase *type)
{
 NspGPixbuf *H  = (type == NULL) ? new_gpixbuf() : type->new();
 if ( H ==  NULLGPIXBUF)
  {
   Sciprintf("No more memory\n");
   return NULLGPIXBUF;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGPIXBUF;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gpixbuf_create_partial(NspGPixbuf *H)
{
  if ( nsp_graphic_create_partial((NspGraphic * ) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_gpixbuf)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->src_x = 0;
  H->obj->src_y = 0;
  H->obj->dest_x = 0;
  H->obj->dest_y = 0;
  H->obj->width = 0;
  H->obj->height = 0;
  H->obj->pixbuf = NULL;
  return OK;
}

int nsp_gpixbuf_check_values(NspGPixbuf *H)
{
  nsp_graphic_check_values((NspGraphic * ) H);
  return OK;
}

NspGPixbuf *nsp_gpixbuf_create(const char *name,gint src_x,gint src_y,gdouble dest_x,gdouble dest_y,gdouble width,gdouble height,void* pixbuf,NspTypeBase *type)
{
  NspGPixbuf *H  = nsp_gpixbuf_create_void(name,type);
  if ( H ==  NULLGPIXBUF) return NULLGPIXBUF;
  if ( nsp_gpixbuf_create_partial(H) == FAIL) return NULLGPIXBUF;
  H->obj->src_x=src_x;
  H->obj->src_y=src_y;
  H->obj->dest_x=dest_x;
  H->obj->dest_y=dest_y;
  H->obj->width=width;
  H->obj->height=height;
  H->obj->pixbuf = pixbuf;
  if ( nsp_gpixbuf_check_values(H) == FAIL) return NULLGPIXBUF;
  return H;
}


NspGPixbuf *nsp_gpixbuf_create_default(const char *name)
{
 NspGPixbuf *H  = nsp_gpixbuf_create_void(name,NULL);
 if ( H ==  NULLGPIXBUF) return NULLGPIXBUF;
  if ( nsp_gpixbuf_create_partial(H) == FAIL) return NULLGPIXBUF;
  if ( nsp_gpixbuf_check_values(H) == FAIL) return NULLGPIXBUF;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGPixbuf *nsp_gpixbuf_copy_partial(NspGPixbuf *H,NspGPixbuf *self)
{
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLGPIXBUF;
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGPixbuf *nsp_gpixbuf_copy(NspGPixbuf *self)
{
  NspGPixbuf *H  =nsp_gpixbuf_create_void(NVOID,(NspTypeBase *) nsp_type_gpixbuf);
  if ( H ==  NULLGPIXBUF) return NULLGPIXBUF;
  if ( nsp_gpixbuf_copy_partial(H,self)== NULL) return NULLGPIXBUF;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGPixbuf *nsp_gpixbuf_full_copy_partial(NspGPixbuf *H,NspGPixbuf *self)
{
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLGPIXBUF;
  if ((H->obj = calloc(1,sizeof(nsp_gpixbuf))) == NULL) return NULLGPIXBUF;
  H->obj->ref_count=1;
  H->obj->src_x=self->obj->src_x;
  H->obj->src_y=self->obj->src_y;
  H->obj->dest_x=self->obj->dest_x;
  H->obj->dest_y=self->obj->dest_y;
  H->obj->width=self->obj->width;
  H->obj->height=self->obj->height;
  H->obj->pixbuf = self->obj->pixbuf;
  return H;
}

NspGPixbuf *nsp_gpixbuf_full_copy(NspGPixbuf *self)
{
  NspGPixbuf *H  =nsp_gpixbuf_create_void(NVOID,(NspTypeBase *) nsp_type_gpixbuf);
  if ( H ==  NULLGPIXBUF) return NULLGPIXBUF;
  if ( nsp_gpixbuf_full_copy_partial(H,self)== NULL) return NULLGPIXBUF;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGPixbuf
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gpixbuf_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGPixbuf *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gpixbuf is initialized */
  nsp_type_gpixbuf = new_type_gpixbuf(T_BASE);
  if(( H = nsp_gpixbuf_create_void(NVOID,(NspTypeBase *) nsp_type_gpixbuf)) == NULLGPIXBUF) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_gpixbuf_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gpixbuf_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gpixbuf_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gpixbuf_get_src_x(void *self,const char *attr)
{
  int ret;
  ret = ((NspGPixbuf *) self)->obj->src_x;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_gpixbuf_set_src_x(void *self,const char *attr, NspObject *O)
{
  int src_x;
  if ( IntScalar(O,&src_x) == FAIL) return FAIL;
  ((NspGPixbuf *) self)->obj->src_x= src_x;
  return OK;
}

static NspObject *_wrap_gpixbuf_get_src_y(void *self,const char *attr)
{
  int ret;
  ret = ((NspGPixbuf *) self)->obj->src_y;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_gpixbuf_set_src_y(void *self,const char *attr, NspObject *O)
{
  int src_y;
  if ( IntScalar(O,&src_y) == FAIL) return FAIL;
  ((NspGPixbuf *) self)->obj->src_y= src_y;
  return OK;
}

static NspObject *_wrap_gpixbuf_get_dest_x(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGPixbuf *) self)->obj->dest_x;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_gpixbuf_set_dest_x(void *self,const char *attr, NspObject *O)
{
  double dest_x;
  if ( DoubleScalar(O,&dest_x) == FAIL) return FAIL;
  ((NspGPixbuf *) self)->obj->dest_x= dest_x;
  return OK;
}

static NspObject *_wrap_gpixbuf_get_dest_y(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGPixbuf *) self)->obj->dest_y;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_gpixbuf_set_dest_y(void *self,const char *attr, NspObject *O)
{
  double dest_y;
  if ( DoubleScalar(O,&dest_y) == FAIL) return FAIL;
  ((NspGPixbuf *) self)->obj->dest_y= dest_y;
  return OK;
}

static NspObject *_wrap_gpixbuf_get_width(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGPixbuf *) self)->obj->width;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_gpixbuf_set_width(void *self,const char *attr, NspObject *O)
{
  double width;
  if ( DoubleScalar(O,&width) == FAIL) return FAIL;
  ((NspGPixbuf *) self)->obj->width= width;
  return OK;
}

static NspObject *_wrap_gpixbuf_get_height(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGPixbuf *) self)->obj->height;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_gpixbuf_set_height(void *self,const char *attr, NspObject *O)
{
  double height;
  if ( DoubleScalar(O,&height) == FAIL) return FAIL;
  ((NspGPixbuf *) self)->obj->height= height;
  return OK;
}

static AttrTab gpixbuf_attrs[] = {
  { "src_x", (attr_get_function * )_wrap_gpixbuf_get_src_x, (attr_set_function * )_wrap_gpixbuf_set_src_x, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "src_y", (attr_get_function * )_wrap_gpixbuf_get_src_y, (attr_set_function * )_wrap_gpixbuf_set_src_y, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "dest_x", (attr_get_function * )_wrap_gpixbuf_get_dest_x, (attr_set_function * )_wrap_gpixbuf_set_dest_x, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "dest_y", (attr_get_function * )_wrap_gpixbuf_get_dest_y, (attr_set_function * )_wrap_gpixbuf_set_dest_y, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "width", (attr_get_function * )_wrap_gpixbuf_get_width, (attr_set_function * )_wrap_gpixbuf_set_width, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "height", (attr_get_function * )_wrap_gpixbuf_get_height, (attr_set_function * )_wrap_gpixbuf_set_height, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GPixbuf_func[]={
  { "gpixbuf_create", int_gpixbuf_create},
  { NULL, NULL}
};

/* call ith function in the GPixbuf interface */

int GPixbuf_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(GPixbuf_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GPixbuf_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = GPixbuf_func[i].name;
  *f = GPixbuf_func[i].fonc;
}
void nsp_initialize_GPixbuf_types(void)
{
  new_type_gpixbuf(T_BASE);
}

#line 68 "codegen/gpixbuf.override"

/* inserted verbatim at the end */

static void nsp_draw_gpixbuf(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  NspGPixbuf *P = (NspGPixbuf *) Obj;
  if ( ((NspGraphic *) P)->obj->show == FALSE ) return;
  /* check if the block is inside drawing rectangle */
  if ( ! nsp_graphic_intersect_rectangle(Obj, rect)) return ;

  Xgc->graphic_engine->scale->draw_pixbuf(Xgc,P->obj->pixbuf,
					  P->obj->src_x, P->obj->src_y, P->obj->dest_x, P->obj->dest_y,
					  P->obj->width, P->obj->height);


}

static void nsp_translate_gpixbuf(NspGraphic *Obj,const double *tr)
{
  NspGPixbuf *P = (NspGPixbuf *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->dest_x  += tr[0];
  P->obj->dest_y  += tr[0];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_gpixbuf(NspGraphic *Obj,double *R)
{
  /* NspGPixbuf *P = (NspGPixbuf *) Obj; */
  Sciprintf("rotate pixbuf not implemented yet\n");
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_gpixbuf(NspGraphic *Obj,double *alpha)
{
  NspGPixbuf *P = (NspGPixbuf *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->width *= alpha[0];
  P->obj->height *= alpha[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of gpixbuf
 *
 */

static int nsp_getbounds_gpixbuf (NspGraphic *Obj,double *bounds)
{
  NspGPixbuf *P = (NspGPixbuf *) Obj;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  bounds[0]=P->obj->dest_x; /* xmin */
  bounds[1]=P->obj->dest_y - P->obj->height; /* ymin */
  bounds[2]=bounds[0] + P->obj->width; /* xmax */
  bounds[3]=P->obj->dest_y; /* ymax */
  return TRUE;
}

#line 805 "gpixbuf.c"
