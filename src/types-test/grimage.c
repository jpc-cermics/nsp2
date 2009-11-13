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





#line 19 "codegen/grimage.override"
#include <gdk/gdk.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 

#line 33 "grimage.c"

/* ----------- NspGrImage ----------- */


#define  NspGrImage_Private 
#include <nsp/object.h>
#include <nsp/grimage.h>
#include <nsp/interf.h>

/* 
 * NspGrImage inherits from Graphic 
 */

int nsp_type_grimage_id=0;
NspTypeGrImage *nsp_type_grimage=NULL;

/*
 * Type object for NspGrImage 
 * all the instance of NspTypeGrImage share the same id. 
 * nsp_type_grimage: is an instance of NspTypeGrImage 
 *    used for objects of NspGrImage type (i.e built with new_grimage) 
 * other instances are used for derived classes 
 */
NspTypeGrImage *new_type_grimage(type_mode mode)
{
  NspTypeGrImage *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_grimage != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_grimage;
    }
  if (( type =  malloc(sizeof(NspTypeGrImage))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = grimage_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = grimage_get_methods;
  type->new = (new_func *) new_grimage;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for grimage */ 

  top->pr = (print_func *) nsp_grimage_print;
  top->dealloc = (dealloc_func *) nsp_grimage_destroy;
  top->copy  =  (copy_func *) nsp_grimage_copy;
  top->size  = (size_func *) nsp_grimage_size;
  top->s_type =  (s_type_func *) nsp_grimage_type_as_string;
  top->sh_type = (sh_type_func *) nsp_grimage_type_short_string;
  top->info = (info_func *) nsp_grimage_info;
  /* top->is_true = (is_true_func  *) nsp_grimage_is_true; */
  /* top->loop =(loop_func *) nsp_grimage_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_grimage_object;
  top->eq  = (eq_func *) nsp_grimage_eq;
  top->neq  = (eq_func *) nsp_grimage_neq;
  top->save  = (save_func *) nsp_grimage_xdr_save;
  top->load  = (load_func *) nsp_grimage_xdr_load;
  top->create = (create_func*) int_grimage_create;
  top->latex = (print_func *) nsp_grimage_latex;
  top->full_copy = (copy_func *) nsp_grimage_full_copy;

  /* specific methods for grimage */

  type->init = (init_func *) init_grimage;

#line 30 "codegen/grimage.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_grimage;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_grimage ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_grimage  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_grimage  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_grimage  ;
  /* next method are defined in NspGraphic and need not be changed here for GrImage */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 118 "grimage.c"
  /* 
   * NspGrImage interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_grimage_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGrImage called nsp_type_grimage
       */
      type->id =  nsp_type_grimage_id = nsp_new_type_id();
      nsp_type_grimage = type;
      if ( nsp_register_type(nsp_type_grimage) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_grimage(mode);
    }
  else 
    {
      type->id = nsp_type_grimage_id;
      return type;
    }
}

/*
 * initialize NspGrImage instances 
 * locally and by calling initializer on parent class 
 */

static int init_grimage(NspGrImage *Obj,NspTypeGrImage *type)
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
 * new instance of NspGrImage 
 */

NspGrImage *new_grimage() 
{
  NspGrImage *loc;
  /* type must exists */
  nsp_type_grimage = new_type_grimage(T_BASE);
  if ( (loc = malloc(sizeof(NspGrImage)))== NULLGRIMAGE) return loc;
  /* initialize object */
  if ( init_grimage(loc,nsp_type_grimage) == FAIL) return NULLGRIMAGE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGrImage 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_grimage_size(NspGrImage *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char grimage_type_name[]="GrImage";
static char grimage_short_type_name[]="grimage";

static char *nsp_grimage_type_as_string(void)
{
  return(grimage_type_name);
}

static char *nsp_grimage_type_short_string(NspObject *v)
{
  return(grimage_short_type_name);
}

/*
 * A == B 
 */

static int nsp_grimage_eq(NspGrImage *A, NspObject *B)
{
  NspGrImage *loc = (NspGrImage *) B;
  if ( check_cast(B,nsp_type_grimage_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->x != loc->obj->x) return FALSE;
  if ( A->obj->y != loc->obj->y) return FALSE;
  if ( A->obj->w != loc->obj->w) return FALSE;
  if ( A->obj->h != loc->obj->h) return FALSE;
  if ( A->obj->border != loc->obj->border) return FALSE;
  if ( A->obj->thickness != loc->obj->thickness) return FALSE;
  if ( strcmp(A->obj->fname,loc->obj->fname) != 0) return FALSE;
  if ( A->obj->image != loc->obj->image) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_grimage_neq(NspGrImage *A, NspObject *B)
{
  return ( nsp_grimage_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_grimage_xdr_save(XDR *xdrs, NspGrImage *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_grimage)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->x) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->y) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->w) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->h) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->border) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->thickness) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->obj->fname) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGrImage  *nsp_grimage_xdr_load_partial(XDR *xdrs, NspGrImage *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if (nsp_xdr_load_d(xdrs, &M->obj->x) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->y) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->w) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->h) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->border) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->thickness) == FAIL) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->obj->fname)) == FAIL) return NULL;
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

static NspGrImage  *nsp_grimage_xdr_load(XDR *xdrs)
{
  NspGrImage *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRIMAGE;
  if ((H  = nsp_grimage_create_void(name,(NspTypeBase *) nsp_type_grimage))== NULLGRIMAGE) return H;
  if ( nsp_grimage_create_partial(H) == FAIL) return NULLGRIMAGE;
  if ((H  = nsp_grimage_xdr_load_partial(xdrs,H))== NULLGRIMAGE) return H;
  if ( nsp_grimage_check_values(H) == FAIL) return NULLGRIMAGE;
  return H;
}

/*
 * delete 
 */

void nsp_grimage_destroy_partial(NspGrImage *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_string_destroy(&(H->obj->fname));
    FREE(H->obj);
   }
}

void nsp_grimage_destroy(NspGrImage *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_grimage_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_grimage_info(NspGrImage *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRIMAGE) 
    {
      Sciprintf("Null Pointer NspGrImage \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_grimage_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_grimage_print(NspGrImage *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRIMAGE) 
    {
      Sciprintf("Null Pointer NspGrImage \n");
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
          nsp_grimage_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_grimage_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n",M->obj->x);
  Sciprintf1(indent+2,"y=%f\n",M->obj->y);
  Sciprintf1(indent+2,"w=%f\n",M->obj->w);
  Sciprintf1(indent+2,"h=%f\n",M->obj->h);
  Sciprintf1(indent+2,"border	= %s\n", ( M->obj->border == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"fname=%s\n",M->obj->fname);
  Sciprintf1(indent+2,"image=%xl\n",M->obj->image);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_grimage_latex(NspGrImage *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_grimage_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n",M->obj->x);
  Sciprintf1(indent+2,"y=%f\n",M->obj->y);
  Sciprintf1(indent+2,"w=%f\n",M->obj->w);
  Sciprintf1(indent+2,"h=%f\n",M->obj->h);
  Sciprintf1(indent+2,"border	= %s\n", ( M->obj->border == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"thickness=%d\n",M->obj->thickness);
  Sciprintf1(indent+2,"fname=%s\n",M->obj->fname);
  Sciprintf1(indent+2,"image=%xl\n",M->obj->image);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGrImage objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGrImage   *nsp_grimage_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_grimage_id) == TRUE ) return ((NspGrImage *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_grimage));
  return NULL;
}

int IsGrImageObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_grimage_id);
}

int IsGrImage(NspObject *O)
{
  return nsp_object_type(O,nsp_type_grimage_id);
}

NspGrImage  *GetGrImageCopy(Stack stack, int i)
{
  if (  GetGrImage(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGrImage  *GetGrImage(Stack stack, int i)
{
  NspGrImage *M;
  if (( M = nsp_grimage_object(NthObj(i))) == NULLGRIMAGE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGrImage instance 
 *-----------------------------------------------------*/

static NspGrImage *nsp_grimage_create_void(char *name,NspTypeBase *type)
{
 NspGrImage *H  = (type == NULL) ? new_grimage() : type->new();
 if ( H ==  NULLGRIMAGE)
  {
   Sciprintf("No more memory\n");
   return NULLGRIMAGE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRIMAGE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_grimage_create_partial(NspGrImage *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_grimage)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = 0.0;
  H->obj->y = 0.0;
  H->obj->w = 0.0;
  H->obj->h = 0.0;
  H->obj->border = TRUE;
  H->obj->thickness = 0;
  H->obj->fname = NULL;
  H->obj->image = NULL;
  H->obj->color = -1;
  return OK;
}

int nsp_grimage_check_values(NspGrImage *H)
{
  if ( H->obj->fname == NULL) 
    {
     if (( H->obj->fname = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspGrImage *nsp_grimage_create(char *name,double x,double y,double w,double h,gboolean border,int thickness,char* fname,void* image,int color,NspTypeBase *type)
{
  NspGrImage *H  = nsp_grimage_create_void(name,type);
  if ( H ==  NULLGRIMAGE) return NULLGRIMAGE;
  if ( nsp_grimage_create_partial(H) == FAIL) return NULLGRIMAGE;
  H->obj->x=x;
  H->obj->y=y;
  H->obj->w=w;
  H->obj->h=h;
  H->obj->border=border;
  H->obj->thickness=thickness;
  H->obj->fname = fname;
  H->obj->image = image;
  H->obj->color=color;
  if ( nsp_grimage_check_values(H) == FAIL) return NULLGRIMAGE;
  return H;
}


NspGrImage *nsp_grimage_create_default(char *name)
{
 NspGrImage *H  = nsp_grimage_create_void(name,NULL);
 if ( H ==  NULLGRIMAGE) return NULLGRIMAGE;
  if ( nsp_grimage_create_partial(H) == FAIL) return NULLGRIMAGE;
 if ( nsp_grimage_check_values(H) == FAIL) return NULLGRIMAGE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGrImage *nsp_grimage_copy_partial(NspGrImage *H,NspGrImage *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGrImage *nsp_grimage_copy(NspGrImage *self)
{
  NspGrImage *H  =nsp_grimage_create_void(NVOID,(NspTypeBase *) nsp_type_grimage);
  if ( H ==  NULLGRIMAGE) return NULLGRIMAGE;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRIMAGE;
  if ( nsp_grimage_copy_partial(H,self)== NULL) return NULLGRIMAGE;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGrImage *nsp_grimage_full_copy_partial(NspGrImage *H,NspGrImage *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_grimage))) == NULL) return NULLGRIMAGE;
  H->obj->ref_count=1;
  H->obj->x=self->obj->x;
  H->obj->y=self->obj->y;
  H->obj->w=self->obj->w;
  H->obj->h=self->obj->h;
  H->obj->border=self->obj->border;
  H->obj->thickness=self->obj->thickness;
  if ((H->obj->fname = nsp_string_copy(self->obj->fname)) == NULL) return NULL;
  H->obj->image = self->obj->image;
  H->obj->color=self->obj->color;
  return H;
}

NspGrImage *nsp_grimage_full_copy(NspGrImage *self)
{
  NspGrImage *H  =nsp_grimage_create_void(NVOID,(NspTypeBase *) nsp_type_grimage);
  if ( H ==  NULLGRIMAGE) return NULLGRIMAGE;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRIMAGE;
  if ( nsp_grimage_full_copy_partial(H,self)== NULL) return NULLGRIMAGE;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGrImage
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_grimage_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGrImage *H;
  CheckStdRhs(0,0);
  /* want to be sure that type grimage is initialized */
  nsp_type_grimage = new_type_grimage(T_BASE);
  if(( H = nsp_grimage_create_void(NVOID,(NspTypeBase *) nsp_type_grimage)) == NULLGRIMAGE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_grimage_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_grimage_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *grimage_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_grimage_get_x(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrImage *) self)->obj->x;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grimage_set_x(void *self,const char *attr, NspObject *O)
{
  double x;

  if ( DoubleScalar(O,&x) == FAIL) return FAIL;
  ((NspGrImage *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_grimage_get_y(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrImage *) self)->obj->y;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grimage_set_y(void *self,const char *attr, NspObject *O)
{
  double y;

  if ( DoubleScalar(O,&y) == FAIL) return FAIL;
  ((NspGrImage *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_grimage_get_w(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrImage *) self)->obj->w;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grimage_set_w(void *self,const char *attr, NspObject *O)
{
  double w;

  if ( DoubleScalar(O,&w) == FAIL) return FAIL;
  ((NspGrImage *) self)->obj->w= w;
  return OK;
}

static NspObject *_wrap_grimage_get_h(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrImage *) self)->obj->h;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grimage_set_h(void *self,const char *attr, NspObject *O)
{
  double h;

  if ( DoubleScalar(O,&h) == FAIL) return FAIL;
  ((NspGrImage *) self)->obj->h= h;
  return OK;
}

static NspObject *_wrap_grimage_get_border(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspGrImage *) self)->obj->border;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_grimage_set_border(void *self,const char *attr, NspObject *O)
{
  int border;

  if ( BoolScalar(O,&border) == FAIL) return FAIL;
  ((NspGrImage *) self)->obj->border= border;
  return OK;
}

static NspObject *_wrap_grimage_get_thickness(void *self,const char *attr)
{
  int ret;

  ret = ((NspGrImage *) self)->obj->thickness;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grimage_set_thickness(void *self,const char *attr, NspObject *O)
{
  int thickness;

  if ( IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspGrImage *) self)->obj->thickness= thickness;
  return OK;
}

static NspObject *_wrap_grimage_get_fname(void *self,const char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = ((NspGrImage *) self)->obj->fname;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_grimage_set_fname(void *self,const char *attr, NspObject *O)
{
  char *fname;

  if ((fname = nsp_string_object(O))==NULL) return FAIL;
  if ((fname = nsp_string_copy(fname)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspGrImage *) self)->obj->fname);
  ((NspGrImage *) self)->obj->fname= fname;
  return OK;
}

static NspObject *_wrap_grimage_get_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspGrImage *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grimage_set_color(void *self,const char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspGrImage *) self)->obj->color= color;
  return OK;
}

static AttrTab grimage_attrs[] = {
  { "x", (attr_get_function *)_wrap_grimage_get_x, (attr_set_function *)_wrap_grimage_set_x,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_grimage_get_y, (attr_set_function *)_wrap_grimage_set_y,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "w", (attr_get_function *)_wrap_grimage_get_w, (attr_set_function *)_wrap_grimage_set_w,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "h", (attr_get_function *)_wrap_grimage_get_h, (attr_set_function *)_wrap_grimage_set_h,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "border", (attr_get_function *)_wrap_grimage_get_border, (attr_set_function *)_wrap_grimage_set_border,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "thickness", (attr_get_function *)_wrap_grimage_get_thickness, (attr_set_function *)_wrap_grimage_set_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "fname", (attr_get_function *)_wrap_grimage_get_fname, (attr_set_function *)_wrap_grimage_set_fname,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "color", (attr_get_function *)_wrap_grimage_get_color, (attr_set_function *)_wrap_grimage_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 53 "codegen/grimage.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_grimage(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 766 "grimage.c"


#line 63 "codegen/grimage.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_grimage(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 779 "grimage.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GrImage_func[]={
  {"extractelts_grimage", _wrap_nsp_extractelts_grimage},
  {"setrowscols_grimage", _wrap_nsp_setrowscols_grimage},
  { "grimage_create", int_grimage_create},
  { NULL, NULL}
};

/* call ith function in the GrImage interface */

int GrImage_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GrImage_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GrImage_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GrImage_func[i].name;
  *f = GrImage_func[i].fonc;
}

#line 74 "codegen/grimage.override"

/* inserted verbatim at the end */

static void nsp_draw_grimage(BCG *Xgc,NspGraphic *Obj, GdkRectangle *rect,void *data)
{
  double val[4];
  int ccolor=-1,cthick=-1;
  NspGrImage *P = (NspGrImage *) Obj;
  
  if ( Obj->obj->show == FALSE ) return ;

  /* check if the block is inside drawing rectangle
   */

  if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
    {
      return ;
    }

  ccolor = Xgc->graphic_engine->xget_pattern(Xgc); 
  val[0]= P->obj->x;
  val[1]= P->obj->y;
  val[2]= P->obj->w;
  val[3]= P->obj->h;

  if ( P->obj->border == TRUE &&  P->obj->color != -2 ) 
    {
      /* draw the rectangle */ 
      if ( P->obj->color != -1 ) 
	Xgc->graphic_engine->xset_pattern(Xgc,P->obj->color);
      if ( P->obj->thickness != -1 ) 
	{
	  cthick = Xgc->graphic_engine->xget_thickness(Xgc); 
	  Xgc->graphic_engine->xset_thickness(Xgc,P->obj->thickness);
	}
      Xgc->graphic_engine->scale->drawrectangle(Xgc,val);
      /* reset to default values */
      if ( P->obj->color != -1 ) 
	Xgc->graphic_engine->xset_pattern(Xgc,ccolor);
      if ( P->obj->thickness != -1 ) 
	Xgc->graphic_engine->xset_thickness(Xgc,cthick);
    }

  /* now we draw the image */
  if ( P->obj->image == NULL ) 
    {
      GError *error = NULL;
      GdkPixbuf *ret;
      ret = gdk_pixbuf_new_from_file(P->obj->fname,&error);
      if ( error != NULL ) {
	Sciprintf("gdk_pixbuf_new_from_file: gtk error\n");
	return;
      }
      P->obj->image = ret;
    }
  if ( P->obj->image != NULL )
    {
      int idest_x,idest_y,iw,ih;
      length_scale_f2i(Xgc->scales,&P->obj->w,&P->obj->h,&iw,&ih,1);
      scale_f2i(Xgc->scales,&P->obj->x,&P->obj->y,&idest_x,&idest_y,1);
      length_scale_f2i(Xgc->scales,&P->obj->w,&P->obj->h,&iw,&ih,1);
      Xgc->graphic_engine->draw_pixbuf(Xgc,P->obj->image,0,0,idest_x,idest_y, iw, ih);
    }
}


static void nsp_translate_grimage(NspGraphic *Obj,const double *tr)
{
  NspGrImage *P = (NspGrImage *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->x += tr[0];
  P->obj->y += tr[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_grimage(NspGraphic *Obj,double *R)
{
  NspGrImage *P = (NspGrImage *) Obj;
  double x1;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  x1 = R[0]*(P->obj->x) -R[1]*(P->obj->y);
  P->obj->y = R[1]*(P->obj->x) +R[0]*(P->obj->y);
  P->obj->x = x1;
  /* Il faut aussi changer l'angle */
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_grimage(NspGraphic *Obj,double *alpha)
{
  NspGrImage *P = (NspGrImage *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->x *= alpha[0];
  P->obj->y *= alpha[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of grimage 
 *
 */

static int nsp_getbounds_grimage(NspGraphic *Obj,double *bounds)
{
  NspGrImage *P = (NspGrImage *) Obj;
  bounds[0]=P->obj->x;/* xmin */
  bounds[1]=P->obj->y-P->obj->w;/* ymin */
  bounds[2]=P->obj->x+P->obj->w;/* xmax */
  bounds[3]=P->obj->y;/* ymax */
  return TRUE;
}


#line 922 "grimage.c"
