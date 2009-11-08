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





#line 19 "codegen/grrect.override"
#include <gdk/gdk.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 

#line 33 "grrect.c"

/* ----------- NspGrRect ----------- */


#define  NspGrRect_Private 
#include <nsp/object.h>
#include <nsp/grrect.h>
#include <nsp/interf.h>

/* 
 * NspGrRect inherits from Graphic 
 */

int nsp_type_grrect_id=0;
NspTypeGrRect *nsp_type_grrect=NULL;

/*
 * Type object for NspGrRect 
 * all the instance of NspTypeGrRect share the same id. 
 * nsp_type_grrect: is an instance of NspTypeGrRect 
 *    used for objects of NspGrRect type (i.e built with new_grrect) 
 * other instances are used for derived classes 
 */
NspTypeGrRect *new_type_grrect(type_mode mode)
{
  NspTypeGrRect *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_grrect != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_grrect;
    }
  if (( type =  malloc(sizeof(NspTypeGrRect))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = grrect_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = grrect_get_methods;
  type->new = (new_func *) new_grrect;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for grrect */ 

  top->pr = (print_func *) nsp_grrect_print;
  top->dealloc = (dealloc_func *) nsp_grrect_destroy;
  top->copy  =  (copy_func *) nsp_grrect_copy;
  top->size  = (size_func *) nsp_grrect_size;
  top->s_type =  (s_type_func *) nsp_grrect_type_as_string;
  top->sh_type = (sh_type_func *) nsp_grrect_type_short_string;
  top->info = (info_func *) nsp_grrect_info;
  /* top->is_true = (is_true_func  *) nsp_grrect_is_true; */
  /* top->loop =(loop_func *) nsp_grrect_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_grrect_object;
  top->eq  = (eq_func *) nsp_grrect_eq;
  top->neq  = (eq_func *) nsp_grrect_neq;
  top->save  = (save_func *) nsp_grrect_xdr_save;
  top->load  = (load_func *) nsp_grrect_xdr_load;
  top->create = (create_func*) int_grrect_create;
  top->latex = (print_func *) nsp_grrect_latex;
  top->full_copy = (copy_func *) nsp_grrect_full_copy;

  /* specific methods for grrect */

  type->init = (init_func *) init_grrect;

#line 30 "codegen/grrect.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_grrect;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_grrect ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_grrect  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_grrect  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_grrect  ;
  /* next method are defined in NspGraphic and need not be changed here for GrRect */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 118 "grrect.c"
  /* 
   * NspGrRect interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_grrect_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGrRect called nsp_type_grrect
       */
      type->id =  nsp_type_grrect_id = nsp_new_type_id();
      nsp_type_grrect = type;
      if ( nsp_register_type(nsp_type_grrect) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_grrect(mode);
    }
  else 
    {
      type->id = nsp_type_grrect_id;
      return type;
    }
}

/*
 * initialize NspGrRect instances 
 * locally and by calling initializer on parent class 
 */

static int init_grrect(NspGrRect *Obj,NspTypeGrRect *type)
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
 * new instance of NspGrRect 
 */

NspGrRect *new_grrect() 
{
  NspGrRect *loc;
  /* type must exists */
  nsp_type_grrect = new_type_grrect(T_BASE);
  if ( (loc = malloc(sizeof(NspGrRect)))== NULLGRRECT) return loc;
  /* initialize object */
  if ( init_grrect(loc,nsp_type_grrect) == FAIL) return NULLGRRECT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGrRect 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_grrect_size(NspGrRect *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char grrect_type_name[]="GrRect";
static char grrect_short_type_name[]="grrect";

static char *nsp_grrect_type_as_string(void)
{
  return(grrect_type_name);
}

static char *nsp_grrect_type_short_string(NspObject *v)
{
  return(grrect_short_type_name);
}

/*
 * A == B 
 */

static int nsp_grrect_eq(NspGrRect *A, NspObject *B)
{
  NspGrRect *loc = (NspGrRect *) B;
  if ( check_cast(B,nsp_type_grrect_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->x != loc->obj->x) return FALSE;
  if ( A->obj->y != loc->obj->y) return FALSE;
  if ( A->obj->w != loc->obj->w) return FALSE;
  if ( A->obj->h != loc->obj->h) return FALSE;
  if ( A->obj->fill_color != loc->obj->fill_color) return FALSE;
  if ( A->obj->thickness != loc->obj->thickness) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_grrect_neq(NspGrRect *A, NspObject *B)
{
  return ( nsp_grrect_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_grrect_xdr_save(XDR *xdrs, NspGrRect *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_grrect)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->x) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->y) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->w) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->h) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->fill_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->thickness) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGrRect  *nsp_grrect_xdr_load_partial(XDR *xdrs, NspGrRect *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if (nsp_xdr_load_d(xdrs, &M->obj->x) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->y) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->w) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->h) == FAIL) return NULL;
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

static NspGrRect  *nsp_grrect_xdr_load(XDR *xdrs)
{
  NspGrRect *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRRECT;
  if ((H  = nsp_grrect_create_void(name,(NspTypeBase *) nsp_type_grrect))== NULLGRRECT) return H;
  if ( nsp_grrect_create_partial(H) == FAIL) return NULLGRRECT;
  if ((H  = nsp_grrect_xdr_load_partial(xdrs,H))== NULLGRRECT) return H;
  if ( nsp_grrect_check_values(H) == FAIL) return NULLGRRECT;
  return H;
}

/*
 * delete 
 */

void nsp_grrect_destroy_partial(NspGrRect *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
}

void nsp_grrect_destroy(NspGrRect *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_grrect_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_grrect_info(NspGrRect *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRRECT) 
    {
      Sciprintf("Null Pointer NspGrRect \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_grrect_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_grrect_print(NspGrRect *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRRECT) 
    {
      Sciprintf("Null Pointer NspGrRect \n");
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
          nsp_grrect_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_grrect_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n",M->obj->x);
  Sciprintf1(indent+2,"y=%f\n",M->obj->y);
  Sciprintf1(indent+2,"w=%f\n",M->obj->w);
  Sciprintf1(indent+2,"h=%f\n",M->obj->h);
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

int nsp_grrect_latex(NspGrRect *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_grrect_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n",M->obj->x);
  Sciprintf1(indent+2,"y=%f\n",M->obj->y);
  Sciprintf1(indent+2,"w=%f\n",M->obj->w);
  Sciprintf1(indent+2,"h=%f\n",M->obj->h);
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
 * for NspGrRect objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGrRect   *nsp_grrect_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_grrect_id) == TRUE ) return ((NspGrRect *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_grrect));
  return NULL;
}

int IsGrRectObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_grrect_id);
}

int IsGrRect(NspObject *O)
{
  return nsp_object_type(O,nsp_type_grrect_id);
}

NspGrRect  *GetGrRectCopy(Stack stack, int i)
{
  if (  GetGrRect(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGrRect  *GetGrRect(Stack stack, int i)
{
  NspGrRect *M;
  if (( M = nsp_grrect_object(NthObj(i))) == NULLGRRECT)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGrRect instance 
 *-----------------------------------------------------*/

static NspGrRect *nsp_grrect_create_void(char *name,NspTypeBase *type)
{
 NspGrRect *H  = (type == NULL) ? new_grrect() : type->new();
 if ( H ==  NULLGRRECT)
  {
   Sciprintf("No more memory\n");
   return NULLGRRECT;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRRECT;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_grrect_create_partial(NspGrRect *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_grrect)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = 0.0;
  H->obj->y = 0.0;
  H->obj->w = 0.0;
  H->obj->h = 0.0;
  H->obj->fill_color = -1;
  H->obj->thickness = 0;
  H->obj->color = -1;
  return OK;
}

int nsp_grrect_check_values(NspGrRect *H)
{
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspGrRect *nsp_grrect_create(char *name,double x,double y,double w,double h,int fill_color,int thickness,int color,NspTypeBase *type)
{
  NspGrRect *H  = nsp_grrect_create_void(name,type);
  if ( H ==  NULLGRRECT) return NULLGRRECT;
  if ( nsp_grrect_create_partial(H) == FAIL) return NULLGRRECT;
  H->obj->x=x;
  H->obj->y=y;
  H->obj->w=w;
  H->obj->h=h;
  H->obj->fill_color=fill_color;
  H->obj->thickness=thickness;
  H->obj->color=color;
  if ( nsp_grrect_check_values(H) == FAIL) return NULLGRRECT;
  return H;
}


NspGrRect *nsp_grrect_create_default(char *name)
{
 NspGrRect *H  = nsp_grrect_create_void(name,NULL);
 if ( H ==  NULLGRRECT) return NULLGRRECT;
  if ( nsp_grrect_create_partial(H) == FAIL) return NULLGRRECT;
 if ( nsp_grrect_check_values(H) == FAIL) return NULLGRRECT;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGrRect *nsp_grrect_copy_partial(NspGrRect *H,NspGrRect *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGrRect *nsp_grrect_copy(NspGrRect *self)
{
  NspGrRect *H  =nsp_grrect_create_void(NVOID,(NspTypeBase *) nsp_type_grrect);
  if ( H ==  NULLGRRECT) return NULLGRRECT;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRRECT;
  if ( nsp_grrect_copy_partial(H,self)== NULL) return NULLGRRECT;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGrRect *nsp_grrect_full_copy_partial(NspGrRect *H,NspGrRect *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_grrect))) == NULL) return NULLGRRECT;
  H->obj->ref_count=1;
  H->obj->x=self->obj->x;
  H->obj->y=self->obj->y;
  H->obj->w=self->obj->w;
  H->obj->h=self->obj->h;
  H->obj->fill_color=self->obj->fill_color;
  H->obj->thickness=self->obj->thickness;
  H->obj->color=self->obj->color;
  return H;
}

NspGrRect *nsp_grrect_full_copy(NspGrRect *self)
{
  NspGrRect *H  =nsp_grrect_create_void(NVOID,(NspTypeBase *) nsp_type_grrect);
  if ( H ==  NULLGRRECT) return NULLGRRECT;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLGRRECT;
  if ( nsp_grrect_full_copy_partial(H,self)== NULL) return NULLGRRECT;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGrRect
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_grrect_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGrRect *H;
  CheckStdRhs(0,0);
  /* want to be sure that type grrect is initialized */
  nsp_type_grrect = new_type_grrect(T_BASE);
  if(( H = nsp_grrect_create_void(NVOID,(NspTypeBase *) nsp_type_grrect)) == NULLGRRECT) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_grrect_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_grrect_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *grrect_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_grrect_get_x(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrRect *) self)->obj->x;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grrect_set_x(void *self,const char *attr, NspObject *O)
{
  double x;

  if ( DoubleScalar(O,&x) == FAIL) return FAIL;
  ((NspGrRect *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_grrect_get_y(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrRect *) self)->obj->y;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grrect_set_y(void *self,const char *attr, NspObject *O)
{
  double y;

  if ( DoubleScalar(O,&y) == FAIL) return FAIL;
  ((NspGrRect *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_grrect_get_w(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrRect *) self)->obj->w;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grrect_set_w(void *self,const char *attr, NspObject *O)
{
  double w;

  if ( DoubleScalar(O,&w) == FAIL) return FAIL;
  ((NspGrRect *) self)->obj->w= w;
  return OK;
}

static NspObject *_wrap_grrect_get_h(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspGrRect *) self)->obj->h;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grrect_set_h(void *self,const char *attr, NspObject *O)
{
  double h;

  if ( DoubleScalar(O,&h) == FAIL) return FAIL;
  ((NspGrRect *) self)->obj->h= h;
  return OK;
}

static NspObject *_wrap_grrect_get_fill_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspGrRect *) self)->obj->fill_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grrect_set_fill_color(void *self,const char *attr, NspObject *O)
{
  int fill_color;

  if ( IntScalar(O,&fill_color) == FAIL) return FAIL;
  ((NspGrRect *) self)->obj->fill_color= fill_color;
  return OK;
}

static NspObject *_wrap_grrect_get_thickness(void *self,const char *attr)
{
  int ret;

  ret = ((NspGrRect *) self)->obj->thickness;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grrect_set_thickness(void *self,const char *attr, NspObject *O)
{
  int thickness;

  if ( IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspGrRect *) self)->obj->thickness= thickness;
  return OK;
}

static NspObject *_wrap_grrect_get_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspGrRect *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grrect_set_color(void *self,const char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspGrRect *) self)->obj->color= color;
  return OK;
}

static AttrTab grrect_attrs[] = {
  { "x", (attr_get_function *)_wrap_grrect_get_x, (attr_set_function *)_wrap_grrect_set_x,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_grrect_get_y, (attr_set_function *)_wrap_grrect_set_y,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "w", (attr_get_function *)_wrap_grrect_get_w, (attr_set_function *)_wrap_grrect_set_w,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "h", (attr_get_function *)_wrap_grrect_get_h, (attr_set_function *)_wrap_grrect_set_h,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "fill_color", (attr_get_function *)_wrap_grrect_get_fill_color, (attr_set_function *)_wrap_grrect_set_fill_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "thickness", (attr_get_function *)_wrap_grrect_get_thickness, (attr_set_function *)_wrap_grrect_set_thickness,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "color", (attr_get_function *)_wrap_grrect_get_color, (attr_set_function *)_wrap_grrect_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 53 "codegen/grrect.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_grrect(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 722 "grrect.c"


#line 63 "codegen/grrect.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_grrect(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 735 "grrect.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GrRect_func[]={
  {"extractelts_grrect", _wrap_nsp_extractelts_grrect},
  {"setrowscols_grrect", _wrap_nsp_setrowscols_grrect},
  { "grrect_create", int_grrect_create},
  { NULL, NULL}
};

/* call ith function in the GrRect interface */

int GrRect_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GrRect_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GrRect_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GrRect_func[i].name;
  *f = GrRect_func[i].fonc;
}

#line 74 "codegen/grrect.override"

/* inserted verbatim at the end */

static void nsp_draw_grrect(BCG *Xgc,NspGraphic *Obj, GdkRectangle *rect,void *data)
{
  double val[4];
  int ccolor=-1,cthick=-1;
  NspGrRect *P = (NspGrRect *) Obj;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  ccolor = Xgc->graphic_engine->xget_pattern(Xgc); 

  if ( ! nsp_graphic_intersect_rectangle(Obj,rect))
    {
      /* Sciprintf("No need to draw one rectangle\n"); */
      return;
    }
  
  val[0]= P->obj->x;
  val[1]= P->obj->y;
  val[2]= P->obj->w;
  val[3]= P->obj->h;

  if ( P->obj->fill_color != -2 ) 
    {
      if (  P->obj->fill_color != -1) 
	Xgc->graphic_engine->xset_pattern(Xgc,P->obj->fill_color);
      Xgc->graphic_engine->scale->fillrectangle(Xgc,val);
      if (  P->obj->fill_color != -1) 
	Xgc->graphic_engine->xset_pattern(Xgc,ccolor);
    }

  if ( P->obj->color != -2 ) 
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
}


static void nsp_translate_grrect(NspGraphic *Obj,const double *tr)
{
  NspGrRect *P = (NspGrRect *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->x += tr[0];
  P->obj->y += tr[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_grrect(NspGraphic *Obj,double *R)
{
  NspGrRect *P = (NspGrRect *) Obj;
  double x1;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  x1 = R[0]*(P->obj->x) -R[1]*(P->obj->y);
  P->obj->y = R[1]*(P->obj->x) +R[0]*(P->obj->y);
  P->obj->x = x1;
  /* Il faut aussi changer l'angle */
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_grrect(NspGraphic *Obj,double *alpha)
{
  NspGrRect *P = (NspGrRect *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->x *= alpha[0];
  P->obj->y *= alpha[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of grrect 
 *
 */

static int nsp_getbounds_grrect(NspGraphic *Obj,double *bounds)
{
  NspGrRect *P = (NspGrRect *) Obj;
  bounds[0]=P->obj->x;/* xmin */
  bounds[1]=P->obj->y-P->obj->w;/* ymin */
  bounds[2]=P->obj->x+P->obj->w;/* xmax */
  bounds[3]=P->obj->y;/* ymax */
  return TRUE;
}


#line 863 "grrect.c"
