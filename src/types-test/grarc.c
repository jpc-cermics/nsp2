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





#line 28 "codegen/grarc.override"
#include <gdk/gdk.h>
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/grarc.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 

#line 36 "grarc.c"

/* -----------NspGrArc ----------- */


#define  NspGrArc_Private 
#include <nsp/objects.h>
#include <nsp/grarc.h>
#include <nsp/interf.h>

/* 
 * NspGrArc inherits from Graphic 
 */

int nsp_type_grarc_id=0;
NspTypeGrArc *nsp_type_grarc=NULL;

/*
 * Type object for NspGrArc 
 * all the instance of NspTypeGrArc share the same id. 
 * nsp_type_grarc: is an instance of NspTypeGrArc 
 *    used for objects of NspGrArc type (i.e built with new_grarc) 
 * other instances are used for derived classes 
 */
NspTypeGrArc *new_type_grarc(type_mode mode)
{
  NspTypeGrArc *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_grarc != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_grarc;
    }
  if (( type =  malloc(sizeof(NspTypeGrArc))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = grarc_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = grarc_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_grarc;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for grarc */ 

  top->pr = (print_func *) nsp_grarc_print;
  top->dealloc = (dealloc_func *) nsp_grarc_destroy;
  top->copy  =  (copy_func *) nsp_grarc_copy;
  top->size  = (size_func *) nsp_grarc_size;
  top->s_type =  (s_type_func *) nsp_grarc_type_as_string;
  top->sh_type = (sh_type_func *) nsp_grarc_type_short_string;
  top->info = (info_func *) nsp_grarc_info;
  /* top->is_true = (is_true_func  *) nsp_grarc_is_true; */
  /* top->loop =(loop_func *) nsp_grarc_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_grarc_object;
  top->eq  = (eq_func *) nsp_grarc_eq;
  top->neq  = (eq_func *) nsp_grarc_neq;
  top->save  = (save_func *) nsp_grarc_xdr_save;
  top->load  = (load_func *) nsp_grarc_xdr_load;
  top->create = (create_func*) int_grarc_create;
  top->latex = (print_func *) nsp_grarc_latex;
  top->full_copy = (copy_func *) nsp_grarc_full_copy;

  /* specific methods for grarc */

  type->init = (init_func *) init_grarc;

#line 42 "codegen/grarc.override"
  /* inserted verbatim in the type definition */
  type->gtk_methods = TRUE;
  /* here we override the method or its father class i.e Graphic */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_grarc;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_grarc ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_grarc  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_grarc  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_grarc  ;
  /* next method are defined in NspGraphic and need not be chnaged here for GrArc */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 122 "grarc.c"
  /* 
   * NspGrArc interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_grarc_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGrArc called nsp_type_grarc
       */
      type->id =  nsp_type_grarc_id = nsp_new_type_id();
      nsp_type_grarc = type;
      if ( nsp_register_type(nsp_type_grarc) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_grarc(mode);
    }
  else 
    {
      type->id = nsp_type_grarc_id;
      return type;
    }
}

/*
 * initialize NspGrArc instances 
 * locally and by calling initializer on parent class 
 */

static int init_grarc(NspGrArc *Obj,NspTypeGrArc *type)
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
 * new instance of NspGrArc 
 */

NspGrArc *new_grarc() 
{
  NspGrArc *loc;
  /* type must exists */
  nsp_type_grarc = new_type_grarc(T_BASE);
  if ( (loc = malloc(sizeof(NspGrArc)))== NULLGRARC) return loc;
  /* initialize object */
  if ( init_grarc(loc,nsp_type_grarc) == FAIL) return NULLGRARC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGrArc 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_grarc_size(NspGrArc *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char grarc_type_name[]="GrArc";
static char grarc_short_type_name[]="grarc";

static char *nsp_grarc_type_as_string(void)
{
  return(grarc_type_name);
}

static char *nsp_grarc_type_short_string(NspObject *v)
{
  return(grarc_short_type_name);
}

/*
 * A == B 
 */

static int nsp_grarc_eq(NspGrArc *A, NspObject *B)
{
  NspGrArc *loc = (NspGrArc *) B;
  if ( check_cast(B,nsp_type_grarc_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->x != loc->obj->x) return FALSE;
  if ( A->obj->y != loc->obj->y) return FALSE;
  if ( A->obj->w != loc->obj->w) return FALSE;
  if ( A->obj->h != loc->obj->h) return FALSE;
  if ( A->obj->a1 != loc->obj->a1) return FALSE;
  if ( A->obj->a2 != loc->obj->a2) return FALSE;
  if ( A->obj->fill_color != loc->obj->fill_color) return FALSE;
  if ( A->obj->thickness != loc->obj->thickness) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->angle != loc->obj->angle) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_grarc_neq(NspGrArc *A, NspObject *B)
{
  return ( nsp_grarc_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_grarc_xdr_save(XDR *xdrs, NspGrArc *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_grarc)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->x) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->y) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->w) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->h) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->a1) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->a2) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->fill_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->thickness) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->angle) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGrArc  *nsp_grarc_xdr_load_partial(XDR *xdrs, NspGrArc *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if (nsp_xdr_load_d(xdrs, &M->obj->x) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->y) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->w) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->h) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->a1) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->a2) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->fill_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->thickness) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->angle) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic * )M) == NULL) return NULL;
 return M;
}

static NspGrArc  *nsp_grarc_xdr_load(XDR *xdrs)
{
  NspGrArc *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRARC;
  if ((H  = nsp_grarc_create_void(name,(NspTypeBase *) nsp_type_grarc))== NULLGRARC) return H;
  if ( nsp_grarc_create_partial(H) == FAIL) return NULLGRARC;
  if ((H  = nsp_grarc_xdr_load_partial(xdrs,H))== NULLGRARC) return H;
  if ( nsp_grarc_check_values(H) == FAIL) return NULLGRARC;
  return H;
}

/*
 * delete 
 */

void nsp_grarc_destroy_partial(NspGrArc *H)
{
  nsp_graphic_destroy_partial((NspGraphic * ) H);
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
}

void nsp_grarc_destroy(NspGrArc *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_grarc_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_grarc_info(NspGrArc *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRARC) 
    {
      Sciprintf("Null Pointer NspGrArc \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_grarc_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_grarc_print(NspGrArc *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRARC) 
    {
      Sciprintf("Null Pointer NspGrArc \n");
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
          nsp_grarc_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_grarc_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"x=%f\n", M->obj->x);
  Sciprintf1(indent+2,"y=%f\n", M->obj->y);
  Sciprintf1(indent+2,"w=%f\n", M->obj->w);
  Sciprintf1(indent+2,"h=%f\n", M->obj->h);
  Sciprintf1(indent+2,"a1=%f\n", M->obj->a1);
  Sciprintf1(indent+2,"a2=%f\n", M->obj->a2);
  Sciprintf1(indent+2,"fill_color=%d\n", M->obj->fill_color);
  Sciprintf1(indent+2,"thickness=%d\n", M->obj->thickness);
  Sciprintf1(indent+2,"color=%d\n", M->obj->color);
  Sciprintf1(indent+2,"angle=%f\n", M->obj->angle);
  nsp_graphic_print((NspGraphic * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_grarc_latex(NspGrArc *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_grarc_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|x| = \\numprint{%f}\n", M->obj->x);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|y| = \\numprint{%f}\n", M->obj->y);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|w| = \\numprint{%f}\n", M->obj->w);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|h| = \\numprint{%f}\n", M->obj->h);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|a1| = \\numprint{%f}\n", M->obj->a1);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|a2| = \\numprint{%f}\n", M->obj->a2);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|fill_color|= \\numprint{%d}\n",M->obj->fill_color);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|thickness|= \\numprint{%d}\n",M->obj->thickness);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|color|= \\numprint{%d}\n",M->obj->color);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|angle| = \\numprint{%f}\n", M->obj->angle);
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
 * for NspGrArc objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGrArc   *nsp_grarc_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_grarc_id)  == TRUE  ) return ((NspGrArc *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_grarc));
  return NULL;
}

int IsGrArcObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_grarc_id);
}

int IsGrArc(NspObject *O)
{
  return nsp_object_type(O,nsp_type_grarc_id);
}

NspGrArc  *GetGrArcCopy(Stack stack, int i)
{
  if (  GetGrArc(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGrArc  *GetGrArc(Stack stack, int i)
{
  NspGrArc *M;
  if (( M = nsp_grarc_object(NthObj(i))) == NULLGRARC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGrArc instance 
 *-----------------------------------------------------*/

static NspGrArc *nsp_grarc_create_void(const char *name,NspTypeBase *type)
{
 NspGrArc *H  = (type == NULL) ? new_grarc() : type->new();
 if ( H ==  NULLGRARC)
  {
   Sciprintf("No more memory\n");
   return NULLGRARC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRARC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_grarc_create_partial(NspGrArc *H)
{
  if ( nsp_graphic_create_partial((NspGraphic * ) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_grarc)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = 0.0;
  H->obj->y = 0.0;
  H->obj->w = 0.0;
  H->obj->h = 0.0;
  H->obj->a1 = 0.0;
  H->obj->a2 = 0.0;
  H->obj->fill_color = -1;
  H->obj->thickness = 0;
  H->obj->color = 0;
  H->obj->angle = 0;
  return OK;
}

int nsp_grarc_check_values(NspGrArc *H)
{
  nsp_graphic_check_values((NspGraphic * ) H);
  return OK;
}

NspGrArc *nsp_grarc_create(const char *name,double x,double y,double w,double h,double a1,double a2,int fill_color,int thickness,int color,double angle,NspTypeBase *type)
{
  NspGrArc *H  = nsp_grarc_create_void(name,type);
  if ( H ==  NULLGRARC) return NULLGRARC;
  if ( nsp_grarc_create_partial(H) == FAIL) return NULLGRARC;
  H->obj->x=x;
  H->obj->y=y;
  H->obj->w=w;
  H->obj->h=h;
  H->obj->a1=a1;
  H->obj->a2=a2;
  H->obj->fill_color=fill_color;
  H->obj->thickness=thickness;
  H->obj->color=color;
  H->obj->angle=angle;
  if ( nsp_grarc_check_values(H) == FAIL) return NULLGRARC;
  return H;
}


NspGrArc *nsp_grarc_create_default(const char *name)
{
 NspGrArc *H  = nsp_grarc_create_void(name,NULL);
 if ( H ==  NULLGRARC) return NULLGRARC;
  if ( nsp_grarc_create_partial(H) == FAIL) return NULLGRARC;
  if ( nsp_grarc_check_values(H) == FAIL) return NULLGRARC;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGrArc *nsp_grarc_copy_partial(NspGrArc *H,NspGrArc *self)
{
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLGRARC;
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGrArc *nsp_grarc_copy(NspGrArc *self)
{
  NspGrArc *H  =nsp_grarc_create_void(NVOID,(NspTypeBase *) nsp_type_grarc);
  if ( H ==  NULLGRARC) return NULLGRARC;
  if ( nsp_grarc_copy_partial(H,self)== NULL) return NULLGRARC;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGrArc *nsp_grarc_full_copy_partial(NspGrArc *H,NspGrArc *self)
{
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic * ) self ) == NULL) return NULLGRARC;
  if ((H->obj = calloc(1,sizeof(nsp_grarc))) == NULL) return NULLGRARC;
  H->obj->ref_count=1;
  H->obj->x=self->obj->x;
  H->obj->y=self->obj->y;
  H->obj->w=self->obj->w;
  H->obj->h=self->obj->h;
  H->obj->a1=self->obj->a1;
  H->obj->a2=self->obj->a2;
  H->obj->fill_color=self->obj->fill_color;
  H->obj->thickness=self->obj->thickness;
  H->obj->color=self->obj->color;
  H->obj->angle=self->obj->angle;
  return H;
}

NspGrArc *nsp_grarc_full_copy(NspGrArc *self)
{
  NspGrArc *H  =nsp_grarc_create_void(NVOID,(NspTypeBase *) nsp_type_grarc);
  if ( H ==  NULLGRARC) return NULLGRARC;
  if ( nsp_grarc_full_copy_partial(H,self)== NULL) return NULLGRARC;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGrArc
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_grarc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGrArc *H;
  CheckStdRhs(0,0);
  /* want to be sure that type grarc is initialized */
  nsp_type_grarc = new_type_grarc(T_BASE);
  if(( H = nsp_grarc_create_void(NVOID,(NspTypeBase *) nsp_type_grarc)) == NULLGRARC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_grarc_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_grarc_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *grarc_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_grarc_get_x(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGrArc *) self)->obj->x;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_x(void *self,const char *attr, NspObject *O)
{
  double x;
  if ( DoubleScalar(O,&x) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_grarc_get_y(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGrArc *) self)->obj->y;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_y(void *self,const char *attr, NspObject *O)
{
  double y;
  if ( DoubleScalar(O,&y) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_grarc_get_w(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGrArc *) self)->obj->w;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_w(void *self,const char *attr, NspObject *O)
{
  double w;
  if ( DoubleScalar(O,&w) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->w= w;
  return OK;
}

static NspObject *_wrap_grarc_get_h(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGrArc *) self)->obj->h;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_h(void *self,const char *attr, NspObject *O)
{
  double h;
  if ( DoubleScalar(O,&h) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->h= h;
  return OK;
}

static NspObject *_wrap_grarc_get_a1(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGrArc *) self)->obj->a1;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_a1(void *self,const char *attr, NspObject *O)
{
  double a1;
  if ( DoubleScalar(O,&a1) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->a1= a1;
  return OK;
}

static NspObject *_wrap_grarc_get_a2(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGrArc *) self)->obj->a2;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_a2(void *self,const char *attr, NspObject *O)
{
  double a2;
  if ( DoubleScalar(O,&a2) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->a2= a2;
  return OK;
}

static NspObject *_wrap_grarc_get_fill_color(void *self,const char *attr)
{
  int ret;
  ret = ((NspGrArc *) self)->obj->fill_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grarc_set_fill_color(void *self,const char *attr, NspObject *O)
{
  int fill_color;
  if ( IntScalar(O,&fill_color) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->fill_color= fill_color;
  return OK;
}

static NspObject *_wrap_grarc_get_thickness(void *self,const char *attr)
{
  int ret;
  ret = ((NspGrArc *) self)->obj->thickness;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grarc_set_thickness(void *self,const char *attr, NspObject *O)
{
  int thickness;
  if ( IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->thickness= thickness;
  return OK;
}

static NspObject *_wrap_grarc_get_color(void *self,const char *attr)
{
  int ret;
  ret = ((NspGrArc *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_grarc_set_color(void *self,const char *attr, NspObject *O)
{
  int color;
  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->color= color;
  return OK;
}

static NspObject *_wrap_grarc_get_angle(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspGrArc *) self)->obj->angle;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_grarc_set_angle(void *self,const char *attr, NspObject *O)
{
  double angle;
  if ( DoubleScalar(O,&angle) == FAIL) return FAIL;
  ((NspGrArc *) self)->obj->angle= angle;
  return OK;
}

static AttrTab grarc_attrs[] = {
  { "x", (attr_get_function * )_wrap_grarc_get_x, (attr_set_function * )_wrap_grarc_set_x, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "y", (attr_get_function * )_wrap_grarc_get_y, (attr_set_function * )_wrap_grarc_set_y, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "w", (attr_get_function * )_wrap_grarc_get_w, (attr_set_function * )_wrap_grarc_set_w, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "h", (attr_get_function * )_wrap_grarc_get_h, (attr_set_function * )_wrap_grarc_set_h, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "a1", (attr_get_function * )_wrap_grarc_get_a1, (attr_set_function * )_wrap_grarc_set_a1, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "a2", (attr_get_function * )_wrap_grarc_get_a2, (attr_set_function * )_wrap_grarc_set_a2, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "fill_color", (attr_get_function * )_wrap_grarc_get_fill_color, (attr_set_function * )_wrap_grarc_set_fill_color, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "thickness", (attr_get_function * )_wrap_grarc_get_thickness, (attr_set_function * )_wrap_grarc_set_thickness, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "color", (attr_get_function * )_wrap_grarc_get_color, (attr_set_function * )_wrap_grarc_set_color, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "angle", (attr_get_function * )_wrap_grarc_get_angle, (attr_set_function * )_wrap_grarc_set_angle, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 64 "codegen/grarc.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_grarc(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 816 "grarc.c"


#line 74 "codegen/grarc.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_grarc(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 829 "grarc.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GrArc_func[]={
  { "extractelts_grarc", _wrap_nsp_extractelts_grarc},
  { "setrowscols_grarc", _wrap_nsp_setrowscols_grarc},
  { "grarc_create", int_grarc_create},
  { NULL, NULL}
};

/* call ith function in the GrArc interface */

int GrArc_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(GrArc_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GrArc_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = GrArc_func[i].name;
  *f = GrArc_func[i].fonc;
}
void nsp_initialize_GrArc_types(void)
{
  new_type_grarc(T_BASE);
}

#line 85 "codegen/grarc.override"

/* inserted verbatim at the end */

static void nsp_draw_grarc(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  NspGrArc *P = (NspGrArc *) Obj;
  double val[6]={ P->obj->x, P->obj->y, P->obj->w, P->obj->h, P->obj->a1, P->obj->a2};
  int ccolor=-1,cthick=-1;

  if ( ((NspGraphic *) P)->obj->show == FALSE ) return;

  /* check if the block is inside drawing rectangle
   */

  if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
    {
      return ;
    }

  ccolor = Xgc->graphic_engine->xget_color(Xgc); 
  
  if ( P->obj->fill_color != -2 ) 
    {
      /* fill the arc */ 
      if (  P->obj->fill_color != -1) 
	Xgc->graphic_engine->xset_color(Xgc,P->obj->fill_color);
      if (  P->obj->angle != 0.0 ) 
	nsp_fill_polyline_grarc(Xgc,P);
      else
	Xgc->graphic_engine->scale->fillarc(Xgc,val);
      if (  P->obj->fill_color != -1) 
	Xgc->graphic_engine->xset_color(Xgc,ccolor);
    }
  
  if ( P->obj->color != -2 ) 
    {
      /* draw the arc */ 
      if ( P->obj->color != -1 ) 
	Xgc->graphic_engine->xset_color(Xgc,P->obj->color);
      if ( P->obj->thickness != -1 ) 
	{
	  cthick = Xgc->graphic_engine->xget_thickness(Xgc); 
	  Xgc->graphic_engine->xset_thickness(Xgc,P->obj->thickness);
	}
      if (  P->obj->angle != 0.0 ) 
	nsp_draw_polyline_grarc(Xgc,P);
      else
	Xgc->graphic_engine->scale->drawarc(Xgc,val);
      /* reset to default values */
      if ( P->obj->color != -1 ) 
	Xgc->graphic_engine->xset_color(Xgc,ccolor);
      if ( P->obj->thickness != -1 ) 
	Xgc->graphic_engine->xset_thickness(Xgc,cthick);
    }
}

static void nsp_translate_grarc(NspGraphic *Obj,const double *tr)
{
  NspGrArc *P = (NspGrArc *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->x += tr[0];
  P->obj->y += tr[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_grarc(NspGraphic *Obj,double *R)
{
  NspGrArc *P = (NspGrArc *) Obj;
  double x1,y1;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  /* the rectangle is rotated in such a way that the 
   * draw function will have to make a rotation with 
   * center the center of the rectangle 
   */
  /* rotate the center */
  x1 = R[0]*(P->obj->x+ P->obj->w/2) -R[1]*(P->obj->y - P->obj->h/2);
  y1 = R[1]*(P->obj->x+ P->obj->w/2) +R[0]*(P->obj->y - P->obj->h/2);
  P->obj->x = x1 - P->obj->w/2;
  P->obj->y = y1 + P->obj->h/2;
  /* changer angle */
  P->obj->angle += - atan2(R[1],R[0])*180/M_PI;
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_grarc(NspGraphic *Obj,double *alpha)
{
  NspGrArc *P = (NspGrArc *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  P->obj->x *= alpha[0];
  P->obj->y *= alpha[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of grarc 
 *
 */

static int nsp_getbounds_grarc(NspGraphic *Obj,double *bounds)
{
  NspGrArc *P = (NspGrArc *) Obj;

  if (  P->obj->angle == 0.0 ) 
    {
      bounds[0]=P->obj->x;/* xmin */
      bounds[1]=P->obj->y-P->obj->h;/* ymin */
      bounds[2]=P->obj->x+P->obj->w;/* xmax */
      bounds[3]=P->obj->y;/* ymax */
    }
  else
    {
      int i;
      double vx[4]={P->obj->x,P->obj->x+P->obj->w,P->obj->x+P->obj->w,P->obj->x};
      double vy[4]={P->obj->y,P->obj->y,P->obj->y-P->obj->h,P->obj->y-P->obj->h};
      double rvx[4],rvy[4],cx=P->obj->x+P->obj->w/2,cy=P->obj->y-P->obj->h/2;
      double cosa = cos(-P->obj->angle*M_PI/180);
      double sina = sin(-P->obj->angle*M_PI/180);
      for ( i= 0 ; i < 4; i++)
	{
	  rvx[i]=cosa*(vx[i]-cx)- sina*(vy[i]-cy)+cx;
	  rvy[i]=sina*(vx[i]-cx)+ cosa*(vy[i]-cy)+cy;
	}
      bounds[0]=bounds[2]=rvx[0];
      bounds[1]=bounds[3]=rvy[0];
      for ( i= 0 ; i < 4; i++)
	{
	  if ( rvx[i] < bounds[0]) bounds[0] = rvx[i];
	  if ( rvy[i] < bounds[1]) bounds[1] = rvy[i];
	  if ( rvx[i] > bounds[2]) bounds[2] = rvx[i];
	  if ( rvy[i] > bounds[3]) bounds[3] = rvy[i];
	}
    }
  return TRUE;
}

/* draw a grarc using a polyline (used when rotation is involved) */

static void nsp_draw_polyline_grarc(BCG *Xgc,NspGrArc *P)
{ 
  double cosa = cos(-P->obj->angle*M_PI/180);
  double sina = sin(-P->obj->angle*M_PI/180);
  double cx=P->obj->x+P->obj->w/2,cy=P->obj->y-P->obj->h/2;
  double vx[365],vy[365];
  int k, n = Min((P->obj->a2/64),360);
  for (k = 0; k < n; ++k) 
    {
      double alpha=(( P->obj->a1/64)+k)*M_PI/180,x,y;
      x = P->obj->x + (P->obj->w/2.0)*(cos(alpha)+1.0);
      y = P->obj->y - (P->obj->h/2.0)*(-sin(alpha)+1.0);
      vx[k]=cosa*(x-cx)- sina*(y-cy)+cx;
      vy[k]=sina*(x-cx)+ cosa*(y-cy)+cy;
    }
  Xgc->graphic_engine->scale->drawpolyline(Xgc,vx, vy,n,0);
}

static void nsp_fill_polyline_grarc( BCG *Xgc,NspGrArc *P)
{ 
  double cosa = cos(-P->obj->angle*M_PI/180);
  double sina = sin(-P->obj->angle*M_PI/180);
  double cx=P->obj->x+P->obj->w/2,cy=P->obj->y-P->obj->h/2;
  double x,y;
  double vx[365],vy[365];
  int k,close = 1;
  /* drawarc_gen(Xgc,arc); */
  int n = Min(P->obj->a2/64,360), count=0;
  
  if (n != 360) 
    {
      x = P->obj->x + (P->obj->w/2.0);
      y = P->obj->y - (P->obj->h/2.0);
      vx[count]=cosa*(x-cx)- sina*(y-cy)+cx;
      vy[count]=sina*(x-cx)+ cosa*(y-cy)+cy;
      count++;
    }
  for (k = 0; k < n ; ++k) 
    {
      double alpha=((P->obj->a1/64)+k)*M_PI/180;
      x = P->obj->x + (P->obj->w/2.0)*(cos(alpha)+1.0);
      y = P->obj->y - (P->obj->h/2.0)*(-sin(alpha)+1.0);
      vx[count]=cosa*(x-cx)- sina*(y-cy)+cx;
      vy[count]=sina*(x-cx)+ cosa*(y-cy)+cy;
      count++;
    }
  if (n != 360) 
    {
      x = P->obj->x + (P->obj->w/2.0);
      y = P->obj->y - (P->obj->h/2.0);
      vx[count]=cosa*(x-cx)- sina*(y-cy)+cx;
      vy[count]=sina*(x-cx)+ cosa*(y-cy)+cy;
      count++;
    }
  Xgc->graphic_engine->scale->fillpolyline(Xgc,vx, vy,count,close);
}


#line 1059 "grarc.c"
