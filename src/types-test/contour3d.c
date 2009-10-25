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





#line 25 "codegen/contour3d.override"
#include <nsp/object.h>
#include <nsp/contour3d.h>
#include <nsp/polyhedron.h>
#include <nsp/spolyhedron.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include "../graphics-new/Plo3dObj.h"
#include <nsp/grcommon.h>


#line 39 "contour3d.c"

/* ----------- NspContour3d ----------- */


#define  NspContour3d_Private 
#include <nsp/object.h>
#include <nsp/contour3d.h>
#include <nsp/interf.h>

/* 
 * NspContour3d inherits from Graphic 
 */

int nsp_type_contour3d_id=0;
NspTypeContour3d *nsp_type_contour3d=NULL;

/*
 * Type object for NspContour3d 
 * all the instance of NspTypeContour3d share the same id. 
 * nsp_type_contour3d: is an instance of NspTypeContour3d 
 *    used for objects of NspContour3d type (i.e built with new_contour3d) 
 * other instances are used for derived classes 
 */
NspTypeContour3d *new_type_contour3d(type_mode mode)
{
  NspTypeContour3d *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_contour3d != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_contour3d;
    }
  if (( type =  malloc(sizeof(NspTypeContour3d))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = contour3d_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = contour3d_get_methods;
  type->new = (new_func *) new_contour3d;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for contour3d */ 

  top->pr = (print_func *) nsp_contour3d_print;
  top->dealloc = (dealloc_func *) nsp_contour3d_destroy;
  top->copy  =  (copy_func *) nsp_contour3d_copy;
  top->size  = (size_func *) nsp_contour3d_size;
  top->s_type =  (s_type_func *) nsp_contour3d_type_as_string;
  top->sh_type = (sh_type_func *) nsp_contour3d_type_short_string;
  top->info = (info_func *) nsp_contour3d_info;
  /* top->is_true = (is_true_func  *) nsp_contour3d_is_true; */
  /* top->loop =(loop_func *) nsp_contour3d_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_contour3d_object;
  top->eq  = (eq_func *) nsp_contour3d_eq;
  top->neq  = (eq_func *) nsp_contour3d_neq;
  top->save  = (save_func *) nsp_contour3d_xdr_save;
  top->load  = (load_func *) nsp_contour3d_xdr_load;
  top->create = (create_func*) int_contour3d_create;
  top->latex = (print_func *) nsp_contour3d_latex;
  top->full_copy = (copy_func *) nsp_contour3d_full_copy;

  /* specific methods for contour3d */

  type->init = (init_func *) init_contour3d;

#line 42 "codegen/contour3d.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_contour3d;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_contour3d ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_contour3d  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_contour3d  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_contour3d  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Contour3d */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 
  ((NspTypeGraphic *) type->surtype)->zmean = nsp_contour3d_zmean;
  ((NspTypeGraphic *) type->surtype)->n_faces = nsp_contour3d_n_faces;

#line 126 "contour3d.c"
  /* 
   * NspContour3d interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_contour3d_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeContour3d called nsp_type_contour3d
       */
      type->id =  nsp_type_contour3d_id = nsp_new_type_id();
      nsp_type_contour3d = type;
      if ( nsp_register_type(nsp_type_contour3d) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_contour3d(mode);
    }
  else 
    {
      type->id = nsp_type_contour3d_id;
      return type;
    }
}

/*
 * initialize NspContour3d instances 
 * locally and by calling initializer on parent class 
 */

static int init_contour3d(NspContour3d *Obj,NspTypeContour3d *type)
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
 * new instance of NspContour3d 
 */

NspContour3d *new_contour3d() 
{
  NspContour3d *loc;
  /* type must exists */
  nsp_type_contour3d = new_type_contour3d(T_BASE);
  if ( (loc = malloc(sizeof(NspContour3d)))== NULLCONTOUR3D) return loc;
  /* initialize object */
  if ( init_contour3d(loc,nsp_type_contour3d) == FAIL) return NULLCONTOUR3D;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspContour3d 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_contour3d_size(NspContour3d *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char contour3d_type_name[]="Contour3d";
static char contour3d_short_type_name[]="contour3d";

static char *nsp_contour3d_type_as_string(void)
{
  return(contour3d_type_name);
}

static char *nsp_contour3d_type_short_string(NspObject *v)
{
  return(contour3d_short_type_name);
}

/*
 * A == B 
 */

static int nsp_contour3d_eq(NspContour3d *A, NspObject *B)
{
  NspContour3d *loc = (NspContour3d *) B;
  if ( check_cast(B,nsp_type_contour3d_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->z)->type->eq(A->obj->z,loc->obj->z) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->zz)->type->eq(A->obj->zz,loc->obj->zz) == FALSE ) return FALSE;
  if ( A->obj->flag != loc->obj->flag) return FALSE;
  if ( A->obj->zlev != loc->obj->zlev) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_contour3d_neq(NspContour3d *A, NspObject *B)
{
  return ( nsp_contour3d_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_contour3d_xdr_save(XDR *xdrs, NspContour3d *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_contour3d)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->z)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->zz)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->flag) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->zlev) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspContour3d  *nsp_contour3d_xdr_load_partial(XDR *xdrs, NspContour3d *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_contour3d))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->z =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->zz =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->flag) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->zlev) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspContour3d  *nsp_contour3d_xdr_load(XDR *xdrs)
{
  NspContour3d *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCONTOUR3D;
  if ((H  = nsp_contour3d_create_void(name,(NspTypeBase *) nsp_type_contour3d))== NULLCONTOUR3D) return H;
  if ((H  = nsp_contour3d_xdr_load_partial(xdrs,H))== NULLCONTOUR3D) return H;
  if ( nsp_contour3d_check_values(H) == FAIL) return NULLCONTOUR3D;

#line 65 "codegen/contour3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_contour3d(NULL,H)== FAIL) return NULL; 

#line 298 "contour3d.c"
  return H;
}

/*
 * delete 
 */

void nsp_contour3d_destroy_partial(NspContour3d *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 70 "codegen/contour3d.override"
  /* verbatim in destroy */

#line 315 "contour3d.c"
    if ( H->obj->x != NULL ) 
      nsp_matrix_destroy(H->obj->x);
    if ( H->obj->y != NULL ) 
      nsp_matrix_destroy(H->obj->y);
    if ( H->obj->z != NULL ) 
      nsp_matrix_destroy(H->obj->z);
    if ( H->obj->zz != NULL ) 
      nsp_matrix_destroy(H->obj->zz);
    FREE(H->obj);
   }
}

void nsp_contour3d_destroy(NspContour3d *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_contour3d_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_contour3d_info(NspContour3d *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCONTOUR3D) 
    {
      Sciprintf("Null Pointer NspContour3d \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_contour3d_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_contour3d_print(NspContour3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCONTOUR3D) 
    {
      Sciprintf("Null Pointer NspContour3d \n");
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
          nsp_contour3d_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_contour3d_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->z != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->z),indent+2,"z",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->zz != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->zz),indent+2,"zz",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"flag=%d\n",M->obj->flag);
  Sciprintf1(indent+2,"zlev=%f\n",M->obj->zlev);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_contour3d_latex(NspContour3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_contour3d_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->z != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->z),indent+2,"z",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->zz != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->zz),indent+2,"zz",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"flag=%d\n",M->obj->flag);
  Sciprintf1(indent+2,"zlev=%f\n",M->obj->zlev);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspContour3d objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspContour3d   *nsp_contour3d_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_contour3d_id) == TRUE ) return ((NspContour3d *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_contour3d));
  return NULL;
}

int IsContour3dObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_contour3d_id);
}

int IsContour3d(NspObject *O)
{
  return nsp_object_type(O,nsp_type_contour3d_id);
}

NspContour3d  *GetContour3dCopy(Stack stack, int i)
{
  if (  GetContour3d(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspContour3d  *GetContour3d(Stack stack, int i)
{
  NspContour3d *M;
  if (( M = nsp_contour3d_object(NthObj(i))) == NULLCONTOUR3D)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspContour3d instance 
 *-----------------------------------------------------*/

static NspContour3d *nsp_contour3d_create_void(char *name,NspTypeBase *type)
{
 NspContour3d *H  = (type == NULL) ? new_contour3d() : type->new();
 if ( H ==  NULLCONTOUR3D)
  {
   Sciprintf("No more memory\n");
   return NULLCONTOUR3D;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCONTOUR3D;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_contour3d_create_partial(NspContour3d *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_contour3d)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->z = NULLMAT;
  H->obj->zz = NULLMAT;
  H->obj->flag = 0;
  H->obj->zlev = 0.0;
  return OK;
}

int nsp_contour3d_check_values(NspContour3d *H)
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
  if ( H->obj->z == NULLMAT) 
    {
       if (( H->obj->z = nsp_matrix_create("z",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->zz == NULLMAT) 
    {
       if (( H->obj->zz = nsp_matrix_create("zz",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspContour3d *nsp_contour3d_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* z,NspMatrix* zz,int flag,double zlev,NspTypeBase *type)
{
 NspContour3d *H  = nsp_contour3d_create_void(name,type);
 if ( H ==  NULLCONTOUR3D) return NULLCONTOUR3D;
  if ( nsp_contour3d_create_partial(H) == FAIL) return NULLCONTOUR3D;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->z= z;
  H->obj->zz= zz;
  H->obj->flag=flag;
  H->obj->zlev=zlev;
 if ( nsp_contour3d_check_values(H) == FAIL) return NULLCONTOUR3D;
 return H;
}


NspContour3d *nsp_contour3d_create_default(char *name)
{
 NspContour3d *H  = nsp_contour3d_create_void(name,NULL);
 if ( H ==  NULLCONTOUR3D) return NULLCONTOUR3D;
  if ( nsp_contour3d_create_partial(H) == FAIL) return NULLCONTOUR3D;
 if ( nsp_contour3d_check_values(H) == FAIL) return NULLCONTOUR3D;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspContour3d *nsp_contour3d_copy_partial(NspContour3d *H,NspContour3d *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspContour3d *nsp_contour3d_copy(NspContour3d *self)
{
  NspContour3d *H  =nsp_contour3d_create_void(NVOID,(NspTypeBase *) nsp_type_contour3d);
  if ( H ==  NULLCONTOUR3D) return NULLCONTOUR3D;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCONTOUR3D;
  if ( nsp_contour3d_copy_partial(H,self)== NULL) return NULLCONTOUR3D;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspContour3d *nsp_contour3d_full_copy_partial(NspContour3d *H,NspContour3d *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_contour3d))) == NULL) return NULLCONTOUR3D;
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
  if ( self->obj->z == NULL )
    { H->obj->z = NULL;}
  else
    {
      if ((H->obj->z = (NspMatrix *) nsp_object_full_copy_and_name("z",NSP_OBJECT(self->obj->z))) == NULLMAT) return NULL;
    }
  if ( self->obj->zz == NULL )
    { H->obj->zz = NULL;}
  else
    {
      if ((H->obj->zz = (NspMatrix *) nsp_object_full_copy_and_name("zz",NSP_OBJECT(self->obj->zz))) == NULLMAT) return NULL;
    }
  H->obj->flag=self->obj->flag;
  H->obj->zlev=self->obj->zlev;
  return H;
}

NspContour3d *nsp_contour3d_full_copy(NspContour3d *self)
{
  NspContour3d *H  =nsp_contour3d_create_void(NVOID,(NspTypeBase *) nsp_type_contour3d);
  if ( H ==  NULLCONTOUR3D) return NULLCONTOUR3D;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCONTOUR3D;
  if ( nsp_contour3d_full_copy_partial(H,self)== NULL) return NULLCONTOUR3D;

#line 65 "codegen/contour3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_contour3d(NULL,H)== FAIL) return NULL; 

#line 623 "contour3d.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspContour3d
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_contour3d_create(Stack stack, int rhs, int opt, int lhs)
{
  NspContour3d *H;
  CheckStdRhs(0,0);
  /* want to be sure that type contour3d is initialized */
  nsp_type_contour3d = new_type_contour3d(T_BASE);
  if(( H = nsp_contour3d_create_void(NVOID,(NspTypeBase *) nsp_type_contour3d)) == NULLCONTOUR3D) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_contour3d_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_contour3d_check_values(H) == FAIL) return RET_BUG;

#line 65 "codegen/contour3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_contour3d(NULL,H)== FAIL) return RET_BUG; 

#line 648 "contour3d.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_nsp_contour3d_full_copy(NspContour3d *self,Stack stack,int rhs,int opt,int lhs)
{
  NspContour3d *ret;

  ret = nsp_contour3d_full_copy(self);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static NspMethods contour3d_methods[] = {
  {"full_copy",(nsp_method *) _wrap_nsp_contour3d_full_copy},
  { NULL, NULL}
};

static NspMethods *contour3d_get_methods(void) { return contour3d_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_contour3d_get_x(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour3d *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour3d_get_obj_x(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour3d *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_contour3d_set_x(void *self,const char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour3d *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspContour3d *) self)->obj->x);
  ((NspContour3d *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_contour3d_get_y(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour3d *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour3d_get_obj_y(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour3d *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_contour3d_set_y(void *self,const char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour3d *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspContour3d *) self)->obj->y);
  ((NspContour3d *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_contour3d_get_z(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour3d *) self)->obj->z;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour3d_get_obj_z(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour3d *) self)->obj->z);
  return (NspObject *) ret;
}

static int _wrap_contour3d_set_z(void *self,const char *attr, NspObject *O)
{
  NspMatrix *z;

  if ( ! IsMat(O) ) return FAIL;
  if ((z = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour3d *) self)->obj->z != NULL ) 
    nsp_matrix_destroy(((NspContour3d *) self)->obj->z);
  ((NspContour3d *) self)->obj->z= z;
  return OK;
}

static NspObject *_wrap_contour3d_get_zz(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour3d *) self)->obj->zz;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour3d_get_obj_zz(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour3d *) self)->obj->zz);
  return (NspObject *) ret;
}

static int _wrap_contour3d_set_zz(void *self,const char *attr, NspObject *O)
{
  NspMatrix *zz;

  if ( ! IsMat(O) ) return FAIL;
  if ((zz = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour3d *) self)->obj->zz != NULL ) 
    nsp_matrix_destroy(((NspContour3d *) self)->obj->zz);
  ((NspContour3d *) self)->obj->zz= zz;
  return OK;
}

static NspObject *_wrap_contour3d_get_flag(void *self,const char *attr)
{
  int ret;

  ret = ((NspContour3d *) self)->obj->flag;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_contour3d_set_flag(void *self,const char *attr, NspObject *O)
{
  int flag;

  if ( IntScalar(O,&flag) == FAIL) return FAIL;
  ((NspContour3d *) self)->obj->flag= flag;
  return OK;
}

static NspObject *_wrap_contour3d_get_zlev(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspContour3d *) self)->obj->zlev;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_contour3d_set_zlev(void *self,const char *attr, NspObject *O)
{
  double zlev;

  if ( DoubleScalar(O,&zlev) == FAIL) return FAIL;
  ((NspContour3d *) self)->obj->zlev= zlev;
  return OK;
}

static AttrTab contour3d_attrs[] = {
  { "x", (attr_get_function *)_wrap_contour3d_get_x, (attr_set_function *)_wrap_contour3d_set_x,(attr_get_object_function *)_wrap_contour3d_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_contour3d_get_y, (attr_set_function *)_wrap_contour3d_set_y,(attr_get_object_function *)_wrap_contour3d_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "z", (attr_get_function *)_wrap_contour3d_get_z, (attr_set_function *)_wrap_contour3d_set_z,(attr_get_object_function *)_wrap_contour3d_get_obj_z, (attr_set_object_function *)int_set_object_failed },
  { "zz", (attr_get_function *)_wrap_contour3d_get_zz, (attr_set_function *)_wrap_contour3d_set_zz,(attr_get_object_function *)_wrap_contour3d_get_obj_zz, (attr_set_object_function *)int_set_object_failed },
  { "flag", (attr_get_function *)_wrap_contour3d_get_flag, (attr_set_function *)_wrap_contour3d_set_flag,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "zlev", (attr_get_function *)_wrap_contour3d_get_zlev, (attr_set_function *)_wrap_contour3d_set_zlev,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 74 "codegen/contour3d.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_contour3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 851 "contour3d.c"


#line 84 "codegen/contour3d.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_contour3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 863 "contour3d.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Contour3d_func[]={
  {"extractelts_contour3d", _wrap_nsp_extractelts_contour3d},
  {"setrowscols_contour3d", _wrap_nsp_setrowscols_contour3d},
  { "contour3d_create", int_contour3d_create},
  { NULL, NULL}
};

/* call ith function in the Contour3d interface */

int Contour3d_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Contour3d_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Contour3d_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Contour3d_func[i].name;
  *f = Contour3d_func[i].fonc;
}

#line 94 "codegen/contour3d.override"

/* inserted verbatim at the end */

static void nsp_draw_contour3d(BCG *Xgc,NspGraphic *Obj, void *data)
{
  if ( Obj->obj->hidden == TRUE ) return ;
  nsp_check_contour3d(Xgc,(NspContour3d *) Obj);
#ifdef  WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      /* if we are using OpenGl we make a full draw of 
       * object and return 
       */
      draw_contour3d_ogl(Xgc,Obj);
      nsp_ogl_set_2dview(Xgc); 
      return; 
    }
#endif 
  {
    /* for contour3d we only have up to now one face
     * i.e we are only able to draw all the contours in 
     * one pass which is not good for mixing contour3d 
     * with other 3d graphics.
     for ( i= 0 ; i < ((NspContour3d*) Obj)->obj->Mface->n ; i++) 
     draw_contour3d_face(Xgc,Obj,i);
    */
    draw_contour3d_face(Xgc,Obj,0);
  }
}


static void nsp_translate_contour3d(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_contour3d(BCG *Xgc,NspGraphic *Obj,double *R)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_contour3d(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of contour3d 
 *
 */

extern void nsp_gr_bounds_min_max(int n,double *A,int incr,double *Amin, double *Amax) ;

static int nsp_getbounds_contour3d(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  /* this should be stored in a cache and recomputed when necessary 
   *
   */
  nsp_contour3d *Q= ((NspContour3d *) Obj)->obj;
  nsp_check_contour3d(Xgc,(NspContour3d *) Obj);
  nsp_gr_bounds_min_max(Q->x->mn,Q->x->R,1,&bounds[0],&bounds[1]);
  nsp_gr_bounds_min_max(Q->y->mn,Q->y->R,1,&bounds[2],&bounds[3]);
  nsp_gr_bounds_min_max(Q->z->mn,Q->z->R,1,&bounds[4],&bounds[5]);
  bounds[4]= Min(  Q->zlev,bounds[4]);
  bounds[5]= Max(  Q->zlev,bounds[5]);
  return TRUE;
}

int nsp_check_contour3d(BCG *Xgc, NspContour3d *P)
{
  nsp_contour3d *Q = P->obj;

  if ( Q->z->m != Q->x->mn ) 
    {
      Scierror("Error: bad coord for contour3d, z first dimension should be %d\n", Q->x->mn);
      return FAIL;
    }
  if ( Q->z->n != Q->y->mn ) 
    {
      Scierror("Error: bad coord for contour3d, z second dimension should be %d\n", Q->y->mn);
      return FAIL;
    }
  
  return OK;
}

extern int nsp_contour3d_draw_new(BCG *Xgc,double *x, double *y, double *z, 
			      int n1, int n2, int nz, double *zz, 
			      int flag, double zlev);


static void draw_contour3d_face(BCG *Xgc,NspGraphic *Ob, int j)
{
  /* draw all the faces */
  double *zz = NULL;
  nsp_contour3d *Q = ((NspContour3d *) Ob)->obj;
  int nz;
  if ( Q->zz->mn == 1) 
    {
      zz = NULL;
      nz = Q->zz->R[0];
    }
  else
    {
      zz = Q->zz->R;
      nz = Q->zz->mn;
    }

  nsp_contour3d_draw_new(Xgc,Q->x->R,Q->y->R,Q->z->R,Q->x->mn,Q->y->mn,nz, zz,Q->flag,Q->zlev);
}

static void draw_contour3d_ogl(BCG *Xgc,void *Ob)
{
  draw_contour3d_face(Xgc,Ob,0);
}



static void zmean_faces_for_Contour3d(void *Obj, double z[], HFstruct HF[], int *n, int k)
{
  /* nsp_contour3d *Q = ((NspContour3d *) Obj)->obj; */
  z[*n] = 0.0;
  HF[*n].num_obj = k;
  HF[*n].num_in_obj = 0;
  (*n)++; 
}

/*
 * requested method for 3d objects.
 */

static void nsp_contour3d_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim)
{
  /* nsp_contour3d *Q= ((NspContour3d *) Obj)->obj; */
  /* apply_transforms_new(Xgc,((NspMatrix *) Q->Mcoord_l)->R,Q->Mcoord->R,Q->pos, lim, Q->Mcoord->m); */
  zmean_faces_for_Contour3d(Obj, z,  HF, n, k);
}

/* requested method for 3d objects.
 *
 */

static int nsp_contour3d_n_faces(BCG *Xgc,NspGraphic *Obj)
{
  return 1;
}


#line 1043 "contour3d.c"
