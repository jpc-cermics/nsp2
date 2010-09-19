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





#line 33 "codegen/polyline3d.override"
#include <gdk/gdk.h>
#include <nsp/objects.h>
#include <nsp/polyline3d.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include "../graphics-new/Plo3dObj.h"
#include <nsp/polyhedron.h>
#include <nsp/spolyhedron.h>
#include <nsp/grcommon.h>

#line 39 "polyline3d.c"

/* ----------- NspPolyline3d ----------- */


#define  NspPolyline3d_Private 
#include <nsp/objects.h>
#include <nsp/polyline3d.h>
#include <nsp/interf.h>

/* 
 * NspPolyline3d inherits from Graphic 
 */

int nsp_type_polyline3d_id=0;
NspTypePolyline3d *nsp_type_polyline3d=NULL;

/*
 * Type object for NspPolyline3d 
 * all the instance of NspTypePolyline3d share the same id. 
 * nsp_type_polyline3d: is an instance of NspTypePolyline3d 
 *    used for objects of NspPolyline3d type (i.e built with new_polyline3d) 
 * other instances are used for derived classes 
 */
NspTypePolyline3d *new_type_polyline3d(type_mode mode)
{
  NspTypePolyline3d *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_polyline3d != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_polyline3d;
    }
  if (( type =  malloc(sizeof(NspTypePolyline3d))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = polyline3d_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = polyline3d_get_methods;
  type->new = (new_func *) new_polyline3d;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for polyline3d */ 

  top->pr = (print_func *) nsp_polyline3d_print;
  top->dealloc = (dealloc_func *) nsp_polyline3d_destroy;
  top->copy  =  (copy_func *) nsp_polyline3d_copy;
  top->size  = (size_func *) nsp_polyline3d_size;
  top->s_type =  (s_type_func *) nsp_polyline3d_type_as_string;
  top->sh_type = (sh_type_func *) nsp_polyline3d_type_short_string;
  top->info = (info_func *) nsp_polyline3d_info;
  /* top->is_true = (is_true_func  *) nsp_polyline3d_is_true; */
  /* top->loop =(loop_func *) nsp_polyline3d_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_polyline3d_object;
  top->eq  = (eq_func *) nsp_polyline3d_eq;
  top->neq  = (eq_func *) nsp_polyline3d_neq;
  top->save  = (save_func *) nsp_polyline3d_xdr_save;
  top->load  = (load_func *) nsp_polyline3d_xdr_load;
  top->create = (create_func*) int_polyline3d_create;
  top->latex = (print_func *) nsp_polyline3d_latex;
  top->full_copy = (copy_func *) nsp_polyline3d_full_copy;

  /* specific methods for polyline3d */

  type->init = (init_func *) init_polyline3d;

#line 50 "codegen/polyline3d.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_polyline3d;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_polyline3d ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_polyline3d  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_polyline3d  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_polyline3d  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Polyline3d */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 
  ((NspTypeGraphic *) type->surtype)->zmean = nsp_polyline3d_zmean;
  ((NspTypeGraphic *) type->surtype)->n_faces = nsp_polyline3d_n_faces;

#line 126 "polyline3d.c"
  /* 
   * NspPolyline3d interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_polyline3d_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePolyline3d called nsp_type_polyline3d
       */
      type->id =  nsp_type_polyline3d_id = nsp_new_type_id();
      nsp_type_polyline3d = type;
      if ( nsp_register_type(nsp_type_polyline3d) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_polyline3d(mode);
    }
  else 
    {
      type->id = nsp_type_polyline3d_id;
      return type;
    }
}

/*
 * initialize NspPolyline3d instances 
 * locally and by calling initializer on parent class 
 */

static int init_polyline3d(NspPolyline3d *Obj,NspTypePolyline3d *type)
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
 * new instance of NspPolyline3d 
 */

NspPolyline3d *new_polyline3d() 
{
  NspPolyline3d *loc;
  /* type must exists */
  nsp_type_polyline3d = new_type_polyline3d(T_BASE);
  if ( (loc = malloc(sizeof(NspPolyline3d)))== NULLPOLYLINE3D) return loc;
  /* initialize object */
  if ( init_polyline3d(loc,nsp_type_polyline3d) == FAIL) return NULLPOLYLINE3D;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspPolyline3d 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_polyline3d_size(NspPolyline3d *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char polyline3d_type_name[]="Polyline3d";
static char polyline3d_short_type_name[]="polyline3d";

static char *nsp_polyline3d_type_as_string(void)
{
  return(polyline3d_type_name);
}

static char *nsp_polyline3d_type_short_string(NspObject *v)
{
  return(polyline3d_short_type_name);
}

/*
 * A == B 
 */

static int nsp_polyline3d_eq(NspPolyline3d *A, NspObject *B)
{
  NspPolyline3d *loc = (NspPolyline3d *) B;
  if ( check_cast(B,nsp_type_polyline3d_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->Mcoord)->type->eq(A->obj->Mcoord,loc->obj->Mcoord) == FALSE ) return FALSE;
  if ( A->obj->Mcoord_l != loc->obj->Mcoord_l) return FALSE;
  if ( NSP_OBJECT(A->obj->Mcolor)->type->eq(A->obj->Mcolor,loc->obj->Mcolor) == FALSE ) return FALSE;
  {int i;
    for ( i = 0 ; i < A->obj->pos_length ; i++)
      if ( A->obj->pos[i] != loc->obj->pos[i]) return FALSE;
  }
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_polyline3d_neq(NspPolyline3d *A, NspObject *B)
{
  return ( nsp_polyline3d_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_polyline3d_xdr_save(XDR *xdrs, NspPolyline3d *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_polyline3d)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcoord)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcolor)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspPolyline3d  *nsp_polyline3d_xdr_load_partial(XDR *xdrs, NspPolyline3d *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ((M->obj->Mcoord =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->Mcolor =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspPolyline3d  *nsp_polyline3d_xdr_load(XDR *xdrs)
{
  NspPolyline3d *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPOLYLINE3D;
  if ((H  = nsp_polyline3d_create_void(name,(NspTypeBase *) nsp_type_polyline3d))== NULLPOLYLINE3D) return H;
  if ( nsp_polyline3d_create_partial(H) == FAIL) return NULLPOLYLINE3D;
  if ((H  = nsp_polyline3d_xdr_load_partial(xdrs,H))== NULLPOLYLINE3D) return H;
  if ( nsp_polyline3d_check_values(H) == FAIL) return NULLPOLYLINE3D;
#line 74 "codegen/polyline3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_polyline3d(H)== FAIL) return NULL; 

#line 290 "polyline3d.c"
  return H;
}

/*
 * delete 
 */

void nsp_polyline3d_destroy_partial(NspPolyline3d *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 79 "codegen/polyline3d.override"
  /* verbatim in destroy */
  nsp_matrix_destroy(H->obj->Mcoord_l);

#line 308 "polyline3d.c"
    if ( H->obj->Mcoord != NULL ) 
      nsp_matrix_destroy(H->obj->Mcoord);
    if ( H->obj->Mcolor != NULL ) 
      nsp_matrix_destroy(H->obj->Mcolor);
    FREE(H->obj->pos);
    FREE(H->obj);
   }
}

void nsp_polyline3d_destroy(NspPolyline3d *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_polyline3d_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_polyline3d_info(NspPolyline3d *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLPOLYLINE3D) 
    {
      Sciprintf("Null Pointer NspPolyline3d \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_polyline3d_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_polyline3d_print(NspPolyline3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLPOLYLINE3D) 
    {
      Sciprintf("Null Pointer NspPolyline3d \n");
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
          nsp_polyline3d_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_polyline3d_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  if ( M->obj->Mcolor != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcolor),indent+2,"Mcolor",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_polyline3d_latex(NspPolyline3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_polyline3d_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  if ( M->obj->Mcolor != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcolor),indent+2,"Mcolor",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPolyline3d objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspPolyline3d   *nsp_polyline3d_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_polyline3d_id) == TRUE ) return ((NspPolyline3d *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_polyline3d));
  return NULL;
}

int IsPolyline3dObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_polyline3d_id);
}

int IsPolyline3d(NspObject *O)
{
  return nsp_object_type(O,nsp_type_polyline3d_id);
}

NspPolyline3d  *GetPolyline3dCopy(Stack stack, int i)
{
  if (  GetPolyline3d(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPolyline3d  *GetPolyline3d(Stack stack, int i)
{
  NspPolyline3d *M;
  if (( M = nsp_polyline3d_object(NthObj(i))) == NULLPOLYLINE3D)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspPolyline3d instance 
 *-----------------------------------------------------*/

static NspPolyline3d *nsp_polyline3d_create_void(const char *name,NspTypeBase *type)
{
 NspPolyline3d *H  = (type == NULL) ? new_polyline3d() : type->new();
 if ( H ==  NULLPOLYLINE3D)
  {
   Sciprintf("No more memory\n");
   return NULLPOLYLINE3D;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLPOLYLINE3D;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_polyline3d_create_partial(NspPolyline3d *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_polyline3d)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->Mcoord = NULLMAT;
  H->obj->Mcoord_l = NULL;
  H->obj->Mcolor = NULLMAT;
  H->obj->pos = NULL; H->obj->pos_length = 0; 
  return OK;
}

int nsp_polyline3d_check_values(NspPolyline3d *H)
{
  if ( H->obj->Mcoord == NULLMAT) 
    {
       if (( H->obj->Mcoord = nsp_matrix_create("Mcoord",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->Mcolor == NULLMAT) 
    {
       if (( H->obj->Mcolor = nsp_matrix_create("Mcolor",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspPolyline3d *nsp_polyline3d_create(const char *name,NspMatrix* Mcoord,void* Mcoord_l,NspMatrix* Mcolor,int* pos, int pos_length,NspTypeBase *type)
{
  NspPolyline3d *H  = nsp_polyline3d_create_void(name,type);
  if ( H ==  NULLPOLYLINE3D) return NULLPOLYLINE3D;
  if ( nsp_polyline3d_create_partial(H) == FAIL) return NULLPOLYLINE3D;
  H->obj->Mcoord= Mcoord;
  H->obj->Mcoord_l = Mcoord_l;
  H->obj->Mcolor= Mcolor;
  H->obj->pos = pos;
  H->obj->pos_length = pos_length;
  if ( nsp_polyline3d_check_values(H) == FAIL) return NULLPOLYLINE3D;
#line 74 "codegen/polyline3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_polyline3d(H)== FAIL) return NULL; 

#line 508 "polyline3d.c"
  return H;
}


NspPolyline3d *nsp_polyline3d_create_default(const char *name)
{
 NspPolyline3d *H  = nsp_polyline3d_create_void(name,NULL);
 if ( H ==  NULLPOLYLINE3D) return NULLPOLYLINE3D;
  if ( nsp_polyline3d_create_partial(H) == FAIL) return NULLPOLYLINE3D;
 if ( nsp_polyline3d_check_values(H) == FAIL) return NULLPOLYLINE3D;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspPolyline3d *nsp_polyline3d_copy_partial(NspPolyline3d *H,NspPolyline3d *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspPolyline3d *nsp_polyline3d_copy(NspPolyline3d *self)
{
  NspPolyline3d *H  =nsp_polyline3d_create_void(NVOID,(NspTypeBase *) nsp_type_polyline3d);
  if ( H ==  NULLPOLYLINE3D) return NULLPOLYLINE3D;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOLYLINE3D;
  if ( nsp_polyline3d_copy_partial(H,self)== NULL) return NULLPOLYLINE3D;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspPolyline3d *nsp_polyline3d_full_copy_partial(NspPolyline3d *H,NspPolyline3d *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_polyline3d))) == NULL) return NULLPOLYLINE3D;
  H->obj->ref_count=1;
  if ( self->obj->Mcoord == NULL )
    { H->obj->Mcoord = NULL;}
  else
    {
      if ((H->obj->Mcoord = (NspMatrix *) nsp_object_full_copy_and_name("Mcoord",NSP_OBJECT(self->obj->Mcoord))) == NULLMAT) return NULL;
    }
  H->obj->Mcoord_l = self->obj->Mcoord_l;
  if ( self->obj->Mcolor == NULL )
    { H->obj->Mcolor = NULL;}
  else
    {
      if ((H->obj->Mcolor = (NspMatrix *) nsp_object_full_copy_and_name("Mcolor",NSP_OBJECT(self->obj->Mcolor))) == NULLMAT) return NULL;
    }
  if ((H->obj->pos = malloc(self->obj->pos_length*sizeof(int)))== NULL) return NULL;
  H->obj->pos_length = self->obj->pos_length;
  memcpy(H->obj->pos,self->obj->pos,self->obj->pos_length*sizeof(int));
  return H;
}

NspPolyline3d *nsp_polyline3d_full_copy(NspPolyline3d *self)
{
  NspPolyline3d *H  =nsp_polyline3d_create_void(NVOID,(NspTypeBase *) nsp_type_polyline3d);
  if ( H ==  NULLPOLYLINE3D) return NULLPOLYLINE3D;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOLYLINE3D;
  if ( nsp_polyline3d_full_copy_partial(H,self)== NULL) return NULLPOLYLINE3D;
#line 74 "codegen/polyline3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_polyline3d(H)== FAIL) return NULL; 

#line 578 "polyline3d.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspPolyline3d
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_polyline3d_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPolyline3d *H;
  CheckStdRhs(0,0);
  /* want to be sure that type polyline3d is initialized */
  nsp_type_polyline3d = new_type_polyline3d(T_BASE);
  if(( H = nsp_polyline3d_create_void(NVOID,(NspTypeBase *) nsp_type_polyline3d)) == NULLPOLYLINE3D) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_polyline3d_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_polyline3d_check_values(H) == FAIL) return RET_BUG;
#line 74 "codegen/polyline3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_polyline3d(H)== FAIL) return RET_BUG; 

#line 602 "polyline3d.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *polyline3d_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_polyline3d_get_Mcoord(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspPolyline3d *) self)->obj->Mcoord;
  return (NspObject *) ret;
}

static NspObject *_wrap_polyline3d_get_obj_Mcoord(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPolyline3d *) self)->obj->Mcoord);
  return (NspObject *) ret;
}

static int _wrap_polyline3d_set_Mcoord(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mcoord;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mcoord = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyline3d *) self)->obj->Mcoord != NULL ) 
    nsp_matrix_destroy(((NspPolyline3d *) self)->obj->Mcoord);
  ((NspPolyline3d *) self)->obj->Mcoord= Mcoord;
  return OK;
}

static NspObject *_wrap_polyline3d_get_Mcolor(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspPolyline3d *) self)->obj->Mcolor;
  return (NspObject *) ret;
}

static NspObject *_wrap_polyline3d_get_obj_Mcolor(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPolyline3d *) self)->obj->Mcolor);
  return (NspObject *) ret;
}

static int _wrap_polyline3d_set_Mcolor(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mcolor;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mcolor = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPolyline3d *) self)->obj->Mcolor != NULL ) 
    nsp_matrix_destroy(((NspPolyline3d *) self)->obj->Mcolor);
  ((NspPolyline3d *) self)->obj->Mcolor= Mcolor;
  return OK;
}

static AttrTab polyline3d_attrs[] = {
  { "Mcoord", (attr_get_function *)_wrap_polyline3d_get_Mcoord, (attr_set_function *)_wrap_polyline3d_set_Mcoord,(attr_get_object_function *)_wrap_polyline3d_get_obj_Mcoord, (attr_set_object_function *)int_set_object_failed },
  { "Mcolor", (attr_get_function *)_wrap_polyline3d_get_Mcolor, (attr_set_function *)_wrap_polyline3d_set_Mcolor,(attr_get_object_function *)_wrap_polyline3d_get_obj_Mcolor, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 84 "codegen/polyline3d.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_polyline3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 692 "polyline3d.c"


#line 94 "codegen/polyline3d.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_polyline3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 705 "polyline3d.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Polyline3d_func[]={
  {"extractelts_polyline3d", _wrap_nsp_extractelts_polyline3d},
  {"setrowscols_polyline3d", _wrap_nsp_setrowscols_polyline3d},
  { "polyline3d_create", int_polyline3d_create},
  { NULL, NULL}
};

/* call ith function in the Polyline3d interface */

int Polyline3d_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Polyline3d_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Polyline3d_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Polyline3d_func[i].name;
  *f = Polyline3d_func[i].fonc;
}

#line 105 "codegen/polyline3d.override"

/* inserted verbatim at the end */

static void nsp_draw_polyline3d(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  int face; 
  if ( Obj->obj->show == FALSE ) return ;
  /*
  if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
    {
      return ;
    }
  */

  if ( data != NULL &&  *((int *) data) < 0 ) 
    {
      nsp_check_polyline3d((NspPolyline3d *) Obj);
      return;
    }

#ifdef  WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      /* if we are using OpenGl we make a full draw of 
       * object and return 
       */
      draw_polyline3d_ogl(Xgc,Obj);
      nsp_ogl_set_2dview(Xgc); 
      return; 
    }
#endif 
  if ( data != NULL) 
    {
      face = *((int *) data);
      draw_polyline3d_face(Xgc,Obj,face);
    }
  else 
    {
      int i;
      /* draw all the faces: this is not really used  
       * since the face order is computed and sequenced in upper object.
       */
      for ( i= 0 ; i < ((NspPolyline3d *) Obj)->obj->Mcoord->m -1; i++) 
	draw_polyline3d_face(Xgc,Obj,i);
    }
}

static void nsp_translate_polyline3d(NspGraphic *Obj,const double *tr)
{
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_polyline3d(NspGraphic *Obj,double *R)
{
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_polyline3d(NspGraphic *Obj,double *alpha)
{
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of polyline3d 
 *
 */
extern void nsp_gr_bounds_min_max(int n,double *A,int incr,double *Amin, double *Amax) ;

static int nsp_getbounds_polyline3d(NspGraphic *Obj,double *bounds)
{
  int i;
  /* this should be stored in a cache and recomputed when necessary 
   *
   */
  nsp_polyline3d *Q= ((NspPolyline3d *) Obj)->obj;
  nsp_check_polyline3d((NspPolyline3d *) Obj);
  if ( Q->Mcoord->mn == 0) return FALSE;
  for ( i = 0 ; i < Q->Mcoord->n ; i++) 
    nsp_gr_bounds_min_max(Q->Mcoord->m,Q->Mcoord->R+i*Q->Mcoord->m,1,&bounds[2*i],&bounds[2*i+1]);
  return TRUE;
}


int nsp_check_polyline3d( NspPolyline3d *P)
{
  nsp_polyline3d *L = P->obj;
  int L_nb_coords = L->Mcoord->m;

  if ( L->Mcoord->n != 3 ) 
    {
      Scierror("Error: bad coord for polyline3d, second dimension should be 3\n");
      return FAIL;
    }

  if ( L->Mcolor->mn != L->Mcoord->m -1 && L->Mcolor->mn != 1 ) 
    {
      Scierror("Erro: bad color for polyline3d object\n");
    }

  L->Mcolor = Mat2int(L->Mcolor);

  /* create extra data for qpos declared int* 
   * Q->pos id only usefull for non opengl driver 
   */
  if ( L->pos == NULL) L->pos = malloc( L_nb_coords * sizeof(VisionPos));
  L->pos_length = L_nb_coords;
  
  /* create extra data for Mcoord_l declared void* */
  if ( L->Mcoord_l == NULL) 
    {
      L->Mcoord_l = nsp_matrix_create("local",'r',L->Mcoord->m, L->Mcoord->n);
    }
  return OK;
}

static void draw_polyline3d_face(BCG *Xgc,NspGraphic *Ob, int j)
{
  nsp_polyline3d *L = ((NspPolyline3d *) Ob)->obj;
  double * L_coord = ((NspMatrix *) L->Mcoord_l)->R;
  int L_nb_colors = L->Mcolor->mn ;
  int *L_color = L->Mcolor->I;
  int L_nb_coords = L->Mcoord->m;
  int x[2], y[2], color, n=2, flag=0;
  x[0] = XScale(Xgc->scales,L_coord[j]);
  y[0] = YScale(Xgc->scales,L_coord[j+L_nb_coords]);
  x[1] = XScale(Xgc->scales,L_coord[j+1]);
  y[1] = YScale(Xgc->scales,L_coord[j+1+L_nb_coords]);
  color = ( L_nb_colors == 1 ) ? L_color[0] : L_color[j];
  Xgc->graphic_engine->drawsegments(Xgc, x, y , n, &color, flag);
}

static void draw_polyline3d_ogl(BCG *Xgc,void *Ob)
{
#ifdef  WITH_GTKGLEXT 
  nsp_polyline3d *L = ((NspPolyline3d *) Ob)->obj;
  int j,color;
  double x[2], y[2],z[2];
  int  n=2, flag=0;
  double * L_coord = ((NspMatrix *) L->Mcoord)->R;
  int L_nb_coords = L->Mcoord->m;
  int *L_color = L->Mcolor->I;
  int L_nb_colors = L->Mcolor->mn ;
  for ( j = 0 ; j < L_nb_coords-1 ; j++ )
    {
      color = ( L_nb_colors == 1 ) ? L_color[0] : L_color[j];
      x[0] = L_coord[j];
      y[0] = L_coord[j+L_nb_coords];
      z[0] = L_coord[j+2*L_nb_coords];
      x[1] = L_coord[j+1];
      y[1] = L_coord[j+1+L_nb_coords];
      z[1] = L_coord[j+1+2*L_nb_coords];
      drawsegments3D(Xgc, x, y ,z, n, &color, flag);
    }
#endif
}

static void zmean_faces_for_Polyline3d(void *Obj, double z[], HFstruct HF[], int *n, int k)
{
  nsp_polyline3d *L = ((NspPolyline3d *) Obj)->obj;
  int j;
  double zmean;
  double * L_coord = ((NspMatrix *) L->Mcoord_l)->R;
  int L_nb_coords = L->Mcoord->m;
  for ( j = 0 ; j < L_nb_coords-1 ; j++ )
    {
      zmean = 0.5 * (L_coord[j+2*L_nb_coords] + L_coord[j+1+2*L_nb_coords]);
      if (L->pos[j] != OUT_Z && L->pos[j+1] != OUT_Z)
	if (L->pos[j] == VIN || L->pos[j+1] == VIN)
	  { 
	    /* le segment rentre dans les "facettes" à traiter */
	    z[*n] = zmean;
	    HF[*n].num_obj = k;
	    HF[*n].num_in_obj = j;
	    (*n)++; 
	  }
    }
}

/*
 * requested method for 3d objects.
 */

static void nsp_polyline3d_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim)
{
  nsp_polyline3d *Q= ((NspPolyline3d *) Obj)->obj;
  apply_transforms_new(Xgc,((NspMatrix *) Q->Mcoord_l)->R,Q->Mcoord->R,Q->pos, lim, Q->Mcoord->m);
  zmean_faces_for_Polyline3d(Obj, z,  HF, n, k);
}

/* requested method for 3d objects.
 *
 */

static int nsp_polyline3d_n_faces(BCG *Xgc,NspGraphic *Obj)
{
  return ((NspPolyline3d *) Obj)->obj->Mcoord->m -1;
}


#line 935 "polyline3d.c"
