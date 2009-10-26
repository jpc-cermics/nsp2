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





#line 32 "codegen/points3d.override"
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include <nsp/polyhedron.h>
#include <nsp/spolyhedron.h>
#include <nsp/grcommon.h>

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 


#line 40 "points3d.c"

/* ----------- NspPoints3d ----------- */


#define  NspPoints3d_Private 
#include <nsp/object.h>
#include <nsp/points3d.h>
#include <nsp/interf.h>

/* 
 * NspPoints3d inherits from Graphic 
 */

int nsp_type_points3d_id=0;
NspTypePoints3d *nsp_type_points3d=NULL;

/*
 * Type object for NspPoints3d 
 * all the instance of NspTypePoints3d share the same id. 
 * nsp_type_points3d: is an instance of NspTypePoints3d 
 *    used for objects of NspPoints3d type (i.e built with new_points3d) 
 * other instances are used for derived classes 
 */
NspTypePoints3d *new_type_points3d(type_mode mode)
{
  NspTypePoints3d *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_points3d != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_points3d;
    }
  if (( type =  malloc(sizeof(NspTypePoints3d))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = points3d_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = points3d_get_methods;
  type->new = (new_func *) new_points3d;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for points3d */ 

  top->pr = (print_func *) nsp_points3d_print;
  top->dealloc = (dealloc_func *) nsp_points3d_destroy;
  top->copy  =  (copy_func *) nsp_points3d_copy;
  top->size  = (size_func *) nsp_points3d_size;
  top->s_type =  (s_type_func *) nsp_points3d_type_as_string;
  top->sh_type = (sh_type_func *) nsp_points3d_type_short_string;
  top->info = (info_func *) nsp_points3d_info;
  /* top->is_true = (is_true_func  *) nsp_points3d_is_true; */
  /* top->loop =(loop_func *) nsp_points3d_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_points3d_object;
  top->eq  = (eq_func *) nsp_points3d_eq;
  top->neq  = (eq_func *) nsp_points3d_neq;
  top->save  = (save_func *) nsp_points3d_xdr_save;
  top->load  = (load_func *) nsp_points3d_xdr_load;
  top->create = (create_func*) int_points3d_create;
  top->latex = (print_func *) nsp_points3d_latex;
  top->full_copy = (copy_func *) nsp_points3d_full_copy;

  /* specific methods for points3d */

  type->init = (init_func *) init_points3d;

#line 50 "codegen/points3d.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_points3d;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_points3d ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_points3d  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_points3d  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_points3d  ;
  /* next method are defined in NspGraphic and need not be chnaged here for Points3d */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 
  ((NspTypeGraphic *) type->surtype)->zmean = nsp_points3d_zmean;
  ((NspTypeGraphic *) type->surtype)->n_faces = nsp_points3d_n_faces;

#line 127 "points3d.c"
  /* 
   * NspPoints3d interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_points3d_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePoints3d called nsp_type_points3d
       */
      type->id =  nsp_type_points3d_id = nsp_new_type_id();
      nsp_type_points3d = type;
      if ( nsp_register_type(nsp_type_points3d) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_points3d(mode);
    }
  else 
    {
      type->id = nsp_type_points3d_id;
      return type;
    }
}

/*
 * initialize NspPoints3d instances 
 * locally and by calling initializer on parent class 
 */

static int init_points3d(NspPoints3d *Obj,NspTypePoints3d *type)
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
 * new instance of NspPoints3d 
 */

NspPoints3d *new_points3d() 
{
  NspPoints3d *loc;
  /* type must exists */
  nsp_type_points3d = new_type_points3d(T_BASE);
  if ( (loc = malloc(sizeof(NspPoints3d)))== NULLPOINTS3D) return loc;
  /* initialize object */
  if ( init_points3d(loc,nsp_type_points3d) == FAIL) return NULLPOINTS3D;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspPoints3d 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_points3d_size(NspPoints3d *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char points3d_type_name[]="Points3d";
static char points3d_short_type_name[]="points3d";

static char *nsp_points3d_type_as_string(void)
{
  return(points3d_type_name);
}

static char *nsp_points3d_type_short_string(NspObject *v)
{
  return(points3d_short_type_name);
}

/*
 * A == B 
 */

static int nsp_points3d_eq(NspPoints3d *A, NspObject *B)
{
  NspPoints3d *loc = (NspPoints3d *) B;
  if ( check_cast(B,nsp_type_points3d_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->Mcoord)->type->eq(A->obj->Mcoord,loc->obj->Mcoord) == FALSE ) return FALSE;
  if ( A->obj->Mcoord_l != loc->obj->Mcoord_l) return FALSE;
  if ( A->obj->color != loc->obj->color) return FALSE;
  if ( A->obj->mark_type != loc->obj->mark_type) return FALSE;
  if ( A->obj->mark_size != loc->obj->mark_size) return FALSE;
  {int i;
    for ( i = 0 ; i < A->obj->pos_length ; i++)
      if ( A->obj->pos[i] != loc->obj->pos[i]) return FALSE;
  }
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_points3d_neq(NspPoints3d *A, NspObject *B)
{
  return ( nsp_points3d_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_points3d_xdr_save(XDR *xdrs, NspPoints3d *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_points3d)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcoord)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark_type) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mark_size) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspPoints3d  *nsp_points3d_xdr_load_partial(XDR *xdrs, NspPoints3d *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_points3d))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->Mcoord =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark_type) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mark_size) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspPoints3d  *nsp_points3d_xdr_load(XDR *xdrs)
{
  NspPoints3d *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPOINTS3D;
  if ((H  = nsp_points3d_create_void(name,(NspTypeBase *) nsp_type_points3d))== NULLPOINTS3D) return H;
  if ((H  = nsp_points3d_xdr_load_partial(xdrs,H))== NULLPOINTS3D) return H;
  if ( nsp_points3d_check_values(H) == FAIL) return NULLPOINTS3D;

#line 74 "codegen/points3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_points3d(H)== FAIL) return NULL; 

#line 298 "points3d.c"
  return H;
}

/*
 * delete 
 */

void nsp_points3d_destroy_partial(NspPoints3d *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 79 "codegen/points3d.override"
  /* verbatim in destroy */
  nsp_matrix_destroy(H->obj->Mcoord_l);


#line 317 "points3d.c"
    if ( H->obj->Mcoord != NULL ) 
      nsp_matrix_destroy(H->obj->Mcoord);
    FREE(H->obj->pos);
    FREE(H->obj);
   }
}

void nsp_points3d_destroy(NspPoints3d *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_points3d_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_points3d_info(NspPoints3d *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLPOINTS3D) 
    {
      Sciprintf("Null Pointer NspPoints3d \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_points3d_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_points3d_print(NspPoints3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLPOINTS3D) 
    {
      Sciprintf("Null Pointer NspPoints3d \n");
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
          nsp_points3d_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_points3d_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"mark_type=%d\n",M->obj->mark_type);
  Sciprintf1(indent+2,"mark_size=%d\n",M->obj->mark_size);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_points3d_latex(NspPoints3d *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_points3d_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  Sciprintf1(indent+2,"color=%d\n",M->obj->color);
  Sciprintf1(indent+2,"mark_type=%d\n",M->obj->mark_type);
  Sciprintf1(indent+2,"mark_size=%d\n",M->obj->mark_size);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPoints3d objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspPoints3d   *nsp_points3d_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_points3d_id) == TRUE ) return ((NspPoints3d *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_points3d));
  return NULL;
}

int IsPoints3dObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_points3d_id);
}

int IsPoints3d(NspObject *O)
{
  return nsp_object_type(O,nsp_type_points3d_id);
}

NspPoints3d  *GetPoints3dCopy(Stack stack, int i)
{
  if (  GetPoints3d(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPoints3d  *GetPoints3d(Stack stack, int i)
{
  NspPoints3d *M;
  if (( M = nsp_points3d_object(NthObj(i))) == NULLPOINTS3D)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspPoints3d instance 
 *-----------------------------------------------------*/

static NspPoints3d *nsp_points3d_create_void(char *name,NspTypeBase *type)
{
 NspPoints3d *H  = (type == NULL) ? new_points3d() : type->new();
 if ( H ==  NULLPOINTS3D)
  {
   Sciprintf("No more memory\n");
   return NULLPOINTS3D;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLPOINTS3D;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_points3d_create_partial(NspPoints3d *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_points3d)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->Mcoord = NULLMAT;
  H->obj->Mcoord_l = NULL;
  H->obj->color = -1;
  H->obj->mark_type = -1;
  H->obj->mark_size = -1;
  H->obj->pos = NULL; H->obj->pos_length = 0; 
  return OK;
}

int nsp_points3d_check_values(NspPoints3d *H)
{
  if ( H->obj->Mcoord == NULLMAT) 
    {
       if (( H->obj->Mcoord = nsp_matrix_create("Mcoord",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspPoints3d *nsp_points3d_create(char *name,NspMatrix* Mcoord,void* Mcoord_l,int color,int mark_type,int mark_size,int* pos, int pos_length,NspTypeBase *type)
{
 NspPoints3d *H  = nsp_points3d_create_void(name,type);
 if ( H ==  NULLPOINTS3D) return NULLPOINTS3D;
  if ( nsp_points3d_create_partial(H) == FAIL) return NULLPOINTS3D;
  H->obj->Mcoord= Mcoord;
  H->obj->Mcoord_l = Mcoord_l;
  H->obj->color=color;
  H->obj->mark_type=mark_type;
  H->obj->mark_size=mark_size;
  H->obj->pos = pos;
  H->obj->pos_length = pos_length;
 if ( nsp_points3d_check_values(H) == FAIL) return NULLPOINTS3D;
 return H;
}


NspPoints3d *nsp_points3d_create_default(char *name)
{
 NspPoints3d *H  = nsp_points3d_create_void(name,NULL);
 if ( H ==  NULLPOINTS3D) return NULLPOINTS3D;
  if ( nsp_points3d_create_partial(H) == FAIL) return NULLPOINTS3D;
 if ( nsp_points3d_check_values(H) == FAIL) return NULLPOINTS3D;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspPoints3d *nsp_points3d_copy_partial(NspPoints3d *H,NspPoints3d *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspPoints3d *nsp_points3d_copy(NspPoints3d *self)
{
  NspPoints3d *H  =nsp_points3d_create_void(NVOID,(NspTypeBase *) nsp_type_points3d);
  if ( H ==  NULLPOINTS3D) return NULLPOINTS3D;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOINTS3D;
  if ( nsp_points3d_copy_partial(H,self)== NULL) return NULLPOINTS3D;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspPoints3d *nsp_points3d_full_copy_partial(NspPoints3d *H,NspPoints3d *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_points3d))) == NULL) return NULLPOINTS3D;
  H->obj->ref_count=1;
  if ( self->obj->Mcoord == NULL )
    { H->obj->Mcoord = NULL;}
  else
    {
      if ((H->obj->Mcoord = (NspMatrix *) nsp_object_full_copy_and_name("Mcoord",NSP_OBJECT(self->obj->Mcoord))) == NULLMAT) return NULL;
    }
  H->obj->Mcoord_l = self->obj->Mcoord_l;
  H->obj->color=self->obj->color;
  H->obj->mark_type=self->obj->mark_type;
  H->obj->mark_size=self->obj->mark_size;
  if ((H->obj->pos = malloc(self->obj->pos_length*sizeof(int)))== NULL) return NULL;
  H->obj->pos_length = self->obj->pos_length;
  memcpy(H->obj->pos,self->obj->pos,self->obj->pos_length*sizeof(int));
  return H;
}

NspPoints3d *nsp_points3d_full_copy(NspPoints3d *self)
{
  NspPoints3d *H  =nsp_points3d_create_void(NVOID,(NspTypeBase *) nsp_type_points3d);
  if ( H ==  NULLPOINTS3D) return NULLPOINTS3D;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLPOINTS3D;
  if ( nsp_points3d_full_copy_partial(H,self)== NULL) return NULLPOINTS3D;

#line 74 "codegen/points3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_points3d(H)== FAIL) return NULL; 

#line 576 "points3d.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspPoints3d
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_points3d_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPoints3d *H;
  CheckStdRhs(0,0);
  /* want to be sure that type points3d is initialized */
  nsp_type_points3d = new_type_points3d(T_BASE);
  if(( H = nsp_points3d_create_void(NVOID,(NspTypeBase *) nsp_type_points3d)) == NULLPOINTS3D) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_points3d_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_points3d_check_values(H) == FAIL) return RET_BUG;

#line 74 "codegen/points3d.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_points3d(H)== FAIL) return RET_BUG; 

#line 601 "points3d.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *points3d_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_points3d_get_Mcoord(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspPoints3d *) self)->obj->Mcoord;
  return (NspObject *) ret;
}

static NspObject *_wrap_points3d_get_obj_Mcoord(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspPoints3d *) self)->obj->Mcoord);
  return (NspObject *) ret;
}

static int _wrap_points3d_set_Mcoord(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mcoord;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mcoord = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspPoints3d *) self)->obj->Mcoord != NULL ) 
    nsp_matrix_destroy(((NspPoints3d *) self)->obj->Mcoord);
  ((NspPoints3d *) self)->obj->Mcoord= Mcoord;
  return OK;
}

static NspObject *_wrap_points3d_get_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspPoints3d *) self)->obj->color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_points3d_set_color(void *self,const char *attr, NspObject *O)
{
  int color;

  if ( IntScalar(O,&color) == FAIL) return FAIL;
  ((NspPoints3d *) self)->obj->color= color;
  return OK;
}

static NspObject *_wrap_points3d_get_mark_type(void *self,const char *attr)
{
  int ret;

  ret = ((NspPoints3d *) self)->obj->mark_type;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_points3d_set_mark_type(void *self,const char *attr, NspObject *O)
{
  int mark_type;

  if ( IntScalar(O,&mark_type) == FAIL) return FAIL;
  ((NspPoints3d *) self)->obj->mark_type= mark_type;
  return OK;
}

static NspObject *_wrap_points3d_get_mark_size(void *self,const char *attr)
{
  int ret;

  ret = ((NspPoints3d *) self)->obj->mark_size;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_points3d_set_mark_size(void *self,const char *attr, NspObject *O)
{
  int mark_size;

  if ( IntScalar(O,&mark_size) == FAIL) return FAIL;
  ((NspPoints3d *) self)->obj->mark_size= mark_size;
  return OK;
}

static AttrTab points3d_attrs[] = {
  { "Mcoord", (attr_get_function *)_wrap_points3d_get_Mcoord, (attr_set_function *)_wrap_points3d_set_Mcoord,(attr_get_object_function *)_wrap_points3d_get_obj_Mcoord, (attr_set_object_function *)int_set_object_failed },
  { "color", (attr_get_function *)_wrap_points3d_get_color, (attr_set_function *)_wrap_points3d_set_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mark_type", (attr_get_function *)_wrap_points3d_get_mark_type, (attr_set_function *)_wrap_points3d_set_mark_type,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mark_size", (attr_get_function *)_wrap_points3d_get_mark_size, (attr_set_function *)_wrap_points3d_set_mark_size,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 85 "codegen/points3d.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_points3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 715 "points3d.c"


#line 95 "codegen/points3d.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_points3d(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 728 "points3d.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Points3d_func[]={
  {"extractelts_points3d", _wrap_nsp_extractelts_points3d},
  {"setrowscols_points3d", _wrap_nsp_setrowscols_points3d},
  { "points3d_create", int_points3d_create},
  { NULL, NULL}
};

/* call ith function in the Points3d interface */

int Points3d_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Points3d_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Points3d_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Points3d_func[i].name;
  *f = Points3d_func[i].fonc;
}

#line 106 "codegen/points3d.override"

/* inserted verbatim at the end */

static void nsp_draw_points3d(BCG *Xgc,NspGraphic *Obj, void *data)
{
  int face; 
  if ( Obj->obj->hidden == TRUE ) return ;
  nsp_check_points3d((NspPoints3d *) Obj);
#ifdef  WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      /* if we are using OpenGl we make a full draw of 
       * object and return 
       */
      draw_points3d_ogl(Xgc,Obj);
      nsp_ogl_set_2dview(Xgc); 
      return; 
    }
#endif 
  if ( data != NULL) 
    {
      face = *((int *) data);
      draw_points3d_face(Xgc,Obj,face);
    }
  else 
    {
      int i;
      /* draw all the faces: this is not really used  
       * since the face order is computed and sequenced in upper object.
       */
      for ( i= 0 ; i < ((NspPoints3d *) Obj)->obj->Mcoord->m; i++) 
	draw_points3d_face(Xgc,Obj,i);
    }
}

static void nsp_translate_points3d(BCG *Xgc,NspGraphic *Obj,double *tr)
{
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_points3d(BCG *Xgc,NspGraphic *Obj,double *R)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_points3d(BCG *Xgc,NspGraphic *Obj,double *alpha)
{
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of points3d 
 *
 */


static int nsp_getbounds_points3d(BCG *Xgc,NspGraphic *Obj,double *bounds)
{
  int i;
  /* this should be stored in a cache and recomputed when necessary 
   *
   */
  nsp_points3d *Q= ((NspPoints3d *) Obj)->obj;
  nsp_check_points3d((NspPoints3d *) Obj);
  if ( Q->Mcoord->mn == 0) return FALSE;
  for ( i = 0 ; i < Q->Mcoord->n ; i++) 
    nsp_gr_bounds_min_max(Q->Mcoord->m,Q->Mcoord->R+i*Q->Mcoord->m,1,
			  &bounds[2*i],&bounds[2*i+1]);
  return TRUE;
}

int nsp_check_points3d( NspPoints3d *Pt)
{
  nsp_points3d *P = Pt->obj;
  int P_nb_coords = P->Mcoord->m;

  if ( P->Mcoord->n != 3 ) 
    {
      Scierror("Error: bad coord for points3d, second dimension should be 3\n");
      return FAIL;
    }

  /* create extra data for qpos declared int* 
   * Q->pos id only usefull for non opengl driver 
   */
  if ( P->pos == NULL) P->pos = malloc( P_nb_coords * sizeof(VisionPos));
  P->pos_length = P_nb_coords;
  
  /* create extra data for Mcoord_l declared void* */
  if ( P->Mcoord_l == NULL) 
    {
      P->Mcoord_l = nsp_matrix_create("local",'r',P->Mcoord->m, P->Mcoord->n);
    }
  return OK;
}


static void draw_points3d_face(BCG *Xgc,NspGraphic *Ob, int j)
{
  int mark[2],current_mark[2],color;
  nsp_points3d *V = ((NspPoints3d *) Ob)->obj;
  double * V_coord = ((NspMatrix *) V->Mcoord_l)->R;
  int V_nb_coords = V->Mcoord->m;
  int x, y; 
  x = XScale(V_coord[j]);
  y = YScale(V_coord[j+V_nb_coords]);
  Xgc->graphic_engine->xget_mark(Xgc,current_mark);
  mark[0]= ( V->mark_type < 0 ) ? current_mark[0] : V->mark_type;
  mark[1]= ( V->mark_size < 0 ) ? current_mark[1] : V->mark_size;
  Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark[1]);
  if ( V->color != -1 ) 
    {
      color = Xgc->graphic_engine->xget_pattern(Xgc);
      Xgc->graphic_engine->xset_pattern(Xgc,  V->color ); 
    }
  Xgc->graphic_engine->drawpolymark(Xgc,&x,&y,1);
  Xgc->graphic_engine->xset_mark(Xgc,current_mark[0],current_mark[1]);
  if ( V->color != -1 ) 
    {
      Xgc->graphic_engine->xset_pattern(Xgc,color);
    }
}



static void draw_points3d_ogl(BCG *Xgc,void *Ob)
{
#ifdef  WITH_GTKGLEXT 
  int mark[2],current_mark[2],color;
  nsp_points3d *V = ((NspPoints3d *) Ob)->obj;
  double x, y,z;
  int j;
  int V_nb_coords = V->Mcoord->m;
  double * V_coord = ((NspMatrix *) V->Mcoord)->R;

  Xgc->graphic_engine->xget_mark(Xgc,current_mark);
  if ( V->color != -1 ) 
    {
      color = Xgc->graphic_engine->xget_pattern(Xgc);
      Xgc->graphic_engine->xset_pattern(Xgc,  V->color ); 
    }
  mark[0]= ( V->mark_type < 0 ) ? current_mark[0] : V->mark_type;
  mark[1]= ( V->mark_size < 0 ) ? current_mark[1] : V->mark_size;
  Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark[1]);
  for ( j = 0 ; j < V_nb_coords ; j++) 
    {
      x = V_coord[j];
      y = V_coord[j+V_nb_coords];
      z = V_coord[j+2*V_nb_coords];
      /* XXX  Xgc->graphic_engine->drawpolymark(Xgc,&x,&y,&z,n); */
    }
  Xgc->graphic_engine->xset_mark(Xgc,current_mark[0],current_mark[1]);
  if ( V->color != -1 ) 
    {
      Xgc->graphic_engine->xset_pattern(Xgc,color);
    }
#endif 
}

static void zmean_faces_for_Points3d(void *Obj, double z[], HFstruct HF[], int *n, int k)
{
  nsp_points3d *V = ((NspPoints3d *) Obj)->obj;
  int j;
  int V_nb_coords = V->Mcoord->m;
  double *V_coord = ((NspMatrix *) V->Mcoord_l)->R;
  for ( j = 0 ; j < V_nb_coords ; j++)
    if (V->pos[j] == VIN)
      {
	z[*n] = V_coord[j+2*V_nb_coords]; 
	HF[*n].num_obj = k; 
	HF[*n].num_in_obj = j;
	(*n)++; 
      }
}

/*
 * requested method for 3d objects.
 */

static void nsp_points3d_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim)
{
  nsp_points3d *Q= ((NspPoints3d *) Obj)->obj;
  apply_transforms_new(Xgc,((NspMatrix *) Q->Mcoord_l)->R,Q->Mcoord->R,Q->pos, lim, Q->Mcoord->m);
  zmean_faces_for_Points3d(Obj, z,  HF, n, k);
}

/* requested method for 3d objects.
 *
 */

static int nsp_points3d_n_faces(BCG *Xgc,NspGraphic *Obj)
{
  return ((NspPoints3d *) Obj)->obj->Mcoord->m;
}



#line 957 "points3d.c"
