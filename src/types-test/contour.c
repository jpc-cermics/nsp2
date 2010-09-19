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





#line 29 "codegen/contour.override"
#include <gdk/gdk.h>
#include <nsp/objects.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include <nsp/axes.h>
#include <nsp/contour.h>

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

#line 40 "contour.c"

/* ----------- NspContour ----------- */


#define  NspContour_Private 
#include <nsp/objects.h>
#include <nsp/contour.h>
#include <nsp/interf.h>

/* 
 * NspContour inherits from Graphic 
 */

int nsp_type_contour_id=0;
NspTypeContour *nsp_type_contour=NULL;

/*
 * Type object for NspContour 
 * all the instance of NspTypeContour share the same id. 
 * nsp_type_contour: is an instance of NspTypeContour 
 *    used for objects of NspContour type (i.e built with new_contour) 
 * other instances are used for derived classes 
 */
NspTypeContour *new_type_contour(type_mode mode)
{
  NspTypeContour *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_contour != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_contour;
    }
  if (( type =  malloc(sizeof(NspTypeContour))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = contour_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = contour_get_methods;
  type->new = (new_func *) new_contour;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for contour */ 

  top->pr = (print_func *) nsp_contour_print;
  top->dealloc = (dealloc_func *) nsp_contour_destroy;
  top->copy  =  (copy_func *) nsp_contour_copy;
  top->size  = (size_func *) nsp_contour_size;
  top->s_type =  (s_type_func *) nsp_contour_type_as_string;
  top->sh_type = (sh_type_func *) nsp_contour_type_short_string;
  top->info = (info_func *) nsp_contour_info;
  /* top->is_true = (is_true_func  *) nsp_contour_is_true; */
  /* top->loop =(loop_func *) nsp_contour_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_contour_object;
  top->eq  = (eq_func *) nsp_contour_eq;
  top->neq  = (eq_func *) nsp_contour_neq;
  top->save  = (save_func *) nsp_contour_xdr_save;
  top->load  = (load_func *) nsp_contour_xdr_load;
  top->create = (create_func*) int_contour_create;
  top->latex = (print_func *) nsp_contour_latex;
  top->full_copy = (copy_func *) nsp_contour_full_copy;

  /* specific methods for contour */

  type->init = (init_func *) init_contour;

#line 47 "codegen/contour.override"
  /* inserted verbatim in the type definition */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_contour;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_contour ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_contour  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_contour  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_contour  ;
  /* next method are defined in NspGraphic and need not be changed here for Contour */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 123 "contour.c"
  /* 
   * NspContour interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_contour_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeContour called nsp_type_contour
       */
      type->id =  nsp_type_contour_id = nsp_new_type_id();
      nsp_type_contour = type;
      if ( nsp_register_type(nsp_type_contour) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_contour(mode);
    }
  else 
    {
      type->id = nsp_type_contour_id;
      return type;
    }
}

/*
 * initialize NspContour instances 
 * locally and by calling initializer on parent class 
 */

static int init_contour(NspContour *Obj,NspTypeContour *type)
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
 * new instance of NspContour 
 */

NspContour *new_contour() 
{
  NspContour *loc;
  /* type must exists */
  nsp_type_contour = new_type_contour(T_BASE);
  if ( (loc = malloc(sizeof(NspContour)))== NULLCONTOUR) return loc;
  /* initialize object */
  if ( init_contour(loc,nsp_type_contour) == FAIL) return NULLCONTOUR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspContour 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_contour_size(NspContour *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char contour_type_name[]="Contour";
static char contour_short_type_name[]="contour";

static char *nsp_contour_type_as_string(void)
{
  return(contour_type_name);
}

static char *nsp_contour_type_short_string(NspObject *v)
{
  return(contour_short_type_name);
}

/*
 * A == B 
 */

static int nsp_contour_eq(NspContour *A, NspObject *B)
{
  NspContour *loc = (NspContour *) B;
  if ( check_cast(B,nsp_type_contour_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->z)->type->eq(A->obj->z,loc->obj->z) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->levels)->type->eq(A->obj->levels,loc->obj->levels) == FALSE ) return FALSE;
  if ( A->obj->nlevels != loc->obj->nlevels) return FALSE;
  if ( NSP_OBJECT(A->obj->style)->type->eq(A->obj->style,loc->obj->style) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_contour_neq(NspContour *A, NspObject *B)
{
  return ( nsp_contour_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_contour_xdr_save(XDR *xdrs, NspContour *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_contour)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->z)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->levels)) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->nlevels) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->style)) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspContour  *nsp_contour_xdr_load_partial(XDR *xdrs, NspContour *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ((M->obj->z =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->levels =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->nlevels) == FAIL) return NULL;
  if ((M->obj->style =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspContour  *nsp_contour_xdr_load(XDR *xdrs)
{
  NspContour *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCONTOUR;
  if ((H  = nsp_contour_create_void(name,(NspTypeBase *) nsp_type_contour))== NULLCONTOUR) return H;
  if ( nsp_contour_create_partial(H) == FAIL) return NULLCONTOUR;
  if ((H  = nsp_contour_xdr_load_partial(xdrs,H))== NULLCONTOUR) return H;
  if ( nsp_contour_check_values(H) == FAIL) return NULLCONTOUR;
  return H;
}

/*
 * delete 
 */

void nsp_contour_destroy_partial(NspContour *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->z != NULL ) 
      nsp_matrix_destroy(H->obj->z);
    if ( H->obj->x != NULL ) 
      nsp_matrix_destroy(H->obj->x);
    if ( H->obj->y != NULL ) 
      nsp_matrix_destroy(H->obj->y);
    if ( H->obj->levels != NULL ) 
      nsp_matrix_destroy(H->obj->levels);
    if ( H->obj->style != NULL ) 
      nsp_matrix_destroy(H->obj->style);
    FREE(H->obj);
   }
}

void nsp_contour_destroy(NspContour *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_contour_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_contour_info(NspContour *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCONTOUR) 
    {
      Sciprintf("Null Pointer NspContour \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_contour_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_contour_print(NspContour *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCONTOUR) 
    {
      Sciprintf("Null Pointer NspContour \n");
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
          nsp_contour_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_contour_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->z != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->z),indent+2,"z",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->levels != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->levels),indent+2,"levels",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"nlevels=%d\n",M->obj->nlevels);
  if ( M->obj->style != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->style),indent+2,"style",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_contour_latex(NspContour *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_contour_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->z != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->z),indent+2,"z",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->levels != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->levels),indent+2,"levels",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"nlevels=%d\n",M->obj->nlevels);
  if ( M->obj->style != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->style),indent+2,"style",rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspContour objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspContour   *nsp_contour_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_contour_id) == TRUE ) return ((NspContour *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_contour));
  return NULL;
}

int IsContourObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_contour_id);
}

int IsContour(NspObject *O)
{
  return nsp_object_type(O,nsp_type_contour_id);
}

NspContour  *GetContourCopy(Stack stack, int i)
{
  if (  GetContour(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspContour  *GetContour(Stack stack, int i)
{
  NspContour *M;
  if (( M = nsp_contour_object(NthObj(i))) == NULLCONTOUR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspContour instance 
 *-----------------------------------------------------*/

static NspContour *nsp_contour_create_void(const char *name,NspTypeBase *type)
{
 NspContour *H  = (type == NULL) ? new_contour() : type->new();
 if ( H ==  NULLCONTOUR)
  {
   Sciprintf("No more memory\n");
   return NULLCONTOUR;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCONTOUR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_contour_create_partial(NspContour *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_contour)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->z = NULLMAT;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->levels = NULLMAT;
  H->obj->nlevels = 0;
  H->obj->style = NULLMAT;
  return OK;
}

int nsp_contour_check_values(NspContour *H)
{
  if ( H->obj->z == NULLMAT) 
    {
       if (( H->obj->z = nsp_matrix_create("z",'r',0,0)) == NULLMAT)
       return FAIL;

    }
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
  if ( H->obj->levels == NULLMAT) 
    {
       if (( H->obj->levels = nsp_matrix_create("levels",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->style == NULLMAT) 
    {
       if (( H->obj->style = nsp_matrix_create("style",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspContour *nsp_contour_create(const char *name,NspMatrix* z,NspMatrix* x,NspMatrix* y,NspMatrix* levels,int nlevels,NspMatrix* style,NspTypeBase *type)
{
  NspContour *H  = nsp_contour_create_void(name,type);
  if ( H ==  NULLCONTOUR) return NULLCONTOUR;
  if ( nsp_contour_create_partial(H) == FAIL) return NULLCONTOUR;
  H->obj->z= z;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->levels= levels;
  H->obj->nlevels=nlevels;
  H->obj->style= style;
  if ( nsp_contour_check_values(H) == FAIL) return NULLCONTOUR;
  return H;
}


NspContour *nsp_contour_create_default(const char *name)
{
 NspContour *H  = nsp_contour_create_void(name,NULL);
 if ( H ==  NULLCONTOUR) return NULLCONTOUR;
  if ( nsp_contour_create_partial(H) == FAIL) return NULLCONTOUR;
 if ( nsp_contour_check_values(H) == FAIL) return NULLCONTOUR;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspContour *nsp_contour_copy_partial(NspContour *H,NspContour *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspContour *nsp_contour_copy(NspContour *self)
{
  NspContour *H  =nsp_contour_create_void(NVOID,(NspTypeBase *) nsp_type_contour);
  if ( H ==  NULLCONTOUR) return NULLCONTOUR;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCONTOUR;
  if ( nsp_contour_copy_partial(H,self)== NULL) return NULLCONTOUR;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspContour *nsp_contour_full_copy_partial(NspContour *H,NspContour *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_contour))) == NULL) return NULLCONTOUR;
  H->obj->ref_count=1;
  if ( self->obj->z == NULL )
    { H->obj->z = NULL;}
  else
    {
      if ((H->obj->z = (NspMatrix *) nsp_object_full_copy_and_name("z",NSP_OBJECT(self->obj->z))) == NULLMAT) return NULL;
    }
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
  if ( self->obj->levels == NULL )
    { H->obj->levels = NULL;}
  else
    {
      if ((H->obj->levels = (NspMatrix *) nsp_object_full_copy_and_name("levels",NSP_OBJECT(self->obj->levels))) == NULLMAT) return NULL;
    }
  H->obj->nlevels=self->obj->nlevels;
  if ( self->obj->style == NULL )
    { H->obj->style = NULL;}
  else
    {
      if ((H->obj->style = (NspMatrix *) nsp_object_full_copy_and_name("style",NSP_OBJECT(self->obj->style))) == NULLMAT) return NULL;
    }
  return H;
}

NspContour *nsp_contour_full_copy(NspContour *self)
{
  NspContour *H  =nsp_contour_create_void(NVOID,(NspTypeBase *) nsp_type_contour);
  if ( H ==  NULLCONTOUR) return NULLCONTOUR;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLCONTOUR;
  if ( nsp_contour_full_copy_partial(H,self)== NULL) return NULLCONTOUR;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspContour
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_contour_create(Stack stack, int rhs, int opt, int lhs)
{
  NspContour *H;
  CheckStdRhs(0,0);
  /* want to be sure that type contour is initialized */
  nsp_type_contour = new_type_contour(T_BASE);
  if(( H = nsp_contour_create_void(NVOID,(NspTypeBase *) nsp_type_contour)) == NULLCONTOUR) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_contour_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_contour_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *contour_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_contour_get_z(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->z;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_z(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->z);
  return (NspObject *) ret;
}

static int _wrap_contour_set_z(void *self,const char *attr, NspObject *O)
{
  NspMatrix *z;

  if ( ! IsMat(O) ) return FAIL;
  if ((z = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->z != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->z);
  ((NspContour *) self)->obj->z= z;
  return OK;
}

static NspObject *_wrap_contour_get_x(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_x(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_contour_set_x(void *self,const char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->x);
  ((NspContour *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_contour_get_y(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_y(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_contour_set_y(void *self,const char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->y);
  ((NspContour *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_contour_get_levels(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->levels;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_levels(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->levels);
  return (NspObject *) ret;
}

static int _wrap_contour_set_levels(void *self,const char *attr, NspObject *O)
{
  NspMatrix *levels;

  if ( ! IsMat(O) ) return FAIL;
  if ((levels = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->levels != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->levels);
  ((NspContour *) self)->obj->levels= levels;
  return OK;
}

static NspObject *_wrap_contour_get_nlevels(void *self,const char *attr)
{
  int ret;

  ret = ((NspContour *) self)->obj->nlevels;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_contour_set_nlevels(void *self,const char *attr, NspObject *O)
{
  int nlevels;

  if ( IntScalar(O,&nlevels) == FAIL) return FAIL;
  ((NspContour *) self)->obj->nlevels= nlevels;
  return OK;
}

static NspObject *_wrap_contour_get_style(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspContour *) self)->obj->style;
  return (NspObject *) ret;
}

static NspObject *_wrap_contour_get_obj_style(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspContour *) self)->obj->style);
  return (NspObject *) ret;
}

static int _wrap_contour_set_style(void *self,const char *attr, NspObject *O)
{
  NspMatrix *style;

  if ( ! IsMat(O) ) return FAIL;
  if ((style = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspContour *) self)->obj->style != NULL ) 
    nsp_matrix_destroy(((NspContour *) self)->obj->style);
  ((NspContour *) self)->obj->style= style;
  return OK;
}

static AttrTab contour_attrs[] = {
  { "z", (attr_get_function *)_wrap_contour_get_z, (attr_set_function *)_wrap_contour_set_z,(attr_get_object_function *)_wrap_contour_get_obj_z, (attr_set_object_function *)int_set_object_failed },
  { "x", (attr_get_function *)_wrap_contour_get_x, (attr_set_function *)_wrap_contour_set_x,(attr_get_object_function *)_wrap_contour_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_contour_get_y, (attr_set_function *)_wrap_contour_set_y,(attr_get_object_function *)_wrap_contour_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "levels", (attr_get_function *)_wrap_contour_get_levels, (attr_set_function *)_wrap_contour_set_levels,(attr_get_object_function *)_wrap_contour_get_obj_levels, (attr_set_object_function *)int_set_object_failed },
  { "nlevels", (attr_get_function *)_wrap_contour_get_nlevels, (attr_set_function *)_wrap_contour_set_nlevels,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "style", (attr_get_function *)_wrap_contour_get_style, (attr_set_function *)_wrap_contour_set_style,(attr_get_object_function *)_wrap_contour_get_obj_style, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 68 "codegen/contour.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_contour(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 838 "contour.c"


#line 78 "codegen/contour.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_contour(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 850 "contour.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Contour_func[]={
  {"extractelts_contour", _wrap_nsp_extractelts_contour},
  {"setrowscols_contour", _wrap_nsp_setrowscols_contour},
  { "contour_create", int_contour_create},
  { NULL, NULL}
};

/* call ith function in the Contour interface */

int Contour_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Contour_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Contour_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Contour_func[i].name;
  *f = Contour_func[i].fonc;
}

#line 88 "codegen/contour.override"

static void nsp_draw_contour(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data)
{
  NspContour *P = (NspContour *) Obj;
  double *x= P->obj->x->R; 
  double *y= P->obj->y->R; 
  double *z= P->obj->z->R; 
  int n1 = P->obj->x->mn;
  int n2 = P->obj->y->mn;
  
  if ( ((NspGraphic *) P)->obj->show == FALSE ) return;

  /* check if the block is inside drawing rectangle   */

  if ( ! nsp_graphic_intersect_rectangle(Obj, rect))
    {
      return ;
    }

  if ( P->obj->x->mn  == 0 || P->obj->y->mn  == 0 ) return;
  Mat2int(P->obj->style);
  nsp_contour2d_draw(Xgc,x,y,z,n1,n2,P->obj->nlevels,P->obj->levels->R,P->obj->style->I);
}

static void nsp_translate_contour(NspGraphic *Obj,const double *tr)
{
  NspContour *P = (NspContour *) Obj;
  int i;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i = 0 ; i < P->obj->x->mn ; i++) 
    P->obj->x->R[i] += tr[0];
  for ( i = 0 ; i < P->obj->y->mn ; i++) 
    P->obj->x->R[i] += tr[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_rotate_contour(NspGraphic *Obj,double *R)
{
  Sciprintf("we should get a double here for alpha\n");
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

static void nsp_scale_contour(NspGraphic *Obj,double *alpha)
{
  int i;
  NspContour *P = (NspContour *) Obj;
  nsp_graphic_invalidate((NspGraphic *) Obj);
  for ( i = 0 ; i < P->obj->x->mn ; i++) 
    P->obj->x->R[i] *= alpha[0];
  for ( i = 0 ; i < P->obj->y->mn ; i++) 
    P->obj->x->R[i] *= alpha[1];
  nsp_graphic_invalidate((NspGraphic *) Obj);
}

/* compute in bounds the enclosing rectangle of contour 
 *
 */

static int nsp_getbounds_contour (NspGraphic *Obj,double *bounds)
{
  NspContour *P = (NspContour *) Obj;
  /* get the bound in parent i.e given by wrect : upper-left w,h */
  if ( P->obj->x->mn == 0 || P->obj->y->mn ==  0) return FALSE;
  if ( P->obj->x->mn != 0 ) 
    {
      bounds[0]=P->obj->x->R[0]; /* xmin */
      bounds[2]=P->obj->x->R[P->obj->x->mn-1];/* xmax */
    }
  if ( P->obj->y->mn != 0 ) 
    {
      bounds[1]=P->obj->y->R[0] ; /* ymin */
      bounds[3]=P->obj->y->R[P->obj->y->mn-1];/* ymax */
    }
  return TRUE;
}



#line 960 "contour.c"
