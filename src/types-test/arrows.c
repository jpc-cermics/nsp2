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





#line 20 "codegen/arrows.override"
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include <nsp/arrows.h>

#line 33 "arrows.c"

/* ----------- NspArrows ----------- */


#define  NspArrows_Private 
#include <nsp/object.h>
#include <nsp/arrows.h>
#include <nsp/interf.h>

/* 
 * NspArrows inherits from Graphic 
 */

int nsp_type_arrows_id=0;
NspTypeArrows *nsp_type_arrows=NULL;

/*
 * Type object for NspArrows 
 * all the instance of NspTypeArrows share the same id. 
 * nsp_type_arrows: is an instance of NspTypeArrows 
 *    used for objects of NspArrows type (i.e built with new_arrows) 
 * other instances are used for derived classes 
 */
NspTypeArrows *new_type_arrows(type_mode mode)
{
  NspTypeArrows *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_arrows != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_arrows;
    }
  if (( type =  malloc(sizeof(NspTypeArrows))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = arrows_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = arrows_get_methods;
  type->new = (new_func *) new_arrows;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for arrows */ 

  top->pr = (print_func *) nsp_arrows_print;
  top->dealloc = (dealloc_func *) nsp_arrows_destroy;
  top->copy  =  (copy_func *) nsp_arrows_copy;
  top->size  = (size_func *) nsp_arrows_size;
  top->s_type =  (s_type_func *) nsp_arrows_type_as_string;
  top->sh_type = (sh_type_func *) nsp_arrows_type_short_string;
  top->info = (info_func *) nsp_arrows_info;
  /* top->is_true = (is_true_func  *) nsp_arrows_is_true; */
  /* top->loop =(loop_func *) nsp_arrows_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_arrows_object;
  top->eq  = (eq_func *) nsp_arrows_eq;
  top->neq  = (eq_func *) nsp_arrows_neq;
  top->save  = (save_func *) nsp_arrows_xdr_save;
  top->load  = (load_func *) nsp_arrows_xdr_load;
  top->create = (create_func*) int_arrows_create;
  top->latex = (print_func *) nsp_arrows_latex;
  top->full_copy = (copy_func *) nsp_arrows_full_copy;

  /* specific methods for arrows */

  type->init = (init_func *) init_arrows;

#line 31 "codegen/arrows.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_arrows;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_arrows ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_arrows  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_arrows  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_arrows  ;
  /* next method are defined in NspGraphic and need not be changed here for Arrows */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 

#line 118 "arrows.c"
  /* 
   * NspArrows interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_arrows_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeArrows called nsp_type_arrows
       */
      type->id =  nsp_type_arrows_id = nsp_new_type_id();
      nsp_type_arrows = type;
      if ( nsp_register_type(nsp_type_arrows) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_arrows(mode);
    }
  else 
    {
      type->id = nsp_type_arrows_id;
      return type;
    }
}

/*
 * initialize NspArrows instances 
 * locally and by calling initializer on parent class 
 */

static int init_arrows(NspArrows *Obj,NspTypeArrows *type)
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
 * new instance of NspArrows 
 */

NspArrows *new_arrows() 
{
  NspArrows *loc;
  /* type must exists */
  nsp_type_arrows = new_type_arrows(T_BASE);
  if ( (loc = malloc(sizeof(NspArrows)))== NULLARROWS) return loc;
  /* initialize object */
  if ( init_arrows(loc,nsp_type_arrows) == FAIL) return NULLARROWS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspArrows 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_arrows_size(NspArrows *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char arrows_type_name[]="Arrows";
static char arrows_short_type_name[]="arrows";

static char *nsp_arrows_type_as_string(void)
{
  return(arrows_type_name);
}

static char *nsp_arrows_type_short_string(NspObject *v)
{
  return(arrows_short_type_name);
}

/*
 * A == B 
 */

static int nsp_arrows_eq(NspArrows *A, NspObject *B)
{
  NspArrows *loc = (NspArrows *) B;
  if ( check_cast(B,nsp_type_arrows_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->x)->type->eq(A->obj->x,loc->obj->x) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->y)->type->eq(A->obj->y,loc->obj->y) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->color)->type->eq(A->obj->color,loc->obj->color) == FALSE ) return FALSE;
  if ( A->obj->arsize != loc->obj->arsize) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_arrows_neq(NspArrows *A, NspObject *B)
{
  return ( nsp_arrows_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_arrows_xdr_save(XDR *xdrs, NspArrows *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_arrows)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->x)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->y)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->color)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->arsize) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspArrows  *nsp_arrows_xdr_load_partial(XDR *xdrs, NspArrows *M)
{
  int fid;
  char name[NAME_MAXL];
  if ((M->obj = calloc(1,sizeof(nsp_arrows))) == NULL) return NULL;
  M->obj->ref_count=1;
  if ((M->obj->x =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->y =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->color =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->arsize) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspArrows  *nsp_arrows_xdr_load(XDR *xdrs)
{
  NspArrows *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLARROWS;
  if ((H  = nsp_arrows_create_void(name,(NspTypeBase *) nsp_type_arrows))== NULLARROWS) return H;
  if ((H  = nsp_arrows_xdr_load_partial(xdrs,H))== NULLARROWS) return H;
  if ( nsp_arrows_check_values(H) == FAIL) return NULLARROWS;
  return H;
}

/*
 * delete 
 */

void nsp_arrows_destroy_partial(NspArrows *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->x != NULL ) 
      nsp_matrix_destroy(H->obj->x);
    if ( H->obj->y != NULL ) 
      nsp_matrix_destroy(H->obj->y);
    if ( H->obj->color != NULL ) 
      nsp_matrix_destroy(H->obj->color);
    FREE(H->obj);
   }
}

void nsp_arrows_destroy(NspArrows *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_arrows_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_arrows_info(NspArrows *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLARROWS) 
    {
      Sciprintf("Null Pointer NspArrows \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_arrows_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_arrows_print(NspArrows *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLARROWS) 
    {
      Sciprintf("Null Pointer NspArrows \n");
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
          nsp_arrows_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_arrows_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->color != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->color),indent+2,"color",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"arsize=%f\n",M->obj->arsize);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_arrows_latex(NspArrows *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_arrows_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->x != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->x),indent+2,"x",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->y != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->y),indent+2,"y",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->color != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->color),indent+2,"color",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"arsize=%f\n",M->obj->arsize);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspArrows objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspArrows   *nsp_arrows_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_arrows_id) == TRUE ) return ((NspArrows *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_arrows));
  return NULL;
}

int IsArrowsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_arrows_id);
}

int IsArrows(NspObject *O)
{
  return nsp_object_type(O,nsp_type_arrows_id);
}

NspArrows  *GetArrowsCopy(Stack stack, int i)
{
  if (  GetArrows(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspArrows  *GetArrows(Stack stack, int i)
{
  NspArrows *M;
  if (( M = nsp_arrows_object(NthObj(i))) == NULLARROWS)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspArrows instance 
 *-----------------------------------------------------*/

static NspArrows *nsp_arrows_create_void(char *name,NspTypeBase *type)
{
 NspArrows *H  = (type == NULL) ? new_arrows() : type->new();
 if ( H ==  NULLARROWS)
  {
   Sciprintf("No more memory\n");
   return NULLARROWS;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLARROWS;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_arrows_create_partial(NspArrows *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_arrows)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->x = NULLMAT;
  H->obj->y = NULLMAT;
  H->obj->color = NULLMAT;
  H->obj->arsize = 0.0;
  return OK;
}

int nsp_arrows_check_values(NspArrows *H)
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
  if ( H->obj->color == NULLMAT) 
    {
       if (( H->obj->color = nsp_matrix_create("color",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspArrows *nsp_arrows_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* color,double arsize,NspTypeBase *type)
{
 NspArrows *H  = nsp_arrows_create_void(name,type);
 if ( H ==  NULLARROWS) return NULLARROWS;
  if ( nsp_arrows_create_partial(H) == FAIL) return NULLARROWS;
  H->obj->x= x;
  H->obj->y= y;
  H->obj->color= color;
  H->obj->arsize=arsize;
 if ( nsp_arrows_check_values(H) == FAIL) return NULLARROWS;
 return H;
}


NspArrows *nsp_arrows_create_default(char *name)
{
 NspArrows *H  = nsp_arrows_create_void(name,NULL);
 if ( H ==  NULLARROWS) return NULLARROWS;
  if ( nsp_arrows_create_partial(H) == FAIL) return NULLARROWS;
 if ( nsp_arrows_check_values(H) == FAIL) return NULLARROWS;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspArrows *nsp_arrows_copy_partial(NspArrows *H,NspArrows *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspArrows *nsp_arrows_copy(NspArrows *self)
{
  NspArrows *H  =nsp_arrows_create_void(NVOID,(NspTypeBase *) nsp_type_arrows);
  if ( H ==  NULLARROWS) return NULLARROWS;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLARROWS;
  if ( nsp_arrows_copy_partial(H,self)== NULL) return NULLARROWS;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspArrows *nsp_arrows_full_copy_partial(NspArrows *H,NspArrows *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_arrows))) == NULL) return NULLARROWS;
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
  if ( self->obj->color == NULL )
    { H->obj->color = NULL;}
  else
    {
      if ((H->obj->color = (NspMatrix *) nsp_object_full_copy_and_name("color",NSP_OBJECT(self->obj->color))) == NULLMAT) return NULL;
    }
  H->obj->arsize=self->obj->arsize;
  return H;
}

NspArrows *nsp_arrows_full_copy(NspArrows *self)
{
  NspArrows *H  =nsp_arrows_create_void(NVOID,(NspTypeBase *) nsp_type_arrows);
  if ( H ==  NULLARROWS) return NULLARROWS;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLARROWS;
  if ( nsp_arrows_full_copy_partial(H,self)== NULL) return NULLARROWS;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspArrows
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_arrows_create(Stack stack, int rhs, int opt, int lhs)
{
  NspArrows *H;
  CheckStdRhs(0,0);
  /* want to be sure that type arrows is initialized */
  nsp_type_arrows = new_type_arrows(T_BASE);
  if(( H = nsp_arrows_create_void(NVOID,(NspTypeBase *) nsp_type_arrows)) == NULLARROWS) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_arrows_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_arrows_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *arrows_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_arrows_get_x(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspArrows *) self)->obj->x;
  return (NspObject *) ret;
}

static NspObject *_wrap_arrows_get_obj_x(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspArrows *) self)->obj->x);
  return (NspObject *) ret;
}

static int _wrap_arrows_set_x(void *self,const char *attr, NspObject *O)
{
  NspMatrix *x;

  if ( ! IsMat(O) ) return FAIL;
  if ((x = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspArrows *) self)->obj->x != NULL ) 
    nsp_matrix_destroy(((NspArrows *) self)->obj->x);
  ((NspArrows *) self)->obj->x= x;
  return OK;
}

static NspObject *_wrap_arrows_get_y(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspArrows *) self)->obj->y;
  return (NspObject *) ret;
}

static NspObject *_wrap_arrows_get_obj_y(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspArrows *) self)->obj->y);
  return (NspObject *) ret;
}

static int _wrap_arrows_set_y(void *self,const char *attr, NspObject *O)
{
  NspMatrix *y;

  if ( ! IsMat(O) ) return FAIL;
  if ((y = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspArrows *) self)->obj->y != NULL ) 
    nsp_matrix_destroy(((NspArrows *) self)->obj->y);
  ((NspArrows *) self)->obj->y= y;
  return OK;
}

static NspObject *_wrap_arrows_get_color(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspArrows *) self)->obj->color;
  return (NspObject *) ret;
}

static NspObject *_wrap_arrows_get_obj_color(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspArrows *) self)->obj->color);
  return (NspObject *) ret;
}

static int _wrap_arrows_set_color(void *self,const char *attr, NspObject *O)
{
  NspMatrix *color;

  if ( ! IsMat(O) ) return FAIL;
  if ((color = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspArrows *) self)->obj->color != NULL ) 
    nsp_matrix_destroy(((NspArrows *) self)->obj->color);
  ((NspArrows *) self)->obj->color= color;
  return OK;
}

static NspObject *_wrap_arrows_get_arsize(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspArrows *) self)->obj->arsize;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_arrows_set_arsize(void *self,const char *attr, NspObject *O)
{
  double arsize;

  if ( DoubleScalar(O,&arsize) == FAIL) return FAIL;
  ((NspArrows *) self)->obj->arsize= arsize;
  return OK;
}

static AttrTab arrows_attrs[] = {
  { "x", (attr_get_function *)_wrap_arrows_get_x, (attr_set_function *)_wrap_arrows_set_x,(attr_get_object_function *)_wrap_arrows_get_obj_x, (attr_set_object_function *)int_set_object_failed },
  { "y", (attr_get_function *)_wrap_arrows_get_y, (attr_set_function *)_wrap_arrows_set_y,(attr_get_object_function *)_wrap_arrows_get_obj_y, (attr_set_object_function *)int_set_object_failed },
  { "color", (attr_get_function *)_wrap_arrows_get_color, (attr_set_function *)_wrap_arrows_set_color,(attr_get_object_function *)_wrap_arrows_get_obj_color, (attr_set_object_function *)int_set_object_failed },
  { "arsize", (attr_get_function *)_wrap_arrows_get_arsize, (attr_set_function *)_wrap_arrows_set_arsize,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 53 "codegen/arrows.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_arrows(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 725 "arrows.c"


#line 63 "codegen/arrows.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_arrows(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}


#line 738 "arrows.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Arrows_func[]={
  {"extractelts_arrows", _wrap_nsp_extractelts_arrows},
  {"setrowscols_arrows", _wrap_nsp_setrowscols_arrows},
  { "arrows_create", int_arrows_create},
  { NULL, NULL}
};

/* call ith function in the Arrows interface */

int Arrows_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Arrows_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Arrows_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Arrows_func[i].name;
  *f = Arrows_func[i].fonc;
}

#line 74 "codegen/arrows.override"

/* inserted verbatim at the end */

static void nsp_draw_arrows(BCG *Xgc,NspGraphic *Obj, void *data)
{
  int ccolor=-1;
  NspArrows *P = (NspArrows *) Obj;
  double arsize= P->obj->arsize ;
  NspMatrix *nx = P->obj->x;
  NspMatrix *ny = P->obj->y;
  if ( ((NspGraphic *) P)->obj->hidden == TRUE ) return;
  if ( P->obj->x->mn == 0 )  return;
  if ( P->obj->color != NULLMAT && P->obj->color->mn != 0 ) 
    {
      if ( P->obj->color->mn == 1) 
	{
	  ccolor = P->obj->color->I[0];
	  Xgc->graphic_engine->scale->drawarrows(Xgc,nx->R,ny->R,nx->mn,arsize,&ccolor,0);
	}
      else 
	{
	  Xgc->graphic_engine->scale->drawarrows(Xgc,nx->R,ny->R,nx->mn,arsize,P->obj->color->I,1);
	}
    }
  else 
    {
      Xgc->graphic_engine->scale->drawarrows(Xgc,nx->R,ny->R,nx->mn,arsize,&ccolor,0);
    }
}

static void nsp_translate_arrows(NspGraphic *Obj,const double *tr)
{
  int i; 
  NspArrows *P = (NspArrows *) Obj;
  double *x=P->obj->x->R,*y= P->obj->y->R;
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) += tr[0];
      *(y++) += tr[1];
    }
  nsp_figure_force_redraw(Obj->obj->Fig);

}

static void nsp_rotate_arrows(NspGraphic *Obj,double *R)
{
  int i;
  NspArrows *P = (NspArrows *) Obj;
  double *x= P->obj->x->R,*y= P->obj->y->R,x1,y1;
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      x1 = R[0]*(*x) -R[1]*(*y);
      y1 = R[1]*(*x) +R[0]*(*y);
      *(x++) =x1;
      *(y++) =y1;
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

static void nsp_scale_arrows(NspGraphic *Obj,double *alpha)
{
  int i;
  NspArrows *P = (NspArrows *) Obj;
  double *x= P->obj->x->R,*y= P->obj->y->R;
  for ( i=0; i < P->obj->x->mn ; i++) 
    {
      *(x++) *= alpha[0];
      *(y++) *= alpha[1];
    }
  nsp_figure_force_redraw(Obj->obj->Fig);
}

/* compute in bounds the enclosing rectangle of arrows 
 *
 */

static int nsp_getbounds_arrows(NspGraphic *Obj,double *bounds)
{
  int i;
  NspArrows *P = (NspArrows *) Obj;
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


#line 873 "arrows.c"
