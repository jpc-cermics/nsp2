/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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





#line 4 "codegen-3.0/cairo.override"

#include <cairo/cairo.h>
#include <cairo/cairo-gobject.h>
#include <cairo/cairo-pdf.h>
#include <cairo/cairo-ps.h>
#include <cairo/cairo-svg.h>
#include <nsp/nsp.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/hash.h>
#include <nsp/list.h>
#include <nsp/cells.h>
#include <nsp/none.h>
#include <nsp/matrix.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gobject-util.h>
#define  Nspcairo_t_Private
#include <nsp/gtk/cairo_t.h>
#define  Nspcairo_surface_t_Private
#include <nsp/gtk/cairo_surface_t.h>
#define  Nspcairo_pattern_t_Private
#include <nsp/gtk/cairo_pattern_t.h>
#define  Nspcairo_region_t_Private
#include <nsp/gtk/cairo_region_t.h>
#define  Nspcairo_matrix_t_Private
#include <nsp/gtk/cairo_matrix_t.h>
#define  Nspcairo_rectangle_t_Private
#include <nsp/gtk/cairo_rectangle_t.h>

#line 62 "cairo.c"
/* ---------- types from other modules ---------- */
#include <nsp/gtk/gobject.h>

/* -----------Nspcairo_t ----------- */


#define  NspCairo_t_Private 
#include <nsp/objects.h>
#include <nsp/gtk/cairo_t.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * Nspcairo_t inherits from GBoxed 
 */

int nsp_type_cairo_t_id=0;
NspTypeCairo_t *nsp_type_cairo_t=NULL;

/*
 * Type object for Nspcairo_t 
 * all the instance of NspTypeCairo_t share the same id. 
 * nsp_type_cairo_t: is an instance of NspTypeCairo_t 
 *    used for objects of Nspcairo_t type (i.e built with new_cairo_t) 
 * other instances are used for derived classes 
 */
NspTypeCairo_t *new_type_cairo_t(type_mode mode)
{
  NspTypeCairo_t *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_cairo_t != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cairo_t;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = cairo_t_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = cairo_t_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_cairo_t;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for cairo_t */ 

  top->s_type =  (s_type_func *) nsp_cairo_t_type_as_string;
  top->sh_type = (sh_type_func *) nsp_cairo_t_type_short_string;
  /* top->create = (create_func*) int_cairo_t_create;*/

  /* specific methods for cairo_t */

  type->init = (init_func *) init_cairo_t;

  /* 
   * Nspcairo_t interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_cairo_t_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCairo_t called nsp_type_cairo_t
       */
      type->id =  nsp_type_cairo_t_id = nsp_new_type_id();
      nsp_type_cairo_t = type;
      if ( nsp_register_type(nsp_type_cairo_t) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_cairo_t, CAIRO_GOBJECT_TYPE_CONTEXT);
      return ( mode == T_BASE ) ? type : new_type_cairo_t(mode);
    }
  else 
    {
      type->id = nsp_type_cairo_t_id;
      return type;
    }
}

/*
 * initialize Nspcairo_t instances 
 * locally and by calling initializer on parent class 
 */

static int init_cairo_t(Nspcairo_t *Obj,NspTypeCairo_t *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of Nspcairo_t 
 */

Nspcairo_t *new_cairo_t() 
{
  Nspcairo_t *loc;
  /* type must exists */
  nsp_type_cairo_t = new_type_cairo_t(T_BASE);
  if ( (loc = malloc(sizeof(Nspcairo_t)))== NULLCAIRO_T) return loc;
  /* initialize object */
  if ( init_cairo_t(loc,nsp_type_cairo_t) == FAIL) return NULLCAIRO_T;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Nspcairo_t 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char cairo_t_type_name[]="Cairo_t";
static char cairo_t_short_type_name[]="Cairo_t";

static char *nsp_cairo_t_type_as_string(void)
{
  return(cairo_t_type_name);
}

static char *nsp_cairo_t_type_short_string(NspObject *v)
{
  return(cairo_t_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Nspcairo_t objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

Nspcairo_t   *nsp_cairo_t_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_cairo_t_id)  == TRUE  ) return ((Nspcairo_t *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_cairo_t));
  return NULL;
}

int IsCairo_tObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_cairo_t_id);
}

int IsCairo_t(NspObject *O)
{
  return nsp_object_type(O,nsp_type_cairo_t_id);
}

Nspcairo_t  *GetCairo_tCopy(Stack stack, int i)
{
  if (  GetCairo_t(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

Nspcairo_t  *GetCairo_t(Stack stack, int i)
{
  Nspcairo_t *M;
  if (( M = nsp_cairo_t_object(NthObj(i))) == NULLCAIRO_T)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

Nspcairo_t *cairo_t_copy(Nspcairo_t *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_cairo_t);
}

/*-------------------------------------------------------------------
 * wrappers for the Cairo_t
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *cairo_t_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab cairo_t_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------Nspcairo_surface_t ----------- */


#define  NspCairo_surface_t_Private 
#include <nsp/objects.h>
#include <nsp/gtk/cairo_surface_t.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * Nspcairo_surface_t inherits from GBoxed 
 */

int nsp_type_cairo_surface_t_id=0;
NspTypeCairo_surface_t *nsp_type_cairo_surface_t=NULL;

/*
 * Type object for Nspcairo_surface_t 
 * all the instance of NspTypeCairo_surface_t share the same id. 
 * nsp_type_cairo_surface_t: is an instance of NspTypeCairo_surface_t 
 *    used for objects of Nspcairo_surface_t type (i.e built with new_cairo_surface_t) 
 * other instances are used for derived classes 
 */
NspTypeCairo_surface_t *new_type_cairo_surface_t(type_mode mode)
{
  NspTypeCairo_surface_t *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_cairo_surface_t != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cairo_surface_t;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = cairo_surface_t_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = cairo_surface_t_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_cairo_surface_t;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for cairo_surface_t */ 

  top->s_type =  (s_type_func *) nsp_cairo_surface_t_type_as_string;
  top->sh_type = (sh_type_func *) nsp_cairo_surface_t_type_short_string;
  /* top->create = (create_func*) int_cairo_surface_t_create;*/

  /* specific methods for cairo_surface_t */

  type->init = (init_func *) init_cairo_surface_t;

  /* 
   * Nspcairo_surface_t interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_cairo_surface_t_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCairo_surface_t called nsp_type_cairo_surface_t
       */
      type->id =  nsp_type_cairo_surface_t_id = nsp_new_type_id();
      nsp_type_cairo_surface_t = type;
      if ( nsp_register_type(nsp_type_cairo_surface_t) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_cairo_surface_t, CAIRO_GOBJECT_TYPE_SURFACE);
      return ( mode == T_BASE ) ? type : new_type_cairo_surface_t(mode);
    }
  else 
    {
      type->id = nsp_type_cairo_surface_t_id;
      return type;
    }
}

/*
 * initialize Nspcairo_surface_t instances 
 * locally and by calling initializer on parent class 
 */

static int init_cairo_surface_t(Nspcairo_surface_t *Obj,NspTypeCairo_surface_t *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of Nspcairo_surface_t 
 */

Nspcairo_surface_t *new_cairo_surface_t() 
{
  Nspcairo_surface_t *loc;
  /* type must exists */
  nsp_type_cairo_surface_t = new_type_cairo_surface_t(T_BASE);
  if ( (loc = malloc(sizeof(Nspcairo_surface_t)))== NULLCAIRO_SURFACE_T) return loc;
  /* initialize object */
  if ( init_cairo_surface_t(loc,nsp_type_cairo_surface_t) == FAIL) return NULLCAIRO_SURFACE_T;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Nspcairo_surface_t 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char cairo_surface_t_type_name[]="Cairo_surface_t";
static char cairo_surface_t_short_type_name[]="Cairo_surface_t";

static char *nsp_cairo_surface_t_type_as_string(void)
{
  return(cairo_surface_t_type_name);
}

static char *nsp_cairo_surface_t_type_short_string(NspObject *v)
{
  return(cairo_surface_t_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Nspcairo_surface_t objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

Nspcairo_surface_t   *nsp_cairo_surface_t_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_cairo_surface_t_id)  == TRUE  ) return ((Nspcairo_surface_t *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_cairo_surface_t));
  return NULL;
}

int IsCairo_surface_tObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_cairo_surface_t_id);
}

int IsCairo_surface_t(NspObject *O)
{
  return nsp_object_type(O,nsp_type_cairo_surface_t_id);
}

Nspcairo_surface_t  *GetCairo_surface_tCopy(Stack stack, int i)
{
  if (  GetCairo_surface_t(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

Nspcairo_surface_t  *GetCairo_surface_t(Stack stack, int i)
{
  Nspcairo_surface_t *M;
  if (( M = nsp_cairo_surface_t_object(NthObj(i))) == NULLCAIRO_SURFACE_T)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

Nspcairo_surface_t *cairo_surface_t_copy(Nspcairo_surface_t *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_cairo_surface_t);
}

/*-------------------------------------------------------------------
 * wrappers for the Cairo_surface_t
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *cairo_surface_t_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab cairo_surface_t_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------Nspcairo_pattern_t ----------- */


#define  NspCairo_pattern_t_Private 
#include <nsp/objects.h>
#include <nsp/gtk/cairo_pattern_t.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * Nspcairo_pattern_t inherits from GBoxed 
 */

int nsp_type_cairo_pattern_t_id=0;
NspTypeCairo_pattern_t *nsp_type_cairo_pattern_t=NULL;

/*
 * Type object for Nspcairo_pattern_t 
 * all the instance of NspTypeCairo_pattern_t share the same id. 
 * nsp_type_cairo_pattern_t: is an instance of NspTypeCairo_pattern_t 
 *    used for objects of Nspcairo_pattern_t type (i.e built with new_cairo_pattern_t) 
 * other instances are used for derived classes 
 */
NspTypeCairo_pattern_t *new_type_cairo_pattern_t(type_mode mode)
{
  NspTypeCairo_pattern_t *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_cairo_pattern_t != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cairo_pattern_t;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = cairo_pattern_t_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = cairo_pattern_t_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_cairo_pattern_t;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for cairo_pattern_t */ 

  top->s_type =  (s_type_func *) nsp_cairo_pattern_t_type_as_string;
  top->sh_type = (sh_type_func *) nsp_cairo_pattern_t_type_short_string;
  /* top->create = (create_func*) int_cairo_pattern_t_create;*/

  /* specific methods for cairo_pattern_t */

  type->init = (init_func *) init_cairo_pattern_t;

  /* 
   * Nspcairo_pattern_t interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_cairo_pattern_t_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCairo_pattern_t called nsp_type_cairo_pattern_t
       */
      type->id =  nsp_type_cairo_pattern_t_id = nsp_new_type_id();
      nsp_type_cairo_pattern_t = type;
      if ( nsp_register_type(nsp_type_cairo_pattern_t) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_cairo_pattern_t, CAIRO_GOBJECT_TYPE_PATTERN);
      return ( mode == T_BASE ) ? type : new_type_cairo_pattern_t(mode);
    }
  else 
    {
      type->id = nsp_type_cairo_pattern_t_id;
      return type;
    }
}

/*
 * initialize Nspcairo_pattern_t instances 
 * locally and by calling initializer on parent class 
 */

static int init_cairo_pattern_t(Nspcairo_pattern_t *Obj,NspTypeCairo_pattern_t *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of Nspcairo_pattern_t 
 */

Nspcairo_pattern_t *new_cairo_pattern_t() 
{
  Nspcairo_pattern_t *loc;
  /* type must exists */
  nsp_type_cairo_pattern_t = new_type_cairo_pattern_t(T_BASE);
  if ( (loc = malloc(sizeof(Nspcairo_pattern_t)))== NULLCAIRO_PATTERN_T) return loc;
  /* initialize object */
  if ( init_cairo_pattern_t(loc,nsp_type_cairo_pattern_t) == FAIL) return NULLCAIRO_PATTERN_T;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Nspcairo_pattern_t 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char cairo_pattern_t_type_name[]="Cairo_pattern_t";
static char cairo_pattern_t_short_type_name[]="Cairo_pattern_t";

static char *nsp_cairo_pattern_t_type_as_string(void)
{
  return(cairo_pattern_t_type_name);
}

static char *nsp_cairo_pattern_t_type_short_string(NspObject *v)
{
  return(cairo_pattern_t_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Nspcairo_pattern_t objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

Nspcairo_pattern_t   *nsp_cairo_pattern_t_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_cairo_pattern_t_id)  == TRUE  ) return ((Nspcairo_pattern_t *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_cairo_pattern_t));
  return NULL;
}

int IsCairo_pattern_tObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_cairo_pattern_t_id);
}

int IsCairo_pattern_t(NspObject *O)
{
  return nsp_object_type(O,nsp_type_cairo_pattern_t_id);
}

Nspcairo_pattern_t  *GetCairo_pattern_tCopy(Stack stack, int i)
{
  if (  GetCairo_pattern_t(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

Nspcairo_pattern_t  *GetCairo_pattern_t(Stack stack, int i)
{
  Nspcairo_pattern_t *M;
  if (( M = nsp_cairo_pattern_t_object(NthObj(i))) == NULLCAIRO_PATTERN_T)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

Nspcairo_pattern_t *cairo_pattern_t_copy(Nspcairo_pattern_t *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_cairo_pattern_t);
}

/*-------------------------------------------------------------------
 * wrappers for the Cairo_pattern_t
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *cairo_pattern_t_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab cairo_pattern_t_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------Nspcairo_region_t ----------- */


#define  NspCairo_region_t_Private 
#include <nsp/objects.h>
#include <nsp/gtk/cairo_region_t.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * Nspcairo_region_t inherits from GBoxed 
 */

int nsp_type_cairo_region_t_id=0;
NspTypeCairo_region_t *nsp_type_cairo_region_t=NULL;

/*
 * Type object for Nspcairo_region_t 
 * all the instance of NspTypeCairo_region_t share the same id. 
 * nsp_type_cairo_region_t: is an instance of NspTypeCairo_region_t 
 *    used for objects of Nspcairo_region_t type (i.e built with new_cairo_region_t) 
 * other instances are used for derived classes 
 */
NspTypeCairo_region_t *new_type_cairo_region_t(type_mode mode)
{
  NspTypeCairo_region_t *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_cairo_region_t != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cairo_region_t;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = cairo_region_t_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = cairo_region_t_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_cairo_region_t;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for cairo_region_t */ 

  top->s_type =  (s_type_func *) nsp_cairo_region_t_type_as_string;
  top->sh_type = (sh_type_func *) nsp_cairo_region_t_type_short_string;
  /* top->create = (create_func*) int_cairo_region_t_create;*/

  /* specific methods for cairo_region_t */

  type->init = (init_func *) init_cairo_region_t;

  /* 
   * Nspcairo_region_t interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_cairo_region_t_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCairo_region_t called nsp_type_cairo_region_t
       */
      type->id =  nsp_type_cairo_region_t_id = nsp_new_type_id();
      nsp_type_cairo_region_t = type;
      if ( nsp_register_type(nsp_type_cairo_region_t) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_cairo_region_t, CAIRO_GOBJECT_TYPE_REGION);
      return ( mode == T_BASE ) ? type : new_type_cairo_region_t(mode);
    }
  else 
    {
      type->id = nsp_type_cairo_region_t_id;
      return type;
    }
}

/*
 * initialize Nspcairo_region_t instances 
 * locally and by calling initializer on parent class 
 */

static int init_cairo_region_t(Nspcairo_region_t *Obj,NspTypeCairo_region_t *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of Nspcairo_region_t 
 */

Nspcairo_region_t *new_cairo_region_t() 
{
  Nspcairo_region_t *loc;
  /* type must exists */
  nsp_type_cairo_region_t = new_type_cairo_region_t(T_BASE);
  if ( (loc = malloc(sizeof(Nspcairo_region_t)))== NULLCAIRO_REGION_T) return loc;
  /* initialize object */
  if ( init_cairo_region_t(loc,nsp_type_cairo_region_t) == FAIL) return NULLCAIRO_REGION_T;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Nspcairo_region_t 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char cairo_region_t_type_name[]="Cairo_region_t";
static char cairo_region_t_short_type_name[]="Cairo_region_t";

static char *nsp_cairo_region_t_type_as_string(void)
{
  return(cairo_region_t_type_name);
}

static char *nsp_cairo_region_t_type_short_string(NspObject *v)
{
  return(cairo_region_t_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Nspcairo_region_t objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

Nspcairo_region_t   *nsp_cairo_region_t_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_cairo_region_t_id)  == TRUE  ) return ((Nspcairo_region_t *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_cairo_region_t));
  return NULL;
}

int IsCairo_region_tObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_cairo_region_t_id);
}

int IsCairo_region_t(NspObject *O)
{
  return nsp_object_type(O,nsp_type_cairo_region_t_id);
}

Nspcairo_region_t  *GetCairo_region_tCopy(Stack stack, int i)
{
  if (  GetCairo_region_t(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

Nspcairo_region_t  *GetCairo_region_t(Stack stack, int i)
{
  Nspcairo_region_t *M;
  if (( M = nsp_cairo_region_t_object(NthObj(i))) == NULLCAIRO_REGION_T)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

Nspcairo_region_t *cairo_region_t_copy(Nspcairo_region_t *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_cairo_region_t);
}

/*-------------------------------------------------------------------
 * wrappers for the Cairo_region_t
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *cairo_region_t_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab cairo_region_t_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------Nspcairo_rectangle_t ----------- */


#define  NspCairo_rectangle_t_Private 
#include <nsp/objects.h>
#include <nsp/gtk/cairo_rectangle_t.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * Nspcairo_rectangle_t inherits from GBoxed 
 */

int nsp_type_cairo_rectangle_t_id=0;
NspTypeCairo_rectangle_t *nsp_type_cairo_rectangle_t=NULL;

/*
 * Type object for Nspcairo_rectangle_t 
 * all the instance of NspTypeCairo_rectangle_t share the same id. 
 * nsp_type_cairo_rectangle_t: is an instance of NspTypeCairo_rectangle_t 
 *    used for objects of Nspcairo_rectangle_t type (i.e built with new_cairo_rectangle_t) 
 * other instances are used for derived classes 
 */
NspTypeCairo_rectangle_t *new_type_cairo_rectangle_t(type_mode mode)
{
  NspTypeCairo_rectangle_t *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_cairo_rectangle_t != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cairo_rectangle_t;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = cairo_rectangle_t_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = cairo_rectangle_t_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_cairo_rectangle_t;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for cairo_rectangle_t */ 

  top->s_type =  (s_type_func *) nsp_cairo_rectangle_t_type_as_string;
  top->sh_type = (sh_type_func *) nsp_cairo_rectangle_t_type_short_string;
  /* top->create = (create_func*) int_cairo_rectangle_t_create;*/

  /* specific methods for cairo_rectangle_t */

  type->init = (init_func *) init_cairo_rectangle_t;

  /* 
   * Nspcairo_rectangle_t interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_cairo_rectangle_t_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCairo_rectangle_t called nsp_type_cairo_rectangle_t
       */
      type->id =  nsp_type_cairo_rectangle_t_id = nsp_new_type_id();
      nsp_type_cairo_rectangle_t = type;
      if ( nsp_register_type(nsp_type_cairo_rectangle_t) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_cairo_rectangle_t, CAIRO_GOBJECT_TYPE_RECTANGLE);
      return ( mode == T_BASE ) ? type : new_type_cairo_rectangle_t(mode);
    }
  else 
    {
      type->id = nsp_type_cairo_rectangle_t_id;
      return type;
    }
}

/*
 * initialize Nspcairo_rectangle_t instances 
 * locally and by calling initializer on parent class 
 */

static int init_cairo_rectangle_t(Nspcairo_rectangle_t *Obj,NspTypeCairo_rectangle_t *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of Nspcairo_rectangle_t 
 */

Nspcairo_rectangle_t *new_cairo_rectangle_t() 
{
  Nspcairo_rectangle_t *loc;
  /* type must exists */
  nsp_type_cairo_rectangle_t = new_type_cairo_rectangle_t(T_BASE);
  if ( (loc = malloc(sizeof(Nspcairo_rectangle_t)))== NULLCAIRO_RECTANGLE_T) return loc;
  /* initialize object */
  if ( init_cairo_rectangle_t(loc,nsp_type_cairo_rectangle_t) == FAIL) return NULLCAIRO_RECTANGLE_T;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Nspcairo_rectangle_t 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char cairo_rectangle_t_type_name[]="Cairo_rectangle_t";
static char cairo_rectangle_t_short_type_name[]="Cairo_rectangle_t";

static char *nsp_cairo_rectangle_t_type_as_string(void)
{
  return(cairo_rectangle_t_type_name);
}

static char *nsp_cairo_rectangle_t_type_short_string(NspObject *v)
{
  return(cairo_rectangle_t_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Nspcairo_rectangle_t objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

Nspcairo_rectangle_t   *nsp_cairo_rectangle_t_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_cairo_rectangle_t_id)  == TRUE  ) return ((Nspcairo_rectangle_t *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_cairo_rectangle_t));
  return NULL;
}

int IsCairo_rectangle_tObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_cairo_rectangle_t_id);
}

int IsCairo_rectangle_t(NspObject *O)
{
  return nsp_object_type(O,nsp_type_cairo_rectangle_t_id);
}

Nspcairo_rectangle_t  *GetCairo_rectangle_tCopy(Stack stack, int i)
{
  if (  GetCairo_rectangle_t(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

Nspcairo_rectangle_t  *GetCairo_rectangle_t(Stack stack, int i)
{
  Nspcairo_rectangle_t *M;
  if (( M = nsp_cairo_rectangle_t_object(NthObj(i))) == NULLCAIRO_RECTANGLE_T)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

Nspcairo_rectangle_t *cairo_rectangle_t_copy(Nspcairo_rectangle_t *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_cairo_rectangle_t);
}

/*-------------------------------------------------------------------
 * wrappers for the Cairo_rectangle_t
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *cairo_rectangle_t_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab cairo_rectangle_t_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------Nspcairo_matrix_t ----------- */


#define  NspCairo_matrix_t_Private 
#include <nsp/objects.h>
#include <nsp/gtk/cairo_matrix_t.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * Nspcairo_matrix_t inherits from GBoxed 
 */

int nsp_type_cairo_matrix_t_id=0;
NspTypeCairo_matrix_t *nsp_type_cairo_matrix_t=NULL;

/*
 * Type object for Nspcairo_matrix_t 
 * all the instance of NspTypeCairo_matrix_t share the same id. 
 * nsp_type_cairo_matrix_t: is an instance of NspTypeCairo_matrix_t 
 *    used for objects of Nspcairo_matrix_t type (i.e built with new_cairo_matrix_t) 
 * other instances are used for derived classes 
 */
NspTypeCairo_matrix_t *new_type_cairo_matrix_t(type_mode mode)
{
  NspTypeCairo_matrix_t *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_cairo_matrix_t != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cairo_matrix_t;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = cairo_matrix_t_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = cairo_matrix_t_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_cairo_matrix_t;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for cairo_matrix_t */ 

  top->s_type =  (s_type_func *) nsp_cairo_matrix_t_type_as_string;
  top->sh_type = (sh_type_func *) nsp_cairo_matrix_t_type_short_string;
  /* top->create = (create_func*) int_cairo_matrix_t_create;*/

  /* specific methods for cairo_matrix_t */

  type->init = (init_func *) init_cairo_matrix_t;

  /* 
   * Nspcairo_matrix_t interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_cairo_matrix_t_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCairo_matrix_t called nsp_type_cairo_matrix_t
       */
      type->id =  nsp_type_cairo_matrix_t_id = nsp_new_type_id();
      nsp_type_cairo_matrix_t = type;
      if ( nsp_register_type(nsp_type_cairo_matrix_t) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_cairo_matrix_t, CAIRO_GOBJECT_TYPE_MATRIX);
      return ( mode == T_BASE ) ? type : new_type_cairo_matrix_t(mode);
    }
  else 
    {
      type->id = nsp_type_cairo_matrix_t_id;
      return type;
    }
}

/*
 * initialize Nspcairo_matrix_t instances 
 * locally and by calling initializer on parent class 
 */

static int init_cairo_matrix_t(Nspcairo_matrix_t *Obj,NspTypeCairo_matrix_t *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of Nspcairo_matrix_t 
 */

Nspcairo_matrix_t *new_cairo_matrix_t() 
{
  Nspcairo_matrix_t *loc;
  /* type must exists */
  nsp_type_cairo_matrix_t = new_type_cairo_matrix_t(T_BASE);
  if ( (loc = malloc(sizeof(Nspcairo_matrix_t)))== NULLCAIRO_MATRIX_T) return loc;
  /* initialize object */
  if ( init_cairo_matrix_t(loc,nsp_type_cairo_matrix_t) == FAIL) return NULLCAIRO_MATRIX_T;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Nspcairo_matrix_t 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char cairo_matrix_t_type_name[]="Cairo_matrix_t";
static char cairo_matrix_t_short_type_name[]="Cairo_matrix_t";

static char *nsp_cairo_matrix_t_type_as_string(void)
{
  return(cairo_matrix_t_type_name);
}

static char *nsp_cairo_matrix_t_type_short_string(NspObject *v)
{
  return(cairo_matrix_t_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Nspcairo_matrix_t objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

Nspcairo_matrix_t   *nsp_cairo_matrix_t_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_cairo_matrix_t_id)  == TRUE  ) return ((Nspcairo_matrix_t *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_cairo_matrix_t));
  return NULL;
}

int IsCairo_matrix_tObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_cairo_matrix_t_id);
}

int IsCairo_matrix_t(NspObject *O)
{
  return nsp_object_type(O,nsp_type_cairo_matrix_t_id);
}

Nspcairo_matrix_t  *GetCairo_matrix_tCopy(Stack stack, int i)
{
  if (  GetCairo_matrix_t(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

Nspcairo_matrix_t  *GetCairo_matrix_t(Stack stack, int i)
{
  Nspcairo_matrix_t *M;
  if (( M = nsp_cairo_matrix_t_object(NthObj(i))) == NULLCAIRO_MATRIX_T)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

Nspcairo_matrix_t *cairo_matrix_t_copy(Nspcairo_matrix_t *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_cairo_matrix_t);
}

/*-------------------------------------------------------------------
 * wrappers for the Cairo_matrix_t
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *cairo_matrix_t_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab cairo_matrix_t_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_cairo_version(Stack stack, int rhs, int opt, int lhs) /* cairo_version */
{
  int ret;
  CheckRhs(0,0);
    ret =cairo_version();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_version_string(Stack stack, int rhs, int opt, int lhs) /* cairo_version_string */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =cairo_version_string();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_create(Stack stack, int rhs, int opt, int lhs) /* cairo_create */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *target = NULL;
  NspObject *nsp_target = NULL, *nsp_ret;
  cairo_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_target) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_target, CAIRO_GOBJECT_TYPE_SURFACE))
      target = nspg_boxed_get(nsp_target, cairo_surface_t);
  else {
      Scierror( "Error: target should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_create(target);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_CONTEXT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_reference(Stack stack, int rhs, int opt, int lhs) /* cairo_reference */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL, *ret;
  NspObject *nsp_cr = NULL, *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_reference(cr);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_CONTEXT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_destroy(Stack stack, int rhs, int opt, int lhs) /* cairo_destroy */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_destroy(cr);
  return 0;
}

int _wrap_cairo_save(Stack stack, int rhs, int opt, int lhs) /* cairo_save */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_save(cr);
  return 0;
}

int _wrap_cairo_restore(Stack stack, int rhs, int opt, int lhs) /* cairo_restore */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_restore(cr);
  return 0;
}

int _wrap_cairo_push_group(Stack stack, int rhs, int opt, int lhs) /* cairo_push_group */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_push_group(cr);
  return 0;
}

int _wrap_cairo_push_group_with_content(Stack stack, int rhs, int opt, int lhs) /* cairo_push_group_with_content */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_content = NULL;
  cairo_content_t content;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_content) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_CONTENT, nsp_content, &content)== FAIL)
      return RET_BUG;
    cairo_push_group_with_content(cr,content);
  return 0;
}

int _wrap_cairo_pop_group(Stack stack, int rhs, int opt, int lhs) /* cairo_pop_group */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_ret;
  cairo_pattern_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_pop_group(cr);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pop_group_to_source(Stack stack, int rhs, int opt, int lhs) /* cairo_pop_group_to_source */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_pop_group_to_source(cr);
  return 0;
}

int _wrap_cairo_set_operator(Stack stack, int rhs, int opt, int lhs) /* cairo_set_operator */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_op = NULL;
  cairo_operator_t op;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_op) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_OPERATOR, nsp_op, &op)== FAIL)
      return RET_BUG;
    cairo_set_operator(cr,op);
  return 0;
}

int _wrap_cairo_set_source(Stack stack, int rhs, int opt, int lhs) /* cairo_set_source */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_source = NULL;
  cairo_pattern_t *source = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_source) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_source, CAIRO_GOBJECT_TYPE_PATTERN))
      source = nspg_boxed_get(nsp_source, cairo_pattern_t);
  else {
      Scierror( "Error: source should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_set_source(cr,source);
  return 0;
}

int _wrap_cairo_set_source_rgb(Stack stack, int rhs, int opt, int lhs) /* cairo_set_source_rgb */
{
  int_types T[] = {obj,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double red, green, blue;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &red, &green, &blue) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_set_source_rgb(cr,red,green,blue);
  return 0;
}

int _wrap_cairo_set_source_rgba(Stack stack, int rhs, int opt, int lhs) /* cairo_set_source_rgba */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double red, green, blue, alpha;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &red, &green, &blue, &alpha) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_set_source_rgba(cr,red,green,blue,alpha);
  return 0;
}

int _wrap_cairo_set_source_surface(Stack stack, int rhs, int opt, int lhs) /* cairo_set_source_surface */
{
  int_types T[] = {obj,obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_surface = NULL;
  cairo_surface_t *surface = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_surface, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_set_source_surface(cr,surface,x,y);
  return 0;
}

int _wrap_cairo_set_tolerance(Stack stack, int rhs, int opt, int lhs) /* cairo_set_tolerance */
{
  int_types T[] = {obj,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double tolerance;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &tolerance) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_set_tolerance(cr,tolerance);
  return 0;
}

int _wrap_cairo_set_antialias(Stack stack, int rhs, int opt, int lhs) /* cairo_set_antialias */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_antialias = NULL;
  cairo_antialias_t antialias;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_antialias) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_ANTIALIAS, nsp_antialias, &antialias)== FAIL)
      return RET_BUG;
    cairo_set_antialias(cr,antialias);
  return 0;
}

int _wrap_cairo_set_fill_rule(Stack stack, int rhs, int opt, int lhs) /* cairo_set_fill_rule */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_fill_rule = NULL;
  cairo_fill_rule_t fill_rule;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_fill_rule) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FILL_RULE, nsp_fill_rule, &fill_rule)== FAIL)
      return RET_BUG;
    cairo_set_fill_rule(cr,fill_rule);
  return 0;
}

int _wrap_cairo_set_line_width(Stack stack, int rhs, int opt, int lhs) /* cairo_set_line_width */
{
  int_types T[] = {obj,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double width;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &width) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_set_line_width(cr,width);
  return 0;
}

int _wrap_cairo_set_line_cap(Stack stack, int rhs, int opt, int lhs) /* cairo_set_line_cap */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_line_cap = NULL;
  cairo_line_cap_t line_cap;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_line_cap) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_LINE_CAP, nsp_line_cap, &line_cap)== FAIL)
      return RET_BUG;
    cairo_set_line_cap(cr,line_cap);
  return 0;
}

int _wrap_cairo_set_line_join(Stack stack, int rhs, int opt, int lhs) /* cairo_set_line_join */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_line_join = NULL;
  cairo_line_join_t line_join;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_line_join) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_LINE_JOIN, nsp_line_join, &line_join)== FAIL)
      return RET_BUG;
    cairo_set_line_join(cr,line_join);
  return 0;
}

int _wrap_cairo_set_miter_limit(Stack stack, int rhs, int opt, int lhs) /* cairo_set_miter_limit */
{
  int_types T[] = {obj,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double limit;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &limit) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_set_miter_limit(cr,limit);
  return 0;
}

int _wrap_cairo_translate(Stack stack, int rhs, int opt, int lhs) /* cairo_translate */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double tx, ty;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &tx, &ty) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_translate(cr,tx,ty);
  return 0;
}

int _wrap_cairo_scale(Stack stack, int rhs, int opt, int lhs) /* cairo_scale */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double sx, sy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &sx, &sy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_scale(cr,sx,sy);
  return 0;
}

int _wrap_cairo_rotate(Stack stack, int rhs, int opt, int lhs) /* cairo_rotate */
{
  int_types T[] = {obj,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double angle;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &angle) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_rotate(cr,angle);
  return 0;
}

int _wrap_cairo_transform(Stack stack, int rhs, int opt, int lhs) /* cairo_transform */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_matrix = NULL;
  cairo_matrix_t *matrix = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_transform(cr,matrix);
  return 0;
}

int _wrap_cairo_set_matrix(Stack stack, int rhs, int opt, int lhs) /* cairo_set_matrix */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_matrix = NULL;
  cairo_matrix_t *matrix = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_set_matrix(cr,matrix);
  return 0;
}

int _wrap_cairo_identity_matrix(Stack stack, int rhs, int opt, int lhs) /* cairo_identity_matrix */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_identity_matrix(cr);
  return 0;
}

int _wrap_cairo_user_to_device(Stack stack, int rhs, int opt, int lhs) /* cairo_user_to_device */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_user_to_device(cr,&x,&y);
  return 0;
}

int _wrap_cairo_user_to_device_distance(Stack stack, int rhs, int opt, int lhs) /* cairo_user_to_device_distance */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double dx, dy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &dx, &dy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_user_to_device_distance(cr,&dx,&dy);
  return 0;
}

int _wrap_cairo_device_to_user(Stack stack, int rhs, int opt, int lhs) /* cairo_device_to_user */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_device_to_user(cr,&x,&y);
  return 0;
}

int _wrap_cairo_device_to_user_distance(Stack stack, int rhs, int opt, int lhs) /* cairo_device_to_user_distance */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double dx, dy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &dx, &dy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_device_to_user_distance(cr,&dx,&dy);
  return 0;
}

int _wrap_cairo_new_path(Stack stack, int rhs, int opt, int lhs) /* cairo_new_path */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_new_path(cr);
  return 0;
}

int _wrap_cairo_move_to(Stack stack, int rhs, int opt, int lhs) /* cairo_move_to */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_move_to(cr,x,y);
  return 0;
}

int _wrap_cairo_new_sub_path(Stack stack, int rhs, int opt, int lhs) /* cairo_new_sub_path */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_new_sub_path(cr);
  return 0;
}

int _wrap_cairo_line_to(Stack stack, int rhs, int opt, int lhs) /* cairo_line_to */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_line_to(cr,x,y);
  return 0;
}

int _wrap_cairo_curve_to(Stack stack, int rhs, int opt, int lhs) /* cairo_curve_to */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x1, y1, x2, y2, x3, y3;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x1, &y1, &x2, &y2, &x3, &y3) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_curve_to(cr,x1,y1,x2,y2,x3,y3);
  return 0;
}

int _wrap_cairo_arc(Stack stack, int rhs, int opt, int lhs) /* cairo_arc */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double xc, yc, radius, angle1, angle2;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &xc, &yc, &radius, &angle1, &angle2) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_arc(cr,xc,yc,radius,angle1,angle2);
  return 0;
}

int _wrap_cairo_arc_negative(Stack stack, int rhs, int opt, int lhs) /* cairo_arc_negative */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double xc, yc, radius, angle1, angle2;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &xc, &yc, &radius, &angle1, &angle2) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_arc_negative(cr,xc,yc,radius,angle1,angle2);
  return 0;
}

int _wrap_cairo_rel_move_to(Stack stack, int rhs, int opt, int lhs) /* cairo_rel_move_to */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double dx, dy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &dx, &dy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_rel_move_to(cr,dx,dy);
  return 0;
}

int _wrap_cairo_rel_line_to(Stack stack, int rhs, int opt, int lhs) /* cairo_rel_line_to */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double dx, dy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &dx, &dy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_rel_line_to(cr,dx,dy);
  return 0;
}

int _wrap_cairo_rel_curve_to(Stack stack, int rhs, int opt, int lhs) /* cairo_rel_curve_to */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double dx1, dy1, dx2, dy2, dx3, dy3;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &dx1, &dy1, &dx2, &dy2, &dx3, &dy3) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_rel_curve_to(cr,dx1,dy1,dx2,dy2,dx3,dy3);
  return 0;
}

int _wrap_cairo_rectangle(Stack stack, int rhs, int opt, int lhs) /* cairo_rectangle */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x, y, width, height;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x, &y, &width, &height) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_rectangle(cr,x,y,width,height);
  return 0;
}

int _wrap_cairo_close_path(Stack stack, int rhs, int opt, int lhs) /* cairo_close_path */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_close_path(cr);
  return 0;
}

int _wrap_cairo_path_extents(Stack stack, int rhs, int opt, int lhs) /* cairo_path_extents */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x1, y1, x2, y2;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x1, &y1, &x2, &y2) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_path_extents(cr,&x1,&y1,&x2,&y2);
  return 0;
}

int _wrap_cairo_paint(Stack stack, int rhs, int opt, int lhs) /* cairo_paint */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_paint(cr);
  return 0;
}

int _wrap_cairo_paint_with_alpha(Stack stack, int rhs, int opt, int lhs) /* cairo_paint_with_alpha */
{
  int_types T[] = {obj,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double alpha;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &alpha) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_paint_with_alpha(cr,alpha);
  return 0;
}

int _wrap_cairo_mask(Stack stack, int rhs, int opt, int lhs) /* cairo_mask */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_pattern = NULL;
  cairo_pattern_t *pattern = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_mask(cr,pattern);
  return 0;
}

int _wrap_cairo_mask_surface(Stack stack, int rhs, int opt, int lhs) /* cairo_mask_surface */
{
  int_types T[] = {obj,obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_surface = NULL;
  cairo_surface_t *surface = NULL;
  double surface_x, surface_y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_surface, &surface_x, &surface_y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_mask_surface(cr,surface,surface_x,surface_y);
  return 0;
}

int _wrap_cairo_stroke(Stack stack, int rhs, int opt, int lhs) /* cairo_stroke */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_stroke(cr);
  return 0;
}

int _wrap_cairo_stroke_preserve(Stack stack, int rhs, int opt, int lhs) /* cairo_stroke_preserve */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_stroke_preserve(cr);
  return 0;
}

int _wrap_cairo_fill(Stack stack, int rhs, int opt, int lhs) /* cairo_fill */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_fill(cr);
  return 0;
}

int _wrap_cairo_fill_preserve(Stack stack, int rhs, int opt, int lhs) /* cairo_fill_preserve */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_fill_preserve(cr);
  return 0;
}

int _wrap_cairo_copy_page(Stack stack, int rhs, int opt, int lhs) /* cairo_copy_page */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_copy_page(cr);
  return 0;
}

int _wrap_cairo_show_page(Stack stack, int rhs, int opt, int lhs) /* cairo_show_page */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_show_page(cr);
  return 0;
}

int _wrap_cairo_stroke_extents(Stack stack, int rhs, int opt, int lhs) /* cairo_stroke_extents */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x1, y1, x2, y2;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x1, &y1, &x2, &y2) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_stroke_extents(cr,&x1,&y1,&x2,&y2);
  return 0;
}

int _wrap_cairo_fill_extents(Stack stack, int rhs, int opt, int lhs) /* cairo_fill_extents */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x1, y1, x2, y2;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x1, &y1, &x2, &y2) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_fill_extents(cr,&x1,&y1,&x2,&y2);
  return 0;
}

int _wrap_cairo_reset_clip(Stack stack, int rhs, int opt, int lhs) /* cairo_reset_clip */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_reset_clip(cr);
  return 0;
}

int _wrap_cairo_clip(Stack stack, int rhs, int opt, int lhs) /* cairo_clip */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_clip(cr);
  return 0;
}

int _wrap_cairo_clip_preserve(Stack stack, int rhs, int opt, int lhs) /* cairo_clip_preserve */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_clip_preserve(cr);
  return 0;
}

int _wrap_cairo_clip_extents(Stack stack, int rhs, int opt, int lhs) /* cairo_clip_extents */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x1, y1, x2, y2;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x1, &y1, &x2, &y2) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_clip_extents(cr,&x1,&y1,&x2,&y2);
  return 0;
}

int _wrap_cairo_select_font_face(Stack stack, int rhs, int opt, int lhs) /* cairo_select_font_face */
{
  int_types T[] = {obj,string,obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_slant = NULL, *nsp_weight = NULL;
  char *family;
  cairo_font_slant_t slant;
  cairo_font_weight_t weight;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &family, &nsp_slant, &nsp_weight) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FONT_SLANT, nsp_slant, &slant)== FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FONT_WEIGHT, nsp_weight, &weight)== FAIL)
      return RET_BUG;
    cairo_select_font_face(cr,family,slant,weight);
  return 0;
}

int _wrap_cairo_set_font_size(Stack stack, int rhs, int opt, int lhs) /* cairo_set_font_size */
{
  int_types T[] = {obj,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double size;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &size) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_set_font_size(cr,size);
  return 0;
}

int _wrap_cairo_set_font_matrix(Stack stack, int rhs, int opt, int lhs) /* cairo_set_font_matrix */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_matrix = NULL;
  cairo_matrix_t *matrix = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_set_font_matrix(cr,matrix);
  return 0;
}

int _wrap_cairo_get_font_matrix(Stack stack, int rhs, int opt, int lhs) /* cairo_get_font_matrix */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_matrix = NULL;
  cairo_matrix_t *matrix = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_get_font_matrix(cr,matrix);
  return 0;
}

int _wrap_cairo_show_text(Stack stack, int rhs, int opt, int lhs) /* cairo_show_text */
{
  int_types T[] = {obj,string, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  char *utf8;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &utf8) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_show_text(cr,utf8);
  return 0;
}

int _wrap_cairo_text_path(Stack stack, int rhs, int opt, int lhs) /* cairo_text_path */
{
  int_types T[] = {obj,string, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  char *utf8;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &utf8) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_text_path(cr,utf8);
  return 0;
}

int _wrap_cairo_get_operator(Stack stack, int rhs, int opt, int lhs) /* cairo_get_operator */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_operator(cr);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_source(Stack stack, int rhs, int opt, int lhs) /* cairo_get_source */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_ret;
  cairo_pattern_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_source(cr);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_get_tolerance(Stack stack, int rhs, int opt, int lhs) /* cairo_get_tolerance */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_tolerance(cr);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_antialias(Stack stack, int rhs, int opt, int lhs) /* cairo_get_antialias */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_antialias(cr);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_current_point(Stack stack, int rhs, int opt, int lhs) /* cairo_get_current_point */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_get_current_point(cr,&x,&y);
  return 0;
}

int _wrap_cairo_get_fill_rule(Stack stack, int rhs, int opt, int lhs) /* cairo_get_fill_rule */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_fill_rule(cr);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_line_width(Stack stack, int rhs, int opt, int lhs) /* cairo_get_line_width */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_line_width(cr);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_line_cap(Stack stack, int rhs, int opt, int lhs) /* cairo_get_line_cap */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_line_cap(cr);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_line_join(Stack stack, int rhs, int opt, int lhs) /* cairo_get_line_join */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_line_join(cr);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_miter_limit(Stack stack, int rhs, int opt, int lhs) /* cairo_get_miter_limit */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_miter_limit(cr);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_dash_count(Stack stack, int rhs, int opt, int lhs) /* cairo_get_dash_count */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_dash_count(cr);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_get_dash(Stack stack, int rhs, int opt, int lhs) /* cairo_get_dash */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  double dashes, offset;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &dashes, &offset) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    cairo_get_dash(cr,&dashes,&offset);
  return 0;
}

int _wrap_cairo_get_matrix(Stack stack, int rhs, int opt, int lhs) /* cairo_get_matrix */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_matrix = NULL;
  cairo_matrix_t *matrix = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_get_matrix(cr,matrix);
  return 0;
}

int _wrap_cairo_get_target(Stack stack, int rhs, int opt, int lhs) /* cairo_get_target */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_ret;
  cairo_surface_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_target(cr);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_get_group_target(Stack stack, int rhs, int opt, int lhs) /* cairo_get_group_target */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_ret;
  cairo_surface_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_get_group_target(cr);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_status(Stack stack, int rhs, int opt, int lhs) /* cairo_status */
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    ret =cairo_status(cr);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_status_to_string(Stack stack, int rhs, int opt, int lhs) /* cairo_status_to_string */
{
  int_types T[] = {obj, t_end};
  cairo_status_t status;
  NspObject *nsp_status = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_status) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_STATUS, nsp_status, &status)== FAIL)
      return RET_BUG;
    ret =cairo_status_to_string(status);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_surface_create_similar(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_create_similar */
{
  int_types T[] = {obj,obj,s_int,s_int, t_end};
  cairo_surface_t *other = NULL, *ret;
  NspObject *nsp_other = NULL, *nsp_content = NULL, *nsp_ret;
  cairo_content_t content;
  int width, height;
  if ( GetArgs(stack,rhs,opt,T,&nsp_other, &nsp_content, &width, &height) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_other, CAIRO_GOBJECT_TYPE_SURFACE))
      other = nspg_boxed_get(nsp_other, cairo_surface_t);
  else {
      Scierror( "Error: other should be a cairo_surface_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_CONTENT, nsp_content, &content)== FAIL)
      return RET_BUG;
    ret =cairo_surface_create_similar(other,content,width,height);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_surface_create_similar_image(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_create_similar_image */
{
  int_types T[] = {obj,obj,s_int,s_int, t_end};
  cairo_surface_t *other = NULL, *ret;
  NspObject *nsp_other = NULL, *nsp_format = NULL, *nsp_ret;
  cairo_format_t format;
  int width, height;
  if ( GetArgs(stack,rhs,opt,T,&nsp_other, &nsp_format, &width, &height) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_other, CAIRO_GOBJECT_TYPE_SURFACE))
      other = nspg_boxed_get(nsp_other, cairo_surface_t);
  else {
      Scierror( "Error: other should be a cairo_surface_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FORMAT, nsp_format, &format)== FAIL)
      return RET_BUG;
    ret =cairo_surface_create_similar_image(other,format,width,height);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_surface_unmap_image(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_unmap_image */
{
  int_types T[] = {obj,obj, t_end};
  cairo_surface_t *surface = NULL, *image = NULL;
  NspObject *nsp_surface = NULL, *nsp_image = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &nsp_image) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_image, CAIRO_GOBJECT_TYPE_SURFACE))
      image = nspg_boxed_get(nsp_image, cairo_surface_t);
  else {
      Scierror( "Error: image should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_unmap_image(surface,image);
  return 0;
}

int _wrap_cairo_surface_create_for_rectangle(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_create_for_rectangle */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_surface_t *target = NULL, *ret;
  NspObject *nsp_target = NULL, *nsp_ret;
  double x, y, width, height;
  if ( GetArgs(stack,rhs,opt,T,&nsp_target, &x, &y, &width, &height) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_target, CAIRO_GOBJECT_TYPE_SURFACE))
      target = nspg_boxed_get(nsp_target, cairo_surface_t);
  else {
      Scierror( "Error: target should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_surface_create_for_rectangle(target,x,y,width,height);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_surface_observer_elapsed(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_observer_elapsed */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_surface_observer_elapsed(surface);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_surface_reference(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_reference */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL, *ret;
  NspObject *nsp_surface = NULL, *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_surface_reference(surface);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_surface_finish(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_finish */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_finish(surface);
  return 0;
}

int _wrap_cairo_surface_destroy(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_destroy */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_destroy(surface);
  return 0;
}

int _wrap_cairo_surface_status(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_status */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_surface_status(surface);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_surface_get_content(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_get_content */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_surface_get_content(surface);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_surface_write_to_png(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_write_to_png */
{
  int_types T[] = {obj,string, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  char *filename;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &filename) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_surface_write_to_png(surface,filename);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_surface_flush(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_flush */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_flush(surface);
  return 0;
}

int _wrap_cairo_surface_mark_dirty(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_mark_dirty */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_mark_dirty(surface);
  return 0;
}

int _wrap_cairo_surface_mark_dirty_rectangle(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_mark_dirty_rectangle */
{
  int_types T[] = {obj,s_int,s_int,s_int,s_int, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  int x, y, width, height;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &x, &y, &width, &height) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_mark_dirty_rectangle(surface,x,y,width,height);
  return 0;
}

int _wrap_cairo_surface_set_device_scale(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_set_device_scale */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double x_scale, y_scale;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &x_scale, &y_scale) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_set_device_scale(surface,x_scale,y_scale);
  return 0;
}

int _wrap_cairo_surface_get_device_scale(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_get_device_scale */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double x_scale, y_scale;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &x_scale, &y_scale) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_get_device_scale(surface,&x_scale,&y_scale);
  return 0;
}

int _wrap_cairo_surface_set_device_offset(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_set_device_offset */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double x_offset, y_offset;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &x_offset, &y_offset) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_set_device_offset(surface,x_offset,y_offset);
  return 0;
}

int _wrap_cairo_surface_get_device_offset(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_get_device_offset */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double x_offset, y_offset;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &x_offset, &y_offset) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_get_device_offset(surface,&x_offset,&y_offset);
  return 0;
}

int _wrap_cairo_surface_set_fallback_resolution(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_set_fallback_resolution */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double x_pixels_per_inch, y_pixels_per_inch;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &x_pixels_per_inch, &y_pixels_per_inch) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_set_fallback_resolution(surface,x_pixels_per_inch,y_pixels_per_inch);
  return 0;
}

int _wrap_cairo_surface_get_fallback_resolution(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_get_fallback_resolution */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double x_pixels_per_inch, y_pixels_per_inch;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &x_pixels_per_inch, &y_pixels_per_inch) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_get_fallback_resolution(surface,&x_pixels_per_inch,&y_pixels_per_inch);
  return 0;
}

int _wrap_cairo_surface_copy_page(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_copy_page */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_copy_page(surface);
  return 0;
}

int _wrap_cairo_surface_show_page(Stack stack, int rhs, int opt, int lhs) /* cairo_surface_show_page */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_surface_show_page(surface);
  return 0;
}

int _wrap_cairo_image_surface_create(Stack stack, int rhs, int opt, int lhs) /* cairo_image_surface_create */
{
  int_types T[] = {obj,s_int,s_int, t_end};
  cairo_format_t format;
  NspObject *nsp_format = NULL, *nsp_ret;
  int width, height;
  cairo_surface_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_format, &width, &height) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FORMAT, nsp_format, &format)== FAIL)
      return RET_BUG;
    ret =cairo_image_surface_create(format,width,height);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_format_stride_for_width(Stack stack, int rhs, int opt, int lhs) /* cairo_format_stride_for_width */
{
  int_types T[] = {obj,s_int, t_end};
  cairo_format_t format;
  NspObject *nsp_format = NULL;
  int width, ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_format, &width) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FORMAT, nsp_format, &format)== FAIL)
      return RET_BUG;
    ret =cairo_format_stride_for_width(format,width);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_image_surface_create_for_data(Stack stack, int rhs, int opt, int lhs) /* cairo_image_surface_create_for_data */
{
  int_types T[] = {string,obj,s_int,s_int,s_int, t_end};
  guchar *data;
  cairo_format_t format;
  NspObject *nsp_format = NULL, *nsp_ret;
  int width, height, stride;
  cairo_surface_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&data, &nsp_format, &width, &height, &stride) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FORMAT, nsp_format, &format)== FAIL)
      return RET_BUG;
    ret =cairo_image_surface_create_for_data(data,format,width,height,stride);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_image_surface_get_format(Stack stack, int rhs, int opt, int lhs) /* cairo_image_surface_get_format */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_image_surface_get_format(surface);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_image_surface_get_width(Stack stack, int rhs, int opt, int lhs) /* cairo_image_surface_get_width */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_image_surface_get_width(surface);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_image_surface_get_height(Stack stack, int rhs, int opt, int lhs) /* cairo_image_surface_get_height */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_image_surface_get_height(surface);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_image_surface_get_stride(Stack stack, int rhs, int opt, int lhs) /* cairo_image_surface_get_stride */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_image_surface_get_stride(surface);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_image_surface_create_from_png(Stack stack, int rhs, int opt, int lhs) /* cairo_image_surface_create_from_png */
{
  int_types T[] = {string, t_end};
  char *filename;
  cairo_surface_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
    ret =cairo_image_surface_create_from_png(filename);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_recording_surface_create(Stack stack, int rhs, int opt, int lhs) /* cairo_recording_surface_create */
{
  int_types T[] = {obj,obj, t_end};
  cairo_content_t content;
  NspObject *nsp_content = NULL, *nsp_extents = NULL, *nsp_ret;
  cairo_rectangle_t *extents = NULL;
  cairo_surface_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_content, &nsp_extents) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_CONTENT, nsp_content, &content)== FAIL)
      return RET_BUG;
  if (nspg_boxed_check(nsp_extents, CAIRO_GOBJECT_TYPE_RECTANGLE))
      extents = nspg_boxed_get(nsp_extents, cairo_rectangle_t);
  else {
      Scierror( "Error: extents should be a cairo_rectangle_t\n");
      return RET_BUG;
  }
    ret =cairo_recording_surface_create(content,extents);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_recording_surface_ink_extents(Stack stack, int rhs, int opt, int lhs) /* cairo_recording_surface_ink_extents */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double x0, y0, width, height;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &x0, &y0, &width, &height) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_recording_surface_ink_extents(surface,&x0,&y0,&width,&height);
  return 0;
}

int _wrap_cairo_pattern_create_rgb(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_create_rgb */
{
  int_types T[] = {s_double,s_double,s_double, t_end};
  double red, green, blue;
  cairo_pattern_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&red, &green, &blue) == FAIL) return RET_BUG;
    ret =cairo_pattern_create_rgb(red,green,blue);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pattern_create_rgba(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_create_rgba */
{
  int_types T[] = {s_double,s_double,s_double,s_double, t_end};
  double red, green, blue, alpha;
  cairo_pattern_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&red, &green, &blue, &alpha) == FAIL) return RET_BUG;
    ret =cairo_pattern_create_rgba(red,green,blue,alpha);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pattern_create_for_surface(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_create_for_surface */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL, *nsp_ret;
  cairo_pattern_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_create_for_surface(surface);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pattern_create_linear(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_create_linear */
{
  int_types T[] = {s_double,s_double,s_double,s_double, t_end};
  double x0, y0, x1, y1;
  cairo_pattern_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&x0, &y0, &x1, &y1) == FAIL) return RET_BUG;
    ret =cairo_pattern_create_linear(x0,y0,x1,y1);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pattern_create_radial(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_create_radial */
{
  int_types T[] = {s_double,s_double,s_double,s_double,s_double,s_double, t_end};
  double cx0, cy0, radius0, cx1, cy1, radius1;
  cairo_pattern_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&cx0, &cy0, &radius0, &cx1, &cy1, &radius1) == FAIL) return RET_BUG;
    ret =cairo_pattern_create_radial(cx0,cy0,radius0,cx1,cy1,radius1);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pattern_create_mesh(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_create_mesh */
{
  cairo_pattern_t *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =cairo_pattern_create_mesh();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pattern_reference(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_reference */
{
  int_types T[] = {obj, t_end};
  cairo_pattern_t *pattern = NULL, *ret;
  NspObject *nsp_pattern = NULL, *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_reference(pattern);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pattern_destroy(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_destroy */
{
  int_types T[] = {obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_pattern_destroy(pattern);
  return 0;
}

int _wrap_cairo_pattern_status(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_status */
{
  int_types T[] = {obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_status(pattern);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_pattern_add_color_stop_rgb(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_add_color_stop_rgb */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  double offset, red, green, blue;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &offset, &red, &green, &blue) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_pattern_add_color_stop_rgb(pattern,offset,red,green,blue);
  return 0;
}

int _wrap_cairo_pattern_add_color_stop_rgba(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_add_color_stop_rgba */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  double offset, red, green, blue, alpha;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &offset, &red, &green, &blue, &alpha) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_pattern_add_color_stop_rgba(pattern,offset,red,green,blue,alpha);
  return 0;
}

int _wrap_cairo_mesh_pattern_begin_patch(Stack stack, int rhs, int opt, int lhs) /* cairo_mesh_pattern_begin_patch */
{
  int_types T[] = {obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_mesh_pattern_begin_patch(pattern);
  return 0;
}

int _wrap_cairo_mesh_pattern_end_patch(Stack stack, int rhs, int opt, int lhs) /* cairo_mesh_pattern_end_patch */
{
  int_types T[] = {obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_mesh_pattern_end_patch(pattern);
  return 0;
}

int _wrap_cairo_mesh_pattern_curve_to(Stack stack, int rhs, int opt, int lhs) /* cairo_mesh_pattern_curve_to */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  double x1, y1, x2, y2, x3, y3;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &x1, &y1, &x2, &y2, &x3, &y3) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_mesh_pattern_curve_to(pattern,x1,y1,x2,y2,x3,y3);
  return 0;
}

int _wrap_cairo_mesh_pattern_line_to(Stack stack, int rhs, int opt, int lhs) /* cairo_mesh_pattern_line_to */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_mesh_pattern_line_to(pattern,x,y);
  return 0;
}

int _wrap_cairo_mesh_pattern_move_to(Stack stack, int rhs, int opt, int lhs) /* cairo_mesh_pattern_move_to */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    cairo_mesh_pattern_move_to(pattern,x,y);
  return 0;
}

int _wrap_cairo_pattern_set_matrix(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_set_matrix */
{
  int_types T[] = {obj,obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL, *nsp_matrix = NULL;
  cairo_matrix_t *matrix = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_pattern_set_matrix(pattern,matrix);
  return 0;
}

int _wrap_cairo_pattern_get_matrix(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_get_matrix */
{
  int_types T[] = {obj,obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL, *nsp_matrix = NULL;
  cairo_matrix_t *matrix = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_pattern_get_matrix(pattern,matrix);
  return 0;
}

int _wrap_cairo_pattern_set_extend(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_set_extend */
{
  int_types T[] = {obj,obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL, *nsp_extend = NULL;
  cairo_extend_t extend;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &nsp_extend) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_EXTEND, nsp_extend, &extend)== FAIL)
      return RET_BUG;
    cairo_pattern_set_extend(pattern,extend);
  return 0;
}

int _wrap_cairo_pattern_get_extend(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_get_extend */
{
  int_types T[] = {obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_get_extend(pattern);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_pattern_set_filter(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_set_filter */
{
  int_types T[] = {obj,obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL, *nsp_filter = NULL;
  cairo_filter_t filter;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &nsp_filter) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FILTER, nsp_filter, &filter)== FAIL)
      return RET_BUG;
    cairo_pattern_set_filter(pattern,filter);
  return 0;
}

int _wrap_cairo_pattern_get_filter(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_get_filter */
{
  int_types T[] = {obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_get_filter(pattern);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_pattern_get_rgba(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_get_rgba */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  double red, green, blue, alpha;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &red, &green, &blue, &alpha) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_get_rgba(pattern,&red,&green,&blue,&alpha);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_pattern_get_color_stop_rgba(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_get_color_stop_rgba */
{
  int_types T[] = {obj,s_int,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  int index;
  double offset, red, green, blue, alpha;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &index, &offset, &red, &green, &blue, &alpha) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_get_color_stop_rgba(pattern,index,&offset,&red,&green,&blue,&alpha);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_pattern_get_color_stop_count(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_get_color_stop_count */
{
  int_types T[] = {obj,s_int, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  int count;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &count) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_get_color_stop_count(pattern,&count);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_pattern_get_linear_points(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_get_linear_points */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  double x0, y0, x1, y1;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &x0, &y0, &x1, &y1) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_get_linear_points(pattern,&x0,&y0,&x1,&y1);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_pattern_get_radial_circles(Stack stack, int rhs, int opt, int lhs) /* cairo_pattern_get_radial_circles */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  double x0, y0, r0, x1, y1, r1;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern, &x0, &y0, &r0, &x1, &y1, &r1) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    ret =cairo_pattern_get_radial_circles(pattern,&x0,&y0,&r0,&x1,&y1,&r1);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_matrix_init(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_init */
{
  int_types T[] = {obj,s_double,s_double,s_double,s_double,s_double,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double xx, yx, xy, yy, x0, y0;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &xx, &yx, &xy, &yy, &x0, &y0) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_init(matrix,xx,yx,xy,yy,x0,y0);
  return 0;
}

int _wrap_cairo_matrix_init_identity(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_init_identity */
{
  int_types T[] = {obj, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_init_identity(matrix);
  return 0;
}

int _wrap_cairo_matrix_init_translate(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_init_translate */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double tx, ty;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &tx, &ty) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_init_translate(matrix,tx,ty);
  return 0;
}

int _wrap_cairo_matrix_init_scale(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_init_scale */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double sx, sy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &sx, &sy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_init_scale(matrix,sx,sy);
  return 0;
}

int _wrap_cairo_matrix_init_rotate(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_init_rotate */
{
  int_types T[] = {obj,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double radians;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &radians) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_init_rotate(matrix,radians);
  return 0;
}

int _wrap_cairo_matrix_translate(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_translate */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double tx, ty;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &tx, &ty) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_translate(matrix,tx,ty);
  return 0;
}

int _wrap_cairo_matrix_scale(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_scale */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double sx, sy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &sx, &sy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_scale(matrix,sx,sy);
  return 0;
}

int _wrap_cairo_matrix_rotate(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_rotate */
{
  int_types T[] = {obj,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double radians;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &radians) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_rotate(matrix,radians);
  return 0;
}

int _wrap_cairo_matrix_invert(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_invert */
{
  int_types T[] = {obj, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    ret =cairo_matrix_invert(matrix);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_matrix_multiply(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_multiply */
{
  int_types T[] = {obj,obj,obj, t_end};
  cairo_matrix_t *result = NULL, *a = NULL, *b = NULL;
  NspObject *nsp_result = NULL, *nsp_a = NULL, *nsp_b = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_result, &nsp_a, &nsp_b) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_result, CAIRO_GOBJECT_TYPE_MATRIX))
      result = nspg_boxed_get(nsp_result, cairo_matrix_t);
  else {
      Scierror( "Error: result should be a cairo_matrix_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_a, CAIRO_GOBJECT_TYPE_MATRIX))
      a = nspg_boxed_get(nsp_a, cairo_matrix_t);
  else {
      Scierror( "Error: a should be a cairo_matrix_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_b, CAIRO_GOBJECT_TYPE_MATRIX))
      b = nspg_boxed_get(nsp_b, cairo_matrix_t);
  else {
      Scierror( "Error: b should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_multiply(result,a,b);
  return 0;
}

int _wrap_cairo_matrix_transform_distance(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_transform_distance */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double dx, dy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &dx, &dy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_transform_distance(matrix,&dx,&dy);
  return 0;
}

int _wrap_cairo_matrix_transform_point(Stack stack, int rhs, int opt, int lhs) /* cairo_matrix_transform_point */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_matrix_t *matrix = NULL;
  NspObject *nsp_matrix = NULL;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_matrix, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_matrix, CAIRO_GOBJECT_TYPE_MATRIX))
      matrix = nspg_boxed_get(nsp_matrix, cairo_matrix_t);
  else {
      Scierror( "Error: matrix should be a cairo_matrix_t\n");
      return RET_BUG;
  }
    cairo_matrix_transform_point(matrix,&x,&y);
  return 0;
}

int _wrap_cairo_region_create(Stack stack, int rhs, int opt, int lhs) /* cairo_region_create */
{
  cairo_region_t *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =cairo_region_create();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_REGION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_region_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_region_copy(Stack stack, int rhs, int opt, int lhs) /* cairo_region_copy */
{
  int_types T[] = {obj, t_end};
  cairo_region_t *original = NULL, *ret;
  NspObject *nsp_original = NULL, *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_original) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_original, CAIRO_GOBJECT_TYPE_REGION))
      original = nspg_boxed_get(nsp_original, cairo_region_t);
  else {
      Scierror( "Error: original should be a cairo_region_t\n");
      return RET_BUG;
  }
    ret =cairo_region_copy(original);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_REGION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_region_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_region_reference(Stack stack, int rhs, int opt, int lhs) /* cairo_region_reference */
{
  int_types T[] = {obj, t_end};
  cairo_region_t *region = NULL, *ret;
  NspObject *nsp_region = NULL, *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    ret =cairo_region_reference(region);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_REGION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_region_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_region_destroy(Stack stack, int rhs, int opt, int lhs) /* cairo_region_destroy */
{
  int_types T[] = {obj, t_end};
  cairo_region_t *region = NULL;
  NspObject *nsp_region = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    cairo_region_destroy(region);
  return 0;
}

int _wrap_cairo_region_status(Stack stack, int rhs, int opt, int lhs) /* cairo_region_status */
{
  int_types T[] = {obj, t_end};
  cairo_region_t *region = NULL;
  NspObject *nsp_region = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    ret =cairo_region_status(region);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_region_num_rectangles(Stack stack, int rhs, int opt, int lhs) /* cairo_region_num_rectangles */
{
  int_types T[] = {obj, t_end};
  cairo_region_t *region = NULL;
  NspObject *nsp_region = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    ret =cairo_region_num_rectangles(region);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_region_translate(Stack stack, int rhs, int opt, int lhs) /* cairo_region_translate */
{
  int_types T[] = {obj,s_int,s_int, t_end};
  cairo_region_t *region = NULL;
  NspObject *nsp_region = NULL;
  int dx, dy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region, &dx, &dy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    cairo_region_translate(region,dx,dy);
  return 0;
}

int _wrap_cairo_region_subtract(Stack stack, int rhs, int opt, int lhs) /* cairo_region_subtract */
{
  int_types T[] = {obj,obj, t_end};
  cairo_region_t *dst = NULL, *other = NULL;
  NspObject *nsp_dst = NULL, *nsp_other = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_dst, &nsp_other) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_dst, CAIRO_GOBJECT_TYPE_REGION))
      dst = nspg_boxed_get(nsp_dst, cairo_region_t);
  else {
      Scierror( "Error: dst should be a cairo_region_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_other, CAIRO_GOBJECT_TYPE_REGION))
      other = nspg_boxed_get(nsp_other, cairo_region_t);
  else {
      Scierror( "Error: other should be a cairo_region_t\n");
      return RET_BUG;
  }
    ret =cairo_region_subtract(dst,other);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_region_intersect(Stack stack, int rhs, int opt, int lhs) /* cairo_region_intersect */
{
  int_types T[] = {obj,obj, t_end};
  cairo_region_t *dst = NULL, *other = NULL;
  NspObject *nsp_dst = NULL, *nsp_other = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_dst, &nsp_other) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_dst, CAIRO_GOBJECT_TYPE_REGION))
      dst = nspg_boxed_get(nsp_dst, cairo_region_t);
  else {
      Scierror( "Error: dst should be a cairo_region_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_other, CAIRO_GOBJECT_TYPE_REGION))
      other = nspg_boxed_get(nsp_other, cairo_region_t);
  else {
      Scierror( "Error: other should be a cairo_region_t\n");
      return RET_BUG;
  }
    ret =cairo_region_intersect(dst,other);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_region_union(Stack stack, int rhs, int opt, int lhs) /* cairo_region_union */
{
  int_types T[] = {obj,obj, t_end};
  cairo_region_t *dst = NULL, *other = NULL;
  NspObject *nsp_dst = NULL, *nsp_other = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_dst, &nsp_other) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_dst, CAIRO_GOBJECT_TYPE_REGION))
      dst = nspg_boxed_get(nsp_dst, cairo_region_t);
  else {
      Scierror( "Error: dst should be a cairo_region_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_other, CAIRO_GOBJECT_TYPE_REGION))
      other = nspg_boxed_get(nsp_other, cairo_region_t);
  else {
      Scierror( "Error: other should be a cairo_region_t\n");
      return RET_BUG;
  }
    ret =cairo_region_union(dst,other);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_region_xor(Stack stack, int rhs, int opt, int lhs) /* cairo_region_xor */
{
  int_types T[] = {obj,obj, t_end};
  cairo_region_t *dst = NULL, *other = NULL;
  NspObject *nsp_dst = NULL, *nsp_other = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_dst, &nsp_other) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_dst, CAIRO_GOBJECT_TYPE_REGION))
      dst = nspg_boxed_get(nsp_dst, cairo_region_t);
  else {
      Scierror( "Error: dst should be a cairo_region_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_other, CAIRO_GOBJECT_TYPE_REGION))
      other = nspg_boxed_get(nsp_other, cairo_region_t);
  else {
      Scierror( "Error: other should be a cairo_region_t\n");
      return RET_BUG;
  }
    ret =cairo_region_xor(dst,other);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_debug_reset_static_data(Stack stack, int rhs, int opt, int lhs) /* cairo_debug_reset_static_data */
{
  CheckRhs(0,0);
    cairo_debug_reset_static_data();
  return 0;
}

int _wrap_cairo_pdf_surface_create(Stack stack, int rhs, int opt, int lhs) /* cairo_pdf_surface_create */
{
  int_types T[] = {string,s_double,s_double, t_end};
  char *filename;
  double width_in_points, height_in_points;
  cairo_surface_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename, &width_in_points, &height_in_points) == FAIL) return RET_BUG;
    ret =cairo_pdf_surface_create(filename,width_in_points,height_in_points);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_pdf_surface_restrict_to_version(Stack stack, int rhs, int opt, int lhs) /* cairo_pdf_surface_restrict_to_version */
{
  int_types T[] = {obj,obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL, *nsp_version = NULL;
  cairo_pdf_version_t version;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &nsp_version) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_version, &version)== FAIL)
      return RET_BUG;
    cairo_pdf_surface_restrict_to_version(surface,version);
  return 0;
}

int _wrap_cairo_pdf_version_to_string(Stack stack, int rhs, int opt, int lhs) /* cairo_pdf_version_to_string */
{
  int_types T[] = {obj, t_end};
  cairo_pdf_version_t version;
  NspObject *nsp_version = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_version) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_version, &version)== FAIL)
      return RET_BUG;
    ret =cairo_pdf_version_to_string(version);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_pdf_surface_set_size(Stack stack, int rhs, int opt, int lhs) /* cairo_pdf_surface_set_size */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double width_in_points, height_in_points;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &width_in_points, &height_in_points) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_pdf_surface_set_size(surface,width_in_points,height_in_points);
  return 0;
}

int _wrap_cairo_ps_surface_create(Stack stack, int rhs, int opt, int lhs) /* cairo_ps_surface_create */
{
  int_types T[] = {string,s_double,s_double, t_end};
  char *filename;
  double width_in_points, height_in_points;
  cairo_surface_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename, &width_in_points, &height_in_points) == FAIL) return RET_BUG;
    ret =cairo_ps_surface_create(filename,width_in_points,height_in_points);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_ps_surface_restrict_to_level(Stack stack, int rhs, int opt, int lhs) /* cairo_ps_surface_restrict_to_level */
{
  int_types T[] = {obj,obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL, *nsp_level = NULL;
  cairo_ps_level_t level;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &nsp_level) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_level, &level)== FAIL)
      return RET_BUG;
    cairo_ps_surface_restrict_to_level(surface,level);
  return 0;
}

int _wrap_cairo_ps_level_to_string(Stack stack, int rhs, int opt, int lhs) /* cairo_ps_level_to_string */
{
  int_types T[] = {obj, t_end};
  cairo_ps_level_t level;
  NspObject *nsp_level = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_level) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_level, &level)== FAIL)
      return RET_BUG;
    ret =cairo_ps_level_to_string(level);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_cairo_ps_surface_set_size(Stack stack, int rhs, int opt, int lhs) /* cairo_ps_surface_set_size */
{
  int_types T[] = {obj,s_double,s_double, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double width_in_points, height_in_points;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &width_in_points, &height_in_points) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_ps_surface_set_size(surface,width_in_points,height_in_points);
  return 0;
}

int _wrap_cairo_ps_surface_dsc_comment(Stack stack, int rhs, int opt, int lhs) /* cairo_ps_surface_dsc_comment */
{
  int_types T[] = {obj,string, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  char *comment;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &comment) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_ps_surface_dsc_comment(surface,comment);
  return 0;
}

int _wrap_cairo_ps_surface_dsc_begin_setup(Stack stack, int rhs, int opt, int lhs) /* cairo_ps_surface_dsc_begin_setup */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_ps_surface_dsc_begin_setup(surface);
  return 0;
}

int _wrap_cairo_ps_surface_dsc_begin_page_setup(Stack stack, int rhs, int opt, int lhs) /* cairo_ps_surface_dsc_begin_page_setup */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    cairo_ps_surface_dsc_begin_page_setup(surface);
  return 0;
}

int _wrap_cairo_svg_surface_create(Stack stack, int rhs, int opt, int lhs) /* cairo_svg_surface_create */
{
  int_types T[] = {string,s_double,s_double, t_end};
  char *filename;
  double width_in_points, height_in_points;
  cairo_surface_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename, &width_in_points, &height_in_points) == FAIL) return RET_BUG;
    ret =cairo_svg_surface_create(filename,width_in_points,height_in_points);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_cairo_svg_surface_restrict_to_version(Stack stack, int rhs, int opt, int lhs) /* cairo_svg_surface_restrict_to_version */
{
  int_types T[] = {obj,obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL, *nsp_version = NULL;
  cairo_svg_version_t version;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface, &nsp_version) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_version, &version)== FAIL)
      return RET_BUG;
    cairo_svg_surface_restrict_to_version(surface,version);
  return 0;
}

int _wrap_cairo_svg_version_to_string(Stack stack, int rhs, int opt, int lhs) /* cairo_svg_version_to_string */
{
  int_types T[] = {obj, t_end};
  cairo_svg_version_t version;
  NspObject *nsp_version = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_version) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_version, &version)== FAIL)
      return RET_BUG;
    ret =cairo_svg_version_to_string(version);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab cairo_func[]={
  { "cairo_version", _wrap_cairo_version},
  { "cairo_version_string", _wrap_cairo_version_string},
  { "cairo_create", _wrap_cairo_create},
  { "cairo_reference", _wrap_cairo_reference},
  { "cairo_destroy", _wrap_cairo_destroy},
  { "cairo_save", _wrap_cairo_save},
  { "cairo_restore", _wrap_cairo_restore},
  { "cairo_push_group", _wrap_cairo_push_group},
  { "cairo_push_group_with_content", _wrap_cairo_push_group_with_content},
  { "cairo_pop_group", _wrap_cairo_pop_group},
  { "cairo_pop_group_to_source", _wrap_cairo_pop_group_to_source},
  { "cairo_set_operator", _wrap_cairo_set_operator},
  { "cairo_set_source", _wrap_cairo_set_source},
  { "cairo_set_source_rgb", _wrap_cairo_set_source_rgb},
  { "cairo_set_source_rgba", _wrap_cairo_set_source_rgba},
  { "cairo_set_source_surface", _wrap_cairo_set_source_surface},
  { "cairo_set_tolerance", _wrap_cairo_set_tolerance},
  { "cairo_set_antialias", _wrap_cairo_set_antialias},
  { "cairo_set_fill_rule", _wrap_cairo_set_fill_rule},
  { "cairo_set_line_width", _wrap_cairo_set_line_width},
  { "cairo_set_line_cap", _wrap_cairo_set_line_cap},
  { "cairo_set_line_join", _wrap_cairo_set_line_join},
  { "cairo_set_miter_limit", _wrap_cairo_set_miter_limit},
  { "cairo_translate", _wrap_cairo_translate},
  { "cairo_scale", _wrap_cairo_scale},
  { "cairo_rotate", _wrap_cairo_rotate},
  { "cairo_transform", _wrap_cairo_transform},
  { "cairo_set_matrix", _wrap_cairo_set_matrix},
  { "cairo_identity_matrix", _wrap_cairo_identity_matrix},
  { "cairo_user_to_device", _wrap_cairo_user_to_device},
  { "cairo_user_to_device_distance", _wrap_cairo_user_to_device_distance},
  { "cairo_device_to_user", _wrap_cairo_device_to_user},
  { "cairo_device_to_user_distance", _wrap_cairo_device_to_user_distance},
  { "cairo_new_path", _wrap_cairo_new_path},
  { "cairo_move_to", _wrap_cairo_move_to},
  { "cairo_new_sub_path", _wrap_cairo_new_sub_path},
  { "cairo_line_to", _wrap_cairo_line_to},
  { "cairo_curve_to", _wrap_cairo_curve_to},
  { "cairo_arc", _wrap_cairo_arc},
  { "cairo_arc_negative", _wrap_cairo_arc_negative},
  { "cairo_rel_move_to", _wrap_cairo_rel_move_to},
  { "cairo_rel_line_to", _wrap_cairo_rel_line_to},
  { "cairo_rel_curve_to", _wrap_cairo_rel_curve_to},
  { "cairo_rectangle", _wrap_cairo_rectangle},
  { "cairo_close_path", _wrap_cairo_close_path},
  { "cairo_path_extents", _wrap_cairo_path_extents},
  { "cairo_paint", _wrap_cairo_paint},
  { "cairo_paint_with_alpha", _wrap_cairo_paint_with_alpha},
  { "cairo_mask", _wrap_cairo_mask},
  { "cairo_mask_surface", _wrap_cairo_mask_surface},
  { "cairo_stroke", _wrap_cairo_stroke},
  { "cairo_stroke_preserve", _wrap_cairo_stroke_preserve},
  { "cairo_fill", _wrap_cairo_fill},
  { "cairo_fill_preserve", _wrap_cairo_fill_preserve},
  { "cairo_copy_page", _wrap_cairo_copy_page},
  { "cairo_show_page", _wrap_cairo_show_page},
  { "cairo_stroke_extents", _wrap_cairo_stroke_extents},
  { "cairo_fill_extents", _wrap_cairo_fill_extents},
  { "cairo_reset_clip", _wrap_cairo_reset_clip},
  { "cairo_clip", _wrap_cairo_clip},
  { "cairo_clip_preserve", _wrap_cairo_clip_preserve},
  { "cairo_clip_extents", _wrap_cairo_clip_extents},
  { "cairo_select_font_face", _wrap_cairo_select_font_face},
  { "cairo_set_font_size", _wrap_cairo_set_font_size},
  { "cairo_set_font_matrix", _wrap_cairo_set_font_matrix},
  { "cairo_get_font_matrix", _wrap_cairo_get_font_matrix},
  { "cairo_show_text", _wrap_cairo_show_text},
  { "cairo_text_path", _wrap_cairo_text_path},
  { "cairo_get_operator", _wrap_cairo_get_operator},
  { "cairo_get_source", _wrap_cairo_get_source},
  { "cairo_get_tolerance", _wrap_cairo_get_tolerance},
  { "cairo_get_antialias", _wrap_cairo_get_antialias},
  { "cairo_get_current_point", _wrap_cairo_get_current_point},
  { "cairo_get_fill_rule", _wrap_cairo_get_fill_rule},
  { "cairo_get_line_width", _wrap_cairo_get_line_width},
  { "cairo_get_line_cap", _wrap_cairo_get_line_cap},
  { "cairo_get_line_join", _wrap_cairo_get_line_join},
  { "cairo_get_miter_limit", _wrap_cairo_get_miter_limit},
  { "cairo_get_dash_count", _wrap_cairo_get_dash_count},
  { "cairo_get_dash", _wrap_cairo_get_dash},
  { "cairo_get_matrix", _wrap_cairo_get_matrix},
  { "cairo_get_target", _wrap_cairo_get_target},
  { "cairo_get_group_target", _wrap_cairo_get_group_target},
  { "cairo_status", _wrap_cairo_status},
  { "cairo_status_to_string", _wrap_cairo_status_to_string},
  { "cairo_surface_create_similar", _wrap_cairo_surface_create_similar},
  { "cairo_surface_create_similar_image", _wrap_cairo_surface_create_similar_image},
  { "cairo_surface_unmap_image", _wrap_cairo_surface_unmap_image},
  { "cairo_surface_create_for_rectangle", _wrap_cairo_surface_create_for_rectangle},
  { "cairo_surface_observer_elapsed", _wrap_cairo_surface_observer_elapsed},
  { "cairo_surface_reference", _wrap_cairo_surface_reference},
  { "cairo_surface_finish", _wrap_cairo_surface_finish},
  { "cairo_surface_destroy", _wrap_cairo_surface_destroy},
  { "cairo_surface_status", _wrap_cairo_surface_status},
  { "cairo_surface_get_content", _wrap_cairo_surface_get_content},
  { "cairo_surface_write_to_png", _wrap_cairo_surface_write_to_png},
  { "cairo_surface_flush", _wrap_cairo_surface_flush},
  { "cairo_surface_mark_dirty", _wrap_cairo_surface_mark_dirty},
  { "cairo_surface_mark_dirty_rectangle", _wrap_cairo_surface_mark_dirty_rectangle},
  { "cairo_surface_set_device_scale", _wrap_cairo_surface_set_device_scale},
  { "cairo_surface_get_device_scale", _wrap_cairo_surface_get_device_scale},
  { "cairo_surface_set_device_offset", _wrap_cairo_surface_set_device_offset},
  { "cairo_surface_get_device_offset", _wrap_cairo_surface_get_device_offset},
  { "cairo_surface_set_fallback_resolution", _wrap_cairo_surface_set_fallback_resolution},
  { "cairo_surface_get_fallback_resolution", _wrap_cairo_surface_get_fallback_resolution},
  { "cairo_surface_copy_page", _wrap_cairo_surface_copy_page},
  { "cairo_surface_show_page", _wrap_cairo_surface_show_page},
  { "cairo_image_surface_create", _wrap_cairo_image_surface_create},
  { "cairo_format_stride_for_width", _wrap_cairo_format_stride_for_width},
  { "cairo_image_surface_create_for_data", _wrap_cairo_image_surface_create_for_data},
  { "cairo_image_surface_get_format", _wrap_cairo_image_surface_get_format},
  { "cairo_image_surface_get_width", _wrap_cairo_image_surface_get_width},
  { "cairo_image_surface_get_height", _wrap_cairo_image_surface_get_height},
  { "cairo_image_surface_get_stride", _wrap_cairo_image_surface_get_stride},
  { "cairo_image_surface_create_from_png", _wrap_cairo_image_surface_create_from_png},
  { "cairo_recording_surface_create", _wrap_cairo_recording_surface_create},
  { "cairo_recording_surface_ink_extents", _wrap_cairo_recording_surface_ink_extents},
  { "cairo_pattern_create_rgb", _wrap_cairo_pattern_create_rgb},
  { "cairo_pattern_create_rgba", _wrap_cairo_pattern_create_rgba},
  { "cairo_pattern_create_for_surface", _wrap_cairo_pattern_create_for_surface},
  { "cairo_pattern_create_linear", _wrap_cairo_pattern_create_linear},
  { "cairo_pattern_create_radial", _wrap_cairo_pattern_create_radial},
  { "cairo_pattern_create_mesh", _wrap_cairo_pattern_create_mesh},
  { "cairo_pattern_reference", _wrap_cairo_pattern_reference},
  { "cairo_pattern_destroy", _wrap_cairo_pattern_destroy},
  { "cairo_pattern_status", _wrap_cairo_pattern_status},
  { "cairo_pattern_add_color_stop_rgb", _wrap_cairo_pattern_add_color_stop_rgb},
  { "cairo_pattern_add_color_stop_rgba", _wrap_cairo_pattern_add_color_stop_rgba},
  { "cairo_mesh_pattern_begin_patch", _wrap_cairo_mesh_pattern_begin_patch},
  { "cairo_mesh_pattern_end_patch", _wrap_cairo_mesh_pattern_end_patch},
  { "cairo_mesh_pattern_curve_to", _wrap_cairo_mesh_pattern_curve_to},
  { "cairo_mesh_pattern_line_to", _wrap_cairo_mesh_pattern_line_to},
  { "cairo_mesh_pattern_move_to", _wrap_cairo_mesh_pattern_move_to},
  { "cairo_pattern_set_matrix", _wrap_cairo_pattern_set_matrix},
  { "cairo_pattern_get_matrix", _wrap_cairo_pattern_get_matrix},
  { "cairo_pattern_set_extend", _wrap_cairo_pattern_set_extend},
  { "cairo_pattern_get_extend", _wrap_cairo_pattern_get_extend},
  { "cairo_pattern_set_filter", _wrap_cairo_pattern_set_filter},
  { "cairo_pattern_get_filter", _wrap_cairo_pattern_get_filter},
  { "cairo_pattern_get_rgba", _wrap_cairo_pattern_get_rgba},
  { "cairo_pattern_get_color_stop_rgba", _wrap_cairo_pattern_get_color_stop_rgba},
  { "cairo_pattern_get_color_stop_count", _wrap_cairo_pattern_get_color_stop_count},
  { "cairo_pattern_get_linear_points", _wrap_cairo_pattern_get_linear_points},
  { "cairo_pattern_get_radial_circles", _wrap_cairo_pattern_get_radial_circles},
  { "cairo_matrix_init", _wrap_cairo_matrix_init},
  { "cairo_matrix_init_identity", _wrap_cairo_matrix_init_identity},
  { "cairo_matrix_init_translate", _wrap_cairo_matrix_init_translate},
  { "cairo_matrix_init_scale", _wrap_cairo_matrix_init_scale},
  { "cairo_matrix_init_rotate", _wrap_cairo_matrix_init_rotate},
  { "cairo_matrix_translate", _wrap_cairo_matrix_translate},
  { "cairo_matrix_scale", _wrap_cairo_matrix_scale},
  { "cairo_matrix_rotate", _wrap_cairo_matrix_rotate},
  { "cairo_matrix_invert", _wrap_cairo_matrix_invert},
  { "cairo_matrix_multiply", _wrap_cairo_matrix_multiply},
  { "cairo_matrix_transform_distance", _wrap_cairo_matrix_transform_distance},
  { "cairo_matrix_transform_point", _wrap_cairo_matrix_transform_point},
  { "cairo_region_create", _wrap_cairo_region_create},
  { "cairo_region_copy", _wrap_cairo_region_copy},
  { "cairo_region_reference", _wrap_cairo_region_reference},
  { "cairo_region_destroy", _wrap_cairo_region_destroy},
  { "cairo_region_status", _wrap_cairo_region_status},
  { "cairo_region_num_rectangles", _wrap_cairo_region_num_rectangles},
  { "cairo_region_translate", _wrap_cairo_region_translate},
  { "cairo_region_subtract", _wrap_cairo_region_subtract},
  { "cairo_region_intersect", _wrap_cairo_region_intersect},
  { "cairo_region_union", _wrap_cairo_region_union},
  { "cairo_region_xor", _wrap_cairo_region_xor},
  { "cairo_debug_reset_static_data", _wrap_cairo_debug_reset_static_data},
  { "cairo_pdf_surface_create", _wrap_cairo_pdf_surface_create},
  { "cairo_pdf_surface_restrict_to_version", _wrap_cairo_pdf_surface_restrict_to_version},
  { "cairo_pdf_version_to_string", _wrap_cairo_pdf_version_to_string},
  { "cairo_pdf_surface_set_size", _wrap_cairo_pdf_surface_set_size},
  { "cairo_ps_surface_create", _wrap_cairo_ps_surface_create},
  { "cairo_ps_surface_restrict_to_level", _wrap_cairo_ps_surface_restrict_to_level},
  { "cairo_ps_level_to_string", _wrap_cairo_ps_level_to_string},
  { "cairo_ps_surface_set_size", _wrap_cairo_ps_surface_set_size},
  { "cairo_ps_surface_dsc_comment", _wrap_cairo_ps_surface_dsc_comment},
  { "cairo_ps_surface_dsc_begin_setup", _wrap_cairo_ps_surface_dsc_begin_setup},
  { "cairo_ps_surface_dsc_begin_page_setup", _wrap_cairo_ps_surface_dsc_begin_page_setup},
  { "cairo_svg_surface_create", _wrap_cairo_svg_surface_create},
  { "cairo_svg_surface_restrict_to_version", _wrap_cairo_svg_surface_restrict_to_version},
  { "cairo_svg_version_to_string", _wrap_cairo_svg_version_to_string},
  { NULL, NULL}
};

/* call ith function in the cairo interface */

int cairo_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(cairo_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void cairo_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = cairo_func[i].name;
  *f = cairo_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
cairo_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_STATUS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_CONTENT, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_FORMAT, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_OPERATOR, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_ANTIALIAS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_FILL_RULE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_LINE_CAP, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_LINE_JOIN, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_TEXT_CLUSTER_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_FONT_SLANT, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_FONT_WEIGHT, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_SUBPIXEL_ORDER, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_HINT_STYLE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_HINT_METRICS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_FONT_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_PATH_DATA_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_DEVICE_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_SURFACE_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_PATTERN_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_EXTEND, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_FILTER, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, CAIRO_GOBJECT_TYPE_REGION_OVERLAP, strip_prefix);
/* enum or flags without typecode: cairo_pdf_version_t */
/* enum or flags without typecode: cairo_ps_level_t */
/* enum or flags without typecode: cairo_script_mode_t */
/* enum or flags without typecode: cairo_svg_version_t */
}

void nsp_initialize_cairo_types(void)
{
  new_type_cairo_t(T_BASE);
  new_type_cairo_surface_t(T_BASE);
  new_type_cairo_pattern_t(T_BASE);
  new_type_cairo_region_t(T_BASE);
  new_type_cairo_rectangle_t(T_BASE);
  new_type_cairo_matrix_t(T_BASE);
}

#line 4755 "cairo.c"
