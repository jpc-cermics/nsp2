/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2014 Jean-Philippe Chancelier Enpc/Cermics
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



#line 4 "atk.override"

#include "nsp/gtk/gobject.h"
#include <atk/atk.h>
#include <atk/atknoopobjectfactory.h>
#include <atk/atknoopobject.h>
#line 32 "atk.c"


/* ---------- types from other modules ---------- */
#include "nsp/gtk/gobject.h"


/* ---------- forward type declarations ---------- */
#include "nsp/gtk/atkhyperlink.h"
#include "nsp/gtk/atkobject.h"
#include "nsp/gtk/atknoopobject.h"
#include "nsp/gtk/atkobjectfactory.h"
#include "nsp/gtk/atknoopobjectfactory.h"
#include "nsp/gtk/atkregistry.h"
#include "nsp/gtk/atkrelation.h"
#include "nsp/gtk/atkrelationset.h"
#include "nsp/gtk/atkstateset.h"
#include "nsp/gtk/atkutil.h"


/* ----------- NspAtkHyperlink ----------- */


#define  AtkHyperlink_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkhyperlink.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkHyperlink inherits from GObject 
 */

int nsp_type_atkhyperlink_id=0;
NspTypeAtkHyperlink *nsp_type_atkhyperlink=NULL;

/*
 * Type object for NspAtkHyperlink 
 * all the instance of NspTypeAtkHyperlink share the same id. 
 * nsp_type_atkhyperlink: is an instance of NspTypeAtkHyperlink 
 *    used for objects of NspAtkHyperlink type (i.e built with new_atkhyperlink) 
 * other instances are used for derived classes 
 */
NspTypeAtkHyperlink *new_type_atkhyperlink(type_mode mode)
{
  NspTypeAtkHyperlink *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkhyperlink != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkhyperlink;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkhyperlink_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atkhyperlink_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atkhyperlink;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atkhyperlink */ 

  top->s_type =  (s_type_func *) nsp_atkhyperlink_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atkhyperlink_type_short_string;
  /* top->create = (create_func*) int_atkhyperlink_create;*/ 
  
  /* specific methods for atkhyperlink */
      
  type->init = (init_func *) init_atkhyperlink;

  /* 
   * NspAtkHyperlink interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkhyperlink_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkHyperlink called nsp_type_atkhyperlink
       */
      type->id =  nsp_type_atkhyperlink_id = nsp_new_type_id();
      nsp_type_atkhyperlink = type;
      if ( nsp_register_type(nsp_type_atkhyperlink) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkhyperlink, ATK_TYPE_HYPERLINK);
      return ( mode == T_BASE ) ? type : new_type_atkhyperlink(mode);
    }
  else 
    {
       type->id = nsp_type_atkhyperlink_id;
       return type;
    }
}

/*
 * initialize NspAtkHyperlink instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkhyperlink(NspAtkHyperlink *Obj,NspTypeAtkHyperlink *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkHyperlink 
 */

NspAtkHyperlink *new_atkhyperlink() 
{
  NspAtkHyperlink *loc; 
  /* type must exists */
  nsp_type_atkhyperlink = new_type_atkhyperlink(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkHyperlink)))== NULLATKHYPERLINK) return loc;
  /* initialize object */
  if ( init_atkhyperlink(loc,nsp_type_atkhyperlink) == FAIL) return NULLATKHYPERLINK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkHyperlink 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atkhyperlink_type_name[]="AtkHyperlink";
static char atkhyperlink_short_type_name[]="AtkHyperlink";

static char *nsp_atkhyperlink_type_as_string(void)
{
  return(atkhyperlink_type_name);
}

static char *nsp_atkhyperlink_type_short_string(NspObject *v)
{
  return(atkhyperlink_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkHyperlink objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkHyperlink *nsp_atkhyperlink_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkhyperlink_id)  == TRUE  ) return ((NspAtkHyperlink *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkhyperlink));
  return NULL;
}

int IsAtkHyperlinkObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atkhyperlink_id);
}

int IsAtkHyperlink(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkhyperlink_id);
}

NspAtkHyperlink  *GetAtkHyperlinkCopy(Stack stack, int i)
{
  if (  GetAtkHyperlink(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkHyperlink  *GetAtkHyperlink(Stack stack, int i)
{
  NspAtkHyperlink *M;
  if (( M = nsp_atkhyperlink_object(NthObj(i))) == NULLATKHYPERLINK)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkHyperlink *atkhyperlink_copy(NspAtkHyperlink *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkhyperlink);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkhyperlink);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkHyperlink
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int _wrap_atk_hyperlink_get_uri(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int i;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
  ret = atk_hyperlink_get_uri(ATK_HYPERLINK(self->obj), i);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_atk_hyperlink_get_object(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int i;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
  ret = atk_hyperlink_get_object(ATK_HYPERLINK(self->obj), i);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_hyperlink_get_end_index(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_hyperlink_get_end_index(ATK_HYPERLINK(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_hyperlink_get_start_index(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_hyperlink_get_start_index(ATK_HYPERLINK(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_hyperlink_is_valid(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_hyperlink_is_valid(ATK_HYPERLINK(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_hyperlink_get_n_anchors(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_hyperlink_get_n_anchors(ATK_HYPERLINK(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods atkhyperlink_methods[] = {
  {"get_uri",(nsp_method *) _wrap_atk_hyperlink_get_uri},
  {"get_object",(nsp_method *) _wrap_atk_hyperlink_get_object},
  {"get_end_index",(nsp_method *) _wrap_atk_hyperlink_get_end_index},
  {"get_start_index",(nsp_method *) _wrap_atk_hyperlink_get_start_index},
  {"is_valid",(nsp_method *) _wrap_atk_hyperlink_is_valid},
  {"get_n_anchors",(nsp_method *) _wrap_atk_hyperlink_get_n_anchors},
  { NULL, NULL}
};

static NspMethods *atkhyperlink_get_methods(void) { return atkhyperlink_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkhyperlink_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkObject ----------- */


#define  AtkObject_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkobject.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkObject inherits from GObject 
 */

int nsp_type_atkobject_id=0;
NspTypeAtkObject *nsp_type_atkobject=NULL;

/*
 * Type object for NspAtkObject 
 * all the instance of NspTypeAtkObject share the same id. 
 * nsp_type_atkobject: is an instance of NspTypeAtkObject 
 *    used for objects of NspAtkObject type (i.e built with new_atkobject) 
 * other instances are used for derived classes 
 */
NspTypeAtkObject *new_type_atkobject(type_mode mode)
{
  NspTypeAtkObject *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkobject != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkobject;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkobject_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atkobject_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atkobject;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atkobject */ 

  top->s_type =  (s_type_func *) nsp_atkobject_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atkobject_type_short_string;
  /* top->create = (create_func*) int_atkobject_create;*/ 
  
  /* specific methods for atkobject */
      
  type->init = (init_func *) init_atkobject;

  /* 
   * NspAtkObject interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkobject_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkObject called nsp_type_atkobject
       */
      type->id =  nsp_type_atkobject_id = nsp_new_type_id();
      nsp_type_atkobject = type;
      if ( nsp_register_type(nsp_type_atkobject) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkobject, ATK_TYPE_OBJECT);
      return ( mode == T_BASE ) ? type : new_type_atkobject(mode);
    }
  else 
    {
       type->id = nsp_type_atkobject_id;
       return type;
    }
}

/*
 * initialize NspAtkObject instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkobject(NspAtkObject *Obj,NspTypeAtkObject *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkObject 
 */

NspAtkObject *new_atkobject() 
{
  NspAtkObject *loc; 
  /* type must exists */
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkObject)))== NULLATKOBJECT) return loc;
  /* initialize object */
  if ( init_atkobject(loc,nsp_type_atkobject) == FAIL) return NULLATKOBJECT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkObject 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atkobject_type_name[]="AtkObject";
static char atkobject_short_type_name[]="AtkObject";

static char *nsp_atkobject_type_as_string(void)
{
  return(atkobject_type_name);
}

static char *nsp_atkobject_type_short_string(NspObject *v)
{
  return(atkobject_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkObject objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkObject *nsp_atkobject_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkobject_id)  == TRUE  ) return ((NspAtkObject *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkobject));
  return NULL;
}

int IsAtkObjectObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atkobject_id);
}

int IsAtkObject(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkobject_id);
}

NspAtkObject  *GetAtkObjectCopy(Stack stack, int i)
{
  if (  GetAtkObject(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkObject  *GetAtkObject(Stack stack, int i)
{
  NspAtkObject *M;
  if (( M = nsp_atkobject_object(NthObj(i))) == NULLATKOBJECT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkObject *atkobject_copy(NspAtkObject *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkobject);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkobject);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkObject
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int _wrap_atk_object_get_name(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
  ret = atk_object_get_name(ATK_OBJECT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_get_description(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
  ret = atk_object_get_description(ATK_OBJECT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_get_parent(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  AtkObject *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret = atk_object_get_parent(ATK_OBJECT(self->obj));
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_get_n_accessible_children(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_object_get_n_accessible_children(ATK_OBJECT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_ref_accessible_child(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int i;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
  ret = atk_object_ref_accessible_child(ATK_OBJECT(self->obj), i);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_ref_relation_set(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  AtkRelationSet *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret = atk_object_ref_relation_set(ATK_OBJECT(self->obj));
  nsp_type_atkrelationset = new_type_atkrelationset(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkrelationset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_get_role(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
  ret = atk_object_get_role(ATK_OBJECT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_get_layer(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
  ret = atk_object_get_layer(ATK_OBJECT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_get_mdi_zorder(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_object_get_mdi_zorder(ATK_OBJECT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_ref_state_set(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  AtkStateSet *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret = atk_object_ref_state_set(ATK_OBJECT(self->obj));
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkstateset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_get_index_in_parent(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_object_get_index_in_parent(ATK_OBJECT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_set_name(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *name;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
  atk_object_set_name(ATK_OBJECT(self->obj), name);
  return 0;
}

static int _wrap_atk_object_set_description(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *description;
  if ( GetArgs(stack,rhs,opt,T,&description) == FAIL) return RET_BUG;
  atk_object_set_description(ATK_OBJECT(self->obj), description);
  return 0;
}

static int _wrap_atk_object_set_parent(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *parent;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkobject, &parent) == FAIL) return RET_BUG;
  atk_object_set_parent(ATK_OBJECT(self->obj), ATK_OBJECT(parent->obj));
  return 0;
}

static int _wrap_atk_object_set_role(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  AtkRole role;
  NspObject *nsp_role = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_role) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_role, &role)== FAIL)
      return RET_BUG;
  atk_object_set_role(ATK_OBJECT(self->obj), role);
  return 0;
}

static int _wrap_atk_object_remove_property_change_handler(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int handler_id;
  if ( GetArgs(stack,rhs,opt,T,&handler_id) == FAIL) return RET_BUG;
  atk_object_remove_property_change_handler(ATK_OBJECT(self->obj), handler_id);
  return 0;
}

static NspMethods atkobject_methods[] = {
  {"get_name",(nsp_method *) _wrap_atk_object_get_name},
  {"get_description",(nsp_method *) _wrap_atk_object_get_description},
  {"get_parent",(nsp_method *) _wrap_atk_object_get_parent},
  {"get_n_accessible_children",(nsp_method *) _wrap_atk_object_get_n_accessible_children},
  {"ref_accessible_child",(nsp_method *) _wrap_atk_object_ref_accessible_child},
  {"ref_relation_set",(nsp_method *) _wrap_atk_object_ref_relation_set},
  {"get_role",(nsp_method *) _wrap_atk_object_get_role},
  {"get_layer",(nsp_method *) _wrap_atk_object_get_layer},
  {"get_mdi_zorder",(nsp_method *) _wrap_atk_object_get_mdi_zorder},
  {"ref_state_set",(nsp_method *) _wrap_atk_object_ref_state_set},
  {"get_index_in_parent",(nsp_method *) _wrap_atk_object_get_index_in_parent},
  {"set_name",(nsp_method *) _wrap_atk_object_set_name},
  {"set_description",(nsp_method *) _wrap_atk_object_set_description},
  {"set_parent",(nsp_method *) _wrap_atk_object_set_parent},
  {"set_role",(nsp_method *) _wrap_atk_object_set_role},
  {"remove_property_change_handler",(nsp_method *) _wrap_atk_object_remove_property_change_handler},
  { NULL, NULL}
};

static NspMethods *atkobject_get_methods(void) { return atkobject_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkobject_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkNoOpObject ----------- */


#define  AtkNoOpObject_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atknoopobject.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkNoOpObject inherits from AtkObject 
 */

int nsp_type_atknoopobject_id=0;
NspTypeAtkNoOpObject *nsp_type_atknoopobject=NULL;

/*
 * Type object for NspAtkNoOpObject 
 * all the instance of NspTypeAtkNoOpObject share the same id. 
 * nsp_type_atknoopobject: is an instance of NspTypeAtkNoOpObject 
 *    used for objects of NspAtkNoOpObject type (i.e built with new_atknoopobject) 
 * other instances are used for derived classes 
 */
NspTypeAtkNoOpObject *new_type_atknoopobject(type_mode mode)
{
  NspTypeAtkNoOpObject *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atknoopobject != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atknoopobject;
    }
  if ((type =  malloc(sizeof(NspTypeAtkObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_atkobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atknoopobject_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atknoopobject_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atknoopobject;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atknoopobject */ 

  top->s_type =  (s_type_func *) nsp_atknoopobject_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atknoopobject_type_short_string;
  /* top->create = (create_func*) int_atknoopobject_create;*/ 
  
  /* specific methods for atknoopobject */
      
  type->init = (init_func *) init_atknoopobject;

  /* 
   * NspAtkNoOpObject interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atknoopobject_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkNoOpObject called nsp_type_atknoopobject
       */
      type->id =  nsp_type_atknoopobject_id = nsp_new_type_id();
      nsp_type_atknoopobject = type;
      if ( nsp_register_type(nsp_type_atknoopobject) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atknoopobject, ATK_TYPE_NO_OP_OBJECT);
      return ( mode == T_BASE ) ? type : new_type_atknoopobject(mode);
    }
  else 
    {
       type->id = nsp_type_atknoopobject_id;
       return type;
    }
}

/*
 * initialize NspAtkNoOpObject instances 
 * locally and by calling initializer on parent class 
 */

static int init_atknoopobject(NspAtkNoOpObject *Obj,NspTypeAtkNoOpObject *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkNoOpObject 
 */

NspAtkNoOpObject *new_atknoopobject() 
{
  NspAtkNoOpObject *loc; 
  /* type must exists */
  nsp_type_atknoopobject = new_type_atknoopobject(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkNoOpObject)))== NULLATKNOOPOBJECT) return loc;
  /* initialize object */
  if ( init_atknoopobject(loc,nsp_type_atknoopobject) == FAIL) return NULLATKNOOPOBJECT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkNoOpObject 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atknoopobject_type_name[]="AtkNoOpObject";
static char atknoopobject_short_type_name[]="AtkNoOpObject";

static char *nsp_atknoopobject_type_as_string(void)
{
  return(atknoopobject_type_name);
}

static char *nsp_atknoopobject_type_short_string(NspObject *v)
{
  return(atknoopobject_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkNoOpObject objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkNoOpObject *nsp_atknoopobject_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atknoopobject_id)  == TRUE  ) return ((NspAtkNoOpObject *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atknoopobject));
  return NULL;
}

int IsAtkNoOpObjectObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atknoopobject_id);
}

int IsAtkNoOpObject(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atknoopobject_id);
}

NspAtkNoOpObject  *GetAtkNoOpObjectCopy(Stack stack, int i)
{
  if (  GetAtkNoOpObject(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkNoOpObject  *GetAtkNoOpObject(Stack stack, int i)
{
  NspAtkNoOpObject *M;
  if (( M = nsp_atknoopobject_object(NthObj(i))) == NULLATKNOOPOBJECT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkNoOpObject *atknoopobject_copy(NspAtkNoOpObject *self)
{
  /* return atkobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atknoopobject);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atknoopobject);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkNoOpObject
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int
_wrap_atknoopobject_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *obj;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gobject, &obj) == FAIL) return RET_BUG;
  if ((ret = (GObject *)atk_no_op_object_new(G_OBJECT(obj->obj)))== NULL) return RET_BUG;

  nsp_type_atknoopobject = new_type_atknoopobject(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_atknoopobject );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *atknoopobject_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atknoopobject_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkObjectFactory ----------- */


#define  AtkObjectFactory_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkobjectfactory.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkObjectFactory inherits from GObject 
 */

int nsp_type_atkobjectfactory_id=0;
NspTypeAtkObjectFactory *nsp_type_atkobjectfactory=NULL;

/*
 * Type object for NspAtkObjectFactory 
 * all the instance of NspTypeAtkObjectFactory share the same id. 
 * nsp_type_atkobjectfactory: is an instance of NspTypeAtkObjectFactory 
 *    used for objects of NspAtkObjectFactory type (i.e built with new_atkobjectfactory) 
 * other instances are used for derived classes 
 */
NspTypeAtkObjectFactory *new_type_atkobjectfactory(type_mode mode)
{
  NspTypeAtkObjectFactory *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkobjectfactory != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkobjectfactory;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkobjectfactory_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atkobjectfactory_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atkobjectfactory;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atkobjectfactory */ 

  top->s_type =  (s_type_func *) nsp_atkobjectfactory_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atkobjectfactory_type_short_string;
  /* top->create = (create_func*) int_atkobjectfactory_create;*/ 
  
  /* specific methods for atkobjectfactory */
      
  type->init = (init_func *) init_atkobjectfactory;

  /* 
   * NspAtkObjectFactory interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkobjectfactory_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkObjectFactory called nsp_type_atkobjectfactory
       */
      type->id =  nsp_type_atkobjectfactory_id = nsp_new_type_id();
      nsp_type_atkobjectfactory = type;
      if ( nsp_register_type(nsp_type_atkobjectfactory) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkobjectfactory, ATK_TYPE_OBJECT_FACTORY);
      return ( mode == T_BASE ) ? type : new_type_atkobjectfactory(mode);
    }
  else 
    {
       type->id = nsp_type_atkobjectfactory_id;
       return type;
    }
}

/*
 * initialize NspAtkObjectFactory instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkobjectfactory(NspAtkObjectFactory *Obj,NspTypeAtkObjectFactory *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkObjectFactory 
 */

NspAtkObjectFactory *new_atkobjectfactory() 
{
  NspAtkObjectFactory *loc; 
  /* type must exists */
  nsp_type_atkobjectfactory = new_type_atkobjectfactory(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkObjectFactory)))== NULLATKOBJECTFACTORY) return loc;
  /* initialize object */
  if ( init_atkobjectfactory(loc,nsp_type_atkobjectfactory) == FAIL) return NULLATKOBJECTFACTORY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkObjectFactory 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atkobjectfactory_type_name[]="AtkObjectFactory";
static char atkobjectfactory_short_type_name[]="AtkObjectFactory";

static char *nsp_atkobjectfactory_type_as_string(void)
{
  return(atkobjectfactory_type_name);
}

static char *nsp_atkobjectfactory_type_short_string(NspObject *v)
{
  return(atkobjectfactory_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkObjectFactory objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkObjectFactory *nsp_atkobjectfactory_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkobjectfactory_id)  == TRUE  ) return ((NspAtkObjectFactory *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkobjectfactory));
  return NULL;
}

int IsAtkObjectFactoryObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atkobjectfactory_id);
}

int IsAtkObjectFactory(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkobjectfactory_id);
}

NspAtkObjectFactory  *GetAtkObjectFactoryCopy(Stack stack, int i)
{
  if (  GetAtkObjectFactory(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkObjectFactory  *GetAtkObjectFactory(Stack stack, int i)
{
  NspAtkObjectFactory *M;
  if (( M = nsp_atkobjectfactory_object(NthObj(i))) == NULLATKOBJECTFACTORY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkObjectFactory *atkobjectfactory_copy(NspAtkObjectFactory *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkobjectfactory);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkobjectfactory);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkObjectFactory
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int _wrap_atk_object_factory_create_accessible(NspAtkObjectFactory *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *obj;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gobject, &obj) == FAIL) return RET_BUG;
  ret = atk_object_factory_create_accessible(ATK_OBJECT_FACTORY(self->obj), G_OBJECT(obj->obj));
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_factory_invalidate(NspAtkObjectFactory *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  atk_object_factory_invalidate(ATK_OBJECT_FACTORY(self->obj));
  return 0;
}

static NspMethods atkobjectfactory_methods[] = {
  {"create_accessible",(nsp_method *) _wrap_atk_object_factory_create_accessible},
  {"invalidate",(nsp_method *) _wrap_atk_object_factory_invalidate},
  { NULL, NULL}
};

static NspMethods *atkobjectfactory_get_methods(void) { return atkobjectfactory_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkobjectfactory_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkNoOpObjectFactory ----------- */


#define  AtkNoOpObjectFactory_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atknoopobjectfactory.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkNoOpObjectFactory inherits from AtkObjectFactory 
 */

int nsp_type_atknoopobjectfactory_id=0;
NspTypeAtkNoOpObjectFactory *nsp_type_atknoopobjectfactory=NULL;

/*
 * Type object for NspAtkNoOpObjectFactory 
 * all the instance of NspTypeAtkNoOpObjectFactory share the same id. 
 * nsp_type_atknoopobjectfactory: is an instance of NspTypeAtkNoOpObjectFactory 
 *    used for objects of NspAtkNoOpObjectFactory type (i.e built with new_atknoopobjectfactory) 
 * other instances are used for derived classes 
 */
NspTypeAtkNoOpObjectFactory *new_type_atknoopobjectfactory(type_mode mode)
{
  NspTypeAtkNoOpObjectFactory *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atknoopobjectfactory != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atknoopobjectfactory;
    }
  if ((type =  malloc(sizeof(NspTypeAtkObjectFactory))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_atkobjectfactory(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atknoopobjectfactory_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atknoopobjectfactory_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atknoopobjectfactory;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atknoopobjectfactory */ 

  top->s_type =  (s_type_func *) nsp_atknoopobjectfactory_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atknoopobjectfactory_type_short_string;
  /* top->create = (create_func*) int_atknoopobjectfactory_create;*/ 
  
  /* specific methods for atknoopobjectfactory */
      
  type->init = (init_func *) init_atknoopobjectfactory;

  /* 
   * NspAtkNoOpObjectFactory interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atknoopobjectfactory_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkNoOpObjectFactory called nsp_type_atknoopobjectfactory
       */
      type->id =  nsp_type_atknoopobjectfactory_id = nsp_new_type_id();
      nsp_type_atknoopobjectfactory = type;
      if ( nsp_register_type(nsp_type_atknoopobjectfactory) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atknoopobjectfactory, ATK_TYPE_NO_OP_OBJECT_FACTORY);
      return ( mode == T_BASE ) ? type : new_type_atknoopobjectfactory(mode);
    }
  else 
    {
       type->id = nsp_type_atknoopobjectfactory_id;
       return type;
    }
}

/*
 * initialize NspAtkNoOpObjectFactory instances 
 * locally and by calling initializer on parent class 
 */

static int init_atknoopobjectfactory(NspAtkNoOpObjectFactory *Obj,NspTypeAtkNoOpObjectFactory *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkNoOpObjectFactory 
 */

NspAtkNoOpObjectFactory *new_atknoopobjectfactory() 
{
  NspAtkNoOpObjectFactory *loc; 
  /* type must exists */
  nsp_type_atknoopobjectfactory = new_type_atknoopobjectfactory(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkNoOpObjectFactory)))== NULLATKNOOPOBJECTFACTORY) return loc;
  /* initialize object */
  if ( init_atknoopobjectfactory(loc,nsp_type_atknoopobjectfactory) == FAIL) return NULLATKNOOPOBJECTFACTORY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkNoOpObjectFactory 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atknoopobjectfactory_type_name[]="AtkNoOpObjectFactory";
static char atknoopobjectfactory_short_type_name[]="AtkNoOpObjectFactory";

static char *nsp_atknoopobjectfactory_type_as_string(void)
{
  return(atknoopobjectfactory_type_name);
}

static char *nsp_atknoopobjectfactory_type_short_string(NspObject *v)
{
  return(atknoopobjectfactory_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkNoOpObjectFactory objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkNoOpObjectFactory *nsp_atknoopobjectfactory_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atknoopobjectfactory_id)  == TRUE  ) return ((NspAtkNoOpObjectFactory *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atknoopobjectfactory));
  return NULL;
}

int IsAtkNoOpObjectFactoryObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atknoopobjectfactory_id);
}

int IsAtkNoOpObjectFactory(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atknoopobjectfactory_id);
}

NspAtkNoOpObjectFactory  *GetAtkNoOpObjectFactoryCopy(Stack stack, int i)
{
  if (  GetAtkNoOpObjectFactory(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkNoOpObjectFactory  *GetAtkNoOpObjectFactory(Stack stack, int i)
{
  NspAtkNoOpObjectFactory *M;
  if (( M = nsp_atknoopobjectfactory_object(NthObj(i))) == NULLATKNOOPOBJECTFACTORY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkNoOpObjectFactory *atknoopobjectfactory_copy(NspAtkNoOpObjectFactory *self)
{
  /* return atkobjectfactory_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atknoopobjectfactory);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atknoopobjectfactory);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkNoOpObjectFactory
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int
_wrap_atknoopobjectfactory_new(Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)atk_no_op_object_factory_new())== NULL) return RET_BUG;

  nsp_type_atknoopobjectfactory = new_type_atknoopobjectfactory(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_atknoopobjectfactory );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *atknoopobjectfactory_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atknoopobjectfactory_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkRegistry ----------- */


#define  AtkRegistry_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkregistry.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkRegistry inherits from GObject 
 */

int nsp_type_atkregistry_id=0;
NspTypeAtkRegistry *nsp_type_atkregistry=NULL;

/*
 * Type object for NspAtkRegistry 
 * all the instance of NspTypeAtkRegistry share the same id. 
 * nsp_type_atkregistry: is an instance of NspTypeAtkRegistry 
 *    used for objects of NspAtkRegistry type (i.e built with new_atkregistry) 
 * other instances are used for derived classes 
 */
NspTypeAtkRegistry *new_type_atkregistry(type_mode mode)
{
  NspTypeAtkRegistry *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkregistry != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkregistry;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkregistry_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atkregistry_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atkregistry;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atkregistry */ 

  top->s_type =  (s_type_func *) nsp_atkregistry_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atkregistry_type_short_string;
  /* top->create = (create_func*) int_atkregistry_create;*/ 
  
  /* specific methods for atkregistry */
      
  type->init = (init_func *) init_atkregistry;

  /* 
   * NspAtkRegistry interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkregistry_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkRegistry called nsp_type_atkregistry
       */
      type->id =  nsp_type_atkregistry_id = nsp_new_type_id();
      nsp_type_atkregistry = type;
      if ( nsp_register_type(nsp_type_atkregistry) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkregistry, ATK_TYPE_REGISTRY);
      return ( mode == T_BASE ) ? type : new_type_atkregistry(mode);
    }
  else 
    {
       type->id = nsp_type_atkregistry_id;
       return type;
    }
}

/*
 * initialize NspAtkRegistry instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkregistry(NspAtkRegistry *Obj,NspTypeAtkRegistry *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkRegistry 
 */

NspAtkRegistry *new_atkregistry() 
{
  NspAtkRegistry *loc; 
  /* type must exists */
  nsp_type_atkregistry = new_type_atkregistry(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkRegistry)))== NULLATKREGISTRY) return loc;
  /* initialize object */
  if ( init_atkregistry(loc,nsp_type_atkregistry) == FAIL) return NULLATKREGISTRY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkRegistry 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atkregistry_type_name[]="AtkRegistry";
static char atkregistry_short_type_name[]="AtkRegistry";

static char *nsp_atkregistry_type_as_string(void)
{
  return(atkregistry_type_name);
}

static char *nsp_atkregistry_type_short_string(NspObject *v)
{
  return(atkregistry_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkRegistry objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkRegistry *nsp_atkregistry_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkregistry_id)  == TRUE  ) return ((NspAtkRegistry *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkregistry));
  return NULL;
}

int IsAtkRegistryObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atkregistry_id);
}

int IsAtkRegistry(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkregistry_id);
}

NspAtkRegistry  *GetAtkRegistryCopy(Stack stack, int i)
{
  if (  GetAtkRegistry(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkRegistry  *GetAtkRegistry(Stack stack, int i)
{
  NspAtkRegistry *M;
  if (( M = nsp_atkregistry_object(NthObj(i))) == NULLATKREGISTRY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkRegistry *atkregistry_copy(NspAtkRegistry *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkregistry);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkregistry);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkRegistry
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int _wrap_atk_registry_set_factory_type(NspAtkRegistry *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, obj,t_end};
  GType type, factory_type;
  NspObject *nsp_type = NULL, *nsp_factory_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type, &nsp_factory_type) == FAIL) return RET_BUG;
  if ((type = nspg_type_from_object(nsp_type)) == FAIL)
      return RET_BUG;
  if ((factory_type = nspg_type_from_object(nsp_factory_type)) == FAIL)
      return RET_BUG;
  atk_registry_set_factory_type(ATK_REGISTRY(self->obj), type, factory_type);
  return 0;
}

static int _wrap_atk_registry_get_factory_type(NspAtkRegistry *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GType type, ret;
  NspObject *nsp_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if ((type = nspg_type_from_object(nsp_type)) == FAIL)
      return RET_BUG;
  ret = atk_registry_get_factory_type(ATK_REGISTRY(self->obj), type);
  return nspg_type_wrapper_new(ret);
}

static int _wrap_atk_registry_get_factory(NspAtkRegistry *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GType type;
  NspObject *nsp_type = NULL, *nsp_ret;
  AtkObjectFactory *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if ((type = nspg_type_from_object(nsp_type)) == FAIL)
      return RET_BUG;
  ret = atk_registry_get_factory(ATK_REGISTRY(self->obj), type);
  nsp_type_atkobjectfactory = new_type_atkobjectfactory(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobjectfactory))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods atkregistry_methods[] = {
  {"set_factory_type",(nsp_method *) _wrap_atk_registry_set_factory_type},
  {"get_factory_type",(nsp_method *) _wrap_atk_registry_get_factory_type},
  {"get_factory",(nsp_method *) _wrap_atk_registry_get_factory},
  { NULL, NULL}
};

static NspMethods *atkregistry_get_methods(void) { return atkregistry_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkregistry_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkRelation ----------- */


#define  AtkRelation_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkrelation.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkRelation inherits from GObject 
 */

int nsp_type_atkrelation_id=0;
NspTypeAtkRelation *nsp_type_atkrelation=NULL;

/*
 * Type object for NspAtkRelation 
 * all the instance of NspTypeAtkRelation share the same id. 
 * nsp_type_atkrelation: is an instance of NspTypeAtkRelation 
 *    used for objects of NspAtkRelation type (i.e built with new_atkrelation) 
 * other instances are used for derived classes 
 */
NspTypeAtkRelation *new_type_atkrelation(type_mode mode)
{
  NspTypeAtkRelation *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkrelation != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkrelation;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkrelation_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atkrelation_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atkrelation;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atkrelation */ 

  top->s_type =  (s_type_func *) nsp_atkrelation_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atkrelation_type_short_string;
  /* top->create = (create_func*) int_atkrelation_create;*/ 
  
  /* specific methods for atkrelation */
      
  type->init = (init_func *) init_atkrelation;

  /* 
   * NspAtkRelation interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkrelation_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkRelation called nsp_type_atkrelation
       */
      type->id =  nsp_type_atkrelation_id = nsp_new_type_id();
      nsp_type_atkrelation = type;
      if ( nsp_register_type(nsp_type_atkrelation) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkrelation, ATK_TYPE_RELATION);
      return ( mode == T_BASE ) ? type : new_type_atkrelation(mode);
    }
  else 
    {
       type->id = nsp_type_atkrelation_id;
       return type;
    }
}

/*
 * initialize NspAtkRelation instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkrelation(NspAtkRelation *Obj,NspTypeAtkRelation *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkRelation 
 */

NspAtkRelation *new_atkrelation() 
{
  NspAtkRelation *loc; 
  /* type must exists */
  nsp_type_atkrelation = new_type_atkrelation(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkRelation)))== NULLATKRELATION) return loc;
  /* initialize object */
  if ( init_atkrelation(loc,nsp_type_atkrelation) == FAIL) return NULLATKRELATION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkRelation 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atkrelation_type_name[]="AtkRelation";
static char atkrelation_short_type_name[]="AtkRelation";

static char *nsp_atkrelation_type_as_string(void)
{
  return(atkrelation_type_name);
}

static char *nsp_atkrelation_type_short_string(NspObject *v)
{
  return(atkrelation_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkRelation objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkRelation *nsp_atkrelation_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkrelation_id)  == TRUE  ) return ((NspAtkRelation *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkrelation));
  return NULL;
}

int IsAtkRelationObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atkrelation_id);
}

int IsAtkRelation(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkrelation_id);
}

NspAtkRelation  *GetAtkRelationCopy(Stack stack, int i)
{
  if (  GetAtkRelation(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkRelation  *GetAtkRelation(Stack stack, int i)
{
  NspAtkRelation *M;
  if (( M = nsp_atkrelation_object(NthObj(i))) == NULLATKRELATION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkRelation *atkrelation_copy(NspAtkRelation *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkrelation);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkrelation);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkRelation
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

#line 18 "atk.override"
static int
_wrap_atkrelation_new (Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {list, s_int , t_end} ;
  /* targets" "relationship" */
  AtkObject **targets;
  int relationship, count, i=0;
  NspList  *nsp_targets;
  Cell *cloc; 
  NspObject *nsp_ret;
  GObject *ret;

  if (GetArgs(stack,rhs,opt,T,&nsp_targets, &relationship) == FAIL) return RET_BUG;

  count =nsp_list_length(nsp_targets);
  targets = g_new(AtkObject *, count);
    
  cloc = nsp_targets->first ;
  while ( cloc != NULLCELL) 
    {
      if ( cloc->O == NULLOBJ || nspgobject_check(cloc->O, &nsp_type_atkobject) == FAIL) 
	{
	  Scierror( "targets argument must be a sequence of AtkObjects.");
	  g_free(targets);
	}
      else 
	{
	  targets[i] = (AtkObject *) nspgobject_get(cloc->O);
	  i++;
	}
    }

  if ((ret = (GObject *)atk_relation_new(targets, count, relationship)) == NULL) return RET_BUG;
  g_free(targets);
  nsp_type_atkrelation = new_type_atkrelation(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_atkrelation );
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}
#line 1823 "atk.c"


static int _wrap_atk_relation_get_relation_type(NspAtkRelation *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
  ret = atk_relation_get_relation_type(ATK_RELATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods atkrelation_methods[] = {
  {"get_relation_type",(nsp_method *) _wrap_atk_relation_get_relation_type},
  { NULL, NULL}
};

static NspMethods *atkrelation_get_methods(void) { return atkrelation_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkrelation_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkRelationSet ----------- */


#define  AtkRelationSet_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkrelationset.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkRelationSet inherits from GObject 
 */

int nsp_type_atkrelationset_id=0;
NspTypeAtkRelationSet *nsp_type_atkrelationset=NULL;

/*
 * Type object for NspAtkRelationSet 
 * all the instance of NspTypeAtkRelationSet share the same id. 
 * nsp_type_atkrelationset: is an instance of NspTypeAtkRelationSet 
 *    used for objects of NspAtkRelationSet type (i.e built with new_atkrelationset) 
 * other instances are used for derived classes 
 */
NspTypeAtkRelationSet *new_type_atkrelationset(type_mode mode)
{
  NspTypeAtkRelationSet *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkrelationset != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkrelationset;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkrelationset_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atkrelationset_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atkrelationset;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atkrelationset */ 

  top->s_type =  (s_type_func *) nsp_atkrelationset_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atkrelationset_type_short_string;
  /* top->create = (create_func*) int_atkrelationset_create;*/ 
  
  /* specific methods for atkrelationset */
      
  type->init = (init_func *) init_atkrelationset;

  /* 
   * NspAtkRelationSet interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkrelationset_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkRelationSet called nsp_type_atkrelationset
       */
      type->id =  nsp_type_atkrelationset_id = nsp_new_type_id();
      nsp_type_atkrelationset = type;
      if ( nsp_register_type(nsp_type_atkrelationset) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkrelationset, ATK_TYPE_RELATION_SET);
      return ( mode == T_BASE ) ? type : new_type_atkrelationset(mode);
    }
  else 
    {
       type->id = nsp_type_atkrelationset_id;
       return type;
    }
}

/*
 * initialize NspAtkRelationSet instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkrelationset(NspAtkRelationSet *Obj,NspTypeAtkRelationSet *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkRelationSet 
 */

NspAtkRelationSet *new_atkrelationset() 
{
  NspAtkRelationSet *loc; 
  /* type must exists */
  nsp_type_atkrelationset = new_type_atkrelationset(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkRelationSet)))== NULLATKRELATIONSET) return loc;
  /* initialize object */
  if ( init_atkrelationset(loc,nsp_type_atkrelationset) == FAIL) return NULLATKRELATIONSET;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkRelationSet 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atkrelationset_type_name[]="AtkRelationSet";
static char atkrelationset_short_type_name[]="AtkRelationSet";

static char *nsp_atkrelationset_type_as_string(void)
{
  return(atkrelationset_type_name);
}

static char *nsp_atkrelationset_type_short_string(NspObject *v)
{
  return(atkrelationset_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkRelationSet objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkRelationSet *nsp_atkrelationset_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkrelationset_id)  == TRUE  ) return ((NspAtkRelationSet *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkrelationset));
  return NULL;
}

int IsAtkRelationSetObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atkrelationset_id);
}

int IsAtkRelationSet(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkrelationset_id);
}

NspAtkRelationSet  *GetAtkRelationSetCopy(Stack stack, int i)
{
  if (  GetAtkRelationSet(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkRelationSet  *GetAtkRelationSet(Stack stack, int i)
{
  NspAtkRelationSet *M;
  if (( M = nsp_atkrelationset_object(NthObj(i))) == NULLATKRELATIONSET)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkRelationSet *atkrelationset_copy(NspAtkRelationSet *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkrelationset);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkrelationset);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkRelationSet
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int
_wrap_atkrelationset_new(Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)atk_relation_set_new())== NULL) return RET_BUG;

  nsp_type_atkrelationset = new_type_atkrelationset(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_atkrelationset );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_relation_set_contains(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  AtkRelationType relationship;
  NspObject *nsp_relationship = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_relationship) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_relationship, &relationship)== FAIL)
      return RET_BUG;
  ret = atk_relation_set_contains(ATK_RELATION_SET(self->obj), relationship);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_relation_set_remove(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *relation;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkrelation, &relation) == FAIL) return RET_BUG;
  atk_relation_set_remove(ATK_RELATION_SET(self->obj), ATK_RELATION(relation->obj));
  return 0;
}

static int _wrap_atk_relation_set_add(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *relation;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkrelation, &relation) == FAIL) return RET_BUG;
  atk_relation_set_add(ATK_RELATION_SET(self->obj), ATK_RELATION(relation->obj));
  return 0;
}

static int _wrap_atk_relation_set_get_n_relations(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_relation_set_get_n_relations(ATK_RELATION_SET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_relation_set_get_relation(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int i;
  AtkRelation *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
  ret = atk_relation_set_get_relation(ATK_RELATION_SET(self->obj), i);
  nsp_type_atkrelation = new_type_atkrelation(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkrelation))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_relation_set_get_relation_by_type(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  AtkRelationType relationship;
  NspObject *nsp_relationship = NULL, *nsp_ret;
  AtkRelation *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_relationship) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_relationship, &relationship)== FAIL)
      return RET_BUG;
  ret = atk_relation_set_get_relation_by_type(ATK_RELATION_SET(self->obj), relationship);
  nsp_type_atkrelation = new_type_atkrelation(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkrelation))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods atkrelationset_methods[] = {
  {"contains",(nsp_method *) _wrap_atk_relation_set_contains},
  {"remove",(nsp_method *) _wrap_atk_relation_set_remove},
  {"add",(nsp_method *) _wrap_atk_relation_set_add},
  {"get_n_relations",(nsp_method *) _wrap_atk_relation_set_get_n_relations},
  {"get_relation",(nsp_method *) _wrap_atk_relation_set_get_relation},
  {"get_relation_by_type",(nsp_method *) _wrap_atk_relation_set_get_relation_by_type},
  { NULL, NULL}
};

static NspMethods *atkrelationset_get_methods(void) { return atkrelationset_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkrelationset_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkStateSet ----------- */


#define  AtkStateSet_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkstateset.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkStateSet inherits from GObject 
 */

int nsp_type_atkstateset_id=0;
NspTypeAtkStateSet *nsp_type_atkstateset=NULL;

/*
 * Type object for NspAtkStateSet 
 * all the instance of NspTypeAtkStateSet share the same id. 
 * nsp_type_atkstateset: is an instance of NspTypeAtkStateSet 
 *    used for objects of NspAtkStateSet type (i.e built with new_atkstateset) 
 * other instances are used for derived classes 
 */
NspTypeAtkStateSet *new_type_atkstateset(type_mode mode)
{
  NspTypeAtkStateSet *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkstateset != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkstateset;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkstateset_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atkstateset_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atkstateset;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atkstateset */ 

  top->s_type =  (s_type_func *) nsp_atkstateset_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atkstateset_type_short_string;
  /* top->create = (create_func*) int_atkstateset_create;*/ 
  
  /* specific methods for atkstateset */
      
  type->init = (init_func *) init_atkstateset;

  /* 
   * NspAtkStateSet interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkstateset_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkStateSet called nsp_type_atkstateset
       */
      type->id =  nsp_type_atkstateset_id = nsp_new_type_id();
      nsp_type_atkstateset = type;
      if ( nsp_register_type(nsp_type_atkstateset) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkstateset, ATK_TYPE_STATE_SET);
      return ( mode == T_BASE ) ? type : new_type_atkstateset(mode);
    }
  else 
    {
       type->id = nsp_type_atkstateset_id;
       return type;
    }
}

/*
 * initialize NspAtkStateSet instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkstateset(NspAtkStateSet *Obj,NspTypeAtkStateSet *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkStateSet 
 */

NspAtkStateSet *new_atkstateset() 
{
  NspAtkStateSet *loc; 
  /* type must exists */
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkStateSet)))== NULLATKSTATESET) return loc;
  /* initialize object */
  if ( init_atkstateset(loc,nsp_type_atkstateset) == FAIL) return NULLATKSTATESET;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkStateSet 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atkstateset_type_name[]="AtkStateSet";
static char atkstateset_short_type_name[]="AtkStateSet";

static char *nsp_atkstateset_type_as_string(void)
{
  return(atkstateset_type_name);
}

static char *nsp_atkstateset_type_short_string(NspObject *v)
{
  return(atkstateset_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkStateSet objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkStateSet *nsp_atkstateset_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkstateset_id)  == TRUE  ) return ((NspAtkStateSet *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkstateset));
  return NULL;
}

int IsAtkStateSetObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atkstateset_id);
}

int IsAtkStateSet(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkstateset_id);
}

NspAtkStateSet  *GetAtkStateSetCopy(Stack stack, int i)
{
  if (  GetAtkStateSet(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkStateSet  *GetAtkStateSet(Stack stack, int i)
{
  NspAtkStateSet *M;
  if (( M = nsp_atkstateset_object(NthObj(i))) == NULLATKSTATESET)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkStateSet *atkstateset_copy(NspAtkStateSet *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkstateset);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkstateset);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkStateSet
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int _wrap_atk_state_set_is_empty(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = atk_state_set_is_empty(ATK_STATE_SET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_state_set_add_state(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  AtkStateType type;
  NspObject *nsp_type = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_type, &type)== FAIL)
      return RET_BUG;
  ret = atk_state_set_add_state(ATK_STATE_SET(self->obj), type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_state_set_clear_states(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  atk_state_set_clear_states(ATK_STATE_SET(self->obj));
  return 0;
}

static int _wrap_atk_state_set_contains_state(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  AtkStateType type;
  NspObject *nsp_type = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_type, &type)== FAIL)
      return RET_BUG;
  ret = atk_state_set_contains_state(ATK_STATE_SET(self->obj), type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_state_set_remove_state(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  AtkStateType type;
  NspObject *nsp_type = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_type, &type)== FAIL)
      return RET_BUG;
  ret = atk_state_set_remove_state(ATK_STATE_SET(self->obj), type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_state_set_and_sets(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *compare_set;
  AtkStateSet *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkstateset, &compare_set) == FAIL) return RET_BUG;
  ret = atk_state_set_and_sets(ATK_STATE_SET(self->obj), ATK_STATE_SET(compare_set->obj));
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkstateset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_state_set_or_sets(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *compare_set;
  AtkStateSet *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkstateset, &compare_set) == FAIL) return RET_BUG;
  ret = atk_state_set_or_sets(ATK_STATE_SET(self->obj), ATK_STATE_SET(compare_set->obj));
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkstateset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_state_set_xor_sets(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *compare_set;
  AtkStateSet *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkstateset, &compare_set) == FAIL) return RET_BUG;
  ret = atk_state_set_xor_sets(ATK_STATE_SET(self->obj), ATK_STATE_SET(compare_set->obj));
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkstateset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods atkstateset_methods[] = {
  {"is_empty",(nsp_method *) _wrap_atk_state_set_is_empty},
  {"add_state",(nsp_method *) _wrap_atk_state_set_add_state},
  {"clear_states",(nsp_method *) _wrap_atk_state_set_clear_states},
  {"contains_state",(nsp_method *) _wrap_atk_state_set_contains_state},
  {"remove_state",(nsp_method *) _wrap_atk_state_set_remove_state},
  {"and_sets",(nsp_method *) _wrap_atk_state_set_and_sets},
  {"or_sets",(nsp_method *) _wrap_atk_state_set_or_sets},
  {"xor_sets",(nsp_method *) _wrap_atk_state_set_xor_sets},
  { NULL, NULL}
};

static NspMethods *atkstateset_get_methods(void) { return atkstateset_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkstateset_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- NspAtkUtil ----------- */


#define  AtkUtil_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkutil.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspAtkUtil inherits from GObject 
 */

int nsp_type_atkutil_id=0;
NspTypeAtkUtil *nsp_type_atkutil=NULL;

/*
 * Type object for NspAtkUtil 
 * all the instance of NspTypeAtkUtil share the same id. 
 * nsp_type_atkutil: is an instance of NspTypeAtkUtil 
 *    used for objects of NspAtkUtil type (i.e built with new_atkutil) 
 * other instances are used for derived classes 
 */
NspTypeAtkUtil *new_type_atkutil(type_mode mode)
{
  NspTypeAtkUtil *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkutil != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkutil;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkutil_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = atkutil_get_methods; 
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_atkutil;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for atkutil */ 

  top->s_type =  (s_type_func *) nsp_atkutil_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_atkutil_type_short_string;
  /* top->create = (create_func*) int_atkutil_create;*/ 
  
  /* specific methods for atkutil */
      
  type->init = (init_func *) init_atkutil;

  /* 
   * NspAtkUtil interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkutil_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkUtil called nsp_type_atkutil
       */
      type->id =  nsp_type_atkutil_id = nsp_new_type_id();
      nsp_type_atkutil = type;
      if ( nsp_register_type(nsp_type_atkutil) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkutil, ATK_TYPE_UTIL);
      return ( mode == T_BASE ) ? type : new_type_atkutil(mode);
    }
  else 
    {
       type->id = nsp_type_atkutil_id;
       return type;
    }
}

/*
 * initialize NspAtkUtil instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkutil(NspAtkUtil *Obj,NspTypeAtkUtil *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspAtkUtil 
 */

NspAtkUtil *new_atkutil() 
{
  NspAtkUtil *loc; 
  /* type must exists */
  nsp_type_atkutil = new_type_atkutil(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkUtil)))== NULLATKUTIL) return loc;
  /* initialize object */
  if ( init_atkutil(loc,nsp_type_atkutil) == FAIL) return NULLATKUTIL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkUtil 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char atkutil_type_name[]="AtkUtil";
static char atkutil_short_type_name[]="AtkUtil";

static char *nsp_atkutil_type_as_string(void)
{
  return(atkutil_type_name);
}

static char *nsp_atkutil_type_short_string(NspObject *v)
{
  return(atkutil_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkUtil objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspAtkUtil *nsp_atkutil_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkutil_id)  == TRUE  ) return ((NspAtkUtil *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkutil));
  return NULL;
}

int IsAtkUtilObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_atkutil_id);
}

int IsAtkUtil(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkutil_id);
}

NspAtkUtil  *GetAtkUtilCopy(Stack stack, int i)
{
  if (  GetAtkUtil(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkUtil  *GetAtkUtil(Stack stack, int i)
{
  NspAtkUtil *M;
  if (( M = nsp_atkutil_object(NthObj(i))) == NULLATKUTIL)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkUtil *atkutil_copy(NspAtkUtil *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkutil);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkutil);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkUtil
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static NspMethods *atkutil_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkutil_attrs[]={{NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_atk_get_default_registry(Stack stack, int rhs, int opt, int lhs) /* get_default_registry */
{
  AtkRegistry *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
ret = atk_get_default_registry();
  nsp_type_atkregistry = new_type_atkregistry(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkregistry))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_atk_relation_type_register(Stack stack, int rhs, int opt, int lhs) /* relation_type_register */
{
  int_types T[] = {string,t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
ret = atk_relation_type_register(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_relation_type_for_name(Stack stack, int rhs, int opt, int lhs) /* relation_type_for_name */
{
  int_types T[] = {string,t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
ret = atk_relation_type_for_name(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_state_type_for_name(Stack stack, int rhs, int opt, int lhs) /* state_type_for_name */
{
  int_types T[] = {string,t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
ret = atk_state_type_for_name(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_text_attribute_get_name(Stack stack, int rhs, int opt, int lhs) /* text_attribute_get_name */
{
  int_types T[] = {obj,t_end};
  AtkTextAttribute attr;
  NspObject *nsp_attr = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_attr) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_attr, &attr)== FAIL)
      return RET_BUG;
ret = atk_text_attribute_get_name(attr);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_text_attribute_get_value(Stack stack, int rhs, int opt, int lhs) /* text_attribute_get_value */
{
  int_types T[] = {obj, s_int,t_end};
  AtkTextAttribute attr;
  NspObject *nsp_attr = NULL;
  int index;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_attr, &index) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_attr, &attr)== FAIL)
      return RET_BUG;
ret = atk_text_attribute_get_value(attr, index);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_remove_focus_tracker(Stack stack, int rhs, int opt, int lhs) /* remove_focus_tracker */
{
  int_types T[] = {s_int,t_end};
  int tracker_id;
  if ( GetArgs(stack,rhs,opt,T,&tracker_id) == FAIL) return RET_BUG;
atk_remove_focus_tracker(tracker_id);
  return 0;
}

int _wrap_atk_focus_tracker_notify(Stack stack, int rhs, int opt, int lhs) /* focus_tracker_notify */
{
  int_types T[] = {obj_check,t_end};
  NspGObject *object;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkobject, &object) == FAIL) return RET_BUG;
atk_focus_tracker_notify(ATK_OBJECT(object->obj));
  return 0;
}

int _wrap_atk_remove_global_event_listener(Stack stack, int rhs, int opt, int lhs) /* remove_global_event_listener */
{
  int_types T[] = {s_int,t_end};
  int listener_id;
  if ( GetArgs(stack,rhs,opt,T,&listener_id) == FAIL) return RET_BUG;
atk_remove_global_event_listener(listener_id);
  return 0;
}

int _wrap_atk_remove_key_event_listener(Stack stack, int rhs, int opt, int lhs) /* remove_key_event_listener */
{
  int_types T[] = {s_int,t_end};
  int listener_id;
  if ( GetArgs(stack,rhs,opt,T,&listener_id) == FAIL) return RET_BUG;
atk_remove_key_event_listener(listener_id);
  return 0;
}

int _wrap_atk_get_root(Stack stack, int rhs, int opt, int lhs) /* get_root */
{
  AtkObject *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
ret = atk_get_root();
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_atk_get_toolkit_name(Stack stack, int rhs, int opt, int lhs) /* get_toolkit_name */
{
  const gchar *ret;
  CheckRhs(0,0);
ret = atk_get_toolkit_name();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_get_toolkit_version(Stack stack, int rhs, int opt, int lhs) /* get_toolkit_version */
{
  const gchar *ret;
  CheckRhs(0,0);
ret = atk_get_toolkit_version();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_role_for_name(Stack stack, int rhs, int opt, int lhs) /* role_for_name */
{
  int_types T[] = {string,t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
ret = atk_role_for_name(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab atk_func[]={
  {"atknoopobject_new", _wrap_atknoopobject_new},
  {"atknoopobjectfactory_new", _wrap_atknoopobjectfactory_new},
  {"atkrelation_new", _wrap_atkrelation_new},
  {"atkrelationset_new", _wrap_atkrelationset_new},
  {"atk_get_default_registry", _wrap_atk_get_default_registry},
  {"atk_relation_type_register", _wrap_atk_relation_type_register},
  {"atk_relation_type_for_name", _wrap_atk_relation_type_for_name},
  {"atk_state_type_for_name", _wrap_atk_state_type_for_name},
  {"atk_text_attribute_get_name", _wrap_atk_text_attribute_get_name},
  {"atk_text_attribute_get_value", _wrap_atk_text_attribute_get_value},
  {"atk_remove_focus_tracker", _wrap_atk_remove_focus_tracker},
  {"atk_focus_tracker_notify", _wrap_atk_focus_tracker_notify},
  {"atk_remove_global_event_listener", _wrap_atk_remove_global_event_listener},
  {"atk_remove_key_event_listener", _wrap_atk_remove_key_event_listener},
  {"atk_get_root", _wrap_atk_get_root},
  {"atk_get_toolkit_name", _wrap_atk_get_toolkit_name},
  {"atk_get_toolkit_version", _wrap_atk_get_toolkit_version},
  {"atk_role_for_name", _wrap_atk_role_for_name},
  { NULL, NULL}
};

/* call ith function in the atk interface */

int atk_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,atk_func[i].fonc,
  					       &stack,rhs,opt,lhs);
#else
  return (*(atk_func[i].fonc))(stack,rhs,opt,lhs);
#endif
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void atk_Interf_Info(int i, char **fname, function (**f))
{
  *fname = atk_func[i].name;
  *f = atk_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
atk_add_constants(NspObject *module, const gchar *strip_prefix)
{
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_INVALID", strip_prefix), ATK_ROLE_INVALID);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_ACCEL_LABEL", strip_prefix), ATK_ROLE_ACCEL_LABEL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_ALERT", strip_prefix), ATK_ROLE_ALERT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_ANIMATION", strip_prefix), ATK_ROLE_ANIMATION);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_ARROW", strip_prefix), ATK_ROLE_ARROW);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_CALENDAR", strip_prefix), ATK_ROLE_CALENDAR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_CANVAS", strip_prefix), ATK_ROLE_CANVAS);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_CHECK_BOX", strip_prefix), ATK_ROLE_CHECK_BOX);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_CHECK_MENU_ITEM", strip_prefix), ATK_ROLE_CHECK_MENU_ITEM);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_COLOR_CHOOSER", strip_prefix), ATK_ROLE_COLOR_CHOOSER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_COLUMN_HEADER", strip_prefix), ATK_ROLE_COLUMN_HEADER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_COMBO_BOX", strip_prefix), ATK_ROLE_COMBO_BOX);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_DATE_EDITOR", strip_prefix), ATK_ROLE_DATE_EDITOR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_DESKTOP_ICON", strip_prefix), ATK_ROLE_DESKTOP_ICON);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_DESKTOP_FRAME", strip_prefix), ATK_ROLE_DESKTOP_FRAME);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_DIAL", strip_prefix), ATK_ROLE_DIAL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_DIALOG", strip_prefix), ATK_ROLE_DIALOG);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_DIRECTORY_PANE", strip_prefix), ATK_ROLE_DIRECTORY_PANE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_DRAWING_AREA", strip_prefix), ATK_ROLE_DRAWING_AREA);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_FILE_CHOOSER", strip_prefix), ATK_ROLE_FILE_CHOOSER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_FILLER", strip_prefix), ATK_ROLE_FILLER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_FONT_CHOOSER", strip_prefix), ATK_ROLE_FONT_CHOOSER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_FRAME", strip_prefix), ATK_ROLE_FRAME);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_GLASS_PANE", strip_prefix), ATK_ROLE_GLASS_PANE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_HTML_CONTAINER", strip_prefix), ATK_ROLE_HTML_CONTAINER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_ICON", strip_prefix), ATK_ROLE_ICON);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_IMAGE", strip_prefix), ATK_ROLE_IMAGE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_INTERNAL_FRAME", strip_prefix), ATK_ROLE_INTERNAL_FRAME);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_LABEL", strip_prefix), ATK_ROLE_LABEL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_LAYERED_PANE", strip_prefix), ATK_ROLE_LAYERED_PANE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_LIST", strip_prefix), ATK_ROLE_LIST);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_LIST_ITEM", strip_prefix), ATK_ROLE_LIST_ITEM);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_MENU", strip_prefix), ATK_ROLE_MENU);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_MENU_BAR", strip_prefix), ATK_ROLE_MENU_BAR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_MENU_ITEM", strip_prefix), ATK_ROLE_MENU_ITEM);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_OPTION_PANE", strip_prefix), ATK_ROLE_OPTION_PANE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_PAGE_TAB", strip_prefix), ATK_ROLE_PAGE_TAB);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_PAGE_TAB_LIST", strip_prefix), ATK_ROLE_PAGE_TAB_LIST);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_PANEL", strip_prefix), ATK_ROLE_PANEL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_PASSWORD_TEXT", strip_prefix), ATK_ROLE_PASSWORD_TEXT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_POPUP_MENU", strip_prefix), ATK_ROLE_POPUP_MENU);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_PROGRESS_BAR", strip_prefix), ATK_ROLE_PROGRESS_BAR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_PUSH_BUTTON", strip_prefix), ATK_ROLE_PUSH_BUTTON);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_RADIO_BUTTON", strip_prefix), ATK_ROLE_RADIO_BUTTON);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_RADIO_MENU_ITEM", strip_prefix), ATK_ROLE_RADIO_MENU_ITEM);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_ROOT_PANE", strip_prefix), ATK_ROLE_ROOT_PANE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_ROW_HEADER", strip_prefix), ATK_ROLE_ROW_HEADER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_SCROLL_BAR", strip_prefix), ATK_ROLE_SCROLL_BAR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_SCROLL_PANE", strip_prefix), ATK_ROLE_SCROLL_PANE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_SEPARATOR", strip_prefix), ATK_ROLE_SEPARATOR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_SLIDER", strip_prefix), ATK_ROLE_SLIDER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_SPLIT_PANE", strip_prefix), ATK_ROLE_SPLIT_PANE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_SPIN_BUTTON", strip_prefix), ATK_ROLE_SPIN_BUTTON);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_STATUSBAR", strip_prefix), ATK_ROLE_STATUSBAR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TABLE", strip_prefix), ATK_ROLE_TABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TABLE_CELL", strip_prefix), ATK_ROLE_TABLE_CELL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TABLE_COLUMN_HEADER", strip_prefix), ATK_ROLE_TABLE_COLUMN_HEADER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TABLE_ROW_HEADER", strip_prefix), ATK_ROLE_TABLE_ROW_HEADER);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TEAR_OFF_MENU_ITEM", strip_prefix), ATK_ROLE_TEAR_OFF_MENU_ITEM);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TERMINAL", strip_prefix), ATK_ROLE_TERMINAL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TEXT", strip_prefix), ATK_ROLE_TEXT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TOGGLE_BUTTON", strip_prefix), ATK_ROLE_TOGGLE_BUTTON);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TOOL_BAR", strip_prefix), ATK_ROLE_TOOL_BAR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TOOL_TIP", strip_prefix), ATK_ROLE_TOOL_TIP);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TREE", strip_prefix), ATK_ROLE_TREE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_TREE_TABLE", strip_prefix), ATK_ROLE_TREE_TABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_UNKNOWN", strip_prefix), ATK_ROLE_UNKNOWN);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_VIEWPORT", strip_prefix), ATK_ROLE_VIEWPORT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_WINDOW", strip_prefix), ATK_ROLE_WINDOW);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_ROLE_LAST_DEFINED", strip_prefix), ATK_ROLE_LAST_DEFINED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_LAYER_INVALID", strip_prefix), ATK_LAYER_INVALID);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_LAYER_BACKGROUND", strip_prefix), ATK_LAYER_BACKGROUND);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_LAYER_CANVAS", strip_prefix), ATK_LAYER_CANVAS);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_LAYER_WIDGET", strip_prefix), ATK_LAYER_WIDGET);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_LAYER_MDI", strip_prefix), ATK_LAYER_MDI);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_LAYER_POPUP", strip_prefix), ATK_LAYER_POPUP);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_LAYER_OVERLAY", strip_prefix), ATK_LAYER_OVERLAY);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_RELATION_NULL", strip_prefix), ATK_RELATION_NULL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_RELATION_CONTROLLED_BY", strip_prefix), ATK_RELATION_CONTROLLED_BY);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_RELATION_CONTROLLER_FOR", strip_prefix), ATK_RELATION_CONTROLLER_FOR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_RELATION_LABEL_FOR", strip_prefix), ATK_RELATION_LABEL_FOR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_RELATION_LABELLED_BY", strip_prefix), ATK_RELATION_LABELLED_BY);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_RELATION_MEMBER_OF", strip_prefix), ATK_RELATION_MEMBER_OF);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_RELATION_NODE_CHILD_OF", strip_prefix), ATK_RELATION_NODE_CHILD_OF);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_RELATION_LAST_DEFINED", strip_prefix), ATK_RELATION_LAST_DEFINED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_INVALID", strip_prefix), ATK_STATE_INVALID);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_ACTIVE", strip_prefix), ATK_STATE_ACTIVE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_ARMED", strip_prefix), ATK_STATE_ARMED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_BUSY", strip_prefix), ATK_STATE_BUSY);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_CHECKED", strip_prefix), ATK_STATE_CHECKED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_DEFUNCT", strip_prefix), ATK_STATE_DEFUNCT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_EDITABLE", strip_prefix), ATK_STATE_EDITABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_ENABLED", strip_prefix), ATK_STATE_ENABLED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_EXPANDABLE", strip_prefix), ATK_STATE_EXPANDABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_EXPANDED", strip_prefix), ATK_STATE_EXPANDED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_FOCUSABLE", strip_prefix), ATK_STATE_FOCUSABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_FOCUSED", strip_prefix), ATK_STATE_FOCUSED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_HORIZONTAL", strip_prefix), ATK_STATE_HORIZONTAL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_ICONIFIED", strip_prefix), ATK_STATE_ICONIFIED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_MODAL", strip_prefix), ATK_STATE_MODAL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_MULTI_LINE", strip_prefix), ATK_STATE_MULTI_LINE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_MULTISELECTABLE", strip_prefix), ATK_STATE_MULTISELECTABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_OPAQUE", strip_prefix), ATK_STATE_OPAQUE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_PRESSED", strip_prefix), ATK_STATE_PRESSED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_RESIZABLE", strip_prefix), ATK_STATE_RESIZABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_SELECTABLE", strip_prefix), ATK_STATE_SELECTABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_SELECTED", strip_prefix), ATK_STATE_SELECTED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_SENSITIVE", strip_prefix), ATK_STATE_SENSITIVE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_SHOWING", strip_prefix), ATK_STATE_SHOWING);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_SINGLE_LINE", strip_prefix), ATK_STATE_SINGLE_LINE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_STALE", strip_prefix), ATK_STATE_STALE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_TRANSIENT", strip_prefix), ATK_STATE_TRANSIENT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_VERTICAL", strip_prefix), ATK_STATE_VERTICAL);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_VISIBLE", strip_prefix), ATK_STATE_VISIBLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_STATE_LAST_DEFINED", strip_prefix), ATK_STATE_LAST_DEFINED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_LEFT_MARGIN", strip_prefix), ATK_TEXT_ATTR_LEFT_MARGIN);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_RIGHT_MARGIN", strip_prefix), ATK_TEXT_ATTR_RIGHT_MARGIN);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_INDENT", strip_prefix), ATK_TEXT_ATTR_INDENT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_INVISIBLE", strip_prefix), ATK_TEXT_ATTR_INVISIBLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_EDITABLE", strip_prefix), ATK_TEXT_ATTR_EDITABLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_PIXELS_ABOVE_LINES", strip_prefix), ATK_TEXT_ATTR_PIXELS_ABOVE_LINES);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_PIXELS_BELOW_LINES", strip_prefix), ATK_TEXT_ATTR_PIXELS_BELOW_LINES);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_PIXELS_INSIDE_WRAP", strip_prefix), ATK_TEXT_ATTR_PIXELS_INSIDE_WRAP);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_BG_FULL_HEIGHT", strip_prefix), ATK_TEXT_ATTR_BG_FULL_HEIGHT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_RISE", strip_prefix), ATK_TEXT_ATTR_RISE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_UNDERLINE", strip_prefix), ATK_TEXT_ATTR_UNDERLINE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_STRIKETHROUGH", strip_prefix), ATK_TEXT_ATTR_STRIKETHROUGH);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_SIZE", strip_prefix), ATK_TEXT_ATTR_SIZE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_SCALE", strip_prefix), ATK_TEXT_ATTR_SCALE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_WEIGHT", strip_prefix), ATK_TEXT_ATTR_WEIGHT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_LANGUAGE", strip_prefix), ATK_TEXT_ATTR_LANGUAGE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_FAMILY_NAME", strip_prefix), ATK_TEXT_ATTR_FAMILY_NAME);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_BG_COLOR", strip_prefix), ATK_TEXT_ATTR_BG_COLOR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_FG_COLOR", strip_prefix), ATK_TEXT_ATTR_FG_COLOR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_BG_STIPPLE", strip_prefix), ATK_TEXT_ATTR_BG_STIPPLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_FG_STIPPLE", strip_prefix), ATK_TEXT_ATTR_FG_STIPPLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_WRAP_MODE", strip_prefix), ATK_TEXT_ATTR_WRAP_MODE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_DIRECTION", strip_prefix), ATK_TEXT_ATTR_DIRECTION);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_JUSTIFICATION", strip_prefix), ATK_TEXT_ATTR_JUSTIFICATION);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_STRETCH", strip_prefix), ATK_TEXT_ATTR_STRETCH);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_VARIANT", strip_prefix), ATK_TEXT_ATTR_VARIANT);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_ATTR_STYLE", strip_prefix), ATK_TEXT_ATTR_STYLE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_BOUNDARY_CHAR", strip_prefix), ATK_TEXT_BOUNDARY_CHAR);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_BOUNDARY_WORD_START", strip_prefix), ATK_TEXT_BOUNDARY_WORD_START);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_BOUNDARY_WORD_END", strip_prefix), ATK_TEXT_BOUNDARY_WORD_END);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_BOUNDARY_SENTENCE_START", strip_prefix), ATK_TEXT_BOUNDARY_SENTENCE_START);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_BOUNDARY_SENTENCE_END", strip_prefix), ATK_TEXT_BOUNDARY_SENTENCE_END);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_BOUNDARY_LINE_START", strip_prefix), ATK_TEXT_BOUNDARY_LINE_START);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_TEXT_BOUNDARY_LINE_END", strip_prefix), ATK_TEXT_BOUNDARY_LINE_END);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_KEY_EVENT_PRESS", strip_prefix), ATK_KEY_EVENT_PRESS);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_KEY_EVENT_RELEASE", strip_prefix), ATK_KEY_EVENT_RELEASE);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_KEY_EVENT_LAST_DEFINED", strip_prefix), ATK_KEY_EVENT_LAST_DEFINED);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_XY_SCREEN", strip_prefix), ATK_XY_SCREEN);*/
/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix("ATK_XY_WINDOW", strip_prefix), ATK_XY_WINDOW);*/
}

