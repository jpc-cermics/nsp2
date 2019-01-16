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





#line 4 "codegen-3.0/atk.override"
#include <atk/atk.h>
#include <atk/atknoopobjectfactory.h>
#include <atk/atknoopobject.h>

#include <nsp/nsp.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/hash.h>
#include <nsp/list.h>
#include <nsp/cells.h>
#include <nsp/none.h>
#include <nsp/mpmatrix.h>
#include <nsp/matrix.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>
#include <nsp/gtk/gobject.h>

#line 47 "atk.c"
/* ---------- types from other modules ---------- */
#include <nsp/gtk/gobject.h>
/* ---------- forward type declarations ---------- */
#include <nsp/gtk/atkrectangle.h>
#include <nsp/gtk/atkhyperlink.h>
#include <nsp/gtk/atkobject.h>
#include <nsp/gtk/atknoopobject.h>
#include <nsp/gtk/atkobjectfactory.h>
#include <nsp/gtk/atknoopobjectfactory.h>
#include <nsp/gtk/atkregistry.h>
#include <nsp/gtk/atkrelation.h>
#include <nsp/gtk/atkrelationset.h>
#include <nsp/gtk/atkstateset.h>
#include <nsp/gtk/atkutil.h>
#include <nsp/gtk/atkaction.h>
#include <nsp/gtk/atkcomponent.h>
#include <nsp/gtk/atkdocument.h>
#include <nsp/gtk/atkeditabletext.h>
#include <nsp/gtk/atkhyperlinkimpl.h>
#include <nsp/gtk/atkhypertext.h>
#include <nsp/gtk/atkimage.h>
#include <nsp/gtk/atkimplementoriface.h>
#include <nsp/gtk/atkselection.h>
#include <nsp/gtk/atkstreamablecontent.h>
#include <nsp/gtk/atktable.h>
#include <nsp/gtk/atktext.h>
#include <nsp/gtk/atkvalue.h>


/* -----------NspAtkRectangle ----------- */


#define  NspAtkRectangle_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkrectangle.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkRectangle inherits from GBoxed 
 */

int nsp_type_atkrectangle_id=0;
NspTypeAtkRectangle *nsp_type_atkrectangle=NULL;

/*
 * Type object for NspAtkRectangle 
 * all the instance of NspTypeAtkRectangle share the same id. 
 * nsp_type_atkrectangle: is an instance of NspTypeAtkRectangle 
 *    used for objects of NspAtkRectangle type (i.e built with new_atkrectangle) 
 * other instances are used for derived classes 
 */
NspTypeAtkRectangle *new_type_atkrectangle(type_mode mode)
{
  NspTypeAtkRectangle *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkrectangle != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkrectangle;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkrectangle_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkrectangle_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkrectangle;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkrectangle */ 

  top->s_type =  (s_type_func *) nsp_atkrectangle_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkrectangle_type_short_string;
  /* top->create = (create_func*) int_atkrectangle_create;*/

  /* specific methods for atkrectangle */

  type->init = (init_func *) init_atkrectangle;

  /* 
   * NspAtkRectangle interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkrectangle_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkRectangle called nsp_type_atkrectangle
       */
      type->id =  nsp_type_atkrectangle_id = nsp_new_type_id();
      nsp_type_atkrectangle = type;
      if ( nsp_register_type(nsp_type_atkrectangle) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkrectangle, ATK_TYPE_RECTANGLE);
      return ( mode == T_BASE ) ? type : new_type_atkrectangle(mode);
    }
  else 
    {
      type->id = nsp_type_atkrectangle_id;
      return type;
    }
}

/*
 * initialize NspAtkRectangle instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkrectangle(NspAtkRectangle *Obj,NspTypeAtkRectangle *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkRectangle 
 */

NspAtkRectangle *new_atkrectangle() 
{
  NspAtkRectangle *loc;
  /* type must exists */
  nsp_type_atkrectangle = new_type_atkrectangle(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkRectangle)))== NULLATKRECTANGLE) return loc;
  /* initialize object */
  if ( init_atkrectangle(loc,nsp_type_atkrectangle) == FAIL) return NULLATKRECTANGLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkRectangle 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkrectangle_type_name[]="AtkRectangle";
static char atkrectangle_short_type_name[]="AtkRectangle";

static char *nsp_atkrectangle_type_as_string(void)
{
  return(atkrectangle_type_name);
}

static char *nsp_atkrectangle_type_short_string(NspObject *v)
{
  return(atkrectangle_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkRectangle objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkRectangle   *nsp_atkrectangle_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_atkrectangle_id)  == TRUE  ) return ((NspAtkRectangle *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkrectangle));
  return NULL;
}

int IsAtkRectangleObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_atkrectangle_id);
}

int IsAtkRectangle(NspObject *O)
{
  return nsp_object_type(O,nsp_type_atkrectangle_id);
}

NspAtkRectangle  *GetAtkRectangleCopy(Stack stack, int i)
{
  if (  GetAtkRectangle(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkRectangle  *GetAtkRectangle(Stack stack, int i)
{
  NspAtkRectangle *M;
  if (( M = nsp_atkrectangle_object(NthObj(i))) == NULLATKRECTANGLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspAtkRectangle *atkrectangle_copy(NspAtkRectangle *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_atkrectangle);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkRectangle
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *atkrectangle_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_atk_rectangle__get_x(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, AtkRectangle)->x;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_atk_rectangle__get_y(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, AtkRectangle)->y;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_atk_rectangle__get_width(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, AtkRectangle)->width;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_atk_rectangle__get_height(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, AtkRectangle)->height;
  return nsp_new_double_obj((double) ret);
}

static AttrTab atkrectangle_attrs[] = {
  { "x", (attr_get_function * )_wrap_atk_rectangle__get_x, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "y", (attr_get_function * )_wrap_atk_rectangle__get_y, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "width", (attr_get_function * )_wrap_atk_rectangle__get_width, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "height", (attr_get_function * )_wrap_atk_rectangle__get_height, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { NULL,NULL,NULL,NULL,NULL },
};



/* -----------NspAtkAction ----------- */


#define  NspAtkAction_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkaction.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkAction inherits from GObject 
 */

int nsp_type_atkaction_id=0;
NspTypeAtkAction *nsp_type_atkaction=NULL;

/*
 * Type object for NspAtkAction 
 * all the instance of NspTypeAtkAction share the same id. 
 * nsp_type_atkaction: is an instance of NspTypeAtkAction 
 *    used for objects of NspAtkAction type (i.e built with new_atkaction) 
 * other instances are used for derived classes 
 */
NspTypeAtkAction *new_type_atkaction(type_mode mode)
{
  NspTypeAtkAction *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkaction != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkaction;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkaction_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkaction_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkaction;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkaction */ 

  top->s_type =  (s_type_func *) nsp_atkaction_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkaction_type_short_string;
  /* top->create = (create_func*) int_atkaction_create;*/

  /* specific methods for atkaction */

  type->init = (init_func *) init_atkaction;

  /* 
   * NspAtkAction interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkaction_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkAction called nsp_type_atkaction
       */
      type->id =  nsp_type_atkaction_id = nsp_new_type_id();
      nsp_type_atkaction = type;
      if ( nsp_register_type(nsp_type_atkaction) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkaction, ATK_TYPE_ACTION);
      return ( mode == T_BASE ) ? type : new_type_atkaction(mode);
    }
  else 
    {
      type->id = nsp_type_atkaction_id;
      return type;
    }
}

/*
 * initialize NspAtkAction instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkaction(NspAtkAction *Obj,NspTypeAtkAction *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkAction 
 */

NspAtkAction *new_atkaction() 
{
  NspAtkAction *loc;
  /* type must exists */
  nsp_type_atkaction = new_type_atkaction(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkAction)))== NULLATKACTION) return loc;
  /* initialize object */
  if ( init_atkaction(loc,nsp_type_atkaction) == FAIL) return NULLATKACTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkAction 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkaction_type_name[]="AtkAction";
static char atkaction_short_type_name[]="AtkAction";

static char *nsp_atkaction_type_as_string(void)
{
  return(atkaction_type_name);
}

static char *nsp_atkaction_type_short_string(NspObject *v)
{
  return(atkaction_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkAction objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkAction   *nsp_atkaction_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkaction_id)   ) return ((NspAtkAction *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkaction));
  return NULL;
}

int IsAtkActionObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkaction_id);
}

int IsAtkAction(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkaction_id);
}

NspAtkAction  *GetAtkActionCopy(Stack stack, int i)
{
  if (  GetAtkAction(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkAction  *GetAtkAction(Stack stack, int i)
{
  NspAtkAction *M;
  if (( M = nsp_atkaction_object(NthObj(i))) == NULLATKACTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkAction *atkaction_copy(NspAtkAction *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkaction);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkaction);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkAction
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_action_do_action(NspAtkAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i, ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_action_do_action(ATK_ACTION(self->obj),i);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_action_get_n_actions(NspAtkAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_action_get_n_actions(ATK_ACTION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_action_get_description(NspAtkAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_action_get_description(ATK_ACTION(self->obj),i);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_action_get_name(NspAtkAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_action_get_name(ATK_ACTION(self->obj),i);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_action_get_keybinding(NspAtkAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_action_get_keybinding(ATK_ACTION(self->obj),i);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_action_set_description(NspAtkAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string, t_end};
  int i, ret;
  char *desc;
  if ( GetArgs(stack,rhs,opt,T,&i, &desc) == FAIL) return RET_BUG;
    ret =atk_action_set_description(ATK_ACTION(self->obj),i,desc);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods atkaction_methods[] = {
  {"do_action",(nsp_method *) _wrap_atk_action_do_action},
  {"get_n_actions",(nsp_method *) _wrap_atk_action_get_n_actions},
  {"get_description",(nsp_method *) _wrap_atk_action_get_description},
  {"get_name",(nsp_method *) _wrap_atk_action_get_name},
  {"get_keybinding",(nsp_method *) _wrap_atk_action_get_keybinding},
  {"set_description",(nsp_method *) _wrap_atk_action_set_description},
  { NULL, NULL}
};

static NspMethods *atkaction_get_methods(void) { return atkaction_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkaction_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkComponent ----------- */


#define  NspAtkComponent_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkcomponent.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkComponent inherits from GObject 
 */

int nsp_type_atkcomponent_id=0;
NspTypeAtkComponent *nsp_type_atkcomponent=NULL;

/*
 * Type object for NspAtkComponent 
 * all the instance of NspTypeAtkComponent share the same id. 
 * nsp_type_atkcomponent: is an instance of NspTypeAtkComponent 
 *    used for objects of NspAtkComponent type (i.e built with new_atkcomponent) 
 * other instances are used for derived classes 
 */
NspTypeAtkComponent *new_type_atkcomponent(type_mode mode)
{
  NspTypeAtkComponent *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkcomponent != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkcomponent;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkcomponent_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkcomponent_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkcomponent;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkcomponent */ 

  top->s_type =  (s_type_func *) nsp_atkcomponent_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkcomponent_type_short_string;
  /* top->create = (create_func*) int_atkcomponent_create;*/

  /* specific methods for atkcomponent */

  type->init = (init_func *) init_atkcomponent;

  /* 
   * NspAtkComponent interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkcomponent_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkComponent called nsp_type_atkcomponent
       */
      type->id =  nsp_type_atkcomponent_id = nsp_new_type_id();
      nsp_type_atkcomponent = type;
      if ( nsp_register_type(nsp_type_atkcomponent) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkcomponent, ATK_TYPE_COMPONENT);
      return ( mode == T_BASE ) ? type : new_type_atkcomponent(mode);
    }
  else 
    {
      type->id = nsp_type_atkcomponent_id;
      return type;
    }
}

/*
 * initialize NspAtkComponent instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkcomponent(NspAtkComponent *Obj,NspTypeAtkComponent *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkComponent 
 */

NspAtkComponent *new_atkcomponent() 
{
  NspAtkComponent *loc;
  /* type must exists */
  nsp_type_atkcomponent = new_type_atkcomponent(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkComponent)))== NULLATKCOMPONENT) return loc;
  /* initialize object */
  if ( init_atkcomponent(loc,nsp_type_atkcomponent) == FAIL) return NULLATKCOMPONENT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkComponent 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkcomponent_type_name[]="AtkComponent";
static char atkcomponent_short_type_name[]="AtkComponent";

static char *nsp_atkcomponent_type_as_string(void)
{
  return(atkcomponent_type_name);
}

static char *nsp_atkcomponent_type_short_string(NspObject *v)
{
  return(atkcomponent_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkComponent objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkComponent   *nsp_atkcomponent_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkcomponent_id)   ) return ((NspAtkComponent *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkcomponent));
  return NULL;
}

int IsAtkComponentObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkcomponent_id);
}

int IsAtkComponent(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkcomponent_id);
}

NspAtkComponent  *GetAtkComponentCopy(Stack stack, int i)
{
  if (  GetAtkComponent(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkComponent  *GetAtkComponent(Stack stack, int i)
{
  NspAtkComponent *M;
  if (( M = nsp_atkcomponent_object(NthObj(i))) == NULLATKCOMPONENT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkComponent *atkcomponent_copy(NspAtkComponent *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkcomponent);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkcomponent);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkComponent
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_component_contains(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,obj, t_end};
  int x, y, ret;
  AtkCoordType coord_type;
  NspObject *nsp_coord_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &nsp_coord_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_coord_type, &coord_type)== FAIL)
      return RET_BUG;
    ret =atk_component_contains(ATK_COMPONENT(self->obj),x,y,coord_type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_component_ref_accessible_at_point(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,obj, t_end};
  int x, y;
  AtkCoordType coord_type;
  NspObject *nsp_coord_type = NULL, *nsp_ret;
  AtkObject *ret;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &nsp_coord_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_coord_type, &coord_type)== FAIL)
      return RET_BUG;
    ret =atk_component_ref_accessible_at_point(ATK_COMPONENT(self->obj),x,y,coord_type);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_component_get_extents(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int,s_int,obj, t_end};
  int x, y, width, height;
  AtkCoordType coord_type;
  NspObject *nsp_coord_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &width, &height, &nsp_coord_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_coord_type, &coord_type)== FAIL)
      return RET_BUG;
    atk_component_get_extents(ATK_COMPONENT(self->obj),&x,&y,&width,&height,coord_type);
  return 0;
}

static int _wrap_atk_component_grab_focus(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_component_grab_focus(ATK_COMPONENT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_component_set_extents(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int,s_int,obj, t_end};
  int x, y, width, height, ret;
  AtkCoordType coord_type;
  NspObject *nsp_coord_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &width, &height, &nsp_coord_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_coord_type, &coord_type)== FAIL)
      return RET_BUG;
    ret =atk_component_set_extents(ATK_COMPONENT(self->obj),x,y,width,height,coord_type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_component_set_position(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,obj, t_end};
  int x, y, ret;
  AtkCoordType coord_type;
  NspObject *nsp_coord_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &nsp_coord_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_coord_type, &coord_type)== FAIL)
      return RET_BUG;
    ret =atk_component_set_position(ATK_COMPONENT(self->obj),x,y,coord_type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_component_set_size(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int width, height, ret;
  if ( GetArgs(stack,rhs,opt,T,&width, &height) == FAIL) return RET_BUG;
    ret =atk_component_set_size(ATK_COMPONENT(self->obj),width,height);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_component_get_layer(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =atk_component_get_layer(ATK_COMPONENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_component_get_mdi_zorder(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_component_get_mdi_zorder(ATK_COMPONENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_component_get_alpha(NspAtkComponent *self,Stack stack,int rhs,int opt,int lhs)
{
  double ret;
  CheckRhs(0,0);
    ret =atk_component_get_alpha(ATK_COMPONENT(self->obj));
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods atkcomponent_methods[] = {
  {"contains",(nsp_method *) _wrap_atk_component_contains},
  {"ref_accessible_at_point",(nsp_method *) _wrap_atk_component_ref_accessible_at_point},
  {"get_extents",(nsp_method *) _wrap_atk_component_get_extents},
  {"grab_focus",(nsp_method *) _wrap_atk_component_grab_focus},
  {"set_extents",(nsp_method *) _wrap_atk_component_set_extents},
  {"set_position",(nsp_method *) _wrap_atk_component_set_position},
  {"set_size",(nsp_method *) _wrap_atk_component_set_size},
  {"get_layer",(nsp_method *) _wrap_atk_component_get_layer},
  {"get_mdi_zorder",(nsp_method *) _wrap_atk_component_get_mdi_zorder},
  {"get_alpha",(nsp_method *) _wrap_atk_component_get_alpha},
  { NULL, NULL}
};

static NspMethods *atkcomponent_get_methods(void) { return atkcomponent_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkcomponent_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkDocument ----------- */


#define  NspAtkDocument_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkdocument.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkDocument inherits from GObject 
 */

int nsp_type_atkdocument_id=0;
NspTypeAtkDocument *nsp_type_atkdocument=NULL;

/*
 * Type object for NspAtkDocument 
 * all the instance of NspTypeAtkDocument share the same id. 
 * nsp_type_atkdocument: is an instance of NspTypeAtkDocument 
 *    used for objects of NspAtkDocument type (i.e built with new_atkdocument) 
 * other instances are used for derived classes 
 */
NspTypeAtkDocument *new_type_atkdocument(type_mode mode)
{
  NspTypeAtkDocument *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkdocument != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkdocument;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkdocument_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkdocument_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkdocument;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkdocument */ 

  top->s_type =  (s_type_func *) nsp_atkdocument_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkdocument_type_short_string;
  /* top->create = (create_func*) int_atkdocument_create;*/

  /* specific methods for atkdocument */

  type->init = (init_func *) init_atkdocument;

  /* 
   * NspAtkDocument interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkdocument_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkDocument called nsp_type_atkdocument
       */
      type->id =  nsp_type_atkdocument_id = nsp_new_type_id();
      nsp_type_atkdocument = type;
      if ( nsp_register_type(nsp_type_atkdocument) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkdocument, ATK_TYPE_DOCUMENT);
      return ( mode == T_BASE ) ? type : new_type_atkdocument(mode);
    }
  else 
    {
      type->id = nsp_type_atkdocument_id;
      return type;
    }
}

/*
 * initialize NspAtkDocument instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkdocument(NspAtkDocument *Obj,NspTypeAtkDocument *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkDocument 
 */

NspAtkDocument *new_atkdocument() 
{
  NspAtkDocument *loc;
  /* type must exists */
  nsp_type_atkdocument = new_type_atkdocument(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkDocument)))== NULLATKDOCUMENT) return loc;
  /* initialize object */
  if ( init_atkdocument(loc,nsp_type_atkdocument) == FAIL) return NULLATKDOCUMENT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkDocument 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkdocument_type_name[]="AtkDocument";
static char atkdocument_short_type_name[]="AtkDocument";

static char *nsp_atkdocument_type_as_string(void)
{
  return(atkdocument_type_name);
}

static char *nsp_atkdocument_type_short_string(NspObject *v)
{
  return(atkdocument_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkDocument objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkDocument   *nsp_atkdocument_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkdocument_id)   ) return ((NspAtkDocument *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkdocument));
  return NULL;
}

int IsAtkDocumentObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkdocument_id);
}

int IsAtkDocument(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkdocument_id);
}

NspAtkDocument  *GetAtkDocumentCopy(Stack stack, int i)
{
  if (  GetAtkDocument(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkDocument  *GetAtkDocument(Stack stack, int i)
{
  NspAtkDocument *M;
  if (( M = nsp_atkdocument_object(NthObj(i))) == NULLATKDOCUMENT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkDocument *atkdocument_copy(NspAtkDocument *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkdocument);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkdocument);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkDocument
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *atkdocument_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkdocument_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkEditableText ----------- */


#define  NspAtkEditableText_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkeditabletext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkEditableText inherits from GObject 
 */

int nsp_type_atkeditabletext_id=0;
NspTypeAtkEditableText *nsp_type_atkeditabletext=NULL;

/*
 * Type object for NspAtkEditableText 
 * all the instance of NspTypeAtkEditableText share the same id. 
 * nsp_type_atkeditabletext: is an instance of NspTypeAtkEditableText 
 *    used for objects of NspAtkEditableText type (i.e built with new_atkeditabletext) 
 * other instances are used for derived classes 
 */
NspTypeAtkEditableText *new_type_atkeditabletext(type_mode mode)
{
  NspTypeAtkEditableText *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkeditabletext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkeditabletext;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkeditabletext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkeditabletext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkeditabletext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkeditabletext */ 

  top->s_type =  (s_type_func *) nsp_atkeditabletext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkeditabletext_type_short_string;
  /* top->create = (create_func*) int_atkeditabletext_create;*/

  /* specific methods for atkeditabletext */

  type->init = (init_func *) init_atkeditabletext;

  /* 
   * NspAtkEditableText interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkeditabletext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkEditableText called nsp_type_atkeditabletext
       */
      type->id =  nsp_type_atkeditabletext_id = nsp_new_type_id();
      nsp_type_atkeditabletext = type;
      if ( nsp_register_type(nsp_type_atkeditabletext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkeditabletext, ATK_TYPE_EDITABLE_TEXT);
      return ( mode == T_BASE ) ? type : new_type_atkeditabletext(mode);
    }
  else 
    {
      type->id = nsp_type_atkeditabletext_id;
      return type;
    }
}

/*
 * initialize NspAtkEditableText instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkeditabletext(NspAtkEditableText *Obj,NspTypeAtkEditableText *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkEditableText 
 */

NspAtkEditableText *new_atkeditabletext() 
{
  NspAtkEditableText *loc;
  /* type must exists */
  nsp_type_atkeditabletext = new_type_atkeditabletext(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkEditableText)))== NULLATKEDITABLETEXT) return loc;
  /* initialize object */
  if ( init_atkeditabletext(loc,nsp_type_atkeditabletext) == FAIL) return NULLATKEDITABLETEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkEditableText 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkeditabletext_type_name[]="AtkEditableText";
static char atkeditabletext_short_type_name[]="AtkEditableText";

static char *nsp_atkeditabletext_type_as_string(void)
{
  return(atkeditabletext_type_name);
}

static char *nsp_atkeditabletext_type_short_string(NspObject *v)
{
  return(atkeditabletext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkEditableText objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkEditableText   *nsp_atkeditabletext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkeditabletext_id)   ) return ((NspAtkEditableText *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkeditabletext));
  return NULL;
}

int IsAtkEditableTextObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkeditabletext_id);
}

int IsAtkEditableText(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkeditabletext_id);
}

NspAtkEditableText  *GetAtkEditableTextCopy(Stack stack, int i)
{
  if (  GetAtkEditableText(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkEditableText  *GetAtkEditableText(Stack stack, int i)
{
  NspAtkEditableText *M;
  if (( M = nsp_atkeditabletext_object(NthObj(i))) == NULLATKEDITABLETEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkEditableText *atkeditabletext_copy(NspAtkEditableText *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkeditabletext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkeditabletext);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkEditableText
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_editable_text_set_text_contents(NspAtkEditableText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *string;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    atk_editable_text_set_text_contents(ATK_EDITABLE_TEXT(self->obj),string);
  return 0;
}

static int _wrap_atk_editable_text_insert_text(NspAtkEditableText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,s_int, t_end};
  char *string;
  int length, position;
  if ( GetArgs(stack,rhs,opt,T,&string, &length, &position) == FAIL) return RET_BUG;
    atk_editable_text_insert_text(ATK_EDITABLE_TEXT(self->obj),string,length,&position);
  return 0;
}

static int _wrap_atk_editable_text_copy_text(NspAtkEditableText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int start_pos, end_pos;
  if ( GetArgs(stack,rhs,opt,T,&start_pos, &end_pos) == FAIL) return RET_BUG;
    atk_editable_text_copy_text(ATK_EDITABLE_TEXT(self->obj),start_pos,end_pos);
  return 0;
}

static int _wrap_atk_editable_text_cut_text(NspAtkEditableText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int start_pos, end_pos;
  if ( GetArgs(stack,rhs,opt,T,&start_pos, &end_pos) == FAIL) return RET_BUG;
    atk_editable_text_cut_text(ATK_EDITABLE_TEXT(self->obj),start_pos,end_pos);
  return 0;
}

static int _wrap_atk_editable_text_delete_text(NspAtkEditableText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int start_pos, end_pos;
  if ( GetArgs(stack,rhs,opt,T,&start_pos, &end_pos) == FAIL) return RET_BUG;
    atk_editable_text_delete_text(ATK_EDITABLE_TEXT(self->obj),start_pos,end_pos);
  return 0;
}

static int _wrap_atk_editable_text_paste_text(NspAtkEditableText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int position;
  if ( GetArgs(stack,rhs,opt,T,&position) == FAIL) return RET_BUG;
    atk_editable_text_paste_text(ATK_EDITABLE_TEXT(self->obj),position);
  return 0;
}

static NspMethods atkeditabletext_methods[] = {
  {"set_text_contents",(nsp_method *) _wrap_atk_editable_text_set_text_contents},
  {"insert_text",(nsp_method *) _wrap_atk_editable_text_insert_text},
  {"copy_text",(nsp_method *) _wrap_atk_editable_text_copy_text},
  {"cut_text",(nsp_method *) _wrap_atk_editable_text_cut_text},
  {"delete_text",(nsp_method *) _wrap_atk_editable_text_delete_text},
  {"paste_text",(nsp_method *) _wrap_atk_editable_text_paste_text},
  { NULL, NULL}
};

static NspMethods *atkeditabletext_get_methods(void) { return atkeditabletext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkeditabletext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkHyperlinkImpl ----------- */


#define  NspAtkHyperlinkImpl_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkhyperlinkimpl.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkHyperlinkImpl inherits from GObject 
 */

int nsp_type_atkhyperlinkimpl_id=0;
NspTypeAtkHyperlinkImpl *nsp_type_atkhyperlinkimpl=NULL;

/*
 * Type object for NspAtkHyperlinkImpl 
 * all the instance of NspTypeAtkHyperlinkImpl share the same id. 
 * nsp_type_atkhyperlinkimpl: is an instance of NspTypeAtkHyperlinkImpl 
 *    used for objects of NspAtkHyperlinkImpl type (i.e built with new_atkhyperlinkimpl) 
 * other instances are used for derived classes 
 */
NspTypeAtkHyperlinkImpl *new_type_atkhyperlinkimpl(type_mode mode)
{
  NspTypeAtkHyperlinkImpl *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkhyperlinkimpl != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkhyperlinkimpl;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkhyperlinkimpl_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkhyperlinkimpl_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkhyperlinkimpl;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkhyperlinkimpl */ 

  top->s_type =  (s_type_func *) nsp_atkhyperlinkimpl_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkhyperlinkimpl_type_short_string;
  /* top->create = (create_func*) int_atkhyperlinkimpl_create;*/

  /* specific methods for atkhyperlinkimpl */

  type->init = (init_func *) init_atkhyperlinkimpl;

  /* 
   * NspAtkHyperlinkImpl interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkhyperlinkimpl_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkHyperlinkImpl called nsp_type_atkhyperlinkimpl
       */
      type->id =  nsp_type_atkhyperlinkimpl_id = nsp_new_type_id();
      nsp_type_atkhyperlinkimpl = type;
      if ( nsp_register_type(nsp_type_atkhyperlinkimpl) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkhyperlinkimpl, ATK_TYPE_HYPERLINK_IMPL);
      return ( mode == T_BASE ) ? type : new_type_atkhyperlinkimpl(mode);
    }
  else 
    {
      type->id = nsp_type_atkhyperlinkimpl_id;
      return type;
    }
}

/*
 * initialize NspAtkHyperlinkImpl instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkhyperlinkimpl(NspAtkHyperlinkImpl *Obj,NspTypeAtkHyperlinkImpl *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkHyperlinkImpl 
 */

NspAtkHyperlinkImpl *new_atkhyperlinkimpl() 
{
  NspAtkHyperlinkImpl *loc;
  /* type must exists */
  nsp_type_atkhyperlinkimpl = new_type_atkhyperlinkimpl(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkHyperlinkImpl)))== NULLATKHYPERLINKIMPL) return loc;
  /* initialize object */
  if ( init_atkhyperlinkimpl(loc,nsp_type_atkhyperlinkimpl) == FAIL) return NULLATKHYPERLINKIMPL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkHyperlinkImpl 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkhyperlinkimpl_type_name[]="AtkHyperlinkImpl";
static char atkhyperlinkimpl_short_type_name[]="AtkHyperlinkImpl";

static char *nsp_atkhyperlinkimpl_type_as_string(void)
{
  return(atkhyperlinkimpl_type_name);
}

static char *nsp_atkhyperlinkimpl_type_short_string(NspObject *v)
{
  return(atkhyperlinkimpl_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkHyperlinkImpl objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkHyperlinkImpl   *nsp_atkhyperlinkimpl_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkhyperlinkimpl_id)   ) return ((NspAtkHyperlinkImpl *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkhyperlinkimpl));
  return NULL;
}

int IsAtkHyperlinkImplObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkhyperlinkimpl_id);
}

int IsAtkHyperlinkImpl(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkhyperlinkimpl_id);
}

NspAtkHyperlinkImpl  *GetAtkHyperlinkImplCopy(Stack stack, int i)
{
  if (  GetAtkHyperlinkImpl(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkHyperlinkImpl  *GetAtkHyperlinkImpl(Stack stack, int i)
{
  NspAtkHyperlinkImpl *M;
  if (( M = nsp_atkhyperlinkimpl_object(NthObj(i))) == NULLATKHYPERLINKIMPL)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkHyperlinkImpl *atkhyperlinkimpl_copy(NspAtkHyperlinkImpl *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkhyperlinkimpl);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkhyperlinkimpl);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkHyperlinkImpl
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *atkhyperlinkimpl_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkhyperlinkimpl_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkHypertext ----------- */


#define  NspAtkHypertext_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkhypertext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkHypertext inherits from GObject 
 */

int nsp_type_atkhypertext_id=0;
NspTypeAtkHypertext *nsp_type_atkhypertext=NULL;

/*
 * Type object for NspAtkHypertext 
 * all the instance of NspTypeAtkHypertext share the same id. 
 * nsp_type_atkhypertext: is an instance of NspTypeAtkHypertext 
 *    used for objects of NspAtkHypertext type (i.e built with new_atkhypertext) 
 * other instances are used for derived classes 
 */
NspTypeAtkHypertext *new_type_atkhypertext(type_mode mode)
{
  NspTypeAtkHypertext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkhypertext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkhypertext;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkhypertext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkhypertext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkhypertext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkhypertext */ 

  top->s_type =  (s_type_func *) nsp_atkhypertext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkhypertext_type_short_string;
  /* top->create = (create_func*) int_atkhypertext_create;*/

  /* specific methods for atkhypertext */

  type->init = (init_func *) init_atkhypertext;

  /* 
   * NspAtkHypertext interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkhypertext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkHypertext called nsp_type_atkhypertext
       */
      type->id =  nsp_type_atkhypertext_id = nsp_new_type_id();
      nsp_type_atkhypertext = type;
      if ( nsp_register_type(nsp_type_atkhypertext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkhypertext, ATK_TYPE_HYPERTEXT);
      return ( mode == T_BASE ) ? type : new_type_atkhypertext(mode);
    }
  else 
    {
      type->id = nsp_type_atkhypertext_id;
      return type;
    }
}

/*
 * initialize NspAtkHypertext instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkhypertext(NspAtkHypertext *Obj,NspTypeAtkHypertext *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkHypertext 
 */

NspAtkHypertext *new_atkhypertext() 
{
  NspAtkHypertext *loc;
  /* type must exists */
  nsp_type_atkhypertext = new_type_atkhypertext(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkHypertext)))== NULLATKHYPERTEXT) return loc;
  /* initialize object */
  if ( init_atkhypertext(loc,nsp_type_atkhypertext) == FAIL) return NULLATKHYPERTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkHypertext 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkhypertext_type_name[]="AtkHypertext";
static char atkhypertext_short_type_name[]="AtkHypertext";

static char *nsp_atkhypertext_type_as_string(void)
{
  return(atkhypertext_type_name);
}

static char *nsp_atkhypertext_type_short_string(NspObject *v)
{
  return(atkhypertext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkHypertext objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkHypertext   *nsp_atkhypertext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkhypertext_id)   ) return ((NspAtkHypertext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkhypertext));
  return NULL;
}

int IsAtkHypertextObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkhypertext_id);
}

int IsAtkHypertext(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkhypertext_id);
}

NspAtkHypertext  *GetAtkHypertextCopy(Stack stack, int i)
{
  if (  GetAtkHypertext(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkHypertext  *GetAtkHypertext(Stack stack, int i)
{
  NspAtkHypertext *M;
  if (( M = nsp_atkhypertext_object(NthObj(i))) == NULLATKHYPERTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkHypertext *atkhypertext_copy(NspAtkHypertext *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkhypertext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkhypertext);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkHypertext
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_hypertext_get_link(NspAtkHypertext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int link_index;
  AtkHyperlink *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&link_index) == FAIL) return RET_BUG;
    ret =atk_hypertext_get_link(ATK_HYPERTEXT(self->obj),link_index);
  nsp_type_atkhyperlink = new_type_atkhyperlink(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkhyperlink))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_hypertext_get_n_links(NspAtkHypertext *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_hypertext_get_n_links(ATK_HYPERTEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_hypertext_get_link_index(NspAtkHypertext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int char_index, ret;
  if ( GetArgs(stack,rhs,opt,T,&char_index) == FAIL) return RET_BUG;
    ret =atk_hypertext_get_link_index(ATK_HYPERTEXT(self->obj),char_index);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods atkhypertext_methods[] = {
  {"get_link",(nsp_method *) _wrap_atk_hypertext_get_link},
  {"get_n_links",(nsp_method *) _wrap_atk_hypertext_get_n_links},
  {"get_link_index",(nsp_method *) _wrap_atk_hypertext_get_link_index},
  { NULL, NULL}
};

static NspMethods *atkhypertext_get_methods(void) { return atkhypertext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkhypertext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkImage ----------- */


#define  NspAtkImage_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkimage.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkImage inherits from GObject 
 */

int nsp_type_atkimage_id=0;
NspTypeAtkImage *nsp_type_atkimage=NULL;

/*
 * Type object for NspAtkImage 
 * all the instance of NspTypeAtkImage share the same id. 
 * nsp_type_atkimage: is an instance of NspTypeAtkImage 
 *    used for objects of NspAtkImage type (i.e built with new_atkimage) 
 * other instances are used for derived classes 
 */
NspTypeAtkImage *new_type_atkimage(type_mode mode)
{
  NspTypeAtkImage *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkimage != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkimage;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkimage_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkimage_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkimage;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkimage */ 

  top->s_type =  (s_type_func *) nsp_atkimage_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkimage_type_short_string;
  /* top->create = (create_func*) int_atkimage_create;*/

  /* specific methods for atkimage */

  type->init = (init_func *) init_atkimage;

  /* 
   * NspAtkImage interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkimage_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkImage called nsp_type_atkimage
       */
      type->id =  nsp_type_atkimage_id = nsp_new_type_id();
      nsp_type_atkimage = type;
      if ( nsp_register_type(nsp_type_atkimage) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkimage, ATK_TYPE_IMAGE);
      return ( mode == T_BASE ) ? type : new_type_atkimage(mode);
    }
  else 
    {
      type->id = nsp_type_atkimage_id;
      return type;
    }
}

/*
 * initialize NspAtkImage instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkimage(NspAtkImage *Obj,NspTypeAtkImage *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkImage 
 */

NspAtkImage *new_atkimage() 
{
  NspAtkImage *loc;
  /* type must exists */
  nsp_type_atkimage = new_type_atkimage(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkImage)))== NULLATKIMAGE) return loc;
  /* initialize object */
  if ( init_atkimage(loc,nsp_type_atkimage) == FAIL) return NULLATKIMAGE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkImage 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkimage_type_name[]="AtkImage";
static char atkimage_short_type_name[]="AtkImage";

static char *nsp_atkimage_type_as_string(void)
{
  return(atkimage_type_name);
}

static char *nsp_atkimage_type_short_string(NspObject *v)
{
  return(atkimage_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkImage objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkImage   *nsp_atkimage_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkimage_id)   ) return ((NspAtkImage *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkimage));
  return NULL;
}

int IsAtkImageObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkimage_id);
}

int IsAtkImage(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkimage_id);
}

NspAtkImage  *GetAtkImageCopy(Stack stack, int i)
{
  if (  GetAtkImage(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkImage  *GetAtkImage(Stack stack, int i)
{
  NspAtkImage *M;
  if (( M = nsp_atkimage_object(NthObj(i))) == NULLATKIMAGE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkImage *atkimage_copy(NspAtkImage *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkimage);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkimage);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkImage
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_image_get_image_description(NspAtkImage *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =atk_image_get_image_description(ATK_IMAGE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_image_get_image_size(NspAtkImage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int width, height;
  if ( GetArgs(stack,rhs,opt,T,&width, &height) == FAIL) return RET_BUG;
    atk_image_get_image_size(ATK_IMAGE(self->obj),&width,&height);
  return 0;
}

static int _wrap_atk_image_set_image_description(NspAtkImage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *description;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&description) == FAIL) return RET_BUG;
    ret =atk_image_set_image_description(ATK_IMAGE(self->obj),description);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_image_get_image_position(NspAtkImage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,obj, t_end};
  int x, y;
  AtkCoordType coord_type;
  NspObject *nsp_coord_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &nsp_coord_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_coord_type, &coord_type)== FAIL)
      return RET_BUG;
    atk_image_get_image_position(ATK_IMAGE(self->obj),&x,&y,coord_type);
  return 0;
}

static NspMethods atkimage_methods[] = {
  {"get_image_description",(nsp_method *) _wrap_atk_image_get_image_description},
  {"get_image_size",(nsp_method *) _wrap_atk_image_get_image_size},
  {"set_image_description",(nsp_method *) _wrap_atk_image_set_image_description},
  {"get_image_position",(nsp_method *) _wrap_atk_image_get_image_position},
  { NULL, NULL}
};

static NspMethods *atkimage_get_methods(void) { return atkimage_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkimage_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkImplementorIface ----------- */


#define  NspAtkImplementorIface_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkimplementoriface.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkImplementorIface inherits from GObject 
 */

int nsp_type_atkimplementoriface_id=0;
NspTypeAtkImplementorIface *nsp_type_atkimplementoriface=NULL;

/*
 * Type object for NspAtkImplementorIface 
 * all the instance of NspTypeAtkImplementorIface share the same id. 
 * nsp_type_atkimplementoriface: is an instance of NspTypeAtkImplementorIface 
 *    used for objects of NspAtkImplementorIface type (i.e built with new_atkimplementoriface) 
 * other instances are used for derived classes 
 */
NspTypeAtkImplementorIface *new_type_atkimplementoriface(type_mode mode)
{
  NspTypeAtkImplementorIface *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkimplementoriface != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkimplementoriface;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkimplementoriface_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkimplementoriface_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkimplementoriface;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkimplementoriface */ 

  top->s_type =  (s_type_func *) nsp_atkimplementoriface_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkimplementoriface_type_short_string;
  /* top->create = (create_func*) int_atkimplementoriface_create;*/

  /* specific methods for atkimplementoriface */

  type->init = (init_func *) init_atkimplementoriface;

  /* 
   * NspAtkImplementorIface interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkimplementoriface_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkImplementorIface called nsp_type_atkimplementoriface
       */
      type->id =  nsp_type_atkimplementoriface_id = nsp_new_type_id();
      nsp_type_atkimplementoriface = type;
      if ( nsp_register_type(nsp_type_atkimplementoriface) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkimplementoriface, ATK_TYPE_IMPLEMENTOR);
      return ( mode == T_BASE ) ? type : new_type_atkimplementoriface(mode);
    }
  else 
    {
      type->id = nsp_type_atkimplementoriface_id;
      return type;
    }
}

/*
 * initialize NspAtkImplementorIface instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkimplementoriface(NspAtkImplementorIface *Obj,NspTypeAtkImplementorIface *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkImplementorIface 
 */

NspAtkImplementorIface *new_atkimplementoriface() 
{
  NspAtkImplementorIface *loc;
  /* type must exists */
  nsp_type_atkimplementoriface = new_type_atkimplementoriface(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkImplementorIface)))== NULLATKIMPLEMENTORIFACE) return loc;
  /* initialize object */
  if ( init_atkimplementoriface(loc,nsp_type_atkimplementoriface) == FAIL) return NULLATKIMPLEMENTORIFACE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkImplementorIface 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkimplementoriface_type_name[]="AtkImplementorIface";
static char atkimplementoriface_short_type_name[]="AtkImplementorIface";

static char *nsp_atkimplementoriface_type_as_string(void)
{
  return(atkimplementoriface_type_name);
}

static char *nsp_atkimplementoriface_type_short_string(NspObject *v)
{
  return(atkimplementoriface_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkImplementorIface objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkImplementorIface   *nsp_atkimplementoriface_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkimplementoriface_id)   ) return ((NspAtkImplementorIface *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkimplementoriface));
  return NULL;
}

int IsAtkImplementorIfaceObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkimplementoriface_id);
}

int IsAtkImplementorIface(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkimplementoriface_id);
}

NspAtkImplementorIface  *GetAtkImplementorIfaceCopy(Stack stack, int i)
{
  if (  GetAtkImplementorIface(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkImplementorIface  *GetAtkImplementorIface(Stack stack, int i)
{
  NspAtkImplementorIface *M;
  if (( M = nsp_atkimplementoriface_object(NthObj(i))) == NULLATKIMPLEMENTORIFACE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkImplementorIface *atkimplementoriface_copy(NspAtkImplementorIface *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkimplementoriface);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkimplementoriface);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkImplementorIface
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *atkimplementoriface_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkimplementoriface_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkSelection ----------- */


#define  NspAtkSelection_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkselection.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkSelection inherits from GObject 
 */

int nsp_type_atkselection_id=0;
NspTypeAtkSelection *nsp_type_atkselection=NULL;

/*
 * Type object for NspAtkSelection 
 * all the instance of NspTypeAtkSelection share the same id. 
 * nsp_type_atkselection: is an instance of NspTypeAtkSelection 
 *    used for objects of NspAtkSelection type (i.e built with new_atkselection) 
 * other instances are used for derived classes 
 */
NspTypeAtkSelection *new_type_atkselection(type_mode mode)
{
  NspTypeAtkSelection *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkselection != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkselection;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkselection_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkselection_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkselection;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkselection */ 

  top->s_type =  (s_type_func *) nsp_atkselection_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkselection_type_short_string;
  /* top->create = (create_func*) int_atkselection_create;*/

  /* specific methods for atkselection */

  type->init = (init_func *) init_atkselection;

  /* 
   * NspAtkSelection interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkselection_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkSelection called nsp_type_atkselection
       */
      type->id =  nsp_type_atkselection_id = nsp_new_type_id();
      nsp_type_atkselection = type;
      if ( nsp_register_type(nsp_type_atkselection) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkselection, ATK_TYPE_SELECTION);
      return ( mode == T_BASE ) ? type : new_type_atkselection(mode);
    }
  else 
    {
      type->id = nsp_type_atkselection_id;
      return type;
    }
}

/*
 * initialize NspAtkSelection instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkselection(NspAtkSelection *Obj,NspTypeAtkSelection *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkSelection 
 */

NspAtkSelection *new_atkselection() 
{
  NspAtkSelection *loc;
  /* type must exists */
  nsp_type_atkselection = new_type_atkselection(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkSelection)))== NULLATKSELECTION) return loc;
  /* initialize object */
  if ( init_atkselection(loc,nsp_type_atkselection) == FAIL) return NULLATKSELECTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkSelection 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkselection_type_name[]="AtkSelection";
static char atkselection_short_type_name[]="AtkSelection";

static char *nsp_atkselection_type_as_string(void)
{
  return(atkselection_type_name);
}

static char *nsp_atkselection_type_short_string(NspObject *v)
{
  return(atkselection_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkSelection objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkSelection   *nsp_atkselection_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkselection_id)   ) return ((NspAtkSelection *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkselection));
  return NULL;
}

int IsAtkSelectionObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkselection_id);
}

int IsAtkSelection(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkselection_id);
}

NspAtkSelection  *GetAtkSelectionCopy(Stack stack, int i)
{
  if (  GetAtkSelection(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkSelection  *GetAtkSelection(Stack stack, int i)
{
  NspAtkSelection *M;
  if (( M = nsp_atkselection_object(NthObj(i))) == NULLATKSELECTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkSelection *atkselection_copy(NspAtkSelection *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkselection);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkselection);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkSelection
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_selection_add_selection(NspAtkSelection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i, ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_selection_add_selection(ATK_SELECTION(self->obj),i);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_selection_clear_selection(NspAtkSelection *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_selection_clear_selection(ATK_SELECTION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_selection_ref_selection(NspAtkSelection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_selection_ref_selection(ATK_SELECTION(self->obj),i);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_selection_get_selection_count(NspAtkSelection *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_selection_get_selection_count(ATK_SELECTION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_selection_is_child_selected(NspAtkSelection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i, ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_selection_is_child_selected(ATK_SELECTION(self->obj),i);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_selection_remove_selection(NspAtkSelection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i, ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_selection_remove_selection(ATK_SELECTION(self->obj),i);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_selection_select_all_selection(NspAtkSelection *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_selection_select_all_selection(ATK_SELECTION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods atkselection_methods[] = {
  {"add_selection",(nsp_method *) _wrap_atk_selection_add_selection},
  {"clear_selection",(nsp_method *) _wrap_atk_selection_clear_selection},
  {"ref_selection",(nsp_method *) _wrap_atk_selection_ref_selection},
  {"get_selection_count",(nsp_method *) _wrap_atk_selection_get_selection_count},
  {"is_child_selected",(nsp_method *) _wrap_atk_selection_is_child_selected},
  {"remove_selection",(nsp_method *) _wrap_atk_selection_remove_selection},
  {"select_all_selection",(nsp_method *) _wrap_atk_selection_select_all_selection},
  { NULL, NULL}
};

static NspMethods *atkselection_get_methods(void) { return atkselection_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkselection_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkStreamableContent ----------- */


#define  NspAtkStreamableContent_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkstreamablecontent.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkStreamableContent inherits from GObject 
 */

int nsp_type_atkstreamablecontent_id=0;
NspTypeAtkStreamableContent *nsp_type_atkstreamablecontent=NULL;

/*
 * Type object for NspAtkStreamableContent 
 * all the instance of NspTypeAtkStreamableContent share the same id. 
 * nsp_type_atkstreamablecontent: is an instance of NspTypeAtkStreamableContent 
 *    used for objects of NspAtkStreamableContent type (i.e built with new_atkstreamablecontent) 
 * other instances are used for derived classes 
 */
NspTypeAtkStreamableContent *new_type_atkstreamablecontent(type_mode mode)
{
  NspTypeAtkStreamableContent *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkstreamablecontent != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkstreamablecontent;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkstreamablecontent_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkstreamablecontent_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkstreamablecontent;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkstreamablecontent */ 

  top->s_type =  (s_type_func *) nsp_atkstreamablecontent_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkstreamablecontent_type_short_string;
  /* top->create = (create_func*) int_atkstreamablecontent_create;*/

  /* specific methods for atkstreamablecontent */

  type->init = (init_func *) init_atkstreamablecontent;

  /* 
   * NspAtkStreamableContent interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkstreamablecontent_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkStreamableContent called nsp_type_atkstreamablecontent
       */
      type->id =  nsp_type_atkstreamablecontent_id = nsp_new_type_id();
      nsp_type_atkstreamablecontent = type;
      if ( nsp_register_type(nsp_type_atkstreamablecontent) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkstreamablecontent, ATK_TYPE_STREAMABLE_CONTENT);
      return ( mode == T_BASE ) ? type : new_type_atkstreamablecontent(mode);
    }
  else 
    {
      type->id = nsp_type_atkstreamablecontent_id;
      return type;
    }
}

/*
 * initialize NspAtkStreamableContent instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkstreamablecontent(NspAtkStreamableContent *Obj,NspTypeAtkStreamableContent *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkStreamableContent 
 */

NspAtkStreamableContent *new_atkstreamablecontent() 
{
  NspAtkStreamableContent *loc;
  /* type must exists */
  nsp_type_atkstreamablecontent = new_type_atkstreamablecontent(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkStreamableContent)))== NULLATKSTREAMABLECONTENT) return loc;
  /* initialize object */
  if ( init_atkstreamablecontent(loc,nsp_type_atkstreamablecontent) == FAIL) return NULLATKSTREAMABLECONTENT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkStreamableContent 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkstreamablecontent_type_name[]="AtkStreamableContent";
static char atkstreamablecontent_short_type_name[]="AtkStreamableContent";

static char *nsp_atkstreamablecontent_type_as_string(void)
{
  return(atkstreamablecontent_type_name);
}

static char *nsp_atkstreamablecontent_type_short_string(NspObject *v)
{
  return(atkstreamablecontent_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkStreamableContent objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkStreamableContent   *nsp_atkstreamablecontent_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkstreamablecontent_id)   ) return ((NspAtkStreamableContent *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkstreamablecontent));
  return NULL;
}

int IsAtkStreamableContentObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkstreamablecontent_id);
}

int IsAtkStreamableContent(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkstreamablecontent_id);
}

NspAtkStreamableContent  *GetAtkStreamableContentCopy(Stack stack, int i)
{
  if (  GetAtkStreamableContent(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkStreamableContent  *GetAtkStreamableContent(Stack stack, int i)
{
  NspAtkStreamableContent *M;
  if (( M = nsp_atkstreamablecontent_object(NthObj(i))) == NULLATKSTREAMABLECONTENT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkStreamableContent *atkstreamablecontent_copy(NspAtkStreamableContent *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkstreamablecontent);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkstreamablecontent);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkStreamableContent
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_streamable_content_get_n_mime_types(NspAtkStreamableContent *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_streamable_content_get_n_mime_types(ATK_STREAMABLE_CONTENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_streamable_content_get_mime_type(NspAtkStreamableContent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_streamable_content_get_mime_type(ATK_STREAMABLE_CONTENT(self->obj),i);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods atkstreamablecontent_methods[] = {
  {"get_n_mime_types",(nsp_method *) _wrap_atk_streamable_content_get_n_mime_types},
  {"get_mime_type",(nsp_method *) _wrap_atk_streamable_content_get_mime_type},
  { NULL, NULL}
};

static NspMethods *atkstreamablecontent_get_methods(void) { return atkstreamablecontent_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkstreamablecontent_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkTable ----------- */


#define  NspAtkTable_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atktable.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkTable inherits from GObject 
 */

int nsp_type_atktable_id=0;
NspTypeAtkTable *nsp_type_atktable=NULL;

/*
 * Type object for NspAtkTable 
 * all the instance of NspTypeAtkTable share the same id. 
 * nsp_type_atktable: is an instance of NspTypeAtkTable 
 *    used for objects of NspAtkTable type (i.e built with new_atktable) 
 * other instances are used for derived classes 
 */
NspTypeAtkTable *new_type_atktable(type_mode mode)
{
  NspTypeAtkTable *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atktable != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atktable;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atktable_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atktable_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atktable;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atktable */ 

  top->s_type =  (s_type_func *) nsp_atktable_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atktable_type_short_string;
  /* top->create = (create_func*) int_atktable_create;*/

  /* specific methods for atktable */

  type->init = (init_func *) init_atktable;

  /* 
   * NspAtkTable interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atktable_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkTable called nsp_type_atktable
       */
      type->id =  nsp_type_atktable_id = nsp_new_type_id();
      nsp_type_atktable = type;
      if ( nsp_register_type(nsp_type_atktable) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atktable, ATK_TYPE_TABLE);
      return ( mode == T_BASE ) ? type : new_type_atktable(mode);
    }
  else 
    {
      type->id = nsp_type_atktable_id;
      return type;
    }
}

/*
 * initialize NspAtkTable instances 
 * locally and by calling initializer on parent class 
 */

static int init_atktable(NspAtkTable *Obj,NspTypeAtkTable *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkTable 
 */

NspAtkTable *new_atktable() 
{
  NspAtkTable *loc;
  /* type must exists */
  nsp_type_atktable = new_type_atktable(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkTable)))== NULLATKTABLE) return loc;
  /* initialize object */
  if ( init_atktable(loc,nsp_type_atktable) == FAIL) return NULLATKTABLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkTable 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atktable_type_name[]="AtkTable";
static char atktable_short_type_name[]="AtkTable";

static char *nsp_atktable_type_as_string(void)
{
  return(atktable_type_name);
}

static char *nsp_atktable_type_short_string(NspObject *v)
{
  return(atktable_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkTable objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkTable   *nsp_atktable_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atktable_id)   ) return ((NspAtkTable *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atktable));
  return NULL;
}

int IsAtkTableObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atktable_id);
}

int IsAtkTable(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atktable_id);
}

NspAtkTable  *GetAtkTableCopy(Stack stack, int i)
{
  if (  GetAtkTable(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkTable  *GetAtkTable(Stack stack, int i)
{
  NspAtkTable *M;
  if (( M = nsp_atktable_object(NthObj(i))) == NULLATKTABLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkTable *atktable_copy(NspAtkTable *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atktable);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atktable);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkTable
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_table_ref_at(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int row, column;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&row, &column) == FAIL) return RET_BUG;
    ret =atk_table_ref_at(ATK_TABLE(self->obj),row,column);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_table_get_n_columns(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_table_get_n_columns(ATK_TABLE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_get_n_rows(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_table_get_n_rows(ATK_TABLE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_get_column_extent_at(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int row, column, ret;
  if ( GetArgs(stack,rhs,opt,T,&row, &column) == FAIL) return RET_BUG;
    ret =atk_table_get_column_extent_at(ATK_TABLE(self->obj),row,column);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_get_row_extent_at(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int row, column, ret;
  if ( GetArgs(stack,rhs,opt,T,&row, &column) == FAIL) return RET_BUG;
    ret =atk_table_get_row_extent_at(ATK_TABLE(self->obj),row,column);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_get_caption(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  AtkObject *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =atk_table_get_caption(ATK_TABLE(self->obj));
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_table_get_column_description(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int column;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&column) == FAIL) return RET_BUG;
    ret =atk_table_get_column_description(ATK_TABLE(self->obj),column);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_get_column_header(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int column;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&column) == FAIL) return RET_BUG;
    ret =atk_table_get_column_header(ATK_TABLE(self->obj),column);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_table_get_row_description(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int row;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&row) == FAIL) return RET_BUG;
    ret =atk_table_get_row_description(ATK_TABLE(self->obj),row);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_get_row_header(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int row;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&row) == FAIL) return RET_BUG;
    ret =atk_table_get_row_header(ATK_TABLE(self->obj),row);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_table_get_summary(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  AtkObject *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =atk_table_get_summary(ATK_TABLE(self->obj));
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_table_set_caption(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *caption;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkobject, &caption) == FAIL) return RET_BUG;
    atk_table_set_caption(ATK_TABLE(self->obj),ATK_OBJECT(caption->obj));
  return 0;
}

static int _wrap_atk_table_set_column_description(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string, t_end};
  int column;
  char *description;
  if ( GetArgs(stack,rhs,opt,T,&column, &description) == FAIL) return RET_BUG;
    atk_table_set_column_description(ATK_TABLE(self->obj),column,description);
  return 0;
}

static int _wrap_atk_table_set_column_header(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int column;
  NspGObject *header;
  if ( GetArgs(stack,rhs,opt,T,&column, &nsp_type_atkobject, &header) == FAIL) return RET_BUG;
    atk_table_set_column_header(ATK_TABLE(self->obj),column,ATK_OBJECT(header->obj));
  return 0;
}

static int _wrap_atk_table_set_row_description(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string, t_end};
  int row;
  char *description;
  if ( GetArgs(stack,rhs,opt,T,&row, &description) == FAIL) return RET_BUG;
    atk_table_set_row_description(ATK_TABLE(self->obj),row,description);
  return 0;
}

static int _wrap_atk_table_set_row_header(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int row;
  NspGObject *header;
  if ( GetArgs(stack,rhs,opt,T,&row, &nsp_type_atkobject, &header) == FAIL) return RET_BUG;
    atk_table_set_row_header(ATK_TABLE(self->obj),row,ATK_OBJECT(header->obj));
  return 0;
}

static int _wrap_atk_table_set_summary(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *accessible;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkobject, &accessible) == FAIL) return RET_BUG;
    atk_table_set_summary(ATK_TABLE(self->obj),ATK_OBJECT(accessible->obj));
  return 0;
}

static int _wrap_atk_table_is_column_selected(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int column, ret;
  if ( GetArgs(stack,rhs,opt,T,&column) == FAIL) return RET_BUG;
    ret =atk_table_is_column_selected(ATK_TABLE(self->obj),column);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_is_row_selected(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int row, ret;
  if ( GetArgs(stack,rhs,opt,T,&row) == FAIL) return RET_BUG;
    ret =atk_table_is_row_selected(ATK_TABLE(self->obj),row);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_is_selected(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int row, column, ret;
  if ( GetArgs(stack,rhs,opt,T,&row, &column) == FAIL) return RET_BUG;
    ret =atk_table_is_selected(ATK_TABLE(self->obj),row,column);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_add_row_selection(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int row, ret;
  if ( GetArgs(stack,rhs,opt,T,&row) == FAIL) return RET_BUG;
    ret =atk_table_add_row_selection(ATK_TABLE(self->obj),row);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_remove_row_selection(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int row, ret;
  if ( GetArgs(stack,rhs,opt,T,&row) == FAIL) return RET_BUG;
    ret =atk_table_remove_row_selection(ATK_TABLE(self->obj),row);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_add_column_selection(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int column, ret;
  if ( GetArgs(stack,rhs,opt,T,&column) == FAIL) return RET_BUG;
    ret =atk_table_add_column_selection(ATK_TABLE(self->obj),column);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_table_remove_column_selection(NspAtkTable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int column, ret;
  if ( GetArgs(stack,rhs,opt,T,&column) == FAIL) return RET_BUG;
    ret =atk_table_remove_column_selection(ATK_TABLE(self->obj),column);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods atktable_methods[] = {
  {"ref_at",(nsp_method *) _wrap_atk_table_ref_at},
  {"get_n_columns",(nsp_method *) _wrap_atk_table_get_n_columns},
  {"get_n_rows",(nsp_method *) _wrap_atk_table_get_n_rows},
  {"get_column_extent_at",(nsp_method *) _wrap_atk_table_get_column_extent_at},
  {"get_row_extent_at",(nsp_method *) _wrap_atk_table_get_row_extent_at},
  {"get_caption",(nsp_method *) _wrap_atk_table_get_caption},
  {"get_column_description",(nsp_method *) _wrap_atk_table_get_column_description},
  {"get_column_header",(nsp_method *) _wrap_atk_table_get_column_header},
  {"get_row_description",(nsp_method *) _wrap_atk_table_get_row_description},
  {"get_row_header",(nsp_method *) _wrap_atk_table_get_row_header},
  {"get_summary",(nsp_method *) _wrap_atk_table_get_summary},
  {"set_caption",(nsp_method *) _wrap_atk_table_set_caption},
  {"set_column_description",(nsp_method *) _wrap_atk_table_set_column_description},
  {"set_column_header",(nsp_method *) _wrap_atk_table_set_column_header},
  {"set_row_description",(nsp_method *) _wrap_atk_table_set_row_description},
  {"set_row_header",(nsp_method *) _wrap_atk_table_set_row_header},
  {"set_summary",(nsp_method *) _wrap_atk_table_set_summary},
  {"is_column_selected",(nsp_method *) _wrap_atk_table_is_column_selected},
  {"is_row_selected",(nsp_method *) _wrap_atk_table_is_row_selected},
  {"is_selected",(nsp_method *) _wrap_atk_table_is_selected},
  {"add_row_selection",(nsp_method *) _wrap_atk_table_add_row_selection},
  {"remove_row_selection",(nsp_method *) _wrap_atk_table_remove_row_selection},
  {"add_column_selection",(nsp_method *) _wrap_atk_table_add_column_selection},
  {"remove_column_selection",(nsp_method *) _wrap_atk_table_remove_column_selection},
  { NULL, NULL}
};

static NspMethods *atktable_get_methods(void) { return atktable_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atktable_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkText ----------- */


#define  NspAtkText_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atktext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkText inherits from GObject 
 */

int nsp_type_atktext_id=0;
NspTypeAtkText *nsp_type_atktext=NULL;

/*
 * Type object for NspAtkText 
 * all the instance of NspTypeAtkText share the same id. 
 * nsp_type_atktext: is an instance of NspTypeAtkText 
 *    used for objects of NspAtkText type (i.e built with new_atktext) 
 * other instances are used for derived classes 
 */
NspTypeAtkText *new_type_atktext(type_mode mode)
{
  NspTypeAtkText *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atktext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atktext;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atktext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atktext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atktext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atktext */ 

  top->s_type =  (s_type_func *) nsp_atktext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atktext_type_short_string;
  /* top->create = (create_func*) int_atktext_create;*/

  /* specific methods for atktext */

  type->init = (init_func *) init_atktext;

  /* 
   * NspAtkText interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atktext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkText called nsp_type_atktext
       */
      type->id =  nsp_type_atktext_id = nsp_new_type_id();
      nsp_type_atktext = type;
      if ( nsp_register_type(nsp_type_atktext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atktext, ATK_TYPE_TEXT);
      return ( mode == T_BASE ) ? type : new_type_atktext(mode);
    }
  else 
    {
      type->id = nsp_type_atktext_id;
      return type;
    }
}

/*
 * initialize NspAtkText instances 
 * locally and by calling initializer on parent class 
 */

static int init_atktext(NspAtkText *Obj,NspTypeAtkText *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkText 
 */

NspAtkText *new_atktext() 
{
  NspAtkText *loc;
  /* type must exists */
  nsp_type_atktext = new_type_atktext(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkText)))== NULLATKTEXT) return loc;
  /* initialize object */
  if ( init_atktext(loc,nsp_type_atktext) == FAIL) return NULLATKTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkText 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atktext_type_name[]="AtkText";
static char atktext_short_type_name[]="AtkText";

static char *nsp_atktext_type_as_string(void)
{
  return(atktext_type_name);
}

static char *nsp_atktext_type_short_string(NspObject *v)
{
  return(atktext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkText objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkText   *nsp_atktext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atktext_id)   ) return ((NspAtkText *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atktext));
  return NULL;
}

int IsAtkTextObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atktext_id);
}

int IsAtkText(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atktext_id);
}

NspAtkText  *GetAtkTextCopy(Stack stack, int i)
{
  if (  GetAtkText(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkText  *GetAtkText(Stack stack, int i)
{
  NspAtkText *M;
  if (( M = nsp_atktext_object(NthObj(i))) == NULLATKTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkText *atktext_copy(NspAtkText *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atktext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atktext);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkText
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_text_get_text(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int start_offset, end_offset;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&start_offset, &end_offset) == FAIL) return RET_BUG;
    ret =atk_text_get_text(ATK_TEXT(self->obj),start_offset,end_offset);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_atk_text_get_character_at_offset(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int offset;
  gunichar ret;
  if ( GetArgs(stack,rhs,opt,T,&offset) == FAIL) return RET_BUG;
    ret =atk_text_get_character_at_offset(ATK_TEXT(self->obj),offset);
  if ( nsp_move_double(stack,1,(double) ret)== FAIL)return RET_BUG;
  return 1;
}

static int _wrap_atk_text_get_caret_offset(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_text_get_caret_offset(ATK_TEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_text_get_character_extents(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int,s_int,s_int,obj, t_end};
  int offset, x, y, width, height;
  AtkCoordType coords;
  NspObject *nsp_coords = NULL;
  if ( GetArgs(stack,rhs,opt,T,&offset, &x, &y, &width, &height, &nsp_coords) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_coords, &coords)== FAIL)
      return RET_BUG;
    atk_text_get_character_extents(ATK_TEXT(self->obj),offset,&x,&y,&width,&height,coords);
  return 0;
}

static int _wrap_atk_text_get_character_count(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_text_get_character_count(ATK_TEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_text_get_offset_at_point(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,obj, t_end};
  int x, y, ret;
  AtkCoordType coords;
  NspObject *nsp_coords = NULL;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &nsp_coords) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_coords, &coords)== FAIL)
      return RET_BUG;
    ret =atk_text_get_offset_at_point(ATK_TEXT(self->obj),x,y,coords);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_text_get_n_selections(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_text_get_n_selections(ATK_TEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_text_get_selection(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int, t_end};
  int selection_num, start_offset, end_offset;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&selection_num, &start_offset, &end_offset) == FAIL) return RET_BUG;
    ret =atk_text_get_selection(ATK_TEXT(self->obj),selection_num,&start_offset,&end_offset);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_atk_text_add_selection(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int start_offset, end_offset, ret;
  if ( GetArgs(stack,rhs,opt,T,&start_offset, &end_offset) == FAIL) return RET_BUG;
    ret =atk_text_add_selection(ATK_TEXT(self->obj),start_offset,end_offset);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_text_remove_selection(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int selection_num, ret;
  if ( GetArgs(stack,rhs,opt,T,&selection_num) == FAIL) return RET_BUG;
    ret =atk_text_remove_selection(ATK_TEXT(self->obj),selection_num);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_text_set_selection(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int, t_end};
  int selection_num, start_offset, end_offset, ret;
  if ( GetArgs(stack,rhs,opt,T,&selection_num, &start_offset, &end_offset) == FAIL) return RET_BUG;
    ret =atk_text_set_selection(ATK_TEXT(self->obj),selection_num,start_offset,end_offset);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_text_set_caret_offset(NspAtkText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int offset, ret;
  if ( GetArgs(stack,rhs,opt,T,&offset) == FAIL) return RET_BUG;
    ret =atk_text_set_caret_offset(ATK_TEXT(self->obj),offset);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods atktext_methods[] = {
  {"get_text",(nsp_method *) _wrap_atk_text_get_text},
  {"get_character_at_offset",(nsp_method *) _wrap_atk_text_get_character_at_offset},
  {"get_caret_offset",(nsp_method *) _wrap_atk_text_get_caret_offset},
  {"get_character_extents",(nsp_method *) _wrap_atk_text_get_character_extents},
  {"get_character_count",(nsp_method *) _wrap_atk_text_get_character_count},
  {"get_offset_at_point",(nsp_method *) _wrap_atk_text_get_offset_at_point},
  {"get_n_selections",(nsp_method *) _wrap_atk_text_get_n_selections},
  {"get_selection",(nsp_method *) _wrap_atk_text_get_selection},
  {"add_selection",(nsp_method *) _wrap_atk_text_add_selection},
  {"remove_selection",(nsp_method *) _wrap_atk_text_remove_selection},
  {"set_selection",(nsp_method *) _wrap_atk_text_set_selection},
  {"set_caret_offset",(nsp_method *) _wrap_atk_text_set_caret_offset},
  { NULL, NULL}
};

static NspMethods *atktext_get_methods(void) { return atktext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atktext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkValue ----------- */


#define  NspAtkValue_Private 
#include <nsp/objects.h>
#include <nsp/gtk/atkvalue.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspAtkValue inherits from GObject 
 */

int nsp_type_atkvalue_id=0;
NspTypeAtkValue *nsp_type_atkvalue=NULL;

/*
 * Type object for NspAtkValue 
 * all the instance of NspTypeAtkValue share the same id. 
 * nsp_type_atkvalue: is an instance of NspTypeAtkValue 
 *    used for objects of NspAtkValue type (i.e built with new_atkvalue) 
 * other instances are used for derived classes 
 */
NspTypeAtkValue *new_type_atkvalue(type_mode mode)
{
  NspTypeAtkValue *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_atkvalue != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_atkvalue;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkvalue_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = atkvalue_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_atkvalue;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for atkvalue */ 

  top->s_type =  (s_type_func *) nsp_atkvalue_type_as_string;
  top->sh_type = (sh_type_func *) nsp_atkvalue_type_short_string;
  /* top->create = (create_func*) int_atkvalue_create;*/

  /* specific methods for atkvalue */

  type->init = (init_func *) init_atkvalue;

  /* 
   * NspAtkValue interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_atkvalue_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAtkValue called nsp_type_atkvalue
       */
      type->id =  nsp_type_atkvalue_id = nsp_new_type_id();
      nsp_type_atkvalue = type;
      if ( nsp_register_type(nsp_type_atkvalue) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_atkvalue, ATK_TYPE_VALUE);
      return ( mode == T_BASE ) ? type : new_type_atkvalue(mode);
    }
  else 
    {
      type->id = nsp_type_atkvalue_id;
      return type;
    }
}

/*
 * initialize NspAtkValue instances 
 * locally and by calling initializer on parent class 
 */

static int init_atkvalue(NspAtkValue *Obj,NspTypeAtkValue *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspAtkValue 
 */

NspAtkValue *new_atkvalue() 
{
  NspAtkValue *loc;
  /* type must exists */
  nsp_type_atkvalue = new_type_atkvalue(T_BASE);
  if ( (loc = malloc(sizeof(NspAtkValue)))== NULLATKVALUE) return loc;
  /* initialize object */
  if ( init_atkvalue(loc,nsp_type_atkvalue) == FAIL) return NULLATKVALUE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAtkValue 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char atkvalue_type_name[]="AtkValue";
static char atkvalue_short_type_name[]="AtkValue";

static char *nsp_atkvalue_type_as_string(void)
{
  return(atkvalue_type_name);
}

static char *nsp_atkvalue_type_short_string(NspObject *v)
{
  return(atkvalue_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAtkValue objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAtkValue   *nsp_atkvalue_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_atkvalue_id)   ) return ((NspAtkValue *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_atkvalue));
  return NULL;
}

int IsAtkValueObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_atkvalue_id);
}

int IsAtkValue(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_atkvalue_id);
}

NspAtkValue  *GetAtkValueCopy(Stack stack, int i)
{
  if (  GetAtkValue(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAtkValue  *GetAtkValue(Stack stack, int i)
{
  NspAtkValue *M;
  if (( M = nsp_atkvalue_object(NthObj(i))) == NULLATKVALUE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspAtkValue *atkvalue_copy(NspAtkValue *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkvalue);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_atkvalue);
}

/*-------------------------------------------------------------------
 * wrappers for the AtkValue
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *atkvalue_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkvalue_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkHyperlink ----------- */


#define  NspAtkHyperlink_Private 
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
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkhyperlink_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkHyperlink   *nsp_atkhyperlink_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atkhyperlink_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_hyperlink_get_uri(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_hyperlink_get_uri(ATK_HYPERLINK(self->obj),i);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_atk_hyperlink_get_object(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_hyperlink_get_object(ATK_HYPERLINK(self->obj),i);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_hyperlink_get_end_index(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_hyperlink_get_end_index(ATK_HYPERLINK(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_hyperlink_get_start_index(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_hyperlink_get_start_index(ATK_HYPERLINK(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_hyperlink_is_valid(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_hyperlink_is_valid(ATK_HYPERLINK(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_hyperlink_get_n_anchors(NspAtkHyperlink *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_hyperlink_get_n_anchors(ATK_HYPERLINK(self->obj));
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

static AttrTab atkhyperlink_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkObject ----------- */


#define  NspAtkObject_Private 
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
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkobject_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkObject   *nsp_atkobject_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atkobject_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_object_get_name(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =atk_object_get_name(ATK_OBJECT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_get_description(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =atk_object_get_description(ATK_OBJECT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_get_parent(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  AtkObject *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =atk_object_get_parent(ATK_OBJECT(self->obj));
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_get_n_accessible_children(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_object_get_n_accessible_children(ATK_OBJECT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_ref_accessible_child(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_object_ref_accessible_child(ATK_OBJECT(self->obj),i);
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_ref_relation_set(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  AtkRelationSet *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =atk_object_ref_relation_set(ATK_OBJECT(self->obj));
  nsp_type_atkrelationset = new_type_atkrelationset(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkrelationset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_get_role(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =atk_object_get_role(ATK_OBJECT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_ref_state_set(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  AtkStateSet *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =atk_object_ref_state_set(ATK_OBJECT(self->obj));
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkstateset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_object_get_index_in_parent(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_object_get_index_in_parent(ATK_OBJECT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_object_set_name(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    atk_object_set_name(ATK_OBJECT(self->obj),name);
  return 0;
}

static int _wrap_atk_object_set_description(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *description;
  if ( GetArgs(stack,rhs,opt,T,&description) == FAIL) return RET_BUG;
    atk_object_set_description(ATK_OBJECT(self->obj),description);
  return 0;
}

static int _wrap_atk_object_set_parent(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *parent;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkobject, &parent) == FAIL) return RET_BUG;
    atk_object_set_parent(ATK_OBJECT(self->obj),ATK_OBJECT(parent->obj));
  return 0;
}

static int _wrap_atk_object_set_role(NspAtkObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  AtkRole role;
  NspObject *nsp_role = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_role) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_role, &role)== FAIL)
      return RET_BUG;
    atk_object_set_role(ATK_OBJECT(self->obj),role);
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
  {"ref_state_set",(nsp_method *) _wrap_atk_object_ref_state_set},
  {"get_index_in_parent",(nsp_method *) _wrap_atk_object_get_index_in_parent},
  {"set_name",(nsp_method *) _wrap_atk_object_set_name},
  {"set_description",(nsp_method *) _wrap_atk_object_set_description},
  {"set_parent",(nsp_method *) _wrap_atk_object_set_parent},
  {"set_role",(nsp_method *) _wrap_atk_object_set_role},
  { NULL, NULL}
};

static NspMethods *atkobject_get_methods(void) { return atkobject_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkobject_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkNoOpObject ----------- */


#define  NspAtkNoOpObject_Private 
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
  if (( type =  malloc(sizeof(NspTypeAtkObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_atkobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atknoopobject_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkNoOpObject   *nsp_atknoopobject_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atknoopobject_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_atk_no_op_object_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *obj = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gobject, &obj) == FAIL) return RET_BUG;
  if ((ret = (GObject *)atk_no_op_object_new(G_OBJECT(obj->obj)))== NULL) return RET_BUG;

  nsp_type_atknoopobject = new_type_atknoopobject(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atknoopobject);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *atknoopobject_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atknoopobject_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkObjectFactory ----------- */


#define  NspAtkObjectFactory_Private 
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
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkobjectfactory_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkObjectFactory   *nsp_atkobjectfactory_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atkobjectfactory_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_object_factory_create_accessible(NspAtkObjectFactory *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *obj = NULL;
  AtkObject *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gobject, &obj) == FAIL) return RET_BUG;
    ret =atk_object_factory_create_accessible(ATK_OBJECT_FACTORY(self->obj),G_OBJECT(obj->obj));
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
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

static AttrTab atkobjectfactory_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkNoOpObjectFactory ----------- */


#define  NspAtkNoOpObjectFactory_Private 
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
  if (( type =  malloc(sizeof(NspTypeAtkObjectFactory))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_atkobjectfactory(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atknoopobjectfactory_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkNoOpObjectFactory   *nsp_atknoopobjectfactory_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atknoopobjectfactory_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_atk_no_op_object_factory_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)atk_no_op_object_factory_new())== NULL) return RET_BUG;

  nsp_type_atknoopobjectfactory = new_type_atknoopobjectfactory(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atknoopobjectfactory);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *atknoopobjectfactory_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atknoopobjectfactory_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkRegistry ----------- */


#define  NspAtkRegistry_Private 
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
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkregistry_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkRegistry   *nsp_atkregistry_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atkregistry_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_registry_set_factory_type(NspAtkRegistry *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj, t_end};
  GType type, factory_type;
  NspObject *nsp_type = NULL, *nsp_factory_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type, &nsp_factory_type) == FAIL) return RET_BUG;
  if ((type = nspg_type_from_object(nsp_type)) == FAIL)
      return RET_BUG;
  if ((factory_type = nspg_type_from_object(nsp_factory_type)) == FAIL)
      return RET_BUG;
    atk_registry_set_factory_type(ATK_REGISTRY(self->obj),type,factory_type);
  return 0;
}

static int _wrap_atk_registry_get_factory_type(NspAtkRegistry *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GType type, ret;
  NspObject *nsp_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if ((type = nspg_type_from_object(nsp_type)) == FAIL)
      return RET_BUG;
    ret =atk_registry_get_factory_type(ATK_REGISTRY(self->obj),type);
  return nspg_type_wrapper_new(ret);
}

static int _wrap_atk_registry_get_factory(NspAtkRegistry *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GType type;
  NspObject *nsp_type = NULL, *nsp_ret;
  AtkObjectFactory *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if ((type = nspg_type_from_object(nsp_type)) == FAIL)
      return RET_BUG;
    ret =atk_registry_get_factory(ATK_REGISTRY(self->obj),type);
  nsp_type_atkobjectfactory = new_type_atkobjectfactory(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobjectfactory))== NULL) return RET_BUG;
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

static AttrTab atkregistry_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkRelation ----------- */


#define  NspAtkRelation_Private 
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
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkrelation_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkRelation   *nsp_atkrelation_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atkrelation_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 31 "codegen-3.0/atk.override"

static int
_wrap_atk_relation_new(Stack stack,int rhs,int opt,int lhs)
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
#line 5549 "atk.c"


static int _wrap_atk_relation_get_relation_type(NspAtkRelation *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =atk_relation_get_relation_type(ATK_RELATION(self->obj));
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

static AttrTab atkrelation_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkRelationSet ----------- */


#define  NspAtkRelationSet_Private 
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
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkrelationset_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkRelationSet   *nsp_atkrelationset_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atkrelationset_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_atk_relation_set_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)atk_relation_set_new())== NULL) return RET_BUG;

  nsp_type_atkrelationset = new_type_atkrelationset(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkrelationset);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_relation_set_contains(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  AtkRelationType relationship;
  NspObject *nsp_relationship = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_relationship) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_relationship, &relationship)== FAIL)
      return RET_BUG;
    ret =atk_relation_set_contains(ATK_RELATION_SET(self->obj),relationship);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_relation_set_remove(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *relation;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkrelation, &relation) == FAIL) return RET_BUG;
    atk_relation_set_remove(ATK_RELATION_SET(self->obj),ATK_RELATION(relation->obj));
  return 0;
}

static int _wrap_atk_relation_set_add(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *relation;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkrelation, &relation) == FAIL) return RET_BUG;
    atk_relation_set_add(ATK_RELATION_SET(self->obj),ATK_RELATION(relation->obj));
  return 0;
}

static int _wrap_atk_relation_set_get_n_relations(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_relation_set_get_n_relations(ATK_RELATION_SET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_relation_set_get_relation(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int i;
  AtkRelation *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&i) == FAIL) return RET_BUG;
    ret =atk_relation_set_get_relation(ATK_RELATION_SET(self->obj),i);
  nsp_type_atkrelation = new_type_atkrelation(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkrelation))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_relation_set_get_relation_by_type(NspAtkRelationSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  AtkRelationType relationship;
  NspObject *nsp_relationship = NULL, *nsp_ret;
  AtkRelation *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_relationship) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_relationship, &relationship)== FAIL)
      return RET_BUG;
    ret =atk_relation_set_get_relation_by_type(ATK_RELATION_SET(self->obj),relationship);
  nsp_type_atkrelation = new_type_atkrelation(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkrelation))== NULL) return RET_BUG;
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

static AttrTab atkrelationset_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkStateSet ----------- */


#define  NspAtkStateSet_Private 
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
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkstateset_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkStateSet   *nsp_atkstateset_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atkstateset_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_atk_state_set_is_empty(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =atk_state_set_is_empty(ATK_STATE_SET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_state_set_add_state(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  AtkStateType type;
  NspObject *nsp_type = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_type, &type)== FAIL)
      return RET_BUG;
    ret =atk_state_set_add_state(ATK_STATE_SET(self->obj),type);
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
  int_types T[] = {obj, t_end};
  AtkStateType type;
  NspObject *nsp_type = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_type, &type)== FAIL)
      return RET_BUG;
    ret =atk_state_set_contains_state(ATK_STATE_SET(self->obj),type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_state_set_remove_state(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  AtkStateType type;
  NspObject *nsp_type = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_type, &type)== FAIL)
      return RET_BUG;
    ret =atk_state_set_remove_state(ATK_STATE_SET(self->obj),type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_atk_state_set_and_sets(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *compare_set;
  AtkStateSet *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkstateset, &compare_set) == FAIL) return RET_BUG;
    ret =atk_state_set_and_sets(ATK_STATE_SET(self->obj),ATK_STATE_SET(compare_set->obj));
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkstateset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_state_set_or_sets(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *compare_set;
  AtkStateSet *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkstateset, &compare_set) == FAIL) return RET_BUG;
    ret =atk_state_set_or_sets(ATK_STATE_SET(self->obj),ATK_STATE_SET(compare_set->obj));
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkstateset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_atk_state_set_xor_sets(NspAtkStateSet *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *compare_set;
  AtkStateSet *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_atkstateset, &compare_set) == FAIL) return RET_BUG;
    ret =atk_state_set_xor_sets(ATK_STATE_SET(self->obj),ATK_STATE_SET(compare_set->obj));
  nsp_type_atkstateset = new_type_atkstateset(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkstateset))== NULL) return RET_BUG;
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

static AttrTab atkstateset_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspAtkUtil ----------- */


#define  NspAtkUtil_Private 
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
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = atkutil_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
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

NspAtkUtil   *nsp_atkutil_object(NspObject *O)
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
  return nsp_object_type(NthObj(i),nsp_type_atkutil_id);
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
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *atkutil_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab atkutil_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_atk_get_default_registry(Stack stack, int rhs, int opt, int lhs) /* get_default_registry */
{
  AtkRegistry *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =atk_get_default_registry();
  nsp_type_atkregistry = new_type_atkregistry(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkregistry))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_atk_relation_type_register(Stack stack, int rhs, int opt, int lhs) /* relation_type_register */
{
  int_types T[] = {string, t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =atk_relation_type_register(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_relation_type_for_name(Stack stack, int rhs, int opt, int lhs) /* relation_type_for_name */
{
  int_types T[] = {string, t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =atk_relation_type_for_name(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_state_type_for_name(Stack stack, int rhs, int opt, int lhs) /* state_type_for_name */
{
  int_types T[] = {string, t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =atk_state_type_for_name(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_text_attribute_get_name(Stack stack, int rhs, int opt, int lhs) /* text_attribute_get_name */
{
  int_types T[] = {obj, t_end};
  AtkTextAttribute attr;
  NspObject *nsp_attr = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_attr) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_attr, &attr)== FAIL)
      return RET_BUG;
    ret =atk_text_attribute_get_name(attr);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_text_attribute_get_value(Stack stack, int rhs, int opt, int lhs) /* text_attribute_get_value */
{
  int_types T[] = {obj,s_int, t_end};
  AtkTextAttribute attr;
  NspObject *nsp_attr = NULL;
  int index;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_attr, &index) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_attr, &attr)== FAIL)
      return RET_BUG;
    ret =atk_text_attribute_get_value(attr,index);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_remove_global_event_listener(Stack stack, int rhs, int opt, int lhs) /* remove_global_event_listener */
{
  int_types T[] = {s_int, t_end};
  int listener_id;
  if ( GetArgs(stack,rhs,opt,T,&listener_id) == FAIL) return RET_BUG;
    atk_remove_global_event_listener(listener_id);
  return 0;
}

int _wrap_atk_remove_key_event_listener(Stack stack, int rhs, int opt, int lhs) /* remove_key_event_listener */
{
  int_types T[] = {s_int, t_end};
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
    ret =atk_get_root();
  nsp_type_atkobject = new_type_atkobject(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_atkobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_atk_get_toolkit_name(Stack stack, int rhs, int opt, int lhs) /* get_toolkit_name */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =atk_get_toolkit_name();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_get_toolkit_version(Stack stack, int rhs, int opt, int lhs) /* get_toolkit_version */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =atk_get_toolkit_version();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_atk_role_for_name(Stack stack, int rhs, int opt, int lhs) /* role_for_name */
{
  int_types T[] = {string, t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =atk_role_for_name(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab atk_func[]={
  { "atk_no_op_object_new", _wrap_atk_no_op_object_new},
  { "atknoopobject_new", _wrap_atk_no_op_object_new},
  { "atk_no_op_object_factory_new", _wrap_atk_no_op_object_factory_new},
  { "atknoopobjectfactory_new", _wrap_atk_no_op_object_factory_new},
  { "atk_relation_new", _wrap_atk_relation_new},
  { "atkrelation_new", _wrap_atk_relation_new},
  { "atk_relation_set_new", _wrap_atk_relation_set_new},
  { "atkrelationset_new", _wrap_atk_relation_set_new},
  { "atk_get_default_registry", _wrap_atk_get_default_registry},
  { "atk_relation_type_register", _wrap_atk_relation_type_register},
  { "atk_relation_type_for_name", _wrap_atk_relation_type_for_name},
  { "atk_state_type_for_name", _wrap_atk_state_type_for_name},
  { "atk_text_attribute_get_name", _wrap_atk_text_attribute_get_name},
  { "atk_text_attribute_get_value", _wrap_atk_text_attribute_get_value},
  { "atk_remove_global_event_listener", _wrap_atk_remove_global_event_listener},
  { "atk_remove_key_event_listener", _wrap_atk_remove_key_event_listener},
  { "atk_get_root", _wrap_atk_get_root},
  { "atk_get_toolkit_name", _wrap_atk_get_toolkit_name},
  { "atk_get_toolkit_version", _wrap_atk_get_toolkit_version},
  { "atk_role_for_name", _wrap_atk_role_for_name},
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

void atk_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = atk_func[i].name;
  *f = atk_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
atk_add_constants(NspObject *module, const gchar *strip_prefix)
{
/* enum or flags without typecode: AtkRole */
/* enum or flags without typecode: AtkLayer */
/* enum or flags without typecode: AtkRelationType */
/* enum or flags without typecode: AtkStateType */
/* enum or flags without typecode: AtkTextAttribute */
/* enum or flags without typecode: AtkTextBoundary */
/* enum or flags without typecode: AtkKeyEventType */
/* enum or flags without typecode: AtkCoordType */
}

void nsp_initialize_atk_types(void)
{
  new_type_atkrectangle(T_BASE);
  new_type_atkaction(T_BASE);
  new_type_atkcomponent(T_BASE);
  new_type_atkdocument(T_BASE);
  new_type_atkeditabletext(T_BASE);
  new_type_atkhyperlinkimpl(T_BASE);
  new_type_atkhypertext(T_BASE);
  new_type_atkimage(T_BASE);
  new_type_atkimplementoriface(T_BASE);
  new_type_atkselection(T_BASE);
  new_type_atkstreamablecontent(T_BASE);
  new_type_atktable(T_BASE);
  new_type_atktext(T_BASE);
  new_type_atkvalue(T_BASE);
  new_type_atkhyperlink(T_BASE);
  new_type_atkobject(T_BASE);
  new_type_atknoopobject(T_BASE);
  new_type_atkobjectfactory(T_BASE);
  new_type_atknoopobjectfactory(T_BASE);
  new_type_atkregistry(T_BASE);
  new_type_atkrelation(T_BASE);
  new_type_atkrelationset(T_BASE);
  new_type_atkstateset(T_BASE);
  new_type_atkutil(T_BASE);
}

#line 6606 "atk.c"
