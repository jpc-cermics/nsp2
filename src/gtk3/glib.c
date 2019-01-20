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





#line 4 "codegen-3.0/glib.override"

#include <glib.h>
#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/hash.h>
#include <nsp/plist.h>
#include <nsp/list.h>
#include <nsp/cells.h>
#include <nsp/none.h>
#include <nsp/mpmatrix.h>
#include <nsp/matrix.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>
#include <nsp/interf.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gobject-util.h>
#include <nsp/gtk/gmaincontext.h>
#include <nsp/gtk/gmainloop.h>
#include <nsp/gtk/gsource.h>

#line 53 "glib.c"

/* -----------NspGMainContext ----------- */


#define  NspGMainContext_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmaincontext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMainContext inherits from GBoxed 
 */

int nsp_type_gmaincontext_id=0;
NspTypeGMainContext *nsp_type_gmaincontext=NULL;

/*
 * Type object for NspGMainContext 
 * all the instance of NspTypeGMainContext share the same id. 
 * nsp_type_gmaincontext: is an instance of NspTypeGMainContext 
 *    used for objects of NspGMainContext type (i.e built with new_gmaincontext) 
 * other instances are used for derived classes 
 */
NspTypeGMainContext *new_type_gmaincontext(type_mode mode)
{
  NspTypeGMainContext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmaincontext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmaincontext;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmaincontext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmaincontext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmaincontext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmaincontext */ 

  top->s_type =  (s_type_func *) nsp_gmaincontext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmaincontext_type_short_string;
  /* top->create = (create_func*) int_gmaincontext_create;*/

  /* specific methods for gmaincontext */

  type->init = (init_func *) init_gmaincontext;

  /* 
   * NspGMainContext interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmaincontext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMainContext called nsp_type_gmaincontext
       */
      type->id =  nsp_type_gmaincontext_id = nsp_new_type_id();
      nsp_type_gmaincontext = type;
      if ( nsp_register_type(nsp_type_gmaincontext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmaincontext, G_TYPE_MAIN_CONTEXT);
      return ( mode == T_BASE ) ? type : new_type_gmaincontext(mode);
    }
  else 
    {
      type->id = nsp_type_gmaincontext_id;
      return type;
    }
}

/*
 * initialize NspGMainContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmaincontext(NspGMainContext *Obj,NspTypeGMainContext *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMainContext 
 */

NspGMainContext *new_gmaincontext() 
{
  NspGMainContext *loc;
  /* type must exists */
  nsp_type_gmaincontext = new_type_gmaincontext(T_BASE);
  if ( (loc = malloc(sizeof(NspGMainContext)))== NULLGMAINCONTEXT) return loc;
  /* initialize object */
  if ( init_gmaincontext(loc,nsp_type_gmaincontext) == FAIL) return NULLGMAINCONTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMainContext 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmaincontext_type_name[]="GMainContext";
static char gmaincontext_short_type_name[]="GMainContext";

static char *nsp_gmaincontext_type_as_string(void)
{
  return(gmaincontext_type_name);
}

static char *nsp_gmaincontext_type_short_string(NspObject *v)
{
  return(gmaincontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMainContext objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMainContext   *nsp_gmaincontext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmaincontext_id)  == TRUE  ) return ((NspGMainContext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmaincontext));
  return NULL;
}

int IsGMainContextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmaincontext_id);
}

int IsGMainContext(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmaincontext_id);
}

NspGMainContext  *GetGMainContextCopy(Stack stack, int i)
{
  if (  GetGMainContext(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMainContext  *GetGMainContext(Stack stack, int i)
{
  NspGMainContext *M;
  if (( M = nsp_gmaincontext_object(NthObj(i))) == NULLGMAINCONTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGMainContext *gmaincontext_copy(NspGMainContext *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gmaincontext);
}

/*-------------------------------------------------------------------
 * wrappers for the GMainContext
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_main_context_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_main_context_new())== NULL) return RET_BUG;

  nsp_type_gmaincontext = new_type_gmaincontext(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_MAIN_CONTEXT, ret,TRUE,TRUE,(NspTypeBase *) nsp_type_gmaincontext);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_main_context_iteration(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int may_block, ret;
  if ( GetArgs(stack,rhs,opt,T,&may_block) == FAIL) return RET_BUG;
  ret =g_main_context_iteration(NSP_GBOXED_GET(self, GMainContext),may_block);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_main_context_pending(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_main_context_pending(NSP_GBOXED_GET(self, GMainContext));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_main_context_find_source_by_id(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int source_id;
  GSource *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&source_id) == FAIL) return RET_BUG;
  ret =g_main_context_find_source_by_id(NSP_GBOXED_GET(self, GMainContext),source_id);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_SOURCE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gsource))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_main_context_wakeup(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_main_context_wakeup(NSP_GBOXED_GET(self, GMainContext));
  return 0;
}

static int _wrap_g_main_context_acquire(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_main_context_acquire(NSP_GBOXED_GET(self, GMainContext));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_main_context_release(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_main_context_release(NSP_GBOXED_GET(self, GMainContext));
  return 0;
}

static int _wrap_g_main_context_is_owner(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_main_context_is_owner(NSP_GBOXED_GET(self, GMainContext));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_main_context_prepare(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int priority, ret;
  if ( GetArgs(stack,rhs,opt,T,&priority) == FAIL) return RET_BUG;
  ret =g_main_context_prepare(NSP_GBOXED_GET(self, GMainContext),&priority);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_main_context_dispatch(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_main_context_dispatch(NSP_GBOXED_GET(self, GMainContext));
  return 0;
}

static int _wrap_g_main_context_push_thread_default(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_main_context_push_thread_default(NSP_GBOXED_GET(self, GMainContext));
  return 0;
}

static int _wrap_g_main_context_pop_thread_default(NspGMainContext *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_main_context_pop_thread_default(NSP_GBOXED_GET(self, GMainContext));
  return 0;
}

static NspMethods gmaincontext_methods[] = {
  {"iteration",(nsp_method *) _wrap_g_main_context_iteration},
  {"pending",(nsp_method *) _wrap_g_main_context_pending},
  {"find_source_by_id",(nsp_method *) _wrap_g_main_context_find_source_by_id},
  {"wakeup",(nsp_method *) _wrap_g_main_context_wakeup},
  {"acquire",(nsp_method *) _wrap_g_main_context_acquire},
  {"release",(nsp_method *) _wrap_g_main_context_release},
  {"is_owner",(nsp_method *) _wrap_g_main_context_is_owner},
  {"prepare",(nsp_method *) _wrap_g_main_context_prepare},
  {"dispatch",(nsp_method *) _wrap_g_main_context_dispatch},
  {"push_thread_default",(nsp_method *) _wrap_g_main_context_push_thread_default},
  {"pop_thread_default",(nsp_method *) _wrap_g_main_context_pop_thread_default},
  { NULL, NULL}
};

static NspMethods *gmaincontext_get_methods(void) { return gmaincontext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmaincontext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMainLoop ----------- */


#define  NspGMainLoop_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmainloop.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMainLoop inherits from GBoxed 
 */

int nsp_type_gmainloop_id=0;
NspTypeGMainLoop *nsp_type_gmainloop=NULL;

/*
 * Type object for NspGMainLoop 
 * all the instance of NspTypeGMainLoop share the same id. 
 * nsp_type_gmainloop: is an instance of NspTypeGMainLoop 
 *    used for objects of NspGMainLoop type (i.e built with new_gmainloop) 
 * other instances are used for derived classes 
 */
NspTypeGMainLoop *new_type_gmainloop(type_mode mode)
{
  NspTypeGMainLoop *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmainloop != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmainloop;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmainloop_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmainloop_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmainloop;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmainloop */ 

  top->s_type =  (s_type_func *) nsp_gmainloop_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmainloop_type_short_string;
  /* top->create = (create_func*) int_gmainloop_create;*/

  /* specific methods for gmainloop */

  type->init = (init_func *) init_gmainloop;

  /* 
   * NspGMainLoop interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmainloop_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMainLoop called nsp_type_gmainloop
       */
      type->id =  nsp_type_gmainloop_id = nsp_new_type_id();
      nsp_type_gmainloop = type;
      if ( nsp_register_type(nsp_type_gmainloop) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmainloop, G_TYPE_MAIN_LOOP);
      return ( mode == T_BASE ) ? type : new_type_gmainloop(mode);
    }
  else 
    {
      type->id = nsp_type_gmainloop_id;
      return type;
    }
}

/*
 * initialize NspGMainLoop instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmainloop(NspGMainLoop *Obj,NspTypeGMainLoop *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMainLoop 
 */

NspGMainLoop *new_gmainloop() 
{
  NspGMainLoop *loc;
  /* type must exists */
  nsp_type_gmainloop = new_type_gmainloop(T_BASE);
  if ( (loc = malloc(sizeof(NspGMainLoop)))== NULLGMAINLOOP) return loc;
  /* initialize object */
  if ( init_gmainloop(loc,nsp_type_gmainloop) == FAIL) return NULLGMAINLOOP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMainLoop 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmainloop_type_name[]="GMainLoop";
static char gmainloop_short_type_name[]="GMainLoop";

static char *nsp_gmainloop_type_as_string(void)
{
  return(gmainloop_type_name);
}

static char *nsp_gmainloop_type_short_string(NspObject *v)
{
  return(gmainloop_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMainLoop objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMainLoop   *nsp_gmainloop_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmainloop_id)  == TRUE  ) return ((NspGMainLoop *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmainloop));
  return NULL;
}

int IsGMainLoopObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmainloop_id);
}

int IsGMainLoop(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmainloop_id);
}

NspGMainLoop  *GetGMainLoopCopy(Stack stack, int i)
{
  if (  GetGMainLoop(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMainLoop  *GetGMainLoop(Stack stack, int i)
{
  NspGMainLoop *M;
  if (( M = nsp_gmainloop_object(NthObj(i))) == NULLGMAINLOOP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGMainLoop *gmainloop_copy(NspGMainLoop *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gmainloop);
}

/*-------------------------------------------------------------------
 * wrappers for the GMainLoop
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_main_loop_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {new_opts, t_end};
  nsp_option opts[] = {
	{"context",obj,NULLOBJ,-1},
	{"is_running",s_bool,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  GMainContext *context = NULL;
  NspObject *nsp_context = NULL;
  int is_running = TRUE;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,opts, &nsp_context, &is_running) == FAIL) return RET_BUG;
  if ( nsp_context != NULL ) {
    if (nspg_boxed_check(nsp_context, G_TYPE_MAIN_CONTEXT))
      context = nspg_boxed_get(nsp_context, GMainContext);
    else if (! IsNone(nsp_context)) {
      Scierror("Error: context should be a GMainContext or None\n");
      return RET_BUG;
    }
  }
  if ((ret = (GObject *)g_main_loop_new(context,is_running))== NULL) return RET_BUG;

  nsp_type_gmainloop = new_type_gmainloop(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_MAIN_LOOP, ret,TRUE,TRUE,(NspTypeBase *) nsp_type_gmainloop);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_main_loop_run(NspGMainLoop *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_main_loop_run(NSP_GBOXED_GET(self, GMainLoop));
  return 0;
}

static int _wrap_g_main_loop_quit(NspGMainLoop *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_main_loop_quit(NSP_GBOXED_GET(self, GMainLoop));
  return 0;
}

static int _wrap_g_main_loop_is_running(NspGMainLoop *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_main_loop_is_running(NSP_GBOXED_GET(self, GMainLoop));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_main_loop_get_context(NspGMainLoop *self,Stack stack,int rhs,int opt,int lhs)
{
  GMainContext *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =g_main_loop_get_context(NSP_GBOXED_GET(self, GMainLoop));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_MAIN_CONTEXT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gmaincontext))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gmainloop_methods[] = {
  {"run",(nsp_method *) _wrap_g_main_loop_run},
  {"quit",(nsp_method *) _wrap_g_main_loop_quit},
  {"is_running",(nsp_method *) _wrap_g_main_loop_is_running},
  {"get_context",(nsp_method *) _wrap_g_main_loop_get_context},
  { NULL, NULL}
};

static NspMethods *gmainloop_get_methods(void) { return gmainloop_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmainloop_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSource ----------- */


#define  NspGSource_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsource.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSource inherits from GBoxed 
 */

int nsp_type_gsource_id=0;
NspTypeGSource *nsp_type_gsource=NULL;

/*
 * Type object for NspGSource 
 * all the instance of NspTypeGSource share the same id. 
 * nsp_type_gsource: is an instance of NspTypeGSource 
 *    used for objects of NspGSource type (i.e built with new_gsource) 
 * other instances are used for derived classes 
 */
NspTypeGSource *new_type_gsource(type_mode mode)
{
  NspTypeGSource *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsource != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsource;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsource_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsource_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsource;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsource */ 

  top->s_type =  (s_type_func *) nsp_gsource_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsource_type_short_string;
  /* top->create = (create_func*) int_gsource_create;*/

  /* specific methods for gsource */

  type->init = (init_func *) init_gsource;

  /* 
   * NspGSource interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsource_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSource called nsp_type_gsource
       */
      type->id =  nsp_type_gsource_id = nsp_new_type_id();
      nsp_type_gsource = type;
      if ( nsp_register_type(nsp_type_gsource) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsource, G_TYPE_SOURCE);
      return ( mode == T_BASE ) ? type : new_type_gsource(mode);
    }
  else 
    {
      type->id = nsp_type_gsource_id;
      return type;
    }
}

/*
 * initialize NspGSource instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsource(NspGSource *Obj,NspTypeGSource *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSource 
 */

NspGSource *new_gsource() 
{
  NspGSource *loc;
  /* type must exists */
  nsp_type_gsource = new_type_gsource(T_BASE);
  if ( (loc = malloc(sizeof(NspGSource)))== NULLGSOURCE) return loc;
  /* initialize object */
  if ( init_gsource(loc,nsp_type_gsource) == FAIL) return NULLGSOURCE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSource 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsource_type_name[]="GSource";
static char gsource_short_type_name[]="GSource";

static char *nsp_gsource_type_as_string(void)
{
  return(gsource_type_name);
}

static char *nsp_gsource_type_short_string(NspObject *v)
{
  return(gsource_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSource objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSource   *nsp_gsource_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsource_id)  == TRUE  ) return ((NspGSource *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsource));
  return NULL;
}

int IsGSourceObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsource_id);
}

int IsGSource(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsource_id);
}

NspGSource  *GetGSourceCopy(Stack stack, int i)
{
  if (  GetGSource(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSource  *GetGSource(Stack stack, int i)
{
  NspGSource *M;
  if (( M = nsp_gsource_object(NthObj(i))) == NULLGSOURCE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGSource *gsource_copy(NspGSource *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gsource);
}

/*-------------------------------------------------------------------
 * wrappers for the GSource
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_source_attach(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {new_opts, t_end};
  nsp_option opts[] = {
	{"context",obj,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  GMainContext *context = NULL;
  NspObject *nsp_context = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,opts, &nsp_context) == FAIL) return RET_BUG;
  if ( nsp_context != NULL ) {
    if (nspg_boxed_check(nsp_context, G_TYPE_MAIN_CONTEXT))
      context = nspg_boxed_get(nsp_context, GMainContext);
    else if (! IsNone(nsp_context)) {
      Scierror("Error: context should be a GMainContext or None\n");
      return RET_BUG;
    }
  }
  ret =g_source_attach(NSP_GBOXED_GET(self, GSource),context);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_source_destroy(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_source_destroy(NSP_GBOXED_GET(self, GSource));
  return 0;
}

static int _wrap_g_source_set_priority(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int priority;
  if ( GetArgs(stack,rhs,opt,T,&priority) == FAIL) return RET_BUG;
  g_source_set_priority(NSP_GBOXED_GET(self, GSource),priority);
  return 0;
}

static int _wrap_g_source_get_priority(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_source_get_priority(NSP_GBOXED_GET(self, GSource));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_source_set_can_recurse(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int can_recurse;
  if ( GetArgs(stack,rhs,opt,T,&can_recurse) == FAIL) return RET_BUG;
  g_source_set_can_recurse(NSP_GBOXED_GET(self, GSource),can_recurse);
  return 0;
}

static int _wrap_g_source_get_can_recurse(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_source_get_can_recurse(NSP_GBOXED_GET(self, GSource));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_source_get_id(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_source_get_id(NSP_GBOXED_GET(self, GSource));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_source_get_context(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  GMainContext *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =g_source_get_context(NSP_GBOXED_GET(self, GSource));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_MAIN_CONTEXT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gmaincontext))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 287 "codegen-3.0/glib.override"

/* g_source_set_callback defined in gobject.c 
 * 
 */
extern int _wrap_g_source_set_callback(NspGSource *self,Stack stack,int rhs,int opt,int lhs);

#line 939 "glib.c"


static int _wrap_g_source_is_destroyed(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_source_is_destroyed(NSP_GBOXED_GET(self, GSource));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_source_set_name(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
  g_source_set_name(NSP_GBOXED_GET(self, GSource),name);
  return 0;
}

static int _wrap_g_source_get_name(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
  ret =g_source_get_name(NSP_GBOXED_GET(self, GSource));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#if GLIB_CHECK_VERSION(2,36,0)
static int _wrap_g_source_set_ready_time(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  gint64 ready_time;
  if ( GetArgs(stack,rhs,opt,T,&ready_time) == FAIL) return RET_BUG;
  g_source_set_ready_time(NSP_GBOXED_GET(self, GSource),ready_time);
  return 0;
}

#else
int _wrap_g_source_set_ready_time(Stack stack, int rhs, int opt, int lhs) /* set_ready_time */
{
  Scierror("Error: function g_source_set_ready_time not available\n");
  return RET_BUG;
}
#endif
#if GLIB_CHECK_VERSION(2,36,0)
static int _wrap_g_source_get_ready_time(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  gint64 ret;
  CheckRhs(0,0);
  ret =g_source_get_ready_time(NSP_GBOXED_GET(self, GSource));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_source_get_ready_time(Stack stack, int rhs, int opt, int lhs) /* get_ready_time */
{
  Scierror("Error: function g_source_get_ready_time not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_source_add_child_source(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GSource *child_source = NULL;
  NspObject *nsp_child_source = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_child_source) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_child_source, G_TYPE_SOURCE))
      child_source = nspg_boxed_get(nsp_child_source, GSource);
  else {
      Scierror( "Error: child_source should be a GSource\n");
      return RET_BUG;
  }
  g_source_add_child_source(NSP_GBOXED_GET(self, GSource),child_source);
  return 0;
}

static int _wrap_g_source_remove_child_source(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GSource *child_source = NULL;
  NspObject *nsp_child_source = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_child_source) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_child_source, G_TYPE_SOURCE))
      child_source = nspg_boxed_get(nsp_child_source, GSource);
  else {
      Scierror( "Error: child_source should be a GSource\n");
      return RET_BUG;
  }
  g_source_remove_child_source(NSP_GBOXED_GET(self, GSource),child_source);
  return 0;
}

static int _wrap_g_source_get_time(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  gint64 ret;
  CheckRhs(0,0);
  ret =g_source_get_time(NSP_GBOXED_GET(self, GSource));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gsource_methods[] = {
  {"attach",(nsp_method *) _wrap_g_source_attach},
  {"destroy",(nsp_method *) _wrap_g_source_destroy},
  {"set_priority",(nsp_method *) _wrap_g_source_set_priority},
  {"get_priority",(nsp_method *) _wrap_g_source_get_priority},
  {"set_can_recurse",(nsp_method *) _wrap_g_source_set_can_recurse},
  {"get_can_recurse",(nsp_method *) _wrap_g_source_get_can_recurse},
  {"get_id",(nsp_method *) _wrap_g_source_get_id},
  {"get_context",(nsp_method *) _wrap_g_source_get_context},
  {"set_callback",(nsp_method *) _wrap_g_source_set_callback},
  {"is_destroyed",(nsp_method *) _wrap_g_source_is_destroyed},
  {"set_name",(nsp_method *) _wrap_g_source_set_name},
  {"get_name",(nsp_method *) _wrap_g_source_get_name},
  {"set_ready_time",(nsp_method *) _wrap_g_source_set_ready_time},
  {"get_ready_time",(nsp_method *) _wrap_g_source_get_ready_time},
  {"add_child_source",(nsp_method *) _wrap_g_source_add_child_source},
  {"remove_child_source",(nsp_method *) _wrap_g_source_remove_child_source},
  {"get_time",(nsp_method *) _wrap_g_source_get_time},
  { NULL, NULL}
};

static NspMethods *gsource_get_methods(void) { return gsource_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsource_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGError ----------- */


#define  NspGError_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gerror.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGError inherits from GBoxed 
 */

int nsp_type_gerror_id=0;
NspTypeGError *nsp_type_gerror=NULL;

/*
 * Type object for NspGError 
 * all the instance of NspTypeGError share the same id. 
 * nsp_type_gerror: is an instance of NspTypeGError 
 *    used for objects of NspGError type (i.e built with new_gerror) 
 * other instances are used for derived classes 
 */
NspTypeGError *new_type_gerror(type_mode mode)
{
  NspTypeGError *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gerror != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gerror;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gerror_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gerror_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gerror;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gerror */ 

  top->s_type =  (s_type_func *) nsp_gerror_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gerror_type_short_string;
  /* top->create = (create_func*) int_gerror_create;*/

  /* specific methods for gerror */

  type->init = (init_func *) init_gerror;

  /* 
   * NspGError interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gerror_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGError called nsp_type_gerror
       */
      type->id =  nsp_type_gerror_id = nsp_new_type_id();
      nsp_type_gerror = type;
      if ( nsp_register_type(nsp_type_gerror) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gerror, G_TYPE_ERROR);
      return ( mode == T_BASE ) ? type : new_type_gerror(mode);
    }
  else 
    {
      type->id = nsp_type_gerror_id;
      return type;
    }
}

/*
 * initialize NspGError instances 
 * locally and by calling initializer on parent class 
 */

static int init_gerror(NspGError *Obj,NspTypeGError *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGError 
 */

NspGError *new_gerror() 
{
  NspGError *loc;
  /* type must exists */
  nsp_type_gerror = new_type_gerror(T_BASE);
  if ( (loc = malloc(sizeof(NspGError)))== NULLGERROR) return loc;
  /* initialize object */
  if ( init_gerror(loc,nsp_type_gerror) == FAIL) return NULLGERROR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGError 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gerror_type_name[]="GError";
static char gerror_short_type_name[]="GError";

static char *nsp_gerror_type_as_string(void)
{
  return(gerror_type_name);
}

static char *nsp_gerror_type_short_string(NspObject *v)
{
  return(gerror_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGError objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGError   *nsp_gerror_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gerror_id)  == TRUE  ) return ((NspGError *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gerror));
  return NULL;
}

int IsGErrorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gerror_id);
}

int IsGError(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gerror_id);
}

NspGError  *GetGErrorCopy(Stack stack, int i)
{
  if (  GetGError(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGError  *GetGError(Stack stack, int i)
{
  NspGError *M;
  if (( M = nsp_gerror_object(NthObj(i))) == NULLGERROR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGError *gerror_copy(NspGError *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gerror);
}

/*-------------------------------------------------------------------
 * wrappers for the GError
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_error_free(NspGError *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_error_free(NSP_GBOXED_GET(self, GError));
  return 0;
}

static int _wrap_g_error_copy(NspGError *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =g_error_copy(NSP_GBOXED_GET(self, GError));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_ERROR, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gerror))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 273 "codegen-3.0/glib.override"

static int _wrap_g_error_matches(GError *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *quark_string;
  int code, ret;
  if ( GetArgs(stack,rhs,opt,T,&quark_string, &code) == FAIL) return RET_BUG;
  ret =g_error_matches(NSP_GBOXED_GET(self, GError),g_quark_from_static_string (quark_string),code);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#line 1297 "glib.c"


static NspMethods gerror_methods[] = {
  {"free",(nsp_method *) _wrap_g_error_free},
  {"copy",(nsp_method *) _wrap_g_error_copy},
  {"matches",(nsp_method *) _wrap_g_error_matches},
  { NULL, NULL}
};

static NspMethods *gerror_get_methods(void) { return gerror_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gerror_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGVariantType ----------- */


#define  NspGVariantType_Private 
#include <nsp/objects.h>
#include <nsp/gvarianttype.h>
#include <nsp/interf.h>

/* 
 * NspGVariantType inherits from Object 
 */

int nsp_type_gvarianttype_id=0;
NspTypeGVariantType *nsp_type_gvarianttype=NULL;

/*
 * Type object for NspGVariantType 
 * all the instance of NspTypeGVariantType share the same id. 
 * nsp_type_gvarianttype: is an instance of NspTypeGVariantType 
 *    used for objects of NspGVariantType type (i.e built with new_gvarianttype) 
 * other instances are used for derived classes 
 */
NspTypeGVariantType *new_type_gvarianttype(type_mode mode)
{
  NspTypeGVariantType *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gvarianttype != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gvarianttype;
    }
  if (( type =  malloc(sizeof(NspTypeGVariantType))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gvarianttype_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gvarianttype_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_gvarianttype;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gvarianttype */ 

  top->pr = (print_func *) nsp_gvarianttype_print;
  top->dealloc = (dealloc_func *) nsp_gvarianttype_destroy;
  top->copy  =  (copy_func *) nsp_gvarianttype_copy;
  top->size  = (size_func *) nsp_gvarianttype_size;
  top->s_type =  (s_type_func *) nsp_gvarianttype_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gvarianttype_type_short_string;
  top->info = (info_func *) nsp_gvarianttype_info;
  /* top->is_true = (is_true_func  *) nsp_gvarianttype_is_true; */
  /* top->loop =(loop_func *) nsp_gvarianttype_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gvarianttype_object;
  top->eq  = (eq_func *) nsp_gvarianttype_eq;
  top->neq  = (eq_func *) nsp_gvarianttype_neq;
  top->save  = (save_func *) nsp_gvarianttype_xdr_save;
  top->load  = (load_func *) nsp_gvarianttype_xdr_load;
  top->create = (create_func*) int_gvarianttype_create;
  top->latex = (print_func *) nsp_gvarianttype_latex;
  top->full_copy = (copy_func *) nsp_gvarianttype_full_copy;

  /* specific methods for gvarianttype */

  type->init = (init_func *) init_gvarianttype;

  /* 
   * NspGVariantType interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gvarianttype_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGVariantType called nsp_type_gvarianttype
       */
      type->id =  nsp_type_gvarianttype_id = nsp_new_type_id();
      nsp_type_gvarianttype = type;
      if ( nsp_register_type(nsp_type_gvarianttype) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gvarianttype(mode);
    }
  else 
    {
      type->id = nsp_type_gvarianttype_id;
      return type;
    }
}

/*
 * initialize NspGVariantType instances 
 * locally and by calling initializer on parent class 
 */

static int init_gvarianttype(NspGVariantType *Obj,NspTypeGVariantType *type)
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
 * new instance of NspGVariantType 
 */

NspGVariantType *new_gvarianttype() 
{
  NspGVariantType *loc;
  /* type must exists */
  nsp_type_gvarianttype = new_type_gvarianttype(T_BASE);
  if ( (loc = malloc(sizeof(NspGVariantType)))== NULLGVARIANTTYPE) return loc;
  /* initialize object */
  if ( init_gvarianttype(loc,nsp_type_gvarianttype) == FAIL) return NULLGVARIANTTYPE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGVariantType 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gvarianttype_size(NspGVariantType *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char gvarianttype_type_name[]="GVariantType";
static char gvarianttype_short_type_name[]="gvarianttype";

static char *nsp_gvarianttype_type_as_string(void)
{
  return(gvarianttype_type_name);
}

static char *nsp_gvarianttype_type_short_string(NspObject *v)
{
  return(gvarianttype_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gvarianttype_eq(NspGVariantType *A, NspObject *B)
{
  NspGVariantType *loc = (NspGVariantType *) B;
  if ( check_cast(B,nsp_type_gvarianttype_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->value != loc->obj->value) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_gvarianttype_neq(NspGVariantType *A, NspObject *B)
{
  return ( nsp_gvarianttype_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gvarianttype_xdr_save(XDR *xdrs, NspGVariantType *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gvarianttype)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGVariantType  *nsp_gvarianttype_xdr_load_partial(XDR *xdrs, NspGVariantType *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspGVariantType  *nsp_gvarianttype_xdr_load(XDR *xdrs)
{
  NspGVariantType *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGVARIANTTYPE;
  if ((H  = nsp_gvarianttype_create_void(name,(NspTypeBase *) nsp_type_gvarianttype))== NULLGVARIANTTYPE) return H;
  if ( nsp_gvarianttype_create_partial(H) == FAIL) return NULLGVARIANTTYPE;
  if ((H  = nsp_gvarianttype_xdr_load_partial(xdrs,H))== NULLGVARIANTTYPE) return H;
  if ( nsp_gvarianttype_check_values(H) == FAIL) return NULLGVARIANTTYPE;
  return H;
}

/*
 * delete 
 */

void nsp_gvarianttype_destroy_partial(NspGVariantType *H)
{
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  if (H->obj->value != NULL)
    { nsp_destroy_GVariantType(H->obj->value,H);}
    FREE(H->obj);
   }
}

void nsp_gvarianttype_destroy(NspGVariantType *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gvarianttype_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gvarianttype_info(NspGVariantType *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGVARIANTTYPE) 
    {
      Sciprintf("Null Pointer NspGVariantType \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gvarianttype_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gvarianttype_print(NspGVariantType *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGVARIANTTYPE) 
    {
      Sciprintf("Null Pointer NspGVariantType \n");
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
          nsp_gvarianttype_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_gvarianttype_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  nsp_print_GVariantType(indent+2,M->obj->value,M);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gvarianttype_latex(NspGVariantType *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gvarianttype_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  nsp_print_GVariantType(indent+2,M->obj->value,M);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+1,"\n");
  Sciprintf("\\end{array}\n");

  Sciprintf("\\right.\n");

  if ( use_math ) Sciprintf("\\end{equation*}\n");

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGVariantType objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGVariantType   *nsp_gvarianttype_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gvarianttype_id)  == TRUE  ) return ((NspGVariantType *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gvarianttype));
  return NULL;
}

int IsGVariantTypeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gvarianttype_id);
}

int IsGVariantType(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gvarianttype_id);
}

NspGVariantType  *GetGVariantTypeCopy(Stack stack, int i)
{
  if (  GetGVariantType(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGVariantType  *GetGVariantType(Stack stack, int i)
{
  NspGVariantType *M;
  if (( M = nsp_gvarianttype_object(NthObj(i))) == NULLGVARIANTTYPE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGVariantType instance 
 *-----------------------------------------------------*/

static NspGVariantType *nsp_gvarianttype_create_void(const char *name,NspTypeBase *type)
{
 NspGVariantType *H  = (type == NULL) ? new_gvarianttype() : type->new();
 if ( H ==  NULLGVARIANTTYPE)
  {
   Sciprintf("No more memory\n");
   return NULLGVARIANTTYPE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGVARIANTTYPE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gvarianttype_create_partial(NspGVariantType *H)
{
  if((H->obj = calloc(1,sizeof(nsp_gvarianttype)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->value = NULL;
  return OK;
}

int nsp_gvarianttype_check_values(NspGVariantType *H)
{
  if ( nsp_check_GVariantType(H->obj->value,H) == FAIL ) return FAIL;
  return OK;
}

NspGVariantType *nsp_gvarianttype_create(const char *name,GVariantType* value,NspTypeBase *type)
{
  NspGVariantType *H  = nsp_gvarianttype_create_void(name,type);
  if ( H ==  NULLGVARIANTTYPE) return NULLGVARIANTTYPE;
  if ( nsp_gvarianttype_create_partial(H) == FAIL) return NULLGVARIANTTYPE;
  H->obj->value = value;
  if ( nsp_gvarianttype_check_values(H) == FAIL) return NULLGVARIANTTYPE;
  return H;
}


NspGVariantType *nsp_gvarianttype_create_default(const char *name)
{
 NspGVariantType *H  = nsp_gvarianttype_create_void(name,NULL);
 if ( H ==  NULLGVARIANTTYPE) return NULLGVARIANTTYPE;
  if ( nsp_gvarianttype_create_partial(H) == FAIL) return NULLGVARIANTTYPE;
  if ( nsp_gvarianttype_check_values(H) == FAIL) return NULLGVARIANTTYPE;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGVariantType *nsp_gvarianttype_copy_partial(NspGVariantType *H,NspGVariantType *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGVariantType *nsp_gvarianttype_copy(NspGVariantType *self)
{
  NspGVariantType *H  =nsp_gvarianttype_create_void(NVOID,(NspTypeBase *) nsp_type_gvarianttype);
  if ( H ==  NULLGVARIANTTYPE) return NULLGVARIANTTYPE;
  if ( nsp_gvarianttype_copy_partial(H,self)== NULL) return NULLGVARIANTTYPE;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGVariantType *nsp_gvarianttype_full_copy_partial(NspGVariantType *H,NspGVariantType *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_gvarianttype))) == NULL) return NULLGVARIANTTYPE;
  H->obj->ref_count=1;
  if( nsp_GVariantType_full_copy(H,H->obj->value,self)== FAIL) return NULL;
  return H;
}

NspGVariantType *nsp_gvarianttype_full_copy(NspGVariantType *self)
{
  NspGVariantType *H  =nsp_gvarianttype_create_void(NVOID,(NspTypeBase *) nsp_type_gvarianttype);
  if ( H ==  NULLGVARIANTTYPE) return NULLGVARIANTTYPE;
  if ( nsp_gvarianttype_full_copy_partial(H,self)== NULL) return NULLGVARIANTTYPE;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGVariantType
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gvarianttype_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGVariantType *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gvarianttype is initialized */
  nsp_type_gvarianttype = new_type_gvarianttype(T_BASE);
  if(( H = nsp_gvarianttype_create_void(NVOID,(NspTypeBase *) nsp_type_gvarianttype)) == NULLGVARIANTTYPE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_gvarianttype_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gvarianttype_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_variant_type_new_dict_entry (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj,obj, t_end};
  const GVariantType *key = NULL, *value = NULL;
  NspObject *nsp_key = NULL, *nsp_value = NULL;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_key, &nsp_value) == FAIL) return RET_BUG;
  if ( IsGVariantType(nsp_key))
    { key = ((NspGVariantType *) nsp_key)->obj->value;
    }
  else
    {
      Scierror("Error: key should be of type GVariantType\n");
      return RET_BUG;
    }
  if ( IsGVariantType(nsp_value))
    { value = ((NspGVariantType *) nsp_value)->obj->value;
    }
  else
    {
      Scierror("Error: value should be of type GVariantType\n");
      return RET_BUG;
    }
  if ((ret = g_variant_type_new_dict_entry(key,value))== NULL) return RET_BUG;

  nsp_type_gvarianttype = new_type_gvarianttype(T_BASE);
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_type_new_maybe (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, t_end};
  const GVariantType *element = NULL;
  NspObject *nsp_element = NULL;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_element) == FAIL) return RET_BUG;
  if ( IsGVariantType(nsp_element))
    { element = ((NspGVariantType *) nsp_element)->obj->value;
    }
  else
    {
      Scierror("Error: element should be of type GVariantType\n");
      return RET_BUG;
    }
  if ((ret = g_variant_type_new_maybe(element))== NULL) return RET_BUG;

  nsp_type_gvarianttype = new_type_gvarianttype(T_BASE);
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_type_new_array (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, t_end};
  const GVariantType *element = NULL;
  NspObject *nsp_element = NULL;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_element) == FAIL) return RET_BUG;
  if ( IsGVariantType(nsp_element))
    { element = ((NspGVariantType *) nsp_element)->obj->value;
    }
  else
    {
      Scierror("Error: element should be of type GVariantType\n");
      return RET_BUG;
    }
  if ((ret = g_variant_type_new_array(element))== NULL) return RET_BUG;

  nsp_type_gvarianttype = new_type_gvarianttype(T_BASE);
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_type_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *type_string;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&type_string) == FAIL) return RET_BUG;
  if ((ret = g_variant_type_new(type_string))== NULL) return RET_BUG;

  nsp_type_gvarianttype = new_type_gvarianttype(T_BASE);
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_variant_type_get_string_length(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_get_string_length(self->obj->value);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_peek_string(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
  ret =g_variant_type_peek_string(self->obj->value);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_dup_string(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
  ret =g_variant_type_dup_string(self->obj->value);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_variant_type_is_definite(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_is_definite(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_is_container(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_is_container(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_is_basic(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_is_basic(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_is_maybe(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_is_maybe(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_is_array(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_is_array(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_is_tuple(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_is_tuple(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_is_dict_entry(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_is_dict_entry(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_is_variant(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_is_variant(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_is_subtype_of(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  const GVariantType *supertype = NULL;
  NspObject *nsp_supertype = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_supertype) == FAIL) return RET_BUG;
  if ( IsGVariantType(nsp_supertype))
    { supertype = ((NspGVariantType *) nsp_supertype)->obj->value;
    }
  else
    {
      Scierror("Error: supertype should be of type GVariantType\n");
      return RET_BUG;
    }
 ret =g_variant_type_is_subtype_of(self->obj->value,supertype);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_element(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  const GVariantType *ret;
  GVariantType *ret1;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret = g_variant_type_element(self->obj->value);
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret1 = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret1,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_variant_type_first(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  const GVariantType *ret;
  GVariantType *ret1;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =g_variant_type_first(self->obj->value);
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret1 = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret1,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_variant_type_next(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  const GVariantType *ret;
  GVariantType *ret1;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =g_variant_type_next(self->obj->value);
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret1 = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret1,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_variant_type_n_items(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_type_n_items(self->obj->value);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_type_key(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  const GVariantType *ret;
  GVariantType *ret1;
  NspObject *nsp_ret;
  CheckRhs(0,0);
ret = g_variant_type_key(self->obj->value);
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret1 = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret1,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_variant_type_value(NspGVariantType *self,Stack stack,int rhs,int opt,int lhs)
{
  const GVariantType *ret;
  GVariantType *ret1;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =g_variant_type_value(self->obj->value);
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret1 = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret1,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gvarianttype_methods[] = {
  {"get_string_length",(nsp_method *) _wrap_g_variant_type_get_string_length},
  {"peek_string",(nsp_method *) _wrap_g_variant_type_peek_string},
  {"dup_string",(nsp_method *) _wrap_g_variant_type_dup_string},
  {"is_definite",(nsp_method *) _wrap_g_variant_type_is_definite},
  {"is_container",(nsp_method *) _wrap_g_variant_type_is_container},
  {"is_basic",(nsp_method *) _wrap_g_variant_type_is_basic},
  {"is_maybe",(nsp_method *) _wrap_g_variant_type_is_maybe},
  {"is_array",(nsp_method *) _wrap_g_variant_type_is_array},
  {"is_tuple",(nsp_method *) _wrap_g_variant_type_is_tuple},
  {"is_dict_entry",(nsp_method *) _wrap_g_variant_type_is_dict_entry},
  {"is_variant",(nsp_method *) _wrap_g_variant_type_is_variant},
  {"is_subtype_of",(nsp_method *) _wrap_g_variant_type_is_subtype_of},
  {"element",(nsp_method *) _wrap_g_variant_type_element},
  {"first",(nsp_method *) _wrap_g_variant_type_first},
  {"next",(nsp_method *) _wrap_g_variant_type_next},
  {"n_items",(nsp_method *) _wrap_g_variant_type_n_items},
  {"key",(nsp_method *) _wrap_g_variant_type_key},
  {"value",(nsp_method *) _wrap_g_variant_type_value},
  { NULL, NULL}
};

static NspMethods *gvarianttype_get_methods(void) { return gvarianttype_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gvarianttype_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGVariant ----------- */


#define  NspGVariant_Private 
#include <nsp/objects.h>
#include <nsp/gvariant.h>
#include <nsp/interf.h>

/* 
 * NspGVariant inherits from Object 
 */

int nsp_type_gvariant_id=0;
NspTypeGVariant *nsp_type_gvariant=NULL;

/*
 * Type object for NspGVariant 
 * all the instance of NspTypeGVariant share the same id. 
 * nsp_type_gvariant: is an instance of NspTypeGVariant 
 *    used for objects of NspGVariant type (i.e built with new_gvariant) 
 * other instances are used for derived classes 
 */
NspTypeGVariant *new_type_gvariant(type_mode mode)
{
  NspTypeGVariant *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gvariant != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gvariant;
    }
  if (( type =  malloc(sizeof(NspTypeGVariant))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gvariant_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gvariant_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_gvariant;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gvariant */ 

  top->pr = (print_func *) nsp_gvariant_print;
  top->dealloc = (dealloc_func *) nsp_gvariant_destroy;
  top->copy  =  (copy_func *) nsp_gvariant_copy;
  top->size  = (size_func *) nsp_gvariant_size;
  top->s_type =  (s_type_func *) nsp_gvariant_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gvariant_type_short_string;
  top->info = (info_func *) nsp_gvariant_info;
  /* top->is_true = (is_true_func  *) nsp_gvariant_is_true; */
  /* top->loop =(loop_func *) nsp_gvariant_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gvariant_object;
  top->eq  = (eq_func *) nsp_gvariant_eq;
  top->neq  = (eq_func *) nsp_gvariant_neq;
  top->save  = (save_func *) nsp_gvariant_xdr_save;
  top->load  = (load_func *) nsp_gvariant_xdr_load;
  top->create = (create_func*) int_gvariant_create;
  top->latex = (print_func *) nsp_gvariant_latex;
  top->full_copy = (copy_func *) nsp_gvariant_full_copy;

  /* specific methods for gvariant */

  type->init = (init_func *) init_gvariant;

  /* 
   * NspGVariant interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gvariant_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGVariant called nsp_type_gvariant
       */
      type->id =  nsp_type_gvariant_id = nsp_new_type_id();
      nsp_type_gvariant = type;
      if ( nsp_register_type(nsp_type_gvariant) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gvariant(mode);
    }
  else 
    {
      type->id = nsp_type_gvariant_id;
      return type;
    }
}

/*
 * initialize NspGVariant instances 
 * locally and by calling initializer on parent class 
 */

static int init_gvariant(NspGVariant *Obj,NspTypeGVariant *type)
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
 * new instance of NspGVariant 
 */

NspGVariant *new_gvariant() 
{
  NspGVariant *loc;
  /* type must exists */
  nsp_type_gvariant = new_type_gvariant(T_BASE);
  if ( (loc = malloc(sizeof(NspGVariant)))== NULLGVARIANT) return loc;
  /* initialize object */
  if ( init_gvariant(loc,nsp_type_gvariant) == FAIL) return NULLGVARIANT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGVariant 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gvariant_size(NspGVariant *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char gvariant_type_name[]="GVariant";
static char gvariant_short_type_name[]="gvariant";

static char *nsp_gvariant_type_as_string(void)
{
  return(gvariant_type_name);
}

static char *nsp_gvariant_type_short_string(NspObject *v)
{
  return(gvariant_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gvariant_eq(NspGVariant *A, NspObject *B)
{
  NspGVariant *loc = (NspGVariant *) B;
  if ( check_cast(B,nsp_type_gvariant_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->value != loc->obj->value) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_gvariant_neq(NspGVariant *A, NspObject *B)
{
  return ( nsp_gvariant_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gvariant_xdr_save(XDR *xdrs, NspGVariant *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gvariant)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGVariant  *nsp_gvariant_xdr_load_partial(XDR *xdrs, NspGVariant *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspGVariant  *nsp_gvariant_xdr_load(XDR *xdrs)
{
  NspGVariant *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGVARIANT;
  if ((H  = nsp_gvariant_create_void(name,(NspTypeBase *) nsp_type_gvariant))== NULLGVARIANT) return H;
  if ( nsp_gvariant_create_partial(H) == FAIL) return NULLGVARIANT;
  if ((H  = nsp_gvariant_xdr_load_partial(xdrs,H))== NULLGVARIANT) return H;
  if ( nsp_gvariant_check_values(H) == FAIL) return NULLGVARIANT;
  return H;
}

/*
 * delete 
 */

void nsp_gvariant_destroy_partial(NspGVariant *H)
{
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  if (H->obj->value != NULL)
    { nsp_destroy_GVariant(H->obj->value,H);}
    FREE(H->obj);
   }
}

void nsp_gvariant_destroy(NspGVariant *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gvariant_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gvariant_info(NspGVariant *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGVARIANT) 
    {
      Sciprintf("Null Pointer NspGVariant \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gvariant_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gvariant_print(NspGVariant *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGVARIANT) 
    {
      Sciprintf("Null Pointer NspGVariant \n");
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
          nsp_gvariant_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_gvariant_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  nsp_print_GVariant(indent+2,M->obj->value,M);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gvariant_latex(NspGVariant *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gvariant_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  nsp_print_GVariant(indent+2,M->obj->value,M);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+1,"\n");
  Sciprintf("\\end{array}\n");

  Sciprintf("\\right.\n");

  if ( use_math ) Sciprintf("\\end{equation*}\n");

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGVariant objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGVariant   *nsp_gvariant_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gvariant_id)  == TRUE  ) return ((NspGVariant *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gvariant));
  return NULL;
}

int IsGVariantObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gvariant_id);
}

int IsGVariant(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gvariant_id);
}

NspGVariant  *GetGVariantCopy(Stack stack, int i)
{
  if (  GetGVariant(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGVariant  *GetGVariant(Stack stack, int i)
{
  NspGVariant *M;
  if (( M = nsp_gvariant_object(NthObj(i))) == NULLGVARIANT)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGVariant instance 
 *-----------------------------------------------------*/

static NspGVariant *nsp_gvariant_create_void(const char *name,NspTypeBase *type)
{
 NspGVariant *H  = (type == NULL) ? new_gvariant() : type->new();
 if ( H ==  NULLGVARIANT)
  {
   Sciprintf("No more memory\n");
   return NULLGVARIANT;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGVARIANT;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gvariant_create_partial(NspGVariant *H)
{
  if((H->obj = calloc(1,sizeof(nsp_gvariant)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->value = NULL;
  return OK;
}

int nsp_gvariant_check_values(NspGVariant *H)
{
  if ( nsp_check_GVariant(H->obj->value,H) == FAIL ) return FAIL;
  return OK;
}

NspGVariant *nsp_gvariant_create(const char *name,GVariant* value,NspTypeBase *type)
{
  NspGVariant *H  = nsp_gvariant_create_void(name,type);
  if ( H ==  NULLGVARIANT) return NULLGVARIANT;
  if ( nsp_gvariant_create_partial(H) == FAIL) return NULLGVARIANT;
  H->obj->value = value;
  if ( nsp_gvariant_check_values(H) == FAIL) return NULLGVARIANT;
  return H;
}


NspGVariant *nsp_gvariant_create_default(const char *name)
{
 NspGVariant *H  = nsp_gvariant_create_void(name,NULL);
 if ( H ==  NULLGVARIANT) return NULLGVARIANT;
  if ( nsp_gvariant_create_partial(H) == FAIL) return NULLGVARIANT;
  if ( nsp_gvariant_check_values(H) == FAIL) return NULLGVARIANT;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGVariant *nsp_gvariant_copy_partial(NspGVariant *H,NspGVariant *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGVariant *nsp_gvariant_copy(NspGVariant *self)
{
  NspGVariant *H  =nsp_gvariant_create_void(NVOID,(NspTypeBase *) nsp_type_gvariant);
  if ( H ==  NULLGVARIANT) return NULLGVARIANT;
  if ( nsp_gvariant_copy_partial(H,self)== NULL) return NULLGVARIANT;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGVariant *nsp_gvariant_full_copy_partial(NspGVariant *H,NspGVariant *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_gvariant))) == NULL) return NULLGVARIANT;
  H->obj->ref_count=1;
  if( nsp_GVariant_full_copy(H,H->obj->value,self)== FAIL) return NULL;
  return H;
}

NspGVariant *nsp_gvariant_full_copy(NspGVariant *self)
{
  NspGVariant *H  =nsp_gvariant_create_void(NVOID,(NspTypeBase *) nsp_type_gvariant);
  if ( H ==  NULLGVARIANT) return NULLGVARIANT;
  if ( nsp_gvariant_full_copy_partial(H,self)== NULL) return NULLGVARIANT;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGVariant
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gvariant_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGVariant *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gvariant is initialized */
  nsp_type_gvariant = new_type_gvariant(T_BASE);
  if(( H = nsp_gvariant_create_void(NVOID,(NspTypeBase *) nsp_type_gvariant)) == NULLGVARIANT) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_gvariant_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gvariant_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_variant_new_parsed (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *format;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&format) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_parsed(format))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 189 "codegen-3.0/glib.override"

static int
_wrap_g_variant_new (Stack stack, int rhs, int opt, int lhs)
{
  GVariant *gvariant=NULL;
  int_types T[] = {obj, t_end};
  NspObject *Obj,*nsp_ret;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&Obj) == FAIL) return RET_BUG;

#define NSP_ITYPE_SWITCH_UC(itype,X,arg)	\
  switch (itype ) {				\
  case nsp_gint: X(Gint,INT,int32,arg);		\
  case nsp_guint: X(Guint,UINT,uint32,arg);	\
  case nsp_gshort: X(Gshort,INT,int16,arg);	\
  case nsp_gushort: X(Gushort,UINT,uint16,arg);	\
  case nsp_glong : X(Glong,LONG,int32,arg );	\
  case nsp_gulong: X(Gulong,ULONG,uint32,arg);	\
  case nsp_gint8: X(Gint8,INT,byte,arg);	\
  case nsp_guint8: X(Guint8,UINT,byte,arg);	\
  case nsp_gint16: X(Gint16,INT,int16,arg);	\
  case nsp_guint16: X(Guint16,UINT,uint16,arg);	\
  case nsp_gint32: X(Gint32,INT,int32,arg);	\
  case nsp_guint32: X(Guint32,UINT,uint32,arg);	\
  case nsp_gint64 : X(Gint64,INT64,int64,arg );	\
  case nsp_guint64 : X(Guint64,UINT64,uint64,arg );}

  if (IsMat(Obj) && ((NspMatrix *) Obj)->mn == 1 )
    {
      gvariant = g_variant_new_double (((NspMatrix *) Obj)->R[0] );
    }
  else if ( IsBMat(Obj) && ((NspBMatrix *) Obj)->mn == 1)
    {
      gvariant =  g_variant_new_boolean (((NspBMatrix *) Obj)->B[0] );
    }
  else if ( IsSMat(Obj) && ((NspSMatrix *) Obj)->mn == 1 )
    {
      char *str = ((NspSMatrix *) Obj)->S[0];
      gvariant =  g_variant_new_string(str);
    }
  else if ( IsIMat(Obj) && ((NspIMatrix *) Obj)->mn == 1 )
    {
      NspIMatrix *A=(NspIMatrix *) Obj;
#define IMAT_SWITCH(name,type_uc,type_lc,arg)			\
      gvariant =  CNAME(g_variant_new_,type_lc)(A->name[0]);	\
      break;
      NSP_ITYPE_SWITCH_UC(A->itype,IMAT_SWITCH,"");
#undef IMAT_SWITCH
    }
  else
    {
      Scierror("Error: unrecognized type for g_variant_new\n");
      return RET_BUG;
    }
  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,gvariant,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}
#line 2667 "glib.c"


static int
_wrap_g_variant_new_bytestring (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *string;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_bytestring(string))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_signature (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *signature;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&signature) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_signature(signature))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_object_path (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *object_path;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&object_path) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_object_path(object_path))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_take_string (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *string;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_take_string(string))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_string (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *string;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_string(string))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_double (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_double, t_end};
  double value;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_double(value))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_handle (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, t_end};
  int value;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_handle(value))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_uint32 (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, t_end};
  gulong value;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_uint32(value))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_int32 (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, t_end};
  int value;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_int32(value))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_uint16 (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, t_end};
  int value;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_uint16(value))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_int16 (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, t_end};
  int value;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_int16(value))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_byte (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, t_end};
  int value;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_byte(value))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_variant_new_boolean (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_bool, t_end};
  int value;
  void *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL) return RET_BUG;
  if ((ret = g_variant_new_boolean(value))== NULL) return RET_BUG;

  nsp_type_gvariant = new_type_gvariant(T_BASE);
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_variant_is_floating(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_is_floating(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_type_string(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
  ret =g_variant_get_type_string(self->obj->value);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_is_of_type(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  const GVariantType *type = NULL;
  NspObject *nsp_type = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if ( IsGVariantType(nsp_type))
    { type = ((NspGVariantType *) nsp_type)->obj->value;
    }
  else
    {
      Scierror("Error: type should be of type GVariantType\n");
      return RET_BUG;
    }
  ret =g_variant_is_of_type(self->obj->value,type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_is_container(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_is_container(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_boolean(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_get_boolean(self->obj->value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_byte(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_get_byte(self->obj->value);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_int16(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_get_int16(self->obj->value);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_uint16(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_get_uint16(self->obj->value);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_int32(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_get_int32(self->obj->value);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_uint32(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  gulong ret;
  CheckRhs(0,0);
  ret =g_variant_get_uint32(self->obj->value);
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_double(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  double ret;
  CheckRhs(0,0);
  ret =g_variant_get_double(self->obj->value);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_string(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
  ret =g_variant_dup_string(self->obj->value,NULL);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_dup_string(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
  ret =g_variant_dup_string(self->obj->value,NULL);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_variant_get_bytestring(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
  ret =g_variant_get_bytestring(self->obj->value);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_variant_get_size(NspGVariant *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =g_variant_get_size(self->obj->value);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gvariant_methods[] = {
  {"is_floating",(nsp_method *) _wrap_g_variant_is_floating},
  {"get_type_string",(nsp_method *) _wrap_g_variant_get_type_string},
  {"is_of_type",(nsp_method *) _wrap_g_variant_is_of_type},
  {"is_container",(nsp_method *) _wrap_g_variant_is_container},
  {"get_boolean",(nsp_method *) _wrap_g_variant_get_boolean},
  {"get_byte",(nsp_method *) _wrap_g_variant_get_byte},
  {"get_int16",(nsp_method *) _wrap_g_variant_get_int16},
  {"get_uint16",(nsp_method *) _wrap_g_variant_get_uint16},
  {"get_int32",(nsp_method *) _wrap_g_variant_get_int32},
  {"get_uint32",(nsp_method *) _wrap_g_variant_get_uint32},
  {"get_double",(nsp_method *) _wrap_g_variant_get_double},
  {"get_string",(nsp_method *) _wrap_g_variant_get_string},
  {"dup_string",(nsp_method *) _wrap_g_variant_dup_string},
  {"get_bytestring",(nsp_method *) _wrap_g_variant_get_bytestring},
  {"get_size",(nsp_method *) _wrap_g_variant_get_size},
  { NULL, NULL}
};

static NspMethods *gvariant_get_methods(void) { return gvariant_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gvariant_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_g_on_error_query(Stack stack, int rhs, int opt, int lhs) /* g_on_error_query */
{
  int_types T[] = {string, t_end};
  char *prg_name;
  if ( GetArgs(stack,rhs,opt,T,&prg_name) == FAIL) return RET_BUG;
    g_on_error_query(prg_name);
  return 0;
}

int _wrap_g_on_error_stack_trace(Stack stack, int rhs, int opt, int lhs) /* g_on_error_stack_trace */
{
  int_types T[] = {string, t_end};
  char *prg_name;
  if ( GetArgs(stack,rhs,opt,T,&prg_name) == FAIL) return RET_BUG;
    g_on_error_stack_trace(prg_name);
  return 0;
}

int _wrap_g_base64_encode_step(Stack stack, int rhs, int opt, int lhs) /* g_base64_encode_step */
{
  int_types T[] = {string,s_int,s_bool,string,s_int,s_int, t_end};
  guchar *in;
  int len, break_lines, state, save, ret;
  char *out;
  if ( GetArgs(stack,rhs,opt,T,&in, &len, &break_lines, &out, &state, &save) == FAIL) return RET_BUG;
    ret =g_base64_encode_step(in,len,break_lines,out,&state,&save);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_base64_encode_close(Stack stack, int rhs, int opt, int lhs) /* g_base64_encode_close */
{
  int_types T[] = {s_bool,string,s_int,s_int, t_end};
  int break_lines, state, save, ret;
  char *out;
  if ( GetArgs(stack,rhs,opt,T,&break_lines, &out, &state, &save) == FAIL) return RET_BUG;
    ret =g_base64_encode_close(break_lines,out,&state,&save);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_base64_encode(Stack stack, int rhs, int opt, int lhs) /* g_base64_encode */
{
  int_types T[] = {string,s_int, t_end};
  guchar *data;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&data, &len) == FAIL) return RET_BUG;
    ret =g_base64_encode(data,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_get_codeset(Stack stack, int rhs, int opt, int lhs) /* g_get_codeset */
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_get_codeset();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_get_locale_variants(Stack stack, int rhs, int opt, int lhs) /* g_get_locale_variants */
{
  int_types T[] = {string, t_end};
  char *locale;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&locale) == FAIL) return RET_BUG;
    ret =g_get_locale_variants(locale);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 251 "codegen-3.0/glib.override"

int _wrap_g_filename_from_uri(Stack stack, int rhs, int opt, int lhs) /* g_filename_from_uri */
{
  int_types T[] = {string, t_end};
  char *uri;
  gchar *ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&uri) == FAIL) return RET_BUG;
  ret =g_filename_from_uri(uri,NULL,&error);
  if ( error != NULL ) 
    {
      Scierror("%s: gtk error, %s\n",NspFname(stack),error->message);
      g_error_free (error);
      return RET_BUG;
    }
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

#line 3154 "glib.c"


int _wrap_g_filename_to_uri(Stack stack, int rhs, int opt, int lhs) /* g_filename_to_uri */
{
  int_types T[] = {string,string, t_end};
  char *filename, *hostname;
  GError *error = NULL;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&filename, &hostname) == FAIL) return RET_BUG;
    ret =g_filename_to_uri(filename,hostname,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_filename_display_name(Stack stack, int rhs, int opt, int lhs) /* g_filename_display_name */
{
  int_types T[] = {string, t_end};
  char *filename;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
    ret =g_filename_display_name(filename);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_filename_display_basename(Stack stack, int rhs, int opt, int lhs) /* g_filename_display_basename */
{
  int_types T[] = {string, t_end};
  char *filename;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
    ret =g_filename_display_basename(filename);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_uri_list_extract_uris(Stack stack, int rhs, int opt, int lhs) /* g_uri_list_extract_uris */
{
  int_types T[] = {string, t_end};
  char *uri_list;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uri_list) == FAIL) return RET_BUG;
    ret =g_uri_list_extract_uris(uri_list);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_getenv(Stack stack, int rhs, int opt, int lhs) /* g_getenv */
{
  int_types T[] = {string, t_end};
  char *variable;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&variable) == FAIL) return RET_BUG;
    ret =g_getenv(variable);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_setenv(Stack stack, int rhs, int opt, int lhs) /* g_setenv */
{
  int_types T[] = {string,string,s_bool, t_end};
  char *variable, *value;
  int overwrite, ret;
  if ( GetArgs(stack,rhs,opt,T,&variable, &value, &overwrite) == FAIL) return RET_BUG;
    ret =g_setenv(variable,value,overwrite);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_unsetenv(Stack stack, int rhs, int opt, int lhs) /* g_unsetenv */
{
  int_types T[] = {string, t_end};
  char *variable;
  if ( GetArgs(stack,rhs,opt,T,&variable) == FAIL) return RET_BUG;
    g_unsetenv(variable);
  return 0;
}

int _wrap_g_listenv(Stack stack, int rhs, int opt, int lhs) /* g_listenv */
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_listenv();
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_get_environ(Stack stack, int rhs, int opt, int lhs) /* g_get_environ */
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_get_environ();
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_environ_getenv(Stack stack, int rhs, int opt, int lhs) /* g_environ_getenv */
{
  int_types T[] = {obj,string, t_end};
  gchar **envp = NULL;
  NspObject *nsp_envp = NULL;
  char *variable;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_envp, &variable) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_envp))
    { envp =  ((NspSMatrix *) nsp_envp)->S;}
  else
    {
      Scierror("Error: envp should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_environ_getenv(envp,variable);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_environ_setenv(Stack stack, int rhs, int opt, int lhs) /* g_environ_setenv */
{
  int_types T[] = {obj,string,string,s_bool, t_end};
  gchar **envp = NULL, **ret;
  NspObject *nsp_envp = NULL, *nsp_ret;
  char *variable, *value;
  int overwrite;
  if ( GetArgs(stack,rhs,opt,T,&nsp_envp, &variable, &value, &overwrite) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_envp))
    { envp =  ((NspSMatrix *) nsp_envp)->S;}
  else
    {
      Scierror("Error: envp should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_environ_setenv(envp,variable,value,overwrite);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_environ_unsetenv(Stack stack, int rhs, int opt, int lhs) /* g_environ_unsetenv */
{
  int_types T[] = {obj,string, t_end};
  gchar **envp = NULL, **ret;
  NspObject *nsp_envp = NULL, *nsp_ret;
  char *variable;
  if ( GetArgs(stack,rhs,opt,T,&nsp_envp, &variable) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_envp))
    { envp =  ((NspSMatrix *) nsp_envp)->S;}
  else
    {
      Scierror("Error: envp should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_environ_unsetenv(envp,variable);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_propagate_error(Stack stack, int rhs, int opt, int lhs) /* g_propagate_error */
{
  int_types T[] = {obj, t_end};
  GError *dest = NULL, *src = NULL;
  NspObject *nsp_src = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_src) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_src, G_TYPE_ERROR))
      src = nspg_boxed_get(nsp_src, GError);
  else {
      Scierror( "Error: src should be a GError\n");
      return RET_BUG;
  }
    g_propagate_error(&dest,src);
  if ( dest != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),dest->message);
    return RET_BUG;
  }
  return 0;
}

int _wrap_g_clear_error(Stack stack, int rhs, int opt, int lhs) /* g_clear_error */
{
  GError *err = NULL;
  CheckRhs(0,0);
    g_clear_error(&err);
  if ( err != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),err->message);
    return RET_BUG;
  }
  return 0;
}

int _wrap_g_file_set_contents(Stack stack, int rhs, int opt, int lhs) /* g_file_set_contents */
{
  int_types T[] = {string,string,s_int, t_end};
  char *filename, *contents;
  int length, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&filename, &contents, &length) == FAIL) return RET_BUG;
    ret =g_file_set_contents(filename,contents,length,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_file_read_link(Stack stack, int rhs, int opt, int lhs) /* g_file_read_link */
{
  int_types T[] = {string, t_end};
  char *filename;
  GError *error = NULL;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
    ret =g_file_read_link(filename,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_mkdtemp(Stack stack, int rhs, int opt, int lhs) /* g_mkdtemp */
{
  int_types T[] = {string, t_end};
  char *tmpl;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&tmpl) == FAIL) return RET_BUG;
    ret =g_mkdtemp(tmpl);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_mkdtemp_full(Stack stack, int rhs, int opt, int lhs) /* g_mkdtemp_full */
{
  int_types T[] = {string,s_int, t_end};
  char *tmpl;
  int mode;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&tmpl, &mode) == FAIL) return RET_BUG;
    ret =g_mkdtemp_full(tmpl,mode);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_mkstemp(Stack stack, int rhs, int opt, int lhs) /* g_mkstemp */
{
  int_types T[] = {string, t_end};
  char *tmpl;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&tmpl) == FAIL) return RET_BUG;
    ret =g_mkstemp(tmpl);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_mkstemp_full(Stack stack, int rhs, int opt, int lhs) /* g_mkstemp_full */
{
  int_types T[] = {string,s_int,s_int, t_end};
  char *tmpl;
  int flags, mode, ret;
  if ( GetArgs(stack,rhs,opt,T,&tmpl, &flags, &mode) == FAIL) return RET_BUG;
    ret =g_mkstemp_full(tmpl,flags,mode);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_file_open_tmp(Stack stack, int rhs, int opt, int lhs) /* g_file_open_tmp */
{
  int_types T[] = {string,obj, t_end};
  char *tmpl;
  gchar **name_used = NULL;
  NspObject *nsp_name_used = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&tmpl, &nsp_name_used) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_name_used))
    { name_used =  ((NspSMatrix *) nsp_name_used)->S;}
  else
    {
      Scierror("Error: name_used should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_file_open_tmp(tmpl,name_used,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dir_make_tmp(Stack stack, int rhs, int opt, int lhs) /* g_dir_make_tmp */
{
  int_types T[] = {string, t_end};
  char *tmpl;
  GError *error = NULL;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&tmpl) == FAIL) return RET_BUG;
    ret =g_dir_make_tmp(tmpl,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_build_pathv(Stack stack, int rhs, int opt, int lhs) /* g_build_pathv */
{
  int_types T[] = {string,obj, t_end};
  char *separator;
  gchar **args = NULL, *ret;
  NspObject *nsp_args = NULL;
  if ( GetArgs(stack,rhs,opt,T,&separator, &nsp_args) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_args))
    { args =  ((NspSMatrix *) nsp_args)->S;}
  else
    {
      Scierror("Error: args should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_build_pathv(separator,args);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_build_filenamev(Stack stack, int rhs, int opt, int lhs) /* g_build_filenamev */
{
  int_types T[] = {obj, t_end};
  gchar **args = NULL, *ret;
  NspObject *nsp_args = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_args) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_args))
    { args =  ((NspSMatrix *) nsp_args)->S;}
  else
    {
      Scierror("Error: args should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_build_filenamev(args);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_mkdir_with_parents(Stack stack, int rhs, int opt, int lhs) /* g_mkdir_with_parents */
{
  int_types T[] = {string,s_int, t_end};
  char *pathname;
  int mode, ret;
  if ( GetArgs(stack,rhs,opt,T,&pathname, &mode) == FAIL) return RET_BUG;
    ret =g_mkdir_with_parents(pathname,mode);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_path_is_absolute(Stack stack, int rhs, int opt, int lhs) /* g_path_is_absolute */
{
  int_types T[] = {string, t_end};
  char *file_name;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&file_name) == FAIL) return RET_BUG;
    ret =g_path_is_absolute(file_name);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_path_skip_root(Stack stack, int rhs, int opt, int lhs) /* g_path_skip_root */
{
  int_types T[] = {string, t_end};
  char *file_name;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&file_name) == FAIL) return RET_BUG;
    ret =g_path_skip_root(file_name);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_current_dir(Stack stack, int rhs, int opt, int lhs) /* g_get_current_dir */
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_get_current_dir();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_path_get_basename(Stack stack, int rhs, int opt, int lhs) /* g_path_get_basename */
{
  int_types T[] = {string, t_end};
  char *file_name;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&file_name) == FAIL) return RET_BUG;
    ret =g_path_get_basename(file_name);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_path_get_dirname(Stack stack, int rhs, int opt, int lhs) /* g_path_get_dirname */
{
  int_types T[] = {string, t_end};
  char *file_name;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&file_name) == FAIL) return RET_BUG;
    ret =g_path_get_dirname(file_name);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strip_context(Stack stack, int rhs, int opt, int lhs) /* g_strip_context */
{
  int_types T[] = {string,string, t_end};
  char *msgid, *msgval;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&msgid, &msgval) == FAIL) return RET_BUG;
    ret =g_strip_context(msgid,msgval);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dgettext(Stack stack, int rhs, int opt, int lhs) /* g_dgettext */
{
  int_types T[] = {string,string, t_end};
  char *domain, *msgid;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&domain, &msgid) == FAIL) return RET_BUG;
    ret =g_dgettext(domain,msgid);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dcgettext(Stack stack, int rhs, int opt, int lhs) /* g_dcgettext */
{
  int_types T[] = {string,string,s_int, t_end};
  char *domain, *msgid;
  int category;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&domain, &msgid, &category) == FAIL) return RET_BUG;
    ret =g_dcgettext(domain,msgid,category);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dngettext(Stack stack, int rhs, int opt, int lhs) /* g_dngettext */
{
  int_types T[] = {string,string,string,s_int, t_end};
  char *domain, *msgid, *msgid_plural;
  gulong n;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&domain, &msgid, &msgid_plural, &n) == FAIL) return RET_BUG;
    ret =g_dngettext(domain,msgid,msgid_plural,n);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dpgettext(Stack stack, int rhs, int opt, int lhs) /* g_dpgettext */
{
  int_types T[] = {string,string,s_int, t_end};
  char *domain, *msgctxtid;
  int msgidoffset;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&domain, &msgctxtid, &msgidoffset) == FAIL) return RET_BUG;
    ret =g_dpgettext(domain,msgctxtid,msgidoffset);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dpgettext2(Stack stack, int rhs, int opt, int lhs) /* g_dpgettext2 */
{
  int_types T[] = {string,string,string, t_end};
  char *domain, *context, *msgid;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&domain, &context, &msgid) == FAIL) return RET_BUG;
    ret =g_dpgettext2(domain,context,msgid);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_hostname_is_non_ascii(Stack stack, int rhs, int opt, int lhs) /* g_hostname_is_non_ascii */
{
  int_types T[] = {string, t_end};
  char *hostname;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&hostname) == FAIL) return RET_BUG;
    ret =g_hostname_is_non_ascii(hostname);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_hostname_is_ascii_encoded(Stack stack, int rhs, int opt, int lhs) /* g_hostname_is_ascii_encoded */
{
  int_types T[] = {string, t_end};
  char *hostname;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&hostname) == FAIL) return RET_BUG;
    ret =g_hostname_is_ascii_encoded(hostname);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_hostname_is_ip_address(Stack stack, int rhs, int opt, int lhs) /* g_hostname_is_ip_address */
{
  int_types T[] = {string, t_end};
  char *hostname;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&hostname) == FAIL) return RET_BUG;
    ret =g_hostname_is_ip_address(hostname);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_hostname_to_ascii(Stack stack, int rhs, int opt, int lhs) /* g_hostname_to_ascii */
{
  int_types T[] = {string, t_end};
  char *hostname;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&hostname) == FAIL) return RET_BUG;
    ret =g_hostname_to_ascii(hostname);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_hostname_to_unicode(Stack stack, int rhs, int opt, int lhs) /* g_hostname_to_unicode */
{
  int_types T[] = {string, t_end};
  char *hostname;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&hostname) == FAIL) return RET_BUG;
    ret =g_hostname_to_unicode(hostname);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_list_alloc(Stack stack, int rhs, int opt, int lhs) /* g_list_alloc */
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_list_alloc();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_g_main_context_default(Stack stack, int rhs, int opt, int lhs) /* g_main_context_default */
{
  GMainContext *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_main_context_default();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_MAIN_CONTEXT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gmaincontext))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_main_depth(Stack stack, int rhs, int opt, int lhs) /* g_main_depth */
{
  int ret;
  CheckRhs(0,0);
    ret =g_main_depth();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_main_current_source(Stack stack, int rhs, int opt, int lhs) /* g_main_current_source */
{
  GSource *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_main_current_source();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_SOURCE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gsource))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_main_context_get_thread_default(Stack stack, int rhs, int opt, int lhs) /* g_main_context_get_thread_default */
{
  GMainContext *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_main_context_get_thread_default();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_MAIN_CONTEXT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gmaincontext))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_main_context_ref_thread_default(Stack stack, int rhs, int opt, int lhs) /* g_main_context_ref_thread_default */
{
  GMainContext *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_main_context_ref_thread_default();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_MAIN_CONTEXT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gmaincontext))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_source_set_name_by_id(Stack stack, int rhs, int opt, int lhs) /* g_source_set_name_by_id */
{
  int_types T[] = {s_int,string, t_end};
  int tag;
  char *name;
  if ( GetArgs(stack,rhs,opt,T,&tag, &name) == FAIL) return RET_BUG;
    g_source_set_name_by_id(tag,name);
  return 0;
}

int _wrap_g_source_remove(Stack stack, int rhs, int opt, int lhs) /* g_source_remove */
{
  int_types T[] = {s_int, t_end};
  int tag, ret;
  if ( GetArgs(stack,rhs,opt,T,&tag) == FAIL) return RET_BUG;
    ret =g_source_remove(tag);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_markup_escape_text(Stack stack, int rhs, int opt, int lhs) /* g_markup_escape_text */
{
  int_types T[] = {string,s_int, t_end};
  char *text;
  int length;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&text, &length) == FAIL) return RET_BUG;
    ret =g_markup_escape_text(text,length);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_mem_is_system_malloc(Stack stack, int rhs, int opt, int lhs) /* g_mem_is_system_malloc */
{
  int ret;
  CheckRhs(0,0);
    ret =g_mem_is_system_malloc();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_mem_profile(Stack stack, int rhs, int opt, int lhs) /* g_mem_profile */
{
  CheckRhs(0,0);
    g_mem_profile();
  return 0;
}

int _wrap_g_log_remove_handler(Stack stack, int rhs, int opt, int lhs) /* g_log_remove_handler */
{
  int_types T[] = {string,s_int, t_end};
  char *log_domain;
  int handler_id;
  if ( GetArgs(stack,rhs,opt,T,&log_domain, &handler_id) == FAIL) return RET_BUG;
    g_log_remove_handler(log_domain,handler_id);
  return 0;
}

int _wrap_g_return_if_fail_warning(Stack stack, int rhs, int opt, int lhs) /* g_return_if_fail_warning */
{
  int_types T[] = {string,string,string, t_end};
  char *log_domain, *pretty_function, *expression;
  if ( GetArgs(stack,rhs,opt,T,&log_domain, &pretty_function, &expression) == FAIL) return RET_BUG;
    g_return_if_fail_warning(log_domain,pretty_function,expression);
  return 0;
}

int _wrap_g_warn_message(Stack stack, int rhs, int opt, int lhs) /* g_warn_message */
{
  int_types T[] = {string,string,s_int,string,string, t_end};
  char *domain, *file, *func, *warnexpr;
  int line;
  if ( GetArgs(stack,rhs,opt,T,&domain, &file, &line, &func, &warnexpr) == FAIL) return RET_BUG;
    g_warn_message(domain,file,line,func,warnexpr);
  return 0;
}

int _wrap_g_pattern_match_simple(Stack stack, int rhs, int opt, int lhs) /* g_pattern_match_simple */
{
  int_types T[] = {string,string, t_end};
  char *pattern, *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&pattern, &string) == FAIL) return RET_BUG;
    ret =g_pattern_match_simple(pattern,string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_spaced_primes_closest(Stack stack, int rhs, int opt, int lhs) /* g_spaced_primes_closest */
{
  int_types T[] = {s_int, t_end};
  int num, ret;
  if ( GetArgs(stack,rhs,opt,T,&num) == FAIL) return RET_BUG;
    ret =g_spaced_primes_closest(num);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_intern_string(Stack stack, int rhs, int opt, int lhs) /* g_intern_string */
{
  int_types T[] = {string, t_end};
  char *string;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_intern_string(string);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_intern_static_string(Stack stack, int rhs, int opt, int lhs) /* g_intern_static_string */
{
  int_types T[] = {string, t_end};
  char *string;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_intern_static_string(string);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_random_set_seed(Stack stack, int rhs, int opt, int lhs) /* g_random_set_seed */
{
  int_types T[] = {s_int, t_end};
  gulong seed;
  if ( GetArgs(stack,rhs,opt,T,&seed) == FAIL) return RET_BUG;
    g_random_set_seed(seed);
  return 0;
}

int _wrap_g_random_int(Stack stack, int rhs, int opt, int lhs) /* g_random_int */
{
  gulong ret;
  CheckRhs(0,0);
    ret =g_random_int();
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_random_int_range(Stack stack, int rhs, int opt, int lhs) /* g_random_int_range */
{
  int_types T[] = {s_int,s_int, t_end};
  int begin, end, ret;
  if ( GetArgs(stack,rhs,opt,T,&begin, &end) == FAIL) return RET_BUG;
    ret =g_random_int_range(begin,end);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_random_double(Stack stack, int rhs, int opt, int lhs) /* g_random_double */
{
  double ret;
  CheckRhs(0,0);
    ret =g_random_double();
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_random_double_range(Stack stack, int rhs, int opt, int lhs) /* g_random_double_range */
{
  int_types T[] = {s_double,s_double, t_end};
  double begin, end, ret;
  if ( GetArgs(stack,rhs,opt,T,&begin, &end) == FAIL) return RET_BUG;
    ret =g_random_double_range(begin,end);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_regex_escape_string(Stack stack, int rhs, int opt, int lhs) /* g_regex_escape_string */
{
  int_types T[] = {string,s_int, t_end};
  char *string;
  int length;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &length) == FAIL) return RET_BUG;
    ret =g_regex_escape_string(string,length);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_regex_escape_nul(Stack stack, int rhs, int opt, int lhs) /* g_regex_escape_nul */
{
  int_types T[] = {string,s_int, t_end};
  char *string;
  int length;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &length) == FAIL) return RET_BUG;
    ret =g_regex_escape_nul(string,length);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_shell_quote(Stack stack, int rhs, int opt, int lhs) /* g_shell_quote */
{
  int_types T[] = {string, t_end};
  char *unquoted_string;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&unquoted_string) == FAIL) return RET_BUG;
    ret =g_shell_quote(unquoted_string);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_shell_unquote(Stack stack, int rhs, int opt, int lhs) /* g_shell_unquote */
{
  int_types T[] = {string, t_end};
  char *quoted_string;
  GError *error = NULL;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&quoted_string) == FAIL) return RET_BUG;
    ret =g_shell_unquote(quoted_string,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_slist_alloc(Stack stack, int rhs, int opt, int lhs) /* g_slist_alloc */
{
  GSList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_slist_alloc();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

}

int _wrap_g_spawn_command_line_async(Stack stack, int rhs, int opt, int lhs) /* g_spawn_command_line_async */
{
  int_types T[] = {string, t_end};
  char *command_line;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&command_line) == FAIL) return RET_BUG;
    ret =g_spawn_command_line_async(command_line,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_spawn_check_exit_status(Stack stack, int rhs, int opt, int lhs) /* g_spawn_check_exit_status */
{
  int_types T[] = {s_int, t_end};
  int exit_status, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&exit_status) == FAIL) return RET_BUG;
    ret =g_spawn_check_exit_status(exit_status,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_ascii_tolower(Stack stack, int rhs, int opt, int lhs) /* g_ascii_tolower */
{
  int_types T[] = {s_int, t_end};
  int c, ret;
  if ( GetArgs(stack,rhs,opt,T,&c) == FAIL) return RET_BUG;
    ret =g_ascii_tolower(c);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_ascii_toupper(Stack stack, int rhs, int opt, int lhs) /* g_ascii_toupper */
{
  int_types T[] = {s_int, t_end};
  int c, ret;
  if ( GetArgs(stack,rhs,opt,T,&c) == FAIL) return RET_BUG;
    ret =g_ascii_toupper(c);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_ascii_digit_value(Stack stack, int rhs, int opt, int lhs) /* g_ascii_digit_value */
{
  int_types T[] = {s_int, t_end};
  int c, ret;
  if ( GetArgs(stack,rhs,opt,T,&c) == FAIL) return RET_BUG;
    ret =g_ascii_digit_value(c);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_ascii_xdigit_value(Stack stack, int rhs, int opt, int lhs) /* g_ascii_xdigit_value */
{
  int_types T[] = {s_int, t_end};
  int c, ret;
  if ( GetArgs(stack,rhs,opt,T,&c) == FAIL) return RET_BUG;
    ret =g_ascii_xdigit_value(c);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_strdelimit(Stack stack, int rhs, int opt, int lhs) /* g_strdelimit */
{
  int_types T[] = {string,string,s_int, t_end};
  char *string, *delimiters;
  int new_delimiter;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &delimiters, &new_delimiter) == FAIL) return RET_BUG;
    ret =g_strdelimit(string,delimiters,new_delimiter);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strcanon(Stack stack, int rhs, int opt, int lhs) /* g_strcanon */
{
  int_types T[] = {string,string,s_int, t_end};
  char *string, *valid_chars;
  int substitutor;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &valid_chars, &substitutor) == FAIL) return RET_BUG;
    ret =g_strcanon(string,valid_chars,substitutor);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strerror(Stack stack, int rhs, int opt, int lhs) /* g_strerror */
{
  int_types T[] = {s_int, t_end};
  int errnum;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&errnum) == FAIL) return RET_BUG;
    ret =g_strerror(errnum);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_strsignal(Stack stack, int rhs, int opt, int lhs) /* g_strsignal */
{
  int_types T[] = {s_int, t_end};
  int signum;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&signum) == FAIL) return RET_BUG;
    ret =g_strsignal(signum);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_strreverse(Stack stack, int rhs, int opt, int lhs) /* g_strreverse */
{
  int_types T[] = {string, t_end};
  char *string;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_strreverse(string);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strlcpy(Stack stack, int rhs, int opt, int lhs) /* g_strlcpy */
{
  int_types T[] = {string,string,s_int, t_end};
  char *dest, *src;
  int dest_size, ret;
  if ( GetArgs(stack,rhs,opt,T,&dest, &src, &dest_size) == FAIL) return RET_BUG;
    ret =g_strlcpy(dest,src,dest_size);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_strlcat(Stack stack, int rhs, int opt, int lhs) /* g_strlcat */
{
  int_types T[] = {string,string,s_int, t_end};
  char *dest, *src;
  int dest_size, ret;
  if ( GetArgs(stack,rhs,opt,T,&dest, &src, &dest_size) == FAIL) return RET_BUG;
    ret =g_strlcat(dest,src,dest_size);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_strstr_len(Stack stack, int rhs, int opt, int lhs) /* g_strstr_len */
{
  int_types T[] = {string,s_int,string, t_end};
  char *haystack, *needle;
  int haystack_len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&haystack, &haystack_len, &needle) == FAIL) return RET_BUG;
    ret =g_strstr_len(haystack,haystack_len,needle);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strrstr(Stack stack, int rhs, int opt, int lhs) /* g_strrstr */
{
  int_types T[] = {string,string, t_end};
  char *haystack, *needle;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&haystack, &needle) == FAIL) return RET_BUG;
    ret =g_strrstr(haystack,needle);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strrstr_len(Stack stack, int rhs, int opt, int lhs) /* g_strrstr_len */
{
  int_types T[] = {string,s_int,string, t_end};
  char *haystack, *needle;
  int haystack_len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&haystack, &haystack_len, &needle) == FAIL) return RET_BUG;
    ret =g_strrstr_len(haystack,haystack_len,needle);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_str_has_suffix(Stack stack, int rhs, int opt, int lhs) /* g_str_has_suffix */
{
  int_types T[] = {string,string, t_end};
  char *str, *suffix;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &suffix) == FAIL) return RET_BUG;
    ret =g_str_has_suffix(str,suffix);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_str_has_prefix(Stack stack, int rhs, int opt, int lhs) /* g_str_has_prefix */
{
  int_types T[] = {string,string, t_end};
  char *str, *prefix;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &prefix) == FAIL) return RET_BUG;
    ret =g_str_has_prefix(str,prefix);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_strtod(Stack stack, int rhs, int opt, int lhs) /* g_strtod */
{
  int_types T[] = {string,obj, t_end};
  char *nptr;
  gchar **endptr = NULL;
  NspObject *nsp_endptr = NULL;
  double ret;
  if ( GetArgs(stack,rhs,opt,T,&nptr, &nsp_endptr) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_endptr))
    { endptr =  ((NspSMatrix *) nsp_endptr)->S;}
  else
    {
      Scierror("Error: endptr should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_strtod(nptr,endptr);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_ascii_strtod(Stack stack, int rhs, int opt, int lhs) /* g_ascii_strtod */
{
  int_types T[] = {string,obj, t_end};
  char *nptr;
  gchar **endptr = NULL;
  NspObject *nsp_endptr = NULL;
  double ret;
  if ( GetArgs(stack,rhs,opt,T,&nptr, &nsp_endptr) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_endptr))
    { endptr =  ((NspSMatrix *) nsp_endptr)->S;}
  else
    {
      Scierror("Error: endptr should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_ascii_strtod(nptr,endptr);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_ascii_dtostr(Stack stack, int rhs, int opt, int lhs) /* g_ascii_dtostr */
{
  int_types T[] = {string,s_int,s_double, t_end};
  char *buffer;
  int buf_len;
  double d;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&buffer, &buf_len, &d) == FAIL) return RET_BUG;
    ret =g_ascii_dtostr(buffer,buf_len,d);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_ascii_formatd(Stack stack, int rhs, int opt, int lhs) /* g_ascii_formatd */
{
  int_types T[] = {string,s_int,string,s_double, t_end};
  char *buffer, *format;
  int buf_len;
  double d;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&buffer, &buf_len, &format, &d) == FAIL) return RET_BUG;
    ret =g_ascii_formatd(buffer,buf_len,format,d);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strchug(Stack stack, int rhs, int opt, int lhs) /* g_strchug */
{
  int_types T[] = {string, t_end};
  char *string;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_strchug(string);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strchomp(Stack stack, int rhs, int opt, int lhs) /* g_strchomp */
{
  int_types T[] = {string, t_end};
  char *string;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_strchomp(string);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_ascii_strcasecmp(Stack stack, int rhs, int opt, int lhs) /* g_ascii_strcasecmp */
{
  int_types T[] = {string,string, t_end};
  char *s1, *s2;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&s1, &s2) == FAIL) return RET_BUG;
    ret =g_ascii_strcasecmp(s1,s2);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_ascii_strncasecmp(Stack stack, int rhs, int opt, int lhs) /* g_ascii_strncasecmp */
{
  int_types T[] = {string,string,s_int, t_end};
  char *s1, *s2;
  int n, ret;
  if ( GetArgs(stack,rhs,opt,T,&s1, &s2, &n) == FAIL) return RET_BUG;
    ret =g_ascii_strncasecmp(s1,s2,n);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_ascii_strdown(Stack stack, int rhs, int opt, int lhs) /* g_ascii_strdown */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &len) == FAIL) return RET_BUG;
    ret =g_ascii_strdown(str,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_ascii_strup(Stack stack, int rhs, int opt, int lhs) /* g_ascii_strup */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &len) == FAIL) return RET_BUG;
    ret =g_ascii_strup(str,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_str_is_ascii(Stack stack, int rhs, int opt, int lhs) /* g_str_is_ascii */
{
  int_types T[] = {string, t_end};
  char *str;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&str) == FAIL) return RET_BUG;
    ret =g_str_is_ascii(str);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_strdup(Stack stack, int rhs, int opt, int lhs) /* g_strdup */
{
  int_types T[] = {string, t_end};
  char *str;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str) == FAIL) return RET_BUG;
    ret =g_strdup(str);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strndup(Stack stack, int rhs, int opt, int lhs) /* g_strndup */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int n;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &n) == FAIL) return RET_BUG;
    ret =g_strndup(str,n);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strnfill(Stack stack, int rhs, int opt, int lhs) /* g_strnfill */
{
  int_types T[] = {s_int,s_int, t_end};
  int length, fill_char;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&length, &fill_char) == FAIL) return RET_BUG;
    ret =g_strnfill(length,fill_char);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strcompress(Stack stack, int rhs, int opt, int lhs) /* g_strcompress */
{
  int_types T[] = {string, t_end};
  char *source;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&source) == FAIL) return RET_BUG;
    ret =g_strcompress(source);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strescape(Stack stack, int rhs, int opt, int lhs) /* g_strescape */
{
  int_types T[] = {string,string, t_end};
  char *source, *exceptions;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&source, &exceptions) == FAIL) return RET_BUG;
    ret =g_strescape(source,exceptions);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strsplit(Stack stack, int rhs, int opt, int lhs) /* g_strsplit */
{
  int_types T[] = {string,string,s_int, t_end};
  char *string, *delimiter;
  int max_tokens;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &delimiter, &max_tokens) == FAIL) return RET_BUG;
    ret =g_strsplit(string,delimiter,max_tokens);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_strsplit_set(Stack stack, int rhs, int opt, int lhs) /* g_strsplit_set */
{
  int_types T[] = {string,string,s_int, t_end};
  char *string, *delimiters;
  int max_tokens;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &delimiters, &max_tokens) == FAIL) return RET_BUG;
    ret =g_strsplit_set(string,delimiters,max_tokens);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_strjoinv(Stack stack, int rhs, int opt, int lhs) /* g_strjoinv */
{
  int_types T[] = {string,obj, t_end};
  char *separator;
  gchar **str_array = NULL, *ret;
  NspObject *nsp_str_array = NULL;
  if ( GetArgs(stack,rhs,opt,T,&separator, &nsp_str_array) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_str_array))
    { str_array =  ((NspSMatrix *) nsp_str_array)->S;}
  else
    {
      Scierror("Error: str_array should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_strjoinv(separator,str_array);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_strfreev(Stack stack, int rhs, int opt, int lhs) /* g_strfreev */
{
  int_types T[] = {obj, t_end};
  gchar **str_array = NULL;
  NspObject *nsp_str_array = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_str_array) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_str_array))
    { str_array =  ((NspSMatrix *) nsp_str_array)->S;}
  else
    {
      Scierror("Error: str_array should be of type SMat\n");
      return RET_BUG;
    }
    g_strfreev(str_array);
  return 0;
}

int _wrap_g_strdupv(Stack stack, int rhs, int opt, int lhs) /* g_strdupv */
{
  int_types T[] = {obj, t_end};
  gchar **str_array = NULL, **ret;
  NspObject *nsp_str_array = NULL, *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_str_array) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_str_array))
    { str_array =  ((NspSMatrix *) nsp_str_array)->S;}
  else
    {
      Scierror("Error: str_array should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_strdupv(str_array);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_strv_length(Stack stack, int rhs, int opt, int lhs) /* g_strv_length */
{
  int_types T[] = {obj, t_end};
  gchar **str_array = NULL;
  NspObject *nsp_str_array = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_str_array) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_str_array))
    { str_array =  ((NspSMatrix *) nsp_str_array)->S;}
  else
    {
      Scierror("Error: str_array should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_strv_length(str_array);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_stpcpy(Stack stack, int rhs, int opt, int lhs) /* g_stpcpy */
{
  int_types T[] = {string,string, t_end};
  char *dest, *src;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&dest, &src) == FAIL) return RET_BUG;
    ret =g_stpcpy(dest,src);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_str_to_ascii(Stack stack, int rhs, int opt, int lhs) /* g_str_to_ascii */
{
  int_types T[] = {string,string, t_end};
  char *str, *from_locale;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &from_locale) == FAIL) return RET_BUG;
    ret =g_str_to_ascii(str,from_locale);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_str_match_string(Stack stack, int rhs, int opt, int lhs) /* g_str_match_string */
{
  int_types T[] = {string,string,s_bool, t_end};
  char *search_term, *potential_hit;
  int accept_alternates, ret;
  if ( GetArgs(stack,rhs,opt,T,&search_term, &potential_hit, &accept_alternates) == FAIL) return RET_BUG;
    ret =g_str_match_string(search_term,potential_hit,accept_alternates);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_strcmp0(Stack stack, int rhs, int opt, int lhs) /* g_strcmp0 */
{
  int_types T[] = {string,string, t_end};
  char *str1, *str2;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&str1, &str2) == FAIL) return RET_BUG;
    ret =g_strcmp0(str1,str2);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_subprocess(Stack stack, int rhs, int opt, int lhs) /* g_test_subprocess */
{
  int ret;
  CheckRhs(0,0);
    ret =g_test_subprocess();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_run(Stack stack, int rhs, int opt, int lhs) /* g_test_run */
{
  int ret;
  CheckRhs(0,0);
    ret =g_test_run();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_fail(Stack stack, int rhs, int opt, int lhs) /* g_test_fail */
{
  CheckRhs(0,0);
    g_test_fail();
  return 0;
}

int _wrap_g_test_incomplete(Stack stack, int rhs, int opt, int lhs) /* g_test_incomplete */
{
  int_types T[] = {string, t_end};
  char *msg;
  if ( GetArgs(stack,rhs,opt,T,&msg) == FAIL) return RET_BUG;
    g_test_incomplete(msg);
  return 0;
}

int _wrap_g_test_skip(Stack stack, int rhs, int opt, int lhs) /* g_test_skip */
{
  int_types T[] = {string, t_end};
  char *msg;
  if ( GetArgs(stack,rhs,opt,T,&msg) == FAIL) return RET_BUG;
    g_test_skip(msg);
  return 0;
}

int _wrap_g_test_failed(Stack stack, int rhs, int opt, int lhs) /* g_test_failed */
{
  int ret;
  CheckRhs(0,0);
    ret =g_test_failed();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_set_nonfatal_assertions(Stack stack, int rhs, int opt, int lhs) /* g_test_set_nonfatal_assertions */
{
  CheckRhs(0,0);
    g_test_set_nonfatal_assertions();
  return 0;
}

int _wrap_g_test_bug_base(Stack stack, int rhs, int opt, int lhs) /* g_test_bug_base */
{
  int_types T[] = {string, t_end};
  char *uri_pattern;
  if ( GetArgs(stack,rhs,opt,T,&uri_pattern) == FAIL) return RET_BUG;
    g_test_bug_base(uri_pattern);
  return 0;
}

int _wrap_g_test_bug(Stack stack, int rhs, int opt, int lhs) /* g_test_bug */
{
  int_types T[] = {string, t_end};
  char *bug_uri_snippet;
  if ( GetArgs(stack,rhs,opt,T,&bug_uri_snippet) == FAIL) return RET_BUG;
    g_test_bug(bug_uri_snippet);
  return 0;
}

int _wrap_g_test_timer_start(Stack stack, int rhs, int opt, int lhs) /* g_test_timer_start */
{
  CheckRhs(0,0);
    g_test_timer_start();
  return 0;
}

int _wrap_g_test_timer_elapsed(Stack stack, int rhs, int opt, int lhs) /* g_test_timer_elapsed */
{
  double ret;
  CheckRhs(0,0);
    ret =g_test_timer_elapsed();
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_timer_last(Stack stack, int rhs, int opt, int lhs) /* g_test_timer_last */
{
  double ret;
  CheckRhs(0,0);
    ret =g_test_timer_last();
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_trap_has_passed(Stack stack, int rhs, int opt, int lhs) /* g_test_trap_has_passed */
{
  int ret;
  CheckRhs(0,0);
    ret =g_test_trap_has_passed();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_trap_reached_timeout(Stack stack, int rhs, int opt, int lhs) /* g_test_trap_reached_timeout */
{
  int ret;
  CheckRhs(0,0);
    ret =g_test_trap_reached_timeout();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_rand_int(Stack stack, int rhs, int opt, int lhs) /* g_test_rand_int */
{
  int ret;
  CheckRhs(0,0);
    ret =g_test_rand_int();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_rand_int_range(Stack stack, int rhs, int opt, int lhs) /* g_test_rand_int_range */
{
  int_types T[] = {s_int,s_int, t_end};
  int begin, end, ret;
  if ( GetArgs(stack,rhs,opt,T,&begin, &end) == FAIL) return RET_BUG;
    ret =g_test_rand_int_range(begin,end);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_rand_double(Stack stack, int rhs, int opt, int lhs) /* g_test_rand_double */
{
  double ret;
  CheckRhs(0,0);
    ret =g_test_rand_double();
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_test_rand_double_range(Stack stack, int rhs, int opt, int lhs) /* g_test_rand_double_range */
{
  int_types T[] = {s_double,s_double, t_end};
  double range_start, range_end, ret;
  if ( GetArgs(stack,rhs,opt,T,&range_start, &range_end) == FAIL) return RET_BUG;
    ret =g_test_rand_double_range(range_start,range_end);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_assertion_message(Stack stack, int rhs, int opt, int lhs) /* g_assertion_message */
{
  int_types T[] = {string,string,s_int,string,string, t_end};
  char *domain, *file, *func, *message;
  int line;
  if ( GetArgs(stack,rhs,opt,T,&domain, &file, &line, &func, &message) == FAIL) return RET_BUG;
    g_assertion_message(domain,file,line,func,message);
  return 0;
}

int _wrap_g_assertion_message_expr(Stack stack, int rhs, int opt, int lhs) /* g_assertion_message_expr */
{
  int_types T[] = {string,string,s_int,string,string, t_end};
  char *domain, *file, *func, *expr;
  int line;
  if ( GetArgs(stack,rhs,opt,T,&domain, &file, &line, &func, &expr) == FAIL) return RET_BUG;
    g_assertion_message_expr(domain,file,line,func,expr);
  return 0;
}

int _wrap_g_assertion_message_cmpstr(Stack stack, int rhs, int opt, int lhs) /* g_assertion_message_cmpstr */
{
  int_types T[] = {string,string,s_int,string,string,string,string,string, t_end};
  char *domain, *file, *func, *expr, *arg1, *cmp, *arg2;
  int line;
  if ( GetArgs(stack,rhs,opt,T,&domain, &file, &line, &func, &expr, &arg1, &cmp, &arg2) == FAIL) return RET_BUG;
    g_assertion_message_cmpstr(domain,file,line,func,expr,arg1,cmp,arg2);
  return 0;
}

int _wrap_g_test_assert_expected_messages_internal(Stack stack, int rhs, int opt, int lhs) /* g_test_assert_expected_messages_internal */
{
  int_types T[] = {string,string,s_int,string, t_end};
  char *domain, *file, *func;
  int line;
  if ( GetArgs(stack,rhs,opt,T,&domain, &file, &line, &func) == FAIL) return RET_BUG;
    g_test_assert_expected_messages_internal(domain,file,line,func);
  return 0;
}

int _wrap_g_thread_yield(Stack stack, int rhs, int opt, int lhs) /* g_thread_yield */
{
  CheckRhs(0,0);
    g_thread_yield();
  return 0;
}

int _wrap_g_get_num_processors(Stack stack, int rhs, int opt, int lhs) /* g_get_num_processors */
{
  int ret;
  CheckRhs(0,0);
    ret =g_get_num_processors();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_thread_pool_set_max_unused_threads(Stack stack, int rhs, int opt, int lhs) /* g_thread_pool_set_max_unused_threads */
{
  int_types T[] = {s_int, t_end};
  int max_threads;
  if ( GetArgs(stack,rhs,opt,T,&max_threads) == FAIL) return RET_BUG;
    g_thread_pool_set_max_unused_threads(max_threads);
  return 0;
}

int _wrap_g_thread_pool_get_max_unused_threads(Stack stack, int rhs, int opt, int lhs) /* g_thread_pool_get_max_unused_threads */
{
  int ret;
  CheckRhs(0,0);
    ret =g_thread_pool_get_max_unused_threads();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_thread_pool_get_num_unused_threads(Stack stack, int rhs, int opt, int lhs) /* g_thread_pool_get_num_unused_threads */
{
  int ret;
  CheckRhs(0,0);
    ret =g_thread_pool_get_num_unused_threads();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_thread_pool_stop_unused_threads(Stack stack, int rhs, int opt, int lhs) /* g_thread_pool_stop_unused_threads */
{
  CheckRhs(0,0);
    g_thread_pool_stop_unused_threads();
  return 0;
}

int _wrap_g_thread_pool_set_max_idle_time(Stack stack, int rhs, int opt, int lhs) /* g_thread_pool_set_max_idle_time */
{
  int_types T[] = {s_int, t_end};
  int interval;
  if ( GetArgs(stack,rhs,opt,T,&interval) == FAIL) return RET_BUG;
    g_thread_pool_set_max_idle_time(interval);
  return 0;
}

int _wrap_g_thread_pool_get_max_idle_time(Stack stack, int rhs, int opt, int lhs) /* g_thread_pool_get_max_idle_time */
{
  int ret;
  CheckRhs(0,0);
    ret =g_thread_pool_get_max_idle_time();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_usleep(Stack stack, int rhs, int opt, int lhs) /* g_usleep */
{
  int_types T[] = {s_int, t_end};
  gulong microseconds;
  if ( GetArgs(stack,rhs,opt,T,&microseconds) == FAIL) return RET_BUG;
    g_usleep(microseconds);
  return 0;
}

int _wrap_g_utf8_get_char(Stack stack, int rhs, int opt, int lhs) /* g_utf8_get_char */
{
  int_types T[] = {string, t_end};
  char *p;
  gunichar ret;
  if ( GetArgs(stack,rhs,opt,T,&p) == FAIL) return RET_BUG;
    ret =g_utf8_get_char(p);
  if ( nsp_move_double(stack,1,(double) ret)== FAIL)return RET_BUG;
  return 1;
}

int _wrap_g_utf8_get_char_validated(Stack stack, int rhs, int opt, int lhs) /* g_utf8_get_char_validated */
{
  int_types T[] = {string,s_int, t_end};
  char *p;
  int max_len;
  gunichar ret;
  if ( GetArgs(stack,rhs,opt,T,&p, &max_len) == FAIL) return RET_BUG;
    ret =g_utf8_get_char_validated(p,max_len);
  if ( nsp_move_double(stack,1,(double) ret)== FAIL)return RET_BUG;
  return 1;
}

int _wrap_g_utf8_offset_to_pointer(Stack stack, int rhs, int opt, int lhs) /* g_utf8_offset_to_pointer */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int offset;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &offset) == FAIL) return RET_BUG;
    ret =g_utf8_offset_to_pointer(str,offset);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_pointer_to_offset(Stack stack, int rhs, int opt, int lhs) /* g_utf8_pointer_to_offset */
{
  int_types T[] = {string,string, t_end};
  char *str, *pos;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &pos) == FAIL) return RET_BUG;
    ret =g_utf8_pointer_to_offset(str,pos);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_utf8_prev_char(Stack stack, int rhs, int opt, int lhs) /* g_utf8_prev_char */
{
  int_types T[] = {string, t_end};
  char *p;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&p) == FAIL) return RET_BUG;
    ret =g_utf8_prev_char(p);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_find_next_char(Stack stack, int rhs, int opt, int lhs) /* g_utf8_find_next_char */
{
  int_types T[] = {string,string, t_end};
  char *p, *end;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&p, &end) == FAIL) return RET_BUG;
    ret =g_utf8_find_next_char(p,end);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_find_prev_char(Stack stack, int rhs, int opt, int lhs) /* g_utf8_find_prev_char */
{
  int_types T[] = {string,string, t_end};
  char *str, *p;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &p) == FAIL) return RET_BUG;
    ret =g_utf8_find_prev_char(str,p);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_strlen(Stack stack, int rhs, int opt, int lhs) /* g_utf8_strlen */
{
  int_types T[] = {string,s_int, t_end};
  char *p;
  int max, ret;
  if ( GetArgs(stack,rhs,opt,T,&p, &max) == FAIL) return RET_BUG;
    ret =g_utf8_strlen(p,max);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_utf8_substring(Stack stack, int rhs, int opt, int lhs) /* g_utf8_substring */
{
  int_types T[] = {string,s_int,s_int, t_end};
  char *str;
  int start_pos, end_pos;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &start_pos, &end_pos) == FAIL) return RET_BUG;
    ret =g_utf8_substring(str,start_pos,end_pos);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_strncpy(Stack stack, int rhs, int opt, int lhs) /* g_utf8_strncpy */
{
  int_types T[] = {string,string,s_int, t_end};
  char *dest, *src;
  int n;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&dest, &src, &n) == FAIL) return RET_BUG;
    ret =g_utf8_strncpy(dest,src,n);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_strchr(Stack stack, int rhs, int opt, int lhs) /* g_utf8_strchr */
{
  int_types T[] = {string,s_int,s_int, t_end};
  char *p;
  int len, nsp_c = 0;
  gunichar c;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&p, &len, &nsp_c) == FAIL) return RET_BUG;
  c = (gunichar)nsp_c;
    ret =g_utf8_strchr(p,len,c);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_strrchr(Stack stack, int rhs, int opt, int lhs) /* g_utf8_strrchr */
{
  int_types T[] = {string,s_int,s_int, t_end};
  char *p;
  int len, nsp_c = 0;
  gunichar c;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&p, &len, &nsp_c) == FAIL) return RET_BUG;
  c = (gunichar)nsp_c;
    ret =g_utf8_strrchr(p,len,c);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_strreverse(Stack stack, int rhs, int opt, int lhs) /* g_utf8_strreverse */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &len) == FAIL) return RET_BUG;
    ret =g_utf8_strreverse(str,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_validate(Stack stack, int rhs, int opt, int lhs) /* g_utf8_validate */
{
  int_types T[] = {string,s_int,obj, t_end};
  char *str;
  int max_len, ret;
  const gchar **end = NULL;
  NspObject *nsp_end = NULL;
  if ( GetArgs(stack,rhs,opt,T,&str, &max_len, &nsp_end) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_end))
    { end = (const gchar **) ((NspSMatrix *) nsp_end)->S;}
  else
    {
      Scierror("Error: end should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_utf8_validate(str,max_len,end);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_utf8_strup(Stack stack, int rhs, int opt, int lhs) /* g_utf8_strup */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &len) == FAIL) return RET_BUG;
    ret =g_utf8_strup(str,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_strdown(Stack stack, int rhs, int opt, int lhs) /* g_utf8_strdown */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &len) == FAIL) return RET_BUG;
    ret =g_utf8_strdown(str,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_casefold(Stack stack, int rhs, int opt, int lhs) /* g_utf8_casefold */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &len) == FAIL) return RET_BUG;
    ret =g_utf8_casefold(str,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_collate(Stack stack, int rhs, int opt, int lhs) /* g_utf8_collate */
{
  int_types T[] = {string,string, t_end};
  char *str1, *str2;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&str1, &str2) == FAIL) return RET_BUG;
    ret =g_utf8_collate(str1,str2);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_utf8_collate_key(Stack stack, int rhs, int opt, int lhs) /* g_utf8_collate_key */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &len) == FAIL) return RET_BUG;
    ret =g_utf8_collate_key(str,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_utf8_collate_key_for_filename(Stack stack, int rhs, int opt, int lhs) /* g_utf8_collate_key_for_filename */
{
  int_types T[] = {string,s_int, t_end};
  char *str;
  int len;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &len) == FAIL) return RET_BUG;
    ret =g_utf8_collate_key_for_filename(str,len);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_uri_unescape_string(Stack stack, int rhs, int opt, int lhs) /* g_uri_unescape_string */
{
  int_types T[] = {string,string, t_end};
  char *escaped_string, *illegal_characters;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&escaped_string, &illegal_characters) == FAIL) return RET_BUG;
    ret =g_uri_unescape_string(escaped_string,illegal_characters);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_uri_unescape_segment(Stack stack, int rhs, int opt, int lhs) /* g_uri_unescape_segment */
{
  int_types T[] = {string,string,string, t_end};
  char *escaped_string, *escaped_string_end, *illegal_characters;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&escaped_string, &escaped_string_end, &illegal_characters) == FAIL) return RET_BUG;
    ret =g_uri_unescape_segment(escaped_string,escaped_string_end,illegal_characters);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_uri_parse_scheme(Stack stack, int rhs, int opt, int lhs) /* g_uri_parse_scheme */
{
  int_types T[] = {string, t_end};
  char *uri;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&uri) == FAIL) return RET_BUG;
    ret =g_uri_parse_scheme(uri);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_uri_escape_string(Stack stack, int rhs, int opt, int lhs) /* g_uri_escape_string */
{
  int_types T[] = {string,string,s_bool, t_end};
  char *unescaped, *reserved_chars_allowed;
  int allow_utf8;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&unescaped, &reserved_chars_allowed, &allow_utf8) == FAIL) return RET_BUG;
    ret =g_uri_escape_string(unescaped,reserved_chars_allowed,allow_utf8);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_get_user_name(Stack stack, int rhs, int opt, int lhs) /* g_get_user_name */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_user_name();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_real_name(Stack stack, int rhs, int opt, int lhs) /* g_get_real_name */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_real_name();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_home_dir(Stack stack, int rhs, int opt, int lhs) /* g_get_home_dir */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_home_dir();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_tmp_dir(Stack stack, int rhs, int opt, int lhs) /* g_get_tmp_dir */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_tmp_dir();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_host_name(Stack stack, int rhs, int opt, int lhs) /* g_get_host_name */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_host_name();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_prgname(Stack stack, int rhs, int opt, int lhs) /* g_get_prgname */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_prgname();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_set_prgname(Stack stack, int rhs, int opt, int lhs) /* g_set_prgname */
{
  int_types T[] = {string, t_end};
  char *prgname;
  if ( GetArgs(stack,rhs,opt,T,&prgname) == FAIL) return RET_BUG;
    g_set_prgname(prgname);
  return 0;
}

int _wrap_g_get_application_name(Stack stack, int rhs, int opt, int lhs) /* g_get_application_name */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_application_name();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_set_application_name(Stack stack, int rhs, int opt, int lhs) /* g_set_application_name */
{
  int_types T[] = {string, t_end};
  char *application_name;
  if ( GetArgs(stack,rhs,opt,T,&application_name) == FAIL) return RET_BUG;
    g_set_application_name(application_name);
  return 0;
}

int _wrap_g_reload_user_special_dirs_cache(Stack stack, int rhs, int opt, int lhs) /* g_reload_user_special_dirs_cache */
{
  CheckRhs(0,0);
    g_reload_user_special_dirs_cache();
  return 0;
}

int _wrap_g_get_user_data_dir(Stack stack, int rhs, int opt, int lhs) /* g_get_user_data_dir */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_user_data_dir();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_user_config_dir(Stack stack, int rhs, int opt, int lhs) /* g_get_user_config_dir */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_user_config_dir();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_user_cache_dir(Stack stack, int rhs, int opt, int lhs) /* g_get_user_cache_dir */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_user_cache_dir();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_get_user_runtime_dir(Stack stack, int rhs, int opt, int lhs) /* g_get_user_runtime_dir */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_get_user_runtime_dir();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_find_program_in_path(Stack stack, int rhs, int opt, int lhs) /* g_find_program_in_path */
{
  int_types T[] = {string, t_end};
  char *program;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&program) == FAIL) return RET_BUG;
    ret =g_find_program_in_path(program);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_bit_nth_msf(Stack stack, int rhs, int opt, int lhs) /* g_bit_nth_msf */
{
  int_types T[] = {s_int,s_int, t_end};
  gulong mask;
  int nth_bit, ret;
  if ( GetArgs(stack,rhs,opt,T,&mask, &nth_bit) == FAIL) return RET_BUG;
    ret =g_bit_nth_msf(mask,nth_bit);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_bit_storage(Stack stack, int rhs, int opt, int lhs) /* g_bit_storage */
{
  int_types T[] = {s_int, t_end};
  gulong number;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&number) == FAIL) return RET_BUG;
    ret =g_bit_storage(number);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_bit_nth_lsf(Stack stack, int rhs, int opt, int lhs) /* g_bit_nth_lsf */
{
  int_types T[] = {s_int,s_int, t_end};
  gulong mask;
  int nth_bit, ret;
  if ( GetArgs(stack,rhs,opt,T,&mask, &nth_bit) == FAIL) return RET_BUG;
    ret =g_bit_nth_lsf(mask,nth_bit);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_variant_is_object_path(Stack stack, int rhs, int opt, int lhs) /* g_variant_is_object_path */
{
  int_types T[] = {string, t_end};
  char *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_variant_is_object_path(string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_variant_is_signature(Stack stack, int rhs, int opt, int lhs) /* g_variant_is_signature */
{
  int_types T[] = {string, t_end};
  char *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_variant_is_signature(string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_variant_parse(Stack stack, int rhs, int opt, int lhs) /* g_variant_parse */
{
  int_types T[] = {obj,string,string,obj, t_end};
  const GVariantType *type = NULL;
  NspObject *nsp_type = NULL, *nsp_endptr = NULL, *nsp_ret;
  char *text, *limit;
  const gchar **endptr = NULL;
  GError *error = NULL;
  GVariant *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type, &text, &limit, &nsp_endptr) == FAIL) return RET_BUG;
  if ( IsGVariantType(nsp_type))
    { type = ((NspGVariantType *) nsp_type)->obj->value;
    }
  else
    {
      Scierror("Error: type should be of type GVariantType\n");
      return RET_BUG;
    }
  if ( IsSMat(nsp_endptr))
    { endptr = (const gchar **) ((NspSMatrix *) nsp_endptr)->S;}
  else
    {
      Scierror("Error: endptr should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_variant_parse(type,text,limit,endptr,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_variant_parse_error_print_context(Stack stack, int rhs, int opt, int lhs) /* g_variant_parse_error_print_context */
{
  int_types T[] = {obj,string, t_end};
  GError *error = NULL;
  NspObject *nsp_error = NULL;
  char *source_str;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_error, &source_str) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_error, G_TYPE_ERROR))
      error = nspg_boxed_get(nsp_error, GError);
  else {
      Scierror( "Error: error should be a GError\n");
      return RET_BUG;
  }
    ret =g_variant_parse_error_print_context(error,source_str);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_variant_type_string_is_valid(Stack stack, int rhs, int opt, int lhs) /* g_variant_type_string_is_valid */
{
  int_types T[] = {string, t_end};
  char *type_string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&type_string) == FAIL) return RET_BUG;
    ret =g_variant_type_string_is_valid(type_string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_variant_type_string_scan(Stack stack, int rhs, int opt, int lhs) /* g_variant_type_string_scan */
{
  int_types T[] = {string,string,obj, t_end};
  char *string, *limit;
  const gchar **endptr = NULL;
  NspObject *nsp_endptr = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &limit, &nsp_endptr) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_endptr))
    { endptr = (const gchar **) ((NspSMatrix *) nsp_endptr)->S;}
  else
    {
      Scierror("Error: endptr should be of type SMat\n");
      return RET_BUG;
    }
    ret =g_variant_type_string_scan(string,limit,endptr);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_glib_check_version(Stack stack, int rhs, int opt, int lhs) /* glib_check_version */
{
  int_types T[] = {s_int,s_int,s_int, t_end};
  int required_major, required_minor, required_micro;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&required_major, &required_minor, &required_micro) == FAIL) return RET_BUG;
    ret =glib_check_version(required_major,required_minor,required_micro);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab glib_func[]={
  { "g_main_context_new", _wrap_g_main_context_new},
  { "gmaincontext_new", _wrap_g_main_context_new},
  { "g_main_loop_new", _wrap_g_main_loop_new},
  { "gmainloop_new", _wrap_g_main_loop_new},
 /* gsource_new g_source_new */
 /* gerror_new g_error_new_literal */
 /* gerror_new g_error_new_valist */
  { "g_variant_type_new", _wrap_g_variant_type_new},
  { "gvarianttype_new", _wrap_g_variant_type_new},
  { "g_variant_type_new_array", _wrap_g_variant_type_new_array},
  { "g_variant_type_new_maybe", _wrap_g_variant_type_new_maybe},
 /* gvarianttype_new g_variant_type_new_tuple */
  { "g_variant_type_new_dict_entry", _wrap_g_variant_type_new_dict_entry},
  { "g_variant_new_boolean", _wrap_g_variant_new_boolean},
  { "g_variant_new_byte", _wrap_g_variant_new_byte},
  { "g_variant_new_int16", _wrap_g_variant_new_int16},
  { "g_variant_new_uint16", _wrap_g_variant_new_uint16},
  { "g_variant_new_int32", _wrap_g_variant_new_int32},
  { "g_variant_new_uint32", _wrap_g_variant_new_uint32},
  { "g_variant_new_handle", _wrap_g_variant_new_handle},
  { "g_variant_new_double", _wrap_g_variant_new_double},
  { "g_variant_new_string", _wrap_g_variant_new_string},
  { "g_variant_new_take_string", _wrap_g_variant_new_take_string},
  { "g_variant_new_object_path", _wrap_g_variant_new_object_path},
  { "g_variant_new_signature", _wrap_g_variant_new_signature},
 /* gvariant_new g_variant_new_strv */
 /* gvariant_new g_variant_new_objv */
  { "g_variant_new_bytestring", _wrap_g_variant_new_bytestring},
 /* gvariant_new g_variant_new_bytestring_array */
 /* gvariant_new g_variant_new_fixed_array */
 /* gvariant_new g_variant_new_array */
 /* gvariant_new g_variant_new_tuple */
 /* gvariant_new g_variant_new_from_bytes */
 /* gvariant_new g_variant_new_from_data */
  { "g_variant_new", _wrap_g_variant_new},
  { "gvariant_new", _wrap_g_variant_new},
 /* gvariant_new g_variant_new_va */
  { "g_variant_new_parsed", _wrap_g_variant_new_parsed},
 /* gvariant_new g_variant_new_parsed_va */
  { "g_on_error_query", _wrap_g_on_error_query},
  { "g_on_error_stack_trace", _wrap_g_on_error_stack_trace},
  { "g_base64_encode_step", _wrap_g_base64_encode_step},
  { "g_base64_encode_close", _wrap_g_base64_encode_close},
  { "g_base64_encode", _wrap_g_base64_encode},
  { "g_get_codeset", _wrap_g_get_codeset},
  { "g_get_locale_variants", _wrap_g_get_locale_variants},
  { "g_filename_from_uri", _wrap_g_filename_from_uri},
  { "g_filename_to_uri", _wrap_g_filename_to_uri},
  { "g_filename_display_name", _wrap_g_filename_display_name},
  { "g_filename_display_basename", _wrap_g_filename_display_basename},
  { "g_uri_list_extract_uris", _wrap_g_uri_list_extract_uris},
  { "g_getenv", _wrap_g_getenv},
  { "g_setenv", _wrap_g_setenv},
  { "g_unsetenv", _wrap_g_unsetenv},
  { "g_listenv", _wrap_g_listenv},
  { "g_get_environ", _wrap_g_get_environ},
  { "g_environ_getenv", _wrap_g_environ_getenv},
  { "g_environ_setenv", _wrap_g_environ_setenv},
  { "g_environ_unsetenv", _wrap_g_environ_unsetenv},
  { "g_propagate_error", _wrap_g_propagate_error},
  { "g_clear_error", _wrap_g_clear_error},
  { "g_file_set_contents", _wrap_g_file_set_contents},
  { "g_file_read_link", _wrap_g_file_read_link},
  { "g_mkdtemp", _wrap_g_mkdtemp},
  { "g_mkdtemp_full", _wrap_g_mkdtemp_full},
  { "g_mkstemp", _wrap_g_mkstemp},
  { "g_mkstemp_full", _wrap_g_mkstemp_full},
  { "g_file_open_tmp", _wrap_g_file_open_tmp},
  { "g_dir_make_tmp", _wrap_g_dir_make_tmp},
  { "g_build_pathv", _wrap_g_build_pathv},
  { "g_build_filenamev", _wrap_g_build_filenamev},
  { "g_mkdir_with_parents", _wrap_g_mkdir_with_parents},
  { "g_path_is_absolute", _wrap_g_path_is_absolute},
  { "g_path_skip_root", _wrap_g_path_skip_root},
  { "g_get_current_dir", _wrap_g_get_current_dir},
  { "g_path_get_basename", _wrap_g_path_get_basename},
  { "g_path_get_dirname", _wrap_g_path_get_dirname},
  { "g_strip_context", _wrap_g_strip_context},
  { "g_dgettext", _wrap_g_dgettext},
  { "g_dcgettext", _wrap_g_dcgettext},
  { "g_dngettext", _wrap_g_dngettext},
  { "g_dpgettext", _wrap_g_dpgettext},
  { "g_dpgettext2", _wrap_g_dpgettext2},
  { "g_hostname_is_non_ascii", _wrap_g_hostname_is_non_ascii},
  { "g_hostname_is_ascii_encoded", _wrap_g_hostname_is_ascii_encoded},
  { "g_hostname_is_ip_address", _wrap_g_hostname_is_ip_address},
  { "g_hostname_to_ascii", _wrap_g_hostname_to_ascii},
  { "g_hostname_to_unicode", _wrap_g_hostname_to_unicode},
  { "g_list_alloc", _wrap_g_list_alloc},
  { "g_main_context_default", _wrap_g_main_context_default},
  { "g_main_depth", _wrap_g_main_depth},
  { "g_main_current_source", _wrap_g_main_current_source},
  { "g_main_context_get_thread_default", _wrap_g_main_context_get_thread_default},
  { "g_main_context_ref_thread_default", _wrap_g_main_context_ref_thread_default},
  { "g_source_set_name_by_id", _wrap_g_source_set_name_by_id},
  { "g_source_remove", _wrap_g_source_remove},
  { "g_markup_escape_text", _wrap_g_markup_escape_text},
  { "g_mem_is_system_malloc", _wrap_g_mem_is_system_malloc},
  { "g_mem_profile", _wrap_g_mem_profile},
  { "g_log_remove_handler", _wrap_g_log_remove_handler},
  { "g_return_if_fail_warning", _wrap_g_return_if_fail_warning},
  { "g_warn_message", _wrap_g_warn_message},
  { "g_pattern_match_simple", _wrap_g_pattern_match_simple},
  { "g_spaced_primes_closest", _wrap_g_spaced_primes_closest},
  { "g_intern_string", _wrap_g_intern_string},
  { "g_intern_static_string", _wrap_g_intern_static_string},
  { "g_random_set_seed", _wrap_g_random_set_seed},
  { "g_random_int", _wrap_g_random_int},
  { "g_random_int_range", _wrap_g_random_int_range},
  { "g_random_double", _wrap_g_random_double},
  { "g_random_double_range", _wrap_g_random_double_range},
  { "g_regex_escape_string", _wrap_g_regex_escape_string},
  { "g_regex_escape_nul", _wrap_g_regex_escape_nul},
  { "g_shell_quote", _wrap_g_shell_quote},
  { "g_shell_unquote", _wrap_g_shell_unquote},
  { "g_slist_alloc", _wrap_g_slist_alloc},
  { "g_spawn_command_line_async", _wrap_g_spawn_command_line_async},
  { "g_spawn_check_exit_status", _wrap_g_spawn_check_exit_status},
  { "g_ascii_tolower", _wrap_g_ascii_tolower},
  { "g_ascii_toupper", _wrap_g_ascii_toupper},
  { "g_ascii_digit_value", _wrap_g_ascii_digit_value},
  { "g_ascii_xdigit_value", _wrap_g_ascii_xdigit_value},
  { "g_strdelimit", _wrap_g_strdelimit},
  { "g_strcanon", _wrap_g_strcanon},
  { "g_strerror", _wrap_g_strerror},
  { "g_strsignal", _wrap_g_strsignal},
  { "g_strreverse", _wrap_g_strreverse},
  { "g_strlcpy", _wrap_g_strlcpy},
  { "g_strlcat", _wrap_g_strlcat},
  { "g_strstr_len", _wrap_g_strstr_len},
  { "g_strrstr", _wrap_g_strrstr},
  { "g_strrstr_len", _wrap_g_strrstr_len},
  { "g_str_has_suffix", _wrap_g_str_has_suffix},
  { "g_str_has_prefix", _wrap_g_str_has_prefix},
  { "g_strtod", _wrap_g_strtod},
  { "g_ascii_strtod", _wrap_g_ascii_strtod},
  { "g_ascii_dtostr", _wrap_g_ascii_dtostr},
  { "g_ascii_formatd", _wrap_g_ascii_formatd},
  { "g_strchug", _wrap_g_strchug},
  { "g_strchomp", _wrap_g_strchomp},
  { "g_ascii_strcasecmp", _wrap_g_ascii_strcasecmp},
  { "g_ascii_strncasecmp", _wrap_g_ascii_strncasecmp},
  { "g_ascii_strdown", _wrap_g_ascii_strdown},
  { "g_ascii_strup", _wrap_g_ascii_strup},
  { "g_str_is_ascii", _wrap_g_str_is_ascii},
  { "g_strdup", _wrap_g_strdup},
  { "g_strndup", _wrap_g_strndup},
  { "g_strnfill", _wrap_g_strnfill},
  { "g_strcompress", _wrap_g_strcompress},
  { "g_strescape", _wrap_g_strescape},
  { "g_strsplit", _wrap_g_strsplit},
  { "g_strsplit_set", _wrap_g_strsplit_set},
  { "g_strjoinv", _wrap_g_strjoinv},
  { "g_strfreev", _wrap_g_strfreev},
  { "g_strdupv", _wrap_g_strdupv},
  { "g_strv_length", _wrap_g_strv_length},
  { "g_stpcpy", _wrap_g_stpcpy},
  { "g_str_to_ascii", _wrap_g_str_to_ascii},
  { "g_str_match_string", _wrap_g_str_match_string},
  { "g_strcmp0", _wrap_g_strcmp0},
  { "g_test_subprocess", _wrap_g_test_subprocess},
  { "g_test_run", _wrap_g_test_run},
  { "g_test_fail", _wrap_g_test_fail},
  { "g_test_incomplete", _wrap_g_test_incomplete},
  { "g_test_skip", _wrap_g_test_skip},
  { "g_test_failed", _wrap_g_test_failed},
  { "g_test_set_nonfatal_assertions", _wrap_g_test_set_nonfatal_assertions},
  { "g_test_bug_base", _wrap_g_test_bug_base},
  { "g_test_bug", _wrap_g_test_bug},
  { "g_test_timer_start", _wrap_g_test_timer_start},
  { "g_test_timer_elapsed", _wrap_g_test_timer_elapsed},
  { "g_test_timer_last", _wrap_g_test_timer_last},
  { "g_test_trap_has_passed", _wrap_g_test_trap_has_passed},
  { "g_test_trap_reached_timeout", _wrap_g_test_trap_reached_timeout},
  { "g_test_rand_int", _wrap_g_test_rand_int},
  { "g_test_rand_int_range", _wrap_g_test_rand_int_range},
  { "g_test_rand_double", _wrap_g_test_rand_double},
  { "g_test_rand_double_range", _wrap_g_test_rand_double_range},
  { "g_assertion_message", _wrap_g_assertion_message},
  { "g_assertion_message_expr", _wrap_g_assertion_message_expr},
  { "g_assertion_message_cmpstr", _wrap_g_assertion_message_cmpstr},
  { "g_test_assert_expected_messages_internal", _wrap_g_test_assert_expected_messages_internal},
  { "g_thread_yield", _wrap_g_thread_yield},
  { "g_get_num_processors", _wrap_g_get_num_processors},
  { "g_thread_pool_set_max_unused_threads", _wrap_g_thread_pool_set_max_unused_threads},
  { "g_thread_pool_get_max_unused_threads", _wrap_g_thread_pool_get_max_unused_threads},
  { "g_thread_pool_get_num_unused_threads", _wrap_g_thread_pool_get_num_unused_threads},
  { "g_thread_pool_stop_unused_threads", _wrap_g_thread_pool_stop_unused_threads},
  { "g_thread_pool_set_max_idle_time", _wrap_g_thread_pool_set_max_idle_time},
  { "g_thread_pool_get_max_idle_time", _wrap_g_thread_pool_get_max_idle_time},
  { "g_usleep", _wrap_g_usleep},
  { "g_utf8_get_char", _wrap_g_utf8_get_char},
  { "g_utf8_get_char_validated", _wrap_g_utf8_get_char_validated},
  { "g_utf8_offset_to_pointer", _wrap_g_utf8_offset_to_pointer},
  { "g_utf8_pointer_to_offset", _wrap_g_utf8_pointer_to_offset},
  { "g_utf8_prev_char", _wrap_g_utf8_prev_char},
  { "g_utf8_find_next_char", _wrap_g_utf8_find_next_char},
  { "g_utf8_find_prev_char", _wrap_g_utf8_find_prev_char},
  { "g_utf8_strlen", _wrap_g_utf8_strlen},
  { "g_utf8_substring", _wrap_g_utf8_substring},
  { "g_utf8_strncpy", _wrap_g_utf8_strncpy},
  { "g_utf8_strchr", _wrap_g_utf8_strchr},
  { "g_utf8_strrchr", _wrap_g_utf8_strrchr},
  { "g_utf8_strreverse", _wrap_g_utf8_strreverse},
  { "g_utf8_validate", _wrap_g_utf8_validate},
  { "g_utf8_strup", _wrap_g_utf8_strup},
  { "g_utf8_strdown", _wrap_g_utf8_strdown},
  { "g_utf8_casefold", _wrap_g_utf8_casefold},
  { "g_utf8_collate", _wrap_g_utf8_collate},
  { "g_utf8_collate_key", _wrap_g_utf8_collate_key},
  { "g_utf8_collate_key_for_filename", _wrap_g_utf8_collate_key_for_filename},
  { "g_uri_unescape_string", _wrap_g_uri_unescape_string},
  { "g_uri_unescape_segment", _wrap_g_uri_unescape_segment},
  { "g_uri_parse_scheme", _wrap_g_uri_parse_scheme},
  { "g_uri_escape_string", _wrap_g_uri_escape_string},
  { "g_get_user_name", _wrap_g_get_user_name},
  { "g_get_real_name", _wrap_g_get_real_name},
  { "g_get_home_dir", _wrap_g_get_home_dir},
  { "g_get_tmp_dir", _wrap_g_get_tmp_dir},
  { "g_get_host_name", _wrap_g_get_host_name},
  { "g_get_prgname", _wrap_g_get_prgname},
  { "g_set_prgname", _wrap_g_set_prgname},
  { "g_get_application_name", _wrap_g_get_application_name},
  { "g_set_application_name", _wrap_g_set_application_name},
  { "g_reload_user_special_dirs_cache", _wrap_g_reload_user_special_dirs_cache},
  { "g_get_user_data_dir", _wrap_g_get_user_data_dir},
  { "g_get_user_config_dir", _wrap_g_get_user_config_dir},
  { "g_get_user_cache_dir", _wrap_g_get_user_cache_dir},
  { "g_get_user_runtime_dir", _wrap_g_get_user_runtime_dir},
  { "g_find_program_in_path", _wrap_g_find_program_in_path},
  { "g_bit_nth_msf", _wrap_g_bit_nth_msf},
  { "g_bit_storage", _wrap_g_bit_storage},
  { "g_bit_nth_lsf", _wrap_g_bit_nth_lsf},
  { "g_variant_is_object_path", _wrap_g_variant_is_object_path},
  { "g_variant_is_signature", _wrap_g_variant_is_signature},
  { "g_variant_parse", _wrap_g_variant_parse},
  { "g_variant_parse_error_print_context", _wrap_g_variant_parse_error_print_context},
  { "g_variant_type_string_is_valid", _wrap_g_variant_type_string_is_valid},
  { "g_variant_type_string_scan", _wrap_g_variant_type_string_scan},
  { "glib_check_version", _wrap_glib_check_version},
  { "glib_create", int_gvarianttype_create},
  { NULL, NULL}
};

/* call ith function in the glib interface */

int glib_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(glib_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void glib_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = glib_func[i].name;
  *f = glib_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
glib_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_IO_CONDITION, strip_prefix);
}

void nsp_initialize_glib_types(void)
{
  new_type_gmaincontext(T_BASE);
  new_type_gmainloop(T_BASE);
  new_type_gsource(T_BASE);
  new_type_gerror(T_BASE);
  new_type_gvarianttype(T_BASE);
  new_type_gvariant(T_BASE);
}

#line 295 "codegen-3.0/glib.override"

static int nsp_destroy_GVariant(GVariant *value,NspGVariant *H)
{
  g_variant_unref(value);
  return OK;
}

static int nsp_print_GVariant(int indent,GVariant *v,NspGVariant *M)
{
  gchar *str = g_variant_print(v,TRUE);
  Sciprintf1(indent+1,"gvariant: %s\n",str);
  g_free(str);
  return 0;
}

static int nsp_check_GVariant(GVariant *v,NspGVariant *H)
{
  if (g_variant_is_floating (v) )
    {
      /* Sciprintf("The variant is floating\n"); */
    }
  g_variant_ref_sink(v);
  return OK;
}

static int nsp_GVariant_full_copy(NspGVariant *H,GVariant *value,NspGVariant *self)
{
#if 0
  memset(value, 0, sizeof(*value));
  g_variant_init (value, G_VALUE_TYPE(&self->value));
  g_variant_copy (&self->value,value);
#endif
  return OK;
}


static int nsp_destroy_GVariantType(GVariantType *value,NspGVariantType *H)
{
  /* nsp_print_GVariantType(0,value,H);  Sciprintf("Destroy a GVariantType\n");*/
  g_variant_type_free(value);
  return OK;
}

static int nsp_print_GVariantType(int indent,GVariantType *v,NspGVariantType *M)
{
  gchar *str = g_variant_type_dup_string(v);
  Sciprintf1(indent+1,"gvariant_type: %s\n",str);
  g_free(str);
  return 0;
}

static int nsp_check_GVariantType(GVariantType *v,NspGVariantType *H)
{
  return OK;
}

static int nsp_GVariantType_full_copy(NspGVariantType *H,GVariantType *value,NspGVariantType *self)
{
#if 0
  /* XXXX */
  memset(value, 0, sizeof(*value));
  g_variant_init (value, G_VALUE_TYPE(&self->value));
  g_variant_copy (&self->value,value);
#endif
  return OK;
}

GVariant *nsp_copy_GVariant(GVariant *gv)
{
  return (gv == NULL) ? gv : g_variant_ref(gv); 
}

GVariantType *nsp_copy_GVariantType(const GVariantType *gv)
{
  return g_variant_type_copy(gv);
}

#line 5841 "glib.c"
