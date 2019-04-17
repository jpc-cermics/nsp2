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





#line 5 "codegen-3.0/girepository.override"
#include <girepository.h>
#include <nsp/nsp.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gobject-util.h>
#include <nsp/gtk/girepository.h>
#define NspGIBaseInfo_Private
#include <nsp/gtk/gibaseinfo.h>

static const char *nsp_gi_info_str (GIBaseInfo *info);
static int nsp_gi_info_check(GIBaseInfo *info, GIInfoType info_type_check);

#line 41 "girepository.c"
/* ---------- types from other modules ---------- */
#include <nsp/gtk/gobject.h>
/* ---------- forward type declarations ---------- */
#include <nsp/gtk/gibaseinfo.h>
#include <nsp/gtk/gifunctioninfo.h>
#include <nsp/gtk/girepository.h>


/* -----------NspGIBaseInfo ----------- */


#define  NspGIBaseInfo_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gibaseinfo.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGIBaseInfo inherits from GBoxed 
 */

int nsp_type_gibaseinfo_id=0;
NspTypeGIBaseInfo *nsp_type_gibaseinfo=NULL;

/*
 * Type object for NspGIBaseInfo 
 * all the instance of NspTypeGIBaseInfo share the same id. 
 * nsp_type_gibaseinfo: is an instance of NspTypeGIBaseInfo 
 *    used for objects of NspGIBaseInfo type (i.e built with new_gibaseinfo) 
 * other instances are used for derived classes 
 */
NspTypeGIBaseInfo *new_type_gibaseinfo(type_mode mode)
{
  NspTypeGIBaseInfo *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gibaseinfo != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gibaseinfo;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gibaseinfo_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gibaseinfo_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gibaseinfo;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gibaseinfo */ 

  top->s_type =  (s_type_func *) nsp_gibaseinfo_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gibaseinfo_type_short_string;
  /* top->create = (create_func*) int_gibaseinfo_create;*/

  /* specific methods for gibaseinfo */

  type->init = (init_func *) init_gibaseinfo;

  /* 
   * NspGIBaseInfo interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gibaseinfo_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGIBaseInfo called nsp_type_gibaseinfo
       */
      type->id =  nsp_type_gibaseinfo_id = nsp_new_type_id();
      nsp_type_gibaseinfo = type;
      if ( nsp_register_type(nsp_type_gibaseinfo) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gibaseinfo, GI_TYPE_BASE_INFO);
      return ( mode == T_BASE ) ? type : new_type_gibaseinfo(mode);
    }
  else 
    {
      type->id = nsp_type_gibaseinfo_id;
      return type;
    }
}

/*
 * initialize NspGIBaseInfo instances 
 * locally and by calling initializer on parent class 
 */

static int init_gibaseinfo(NspGIBaseInfo *Obj,NspTypeGIBaseInfo *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGIBaseInfo 
 */

NspGIBaseInfo *new_gibaseinfo() 
{
  NspGIBaseInfo *loc;
  /* type must exists */
  nsp_type_gibaseinfo = new_type_gibaseinfo(T_BASE);
  if ( (loc = malloc(sizeof(NspGIBaseInfo)))== NULLGIBASEINFO) return loc;
  /* initialize object */
  if ( init_gibaseinfo(loc,nsp_type_gibaseinfo) == FAIL) return NULLGIBASEINFO;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGIBaseInfo 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gibaseinfo_type_name[]="GIBaseInfo";
static char gibaseinfo_short_type_name[]="GIBaseInfo";

static char *nsp_gibaseinfo_type_as_string(void)
{
  return(gibaseinfo_type_name);
}

static char *nsp_gibaseinfo_type_short_string(NspObject *v)
{
  return(gibaseinfo_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGIBaseInfo objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGIBaseInfo   *nsp_gibaseinfo_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gibaseinfo_id)  == TRUE  ) return ((NspGIBaseInfo *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gibaseinfo));
  return NULL;
}

int IsGIBaseInfoObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gibaseinfo_id);
}

int IsGIBaseInfo(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gibaseinfo_id);
}

NspGIBaseInfo  *GetGIBaseInfoCopy(Stack stack, int i)
{
  if (  GetGIBaseInfo(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGIBaseInfo  *GetGIBaseInfo(Stack stack, int i)
{
  NspGIBaseInfo *M;
  if (( M = nsp_gibaseinfo_object(NthObj(i))) == NULLGIBASEINFO)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGIBaseInfo *gibaseinfo_copy(NspGIBaseInfo *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gibaseinfo);
}

/*-------------------------------------------------------------------
 * wrappers for the GIBaseInfo
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 126 "codegen-3.0/girepository.override"


static int _wrap_g_base_info_get_attributes(NspGIBaseInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  GIAttributeIter iter = { 0, };
  char *name;
  char *value;
  CheckRhs(0,0);
  Sciprintf("attributes start \n");
  
  GIBaseInfo *info = NSP_GBOXED_GET(self, GIBaseInfo);
  if (info != NULL)
    {
      Sciprintf("for object %s of type %s\n",g_base_info_get_name (info),nsp_gi_info_str (info));
    }
  while (g_base_info_iterate_attributes (NSP_GBOXED_GET(self, GIBaseInfo), &iter, &name, &value))
    {
      Sciprintf("attribute name: %s value: %s\n", name, value);
    }
  Sciprintf("attributes end \n");
  return 0;
}

#line 265 "girepository.c"


static int _wrap_g_function_info_get_symbol(NspGIBaseInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
 if ( nsp_gi_info_check(NSP_GBOXED_GET(self, GIBaseInfo), GI_INFO_TYPE_FUNCTION) == FAIL) return RET_BUG;
          ret =g_function_info_get_symbol(NSP_GBOXED_GET(self, GIBaseInfo));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods gibaseinfo_methods[] = {
  {"get_attributes",(nsp_method *) _wrap_g_base_info_get_attributes},
  {"get_symbol",(nsp_method *) _wrap_g_function_info_get_symbol},
  { NULL, NULL}
};

static NspMethods *gibaseinfo_get_methods(void) { return gibaseinfo_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gibaseinfo_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGIFunctionInfo ----------- */


#define  NspGIFunctionInfo_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gifunctioninfo.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGIFunctionInfo inherits from GBoxed 
 */

int nsp_type_gifunctioninfo_id=0;
NspTypeGIFunctionInfo *nsp_type_gifunctioninfo=NULL;

/*
 * Type object for NspGIFunctionInfo 
 * all the instance of NspTypeGIFunctionInfo share the same id. 
 * nsp_type_gifunctioninfo: is an instance of NspTypeGIFunctionInfo 
 *    used for objects of NspGIFunctionInfo type (i.e built with new_gifunctioninfo) 
 * other instances are used for derived classes 
 */
NspTypeGIFunctionInfo *new_type_gifunctioninfo(type_mode mode)
{
  NspTypeGIFunctionInfo *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gifunctioninfo != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gifunctioninfo;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gifunctioninfo_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gifunctioninfo_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gifunctioninfo;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gifunctioninfo */ 

  top->s_type =  (s_type_func *) nsp_gifunctioninfo_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gifunctioninfo_type_short_string;
  /* top->create = (create_func*) int_gifunctioninfo_create;*/

  /* specific methods for gifunctioninfo */

  type->init = (init_func *) init_gifunctioninfo;

  /* 
   * NspGIFunctionInfo interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gifunctioninfo_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGIFunctionInfo called nsp_type_gifunctioninfo
       */
      type->id =  nsp_type_gifunctioninfo_id = nsp_new_type_id();
      nsp_type_gifunctioninfo = type;
      if ( nsp_register_type(nsp_type_gifunctioninfo) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gifunctioninfo, GI_TYPE_BASE_INFO);
      return ( mode == T_BASE ) ? type : new_type_gifunctioninfo(mode);
    }
  else 
    {
      type->id = nsp_type_gifunctioninfo_id;
      return type;
    }
}

/*
 * initialize NspGIFunctionInfo instances 
 * locally and by calling initializer on parent class 
 */

static int init_gifunctioninfo(NspGIFunctionInfo *Obj,NspTypeGIFunctionInfo *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGIFunctionInfo 
 */

NspGIFunctionInfo *new_gifunctioninfo() 
{
  NspGIFunctionInfo *loc;
  /* type must exists */
  nsp_type_gifunctioninfo = new_type_gifunctioninfo(T_BASE);
  if ( (loc = malloc(sizeof(NspGIFunctionInfo)))== NULLGIFUNCTIONINFO) return loc;
  /* initialize object */
  if ( init_gifunctioninfo(loc,nsp_type_gifunctioninfo) == FAIL) return NULLGIFUNCTIONINFO;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGIFunctionInfo 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gifunctioninfo_type_name[]="GIFunctionInfo";
static char gifunctioninfo_short_type_name[]="GIFunctionInfo";

static char *nsp_gifunctioninfo_type_as_string(void)
{
  return(gifunctioninfo_type_name);
}

static char *nsp_gifunctioninfo_type_short_string(NspObject *v)
{
  return(gifunctioninfo_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGIFunctionInfo objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGIFunctionInfo   *nsp_gifunctioninfo_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gifunctioninfo_id)  == TRUE  ) return ((NspGIFunctionInfo *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gifunctioninfo));
  return NULL;
}

int IsGIFunctionInfoObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gifunctioninfo_id);
}

int IsGIFunctionInfo(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gifunctioninfo_id);
}

NspGIFunctionInfo  *GetGIFunctionInfoCopy(Stack stack, int i)
{
  if (  GetGIFunctionInfo(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGIFunctionInfo  *GetGIFunctionInfo(Stack stack, int i)
{
  NspGIFunctionInfo *M;
  if (( M = nsp_gifunctioninfo_object(NthObj(i))) == NULLGIFUNCTIONINFO)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGIFunctionInfo *gifunctioninfo_copy(NspGIFunctionInfo *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gifunctioninfo);
}

/*-------------------------------------------------------------------
 * wrappers for the GIFunctionInfo
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gifunctioninfo_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gifunctioninfo_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGIRepository ----------- */


#define  NspGIRepository_Private 
#include <nsp/objects.h>
#include <nsp/gtk/girepository.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGIRepository inherits from GObject 
 */

int nsp_type_girepository_id=0;
NspTypeGIRepository *nsp_type_girepository=NULL;

/*
 * Type object for NspGIRepository 
 * all the instance of NspTypeGIRepository share the same id. 
 * nsp_type_girepository: is an instance of NspTypeGIRepository 
 *    used for objects of NspGIRepository type (i.e built with new_girepository) 
 * other instances are used for derived classes 
 */
NspTypeGIRepository *new_type_girepository(type_mode mode)
{
  NspTypeGIRepository *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_girepository != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_girepository;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = girepository_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = girepository_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_girepository;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for girepository */ 

  top->s_type =  (s_type_func *) nsp_girepository_type_as_string;
  top->sh_type = (sh_type_func *) nsp_girepository_type_short_string;
  /* top->create = (create_func*) int_girepository_create;*/

  /* specific methods for girepository */

  type->init = (init_func *) init_girepository;

  /* 
   * NspGIRepository interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_girepository_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGIRepository called nsp_type_girepository
       */
      type->id =  nsp_type_girepository_id = nsp_new_type_id();
      nsp_type_girepository = type;
      if ( nsp_register_type(nsp_type_girepository) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_girepository, G_TYPE_IREPOSITORY);
      return ( mode == T_BASE ) ? type : new_type_girepository(mode);
    }
  else 
    {
      type->id = nsp_type_girepository_id;
      return type;
    }
}

/*
 * initialize NspGIRepository instances 
 * locally and by calling initializer on parent class 
 */

static int init_girepository(NspGIRepository *Obj,NspTypeGIRepository *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGIRepository 
 */

NspGIRepository *new_girepository() 
{
  NspGIRepository *loc;
  /* type must exists */
  nsp_type_girepository = new_type_girepository(T_BASE);
  if ( (loc = malloc(sizeof(NspGIRepository)))== NULLGIREPOSITORY) return loc;
  /* initialize object */
  if ( init_girepository(loc,nsp_type_girepository) == FAIL) return NULLGIREPOSITORY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGIRepository 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char girepository_type_name[]="GIRepository";
static char girepository_short_type_name[]="GIRepository";

static char *nsp_girepository_type_as_string(void)
{
  return(girepository_type_name);
}

static char *nsp_girepository_type_short_string(NspObject *v)
{
  return(girepository_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGIRepository objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGIRepository   *nsp_girepository_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_girepository_id)  == TRUE  ) return ((NspGIRepository *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_girepository));
  return NULL;
}

int IsGIRepositoryObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_girepository_id);
}

int IsGIRepository(NspObject *O)
{
  return nsp_object_type(O,nsp_type_girepository_id);
}

NspGIRepository  *GetGIRepositoryCopy(Stack stack, int i)
{
  if (  GetGIRepository(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGIRepository  *GetGIRepository(Stack stack, int i)
{
  NspGIRepository *M;
  if (( M = nsp_girepository_object(NthObj(i))) == NULLGIREPOSITORY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGIRepository *girepository_copy(NspGIRepository *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_girepository);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_girepository);
}

/*-------------------------------------------------------------------
 * wrappers for the GIRepository
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_irepository_is_registered(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *namespace_, *version;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_, &version) == FAIL) return RET_BUG;
    ret =g_irepository_is_registered(G_IREPOSITORY(self->obj),namespace_,version);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_irepository_find_by_name(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *namespace_, *name;
  GIBaseInfo *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_, &name) == FAIL) return RET_BUG;
    ret =g_irepository_find_by_name(G_IREPOSITORY(self->obj),namespace_,name);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GI_TYPE_BASE_INFO, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gibaseinfo))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 29 "codegen-3.0/girepository.override"

static int _wrap_g_irepository_enumerate_versions
(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *namespace;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&namespace) == FAIL) return RET_BUG;
  ret = g_irepository_enumerate_versions(G_IREPOSITORY(self->obj), namespace);
  NSP_LIST_FROM_GLIST(ret,nsp_new_string_obj("lel",tmp->data,-1),g_list_free);
}

#line 722 "girepository.c"


#line 59 "codegen-3.0/girepository.override"

static int _wrap_g_irepository_require
(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,t_end};
  char *namespace, *version;
  GIRepositoryLoadFlags flags = 0;
  GError *error= NULL;
  if ( GetArgs(stack,rhs,opt,T,&namespace,&version) == FAIL) return RET_BUG;
  /* optional argument lazy
   * if true flags |= G_IREPOSITORY_LOAD_FLAG_LAZY;
   */
  g_irepository_require (G_IREPOSITORY(self->obj), namespace, version, flags, &error);
  if (error != NULL) {
    Scierror("Error: %s\n",error->message);
    g_error_free (error);
    return RET_BUG;
  }
  return 0;
}

#line 747 "girepository.c"


#if GTK_CHECK_VERSION(1,44,0)
static int _wrap_g_irepository_get_immediate_dependencies(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *namespace_;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_) == FAIL) return RET_BUG;
    ret =g_irepository_get_immediate_dependencies(G_IREPOSITORY(self->obj),namespace_);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_irepository_get_immediate_dependencies(Stack stack, int rhs, int opt, int lhs) /* get_immediate_dependencies */
{
  Scierror("Error: function g_irepository_get_immediate_dependencies not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_irepository_get_dependencies(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *namespace_;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_) == FAIL) return RET_BUG;
    ret =g_irepository_get_dependencies(G_IREPOSITORY(self->obj),namespace_);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 44 "codegen-3.0/girepository.override"

static int _wrap_g_irepository_get_loaded_namespaces
(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  char **namespaces;
  /* GIBaseInfo *info; */
  CheckRhs(0,0);
  namespaces = g_irepository_get_loaded_namespaces (G_IREPOSITORY(self->obj));
  NspSMatrix*Res= nsp_smatrix_create_from_table(namespaces);
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

#line 802 "girepository.c"


static int _wrap_g_irepository_find_by_gtype(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GType gtype;
  NspObject *nsp_gtype = NULL, *nsp_ret;
  GIBaseInfo *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_gtype) == FAIL) return RET_BUG;
  if ((gtype = nspg_type_from_object(nsp_gtype)) == FAIL)
      return RET_BUG;
    ret =g_irepository_find_by_gtype(G_IREPOSITORY(self->obj),gtype);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GI_TYPE_BASE_INFO, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gibaseinfo))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 82 "codegen-3.0/girepository.override"

static int _wrap_g_irepository_get_n_infos
(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  gssize n_infos;
  /* NspObject *infos;  */
  gssize i;
  int_types T[] = {string,t_end};
  char *namespace;
  if ( GetArgs(stack,rhs,opt,T,&namespace) == FAIL) return RET_BUG;
  n_infos = g_irepository_get_n_infos (G_IREPOSITORY(self->obj), namespace);
  if (n_infos < 0)
    {
      Scierror("Error: Namespace %s is not loaded",namespace);
      return RET_BUG;
    }
  Sciprintf("Found %d infos\n",n_infos);
  for (i = 0; i < n_infos; i++)
    {
      GIBaseInfo *info;
      info = g_irepository_get_info(G_IREPOSITORY(self->obj), namespace, i);
      if (info != NULL)
	{
	  GIBaseInfo *container;
	  Sciprintf("object %d is %s\n",i, g_base_info_get_name (info));
	  container = g_base_info_get_container (info);
	  if ( container != NULL)
	    Sciprintf("  container is %s\n", g_base_info_get_name (container));
	  else
	    Sciprintf("  no container for %s\n", g_base_info_get_name (info));
	  Sciprintf("%s\n",nsp_gi_info_str (info));
	}
    }
  return 0;
}

/*
 * g_base_info_get_namespace (self->info);
 * GIBaseInfo *info = g_base_info_get_container (self->info);
 * g_base_info_get_namespace(self->info);
 */

#line 865 "girepository.c"


static int _wrap_g_irepository_get_info(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *namespace_;
  int index;
  GIBaseInfo *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_, &index) == FAIL) return RET_BUG;
    ret =g_irepository_get_info(G_IREPOSITORY(self->obj),namespace_,index);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GI_TYPE_BASE_INFO, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gibaseinfo))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_irepository_get_typelib_path(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *namespace_;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_) == FAIL) return RET_BUG;
    ret =g_irepository_get_typelib_path(G_IREPOSITORY(self->obj),namespace_);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_irepository_get_shared_library(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *namespace_;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_) == FAIL) return RET_BUG;
    ret =g_irepository_get_shared_library(G_IREPOSITORY(self->obj),namespace_);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_irepository_get_c_prefix(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *namespace_;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_) == FAIL) return RET_BUG;
    ret =g_irepository_get_c_prefix(G_IREPOSITORY(self->obj),namespace_);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_irepository_get_version(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *namespace_;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace_) == FAIL) return RET_BUG;
    ret =g_irepository_get_version(G_IREPOSITORY(self->obj),namespace_);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods girepository_methods[] = {
  {"is_registered",(nsp_method *) _wrap_g_irepository_is_registered},
  {"find_by_name",(nsp_method *) _wrap_g_irepository_find_by_name},
  {"enumerate_versions",(nsp_method *) _wrap_g_irepository_enumerate_versions},
  {"require",(nsp_method *) _wrap_g_irepository_require},
  {"get_immediate_dependencies",(nsp_method *) _wrap_g_irepository_get_immediate_dependencies},
  {"get_dependencies",(nsp_method *) _wrap_g_irepository_get_dependencies},
  {"get_loaded_namespaces",(nsp_method *) _wrap_g_irepository_get_loaded_namespaces},
  {"find_by_gtype",(nsp_method *) _wrap_g_irepository_find_by_gtype},
  {"get_n_infos",(nsp_method *) _wrap_g_irepository_get_n_infos},
  {"get_info",(nsp_method *) _wrap_g_irepository_get_info},
  {"get_typelib_path",(nsp_method *) _wrap_g_irepository_get_typelib_path},
  {"get_shared_library",(nsp_method *) _wrap_g_irepository_get_shared_library},
  {"get_c_prefix",(nsp_method *) _wrap_g_irepository_get_c_prefix},
  {"get_version",(nsp_method *) _wrap_g_irepository_get_version},
  { NULL, NULL}
};

static NspMethods *girepository_get_methods(void) { return girepository_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab girepository_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_g_base_info_ref(Stack stack, int rhs, int opt, int lhs) /* g_base_info_ref */
{
  int_types T[] = {obj, t_end};
  GIBaseInfo *info = NULL, *ret;
  NspObject *nsp_info = NULL, *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info, GI_TYPE_BASE_INFO))
      info = nspg_boxed_get(nsp_info, GIBaseInfo);
  else {
      Scierror( "Error: info should be a GIBaseInfo\n");
      return RET_BUG;
  }
    ret =g_base_info_ref(info);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GI_TYPE_BASE_INFO, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gibaseinfo))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_base_info_unref(Stack stack, int rhs, int opt, int lhs) /* g_base_info_unref */
{
  int_types T[] = {obj, t_end};
  GIBaseInfo *info = NULL;
  NspObject *nsp_info = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info, GI_TYPE_BASE_INFO))
      info = nspg_boxed_get(nsp_info, GIBaseInfo);
  else {
      Scierror( "Error: info should be a GIBaseInfo\n");
      return RET_BUG;
  }
    g_base_info_unref(info);
  return 0;
}

int _wrap_g_base_info_get_name(Stack stack, int rhs, int opt, int lhs) /* g_base_info_get_name */
{
  int_types T[] = {obj, t_end};
  GIBaseInfo *info = NULL;
  NspObject *nsp_info = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info, GI_TYPE_BASE_INFO))
      info = nspg_boxed_get(nsp_info, GIBaseInfo);
  else {
      Scierror( "Error: info should be a GIBaseInfo\n");
      return RET_BUG;
  }
    ret =g_base_info_get_name(info);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_base_info_get_namespace(Stack stack, int rhs, int opt, int lhs) /* g_base_info_get_namespace */
{
  int_types T[] = {obj, t_end};
  GIBaseInfo *info = NULL;
  NspObject *nsp_info = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info, GI_TYPE_BASE_INFO))
      info = nspg_boxed_get(nsp_info, GIBaseInfo);
  else {
      Scierror( "Error: info should be a GIBaseInfo\n");
      return RET_BUG;
  }
    ret =g_base_info_get_namespace(info);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_base_info_is_deprecated(Stack stack, int rhs, int opt, int lhs) /* g_base_info_is_deprecated */
{
  int_types T[] = {obj, t_end};
  GIBaseInfo *info = NULL;
  NspObject *nsp_info = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info, GI_TYPE_BASE_INFO))
      info = nspg_boxed_get(nsp_info, GIBaseInfo);
  else {
      Scierror( "Error: info should be a GIBaseInfo\n");
      return RET_BUG;
  }
    ret =g_base_info_is_deprecated(info);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_base_info_get_attribute(Stack stack, int rhs, int opt, int lhs) /* g_base_info_get_attribute */
{
  int_types T[] = {obj,string, t_end};
  GIBaseInfo *info = NULL;
  NspObject *nsp_info = NULL;
  char *name;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info, &name) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info, GI_TYPE_BASE_INFO))
      info = nspg_boxed_get(nsp_info, GIBaseInfo);
  else {
      Scierror( "Error: info should be a GIBaseInfo\n");
      return RET_BUG;
  }
    ret =g_base_info_get_attribute(info,name);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_base_info_get_container(Stack stack, int rhs, int opt, int lhs) /* g_base_info_get_container */
{
  int_types T[] = {obj, t_end};
  GIBaseInfo *info = NULL, *ret;
  NspObject *nsp_info = NULL, *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info, GI_TYPE_BASE_INFO))
      info = nspg_boxed_get(nsp_info, GIBaseInfo);
  else {
      Scierror( "Error: info should be a GIBaseInfo\n");
      return RET_BUG;
  }
    ret =g_base_info_get_container(info);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GI_TYPE_BASE_INFO, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gibaseinfo))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_base_info_equal(Stack stack, int rhs, int opt, int lhs) /* g_base_info_equal */
{
  int_types T[] = {obj,obj, t_end};
  GIBaseInfo *info1 = NULL, *info2 = NULL;
  NspObject *nsp_info1 = NULL, *nsp_info2 = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info1, &nsp_info2) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info1, GI_TYPE_BASE_INFO))
      info1 = nspg_boxed_get(nsp_info1, GIBaseInfo);
  else {
      Scierror( "Error: info1 should be a GIBaseInfo\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_info2, GI_TYPE_BASE_INFO))
      info2 = nspg_boxed_get(nsp_info2, GIBaseInfo);
  else {
      Scierror( "Error: info2 should be a GIBaseInfo\n");
      return RET_BUG;
  }
    ret =g_base_info_equal(info1,info2);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_function_info_get_flags(Stack stack, int rhs, int opt, int lhs) /* g_function_info_get_flags */
{
  int_types T[] = {obj, t_end};
  GIFunctionInfo *info = NULL;
  NspObject *nsp_info = NULL;
  guint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_info) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_info, GI_TYPE_BASE_INFO))
      info = nspg_boxed_get(nsp_info, GIFunctionInfo);
  else {
      Scierror( "Error: info should be a GIFunctionInfo\n");
      return RET_BUG;
  }
    ret =g_function_info_get_flags(info);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_irepository_get_default(Stack stack, int rhs, int opt, int lhs) /* g_irepository_get_default */
{
  GIRepository *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_irepository_get_default();
  nsp_type_girepository = new_type_girepository(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_girepository))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_irepository_prepend_search_path(Stack stack, int rhs, int opt, int lhs) /* g_irepository_prepend_search_path */
{
  int_types T[] = {string, t_end};
  char *directory;
  if ( GetArgs(stack,rhs,opt,T,&directory) == FAIL) return RET_BUG;
    g_irepository_prepend_search_path(directory);
  return 0;
}

int _wrap_g_irepository_prepend_library_path(Stack stack, int rhs, int opt, int lhs) /* g_irepository_prepend_library_path */
{
  int_types T[] = {string, t_end};
  char *directory;
  if ( GetArgs(stack,rhs,opt,T,&directory) == FAIL) return RET_BUG;
    g_irepository_prepend_library_path(directory);
  return 0;
}

int _wrap_g_irepository_get_search_path(Stack stack, int rhs, int opt, int lhs) /* g_irepository_get_search_path */
{
  GSList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_irepository_get_search_path();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

}

int _wrap_g_irepository_dump(Stack stack, int rhs, int opt, int lhs) /* g_irepository_dump */
{
  int_types T[] = {string, t_end};
  char *arg;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&arg) == FAIL) return RET_BUG;
    ret =g_irepository_dump(arg,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_type_tag_to_string(Stack stack, int rhs, int opt, int lhs) /* g_type_tag_to_string */
{
  int_types T[] = {obj, t_end};
  GITypeTag type;
  NspObject *nsp_type = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_type, &type)== FAIL)
      return RET_BUG;
    ret =g_type_tag_to_string(type);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_info_type_to_string(Stack stack, int rhs, int opt, int lhs) /* g_info_type_to_string */
{
  int_types T[] = {obj, t_end};
  GIInfoType type;
  NspObject *nsp_type = NULL;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_NONE, nsp_type, &type)== FAIL)
      return RET_BUG;
    ret =g_info_type_to_string(type);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab girepository_func[]={
  { "g_base_info_ref", _wrap_g_base_info_ref},
  { "g_base_info_unref", _wrap_g_base_info_unref},
  { "g_base_info_get_name", _wrap_g_base_info_get_name},
  { "g_base_info_get_namespace", _wrap_g_base_info_get_namespace},
  { "g_base_info_is_deprecated", _wrap_g_base_info_is_deprecated},
  { "g_base_info_get_attribute", _wrap_g_base_info_get_attribute},
  { "g_base_info_get_container", _wrap_g_base_info_get_container},
  { "g_base_info_equal", _wrap_g_base_info_equal},
  { "g_function_info_get_flags", _wrap_g_function_info_get_flags},
  { "g_irepository_get_default", _wrap_g_irepository_get_default},
  { "g_irepository_prepend_search_path", _wrap_g_irepository_prepend_search_path},
  { "g_irepository_prepend_library_path", _wrap_g_irepository_prepend_library_path},
  { "g_irepository_get_search_path", _wrap_g_irepository_get_search_path},
  { "g_irepository_dump", _wrap_g_irepository_dump},
  { "g_type_tag_to_string", _wrap_g_type_tag_to_string},
  { "g_info_type_to_string", _wrap_g_info_type_to_string},
  { NULL, NULL}
};

/* call ith function in the girepository interface */

int girepository_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,girepository_func[i].fonc,
  					       &stack,rhs,opt,lhs);
#else
  return (*(girepository_func[i].fonc))(stack,rhs,opt,lhs);
#endif
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void girepository_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = girepository_func[i].name;
  *f = girepository_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
girepository_add_constants(NspObject *module, const gchar *strip_prefix)
{
/* enum or flags without typecode: GInvokeError */
/* enum or flags without typecode: GIRepositoryLoadFlags */
/* enum or flags without typecode: GIRepositoryError */
/* enum or flags without typecode: GIInfoType */
/* enum or flags without typecode: GITransfer */
/* enum or flags without typecode: GIDirection */
/* enum or flags without typecode: GIScopeType */
/* enum or flags without typecode: GITypeTag */
/* enum or flags without typecode: GIArrayType */
/* enum or flags without typecode: GIFieldInfoFlags */
/* enum or flags without typecode: GIVFuncInfoFlags */
/* enum or flags without typecode: GIFunctionInfoFlags */
}

void nsp_initialize_girepository_types(void)
{
  new_type_gibaseinfo(T_BASE);
  new_type_gifunctioninfo(T_BASE);
  new_type_girepository(T_BASE);
}

#line 151 "codegen-3.0/girepository.override"

static const char *nsp_gi_info_str (GIBaseInfo *info)
{
  GIInfoType info_type;
  info_type = g_base_info_get_type (info);
  return g_info_type_to_string (info_type);
}

static int nsp_gi_info_check(GIBaseInfo *info, GIInfoType info_type_check)
{
  GIInfoType info_type = g_base_info_get_type (info);
  if ( info_type != info_type_check )
    {
      Scierror("Error: expecting info type to be %s\n", g_info_type_to_string (info_type));
      return FAIL;
    }
  return OK;
}


#line 1304 "girepository.c"
