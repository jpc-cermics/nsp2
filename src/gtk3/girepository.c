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

#line 38 "girepository.c"
/* ---------- types from other modules ---------- */
#include <nsp/gtk/gobject.h>
/* ---------- forward type declarations ---------- */
#include <nsp/gtk/gibaseinfo.h>
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
static int _wrap_g_base_info_get_attribute(NspGIBaseInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
  ret =g_base_info_get_attribute(NSP_GBOXED_GET(self, GIBaseInfo),name);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#line 139 "codegen-3.0/girepository.override"


static int _wrap_g_base_info_get_attributes(NspGIBaseInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  GIAttributeIter iter = { 0, };
  char *name;
  char *value;
  CheckRhs(0,0);
  while (g_base_info_iterate_attributes (NSP_GBOXED_GET(self, GIBaseInfo), &iter, &name, &value))
    {
      Sciprintf("attribute name: %s value: %s\n", name, value);
    }
  return 0;
}

#line 264 "girepository.c"


static NspMethods gibaseinfo_methods[] = {
  {"get_attribute",(nsp_method *) _wrap_g_base_info_get_attribute},
  {"get_attributes",(nsp_method *) _wrap_g_base_info_get_attributes},
  { NULL, NULL}
};

static NspMethods *gibaseinfo_get_methods(void) { return gibaseinfo_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gibaseinfo_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


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
#line 26 "codegen-3.0/girepository.override"

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

#line 486 "girepository.c"


static int _wrap_g_irepository_find_by_name(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *namespace, *name;
  GIBaseInfo *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&namespace, &name) == FAIL) return RET_BUG;
    ret =g_irepository_find_by_name(G_IREPOSITORY(self->obj),namespace,name);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GI_TYPE_BASE_INFO, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gibaseinfo))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 41 "codegen-3.0/girepository.override"

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

#line 518 "girepository.c"


#line 70 "codegen-3.0/girepository.override"

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

#line 543 "girepository.c"


#line 93 "codegen-3.0/girepository.override"

static void nsp_gi_info_new (GIBaseInfo *info);

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
	  nsp_gi_info_new (info);
	}
    }
  return 0;
}

/*
 * g_base_info_get_namespace (self->info);
 * GIBaseInfo *info = g_base_info_get_container (self->info);
 * g_base_info_get_namespace(self->info);
 */

#line 591 "girepository.c"


static NspMethods girepository_methods[] = {
  {"enumerate_versions",(nsp_method *) _wrap_g_irepository_enumerate_versions},
  {"find_by_name",(nsp_method *) _wrap_g_irepository_find_by_name},
  {"get_loaded_namespaces",(nsp_method *) _wrap_g_irepository_get_loaded_namespaces},
  {"require",(nsp_method *) _wrap_g_irepository_require},
  {"get_n_infos",(nsp_method *) _wrap_g_irepository_get_n_infos},
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

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab girepository_func[]={
  { "g_irepository_get_default", _wrap_g_irepository_get_default},
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
void nsp_initialize_girepository_types(void)
{
  new_type_gibaseinfo(T_BASE);
  new_type_girepository(T_BASE);
}

#line 156 "codegen-3.0/girepository.override"

static void nsp_gi_info_new (GIBaseInfo *info)
{
  GIInfoType info_type;
  info_type = g_base_info_get_type (info);
  switch (info_type)
    {
    case GI_INFO_TYPE_INVALID:
      Scierror("Invalid info type\n");
      break;
    case GI_INFO_TYPE_FUNCTION:
      Sciprintf("  type = &PyGIFunctionInfo_Type;\n");
      break;
    case GI_INFO_TYPE_CALLBACK:
      Sciprintf("  type = &PyGICallbackInfo_Type;\n");
      break;
    case GI_INFO_TYPE_STRUCT:
      Sciprintf("  type = &PyGIStructInfo_Type;\n");
      break;
    case GI_INFO_TYPE_BOXED:
      Sciprintf("  type = &PyGIBoxedInfo_Type;\n");
      break;
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
      Sciprintf("  type = &PyGIEnumInfo_Type;\n");
      break;
    case GI_INFO_TYPE_OBJECT:
      Sciprintf("  type = &PyGIObjectInfo_Type;\n");
      break;
    case GI_INFO_TYPE_INTERFACE:
      Sciprintf("  type = &PyGIInterfaceInfo_Type;\n");
      break;
    case GI_INFO_TYPE_CONSTANT:
      Sciprintf("  type = &PyGIConstantInfo_Type;\n");
      break;
    case GI_INFO_TYPE_UNION:
      Sciprintf("  type = &PyGIUnionInfo_Type;\n");
      break;
    case GI_INFO_TYPE_VALUE:
      Sciprintf("  type = &PyGIValueInfo_Type;\n");
      break;
    case GI_INFO_TYPE_SIGNAL:
      Sciprintf("  type = &PyGISignalInfo_Type;\n");
      break;
    case GI_INFO_TYPE_VFUNC:
      Sciprintf("  type = &PyGIVFuncInfo_Type;\n");
      break;
    case GI_INFO_TYPE_PROPERTY:
      Sciprintf("  type = &PyGIPropertyInfo_Type;\n");
      break;
    case GI_INFO_TYPE_FIELD:
      Sciprintf("  type = &PyGIFieldInfo_Type;\n");
      break;
    case GI_INFO_TYPE_ARG:
      Sciprintf("  type = &PyGIArgInfo_Type;\n");
      break;
    case GI_INFO_TYPE_TYPE:
      Sciprintf("  type = &PyGITypeInfo_Type;\n");
      break;
    case GI_INFO_TYPE_UNRESOLVED:
      Sciprintf("  type = &PyGIUnresolvedInfo_Type;\n");
      break;
    default:
      g_assert_not_reached();
      break;
    }
}



#line 732 "girepository.c"
