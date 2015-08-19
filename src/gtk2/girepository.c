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





#line 5 "codegen/girepository.override"
#include <nsp/gtk/gobject.h> 
#include <nsp/gtk/gobject-util.h>

#include <girepository.h>

#line 34 "girepository.c"
/* ---------- types from other modules ---------- */
#include <nsp/gtk/gobject.h>
/* ---------- forward type declarations ---------- */
#include <nsp/gtk/girepository.h>
#include <nsp/gibaseinfo.h>


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
#line 20 "codegen/girepository.override"

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

#line 247 "girepository.c"


#line 35 "codegen/girepository.override"

static int _wrap_g_irepository_find_by_name 
(NspGIRepository *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,t_end};
  char *namespace, *name;
  GIBaseInfo *info;
  if ( GetArgs(stack,rhs,opt,T,&namespace,&name) == FAIL) return RET_BUG;
  info = g_irepository_find_by_name (G_IREPOSITORY(self->obj), namespace, name);
  if (info == NULL) 
    {
      Sciprintf("Cannot fing %s\n",name);
    }
  /* 
  py_info = _pygi_info_new (info);
  g_base_info_unref (info);
  return py_info; */
  return 0;
}

#line 271 "girepository.c"


#line 57 "codegen/girepository.override"

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

#line 288 "girepository.c"


#line 86 "codegen/girepository.override"

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

#line 313 "girepository.c"


#line 109 "codegen/girepository.override"

void nsp_gi_info_new (GIBaseInfo *info);

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

#line 361 "girepository.c"


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


/* -----------NspGIBaseInfo ----------- */


#define  NspGIBaseInfo_Private 
#include <nsp/objects.h>
#include <nsp/gibaseinfo.h>
#include <nsp/interf.h>

/* 
 * NspGIBaseInfo inherits from Object 
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
  if (( type =  malloc(sizeof(NspTypeGIBaseInfo))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gibaseinfo_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gibaseinfo_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_gibaseinfo;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gibaseinfo */ 

  top->pr = (print_func *) nsp_gibaseinfo_print;
  top->dealloc = (dealloc_func *) nsp_gibaseinfo_destroy;
  top->copy  =  (copy_func *) nsp_gibaseinfo_copy;
  top->size  = (size_func *) nsp_gibaseinfo_size;
  top->s_type =  (s_type_func *) nsp_gibaseinfo_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gibaseinfo_type_short_string;
  top->info = (info_func *) nsp_gibaseinfo_info;
  /* top->is_true = (is_true_func  *) nsp_gibaseinfo_is_true; */
  /* top->loop =(loop_func *) nsp_gibaseinfo_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gibaseinfo_object;
  top->eq  = (eq_func *) nsp_gibaseinfo_eq;
  top->neq  = (eq_func *) nsp_gibaseinfo_neq;
  top->save  = (save_func *) nsp_gibaseinfo_xdr_save;
  top->load  = (load_func *) nsp_gibaseinfo_xdr_load;
  top->create = (create_func*) int_gibaseinfo_create;
  top->latex = (print_func *) nsp_gibaseinfo_latex;
  top->full_copy = (copy_func *) nsp_gibaseinfo_full_copy;

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
  Obj->obj = NULL;
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
 * size 
 */

static int nsp_gibaseinfo_size(NspGIBaseInfo *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char gibaseinfo_type_name[]="GIBaseInfo";
static char gibaseinfo_short_type_name[]="gibaseinfo";

static char *nsp_gibaseinfo_type_as_string(void)
{
  return(gibaseinfo_type_name);
}

static char *nsp_gibaseinfo_type_short_string(NspObject *v)
{
  return(gibaseinfo_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gibaseinfo_eq(NspGIBaseInfo *A, NspObject *B)
{
  NspGIBaseInfo *loc = (NspGIBaseInfo *) B;
  if ( check_cast(B,nsp_type_gibaseinfo_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->info != loc->obj->info) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_gibaseinfo_neq(NspGIBaseInfo *A, NspObject *B)
{
  return ( nsp_gibaseinfo_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gibaseinfo_xdr_save(XDR *xdrs, NspGIBaseInfo *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gibaseinfo)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGIBaseInfo  *nsp_gibaseinfo_xdr_load_partial(XDR *xdrs, NspGIBaseInfo *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspGIBaseInfo  *nsp_gibaseinfo_xdr_load(XDR *xdrs)
{
  NspGIBaseInfo *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGIBASEINFO;
  if ((H  = nsp_gibaseinfo_create_void(name,(NspTypeBase *) nsp_type_gibaseinfo))== NULLGIBASEINFO) return H;
  if ( nsp_gibaseinfo_create_partial(H) == FAIL) return NULLGIBASEINFO;
  if ((H  = nsp_gibaseinfo_xdr_load_partial(xdrs,H))== NULLGIBASEINFO) return H;
  if ( nsp_gibaseinfo_check_values(H) == FAIL) return NULLGIBASEINFO;
  return H;
}

/*
 * delete 
 */

void nsp_gibaseinfo_destroy_partial(NspGIBaseInfo *H)
{
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
}

void nsp_gibaseinfo_destroy(NspGIBaseInfo *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gibaseinfo_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gibaseinfo_info(NspGIBaseInfo *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGIBASEINFO) 
    {
      Sciprintf("Null Pointer NspGIBaseInfo \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gibaseinfo_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gibaseinfo_print(NspGIBaseInfo *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGIBASEINFO) 
    {
      Sciprintf("Null Pointer NspGIBaseInfo \n");
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
          nsp_gibaseinfo_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_gibaseinfo_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"info=0x%x\n", M->obj->info);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gibaseinfo_latex(NspGIBaseInfo *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gibaseinfo_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"info=0x%x\n", M->obj->info);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGIBaseInfo objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGIBaseInfo   *nsp_gibaseinfo_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
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

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGIBaseInfo instance 
 *-----------------------------------------------------*/

static NspGIBaseInfo *nsp_gibaseinfo_create_void(const char *name,NspTypeBase *type)
{
 NspGIBaseInfo *H  = (type == NULL) ? new_gibaseinfo() : type->new();
 if ( H ==  NULLGIBASEINFO)
  {
   Sciprintf("No more memory\n");
   return NULLGIBASEINFO;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGIBASEINFO;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gibaseinfo_create_partial(NspGIBaseInfo *H)
{
  if((H->obj = calloc(1,sizeof(nsp_gibaseinfo)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->info = NULL;
  return OK;
}

int nsp_gibaseinfo_check_values(NspGIBaseInfo *H)
{
  return OK;
}

NspGIBaseInfo *nsp_gibaseinfo_create(const char *name,void* info,NspTypeBase *type)
{
  NspGIBaseInfo *H  = nsp_gibaseinfo_create_void(name,type);
  if ( H ==  NULLGIBASEINFO) return NULLGIBASEINFO;
  if ( nsp_gibaseinfo_create_partial(H) == FAIL) return NULLGIBASEINFO;
  H->obj->info = info;
  if ( nsp_gibaseinfo_check_values(H) == FAIL) return NULLGIBASEINFO;
  return H;
}


NspGIBaseInfo *nsp_gibaseinfo_create_default(const char *name)
{
 NspGIBaseInfo *H  = nsp_gibaseinfo_create_void(name,NULL);
 if ( H ==  NULLGIBASEINFO) return NULLGIBASEINFO;
  if ( nsp_gibaseinfo_create_partial(H) == FAIL) return NULLGIBASEINFO;
  if ( nsp_gibaseinfo_check_values(H) == FAIL) return NULLGIBASEINFO;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGIBaseInfo *nsp_gibaseinfo_copy_partial(NspGIBaseInfo *H,NspGIBaseInfo *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGIBaseInfo *nsp_gibaseinfo_copy(NspGIBaseInfo *self)
{
  NspGIBaseInfo *H  =nsp_gibaseinfo_create_void(NVOID,(NspTypeBase *) nsp_type_gibaseinfo);
  if ( H ==  NULLGIBASEINFO) return NULLGIBASEINFO;
  if ( nsp_gibaseinfo_copy_partial(H,self)== NULL) return NULLGIBASEINFO;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGIBaseInfo *nsp_gibaseinfo_full_copy_partial(NspGIBaseInfo *H,NspGIBaseInfo *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_gibaseinfo))) == NULL) return NULLGIBASEINFO;
  H->obj->ref_count=1;
  H->obj->info = self->obj->info;
  return H;
}

NspGIBaseInfo *nsp_gibaseinfo_full_copy(NspGIBaseInfo *self)
{
  NspGIBaseInfo *H  =nsp_gibaseinfo_create_void(NVOID,(NspTypeBase *) nsp_type_gibaseinfo);
  if ( H ==  NULLGIBASEINFO) return NULLGIBASEINFO;
  if ( nsp_gibaseinfo_full_copy_partial(H,self)== NULL) return NULLGIBASEINFO;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGIBaseInfo
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gibaseinfo_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGIBaseInfo *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gibaseinfo is initialized */
  nsp_type_gibaseinfo = new_type_gibaseinfo(T_BASE);
  if(( H = nsp_gibaseinfo_create_void(NVOID,(NspTypeBase *) nsp_type_gibaseinfo)) == NULLGIBASEINFO) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_gibaseinfo_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gibaseinfo_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gibaseinfo_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gibaseinfo_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

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
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_girepository))== NULL) return RET_BUG;
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
  new_type_girepository(T_BASE);
  new_type_gibaseinfo(T_BASE);
}

#line 155 "codegen/girepository.override"

void nsp_gi_info_new (GIBaseInfo *info)
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


/* static NspObject * */
/* _wrap_g_irepository_get_typelib_path (NspGIRepository *self, */
/*                                       NspObject       *args, */
/*                                       NspObject       *kwargs) */
/* { */
/*     static char *kwlist[] = { "namespace", NULL }; */
/*     const char *namespace_; */
/*     const gchar *typelib_path; */

/*     if (!NspArg_ParseTupleAndKeywords (args, kwargs, */
/*                                       "s:Repository.get_typelib_path", kwlist, &namespace_)) { */
/*         return NULL; */
/*     } */

/*     typelib_path = g_irepository_get_typelib_path (self->repository, namespace_); */
/*     if (typelib_path == NULL) { */
/*         NspErr_Format (NspExc_RuntimeError, "Namespace '%s' not loaded", namespace_); */
/*         return NULL; */
/*     } */

/*     return PYGLIB_NspBytes_FromString (typelib_path); */
/* } */


#line 984 "girepository.c"
