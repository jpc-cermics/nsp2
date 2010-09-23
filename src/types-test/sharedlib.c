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





#line 22 "codegen/sharedlib.override"
/* headers in C-file */

#line 31 "sharedlib.c"

/* ----------- NspSharedlib ----------- */


#define  NspSharedlib_Private 
#include <nsp/objects.h>
#include <nsp/sharedlib.h>
#include <nsp/interf.h>

/* 
 * NspSharedlib inherits from Object 
 */

int nsp_type_sharedlib_id=0;
NspTypeSharedlib *nsp_type_sharedlib=NULL;

/*
 * Type object for NspSharedlib 
 * all the instance of NspTypeSharedlib share the same id. 
 * nsp_type_sharedlib: is an instance of NspTypeSharedlib 
 *    used for objects of NspSharedlib type (i.e built with new_sharedlib) 
 * other instances are used for derived classes 
 */
NspTypeSharedlib *new_type_sharedlib(type_mode mode)
{
  NspTypeSharedlib *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_sharedlib != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_sharedlib;
    }
  if (( type =  malloc(sizeof(NspTypeSharedlib))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = sharedlib_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = sharedlib_get_methods;
  type->new = (new_func *) new_sharedlib;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for sharedlib */ 

  top->pr = (print_func *) nsp_sharedlib_print;
  top->dealloc = (dealloc_func *) nsp_sharedlib_destroy;
  top->copy  =  (copy_func *) nsp_sharedlib_copy;
  top->size  = (size_func *) nsp_sharedlib_size;
  top->s_type =  (s_type_func *) nsp_sharedlib_type_as_string;
  top->sh_type = (sh_type_func *) nsp_sharedlib_type_short_string;
  top->info = (info_func *) nsp_sharedlib_info;
  /* top->is_true = (is_true_func  *) nsp_sharedlib_is_true; */
  /* top->loop =(loop_func *) nsp_sharedlib_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_sharedlib_object;
  top->eq  = (eq_func *) nsp_sharedlib_eq;
  top->neq  = (eq_func *) nsp_sharedlib_neq;
  top->save  = (save_func *) nsp_sharedlib_xdr_save;
  top->load  = (load_func *) nsp_sharedlib_xdr_load;
  top->create = (create_func*) int_sharedlib_create;
  top->latex = (print_func *) nsp_sharedlib_latex;
  top->full_copy = (copy_func *) nsp_sharedlib_full_copy;

  /* specific methods for sharedlib */

  type->init = (init_func *) init_sharedlib;

#line 31 "codegen/sharedlib.override"
  /* inserted verbatim in the type definition */

#line 106 "sharedlib.c"
  /* 
   * NspSharedlib interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_sharedlib_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeSharedlib called nsp_type_sharedlib
       */
      type->id =  nsp_type_sharedlib_id = nsp_new_type_id();
      nsp_type_sharedlib = type;
      if ( nsp_register_type(nsp_type_sharedlib) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_sharedlib(mode);
    }
  else 
    {
      type->id = nsp_type_sharedlib_id;
      return type;
    }
}

/*
 * initialize NspSharedlib instances 
 * locally and by calling initializer on parent class 
 */

static int init_sharedlib(NspSharedlib *Obj,NspTypeSharedlib *type)
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
 * new instance of NspSharedlib 
 */

NspSharedlib *new_sharedlib() 
{
  NspSharedlib *loc;
  /* type must exists */
  nsp_type_sharedlib = new_type_sharedlib(T_BASE);
  if ( (loc = malloc(sizeof(NspSharedlib)))== NULLSHAREDLIB) return loc;
  /* initialize object */
  if ( init_sharedlib(loc,nsp_type_sharedlib) == FAIL) return NULLSHAREDLIB;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspSharedlib 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_sharedlib_size(NspSharedlib *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char sharedlib_type_name[]="Sharedlib";
static char sharedlib_short_type_name[]="sharedlib";

static char *nsp_sharedlib_type_as_string(void)
{
  return(sharedlib_type_name);
}

static char *nsp_sharedlib_type_short_string(NspObject *v)
{
  return(sharedlib_short_type_name);
}

/*
 * A == B 
 */

static int nsp_sharedlib_eq(NspSharedlib *A, NspObject *B)
{
  NspSharedlib *loc = (NspSharedlib *) B;
  if ( check_cast(B,nsp_type_sharedlib_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->shd != loc->obj->shd) return FALSE;
  if ( A->obj->id != loc->obj->id) return FALSE;
  if ( strcmp(A->obj->path,loc->obj->path) != 0) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_sharedlib_neq(NspSharedlib *A, NspObject *B)
{
  return ( nsp_sharedlib_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_sharedlib_xdr_save(XDR *xdrs, NspSharedlib *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_sharedlib)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspSharedlib  *nsp_sharedlib_xdr_load_partial(XDR *xdrs, NspSharedlib *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspSharedlib  *nsp_sharedlib_xdr_load(XDR *xdrs)
{
  NspSharedlib *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSHAREDLIB;
  if ((H  = nsp_sharedlib_create_void(name,(NspTypeBase *) nsp_type_sharedlib))== NULLSHAREDLIB) return H;
  if ( nsp_sharedlib_create_partial(H) == FAIL) return NULLSHAREDLIB;
  if ((H  = nsp_sharedlib_xdr_load_partial(xdrs,H))== NULLSHAREDLIB) return H;
  if ( nsp_sharedlib_check_values(H) == FAIL) return NULLSHAREDLIB;
  return H;
}

/*
 * delete 
 */

void nsp_sharedlib_destroy_partial(NspSharedlib *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  nsp_string_destroy(&(H->obj->path));
    FREE(H->obj);
   }
}

void nsp_sharedlib_destroy(NspSharedlib *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_sharedlib_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_sharedlib_info(NspSharedlib *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSHAREDLIB) 
    {
      Sciprintf("Null Pointer NspSharedlib \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_sharedlib_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_sharedlib_print(NspSharedlib *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSHAREDLIB) 
    {
      Sciprintf("Null Pointer NspSharedlib \n");
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
          nsp_sharedlib_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_sharedlib_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"shd=0x%x\n",M->obj->shd);
  Sciprintf1(indent+2,"id=%d\n",M->obj->id);
  Sciprintf1(indent+2,"path=%s\n",M->obj->path);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_sharedlib_latex(NspSharedlib *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_sharedlib_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"shd=0x%x\n",M->obj->shd);
  Sciprintf1(indent+2,"id=%d\n",M->obj->id);
  Sciprintf1(indent+2,"path=%s\n",M->obj->path);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspSharedlib objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspSharedlib   *nsp_sharedlib_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_sharedlib_id) == TRUE ) return ((NspSharedlib *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_sharedlib));
  return NULL;
}

int IsSharedlibObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_sharedlib_id);
}

int IsSharedlib(NspObject *O)
{
  return nsp_object_type(O,nsp_type_sharedlib_id);
}

NspSharedlib  *GetSharedlibCopy(Stack stack, int i)
{
  if (  GetSharedlib(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSharedlib  *GetSharedlib(Stack stack, int i)
{
  NspSharedlib *M;
  if (( M = nsp_sharedlib_object(NthObj(i))) == NULLSHAREDLIB)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspSharedlib instance 
 *-----------------------------------------------------*/

static NspSharedlib *nsp_sharedlib_create_void(const char *name,NspTypeBase *type)
{
 NspSharedlib *H  = (type == NULL) ? new_sharedlib() : type->new();
 if ( H ==  NULLSHAREDLIB)
  {
   Sciprintf("No more memory\n");
   return NULLSHAREDLIB;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSHAREDLIB;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_sharedlib_create_partial(NspSharedlib *H)
{
  if((H->obj = calloc(1,sizeof(nsp_sharedlib)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->shd = NULL;
  H->obj->id = -1;
  H->obj->path = nsp_new_string("NULL",-1);
  return OK;
}

int nsp_sharedlib_check_values(NspSharedlib *H)
{
  if ( H->obj->path == NULL) 
    {
     if (( H->obj->path = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  return OK;
}

NspSharedlib *nsp_sharedlib_create(const char *name,void* shd,int id,char* path,NspTypeBase *type)
{
  NspSharedlib *H  = nsp_sharedlib_create_void(name,type);
  if ( H ==  NULLSHAREDLIB) return NULLSHAREDLIB;
  if ( nsp_sharedlib_create_partial(H) == FAIL) return NULLSHAREDLIB;
  H->obj->shd = shd;
  H->obj->id=id;
  H->obj->path = path;
  if ( nsp_sharedlib_check_values(H) == FAIL) return NULLSHAREDLIB;
  return H;
}


NspSharedlib *nsp_sharedlib_create_default(const char *name)
{
 NspSharedlib *H  = nsp_sharedlib_create_void(name,NULL);
 if ( H ==  NULLSHAREDLIB) return NULLSHAREDLIB;
  if ( nsp_sharedlib_create_partial(H) == FAIL) return NULLSHAREDLIB;
 if ( nsp_sharedlib_check_values(H) == FAIL) return NULLSHAREDLIB;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspSharedlib *nsp_sharedlib_copy_partial(NspSharedlib *H,NspSharedlib *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspSharedlib *nsp_sharedlib_copy(NspSharedlib *self)
{
  NspSharedlib *H  =nsp_sharedlib_create_void(NVOID,(NspTypeBase *) nsp_type_sharedlib);
  if ( H ==  NULLSHAREDLIB) return NULLSHAREDLIB;
  if ( nsp_sharedlib_copy_partial(H,self)== NULL) return NULLSHAREDLIB;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspSharedlib *nsp_sharedlib_full_copy_partial(NspSharedlib *H,NspSharedlib *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_sharedlib))) == NULL) return NULLSHAREDLIB;
  H->obj->ref_count=1;
  H->obj->shd = self->obj->shd;
  H->obj->id=self->obj->id;
  if ((H->obj->path = nsp_string_copy(self->obj->path)) == NULL) return NULL;
  return H;
}

NspSharedlib *nsp_sharedlib_full_copy(NspSharedlib *self)
{
  NspSharedlib *H  =nsp_sharedlib_create_void(NVOID,(NspTypeBase *) nsp_type_sharedlib);
  if ( H ==  NULLSHAREDLIB) return NULLSHAREDLIB;
  if ( nsp_sharedlib_full_copy_partial(H,self)== NULL) return NULLSHAREDLIB;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspSharedlib
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_sharedlib_create(Stack stack, int rhs, int opt, int lhs)
{
  NspSharedlib *H;
  CheckStdRhs(0,0);
  /* want to be sure that type sharedlib is initialized */
  nsp_type_sharedlib = new_type_sharedlib(T_BASE);
  if(( H = nsp_sharedlib_create_void(NVOID,(NspTypeBase *) nsp_type_sharedlib)) == NULLSHAREDLIB) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_sharedlib_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_sharedlib_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *sharedlib_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab sharedlib_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Sharedlib_func[]={
  { "sharedlib_create", int_sharedlib_create},
  { NULL, NULL}
};

/* call ith function in the Sharedlib interface */

int Sharedlib_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Sharedlib_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Sharedlib_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Sharedlib_func[i].name;
  *f = Sharedlib_func[i].fonc;
}

#line 43 "codegen/sharedlib.override"
/* inserted verbatim at the end */

static NspHash *SharedLibs = NULL;

static int nsp_sharedlib_table_init(void)
{
  if ( SharedLibs != NULL ) return OK;
  if (( SharedLibs = nsp_hash_create("sharedlibs",256)) == NULL) 
    {
      return FAIL;
    }
  return OK;
}

NspSharedlib *nsp_sharedlib_table_insert( void *shd,unsigned int id, const  char *path)
{
  NspSharedlib *sh = NULL;
  char name[9]; /* size int +1 */
  char *path_c = NULL;
  snprintf(name,9, "%x", id );

  if ( SharedLibs == NULL ) 
    {
      if ( nsp_sharedlib_table_init() == FAIL) return NULL;
    }
  if (( path_c = nsp_string_copy(path)) == NULL) return NULL;
  if ((sh = nsp_sharedlib_create(name,shd,id,path_c, NULL))== NULL) 
    {
      nsp_string_destroy(&path_c);
      return NULL;
    }
  if ( nsp_hash_enter(SharedLibs,NSP_OBJECT(sh)) == FAIL ) 
    {
      nsp_sharedlib_destroy(sh);
      return NULL;
    }
  return sh;
}

NspSharedlib *nsp_sharedlib_table_find( int id) 
{
  NspObject *Obj;
  char name[9]; /* size int +1 */
  snprintf(name,9, "%x", id );
  if ( SharedLibs == NULL ) return NULL;
  if ( nsp_hash_find(SharedLibs,name,&Obj) == FAIL) 
    return NULL;
  return ( NspSharedlib *) Obj;
}

void nsp_sharedlib_table_remove_lib(int id)
{
  char name[9]; /* size int +1 */
  snprintf(name,9, "%x", id );
  if ( SharedLibs == NULL ) return;
  nsp_hash_remove(SharedLibs,name);
}

NspSharedlib *nsp_sharedlib_table_find_by_path(const char *name)
{
  NspObject *Obj;
  int i=0;
  if  ( SharedLibs == NULL ) return NULL;
  while (1) 
    {
      int rep = nsp_hash_get_next_object(SharedLibs,&i,&Obj);
      if ( Obj != NULLOBJ )
	{ 
	  NspSharedlib *sh = (NspSharedlib *) Obj;
	  if ( strcmp(name,sh->obj->path) == 0 ) 
	    {
	      return sh;
	    }
	}
      if ( rep == FAIL) break;
    }
  return NULL;
}

void nsp_sharedlib_table_show()
{
  if  ( SharedLibs == NULL ) return;
  nsp_hash_print(SharedLibs,0,0,0);
}

#line 627 "sharedlib.c"
