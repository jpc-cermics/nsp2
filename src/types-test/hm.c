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





#line 47 "codegen/hm.override"
#include <nsp/cnumeric.h> 

#line 31 "hm.c"

/* ----------- NspHm ----------- */


#define  NspHm_Private 
#include <nsp/objects.h>
#include <nsp/hm.h>
#include <nsp/interf.h>

/* 
 * NspHm inherits from Object 
 */

int nsp_type_hm_id=0;
NspTypeHm *nsp_type_hm=NULL;

/*
 * Type object for NspHm 
 * all the instance of NspTypeHm share the same id. 
 * nsp_type_hm: is an instance of NspTypeHm 
 *    used for objects of NspHm type (i.e built with new_hm) 
 * other instances are used for derived classes 
 */
NspTypeHm *new_type_hm(type_mode mode)
{
  NspTypeHm *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_hm != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_hm;
    }
  if (( type =  malloc(sizeof(NspTypeHm))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = hm_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = hm_get_methods;
  type->new = (new_func *) new_hm;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for hm */ 

  top->pr = (print_func *) nsp_hm_print;
  top->dealloc = (dealloc_func *) nsp_hm_destroy;
  top->copy  =  (copy_func *) nsp_hm_copy;
  top->size  = (size_func *) nsp_hm_size;
  top->s_type =  (s_type_func *) nsp_hm_type_as_string;
  top->sh_type = (sh_type_func *) nsp_hm_type_short_string;
  top->info = (info_func *) nsp_hm_info;
  /* top->is_true = (is_true_func  *) nsp_hm_is_true; */
  /* top->loop =(loop_func *) nsp_hm_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_hm_object;
  top->eq  = (eq_func *) nsp_hm_eq;
  top->neq  = (eq_func *) nsp_hm_neq;
  top->save  = (save_func *) nsp_hm_xdr_save;
  top->load  = (load_func *) nsp_hm_xdr_load;
  top->create = (create_func*) int_hm_create;
  top->latex = (print_func *) nsp_hm_latex;
  top->full_copy = (copy_func *) nsp_hm_full_copy;

  /* specific methods for hm */

  type->init = (init_func *) init_hm;

  /* 
   * NspHm interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_hm_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeHm called nsp_type_hm
       */
      type->id =  nsp_type_hm_id = nsp_new_type_id();
      nsp_type_hm = type;
      if ( nsp_register_type(nsp_type_hm) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_hm(mode);
    }
  else 
    {
      type->id = nsp_type_hm_id;
      return type;
    }
}

/*
 * initialize NspHm instances 
 * locally and by calling initializer on parent class 
 */

static int init_hm(NspHm *Obj,NspTypeHm *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->htable = NULL;
  Obj->hsize = 0;
  Obj->filled = 0;
  Obj->base = 4;
  Obj->keysize = 16;
  return OK;
}

/*
 * new instance of NspHm 
 */

NspHm *new_hm() 
{
  NspHm *loc;
  /* type must exists */
  nsp_type_hm = new_type_hm(T_BASE);
  if ( (loc = malloc(sizeof(NspHm)))== NULLHM) return loc;
  /* initialize object */
  if ( init_hm(loc,nsp_type_hm) == FAIL) return NULLHM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspHm 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_hm_size(NspHm *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char hm_type_name[]="Hm";
static char hm_short_type_name[]="hm";

static char *nsp_hm_type_as_string(void)
{
  return(hm_type_name);
}

static char *nsp_hm_type_short_string(NspObject *v)
{
  return(hm_short_type_name);
}

/*
 * A == B 
 */

static int nsp_hm_eq(NspHm *A, NspObject *B)
{
  NspHm *loc = (NspHm *) B;
  if ( check_cast(B,nsp_type_hm_id) == FALSE) return FALSE ;
  if ( A->htable != loc->htable) return FALSE;
  if ( A->hsize != loc->hsize) return FALSE;
  if ( A->filled != loc->filled) return FALSE;
  if ( A->base != loc->base) return FALSE;
  if ( A->keysize != loc->keysize) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_hm_neq(NspHm *A, NspObject *B)
{
  return ( nsp_hm_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_hm_xdr_save(XDR *xdrs, NspHm *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_hm)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->base) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->keysize) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspHm  *nsp_hm_xdr_load_partial(XDR *xdrs, NspHm *M)
{
  if (nsp_xdr_load_i(xdrs, &M->base) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->keysize) == FAIL) return NULL;
 return M;
}

static NspHm  *nsp_hm_xdr_load(XDR *xdrs)
{
  NspHm *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLHM;
  if ((H  = nsp_hm_create_void(name,(NspTypeBase *) nsp_type_hm))== NULLHM) return H;
  if ( nsp_hm_create_partial(H) == FAIL) return NULLHM;
  if ((H  = nsp_hm_xdr_load_partial(xdrs,H))== NULLHM) return H;
  if ( nsp_hm_check_values(H) == FAIL) return NULLHM;
#line 64 "codegen/hm.override"
  /* verbatim in create interface  */

#line 255 "hm.c"
  return H;
}

/*
 * delete 
 */

void nsp_hm_destroy_partial(NspHm *H)
{
#line 68 "codegen/hm.override"
  /* verbatim in destroy */

#line 268 "hm.c"
}

void nsp_hm_destroy(NspHm *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_hm_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_hm_info(NspHm *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLHM) 
    {
      Sciprintf("Null Pointer NspHm \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_hm_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_hm_print(NspHm *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLHM) 
    {
      Sciprintf("Null Pointer NspHm \n");
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
          nsp_hm_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_hm_type_short_string(NSP_OBJECT(M)) );
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"htable=0x%x\n",M->htable);
  Sciprintf1(indent+2,"hsize=%d\n",M->hsize);
  Sciprintf1(indent+2,"filled=%d\n",M->filled);
  Sciprintf1(indent+2,"base=%d\n",M->base);
  Sciprintf1(indent+2,"keysize=%d\n",M->keysize);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_hm_latex(NspHm *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_hm_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"htable=0x%x\n",M->htable);
  Sciprintf1(indent+2,"hsize=%d\n",M->hsize);
  Sciprintf1(indent+2,"filled=%d\n",M->filled);
  Sciprintf1(indent+2,"base=%d\n",M->base);
  Sciprintf1(indent+2,"keysize=%d\n",M->keysize);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspHm objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspHm   *nsp_hm_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_hm_id) == TRUE ) return ((NspHm *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_hm));
  return NULL;
}

int IsHmObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_hm_id);
}

int IsHm(NspObject *O)
{
  return nsp_object_type(O,nsp_type_hm_id);
}

NspHm  *GetHmCopy(Stack stack, int i)
{
  if (  GetHm(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspHm  *GetHm(Stack stack, int i)
{
  NspHm *M;
  if (( M = nsp_hm_object(NthObj(i))) == NULLHM)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspHm instance 
 *-----------------------------------------------------*/

static NspHm *nsp_hm_create_void(const char *name,NspTypeBase *type)
{
 NspHm *H  = (type == NULL) ? new_hm() : type->new();
 if ( H ==  NULLHM)
  {
   Sciprintf("No more memory\n");
   return NULLHM;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLHM;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_hm_create_partial(NspHm *H)
{
  return OK;
}

int nsp_hm_check_values(NspHm *H)
{
  return OK;
}

NspHm *nsp_hm_create(const char *name,void* htable,int hsize,int filled,int base,int keysize,NspTypeBase *type)
{
  NspHm *H  = nsp_hm_create_void(name,type);
  if ( H ==  NULLHM) return NULLHM;
  H->htable = htable;
  H->hsize=hsize;
  H->filled=filled;
  H->base=base;
  H->keysize=keysize;
  if ( nsp_hm_check_values(H) == FAIL) return NULLHM;
#line 64 "codegen/hm.override"
  /* verbatim in create interface  */

#line 433 "hm.c"
  return H;
}


NspHm *nsp_hm_create_default(const char *name)
{
 NspHm *H  = nsp_hm_create_void(name,NULL);
 if ( H ==  NULLHM) return NULLHM;
 if ( nsp_hm_check_values(H) == FAIL) return NULLHM;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspHm *nsp_hm_copy_partial(NspHm *H,NspHm *self)
{
  H->htable = self->htable;
  H->hsize=self->hsize;
  H->filled=self->filled;
  H->base=self->base;
  H->keysize=self->keysize;
  return H;
}

NspHm *nsp_hm_copy(NspHm *self)
{
  NspHm *H  =nsp_hm_create_void(NVOID,(NspTypeBase *) nsp_type_hm);
  if ( H ==  NULLHM) return NULLHM;
  if ( nsp_hm_copy_partial(H,self)== NULL) return NULLHM;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspHm *nsp_hm_full_copy_partial(NspHm *H,NspHm *self)
{
  H->htable = self->htable;
  H->hsize=self->hsize;
  H->filled=self->filled;
  H->base=self->base;
  H->keysize=self->keysize;
  return H;
}

NspHm *nsp_hm_full_copy(NspHm *self)
{
  NspHm *H  =nsp_hm_create_void(NVOID,(NspTypeBase *) nsp_type_hm);
  if ( H ==  NULLHM) return NULLHM;
  if ( nsp_hm_full_copy_partial(H,self)== NULL) return NULLHM;

#line 64 "codegen/hm.override"
  /* verbatim in create interface  */

#line 491 "hm.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspHm
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

#line 72 "codegen/hm.override"
/* override the default int_create */

int int_hm_create(Stack stack, int rhs, int opt, int lhs)
{
  nsp_option opts[] ={{ "base",s_int,NULLOBJ,-1},
		      { "keysize",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int size=10, base = 4, keysize = 16;
  NspHm *H;
  CheckStdRhs(0,1);
  CheckLhs(0,1);
  if ( rhs == 1) 
    {
      if (GetScalarInt(stack,1,&size) == FAIL) return RET_BUG;           
      size= Max(1,size);
    }
  if ( get_optional_args(stack, rhs, opt, opts, &base, &keysize) == FAIL )
    return RET_BUG;
  if(( H = nsp_bhcreate(NVOID,size)) == NULLHM) return RET_BUG;
  H->base = Max(base,2);
  H->keysize = Max(keysize,1);
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
}

#line 526 "hm.c"
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 99 "codegen/hm.override"

static int _wrap_nsp_hm_remove(NspHm *self,Stack stack,int rhs,int opt,int lhs)
{
  gint64 ikey;
  int_types T[] = {realmat,t_end};
  NspMatrix *key;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
  if ( nsp_hm_compute_key(self,key, &ikey)== FAIL) return RET_BUG;
  nsp_hm_remove(self, ikey);
  return 0;
}

#line 543 "hm.c"


#line 113 "codegen/hm.override"

static int _wrap_nsp_hm_enter(NspHm *self,Stack stack,int rhs,int opt,int lhs)
{
  gint64 ikey;
  double val;
  int_types T[] = {realmat, s_double,t_end};
  NspMatrix *key;
  if ( GetArgs(stack,rhs,opt,T,&key,&val) == FAIL) return RET_BUG;
  if ( nsp_hm_compute_key(self,key, &ikey)== FAIL) return RET_BUG;
  nsp_hm_enter(self, ikey,val);
  return 0;
}

#line 560 "hm.c"


#line 128 "codegen/hm.override"

static int _wrap_nsp_hm_find(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *S;
  int j,count=0;
  NspHm *H=self;
  NspObject *O;
  double val; 
  CheckRhs(1,1000);
  CheckLhs(1,1000);
  lhs=Max(lhs,1);
  for ( j = 1 ; j <= rhs ; j++ )
    {
      gint64 ikey;
      if ((S = GetRealMat(stack,j)) == NULLMAT) return RET_BUG;        
      if ( nsp_hm_compute_key(H,S, &ikey)== FAIL) return RET_BUG;
      if (nsp_hm_find(H,ikey,&val) == FAIL)   
	{
	  Scierror("%s: key %d not found in bhash table \n",NspFname(stack),ikey);
	  return RET_BUG  ;
	}
      else
	{
	  if ((O=nsp_create_object_from_double(NVOID,val))==NULLOBJ) return RET_BUG;    
	  NthObj(rhs+ ++count) = O ;
	  NSP_OBJECT(O)->ret_pos = count;
	}
      if (count == lhs) break;
    }
  return count;
}
#line 595 "hm.c"


#line 161 "codegen/hm.override"

static int _wrap_nsp_hm_iskey(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *S;
  int j;
  NspHm *H=self;
  double val;
  NspBMatrix *Res;
  CheckRhs(1,1000);
  CheckLhs(0,1);
  lhs=Max(lhs,1);
  if ((Res = nsp_bmatrix_create(NVOID,1,rhs))== NULLBMAT) return RET_BUG;
  for ( j = 1 ; j <= rhs ; j++ )
    {
      gint64 ikey;
      if ((S = GetRealMat(stack,j)) == NULLMAT) return RET_BUG;        
      if ( nsp_hm_compute_key(H,S, &ikey)== FAIL) return RET_BUG;
      Res->B[j-1]= (nsp_hm_find(H,ikey,&val) == FAIL)? FALSE:TRUE;
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}
#line 621 "hm.c"


#line 199 "codegen/hm.override"

static int _wrap_nsp_hm_key2m(NspHm *self,Stack stack,int rhs,int opt,int lhs)
{
  double dkey;
  int_types T[] = {s_double,t_end};
  NspMatrix *ret;
  gint64 key;
  if ( GetArgs(stack,rhs,opt,T,&dkey) == FAIL) return RET_BUG;
  key = dkey;
  ret = nsp_hm_key2m(self, key);
  if ( ret == NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}
#line 639 "hm.c"


#line 185 "codegen/hm.override"

static int _wrap_nsp_hm_m2key(void *self,Stack stack, int rhs, int opt, int lhs)
{
  gint64 ikey;
  int_types T[] = {realmat,t_end};
  NspMatrix *key;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
  if ( nsp_hm_compute_key(self,key, &ikey)== FAIL) return RET_BUG;
  if ( nsp_move_double(stack,1,ikey) == FAIL) return RET_BUG;
  return 1;
}

#line 655 "hm.c"


static int _wrap_nsp_hm_check_slope(NspHm *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {mat,t_end};
  int ret;
  NspMatrix *M;

  if ( GetArgs(stack,rhs,opt,T,&M) == FAIL) return RET_BUG;
/* M << 1 */
  ret = nsp_hm_check_slope(self, M);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#line 215 "codegen/hm.override"

static int _wrap_nsp_hm_get_keys(NspHm *self,Stack stack,int rhs,int opt,int lhs)
{
  NspIMatrix *ret;
  if ((ret = nsp_hm_get_keys(NVOID,self)) == NULL) 
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 682 "hm.c"


static NspMethods hm_methods[] = {
  {"delete",(nsp_method *) _wrap_nsp_hm_remove},
  {"enter",(nsp_method *) _wrap_nsp_hm_enter},
  {"find",(nsp_method *) _wrap_nsp_hm_find},
  {"iskey",(nsp_method *) _wrap_nsp_hm_iskey},
  {"key2m",(nsp_method *) _wrap_nsp_hm_key2m},
  {"m2key",(nsp_method *) _wrap_nsp_hm_m2key},
  {"check_slope",(nsp_method *) _wrap_nsp_hm_check_slope},
  {"get_keys",(nsp_method *) _wrap_nsp_hm_get_keys},
  { NULL, NULL}
};

static NspMethods *hm_get_methods(void) { return hm_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_hm_get_base(void *self,const char *attr)
{
  int ret;

  ret = ((NspHm *) self)->base;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_hm_set_base(void *self,const char *attr, NspObject *O)
{
  int base;

  if ( IntScalar(O,&base) == FAIL) return FAIL;
  ((NspHm *) self)->base= base;
  return OK;
}

static NspObject *_wrap_hm_get_keysize(void *self,const char *attr)
{
  int ret;

  ret = ((NspHm *) self)->keysize;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_hm_set_keysize(void *self,const char *attr, NspObject *O)
{
  int keysize;

  if ( IntScalar(O,&keysize) == FAIL) return FAIL;
  ((NspHm *) self)->keysize= keysize;
  return OK;
}

static AttrTab hm_attrs[] = {
  { "base", (attr_get_function *)_wrap_hm_get_base, (attr_set_function *)_wrap_hm_set_base,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "keysize", (attr_get_function *)_wrap_hm_get_keysize, (attr_set_function *)_wrap_hm_set_keysize,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab hm_func[]={
  { "hm_create", int_hm_create},
  { NULL, NULL}
};

/* call ith function in the hm interface */

int hm_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(hm_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void hm_Interf_Info(int i, char **fname, function (**f))
{
  *fname = hm_func[i].name;
  *f = hm_func[i].fonc;
}

#line 227 "codegen/hm.override"


/* The bhash table code at the end of the file is Copyrighted
 * See Copyright (c) 1990, 1993 The Regents of the University of California.
 * in the file. 
 */

/*
 * FIXME : check increasing size criteria 
 */

/*
 * HMTable Object in Scilab : 
 *    store (key,value) in a bhash table where key is always a string 
 *    and value is a pointer to a Scilab Object Obj
 */

/**
 *nsp_hm_resize:
 * @H: 
 * @new_size: 
 * 
 * Resizes a  HMTable
 *
 * Return value: %OK or %FAIL
 **/

int nsp_hm_resize(NspHm *H, unsigned int new_size)
{
  unsigned int i;
  NspHm *Loc;
  if ((Loc = nsp_bhcreate(NVOID,new_size)) == NULLHM ) return FAIL;
  for ( i =0 ; i <= H->hsize ; i++) 
    {
      HM_Entry *loc = ((HM_Entry *)H->htable) + i;
      if ( loc->used && loc->key != no_key )
	{
	  if ( nsp_bhsearch(Loc,loc->key,&loc->val,HM_ENTER) == FAIL) 
	    return FAIL;
	}
    }
  FREE(H->htable);
  H->htable = Loc->htable;
  H->hsize  = Loc->hsize;
  H->filled = Loc->filled;
  nsp_object_destroy_name(NSP_OBJECT(Loc));
  FREE(Loc);
  return OK;
}

/**
 *nsp_hm_merge:
 * @H1: 
 * @H2: 
 * 
 * Insert Copies of elements of has table H2 in hm table H1
 * 
 * Return value: %OK or %FAIL 
 **/

int nsp_hm_merge(NspHm *H1,NspHm *H2)
{
  unsigned int i;
  if ( H2 == NULLHM ||  H2->filled == 0 ) return OK;
  if ( (i=H1->filled + H2->filled) >= 2*(H1->hsize/3) ) 
    {
      if (nsp_hm_resize(H1,2*i) == FAIL ) 
	{
	  Scierror("Error: running out of memory");
	  return FAIL;
	}
    }
  for ( i =0 ; i <= H2->hsize ; i++) 
    {
      HM_Entry *loc = ((HM_Entry *)H2->htable) + i;
      if ( loc->used && loc->key != no_key )
	{
	  if ( nsp_bhsearch(H1,loc->key,&loc->val,HM_ENTER)  == FAIL) 
	    return FAIL;
	}
    }
  return OK;
}

/**
 *nsp_hm_get_next_object:
 * @H: 
 * @i: 
 * @str:
 * @val: 
 * 
 * Used to walk through all the elements of the hm table 
 * return %FAIL when the end of the hm table is reached
 * and nsp_hm_get_next_object() is not to be called again 
 * The values present in the HM table are returned 
 * in sequence (note that the key value is stored in the object) 
 * @i is incremented at each call. 
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_hm_get_next_object(NspHm *H, int *i,gint64 *key,double *val)
{
  HM_Entry *loc = ((HM_Entry *) H->htable) + *i;  
  if ( loc->used && loc->key != no_key ) 
    {
      *val = loc->val ;
      *key = loc->key;
    }

  else
    {
      *val = 0;
      *key = no_key;
    }
  (*i)++;
  return ( (*i) >= (int) H->hsize +1   ) ? FAIL: OK ;
}



/**
 * nsp_hm_enter_pos_i:
 * @H: 
 * @i: 
 * @val: 
 * 
 * enter a value in the hash table entry given by its indice.
 **/

void nsp_hm_enter_pos_i(NspHm *H, int i,int val)
{
  HM_Entry *loc = ((HM_Entry *) H->htable) + i;  
  loc->val = val;
}



#define FAIL_FULL -2

/**
 * nsp_hm_enter:
 * @H: 
 * @str: a string 
 * @val: an integer
 * 
 * Enters (copy(@str),@val) in the hash table @H. The 
 * string @str is copied.
 * 
 * Return value: %OK or %FAIL
 **/

static int nsp_hm_enter(NspHm *H, gint64 key,double val)
{
  if ( key == no_key ) return FAIL;
  if ( H->filled >= 2*(H->hsize/3) ) 
    {
      if (nsp_hm_resize(H,2*H->hsize) == FAIL ) 
	{
	  Scierror("Error: running out of memory");
	  return FAIL;
	}
    }
  if ( nsp_bhsearch(H,key,&val,HM_ENTER) == FAIL )
    {
      return FAIL;
    }
  return OK;
}

/**
 *nsp_hm_remove
 * @H: 
 * @str: 
 * 
 * Remove entry with key str from HM Table
 * 
 **/

static void nsp_hm_remove(NspHm *H, gint64 key )
{
  double  val;
  nsp_bhsearch(H,key,&val,HM_REMOVE);
}

/**
 *nsp_hm_find:
 * @H: 
 * @str: 
 * @val: 
 * 
 * Search hm table @H for entry with key @str and returns it in @val.
 * 
 * Return value: %OK or %FAIL
 **/

static int nsp_hm_find(NspHm *H, gint64 key, double *val)
{
  return( nsp_bhfind(H,key,val));
}


/**
 *nsp_hm_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * Return value: 
 **/

NspBMatrix  *nsp_hm_equal(NspHm *L1, NspHm *L2)
{
  NspBMatrix *B;
  int i=0, count=0;
  if ( L1->filled != L2->filled ) 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,1))== NULLBMAT) return NULLBMAT;
      B->B[0]=FALSE; 
      return B;
    }
  else 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,L1->filled))== NULLBMAT) return NULLBMAT;
      while (1) 
	{
	  gint64 str=no_key;
	  double val,val2;
	  int rep =nsp_hm_get_next_object(L1,&i,&str,&val);
	  if ( str != no_key )
	    { 
	      if ( nsp_hm_find(L2,str,&val2) == FAIL)
		{
		  B->B[count]= FALSE;
		}
	      else 
		{
		  B->B[count]= (val == val2 );
		}
	      count++;
	    }
	  if ( rep == FAIL) break;
	}
    }
  return  B;
}


/**
 *nsp_hm_not_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
 **/


NspBMatrix  *nsp_hm_not_equal(NspHm *L1, NspHm *L2)
{
  NspBMatrix *B;
  int i=0,count=0;
  if ( L1->filled != L2->filled ) 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,1))== NULLBMAT) return NULLBMAT;
      B->B[0]=TRUE; 
      return B;
    }
  else 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,L1->filled))== NULLBMAT) return NULLBMAT;
      while (1) 
	{
	  gint64 str=no_key;
	  double val,val2;
	  int rep =nsp_hm_get_next_object(L1,&i,&str,&val);
	  if ( str != no_key )
	    { 
	      if ( nsp_hm_find(L2,str,&val2) == FAIL)
		{
		  B->B[count]= TRUE;
		}
	      else 
		{
		  B->B[count]= (val != val2);
		}
	      count++;
	    }
	  if ( rep == FAIL) break;
	}
    }
  return B;
} 


/**
 *nsp_hm_full_equal:
 * @L1: 
 * @L2: 
 * 
 * nsp_hm_equal(L1,L2)
 * if the two tables do not have the same length returns FALSE 
 * else returns and(L1(i)== L2(i)) 
 * 
 * 
 * Return value: 
 **/

int nsp_hm_full_equal(NspHm *L1, NspHm *L2)
{
  int i=0,rep=TRUE;
  if ( L1->filled != L2->filled ) return FALSE;
  while (1) 
    {
      gint64 str=no_key;
      double val,val2;
      int rep1 = nsp_hm_get_next_object(L1,&i,&str,&val);
      if ( str != no_key )
	{ 
	  if ( nsp_hm_find(L2,str,&val2) == FAIL)
	    {
	      return FALSE;
	    }
	  else 
	    {
	      rep = (val == val2 );
	      if ( rep == FALSE) return rep;
	    }
	}
      if ( rep1 == FAIL) break;
    }
  return rep;
} 

/**
 *nsp_hm_full_not_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_hm_full_not_equal(NspHm *L1, NspHm *L2)
{
  int i=0,rep=FALSE;
  if ( L1->filled != L2->filled ) return TRUE;
  while (1) 
    {
      gint64 str=no_key;
      double val,val2;
      int rep1 = nsp_hm_get_next_object(L1,&i,&str,&val);
      if ( str != no_key )
	{ 
	  if ( nsp_hm_find(L2,str,&val2) == FAIL)
	    {
	      return TRUE;
	    }
	  else 
	    {
	      rep = (val != val2 );
	      if ( rep == TRUE) return rep;
	    }
	}
      if (rep1 == FAIL) break;
    }
  return rep;
} 


/**
 * nsp_hm_get_keys:
 * @name: name to give to object 
 * @Hv: a #NspHm 
 * 
 * creates a #NspIMatrix filled with the keys of the hash table 
 * 
 * Return value: a new #NspMatrix or NULLSMAT
 **/

static NspIMatrix *nsp_hm_get_keys(const char *name,NspHm *Hv)
{
  NspIMatrix *Loc;
  NspHm *H = Hv;
  int i=0,count =0;
  if ( H->filled == 0) 
    {
      if ((Loc =nsp_imatrix_create(name,0,0,nsp_gint64)) == NULLIMAT)
	return NULLIMAT;
    }
  else 
    {
      if ( ( Loc =nsp_imatrix_create(name,H->filled,1,nsp_gint64)) == NULLIMAT)
	return NULLIMAT;
      /* allocate elements and store keys **/
      while (1) 
	{
	  gint64 str=no_key;
	  double val;
	  int rep = nsp_hm_get_next_object(H,&i,&str,&val);
	  if ( str != no_key )
	    { 
	      Loc->Gint64[count++]= str;
	    }
	  if (rep == FAIL) break;
	}
      if ( count != H->filled )
	{
	  int i;
	  Sciprintf("Warning: less objects (%d) in hm table than expected (%d) !\n",count,H->filled);
	  for ( i = count ; i < H->filled ; i++) Loc->Gint64[i]=0;
	  if ( nsp_imatrix_resize(Loc,count,1) == FAIL) return NULLIMAT;
	}
    }
  return  Loc;
}

/*
 * HMtable code : 
 * slightly modified to add REMOVE 
 * Jean-Philippe Chancelier 
 * 1998-1999
 */

/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)search.h	8.1 (Berkeley) 6/4/93
 */

/* Copyright (C) 1993 Free Software Foundation, Inc.
 *  This file is part of the GNU C Library.
 *  Contributed by Ulrich Drepper <drepper@ira.uka.de>
 *
 *  The GNU C Library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *   License, or (at your option) any later version.
 *
 *  The GNU C Library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with the GNU C Library; see the file COPYING.LIB.  If
 *  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
 *  Cambridge, MA 02139, USA.  
 */


/*
 * [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
 * [Knuth]            The Art of Computer Programming, part 3 (6.4)
 */


/* 
 * For the used double hm method the table size has to be a prime. To
 * correct the user given table size we need a prime test.  This trivial
 * algorithm is adequate because
 * a)  the code is (most probably) only called once per program run and
 * b)  the number is small because the table must fit in the core
 */

static int isprime(unsigned int number)
{
  /* no even number will be passed */
  unsigned div = 3;
  
  while (div*div < number && number%div != 0)
    div += 2;
  
  return number%div != 0;
}

/**
 * nsp_bhcreate:
 * @name: #NspHm object name 
 * @nel: initial size of the hm table object.
 * 
 * creates a #NspHm object with initial size greater than 
 * #nel. 
 * Before using the hm table we must allocate memory for it.
 * Test for an existing table are done. We allocate one element
 * more as the found prime number says. This is done for more effective
 * indexing as explained in the comment for the hsearch function.
 * The contents of the table is zeroed, especially the field used 
 * becomes zero.
 *  
 * Return value: a #NspHm object or %NULLHM
 **/

static NspHm *nsp_bhcreate(const char *name, unsigned int nel)
{
  NspHm *H = new_hm();
  HM_Entry *htable;
  /* Change nel to the first prime number not smaller as nel. */
  nel |= 1;      /* make odd */
  while (!isprime(nel)) nel += 2;
  if ( H == NULLHM)
    {
      Sciprintf("No more memory\n");
      return NULLHM;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return NULLHM;
  NSP_OBJECT(H)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  H->hsize  = nel;
  H->filled = 0;
  if (( htable = (HM_Entry *)calloc(H->hsize+1, sizeof(HM_Entry))) == NULL)
    {
      Sciprintf("No more memory\n");
      return NULLHM;
    }
  H->htable = htable ;
  return H;
}

/**
 * nsp_hmdestroy:
 * @H: a #NspHm object 
 * 
 * free a #NspHm object but not the elements which were 
 * stored in the hm table. This function is not to be used 
 * directly but through nsp_hm_destroy() call.
 * 
 **/

void nsp_hmdestroy(NspHm *H)
{
  /* free used memory */
  if ( H != NULLHM )
    {
      FREE(H->htable);
      nsp_object_destroy_name(NSP_OBJECT(H));
      FREE(H);
    }
}

/**
 * nsp_bhsearch:
 * @H: #NspHm object 
 * @key: key to search in the hm table 
 * @data: a #NspObject pointer to be set with the searched object
 * @action: action to perform.
 * 
 * This is the search function. It uses double hming with open adressing.
 * The argument item.key has to be a pointer to an zero terminated, most
 * probably strings of chars. The function for generating a number of the
 * strings is simple but fast. It can be replaced by a more complex function
 * like ajw (see [Aho,Sethi,Ullman]) if the needs are shown.
 *
 * We use an trick to speed up the lookup. The table is created by hcreate
 * with one more element available. This enables us to use the index zero
 * special. This index will never be used because we store the first hm
 * index in the field used where zero means not used. Every other value
 * means used. The used field can be used as a first fast comparison for
 * equality of the stored and the parameter value. This helps to prevent
 * unnecessary expensive calls of strcmp.
 * 
 * 
 * Return value: %OK, %FAIL. 
 **/

#define ACTION1								\
  switch (action)							\
    {									\
    case HM_REMOVE :							\
      break;								\
    case HM_ENTER :							\
      htable[idx].key = key;						\
      htable[idx].val = *val;						\
      (H->filled)++;							\
      return OK;							\
    case HM_FIND:							\
      break;								\
    }

#define ACTION2								\
  switch (action)							\
    {									\
    case HM_REMOVE :							\
      /* since other objects can be present with second			\
       * level keys with same hm value we must keep the cell in use	\
       */								\
      /* htable[idx].used = 0; */					\
      htable[idx].key = no_key;						\
      (H->filled)--;							\
      return OK ;							\
      break;								\
    case HM_ENTER:							\
      htable[idx].key = key;						\
      htable[idx].val = *val;						\
      /* we are just replacing an existing object (H->filled)++;*/	\
      return OK;							\
    case HM_FIND :							\
      *val= htable[idx].val;						\
      return OK;							\
    }									

static int nsp_bhsearch(NspHm *H, gint64 key,double *val, HMOperation action)
{
  register unsigned hval;
  register unsigned hval2;
  register unsigned idx;
  register int str;
  HM_Entry *htable = H->htable;

  /*
   * If table is full and another entry should be entered return with 
   * error. We keep one free position to let the H_FIND, H_REMOVE work.
   */
  if (action == HM_ENTER && H->filled == H->hsize -1 ) 
    {
      Scierror("HM Table %s is full\n",NSP_OBJECT(H)->name);
      return FAIL_FULL;
    }

  /* Compute a value for the given string. Perhaps use a better method. */
  str = key;
  hval = key;
  hval %= H->hsize;
  
  /* First hm function: simply take the modulo but prevent zero. */
  if (hval == 0) hval++;

  /* The first index tried. */
  idx = hval;

  if (htable[idx].used) 
    {
      /* Further action might be required according to the action value. */
      /* Sciprintf("First  hm Testing idx=%d\n",idx); */
      if (htable[idx].used == hval )
	{
	  if (htable[idx].key == no_key ) 
	    {
	      ACTION1;
	    }
	  else if ( htable[idx].key == key ) 
	    {
	      ACTION2;
	    }
	}
      
      /* Second hm function, as suggested in [Knuth] */

      hval2 = 1 + hval % (H->hsize-2);
	
      do {
	/* 
	 * Because hsize is prime this guarantees to step through all
	 * available indeces.
	 */
	if (idx <= hval2)
	  idx = H->hsize+idx-hval2;
	else
	  idx -= hval2;

	/* Sciprintf("2nd hm Testing idx=%d\n",idx); */
	/* If entry is found use it. */
	if (htable[idx].used == hval ) 
	  {
	    if (htable[idx].key == no_key )
	      {
		ACTION1;
	      }
	    else if ( htable[idx].key == key)
	      {
		ACTION2;
	      }
	  }
      } while (htable[idx].used);
	
    }

  /* Sciprintf("End of hm search idx=%d must be free \n",idx); **/
    
  /* An empty bucket has been found. */
  
  if (action == HM_ENTER )
    {
      htable[idx].key = key;
      htable[idx].used  = hval;
      htable[idx].val = *val;
      (H->filled)++;
      return OK ;
    }
  else 
    return FAIL;
}

/**
 * nsp_bhfind:
 * @H: #NspHm object 
 * @key: key to search in the hm table 
 * @data: a #NspObject pointer to be set with the searched object
 * @action: action to perform.
 * 
 * This is the function used for the HM_FIND action. It is almost 
 * a copy of the previous function but here we want the key to be const.
 * Return value: %OK, %FAIL. 
 **/

#define FIND_ACTION2	*val= htable[idx].val;return OK;

static int nsp_bhfind(NspHm *H, gint64 key,double *val)
{
  register unsigned hval;
  register unsigned hval2;
  register unsigned idx;
  register int str;
  HM_Entry *htable = H->htable;

  /*
   * If table is full and another entry should be entered return with 
   * error. We keep one free position to let the H_FIND, H_REMOVE work.
   */

  /* Compute a value for the given string. Perhaps use a better method. */
  str = key;
  hval = str;
  hval %= H->hsize;

  /* First hm function: simply take the modulo but prevent zero. */
  if (hval == 0) hval++;

  /* The first index tried. */
  idx = hval;

  if (htable[idx].used) 
    {
      /* Further action might be required according to the action value. */
      /* Sciprintf("First  hm Testing idx=%d\n",idx); */
      if (htable[idx].used == hval )
	{
	  if (htable[idx].key != no_key &&  htable[idx].key== key )
	    {
	      FIND_ACTION2;
	    }
	}
      
      /* Second hm function, as suggested in [Knuth] */

      hval2 = 1 + hval % (H->hsize-2);
	
      do {
	/* 
	 * Because hsize is prime this guarantees to step through all
	 * available indeces.
	 */
	if (idx <= hval2)
	  idx = H->hsize+idx-hval2;
	else
	  idx -= hval2;

	/* Sciprintf("2nd hm Testing idx=%d\n",idx); */
	/* If entry is found use it. */
	if (htable[idx].used == hval ) 
	  {
	    if (htable[idx].key != no_key &&  htable[idx].key==key )
	      {
		FIND_ACTION2;
	      }
	  }
      } while (htable[idx].used);
	
    }
  /* Sciprintf("End of hm search idx=%d must be free \n",idx); **/
  return FAIL;
}


static int nsp_hm_compute_key(NspHm *Hm,NspMatrix *M, gint64 *key)
{
  gint64 lkey=M->R[M->mn-1], i;
  for ( i= M->mn - 2  ; i >= 0  ; i--)
    {
      if ( M->R[i] < 0 || M->R[i] >= Hm->base)
	{
	  Scierror("Error: matrix key has a wrong indice at position %d\n",i+1);
	  return FAIL;
	}
      lkey = M->R[i] + Hm->base*lkey;
    }
  
  *key = lkey;
  return OK;
}

NspMatrix *nsp_hm_key2m(NspHm *Hm,gint64 key) 
{
  NspMatrix *M;
  int i;
  gint64 ltemp = key;
  if ((M= nsp_matrix_create(NVOID,'r',1,Hm->keysize)) == NULL) 
    return NULL;
  for ( i = 0 ; i < Hm->keysize ; i++ )
    {
      M->R[i] = ltemp % Hm->base ;
      ltemp /= Hm->base;
      if ( ltemp == 0) break;
    }
  if ( i < Hm->keysize - 1 ) 
    {
      int j;
      for ( j=i+1 ; j < Hm->keysize ; j++) M->R[j]=0;
    }
  return M;
}


static int nsp_hm_check_slope(NspHm *H,NspMatrix *M)
{
  int i,j,k,l;
  /* boundaries */
  for ( i = 0; i < M->m ; i++) 
    {
      if ( M->R[i] > 1 ) return FALSE;
    }
  for ( i = 0; i < M->m ; i++) 
    {
      if ( M->R[i+M->m*(M->n-1)] > 1 ) return FALSE;
    }
  /* internal */
  for ( i = 1; i < M->m -1 ; i++) 
    for ( j = 1; j < M->n -1 ; j++) 
      {
	int val = M->R[i+M->m*j];
	for ( k = -1 ; k <= 1 ; k++ )
	  for ( l = -1 ; l <= 1 ; l++ )
	    {
	      int val1 = M->R[(k+i)+M->m*(l+j)];
	      if ( Abs(val1 -val) > 1) return FALSE;
	    }
      }
  return TRUE;
}


#line 1649 "hm.c"
