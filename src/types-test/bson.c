/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2011 Jean-Philippe Chancelier Enpc/Cermics
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





#line 22 "codegen/bson.override"
#include <nsp/objects.h>
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/parse.h>
#include <mongoc.h>
#include <nsp/mcollection.h>
#include <nsp/mcursor.h>

#line 37 "bson.c"

/* ----------- NspBson ----------- */


#define  NspBson_Private 
#include <nsp/objects.h>
#include <nsp/bson.h>
#include <nsp/interf.h>

/* 
 * NspBson inherits from Object 
 */

int nsp_type_bson_id=0;
NspTypeBson *nsp_type_bson=NULL;

/*
 * Type object for NspBson 
 * all the instance of NspTypeBson share the same id. 
 * nsp_type_bson: is an instance of NspTypeBson 
 *    used for objects of NspBson type (i.e built with new_bson) 
 * other instances are used for derived classes 
 */
NspTypeBson *new_type_bson(type_mode mode)
{
  NspTypeBson *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_bson != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_bson;
    }
  if (( type =  malloc(sizeof(NspTypeBson))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = bson_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = bson_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_bson;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for bson */ 

  top->pr = (print_func *) nsp_bson_print;
  top->dealloc = (dealloc_func *) nsp_bson_destroy;
  top->copy  =  (copy_func *) nsp_bson_copy;
  top->size  = (size_func *) nsp_bson_size;
  top->s_type =  (s_type_func *) nsp_bson_type_as_string;
  top->sh_type = (sh_type_func *) nsp_bson_type_short_string;
  top->info = (info_func *) nsp_bson_info;
  /* top->is_true = (is_true_func  *) nsp_bson_is_true; */
#line 133 "codegen/bson.override"
top->loop = (loop_func *) nsp_bson_loop; /* loop with bson type */

#line 98 "bson.c"
#line 133 "codegen/bson.override"
top->path_extract = (path_func *) NULL; /* path extract as for matrix type */

#line 102 "bson.c"
  top->get_from_obj = (get_from_obj_func *) nsp_bson_object;
  top->eq  = (eq_func *) nsp_bson_eq;
  top->neq  = (eq_func *) nsp_bson_neq;
  top->save  = (save_func *) nsp_bson_xdr_save;
  top->load  = (load_func *) nsp_bson_xdr_load;
  top->create = (create_func*) int_bson_create;
  top->latex = (print_func *) nsp_bson_latex;
  top->full_copy = (copy_func *) nsp_bson_full_copy;

  /* specific methods for bson */

  type->init = (init_func *) init_bson;

#line 45 "codegen/bson.override"
  /* inserted verbatim in the type definition */
  top->is_true = (is_true_func  *) nsp_bson_is_true; 

#line 120 "bson.c"
  /* 
   * NspBson interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_bson_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeBson called nsp_type_bson
       */
      type->id =  nsp_type_bson_id = nsp_new_type_id();
      nsp_type_bson = type;
      if ( nsp_register_type(nsp_type_bson) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_bson(mode);
    }
  else 
    {
      type->id = nsp_type_bson_id;
      return type;
    }
}

/*
 * initialize NspBson instances 
 * locally and by calling initializer on parent class 
 */

static int init_bson(NspBson *Obj,NspTypeBson *type)
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
 * new instance of NspBson 
 */

NspBson *new_bson() 
{
  NspBson *loc;
  /* type must exists */
  nsp_type_bson = new_type_bson(T_BASE);
  if ( (loc = malloc(sizeof(NspBson)))== NULLBSON) return loc;
  /* initialize object */
  if ( init_bson(loc,nsp_type_bson) == FAIL) return NULLBSON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspBson 
 *-----------------------------------------------*/
#line 152 "codegen/bson.override"

/*
 * size can be overriden here
 */

static int nsp_bson_size(NspBson *Mat, int flag)
{
  /* redirect the size value to Mat->value */
  return 0;
}

#line 191 "bson.c"
/*
 * type as string 
 */

static char bson_type_name[]="Bson";
static char bson_short_type_name[]="bson";

static char *nsp_bson_type_as_string(void)
{
  return(bson_type_name);
}

static char *nsp_bson_type_short_string(NspObject *v)
{
  return(bson_short_type_name);
}

/*
 * A == B 
 */

static int nsp_bson_eq(NspBson *A, NspObject *B)
{
  NspBson *loc = (NspBson *) B;
  if ( check_cast(B,nsp_type_bson_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->b != loc->obj->b) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_bson_neq(NspBson *A, NspObject *B)
{
  return ( nsp_bson_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_bson_xdr_save(XDR *xdrs, NspBson *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_bson)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspBson  *nsp_bson_xdr_load_partial(XDR *xdrs, NspBson *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspBson  *nsp_bson_xdr_load(XDR *xdrs)
{
  NspBson *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLBSON;
  if ((H  = nsp_bson_create_void(name,(NspTypeBase *) nsp_type_bson))== NULLBSON) return H;
  if ( nsp_bson_create_partial(H) == FAIL) return NULLBSON;
  if ((H  = nsp_bson_xdr_load_partial(xdrs,H))== NULLBSON) return H;
  if ( nsp_bson_check_values(H) == FAIL) return NULLBSON;
#line 97 "codegen/bson.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */

#line 267 "bson.c"
  return H;
}

/*
 * delete 
 */

void nsp_bson_destroy_partial(NspBson *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 101 "codegen/bson.override"
  /* verbatim in destroy */
  bson_free(H->obj->b);

#line 284 "bson.c"
    FREE(H->obj);
   }
}

void nsp_bson_destroy(NspBson *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_bson_destroy_partial(H);
  FREE(H);
}

#line 141 "codegen/bson.override"
/*
 * info overriden 
 */

int nsp_bson_info(NspBson *M, int indent,const char *name, int rec_level)
{
  return nsp_bson_print(M,indent,name,rec_level);
}

#line 306 "bson.c"
/*
 * print 
 */

int nsp_bson_print(NspBson *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLBSON) 
    {
      Sciprintf("Null Pointer NspBson \n");
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
          nsp_bson_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_bson_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"b=0x%x\n",M->obj->b);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_bson_latex(NspBson *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_bson_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"b=0x%x\n",M->obj->b);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspBson objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspBson   *nsp_bson_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_bson_id) == TRUE ) return ((NspBson *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_bson));
  return NULL;
}

int IsBsonObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_bson_id);
}

int IsBson(NspObject *O)
{
  return nsp_object_type(O,nsp_type_bson_id);
}

NspBson  *GetBsonCopy(Stack stack, int i)
{
  if (  GetBson(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspBson  *GetBson(Stack stack, int i)
{
  NspBson *M;
  if (( M = nsp_bson_object(NthObj(i))) == NULLBSON)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspBson instance 
 *-----------------------------------------------------*/

static NspBson *nsp_bson_create_void(const char *name,NspTypeBase *type)
{
 NspBson *H  = (type == NULL) ? new_bson() : type->new();
 if ( H ==  NULLBSON)
  {
   Sciprintf("No more memory\n");
   return NULLBSON;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLBSON;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_bson_create_partial(NspBson *H)
{
  if((H->obj = calloc(1,sizeof(nsp_bson)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->b = NULL;
  return OK;
}

int nsp_bson_check_values(NspBson *H)
{
  return OK;
}

NspBson *nsp_bson_create(const char *name,void* b,NspTypeBase *type)
{
  NspBson *H  = nsp_bson_create_void(name,type);
  if ( H ==  NULLBSON) return NULLBSON;
  if ( nsp_bson_create_partial(H) == FAIL) return NULLBSON;
  H->obj->b = b;
  if ( nsp_bson_check_values(H) == FAIL) return NULLBSON;
#line 97 "codegen/bson.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */

#line 436 "bson.c"
  return H;
}


NspBson *nsp_bson_create_default(const char *name)
{
 NspBson *H  = nsp_bson_create_void(name,NULL);
 if ( H ==  NULLBSON) return NULLBSON;
  if ( nsp_bson_create_partial(H) == FAIL) return NULLBSON;
 if ( nsp_bson_check_values(H) == FAIL) return NULLBSON;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspBson *nsp_bson_copy_partial(NspBson *H,NspBson *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspBson *nsp_bson_copy(NspBson *self)
{
  NspBson *H  =nsp_bson_create_void(NVOID,(NspTypeBase *) nsp_type_bson);
  if ( H ==  NULLBSON) return NULLBSON;
  if ( nsp_bson_copy_partial(H,self)== NULL) return NULLBSON;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspBson *nsp_bson_full_copy_partial(NspBson *H,NspBson *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_bson))) == NULL) return NULLBSON;
  H->obj->ref_count=1;
  H->obj->b = self->obj->b;
  return H;
}

NspBson *nsp_bson_full_copy(NspBson *self)
{
  NspBson *H  =nsp_bson_create_void(NVOID,(NspTypeBase *) nsp_type_bson);
  if ( H ==  NULLBSON) return NULLBSON;
  if ( nsp_bson_full_copy_partial(H,self)== NULL) return NULLBSON;
#line 97 "codegen/bson.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */

#line 488 "bson.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspBson
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

#line 50 "codegen/bson.override"

/* override the default int_create */

int int_bson_create(Stack stack, int rhs, int opt, int lhs)
{
  bson_t* b;
  NspBson *B;
  CheckStdRhs(0,1);
  /* want to be sure that type bvar is initialized */
  nsp_type_bson = new_type_bson(T_BASE);
  if (( b = bson_malloc(sizeof(bson_t))) == NULL) 
    {
      Scierror("Error: bson_malloc failed to create a bson object\n");
      return RET_BUG;
    }
  bson_init(b);

  if ( rhs == 1) 
    {
      int i;
      NspHash *H;
      NspObject *O1;
      if ((H = GetHash(stack,1)) == NULLHASH) return RET_BUG;
      i=0;
      while (1) 
	{
	  int rep =nsp_hash_get_next_object(H,&i,&O1);
	  if ( O1 != NULLOBJ )
	    { 
	      const char *name = nsp_object_get_name(O1);
	      nsp_bson_insert(b,name, O1);
	    }
	  if ( rep == FAIL) break;
	}
    }
  if(( B = nsp_bson_create(NVOID,(void *) b,(NspTypeBase *) nsp_type_bson)) == NULL)
    {
      Scierror("Error: failed to create a bson object\n");
      return RET_BUG;
    }
  MoveObj(stack,1,(NspObject  *) B);
  return 1;
} 


#line 543 "bson.c"
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_bson_show(NspBson *self,Stack stack,int rhs,int opt,int lhs)
{
  bson_show(self);
  return 0;
}

#line 106 "codegen/bson.override"

static int _wrap_bson_to_hash(NspBson *self,Stack stack,int rhs,int opt,int lhs)
{
  NspHash *H;
  CheckLhs(0,1);
  CheckRhs(0,0);
  if ( ( H = bson_to_hash(self)) == NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
}

#line 565 "bson.c"


static NspMethods bson_methods[] = {
  {"show",(nsp_method *) _wrap_bson_show},
  {"to_hash",(nsp_method *) _wrap_bson_to_hash},
  { NULL, NULL}
};

static NspMethods *bson_get_methods(void) { return bson_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab bson_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;


/* ----------- NspMclient ----------- */


#define  NspMclient_Private 
#include <nsp/objects.h>
#include <nsp/mclient.h>
#include <nsp/interf.h>

/* 
 * NspMclient inherits from Object 
 */

int nsp_type_mclient_id=0;
NspTypeMclient *nsp_type_mclient=NULL;

/*
 * Type object for NspMclient 
 * all the instance of NspTypeMclient share the same id. 
 * nsp_type_mclient: is an instance of NspTypeMclient 
 *    used for objects of NspMclient type (i.e built with new_mclient) 
 * other instances are used for derived classes 
 */
NspTypeMclient *new_type_mclient(type_mode mode)
{
  NspTypeMclient *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_mclient != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_mclient;
    }
  if (( type =  malloc(sizeof(NspTypeMclient))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = mclient_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = mclient_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_mclient;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for mclient */ 

  top->pr = (print_func *) nsp_mclient_print;
  top->dealloc = (dealloc_func *) nsp_mclient_destroy;
  top->copy  =  (copy_func *) nsp_mclient_copy;
  top->size  = (size_func *) nsp_mclient_size;
  top->s_type =  (s_type_func *) nsp_mclient_type_as_string;
  top->sh_type = (sh_type_func *) nsp_mclient_type_short_string;
  top->info = (info_func *) nsp_mclient_info;
  /* top->is_true = (is_true_func  *) nsp_mclient_is_true; */
  /* top->loop =(loop_func *) nsp_mclient_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_mclient_object;
  top->eq  = (eq_func *) nsp_mclient_eq;
  top->neq  = (eq_func *) nsp_mclient_neq;
  top->save  = (save_func *) nsp_mclient_xdr_save;
  top->load  = (load_func *) nsp_mclient_xdr_load;
  top->create = (create_func*) int_mclient_create;
  top->latex = (print_func *) nsp_mclient_latex;
  top->full_copy = (copy_func *) nsp_mclient_full_copy;

  /* specific methods for mclient */

  type->init = (init_func *) init_mclient;

  /* 
   * NspMclient interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_mclient_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMclient called nsp_type_mclient
       */
      type->id =  nsp_type_mclient_id = nsp_new_type_id();
      nsp_type_mclient = type;
      if ( nsp_register_type(nsp_type_mclient) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_mclient(mode);
    }
  else 
    {
      type->id = nsp_type_mclient_id;
      return type;
    }
}

/*
 * initialize NspMclient instances 
 * locally and by calling initializer on parent class 
 */

static int init_mclient(NspMclient *Obj,NspTypeMclient *type)
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
 * new instance of NspMclient 
 */

NspMclient *new_mclient() 
{
  NspMclient *loc;
  /* type must exists */
  nsp_type_mclient = new_type_mclient(T_BASE);
  if ( (loc = malloc(sizeof(NspMclient)))== NULLMCLIENT) return loc;
  /* initialize object */
  if ( init_mclient(loc,nsp_type_mclient) == FAIL) return NULLMCLIENT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspMclient 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_mclient_size(NspMclient *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char mclient_type_name[]="Mclient";
static char mclient_short_type_name[]="mclient";

static char *nsp_mclient_type_as_string(void)
{
  return(mclient_type_name);
}

static char *nsp_mclient_type_short_string(NspObject *v)
{
  return(mclient_short_type_name);
}

/*
 * A == B 
 */

static int nsp_mclient_eq(NspMclient *A, NspObject *B)
{
  NspMclient *loc = (NspMclient *) B;
  if ( check_cast(B,nsp_type_mclient_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->cl != loc->obj->cl) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_mclient_neq(NspMclient *A, NspObject *B)
{
  return ( nsp_mclient_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_mclient_xdr_save(XDR *xdrs, NspMclient *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_mclient)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspMclient  *nsp_mclient_xdr_load_partial(XDR *xdrs, NspMclient *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspMclient  *nsp_mclient_xdr_load(XDR *xdrs)
{
  NspMclient *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLMCLIENT;
  if ((H  = nsp_mclient_create_void(name,(NspTypeBase *) nsp_type_mclient))== NULLMCLIENT) return H;
  if ( nsp_mclient_create_partial(H) == FAIL) return NULLMCLIENT;
  if ((H  = nsp_mclient_xdr_load_partial(xdrs,H))== NULLMCLIENT) return H;
  if ( nsp_mclient_check_values(H) == FAIL) return NULLMCLIENT;
  return H;
}

/*
 * delete 
 */

void nsp_mclient_destroy_partial(NspMclient *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 223 "codegen/bson.override"
  /* verbatim in destroy */
  mongoc_client_destroy(H->obj->cl);

#line 808 "bson.c"
    FREE(H->obj);
   }
}

void nsp_mclient_destroy(NspMclient *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_mclient_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_mclient_info(NspMclient *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLMCLIENT) 
    {
      Sciprintf("Null Pointer NspMclient \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_mclient_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_mclient_print(NspMclient *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLMCLIENT) 
    {
      Sciprintf("Null Pointer NspMclient \n");
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
          nsp_mclient_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_mclient_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"cl=0x%x\n",M->obj->cl);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_mclient_latex(NspMclient *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_mclient_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"cl=0x%x\n",M->obj->cl);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspMclient objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspMclient   *nsp_mclient_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_mclient_id) == TRUE ) return ((NspMclient *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_mclient));
  return NULL;
}

int IsMclientObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_mclient_id);
}

int IsMclient(NspObject *O)
{
  return nsp_object_type(O,nsp_type_mclient_id);
}

NspMclient  *GetMclientCopy(Stack stack, int i)
{
  if (  GetMclient(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspMclient  *GetMclient(Stack stack, int i)
{
  NspMclient *M;
  if (( M = nsp_mclient_object(NthObj(i))) == NULLMCLIENT)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspMclient instance 
 *-----------------------------------------------------*/

static NspMclient *nsp_mclient_create_void(const char *name,NspTypeBase *type)
{
 NspMclient *H  = (type == NULL) ? new_mclient() : type->new();
 if ( H ==  NULLMCLIENT)
  {
   Sciprintf("No more memory\n");
   return NULLMCLIENT;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLMCLIENT;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_mclient_create_partial(NspMclient *H)
{
  if((H->obj = calloc(1,sizeof(nsp_mclient)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->cl = NULL;
  return OK;
}

int nsp_mclient_check_values(NspMclient *H)
{
  return OK;
}

NspMclient *nsp_mclient_create(const char *name,void* cl,NspTypeBase *type)
{
  NspMclient *H  = nsp_mclient_create_void(name,type);
  if ( H ==  NULLMCLIENT) return NULLMCLIENT;
  if ( nsp_mclient_create_partial(H) == FAIL) return NULLMCLIENT;
  H->obj->cl = cl;
  if ( nsp_mclient_check_values(H) == FAIL) return NULLMCLIENT;
  return H;
}


NspMclient *nsp_mclient_create_default(const char *name)
{
 NspMclient *H  = nsp_mclient_create_void(name,NULL);
 if ( H ==  NULLMCLIENT) return NULLMCLIENT;
  if ( nsp_mclient_create_partial(H) == FAIL) return NULLMCLIENT;
 if ( nsp_mclient_check_values(H) == FAIL) return NULLMCLIENT;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspMclient *nsp_mclient_copy_partial(NspMclient *H,NspMclient *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspMclient *nsp_mclient_copy(NspMclient *self)
{
  NspMclient *H  =nsp_mclient_create_void(NVOID,(NspTypeBase *) nsp_type_mclient);
  if ( H ==  NULLMCLIENT) return NULLMCLIENT;
  if ( nsp_mclient_copy_partial(H,self)== NULL) return NULLMCLIENT;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspMclient *nsp_mclient_full_copy_partial(NspMclient *H,NspMclient *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_mclient))) == NULL) return NULLMCLIENT;
  H->obj->ref_count=1;
  H->obj->cl = self->obj->cl;
  return H;
}

NspMclient *nsp_mclient_full_copy(NspMclient *self)
{
  NspMclient *H  =nsp_mclient_create_void(NVOID,(NspTypeBase *) nsp_type_mclient);
  if ( H ==  NULLMCLIENT) return NULLMCLIENT;
  if ( nsp_mclient_full_copy_partial(H,self)== NULL) return NULLMCLIENT;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspMclient
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

#line 197 "codegen/bson.override"

int int_mclient_create(Stack stack, int rhs, int opt, int lhs)
{
  char *uristr;
  mongoc_client_t *client;
  NspMclient *B;
  CheckStdRhs(1,1);
  if ((uristr = GetString(stack,1)) == (char*)0) return RET_BUG;
  /* want to be sure that type bvar is initialized */
  nsp_type_mclient = new_type_mclient(T_BASE);
  if (( client = mongoc_client_new (uristr)) == NULL) 
    {
      Scierror("Error: failed to parse uri %s\n",uristr);
      return RET_BUG;
    }
  if(( B = nsp_mclient_create(NVOID,(void *) client,(NspTypeBase *) nsp_type_mclient)) == NULL)
    {
      Scierror("Error: failed to create a mongodb client\n");
      return RET_BUG;
    }
  MoveObj(stack,1,(NspObject  *) B);
  return 1;
} 

#line 1045 "bson.c"
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 228 "codegen/bson.override"

static int _wrap_mongoc_client_get_collection(NspMclient *self,Stack stack,int rhs,int opt,int lhs)
{
  NspMcollection *M;
  char *collection_name, *base_name;
  mongoc_collection_t *collection;
  CheckStdRhs(2,2);
  CheckLhs(0,1);
  if ((base_name = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((collection_name = GetString(stack,2)) == (char*)0) return RET_BUG;

  if ((collection = mongoc_client_get_collection(self->obj->cl,base_name,collection_name)) == NULL) 
    {
      Scierror("Error: failed to obtain collection %s in base %s\n",collection_name,base_name);
      return RET_BUG;
    }
  nsp_type_mcollection = new_type_mcollection(T_BASE);
  if(( M = nsp_mcollection_create(NVOID,(void *) collection,(NspTypeBase *) nsp_type_mcollection)) == NULL)
    {
      Scierror("Error: failed to create a NspMcollection\n");
      return RET_BUG;
    }
  MoveObj(stack,1,(NspObject  *) M);
  return 1;
}

#line 1076 "bson.c"


static NspMethods mclient_methods[] = {
  {"get_collection",(nsp_method *) _wrap_mongoc_client_get_collection},
  { NULL, NULL}
};

static NspMethods *mclient_get_methods(void) { return mclient_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab mclient_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;


/* ----------- NspMcollection ----------- */


#define  NspMcollection_Private 
#include <nsp/objects.h>
#include <nsp/mcollection.h>
#include <nsp/interf.h>

/* 
 * NspMcollection inherits from Object 
 */

int nsp_type_mcollection_id=0;
NspTypeMcollection *nsp_type_mcollection=NULL;

/*
 * Type object for NspMcollection 
 * all the instance of NspTypeMcollection share the same id. 
 * nsp_type_mcollection: is an instance of NspTypeMcollection 
 *    used for objects of NspMcollection type (i.e built with new_mcollection) 
 * other instances are used for derived classes 
 */
NspTypeMcollection *new_type_mcollection(type_mode mode)
{
  NspTypeMcollection *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_mcollection != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_mcollection;
    }
  if (( type =  malloc(sizeof(NspTypeMcollection))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = mcollection_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = mcollection_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_mcollection;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for mcollection */ 

  top->pr = (print_func *) nsp_mcollection_print;
  top->dealloc = (dealloc_func *) nsp_mcollection_destroy;
  top->copy  =  (copy_func *) nsp_mcollection_copy;
  top->size  = (size_func *) nsp_mcollection_size;
  top->s_type =  (s_type_func *) nsp_mcollection_type_as_string;
  top->sh_type = (sh_type_func *) nsp_mcollection_type_short_string;
  top->info = (info_func *) nsp_mcollection_info;
  /* top->is_true = (is_true_func  *) nsp_mcollection_is_true; */
  /* top->loop =(loop_func *) nsp_mcollection_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_mcollection_object;
  top->eq  = (eq_func *) nsp_mcollection_eq;
  top->neq  = (eq_func *) nsp_mcollection_neq;
  top->save  = (save_func *) nsp_mcollection_xdr_save;
  top->load  = (load_func *) nsp_mcollection_xdr_load;
  top->create = (create_func*) int_mcollection_create;
  top->latex = (print_func *) nsp_mcollection_latex;
  top->full_copy = (copy_func *) nsp_mcollection_full_copy;

  /* specific methods for mcollection */

  type->init = (init_func *) init_mcollection;

  /* 
   * NspMcollection interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_mcollection_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMcollection called nsp_type_mcollection
       */
      type->id =  nsp_type_mcollection_id = nsp_new_type_id();
      nsp_type_mcollection = type;
      if ( nsp_register_type(nsp_type_mcollection) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_mcollection(mode);
    }
  else 
    {
      type->id = nsp_type_mcollection_id;
      return type;
    }
}

/*
 * initialize NspMcollection instances 
 * locally and by calling initializer on parent class 
 */

static int init_mcollection(NspMcollection *Obj,NspTypeMcollection *type)
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
 * new instance of NspMcollection 
 */

NspMcollection *new_mcollection() 
{
  NspMcollection *loc;
  /* type must exists */
  nsp_type_mcollection = new_type_mcollection(T_BASE);
  if ( (loc = malloc(sizeof(NspMcollection)))== NULLMCOLLECTION) return loc;
  /* initialize object */
  if ( init_mcollection(loc,nsp_type_mcollection) == FAIL) return NULLMCOLLECTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspMcollection 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_mcollection_size(NspMcollection *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char mcollection_type_name[]="Mcollection";
static char mcollection_short_type_name[]="mcollection";

static char *nsp_mcollection_type_as_string(void)
{
  return(mcollection_type_name);
}

static char *nsp_mcollection_type_short_string(NspObject *v)
{
  return(mcollection_short_type_name);
}

/*
 * A == B 
 */

static int nsp_mcollection_eq(NspMcollection *A, NspObject *B)
{
  NspMcollection *loc = (NspMcollection *) B;
  if ( check_cast(B,nsp_type_mcollection_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->co != loc->obj->co) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_mcollection_neq(NspMcollection *A, NspObject *B)
{
  return ( nsp_mcollection_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_mcollection_xdr_save(XDR *xdrs, NspMcollection *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_mcollection)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspMcollection  *nsp_mcollection_xdr_load_partial(XDR *xdrs, NspMcollection *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspMcollection  *nsp_mcollection_xdr_load(XDR *xdrs)
{
  NspMcollection *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLMCOLLECTION;
  if ((H  = nsp_mcollection_create_void(name,(NspTypeBase *) nsp_type_mcollection))== NULLMCOLLECTION) return H;
  if ( nsp_mcollection_create_partial(H) == FAIL) return NULLMCOLLECTION;
  if ((H  = nsp_mcollection_xdr_load_partial(xdrs,H))== NULLMCOLLECTION) return H;
  if ( nsp_mcollection_check_values(H) == FAIL) return NULLMCOLLECTION;
  return H;
}

/*
 * delete 
 */

void nsp_mcollection_destroy_partial(NspMcollection *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 265 "codegen/bson.override"
  /* verbatim in destroy */
  mongoc_collection_destroy(H->obj->co);

#line 1318 "bson.c"
    FREE(H->obj);
   }
}

void nsp_mcollection_destroy(NspMcollection *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_mcollection_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_mcollection_info(NspMcollection *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLMCOLLECTION) 
    {
      Sciprintf("Null Pointer NspMcollection \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_mcollection_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_mcollection_print(NspMcollection *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLMCOLLECTION) 
    {
      Sciprintf("Null Pointer NspMcollection \n");
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
          nsp_mcollection_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_mcollection_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"co=0x%x\n",M->obj->co);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_mcollection_latex(NspMcollection *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_mcollection_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"co=0x%x\n",M->obj->co);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspMcollection objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspMcollection   *nsp_mcollection_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_mcollection_id) == TRUE ) return ((NspMcollection *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_mcollection));
  return NULL;
}

int IsMcollectionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_mcollection_id);
}

int IsMcollection(NspObject *O)
{
  return nsp_object_type(O,nsp_type_mcollection_id);
}

NspMcollection  *GetMcollectionCopy(Stack stack, int i)
{
  if (  GetMcollection(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspMcollection  *GetMcollection(Stack stack, int i)
{
  NspMcollection *M;
  if (( M = nsp_mcollection_object(NthObj(i))) == NULLMCOLLECTION)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspMcollection instance 
 *-----------------------------------------------------*/

static NspMcollection *nsp_mcollection_create_void(const char *name,NspTypeBase *type)
{
 NspMcollection *H  = (type == NULL) ? new_mcollection() : type->new();
 if ( H ==  NULLMCOLLECTION)
  {
   Sciprintf("No more memory\n");
   return NULLMCOLLECTION;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLMCOLLECTION;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_mcollection_create_partial(NspMcollection *H)
{
  if((H->obj = calloc(1,sizeof(nsp_mcollection)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->co = NULL;
  return OK;
}

int nsp_mcollection_check_values(NspMcollection *H)
{
  return OK;
}

NspMcollection *nsp_mcollection_create(const char *name,void* co,NspTypeBase *type)
{
  NspMcollection *H  = nsp_mcollection_create_void(name,type);
  if ( H ==  NULLMCOLLECTION) return NULLMCOLLECTION;
  if ( nsp_mcollection_create_partial(H) == FAIL) return NULLMCOLLECTION;
  H->obj->co = co;
  if ( nsp_mcollection_check_values(H) == FAIL) return NULLMCOLLECTION;
  return H;
}


NspMcollection *nsp_mcollection_create_default(const char *name)
{
 NspMcollection *H  = nsp_mcollection_create_void(name,NULL);
 if ( H ==  NULLMCOLLECTION) return NULLMCOLLECTION;
  if ( nsp_mcollection_create_partial(H) == FAIL) return NULLMCOLLECTION;
 if ( nsp_mcollection_check_values(H) == FAIL) return NULLMCOLLECTION;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspMcollection *nsp_mcollection_copy_partial(NspMcollection *H,NspMcollection *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspMcollection *nsp_mcollection_copy(NspMcollection *self)
{
  NspMcollection *H  =nsp_mcollection_create_void(NVOID,(NspTypeBase *) nsp_type_mcollection);
  if ( H ==  NULLMCOLLECTION) return NULLMCOLLECTION;
  if ( nsp_mcollection_copy_partial(H,self)== NULL) return NULLMCOLLECTION;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspMcollection *nsp_mcollection_full_copy_partial(NspMcollection *H,NspMcollection *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_mcollection))) == NULL) return NULLMCOLLECTION;
  H->obj->ref_count=1;
  H->obj->co = self->obj->co;
  return H;
}

NspMcollection *nsp_mcollection_full_copy(NspMcollection *self)
{
  NspMcollection *H  =nsp_mcollection_create_void(NVOID,(NspTypeBase *) nsp_type_mcollection);
  if ( H ==  NULLMCOLLECTION) return NULLMCOLLECTION;
  if ( nsp_mcollection_full_copy_partial(H,self)== NULL) return NULLMCOLLECTION;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspMcollection
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_mcollection_create(Stack stack, int rhs, int opt, int lhs)
{
  NspMcollection *H;
  CheckStdRhs(0,0);
  /* want to be sure that type mcollection is initialized */
  nsp_type_mcollection = new_type_mcollection(T_BASE);
  if(( H = nsp_mcollection_create_void(NVOID,(NspTypeBase *) nsp_type_mcollection)) == NULLMCOLLECTION) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_mcollection_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_mcollection_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 270 "codegen/bson.override"

static int _wrap_mongoc_collection_find(NspMcollection *self,Stack stack,int rhs,int opt,int lhs)
{
  NspMcursor *M;
  bson_error_t error;
  bson_t query;
  mongoc_cursor_t *cursor;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  bson_init (&query);
  cursor = mongoc_collection_find (self->obj->co,
				   MONGOC_QUERY_NONE,
				   0,
				   0,
				   0,
				   &query,
				   NULL,  /* Fields, NULL for all. */
				   NULL); /* Read Prefs, NULL for default */

  if (mongoc_cursor_error (cursor, &error)) 
    {
      Scierror("Error: %s\n", error.message);
      return RET_BUG;
    }
  bson_destroy (&query); /* can we kill the query here ? */
  nsp_type_mcursor = new_type_mcursor(T_BASE);
  if(( M = nsp_mcursor_create(NVOID,(void *) cursor,NULL,(NspTypeBase *) nsp_type_mcursor)) == NULL)
    {
      Scierror("Error: failed to create a NspMcursor\n");
      return RET_BUG;
    }
  MoveObj(stack,1,(NspObject  *) M);
  return 1;
}

#line 1584 "bson.c"


#line 339 "codegen/bson.override"

static int _wrap_mongoc_collection_insert(NspMcollection *self,Stack stack,int rhs,int opt,int lhs)
{
  NspBson *B;
  bson_bool_t r;
  bson_error_t error;
  CheckStdRhs(1,1);
  CheckLhs(0,1);
  if ((B= GetBson(stack,1))== NULL) return RET_BUG;

  r = mongoc_collection_insert(self->obj->co, MONGOC_INSERT_NONE, B->obj->b, NULL, &error);
  if (!r && lhs != 1) 
    {
      Scierror("Error: %s\n", error.message);
      return RET_BUG;
    }
  if ( lhs == 1) 
    {
      if ( nsp_move_boolean(stack,1,r)==FAIL) return RET_BUG;
      return 1;
    }
  else
    return 0;
}


#line 1614 "bson.c"


#line 307 "codegen/bson.override"

static int _wrap_mongoc_collection_delete(NspMcollection *self,Stack stack,int rhs,int opt,int lhs)
{
  bson_bool_t r;
  bson_error_t error;
  bson_t b;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  bson_init (&b);
  r = mongoc_collection_delete (self->obj->co,
				MONGOC_DELETE_NONE,
				&b,
				NULL,
				&error);
  bson_destroy(&b);
  if (!r && lhs != 1) 
    {
      Scierror("Error: %s\n", error.message);
      return RET_BUG;
    }

  if ( lhs == 1) 
    {
      if ( nsp_move_boolean(stack,1,r)==FAIL) return RET_BUG;
      return 1;
    }
  else
    return 0;
}

#line 1648 "bson.c"


static NspMethods mcollection_methods[] = {
  {"find",(nsp_method *) _wrap_mongoc_collection_find},
  {"insert",(nsp_method *) _wrap_mongoc_collection_insert},
  {"delete",(nsp_method *) _wrap_mongoc_collection_delete},
  { NULL, NULL}
};

static NspMethods *mcollection_get_methods(void) { return mcollection_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab mcollection_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;


/* ----------- NspMcursor ----------- */


#define  NspMcursor_Private 
#include <nsp/objects.h>
#include <nsp/mcursor.h>
#include <nsp/interf.h>

/* 
 * NspMcursor inherits from Object 
 */

int nsp_type_mcursor_id=0;
NspTypeMcursor *nsp_type_mcursor=NULL;

/*
 * Type object for NspMcursor 
 * all the instance of NspTypeMcursor share the same id. 
 * nsp_type_mcursor: is an instance of NspTypeMcursor 
 *    used for objects of NspMcursor type (i.e built with new_mcursor) 
 * other instances are used for derived classes 
 */
NspTypeMcursor *new_type_mcursor(type_mode mode)
{
  NspTypeMcursor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_mcursor != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_mcursor;
    }
  if (( type =  malloc(sizeof(NspTypeMcursor))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = mcursor_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = mcursor_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_mcursor;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for mcursor */ 

  top->pr = (print_func *) nsp_mcursor_print;
  top->dealloc = (dealloc_func *) nsp_mcursor_destroy;
  top->copy  =  (copy_func *) nsp_mcursor_copy;
  top->size  = (size_func *) nsp_mcursor_size;
  top->s_type =  (s_type_func *) nsp_mcursor_type_as_string;
  top->sh_type = (sh_type_func *) nsp_mcursor_type_short_string;
  top->info = (info_func *) nsp_mcursor_info;
  /* top->is_true = (is_true_func  *) nsp_mcursor_is_true; */
  /* top->loop =(loop_func *) nsp_mcursor_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_mcursor_object;
  top->eq  = (eq_func *) nsp_mcursor_eq;
  top->neq  = (eq_func *) nsp_mcursor_neq;
  top->save  = (save_func *) nsp_mcursor_xdr_save;
  top->load  = (load_func *) nsp_mcursor_xdr_load;
  top->create = (create_func*) int_mcursor_create;
  top->latex = (print_func *) nsp_mcursor_latex;
  top->full_copy = (copy_func *) nsp_mcursor_full_copy;

  /* specific methods for mcursor */

  type->init = (init_func *) init_mcursor;

  /* 
   * NspMcursor interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_mcursor_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMcursor called nsp_type_mcursor
       */
      type->id =  nsp_type_mcursor_id = nsp_new_type_id();
      nsp_type_mcursor = type;
      if ( nsp_register_type(nsp_type_mcursor) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_mcursor(mode);
    }
  else 
    {
      type->id = nsp_type_mcursor_id;
      return type;
    }
}

/*
 * initialize NspMcursor instances 
 * locally and by calling initializer on parent class 
 */

static int init_mcursor(NspMcursor *Obj,NspTypeMcursor *type)
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
 * new instance of NspMcursor 
 */

NspMcursor *new_mcursor() 
{
  NspMcursor *loc;
  /* type must exists */
  nsp_type_mcursor = new_type_mcursor(T_BASE);
  if ( (loc = malloc(sizeof(NspMcursor)))== NULLMCURSOR) return loc;
  /* initialize object */
  if ( init_mcursor(loc,nsp_type_mcursor) == FAIL) return NULLMCURSOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspMcursor 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_mcursor_size(NspMcursor *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char mcursor_type_name[]="Mcursor";
static char mcursor_short_type_name[]="mcursor";

static char *nsp_mcursor_type_as_string(void)
{
  return(mcursor_type_name);
}

static char *nsp_mcursor_type_short_string(NspObject *v)
{
  return(mcursor_short_type_name);
}

/*
 * A == B 
 */

static int nsp_mcursor_eq(NspMcursor *A, NspObject *B)
{
  NspMcursor *loc = (NspMcursor *) B;
  if ( check_cast(B,nsp_type_mcursor_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->cu != loc->obj->cu) return FALSE;
  if ( A->obj->doc != loc->obj->doc) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_mcursor_neq(NspMcursor *A, NspObject *B)
{
  return ( nsp_mcursor_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_mcursor_xdr_save(XDR *xdrs, NspMcursor *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_mcursor)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspMcursor  *nsp_mcursor_xdr_load_partial(XDR *xdrs, NspMcursor *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspMcursor  *nsp_mcursor_xdr_load(XDR *xdrs)
{
  NspMcursor *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLMCURSOR;
  if ((H  = nsp_mcursor_create_void(name,(NspTypeBase *) nsp_type_mcursor))== NULLMCURSOR) return H;
  if ( nsp_mcursor_create_partial(H) == FAIL) return NULLMCURSOR;
  if ((H  = nsp_mcursor_xdr_load_partial(xdrs,H))== NULLMCURSOR) return H;
  if ( nsp_mcursor_check_values(H) == FAIL) return NULLMCURSOR;
  return H;
}

/*
 * delete 
 */

void nsp_mcursor_destroy_partial(NspMcursor *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 376 "codegen/bson.override"
  /* verbatim in destroy */
  mongoc_cursor_destroy(H->obj->cu);
  if (H->obj->doc != NULL) bson_destroy(H->obj->doc);

#line 1894 "bson.c"
    FREE(H->obj);
   }
}

void nsp_mcursor_destroy(NspMcursor *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_mcursor_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_mcursor_info(NspMcursor *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLMCURSOR) 
    {
      Sciprintf("Null Pointer NspMcursor \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_mcursor_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_mcursor_print(NspMcursor *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLMCURSOR) 
    {
      Sciprintf("Null Pointer NspMcursor \n");
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
          nsp_mcursor_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_mcursor_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"cu=0x%x\n",M->obj->cu);
  Sciprintf1(indent+2,"doc=0x%x\n",M->obj->doc);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_mcursor_latex(NspMcursor *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_mcursor_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"cu=0x%x\n",M->obj->cu);
  Sciprintf1(indent+2,"doc=0x%x\n",M->obj->doc);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspMcursor objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspMcursor   *nsp_mcursor_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_mcursor_id) == TRUE ) return ((NspMcursor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_mcursor));
  return NULL;
}

int IsMcursorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_mcursor_id);
}

int IsMcursor(NspObject *O)
{
  return nsp_object_type(O,nsp_type_mcursor_id);
}

NspMcursor  *GetMcursorCopy(Stack stack, int i)
{
  if (  GetMcursor(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspMcursor  *GetMcursor(Stack stack, int i)
{
  NspMcursor *M;
  if (( M = nsp_mcursor_object(NthObj(i))) == NULLMCURSOR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspMcursor instance 
 *-----------------------------------------------------*/

static NspMcursor *nsp_mcursor_create_void(const char *name,NspTypeBase *type)
{
 NspMcursor *H  = (type == NULL) ? new_mcursor() : type->new();
 if ( H ==  NULLMCURSOR)
  {
   Sciprintf("No more memory\n");
   return NULLMCURSOR;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLMCURSOR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_mcursor_create_partial(NspMcursor *H)
{
  if((H->obj = calloc(1,sizeof(nsp_mcursor)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->cu = NULL;
  H->obj->doc = NULL;
  return OK;
}

int nsp_mcursor_check_values(NspMcursor *H)
{
  return OK;
}

NspMcursor *nsp_mcursor_create(const char *name,void* cu,void* doc,NspTypeBase *type)
{
  NspMcursor *H  = nsp_mcursor_create_void(name,type);
  if ( H ==  NULLMCURSOR) return NULLMCURSOR;
  if ( nsp_mcursor_create_partial(H) == FAIL) return NULLMCURSOR;
  H->obj->cu = cu;
  H->obj->doc = doc;
  if ( nsp_mcursor_check_values(H) == FAIL) return NULLMCURSOR;
  return H;
}


NspMcursor *nsp_mcursor_create_default(const char *name)
{
 NspMcursor *H  = nsp_mcursor_create_void(name,NULL);
 if ( H ==  NULLMCURSOR) return NULLMCURSOR;
  if ( nsp_mcursor_create_partial(H) == FAIL) return NULLMCURSOR;
 if ( nsp_mcursor_check_values(H) == FAIL) return NULLMCURSOR;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspMcursor *nsp_mcursor_copy_partial(NspMcursor *H,NspMcursor *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspMcursor *nsp_mcursor_copy(NspMcursor *self)
{
  NspMcursor *H  =nsp_mcursor_create_void(NVOID,(NspTypeBase *) nsp_type_mcursor);
  if ( H ==  NULLMCURSOR) return NULLMCURSOR;
  if ( nsp_mcursor_copy_partial(H,self)== NULL) return NULLMCURSOR;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspMcursor *nsp_mcursor_full_copy_partial(NspMcursor *H,NspMcursor *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_mcursor))) == NULL) return NULLMCURSOR;
  H->obj->ref_count=1;
  H->obj->cu = self->obj->cu;
  H->obj->doc = self->obj->doc;
  return H;
}

NspMcursor *nsp_mcursor_full_copy(NspMcursor *self)
{
  NspMcursor *H  =nsp_mcursor_create_void(NVOID,(NspTypeBase *) nsp_type_mcursor);
  if ( H ==  NULLMCURSOR) return NULLMCURSOR;
  if ( nsp_mcursor_full_copy_partial(H,self)== NULL) return NULLMCURSOR;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspMcursor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_mcursor_create(Stack stack, int rhs, int opt, int lhs)
{
  NspMcursor *H;
  CheckStdRhs(0,0);
  /* want to be sure that type mcursor is initialized */
  nsp_type_mcursor = new_type_mcursor(T_BASE);
  if(( H = nsp_mcursor_create_void(NVOID,(NspTypeBase *) nsp_type_mcursor)) == NULLMCURSOR) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_mcursor_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_mcursor_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 382 "codegen/bson.override"

static int _wrap_mongoc_cursor_error(NspMcursor *self,Stack stack,int rhs,int opt,int lhs)
{
  bson_bool_t r;
  bson_error_t error;
  mongoc_cursor_t *cursor = self->obj->cu;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  r= mongoc_cursor_error (cursor, &error);
  if ( r == TRUE && lhs == 0 ) 
    {
      Scierror("Error: %s\n", error.message);
      return RET_BUG;
    }
  if ( lhs == 1) 
    {
      if ( nsp_move_boolean(stack,1,r)==FAIL) return RET_BUG;
      return 1;
    }
  else
    return 0;
}

#line 2153 "bson.c"


#line 407 "codegen/bson.override"

static int _wrap_mongoc_cursor_next(NspMcursor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  NspBson *B;
  const bson_t *doc= self->obj->doc;
  bson_bool_t r;
  mongoc_cursor_t *cursor = self->obj->cu;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  r= mongoc_cursor_next(cursor, &doc);
  if ( r == TRUE ) 
    {
      const bson_t *doc1= bson_copy(doc);
      if(( B = nsp_bson_create(NVOID,(void *) doc1,
			       (NspTypeBase *) nsp_type_bson)) == NULL)
	{
	  Scierror("Error: failed to create a bson object\n");
	  ret = RET_BUG;
	}
      MoveObj(stack,1,(NspObject  *) B);
      ret =1;
    }
  else
    {
      if ( nsp_move_boolean(stack,1,FALSE)==FAIL) return RET_BUG;
      ret=1;
    }
  return ret;
}

#line 2188 "bson.c"


#line 440 "codegen/bson.override"

static int _wrap_mongoc_cursor_more(NspMcursor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = mongoc_cursor_more(self->obj->cu);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}


#line 2203 "bson.c"


static NspMethods mcursor_methods[] = {
  {"error",(nsp_method *) _wrap_mongoc_cursor_error},
  {"next",(nsp_method *) _wrap_mongoc_cursor_next},
  {"more",(nsp_method *) _wrap_mongoc_cursor_more},
  { NULL, NULL}
};

static NspMethods *mcursor_get_methods(void) { return mcursor_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab mcursor_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 182 "codegen/bson.override"

static int _wrap_mclient_create(Stack stack,int rhs,int opt,int lhs)
{
  return int_mclient_create(stack,rhs,opt,lhs);
}

#line 2230 "bson.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Bson_func[]={
  {"mclient_create", _wrap_mclient_create},
  { "bson_create", int_bson_create},
  { NULL, NULL}
};

/* call ith function in the Bson interface */

int Bson_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Bson_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Bson_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Bson_func[i].name;
  *f = Bson_func[i].fonc;
}

#line 460 "codegen/bson.override"

/* loop extraction for bson variable 
 * @str: name to give to created object 
 * @O: a #NspObject pointer to use to store extracted column or a NULL pointer 
 * @O1: a #NspObject from which we must extract columns
 * @i: undice of the column to be extracted 
 * @rep: returned error value.
 */

static NspObject *nsp_bson_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  /*
  NspBson *b = (NspBson *) O1;
  if (b->sym == TRUE)
    {
      Scierror("Error: loop cannot work for symbolic values\n");
      return NULLOBJ;
    }
  if ( O != NULL) 
    {
      b->value->type->loop(str,((NspBson *) O)->value,b->value,i,rep);
      if ((*rep == RET_ENDFOR))
	return NULLOBJ;
      else
	return O;
    }
  else
    {
      char *str1;
      NspObject *Res1;
      NspObject *Res= b->value->type->loop(str,O,b->value,i,rep);
      Res1 = (NspObject *) nsp_bson(Res,FALSE);
      if ((str1 = nsp_string_copy(str)) ==NULL) return NULLOBJ;
      free(((NspBson *) Res1)->varname);
      ((NspBson *) Res1)->varname=str1;
      if (nsp_object_set_name(Res1,str) == FAIL) return NULLOBJ;
      nsp_object_destroy(&Res);
      return Res1;
    }
  */
  return NULL;
}


static int nsp_bson_is_true(void *Obj)
{
  /*
  NspBson *b = (NspBson *) Obj;
  if ( b->sym == TRUE ) 
    {
      Scierror("Error: do not use symbolic variables in conditions (if, while,case) \n");
      return FALSE;
    }
  else
    {
      return  NSP_OBJECT(b->value)->type->is_true(b->value);
    }
  */
  return FALSE;
}

static int nsp_bson_insert(bson_t *b,const char *name, NspObject* Obj)
{
  NspMatrix *M =  (NspMatrix *) Obj;
  if (IsMat(Obj) &&  M->rc_type == 'r' )
    {
      NspMatrix *M =  (NspMatrix *) Obj;
      if ( M->mn == 1)
	{
	  bson_append_double(b, name, -1, M->R[0]);
	}
      else
	{
	  
	}
    }
  else if (IsBMat(Obj) ) 
    {
      NspBMatrix *M =  (NspBMatrix *) Obj;
      if ( M->mn == 1)
	{
	  bson_append_bool(b, name, -1, M->B[0]);
	}
      else
	{
	  
	}
    }
  else if (IsSMat(Obj) ) 
    {
      NspSMatrix *S =  (NspSMatrix *) Obj;
      if ( nsp_smatrix_utf8_validate(S) == FALSE )
	{
	  /* Copy the matrix XXXXXX */
	  if ( nsp_smatrix_to_utf8(S) == FAIL) 
	    {
	      Scierror("Error: expecting utf8 strings in bson_append_string\n");
	      return FAIL;
	    }
	}
      if ( S->mn == 1)
	{
	  bson_append_utf8(b, name, -1, S->S[0],-1);
	}
      else
	{
	  
	}
    }
  return FAIL;
}


static void bson_show(void *self)
{
  const char *key;
  NspBson *B= self;
  bson_t * b = B->obj->b;
  bson_iter_t iter;
  bson_iter_init(&iter, b);
  while (1) 
    {
      if ( bson_iter_next(&iter) == FALSE ) return ;
      key =bson_iter_key(&iter);
       switch (bson_iter_type_unsafe (&iter)) {
       case BSON_TYPE_EOD: 
	 break;
       case BSON_TYPE_DOUBLE:
	 Sciprintf("Key %s=%f\n",key, bson_iter_double(&iter));
	 break;
       case BSON_TYPE_UTF8:
	 {
	   bson_uint32_t len = 0;
	   Sciprintf("Key %s=%s\n",key, bson_iter_utf8(&iter,&len));
	 }
	 break;
       case BSON_TYPE_DOCUMENT:
	 break;
       case BSON_TYPE_ARRAY:
	 break;
       case BSON_TYPE_BINARY:
	 break;
       case BSON_TYPE_UNDEFINED:
	 break;
       case BSON_TYPE_OID:
	 break;
       case BSON_TYPE_BOOL:
	 Sciprintf("Key %s=%d\n",key, bson_iter_bool(&iter));
	 break;
       case BSON_TYPE_DATE_TIME:
	 break;
       case BSON_TYPE_NULL:
	 break;
       case BSON_TYPE_REGEX:
	 break;
       case BSON_TYPE_DBPOINTER:
	 break;
       case BSON_TYPE_CODE:
	 break;
       case BSON_TYPE_SYMBOL:
	 break;
       case BSON_TYPE_CODEWSCOPE:
	 break;
       case BSON_TYPE_INT32:
	 break;
       case BSON_TYPE_TIMESTAMP:
	 break;
       case BSON_TYPE_INT64:
	 break;
       case BSON_TYPE_MAXKEY:
	 break;
       case BSON_TYPE_MINKEY:
	 break;
       default:
	 break;
       }
    }
}

static NspHash *bson_to_hash(void *self)
{
  const char *key;
  NspBson *B= self;
  bson_t * b = B->obj->b;
  bson_iter_t iter;
  NspHash *H;
  NspObject *O;
  if(( H = nsp_hash_create(NVOID,10)) == NULLHASH) return NULLHASH;
  bson_iter_init(&iter, b);
  while (1) 
    {
      if ( bson_iter_next(&iter) == FALSE ) break;

      key =bson_iter_key(&iter);
      switch (bson_iter_type_unsafe (&iter)) 
	{
	case BSON_TYPE_EOD: 
	  break;
	case BSON_TYPE_DOUBLE:
	  if (( O =  (NspObject *) nsp_create_object_from_double(key,bson_iter_double(&iter))) == NULLOBJ ) 
	    return  NULLHASH;
	  if (nsp_hash_enter(H,O) == FAIL) return NULLHASH;
	  break;
	case BSON_TYPE_UTF8:
	  {
	    bson_uint32_t len = 0;
	    NspSMatrix *Loc;
	    if ( ( Loc =nsp_smatrix_create_with_length(key,1,1,-1) ) == NULLSMAT) 
	      return NULLHASH;
	    if ((Loc->S[0] =nsp_string_copy( bson_iter_utf8(&iter,&len))) == (nsp_string) 0) 
	      return NULLHASH;
	    if (nsp_hash_enter(H,NSP_OBJECT(Loc)) == FAIL)
	      return NULLHASH;
	  }
	  break;
	case BSON_TYPE_DOCUMENT:
	  break;
	case BSON_TYPE_ARRAY:
	  break;
	case BSON_TYPE_BINARY:
	  break;
	case BSON_TYPE_UNDEFINED:
	  break;
	case BSON_TYPE_OID:
	  break;
	case BSON_TYPE_BOOL:
	  if (( O =  (NspObject *) nsp_create_boolean_object(key, bson_iter_bool(&iter))) == NULLOBJ ) 
	    return  NULLHASH;
	  if (nsp_hash_enter(H,O) == FAIL) return NULLHASH;
	  break;
	  break;
	case BSON_TYPE_DATE_TIME:
	  break;
	case BSON_TYPE_NULL:
	  break;
	case BSON_TYPE_REGEX:
	  break;
	case BSON_TYPE_DBPOINTER:
	  break;
	case BSON_TYPE_CODE:
	  break;
	case BSON_TYPE_SYMBOL:
	  break;
	case BSON_TYPE_CODEWSCOPE:
	  break;
	case BSON_TYPE_INT32:
	  break;
	case BSON_TYPE_TIMESTAMP:
	  break;
	case BSON_TYPE_INT64:
	  break;
	case BSON_TYPE_MAXKEY:
	  break;
	case BSON_TYPE_MINKEY:
	  break;
	default:
	  break;
	}
    }
  return H;
}


#line 2524 "bson.c"
