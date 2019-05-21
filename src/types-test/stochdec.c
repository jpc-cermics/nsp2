/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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





#line 42 "codegen/stochdec.override"
/* headers */

#line 31 "stochdec.c"

/* -----------NspStochdec ----------- */


#define  NspStochdec_Private 
#include <nsp/objects.h>
#include <nsp/stochdec.h>
#include <nsp/interf.h>

/* 
 * NspStochdec inherits from Object 
 */

int nsp_type_stochdec_id=0;
NspTypeStochdec *nsp_type_stochdec=NULL;

/*
 * Type object for NspStochdec 
 * all the instance of NspTypeStochdec share the same id. 
 * nsp_type_stochdec: is an instance of NspTypeStochdec 
 *    used for objects of NspStochdec type (i.e built with new_stochdec) 
 * other instances are used for derived classes 
 */
NspTypeStochdec *new_type_stochdec(type_mode mode)
{
  NspTypeStochdec *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_stochdec != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_stochdec;
    }
  if (( type =  malloc(sizeof(NspTypeStochdec))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = stochdec_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = stochdec_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_stochdec;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for stochdec */ 

  top->pr = (print_func *) nsp_stochdec_print;
  top->dealloc = (dealloc_func *) nsp_stochdec_destroy;
  top->copy  =  (copy_func *) nsp_stochdec_copy;
  top->size  = (size_func *) nsp_stochdec_size;
  top->s_type =  (s_type_func *) nsp_stochdec_type_as_string;
  top->sh_type = (sh_type_func *) nsp_stochdec_type_short_string;
  top->info = (info_func *) nsp_stochdec_info;
  /* top->is_true = (is_true_func  *) nsp_stochdec_is_true; */
  /* top->loop =(loop_func *) nsp_stochdec_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_stochdec_object;
  top->eq  = (eq_func *) nsp_stochdec_eq;
  top->neq  = (eq_func *) nsp_stochdec_neq;
  top->save  = (save_func *) nsp_stochdec_xdr_save;
  top->load  = (load_func *) nsp_stochdec_xdr_load;
  top->create = (create_func*) int_stochdec_create;
  top->latex = (print_func *) nsp_stochdec_latex;
  top->full_copy = (copy_func *) nsp_stochdec_full_copy;

  /* specific methods for stochdec */

  type->init = (init_func *) init_stochdec;

  /* 
   * NspStochdec interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_stochdec_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeStochdec called nsp_type_stochdec
       */
      type->id =  nsp_type_stochdec_id = nsp_new_type_id();
      nsp_type_stochdec = type;
      if ( nsp_register_type(nsp_type_stochdec) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_stochdec(mode);
    }
  else 
    {
      type->id = nsp_type_stochdec_id;
      return type;
    }
}

/*
 * initialize NspStochdec instances 
 * locally and by calling initializer on parent class 
 */

static int init_stochdec(NspStochdec *Obj,NspTypeStochdec *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->xdim = 0;
 return OK;
}

/*
 * new instance of NspStochdec 
 */

NspStochdec *new_stochdec() 
{
  NspStochdec *loc;
  /* type must exists */
  nsp_type_stochdec = new_type_stochdec(T_BASE);
  if ( (loc = malloc(sizeof(NspStochdec)))== NULLSTOCHDEC) return loc;
  /* initialize object */
  if ( init_stochdec(loc,nsp_type_stochdec) == FAIL) return NULLSTOCHDEC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspStochdec 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_stochdec_size(NspStochdec *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char stochdec_type_name[]="Stochdec";
static char stochdec_short_type_name[]="stochdec";

static char *nsp_stochdec_type_as_string(void)
{
  return(stochdec_type_name);
}

static char *nsp_stochdec_type_short_string(NspObject *v)
{
  return(stochdec_short_type_name);
}

/*
 * A == B 
 */

static int nsp_stochdec_eq(NspStochdec *A, NspObject *B)
{
  NspStochdec *loc = (NspStochdec *) B;
  if ( check_cast(B,nsp_type_stochdec_id) == FALSE) return FALSE ;
  if ( A->xdim != loc->xdim) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_stochdec_neq(NspStochdec *A, NspObject *B)
{
  return ( nsp_stochdec_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_stochdec_xdr_save(XDR *xdrs, NspStochdec *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_stochdec)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->xdim) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspStochdec  *nsp_stochdec_xdr_load_partial(XDR *xdrs, NspStochdec *M)
{
  if (nsp_xdr_load_i(xdrs, &M->xdim) == FAIL) return NULL;
 return M;
}

static NspStochdec  *nsp_stochdec_xdr_load(XDR *xdrs)
{
  NspStochdec *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSTOCHDEC;
  if ((H  = nsp_stochdec_create_void(name,(NspTypeBase *) nsp_type_stochdec))== NULLSTOCHDEC) return H;
  if ( nsp_stochdec_create_partial(H) == FAIL) return NULLSTOCHDEC;
  if ((H  = nsp_stochdec_xdr_load_partial(xdrs,H))== NULLSTOCHDEC) return H;
  if ( nsp_stochdec_check_values(H) == FAIL) return NULLSTOCHDEC;
  return H;
}

/*
 * delete 
 */

void nsp_stochdec_destroy_partial(NspStochdec *H)
{
#line 63 "codegen/stochdec.override"
   /* verbatim in destroy */

#line 255 "stochdec.c"
}

void nsp_stochdec_destroy(NspStochdec *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_stochdec_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_stochdec_info(NspStochdec *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSTOCHDEC) 
    {
      Sciprintf("Null Pointer NspStochdec \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_stochdec_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_stochdec_print(NspStochdec *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSTOCHDEC) 
    {
      Sciprintf("Null Pointer NspStochdec \n");
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
          nsp_stochdec_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_stochdec_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"xdim=%d\n", M->xdim);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_stochdec_latex(NspStochdec *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_stochdec_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|xdim|= \\numprint{%d}\n",M->xdim);
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
 * for NspStochdec objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspStochdec   *nsp_stochdec_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_stochdec_id)  == TRUE  ) return ((NspStochdec *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_stochdec));
  return NULL;
}

int IsStochdecObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_stochdec_id);
}

int IsStochdec(NspObject *O)
{
  return nsp_object_type(O,nsp_type_stochdec_id);
}

NspStochdec  *GetStochdecCopy(Stack stack, int i)
{
  if (  GetStochdec(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspStochdec  *GetStochdec(Stack stack, int i)
{
  NspStochdec *M;
  if (( M = nsp_stochdec_object(NthObj(i))) == NULLSTOCHDEC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspStochdec instance 
 *-----------------------------------------------------*/

static NspStochdec *nsp_stochdec_create_void(const char *name,NspTypeBase *type)
{
 NspStochdec *H  = (type == NULL) ? new_stochdec() : type->new();
 if ( H ==  NULLSTOCHDEC)
  {
   Sciprintf("No more memory\n");
   return NULLSTOCHDEC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSTOCHDEC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_stochdec_create_partial(NspStochdec *H)
{
  return OK;
}

int nsp_stochdec_check_values(NspStochdec *H)
{
  return OK;
}

NspStochdec *nsp_stochdec_create(const char *name,int xdim,NspTypeBase *type)
{
  NspStochdec *H  = nsp_stochdec_create_void(name,type);
  if ( H ==  NULLSTOCHDEC) return NULLSTOCHDEC;
  H->xdim=xdim;
  if ( nsp_stochdec_check_values(H) == FAIL) return NULLSTOCHDEC;
  return H;
}


NspStochdec *nsp_stochdec_create_default(const char *name)
{
 NspStochdec *H  = nsp_stochdec_create_void(name,NULL);
 if ( H ==  NULLSTOCHDEC) return NULLSTOCHDEC;
  if ( nsp_stochdec_check_values(H) == FAIL) return NULLSTOCHDEC;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspStochdec *nsp_stochdec_copy_partial(NspStochdec *H,NspStochdec *self)
{
  H->xdim=self->xdim;
  return H;
}

NspStochdec *nsp_stochdec_copy(NspStochdec *self)
{
  NspStochdec *H  =nsp_stochdec_create_void(NVOID,(NspTypeBase *) nsp_type_stochdec);
  if ( H ==  NULLSTOCHDEC) return NULLSTOCHDEC;
  if ( nsp_stochdec_copy_partial(H,self)== NULL) return NULLSTOCHDEC;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspStochdec *nsp_stochdec_full_copy_partial(NspStochdec *H,NspStochdec *self)
{
  H->xdim=self->xdim;
  return H;
}

NspStochdec *nsp_stochdec_full_copy(NspStochdec *self)
{
  NspStochdec *H  =nsp_stochdec_create_void(NVOID,(NspTypeBase *) nsp_type_stochdec);
  if ( H ==  NULLSTOCHDEC) return NULLSTOCHDEC;
  if ( nsp_stochdec_full_copy_partial(H,self)== NULL) return NULLSTOCHDEC;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspStochdec
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_stochdec_create(Stack stack, int rhs, int opt, int lhs)
{
  NspStochdec *H;
  CheckStdRhs(0,0);
  /* want to be sure that type stochdec is initialized */
  nsp_type_stochdec = new_type_stochdec(T_BASE);
  if(( H = nsp_stochdec_create_void(NVOID,(NspTypeBase *) nsp_type_stochdec)) == NULLSTOCHDEC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_stochdec_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *stochdec_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_stochdec_get_xdim(void *self,const char *attr)
{
  int ret;
  ret = ((NspStochdec *) self)->xdim;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_stochdec_set_xdim(void *self,const char *attr, NspObject *O)
{
  int xdim;
  if ( IntScalar(O,&xdim) == FAIL) return FAIL;
  ((NspStochdec *) self)->xdim= xdim;
  return OK;
}

static AttrTab stochdec_attrs[] = {
  { "xdim", (attr_get_function * )_wrap_stochdec_get_xdim, (attr_set_function * )_wrap_stochdec_set_xdim, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};



/* -----------NspValueFn ----------- */


#define  NspValueFn_Private 
#include <nsp/objects.h>
#include <nsp/valuefn.h>
#include <nsp/interf.h>

/* 
 * NspValueFn inherits from Object 
 */

int nsp_type_valuefn_id=0;
NspTypeValueFn *nsp_type_valuefn=NULL;

/*
 * Type object for NspValueFn 
 * all the instance of NspTypeValueFn share the same id. 
 * nsp_type_valuefn: is an instance of NspTypeValueFn 
 *    used for objects of NspValueFn type (i.e built with new_valuefn) 
 * other instances are used for derived classes 
 */
NspTypeValueFn *new_type_valuefn(type_mode mode)
{
  NspTypeValueFn *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_valuefn != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_valuefn;
    }
  if (( type =  malloc(sizeof(NspTypeValueFn))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = valuefn_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = valuefn_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_valuefn;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for valuefn */ 

  top->pr = (print_func *) nsp_valuefn_print;
  top->dealloc = (dealloc_func *) nsp_valuefn_destroy;
  top->copy  =  (copy_func *) nsp_valuefn_copy;
  top->size  = (size_func *) nsp_valuefn_size;
  top->s_type =  (s_type_func *) nsp_valuefn_type_as_string;
  top->sh_type = (sh_type_func *) nsp_valuefn_type_short_string;
  top->info = (info_func *) nsp_valuefn_info;
  /* top->is_true = (is_true_func  *) nsp_valuefn_is_true; */
  /* top->loop =(loop_func *) nsp_valuefn_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_valuefn_object;
  top->eq  = (eq_func *) nsp_valuefn_eq;
  top->neq  = (eq_func *) nsp_valuefn_neq;
  top->save  = (save_func *) nsp_valuefn_xdr_save;
  top->load  = (load_func *) nsp_valuefn_xdr_load;
  top->create = (create_func*) int_valuefn_create;
  top->latex = (print_func *) nsp_valuefn_latex;
  top->full_copy = (copy_func *) nsp_valuefn_full_copy;

  /* specific methods for valuefn */

  type->init = (init_func *) init_valuefn;

  /* 
   * NspValueFn interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_valuefn_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeValueFn called nsp_type_valuefn
       */
      type->id =  nsp_type_valuefn_id = nsp_new_type_id();
      nsp_type_valuefn = type;
      if ( nsp_register_type(nsp_type_valuefn) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_valuefn(mode);
    }
  else 
    {
      type->id = nsp_type_valuefn_id;
      return type;
    }
}

/*
 * initialize NspValueFn instances 
 * locally and by calling initializer on parent class 
 */

static int init_valuefn(NspValueFn *Obj,NspTypeValueFn *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->xdim = 0;
  Obj->xmin = NULLMAT;
  Obj->xmax = NULLMAT;
 return OK;
}

/*
 * new instance of NspValueFn 
 */

NspValueFn *new_valuefn() 
{
  NspValueFn *loc;
  /* type must exists */
  nsp_type_valuefn = new_type_valuefn(T_BASE);
  if ( (loc = malloc(sizeof(NspValueFn)))== NULLVALUEFN) return loc;
  /* initialize object */
  if ( init_valuefn(loc,nsp_type_valuefn) == FAIL) return NULLVALUEFN;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspValueFn 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_valuefn_size(NspValueFn *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char valuefn_type_name[]="ValueFn";
static char valuefn_short_type_name[]="valuefn";

static char *nsp_valuefn_type_as_string(void)
{
  return(valuefn_type_name);
}

static char *nsp_valuefn_type_short_string(NspObject *v)
{
  return(valuefn_short_type_name);
}

/*
 * A == B 
 */

static int nsp_valuefn_eq(NspValueFn *A, NspObject *B)
{
  NspValueFn *loc = (NspValueFn *) B;
  if ( check_cast(B,nsp_type_valuefn_id) == FALSE) return FALSE ;
  if ( A->xdim != loc->xdim) return FALSE;
  if ( NSP_OBJECT(A->xmin)->type->eq(A->xmin,loc->xmin) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->xmax)->type->eq(A->xmax,loc->xmax) == FALSE ) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_valuefn_neq(NspValueFn *A, NspObject *B)
{
  return ( nsp_valuefn_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_valuefn_xdr_save(XDR *xdrs, NspValueFn *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_valuefn)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->xdim) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspValueFn  *nsp_valuefn_xdr_load_partial(XDR *xdrs, NspValueFn *M)
{
  if (nsp_xdr_load_i(xdrs, &M->xdim) == FAIL) return NULL;
 return M;
}

static NspValueFn  *nsp_valuefn_xdr_load(XDR *xdrs)
{
  NspValueFn *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLVALUEFN;
  if ((H  = nsp_valuefn_create_void(name,(NspTypeBase *) nsp_type_valuefn))== NULLVALUEFN) return H;
  if ( nsp_valuefn_create_partial(H) == FAIL) return NULLVALUEFN;
  if ((H  = nsp_valuefn_xdr_load_partial(xdrs,H))== NULLVALUEFN) return H;
  if ( nsp_valuefn_check_values(H) == FAIL) return NULLVALUEFN;
  return H;
}

/*
 * delete 
 */

void nsp_valuefn_destroy_partial(NspValueFn *H)
{
  if ( H->xmin != NULL ) 
    nsp_matrix_destroy(H->xmin);
  if ( H->xmax != NULL ) 
    nsp_matrix_destroy(H->xmax);
}

void nsp_valuefn_destroy(NspValueFn *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_valuefn_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_valuefn_info(NspValueFn *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLVALUEFN) 
    {
      Sciprintf("Null Pointer NspValueFn \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_valuefn_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_valuefn_print(NspValueFn *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLVALUEFN) 
    {
      Sciprintf("Null Pointer NspValueFn \n");
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
          nsp_valuefn_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_valuefn_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"xdim=%d\n", M->xdim);
  if ( M->xmin != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->xmin),indent+2,"xmin", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->xmax != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->xmax),indent+2,"xmax", rec_level+1)== FALSE ) return FALSE ;
    }
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_valuefn_latex(NspValueFn *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_valuefn_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|xdim|= \\numprint{%d}\n",M->xdim);
  Sciprintf1(2,"\\\\\n");
  if ( M->xmin != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->xmin),FALSE,"xmin", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->xmax != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->xmax),FALSE,"xmax", rec_level+1)== FALSE ) return FALSE ;
    }
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
 * for NspValueFn objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspValueFn   *nsp_valuefn_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_valuefn_id)  == TRUE  ) return ((NspValueFn *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_valuefn));
  return NULL;
}

int IsValueFnObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_valuefn_id);
}

int IsValueFn(NspObject *O)
{
  return nsp_object_type(O,nsp_type_valuefn_id);
}

NspValueFn  *GetValueFnCopy(Stack stack, int i)
{
  if (  GetValueFn(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspValueFn  *GetValueFn(Stack stack, int i)
{
  NspValueFn *M;
  if (( M = nsp_valuefn_object(NthObj(i))) == NULLVALUEFN)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspValueFn instance 
 *-----------------------------------------------------*/

static NspValueFn *nsp_valuefn_create_void(const char *name,NspTypeBase *type)
{
 NspValueFn *H  = (type == NULL) ? new_valuefn() : type->new();
 if ( H ==  NULLVALUEFN)
  {
   Sciprintf("No more memory\n");
   return NULLVALUEFN;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLVALUEFN;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_valuefn_create_partial(NspValueFn *H)
{
  return OK;
}

int nsp_valuefn_check_values(NspValueFn *H)
{
  if ( H->xmin == NULLMAT) 
    {
       if (( H->xmin = nsp_matrix_create("xmin",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->xmax == NULLMAT) 
    {
       if (( H->xmax = nsp_matrix_create("xmax",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  return OK;
}

NspValueFn *nsp_valuefn_create(const char *name,int xdim,NspMatrix* xmin,NspMatrix* xmax,NspTypeBase *type)
{
  NspValueFn *H  = nsp_valuefn_create_void(name,type);
  if ( H ==  NULLVALUEFN) return NULLVALUEFN;
  H->xdim=xdim;
  H->xmin= xmin;
  H->xmax= xmax;
  if ( nsp_valuefn_check_values(H) == FAIL) return NULLVALUEFN;
  return H;
}


NspValueFn *nsp_valuefn_create_default(const char *name)
{
 NspValueFn *H  = nsp_valuefn_create_void(name,NULL);
 if ( H ==  NULLVALUEFN) return NULLVALUEFN;
  if ( nsp_valuefn_check_values(H) == FAIL) return NULLVALUEFN;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspValueFn *nsp_valuefn_copy_partial(NspValueFn *H,NspValueFn *self)
{
  H->xdim=self->xdim;
  if ( self->xmin == NULL )
    { H->xmin = NULL;}
  else
    {
      if ((H->xmin = (NspMatrix *) nsp_object_copy_and_name("xmin", NSP_OBJECT(self->xmin))) == NULLMAT) return NULL;
    }
  if ( self->xmax == NULL )
    { H->xmax = NULL;}
  else
    {
      if ((H->xmax = (NspMatrix *) nsp_object_copy_and_name("xmax", NSP_OBJECT(self->xmax))) == NULLMAT) return NULL;
    }
  return H;
}

NspValueFn *nsp_valuefn_copy(NspValueFn *self)
{
  NspValueFn *H  =nsp_valuefn_create_void(NVOID,(NspTypeBase *) nsp_type_valuefn);
  if ( H ==  NULLVALUEFN) return NULLVALUEFN;
  if ( nsp_valuefn_copy_partial(H,self)== NULL) return NULLVALUEFN;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspValueFn *nsp_valuefn_full_copy_partial(NspValueFn *H,NspValueFn *self)
{
  H->xdim=self->xdim;
  if ( self->xmin == NULL )
    { H->xmin = NULL;}
  else
    {
      if ((H->xmin = (NspMatrix *) nsp_object_full_copy_and_name("xmin", NSP_OBJECT(self->xmin))) == NULLMAT) return NULL;
    }
  if ( self->xmax == NULL )
    { H->xmax = NULL;}
  else
    {
      if ((H->xmax = (NspMatrix *) nsp_object_full_copy_and_name("xmax", NSP_OBJECT(self->xmax))) == NULLMAT) return NULL;
    }
  return H;
}

NspValueFn *nsp_valuefn_full_copy(NspValueFn *self)
{
  NspValueFn *H  =nsp_valuefn_create_void(NVOID,(NspTypeBase *) nsp_type_valuefn);
  if ( H ==  NULLVALUEFN) return NULLVALUEFN;
  if ( nsp_valuefn_full_copy_partial(H,self)== NULL) return NULLVALUEFN;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspValueFn
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_valuefn_create(Stack stack, int rhs, int opt, int lhs)
{
  NspValueFn *H;
  CheckStdRhs(0,0);
  /* want to be sure that type valuefn is initialized */
  nsp_type_valuefn = new_type_valuefn(T_BASE);
  if(( H = nsp_valuefn_create_void(NVOID,(NspTypeBase *) nsp_type_valuefn)) == NULLVALUEFN) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_valuefn_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *valuefn_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_valuefn_get_xdim(void *self,const char *attr)
{
  int ret;
  ret = ((NspValueFn *) self)->xdim;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_valuefn_set_xdim(void *self,const char *attr, NspObject *O)
{
  int xdim;
  if ( IntScalar(O,&xdim) == FAIL) return FAIL;
  ((NspValueFn *) self)->xdim= xdim;
  return OK;
}

static AttrTab valuefn_attrs[] = {
  { "xdim", (attr_get_function * )_wrap_valuefn_get_xdim, (attr_set_function * )_wrap_valuefn_set_xdim, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};



/* -----------NspGridValueFn ----------- */


#define  NspGridValueFn_Private 
#include <nsp/objects.h>
#include <nsp/gridvaluefn.h>
#include <nsp/interf.h>

/* 
 * NspGridValueFn inherits from ValueFn 
 */

int nsp_type_gridvaluefn_id=0;
NspTypeGridValueFn *nsp_type_gridvaluefn=NULL;

/*
 * Type object for NspGridValueFn 
 * all the instance of NspTypeGridValueFn share the same id. 
 * nsp_type_gridvaluefn: is an instance of NspTypeGridValueFn 
 *    used for objects of NspGridValueFn type (i.e built with new_gridvaluefn) 
 * other instances are used for derived classes 
 */
NspTypeGridValueFn *new_type_gridvaluefn(type_mode mode)
{
  NspTypeGridValueFn *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gridvaluefn != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gridvaluefn;
    }
  if (( type =  malloc(sizeof(NspTypeGridValueFn))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_valuefn(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gridvaluefn_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gridvaluefn_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_gridvaluefn;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gridvaluefn */ 

  top->pr = (print_func *) nsp_gridvaluefn_print;
  top->dealloc = (dealloc_func *) nsp_gridvaluefn_destroy;
  top->copy  =  (copy_func *) nsp_gridvaluefn_copy;
  top->size  = (size_func *) nsp_gridvaluefn_size;
  top->s_type =  (s_type_func *) nsp_gridvaluefn_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gridvaluefn_type_short_string;
  top->info = (info_func *) nsp_gridvaluefn_info;
  /* top->is_true = (is_true_func  *) nsp_gridvaluefn_is_true; */
  /* top->loop =(loop_func *) nsp_gridvaluefn_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gridvaluefn_object;
  top->eq  = (eq_func *) nsp_gridvaluefn_eq;
  top->neq  = (eq_func *) nsp_gridvaluefn_neq;
  top->save  = (save_func *) nsp_gridvaluefn_xdr_save;
  top->load  = (load_func *) nsp_gridvaluefn_xdr_load;
  top->create = (create_func*) int_gridvaluefn_create;
  top->latex = (print_func *) nsp_gridvaluefn_latex;
  top->full_copy = (copy_func *) nsp_gridvaluefn_full_copy;

  /* specific methods for gridvaluefn */

  type->init = (init_func *) init_gridvaluefn;

  /* 
   * NspGridValueFn interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gridvaluefn_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGridValueFn called nsp_type_gridvaluefn
       */
      type->id =  nsp_type_gridvaluefn_id = nsp_new_type_id();
      nsp_type_gridvaluefn = type;
      if ( nsp_register_type(nsp_type_gridvaluefn) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gridvaluefn(mode);
    }
  else 
    {
      type->id = nsp_type_gridvaluefn_id;
      return type;
    }
}

/*
 * initialize NspGridValueFn instances 
 * locally and by calling initializer on parent class 
 */

static int init_gridvaluefn(NspGridValueFn *Obj,NspTypeGridValueFn *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->n = NULLMAT;
  Obj->step = NULLMAT;
  Obj->values = NULLMAT;
 return OK;
}

/*
 * new instance of NspGridValueFn 
 */

NspGridValueFn *new_gridvaluefn() 
{
  NspGridValueFn *loc;
  /* type must exists */
  nsp_type_gridvaluefn = new_type_gridvaluefn(T_BASE);
  if ( (loc = malloc(sizeof(NspGridValueFn)))== NULLGRIDVALUEFN) return loc;
  /* initialize object */
  if ( init_gridvaluefn(loc,nsp_type_gridvaluefn) == FAIL) return NULLGRIDVALUEFN;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGridValueFn 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gridvaluefn_size(NspGridValueFn *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char gridvaluefn_type_name[]="GridValueFn";
static char gridvaluefn_short_type_name[]="gridvaluefn";

static char *nsp_gridvaluefn_type_as_string(void)
{
  return(gridvaluefn_type_name);
}

static char *nsp_gridvaluefn_type_short_string(NspObject *v)
{
  return(gridvaluefn_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gridvaluefn_eq(NspGridValueFn *A, NspObject *B)
{
  NspGridValueFn *loc = (NspGridValueFn *) B;
  if ( check_cast(B,nsp_type_gridvaluefn_id) == FALSE) return FALSE ;
  if ( NSP_OBJECT(A->n)->type->eq(A->n,loc->n) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->step)->type->eq(A->step,loc->step) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->values)->type->eq(A->values,loc->values) == FALSE ) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_gridvaluefn_neq(NspGridValueFn *A, NspObject *B)
{
  return ( nsp_gridvaluefn_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gridvaluefn_xdr_save(XDR *xdrs, NspGridValueFn *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gridvaluefn)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if ( nsp_valuefn_xdr_save(xdrs, (NspValueFn * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGridValueFn  *nsp_gridvaluefn_xdr_load_partial(XDR *xdrs, NspGridValueFn *M)
{
  int fid;
  char name[NAME_MAXL];
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_valuefn_xdr_load_partial(xdrs,(NspValueFn * )M) == NULL) return NULL;
 return M;
}

static NspGridValueFn  *nsp_gridvaluefn_xdr_load(XDR *xdrs)
{
  NspGridValueFn *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRIDVALUEFN;
  if ((H  = nsp_gridvaluefn_create_void(name,(NspTypeBase *) nsp_type_gridvaluefn))== NULLGRIDVALUEFN) return H;
  if ( nsp_gridvaluefn_create_partial(H) == FAIL) return NULLGRIDVALUEFN;
  if ((H  = nsp_gridvaluefn_xdr_load_partial(xdrs,H))== NULLGRIDVALUEFN) return H;
  if ( nsp_gridvaluefn_check_values(H) == FAIL) return NULLGRIDVALUEFN;
  return H;
}

/*
 * delete 
 */

void nsp_gridvaluefn_destroy_partial(NspGridValueFn *H)
{
  nsp_valuefn_destroy_partial((NspValueFn * ) H);
  if ( H->n != NULL ) 
    nsp_matrix_destroy(H->n);
  if ( H->step != NULL ) 
    nsp_matrix_destroy(H->step);
  if ( H->values != NULL ) 
    nsp_matrix_destroy(H->values);
}

void nsp_gridvaluefn_destroy(NspGridValueFn *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gridvaluefn_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gridvaluefn_info(NspGridValueFn *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRIDVALUEFN) 
    {
      Sciprintf("Null Pointer NspGridValueFn \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gridvaluefn_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gridvaluefn_print(NspGridValueFn *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRIDVALUEFN) 
    {
      Sciprintf("Null Pointer NspGridValueFn \n");
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
          nsp_gridvaluefn_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_gridvaluefn_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
  if ( M->n != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->n),indent+2,"n", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->step != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->step),indent+2,"step", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->values != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->values),indent+2,"values", rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_valuefn_print((NspValueFn * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gridvaluefn_latex(NspGridValueFn *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gridvaluefn_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  if ( M->n != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->n),FALSE,"n", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->step != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->step),FALSE,"step", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->values != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->values),FALSE,"values", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  nsp_valuefn_latex((NspValueFn * ) M, FALSE,NULL,rec_level);
  Sciprintf1(indent+1,"\n");
  Sciprintf("\\end{array}\n");

  Sciprintf("\\right.\n");

  if ( use_math ) Sciprintf("\\end{equation*}\n");

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGridValueFn objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGridValueFn   *nsp_gridvaluefn_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gridvaluefn_id)  == TRUE  ) return ((NspGridValueFn *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gridvaluefn));
  return NULL;
}

int IsGridValueFnObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gridvaluefn_id);
}

int IsGridValueFn(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gridvaluefn_id);
}

NspGridValueFn  *GetGridValueFnCopy(Stack stack, int i)
{
  if (  GetGridValueFn(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGridValueFn  *GetGridValueFn(Stack stack, int i)
{
  NspGridValueFn *M;
  if (( M = nsp_gridvaluefn_object(NthObj(i))) == NULLGRIDVALUEFN)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGridValueFn instance 
 *-----------------------------------------------------*/

static NspGridValueFn *nsp_gridvaluefn_create_void(const char *name,NspTypeBase *type)
{
 NspGridValueFn *H  = (type == NULL) ? new_gridvaluefn() : type->new();
 if ( H ==  NULLGRIDVALUEFN)
  {
   Sciprintf("No more memory\n");
   return NULLGRIDVALUEFN;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRIDVALUEFN;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gridvaluefn_create_partial(NspGridValueFn *H)
{
  return OK;
}

int nsp_gridvaluefn_check_values(NspGridValueFn *H)
{
  if ( H->n == NULLMAT) 
    {
       if (( H->n = nsp_matrix_create("n",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->step == NULLMAT) 
    {
       if (( H->step = nsp_matrix_create("step",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->values == NULLMAT) 
    {
       if (( H->values = nsp_matrix_create("values",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_valuefn_check_values((NspValueFn * ) H);
  return OK;
}

NspGridValueFn *nsp_gridvaluefn_create(const char *name,NspMatrix* n,NspMatrix* step,NspMatrix* values,NspTypeBase *type)
{
  NspGridValueFn *H  = nsp_gridvaluefn_create_void(name,type);
  if ( H ==  NULLGRIDVALUEFN) return NULLGRIDVALUEFN;
  H->n= n;
  H->step= step;
  H->values= values;
  if ( nsp_gridvaluefn_check_values(H) == FAIL) return NULLGRIDVALUEFN;
  return H;
}


NspGridValueFn *nsp_gridvaluefn_create_default(const char *name)
{
 NspGridValueFn *H  = nsp_gridvaluefn_create_void(name,NULL);
 if ( H ==  NULLGRIDVALUEFN) return NULLGRIDVALUEFN;
  if ( nsp_gridvaluefn_check_values(H) == FAIL) return NULLGRIDVALUEFN;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGridValueFn *nsp_gridvaluefn_copy_partial(NspGridValueFn *H,NspGridValueFn *self)
{
  if ( nsp_valuefn_copy_partial((NspValueFn *) H,(NspValueFn * ) self ) == NULL) return NULLGRIDVALUEFN;
  if ( self->n == NULL )
    { H->n = NULL;}
  else
    {
      if ((H->n = (NspMatrix *) nsp_object_copy_and_name("n", NSP_OBJECT(self->n))) == NULLMAT) return NULL;
    }
  if ( self->step == NULL )
    { H->step = NULL;}
  else
    {
      if ((H->step = (NspMatrix *) nsp_object_copy_and_name("step", NSP_OBJECT(self->step))) == NULLMAT) return NULL;
    }
  if ( self->values == NULL )
    { H->values = NULL;}
  else
    {
      if ((H->values = (NspMatrix *) nsp_object_copy_and_name("values", NSP_OBJECT(self->values))) == NULLMAT) return NULL;
    }
  return H;
}

NspGridValueFn *nsp_gridvaluefn_copy(NspGridValueFn *self)
{
  NspGridValueFn *H  =nsp_gridvaluefn_create_void(NVOID,(NspTypeBase *) nsp_type_gridvaluefn);
  if ( H ==  NULLGRIDVALUEFN) return NULLGRIDVALUEFN;
  if ( nsp_gridvaluefn_copy_partial(H,self)== NULL) return NULLGRIDVALUEFN;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGridValueFn *nsp_gridvaluefn_full_copy_partial(NspGridValueFn *H,NspGridValueFn *self)
{
  if ( nsp_valuefn_full_copy_partial((NspValueFn *) H,(NspValueFn * ) self ) == NULL) return NULLGRIDVALUEFN;
  if ( self->n == NULL )
    { H->n = NULL;}
  else
    {
      if ((H->n = (NspMatrix *) nsp_object_full_copy_and_name("n", NSP_OBJECT(self->n))) == NULLMAT) return NULL;
    }
  if ( self->step == NULL )
    { H->step = NULL;}
  else
    {
      if ((H->step = (NspMatrix *) nsp_object_full_copy_and_name("step", NSP_OBJECT(self->step))) == NULLMAT) return NULL;
    }
  if ( self->values == NULL )
    { H->values = NULL;}
  else
    {
      if ((H->values = (NspMatrix *) nsp_object_full_copy_and_name("values", NSP_OBJECT(self->values))) == NULLMAT) return NULL;
    }
  return H;
}

NspGridValueFn *nsp_gridvaluefn_full_copy(NspGridValueFn *self)
{
  NspGridValueFn *H  =nsp_gridvaluefn_create_void(NVOID,(NspTypeBase *) nsp_type_gridvaluefn);
  if ( H ==  NULLGRIDVALUEFN) return NULLGRIDVALUEFN;
  if ( nsp_gridvaluefn_full_copy_partial(H,self)== NULL) return NULLGRIDVALUEFN;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGridValueFn
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gridvaluefn_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGridValueFn *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gridvaluefn is initialized */
  nsp_type_gridvaluefn = new_type_gridvaluefn(T_BASE);
  if(( H = nsp_gridvaluefn_create_void(NVOID,(NspTypeBase *) nsp_type_gridvaluefn)) == NULLGRIDVALUEFN) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gridvaluefn_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 89 "codegen/stochdec.override"

/* method overriden  
 * take care that indices are starting at 0 and we want 1 at nsp level
 */

static int _wrap_nsp_gvf_ind_to_point(NspGridValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  int i;
  NspMatrix *Ind, *ret;
  NspValueFn *Vf = (NspValueFn *) self;
  int_types T[] = {mat,t_end};
  if ( GetArgs(stack,rhs,opt,T,&Ind) == FAIL) return RET_BUG;
  if ((ret = nsp_matrix_create(NVOID,'r', Vf->xdim,Ind->mn)) == NULLMAT) return RET_BUG;
  for (i=0; i < Ind->mn; i++)
    {      
      if ( nsp_gvf_ind_to_point(self,ret->R+ ret->m*i, -1 + (int) Ind->R[i]) == FAIL)  return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#line 1637 "stochdec.c"


#line 112 "codegen/stochdec.override"

/* method overriden  
 * take care that indices are starting at 0 
 */

static int _wrap_nsp_gvf_point_to_ind(NspGridValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  NspValueFn *Vf = (NspValueFn *) self;
  int_types T[] = {realmat,t_end};
  NspMatrix *pts,*Ret;
  int i;
  if ( GetArgs(stack,rhs,opt,T,&pts) == FAIL) return RET_BUG;
  if ( pts->m != Vf->xdim) 
    {
      Scierror("Error: first argument should be a %dxn matrix\n",Vf->xdim);
      return RET_BUG;
    }
  if ((Ret = nsp_matrix_create(NVOID,'r', 1 , pts->n)) == NULLMAT) return RET_BUG;
  for ( i = 0 ; i < pts->n ; i++)
    {
      Ret->R[i] = 1+ nsp_gvf_point_to_ind(self, pts->R+ i*pts->m);
    }
  MoveObj(stack,1, NSP_OBJECT(Ret));
  return 1;
}

#line 1667 "stochdec.c"


#line 169 "codegen/stochdec.override"

static int _wrap_nsp_gvf_set_i_value(NspGridValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  int i;
  NspMatrix *ind, * val;
  int_types T[] = { realmat, realmat,t_end};
  if ( GetArgs(stack,rhs,opt,T,&ind, &val) == FAIL) return RET_BUG;
  CheckDimProp(NspFname(stack),1,2, ind->mn != val->mn);
  for ( i = 0 ; i < ind->mn; i++)
    nsp_gvf_set_i_value(self, ((int) ind->R[i]) - 1, val->R[i]);
  return 0;
}
#line 1683 "stochdec.c"


#line 140 "codegen/stochdec.override"

/* method overriden  
 * take care that indices are starting at 0 
 */
static int _wrap_nsp_gvf_set_pt_value(NspGridValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  int i;
  NspValueFn *Vf = (NspValueFn *) self;
  int_types T[] = {realmat, realmat,t_end};
  NspMatrix *pts, *val;
  if ( GetArgs(stack,rhs,opt,T,&pts, &val) == FAIL) return RET_BUG;
  if ( pts->m != Vf->xdim) 
    {
      Scierror("Error: first argument should be a %dxn matrix\n",Vf->xdim);
      return RET_BUG;
    }
  if ( pts->n != val->mn) 
    {
      Scierror("Error: the number of columns of first argument (%d) is not equal to the size of second argument (%d)\n",pts->n,val->mn);
      return RET_BUG;
    }
  /* pt << 1 */
  for ( i= 0 ; i < pts->n ; i++ )
    nsp_gvf_set_pt_value(self, pts->R + i*pts->m, val->R[i]);
  return 0;
}

#line 1714 "stochdec.c"


#line 207 "codegen/stochdec.override"

static int _wrap_nsp_gvf_get_i_value(NspGridValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  int i;
  int_types T[] = {realmat,t_end};
  NspMatrix *ind, *ret;
  if ( GetArgs(stack,rhs,opt,T,&ind) == FAIL) return RET_BUG;
  if ((ret = nsp_matrix_create(NVOID,'r', 1, ind->mn)) == NULLMAT) return RET_BUG;
  for ( i = 0 ; i < ret->mn ; i++)
    {
      ret->R[i] = nsp_gvf_get_i_value(self, ((int) ind->R[i]) -1);
    }
  MoveObj(stack,1, NSP_OBJECT(ret));
  return 1;
}
#line 1733 "stochdec.c"


#line 183 "codegen/stochdec.override"

static int _wrap_nsp_gvf_get_pt_value(NspGridValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  int i;
  NspValueFn *Vf = (NspValueFn *) self;
  int_types T[] = {realmat,t_end};
  NspMatrix *pts, *ret;
  if ( GetArgs(stack,rhs,opt,T,&pts) == FAIL) return RET_BUG;
  if ( pts->m != Vf->xdim) 
    {
      Scierror("Error: first argument should be a %dxn matrix\n",Vf->xdim);
      return RET_BUG;
    }
  if ((ret = nsp_matrix_create(NVOID,'r', 1, pts->n)) == NULLMAT) return RET_BUG;
  for ( i = 0 ; i < ret->mn ; i++)
    {
      ret->R[i] = nsp_gvf_get_pt_value(self, pts->R + pts->m*i);
    }
  MoveObj(stack,1, NSP_OBJECT(ret));
  return 1;
}

#line 1759 "stochdec.c"


static int _wrap_nsp_gvf_get_nx(NspGridValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *ret;
  CheckRhs(0,0);
    ret =nsp_gvf_get_nx(self);
  if ( ret == NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static NspMethods gridvaluefn_methods[] = {
  {"i2p",(nsp_method *) _wrap_nsp_gvf_ind_to_point},
  {"p2i",(nsp_method *) _wrap_nsp_gvf_point_to_ind},
  {"i_set_value",(nsp_method *) _wrap_nsp_gvf_set_i_value},
  {"pt_set_value",(nsp_method *) _wrap_nsp_gvf_set_pt_value},
  {"i_get_value",(nsp_method *) _wrap_nsp_gvf_get_i_value},
  {"pt_get_value",(nsp_method *) _wrap_nsp_gvf_get_pt_value},
  {"get_nx",(nsp_method *) _wrap_nsp_gvf_get_nx},
  { NULL, NULL}
};

static NspMethods *gridvaluefn_get_methods(void) { return gridvaluefn_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gridvaluefn_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspCutsValueFn ----------- */


#define  NspCutsValueFn_Private 
#include <nsp/objects.h>
#include <nsp/cutsvaluefn.h>
#include <nsp/interf.h>

/* 
 * NspCutsValueFn inherits from ValueFn 
 */

int nsp_type_cutsvaluefn_id=0;
NspTypeCutsValueFn *nsp_type_cutsvaluefn=NULL;

/*
 * Type object for NspCutsValueFn 
 * all the instance of NspTypeCutsValueFn share the same id. 
 * nsp_type_cutsvaluefn: is an instance of NspTypeCutsValueFn 
 *    used for objects of NspCutsValueFn type (i.e built with new_cutsvaluefn) 
 * other instances are used for derived classes 
 */
NspTypeCutsValueFn *new_type_cutsvaluefn(type_mode mode)
{
  NspTypeCutsValueFn *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_cutsvaluefn != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cutsvaluefn;
    }
  if (( type =  malloc(sizeof(NspTypeCutsValueFn))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_valuefn(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = cutsvaluefn_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = cutsvaluefn_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_cutsvaluefn;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for cutsvaluefn */ 

  top->pr = (print_func *) nsp_cutsvaluefn_print;
  top->dealloc = (dealloc_func *) nsp_cutsvaluefn_destroy;
  top->copy  =  (copy_func *) nsp_cutsvaluefn_copy;
  top->size  = (size_func *) nsp_cutsvaluefn_size;
  top->s_type =  (s_type_func *) nsp_cutsvaluefn_type_as_string;
  top->sh_type = (sh_type_func *) nsp_cutsvaluefn_type_short_string;
  top->info = (info_func *) nsp_cutsvaluefn_info;
  /* top->is_true = (is_true_func  *) nsp_cutsvaluefn_is_true; */
  /* top->loop =(loop_func *) nsp_cutsvaluefn_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_cutsvaluefn_object;
  top->eq  = (eq_func *) nsp_cutsvaluefn_eq;
  top->neq  = (eq_func *) nsp_cutsvaluefn_neq;
  top->save  = (save_func *) nsp_cutsvaluefn_xdr_save;
  top->load  = (load_func *) nsp_cutsvaluefn_xdr_load;
  top->create = (create_func*) int_cutsvaluefn_create;
  top->latex = (print_func *) nsp_cutsvaluefn_latex;
  top->full_copy = (copy_func *) nsp_cutsvaluefn_full_copy;

  /* specific methods for cutsvaluefn */

  type->init = (init_func *) init_cutsvaluefn;

  /* 
   * NspCutsValueFn interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_cutsvaluefn_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCutsValueFn called nsp_type_cutsvaluefn
       */
      type->id =  nsp_type_cutsvaluefn_id = nsp_new_type_id();
      nsp_type_cutsvaluefn = type;
      if ( nsp_register_type(nsp_type_cutsvaluefn) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_cutsvaluefn(mode);
    }
  else 
    {
      type->id = nsp_type_cutsvaluefn_id;
      return type;
    }
}

/*
 * initialize NspCutsValueFn instances 
 * locally and by calling initializer on parent class 
 */

static int init_cutsvaluefn(NspCutsValueFn *Obj,NspTypeCutsValueFn *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->heights = NULLMAT;
  Obj->slopes = NULLMAT;
 return OK;
}

/*
 * new instance of NspCutsValueFn 
 */

NspCutsValueFn *new_cutsvaluefn() 
{
  NspCutsValueFn *loc;
  /* type must exists */
  nsp_type_cutsvaluefn = new_type_cutsvaluefn(T_BASE);
  if ( (loc = malloc(sizeof(NspCutsValueFn)))== NULLCUTSVALUEFN) return loc;
  /* initialize object */
  if ( init_cutsvaluefn(loc,nsp_type_cutsvaluefn) == FAIL) return NULLCUTSVALUEFN;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspCutsValueFn 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_cutsvaluefn_size(NspCutsValueFn *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char cutsvaluefn_type_name[]="CutsValueFn";
static char cutsvaluefn_short_type_name[]="cutsvaluefn";

static char *nsp_cutsvaluefn_type_as_string(void)
{
  return(cutsvaluefn_type_name);
}

static char *nsp_cutsvaluefn_type_short_string(NspObject *v)
{
  return(cutsvaluefn_short_type_name);
}

/*
 * A == B 
 */

static int nsp_cutsvaluefn_eq(NspCutsValueFn *A, NspObject *B)
{
  NspCutsValueFn *loc = (NspCutsValueFn *) B;
  if ( check_cast(B,nsp_type_cutsvaluefn_id) == FALSE) return FALSE ;
  if ( NSP_OBJECT(A->heights)->type->eq(A->heights,loc->heights) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->slopes)->type->eq(A->slopes,loc->slopes) == FALSE ) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_cutsvaluefn_neq(NspCutsValueFn *A, NspObject *B)
{
  return ( nsp_cutsvaluefn_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_cutsvaluefn_xdr_save(XDR *xdrs, NspCutsValueFn *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_cutsvaluefn)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if ( nsp_valuefn_xdr_save(xdrs, (NspValueFn * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspCutsValueFn  *nsp_cutsvaluefn_xdr_load_partial(XDR *xdrs, NspCutsValueFn *M)
{
  int fid;
  char name[NAME_MAXL];
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_valuefn_xdr_load_partial(xdrs,(NspValueFn * )M) == NULL) return NULL;
 return M;
}

static NspCutsValueFn  *nsp_cutsvaluefn_xdr_load(XDR *xdrs)
{
  NspCutsValueFn *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCUTSVALUEFN;
  if ((H  = nsp_cutsvaluefn_create_void(name,(NspTypeBase *) nsp_type_cutsvaluefn))== NULLCUTSVALUEFN) return H;
  if ( nsp_cutsvaluefn_create_partial(H) == FAIL) return NULLCUTSVALUEFN;
  if ((H  = nsp_cutsvaluefn_xdr_load_partial(xdrs,H))== NULLCUTSVALUEFN) return H;
  if ( nsp_cutsvaluefn_check_values(H) == FAIL) return NULLCUTSVALUEFN;
  return H;
}

/*
 * delete 
 */

void nsp_cutsvaluefn_destroy_partial(NspCutsValueFn *H)
{
  nsp_valuefn_destroy_partial((NspValueFn * ) H);
  if ( H->heights != NULL ) 
    nsp_matrix_destroy(H->heights);
  if ( H->slopes != NULL ) 
    nsp_matrix_destroy(H->slopes);
}

void nsp_cutsvaluefn_destroy(NspCutsValueFn *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_cutsvaluefn_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_cutsvaluefn_info(NspCutsValueFn *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCUTSVALUEFN) 
    {
      Sciprintf("Null Pointer NspCutsValueFn \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_cutsvaluefn_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_cutsvaluefn_print(NspCutsValueFn *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCUTSVALUEFN) 
    {
      Sciprintf("Null Pointer NspCutsValueFn \n");
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
          nsp_cutsvaluefn_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_cutsvaluefn_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
  if ( M->heights != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->heights),indent+2,"heights", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->slopes != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->slopes),indent+2,"slopes", rec_level+1)== FALSE ) return FALSE ;
    }
  nsp_valuefn_print((NspValueFn * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_cutsvaluefn_latex(NspCutsValueFn *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_cutsvaluefn_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  if ( M->heights != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->heights),FALSE,"heights", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  if ( M->slopes != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->slopes),FALSE,"slopes", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  nsp_valuefn_latex((NspValueFn * ) M, FALSE,NULL,rec_level);
  Sciprintf1(indent+1,"\n");
  Sciprintf("\\end{array}\n");

  Sciprintf("\\right.\n");

  if ( use_math ) Sciprintf("\\end{equation*}\n");

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspCutsValueFn objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspCutsValueFn   *nsp_cutsvaluefn_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_cutsvaluefn_id)  == TRUE  ) return ((NspCutsValueFn *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_cutsvaluefn));
  return NULL;
}

int IsCutsValueFnObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_cutsvaluefn_id);
}

int IsCutsValueFn(NspObject *O)
{
  return nsp_object_type(O,nsp_type_cutsvaluefn_id);
}

NspCutsValueFn  *GetCutsValueFnCopy(Stack stack, int i)
{
  if (  GetCutsValueFn(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspCutsValueFn  *GetCutsValueFn(Stack stack, int i)
{
  NspCutsValueFn *M;
  if (( M = nsp_cutsvaluefn_object(NthObj(i))) == NULLCUTSVALUEFN)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspCutsValueFn instance 
 *-----------------------------------------------------*/

static NspCutsValueFn *nsp_cutsvaluefn_create_void(const char *name,NspTypeBase *type)
{
 NspCutsValueFn *H  = (type == NULL) ? new_cutsvaluefn() : type->new();
 if ( H ==  NULLCUTSVALUEFN)
  {
   Sciprintf("No more memory\n");
   return NULLCUTSVALUEFN;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCUTSVALUEFN;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_cutsvaluefn_create_partial(NspCutsValueFn *H)
{
  return OK;
}

int nsp_cutsvaluefn_check_values(NspCutsValueFn *H)
{
  if ( H->heights == NULLMAT) 
    {
       if (( H->heights = nsp_matrix_create("heights",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->slopes == NULLMAT) 
    {
       if (( H->slopes = nsp_matrix_create("slopes",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_valuefn_check_values((NspValueFn * ) H);
  return OK;
}

NspCutsValueFn *nsp_cutsvaluefn_create(const char *name,NspMatrix* heights,NspMatrix* slopes,NspTypeBase *type)
{
  NspCutsValueFn *H  = nsp_cutsvaluefn_create_void(name,type);
  if ( H ==  NULLCUTSVALUEFN) return NULLCUTSVALUEFN;
  H->heights= heights;
  H->slopes= slopes;
  if ( nsp_cutsvaluefn_check_values(H) == FAIL) return NULLCUTSVALUEFN;
  return H;
}


NspCutsValueFn *nsp_cutsvaluefn_create_default(const char *name)
{
 NspCutsValueFn *H  = nsp_cutsvaluefn_create_void(name,NULL);
 if ( H ==  NULLCUTSVALUEFN) return NULLCUTSVALUEFN;
  if ( nsp_cutsvaluefn_check_values(H) == FAIL) return NULLCUTSVALUEFN;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspCutsValueFn *nsp_cutsvaluefn_copy_partial(NspCutsValueFn *H,NspCutsValueFn *self)
{
  if ( nsp_valuefn_copy_partial((NspValueFn *) H,(NspValueFn * ) self ) == NULL) return NULLCUTSVALUEFN;
  if ( self->heights == NULL )
    { H->heights = NULL;}
  else
    {
      if ((H->heights = (NspMatrix *) nsp_object_copy_and_name("heights", NSP_OBJECT(self->heights))) == NULLMAT) return NULL;
    }
  if ( self->slopes == NULL )
    { H->slopes = NULL;}
  else
    {
      if ((H->slopes = (NspMatrix *) nsp_object_copy_and_name("slopes", NSP_OBJECT(self->slopes))) == NULLMAT) return NULL;
    }
  return H;
}

NspCutsValueFn *nsp_cutsvaluefn_copy(NspCutsValueFn *self)
{
  NspCutsValueFn *H  =nsp_cutsvaluefn_create_void(NVOID,(NspTypeBase *) nsp_type_cutsvaluefn);
  if ( H ==  NULLCUTSVALUEFN) return NULLCUTSVALUEFN;
  if ( nsp_cutsvaluefn_copy_partial(H,self)== NULL) return NULLCUTSVALUEFN;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspCutsValueFn *nsp_cutsvaluefn_full_copy_partial(NspCutsValueFn *H,NspCutsValueFn *self)
{
  if ( nsp_valuefn_full_copy_partial((NspValueFn *) H,(NspValueFn * ) self ) == NULL) return NULLCUTSVALUEFN;
  if ( self->heights == NULL )
    { H->heights = NULL;}
  else
    {
      if ((H->heights = (NspMatrix *) nsp_object_full_copy_and_name("heights", NSP_OBJECT(self->heights))) == NULLMAT) return NULL;
    }
  if ( self->slopes == NULL )
    { H->slopes = NULL;}
  else
    {
      if ((H->slopes = (NspMatrix *) nsp_object_full_copy_and_name("slopes", NSP_OBJECT(self->slopes))) == NULLMAT) return NULL;
    }
  return H;
}

NspCutsValueFn *nsp_cutsvaluefn_full_copy(NspCutsValueFn *self)
{
  NspCutsValueFn *H  =nsp_cutsvaluefn_create_void(NVOID,(NspTypeBase *) nsp_type_cutsvaluefn);
  if ( H ==  NULLCUTSVALUEFN) return NULLCUTSVALUEFN;
  if ( nsp_cutsvaluefn_full_copy_partial(H,self)== NULL) return NULLCUTSVALUEFN;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspCutsValueFn
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_cutsvaluefn_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCutsValueFn *H;
  CheckStdRhs(0,0);
  /* want to be sure that type cutsvaluefn is initialized */
  nsp_type_cutsvaluefn = new_type_cutsvaluefn(T_BASE);
  if(( H = nsp_cutsvaluefn_create_void(NVOID,(NspTypeBase *) nsp_type_cutsvaluefn)) == NULLCUTSVALUEFN) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_cutsvaluefn_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 252 "codegen/stochdec.override"

/* method overriden */

static int _wrap_nsp_cvf_add_slopes(NspCutsValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *heights,*slopes;
  /* NspValueFn *Vf = (NspValueFn *) self; */
  int_types T[] = {realmat,realmat,t_end};
  CheckLhs(0,0);
  if ( GetArgs(stack,rhs,opt,T,&heights,&slopes) == FAIL) return RET_BUG;
  CheckDimProp(NspFname(stack),1,2, heights->mn != slopes->n);
  if ( slopes->m != self->slopes->m )
    {
      Scierror("Error: given slopes are incompatible with objects expecting %d rows\n",self->slopes->m);
      return RET_BUG;
    }
  if ( nsp_cvf_add_slopes(self,heights, slopes ) == FAIL) 
    return RET_BUG;
  return 0;
}

#line 2335 "stochdec.c"


#line 275 "codegen/stochdec.override"

static int _wrap_nsp_cvf_get_value(NspCutsValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  int i;
  /* NspValueFn *Vf = (NspValueFn *) self; */
  int_types T[] = {realmat,t_end};
  NspMatrix *pts, *ret;
  if ( GetArgs(stack,rhs,opt,T,&pts) == FAIL) return RET_BUG;
  if ( pts->m != self->slopes->m ) 
    {
      Scierror("Error: first argument should be a %dxn matrix\n",self->slopes->m );
      return RET_BUG;
    }
  if ((ret = nsp_matrix_create(NVOID,'r', 1, pts->n)) == NULLMAT) return RET_BUG;
  for ( i = 0 ; i < ret->mn ; i++)
    {
      ret->R[i] = nsp_cvf_get_value(self, pts->R+ pts->m*i);
    }
  MoveObj(stack,1, NSP_OBJECT(ret));
  return 1;
}

#line 2361 "stochdec.c"


static int _wrap_nsp_cvf_get_slopes(NspCutsValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *ret;
  CheckRhs(0,0);
    ret =nsp_cvf_get_slopes(self);
  if ( ret == NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static int _wrap_nsp_cvf_get_heights(NspCutsValueFn *self,Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *ret;
  CheckRhs(0,0);
    ret =nsp_cvf_get_heights(self);
  if ( ret == NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

static NspMethods cutsvaluefn_methods[] = {
  {"add_slopes",(nsp_method *) _wrap_nsp_cvf_add_slopes},
  {"get_value",(nsp_method *) _wrap_nsp_cvf_get_value},
  {"get_slopes",(nsp_method *) _wrap_nsp_cvf_get_slopes},
  {"get_heights",(nsp_method *) _wrap_nsp_cvf_get_heights},
  { NULL, NULL}
};

static NspMethods *cutsvaluefn_get_methods(void) { return cutsvaluefn_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab cutsvaluefn_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 67 "codegen/stochdec.override"

int _wrap_nsp_gridfn(Stack stack, int rhs, int opt, int lhs) 
{
  int use_values = TRUE;
  NspMatrix *xmin, *xmax,*nx;
  NspGridValueFn *H;
  int_types T[] = {realmat,realmat,realmat,new_opts, t_end};

  nsp_option opts[] ={{ "values",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  
  if ( GetArgs(stack,rhs,opt,T,&nx, &xmin, &xmax,&opts, &use_values) == FAIL) return RET_BUG;
  CheckDimProp(NspFname(stack),1,3, nx->mn != xmin->mn);
  CheckDimProp(NspFname(stack),2,3, xmin->mn != xmax->mn);
  if ((H=nsp_gvf_create(NVOID,nx,xmin,xmax, use_values)) == NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
}

#line 2423 "stochdec.c"


#line 224 "codegen/stochdec.override"

int _wrap_nsp_cutsfn(Stack stack, int rhs, int opt, int lhs) 
{
  NspMatrix *heights=NULL, *slopes = NULL;
  NspMatrix *heights_c=NULL, *slopes_c = NULL;
  NspCutsValueFn *H;
  NspValueFn *F;
  int_types T[] = {realmat,realmat,t_end};
  if ( GetArgs(stack,rhs,opt,T,&heights, &slopes) == FAIL) return RET_BUG;
  CheckDimProp(NspFname(stack),1,2, heights->mn != slopes->n);
  if ((slopes_c = (NspMatrix *) nsp_object_copy_and_name("slopes",NSP_OBJECT(slopes))) == NULLMAT) 
    return RET_BUG;
  if ((heights_c = (NspMatrix *) nsp_object_copy_and_name("heights",NSP_OBJECT(heights))) == NULLMAT) 
    return RET_BUG;
  /* want to be sure that type gridvaluefn is initialized */
  nsp_type_cutsvaluefn = new_type_cutsvaluefn(T_BASE);
  H = nsp_cutsvaluefn_create(NVOID, heights_c, slopes_c ,(NspTypeBase *) nsp_type_cutsvaluefn);
  if ( H == NULL) return RET_BUG;
  F= (NspValueFn *)H;
  /* nsp_matrix_destroy(F->xmin);  F->xmin = xmin_c; */
  /* nsp_matrix_destroy(F->xmax);  F->xmax = xmax_c; */
  F->xdim = heights->mn;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
}

#line 2453 "stochdec.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Stochdec_func[]={
  { "gridfn", _wrap_nsp_gridfn},
  { "cutsfn", _wrap_nsp_cutsfn},
  { "stochdec_create", int_stochdec_create},
  { NULL, NULL}
};

/* call ith function in the Stochdec interface */

int Stochdec_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Stochdec_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Stochdec_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Stochdec_func[i].name;
  *f = Stochdec_func[i].fonc;
}
void nsp_initialize_Stochdec_types(void)
{
  new_type_stochdec(T_BASE);
  new_type_valuefn(T_BASE);
  new_type_gridvaluefn(T_BASE);
  new_type_cutsvaluefn(T_BASE);
}

#line 299 "codegen/stochdec.override"

/***************************************
 * a set of functions for GridValueFn 
 ***************************************/

int nsp_gvf_check_nx(NspMatrix *nx)
{
  int i;
  for( i = 0 ; i < nx->mn ; i++)
    {
      if ((int) nx->R[i] <= 1 ) return FAIL;
    }
  return OK;
}

NspMatrix *nsp_gvf_create_steps(NspMatrix *nx,NspMatrix *xmin,NspMatrix *xmax) 
{
  int i;
  NspMatrix *Pas = NULL;
  if ((Pas = nsp_matrix_create("steps",'r',nx->mn,1)) == NULLMAT) return NULLMAT;
  for( i = 0 ; i < nx->mn ; i++)
    {
      Pas->R[i] = (xmax->R[i]-xmin->R[i]) / (nx->R[i]-1);
    }
  return Pas;
}

NspMatrix *nsp_gvf_create_values(NspMatrix *nx)
{
  int i, prod=1;
  NspMatrix *V = NULL;
  for( i = 0 ; i < nx->mn ; i++)
    {
      prod *= (int) nx->R[i];
    }
  V = nsp_matrix_create("values",'r',prod,1);
  memset(V->R,0,V->mn*sizeof(double));
  return V;
}


int nsp_gvf_ind_to_point(NspGridValueFn *Gvf,double pt[], int i)
{
  NspValueFn *Vf = (NspValueFn *) Gvf;
  int k, ind;
  /* Case where x is out of bounds */
  if(i < 0) return FAIL;
  for ( k=0 ; k < Vf->xdim; k++)
    {
      int inx = (int) Gvf->n->R[k];
      ind = i % inx ; /*  i - (i/nx)*nx; remainder */
      pt[k] = Vf->xmin->R[k] + Gvf->step->R[k]*ind;
      i = (i-ind)/inx;
    }
  return OK;
}

int nsp_ind_to_point(NspMatrix *pt, int i, NspMatrix *min,NspMatrix *nx, NspMatrix *step, int t)
{
  int k, ind;
  /* Case where x is out of bounds */
  if(i < 0) return FAIL;
  for ( k=0 ; k < pt->mn; k++)
    {
      int inx = (int) nx->R[k];
      ind = i % inx ; /*  i - (i/nx)*nx; remainder */
      pt->R[k] = min->R[k] + step->R[k]*ind;
      i = (i-ind)/inx;
    }
  return OK;
}

int nsp_gvf_point_to_ind(NspGridValueFn *Gvf,const double pt[]) 
{
  const double eps1 = 1.e-4, eps2= 1.e-10;
  NspValueFn *Vf = (NspValueFn *) Gvf;
  int k, ind = 0, cumprod = 1, ik = 0;
  for( k = 0; k < Vf->xdim; k++)
    {
      if( pt[k] < Vf->xmin->R[k] -eps1 || pt[k] > Vf->xmax->R[k]+ eps1)
	{
	  return -1;
	}
      else if (Vf->xmax->R[k] - Vf->xmin->R[k] < eps2 )
	{
	  ik = 0;
	}
      else
	{
	  ik = (int) floor((pt[k]-Vf->xmin->R[k])/Gvf->step->R[k]+0.5);
	}
      ind += cumprod * ik;
      cumprod *= (int) Gvf->n->R[k];
    }
  return ind;
}

/* return Nan if outside */

double nsp_gvf_get_i_value(NspGridValueFn *Gvf, int i)
{
  i = Max(Min(i,Gvf->values->mn-1),0);
  return Gvf->values->R[i];
}

void nsp_gvf_set_i_value(NspGridValueFn *Gvf, int i,const double val)
{
  i = Max(Min(i,Gvf->values->mn-1),0);
  Gvf->values->R[i]= val;
}

double nsp_gvf_get_pt_value(NspGridValueFn *Gvf,const double pt[]) 
{
  int ind = nsp_gvf_point_to_ind(Gvf,pt);
  int i = Max(Min(ind,Gvf->values->mn-1),0);
  return Gvf->values->R[i];
}

void nsp_gvf_set_pt_value(NspGridValueFn *Gvf,const double pt[],const double val) 
{
  int ind = nsp_gvf_point_to_ind(Gvf,pt);
  int i = Max(Min(ind,Gvf->values->mn-1),0);
  Gvf->values->R[i]= val;
}

/***************************************
 * a set of functions for CutsValueFn 
 ***************************************/

double nsp_cvf_get_value(NspCutsValueFn *Cvf,const double pt[]) 
{
  int i, j;
  double res = -HUGE_VAL, val;
  for( i=0 ; i < Cvf->heights->mn ; i++)
    {
      
      val = Cvf->heights->R[i];
      for( j = 0; j < Cvf->slopes->m ; j++)
	{
	  val += Cvf->slopes->R[j + Cvf->slopes->m*i ] * pt[j];
	}
      res = Max(res, val);
    }
  return res;
}

int nsp_cvf_add_slope(NspCutsValueFn *Cvf,double height,double slope[])
{
  int i, offset;
  if ( nsp_matrix_add_columns(Cvf->heights,1,height) == FAIL) 
    return(FAIL);
  if ( nsp_matrix_add_columns(Cvf->slopes,1,0.0) == FAIL) 
    return(FAIL);
  offset = Cvf->slopes->m*(Cvf->slopes->n-1);
  for( i=0 ; i < Cvf->heights->mn ; i++)
    {
      Cvf->slopes->R[i + offset ]= slope[i];
    }
  return OK;
}

/* we assume that size are checked elsewhere */

int nsp_cvf_add_slopes(NspCutsValueFn *Cvf,NspMatrix *height,NspMatrix *slopes)
{
  int i,j, n = Cvf->heights->mn;

  if ( Cvf->slopes->m != slopes->m ) return FAIL;
  if ( height->mn != slopes->n ) return FAIL;
  
  if ( nsp_matrix_add_columns(Cvf->heights,height->mn,0.0) == FAIL) 
    return(FAIL);
  for ( i = 0 ; i < height->mn; i++) 
    Cvf->heights->R[n+i] = height->R[i];

  n = Cvf->slopes->n;
  if ( nsp_matrix_add_columns(Cvf->slopes,slopes->n,0.0) == FAIL) 
    return(FAIL);
  for( j=0 ; j < slopes->n ; j++)
    {
      int offset1 = Cvf->slopes->m*(j+n);
      int offset2 = slopes->m*j;
      memcpy(Cvf->slopes->R + offset1, slopes->R +offset2, Cvf->slopes->m*sizeof(double));
    }
  return OK;
}

NspMatrix *nsp_cvf_get_slopes(NspCutsValueFn *self)
{
  return  (NspMatrix *) nsp_object_copy(NSP_OBJECT(self->slopes));
}

NspMatrix *nsp_cvf_get_heights(NspCutsValueFn *self)
{
  return   (NspMatrix *) nsp_object_copy(NSP_OBJECT(self->heights));
}

NspMatrix *nsp_gvf_get_nx(NspGridValueFn *Gvf)
{
  return (NspMatrix *) nsp_object_copy(NSP_OBJECT(Gvf->n));
}

/* if use_values is FALSE the values field is not allocated and the 
 * the GridValueFn is only used to perform points to/from integer operations
 */

NspGridValueFn *nsp_gvf_create(const char *name,NspMatrix *nx,NspMatrix *xmin,NspMatrix *xmax, int use_values)
{
  NspGridValueFn *H;
  NspValueFn *F;
  NspMatrix *step=NULL, *values = NULL, *nx_c=NULL, *xmin_c=NULL,*xmax_c=NULL;
  if (nsp_gvf_check_nx(nx) == FAIL) 
    {
      Scierror("Error: first argument should contain values greater than 1\n");
      return NULL;
    }
  if ( (step=nsp_gvf_create_steps(nx,xmin,xmax))== NULL) 
    {
      Scierror("Error: unable to create steps\n");
      return NULL;
    }
  if (  use_values )
    {
      values = nsp_gvf_create_values(nx);
      if ( values == NULL )
	{
	  Scierror("Error: unable to allocate space for values \n");
	  return NULL;
	}
    }
  else
    {
      values = nsp_matrix_create("values",'r',1,1);
      if ( values == NULL )
	{
	  Scierror("Error: unable to allocate space for values \n");
	  return NULL;
	}
      values->R[0]=0.0;
    }
  if ((nx_c = (NspMatrix *) nsp_object_copy_and_name("nx",NSP_OBJECT(nx))) == NULLMAT) 
    return NULL;
  if ((xmin_c = (NspMatrix *) nsp_object_copy_and_name("xmin",NSP_OBJECT(xmin))) == NULLMAT) 
    return NULL;
  if ((xmax_c = (NspMatrix *) nsp_object_copy_and_name("xmax",NSP_OBJECT(xmax))) == NULLMAT) 
    return NULL;
  /* want to be sure that type gridvaluefn is initialized */
  nsp_type_gridvaluefn = new_type_gridvaluefn(T_BASE);
  H = nsp_gridvaluefn_create(name , nx_c, step, values,(NspTypeBase *) nsp_type_gridvaluefn);
  if ( H == NULL) return NULL;
  F= (NspValueFn *)H;
  nsp_matrix_destroy(F->xmin);  F->xmin = xmin_c;
  nsp_matrix_destroy(F->xmax);  F->xmax = xmax_c;
  F->xdim = nx->mn;
  return H;
}


#line 2750 "stochdec.c"
