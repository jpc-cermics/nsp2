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





#line 24 "codegen/rational.override"
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h> 

#line 32 "rational.c"

/* -----------NspRational ----------- */


#define  NspRational_Private 
#include <nsp/objects.h>
#include <nsp/rational.h>
#include <nsp/interf.h>

/* 
 * NspRational inherits from Object 
 * and implements Matint
 */

int nsp_type_rational_id=0;
NspTypeRational *nsp_type_rational=NULL;

/*
 * Type object for NspRational 
 * all the instance of NspTypeRational share the same id. 
 * nsp_type_rational: is an instance of NspTypeRational 
 *    used for objects of NspRational type (i.e built with new_rational) 
 * other instances are used for derived classes 
 */
NspTypeRational *new_type_rational(type_mode mode)
{
  NspTypeMatint *t_matint;
  NspTypeRational *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_rational != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_rational;
    }
  if (( type =  malloc(sizeof(NspTypeRational))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = rational_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = rational_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_rational;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for rational */ 

  top->pr = (print_func *) nsp_rational_print;
  top->dealloc = (dealloc_func *) nsp_rational_destroy;
  top->copy  =  (copy_func *) nsp_rational_copy;
  top->size  = (size_func *) nsp_rational_size;
  top->s_type =  (s_type_func *) nsp_rational_type_as_string;
  top->sh_type = (sh_type_func *) nsp_rational_type_short_string;
  top->info = (info_func *) nsp_rational_info;
  /* top->is_true = (is_true_func  *) nsp_rational_is_true; */
  /* top->loop =(loop_func *) nsp_rational_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_rational_object;
  top->eq  = (eq_func *) nsp_rational_eq;
  top->neq  = (eq_func *) nsp_rational_neq;
  top->save  = (save_func *) nsp_rational_xdr_save;
  top->load  = (load_func *) nsp_rational_xdr_load;
  top->create = (create_func*) int_rational_create;
  top->latex = (print_func *) nsp_rational_latex;
  top->full_copy = (copy_func *) nsp_rational_full_copy;

  /* specific methods for rational */

  type->init = (init_func *) init_rational;

  /* 
   * NspRational interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  t_matint = new_type_matint(T_DERIVED);
  type->interface = (NspTypeBase * ) t_matint;
  if ( nsp_type_rational_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeRational called nsp_type_rational
       */
      type->id =  nsp_type_rational_id = nsp_new_type_id();
      nsp_type_rational = type;
      if ( nsp_register_type(nsp_type_rational) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_rational(mode);
    }
  else 
    {
      type->id = nsp_type_rational_id;
      return type;
    }
}

/*
 * initialize NspRational instances 
 * locally and by calling initializer on parent class 
 */

static int init_rational(NspRational *Obj,NspTypeRational *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->n = NULLPMAT;
  Obj->d = NULLPMAT;
  Obj->mode = NULL;
 return OK;
}

/*
 * new instance of NspRational 
 */

NspRational *new_rational() 
{
  NspRational *loc;
  /* type must exists */
  nsp_type_rational = new_type_rational(T_BASE);
  if ( (loc = malloc(sizeof(NspRational)))== NULLRATIONAL) return loc;
  /* initialize object */
  if ( init_rational(loc,nsp_type_rational) == FAIL) return NULLRATIONAL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspRational 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_rational_size(NspRational *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char rational_type_name[]="Rational";
static char rational_short_type_name[]="rational";

static char *nsp_rational_type_as_string(void)
{
  return(rational_type_name);
}

static char *nsp_rational_type_short_string(NspObject *v)
{
  return(rational_short_type_name);
}

/*
 * A == B 
 */

static int nsp_rational_eq(NspRational *A, NspObject *B)
{
  NspRational *loc = (NspRational *) B;
  if ( check_cast(B,nsp_type_rational_id) == FALSE) return FALSE ;
  if ( NSP_OBJECT(A->n)->type->eq(A->n,loc->n) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->d)->type->eq(A->d,loc->d) == FALSE ) return FALSE;
  if ( strcmp(A->mode,loc->mode) != 0) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_rational_neq(NspRational *A, NspObject *B)
{
  return ( nsp_rational_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_rational_xdr_save(XDR *xdrs, NspRational *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_rational)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->n)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->d)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->mode) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspRational  *nsp_rational_xdr_load_partial(XDR *xdrs, NspRational *M)
{
  if ((M->n =(NspPMatrix *) nsp_object_xdr_load(xdrs))== NULLPMAT) return NULL;
  if ((M->d =(NspPMatrix *) nsp_object_xdr_load(xdrs))== NULLPMAT) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->mode)) == FAIL) return NULL;
 return M;
}

static NspRational  *nsp_rational_xdr_load(XDR *xdrs)
{
  NspRational *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLRATIONAL;
  if ((H  = nsp_rational_create_void(name,(NspTypeBase *) nsp_type_rational))== NULLRATIONAL) return H;
  if ( nsp_rational_create_partial(H) == FAIL) return NULLRATIONAL;
  if ((H  = nsp_rational_xdr_load_partial(xdrs,H))== NULLRATIONAL) return H;
  if ( nsp_rational_check_values(H) == FAIL) return NULLRATIONAL;
#line 42 "codegen/rational.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
#line 258 "rational.c"
  return H;
}

/*
 * delete 
 */

void nsp_rational_destroy_partial(NspRational *H)
{
#line 45 "codegen/rational.override"
  /* verbatim in destroy */



#line 273 "rational.c"
  if ( H->n != NULL ) 
    nsp_pmatrix_destroy(H->n);
  if ( H->d != NULL ) 
    nsp_pmatrix_destroy(H->d);
  nsp_string_destroy(&(H->mode));
}

void nsp_rational_destroy(NspRational *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_rational_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_rational_info(NspRational *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLRATIONAL) 
    {
      Sciprintf("Null Pointer NspRational \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_rational_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_rational_print(NspRational *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLRATIONAL) 
    {
      Sciprintf("Null Pointer NspRational \n");
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
          nsp_rational_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_rational_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
  if ( M->n != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->n),indent+2,"n", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->d != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->d),indent+2,"d", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"mode=%s\n",M->mode);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_rational_latex(NspRational *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_rational_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->n != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->n),indent+2,"n", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->d != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->d),indent+2,"d", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"mode=%s\n",M->mode);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspRational objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspRational   *nsp_rational_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_rational_id)  == TRUE  ) return ((NspRational *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_rational));
  return NULL;
}

int IsRationalObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_rational_id);
}

int IsRational(NspObject *O)
{
  return nsp_object_type(O,nsp_type_rational_id);
}

NspRational  *GetRationalCopy(Stack stack, int i)
{
  if (  GetRational(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspRational  *GetRational(Stack stack, int i)
{
  NspRational *M;
  if (( M = nsp_rational_object(NthObj(i))) == NULLRATIONAL)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspRational instance 
 *-----------------------------------------------------*/

static NspRational *nsp_rational_create_void(const char *name,NspTypeBase *type)
{
 NspRational *H  = (type == NULL) ? new_rational() : type->new();
 if ( H ==  NULLRATIONAL)
  {
   Sciprintf("No more memory\n");
   return NULLRATIONAL;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLRATIONAL;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_rational_create_partial(NspRational *H)
{
  return OK;
}

int nsp_rational_check_values(NspRational *H)
{
  if ( H->n == NULLPMAT) 
    {
     if (( H->n = nsp_pmatrix_create("n",0,0,NULL,-1,NULL)) == NULLPMAT)
       return FAIL;
    }
  if ( H->d == NULLPMAT) 
    {
     if (( H->d = nsp_pmatrix_create("d",0,0,NULL,-1,NULL)) == NULLPMAT)
       return FAIL;
    }
  if ( H->mode == NULL) 
    {
  if (( H->mode = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  return OK;
}

NspRational *nsp_rational_create(const char *name,NspPMatrix* n,NspPMatrix* d,char* mode,NspTypeBase *type)
{
  NspRational *H  = nsp_rational_create_void(name,type);
  if ( H ==  NULLRATIONAL) return NULLRATIONAL;
  H->n= n;
  H->d= d;
  H->mode = mode;
  if ( nsp_rational_check_values(H) == FAIL) return NULLRATIONAL;
#line 42 "codegen/rational.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
#line 459 "rational.c"
  return H;
}


NspRational *nsp_rational_create_default(const char *name)
{
 NspRational *H  = nsp_rational_create_void(name,NULL);
 if ( H ==  NULLRATIONAL) return NULLRATIONAL;
  if ( nsp_rational_check_values(H) == FAIL) return NULLRATIONAL;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspRational *nsp_rational_copy_partial(NspRational *H,NspRational *self)
{
  if ( self->n == NULL )
    { H->n = NULL;}
  else
    {
      if ((H->n = (NspPMatrix *) nsp_object_copy_and_name("n", NSP_OBJECT(self->n))) == NULLPMAT) return NULL;
    }
  if ( self->d == NULL )
    { H->d = NULL;}
  else
    {
      if ((H->d = (NspPMatrix *) nsp_object_copy_and_name("d", NSP_OBJECT(self->d))) == NULLPMAT) return NULL;
    }
  if ((H->mode = nsp_string_copy(self->mode)) == NULL) return NULL;
  return H;
}

NspRational *nsp_rational_copy(NspRational *self)
{
  NspRational *H  =nsp_rational_create_void(NVOID,(NspTypeBase *) nsp_type_rational);
  if ( H ==  NULLRATIONAL) return NULLRATIONAL;
  if ( nsp_rational_copy_partial(H,self)== NULL) return NULLRATIONAL;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspRational *nsp_rational_full_copy_partial(NspRational *H,NspRational *self)
{
  if ( self->n == NULL )
    { H->n = NULL;}
  else
    {
      if ((H->n = (NspPMatrix *) nsp_object_full_copy_and_name("n", NSP_OBJECT(self->n))) == NULLPMAT) return NULL;
    }
  if ( self->d == NULL )
    { H->d = NULL;}
  else
    {
      if ((H->d = (NspPMatrix *) nsp_object_full_copy_and_name("d", NSP_OBJECT(self->d))) == NULLPMAT) return NULL;
    }
  if ((H->mode = nsp_string_copy(self->mode)) == NULL) return NULL;
  return H;
}

NspRational *nsp_rational_full_copy(NspRational *self)
{
  NspRational *H  =nsp_rational_create_void(NVOID,(NspTypeBase *) nsp_type_rational);
  if ( H ==  NULLRATIONAL) return NULLRATIONAL;
  if ( nsp_rational_full_copy_partial(H,self)== NULL) return NULLRATIONAL;

#line 42 "codegen/rational.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
#line 532 "rational.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspRational
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_rational_create(Stack stack, int rhs, int opt, int lhs)
{
  NspRational *H;
  CheckStdRhs(0,0);
  /* want to be sure that type rational is initialized */
  nsp_type_rational = new_type_rational(T_BASE);
  if(( H = nsp_rational_create_void(NVOID,(NspTypeBase *) nsp_type_rational)) == NULLRATIONAL) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_rational_check_values(H) == FAIL) return RET_BUG;
  #line 42 "codegen/rational.override"
  /* verbatim in create/load/full_copy interface use RET_BUG for returned value */
#line 553 "rational.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *rational_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_rational_get_n(void *self,const char *attr)
{
  NspPMatrix *ret;
  ret = ((NspRational *) self)->n;
  return (NspObject *) ret;
}

static NspObject *_wrap_rational_get_obj_n(void *self,const char *attr, int *copy)
{
  NspPMatrix *ret;
  *copy = FALSE;
  ret = ((NspPMatrix*) ((NspRational *) self)->n);
  return (NspObject *) ret;
}

static int _wrap_rational_set_n(void *self,const char *attr, NspObject *O)
{
  NspPMatrix *n;
  if ( ! IsPMat(O) ) return FAIL;
  if ((n = (NspPMatrix *) nsp_object_copy_and_name(attr,O)) == NULLPMAT) return FAIL;
  if (((NspRational *) self)->n != NULL ) 
  nsp_pmatrix_destroy(((NspRational *) self)->n);
  ((NspRational *) self)->n= n;
  return OK;
}

static NspObject *_wrap_rational_get_d(void *self,const char *attr)
{
  NspPMatrix *ret;
  ret = ((NspRational *) self)->d;
  return (NspObject *) ret;
}

static NspObject *_wrap_rational_get_obj_d(void *self,const char *attr, int *copy)
{
  NspPMatrix *ret;
  *copy = FALSE;
  ret = ((NspPMatrix*) ((NspRational *) self)->d);
  return (NspObject *) ret;
}

static int _wrap_rational_set_d(void *self,const char *attr, NspObject *O)
{
  NspPMatrix *d;
  if ( ! IsPMat(O) ) return FAIL;
  if ((d = (NspPMatrix *) nsp_object_copy_and_name(attr,O)) == NULLPMAT) return FAIL;
  if (((NspRational *) self)->d != NULL ) 
  nsp_pmatrix_destroy(((NspRational *) self)->d);
  ((NspRational *) self)->d= d;
  return OK;
}

static NspObject *_wrap_rational_get_mode(void *self,const char *attr)
{
  NspObject *nsp_ret;
  const gchar *ret;
  ret = ((NspRational *) self)->mode;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_rational_set_mode(void *self,const char *attr, NspObject *O)
{
  char *mode;
  if ((mode = nsp_string_object(O))==NULL) return FAIL;
  if ((mode = nsp_string_copy(mode)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspRational *) self)->mode);
  ((NspRational *) self)->mode= mode;
  return OK;
}

static AttrTab rational_attrs[] = {
  { "n", (attr_get_function * )_wrap_rational_get_n, (attr_set_function * )_wrap_rational_set_n, (attr_get_object_function * )_wrap_rational_get_obj_n, (attr_set_object_function * )int_set_object_failed },
  { "d", (attr_get_function * )_wrap_rational_get_d, (attr_set_function * )_wrap_rational_set_d, (attr_get_object_function * )_wrap_rational_get_obj_d, (attr_set_object_function * )int_set_object_failed },
  { "mode", (attr_get_function * )_wrap_rational_get_mode, (attr_set_function * )_wrap_rational_set_mode, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Rational_func[]={
  { "rational_create", int_rational_create},
  { NULL, NULL}
};

/* call ith function in the Rational interface */

int Rational_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Rational_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Rational_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Rational_func[i].name;
  *f = Rational_func[i].fonc;
}
void nsp_initialize_Rational_types(void)
{
  new_type_rational(T_BASE);
}

#line 678 "rational.c"
