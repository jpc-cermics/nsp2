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





#line 27 "codegen/bvar.override"
#include <nsp/objects.h>
#include <nsp/plist.h>
#include <nsp/plistc.h>
#include <nsp/parse.h>

#line 34 "bvar.c"

/* -----------NspBvar ----------- */


#define  NspBvar_Private 
#include <nsp/objects.h>
#include <nsp/bvar.h>
#include <nsp/interf.h>

/* 
 * NspBvar inherits from Object 
 */

int nsp_type_bvar_id=0;
NspTypeBvar *nsp_type_bvar=NULL;

/*
 * Type object for NspBvar 
 * all the instance of NspTypeBvar share the same id. 
 * nsp_type_bvar: is an instance of NspTypeBvar 
 *    used for objects of NspBvar type (i.e built with new_bvar) 
 * other instances are used for derived classes 
 */
NspTypeBvar *new_type_bvar(type_mode mode)
{
  NspTypeBvar *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_bvar != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_bvar;
    }
  if (( type =  malloc(sizeof(NspTypeBvar))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = bvar_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = bvar_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_bvar;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for bvar */ 

  top->pr = (print_func *) nsp_bvar_print;
  top->dealloc = (dealloc_func *) nsp_bvar_destroy;
  top->copy  =  (copy_func *) nsp_bvar_copy;
  top->size  = (size_func *) nsp_bvar_size;
  top->s_type =  (s_type_func *) nsp_bvar_type_as_string;
  top->sh_type = (sh_type_func *) nsp_bvar_type_short_string;
  top->info = (info_func *) nsp_bvar_info;
  /* top->is_true = (is_true_func  *) nsp_bvar_is_true; */
#line 152 "codegen/bvar.override"
top->loop = (loop_func *) nsp_bvar_loop; /* loop with bvar type */

#line 95 "bvar.c"
#line 148 "codegen/bvar.override"
top->path_extract = (path_func *) NULL; /* path extract as for matrix type */

#line 99 "bvar.c"
  top->get_from_obj = (get_from_obj_func *) nsp_bvar_object;
  top->eq  = (eq_func *) nsp_bvar_eq;
  top->neq  = (eq_func *) nsp_bvar_neq;
  top->save  = (save_func *) nsp_bvar_xdr_save;
  top->load  = (load_func *) nsp_bvar_xdr_load;
  top->create = (create_func*) int_bvar_create;
  top->latex = (print_func *) nsp_bvar_latex;
  top->full_copy = (copy_func *) nsp_bvar_full_copy;

  /* specific methods for bvar */

  type->init = (init_func *) init_bvar;

#line 47 "codegen/bvar.override"
  /* inserted verbatim in the type definition */
  top->is_true = (is_true_func  *) nsp_bvar_is_true;

#line 117 "bvar.c"
  /* 
   * NspBvar interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_bvar_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeBvar called nsp_type_bvar
       */
      type->id =  nsp_type_bvar_id = nsp_new_type_id();
      nsp_type_bvar = type;
      if ( nsp_register_type(nsp_type_bvar) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_bvar(mode);
    }
  else 
    {
      type->id = nsp_type_bvar_id;
      return type;
    }
}

/*
 * initialize NspBvar instances 
 * locally and by calling initializer on parent class 
 */

static int init_bvar(NspBvar *Obj,NspTypeBvar *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->sym = FALSE;
  Obj->value = NULLOBJ;
  Obj->varname = NULL;
 return OK;
}

/*
 * new instance of NspBvar 
 */

NspBvar *new_bvar() 
{
  NspBvar *loc;
  /* type must exists */
  nsp_type_bvar = new_type_bvar(T_BASE);
  if ( (loc = malloc(sizeof(NspBvar)))== NULLBVAR) return loc;
  /* initialize object */
  if ( init_bvar(loc,nsp_type_bvar) == FAIL) return NULLBVAR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspBvar 
 *-----------------------------------------------*/
#line 167 "codegen/bvar.override"

/*
 * size can be overriden here
 */

static int nsp_bvar_size(NspBvar *Mat, int flag)
{
  /* redirect the size value to Mat->value */
  if ( Mat->value == NULL)
    return 0;
  else
    return nsp_object_get_size(Mat->value,flag);
}

#line 193 "bvar.c"
/*
 * type as string 
 */

static char bvar_type_name[]="Bvar";
static char bvar_short_type_name[]="bvar";

static char *nsp_bvar_type_as_string(void)
{
  return(bvar_type_name);
}

static char *nsp_bvar_type_short_string(NspObject *v)
{
  return(bvar_short_type_name);
}

#line 220 "codegen/bvar.override"

/*
 * A == B
 */

static int nsp_bvar_eq(NspBvar *A, NspObject *B)
{
  NspBvar *loc = (NspBvar *) B;
  if ( check_cast(B,nsp_type_bvar_id) == FALSE) return FALSE ;
  if ( A->value == NULL && loc->value == NULL) return TRUE;
  if ( A->value != NULL && loc->value != NULL)
    {
      if ( NSP_OBJECT(A->value)->type->eq(A->value,loc->value) == FALSE ) return FALSE;
    }
  else
    {
      return FALSE;
    }
  return TRUE;
}

/*
 * A != B
 */

static int nsp_bvar_neq(NspBvar *A, NspObject *B)
{
  return ( nsp_bvar_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

#line 242 "bvar.c"
int nsp_bvar_xdr_save(XDR *xdrs, NspBvar *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_bvar)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspBvar  *nsp_bvar_xdr_load_partial(XDR *xdrs, NspBvar *M)
{
 return M;
}

static NspBvar  *nsp_bvar_xdr_load(XDR *xdrs)
{
  NspBvar *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLBVAR;
  if ((H  = nsp_bvar_create_void(name,(NspTypeBase *) nsp_type_bvar))== NULLBVAR) return H;
  if ( nsp_bvar_create_partial(H) == FAIL) return NULLBVAR;
  if ((H  = nsp_bvar_xdr_load_partial(xdrs,H))== NULLBVAR) return H;
  if ( nsp_bvar_check_values(H) == FAIL) return NULLBVAR;
#line 52 "codegen/bvar.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 273 "bvar.c"
  return H;
}

/*
 * delete 
 */

void nsp_bvar_destroy_partial(NspBvar *H)
{
#line 55 "codegen/bvar.override"
/* verbatim in destroy */

#line 286 "bvar.c"
  if (H->value != NULL)
    nsp_object_destroy(&H->value);
  nsp_string_destroy(&(H->varname));
}

void nsp_bvar_destroy(NspBvar *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_bvar_destroy_partial(H);
  FREE(H);
}

#line 156 "codegen/bvar.override"
/*
 * info overriden
 */

int nsp_bvar_info(NspBvar *M, int indent,const char *name, int rec_level)
{
  return nsp_bvar_print(M,indent,name,rec_level);
}

#line 309 "bvar.c"
#line 183 "codegen/bvar.override"
/*
 * print overriden
 */

int nsp_bvar_print(NspBvar *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0)
	{
	  nsp_object_print(M->value,indent,"_val",rec_level);
	  Sciprintf1(indent,"%s=bvar(sym=%s,varname=\"%s\",value=_val);",pname,
		     (M->sym == TRUE) ? "%t" : "%f");
	}
      else
	{
	  nsp_object_print(M->value,indent,"_val",rec_level);
	  Sciprintf1(indent,"%s=bvar(sym=%s,varname=\"%s\",value=_val);",pname,
		     (M->sym == TRUE) ? "%t" : "%f");
	}
    }
  else
    {
      Sciprintf1(indent,"%s\t= name=\"%s\",symbolic=%s,type=%s,size=[%d,%d]\n",
		 pname,M->varname, (M->sym == TRUE ) ? "%t" : "%f",
		 nsp_object_type_as_string(M->value),
		 nsp_object_get_size(M->value,1),
		 nsp_object_get_size(M->value,2)
		 );
    }
  return TRUE;
}


#line 346 "bvar.c"
/*
 * latex print 
 */

int nsp_bvar_latex(NspBvar *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_bvar_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|sym|= %s\n",( M->sym == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
        if ( M->value->type->latex(M->value,FALSE,"value",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|varname|=\\verb@\"%s\"@\n",(M->varname==NULL) ? "NULL": M->varname);
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
 * for NspBvar objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspBvar   *nsp_bvar_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_bvar_id)  == TRUE  ) return ((NspBvar *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_bvar));
  return NULL;
}

int IsBvarObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_bvar_id);
}

int IsBvar(NspObject *O)
{
  return nsp_object_type(O,nsp_type_bvar_id);
}

NspBvar  *GetBvarCopy(Stack stack, int i)
{
  if (  GetBvar(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspBvar  *GetBvar(Stack stack, int i)
{
  NspBvar *M;
  if (( M = nsp_bvar_object(NthObj(i))) == NULLBVAR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspBvar instance 
 *-----------------------------------------------------*/

static NspBvar *nsp_bvar_create_void(const char *name,NspTypeBase *type)
{
 NspBvar *H  = (type == NULL) ? new_bvar() : type->new();
 if ( H ==  NULLBVAR)
  {
   Sciprintf("No more memory\n");
   return NULLBVAR;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLBVAR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_bvar_create_partial(NspBvar *H)
{
  return OK;
}

int nsp_bvar_check_values(NspBvar *H)
{
  if ( H->value == NULLOBJ) 
    {
     if (( H->value =(NspObject*) nsp_matrix_create("value",'r',0,0)) == NULL)
       return FAIL;
    }
  if ( H->varname == NULL) 
    {
  if (( H->varname = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  return OK;
}

NspBvar *nsp_bvar_create(const char *name,gboolean sym,NspObject* value,char* varname,NspTypeBase *type)
{
  NspBvar *H  = nsp_bvar_create_void(name,type);
  if ( H ==  NULLBVAR) return NULLBVAR;
  H->sym=sym;
  H->value= value;
  H->varname = varname;
  if ( nsp_bvar_check_values(H) == FAIL) return NULLBVAR;
#line 52 "codegen/bvar.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 473 "bvar.c"
  return H;
}


NspBvar *nsp_bvar_create_default(const char *name)
{
 NspBvar *H  = nsp_bvar_create_void(name,NULL);
 if ( H ==  NULLBVAR) return NULLBVAR;
  if ( nsp_bvar_check_values(H) == FAIL) return NULLBVAR;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspBvar *nsp_bvar_copy_partial(NspBvar *H,NspBvar *self)
{
  H->sym=self->sym;
  if ( self->value == NULL )
    { H->value = NULL;}
  else
    {
      if ((H->value = (NspObject *) nsp_object_copy_and_name("value",NSP_OBJECT(self->value))) == NULLOBJ) return NULL;
    }
  if ((H->varname = nsp_string_copy(self->varname)) == NULL) return NULL;
  return H;
}

NspBvar *nsp_bvar_copy(NspBvar *self)
{
  NspBvar *H  =nsp_bvar_create_void(NVOID,(NspTypeBase *) nsp_type_bvar);
  if ( H ==  NULLBVAR) return NULLBVAR;
  if ( nsp_bvar_copy_partial(H,self)== NULL) return NULLBVAR;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspBvar *nsp_bvar_full_copy_partial(NspBvar *H,NspBvar *self)
{
  H->sym=self->sym;
  if ( self->value == NULL )
    { H->value = NULL;}
  else
    {
      if ((H->value = (NspObject *) nsp_object_full_copy_and_name("value",NSP_OBJECT(self->value))) == NULLOBJ) return NULL;
    }
  if ((H->varname = nsp_string_copy(self->varname)) == NULL) return NULL;
  return H;
}

NspBvar *nsp_bvar_full_copy(NspBvar *self)
{
  NspBvar *H  =nsp_bvar_create_void(NVOID,(NspTypeBase *) nsp_type_bvar);
  if ( H ==  NULLBVAR) return NULLBVAR;
  if ( nsp_bvar_full_copy_partial(H,self)== NULL) return NULLBVAR;

#line 52 "codegen/bvar.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 536 "bvar.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspBvar
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_bvar_create(Stack stack, int rhs, int opt, int lhs)
{
  NspBvar *H;
  CheckStdRhs(0,0);
  /* want to be sure that type bvar is initialized */
  nsp_type_bvar = new_type_bvar(T_BASE);
  if(( H = nsp_bvar_create_void(NVOID,(NspTypeBase *) nsp_type_bvar)) == NULLBVAR) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_bvar_check_values(H) == FAIL) return RET_BUG;
  #line 52 "codegen/bvar.override"
/* verbatim in create/load/full_copy interface use RET_BUG for returned value */
#line 557 "bvar.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 59 "codegen/bvar.override"
/* override a method */
static int _wrap_bvar_get_value(NspBvar *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((Obj = nsp_object_copy(self->value)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1, Obj);
  return Max(lhs,1);
}

#line 577 "bvar.c"


#line 72 "codegen/bvar.override"
/* override a method */
static int _wrap_bvar_set_value(NspBvar *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj,*Arg;
  CheckRhs(1,1);
  CheckLhs(0,1);
  /* take care that argument can be an Hobj */
  if ((Arg =nsp_get_object(stack,1)) == NULLOBJ) return RET_BUG;
  if ((Obj = nsp_object_copy_and_name("ud",Arg)) == NULLOBJ)
    return RET_BUG;
  if ( IsMat(Obj))
    {
      /* be sure that matrix are expanded to double stream */
      NspMatrix *M = (NspMatrix *) Mat2double ((NspMatrix *) Obj);
      if ( M == NULLMAT)
	{
	  Scierror("Error: cannot convert a scalar matrix in double mode\n");
	  return RET_BUG;
	}
    }
  if ( self->value != NULL )
    {
      nsp_object_destroy(&self->value);
    }
  self->value = Obj;
  return 0;
}

#line 609 "bvar.c"


#line 102 "codegen/bvar.override"

static int _wrap_bvar_get_varname(NspBvar *self,Stack stack,int rhs,int opt,int lhs)
{
  if ( nsp_move_string(stack,1,self->varname,-1) ==FAIL)   return RET_BUG;
  return 1;
}

#line 620 "bvar.c"


#line 111 "codegen/bvar.override"

static int _wrap_bvar_set_varname(NspBvar *self,Stack stack,int rhs,int opt,int lhs)
{
  char *str;
  CheckRhs(1,1);
  CheckLhs(0,0);
  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((str = nsp_string_copy(str)) ==NULL) return RET_BUG;
  free(self->varname);
  self->varname=str;
  return 0;
}

#line 637 "bvar.c"


#line 126 "codegen/bvar.override"

static int _wrap_bvar_is_symbolic(NspBvar *self,Stack stack,int rhs,int opt,int lhs)
{
  if ( nsp_move_boolean(stack,1,(self->sym == TRUE))==FAIL) return RET_BUG;
  return 1;
}

#line 648 "bvar.c"


#line 135 "codegen/bvar.override"

static int _wrap_bvar_set_symbolic(NspBvar *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int status;
  if ( GetArgs(stack,rhs,opt,T,&status) == FAIL) return RET_BUG;
  self->sym = status;
  return 0;
}


#line 663 "bvar.c"


static NspMethods bvar_methods[] = {
  {"get_value",(nsp_method *) _wrap_bvar_get_value},
  {"set_value",(nsp_method *) _wrap_bvar_set_value},
  {"get_varname",(nsp_method *) _wrap_bvar_get_varname},
  {"set_varname",(nsp_method *) _wrap_bvar_set_varname},
  {"is_symbolic",(nsp_method *) _wrap_bvar_is_symbolic},
  {"set_symbolic",(nsp_method *) _wrap_bvar_set_symbolic},
  { NULL, NULL}
};

static NspMethods *bvar_get_methods(void) { return bvar_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab bvar_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 252 "codegen/bvar.override"

int _wrap_bvar_code_getvarname(Stack stack, int rhs, int opt, int lhs) /* bvar_get_varname */
{
  NspHash *H;
  int_types T[] = {obj, t_end};
  NspObject *Obj;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&Obj,&H) == FAIL) return RET_BUG;
  if ((H = nsp_hash_create(NVOID,20)) == NULL)    return RET_BUG;
  if ( bvar_code_getvarname(Obj, H) == FAIL)
    {
      nsp_hash_destroy(H);
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
}

#line 705 "bvar.c"


#line 272 "codegen/bvar.override"

int _wrap_bvar_code_countvarname(Stack stack, int rhs, int opt, int lhs) /* bvar_get_varname */
{
  char *name = NULL;
  int count=0;
  int_types T[] = {obj, string, s_int ,t_end};
  NspObject *Obj;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&Obj,&name,&count) == FAIL) return RET_BUG;
  if ( bvar_code_countvarname(Obj,name,&count) == FAIL)
    {
      return RET_BUG;
    }
  if ( nsp_move_double(stack,1,(double) count)== FAIL) return RET_BUG;
  return 1;
}

#line 726 "bvar.c"


#line 291 "codegen/bvar.override"

int _wrap_bvar_code_isvarname(Stack stack, int rhs, int opt, int lhs)
{
  int isvarname = FALSE;
  char *name = NULL;
  int_types T[] = {obj, string, t_end};
  NspObject *Obj;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&Obj,&name) == FAIL) return RET_BUG;
  if ( bvar_code_isvarname(Obj,name,&isvarname) == FAIL)
    {
      return RET_BUG;
    }
  if ( nsp_move_boolean(stack,1,isvarname)== FAIL) return RET_BUG;
  return 1;
}

#line 747 "bvar.c"


#line 310 "codegen/bvar.override"

int _wrap_bvar_code_replacevarname(Stack stack, int rhs, int opt, int lhs)
{
  int changed = FALSE;
  char *name = NULL;
  int_types T[] = {objcopy, string, obj, t_end};
  NspObject *Obj,*expr,*Res;
  CheckLhs(0,2);
  if ( GetArgs(stack,rhs,opt,T,&Obj,&name,&expr) == FAIL) return RET_BUG;
  Res = bvar_code_replacevarname(Obj,name,expr,&changed);
  if ( Res == NULL ) return RET_BUG;
  if ( nsp_move_boolean(stack,1,changed)== FAIL) return RET_BUG;
  if ( lhs >= 2 ) MoveObj(stack,2,Res);
  return Max(lhs,1);
}

#line 767 "bvar.c"


#line 328 "codegen/bvar.override"

int _wrap_bvar_code_vars(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  int_types T[] = { list ,  t_end};
  NspList *code;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&code) == FAIL) return RET_BUG;
  if ((H = nsp_hash_create(NVOID,20)) == NULL)    return RET_BUG;
  if ( bvar_code_vars(code,H) == FAIL)
    {
      nsp_hash_destroy(H);
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
}

#line 789 "bvar.c"


#line 348 "codegen/bvar.override"

int _wrap_bvar_code_vars_used(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  int_types T[] = { list ,  t_end};
  NspList *code;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&code) == FAIL) return RET_BUG;
  if ((H = nsp_hash_create(NVOID,20)) == NULL)    return RET_BUG;
  if ( bvar_code_vars_used(code,H) == FAIL)
    {
      nsp_hash_destroy(H);
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
}

#line 811 "bvar.c"


#line 368 "codegen/bvar.override"

int _wrap_bvar_code_replacevar(Stack stack, int rhs, int opt, int lhs)
{
  int callf = FALSE;
  NspObject *expr;
  char *name = NULL;
  NspMatrix *Inds;
  int_types T[] = { list , realmat, string, obj, t_end};
  NspList *code;
  CheckRhs(4,4);
  CheckLhs(0,2);
  if ( GetArgs(stack,rhs,opt,T,&code,&Inds,&name,&expr) == FAIL) return RET_BUG;
  if ( (code = nsp_list_copy(code)) == NULL) return RET_BUG;
  if ( bvar_code_replacevar(code,Inds,name,expr,&callf) == FAIL)
    {
      nsp_list_destroy(code);
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(code));
  if ( lhs == 2 )
    {
      if ( nsp_move_boolean(stack,2,callf) == FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}

#line 841 "bvar.c"


#line 396 "codegen/bvar.override"

int _wrap_bvar_code_varstatus(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Idx_used, *Idx_modified;
  char *name = NULL;
  int_types T[] = { list , string, t_end};
  NspList *code;
  CheckLhs(0,2);
  if ( GetArgs(stack,rhs,opt,T,&code,&name) == FAIL) return RET_BUG;
  if ( bvar_code_varstatus(code,&Idx_used,&Idx_modified,name) == FAIL)
    {
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(Idx_used));
  if ( lhs == 2 )
    MoveObj(stack,2,NSP_OBJECT(Idx_modified));
  return Max(1,lhs);
}

#line 864 "bvar.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Bvar_func[]={
  { "bvar_code_getvarname", _wrap_bvar_code_getvarname},
  { "bvar_code_countvarname", _wrap_bvar_code_countvarname},
  { "bvar_code_isvarname", _wrap_bvar_code_isvarname},
  { "bvar_code_replacevarname", _wrap_bvar_code_replacevarname},
  { "bvar_code_vars", _wrap_bvar_code_vars},
  { "bvar_code_vars_used", _wrap_bvar_code_vars_used},
  { "bvar_code_replacevar", _wrap_bvar_code_replacevar},
  { "bvar_code_varstatus", _wrap_bvar_code_varstatus},
  { "bvar_create", int_bvar_create},
  { NULL, NULL}
};

/* call ith function in the Bvar interface */

int Bvar_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Bvar_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Bvar_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Bvar_func[i].name;
  *f = Bvar_func[i].fonc;
}
void nsp_initialize_Bvar_types(void)
{
  new_type_bvar(T_BASE);
}

#line 417 "codegen/bvar.override"

NspBvar *nsp_bvar(NspObject *Obj,int sym)
{
  NspObject *O1;
  NspBvar *H;
  /* want to be sure that type bvar is initialized */
  nsp_type_bvar = new_type_bvar(T_BASE);
  if(( H = nsp_bvar_create_void(NVOID,(NspTypeBase *) nsp_type_bvar)) == NULLBVAR) return NULL;
  if ((O1 = nsp_object_copy_and_name("ud",Obj)) == NULLOBJ)
    {
      nsp_bvar_destroy(H);
      return NULL;
    }
  if ( IsMat(O1))
    {
      /* be sure that matrix are expanded to double stream */
      NspMatrix *M = (NspMatrix *) Mat2double ((NspMatrix *) O1);
      if ( M == NULLMAT)
	{
	  nsp_bvar_destroy(H);
	  nsp_object_destroy(&O1);
	  return NULL;
	}
    }
  if ( H->value != NULL )
    {
      nsp_object_destroy(&H->value);
    }
  H->value = O1;
  H->sym = sym;
  return H;
}


/* loop extraction for bvar variable
 * @str: name to give to created object
 * @O: a #NspObject pointer to use to store extracted column or a NULL pointer
 * @O1: a #NspObject from which we must extract columns
 * @i: undice of the column to be extracted
 * @rep: returned error value.
 */

static NspObject *nsp_bvar_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  NspBvar *b = (NspBvar *) O1;
  if (b->sym == TRUE)
    {
      Scierror("Error: loop cannot work for symbolic values\n");
      return NULLOBJ;
    }
  if ( O != NULL)
    {
      b->value->type->loop(str,((NspBvar *) O)->value,b->value,i,rep);
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
      Res1 = (NspObject *) nsp_bvar(Res,FALSE);
      if ((str1 = nsp_string_copy(str)) ==NULL) return NULLOBJ;
      free(((NspBvar *) Res1)->varname);
      ((NspBvar *) Res1)->varname=str1;
      if (nsp_object_set_name(Res1,str) == FAIL) return NULLOBJ;
      nsp_object_destroy(&Res);
      return Res1;
    }
}


static int nsp_bvar_is_true(void *Obj)
{
  NspBvar *b = (NspBvar *) Obj;
  if ( b->sym == TRUE )
    {
      Scierror("Error: do not use symbolic variables in conditions (if, while,case) \n");
      return FALSE;
    }
  else
    {
      return  NSP_OBJECT(b->value)->type->is_true(b->value);
    }
}

/* set of function used for bvar code */

static int bvar_code_is_op(NspObject *Obj)
{
  if ( IsHash(Obj) )
    {
      NspObject *O1;
      NspHash *H = (NspHash *) Obj;
      if (nsp_hash_find(H,"type",&O1) == FAIL )
	{
	  return FAIL;
	}
      else
	{
	  if ( IsSMat(O1) && ((NspSMatrix *) O1)->mn == 1 && strcmp(((NspSMatrix *) O1)->S[0],"op") == 0)
	    return OK;
	}
    }
  return FAIL;
}

/* */

static int bvar_code_countvarname(NspObject *Obj,const char *name, int *count)
{
  /* a bvar */
  if ( IsBvar(Obj))
    {
      NspBvar *B = (NspBvar *) Obj;
      if ( strcmp(B->varname,name)== 0 ) (*count)++;
      return OK;
    }
  /* a hash table with type="op" */
  if ( bvar_code_is_op(Obj) == OK)
    {
      NspObject *Elt, *O1;
      NspHash *H1= (NspHash *) Obj;
      if (nsp_hash_find(H1,"exp",&O1) == FAIL )
	return FAIL;
      if ( ! IsList(O1) ) return FAIL;
      if ( (Elt= nsp_list_get_element((NspList *) O1,2)) == NULL)
	return FAIL;
      return bvar_code_countvarname(Elt,name,count);
    }
  /* a list */
  if ( IsList(Obj) )
    {
      /* loop on elements */
      NspList *L= (NspList *) Obj;
      Cell *C = L->first;
      while ( C != NULLCELL)
	{
	  if ( C->O != NULLOBJ )
	    {
	      if ( bvar_code_countvarname(C->O,name,count) == FAIL) return FAIL;
	    }
	  C = C->next;
	}
    }
  return OK;
}

/* */

static int bvar_code_getvarname(NspObject *Obj,NspHash *H)
{
  /* a bvar */
  if ( IsBvar(Obj))
    {
      NspObject *Res;
      NspBvar *B = (NspBvar *) Obj;
      if ((Res=nsp_create_true_object(B->varname)) == NULL) return FAIL;
      if (nsp_hash_enter( H,Res) == FAIL ) return FAIL;
      return OK;
    }
  /* a hash table with type="op" */
  if ( bvar_code_is_op(Obj) == OK)
    {
      NspObject *Elt, *O1;
      NspHash *H1= (NspHash *) Obj;
      if (nsp_hash_find(H1,"exp",&O1) == FAIL )
	return FAIL;
      if ( ! IsList(O1) ) return FAIL;
      if ( (Elt= nsp_list_get_element((NspList *) O1,2)) == NULL)
	return FAIL;
      return bvar_code_getvarname(Elt,H);
    }
  /* a list */
  if ( IsList(Obj) )
    {
      /* loop on elements */
      NspList *L= (NspList *) Obj;
      Cell *C = L->first;
      while ( C != NULLCELL)
	{
	  if ( C->O != NULLOBJ )
	    {
	      if ( bvar_code_getvarname(C->O,H) == FAIL) return FAIL;
	    }
	  C = C->next;
	}
    }
  return OK;
}

/* */

static int bvar_code_isvarname(NspObject *Obj,const char *name,int *res)
{
  /* a bvar */
  *res = FALSE;
  if ( IsBvar(Obj))
    {
      NspBvar *B = (NspBvar *) Obj;
      *res = (strcmp(B->varname,name)==0) ? TRUE: FALSE;
      return OK;
    }
  /* a hash table with type="op" */
  if ( bvar_code_is_op(Obj) == OK)
    {
      NspObject *Elt, *O1;
      NspHash *H1= (NspHash *) Obj;
      if (nsp_hash_find(H1,"exp",&O1) == FAIL )
	return FAIL;
      if ( ! IsList(O1) ) return FAIL;
      if ( (Elt= nsp_list_get_element((NspList *) O1,2)) == NULL)
	return FAIL;
      return bvar_code_isvarname(Elt,name,res);
    }
  /* a list */
  if ( IsList(Obj) )
    {
      *res = FALSE;
      /* loop on elements */
      NspList *L= (NspList *) Obj;
      Cell *C = L->first;
      while ( C != NULLCELL)
	{
	  if ( C->O != NULLOBJ )
	    {
	      if ( bvar_code_isvarname(C->O,name,res) == FAIL) return FAIL;
	      if ( *res == TRUE ) return OK;
	    }
	  C = C->next;
	}
    }
  return OK;
}

/* replaces name by exp
 * changed is set to TRUE if a replacement was done
 */

static NspObject *bvar_code_replacevarname(NspObject *Obj,const char *name,NspObject *expr,int *changed)
{
  *changed = FALSE;
  /* a bvar */
  if ( IsBvar(Obj))
    {
      NspBvar *B = (NspBvar *) Obj;
      if  (strcmp(B->varname,name)==0)
	{
	  *changed = TRUE;
	  return nsp_object_copy_with_name(expr);
	}
      return Obj;
    }
  /* a hash table with type="op" */
  if ( bvar_code_is_op(Obj) == OK)
    {
      NspObject *Elt,*Elt1, *O1;
      NspHash *H1= (NspHash *) Obj;
      if (nsp_hash_find(H1,"exp",&O1) == FAIL )	return NULL;
      if ( ! IsList(O1) ) return NULL;
      if ((Elt= nsp_list_get_element((NspList *) O1,2)) == NULL) return NULL;
      if ((Elt1= bvar_code_replacevarname(Elt,name,expr,changed)) == NULL) return NULL;
      if ( *changed == TRUE && Elt1 != Elt )
	{
	  if ( nsp_list_insert((NspList *) O1,Elt1,2)==FAIL) return NULL;
	}
      return Obj;
    }
  /* a list */
  if ( IsList(Obj) )
    {
      int changed1 = FALSE;
      /* loop on elements */
      NspList *L= (NspList *) Obj;
      Cell *C = L->first;
      while ( C != NULLCELL)
	{
	  changed1= FALSE;
	  if ( C->O != NULLOBJ )
	    {
	      NspObject *Res;
	      if ((Res= bvar_code_replacevarname(C->O,name,expr,&changed1)) == NULL) return NULL;
	      if ( changed1 == TRUE )   *changed = TRUE;
	      if  (Res != C->O)
		{
		  nsp_object_destroy(&C->O);
		  C->O = Res;
		}
	    }
	  C = C->next;
	}
      return Obj;
    }
  return Obj;
}

/* */

static int bvar_code_vars(NspList *L, NspHash *H)
{
  /* walk through the list */
  Cell *C = L->first;
  while ( C != NULLCELL)
    {
      NspObject *Elt = C->O;
      if ( Elt != NULLOBJ )
	{
	  int i;
	  const char *name;
	  NspObject *elt1,*eltn;
	  if (! IsList(Elt) ) goto fail;
	  /* get first element */
	  if ( (elt1= nsp_list_get_element((NspList *) Elt,1)) == NULL) goto fail;
	  if ( ! ( IsSMat(elt1) && ((NspSMatrix *) elt1)->mn ==1 )) goto fail;
	  name = ((NspSMatrix *) elt1)->S[0];
	  if (strcmp(name,"nop" )==0)
	    {}
	  else if (strcmp(name,"annotation")==0)
	    {}
	  else if (strcmp(name,"set")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"bi_insert")==0)
	    {
	      for ( i = 2 ; i <= 5 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"uni_insert")==0)
	    {
	      for ( i = 2 ; i <= 4 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"assign")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"mcopy")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"if_expr")==0)
	    {
	      for ( i = 2 ; i <= 4 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"endfunction")==0)
	    {}
	  else if (strcmp(name,"switch_expr")==0)
	    {}
	  else if (strcmp(name,"callf")==0)
	    {
	      for ( i = 2 ; i <= 2 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	}
      C = C->next;
    }
  return OK;
 fail:
  return FAIL;
}

/* similar to bvar_code_vars but just checks use of variables */

static int bvar_code_vars_used(NspList *L, NspHash *H)
{
  /* walk through the list */
  Cell *C = L->first;
  while ( C != NULLCELL)
    {
      NspObject *Elt = C->O;
      if ( Elt != NULLOBJ )
	{
	  int i;
	  const char *name;
	  NspObject *elt1,*eltn;
	  if (! IsList(Elt) ) goto fail;
	  /* get first element */
	  if ( (elt1= nsp_list_get_element((NspList *) Elt,1)) == NULL) goto fail;
	  if ( ! ( IsSMat(elt1) && ((NspSMatrix *) elt1)->mn ==1 )) goto fail;
	  name = ((NspSMatrix *) elt1)->S[0];
	  if (strcmp(name,"nop" )==0)
	    {}
	  else if (strcmp(name,"annotation")==0)
	    {}
	  else if (strcmp(name,"set")==0)
	    {
	      for ( i = 3 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"bi_insert")==0)
	    {
	      for ( i = 3 ; i <= 5 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"uni_insert")==0)
	    {
	      for ( i = 3 ; i <= 4 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"assign")==0)
	    {
	      for ( i = 3 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"mcopy")==0)
	    {
	      for ( i = 3 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"if_expr")==0)
	    {
	      for ( i = 3 ; i <= 4 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if (bvar_code_getvarname(eltn,H) == FAIL ) goto fail;
		}
	    }
	  else if (strcmp(name,"endfunction")==0)
	    {}
	  else if (strcmp(name,"switch_expr")==0)
	    {}
	  else if (strcmp(name,"callf")==0)
	    {}
	}
      C = C->next;
    }
  return OK;
 fail:
  return FAIL;
}


/* L is changed, expr is copied each time it is inserted */

static int bvar_code_replacevar(NspList *L,NspMatrix *Inds, const char *vname,NspObject *expr, int *callf)
{
  int count = 0, inds_i=0;
  NspObject *Res;
  int changed=FALSE;
  /* walk through the list */
  Cell *C = L->first;
  *callf = FALSE;
  if ( Inds->mn == 0) return OK;  /* nothing to do */
  while ( C != NULLCELL)
    {
      NspObject *Elt = C->O;
      count++;
      if ( count == Inds->R[inds_i] &&  Elt != NULLOBJ )
	{
	  int i;
	  const char *name;
	  NspObject *elt1,*eltn;
	  inds_i = Min(inds_i+1,Inds->mn -1);
	  if (! IsList(Elt) ) goto fail;
	  /* get first element */
	  if ( (elt1= nsp_list_get_element((NspList *) Elt,1)) == NULL) goto fail;
	  if ( ! ( IsSMat(elt1) && ((NspSMatrix *) elt1)->mn ==1 )) goto fail;
	  name = ((NspSMatrix *) elt1)->S[0];
	  if (strcmp(name,"nop" )==0)
	    {}
	  else if (strcmp(name,"annotation")==0)
	    {}
	  else if (strcmp(name,"set")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( (Res = bvar_code_replacevarname(eltn,vname,expr,&changed)) == NULL) goto fail;
		  if ( changed == TRUE && Res != eltn )
		    {
		      if ( nsp_list_insert((NspList *) Elt,Res,i)==FAIL) goto fail;
		    }
 		}
	    }
	  else if (strcmp(name,"bi_insert")==0)
	    {
	      for ( i = 2 ; i <= 5 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( (Res = bvar_code_replacevarname(eltn,vname,expr,&changed)) == NULL) goto fail;
		  if ( changed == TRUE && Res != eltn )
		    {
		      if ( nsp_list_insert((NspList *) Elt,Res,i)==FAIL) goto fail;
		    }
		}
	    }
	  else if (strcmp(name,"uni_insert")==0)
	    {
	      for ( i = 2 ; i <= 4 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( (Res = bvar_code_replacevarname(eltn,vname,expr,&changed)) == NULL) goto fail;
		  if ( changed == TRUE && Res != eltn )
		    {
		      if ( nsp_list_insert((NspList *) Elt,Res,i)==FAIL) goto fail;
		    }
		}
	    }
	  else if (strcmp(name,"assign")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( (Res = bvar_code_replacevarname(eltn,vname,expr,&changed)) == NULL) goto fail;
		  if ( changed == TRUE && Res != eltn )
		    {
		      if ( nsp_list_insert((NspList *) Elt,Res,i)==FAIL) goto fail;
		    }
		}
	    }
	  else if (strcmp(name,"mcopy")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( (Res = bvar_code_replacevarname(eltn,vname,expr,&changed)) == NULL) goto fail;
		  if ( changed == TRUE && Res != eltn )
		    {
		      if ( nsp_list_insert((NspList *) Elt,Res,i)==FAIL) goto fail;
		    }
		}
	    }
	  else if (strcmp(name,"if_expr")==0)
	    {
	      for ( i = 2 ; i <= 4 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( (Res = bvar_code_replacevarname(eltn,vname,expr,&changed)) == NULL) goto fail;
		  if ( changed == TRUE && Res != eltn )
		    {
		      if ( nsp_list_insert((NspList *) Elt,Res,i)==FAIL) goto fail;
		    }
		}
	    }
	  else if (strcmp(name,"endfunction")==0)
	    {}
	  else if (strcmp(name,"switch_expr")==0)
	    {}
	  else if (strcmp(name,"callf")==0)
	    {
	      for ( i = 2 ; i <= 2 ; i++)
		{
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ((Res = bvar_code_replacevarname(eltn,vname,expr,&changed)) == NULL) goto fail;
		  if ( changed == TRUE )
		    {
		      *callf = TRUE;
		      if ( Res != eltn )
			if ( nsp_list_insert((NspList *) Elt,Res,i)==FAIL) goto fail;
		    }
		}

	    }
	}
      C = C->next;
    }
  return OK;
 fail:
  return FAIL;
}

static int bvar_code_varstatus(NspList *L,NspMatrix **Idx_used,NspMatrix **Idx_modified, const char *vname)
{
  NspMatrix *Used,*Modified;
  int count = 0;
  /* walk through the list */
  Cell *C = L->first;
  if (( Used= nsp_matrix_create(NVOID,'r',1,0)) == NULLMAT) goto fail;
  if (( Modified = nsp_matrix_create(NVOID,'r',1,0)) == NULLMAT) goto fail;
  while ( C != NULLCELL)
    {
      NspObject *Elt = C->O;
      count++;
      int ok_used=FALSE, ok_modified=FALSE;
      if ( Elt != NULLOBJ )
	{
	  int i;
	  const char *name;
	  NspObject *elt1,*eltn;
	  if (! IsList(Elt) ) goto fail;
	  /* get first element */
	  if ( (elt1= nsp_list_get_element((NspList *) Elt,1)) == NULL) goto fail;
	  if ( ! ( IsSMat(elt1) && ((NspSMatrix *) elt1)->mn ==1 )) goto fail;
	  name = ((NspSMatrix *) elt1)->S[0];
	  if (strcmp(name,"nop" )==0)
	    {}
	  else if (strcmp(name,"annotation")==0)
	    {}
	  else if (strcmp(name,"set")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  int ok;
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( bvar_code_isvarname(eltn,vname,&ok)== FAIL ) goto fail;
		  if ( i == 2 ) ok_modified = ok; else ok_used = ok;
 		}
	    }
	  else if (strcmp(name,"bi_insert")==0)
	    {
	      for ( i = 2 ; i <= 5 ; i++)
		{
		  int ok;
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( bvar_code_isvarname(eltn,vname,&ok)== FAIL ) goto fail;
		  if ( i == 2 ) ok_modified = ok; else ok_used = ok;
 		}
	    }
	  else if (strcmp(name,"uni_insert")==0)
	    {
	      for ( i = 2 ; i <= 4 ; i++)
		{
		  int ok;
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( bvar_code_isvarname(eltn,vname,&ok)== FAIL ) goto fail;
		  if ( i == 2 ) ok_modified = ok; else ok_used = ok;
 		}

	    }
	  else if (strcmp(name,"assign")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  int ok;
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( bvar_code_isvarname(eltn,vname,&ok)== FAIL ) goto fail;
		  if ( i == 2 ) ok_modified = ok; else ok_used = ok;
 		}

	    }
	  else if (strcmp(name,"mcopy")==0)
	    {
	      for ( i = 2 ; i <= 3 ; i++)
		{
		  int ok;
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( bvar_code_isvarname(eltn,vname,&ok)== FAIL ) goto fail;
		  if ( i == 2 ) ok_modified = ok; else ok_used = ok;
 		}

	    }
	  else if (strcmp(name,"if_expr")==0)
	    {
	      for ( i = 2 ; i <= 4 ; i++)
		{
		  int ok;
		  if ((eltn= nsp_list_get_element((NspList *) Elt,i)) == NULL) goto fail;
		  if ( bvar_code_isvarname(eltn,vname,&ok)== FAIL ) goto fail;
		  ok_used = ok;
		  if ( i != 2 ) ok_modified = ok;
 		}

	    }
	  else if (strcmp(name,"endfunction")==0)
	    {}
	  else if (strcmp(name,"switch_expr")==0)
	    {}
	  else if (strcmp(name,"callf")==0)
	    {
	      NspHash *H;
	      NspObject *O1,*O2,*out_list,*in_list;
	      if ((eltn= nsp_list_get_element((NspList *) Elt,2)) == NULL) goto fail;
	      /* now we need
	       * ins=elt(2).exp(2)(3); // in-list
	       * ok_used=bvar_code_isvarname(ins,varname);
	       * outs=elt(2).exp(2)(2);// out-list
	       * ok_modified=bvar_code_isvarname(outs,varname);
	       */
	      H = (NspHash *) eltn;
	      if (nsp_hash_find(H,"exp",&O1) == FAIL ) goto fail;
	      if ( ! IsList(O1) ) goto fail;
	      if ( (O2 = nsp_list_get_element((NspList *) O1,2)) == NULL) goto fail;
	      if ( ! IsList(O2) ) goto fail;
	      if ( (out_list= nsp_list_get_element((NspList *) O2,2)) == NULL) goto fail;
	      if ( (in_list= nsp_list_get_element((NspList *) O2,3)) == NULL) goto fail;
	      if ( bvar_code_countvarname(in_list,vname,&ok_used)== FAIL ) goto fail;
	      if ( bvar_code_countvarname(out_list,vname,&ok_modified)== FAIL ) goto fail;
	    }
	  if ( ok_used == TRUE )
	    {
	      if ( nsp_matrix_add_columns(Used,1,count) == FAIL) goto fail;
	    }
	  if ( ok_modified == TRUE )
	    {
	      if ( nsp_matrix_add_columns(Modified,1,count) == FAIL) goto fail;
	    }
	}
      C = C->next;
    }
  *Idx_used = Used;
  *Idx_modified = Modified;
  return OK;
 fail:
  return FAIL;
}

#line 1645 "bvar.c"
