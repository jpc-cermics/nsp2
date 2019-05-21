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





#line 20 "codegen/astv.override"
#include <nsp/objects.h>
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/parse.h>

#line 34 "astv.c"

/* -----------NspAstv ----------- */


#define  NspAstv_Private 
#include <nsp/objects.h>
#include <nsp/astv.h>
#include <nsp/interf.h>

/* 
 * NspAstv inherits from Object 
 */

int nsp_type_astv_id=0;
NspTypeAstv *nsp_type_astv=NULL;

/*
 * Type object for NspAstv 
 * all the instance of NspTypeAstv share the same id. 
 * nsp_type_astv: is an instance of NspTypeAstv 
 *    used for objects of NspAstv type (i.e built with new_astv) 
 * other instances are used for derived classes 
 */
NspTypeAstv *new_type_astv(type_mode mode)
{
  NspTypeAstv *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_astv != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_astv;
    }
  if (( type =  malloc(sizeof(NspTypeAstv))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = astv_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = astv_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_astv;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for astv */ 

  top->pr = (print_func *) nsp_astv_print;
  top->dealloc = (dealloc_func *) nsp_astv_destroy;
  top->copy  =  (copy_func *) nsp_astv_copy;
  top->size  = (size_func *) nsp_astv_size;
  top->s_type =  (s_type_func *) nsp_astv_type_as_string;
  top->sh_type = (sh_type_func *) nsp_astv_type_short_string;
  top->info = (info_func *) nsp_astv_info;
  /* top->is_true = (is_true_func  *) nsp_astv_is_true; */
  /* top->loop =(loop_func *) nsp_astv_loop;*/
#line 138 "codegen/astv.override"
top->path_extract = (path_func *) NULL; /* path extract as for matrix type */

#line 96 "astv.c"
  top->get_from_obj = (get_from_obj_func *) nsp_astv_object;
  top->eq  = (eq_func *) nsp_astv_eq;
  top->neq  = (eq_func *) nsp_astv_neq;
  top->save  = (save_func *) nsp_astv_xdr_save;
  top->load  = (load_func *) nsp_astv_xdr_load;
  top->create = (create_func*) int_astv_create;
  top->latex = (print_func *) nsp_astv_latex;
  top->full_copy = (copy_func *) nsp_astv_full_copy;

  /* specific methods for astv */

  type->init = (init_func *) init_astv;

  /* 
   * NspAstv interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_astv_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAstv called nsp_type_astv
       */
      type->id =  nsp_type_astv_id = nsp_new_type_id();
      nsp_type_astv = type;
      if ( nsp_register_type(nsp_type_astv) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_astv(mode);
    }
  else 
    {
      type->id = nsp_type_astv_id;
      return type;
    }
}

/*
 * initialize NspAstv instances 
 * locally and by calling initializer on parent class 
 */

static int init_astv(NspAstv *Obj,NspTypeAstv *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->hv = FALSE;
  Obj->rows = 0;
  Obj->columns = 0;
  Obj->value = NULLOBJ;
  Obj->ast_rows = NULLOBJ;
  Obj->ast_columns = NULLOBJ;
  Obj->ast_value = NULLOBJ;
  Obj->args = NULLOBJ;
  Obj->stype = nsp_new_string("",-1);
  Obj->ssubtype = nsp_new_string("",-1);
 return OK;
}

/*
 * new instance of NspAstv 
 */

NspAstv *new_astv() 
{
  NspAstv *loc;
  /* type must exists */
  nsp_type_astv = new_type_astv(T_BASE);
  if ( (loc = malloc(sizeof(NspAstv)))== NULLASTV) return loc;
  /* initialize object */
  if ( init_astv(loc,nsp_type_astv) == FAIL) return NULLASTV;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAstv 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_astv_size(NspAstv *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char astv_type_name[]="Astv";
static char astv_short_type_name[]="astv";

static char *nsp_astv_type_as_string(void)
{
  return(astv_type_name);
}

static char *nsp_astv_type_short_string(NspObject *v)
{
  return(astv_short_type_name);
}

#line 192 "codegen/astv.override"

/*
 * A == B 
 */

static int nsp_astv_eq(NspAstv *A, NspObject *B)
{
  NspAstv *loc = (NspAstv *) B;
  if ( check_cast(B,nsp_type_astv_id) == FALSE) return FALSE ;
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

static int nsp_astv_neq(NspAstv *A, NspObject *B)
{
  return ( nsp_astv_eq(A,B) == TRUE ) ? FALSE : TRUE;
}


#line 235 "astv.c"
int nsp_astv_xdr_save(XDR *xdrs, NspAstv *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_astv)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAstv  *nsp_astv_xdr_load_partial(XDR *xdrs, NspAstv *M)
{
 return M;
}

static NspAstv  *nsp_astv_xdr_load(XDR *xdrs)
{
  NspAstv *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLASTV;
  if ((H  = nsp_astv_create_void(name,(NspTypeBase *) nsp_type_astv))== NULLASTV) return H;
  if ( nsp_astv_create_partial(H) == FAIL) return NULLASTV;
  if ((H  = nsp_astv_xdr_load_partial(xdrs,H))== NULLASTV) return H;
  if ( nsp_astv_check_values(H) == FAIL) return NULLASTV;
#line 40 "codegen/astv.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 266 "astv.c"
  return H;
}

/*
 * delete 
 */

void nsp_astv_destroy_partial(NspAstv *H)
{
#line 43 "codegen/astv.override"
/* verbatim in destroy */

#line 279 "astv.c"
  if (H->value != NULL)
    nsp_object_destroy(&H->value);
  if (H->ast_rows != NULL)
    nsp_object_destroy(&H->ast_rows);
  if (H->ast_columns != NULL)
    nsp_object_destroy(&H->ast_columns);
  if (H->ast_value != NULL)
    nsp_object_destroy(&H->ast_value);
  if (H->args != NULL)
    nsp_object_destroy(&H->args);
  nsp_string_destroy(&(H->stype));
  nsp_string_destroy(&(H->ssubtype));
}

void nsp_astv_destroy(NspAstv *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_astv_destroy_partial(H);
  FREE(H);
}

#line 142 "codegen/astv.override"
/*
 * info overriden 
 */

int nsp_astv_info(NspAstv *M, int indent,const char *name, int rec_level)
{
  return nsp_astv_print(M,indent,name,rec_level);
}

#line 311 "astv.c"
#line 153 "codegen/astv.override"
/*
 * print overriden 
 */

int nsp_astv_print(NspAstv *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=astv([%d,%d],\"%s\",%s)",pname,
		     M->rows, M->columns, (M->hv == FALSE) ? "" :
		     nsp_object_type_as_string(M->value),
		     ( M->hv == FALSE) ? "%f":"%t");
	}
      else 
	{
	  Sciprintf1(indent,"astv([%d,%d],\"%s\",%s)",
		     M->rows, M->columns, (M->hv == FALSE) ? "" :
		     nsp_object_type_as_string(M->value),
		     ( M->hv == FALSE) ? "%f":"%t");
		     
	}
    }
  else 
    {
      Sciprintf1(indent,"%s\t=[%d,%d],\"%s\",%s\n",pname,
		 M->rows, M->columns, (M->hv == FALSE) ? "" :
		 nsp_object_type_as_string(M->value),
		 ( M->hv == FALSE) ? "%f":"%t");

    }
  return TRUE;
}


#line 350 "astv.c"
/*
 * latex print 
 */

int nsp_astv_latex(NspAstv *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_astv_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|hv|= %s\n",( M->hv == TRUE) ? "T" : "F" );
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|rows|= \\numprint{%d}\n",M->rows);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|columns|= \\numprint{%d}\n",M->columns);
  Sciprintf1(2,"\\\\\n");
        if ( M->value->type->latex(M->value,FALSE,"value",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(2,"\\\\\n");
        if ( M->ast_rows->type->latex(M->ast_rows,FALSE,"ast_rows",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(2,"\\\\\n");
        if ( M->ast_columns->type->latex(M->ast_columns,FALSE,"ast_columns",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(2,"\\\\\n");
        if ( M->ast_value->type->latex(M->ast_value,FALSE,"ast_value",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(2,"\\\\\n");
        if ( M->args->type->latex(M->args,FALSE,"args",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|stype|=\\verb@\"%s\"@\n",(M->stype==NULL) ? "NULL": M->stype);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|ssubtype|=\\verb@\"%s\"@\n",(M->ssubtype==NULL) ? "NULL": M->ssubtype);
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
 * for NspAstv objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAstv   *nsp_astv_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_astv_id)  == TRUE  ) return ((NspAstv *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_astv));
  return NULL;
}

int IsAstvObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_astv_id);
}

int IsAstv(NspObject *O)
{
  return nsp_object_type(O,nsp_type_astv_id);
}

NspAstv  *GetAstvCopy(Stack stack, int i)
{
  if (  GetAstv(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAstv  *GetAstv(Stack stack, int i)
{
  NspAstv *M;
  if (( M = nsp_astv_object(NthObj(i))) == NULLASTV)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspAstv instance 
 *-----------------------------------------------------*/

static NspAstv *nsp_astv_create_void(const char *name,NspTypeBase *type)
{
 NspAstv *H  = (type == NULL) ? new_astv() : type->new();
 if ( H ==  NULLASTV)
  {
   Sciprintf("No more memory\n");
   return NULLASTV;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLASTV;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_astv_create_partial(NspAstv *H)
{
  return OK;
}

int nsp_astv_check_values(NspAstv *H)
{
  if ( H->value == NULLOBJ) 
    {
     if (( H->value =(NspObject*) nsp_matrix_create("value",'r',0,0)) == NULL)
       return FAIL;
    }
  if ( H->ast_rows == NULLOBJ) 
    {
     if (( H->ast_rows =(NspObject*) nsp_matrix_create("ast_rows",'r',0,0)) == NULL)
       return FAIL;
    }
  if ( H->ast_columns == NULLOBJ) 
    {
     if (( H->ast_columns =(NspObject*) nsp_matrix_create("ast_columns",'r',0,0)) == NULL)
       return FAIL;
    }
  if ( H->ast_value == NULLOBJ) 
    {
     if (( H->ast_value =(NspObject*) nsp_matrix_create("ast_value",'r',0,0)) == NULL)
       return FAIL;
    }
  if ( H->args == NULLOBJ) 
    {
     if (( H->args =(NspObject*) nsp_matrix_create("args",'r',0,0)) == NULL)
       return FAIL;
    }
  if ( H->stype == NULL) 
    {
  if (( H->stype = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->ssubtype == NULL) 
    {
  if (( H->ssubtype = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  return OK;
}

NspAstv *nsp_astv_create(const char *name,gboolean hv,int rows,int columns,NspObject* value,NspObject* ast_rows,NspObject* ast_columns,NspObject* ast_value,NspObject* args,char* stype,char* ssubtype,NspTypeBase *type)
{
  NspAstv *H  = nsp_astv_create_void(name,type);
  if ( H ==  NULLASTV) return NULLASTV;
  H->hv=hv;
  H->rows=rows;
  H->columns=columns;
  H->value= value;
  H->ast_rows= ast_rows;
  H->ast_columns= ast_columns;
  H->ast_value= ast_value;
  H->args= args;
  H->stype = stype;
  H->ssubtype = ssubtype;
  if ( nsp_astv_check_values(H) == FAIL) return NULLASTV;
#line 40 "codegen/astv.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 523 "astv.c"
  return H;
}


NspAstv *nsp_astv_create_default(const char *name)
{
 NspAstv *H  = nsp_astv_create_void(name,NULL);
 if ( H ==  NULLASTV) return NULLASTV;
  if ( nsp_astv_check_values(H) == FAIL) return NULLASTV;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAstv *nsp_astv_copy_partial(NspAstv *H,NspAstv *self)
{
  H->hv=self->hv;
  H->rows=self->rows;
  H->columns=self->columns;
  if ( self->value == NULL )
    { H->value = NULL;}
  else
    {
      if ((H->value = (NspObject *) nsp_object_copy_and_name("value",NSP_OBJECT(self->value))) == NULLOBJ) return NULL;
    }
  if ( self->ast_rows == NULL )
    { H->ast_rows = NULL;}
  else
    {
      if ((H->ast_rows = (NspObject *) nsp_object_copy_and_name("ast_rows",NSP_OBJECT(self->ast_rows))) == NULLOBJ) return NULL;
    }
  if ( self->ast_columns == NULL )
    { H->ast_columns = NULL;}
  else
    {
      if ((H->ast_columns = (NspObject *) nsp_object_copy_and_name("ast_columns",NSP_OBJECT(self->ast_columns))) == NULLOBJ) return NULL;
    }
  if ( self->ast_value == NULL )
    { H->ast_value = NULL;}
  else
    {
      if ((H->ast_value = (NspObject *) nsp_object_copy_and_name("ast_value",NSP_OBJECT(self->ast_value))) == NULLOBJ) return NULL;
    }
  if ( self->args == NULL )
    { H->args = NULL;}
  else
    {
      if ((H->args = (NspObject *) nsp_object_copy_and_name("args",NSP_OBJECT(self->args))) == NULLOBJ) return NULL;
    }
  if ((H->stype = nsp_string_copy(self->stype)) == NULL) return NULL;
  if ((H->ssubtype = nsp_string_copy(self->ssubtype)) == NULL) return NULL;
  return H;
}

NspAstv *nsp_astv_copy(NspAstv *self)
{
  NspAstv *H  =nsp_astv_create_void(NVOID,(NspTypeBase *) nsp_type_astv);
  if ( H ==  NULLASTV) return NULLASTV;
  if ( nsp_astv_copy_partial(H,self)== NULL) return NULLASTV;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspAstv *nsp_astv_full_copy_partial(NspAstv *H,NspAstv *self)
{
  H->hv=self->hv;
  H->rows=self->rows;
  H->columns=self->columns;
  if ( self->value == NULL )
    { H->value = NULL;}
  else
    {
      if ((H->value = (NspObject *) nsp_object_full_copy_and_name("value",NSP_OBJECT(self->value))) == NULLOBJ) return NULL;
    }
  if ( self->ast_rows == NULL )
    { H->ast_rows = NULL;}
  else
    {
      if ((H->ast_rows = (NspObject *) nsp_object_full_copy_and_name("ast_rows",NSP_OBJECT(self->ast_rows))) == NULLOBJ) return NULL;
    }
  if ( self->ast_columns == NULL )
    { H->ast_columns = NULL;}
  else
    {
      if ((H->ast_columns = (NspObject *) nsp_object_full_copy_and_name("ast_columns",NSP_OBJECT(self->ast_columns))) == NULLOBJ) return NULL;
    }
  if ( self->ast_value == NULL )
    { H->ast_value = NULL;}
  else
    {
      if ((H->ast_value = (NspObject *) nsp_object_full_copy_and_name("ast_value",NSP_OBJECT(self->ast_value))) == NULLOBJ) return NULL;
    }
  if ( self->args == NULL )
    { H->args = NULL;}
  else
    {
      if ((H->args = (NspObject *) nsp_object_full_copy_and_name("args",NSP_OBJECT(self->args))) == NULLOBJ) return NULL;
    }
  if ((H->stype = nsp_string_copy(self->stype)) == NULL) return NULL;
  if ((H->ssubtype = nsp_string_copy(self->ssubtype)) == NULL) return NULL;
  return H;
}

NspAstv *nsp_astv_full_copy(NspAstv *self)
{
  NspAstv *H  =nsp_astv_create_void(NVOID,(NspTypeBase *) nsp_type_astv);
  if ( H ==  NULLASTV) return NULLASTV;
  if ( nsp_astv_full_copy_partial(H,self)== NULL) return NULLASTV;

#line 40 "codegen/astv.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 640 "astv.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspAstv
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_astv_create(Stack stack, int rhs, int opt, int lhs)
{
  NspAstv *H;
  CheckStdRhs(0,0);
  /* want to be sure that type astv is initialized */
  nsp_type_astv = new_type_astv(T_BASE);
  if(( H = nsp_astv_create_void(NVOID,(NspTypeBase *) nsp_type_astv)) == NULLASTV) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_astv_check_values(H) == FAIL) return RET_BUG;
  #line 40 "codegen/astv.override"
/* verbatim in create/load/full_copy interface use RET_BUG for returned value */
#line 661 "astv.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 61 "codegen/astv.override"
/* override a method */
static int _wrap_astv_get_value(NspAstv *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  MoveObj(stack,1, self->value);
  return Max(lhs,1);
}

#line 679 "astv.c"


#line 72 "codegen/astv.override"
/* override a method */
static int _wrap_astv_have_value(NspAstv *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  if ( nsp_move_boolean(stack,1,self->hv) == FAIL)
    return RET_BUG;
  return Max(lhs,1);
}

#line 693 "astv.c"


#line 84 "codegen/astv.override"
/* override a method */
static int _wrap_astv_set_value(NspAstv *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  CheckRhs(1,1);
  CheckLhs(0,1); 
  if ((Obj = nsp_object_copy_and_name("ud",NthObj(1))) == NULLOBJ) 
    return RET_BUG;
  if ( self->value != NULL )
    {
      nsp_object_destroy(&self->value);
    }
  self->value = Obj;
  self->hv = TRUE;
  self->rows= nsp_object_get_size(self->value,1);
  self->columns= nsp_object_get_size(self->value,2);
  return 0;
}

#line 716 "astv.c"


#line 47 "codegen/astv.override"
/* override a method */
static int _wrap_astv_get_dims(NspAstv *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  if ((Ret =(NspObject*) nsp_matrix_create_from_doubles(NVOID,1,2,(double) self->rows,(double) self->columns))==NULL) 
    return RET_BUG;
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

#line 732 "astv.c"


#line 105 "codegen/astv.override"
/* override a method */

static int _wrap_astv_get_args(NspAstv *self,Stack stack,int rhs,int opt,int lhs)
{
  if ( self->args == NULL) return RET_BUG;
  if ( !IsHash(self->args) ) return RET_BUG;
  MoveObj(stack,1,self->args);
  return 1;
}


#line 747 "astv.c"


#line 118 "codegen/astv.override"
/* override a method */

static int _wrap_astv_set_args(NspAstv *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspObject *args=NULL,*Obj;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_hash,&args) == FAIL) return RET_BUG;
  if ((Obj = nsp_object_copy_and_name("args",args)) == NULLOBJ) 
    return RET_BUG;
  if ( self->args != NULL )
    {
      nsp_object_destroy(&self->args);
    }
  self->args = Obj;
  return 0;
}


#line 769 "astv.c"


static NspMethods astv_methods[] = {
  {"get_value",(nsp_method *) _wrap_astv_get_value},
  {"have_value",(nsp_method *) _wrap_astv_have_value},
  {"set_value",(nsp_method *) _wrap_astv_set_value},
  {"get_dims",(nsp_method *) _wrap_astv_get_dims},
  {"get_args",(nsp_method *) _wrap_astv_get_args},
  {"set_args",(nsp_method *) _wrap_astv_set_args},
  { NULL, NULL}
};

static NspMethods *astv_get_methods(void) { return astv_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab astv_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Astv_func[]={
  { "astv_create", int_astv_create},
  { NULL, NULL}
};

/* call ith function in the Astv interface */

int Astv_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Astv_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Astv_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Astv_func[i].name;
  *f = Astv_func[i].fonc;
}
void nsp_initialize_Astv_types(void)
{
  new_type_astv(T_BASE);
}

#line 225 "codegen/astv.override"

NspAstv *nsp_astv(NspObject *Obj,int flag)
{
  NspObject *O1;
  NspAstv *H;
  /* want to be sure that type astv is initialized */
  nsp_type_astv = new_type_astv(T_BASE);
  if(( H = nsp_astv_create_void(NVOID,(NspTypeBase *) nsp_type_astv)) == NULLASTV) return NULL;
  if ((O1 = nsp_object_copy_and_name("ud",Obj)) == NULLOBJ) 
    {
      nsp_astv_destroy(H);
      return NULL;
    }
  if ( H->value != NULL )
    {
      nsp_object_destroy(&H->value);
    }
  H->value = O1;
  H->hv = flag;
  H->rows= nsp_object_get_size(O1,1);
  H->columns= nsp_object_get_size(O1,2);
  return H;
}



#line 849 "astv.c"
