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





#line 41 "codegen/ast.override"
#include <ctype.h>
#include <nsp/objects.h>
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/parse.h>

#line 35 "ast.c"

/* -----------NspAst ----------- */


#define  NspAst_Private 
#include <nsp/objects.h>
#include <nsp/ast.h>
#include <nsp/interf.h>

/* 
 * NspAst inherits from Object 
 */

int nsp_type_ast_id=0;
NspTypeAst *nsp_type_ast=NULL;

/*
 * Type object for NspAst 
 * all the instance of NspTypeAst share the same id. 
 * nsp_type_ast: is an instance of NspTypeAst 
 *    used for objects of NspAst type (i.e built with new_ast) 
 * other instances are used for derived classes 
 */
NspTypeAst *new_type_ast(type_mode mode)
{
  NspTypeAst *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_ast != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_ast;
    }
  if (( type =  malloc(sizeof(NspTypeAst))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = ast_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = ast_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_ast;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for ast */ 

  top->pr = (print_func *) nsp_ast_print;
  top->dealloc = (dealloc_func *) nsp_ast_destroy;
  top->copy  =  (copy_func *) nsp_ast_copy;
  top->size  = (size_func *) nsp_ast_size;
  top->s_type =  (s_type_func *) nsp_ast_type_as_string;
  top->sh_type = (sh_type_func *) nsp_ast_type_short_string;
  top->info = (info_func *) nsp_ast_info;
  /* top->is_true = (is_true_func  *) nsp_ast_is_true; */
  /* top->loop =(loop_func *) nsp_ast_loop;*/
#line 341 "codegen/ast.override"
top->path_extract = (path_func *) NULL; /* path extract as for matrix type */

#line 97 "ast.c"
  top->get_from_obj = (get_from_obj_func *) nsp_ast_object;
  top->eq  = (eq_func *) nsp_ast_eq;
  top->neq  = (eq_func *) nsp_ast_neq;
  top->save  = (save_func *) nsp_ast_xdr_save;
  top->load  = (load_func *) nsp_ast_xdr_load;
  top->create = (create_func*) int_ast_create;
  top->latex = (print_func *) nsp_ast_latex;
  top->full_copy = (copy_func *) nsp_ast_full_copy;

  /* specific methods for ast */

  type->init = (init_func *) init_ast;

  /* 
   * NspAst interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_ast_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAst called nsp_type_ast
       */
      type->id =  nsp_type_ast_id = nsp_new_type_id();
      nsp_type_ast = type;
      if ( nsp_register_type(nsp_type_ast) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_ast(mode);
    }
  else 
    {
      type->id = nsp_type_ast_id;
      return type;
    }
}

/*
 * initialize NspAst instances 
 * locally and by calling initializer on parent class 
 */

static int init_ast(NspAst *Obj,NspTypeAst *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->op = 0;
  Obj->arity = 0;
  Obj->str = NULL;
  Obj->xobj = NULLOBJ;
  Obj->args = NULLLIST;
  Obj->user_data = NULLOBJ;
 return OK;
}

/*
 * new instance of NspAst 
 */

NspAst *new_ast() 
{
  NspAst *loc;
  /* type must exists */
  nsp_type_ast = new_type_ast(T_BASE);
  if ( (loc = malloc(sizeof(NspAst)))== NULLAST) return loc;
  /* initialize object */
  if ( init_ast(loc,nsp_type_ast) == FAIL) return NULLAST;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspAst 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_ast_size(NspAst *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char ast_type_name[]="Ast";
static char ast_short_type_name[]="ast";

static char *nsp_ast_type_as_string(void)
{
  return(ast_type_name);
}

static char *nsp_ast_type_short_string(NspObject *v)
{
  return(ast_short_type_name);
}

#line 441 "codegen/ast.override"

/*
 * A == B 
 */

static int nsp_ast_eq(NspAst *A, NspObject *B)
{
  NspAst *loc = (NspAst *) B;
  if ( check_cast(B,nsp_type_ast_id) == FALSE) return FALSE ;
  if ( A->op != loc->op) return FALSE;
  if ( A->arity != loc->arity) return FALSE;
  if ( nsp_ast_obj_equal(A,loc)== FALSE) return FALSE;
  if ( NSP_OBJECT(A->args)->type->eq(A->args,loc->args) == FALSE ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_ast_neq(NspAst *A, NspObject *B)
{
  return ( nsp_ast_eq(A,B) == TRUE ) ? FALSE : TRUE;
}


#line 227 "ast.c"
int nsp_ast_xdr_save(XDR *xdrs, NspAst *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_ast)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspAst  *nsp_ast_xdr_load_partial(XDR *xdrs, NspAst *M)
{
 return M;
}

static NspAst  *nsp_ast_xdr_load(XDR *xdrs)
{
  NspAst *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLAST;
  if ((H  = nsp_ast_create_void(name,(NspTypeBase *) nsp_type_ast))== NULLAST) return H;
  if ( nsp_ast_create_partial(H) == FAIL) return NULLAST;
  if ((H  = nsp_ast_xdr_load_partial(xdrs,H))== NULLAST) return H;
  if ( nsp_ast_check_values(H) == FAIL) return NULLAST;
#line 62 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 258 "ast.c"
  return H;
}

/*
 * delete 
 */

void nsp_ast_destroy_partial(NspAst *H)
{
#line 65 "codegen/ast.override"
/* verbatim in destroy */

#line 271 "ast.c"
  nsp_string_destroy(&(H->str));
  if (H->xobj != NULL)
    nsp_object_destroy(&H->xobj);
  if ( H->args != NULL ) 
    nsp_list_destroy(H->args);
  if (H->user_data != NULL)
    nsp_object_destroy(&H->user_data);
}

void nsp_ast_destroy(NspAst *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_ast_destroy_partial(H);
  FREE(H);
}

#line 345 "codegen/ast.override"
/*
 * info overriden 
 */

int nsp_ast_info(NspAst *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( user_pref.list_as_tree == TRUE ) 
    {
      nsp_ast_info_tree(M,indent,name,rec_level);
    }
  else
    {
      Sciprintf1(indent,"%s\t={",pname);
      nsp_ast_print_node(M);
      Sciprintf("}\t\t%s\n",nsp_ast_type_short_string(NSP_OBJECT(M)));
      if ( M->args != NULL && nsp_list_length(M->args) > 0) 
	{
	  nsp_list_info(M->args,indent+2,"|",rec_level+1);
	}
    }
  return TRUE;
}

#line 313 "ast.c"
#line 371 "codegen/ast.override"
/*
 * print overriden 
 */

int nsp_ast_print(NspAst *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=ast_create(%d);",pname,M->op);
	}
      else 
	{
	  Sciprintf1(indent,"ast_create(%d);",M->op);
	}
    }
  else 
    {
      Sciprintf1(indent,"%s\t={",pname);
      nsp_ast_print_node(M);
      Sciprintf("}\t\t%s\n",nsp_ast_type_short_string(NSP_OBJECT(M)));
      if ( M->args != NULL && nsp_list_length(M->args) > 0) 
	{
	  nsp_list_print(M->args,indent+2,"args",rec_level+1);
	}
    }
  return TRUE;
}


static void nsp_ast_print_node(NspAst *ast) 
{
  const char *s;
  switch ( ast->op ) 
    {
    case STRING:
      Sciprintf("\"%s\"", ast->str);
      break;
    case COMMENT:
      Sciprintf("//%s", ast->str);
      break;
    case NUMBER:
    case INUMBER32:
    case INUMBER64:
    case UNUMBER32:
    case UNUMBER64:
      Sciprintf("%s",ast->str);
      break;
    case NAME :
      Sciprintf("%s", ast->str);
      break;
    case OPNAME :
      Sciprintf("'%s'", ast->str);
      break;
    case OBJECT : 
      Sciprintf("obj:0x%x",ast->xobj);
      break;
    default:
      s=nsp_astcode_to_name( ast->op);
      if ( s != (char *) 0 )
	Sciprintf("%s",s);
      else 
	Sciprintf("UNKNOWN->%d", ast->op);
    }
}

#line 383 "ast.c"
/*
 * latex print 
 */

int nsp_ast_latex(NspAst *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_ast_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"op=%d\n", M->op);
  Sciprintf1(indent+2,"arity=%d\n", M->arity);
  Sciprintf1(indent+2,"str=%s\n",M->str);
        if ( M->xobj->type->pr(M->xobj,indent+2,"xobj",rec_level+1)==FALSE) return FALSE;
  if ( M->args != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->args),indent+2,"args", rec_level+1)== FALSE ) return FALSE ;
    }
        if ( M->user_data->type->pr(M->user_data,indent+2,"user_data",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspAst objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspAst   *nsp_ast_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_ast_id)  == TRUE  ) return ((NspAst *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_ast));
  return NULL;
}

int IsAstObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_ast_id);
}

int IsAst(NspObject *O)
{
  return nsp_object_type(O,nsp_type_ast_id);
}

NspAst  *GetAstCopy(Stack stack, int i)
{
  if (  GetAst(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAst  *GetAst(Stack stack, int i)
{
  NspAst *M;
  if (( M = nsp_ast_object(NthObj(i))) == NULLAST)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspAst instance 
 *-----------------------------------------------------*/

static NspAst *nsp_ast_create_void(const char *name,NspTypeBase *type)
{
 NspAst *H  = (type == NULL) ? new_ast() : type->new();
 if ( H ==  NULLAST)
  {
   Sciprintf("No more memory\n");
   return NULLAST;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLAST;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_ast_create_partial(NspAst *H)
{
  return OK;
}

int nsp_ast_check_values(NspAst *H)
{
  if ( H->str == NULL) 
    {
  if (( H->str = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  if ( H->xobj == NULLOBJ) 
    {
     if (( H->xobj =(NspObject*) nsp_matrix_create("xobj",'r',0,0)) == NULL)
       return FAIL;
    }
  if ( H->args == NULLLIST) 
    {
     if (( H->args = nsp_list_create("args")) == NULLLIST)
       return FAIL;
    }
  if ( H->user_data == NULLOBJ) 
    {
     if (( H->user_data =(NspObject*) nsp_matrix_create("user_data",'r',0,0)) == NULL)
       return FAIL;
    }
  return OK;
}

NspAst *nsp_ast_create(const char *name,int op,int arity,char* str,NspObject* xobj,NspList* args,NspObject* user_data,NspTypeBase *type)
{
  NspAst *H  = nsp_ast_create_void(name,type);
  if ( H ==  NULLAST) return NULLAST;
  H->op=op;
  H->arity=arity;
  H->str = str;
  H->xobj= xobj;
  H->args= args;
  H->user_data= user_data;
  if ( nsp_ast_check_values(H) == FAIL) return NULLAST;
#line 62 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 509 "ast.c"
  return H;
}


NspAst *nsp_ast_create_default(const char *name)
{
 NspAst *H  = nsp_ast_create_void(name,NULL);
 if ( H ==  NULLAST) return NULLAST;
  if ( nsp_ast_check_values(H) == FAIL) return NULLAST;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspAst *nsp_ast_copy_partial(NspAst *H,NspAst *self)
{
  H->op=self->op;
  H->arity=self->arity;
  if ((H->str = nsp_string_copy(self->str)) == NULL) return NULL;
  if ( self->xobj == NULL )
    { H->xobj = NULL;}
  else
    {
      if ((H->xobj = (NspObject *) nsp_object_copy_and_name("xobj",NSP_OBJECT(self->xobj))) == NULLOBJ) return NULL;
    }
  if ( self->args == NULL )
    { H->args = NULL;}
  else
    {
      if ((H->args = (NspList *) nsp_object_copy_and_name("args", NSP_OBJECT(self->args))) == NULLLIST) return NULL;
    }
  if ( self->user_data == NULL )
    { H->user_data = NULL;}
  else
    {
      if ((H->user_data = (NspObject *) nsp_object_copy_and_name("user_data",NSP_OBJECT(self->user_data))) == NULLOBJ) return NULL;
    }
  return H;
}

NspAst *nsp_ast_copy(NspAst *self)
{
  NspAst *H  =nsp_ast_create_void(NVOID,(NspTypeBase *) nsp_type_ast);
  if ( H ==  NULLAST) return NULLAST;
  if ( nsp_ast_copy_partial(H,self)== NULL) return NULLAST;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspAst *nsp_ast_full_copy_partial(NspAst *H,NspAst *self)
{
  H->op=self->op;
  H->arity=self->arity;
  if ((H->str = nsp_string_copy(self->str)) == NULL) return NULL;
  if ( self->xobj == NULL )
    { H->xobj = NULL;}
  else
    {
      if ((H->xobj = (NspObject *) nsp_object_full_copy_and_name("xobj",NSP_OBJECT(self->xobj))) == NULLOBJ) return NULL;
    }
  if ( self->args == NULL )
    { H->args = NULL;}
  else
    {
      if ((H->args = (NspList *) nsp_object_full_copy_and_name("args", NSP_OBJECT(self->args))) == NULLLIST) return NULL;
    }
  if ( self->user_data == NULL )
    { H->user_data = NULL;}
  else
    {
      if ((H->user_data = (NspObject *) nsp_object_full_copy_and_name("user_data",NSP_OBJECT(self->user_data))) == NULLOBJ) return NULL;
    }
  return H;
}

NspAst *nsp_ast_full_copy(NspAst *self)
{
  NspAst *H  =nsp_ast_create_void(NVOID,(NspTypeBase *) nsp_type_ast);
  if ( H ==  NULLAST) return NULLAST;
  if ( nsp_ast_full_copy_partial(H,self)== NULL) return NULLAST;

#line 62 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 598 "ast.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspAst
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_ast_create(Stack stack, int rhs, int opt, int lhs)
{
  NspAst *H;
  CheckStdRhs(0,0);
  /* want to be sure that type ast is initialized */
  nsp_type_ast = new_type_ast(T_BASE);
  if(( H = nsp_ast_create_void(NVOID,(NspTypeBase *) nsp_type_ast)) == NULLAST) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_ast_check_values(H) == FAIL) return RET_BUG;
  #line 62 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use RET_BUG for returned value */
#line 619 "ast.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 95 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_str(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  char *str=NULL;
  int op; 
  CheckRhs(0,0); 
  CheckLhs(1,1);
  op = ((int) self->op);
  switch ( op ) 
    {
    case NAME :
    case OPNAME :
    case STRING :
    case COMMENT :
    case NUMBER :
    case INUMBER32:
    case INUMBER64:
    case UNUMBER32:
    case UNUMBER64:
      str = ((NspAst *) self)->str;
      if ( str == NULL) str = "@";
      if ((Ret = nsp_new_string_obj(NVOID,str,-1))== NULLOBJ) return RET_BUG;
      break;
    default:
      if ((Ret = (NspObject *) nsp_smatrix_create(NVOID,0,0,"v",(int)0)) == NULLOBJ) 
	return RET_BUG;
    }
  MoveObj(stack,1,Ret);
  return 1;
}

#line 660 "ast.c"


#line 129 "codegen/ast.override"
static int _wrap_ast_set_str(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *str;
  CheckRhs(1,1); 
  if ( GetArgs(stack,rhs,opt,T,&str) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ( nsp_ast_set_str(self,str) == FAIL) 
    return RET_BUG;
  return 0;
}

#line 676 "ast.c"


#line 143 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_op(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  int ret ;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  ret = ((int) self->op);
  if ((Ret=nsp_new_double_obj((double) ret))== NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

#line 693 "ast.c"


#line 158 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_codename(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  const char *str;
  NspObject *Ret;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  str=nsp_astcode_to_codename(self->op);
  if ( str != (char *) 0 )
    {
      if ((Ret = nsp_new_string_obj(NVOID,str,-1))== NULLOBJ) return RET_BUG;
    }
  else 
    {
      if ((Ret = (NspObject *) nsp_smatrix_create(NVOID,0,0,"v",(int)0)) == NULLOBJ) 
	return RET_BUG;
    }
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

#line 718 "ast.c"


#line 181 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_opname(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  const char *str;
  NspObject *Ret;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  switch ( ((int) self->op)) 
    {
    default:
      str=nsp_astcode_to_name(self->op);
      if ( str != (char *) 0 )
	{
	  if ((Ret = nsp_new_string_obj(NVOID,str,-1))== NULLOBJ) return RET_BUG;
	}
      else 
	{
	  if ((Ret = (NspObject *) nsp_smatrix_create(NVOID,0,0,"v",(int)0)) == NULLOBJ) 
	    return RET_BUG;
	}
    }
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

#line 747 "ast.c"


#line 223 "codegen/ast.override"
/* override a method */
static int _wrap_ast_is(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  int rep = FALSE;
  const char *str,*str1;
  CheckRhs(1,1);
  CheckLhs(1,1); 
  if ((str1 = GetString(stack,1)) == (char*)0) return RET_BUG;
  switch ( ((int) self->op)) 
    {
    default:
      str=nsp_astcode_to_codename(self->op);
      if ( str != (char *) 0 && strcmp(str,str1)==0 ) rep = TRUE;
    }
  if ( nsp_move_boolean(stack,1, rep) == FAIL)  return RET_BUG;
  return Max(lhs,1);
}

#line 769 "ast.c"


#line 243 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_obj(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  if ( ((int) self->op) != OBJECT )
    {
      Scierror("Error: an object can be returned only for astnode of id OBJECT\n");
      return RET_BUG;
    }
  MoveObj(stack,1, self->xobj);
  return Max(lhs,1);
}

#line 787 "ast.c"


#line 277 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_user_data(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  if ( self->user_data == NULL )
    {
      Scierror("Error: astnode has no user_data\n");
      return RET_BUG;
    }
  MoveObj(stack,1, self->user_data);
  return Max(lhs,1);
}

#line 805 "ast.c"


#line 259 "codegen/ast.override"
/* override a method */
static int _wrap_ast_set_user_data(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  CheckRhs(1,1);
  CheckLhs(0,1); 
  if ((Obj = nsp_object_copy_and_name("ud",NthObj(1))) == NULLOBJ) 
    return RET_BUG;
  if ( self->user_data != NULL )
    {
      nsp_object_destroy(&self->user_data);
    }
  self->user_data = Obj;
  return 0;
}

#line 825 "ast.c"


#line 293 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_args(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  MoveObj(stack,1, NSP_OBJECT(self->args));
  return Max(lhs,1);
}

#line 838 "ast.c"


#line 69 "codegen/ast.override"
/* a method can be overriden by giving its name or 
 * class.name 
 */

static int _wrap_set_args(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {list,t_end};
  NspList *args;
  if ( GetArgs(stack,rhs,opt,T,&args) == FAIL) return RET_BUG;
  if ( nsp_ast_check_args(args) == FAIL)  return RET_BUG;
  if ( (args= (NspList *) nsp_object_copy((NspObject *)args)) == NULL )
    return RET_BUG;


  if ( nsp_object_set_name((NspObject *) args,"args") == FAIL ) goto err;
  nsp_list_destroy(self->args);
  self->args = args;
  self->arity =  nsp_list_length(self->args);
  return 0;
 err: 
  nsp_list_destroy(args);
  return RET_BUG;
}

#line 866 "ast.c"


#line 208 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_arity(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  int ret ;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  ret = ((int) self->arity);
  if ((Ret=nsp_new_double_obj((double) ret))== NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

#line 883 "ast.c"


#line 313 "codegen/ast.override"
/* override a method */

static int _wrap_ast_sprint(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_sprint(self,stack,rhs,opt,lhs);
}
#line 893 "ast.c"


#line 321 "codegen/ast.override"
/* override a method */

static int _wrap_ast_fprint(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_fprint(self,stack,rhs,opt,lhs);
}

#line 904 "ast.c"


#line 304 "codegen/ast.override"
/* override a method */

static int _wrap_ast_print(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_print(self,stack,rhs,opt,lhs);
}

#line 915 "ast.c"


#line 330 "codegen/ast.override"
static int _wrap_ast_print_length(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = nsp_ast_printlength(self,0);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 928 "ast.c"


static NspMethods ast_methods[] = {
  {"get_str",(nsp_method *) _wrap_ast_get_str},
  {"set_str",(nsp_method *) _wrap_ast_set_str},
  {"get_op",(nsp_method *) _wrap_ast_get_op},
  {"get_codename",(nsp_method *) _wrap_ast_get_codename},
  {"get_opname",(nsp_method *) _wrap_ast_get_opname},
  {"is",(nsp_method *) _wrap_ast_is},
  {"get_obj",(nsp_method *) _wrap_ast_get_obj},
  {"get_user_data",(nsp_method *) _wrap_ast_get_user_data},
  {"set_user_data",(nsp_method *) _wrap_ast_set_user_data},
  {"get_args",(nsp_method *) _wrap_ast_get_args},
  {"set_args",(nsp_method *) _wrap_set_args},
  {"get_arity",(nsp_method *) _wrap_ast_get_arity},
  {"sprint",(nsp_method *) _wrap_ast_sprint},
  {"fprint",(nsp_method *) _wrap_ast_fprint},
  {"print",(nsp_method *) _wrap_ast_print},
  {"print_length",(nsp_method *) _wrap_ast_print_length},
  { NULL, NULL}
};

static NspMethods *ast_get_methods(void) { return ast_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab ast_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Ast_func[]={
  { "ast_create", int_ast_create},
  { NULL, NULL}
};

/* call ith function in the Ast interface */

int Ast_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(Ast_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Ast_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = Ast_func[i].name;
  *f = Ast_func[i].fonc;
}
void nsp_initialize_Ast_types(void)
{
  new_type_ast(T_BASE);
}

#line 469 "codegen/ast.override"

/* pretty print of the ast i.e ast -> code */

static int _nsp_ast_pprint(NspAst *ast, int indent, int pos, int posret);
static int _nsp_ast_pprint_arg(NspAst *ast,int elt, int i, int pos, int posret);
static int nsp_ast_pprint_opname(int type, int indent, int pos);
static int _nsp_ast_pprint_args(NspAst *ast, int start, int last, int indent, int pos,
				int posret, char *sep, int breakable,const char *breakstr);
static int _nsp_ast_equalop_mlhs_length(NspAst *ast);

/* very similar to the generic function for printing objects and 
 * redirection of output to string, file or stdout
 */

typedef enum { string_out, stdout_out, file_out } print_mode; 
static int int_ast_print_gen(NspAst *self,Stack stack, int rhs, int opt, int lhs, print_mode mode);

static int meth_ast_print(NspAst *self,Stack stack, int rhs, int opt, int lhs)
{
  return int_ast_print_gen(self,stack,rhs,opt,lhs,stdout_out);
}

static int meth_ast_sprint(NspAst *self,Stack stack, int rhs, int opt, int lhs)
{
  return int_ast_print_gen(self,stack,rhs,opt,lhs,string_out);
}

static int meth_ast_fprint(NspAst *self,Stack stack, int rhs, int opt, int lhs)
{
  return int_ast_print_gen(self,stack,rhs,opt,lhs,file_out);
}

static int int_ast_print_gen(NspAst *self,Stack stack, int rhs, int opt, int lhs, print_mode mode)
{
  NspFile *F=NULL;
  FILE *f=NULL;
  IOVFun def=NULL ;
  MoreFun mf=NULL; 
  
  NspObject *res;
  int dp=user_pref.pr_depth;
  int cr=user_pref.color;
  int as_read=FALSE,depth=INT_MAX,indent=0,color=TRUE,html=FALSE,gtk=FALSE;
  nsp_option print_opts[] ={{ "as_read",s_bool,NULLOBJ,-1},
			    { "color",s_bool,NULLOBJ,-1},
			    { "depth", s_int,NULLOBJ,-1},
			    { "indent",s_int,NULLOBJ,-1},
			    { "html",s_bool,NULLOBJ,-1},
			    { "gtk",s_bool,NULLOBJ,-1},
			    { NULL,t_end,NULLOBJ,-1}};

  if ( mode == file_out ) 
    {
      CheckStdRhs(1,1);
      if ((F= GetSciFile(stack,1))== NULL) return RET_BUG; 
    }
  else 
    {
      CheckStdRhs(0,0);
    }
  CheckLhs(0,1);

  if ( mode == string_out) color=FALSE;
  
  if ( get_optional_args(stack, rhs, opt, print_opts,
			 &as_read,&color,&depth,&indent,&html,&gtk) == FAIL) 
    return RET_BUG;
  
  /* initialize according to mode */
  switch ( mode ) 
    {
    case string_out: 
      def = SetScilabIO(Sciprint2string);
      mf =nsp_set_nsp_more(scimore_void);
      break;
    case stdout_out:
      break;
    case file_out : 
      /* changes io in order to write to file F */
      if ( !IS_OPENED(F->obj->flag))
	{
	  Scierror("Warning:\tfile %s is already closed\n",F->obj->fname);
	  return RET_BUG;
	}
      f=Sciprint_file(F->obj->file); 
      def = SetScilabIO(Sciprint2file);
      mf =nsp_set_nsp_more(scimore_void);
      break;
    }
  /* print object */
  user_pref.pr_depth= depth;
  user_pref.color=color;

  set_use_html_color_class(html);
  set_use_gtk_color_class(gtk);

  if ( gtk == TRUE )
    {
      user_pref.color= color = TRUE;
    }
  
  if ( html == TRUE )
    {
      /*
	.nsp_code  { background: #EEEEEE; color: Black;}
	.nsp_code .code { color : Black; }
	.nsp_code .keyword { color : blue; }
	.nsp_code .comment { color : red;}
	.nsp_code .string { color : brown;}
	.nsp_code .number { color : YellowGreen; }
	.nsp_code .function { color : SkyBlue ;  font-weight: bold;}
      */
      
      user_pref.color= color = TRUE;
      Sciprintf2(indent," ","<!-- style directives ");
      Sciprintf2(indent," ","<head>\n");
      Sciprintf2(indent+2," ","<style>\n");
      Sciprintf2(indent+4," ",".nsp_code  { background:  WhiteSmoke; color: Black; font-size: 120%;}\n");
      Sciprintf2(indent+4," ",".nsp_code .code { color : Black; }\n");
      Sciprintf2(indent+4," ",".nsp_code .keyword { color : MediumPurple; }\n");
      Sciprintf2(indent+4," ",".nsp_code .comment { color :  OrangeRed; }\n");
      Sciprintf2(indent+4," ",".nsp_code .string { color : LightSalmon; }\n");
      Sciprintf2(indent+4," ",".nsp_code .number { color : YellowGreen; }\n");
      Sciprintf2(indent+4," ",".nsp_code .function { color : SkyBlue ;  font-weight: bold;}\n");
      Sciprintf2(indent+2," ","</style>\n");
      Sciprintf2(indent," ","</head>\n");
      Sciprintf2(indent," ","-->\n");
      Sciprintf2(indent," ","%s\n","<div class=\"nsp_code\">");
      Sciprintf2(indent+2," ","%s\n","<pre class=\"code\">");
      nsp_ast_pprint(self, indent + 2);
      Sciprintf2(indent+2," ","\n");
      Sciprintf2(indent+2," ","%s\n","</pre>");
      Sciprintf2(indent," ","%s\n","</div>");
    }
  else
    {
      nsp_ast_pprint(self, indent);
    }


  
  user_pref.color=cr;
  user_pref.pr_depth= dp;
  /* restore to default values */
  switch ( mode ) 
    {
    case string_out: 
      res = Sciprint2string_reset(); 
      SetScilabIO(def);
      nsp_set_nsp_more(mf);
      if ( res == NULL) return RET_BUG; 
      MoveObj(stack,1, res);
      return 1;
    case stdout_out: 
      return 0;
    case file_out:
      SetScilabIO(def);
      nsp_set_nsp_more(mf);
      Sciprint_file(f); 
      return 0;
    }
  return 0;
}


/**
 * nsp_ast_pprint:
 * @L: 
 * @indent: the curent indentation to use.
 * 
 * pretty printing of a #PList. 
 * 
 **/

static void nsp_ast_pprint(NspAst * L, int indent)
{
  _nsp_ast_pprint(L,indent,0,indent);
}

/* set of tools for colorizing */

static int use_html_color_class = FALSE;

static void set_use_html_color_class(int flag)
{
  use_html_color_class=flag;
}

static int use_gtk_color_class = FALSE;

static void set_use_gtk_color_class(int flag)
{
  use_gtk_color_class=flag;
}



/* set of colors that can be used in a terminal 
 * if 3 is replaced by 9 we obtain light colors.
 */

typedef enum { p_black =30, 
               p_red =31,
               p_green=32,
               p_yellow=33,
               p_blue=34,
               p_purple=35,
               p_cyan=36,
               p_white=37} nsp_term_colors;

typedef enum { type_comment,
	       type_string,
	       type_keyword,
	       type_number,
	       type_fname,
             } nsp_colorized_types;

static int color_from_key( nsp_colorized_types key)
{
  switch (key)
    {
    case type_comment: return p_red;break;
    case type_string:  return p_cyan;break;
    case type_keyword: return p_purple;break;
    case type_number:  return p_green;break;
    case type_fname:   return p_blue;break;
    }
  return p_black;
}

static char* colorname_from_key( nsp_colorized_types key)
{
  switch (key)
    {
    case type_comment: return "red";break;
    case type_string:  return "cyan";break;
    case type_keyword: return "purple";break;
    case type_number:  return "green";break;
    case type_fname:   return "blue";break;
    }
  return "black";
}

static char *colortag_from_key( nsp_colorized_types key)
{
  switch (key)
    {
    case type_comment: return "comment";break;
    case type_string:  return "string";break;
    case type_keyword: return "keyword";break;
    case type_number:  return "number";break;
    case type_fname:   return "function";break;
    }
  return "undefined";
}

static int nsp_ast_pprint_pre_tag_color( nsp_colorized_types key)
{
  if (user_pref.color == TRUE )
    {
      if (use_html_color_class == TRUE)
	Sciprintf("<span class=\"%s\">",colortag_from_key(key));
      else if (use_gtk_color_class == TRUE)
	Sciprintf("<span color=\"%s\">",colorname_from_key(key));
      else
	Sciprintf("\033[%dm",color_from_key(key));
    }
  return 0;
}

static int nsp_ast_pprint_post_tag_color(void)
{
  if (user_pref.color == TRUE )
    {
      if (use_html_color_class == TRUE ||
	  use_gtk_color_class == TRUE)
	Sciprintf("</span>");
      else
	Sciprintf("\033[0m");
    }
  return 0;
}

static int nsp_ast_pprint_comment(int indent,const char *str)
{
  int pos=0;
  pos += Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
  pos += nsp_ast_pprint_pre_tag_color(type_comment);
  if  (use_html_color_class == TRUE ||
       use_gtk_color_class == TRUE )
    {
      nsp_print_comment_for_html(str);
      pos += strlen(str)+2;
    }
  else
    {
      pos += Sciprintf("//%s",str);
    }
  pos += nsp_ast_pprint_post_tag_color();
  return pos;
}

static void nsp_print_comment_for_html(const char *str)
{
  Sciprintf("//");
  while ( *str != '\0') 
    {
      switch (*str) 
	{
	case '<' : Sciprintf("%s","&lt;");break;
	case '>' : Sciprintf("%s","&gt;");break;
	case '&' : Sciprintf("%s","&amp;");break;
	default :  Sciprintf("%c",*str);
	}
      str++;
    }
}

static int nsp_ast_pprint_string(int indent,const char *str)
{
  int pos=0;
  pos += Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
  pos += nsp_ast_pprint_pre_tag_color(type_string);
  pos += 2 + strlen(str);
  if  (use_html_color_class == TRUE ||
       use_gtk_color_class == TRUE )
    {
      nsp_print_string_as_read_for_html(str);
    }
  else
    {
      nsp_print_string_as_read(str);
    }
  pos += nsp_ast_pprint_post_tag_color();
  return pos;
}

static void nsp_print_string_as_read_for_html(const char *str)
{
  Sciprintf("\"");
  while ( *str != '\0') 
    {
      switch (*str) 
	{
	case '\'' :  Sciprintf("%s","''");break;
	case '\"' :  Sciprintf("%s","\"\"");break;
	case '\\' :  
	  switch (*(str+1)) 
	    {
	    case 'a':
	    case 'b' : 
	    case 'f' :  
	    case 'n' :  
	    case 'r' :  
	    case 't' :  
	    case 'v' :  
	    case '\a' :  
	    case '\b' :  
	    case '\f' :  
	    case '\n' :  
	    case '\r' :  
	    case '\t' :  
	    case '\v' : Sciprintf("%s","\\\\");break;
	    default: Sciprintf("%s","\\");break;
	    }
	  break;
	case '\a' :  Sciprintf("%s","\\a"); break;
	case '\b' :  Sciprintf("%s","\\b"); break;
	case '\f' :  Sciprintf("%s","\\f"); break;
	case '\n' :  Sciprintf("%s","\\n");break;
	case '\r' :  Sciprintf("%s","\\r"); break;
	case '\t' :  Sciprintf("%s","\\t"); break;
	case '\v' :  Sciprintf("%s","\\v"); break;
	default: 
	  if (isprint(*str)) 
	    {
	      switch (*str ) {
	      case '<' : Sciprintf("%s","&lt;");break;
	      case '>' : Sciprintf("%s","&gt;");break;
	      default:
		Sciprintf("%c",*str);
	      }
	    }
	  else 
	    {
	      unsigned char c = *str;
	      Sciprintf("\\%0.3o",c);
	    }
	}
      str++;
    }
  Sciprintf("\"");
}




static int nsp_ast_pprint_number(int indent,const char *str)
{
  int pos=0;
  pos += Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
  pos += nsp_ast_pprint_pre_tag_color(type_number);
  pos += Sciprintf("%s",str);
  pos += nsp_ast_pprint_post_tag_color();
  return pos;
}

static int nsp_ast_pprint_keyword(int indent,const char *str)
{
  int pos=0;
  pos += Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
  pos += nsp_ast_pprint_pre_tag_color(type_keyword);
  pos += Sciprintf("%s",str);
  pos += nsp_ast_pprint_post_tag_color();
  return pos;
}

/* print operator name after @indent blank characters 
 * and return the new position.
 * pos is giving the current position.
 */

static int nsp_ast_pprint_opname(int type, int indent, int pos)
{
  const char *s = nsp_astcode_to_name(type);
  const char *s1= s;
  if (use_html_color_class == TRUE ||
      use_gtk_color_class == TRUE )
    {
      if ( strcmp(s,">") == 0)
	s1 = "&gt;";
      else if ( strcmp(s,"<") == 0)
	s1 = "&lt;";
      else if ( strcmp(s,"<=") == 0)
	s1 = "&lt;=";
      else if ( strcmp(s,">=") == 0)
	s1 = "&gt;=";
      else if ( strcmp(s,"<>") == 0)
	s1 = "&lt;&gt";
      else s1 = s;
    }
  if (  use_gtk_color_class == TRUE )
    {
      if ( strcmp(s,"&") == 0) s1 ="&amp;";
      else if ( strcmp(s,"&&") == 0) s1 ="&amp;&amp;";
    }
  Sciprintf2(indent,(use_html_color_class == TRUE) ? "&nbsp;": " ","%s",s1);
  return pos + indent + strlen(s);
}

#define NSP_PRINTF1_COLOR(posret,col,str) nsp_ast_pprint_str(posret,col,str)

#define CMAX 90

/* posret: indentation to use if line-break. 
 *
 */

static int _nsp_ast_pprint(NspAst *ast, int indent, int pos, int posret)
{
  const char *s;
  int j,newpos=0;

  /* be sure that we are starting to print at least at column postret */
  if ( pos < posret ) 
    {
#ifdef WITH_SYMB_TABLE_DEBUG
      pos=Sciprintf2(posret-pos,(use_html_color_class == TRUE) ? "&nbsp;": " ","");
#else
      if ( ast->op != OBJECT ) pos=Sciprintf2(posret-pos, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
#endif
    }
  /* select print operation */
  if ( ast->op > 0 ) 
    {
      int len;
      /* operators **/
      switch ( ast->arity ) 
	{
	case 0: /* 0-ary operators */
	  return nsp_ast_pprint_opname(ast->op,indent,pos);
	  break;
	case 1:
	  switch ( ast->op ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_pprint_opname(ast->op,0,newpos);
	      return newpos;
	      break;
	    case  COMMA_RET_OP : 
	    case  SEMICOLON_RET_OP  :
	      newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_pprint_opname(ast->op,0,newpos);
	      Sciprintf("%s","\n");newpos=0;
	      return newpos;
	      break;
	    case QUOTE_OP : 
	    case DOTPRIM:
	      newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_pprint_opname(ast->op,0,newpos);
	      return  newpos;
	      break;
	    case RETURN_OP : 
	      _nsp_ast_pprint_arg(ast,1,indent,pos,posret);
	      Sciprintf("%s","\n");newpos=0;
	      return newpos;
	      break;
	    case TILDE_OP : 
	    default:
	      newpos =nsp_ast_pprint_opname(ast->op,indent,pos);
	      newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret);
	      return newpos;
	    }
	  break;
	case 2:
	  newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret);
	  newpos =nsp_ast_pprint_opname(ast->op,1,newpos);
	  newpos += Sciprintf(" ");
	  len =  _nsp_ast_printlength_arg(ast,2,0,newpos,posret);
	  if ( newpos > CMAX || len > CMAX)
	    {
	      /* we have the right to break line here 
	       * we indent on the next line with posret 
	       */
	      Sciprintf("%s"," ...\n");newpos= Sciprintf2(posret, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
	    }
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret);
	  return newpos;
	  break;
	default :
	  /* XXX make breaks as in the 2-ary ops */
	  newpos = pos;
	  for ( j = 0 ; j <  ast->arity ; j++ )
	    {
	      newpos =_nsp_ast_pprint_arg(ast,j+1,(j == 0) ? indent : 1,
					   newpos,posret);
	      if ( j != ast->arity -1 ) 
		{
		  newpos =nsp_ast_pprint_opname(ast->op,1,newpos);
		  newpos += Sciprintf(" ");
		}
	    }
	  return newpos;
	  break;
	}
    }
  else 
    {
      switch ( ast->op ) 
	{
	case OPT:
	  /* val = value in a calling list */
	  newpos = _nsp_ast_pprint_arg(ast,1,indent,pos,posret);
	  newpos += Sciprintf(" = ");
	  newpos = _nsp_ast_pprint_arg(ast,2,0,newpos,posret);
	  return newpos;
	  break;
	case EQUAL_OP:
	  /* affectations */
	  newpos = _nsp_ast_pprint_arg(ast,1,indent,pos,posret);
	  if (  _nsp_ast_equalop_mlhs_length(ast) > 0 ) 
	    newpos += Sciprintf("=");
	  /* fix new return position after = */
	  newpos = _nsp_ast_pprint_arg(ast,2,0,newpos, newpos);
	  return newpos;
	  break;
	case MLHS  :
	  /* left hand side of an equality 
	   * we do not display the left and right bracket 
	   *  if arity is one 
	   */
	  newpos = pos +  Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ",
				     ( ast->arity > 1) ? "[" : "");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  if ( ast->arity > 1) newpos += Sciprintf("]");
	  return newpos;
	  break;
	case ARGS :
	  /* a sequence of expressions inside () for x()*/
	  newpos = pos +  Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","(");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CELLARGS :
	  /* a sequence of expressions inside {} for x{} */
	  newpos = pos +  Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","{");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf("}");
	  return newpos;
	  break;
	case METARGS :
	  /* a sequence of expressions inside [] for x[] */
	  newpos = pos +  Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","[");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case DOTARGS :
	  {
	    NspAst *ast1;
	    if ((ast1 =(NspAst*) nsp_list_get_element(ast->args,1)) == NULL) return newpos;
	    if (ast1->op != STRING ) return newpos;
	    newpos = pos + Sciprintf2(0,(use_html_color_class == TRUE) ? "&nbsp;": " ",".%s",(char *) ast1->str);
	    return newpos;
	  }
	case CALLEVAL:
	case LISTEVAL :
	  newpos = pos +  Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,"",FALSE,"");
	  return newpos;
	  break;
	case FEVAL :
	  newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret);
	  newpos += Sciprintf("(");
	  newpos = _nsp_ast_pprint_args(ast,2,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case PLIST :
	  /* such node should not appear here */
	  return newpos;
	  break;
	case COMMENT:
	  return pos+ nsp_ast_pprint_comment(indent,(const char *) ast->str);
	  break;
	case NAME :
#ifdef WITH_SYMB_TABLE_DEBUG
	  return pos+Sciprintf2(indent,(use_html_color_class == TRUE) ? "&nbsp;": " ","%s<%d>",(char *) ast->str,ast->arity);
#else 
	  return pos+Sciprintf2(indent,(use_html_color_class == TRUE) ? "&nbsp;": " ","%s",(char *) ast->str);
#endif 
	  break;
	case OPNAME :
	  return pos+Sciprintf2(indent,(use_html_color_class == TRUE) ? "&nbsp;": " ","'%s'",(char *) ast->str);
	  break;
	case NUMBER:
	  return pos + nsp_ast_pprint_number(indent,(const char *) ast->str);
	  break;
	case STRING:
	  return pos + nsp_ast_pprint_string(indent,(const char *) ast->str);
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  return pos+  nsp_ast_pprint_number(indent,(const char *) ast->str);
	  break;
	case OBJECT: 
#ifdef WITH_SYMB_TABLE_DEBUG      
	  n = pos+Sciprintf2(indent,(use_html_color_class == TRUE) ? "&nbsp;": " ","{object:Ox%x}\n",(unsigned int) ast->xobj);
	  nsp_object_print(ast->xobj,0,0,0);
	  return n;
#else 
	  return pos;
#endif
	  break;
	case EMPTYMAT:  return pos+Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","[]");break;
	case EMPTYCELL: return pos+Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","{}");break;
	case P_MATRIX :
	  newpos = pos + Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","[");
	  newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret+1);
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case P_CELL :
	  newpos = pos + Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","{");
	  newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret+1);
	  newpos += Sciprintf("}");
	  return newpos;
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  newpos = posret = pos;
	  for ( j = 0 ; j < ast->arity ; j++)
	    {
	      int is_comment,len;
	      /* get the length of child if the child is not cut 
	       * to decide if it's better to break here
	       */
	      len =  _nsp_ast_printlength_arg(ast,j+1,0,newpos,posret);
	      if ( ( j > 0) && ( newpos > CMAX || len > CMAX))
		{
		  if ( ast->op == ROWCONCAT || ast->op == CELLROWCONCAT)
		    Sciprintf("\n");
		  else
		    Sciprintf(" ...\n");
		  newpos= Sciprintf2(posret, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
		}
	      newpos =_nsp_ast_pprint_arg(ast,j+1,0,newpos,posret);
	      /* some child can be comment-ended, in that case a newline is requested */
	      is_comment =_nsp_ast_pprint_op_comment_ended(ast,j+1);
	      if ( j < ast->arity-1)
		{
		  if ( is_comment ) 
		    {
		      Sciprintf("\n");Sciprintf2(posret,(use_html_color_class == TRUE) ? "&nbsp;": " ","");
		      newpos = posret;
		    }
		  else
		    {
		      newpos =nsp_ast_pprint_opname(ast->op,0,newpos);
		    }
		}
	    }
	  return newpos;
	  break;
	case WHILE:
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),"while");
	  newpos =_nsp_ast_pprint_arg(ast,1,1,newpos,posret);
	  newpos += nsp_ast_pprint_keyword(1,"do");
	  newpos =_nsp_ast_pprint_check_newline(ast,2,newpos);
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret+2);
	  newpos = nsp_ast_pprint_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case FUNCTION:
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),"function");
	  newpos = _nsp_ast_pprint_arg(ast,1,1,newpos,newpos);
	  newpos =_nsp_ast_pprint_check_newline(ast,2,newpos);
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret+2);
	  if ( ast->arity == 3 ) 
	    {
	      newpos =_nsp_ast_pprint_arg(ast,3,0,newpos,posret+2);
	    }
	  newpos= nsp_ast_pprint_keyword(Max(posret-newpos,0),"endfunction");
	  return newpos;
	  break;
	case FOR:
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),"for");
	  newpos = _nsp_ast_pprint_arg(ast,1,1,newpos,posret);
	  newpos += Sciprintf("=") ;
	  newpos = _nsp_ast_pprint_arg(ast,2,0,newpos,newpos);
	  newpos += nsp_ast_pprint_keyword(1,"do");
	  newpos = _nsp_ast_pprint_check_newline(ast,3,newpos);
	  newpos = _nsp_ast_pprint_arg(ast,3,0,newpos,posret+2);
	  newpos += nsp_ast_pprint_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case IF:
	  /* a sequence of if elseif etc.... */
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),"if");
	  for ( j = 0 ; j < ast->arity  ; j += 2 )
	    {
	      if ( j == ast->arity-1 ) 
		{
		  /* we have reached the last else **/
		  newpos +=  nsp_ast_pprint_keyword(Max(posret-newpos,0),"else");
		  newpos =_nsp_ast_pprint_check_newline(ast,j+1,newpos);
		  newpos =_nsp_ast_pprint_arg(ast,j+1,0,newpos,posret+2);
		}
	      else 
		{ 
		  if ( j != 0) 
		    {
		      newpos +=nsp_ast_pprint_keyword(Max(posret-newpos,0),"elseif");
		    }
		  newpos =_nsp_ast_pprint_arg(ast,j+1,1,newpos,posret+2);
		  newpos += nsp_ast_pprint_keyword(1,"then");
		  newpos =_nsp_ast_pprint_check_newline(ast,j+2,newpos);
		  newpos =_nsp_ast_pprint_arg(ast,j+2,0,newpos,posret+2);
		}
	    }
	  newpos +=  nsp_ast_pprint_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case TRYCATCH :
	  /* try catch sequence */
	  newpos = pos+ nsp_ast_pprint_keyword(Max(posret-pos,0),"try");
	  newpos =_nsp_ast_pprint_check_newline(ast,1,newpos);
	  newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret+2);
	  newpos += nsp_ast_pprint_keyword(Max(posret-newpos,0),"catch");
	  newpos =_nsp_ast_pprint_check_newline(ast,2,newpos);
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret+2);
	  if ( ast->arity == 3 ) 
	    {
	      newpos += nsp_ast_pprint_keyword(Max(posret-newpos,0),"finally");
	      newpos =_nsp_ast_pprint_check_newline(ast,3,newpos);
	      newpos =_nsp_ast_pprint_arg(ast,3,0,newpos,posret+2);
	    }
	  newpos = nsp_ast_pprint_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case SELECT :
	  /* first argument is the test.
	   * next ones are cases 
	   */
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),"select");
	  for ( j = 0 ; j < ast->arity ; j++)
	    {
	      if ( j==0) 
		{
		  _nsp_ast_pprint_arg(ast,j+1,1,newpos,posret);
		  Sciprintf("\n");newpos= 0;
		}
	      else
		{
		  newpos=_nsp_ast_pprint_arg(ast,j+1,0,newpos,posret+2);
		}
	    }
	  newpos = nsp_ast_pprint_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case STATEMENTS :
	  newpos=pos;
	  //newpos = pos +  Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
	  newpos= _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,posret,"",TRUE,"\n");
	  return newpos;
	  break;
	case STATEMENTS1 :
	  newpos=pos;
	  //newpos = pos +  Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
	  newpos= _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,posret,"",TRUE,"\n");
	  return newpos;
	  break;
	case PARENTH :
	  newpos = pos + Sciprintf2(indent, (use_html_color_class == TRUE) ? "&nbsp;": " ","(") ;
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CASE : 
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),"case");
	  newpos =_nsp_ast_pprint_arg(ast,1,1,newpos,posret+2);
	  newpos += nsp_ast_pprint_keyword(1,"then");
	  newpos =_nsp_ast_pprint_check_newline(ast,2,newpos);
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret+2);
	  return newpos;
	  break;
	case LASTCASE :
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),"else");
	  newpos =_nsp_ast_pprint_check_newline(ast,1,newpos);
	  newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret+2);
	  return newpos;
	  break;
	case GLOBAL:
	  /* n-ary global */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"global");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CLEAR:
	  /* n-ary clear */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"clear");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CLEARGLOBAL:
	  /* n-ary global */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"clearglobal");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case PAUSE:
	  /* can be 0 or 1-ary pause */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"pause");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case HELP:
	  /* 0 or  1-ary help */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"help");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case WHO:
	  /* 0 or 1-ary who */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"who");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case EXEC:
	  /* 1-ary exec */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"exec");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case APROPOS:
	  /* 1-ary apropos */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"apropos");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CD_COMMAND:
	  /* 1-ary cd */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"cd");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case LS_COMMAND:
	  /* 1-ary ls */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"ls");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case PWD_COMMAND:
	  /* 1-ary pwd */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),"pwd");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;

	case BREAK: return pos+nsp_ast_pprint_keyword(indent,"break");break;
	case PRETURN:  return pos+nsp_ast_pprint_keyword(indent,"return"); break;
	case QUIT :   return pos+nsp_ast_pprint_keyword(indent,"quit");   break;
	case NSP_EXIT :  return pos+nsp_ast_pprint_keyword(indent,"exit");  break;
	case ABORT :  return pos+nsp_ast_pprint_keyword(indent,"abort");  break;
	case CONTINUE : return pos+nsp_ast_pprint_keyword(indent,"continue");  break;
	case WHAT :  return pos+nsp_ast_pprint_keyword(indent,"what");  break;
	  
	default:
	  Sciprintf("Warning in PlistPrettyPrint :");
	  s=nsp_astcode_to_name(ast->op);
	  if ( s != (char *) 0) Sciprintf(" %s ",s);
	}
    }
  return newpos;
}

static int _nsp_ast_pprint_args(NspAst *ast, int start, int last, int indent, int pos, 
				int posret, char *sep, int breakable, const char *breakstr)
{
  int j, len, newpos=pos;
  for ( j = start ; j <= last ; j++)
    {
      len =  _nsp_ast_printlength_arg(ast,j,indent,newpos,posret);
      if (breakable==TRUE && ( posret < newpos ) && ( newpos > CMAX || len > CMAX))
	{
	  Sciprintf(breakstr);newpos= Sciprintf2(posret, (use_html_color_class == TRUE) ? "&nbsp;": " ","");
	}
      newpos = _nsp_ast_pprint_arg(ast,j,indent,newpos,posret);
      if ( j != last ) newpos += Sciprintf(sep);
    }
  return newpos;
}

static int _nsp_ast_pprint_arg(NspAst *ast,int elt, int indent, int pos, int posret)
{
  NspList *L = ast->args;
  NspAst *ast1;
  if ( nsp_list_length(L) < elt) return 0; /* BUG */
  if ((ast1 = (NspAst*)nsp_list_get_element(L,elt)) == NULL) return 0; /* BUG */
  return _nsp_ast_pprint(ast1,indent,pos,posret);
}

static int _nsp_ast_pprint_arg_comment_ended(NspAst *ast,int elt)
{
  NspList *L = ast->args;
  NspAst *ast1;
  if ( nsp_list_length(L) < elt) return FALSE;
  if ((ast1 = (NspAst*)nsp_list_get_element(L,elt)) == NULL) return FALSE;
  return (ast1->op == COMMENT);
}

static int _nsp_ast_pprint_op_comment_ended(NspAst *ast, int elt)
{
  NspList *L = ast->args;
  NspAst *ast1;
  if ( nsp_list_length(L) < elt) return FALSE;
  if ((ast1 = (NspAst*)nsp_list_get_element(L,elt)) == NULL) return FALSE;
  if ( ast1->op == COLCONCAT ||  ast1->op ==CELLCOLCONCAT)
    {
      return _nsp_ast_pprint_arg_comment_ended(ast1,nsp_list_length(ast1->args));
    }
  return FALSE;
}

/*
 * returns the length of a mlhs  in an equal_op 
 */

static int _nsp_ast_equalop_mlhs_length(NspAst *ast)
{
  NspAst *mlhs = (NspAst *) nsp_list_get_element(ast->args,1);
  if (mlhs == NULL) return -1;
  if ( mlhs->op != MLHS ) return -1;
  return nsp_list_length(mlhs->args);
}

static int nsp_ast_obj_equal(NspAst *ast1,NspAst *ast2)
{
  if ( ast1->op != ast2->op ) return FALSE;
  switch ( ast1->op ) 
    {
    case NUMBER :
    case INUMBER32 :
    case INUMBER64 :
    case UNUMBER32 :
    case UNUMBER64 :
    case STRING:
    case COMMENT:
    case NAME :
    case OPNAME :
      return strcmp((char *) ast1->str,(char *) ast2->str)==0;
      break;
    default: 
      return TRUE;
    }
  return TRUE;
}

/*
 * checks that all elements are of type NspAst 
 */ 

int nsp_ast_check_args(NspList *L)
{
  int n = nsp_list_length(L);
  int i;
  for ( i = 1; i <= n ; i++)
    {
      NspObject *Obj = nsp_list_get_element(L,i);
      if ( IsAst(Obj) == FALSE ) return FAIL;
    }
  return OK;
}


static void nsp_ast_info_tree(NspAst *ast, int indent,const char *name,int rec_level)
{
  const int name_len=128;
  char epname[name_len];
  const char *pname = (name != NULL) ? name : NSP_OBJECT(ast)->name;
  Cell *C;
  int i=1;
  Sciprintf1(indent,"%s[",(strcmp(pname,NVOID) != 0) ? pname : "");
  nsp_ast_print_node(ast);Sciprintf("]\n");
  if ( rec_level <= user_pref.pr_depth ) 
    {
      C= ast->args->first;
      while ( C != NULLCELL) 
	{
	  int j;
	  sprintf(epname,"%s",pname);
	  for ( j = 0 ; j < strlen(epname);j++) 
	    {
	      if (epname[j] !='-' && epname[j] != '`' && epname[j] != ' ' && epname[j] != '|') epname[j]=' ';
	    }
	  for ( j = 0 ; j < strlen(epname);j++) if (epname[j]=='-' || epname[j] == '`' ) epname[j]=' ';
	  if ( C->next == NULLCELL) 
	    strcat(epname,"`-");
	  else 
	    strcat(epname,"|-");
	  if ( C->O != NULLOBJ )
	    {
	      nsp_object_info(C->O,indent,epname,rec_level+1);      
	    }
	  else
	    {
	      Sciprintf1(indent+2,"%s Undefined\n",epname);
	    }
	  C = C->next ;i++;
	}
    }
} 



int nsp_ast_set_str(NspAst *ast,const char *str)
{
  char *str1,*ast_str =((NspAst *) ast)->str ;
  int op = ((int) ast->op);
  switch ( op ) 
    {
    case NAME :
    case OPNAME :
    case STRING :
    case COMMENT :
    case NUMBER :
    case INUMBER32:
    case INUMBER64:
    case UNUMBER32:
    case UNUMBER64:
      if (( str1 =new_nsp_string(str)) == NULLSTRING) 
	{
	  Scierror("Error:\tCannot allocate string, running out of memory\n");
	  return FAIL;
	}
      if (ast_str  != NULL) nsp_string_destroy(&ast_str);
      ((NspAst *) ast)->str = str1 ;
      break;
    default:
      Scierror("Error: str cannot be set for the given ast node\n");
      return FAIL;
    }
  return OK;
}

/* check statements to detect if they contain newlines 
 */

static int _nsp_ast_pprint_statements_with_ret(NspAst *ast,int elt)
{
  NspAst *astel=  (NspAst*) nsp_list_get_element(ast->args,elt);
  int l, j;
  if ( astel->op != STATEMENTS )
    {
      Sciprintf("Should be statements\n");
      return FALSE;
    }
  l = nsp_list_length(astel->args);
  for ( j = 1 ; j <= l ; j++)
    {
      NspAst *ast1 = (NspAst*) nsp_list_get_element(astel->args,j);
      if ( ast1 == NULL) continue;
      switch (ast1->op)
	{
	case RETURN_OP : 
	case  COMMA_RET_OP : 
	case  SEMICOLON_RET_OP  :
	  return TRUE;
	}
    }
  return FALSE;
}


static int _nsp_ast_pprint_check_newline(NspAst *ast,int elt,int pos)
{
  int newpos;
  if ( _nsp_ast_pprint_statements_with_ret(ast,elt) == TRUE) 
    {
      Sciprintf("\n"); newpos=0;
    }
  else
    {
      newpos = pos+ Sciprintf(" ");
    }
  return newpos;
}

/**
 * nsp_ast_printlength:
 * @L: 
 * @indent: the curent indentation to use.
 * 
 * pretty printing of a #PList. 
 * 
 **/


static int _nsp_ast_printlength(NspAst *ast, int indent, int pos, int posret);
static int _nsp_ast_printlength_args(NspAst *ast, int start, int last, int indent, int pos, 
				     int posret, char *sep, int breakable, const char *breakstr);
static int _nsp_ast_printlength_arg(NspAst *ast,int elt, int indent, int pos, int posret);
static int _nsp_ast_printlength_arg_comment_ended(NspAst *ast,int elt);
static int _nsp_ast_printlength_op_comment_ended(NspAst *ast, int elt);

static int nsp_ast_printlength(NspAst * L, int indent)
{
  return _nsp_ast_printlength(L,indent,0,indent);
}

static int nsp_ast_printlength_comment(int indent,const char *str)
{
  return indent + 2 + strlen(str); // 
}

static int nsp_ast_printlength_string(int indent,const char *str)
{
  return indent +  2 + strlen(str);
}

static int nsp_ast_printlength_number(int indent,const char *str)
{
  return indent + strlen(str);
}

static int nsp_ast_printlength_keyword(int indent,const char *str)
{
  return indent + strlen(str);
}

/* print operator name after @indent blank characters 
 * and return the new position.
 * pos is giving the current position.
 */

static int nsp_ast_printlength_opname(int type, int indent, int pos)
{
  const char *s = nsp_astcode_to_name(type);
  return pos + indent + strlen(s);
}

/* posret: indentation to use if line-break. 
 *
 */

static int _nsp_ast_printlength(NspAst *ast, int indent, int pos, int posret)
{
  int j,newpos=0;
  
  /* be sure that we are starting to print at least at column postret */
  if ( pos < posret ) 
    {
      if ( ast->op != OBJECT ) pos= posret-pos +1;
    }
  /* select print operation */
  if ( ast->op > 0 ) 
    {
      /* operators **/
      switch ( ast->arity ) 
	{
	case 0: /* 0-ary operators */
	  return nsp_ast_printlength_opname(ast->op,indent,pos);
	  break;
	case 1:
	  switch ( ast->op ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_printlength_opname(ast->op,0,newpos);
	      return newpos;
	      break;
	    case  COMMA_RET_OP : 
	    case  SEMICOLON_RET_OP  :
	      newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_printlength_opname(ast->op,0,newpos);
	      return newpos;
	      break;
	    case QUOTE_OP : 
	    case DOTPRIM:
	      newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_printlength_opname(ast->op,0,newpos);
	      return  newpos;
	      break;
	    case RETURN_OP : 
	      return _nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	      break;
	    case TILDE_OP : 
	    default:
	      newpos =nsp_ast_printlength_opname(ast->op,indent,pos);
	      newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret);
	      return newpos;
	    }
	  break;
	case 2:
	  newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	  newpos =nsp_ast_printlength_opname(ast->op,1,newpos);
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret);
	  return newpos;
	  break;
	default :
	  newpos = pos;
	  for ( j = 0 ; j <  ast->arity ; j++ )
	    {
	      newpos =_nsp_ast_printlength_arg(ast,j+1,(j == 0) ? indent : 1,
					   newpos,posret);
	      if ( j != ast->arity -1 ) 
		{
		  newpos =nsp_ast_printlength_opname(ast->op,1,newpos);
		  newpos += 1;
		}
	    }
	  return newpos;
	  break;
	}
    }
  else 
    {
      switch ( ast->op ) 
	{
	case OPT:
	  /* val = value in a calling list */
	  newpos = _nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	  newpos += 3; /* Sciprintf(" = "); */
	  newpos = _nsp_ast_printlength_arg(ast,2,0,newpos,posret);
	  return newpos;
	  break;
	case EQUAL_OP:
	  /* affectations */
	  newpos = _nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	  if (  _nsp_ast_equalop_mlhs_length(ast) > 0 ) 
	    newpos += 1; /*  Sciprintf("="); */
	  /* fix new return position after = */
	  newpos = _nsp_ast_printlength_arg(ast,2,0,newpos, newpos);
	  return newpos;
	  break;
	case MLHS  :
	  /* left hand side of an equality 
	   * we do not display the left and right bracket 
	   *  if arity is one 
	   */
	  newpos = pos +  indent + (( ast->arity > 1) ? 1 : 0);
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  if ( ast->arity > 1) newpos += 1;
	  return newpos;
	  break;
	case ARGS :
	  /* a sequence of expressions inside () for x()*/
	  newpos = pos +  1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE,"\n");
	  newpos += 1;
	  return newpos;
	  break;
	case CELLARGS :
	  /* a sequence of expressions inside {} for x{} */
	  newpos = pos + 1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE,"\n");
	  newpos += 1;
	  return newpos;
	  break;
	case METARGS :
	  /* a sequence of expressions inside [] for x[] */
	  newpos = pos + 1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE,"\n");
	  newpos += 1;
	  return newpos;
	  break;
	case DOTARGS :
	  {
	    NspAst *ast1;
	    if ((ast1 =(NspAst*) nsp_list_get_element(ast->args,1)) == NULL) return newpos;
	    if (ast1->op != STRING ) return newpos;
	    newpos = pos + 1+ strlen((char *) ast1->str);
	    return newpos;
	  }
	case CALLEVAL:
	case LISTEVAL :
	  newpos = pos +  1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,"",FALSE,"");
	  return newpos;
	  break;
	case FEVAL :
	  newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	  newpos += 1;
	  newpos = _nsp_ast_printlength_args(ast,2,ast->arity,0,newpos,newpos,",",TRUE,"\n");
	  newpos += 1;
	  return newpos;
	  break;
	case PLIST :
	  /* such node should not appear here */
	  return newpos;
	  break;
	case COMMENT:
	  return pos+ nsp_ast_printlength_comment(indent,(const char *) ast->str); break;
	case NAME :
	  return pos+ indent + strlen((char *) ast->str); break;
	case OPNAME :
	  return pos+ indent + strlen((char *) ast->str); break;
	case NUMBER:
	  return pos + nsp_ast_printlength_number(indent,(const char *) ast->str);break;
	case STRING:
	  return pos + nsp_ast_printlength_string(indent,(const char *) ast->str);break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  return pos+  nsp_ast_printlength_number(indent,(const char *) ast->str);
	  break;
	case OBJECT: 
	  return pos; break;
	case EMPTYMAT:  return pos+ 2;break;
	case EMPTYCELL: return pos+ 2;break;
	case P_MATRIX :
	  newpos = pos + 1;
	  newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret+1);
	  newpos += 1;
	  return newpos;
	  break;
	case P_CELL :
	  newpos = pos + 1;
	  newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret+1);
	  newpos += 1;
	  return newpos;
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  newpos = pos;
	  for ( j = 0 ; j < ast->arity ; j++)
	    {
	      int is_comment;
	      newpos =_nsp_ast_printlength_arg(ast,j+1,0,newpos,posret);
	      is_comment =_nsp_ast_printlength_op_comment_ended(ast,j+1);
	      if ( j < ast->arity-1)
		{
		  if ( is_comment ) 
		    {
		      newpos = posret;
		    }
		  else
		    {
		      newpos =nsp_ast_printlength_opname(ast->op,0,newpos);
		    }
		}
	    }
	  return newpos;
	  break;
	case WHILE:
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"while");
	  newpos =_nsp_ast_printlength_arg(ast,1,1,newpos,posret);
	  newpos += nsp_ast_printlength_keyword(1,"do");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret+2);
	  newpos = nsp_ast_printlength_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case FUNCTION:
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"function");
	  newpos = _nsp_ast_printlength_arg(ast,1,1,newpos,newpos);
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret+2);
	  if ( ast->arity == 3 ) 
	    {
	      newpos =_nsp_ast_printlength_arg(ast,3,0,newpos,posret+2);
	    }
	  newpos= nsp_ast_printlength_keyword(Max(posret-newpos,0),"endfunction");
	  return newpos;
	  break;
	case FOR:
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"for");
	  newpos =_nsp_ast_printlength_arg(ast,1,1,newpos,posret);
	  newpos += 1; /* = */
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,newpos);
	  newpos += nsp_ast_printlength_keyword(1,"do");
	  newpos += 1; /* checknewline */
	  newpos =_nsp_ast_printlength_arg(ast,3,0,newpos,posret+2);
	  newpos += nsp_ast_printlength_keyword(0,"end");
	  return newpos;
	  break;
	case IF:
	  /* a sequence of if elseif etc.... */
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"if");
	  for ( j = 0 ; j < ast->arity  ; j += 2 )
	    {
	      if ( j == ast->arity-1 ) 
		{
		  /* we have reached the last else **/
		  newpos +=  nsp_ast_printlength_keyword(Max(posret-newpos,0),"else");
		  newpos += 1;
		  newpos =_nsp_ast_printlength_arg(ast,j+1,0,newpos,posret+2);
		}
	      else 
		{ 
		  if ( j != 0) 
		    {
		      newpos +=nsp_ast_printlength_keyword(Max(posret-newpos,0),"elseif");
		    }
		  newpos =_nsp_ast_printlength_arg(ast,j+1,1,newpos,posret+2);
		  newpos += nsp_ast_printlength_keyword(1,"then");
		  newpos += 1;
		  newpos =_nsp_ast_printlength_arg(ast,j+2,0,newpos,posret+2);
		}
	    }
	  newpos +=  nsp_ast_printlength_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case TRYCATCH :
	  /* try catch sequence */
	  newpos = pos+ nsp_ast_printlength_keyword(Max(posret-pos,0),"try");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret+2);
	  newpos += nsp_ast_printlength_keyword(Max(posret-newpos,0),"catch");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret+2);
	  if ( ast->arity == 3 ) 
	    {
	      newpos += nsp_ast_printlength_keyword(Max(posret-newpos,0),"finally");
	      newpos += 1;
	      newpos =_nsp_ast_printlength_arg(ast,3,0,newpos,posret+2);
	    }
	  newpos = nsp_ast_printlength_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case SELECT :
	  /* first argument is the test.
	   * next ones are cases 
	   */
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"select");
	  for ( j = 0 ; j < ast->arity ; j++)
	    {
	      if ( j==0) 
		{
		  _nsp_ast_printlength_arg(ast,j+1,1,newpos,posret);
		  newpos= 0;
		}
	      else
		{
		  newpos=_nsp_ast_printlength_arg(ast,j+1,0,newpos,posret+2);
		}
	    }
	  newpos = nsp_ast_printlength_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case STATEMENTS :
	  newpos=pos;
	  newpos= _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,posret,"",TRUE,"\n");
	  return newpos;
	  break;
	case STATEMENTS1 :
	  newpos=pos;
	  newpos= _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,posret,"",TRUE,"\n");
	  return newpos;
	  break;
	case PARENTH :
	  newpos = pos + 1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  newpos += 1;
	  return newpos;
	  break;
	case CASE : 
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"case");
	  newpos =_nsp_ast_printlength_arg(ast,1,1,newpos,posret+2);
	  nsp_ast_printlength_keyword(1,"then");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret+2);
	  return newpos;
	  break;
	case LASTCASE :
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"else");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret+2);
	  return newpos;
	  break;
	case GLOBAL:
	  /* n-ary global */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"global")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CLEAR:
	  /* n-ary clear */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"clear")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CLEARGLOBAL:
	  /* n-ary global */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"clearglobal")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case PAUSE:
	  /* can be 0 or 1-ary pause */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"pause")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case HELP:
	  /* 0 or  1-ary help */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"help")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case WHO:
	  /* 0 or 1-ary who */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"who")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case EXEC:
	  /* 1-ary exec */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"exec")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case APROPOS:
	  /* 1-ary apropos */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"apropos")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CD_COMMAND:
	  /* 1-ary cd */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"cd")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case LS_COMMAND:
	  /* 1-ary ls */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"ls")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case PWD_COMMAND:
	  /* 1-ary pwd */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"pwd")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->arity,0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;

	case BREAK: return pos+nsp_ast_printlength_keyword(indent,"break");break;
	case PRETURN:  return pos+nsp_ast_printlength_keyword(indent,"return"); break;
	case QUIT :   return pos+nsp_ast_printlength_keyword(indent,"quit");   break;
	case NSP_EXIT :  return pos+nsp_ast_printlength_keyword(indent,"exit");  break;
	case ABORT :  return pos+nsp_ast_printlength_keyword(indent,"abort");  break;
	case CONTINUE : return pos+nsp_ast_printlength_keyword(indent,"continue");  break;
	case WHAT :  return pos+nsp_ast_printlength_keyword(indent,"what");  break;
	  
	default: break;
	}
    }
  return newpos;
}

/* a set of Args separated by sep */

static int _nsp_ast_printlength_args(NspAst *ast, int start, int last, int indent, int pos, 
				int posret, char *sep, int breakable, const char *breakstr)
{
  NspList *L= ast->args;
  int j,  newpos=pos;
  for ( j = start ; j <= last ; j++)
    {
      NspAst *ast1;
      if ((ast1 =(NspAst*) nsp_list_get_element(L,j )) == NULL) return 0;
      newpos =_nsp_ast_printlength(ast1,indent,newpos,posret);
      if ( j != last ) newpos += strlen(sep);
    }
  return newpos;
}

static int _nsp_ast_printlength_arg(NspAst *ast,int elt, int indent, int pos, int posret)
{
  NspList *L = ast->args;
  NspAst *ast1;
  if ( nsp_list_length(L) < elt) return 0; /* BUG */
  if ((ast1 = (NspAst*)nsp_list_get_element(L,elt)) == NULL) return 0; /* BUG */
  return _nsp_ast_printlength(ast1,indent,pos,posret);
}

static int _nsp_ast_printlength_arg_comment_ended(NspAst *ast,int elt)
{
  NspList *L = ast->args;
  NspAst *ast1;
  if ( nsp_list_length(L) < elt) return FALSE;
  if ((ast1 = (NspAst*)nsp_list_get_element(L,elt)) == NULL) return FALSE;
  return (ast1->op == COMMENT);
}

static int _nsp_ast_printlength_op_comment_ended(NspAst *ast, int elt)
{
  NspList *L = ast->args;
  NspAst *ast1;
  if ( nsp_list_length(L) < elt) return FALSE;
  if ((ast1 = (NspAst*)nsp_list_get_element(L,elt)) == NULL) return FALSE;
  if ( ast1->op == COLCONCAT ||  ast1->op ==CELLCOLCONCAT)
    {
      return _nsp_ast_printlength_arg_comment_ended(ast1,nsp_list_length(ast1->args));
    }
  return FALSE;
}

#line 2647 "ast.c"
