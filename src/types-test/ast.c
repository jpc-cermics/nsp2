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





#line 33 "codegen/ast.override"
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
#line 348 "codegen/ast.override"
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
  Obj->line = 0;
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

#line 448 "codegen/ast.override"

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


#line 228 "ast.c"
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
#line 54 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 259 "ast.c"
  return H;
}

/*
 * delete 
 */

void nsp_ast_destroy_partial(NspAst *H)
{
#line 57 "codegen/ast.override"
/* verbatim in destroy */

#line 272 "ast.c"
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

#line 352 "codegen/ast.override"
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

#line 314 "ast.c"
#line 378 "codegen/ast.override"
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

#line 384 "ast.c"
/*
 * latex print 
 */

int nsp_ast_latex(NspAst *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_ast_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|op|= \\numprint{%d}\n",M->op);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|arity|= \\numprint{%d}\n",M->arity);
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|str|=\\verb@\"%s\"@\n",(M->str==NULL) ? "NULL": M->str);
  Sciprintf1(2,"\\\\\n");
        if ( M->xobj->type->latex(M->xobj,FALSE,"xobj",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(2,"\\\\\n");
  if ( M->args != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->args),FALSE,"args", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
        if ( M->user_data->type->latex(M->user_data,FALSE,"user_data",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+2,"\\verb|line|= \\numprint{%d}\n",M->line);
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

NspAst *nsp_ast_create(const char *name,int op,int arity,char* str,NspObject* xobj,NspList* args,NspObject* user_data,int line,NspTypeBase *type)
{
  NspAst *H  = nsp_ast_create_void(name,type);
  if ( H ==  NULLAST) return NULLAST;
  H->op=op;
  H->arity=arity;
  H->str = str;
  H->xobj= xobj;
  H->args= args;
  H->user_data= user_data;
  H->line=line;
  if ( nsp_ast_check_values(H) == FAIL) return NULLAST;
#line 54 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 535 "ast.c"
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
  H->line=self->line;
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
  H->line=self->line;
  return H;
}

NspAst *nsp_ast_full_copy(NspAst *self)
{
  NspAst *H  =nsp_ast_create_void(NVOID,(NspTypeBase *) nsp_type_ast);
  if ( H ==  NULLAST) return NULLAST;
  if ( nsp_ast_full_copy_partial(H,self)== NULL) return NULLAST;

#line 54 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 626 "ast.c"
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
  #line 54 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use RET_BUG for returned value */
#line 647 "ast.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 87 "codegen/ast.override"
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

#line 688 "ast.c"


#line 121 "codegen/ast.override"
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

#line 704 "ast.c"


#line 135 "codegen/ast.override"
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

#line 721 "ast.c"


#line 150 "codegen/ast.override"
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

#line 746 "ast.c"


#line 173 "codegen/ast.override"
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

#line 775 "ast.c"


#line 230 "codegen/ast.override"
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

#line 797 "ast.c"


#line 250 "codegen/ast.override"
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

#line 815 "ast.c"


#line 284 "codegen/ast.override"
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

#line 833 "ast.c"


#line 266 "codegen/ast.override"
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

#line 853 "ast.c"


#line 300 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_args(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  MoveObj(stack,1, NSP_OBJECT(self->args));
  return Max(lhs,1);
}

#line 866 "ast.c"


#line 61 "codegen/ast.override"
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

#line 894 "ast.c"


#line 200 "codegen/ast.override"
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

#line 911 "ast.c"


#line 215 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_line(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  int ret ;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  ret = ((int) self->line);
  if ((Ret=nsp_new_double_obj((double) ret))== NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

#line 928 "ast.c"


#line 320 "codegen/ast.override"
/* override a method */

static int _wrap_ast_sprint(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_sprint(self,stack,rhs,opt,lhs);
}
#line 938 "ast.c"


#line 328 "codegen/ast.override"
/* override a method */

static int _wrap_ast_fprint(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_fprint(self,stack,rhs,opt,lhs);
}

#line 949 "ast.c"


#line 311 "codegen/ast.override"
/* override a method */

static int _wrap_ast_print(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_print(self,stack,rhs,opt,lhs);
}

#line 960 "ast.c"


#line 337 "codegen/ast.override"
static int _wrap_ast_print_length(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = nsp_ast_printlength(self,0);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 973 "ast.c"


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
  {"get_line",(nsp_method *) _wrap_ast_get_line},
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

#line 476 "codegen/ast.override"

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
  int target = 4, columns = 90;
  char *s_target = NULL;
  const char *targets[]={"html", "gtk", "latex", "term", NULL };
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
			    { "target",string,NULLOBJ,-1},
			    { "columns",s_int, NULLOBJ,-1},
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
			    &as_read,&color,&depth,&indent,&s_target,&columns) == FAIL) 
    return RET_BUG;

  if ( s_target != NULL ) 
    {
      int rep;
      if ( (rep= is_string_in_array(s_target,targets,0)) == -1 )
	{
	  string_not_in_array(stack,s_target,targets , "optional argument targets");
	  return RET_BUG;
	}
      target = rep+1;
    }
  
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
			    nsp_ast_pprint(self, indent + 2, user_pref.color, target, TRUE,columns);
      Sciprintf2(indent+2," ","\n");
      Sciprintf2(indent+2," ","%s\n","</pre>");
      Sciprintf2(indent," ","%s\n","</div>");
    }
  else
    {
      nsp_ast_pprint(self, indent, user_pref.color, target, FALSE,columns);
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

static void nsp_ast_pprint(NspAst * L, int indent, int color,int target , int space, int columns)
{
  nsp_ast_generic_pretty_printer(L,indent,color, target, space, columns);
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


#line 1322 "ast.c"
