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





#line 27 "codegen/ast.override"
#include <nsp/objects.h>
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/parse.h>
#line 33 "ast.c"

/* ----------- NspAst ----------- */


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
  top->path_extract = (path_func *)  object_path_extract;
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
  Obj->obj = NULL;
  Obj->args = NULLLIST;
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

/*
 * A == B 
 */

static int nsp_ast_eq(NspAst *A, NspObject *B)
{
  NspAst *loc = (NspAst *) B;
  if ( check_cast(B,nsp_type_ast_id) == FALSE) return FALSE ;
  if ( A->op != loc->op) return FALSE;
  if ( A->arity != loc->arity) return FALSE;
  if ( A->obj != loc->obj) return FALSE;
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

/*
 * save 
 */

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
#line 46 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 250 "ast.c"
  return H;
}

/*
 * delete 
 */

void nsp_ast_destroy_partial(NspAst *H)
{
#line 49 "codegen/ast.override"
/* verbatim in destroy */
#line 262 "ast.c"
  if ( H->args != NULL ) 
    nsp_list_destroy(H->args);
}

void nsp_ast_destroy(NspAst *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_ast_destroy_partial(H);
  FREE(H);
}

#line 342 "codegen/ast.override"
/*
 * info overriden 
 */

int nsp_ast_info(NspAst *M, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t= [op:%s,%d,arity:%d,data:%d]\t\t%s ()\n",
	    pname,
	    nsp_astcode_to_name(M->op),
	    M->op, M->arity,
	    NSP_POINTER_TO_INT(M->obj),
	    nsp_ast_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

#line 293 "ast.c"
#line 362 "codegen/ast.override"
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
	  Sciprintf1(indent,"%s=ast_create(op=%d,arity=%d,line=%d);",pname,
		     M->op, M->arity, M->obj);
	}
      else 
	{
	  Sciprintf1(indent,"ast_create(op=%d,arity=%d,line=%d);",
		     M->op, M->arity, M->obj);
	}
    }
  else 
    {
      const char *s;
      Sciprintf1(indent,"%s\t={",pname);
      switch ( M->op ) 
	{
	case STRING:
	  Sciprintf("\"%s\"", M->obj);
	  break;
	case COMMENT:
	  Sciprintf1(indent,"//%s", M->obj);
	  break;
	case NUMBER:
	case INUMBER32:
	case INUMBER64:
	case UNUMBER32:
	case UNUMBER64:
	  Sciprintf("%s",M->obj);
	  break;
	case NAME :
	  Sciprintf("%s", M->obj);
	  break;
	case OPNAME :
	  Sciprintf("'%s'", M->obj);
	  break;
	case OBJECT : 
	  Sciprintf("obj:0x%x",M->obj);
	  break;
	default:
	  s=nsp_astcode_to_name( M->op);
	  if ( s != (char *) 0 )
	    Sciprintf("%s",s);
	  else 
	    Sciprintf("UNKNOWN->%d", M->op);
	  /* 
	  s=nsp_astcode_to_codename( M->op);
	  if ( s != (char *) 0 )
	    Sciprintf("(%s)",s);
	  else 
	    Sciprintf("(UNKNOWN->%d)", M->op);
	  */
	}
      Sciprintf("}\t\t%s\n",nsp_ast_type_short_string(NSP_OBJECT(M)));
      if ( M->args != NULL && nsp_list_length(M->args) > 0) 
	{
	  nsp_list_print(M->args,indent+2,"args",rec_level+1);
	}
    }
  return TRUE;
}

#line 366 "ast.c"
/*
 * latex print 
 */

int nsp_ast_latex(NspAst *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_ast_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"op=%d\n",M->op);
  Sciprintf1(indent+2,"arity=%d\n",M->arity);
  Sciprintf1(indent+2,"obj=0x%x\n",M->obj);
  if ( M->args != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->args),indent+2,"args",rec_level+1)== FALSE ) return FALSE ;
    }
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
  if ( check_cast (O,nsp_type_ast_id) == TRUE ) return ((NspAst *) O);
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
  if ( H->args == NULLLIST) 
    {
     if (( H->args = nsp_list_create("args")) == NULLLIST)
       return FAIL;
    }
  return OK;
}

NspAst *nsp_ast_create(const char *name,int op,int arity,void* obj,NspList* args,NspTypeBase *type)
{
  NspAst *H  = nsp_ast_create_void(name,type);
  if ( H ==  NULLAST) return NULLAST;
  H->op=op;
  H->arity=arity;
  H->obj = obj;
  H->args= args;
  if ( nsp_ast_check_values(H) == FAIL) return NULLAST;
#line 46 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 473 "ast.c"
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
  H->obj = self->obj;
  if ( self->args == NULL )
    { H->args = NULL;}
  else
    {
      if ((H->args = (NspList *) nsp_object_copy_and_name("args",NSP_OBJECT(self->args))) == NULLLIST) return NULL;
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
  H->obj = self->obj;
  if ( self->args == NULL )
    { H->args = NULL;}
  else
    {
      if ((H->args = (NspList *) nsp_object_full_copy_and_name("args",NSP_OBJECT(self->args))) == NULLLIST) return NULL;
    }
  return H;
}

NspAst *nsp_ast_full_copy(NspAst *self)
{
  NspAst *H  =nsp_ast_create_void(NVOID,(NspTypeBase *) nsp_type_ast);
  if ( H ==  NULLAST) return NULLAST;
  if ( nsp_ast_full_copy_partial(H,self)== NULL) return NULLAST;

#line 46 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 538 "ast.c"
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
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_ast_check_values(H) == FAIL) return RET_BUG;
#line 46 "codegen/ast.override"
/* verbatim in create/load/full_copy interface use RET_BUG for returned value */
#line 559 "ast.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 122 "codegen/ast.override"
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
      str = ((NspAst *) self)->obj;
      if ((Ret = nsp_new_string_obj(NVOID,str,-1))== NULLOBJ) return RET_BUG;
      break;
    default:
      if ((Ret = (NspObject *) nsp_smatrix_create(NVOID,0,0,"v",(int)0)) == NULLOBJ) 
	return RET_BUG;
    }
  MoveObj(stack,1,Ret);
  return 1;
}

#line 599 "ast.c"


#line 155 "codegen/ast.override"
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

#line 616 "ast.c"


#line 170 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_codename(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  const char *str;
  NspObject *Ret;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  switch ( ((int) self->op)) 
    {
    case STRING: if ((Ret = nsp_new_string_obj(NVOID,"STRING",-1))== NULLOBJ) return RET_BUG;break;
    case COMMENT: if ((Ret = nsp_new_string_obj(NVOID,"COMMENT",-1))== NULLOBJ) return RET_BUG;break;
    case NUMBER: if ((Ret = nsp_new_string_obj(NVOID,"NUMBER",-1))== NULLOBJ) return RET_BUG;break;
    case INUMBER32: if ((Ret = nsp_new_string_obj(NVOID,"INUMBER32",-1))== NULLOBJ) return RET_BUG;break;
    case INUMBER64: if ((Ret = nsp_new_string_obj(NVOID,"INUMBER64",-1))== NULLOBJ) return RET_BUG;break;
    case UNUMBER32: if ((Ret = nsp_new_string_obj(NVOID,"UNUMBER32",-1))== NULLOBJ) return RET_BUG;break;
    case UNUMBER64: if ((Ret = nsp_new_string_obj(NVOID,"UNUMBER64",-1))== NULLOBJ) return RET_BUG;break;
    case NAME : if ((Ret = nsp_new_string_obj(NVOID,"NAME",-1))== NULLOBJ) return RET_BUG;break;
    case OPNAME : if ((Ret = nsp_new_string_obj(NVOID,"OPNAME",-1))== NULLOBJ) return RET_BUG;break;
    case OBJECT :  if ((Ret = nsp_new_string_obj(NVOID,"OBJECT",-1))== NULLOBJ) return RET_BUG;break;
    default:
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
    }
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

#line 655 "ast.c"


#line 207 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_opname(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  const char *str;
  NspObject *Ret;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  switch ( ((int) self->op)) 
    {
    case STRING: if ((Ret = nsp_new_string_obj(NVOID,"STRING",-1))== NULLOBJ) return RET_BUG;break;
    case COMMENT: if ((Ret = nsp_new_string_obj(NVOID,"COMMENT",-1))== NULLOBJ) return RET_BUG;break;
    case NUMBER: if ((Ret = nsp_new_string_obj(NVOID,"NUMBER",-1))== NULLOBJ) return RET_BUG;break;
    case INUMBER32: if ((Ret = nsp_new_string_obj(NVOID,"INUMBER32",-1))== NULLOBJ) return RET_BUG;break;
    case INUMBER64: if ((Ret = nsp_new_string_obj(NVOID,"INUMBER64",-1))== NULLOBJ) return RET_BUG;break;
    case UNUMBER32: if ((Ret = nsp_new_string_obj(NVOID,"UNUMBER32",-1))== NULLOBJ) return RET_BUG;break;
    case UNUMBER64: if ((Ret = nsp_new_string_obj(NVOID,"UNUMBER64",-1))== NULLOBJ) return RET_BUG;break;
    case NAME : if ((Ret = nsp_new_string_obj(NVOID,"NAME",-1))== NULLOBJ) return RET_BUG;break;
    case OPNAME : if ((Ret = nsp_new_string_obj(NVOID,"OPNAME",-1))== NULLOBJ) return RET_BUG;break;
    case OBJECT :  if ((Ret = nsp_new_string_obj(NVOID,"OBJECT",-1))== NULLOBJ) return RET_BUG;break;
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

#line 694 "ast.c"


#line 259 "codegen/ast.override"
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
    case STRING: if ( strcmp(str1,"STRING")== 0) rep=TRUE;break;
    case COMMENT: if ( strcmp(str1,"COMMENT")== 0) rep=TRUE;break;
    case NUMBER: if ( strcmp(str1,"NUMBER")== 0) rep=TRUE;break;
    case INUMBER32: if ( strcmp(str1,"INUMBER32")== 0) rep=TRUE;break;
    case INUMBER64: if ( strcmp(str1,"INUMBER64")== 0) rep=TRUE;break;
    case UNUMBER32: if ( strcmp(str1,"UNUMBER32")== 0) rep=TRUE;break;
    case UNUMBER64: if ( strcmp(str1,"UNUMBER64")== 0)rep=TRUE;break;
    case NAME : if ( strcmp(str1,"NAME")== 0) rep=TRUE;break;
    case OPNAME : if ( strcmp(str1,"OPNAME")== 0) rep=TRUE;break;
    case OBJECT :  if ( strcmp(str1,"OBJECT")== 0) rep=TRUE;break;
    default:
      str=nsp_astcode_to_name(self->op);
      if ( str != (char *) 0 && strcmp(str,str1)==0 ) rep = TRUE;
    }
  if ( nsp_move_boolean(stack,1, rep) == FAIL)  return RET_BUG;
  return Max(lhs,1);
}

#line 726 "ast.c"


#line 289 "codegen/ast.override"
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
  MoveObj(stack,1, self->obj);
  return Max(lhs,1);
}

#line 744 "ast.c"


#line 305 "codegen/ast.override"
/* override a method */
static int _wrap_ast_get_args(NspAst *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  MoveObj(stack,1, NSP_OBJECT(self->args));
  return Max(lhs,1);
}

#line 757 "ast.c"


#line 244 "codegen/ast.override"
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

#line 774 "ast.c"


#line 325 "codegen/ast.override"
/* override a method */

static int _wrap_ast_sprint(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_sprint(self,stack,rhs,opt,lhs);
}
#line 784 "ast.c"


#line 333 "codegen/ast.override"
/* override a method */

static int _wrap_ast_fprint(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_fprint(self,stack,rhs,opt,lhs);
}

#line 795 "ast.c"


#line 316 "codegen/ast.override"
/* override a method */

static int _wrap_ast_print(NspAst *self,Stack stack,int rhs,int opt,int lhs)
{
  return meth_ast_print(self,stack,rhs,opt,lhs);
}

#line 806 "ast.c"


static NspMethods ast_methods[] = {
  {"get_str",(nsp_method *) _wrap_ast_get_str},
  {"get_op",(nsp_method *) _wrap_ast_get_op},
  {"get_codename",(nsp_method *) _wrap_ast_get_codename},
  {"get_opname",(nsp_method *) _wrap_ast_get_opname},
  {"is",(nsp_method *) _wrap_ast_is},
  {"get_obj",(nsp_method *) _wrap_ast_get_obj},
  {"get_args",(nsp_method *) _wrap_ast_get_args},
  {"get_arity",(nsp_method *) _wrap_ast_get_arity},
  {"sprint",(nsp_method *) _wrap_ast_sprint},
  {"fprint",(nsp_method *) _wrap_ast_fprint},
  {"print",(nsp_method *) _wrap_ast_print},
  { NULL, NULL}
};

static NspMethods *ast_get_methods(void) { return ast_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab ast_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_nsp_m2ast(Stack stack, int rhs, int opt, int lhs) /* m2ast */
{
  int_types T[] = {smat,t_end};
  NspSMatrix *S;
  NspAst *ret;

  if ( GetArgs(stack,rhs,opt,T,&S) == FAIL) return RET_BUG;
/* S << 1 */
    ret = nsp_m2ast(S);
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Ast_func[]={
  {"m2ast", _wrap_nsp_m2ast},
  { "ast_create", int_ast_create},
  { NULL, NULL}
};

/* call ith function in the Ast interface */

int Ast_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Ast_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Ast_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Ast_func[i].name;
  *f = Ast_func[i].fonc;
}

#line 113 "codegen/ast.override"

/* verbatim at the end of file */

/* methods 
 *
 */


#line 435 "codegen/ast.override"

/* pretty print of the ast i.e ast -> code */

static int _nsp_ast_pprint(NspAst *ast, int indent, int pos, int posret);
static int _nsp_ast_pprint_arg(NspList *L,int elt, int i, int pos, int posret);
static int _nsp_ast_pprint_arg_ret(NspList *L,int elt, int i, int pos, int posret, int *ret);
static int _nsp_ast_pprint_opname(int type, int indent, int pos);
static int _nsp_ast_pprint_args(NspList *L, int start, int last, int indent, int pos, int posret, char *sep);

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
  int as_read=FALSE,depth=INT_MAX,indent=0,color=TRUE;
  nsp_option print_opts[] ={{ "as_read",s_bool,NULLOBJ,-1},
			    { "color",s_bool,NULLOBJ,-1},
			    { "depth", s_int,NULLOBJ,-1},
			    { "indent",s_int,NULLOBJ,-1},
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
			 &as_read,&color,&depth,&indent) == FAIL) 
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
  
  nsp_ast_pprint(self, indent);
  
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

typedef enum { p_black =30, 
               p_red =31,
               p_green=32,
               p_yellow=33,
               p_blue=34,
               p_purple=35,
               p_cyan=36,
               p_white=37} nsp_term_colors;

#define NSP_PRINTF_COLOR(col,str)					\
  ((user_pref.color == TRUE ) ? Sciprintf("\033[%dm%s\033[0m",col,str)	\
   :Sciprintf("%s",str));

#define NSP_PRINTF1_COLOR(posret,col,str) \
  ((user_pref.color == TRUE ) ? Sciprintf1(posret,"\033[%dm%s\033[0m",col,str) \
   : Sciprintf1(posret,"%s",str))

#define CMAX 50

#define PRINTTAG(tag)							\
  if (pos != posret ) {							\
    Sciprintf("\n");newpos = NSP_PRINTF1_COLOR(posret,p_blue,tag) ;}	\
  else { newpos = pos+ NSP_PRINTF_COLOR(p_blue,tag) ; }

static int _nsp_ast_pprint(NspAst *ast, int indent, int pos, int posret)
{
  const char *s;
  int j,newpos=0,ret=FALSE;
  if ( ast->op > 0 ) 
    {
      /* operators **/
      switch ( ast->arity ) 
	{
	case 0: /* 0-ary operators */
	  return _nsp_ast_pprint_opname(ast->op,indent,pos);
	  break;
	case 1:
	  switch ( ast->op ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      newpos =_nsp_ast_pprint_arg(ast->args,1,indent,pos,posret);
	      newpos =_nsp_ast_pprint_opname(ast->op,0,newpos);
	      Sciprintf("\n");
	      return 0;
	      break;
	    case QUOTE_OP : 
	    case DOTPRIM:
	      newpos =_nsp_ast_pprint_arg(ast->args,1,indent,pos,posret);
	      newpos =_nsp_ast_pprint_opname(ast->op,0,newpos);
	      return  newpos;
	      break;
	    case RETURN_OP : 
	      _nsp_ast_pprint_arg(ast->args,1,indent,pos,posret);
	      Sciprintf("\n");
	      return 0;
	      break;
	    case TILDE_OP : 
	    default:
	      newpos =_nsp_ast_pprint_opname(ast->op,indent,pos);
	      newpos =_nsp_ast_pprint_arg(ast->args,1,0,newpos,posret);
	      return newpos;
	    }
	  break;
	case 2:
	  newpos =_nsp_ast_pprint_arg(ast->args,1,indent,pos,posret);
	  newpos =_nsp_ast_pprint_opname(ast->op,0,newpos);
	  if ( newpos > CMAX) 
	    {
	      Sciprintf("\n");
	      newpos =_nsp_ast_pprint_arg(ast->args,2,posret,0,posret);
	    }
	  else
	    {
	      newpos =_nsp_ast_pprint_arg(ast->args,2,0,newpos,posret);
	    }
	  return newpos;
	  break;
	default :
	  newpos = pos;
	  for ( j = 0 ; j <  ast->arity ; j++ )
	    {
	      newpos =_nsp_ast_pprint_arg(ast->args,j+1,(j == 0) ? indent : 1,
					   newpos,posret);
	      if ( j != ast->arity -1 ) 
		newpos =_nsp_ast_pprint_opname(ast->op,1,newpos);
	    }
	  break;
	}
    }
  else 
    {
      switch ( ast->op ) 
	{
	case OPT:
	  /* val = value in a calling list */
	  newpos = _nsp_ast_pprint_arg(ast->args,1,indent,pos,posret);
	  newpos += Sciprintf(" = ");
	  newpos = _nsp_ast_pprint_arg(ast->args,2,0,newpos,posret);
	  return newpos;
	  break;
	case EQUAL_OP:
	  /* affectations */
	  newpos = _nsp_ast_pprint_arg(ast->args,1,indent,pos,posret);
	  newpos += Sciprintf("=");
	  newpos = _nsp_ast_pprint_arg(ast->args,2,0,newpos,newpos);
	  return newpos;
	  break;
	case MLHS  :
	  /* left hand side of an equality 
	   * we do not display the left and right bracket 
	   *  if arity is one 
	   */
	  newpos = pos +  Sciprintf1(indent,( ast->arity > 1) ? "[" : "");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  if ( ast->arity > 1) newpos += Sciprintf("]");
	  return newpos;
	  break;
	case ARGS :
	  /* a sequence of expressions inside () for x()*/
	  newpos = pos +  Sciprintf1(indent,"(");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CELLARGS :
	  /* a sequence of expressions inside {} for x{} */
	  newpos = pos +  Sciprintf1(indent,"{");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  newpos += Sciprintf("}");
	  return newpos;
	  break;
	case METARGS :
	  /* a sequence of expressions inside [] for x[] */
	  newpos = pos +  Sciprintf1(indent,"[");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case DOTARGS :
	  /* a ? sequence of expressions inside {} for x{} */
	  newpos = pos + Sciprintf1(0,".");
	  newpos += _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,"");
	  return newpos;
	case CALLEVAL:
	case LISTEVAL :
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,"");
	  return newpos;
	  break;
	case FEVAL :
	  newpos =_nsp_ast_pprint_arg(ast->args,1,indent,pos,posret);
	  newpos += Sciprintf("(");
	  newpos = _nsp_ast_pprint_args(ast->args,2,ast->arity,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case PLIST :
	  /* such node should not appear here */
	  return newpos;
	  break;
	case COMMENT:
	  return pos+Sciprintf1(indent,"//%s",(char *) ast->obj);
	  break;
	case NAME :
#ifdef WITH_SYMB_TABLE_DEBUG
	  return pos+Sciprintf1(indent,"%s<%d>",(char *) ast->obj,ast->arity);
#else 
	  return pos+Sciprintf1(indent,"%s",(char *) ast->obj);
#endif 
	  break;
	case OPNAME :
	  return pos+Sciprintf1(indent,"'%s'",(char *) ast->obj);
	  break;
	case NUMBER:
	  return pos+  Sciprintf1(indent,"%s",(char *) ast->obj);
	  break;
	case STRING:
	  newpos = pos + Sciprintf1(indent,"");
	  newpos += strlen(((char *) ast->obj));
	  nsp_print_string_as_read((char *) ast->obj);
	  return newpos;
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  return pos+  Sciprintf1(indent,"%s",(char *) ast->obj);
	  break;
	case OBJECT: 
#ifdef WITH_SYMB_TABLE_DEBUG      
	  n = pos+Sciprintf1(indent,"{object:Ox%x}\n",(unsigned int) ast->obj);
	  nsp_object_print(ast->obj,0,0,0);
	  return n;
#else 
	  return pos;
#endif
	  break;
	case EMPTYMAT:  return pos+Sciprintf1(indent,"[]");break;
	case EMPTYCELL: return pos+Sciprintf1(indent,"{}");break;
	case P_MATRIX :
	  newpos = pos + Sciprintf1(indent,"[");
	  newpos =_nsp_ast_pprint_arg(ast->args,1,0,newpos,posret+1);
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case P_CELL :
	  newpos = pos + Sciprintf1(indent,"{");
	  newpos =_nsp_ast_pprint_arg(ast->args,1,0,newpos,posret+1);
	  newpos += Sciprintf("}");
	  return newpos;
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	  newpos =_nsp_ast_pprint_arg_ret(ast->args,1,indent,pos,posret,&ret);
	  if ( newpos == 0) 
	    {
	      newpos =_nsp_ast_pprint_opname(ast->op,posret,newpos);
	    }
	  else 
	    {
	      newpos =_nsp_ast_pprint_opname(ast->op,0,newpos);
	    }
	  if ( newpos > CMAX )
	    {
	      if ( ast->op == COLCONCAT ) Sciprintf("...");
	      Sciprintf("\n");
	      newpos =_nsp_ast_pprint_arg_ret(ast->args,2,posret,0,posret,&ret);
	    }
	  else 
	    {
	      newpos =_nsp_ast_pprint_arg_ret(ast->args,2,0,newpos,posret,&ret);
	    }
	  return newpos;
	  break;
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  for ( j = 0 ; j < ast->arity ; j++)
	    {
	      if ( j > 0 && newpos > CMAX  ) 
		{
		  Sciprintf("\n");
		  newpos =_nsp_ast_pprint_arg(ast->args,j+1,posret,0,posret);
		}
	      else
		{
		  newpos =_nsp_ast_pprint_arg(ast->args,j+1,0,newpos,posret);
		}
	      if ( j < ast->arity-1)
		newpos =_nsp_ast_pprint_opname(ast->op,0,newpos);
	    }
	  return newpos;
	  break;
	case WHILE:
	  PRINTTAG("while");
	  newpos =_nsp_ast_pprint_arg(ast->args,1,1,newpos,posret);
	  newpos += NSP_PRINTF1_COLOR(1,p_blue,"do\n");
	  newpos =_nsp_ast_pprint_arg(ast->args,2,posret+2,0,posret+2);
	  newpos += NSP_PRINTF1_COLOR(posret,p_blue,"end");
	  return newpos;
	  break;
	case FUNCTION:
	  /* Sciprintf("function arity %d\n",ast->arity); */
	  PRINTTAG("function");
	  _nsp_ast_pprint_arg(ast->args,1,1,pos,newpos+1);
	  Sciprintf("\n");
	  newpos =_nsp_ast_pprint_arg(ast->args,2,posret+2,pos,posret+2);
	  if ( newpos != 0)  Sciprintf("\n");
	  if ( ast->arity == 3 ) 
	    {
	      newpos =_nsp_ast_pprint_arg(ast->args,3,posret+2,pos,posret+2);
	    }
	  return NSP_PRINTF1_COLOR(posret,p_blue,"endfunction");
	  break;
	case FOR:
	  PRINTTAG("for");
	  newpos =_nsp_ast_pprint_arg(ast->args,1,1,newpos,posret);
	  newpos += Sciprintf("=") ;
	  newpos =_nsp_ast_pprint_arg(ast->args,2,0,newpos,newpos+1);
	  newpos += Sciprintf(" ");
	  PRINTTAG("do");
	  newpos += Sciprintf("\n");
	  newpos =_nsp_ast_pprint_arg(ast->args,3,posret+2,0,posret+2);
	  if ( newpos != 0)  Sciprintf("\n");
	  return NSP_PRINTF1_COLOR(posret,p_blue,"end");
	  break;
	case IF:
	  /* a sequence of if elseif etc.... */
	  PRINTTAG("if");
	  for ( j = 0 ; j < ast->arity  ; j += 2 )
	    {
	      if ( j == ast->arity-1 ) 
		{
		  /* we have reached the last else **/
		  if ( newpos != 0) Sciprintf("\n");
		  NSP_PRINTF1_COLOR(posret,p_blue,"else\n");
		  newpos =_nsp_ast_pprint_arg(ast->args,j+1,posret+2,0,posret+2);
		}
	      else 
		{ 
		  if ( j != 0) 
		    {
		      if ( newpos != 0) Sciprintf("\n");
		      newpos = NSP_PRINTF1_COLOR(posret,p_blue,"elseif");
		    }
		  newpos =_nsp_ast_pprint_arg(ast->args,j+1,1,newpos+1,newpos+1);
		  NSP_PRINTF1_COLOR(1,p_blue,"then\n");
		  newpos =_nsp_ast_pprint_arg(ast->args,j+2,posret+2,0,posret+2);
		}
	    } 
	  if ( newpos != 0) Sciprintf("\n");
	  newpos = NSP_PRINTF1_COLOR(posret,p_blue,"end");
	  return newpos;
	  break;
	case TRYCATCH :
	  /* try catch sequence */
	  PRINTTAG("try");
	  newpos += Sciprintf1(1,"\n");
	  newpos =_nsp_ast_pprint_arg(ast->args,1,posret+2,0,posret+2);
	  newpos += NSP_PRINTF1_COLOR(posret,p_blue,"catch");
	  newpos += Sciprintf1(1,"\n");
	  newpos =_nsp_ast_pprint_arg(ast->args,2,posret+2,0,posret+2);
	  if ( ast->arity == 2 ) 
	    {
	      newpos += NSP_PRINTF1_COLOR(posret,p_blue,"end");
	    }
	  else 
	    {
	      newpos += NSP_PRINTF1_COLOR(posret,p_blue,"finally");
	      newpos =_nsp_ast_pprint_arg(ast->args,3,posret+2,0,posret+2);
	      newpos += NSP_PRINTF1_COLOR(posret,p_blue,"end");
	    }
	  return newpos;
	  break;
	case SELECT :
	  /* arity N. ast argument is the test other arguments are 
	   * the cases 
	   */
	  PRINTTAG("select");
	  for ( j = 0 ; j < ast->arity ; j++)
	    {
	      if ( j==0) 
		{
		  _nsp_ast_pprint_arg(ast->args,j+1,1,newpos,posret);
		  Sciprintf("\n");
		  newpos = 0;
		}
	      else
		{
		  newpos=_nsp_ast_pprint_arg(ast->args,j+1,posret+2,newpos,posret+2);
		}
	    }
	  if ( newpos != 0) Sciprintf("\n");
	  newpos = NSP_PRINTF1_COLOR(posret,p_blue,"end");
	  return newpos;
	  break;
	case STATEMENTS :
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos= _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,posret,"");
	  return newpos;
	  break;
	case STATEMENTS1 :
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos= _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,posret,"");
	  return newpos;
	  break;
	case PARENTH :
	  newpos = pos + Sciprintf1(indent,"(") ;
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CASE :
	  if ( pos != 0) Sciprintf("\n");
	  if ( ast->op  == COMMENT )
	    {
	      newpos =_nsp_ast_pprint_arg(ast->args,1,posret,0,posret);
	    }
	  else
	    {
	      newpos = NSP_PRINTF1_COLOR(posret,p_blue,"case") ;
	      newpos =_nsp_ast_pprint_arg(ast->args,1,1,newpos,newpos+1);
	      newpos += NSP_PRINTF1_COLOR(1,p_blue,"then\n") ;
	      newpos =_nsp_ast_pprint_arg(ast->args,2,posret+2,0,posret+2);
	    }
	  return newpos;
	  break;
	case LASTCASE :
	  if ( pos != 0) Sciprintf("\n");
	  NSP_PRINTF1_COLOR(posret,p_blue,"else\n") ;
	  newpos =_nsp_ast_pprint_arg(ast->args,1,posret+2,0,posret+2);
	  return newpos;
	  break;
	case GLOBAL:
	  /* n-ary global */
	  PRINTTAG("global");Sciprintf(" ");
	  /* newpos = NSP_PRINTF1_COLOR(posret,p_blue,"global ") ; */
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case CLEAR:
	  /* n-ary clear */
	  PRINTTAG("clear");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case CLEARGLOBAL:
	  /* n-ary global */
	  PRINTTAG("clearglobal");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case PAUSE:
	  /* can be 0 or 1-ary pause */
	  PRINTTAG("pause");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case HELP:
	  /* 0 or  1-ary help */
	  PRINTTAG("help");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case WHO:
	  /* 0 or 1-ary who */
	  PRINTTAG("who");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case EXEC:
	  /* 1-ary exec */
	  PRINTTAG("exec");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case APROPOS:
	  /* 1-ary apropos */
	  PRINTTAG("apropos");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case CD_COMMAND:
	  /* 1-ary cd */
	  PRINTTAG("cd");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case LS_COMMAND:
	  /* 1-ary ls */
	  PRINTTAG("ls");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case PWD_COMMAND:
	  /* 1-ary pwd */
	  PRINTTAG("pwd");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast->args,1,ast->arity,0,newpos,newpos,",");
	  return newpos;
	  break;

	case BREAK: return pos+NSP_PRINTF1_COLOR(indent,p_blue,"break");break;
	case PRETURN:  return pos+NSP_PRINTF1_COLOR(indent,p_blue,"return"); break;
	case QUIT :   return pos+NSP_PRINTF1_COLOR(indent,p_blue,"quit");   break;
	case NSP_EXIT :  return pos+NSP_PRINTF1_COLOR(indent,p_blue,"exit");  break;
	case ABORT :  return pos+NSP_PRINTF1_COLOR(indent,p_blue,"abort");  break;
	case CONTINUE : return pos+NSP_PRINTF1_COLOR(indent,p_blue,"continue");  break;
	case WHAT :  return pos+NSP_PRINTF1_COLOR(indent,p_blue,"what");  break;
	  
	default:
	  Sciprintf("Warning in PlistPrettyPrint :");
	  s=nsp_astcode_to_name(ast->op);
	  if ( s != (char *) 0) Sciprintf(" %s ",s);
	}
    }
  return 0;
}
 
static int _nsp_ast_pprint_opname(int type, int indent, int pos)
{
  Sciprintf1(indent,"");
  return pos+ nsp_print_opname(type) + indent;
}

/* a set of Args separated by sep */

/* #define WITH_SYMB_TABLE_DEBUG */

static int _nsp_ast_pprint_args(NspList *L, int start, int last, int indent, int pos, int posret, char *sep)
{
  NspAst *ast;
  int j,  newpos=pos,indent1=indent;
  for ( j = start ; j <= last ; j++)
    {
      if ((ast =(NspAst*) nsp_list_get_element(L,j )) == NULL) return 0;
      newpos =_nsp_ast_pprint(ast,indent1,newpos,posret);
      if ( j != last ) newpos += Sciprintf(sep);
      /* reset indent for next argument of necessary **/
      if ( indent1 != indent ) indent1 = indent; 
      /* newpos==0 if the previous ArgPrettyPrint has inserted a \n **/
      if ( newpos == 0) indent1 = posret;
      /* if we have remaining arguments and  line is too long we insert \n */
      if ( newpos > CMAX && j != last ) 
	{
	  newpos=0; indent1=indent;Sciprintf("\n");
	}
    }
  return newpos;
}

static int _nsp_ast_pprint_arg(NspList *L,int elt, int indent, int pos, int posret)
{
  NspAst *ast;
  if ( nsp_list_length(L) < elt) return 0; /* BUG */
  if ((ast = (NspAst*)nsp_list_get_element(L,elt)) == NULL) return 0; /* BUG */
  return _nsp_ast_pprint(ast,indent,pos,posret);
}

/* similar to _nsp_ast_pprint_arg_ 
 * but add a newline if ar is a comment 
 */

static int _nsp_ast_pprint_arg_ret(NspList *L,int elt, int indent, int pos, int posret, int *ret)
{
  int newpos;
  NspAst *ast =  (NspAst *) nsp_list_get_element(L,elt);
  if ( ast == NULL ) return 0; /* BUG */
  newpos = _nsp_ast_pprint(ast,indent,pos,posret);
  if ( ast->op == COMMENT )
    {
      Sciprintf("\n");
      newpos = 0;
      *ret = TRUE;
    }
  else 
    {
      *ret = FALSE;
    }
  return newpos; 
}

/* a enlever */

static NspAst *nsp_m2ast(NspSMatrix *S)
{
  NspAst *ast;
  PList p;
  if ((p=nsp_parse_expr(S)) == NULL) return NULL;
  if ((ast= nsp_plist_to_ast(NVOID,p))== NULLAST ) return NULL;
  return ast;
}

#line 1569 "ast.c"
