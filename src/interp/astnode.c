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
 *
 * Astnode are used at nsp level to manipulate an Ast 
 * On can get at nsp level an ast build as follows 
 * ast := list(arg1 ... argn) 
 * argi := astnode | ast.
 *--------------------------------------------------------------------------*/

#include <nsp/nsp.h>
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/smatrix.h> 
#include <nsp/matrix.h> 
#include <nsp/file.h> 
#include <nsp/hobj.h> 
#define  AstNode_Private 
#include <nsp/astnode.h> 
#include <nsp/type.h> 
#include <nsp/interf.h>
#include <nsp/parse.h>

/* 
 * NspAstNode inherits from NspObject 
 */

int nsp_type_astnode_id=0;
NspTypeAstNode *nsp_type_astnode=NULL;

/*
 * Type object for AstNode 
 * all the instance of NspTypeAstNode share the same id. 
 * nsp_type_astnode: is an instance of NspTypeAstNode 
 *    used for objects of NspAstNode type (i.e built with new_astnode) 
 * other instances are used for derived classes 
 */
NspTypeAstNode *new_type_astnode(type_mode mode)
{
  NspTypeAstNode *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_astnode != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_astnode;
    }
  if ((type =  malloc(sizeof(NspTypeAstNode))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = astnode_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = astnode_get_methods; 
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_astnode;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for astnode */ 

  top->pr = (print_func *) nsp_astnode_print;                  
  top->dealloc = (dealloc_func *) nsp_astnode_destroy;
  top->copy  =  (copy_func *) nsp_astnode_copy;                 
  top->size  = (size_func *) nsp_astnode_size;                
  top->s_type =  (s_type_func *) nsp_astnode_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_astnode_type_short_string;
  top->info = (info_func *) nsp_astnode_info ;                  
  /* top->is_true = (is_true_func  *) nsp_astnode_is_true; */
  /* top->loop =(loop_func *) nsp_astnode_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_astnode_object;
  top->eq  = (eq_func *) nsp_astnode_eq;
  top->neq  = (eq_func *) nsp_astnode_neq;
  top->save  = (save_func *) nsp_astnode_xdr_save;
  top->load  = (load_func *) nsp_astnode_xdr_load;
  top->create = (create_func*) int_astnode_create;
  
  /* specific methods for astnode */
      
  type->init = (init_func *) init_astnode;

  /* 
   * AstNode interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_astnode_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeAstNode called nsp_type_astnode
       */
      type->id =  nsp_type_astnode_id = nsp_new_type_id();
      nsp_type_astnode = type;
      if ( nsp_register_type(nsp_type_astnode) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_astnode(mode);
    }
  else 
    {
      type->id = nsp_type_astnode_id;
      return type;
    }
}

/*
 * initialize AstNode instances 
 * locally and by calling initializer on parent class 
 */

static int init_astnode(NspAstNode *o,NspTypeAstNode *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of AstNode 
 */

NspAstNode *new_astnode() 
{
  NspAstNode *loc; 
  /* type must exists */
  nsp_type_astnode = new_type_astnode(T_BASE);
  if ( (loc = malloc(sizeof(NspAstNode)))== NULLASTNODE) return loc;
  /* initialize object */
  if ( init_astnode(loc,nsp_type_astnode) == FAIL) return NULLASTNODE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for AstNode 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_astnode_size(NspAstNode *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char astnode_type_name[]="AstNode";
static char astnode_short_type_name[]="astnode";

static char *nsp_astnode_type_as_string(void)
{
  return(astnode_type_name);
}

static char *nsp_astnode_type_short_string(NspObject *v)
{
  return(astnode_short_type_name);
}

/*
 * A == B 
 */

static int nsp_astnode_eq(NspAstNode *A, NspObject *B)
{
  NspAstNode *loc = (NspAstNode *) B;
  if ( check_cast(B,nsp_type_astnode_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->op != loc->obj->op) return FALSE;
  if ( A->obj->arity != loc->obj->arity) return FALSE;
  if ( A->obj->obj != loc->obj->obj) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_astnode_neq(NspAstNode *A, NspObject *B)
{
  return ( nsp_astnode_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_astnode_xdr_save(XDR *xdrs, NspAstNode *M)
{
#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_astnode)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->op) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->arity) == FAIL) return FAIL;
  switch ( M->obj->op ) 
    {
    case NUMBER:
    case INUMBER32:
    case INUMBER64:
    case UNUMBER32:
    case UNUMBER64:
    case NAME :
    case OPNAME :
      if (nsp_xdr_save_string(xdrs,(char *) M->obj->obj) == FAIL) return FAIL;
      break;
    case STRING:
    case COMMENT:
      if (nsp_xdr_save_i(xdrs,strlen( M->obj->obj))== FAIL) return FAIL;
      if (nsp_xdr_save_array_c(xdrs,(char *) M->obj->obj, strlen( M->obj->obj)) == FAIL) return FAIL;
      break;
    case OBJECT :
      if (nsp_object_xdr_save(xdrs,(NspObject *) M->obj->obj) == FAIL) return FAIL;
      break;
    default:
      if (nsp_xdr_save_i(xdrs, NSP_POINTER_TO_INT(M->obj->obj)) == FAIL) return FAIL;
    }
  return OK;
}

/*
 * load 
 */

static NspAstNode  *nsp_astnode_xdr_load(XDR *xdrs)
{
  NspObject *Obj;
  int line,lstr;
  NspAstNode *M = NULL;
  static char name[NAME_MAXL];
  char *str;
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLASTNODE;
  if ((M  = astnode_create_void(name,(NspTypeBase *) nsp_type_astnode))== NULLASTNODE)
    return M;
  if((M->obj = calloc(1,sizeof(nsp_astnode)))== NULL ) return NULL;
  M->obj->ref_count = 1;
  if (nsp_xdr_load_i(xdrs, &M->obj->op) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->arity) == FAIL) return NULL;
  switch ( M->obj->op ) 
    {
    case NUMBER:
    case INUMBER32:
    case INUMBER64:
    case UNUMBER32:
    case UNUMBER64:
    case NAME :
    case OPNAME :
      if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLASTNODE;
      if ((str = nsp_string_copy(name)) ==NULL) return NULLASTNODE;
      M->obj->obj = str;
      break;
    case STRING:
    case COMMENT:
      if (nsp_xdr_load_i(xdrs,&lstr) == FAIL) return  NULLASTNODE;
      if ((M->obj->obj  = new_nsp_string_n(lstr)) == NULLSTRING ) return NULLASTNODE; 
      if (nsp_xdr_load_array_c(xdrs,M->obj->obj, lstr) == FAIL) return  NULLASTNODE;
      break;
    case OBJECT :
      if ((Obj =nsp_object_xdr_load(xdrs)) == NULL) return NULLASTNODE;
      M->obj->obj = Obj;
      break;
    default:
      if (nsp_xdr_load_i(xdrs, &line) == FAIL) return NULL;
      M->obj->obj = NSP_INT_TO_POINTER(line);
    }
  return M;
}

/*
 * delete 
 */

void nsp_astnode_destroy(NspAstNode *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
    {
      NspObject *Obj;
      switch ( H->obj->op ) 
	{
	case NUMBER:
	case INUMBER32:
	case INUMBER64:
	case UNUMBER32:
	case UNUMBER64:
	case NAME :
	case OPNAME :
	case STRING:
	case COMMENT:
	  FREE(H->obj->obj);
	  break;
	case OBJECT :
	  Obj = (NspObject *) H->obj->obj;
	  nsp_object_destroy(&Obj);
	  break;
	}
      FREE(H->obj);
    }
  FREE(H);
}

/*
 * info 
 */

int nsp_astnode_info(NspAstNode *M, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t= [op:%s,%d,arity:%d,data:%d]\t\t%s ()\n",
	    pname,
	    nsp_astcode_to_name(M->obj->op),
	    M->obj->op, M->obj->arity,
	    NSP_POINTER_TO_INT(M->obj->obj),
	    nsp_astnode_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_astnode_print(NspAstNode *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=astnode_create(op=%d,arity=%d,line=%d);",pname,
		     M->obj->op, M->obj->arity, M->obj->obj);
	}
      else 
	{
	  Sciprintf1(indent,"astnode_create(op=%d,arity=%d,line=%d);",
		     M->obj->op, M->obj->arity, M->obj->obj);
	}
    }
  else 
    {
      const char *s;
      Sciprintf1(indent,"%s\t={",pname);
      switch ( M->obj->op ) 
	{
	case STRING:
	  Sciprintf("\"%s\"", M->obj->obj);
	  break;
	case COMMENT:
	  Sciprintf1(indent,"//%s", M->obj->obj);
	  break;
	case NUMBER:
	case INUMBER32:
	case INUMBER64:
	case UNUMBER32:
	case UNUMBER64:
	  Sciprintf("%s",M->obj->obj);
	  break;
	case NAME :
	  Sciprintf("%s", M->obj->obj);
	  break;
	case OPNAME :
	  Sciprintf("'%s'", M->obj->obj);
	  break;
	case OBJECT : 
	  Sciprintf("obj:0x%x",M->obj->obj);
	  break;
	default:
	  s=nsp_astcode_to_name( M->obj->op);
	  if ( s != (char *) 0 )
	    Sciprintf("%s",s);
	  else 
	    Sciprintf("UNKNOWN->%d", M->obj->op);
	  /* 
	  s=nsp_astcode_to_codename( M->obj->op);
	  if ( s != (char *) 0 )
	    Sciprintf("(%s)",s);
	  else 
	    Sciprintf("(UNKNOWN->%d)", M->obj->op);
	  */
	}
      Sciprintf("}\t\t%s\n",nsp_astnode_type_short_string(NSP_OBJECT(M)));
    }
  return TRUE;
}




/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for AstNode objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspAstNode   *nsp_astnode_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_astnode_id) == TRUE ) return ((NspAstNode *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_astnode));
  return NULL;
}

int IsAstNodeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_astnode_id);
}

int IsAstNode(NspObject *O)
{
  return nsp_object_type(O,nsp_type_astnode_id);
}

NspAstNode  *GetAstNodeCopy(Stack stack, int i)
{
  if (  GetAstNode(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspAstNode  *GetAstNode(Stack stack, int i)
{
  NspAstNode *M;
  if (( M = nsp_astnode_object(NthObj(i))) == NULLASTNODE)
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspAstNode *astnode_create_void(char *name,NspTypeBase *type)
{
  NspAstNode *H  = (type == NULL) ? new_astnode() : type->new();
  if ( H ==  NULLASTNODE)
    {
      Sciprintf("No more memory\n");
      return NULLASTNODE;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return NULLASTNODE;
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->obj = NULL;
  return H;
}

NspAstNode *astnode_create(char *name,int op,int arity,void *data,NspTypeBase *type)
{
  NspAstNode *H  = astnode_create_void(name,type);
  if ( H ==  NULLASTNODE) return NULLASTNODE;
  if ((H->obj = malloc(sizeof(nsp_astnode))) == NULL) return NULL;
  H->obj->ref_count=1;
  H->obj->op=op;
  H->obj->arity=arity;
  H->obj->obj= data;
  return H;
}

/*
 * copy for gobject derived class  
 */

NspAstNode *nsp_astnode_copy(NspAstNode *self)
{
  NspAstNode *H  =astnode_create_void(NVOID,(NspTypeBase *) nsp_type_astnode);
  if ( H ==  NULLASTNODE) return NULLASTNODE;
  H->obj = self->obj;
  self->obj->ref_count++;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the AstNode
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_astnode_create(Stack stack, int rhs, int opt, int lhs)
{
  NspAstNode *H;
  CheckStdRhs(0,0);
  /* want to be sure that type astnode is initialized */
  nsp_type_astnode = new_type_astnode(T_BASE);
  if(( H = astnode_create_void(NVOID,(NspTypeBase *) nsp_type_astnode)) == NULLASTNODE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if((H->obj = calloc(1,sizeof(nsp_astnode)))== NULL ) return RET_BUG;
  H->obj->ref_count = 1;
  H->obj->op= 0;
  H->obj->arity=0;
  H->obj->obj= NULL;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/* methods 
 *
 */


static int int_astnode_meth_get_str(NspAstNode *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  char *str=NULL;
  int op; 
  CheckRhs(0,0); 
  CheckLhs(1,1);
  op = ((int) self->obj->op);
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
      str = ((NspAstNode *) self)->obj->obj;
      if ((Ret = nsp_new_string_obj(NVOID,str,-1))== NULLOBJ) return RET_BUG;
      break;
    default:
      if ((Ret = (NspObject *) nsp_smatrix_create(NVOID,0,0,"v",(int)0)) == NULLOBJ) 
	return RET_BUG;
    }
  MoveObj(stack,1,Ret);
  return 1;
}

static int int_astnode_meth_get_op(NspAstNode *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  int ret ;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  ret = ((int) self->obj->op);
  if ((Ret=nsp_new_double_obj((double) ret))== NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

static int int_astnode_meth_get_codename(NspAstNode *self, Stack stack, int rhs, int opt, int lhs)
{
  const char *str;
  NspObject *Ret;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  switch ( ((int) self->obj->op)) 
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
      str=nsp_astcode_to_codename(self->obj->op);
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

static int int_astnode_meth_get_opname(NspAstNode *self, Stack stack, int rhs, int opt, int lhs)
{
  const char *str;
  NspObject *Ret;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  switch ( ((int) self->obj->op)) 
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
      str=nsp_astcode_to_name(self->obj->op);
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


static int int_astnode_meth_is(NspAstNode *self, Stack stack, int rhs, int opt, int lhs)
{
  int rep = FALSE;
  const char *str,*str1;
  CheckRhs(1,1);
  CheckLhs(1,1); 
  if ((str1 = GetString(stack,1)) == (char*)0) return RET_BUG;
  switch ( ((int) self->obj->op)) 
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
      str=nsp_astcode_to_name(self->obj->op);
      if ( str != (char *) 0 && strcmp(str,str1)==0 ) rep = TRUE;
    }
  if ( nsp_move_boolean(stack,1, rep) == FAIL)  return RET_BUG;
  return Max(lhs,1);
}

static int int_astnode_meth_get_obj(NspAstNode *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  if ( ((int) self->obj->op) != OBJECT )
    {
      Scierror("Error: an object can be returned only for astnode of id OBJECT\n");
      return RET_BUG;
    }
  MoveObj(stack,1, self->obj->obj);
  return Max(lhs,1);
}


static NspMethods astnode_methods[] = {
  {"get_id",(nsp_method *) int_astnode_meth_get_op},
  {"get_str",(nsp_method *) int_astnode_meth_get_str},
  {"get_idname",(nsp_method *) int_astnode_meth_get_codename},
  {"get_opname",(nsp_method *) int_astnode_meth_get_opname},
  {"is", (nsp_method *) int_astnode_meth_is},
  {"get_obj", (nsp_method *) int_astnode_meth_get_obj},
  { NULL, NULL}
};

static NspMethods *astnode_get_methods(void) { return astnode_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_astnode_get_op(void *self,char *attr)
  {
  int ret;

  ret = ((int) ((NspAstNode *) self)->obj->op);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_astnode_set_op(void *self, char *attr, NspObject *O)
{
  int op;

  if ( IntScalar(O,&op) == FAIL) return FAIL;
  ((NspAstNode *) self)->obj->op = op;
  return OK;
}

static NspObject *_wrap_astnode_get_arity(void *self,char *attr)
{
  int ret;

  ret = ((int) ((NspAstNode *) self)->obj->arity);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_astnode_set_arity(void *self, char *attr, NspObject *O)
{
  int arity;
  if ( IntScalar(O,&arity) == FAIL) return FAIL;
  ((NspAstNode *) self)->obj->arity = arity;
  return OK;
}

static NspObject *_wrap_astnode_get_line(void *self,char *attr)
{
  int ret= NSP_POINTER_TO_INT(((NspAstNode *) self)->obj->obj);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_astnode_set_line(void *self, char *attr, NspObject *O)
{
  int line;
  if ( IntScalar(O,&line) == FAIL) return FAIL;
  switch (((NspAstNode *) self)->obj->op ) 
    {
    case NUMBER:
    case INUMBER32:
    case INUMBER64:
    case UNUMBER32:
    case UNUMBER64:
    case NAME :
    case OPNAME :
    case STRING:
    case COMMENT:
    case OBJECT :
      Scierror("Error: cannot set line for this type of astnode\n");
      return RET_BUG;
    }
  ((NspAstNode *) self)->obj->obj =(void *) NSP_INT_TO_POINTER(line);
  return OK;
}

static AttrTab astnode_attrs[] = {
  { "op", (attr_get_function *)_wrap_astnode_get_op, (attr_set_function *)_wrap_astnode_set_op,(attr_get_object_function *)int_get_object_failed ,(attr_set_object_function *)int_set_object_failed},
  { "arity", (attr_get_function *)_wrap_astnode_get_arity, (attr_set_function *)_wrap_astnode_set_arity,(attr_get_object_function *)int_get_object_failed,(attr_set_object_function *)int_set_object_failed },
  { "line", (attr_get_function *)_wrap_astnode_get_line, (attr_set_function *)_wrap_astnode_set_line,(attr_get_object_function *)int_get_object_failed,(attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL, NULL  },
};

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab astnode_func[]={
  { "astnode_create", int_astnode_create},
  { NULL, NULL}
};

/* call ith function in the astnode interface */

int astnode_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(astnode_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) */

void astnode_Interf_Info(int i, char **fname, function (**f))
{
  *fname = astnode_func[i].name;
  *f = astnode_func[i].fonc;
}
