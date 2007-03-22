/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

#include "nsp/object.h"
#define  Function_Private 
#include "nsp/function.h"
#include "nsp/interf.h"
#include "../interp/Eval.h" /* FIXME: FuncEval */


/**
 * SECTION:function
 * @title: Store information for an internal nsp function.
 * @short_description: An object used to manipulate internal functions.
 * @see_also: 
 *
 * <para>
 * A #NspFunction object is used to manipulate at nsp level internal functions.
 * </para>
 **/

/* 
 * NspFunction inherits from NspObject
 */

int nsp_type_function_id=0;
NspTypeFunction *nsp_type_function=NULL;

/*
 * Type object for Function 
 * all the instance of NspTypeFunction share the same id. 
 * nsp_type_function: is an instance of NspTypeFunction 
 *    used for objects of NspFunction type (i.e built with new_function) 
 * other instances are used for derived classes 
 */

NspTypeFunction *new_type_function(type_mode mode)
{
  NspTypeFunction *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_function != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_function;
    }
  
  if ((type =  malloc(sizeof(NspTypeFunction))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL ; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = function_get_methods; 
  type->new = (new_func *) new_function;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for function */ 
  
  top->pr = (print_func *) function_print;                    
  top->dealloc = (dealloc_func *) function_destroy;
  top->copy  =  (copy_func *) function_copy;                   
  top->size  = (size_func *) function_size;                  
  top->s_type =  (s_type_func *) function_type_as_string;    
  top->sh_type = (sh_type_func *) function_type_short_string;
  top->info = (info_func *) function_info ;                    
  /* top->is_true = (is_true_func  *) FunctionIsTrue; */
  /* top->loop =(loop_func *) function_loop;*/
  top->path_extract = (path_func *)  object_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) function_object;
  top->eq  = (eq_func *) function_eq;
  top->neq  = (eq_func *) function_neq;
  top->save  = (save_func *) function_xdr_save;
  top->load  = (load_func *) function_xdr_load;
  /* top->create = (create_func*) int_f_create; */

  /* specific methods for function */
      
  type->init = (init_func *) init_function;
      
  /* 
   * Function interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_function_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeFunction called nsp_type_function
       */
      type->id =  nsp_type_function_id = nsp_new_type_id();
      nsp_type_function = type;
      if ( nsp_register_type(nsp_type_function) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_function(mode);
    }
  else 
    {
      type->id = nsp_type_function_id;
      return type;
    }
}

/*
 * initialize Function instances 
 * locally and by calling initializer on parent class 
 */

static int init_function(NspFunction *o,NspTypeFunction *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* FIXME : specific */
  return OK;
}

/*
 * new instance of Function 
 */

NspFunction *new_function() 
{
  NspFunction *loc; 
  /* type must exists */
  nsp_type_function = new_type_function(T_BASE);
  if ( (loc = malloc(sizeof(NspFunction)))== NULLFUNC) return loc;
  /* initialize object */
  if ( init_function(loc,nsp_type_function) == FAIL) return NULLFUNC;
  /* specific */
  loc->iface = loc->pos = loc->status= -1;
  loc->fname = NULL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Function 
 *-----------------------------------------------*/

/*
 * size 
 */

static int function_size(NspFunction *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char function_type_name[]="Func";
static char function_short_type_name[]="f";

static char *function_type_as_string(void)
{
  return(function_type_name);
}

static char *function_type_short_string(NspObject *v)
{
  return(function_short_type_name);
}

static int function_full_comp(NspFunction * A,NspFunction * B,char *op,int *err)
{
  Scierror("function_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int function_eq(NspFunction *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_function_id) == FALSE) return FALSE ;
  rep = function_full_comp(A,(NspFunction *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int function_neq(NspFunction *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_function_id) == FALSE) return TRUE;
  rep = function_full_comp(A,(NspFunction *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int function_xdr_save(XDR *xdrs, NspFunction *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("function_xdr_save: to be implemented, cannot save %s\n",
	   NSP_OBJECT(M)->name);
  return FAIL;
}

/*
 * load 
 */

static NspFunction  *function_xdr_load(XDR *xdrs)
{
  NspFunction *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLFUNC;
  Scierror("function_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void function_destroy(NspFunction *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  FREE(H);
}

/*
 * info 
 */

void function_info(NspFunction *H, int indent,const char *name,int rec_level)
{
  const char *pname;
  if (H == NULLFUNC) 
    {
      Sciprintf1(indent,"Null Pointer SciFile \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(H)->name;
  Sciprintf1(indent,"%s\t= %s\t\t%s\n",
	     (pname==NULL) ? "" : pname,
	     (H->fname==NULL) ? "" : H->fname,
	     function_type_short_string(NSP_OBJECT(H)));
}

/*
 * print 
 */

void function_print(NspFunction *H, int indent,const char *name, int rec_level)
{
  const char *pname;
  if ( H == NULLFUNC) 
    {
      Sciprintf1(indent,"Null Pointer Function \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(H)->name;
  if (user_pref.pr_as_read_syntax)
    {
      Sciprintf1(indent,"%s\t= %s;\n",
		 (pname==NULL) ? "" : pname,
		 (H->fname==NULL) ? "" : H->fname);
    }
  else 
    {
      function_info(H,indent,name,rec_level);
    }
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Function objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspFunction   *function_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast(O,nsp_type_function_id) == TRUE) return ((NspFunction *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_function));
  return(NULL);
}

int IsFunctionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_function_id);
}

int IsFunction(NspObject *O)
{
  return nsp_object_type(O,nsp_type_function_id);
}

NspFunction  *GetFunctionCopy(Stack stack, int i)
{
  if (  GetFunction(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspFunction  *GetFunction(Stack stack, int i)
{
  NspFunction *M;
  if (( M = function_object(NthObj(i))) == NULLFUNC)
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspFunction instance 
 *-----------------------------------------------------*/

NspFunction *function_create(char *name,char *fname,int iface,int pos,int status,NspTypeBase *type)
{
  NspFunction *H  = (type == NULL) ? new_function() : type->new();
  if ( H ==  NULLFUNC)
    {
      Sciprintf("No more memory\n");
      return NULLFUNC;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return(NULLFUNC);
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->iface = iface;
  H->pos = pos;
  H->status = status;
  if ((H->fname =new_nsp_string(fname))== NULLSTRING) return NULLFUNC;
  return H;
}

/*
 * copy 
 */

NspFunction *function_copy(NspFunction *H)
{
  return function_create(NVOID,H->fname,H->iface,H->pos,H->status,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the Function
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/* static AttrTab function_attrs[] = NULL; */

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods *function_get_methods(void) { return NULL;}


/*-------------------------------------------
 * function 
 *-------------------------------------------*/

/*
 * f(....)
 * we must perform a function call
 */

static int int_func_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  CheckStdRhs (1, 1000);
  NspFunction  *F;
  if ( (F = GetFunction(stack,1)) == NULLFUNC) return RET_BUG;
  /* since we want name mangling we just use the function name */
  if (nsp_eval_func(NULL,F->fname,2,stack,stack.first+1,rhs-1,opt,1)== RET_BUG ) 
    return RET_BUG;
  NthObj(2)->ret_pos = 1;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Function_func[]={
  {"extractelts_f", int_func_extractelts},
  {(char *) 0, NULL}
};

/* call ith function in the Function interface */

int Function_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Function_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) */

void Function_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Function_func[i].name;
  *f = Function_func[i].fonc;
}

