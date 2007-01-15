/* Nsp
 * Copyright (C) 1998-2006 Jean-Philippe Chancelier Enpc/Cermics
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
 * 
 *--------------------------------------------------------------------------*/

#include <nsp/object.h>
#include <gtk/gtk.h>

#define  ScalExp_Private 
#include "nsp/object.h"
#include "scalexp.h"
#include "nsp/interf.h"
#include "nsp/plistc.h"
#include "nsp/plisttoken.h" /*for name_maxl **/
#include "nsp/stack.h" 
#include "nsp/parse.h" 
#include "../objects/frame.h" /* XXX */
#include "Functions.h" 
#include "nsp/gsort-p.h" 

static int nsp_scalexp_byte_comp(NspScalExp *Se,int check);
static NspSMatrix *nsp_expr_get_vars(PList L1,NspSMatrix *extra_names);
static int nsp_expr_check(PList L1);
static int nsp_expr_count_logical(PList L1);
static int nsp_expr_action_arg(PList *L,void *context,int action);
static int nsp_eval_expr_context(PList L1,NspHash *context);
static int nsp_eval_expr(PList L1,NspFrame *Fr,double *val,const double *var_table);
static int nsp_bytecomp_expr(PList L1,NspFrame *Fr,int *code,int *pos,double *constv,int *posv);
static int nsp_scalarexp_byte_eval(const int *code,int lcode,const double *constv,const double *vars, double *res);

/* 
 * NspScalExp inherits from NspObject 
 */

int nsp_type_scalexp_id=0;
NspTypeScalExp *nsp_type_scalexp=NULL;

/*
 * Type object for ScalExp 
 * all the instance of NspTypeScalExp share the same id. 
 * nsp_type_scalexp: is an instance of NspTypeScalExp 
 *    used for objects of NspScalExp type (i.e built with new_scalexp) 
 * other instances are used for derived classes 
 */
NspTypeScalExp *new_type_scalexp(type_mode mode)
{
  NspTypeScalExp *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_scalexp != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_scalexp;
    }
  if ((type =  malloc(sizeof(NspTypeScalExp))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = scalexp_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = scalexp_get_methods; 
  type->new = (new_func *) new_scalexp;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for scalexp */ 

  top->pr = (print_func *) nsp_scalexp_print;                  
  top->dealloc = (dealloc_func *) nsp_scalexp_destroy;
  top->copy  =  (copy_func *) nsp_scalexp_copy;                 
  top->size  = (size_func *) nsp_scalexp_size;                
  top->s_type =  (s_type_func *) nsp_scalexp_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_scalexp_type_short_string;
  top->info = (info_func *) nsp_scalexp_info ;                  
  /* top->is_true = (is_true_func  *) nsp_scalexp_is_true; */
  /* top->loop =(loop_func *) nsp_scalexp_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_scalexp_object;
  top->eq  = (eq_func *) nsp_scalexp_eq;
  top->neq  = (eq_func *) nsp_scalexp_neq;
  top->save  = (save_func *) nsp_scalexp_xdr_save;
  top->load  = (load_func *) nsp_scalexp_xdr_load;
  top->create = (create_func*) int_scalexp_create;
  
  /* specific methods for scalexp */
      
  type->init = (init_func *) init_scalexp;

  /* 
   * ScalExp interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_scalexp_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeScalExp called nsp_type_scalexp
       */
      type->id =  nsp_type_scalexp_id = nsp_new_type_id();
      nsp_type_scalexp = type;
      if ( nsp_register_type(nsp_type_scalexp) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_scalexp(mode);
    }
  else 
    {
      type->id = nsp_type_scalexp_id;
      return type;
    }
}

/*
 * initialize ScalExp instances 
 * locally and by calling initializer on parent class 
 */

static int init_scalexp(NspScalExp *o,NspTypeScalExp *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of ScalExp 
 */

NspScalExp *new_scalexp() 
{
  NspScalExp *loc; 
  /* type must exists */
  nsp_type_scalexp = new_type_scalexp(T_BASE);
  if ( (loc = malloc(sizeof(NspScalExp)))== NULLSCALEXP) return loc;
  /* initialize object */
  if ( init_scalexp(loc,nsp_type_scalexp) == FAIL) return NULLSCALEXP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for ScalExp 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_scalexp_size(NspScalExp *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char scalexp_type_name[]="ScalExp";
static char scalexp_short_type_name[]="scalexp";

static char *nsp_scalexp_type_as_string(void)
{
  return(scalexp_type_name);
}

static char *nsp_scalexp_type_short_string(void)
{
  return(scalexp_short_type_name);
}

/*
 * A == B 
 */

static int nsp_scalexp_eq(NspScalExp *A, NspObject *B)
{
  NspScalExp *loc = (NspScalExp *) B;
  if ( check_cast(B,nsp_type_scalexp_id) == FALSE) return FALSE ;
  if ( A->code != loc->code ) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_scalexp_neq(NspScalExp *A, NspObject *B)
{
  return ( nsp_scalexp_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_scalexp_xdr_save(XDR *xdrs, NspScalExp *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  /* 
  PList code;
  NspSMatrix *expr;
  NspSMatrix *vars;
  NspSMatrix *extra_vars;
  NspMatrix *bcode;
  NspMatrix *values;
  */
  return FAIL;
}

/*
 * load 
 */

static NspScalExp  *nsp_scalexp_xdr_load(XDR *xdrs)
{
  NspScalExp *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSCALEXP;
  if ((M  = scalexp_create_void(name,(NspTypeBase *) nsp_type_scalexp))== NULLSCALEXP) return M;
  return NULLSCALEXP;
}

/*
 * delete 
 */

void nsp_scalexp_destroy(NspScalExp *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_smatrix_destroy(H->expr);
  nsp_plist_destroy(&H->code);
  FREE(H);
}

/*
 * info 
 */

void nsp_scalexp_info(NspScalExp *M, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t= \t\t%s ()\n",
	    pname,
	    nsp_scalexp_type_short_string());
}

/*
 * print 
 */

void nsp_scalexp_print(NspScalExp *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=XXXXXXXscalexp_create()\n",pname);
	}
      else 
	{
	  Sciprintf1(indent,"XXXXXXXscalexp_create()\n");
	}
    }
  else 
    {
      /* gerer le rec_level */
      Sciprintf1(indent,"%s=\t\tscalexp\n",pname);
      nsp_object_print((NspObject *)M->expr,indent+2,NULL,rec_level+1);      
      /* nsp_plist_pretty_print(M->code, indent+2); 
	 Sciprintf("\n"); */
    }
}



/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ScalExp objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspScalExp   *nsp_scalexp_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_scalexp_id) == TRUE ) return ((NspScalExp *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_scalexp));
  return NULL;
}

int IsScalExpObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_scalexp_id);
}

int IsScalExp(NspObject *O)
{
  return nsp_object_type(O,nsp_type_scalexp_id);
}

NspScalExp  *GetScalExpCopy(Stack stack, int i)
{
  if (  GetScalExp(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspScalExp  *GetScalExp(Stack stack, int i)
{
  NspScalExp *M;
  if (( M = nsp_scalexp_object(NthObj(i))) == NULLSCALEXP)
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspScalExp *scalexp_create_void(char *name,NspTypeBase *type)
{
  NspScalExp *H  = (type == NULL) ? new_scalexp() : type->new();
  if ( H ==  NULLSCALEXP)
    {
      Sciprintf("No more memory\n");
      return NULLSCALEXP;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return NULLSCALEXP;
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->code = NULL;
  H->bcode = NULL;
  H->values = NULL;
  H->vars=NULL;
  H->extra_vars=NULL;
  return H;
}

NspScalExp *scalexp_create(char *name,NspSMatrix *expr,NspTypeBase *type)
{
  NspScalExp *H  = scalexp_create_void(name,type);
  if ( H ==  NULLSCALEXP) return NULLSCALEXP;
  if ((H->expr = (NspSMatrix *) nsp_object_copy_and_name("expr",(NspObject *) expr)) == NULL) return NULL;
  if ((H->code = nsp_parse_expr(expr))== NULL) return NULL;
  if ( nsp_expr_check(H->code) == FAIL) return NULL;
  if ((H->vars =nsp_expr_get_vars(H->code,NULL))==NULL) return NULL;
  return H;
}

/*
 * copy for gobject derived class  
 */

NspScalExp *nsp_scalexp_copy(NspScalExp *self)
{
  NspScalExp *H  =scalexp_create(NVOID,self->expr,NULL);
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the ScalExp
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_scalexp_create(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *expr;
  NspScalExp *H;
  CheckStdRhs(1,1);
  /* want to be sure that type scalexp is initialized */
  if ((expr = GetSMat(stack,1))== NULL) 
    return RET_BUG;
  nsp_type_scalexp = new_type_scalexp(T_BASE);
  if(( H = scalexp_create(NVOID,expr,(NspTypeBase *) nsp_type_scalexp)) == NULLSCALEXP) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/* methods 
 *
 */

/**
 * nsp_scalarexp_byte_eval:
 * @code: 
 * @lcode: 
 * @constv: 
 * @vars: 
 * @res: 
 * 
 * 
 * 
 * Return value: 
 **/

static int int_scalexp_meth_eval(NspScalExp *self, Stack stack, int rhs, int opt, int lhs)
{
  int ok=FALSE,i,nres;
  NspMatrix *M,*Res;
  CheckRhs(1,1);
  if (( M= GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( M->m == self->vars->mn ) 
    {
      ok = TRUE; 
      nres = M->n; 
    }
  else if ( M->m == 1 && M->n == self->vars->mn )
    {
      ok = TRUE;
      nres =1;
    }
  if ( ok == FALSE ) 
    {
      Scierror("Error: argument should be of length 1x%d or %dxn\n",self->vars->mn,self->vars->mn);
      return RET_BUG;
    }
  /* note that nres can be null */
  if ((Res = nsp_matrix_create(NVOID,'r',1,nres))==NULLMAT) 
    return RET_BUG;
  for ( i = 0 ; i < nres ; i++) 
    nsp_eval_expr(self->code,NULL,&Res->R[i],M->R + M->m*i);
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

/**
 * int_scalexp_meth_bcomp:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * 
 * 
 * Return value: 
 **/

static int int_scalexp_meth_bcomp(NspScalExp *self, Stack stack, int rhs, int opt, int lhs)
{
  if ( nsp_scalexp_byte_comp(self,FALSE) == FAIL ) return RET_BUG;
  return 0;
}

/**
 * int_scalexp_meth_get_bcode:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * 
 * 
 * Return value: 
 **/

static int int_scalexp_meth_get_bcode(NspScalExp *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckLhs(0,2);
  CheckRhs(0,0);
  if ( self->bcode == NULLMAT ) 
    {
      Scierror("Error: please call bcomp method first\n");
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(self->bcode));
  if ( lhs == 2)
    MoveObj(stack,2,NSP_OBJECT(self->values));
  return Max(lhs,1);
}

/**
 * int_scalexp_meth_byte_eval:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * method: evaluates expression @self.
 * 
 * Return value: 
 **/

static int int_scalexp_meth_byte_eval(NspScalExp *self, Stack stack, int rhs, int opt, int lhs)
{
  int ok=FALSE,i,nres;
  NspMatrix *M,*Res;
  CheckRhs(1,1);
  if ( self->bcode == NULLMAT ) 
    {
      Scierror("Error: please call bcomp method first\n");
      return RET_BUG;
    }
  if (( M= GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( M->m == self->vars->mn ) 
    {
      ok = TRUE; 
      nres = M->n; 
    }
  else if ( M->m == 1 && M->n == self->vars->mn )
    {
      ok = TRUE;
      nres =1;
    }
  if ( ok == FALSE ) 
    {
      Scierror("Error: argument should be of length 1x%d or %dxn\n",self->vars->mn,self->vars->mn);
      return RET_BUG;
    }
  /* note that nres can be null */
  if ((Res = nsp_matrix_create(NVOID,'r',1,nres))==NULLMAT) 
    return RET_BUG;
  for (i=0; i < nres ; i++) 
    nsp_scalarexp_byte_eval(self->bcode->I,self->bcode->mn,self->values->R,M->R + M->m*i,&Res->R[i]);
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

/**
 * int_scalexp_meth_apply_context:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * method: apply nsp_eval_expr_context()
 * 
 * Return value: 
 **/

static int int_scalexp_meth_apply_context(NspScalExp *self,Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  NspSMatrix *S;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((H = GetHash(stack,1)) == NULLHASH) return RET_BUG;
  nsp_eval_expr_context(self->code,H);
  /* update vars if changed this should be returned by previous function */
  if ((S = nsp_expr_get_vars(self->code,self->extra_vars))==NULL) return RET_BUG;
  nsp_smatrix_destroy(self->vars);
  self->vars = S;
  /* need to byte_compile again */
  if ( self->bcode != NULL) 
    {
      if ( nsp_scalexp_byte_comp(self,FALSE)== FAIL)
	{
	  Scierror("Failed to byte compile expression\n");
	  return RET_BUG;
	}
    }
  return 0;
}

/**
 * int_scalexp_meth_get_vars:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * method: returns the number of free variables contained in expression @self
 * 
 * Return value: 1
 **/

static int int_scalexp_meth_get_vars(NspScalExp *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  MoveObj(stack,1,NSP_OBJECT(self->vars));
  return 1;
}


/**
 * int_scalexp_meth_print_code:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * method:prints internal code (debug use).
 * 
 * Return value: 
 **/
static int int_scalexp_meth_print_code(NspScalExp *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,0);
  nsp_plist_print_internal(self->code);
  return 0;
}

/**
 * int_scalexp_meth_nlogicals:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * method: used by scicos to know how many zero crossing are contained in the 
 * expression @self
 * 
 * Return value: 1 
 **/

static int int_scalexp_meth_nlogicals(NspScalExp *self,Stack stack, int rhs, int opt, int lhs)
{
  int nlogical;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  nlogical = nsp_expr_count_logical(self->code);
  nsp_move_double(stack,1,nlogical);
  return 1;
}

/**
 * int_scalexp_meth_reset_context:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * method: variables which were binded to fixed values are reset. The set of variables is updated 
 * and the expression @self is re-compiled
 * 
 * Return value: 0 or %RET_BUG
 **/

static int int_scalexp_meth_reset_context(NspScalExp *self,Stack stack, int rhs, int opt, int lhs)
{
  PList pcode;
  NspSMatrix *vars;
  CheckRhs(0,0);
  CheckLhs(-1,0);
  /* ----------- */
  if ((pcode = nsp_parse_expr(self->expr))== NULL) return RET_BUG;
  if ( nsp_expr_check(pcode) == FAIL) return RET_BUG;
  nsp_plist_destroy(&self->code);
  self->code = pcode ;
  if ((vars =nsp_expr_get_vars(self->code,self->extra_vars))==NULL) return RET_BUG;
  nsp_smatrix_destroy(self->vars);
  self->vars = vars;
  if ( self->bcode != NULL) 
    {
      if ( nsp_scalexp_byte_comp(self,FALSE)== FAIL)
	{
	  Scierror("Failed to byte compile expression\n");
	  return RET_BUG;
	}
    }
  return 0;
}

/**
 * int_scalexp_meth_set_extra_names:
 * @self: 
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * method: gives a set of variable names which are to be added to the set of 
 * variables of scalar expression @self. The set of variables is updated 
 * and the expression @self is re-compiled. 
 * 
 * Return value: 0 or %RET_BUG
 **/

static int int_scalexp_meth_set_extra_names(NspScalExp *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *vars;
  NspSMatrix *extra_vars;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  /* ----------- */
  if ((extra_vars = GetSMat(stack,1))== NULL)    return RET_BUG;
  if ((self->extra_vars = (NspSMatrix *) nsp_object_copy_and_name("extra_vars",(NspObject *) extra_vars))
      == NULL) return RET_BUG;
  if ((vars =nsp_expr_get_vars(self->code,self->extra_vars))==NULL) return RET_BUG;
  nsp_smatrix_destroy(self->vars);
  self->vars = vars;
  if ( self->bcode != NULL) 
    {
      if ( nsp_scalexp_byte_comp(self,FALSE)== FAIL)
	{
	  Scierror("Failed to byte compile expression\n");
	  return RET_BUG;
	}
    }
  return 0;
}


static NspMethods scalexp_methods[] = {
  {"eval",(nsp_method *) int_scalexp_meth_eval},
  {"bcomp",(nsp_method *) int_scalexp_meth_bcomp},
  {"byte_eval",(nsp_method *) int_scalexp_meth_byte_eval},
  {"apply_context",(nsp_method *) int_scalexp_meth_apply_context},
  {"reset_context",(nsp_method *) int_scalexp_meth_reset_context},
  {"get_vars",(nsp_method *) int_scalexp_meth_get_vars},
  {"print_code",(nsp_method *) int_scalexp_meth_print_code},
  {"logicals",(nsp_method *)  int_scalexp_meth_nlogicals},
  {"get_bcode",(nsp_method *) int_scalexp_meth_get_bcode},
  {"set_extra_names",(nsp_method *) int_scalexp_meth_set_extra_names},
  { NULL, NULL}
};

static NspMethods *scalexp_get_methods(void) { return scalexp_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab scalexp_attrs[] = {
  { NULL,NULL,NULL,NULL },
};

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/




/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab scalexp_func[]={
  { "scalexp_create", int_scalexp_create},
  { NULL, NULL}
};

/* call ith function in the scalexp interface */

int scalexp_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(scalexp_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) */

void scalexp_Interf_Info(int i, char **fname, function (**f))
{
  *fname = scalexp_func[i].name;
  *f = scalexp_func[i].fonc;
}


/*-------------------------------------------------------------------------
 *  set of functions 
 *--------------------------------------------------------------------------*/

/**
 * nsp_scalexp_byte_comp:
 * @Se: a #NspScalExp object 
 * @check: %TRUE or %FALSE 
 * 
 * byte compile the expression associated to @Se. If @check is %TRUE 
 * the correctness of expression is first checked. 
 * 
 * Return value: %OK or %FAIL 
 **/

static int nsp_scalexp_byte_comp(NspScalExp *Se,int check)
{
  int code[512],pos=0,posv=0;
  double dval[512];
  NspMatrix *bcode;
  NspMatrix *values;
  /* should we make a check first ? */
  if ( check == TRUE && nsp_expr_check(Se->code) == FAIL) return FAIL;
  nsp_bytecomp_expr(Se->code,NULL,code,&pos,dval,&posv);
  /* Sciprintf("taille du code %d et taille des var %d\n",pos,posv); */
  if ((values=nsp_matrix_create_from_array("Const",1,posv,dval,NULL)) == NULL) 
    return FAIL;
  if ((bcode=nsp_matrix_create("code",'r',1,pos)) == NULL)
    return FAIL;
  memcpy(bcode->I,code,pos*sizeof(int));
  bcode->convert = 'i';
  /* store the byte code */
  if ( Se->bcode != NULL) nsp_matrix_destroy(Se->bcode);
  Se->bcode = bcode;
  /* store the constant values used in the byte code */
  if ( Se->values != NULL)  nsp_matrix_destroy(Se->values);
  Se->values= values;
  return OK;
}



static int nsp_eval_expr_arg(PList L,NspFrame *Fr,double *val,const double *var_table);

#if WIN32
double asinh(double x) { return log(x+sqrt(x*x+1));}
double acosh(double x) { return log(x+sqrt(x*x-1));}
double atanh(double x) { 
  return  (x >=0.5) ? 0.5*log((1+x)/(1-x)) : 0.5*log((2*x)+(2*x)*x/(1-x));
}
#endif

extern double round(double);
static double sign(double x) { return (x>0) ? 1: ((x==0) ? 0:-1);}
static double dmax(double x,double y) { return Max(x,y);}
static double dmin(double x,double y) { return Min(x,y);}
static double dabs(double x) { return Abs(x);}
static double dgamma(double x) {
#ifdef HAVE_TGAMMA
  return tgamma(x);
#else 
  return cdf_gamma(&x);
#endif 
}

expr_func expr_functions[] = 
  {
    {"sin",f_sin,sin,NULL,FALSE},
    {"cos",f_cos,cos,NULL,FALSE},
    {"tan",f_tan,tan,NULL,FALSE},
    {"exp",f_exp,exp,NULL,FALSE},
    {"log",f_log,log,NULL,FALSE},
    {"sinh",f_sinh,sinh,NULL,FALSE},
    {"cosh",f_cosh,cosh,NULL,FALSE},
    {"tanh",f_tanh,tanh,NULL,FALSE},
    {"int",f_int,rint,NULL,TRUE},
    {"round",f_round,round,NULL,TRUE},
    {"ceil",f_ceil,ceil,NULL,TRUE},
    {"floor",f_floor,floor,NULL,TRUE},
    {"sign",f_sign,sign,NULL,TRUE},
    {"abs",f_abs,dabs,NULL,TRUE},
    {"max",f_max,NULL,dmax,TRUE},
    {"min",f_min,NULL,dmin,TRUE},
    {"asin",f_asin,asin,NULL,FALSE},
    {"acos",f_acos,acos,NULL,FALSE},
    {"atan",f_atan,atan,NULL,FALSE},
    {"asinh",f_asinh,asinh,NULL,FALSE},
    {"acosh",f_acosh,acosh,NULL,FALSE},
    {"atanh",f_atanh,atanh,NULL,FALSE},
    {"atan2",f_atan2,NULL,atan2,FALSE},
    {"log10",f_log10,log10,NULL,FALSE},
    {"gamma",f_gamma,dgamma,NULL,FALSE},
    {NULL,0}
  };


/**
 * nsp_eval_expr:
 * @L1: 
 * @Fr: 
 * @val: 
 * @var_table: 
 * 
 * evaluates a scalarexp
 * 
 * Return value: 
 **/

int nsp_eval_expr(PList L1,NspFrame *Fr,double *val,const double *var_table)
{
  int nargs=-1;
  PList L,loc;
  int j;
  L = L1; /* operator */
  L1= L->next ; /* first arg */
  if ( L->type > 0  ) 
    {
      double args[2];
      const char *opcode ;
      /* Evaluation of operators */
      loc = L1;
      for ( j = 0 ; j < L->arity  ; j++ )
	{
	  if ( nsp_eval_expr_arg(loc,Fr,args+j,var_table) < 0 ) return RET_BUG;
	  loc = loc->next ;
	}
      switch ( L->type ) 
	{
	case TILDE_OP: *val = (args[0]==0) ? 1 : 0;break;
	case DOTPRIM :
	case QUOTE_OP :	*val= args[0] ;break;
	case DOTSTARDOT:
	case DOTSTAR :
	case STAR_OP : 	*val= args[0]*args[1];break;			      
	case DOTPLUS: 
	case PLUS_OP : 	*val= args[0]+args[1];break;			      
	case HAT_OP : 	*val= pow(args[0],args[1]);break;			      
	case SEQOR : 	
	case OR_OP : 	*val= ((int) args[0]) || ((int) args[1]) ;break;			      
	case SEQAND   : 
	case AND_OP : 	*val= ((int) args[0]) && ((int) args[1]) ;break;
	case COMMA_OP : *val= args[0];break;
	case SEMICOLON_OP :*val= args[0];break;
	case RETURN_OP :*val= args[0];break;
	case MINUS_OP : *val= (L->arity == 1) ? -args[0] : args[0]-args[1];break;
	case DOTSLASH:
	case DOTSLASHDOT:
	case SLASH_OP : *val= args[0]/args[1];break;
	case DOTBSLASH :
	case DOTBSLASHDOT: 
	case BACKSLASH_OP: *val= args[1]/args[0];break;
	case DOTHAT : 	*val= pow(args[0],args[1]);break;
	case DOTEQ :
	case EQ     : *val= args[0]== args[1];break;
	case DOTLEQ:
	case LEQ    : *val= args[0] <= args[1];break;
	case DOTGEQ :
	case GEQ    :  *val= args[0] >= args[1];break;
	case DOTNEQ :
	case NEQ    :  *val= args[0] != args[1];break;
	case MOINS   : 	*val=-args[0] ;break;   /* unary minus */	      
	case DOTLT :
	case LT_OP: *val= args[0] < args[1];break;
	case DOTGT:
	case GT_OP: *val= args[0] > args[1];break;	
	default: 
	  opcode =nsp_astcode_to_nickname(L->type);
	  Sciprintf("Need to emit %s with arity %d, unknown op\n",opcode,L->arity);
	  return RET_BUG;
	}
      /* opcode =nsp_astcode_to_nickname(L->type);
       * Sciprintf("Need to emit %s with arity %d -> %f\n",opcode,L->arity,*val);*/
      return 1;
    }
  else 
    {
      switch ( L->type ) 
	{
	case CALLEVAL : 
	  /* Sciprintf("Need to perform a CALLEVAL \n"); */
	  {
	    int k,n,nargs;
	    double args[2];
	    char *name;
	    PList Largs,Lf;
	    /* we know here that arity is 2 when entering Eval RhsCall 
	     * L == (CALLEVAL fname (ARGS ....)) 
	     */
	    Lf = L->next;
	    name = Lf->O;
	    Largs = Lf->next->O;
	    nargs = Largs->arity;
	    Largs = Largs->next;/* point to first element of ARGS */
	    if ( nargs > 2 ) return RET_BUG;
	    for ( k= 1; k <= nargs ; k++) 
	      {
		if ( nsp_eval_expr_arg(Largs,Fr,args+(k-1),var_table) < 0) return RET_BUG;
		Largs = Largs->next;
	      }
	    if ( Lf->arity == -1 ) 
	      {
		/* Sciprintf("Try to detect %s on first call\n",name); */
		if ((n = is_string_in_struct(name,(void **)expr_functions ,sizeof(expr_func),1)) >=0)
		  {
		    Lf->arity = n;
		  }
		else 
		  {
		    Sciprintf("Must emit a call to %s which is not correct\n",name);
		    return RET_BUG;
		  }
	      }
	    n= Lf->arity;
	    /* Sciprintf("Must emit a call to %s (%d) on %d arguments\n",name,n,count); */
	    /* store n for next calls */
	    if ( expr_functions[n].f1 != NULL) 
	      {
		*val = (expr_functions[n].f1)(args[0]);
		/* Sciprintf("%s[id=%d](%f)->%f\n",name,n,args[0],*val);*/
	      }
	    else 
	      {
		*val = (expr_functions[n].f2)(args[0],args[1]);
		/* Sciprintf("%s[id=%d](%f)->%f\n",name,n,args[0],args[1],*val);*/
	      }
	  }
	  return 1;
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    {
	      if ((nargs=nsp_eval_expr_arg(L,Fr,val,var_table)) < 0) 
		/* SHOWBUG(stack,nargs,L1); */
	      return nargs;
	    }
	  return 0;
	  break;
	case STATEMENTS :
	case STATEMENTS1 :
	  /*ici lhs n'est pas utilise XXX **/
	  nargs = 0;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( (nargs=nsp_eval_expr_arg(L1,Fr,val,var_table)) < 0) 
		{
		  return nargs;
		}
	      L1 = L1->next;
	    }
	  return 0;
	case PARENTH: 
	  nargs = 0;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( (nargs=nsp_eval_expr_arg(L1,Fr,val,var_table)) < 0) 
		{
		  return nargs;
		}
	      L1 = L1->next;
	    }
	  return 0;
	default:
	  return RET_BUG;
	}
    }
  return nargs ;
}

static int nsp_eval_expr_arg(PList L,NspFrame *Fr,double *val,const double *var_table)
{
  switch (L->type) 
    {
    case NAME :
    case OPNAME :
      /* Sciprintf("Need to evaluate a name or opname %s (id=%d) %f\n",
       *           (char *) L->O,L->arity, var_table[L->arity-1]);
       */
       *val =var_table[L->arity-1] ;
      return 1;
    case NUMBER:
      /* Sciprintf("Need to evaluate a number %s %f\n",((parse_double *) L->O)->str,((parse_double *) L->O)->val); */
      *val = ((parse_double *) L->O)->val;
      return 1;
      break;
    case PLIST :
      return nsp_eval_expr((PList) L->O,Fr,val,var_table);
      break;
    default: 
      return RET_BUG;
    }
  return RET_BUG;
}


/**
 * nsp_bytecomp_expr_arg:
 * @L: 
 * @Fr: 
 * @code: 
 * @pos: 
 * @constv: 
 * @posv: 
 * 
 * bye compile a scalar expression.
 * 
 * Return value: %OK or %FAIL
 **/

static int nsp_bytecomp_expr_arg(PList L,NspFrame *Fr,int *code,int *pos,double *constv,int *posv);

int nsp_bytecomp_expr(PList L1,NspFrame *Fr,int *code, int *pos,double *constv,int *posv)
{
  PList L,loc;
  int j;
  L = L1; /* operator */
  L1= L->next ; /* first arg */
  if ( L->type > 0  ) 
    {
      const char *opcode ;
      /* Evaluation of operators */
      loc = L1;
      for ( j = 0 ; j < L->arity  ; j++ )
	{
	  if ( nsp_bytecomp_expr_arg(loc,Fr,code,pos,constv,posv) == FAIL ) return FAIL;
	  loc = loc->next ;
	}
      if ( L->type == MINUS_OP && L->arity == 1) L->type = MOINS; /* to be inserted in parser */
      code[*pos] = ( 1 << 16 ) | L->type ; *pos += 1;
      opcode =nsp_astcode_to_nickname(L->type);
      /* Sciprintf("Need to emit %s (id %d) with arity %d and code %d \n",opcode,L->type,code[*pos-1],code[*pos-2]); */
      return OK;
    }
  else 
    {
      switch ( L->type ) 
	{
	case CALLEVAL : 
	  /* Sciprintf("Need to perform a CALLEVAL \n"); */
	  {
	    int k,n,nargs;
	    char *name;
	    PList Largs,Lf;
	    /* we know here that arity is 2 when entering Ecode RhsCall 
	     * L == (CALLEVAL fname (ARGS ....)) 
	     */
	    Lf = L->next;
	    name = Lf->O;
	    Largs = Lf->next->O;
	    nargs = Largs->arity;
	    Largs = Largs->next;/* point to first element of ARGS */
	    if ( nargs > 2 ) return FAIL;
	    for ( k= 1; k <= nargs ; k++) 
	      {
		if ( nsp_bytecomp_expr_arg(Largs,Fr,code,pos,constv,posv) == FAIL) return FAIL;
		Largs = Largs->next;
	      }
	    if ( Lf->arity == -1 ) 
	      {
		/* Sciprintf("Try to detect %s on first call\n",name); */
		if ((n = is_string_in_struct(name,(void **)expr_functions ,sizeof(expr_func),1)) >=0)
		  {
		    Lf->arity = n;
		  }
		else 
		  {
		    Scierror("Must emit a call to %s which is not correct\n",name);
		    return FAIL;
		  }
	      }
	    n= Lf->arity;
	    code[*pos] = ( 2 << 16 ) + n;*pos += 1;
	    /* Sciprintf("Must emit a call to %s (id %d) which is coded %d\n",name,n,code[*pos-2]); */
	  }
	  return OK;
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    {
	      if (nsp_bytecomp_expr_arg(L,Fr,code,pos,constv,posv) == FAIL ) return FAIL;
	    }
	  return OK;
	  break;
	case STATEMENTS :
	case STATEMENTS1 :
	case PARENTH: 
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( nsp_bytecomp_expr_arg(L1,Fr,code,pos,constv,posv) == FAIL) return FAIL;
	      L1 = L1->next;
	    }
	  return OK;
	default:
	  return FAIL;
	}
    }
  return FAIL;
}

static int nsp_bytecomp_expr_arg(PList L,NspFrame *Fr,int *code,int *pos,double *constv,int *posv)
{
  switch (L->type) 
    {
    case NAME :
    case OPNAME :
      /* Sciprintf("Need  a name or opname %s (id=%d)\n",(char *) L->O,L->arity); */
      code[*pos] =( 3 << 16 ) | (L->arity-1); *pos += 1;
      if ( L->arity < 1 ) 
	{
	  Scierror("Error: variable %s not correctly bounded\n",(char *) L->O);
	  return FAIL;
	}
      return OK ;
    case NUMBER:
      /* Sciprintf("Need  a number %s %f\n",((parse_double *) L->O)->str,((parse_double *) L->O)->val); */
      /* *code++ = ((parse_double *) L->O)->val;  */
      code[*pos] = ( 4 << 16 ) | *posv ; *pos += 1;
      constv[*posv]= ((parse_double *) L->O)->val; *posv +=1;
      return OK ;
      break;
    case PLIST :
      return nsp_bytecomp_expr((PList) L->O,Fr,code,pos,constv,posv);
      break;
    default: 
      return FAIL;
    }
  return FAIL;
}

/**
 * nsp_scalarexp_byte_eval:
 * @code: 
 * @lcode: 
 * @constv: 
 * @vars: 
 * @res: 
 * 
 * evalues byte code. 
 * 
 * Return value: 
 **/

int nsp_scalarexp_byte_eval(const int *code,int lcode,const double *constv,const double *vars, double *res)
{
  unsigned int type;
  int i,s_pos=0,n;
  double stack[512];
  for ( i = 0 ; i < lcode ; i++)
    {
      unsigned int bcode = *code;
      code++;
      type = (bcode & 0xefff0000 ) >> 16;
      switch (type )
	{
	case 1:
	  n = bcode & 0xffff;
	  /* Sciprintf("Need  an operator %d\n",n);*/
	  switch (n) 
	    {
	    case TILDE_OP: stack[s_pos-1] = (stack[s_pos-1] ==0 ) ? 1 : 0;break;
	    case DOTPRIM :
	    case QUOTE_OP : break;
	    case DOTSTARDOT:
	    case DOTSTAR :
	    case STAR_OP :  stack[s_pos-2] *= stack[s_pos-1];s_pos--; break;			      
	    case DOTPLUS: 
	    case PLUS_OP :  stack[s_pos-2] += stack[s_pos-1];s_pos--; break;			      
	    case HAT_OP :   stack[s_pos-2]= pow(stack[s_pos-2],stack[s_pos-1]);s_pos--; break;			      
	    case SEQOR : 	
	    case OR_OP :    stack[s_pos-2]= ((int) stack[s_pos-2]) || ((int) stack[s_pos-1]) ;s_pos--;break;			      
	    case SEQAND   : 
	    case AND_OP : 	stack[s_pos-2]= ((int) stack[s_pos-2]) && ((int) stack[s_pos-1]) ;s_pos--;break;
	    case COMMA_OP : break;
	    case SEMICOLON_OP :break;
	    case RETURN_OP : break;
	    case MINUS_OP :  stack[s_pos-2] -= stack[s_pos-1];s_pos--;break; /* binary */
	    case DOTSLASH:
	    case DOTSLASHDOT:
	    case SLASH_OP : stack[s_pos-2] /= stack[s_pos-1];s_pos--;break;
	    case DOTBSLASH :
	    case DOTBSLASHDOT: 
	    case BACKSLASH_OP: stack[s_pos-2] = stack[s_pos-1]/stack[s_pos-2];s_pos--;break;
	    case DOTHAT : 	stack[s_pos-2]= pow(stack[s_pos-2],stack[s_pos-1]);s_pos--;break;
	    case DOTEQ :
	    case EQ     : stack[s_pos-2]= stack[s_pos-2]== stack[s_pos-1];s_pos--;break;
	    case DOTLEQ:
	    case LEQ    : stack[s_pos-2]= stack[s_pos-2] <= stack[s_pos-1];s_pos--;break;
	    case DOTGEQ :
	    case GEQ    :  stack[s_pos-2]= stack[s_pos-2] >= stack[s_pos-1];s_pos--;break;
	    case DOTNEQ :
	    case NEQ    :  stack[s_pos-2]= stack[s_pos-2] != stack[s_pos-1];s_pos--;break;
	    case MOINS   : 	stack[s_pos-1] =-stack[s_pos-1] ;break;   /* unary minus */	      
	    case DOTLT :
	    case LT_OP: stack[s_pos-2]= stack[s_pos-2] < stack[s_pos-1];s_pos--;break;	
	    case DOTGT:
	    case GT_OP: stack[s_pos-2]= stack[s_pos-2] > stack[s_pos-1];s_pos--;break;	
	  }
	  break;
	case 2:
	  n = bcode & 0xffff;
	  /* Sciprintf("Need  a function %d\n", n); */
	  if ( expr_functions[n].f1 != NULL) 
	    {
	      stack[s_pos-1] = (expr_functions[n].f1)(stack[s_pos-1]);
	    }
	  else 
	    {
	      stack[s_pos-2] = (expr_functions[n].f2)(stack[s_pos-2],stack[s_pos-1]);
	      s_pos--;
	    }
	  break;
	case 3:
	  /* Sciprintf("Need  a name %d\n", bcode & 0xffff); */
	  stack[s_pos]=vars[  bcode & 0xffff];
	  s_pos++;
	  break;
	case 4:
	  /* Sciprintf("A number %f\n",constv[ bcode & 0xffff]); */
	  stack[s_pos]=constv[ bcode & 0xffff]; 
	  s_pos++;
	  break;
	}
    }
  *res = stack[0];
  return OK;
}


typedef enum { get_data, store_name, update_id, check_expr, count_logical } expr_action; 

/**
 * nsp_expr_action:
 * @L1: 
 * @context: 
 * @action: 
 * 
 * utility function for performing actions on scalar expressions 
 * 
 * Return value: %OK or %FAIL
 **/

static int nsp_expr_action(PList L1,void *context,int action )
{
  PList L,loc;
  int j,ans;
  L = L1; /* operator */
  L1= L->next ; /* first arg */
  if ( L->type > 0  ) 
    {
      /* Evaluation of operators */
      loc = L1;
      for ( j = 0 ; j < L->arity  ; j++ )
	{
	  ans=nsp_expr_action_arg(&loc,context,action);
	  if ( action == check_expr && ans == FAIL) return FAIL;
	  loc = loc->next ;
	}
      switch (action) 
	{
	case check_expr:
	  {
	    int code = L->type;
	    if ( ! ( code > NOTCODE_OP && code < LASTCODE_OP ) 
		 ||  code == COLON_OP || code == STARDOT || code == SLASHDOT || code== BSLASHDOT )
	      {
		const char *opcode =nsp_astcode_to_nickname(L->type);
		Sciprintf("Error: Unknown operator %s\n",opcode);
		return FAIL;
	      }
	  }
	  break;
	case count_logical: 
	  /* count logical operators */
	  switch (L->type) 
	    {
	    case TILDE_OP: 
	    case SEQOR : 	
	    case OR_OP : 
	    case SEQAND   : 
	    case AND_OP :
	    case DOTEQ :
	    case EQ     :
	    case DOTLEQ:
	    case LEQ    :
	    case DOTGEQ :
	    case GEQ    :
	    case DOTNEQ :
	    case NEQ    :
	    case DOTLT :
	    case LT_OP:  
	    case DOTGT:
	    case GT_OP:  
		{
		  int *count = context;
		  (*count)++;
		}
		break;
	    }
	  break;
	}
      return OK;
    }
  else 
    {
      switch ( L->type ) 
	{
	case CALLEVAL : 
	  /* Sciprintf("Need to perform a CALLEVAL \n"); */
	  {
	    int k,nargs;
	    char *name;
	    PList Largs,Lf;
	    /* we know here that arity is 2 when entering Eval RhsCall 
	     * L == (CALLEVAL fname (ARGS ....)) 
	     */
	    Lf = L->next;
	    name = Lf->O;
	    Largs = Lf->next->O;
	    nargs = Largs->arity;
	    Largs = Largs->next;/* point to first element of ARGS */
	    if ( nargs > 2 ) 
	      {
		if ( action == check_expr)
		  {
		    Scierror("Error: too many arguments for %s\n",name);
		  }
		return FAIL;
	      }
	    for ( k= 1; k <= nargs ; k++) 
	      {
		ans=nsp_expr_action_arg(&Largs,context,action);
		if ( action == check_expr && ans == FAIL) 
		  {
		    return FAIL;
		  }
		Largs = Largs->next;
	      }
	    switch (action) 
	      {
	      case check_expr: 
		if ((ans = is_string_in_struct(name,(void **)expr_functions ,sizeof(expr_func),1)) < 0) 
		  {
		    Scierror("Error: unknown function %s\n",name);
		    return FAIL;
		  }
		else 
		  {
		    if ( expr_functions[ans].f1 != NULL )
		      {
			if ( nargs != 1 ) 
			  {
			    Scierror("Error: expecting one argument for function %s\n",name);
			    return FAIL;
			  }
		      }
		    else  
		      {
			if ( nargs != 2 ) 
			  {
			    Scierror("Error: expecting two arguments for function %s\n",name);
			    return FAIL;
			  }
		      }
		  }
		break;
	      case count_logical :
		ans = is_string_in_struct(name,(void **)expr_functions ,sizeof(expr_func),1);
		if ( ans >= 0 && expr_functions[ans].logical == TRUE ) 
		  {
		    int *count = context;
		    (*count)++;
		  }
		break;
	      }
	  }
	  return OK;
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    {
	      ans=nsp_expr_action_arg(&L,context,action);
	      if ( action == check_expr && ans == FAIL) return FAIL;
	      return OK;
	    }
	  return OK;
	  break;
	case STATEMENTS :
	case STATEMENTS1 :
	  /*ici lhs n'est pas utilise XXX **/
	  if ( action == check_expr && L->arity > 1 )
	    {
	      Scierror("Error: too many expressions found (%d)\n",L->arity);
	      return FAIL;
	    }
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ans=nsp_expr_action_arg(&L1,context,action);
	      if ( action == check_expr && ans == FAIL) return FAIL;
	      L1 = L1->next;
	    }
	  return OK;
	case PARENTH: 
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ans=nsp_expr_action_arg(&L1,context,action);
	      if ( action == check_expr && ans == FAIL) return FAIL;
	      L1 = L1->next;
	    }
	  return OK;
	default:
	  Scierror("Error: unknown operator \n");
	  return FAIL;
	}
    }
  return FAIL;
}


static int nsp_expr_action_arg(PList *L,void *context,int action)
{
  int val;
  NspObject *Obj;
  switch ((*L)->type) 
    {
    case NAME :
    case OPNAME :
      switch (action ) 
	{
	case get_data : 
	  /* here context is a Hash Table : we replace names 
	   * by constants 
	   */ 
	  /* Sciprintf("search %s\n",(char *) (*L)->O);*/
	  if (nsp_hash_find(context,(char *) (*L)->O,&Obj) == OK )
	    {
	      PList L1=NULLPLIST;
	      if ( IsMat(Obj) && ((NspMatrix *) Obj)->mn == 1) 
		{
		  char str[56];
		  /* Sciprintf("use value %f\n",((NspMatrix *) Obj)->R[0]); */
		  snprintf(str,55,"%f",((NspMatrix *) Obj)->R[0]);
		  if ( nsp_parse_add_doublei(&L1,str) == OK)
		    {
		      /* we want the real value in the double */
		      ((parse_double *) L1->O)->val = ((NspMatrix *) Obj)->R[0];
		      L1->prev=(*L)->prev;
		      L1->next=(*L)->next;
		      if ( (*L)->prev != NULLPLIST) (*L)->prev->next=L1;
		      if ( (*L)->next != NULLPLIST) (*L)->next->prev=L1;
		      (*L)->prev = (*L)->next = NULLPLIST;
		      nsp_plist_destroy(L);
		      *L=L1;
		    }
		}
	    }
	  return OK;
	case store_name : 
	  /* push names in the Bhash table */
	  if ( nsp_bhash_find(context,(char *) (*L)->O,&val) == FAIL) 
	    {
	      if (nsp_bhash_enter(context,(char *) (*L)->O,0) == FAIL) return FAIL;
	    }
	  return OK; 
	case update_id :
	  if ( nsp_bhash_find(context,(char *) (*L)->O,&val) == OK) 
	    {
	      (*L)->arity = val ;
	    }
	  else 
	    {
	      (*L)->arity = -1;
	    }
	  return OK;
	case check_expr: 
	  return OK;
	}
      return OK;
    case NUMBER:
      return OK;
      break;
    case PLIST :
      return nsp_expr_action((PList) (*L)->O,context,action);
      break;
    default: 
      return FAIL;
    }
  return FAIL;
}


/**
 * nsp_eval_expr_context:
 * @L1: a #PList 
 * @context: a hash table 
 * 
 * names in @L1 which are present in hash table @context 
 * are replaced by their values.
 * 
 * Return value: %OK or %FAIL
 **/

static int nsp_eval_expr_context(PList L1,NspHash *context)
{
  return nsp_expr_action(L1,context, get_data);
}

/**
 * nsp_expr_check:
 * @L1:  a #PList 
 * 
 * check that @L1 is valid 
 * 
 * Return value: %OK or %FAIL
 **/

static int nsp_expr_check(PList L1)
{
  return nsp_expr_action(L1,NULL, check_expr);
}

static int nsp_expr_count_logical(PList L1)
{
  int nlogical=0;
  nsp_expr_action(L1,&nlogical, count_logical );
  return nlogical;
}

/**
 * nsp_expr_get_vars:
 * @L1: a #PList 
 * @extra_names: a #NspSMatrix. 
 *
 * returns a string matrix filled with the variables 
 * names of expression @L1 (which are sorted). 
 * As a side effect the expression @L1 is updated in such a way that all the 
 * variables in the expression are associated to their 
 * indice in the string matrix.
 * 
 * If @extra_names is given then the strings from @extra_names are 
 * added to the variable list. This is usefull for scicos simulator, 
 * when for example you have an expression depending only on u1 and u3 and 
 * the block entry is [u1,u2,u3]. In that case you want to add u2 to the 
 * list of variables. 
 * 
 * Return value: a new #NspSMatrix or NULL.
 * 
 **/

static NspSMatrix *nsp_expr_get_vars(PList L1,NspSMatrix *extra_names)
{
  int i,val;
  NspSMatrix *symb_names;
  NspBHash *symbols = NULLBHASH;
  if ((symbols = nsp_bhcreate(NVOID,10)) == NULLBHASH ) return NULL;
  nsp_expr_action(L1,symbols, store_name);
  if ( extra_names != NULL )  
    {
      int i;
      for ( i=0; i < extra_names->mn ; i++) 
	if ( nsp_bhash_find(symbols,extra_names->S[i],&val) == FAIL) 
	  {
	    if (nsp_bhash_enter(symbols,extra_names->S[i],0) == FAIL) return NULL;
	  }
    }
  symb_names= nsp_bhash_get_keys("vars",symbols);
  nsp_qsort_nsp_string(symb_names->S,NULL,FALSE,symb_names->mn,'i');
  for ( i = 0 ; i < symb_names->mn ; i++)
    {
      nsp_bhash_enter(symbols,symb_names->S[i],i+1) ;
    }
  nsp_expr_action(L1,symbols, update_id);
  nsp_bhash_destroy(symbols);
  return symb_names;
}

