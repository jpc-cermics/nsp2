/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2005-2019 Bruno Pincon Esial/Iecn
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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SpMaxpColMatrix_Private 
#include <nsp/object.h> 
#include <nsp/matint.h> 
#include <nsp/matrix.h> 
#include <nsp/spcolmatrix.h> 
#include <nsp/sprowmatrix.h> 
#include <nsp/spmaxpcolmatrix.h> 
#include <nsp/matrix.h> 
#include <nsp/mpmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/cells.h> 
#include <nsp/matint.h> 
#include <nsp/hobj.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
static int int_spmaxpcolmatrix_create(Stack stack, int rhs, int opt, int lhs);

/*
 * NspSpMaxpColMatrix inherits from NspObject 
 */

int nsp_type_spmaxpcolmatrix_id=0;
NspTypeSpMaxpColMatrix *nsp_type_spmaxpcolmatrix=NULL;
int nsp_type_spmaxpcolmatrix_init();

NspTypeSpMaxpColMatrix *new_type_spmaxpcolmatrix(type_mode mode)
{
  NspTypeSpMaxpColMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_spmaxpcolmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_spmaxpcolmatrix;
    }
  if ((type =  malloc(sizeof(NspTypeSpMaxpColMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* spmaxpcolmatrix_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = spmaxpcolmatrix_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_spmaxpcolmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for spmaxpcolmatrix */ 

  top->pr = (print_func *)nsp_spmaxpcolmatrix_print;                  /* printing*/   
  top->dealloc = (dealloc_func *)nsp_spmaxpcolmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_spmaxpcolmatrix_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_spmaxpcolmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_spmaxpcolmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_spmaxpcolmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_spmaxpcolmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) SpMaxpColMatIsTrue;*/      /* check if object can be considered as true */  
  /*top->loop =(loop_func *) SpMaxpColLoopExtract ; */               /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_spmaxpcolmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_spmaxpcolmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_spmaxpcolmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_spmaxpcolmatrix_xdr_save;
  top->load  = (load_func *)nsp_spmaxpcolmatrix_xdr_load;
  top->create = (create_func*) int_spmaxpcolmatrix_create; 

  top->full_copy  =  (copy_func *)nsp_spmaxpcolmatrix_copy;                   /* copy object */  

  /* specific methods for spmaxpcolmatrix */
  type->init = (init_func *) init_spmaxpcolmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_spmaxpcolmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_spmaxpcolmatrix
       */
      type->id =  nsp_type_spmaxpcolmatrix_id = nsp_new_type_id();
      nsp_type_spmaxpcolmatrix = type;
      if ( nsp_register_type(nsp_type_spmaxpcolmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_spmaxpcolmatrix(mode); 
    }
  else 
    {
      type->id = nsp_type_spmaxpcolmatrix_id;
      return type;
    }
}
/*
 * initialize SpMaxpColmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_spmaxpcolmatrix(NspSpMaxpColMatrix *o,NspTypeSpMaxpColMatrix *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Spmatrix 
 */

NspSpMaxpColMatrix *new_spmaxpcolmatrix() 
{
  NspSpMaxpColMatrix *loc; 
  /* type must exists */
  nsp_type_spmaxpcolmatrix = new_type_spmaxpcolmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspSpMaxpColMatrix)))== NULLSPMAXPCOL) return loc;
  /* initialize object */
  if ( init_spmaxpcolmatrix(loc,nsp_type_spmaxpcolmatrix) == FAIL) return NULLSPMAXPCOL;
  return loc;
}


/*
 *nsp_spmaxpcolmatrix_size: returns filled,hsize,or hsize 
 */

int nsp_spmaxpcolmatrix_size(NspSpMaxpColMatrix *H, int flag)
{
  switch (flag) 
    {
    case 0: return H->m*H->n;
    case 1: return H->m;
    case 2: return H->n;
    }
  return 0;
}

/*
 * MatType 
 */

static char sp_type_name[]="SpMaxpColMat";
static char sp_short_type_name[]="spmp";

char *nsp_spmaxpcolmatrix_type_as_string(void)
{
  return(sp_type_name);
}

char *nsp_spmaxpcolmatrix_type_short_string(NspObject *v)
{
  return(sp_short_type_name);
}

int nsp_spmaxpcolmatrix_fullcomp(NspSpMaxpColMatrix * A,NspSpMaxpColMatrix * B,char *op,int *err)
{
  Scierror("SpMatFullComp: to be implemented \n");
  return FALSE;
}

int nsp_spmaxpcolmatrix_eq(NspObject *a, NspObject *b)
{
  int j, k;
  NspSpMaxpColMatrix *A, *B;
  if ( check_cast(b,nsp_type_spmaxpcolmatrix_id) == FALSE) return FALSE ;

  A = (NspSpMaxpColMatrix *) a; B = (NspSpMaxpColMatrix *) b; 
  if ( A->m != B->m) return FALSE;
  if ( A->n != B->n) return FALSE;
  if ( A->rc_type != B->rc_type ) return FALSE;
  
  for ( j = 0 ; j < B->n ; j++ )
    {
      if ( A->D[j]->size !=  B->D[j]->size ) return FALSE;
      for ( k = 0 ; k < A->D[j]->size ; k++ )
	if ( A->D[j]->J[k] !=  B->D[j]->J[k] ) 
	  return FALSE;
      if ( A->rc_type == 'r' )
	{
	  for ( k = 0 ; k < A->D[j]->size ; k++ )
	    if ( A->D[j]->R[k] !=  B->D[j]->R[k] ) 
	      return FALSE;
	}
      else
	{
	  for ( k = 0 ; k < A->D[j]->size ; k++ )
	    if ( A->D[j]->C[k].r !=  B->D[j]->C[k].r  ||  A->D[j]->C[k].i !=  B->D[j]->C[k].i ) 
	      return FALSE;
	}
    }
  return TRUE;
}

int nsp_spmaxpcolmatrix_neq(NspObject *A, NspObject *B)
{
  return ( nsp_spmaxpcolmatrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
}


/*
 * Save a Matrix in a file stream 
 */

static int nsp_spmaxpcolmatrix_xdr_save(XDR *xdrs, NspSpMaxpColMatrix *M)
{
  int rep=FAIL;
  NspMatrix *RC=NULL, *Values=NULL;
  if ( nsp_spmaxpcolmatrix_get(M,&RC,&Values) == FAIL) goto fail;

#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_spmaxpcolmatrix)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) goto fail;
  if (nsp_xdr_save_i(xdrs, M->m) == FAIL) goto fail;
  if (nsp_xdr_save_i(xdrs, M->n) == FAIL) goto fail;
  if ( NSP_OBJECT(RC)->type->save (xdrs,RC) == FAIL) goto fail;
  if ( NSP_OBJECT(Values)->type->save (xdrs,Values) == FAIL) goto fail;
  rep=OK;
 fail:
  if ( RC != NULL) nsp_matrix_destroy(RC);
  if ( Values != NULL) nsp_matrix_destroy(Values);
  return rep;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_xdr_load(XDR *xdrs)
{
  int m,n;
  NspObject *RC=NULL, *Values=NULL;
  NspSpMaxpColMatrix *Loc;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSPMAXPCOL;
  if (nsp_xdr_load_i(xdrs, &m) == FAIL)  return NULLSPMAXPCOL;
  if (nsp_xdr_load_i(xdrs, &n) == FAIL)  return NULLSPMAXPCOL;
  if ( (RC= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSPMAXPCOL;
  if ( (Values= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSPMAXPCOL;
  if ((Loc = nsp_spmaxpcolmatrix_sparse(name,(NspMatrix *)RC,(NspMatrix *)Values,m,n)) == NULLSPMAXPCOL) return NULLSPMAXPCOL;
  if ( RC != NULL) nsp_object_destroy(&RC);
  if ( Values != NULL) nsp_object_destroy(&Values);
  return Loc;
}


/*
 * A =nsp_spmaxpcolmatrix_object(O);
 * checks that O is an object of NspSpMaxpColMatrix type. 
 * or a Hobj which points to an object of type SpMaxpColMatrix
 * if so, returns a pointer to that NspSpMaxpColMatrix and else returns NULL
 */

NspSpMaxpColMatrix   *nsp_spmaxpcolmatrix_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_spmaxpcolmatrix_id) == TRUE) return ((NspSpMaxpColMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_spmaxpcolmatrix));
  return(NULL);
}


/*
 * IsSpMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  SpMaxpColMatrix 
 * or a Hobj which points to an object of type SpMaxpColMatrix
 */

int IsSpMaxpColMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_spmaxpcolmatrix_id);
}

/*
 * IsSpMaxpColMat(O)
 * only checks that object is an object of type  SpMaxpColMatrix 
 * or a Hobj which points to an object of type SpMaxpColMatrix
 */

int IsSpMaxpColMat(const NspObject *O)
{
  return nsp_object_type(O , nsp_type_spmaxpcolmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a Sp and returns that Sp  
 * or a copy of that Sp if its name 
 * is != NVOID 
 */

NspSpMaxpColMatrix *GetSpMaxpColCopy(Stack stack, int i)
{
  if (  GetSpMaxpCol(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a SpMaxpColMatrix and returns that SpMaxpColMatrix  
 */

NspSpMaxpColMatrix *GetSpMaxpCol(Stack stack, int i)
{
  NspSpMaxpColMatrix *M;
  if (( M =nsp_spmaxpcolmatrix_object(NthObj(i))) == NULLSPMAXPCOL )
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a SpMaxpColMatrix and returns that SpMaxpColMatrix  
 */

NspSpMaxpColMatrix *GetRealSpMaxpCol(Stack stack, int i)
{
  NspSpMaxpColMatrix *M;
  if (( M =nsp_spmaxpcolmatrix_object(NthObj(i))) == NULLSPMAXPCOL )
    ArgMessage(stack,i);
  if (M->rc_type == 'c')
    {
      Scierror ("\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should not be complex\n", NspFname(stack));
      return NULLSPMAXPCOL ;
    }
  return M;
}


/*
 *    spmaxpcolmatrix methods
 */


/* 
 *  scale_rows[x]
 *
 *    A <- diag(x)*A
 *
 *    A.scale_rows[x]
 */
static int int_meth_spmaxpcolmatrix_scale_rows(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspSpMaxpColMatrix *A = (NspSpMaxpColMatrix *) self;
  NspMatrix *x;
  char *op=NULL; char ope='*'; 
  nsp_option opts[] ={{"op",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,0);
  CheckStdRhs(1, 1);
  CheckOptRhs(0, 1)

  if ((x = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ( x->mn != A->m )
    { 
      Scierror("%s: the argument should have %d components \n",NspFname(stack),A->m);
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &op) == FAIL )
    return RET_BUG;

  if ( op != NULL) 
    {
      if ( strcmp(op,"*") == 0 )
	ope = '*';
      else if ( strcmp(op,"/") == 0 )
	ope = '/';
      else
	{
	  Scierror("%s: optional named arg op should be set to '*' or '/'\n",NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( nsp_spmaxpcolmatrix_scale_rows(A, x, ope) == FAIL )
    return RET_BUG;

  return 0;
}

/* 
 *  scale_cols[x]
 *
 *    A <- A*diag(x)
 *
 *    A.scale_cols[x]
 */

static int int_meth_spmaxpcolmatrix_scale_cols(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspSpMaxpColMatrix *A = (NspSpMaxpColMatrix *) self;
  NspMatrix *x;
  char *op=NULL; char ope='*'; 
  nsp_option opts[] ={{"op",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,0);
  CheckStdRhs(1, 1);
  CheckOptRhs(0, 1)

  if ((x = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ( x->mn != A->n )
    { 
      Scierror("%s: the argument should have %d components \n",NspFname(stack),A->n);
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &op) == FAIL )
    return RET_BUG;

  if ( op != NULL) 
    {
      if ( strcmp(op,"*") == 0 )
	ope = '*';
      else if ( strcmp(op,"/") == 0 )
	ope = '/';
      else
	{
	  Scierror("%s: optional named arg op should be set to '*' or '/'\n",NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( nsp_spmaxpcolmatrix_scale_cols(A, x, ope) == FAIL )
    return RET_BUG;

  return 0;
}

static int int_meth_spmaxpcolmatrix_redim(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspSpMaxpColMatrix *A = (NspSpMaxpColMatrix *) self;
  int mm, nn;
  CheckLhs(0,0);
  CheckRhs (1,2);

  if ( rhs == 1 )
    {
      NspMatrix *B;
      if ((B = GetRealMat (stack, 1)) == NULLMAT)
	return RET_BUG;
      if (B->mn != 2)
	{
	  Scierror ("Error:\t expecting a vector of size 2\n");
	  return RET_BUG;
	}
      mm = (int) B->R[0];
      nn = (int) B->R[1];
    }
  else
    {
      if (GetScalarInt (stack, 1, &mm) == FAIL) return RET_BUG;
      if (GetScalarInt (stack, 2, &nn) == FAIL) return RET_BUG;
    }

  if ( mm < -1 || nn < -1 || (mm == -1 && nn == -1) )
    {
      Scierror("Error:\tBad arguments (must be >= -1) or not both equal to -1\n");
      return RET_BUG;
    }

  if ( nsp_spmaxpcolmatrix_redim(A,mm,nn,TRUE) == NULLSPMAXPCOL )
    return RET_BUG;

  return 0;
}

/* 
 * get_nnz 
 */
static int int_meth_spmaxpcolmatrix_get_nnz(void *self, Stack stack,int rhs,int opt,int lhs)
{
  CheckLhs(0,1);
  CheckRhs(0,0);
  if ( nsp_move_double(stack,1, nsp_spmaxpcolmatrix_nnz((NspSpMaxpColMatrix *) self)) == FAIL) return RET_BUG;
  return 1;
}

static int int_meth_spmaxpcolmatrix_set_diag(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspObject *ODiag;
  int k=0;
  CheckRhs (1,2);
  CheckLhs (0,0); 
  if ((ODiag = nsp_get_object(stack, 1)) == NULLOBJ)   return RET_BUG;
  if ( ! ( IsSpMaxpColMat(ODiag) || IsMat(ODiag) ) )
    {
      Scierror("%s: first argument should be a sparse or full matrix/vector \n",NspFname(stack));
      return RET_BUG;
    }

  if ( rhs == 2 )
    {
      if (GetScalarInt (stack,2 , &k) == FAIL)   return RET_BUG;
    }
  if (nsp_spmaxpcolmatrix_set_diag ((NspSpMaxpColMatrix *) self, ODiag, k) != OK)
    return RET_BUG;
  return 0;
}

static NspMethods spmaxpcolmatrix_methods[] = {
  { "scale_rows",int_meth_spmaxpcolmatrix_scale_rows}, 
  { "scale_cols",int_meth_spmaxpcolmatrix_scale_cols}, 
  { "get_nnz", int_meth_spmaxpcolmatrix_get_nnz},
  { "set_diag", int_meth_spmaxpcolmatrix_set_diag},
  { "redim", int_meth_spmaxpcolmatrix_redim},
  { (char *) 0, NULL}
};

static NspMethods *spmaxpcolmatrix_get_methods(void) { return spmaxpcolmatrix_methods;};



typedef int (*SpC) (NspSpMaxpColMatrix *A,NspSpMaxpColMatrix *B);


/*
 * Now the interfaced function for basic sparse operations
 */

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPMAXPCOL on failure 
 * The matrix is created with no initial value 
 */

static int int_spmaxpcolmatrix_create(Stack stack, int rhs, int opt, int lhs)
{  
  int m1,n1;
  NspSpMaxpColMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ((HMat =nsp_spmaxpcolmatrix_create(NVOID,'r',m1,n1) ) == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1, (NspObject *) HMat);
  return 1;
}

/*
 * interface for creation of a maxplus sparse matrix 
 * sparsemp(rc,values [, [m,n]])
 * sparsemp(m,n) 
 * 
 * sparse(A) when A is a maxplus matrix 
 * 
 */

static int int_spmaxpcolmatrix_mp2spmp(Stack stack, int rhs, int opt, int lhs);

static int int_spmaxpcolmatrix_sparse(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpMaxpColMatrix *A;
  NspMatrix *RC,*Values,*MN;
  if ( rhs == 1) 
    return  int_spmaxpcolmatrix_mp2spmp(stack,rhs,opt,lhs);
  CheckStdRhs(2,3);
  CheckLhs(1,1);
  if ((RC = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( RC->mn == 1) 
    {
      /* special case sparse(m,n) */
      int m = RC->R[0], n;
      if ( GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
      if ((A =nsp_spmaxpcolmatrix_create(NVOID,'r',m,n) ) == NULLSPMAXPCOL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) A);
      return 1;
    }
  if (RC->mn != 0 &&  RC->n != 2)
    {
      Scierror("Error: first argument of function %s must have 2 columns\n\n",
	       NspFname(stack));
      return RET_BUG;
    }
  if ((Values = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( RC->m != Values->mn  ) 
    {
      Scierror("Error: first and second arguments of function %s have incompatible sizes\n",
	       NspFname(stack));
      return RET_BUG;
    }
  if ( rhs == 3 )
    {
      if ((MN = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;
      if (( A =nsp_spmaxpcolmatrix_sparse(NVOID,RC,Values,(int) MN->R[0],(int) MN->R[1])) == NULLSPMAXPCOL) 
	return RET_BUG;
    }
  else
    {
      if (( A =nsp_spmaxpcolmatrix_sparse(NVOID,RC,Values,-1,-1)) == NULLSPMAXPCOL) 
	return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*                                                                              
 * return [rc,values,[m,n]] from                                                
 * a sparse matrix.                                                             
 * This could be a method                                                       
 */

static int int_spmaxpcolmatrix_get(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpMaxpColMatrix *A;
  NspMatrix *RC,*Values,*MN=NULL;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ( nsp_spmaxpcolmatrix_get(A,&RC,&Values) == FAIL) return RET_BUG;
  if ( lhs == 3) 
    {
      if (( MN = nsp_matrix_create(NVOID,'r',1,2)) == NULLMAT) return RET_BUG;
      MN->R[0] = A->m; MN->R[1] = A->n;
    }
  if ( lhs >= 2) 
    {
      NthObj(2) = (NspObject *)Values;
      NthObj(2)->ret_pos = 2;
    }
  if ( lhs >= 3) 
    {
      NthObj(3) = (NspObject *)MN;
      NthObj(3)->ret_pos = 3;
    }
  MoveObj(stack,1,(NspObject *) RC);
  return Max(lhs,1);
}

/* get a mtlb triplet                                                           
 *                                                                              
 */

static int int_spmaxpcolmatrix_get_mtlb(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMatrix *Jc=NULL,*Ir=NULL,*Pr=NULL;
  int nzmax,i;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ( nsp_spcol_set_triplet_from_m((NspSpColMatrix *)A,TRUE) == FAIL) return RET_BUG;
  nzmax = A->triplet.Aisize;
  if ( lhs >=2 ) 
    {
      if ((Ir = nsp_matrix_create(NVOID,'r',1,nzmax))== NULLMAT) goto err;
      for ( i= 0 ; i < nzmax ; i++) Ir->R[i]= A->triplet.Ir[i];
      MoveObj(stack,2,NSP_OBJECT(Ir));
    }
  if ( lhs >= 3 ) 
    {
      if ( A->rc_type == 'r')
	{
	  if ((Pr = nsp_matrix_create(NVOID,'r',1,nzmax))== NULLMAT) goto err;
	  memcpy(Pr->R,A->triplet.Pr, nzmax*sizeof(double));
	}
      else 
	{
	  int i;
	  if ((Pr = nsp_matrix_create(NVOID,A->rc_type,1,nzmax))== NULLMAT) goto err;
	  for ( i= 0 ; i < Pr->mn ; i++) 
	    {
	      Pr->C[i].r = A->triplet.Pr[i];
	      Pr->C[i].i = A->triplet.Pi[i];
	    }
	}
      MoveObj(stack,3,NSP_OBJECT(Pr));
    }
  /* Note that this one is performed last because 
   * MoveObj will free the triplet of matrix A.
   */
  if ((Jc = nsp_matrix_create(NVOID,'r',1,(A->n+1)))== NULLMAT) goto err;
  for ( i= 0 ; i < (A->n+1) ; i++) Jc->R[i]= A->triplet.Jc[i];
  nsp_spcol_free_triplet((NspSpColMatrix *) A);
  MoveObj(stack,1,NSP_OBJECT(Jc));
  return Max(lhs,1);
 err:
  nsp_matrix_destroy(Jc);
  nsp_matrix_destroy(Ir);
  nsp_matrix_destroy(Pr);
  if (A->convert == 't') 
    nsp_spcol_free_triplet((NspSpColMatrix *) A);
  return RET_BUG;
}

/* XXX: remains to check that arguments are 
 * correct 
 *
 */

static int int_spmaxpcolmatrix_from_mtlb(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMatrix *Jc=NULL,*Ir=NULL,*Pr=NULL,*Mn=NULL;
  int i, imin,imax, prev,Irmn,Irmn1;
  CheckRhs(4,4);
  if ((Jc = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((Ir = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Pr = GetMat(stack,3)) == NULLMAT) return RET_BUG;
  if ((Mn = GetRealMat(stack,4)) == NULLMAT) return RET_BUG;
  if ( ((int)Mn->R[0]) < 0 || ((int) Mn->R[1] < 0))
    {
      Scierror("Error: fourth argument of %s should be [m,n] with non negative values\n",NspFname(stack));
      return RET_BUG;
    }
  if ( ((int) Mn->R[1] ) != Jc->mn -1 )
    {
      Scierror("Error: first argument of %s should be of length %d+1\n", NspFname(stack),(int) Mn->R[1]);
      return RET_BUG;
    }

  if ( Pr->mn != Jc->R[Jc->mn-1] )
    {
      Scierror("Error: third argument of %s should be of length %d\n", NspFname(stack),(int) Jc->R[Jc->mn-1]);
      return RET_BUG;
    }
  /* Ir->mn can be bigger Than expected size Jc->R[Jc->mn-1] 
   * we ignore last entries which should be zero.
   */
  if ( Ir->mn < Jc->R[Jc->mn-1] )
    {
      Scierror("Error: second argument of %s should be of length %d\n", NspFname(stack),(int) Jc->R[Jc->mn-1]);
      return RET_BUG;
    }
  Irmn = Min(Ir->mn,Jc->R[Jc->mn-1]);
  Irmn1 =Ir->mn;
  Ir->mn = Irmn;
  Bounds (Ir,&imin,&imax);
  Ir->mn = Irmn1;
  if ( imin < 0 || imax > ((int) Mn->R[0]) -1) 
    {
      Scierror("Error: second argument of %s should have values in [%d,%d]\n", NspFname(stack),0,((int) Mn->R[0]) -1);
      return RET_BUG;
    }
  /* checks that Jc is increasing with positive numbers */
  prev=0;
  for ( i= 0 ; i < Jc->mn ; i++)
    {
      if ( Jc->R[i] < prev )
	{
	  Scierror("Error: third argument of %s should have increasing values\n", NspFname(stack));
	  return RET_BUG;
	}
      prev = Jc->R[i];
    }
  /* XXX : check mn [m,n] */
  if ((A =nsp_spmaxpcolmatrix_create(NVOID,Pr->rc_type,Mn->R[0],Mn->R[1]) ) == NULLSPMAXPCOL) return RET_BUG;
  if ( nsp_spcol_alloc_col_triplet((NspSpColMatrix *) A,Jc->R[Jc->mn-1])== FAIL) return RET_BUG;
  for ( i= 0 ; i < Irmn ; i++)  A->triplet.Ir[i] = Ir->R[i];
  for ( i= 0 ; i < Jc->mn ; i++)  A->triplet.Jc[i] = Jc->R[i];
  if ( Pr->rc_type == 'r' ) 
    {
      memcpy(A->triplet.Pr,Pr->R, Pr->mn*sizeof(double));
    }
  else 
    {
      for ( i = 0 ; i < Pr->mn ; i++) 
	{
	  A->triplet.Pr[i]= Pr->C[i].r;
	  A->triplet.Pi[i]= Pr->C[i].i;
	}
    }
  if ( nsp_spcol_update_from_triplet((NspSpColMatrix *) A) == FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(A));
  return Max(lhs,1);
}




/*
 * Matredim : changes Matrix dimensions
 * but keeps m*n constant
 */

static int int_spmaxpcolmatrix_redim(Stack stack, int rhs, int opt, int lhs)
{
  int mm,nn;
  NspSpMaxpColMatrix  *A;
  CheckRhs(2,3);
  CheckLhs(1,1);

  if ( (A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;

  if ( rhs == 2 )
    {
      NspMatrix *B;
      if ((B = GetRealMat (stack, 2)) == NULLMAT)
	return RET_BUG;
      if (B->mn != 2)
	{
	  Scierror ("Error:\t expecting a vector of size 2\n");
	  return RET_BUG;
	}
      mm = (int) B->R[0];
      nn = (int) B->R[1];
    }
  else
    {
      if (GetScalarInt (stack, 2, &mm) == FAIL) return RET_BUG;
      if (GetScalarInt (stack, 3, &nn) == FAIL) return RET_BUG;
    }

  if ( mm < -1 || nn < -1 || (mm == -1 && nn == -1) )
    {
      Scierror("Error:\tBad arguments (must be >= -1) or not both equal to -1\n");
      return RET_BUG;
    }

  if (( A =nsp_spmaxpcolmatrix_redim(A,mm,nn,FALSE)) == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}

static int int_spmaxpcolmatrix_resize2vect(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix  *A;
  CheckRhs(1,1);
  CheckLhs(1,1);

  if ( (A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;

  if (( A =nsp_spmaxpcolmatrix_redim(A,-1,1,FALSE)) == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * Right Concatenation 
 * A= [A,B] 
 */


static int int_spmaxpcolmatrix_concat_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpMaxpColMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpMaxpCol(stack,1))  == NULLSPMAXPCOL) return RET_BUG;
  if ( HMat1->m ==0 && HMat1->n == 0) 
    {
      /* return 2 */
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
  if ( HMat2->m == 0 && HMat2->n == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetSpMaxpColCopy(stack,1))  == NULLSPMAXPCOL) return RET_BUG;
      if ( (*F)(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}


static int int_spmaxpcolmatrix_concatr(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPMAXPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spmaxpcolmatrix_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPMAXPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spmaxpcolmatrix_concatdiag(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatdiag);
}

/* [Sp op Full]
 *
 */

static int int_spmaxpcolmatrix_concat_sp_m_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpMaxpColMatrix *HMat1,*B;
  NspMaxpMatrix *HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpMaxpCol(stack,1))  == NULLSPMAXPCOL) return RET_BUG;
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat1->m ==0 && HMat1->n == 0) 
    {
      /* return 2 */
      if ((B=nsp_spmaxpcolmatrix_from_mat((NspMatrix *)HMat2)) == NULLSPMAXPCOL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(B));
      return 1;
    }
  if ( HMat2->m == 0 && HMat2->n == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetSpMaxpColCopy(stack,1))  == NULLSPMAXPCOL) return RET_BUG;
      if ((B=nsp_spmaxpcolmatrix_from_mat((NspMatrix *)HMat2)) == NULLSPMAXPCOL) return RET_BUG;
      if ( (*F)(HMat1,B)!= OK) 
	{
	  nsp_spmaxpcolmatrix_destroy(B);
	  return RET_BUG;
	}
      nsp_spmaxpcolmatrix_destroy(B);
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}

static int int_spmaxpcolmatrix_concatr_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_sp_m_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPMAXPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spmaxpcolmatrix_concatd_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_sp_m_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPMAXPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spmaxpcolmatrix_concatdiag_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_sp_m_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatdiag);
}


/* [Sp op Full]
 *
 */

static int int_spmaxpcolmatrix_concat_m_sp_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpMaxpColMatrix *HMat2,*B;
  NspMaxpMatrix *HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1))  == NULLMAXPMAT ) return RET_BUG;
  if ( HMat1->m ==0 && HMat1->n == 0) 
    {
      /* return 2 */
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }

  if ((HMat2 = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
  if ( HMat2->m == 0 && HMat2->n == 0) 
    {
      if ((B=nsp_spmaxpcolmatrix_from_mat((NspMatrix *)HMat1)) == NULLSPMAXPCOL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(B));
      return 1;
    }
  else
    {
      if ((B=nsp_spmaxpcolmatrix_from_mat((NspMatrix *)HMat1)) == NULLSPMAXPCOL) return RET_BUG;
      if ( (*F)(B,HMat2)!= OK) 
	{
	  return RET_BUG;
	}
      MoveObj(stack,1,NSP_OBJECT(B));
    }
  return 1;
}

static int int_spmaxpcolmatrix_concatr_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_m_sp_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPMAXPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spmaxpcolmatrix_concatd_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_m_sp_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPMAXPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spmaxpcolmatrix_concatdiag_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_concat_m_sp_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_concatdiag);
}



/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *  =======
 */

static int int_spmaxpcolmatrix_setrc(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A,*B=NULLSPMAXPCOL,*B1=NULLSPMAXPCOL;
  NspMaxpMatrix *Bm = NULLMAXPMAT;
  NspObject *Rows,*Cols=NULL;
  Boolean full_case = FALSE;

  CheckRhs(3,4);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) goto ret_bug;
  if ((Rows =nsp_get_object(stack,2)) == NULLOBJ)  goto ret_bug;
  if ( rhs == 4 )
    {
      if ((Cols =nsp_get_object(stack,3)) == NULLOBJ)  goto ret_bug;
    }
  
  if ( IsMpMatObj(stack,rhs) )
    {
      full_case = TRUE;
      if ((Bm = GetMpMat(stack,rhs)) == NULLMAXPMAT ) goto ret_bug;
      if ( rhs == 3 )
	if ((B1= B=nsp_spmaxpcolmatrix_from_mat((NspMatrix *) Bm)) == NULLSPMAXPCOL) goto ret_bug;
    }
  else if ( IsSpMaxpColMatObj(stack,rhs)) 
    {
      if ((B = GetSpMaxpCol(stack,rhs)) == NULLSPMAXPCOL) goto ret_bug;
    }
  else 
    {
      Scierror("Error: expecting matrix or sparse when setting values in a sparse\n");
      goto ret_bug;
    }

  if ( B == A ) 
    { if ((B = GetSpMaxpColCopy(stack,rhs)) == NULLSPMAXPCOL) goto ret_bug;}
  if ( rhs == 3 ) 
    { if (nsp_spmaxpcolmatrix_set_row( A, Rows,B) == FAIL) goto ret_bug; }
  else 
    { 
      if ( full_case )
      	{
      	  if (nsp_spmaxpcolmatrix_set_rowcol_from_full( A, Rows,Cols,(NspMatrix*) Bm) == FAIL )  goto ret_bug;
      	}
      else
      	{
	  if (nsp_spmaxpcolmatrix_set_rowcol( A, Rows,Cols,B) == FAIL )  goto ret_bug;
	}
    } 
  NSP_OBJECT(A)->ret_pos = 1;
  nsp_spmaxpcolmatrix_destroy(B1);
  return 1;
 ret_bug: 
  /* delete if non null; */
  nsp_spmaxpcolmatrix_destroy(B1);
  return RET_BUG;
}


/*
 * Res=MatDeletecols(A,Cols)
 *     Cols unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 * =======
 */	


static int int_spmaxpcolmatrix_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspObject *Cols;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  /*  Note that if Cols is authorized to be a sparse 
   *  then checking that A and Cols are different 
   *  should be checked 
   */
  if ((Cols =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  /*
   */
  if (nsp_spmaxpcolmatrix_delete_cols( A, Cols) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=MatDeleterows(A,Rows)
 * WARNING : A must be changed by this routine
 * =======
 */	

static int int_spmaxpcolmatrix_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspObject *Rows;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  /*  Note that if Cols is authorized to be a sparse 
   *  then checking that A and Cols are different 
   *  should be checked 
   */
  if ((Rows =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  /*
   */
  if (nsp_spmaxpcolmatrix_delete_rows( A, Rows) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=nsp_spmaxpcolmatrix_extract(Rows,Cols,A)
 * A unchanged, Rows and Cols are unchanged 
 */	

static int int_spmaxpcolmatrix_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A,*Res;
  NspObject *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
#if 0 
  /* Rows id changed by nsp_spmaxpcolmatrix_extract */
  if ((Rows = GetRealMatCopy(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;
#else 
  /* Rows is no more changed */
  if ((Rows =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  if ((Cols =nsp_get_object(stack,3)) == NULLOBJ) return RET_BUG;
#endif 
  Res =nsp_spmaxpcolmatrix_extract( A, Rows,Cols);
  if ( Res == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * Res=nsp_matrix_extract_elements(Elts,A)
 * A unchanged, Elts
 * WARNING note that on the stack we have Elts,A 
 */	

static int int_spmaxpcolmatrix_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A,*Res;
  NspObject *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((Elts =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  if ((Res =nsp_spmaxpcolmatrix_extract_elts( A, Elts)) == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * columns extraction  Cols A --> A(Cols)				  * 
 */	

static int int_spmaxpcolmatrix_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A,*Res;
  NspObject *Cols;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((Cols =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  Res =nsp_spmaxpcolmatrix_extract_cols( A,Cols,&err);
  /* XXXXX Attention ici il faut un message d''erreur **/
  if ( err == 1) return RET_ENDFOR; 
  if ( Res == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * rows extraction 			
 * Rows , A -> A(Rows,:)
 */	

static int int_spmaxpcolmatrix_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A,*Res;
  NspObject *Rows;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  /* Rows is changed by nsp_spmaxpcolmatrix_extract_rows */
  if ((Rows =nsp_get_object_copy(stack,2)) == NULLOBJ) return RET_BUG;
  Res =nsp_spmaxpcolmatrix_extract_rows( A,Rows,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


/*
 * diag interface 
 */

int int_spmaxpcolmatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspSpMaxpColMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_spmaxpcolmatrix_diag_create (A, k1);
  else
    Res = nsp_spmaxpcolmatrix_diag_extract (A, k1);
  if (Res == NULLSPMAXPCOL)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Returns the kthe diag of a Matrix 
 */

static int int_spmaxpcolmatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpMaxpColMatrix *A,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    { if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;}
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  Res =nsp_spmaxpcolmatrix_diag_extract( A,k1);
  if ( Res == NULLSPMAXPCOL)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 *  Creates a Matrix with kth diag set to Diag 
 */

static int int_spmaxpcolmatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpMaxpColMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_spmaxpcolmatrix_diag_create(Diag,k1)) == NULLSPMAXPCOL) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= mx2spmx(A) 
 * A is not changed 
 */

static int int_spmaxpcolmatrix_mp2spmp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *Res;
  NspMaxpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMpMat(stack,1)) == NULLMAXPMAT)  return RET_BUG;
  if (( Res=nsp_spmaxpcolmatrix_from_mat((NspMatrix *) A)) == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Res= full(A) 
 * A is not changed 
 */

static int int_spmaxpcolmatrix_spmp2mp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMaxpMatrix *Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL)  return RET_BUG;
  if (( Res=nsp_spmaxpcolmatrix_to_mpmat(A)) == NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= full(A) when A is not sparse 
 * do nothing 
 */
#if 0
static int int_spmaxpcolmatrix_m2m(Stack stack, int rhs, int opt, int lhs)
{
  /* full(x) when x is already full **/
  NthObj(1)->ret_pos = 1;
  return 1;
}
#endif 


/*
 * Res= A*B 
 * return NULLSPMAXPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

/* Generic function for * or .* **/
typedef NspSpMaxpColMatrix * (*Sp21) (NspSpMaxpColMatrix *A,NspSpMaxpColMatrix *B);

static int int_spmaxpcolmatrix_mult_gen(Stack stack, int rhs, int opt, int lhs, Sp21 F)
{
  NspSpMaxpColMatrix *HMat1,*HMat2,*HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((HMat2 = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
  if ( HMat2->m == 1 && HMat2->n == 1) 
    {
      if ( HMat2->D[0]->size == 0) 
	{
	  /* HMat2 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_spmaxpcolmatrix_create(NVOID,'r',HMat1->m,HMat1->n)) == NULLSPMAXPCOL) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else 
	{
	  /* A * <non-nul-scalar> **/
	  if ((HMat1 = GetSpMaxpColCopy(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
	  if (nsp_spmaxpcolmatrix_mult_scal(HMat1,HMat2) != OK) return RET_BUG;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
    }
  else if ( HMat1->m==1 && HMat1->n == 1 ) 
    {
      if ( HMat1->D[0]->size == 0) 
	{
	  /* HMat1 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_spmaxpcolmatrix_create(NVOID,'r',HMat2->m,HMat2->n)) == NULLSPMAXPCOL) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else
	{
	  /* since Mat1 is scalar we store the result in Mat2 so we 
	     must copy it **/
	  if ((HMat2 = GetSpMaxpColCopy(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
	  if (nsp_spmaxpcolmatrix_mult_scal(HMat2,HMat1) != OK) return RET_BUG;
	  NSP_OBJECT(HMat2)->ret_pos = 1;
	}
    }
  else 
    {
      if ((HMat3=(*F)(HMat1,HMat2)) == NULLSPMAXPCOL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

static int int_spmaxpcolmatrix_mult(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_mult_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_mult);
}

/*
 *   Res = A * X , A sparse matrix, X full matrix
 */
static int int_spmaxpcolmatrix_mult_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat1;
  NspMatrix *HMat2, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT) return RET_BUG;

  if ( HMat2->m == 1 && HMat2->n == 1 )
    {
      if ((HMat1 = GetSpMaxpColCopy(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
      if ( nsp_spmaxpcolmatrix_mult_scalar(HMat2->R, HMat2->rc_type, HMat1) == FAIL ) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else if ( HMat1->n == HMat2->m )
    {
      if ( (HMat3 = nsp_spmaxpcolmatrix_mult_sp_m(HMat1, HMat2, NULLMAT)) == NULLMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  else
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return RET_BUG;
    }

  return 1;
}

/*
 *   prime mult:
 *   Res = A' * X , A sparse matrix, X full matrix
 */
static int int_spmaxpcolmatrix_pmult_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat1;
  NspMatrix *HMat2, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT) return RET_BUG;

  if ( HMat1->m == HMat2->m )
    {
      if ( (HMat3 = nsp_spmaxpcolmatrix_pmult_sp_m(HMat1, HMat2, NULLMAT)) == NULLMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  else
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return RET_BUG;
    }

  return 1;
}

/*
 *   Res = X * A , A sparse matrix, X full matrix
 *   A and X are left unchanged
 */

static int int_spmaxpcolmatrix_mult_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat2;
  NspMatrix *HMat1, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  if ((HMat2 = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;

  if ( HMat1->m == 1 && HMat1->n == 1 )
    {
      if ((HMat2 = GetSpMaxpColCopy(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
      if ( nsp_spmaxpcolmatrix_mult_scalar(HMat1->R, HMat1->rc_type, HMat2) == FAIL ) return RET_BUG;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else if ( HMat1->n == HMat2->m )
    {
      if ( (HMat3 = nsp_spmaxpcolmatrix_mult_m_sp(HMat1, HMat2)) == NULLMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  else
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return RET_BUG;
    }

  return 1;

}

/*
 * Res= A'
 * return NULLSPMAXPCOLon failure 
 * A is left unchanged 
 */

static int int_spmaxpcolmatrix_quote(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((A =nsp_spmaxpcolmatrix_transpose(A)) == NULLSPMAXPCOL)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * _
 * A'
 */

static int int_spmaxpcolmatrix_dquote (Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)
    return RET_BUG;
  if ((B = nsp_spmaxpcolmatrix_transpose (A)) == NULLSPMAXPCOL)
    return RET_BUG;
  nsp_spmaxpcolmatrix_conj(B);
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}



/*
 * C = A + B with special cases [] and [x] 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

static int int_spmaxpcolmatrix_plus(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpMaxpColMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((B = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
  if ( A->m == 1 && A->n == 1) 
    {
      NspMatrix *C=nsp_spmaxpcolmatrix_op_scal(B,A,&flag,'+');
      if ( flag == 1) 
	{
	  NSP_OBJECT(B)->ret_pos = 1;
	  return 1; 	  /* A was [] or [0] **/
	}
      else
	{
	  /* C = A + scal is returned in a Matrix **/
	  MoveObj(stack,1,(NspObject *)C);
	  return 1;
	}
    }
  if ( B->m == 1 && B->n == 1 ) 
    {
      NspMatrix *C=nsp_spmaxpcolmatrix_op_scal(A,B,&flag,'+');
      if ( flag == 1) 
	{
	  NSP_OBJECT(A)->ret_pos = 1;
	  return 1; 	  /* B was [] or [0] **/
	}
      else
	{
	  /* C = A + scal is returned in a Matrix **/
	  MoveObj(stack,1,(NspObject *)C);
	  return 1;
	}
    }
  if ( SameDim(A,B) ) 
    {
      NspSpMaxpColMatrix *Res =nsp_spmaxpcolmatrix_add(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
}

/*
 * {"plus_m_sp",int_spmaxpcolmatrix_plus_m_sp},
 *  {"plus_sp_m",int_spmaxpcolmatrix_plus_sp_m},
 */

static int int_spmaxpcolmatrix_plus_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *B;
  NspMaxpMatrix *A,*Bfull=NULL;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
  if (( Bfull=nsp_spmaxpcolmatrix_to_mpmat(B)) == NULLMAXPMAT) return RET_BUG;

  if ( SameDim(A,Bfull) ) 
    {
      /* compute result in Bfull */
      if (  nsp_mat_dadd((NspMatrix*) Bfull, (NspMatrix *) A)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Bfull );
      return 1;
    }
  if ( A->mn == 1) 
    {
      if ( nsp_mat_add_scalar_bis((NspMatrix*)Bfull,(NspMatrix*)A)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Bfull );
      return 1;
    }
  if ( B->m == 1 && B->n == 1) 
    {
      int rep;
      if ((A = GetMpMatCopy(stack,1)) == NULLMAXPMAT) return RET_BUG;
      rep= nsp_mat_add_scalar_bis((NspMatrix*) A,(NspMatrix*) Bfull);
      nsp_mpmatrix_destroy(Bfull);
      if (rep == FAIL) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
 err:
  nsp_mpmatrix_destroy(Bfull);
  return RET_BUG;
}

static int int_spmaxpcolmatrix_plus_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMaxpMatrix *B,*Afull=NULL;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((B = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if (( Afull= nsp_spmaxpcolmatrix_to_mpmat(A)) == NULLMAXPMAT) return RET_BUG;

  if ( SameDim(Afull,B) ) 
    {
      /* compute result in Afull */
      if (  nsp_mat_dadd((NspMatrix*)Afull,(NspMatrix*)B)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Afull );
      return 1;
    }
  if ( B->mn == 1) 
    {
      if ( nsp_mat_add_scalar_bis((NspMatrix*)Afull,(NspMatrix*)B)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Afull );
      return 1;
    }

  if ( A->m == 1 && A->n == 1) 
    {
      int rep;
      if ((B = GetMpMatCopy(stack,2)) == NULLMAXPMAT) return RET_BUG;
      rep= nsp_mat_add_scalar_bis((NspMatrix*)B,(NspMatrix*)Afull);
      nsp_mpmatrix_destroy(Afull);
      if (rep == FAIL) return RET_BUG;
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
 err:
  nsp_mpmatrix_destroy(Afull);
  return RET_BUG;
}


/*
 * OHMat3 = OHMat1-OHMat2 
 * return NULLSPMAXPCOLon failure 
 */
static int int_spmaxpcolmatrix_minus(Stack stack, int rhs, int opt, int lhs);

static int int_spmaxpcolmatrix_sub(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpMaxpColMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((B = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      NspSpMaxpColMatrix *Res =nsp_spmaxpcolmatrix_sub(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  if ( A->m == 1 && A->n == 1) 
    {
      NspMatrix *C=nsp_spmaxpcolmatrix_op_scal(B,A,&flag,'#'); /* -B + scalar **/
      if ( flag == 1) 
	{
	  /* B -> -B **/
	  stack.first += 1;
	  int_spmaxpcolmatrix_minus(stack,1,opt,1);
	  stack.first -= 1;
	  NSP_OBJECT(B)->ret_pos = 1;
	  return 1; 	  /* A was [] or [0] **/
	}
      else
	{
	  /* C = A - scal is returned in a Matrix **/
	  MoveObj(stack,1,(NspObject *)C);
	  return 1;
	}
    }
  if ( B->m == 1 && B->n == 1 ) 
    {
      NspMatrix *C=nsp_spmaxpcolmatrix_op_scal(A,B,&flag,'-');
      if ( flag == 1) 
	{
	  NSP_OBJECT(A)->ret_pos = 1;
	  return 1; 	  /* B was [] or [0] **/
	}
      else
	{
	  /* C = A + scal is returned in a Matrix **/
	  MoveObj(stack,1,(NspObject *)C);
	  return 1;
	}
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
}



static int int_spmaxpcolmatrix_sub_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *B;
  NspMaxpMatrix *A,*Bfull=NULL;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
  if (( Bfull=nsp_spmaxpcolmatrix_to_mpmat(B)) == NULLMAXPMAT) return RET_BUG;

  if ( SameDim(A,Bfull) ) 
    {
      /* compute result in Bfull */
      if (  nsp_mat_sub((NspMatrix*)Bfull,(NspMatrix*) A)== FAIL) goto err; 
      /* change sign */
      nsp_mat_minus((NspMatrix*) Bfull);
      MoveObj(stack,1,(NspObject *) Bfull );
      return 1;
    }
  if ( A->mn == 1) 
    {
      if ( nsp_scalar_sub_mat_bis((NspMatrix*)Bfull,(NspMatrix*)A)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Bfull );
      return 1;
    }
  if ( B->m == 1 && B->n == 1)
    {
      int rep;
      if ((A = GetMpMatCopy(stack,1)) == NULLMAXPMAT) return RET_BUG;
      rep= nsp_mat_sub_scalar_bis((NspMatrix*)A,(NspMatrix*) Bfull);
      nsp_mpmatrix_destroy(Bfull);
      if (rep == FAIL) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
 err:
  nsp_mpmatrix_destroy(Bfull);
  return RET_BUG;
}

/* A - 
 *
 */

static int int_spmaxpcolmatrix_sub_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMaxpMatrix *B,*Afull=NULL;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((B = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if (( Afull=nsp_spmaxpcolmatrix_to_mpmat(A)) == NULLMAXPMAT) return RET_BUG;
  
  if ( SameDim(Afull,B) ) 
    {
      /* compute result in Bfull */
      if (  nsp_mat_sub((NspMatrix*)Afull,(NspMatrix*)B)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Afull );
      return 1;
    }
  if ( B->mn == 1) 
    {
      if ( nsp_mat_sub_scalar_bis((NspMatrix*)Afull,(NspMatrix*)B)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Afull );
      return 1;
    }

  if ( A->m == 1 && A->m == 1) 
    {
      int rep;
      if ((B = GetMpMatCopy(stack,2)) == NULLMAXPMAT) return RET_BUG;
      rep=  nsp_scalar_sub_mat_bis((NspMatrix*)B,(NspMatrix*)Afull);
      nsp_mpmatrix_destroy(Afull);
      if (rep == FAIL) return RET_BUG;
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
 err:
  nsp_mpmatrix_destroy(Afull);
  return RET_BUG;
}

/* As .* Bs 
 */

static int int_spmaxpcolmatrix_multt(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_mult_gen(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_multtt);
}

/* A .* Bs 
 *
 */

static int int_spmaxpcolmatrix_multt_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *B,*As=NULL;
  NspMatrix *A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1)) == NULLMAT) goto err;
  if ((B = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) goto err;
  if ((As=nsp_spmaxpcolmatrix_from_mat(A)) == NULLSPMAXPCOL) goto err;
  if ( A->mn == 1) 
    {
      /* A * <non-nul-scalar> **/
      if ((B = GetSpMaxpColCopy(stack,2)) == NULLSPMAXPCOL) goto err;
      if (nsp_spmaxpcolmatrix_mult_scal(B,As) != OK) goto err;
      NSP_OBJECT(B)->ret_pos = 1;
      nsp_spmaxpcolmatrix_destroy(As);
      return 1;
    }
  if ( B->m==1 && B->n == 1) 
    {
      if (nsp_spmaxpcolmatrix_mult_scal(As,B) != OK) goto err;
      MoveObj(stack,1,NSP_OBJECT(As));
      return 1;
    }
  
  if ( SameDim(A,B) ) 
    {
      NspSpMaxpColMatrix *Res;
      if ((Res=nsp_spmaxpcolmatrix_multtt(As,B)) == NULLSPMAXPCOL) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      nsp_spmaxpcolmatrix_destroy(As);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
 err: 
  nsp_spmaxpcolmatrix_destroy(As);
  return RET_BUG;
}

/* As .* B 
 *
 */

static int int_spmaxpcolmatrix_multt_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A, *Bs=NULL;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) goto err;
  if ((B = GetMat(stack,2)) == NULLMAT) goto err;
  if ((Bs=nsp_spmaxpcolmatrix_from_mat(B)) == NULLSPMAXPCOL) goto err;

  if ( B->mn == 1) 
    {
      /* A * <non-nul-scalar> **/
      if ((A = GetSpMaxpColCopy(stack,1)) == NULLSPMAXPCOL) goto err;
      if (nsp_spmaxpcolmatrix_mult_scal(A,Bs) != OK) goto err;
      NSP_OBJECT(A)->ret_pos = 1;
      nsp_spmaxpcolmatrix_destroy(Bs);
      return 1;
    }

  if ( A->m==1 && A->n == 1) 
    {
      if (nsp_spmaxpcolmatrix_mult_scal(Bs,A) != OK) goto err;
      MoveObj(stack,1,NSP_OBJECT(Bs));
      return 1;
    }

  if ( SameDim(A,B) ) 
    {
      NspSpMaxpColMatrix *Res;
      if ((Res=nsp_spmaxpcolmatrix_multtt(A,Bs)) == NULLSPMAXPCOL) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      nsp_spmaxpcolmatrix_destroy(Bs);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
 err:
  nsp_spmaxpcolmatrix_destroy(Bs);
  return RET_BUG;
}

/*
 * As ./ Bs 
 */

int int_spmaxpcolmatrix_div_el(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat1,*HMat2,*HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((HMat2 = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) return RET_BUG;
  if ( HMat2->m == 1 && HMat2->n == 1 ) 
    {
      if ( HMat2->D[0]->size == 0 ) 
	{
	  if ((HMat3 = nsp_spmaxpcolmatrix_div_zero_tt(HMat1)) == NULLSPMAXPCOL) return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else 
	{
	  if ((HMat1 = GetSpMaxpColCopy(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
	  if (nsp_spmaxpcolmatrix_div_scal_tt(HMat1,HMat2) != OK) return RET_BUG;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
    }
  else if ( HMat1->m == 1 && HMat1->n == 1 ) 
    {
      if ((HMat3 =nsp_spmaxpcolmatrix_scal_div_tt(HMat1,HMat2))  == NULLSPMAXPCOL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  else 
    {
      if ((HMat3 = nsp_spmaxpcolmatrix_divel(HMat1,HMat2)) == NULLSPMAXPCOL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

/* A ./ As 
 *
 */

static int int_spmaxpcolmatrix_div_el_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *B,*As=NULL,*Res=NULL;
  NspMatrix *A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1)) == NULLMAT) goto err;
  if ((B = GetSpMaxpCol(stack,2)) == NULLSPMAXPCOL) goto err;
  if ((As=nsp_spmaxpcolmatrix_from_mat(A)) == NULLSPMAXPCOL) goto err;
  if ( A->mn == 1) 
    {
      /* A * <non-nul-scalar> **/
      if ((Res =nsp_spmaxpcolmatrix_scal_div_tt(As,B))  == NULLSPMAXPCOL) goto err;
      MoveObj(stack,1,(NspObject *) Res);
      nsp_spmaxpcolmatrix_destroy(As);
      return 1;
    }
  if ( B->m==1 && B->n == 1) 
    {
      if ( B->D[0]->size == 0 ) 
	{
	  if ((Res = nsp_spmaxpcolmatrix_div_zero_tt(As)) == NULLSPMAXPCOL) goto err;
	  nsp_spmaxpcolmatrix_destroy(As);
	  MoveObj(stack,1,(NspObject *) Res);
	}
      else 
	{
	  if (nsp_spmaxpcolmatrix_div_scal_tt(As,B) != OK) goto err;
	  MoveObj(stack,1,(NspObject *) As);
	}
      return 1;
    }
  if ( SameDim(A,B) ) 
    {
      NspSpMaxpColMatrix *Res;
      if ((Res=nsp_spmaxpcolmatrix_divel(As,B)) == NULLSPMAXPCOL) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      nsp_spmaxpcolmatrix_destroy(As);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
 err: 
  nsp_spmaxpcolmatrix_destroy(As);
  return RET_BUG;
}

/* As ./ A 
 *
 */

static int int_spmaxpcolmatrix_div_el_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A, *Bs=NULL,*Res=NULL;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) goto err;
  if ((B = GetMat(stack,2)) == NULLMAT) goto err;
  if ((Bs=nsp_spmaxpcolmatrix_from_mat(B)) == NULLSPMAXPCOL) goto err;

  if ( B->mn == 1) 
    {
      if ( Bs->D[0]->size == 0 ) 
	{
	  if ((Res = nsp_spmaxpcolmatrix_div_zero_tt(A)) == NULLSPMAXPCOL) goto err;
	  nsp_spmaxpcolmatrix_destroy(Bs);
	  MoveObj(stack,1,(NspObject *) Res);
	}
      else 
	{
	  if ((A = GetSpMaxpColCopy(stack,1)) == NULLSPMAXPCOL) goto err;
	  if (nsp_spmaxpcolmatrix_div_scal_tt(A,Bs) != OK) goto err;
	  nsp_spmaxpcolmatrix_destroy(Bs);
	  NSP_OBJECT(A)->ret_pos = 1;
	}
      return 1;
    }

  if ( A->m==1 && A->n == 1) 
    {
      /* A * <non-nul-scalar> **/
      if ((Res =nsp_spmaxpcolmatrix_scal_div_tt(A,Bs))  == NULLSPMAXPCOL) goto err;
      MoveObj(stack,1,(NspObject *) Res);
      nsp_spmaxpcolmatrix_destroy(Bs);
      return 1;
    }

  if ( SameDim(A,B) ) 
    {
      NspSpMaxpColMatrix *Res;
      if ((Res=nsp_spmaxpcolmatrix_divel(A,Bs)) == NULLSPMAXPCOL) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      nsp_spmaxpcolmatrix_destroy(Bs);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
 err:
  nsp_spmaxpcolmatrix_destroy(Bs);
  return RET_BUG;
}



/*
 *nsp_mat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspSpMaxpColMatrix *(*SuPro) (NspSpMaxpColMatrix *A, int dim);

static int int_spmaxpcolmatrix__sum(Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  int dim=0;
  NspSpMaxpColMatrix *Res,*HMat; 
  CheckRhs(1,2);
  CheckOptRhs(0, 1)
  CheckLhs(1,1);

  if ((HMat = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;

  if (rhs == 2)
    {
      if ( opt == 0 )
	{
	  if ( GetDimArg(stack, 2, &dim) == FAIL )
	    return RET_BUG;
	}
      else /* opt == 1 */
	{
	  nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &dim) == FAIL )
	    return RET_BUG;
 	}
 
     if ( dim == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(HMat);
    }

  if ((Res= (*F)(HMat,dim)) == NULLSPMAXPCOL) 
    return RET_BUG;

  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_spmaxpcolmatrix_isnan(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj=NULLOBJ;
  int dim=-1; /* default value isnan(A,dim='.') */
  NspSpMaxpColMatrix *Res,*HMat; 
  nsp_option opts[] ={{"dim",obj,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  /* and(A,dim=) */
  if ( get_optional_args(stack, rhs, opt, opts, &Obj) == FAIL )
    return RET_BUG;
  if ( Obj != NULL) 
    {
      if ( GetDimArg(stack, opts[0].position, &dim ) == FAIL ) return RET_BUG;
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(HMat);
    }
  if ((Res= nsp_spmaxpcolmatrix_isnan(HMat,dim)) == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_spmaxpcolmatrix_isinf(Stack stack, int rhs, int opt, int lhs)
{
  int dim=-1; /* default value isnan(A,dim='.') */
  NspObject *Obj=NULLOBJ;
  NspSpMaxpColMatrix *Res,*HMat; 
  nsp_option opts[] ={{"dim",obj,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  /* and(A,dim=) */
  if ( get_optional_args(stack, rhs, opt, opts, &Obj) == FAIL )
    return RET_BUG;
  if ( Obj != NULL) 
    {
      if ( GetDimArg(stack, opts[0].position, &dim ) == FAIL ) return RET_BUG;
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(HMat);
    }
  if ((Res= nsp_spmaxpcolmatrix_isinf(HMat,dim)) == NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_spmaxpcolmatrix_nnz(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat; 
  NspMatrix *Res;
  int  nnz;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((Res = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) return FAIL;

  nnz = nsp_spmaxpcolmatrix_nnz(HMat);
  Res->R[0] = (double) nnz;
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_spmaxpcolmatrix_sum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spmaxpcolmatrix__sum(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_sum) );
}


/*
 * nsp_mat_maxi: Maxi(*HMat);
 * A is unchanged 
 * XXXXXX : pas fini 
 */

typedef NspSpMaxpColMatrix *(*SpMiMax) (NspSpMaxpColMatrix *A,int dim,NspMatrix **Imax,int lhs);

static int int_spmaxpcolmatrix__maxi(Stack stack, int rhs, int opt, int lhs, SpMiMax F,int minmax)
{
  int dim=0;
  NspSpMaxpColMatrix *A,*M;
  NspMatrix *Imax;
  CheckRhs(1,2);  /* the form max(A1,A2,..) is currently limited to 2 matrices */
  CheckLhs(1,2);

  if ( rhs == 1 || (rhs - opt ) == 1 || ( rhs == 2 && IsSMatObj(stack,2)  ))
    {
      /* max(A), or max(A,str) or max(A,dim=options) idem for min */
      /* XXXXXX : Attention pas fini ici il faut un getrealsp         */ 
      /* en attendant je mets un test */
      if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL) return RET_BUG;
      if ( A->rc_type != 'r' )
	{ 
	  Scierror("Error:\t sparse matrix should real for function %s\n",NspFname(stack));
	  return RET_BUG;
	}
 
      if (rhs == 2 )
	{
	  if ( opt == 0 )
	    {
	      if ( GetDimArg(stack, 2, &dim) == FAIL )
		return RET_BUG;
	    }
	  else /* opt == 1 */
	    {
	      nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
				  { NULL,t_end,NULLOBJ,-1}};
	      if ( get_optional_args(stack, rhs, opt, opts, &dim) == FAIL )
		return RET_BUG;
	    }
	  if ( dim == -1 )
	    {
	      Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	      return RET_BUG;
	    }
	  if ( dim == -2 )  /* matlab compatibility flag */
	    dim = GiveMatlabDimFlag(A);
	}

      if (( M= (*F)(A,dim,&Imax,lhs)) == NULLSPMAXPCOL) return RET_BUG;

      MoveObj(stack,1,NSP_OBJECT(M));
      if ( lhs == 2)
	{
	  MoveObj(stack,2,NSP_OBJECT(Imax));
	}
    }
  else
    {
      /* max(A1,A2)   */
      int index= (lhs == 2 ) ? 1 : 0, err;
      NspSpMaxpColMatrix *Index=NULL;
      if ((A = GetSpMaxpColCopy(stack,1))  == NULLSPMAXPCOL) return RET_BUG;
      if ((M = GetSpMaxpCol(stack,2))  == NULLSPMAXPCOL) return RET_BUG;
      Index = nsp_spmaxpcolmatrix_maximinitt_g(A,M,index,minmax,&err);
      if (err == TRUE ) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      if ( lhs == 2 ) 
	{
	  MoveObj(stack,2,NSP_OBJECT(Index));
	}
    }
  return Max(lhs,1);
}


static int int_spmaxpcolmatrix_maxi(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spmaxpcolmatrix__maxi(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_maxi,1));
}

static int int_spmaxpcolmatrix_mini(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spmaxpcolmatrix__maxi(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_mini,-1));
  return RET_BUG;
}

/* interface for triu 
 *
 */

static int int_spmaxpcolmatrix_triu(Stack stack, int rhs, int opt, int lhs)
{
  int k=0;
  NspSpMaxpColMatrix *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( A->m==0 && A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
    }
  if ((A=GetSpMaxpColCopy(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( nsp_spmaxpcolmatrix_triu(A,k)==FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;

}

/* interface for tril
 *
 */

static int int_spmaxpcolmatrix_tril(Stack stack, int rhs, int opt, int lhs)
{
  int k=0;
  NspSpMaxpColMatrix *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( A->m==0 && A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
    }
  if ((A=GetSpMaxpColCopy(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( nsp_spmaxpcolmatrix_tril(A,k)==FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/* interface for eye,ones,zeros
 */

static int int_spmaxpcolmatrix_spones(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A, *Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ((Res= nsp_spmaxpcolmatrix_spones(A))== NULLSPMAXPCOL) return RET_BUG; 
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}


typedef NspSpMaxpColMatrix *(Feoz)(int m,int n);

static int int_spmaxpcolmatrix_eoz(Stack stack, int rhs, int opt, int lhs,Feoz *F)
{
  int m,n;
  NspSpMaxpColMatrix *A;
  CheckStdRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 1 ) 
    {
      m=nsp_object_get_size(NthObj(1),1);
      n=nsp_object_get_size(NthObj(1),2);
    }
  else 
    {
      if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
    }
  if ((A= (*F)(m,n))== NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1, NSP_OBJECT(A));
  return 1;
}

static int int_spmaxpcolmatrix_eye(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_eoz(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_eye);
}

static int int_spmaxpcolmatrix_zeros(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix_eoz(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_zeros);
}


static int int_spmaxpcolmatrix_clean(Stack stack, int rhs, int opt, int lhs)
{
  double epsa =DBL_EPSILON, epsr = DBL_EPSILON;
  NspSpMaxpColMatrix *A;
  CheckRhs (1, 3);
  CheckLhs (1, 1);
  if (rhs >= 2)
    {
      if (GetScalarDouble (stack, 2, &epsa) == FAIL)
	return RET_BUG;
    }
  if (rhs >= 3)
    {
      if (GetScalarDouble (stack, 3, &epsr) == FAIL)
	return RET_BUG;
    }
  if ((A = GetSpMaxpColCopy(stack, 1)) == NULL)
    return RET_BUG;
  nsp_spmaxpcolmatrix_clean (A, rhs, epsa, epsr);
  NSP_OBJECT (A)->ret_pos = 1;
  return 1;
}

static int int_spmaxpcolmatrix_spmprand(Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  char crand='u';
  double sparsity=0.1;
  int m,n;
  NspSpMaxpColMatrix *A;
  CheckRhs(2,4);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
  if ( rhs >= 3 )
    if (GetScalarDouble(stack,3,&sparsity) == FAIL) return RET_BUG;
  if ( rhs >= 4 )
    {
      if ((str=GetString(stack,4)) == NULL) return RET_BUG;
      if ( strlen(str) >= 1 ) crand=str[0];
    }
  if ((A= nsp_spmaxpcolmatrix_rand(m,n,sparsity,crand))== NULLSPMAXPCOL) return RET_BUG;
  MoveObj(stack,1, NSP_OBJECT(A));
  return 1;
}

/*
 *  A=op(A) 
 */

typedef int (*M11) (NspSpMaxpColMatrix *A);
typedef void (*VM11) (NspSpMaxpColMatrix *A);
 
static int int_spmaxpcolmatrix__gen11(Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspSpMaxpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( A->m==0 || A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpMaxpColCopy(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ((*F)(A) == FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

static int int_spmaxpcolmatrix__genv11(Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspSpMaxpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( A->m==0 || A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpMaxpColCopy(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  (*F)(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}




/*
 *nsp_spmatrix_minus: A=-(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmaxpcolmatrix_minus(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_minus);
}


/*
 * A=Abs(A), absolue value or module of each element 
 */

static int int_spmaxpcolmatrix_abs(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_abs);
}

/*
 * A=Erf(A)
 */

/*
 * A=Erfc(A),  * A is changed 
 */

/*
 * A=Arg(A),  * A is changed 
 */

static int int_spmaxpcolmatrix_arg(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_arg);
}

/*
 *nsp_spmatrix_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

typedef NspMatrix* (*SpM) (NspSpMaxpColMatrix *A);

static int int_spmaxpcolmatrix__m_gen11(Stack stack, int rhs, int opt, int lhs, SpM F)
{
  NspMatrix *Loc;
  NspSpMaxpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ((Loc = (*F)(A)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Loc);
  return 1;
}

static int int_spmaxpcolmatrix_cos(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_cos);
}

/*
 *nsp_spmatrix_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */
static int int_spmaxpcolmatrix_cosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_cosh);
}

/*
 * SpExpl : Exponentiation terme a term 
 * A is changed 
 */

static int int_spmaxpcolmatrix_expel(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_expel);
}

/*
 * SpLog : A=LogEl(A) 
 */

static int int_spmaxpcolmatrix_logel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( A->m==0 || A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpMaxpColCopy(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if (nsp_spmaxpcolmatrix_logel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_spmatrix_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmaxpcolmatrix_sin(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_sin);
}

/*
 *nsp_spmatrix_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmaxpcolmatrix_sinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_sinh);
}

/*
 *nsp_spmatrix_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

static int int_spmaxpcolmatrix_sqrtel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( A->m==0 || A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpMaxpColCopy(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if (nsp_spmaxpcolmatrix_sqrtel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_spmatrix_acos: A=Acos(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmaxpcolmatrix_acos(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_acos);
}

/*
 *nsp_spmatrix_acosh: A=Acosh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmaxpcolmatrix_acosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_acosh);
}

/*
 *nsp_spmatrix_asin: A=Asin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmaxpcolmatrix_asin(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_asin);
}

/*
 *nsp_spmatrix_asinh: A=Asinh(A)
 * A is changed  
 * return 0 if error 
 */


static int int_spmaxpcolmatrix_asinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_asinh);
}

/*
 * SpATan : res= arctang(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spmaxpcolmatrix_atan(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_atan);
}

/*
 * SpArcTangH : res= atanh(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spmaxpcolmatrix_atanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_atanh);
}

/*
 *nsp_spmatrix_ceil: A=Ceil(A)
 * A is changed  
 */

static int int_spmaxpcolmatrix_ceil(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_ceil);
}

/*
 *nsp_spmatrix_int: A=Int(A)
 * A is changed  
 */

static int int_spmaxpcolmatrix_int(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_int);
}

/*
 *nsp_spmatrix_floor: A=Floor(A)
 * A is changed  
 */
 
static int int_spmaxpcolmatrix_floor(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_floor);
}

/*
 *nsp_spmatrix_round: A=Round(A)
 * A is changed  
 */
 
static int int_spmaxpcolmatrix_round(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_round);
}

/*
 *nsp_spmatrix_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spmaxpcolmatrix_sign(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_sign);
}

/*
 *nsp_spmatrix_tan: A=Tan(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spmaxpcolmatrix_tan(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_tan);
}

/*
 *nsp_spmatrix_tanh: A=Tanh(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spmaxpcolmatrix_tanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_tanh);
}

/*
 * A=Polar(A,B),  * A is changed 
 */

/*
 *nsp_spmatrix_conj: A=real(A)-i*Imag(A)
 * A is changed  if imaginary not changed if real 
 */

static int int_spmaxpcolmatrix_conj(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpMaxpCol(stack,1))== NULLSPMAXPCOL) return RET_BUG;
  if ( (A->m == 0 || A->n == 0) || A->rc_type == 'r' )
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpMaxpColCopy(stack,1))== NULLSPMAXPCOL) return RET_BUG;
 nsp_spmaxpcolmatrix_conj(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * A=nsp_mat_pow_el(A,B), A.^ B 
 * with special cases Mat.^[]  and Mat.^scalar
 *                    [].^Mat ans scalar.^Mat
 */
/*
 * A=DivEl(A,B),  A ./ B 
 */
/*
 * A=BackDivEl(A,B),  A .\ B 
 */
/*
 * A=MultEl(A,B),  A .* B 
 */

/*
 * Matrix multiplication  Res= A*B  
 * with special cases Mat * [] and Mat * scalar
 * very similar to mopscal but MatMult returns a new matrix 
 */

/*
 * returns in a Matrix the indices for which the Matrix is true 
 */

static int int_spmaxpcolmatrix_find(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMatrix *Rc,*Rr,*V;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((A = GetSpMaxpCol(stack,1)) == NULLSPMAXPCOL)  return RET_BUG;
  if (nsp_spmaxpcolmatrix_find(A,Max(lhs,1),&Rr,&Rc,&V) == FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Rr));
  if ( lhs >= 2 )
    {
      NthObj(2) = (NspObject *) Rc;
      NthObj(2)->ret_pos = 2;
    }
  if ( lhs >= 3)
    {
      NthObj(3) = (NspObject *) V;
      NthObj(3)->ret_pos = 3;
    }
  return Max(lhs,1);
}

static int int_spmaxpcolmatrix_real(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_realpart);
}

static int int_spmaxpcolmatrix_imag(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmaxpcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spmaxpcolmatrix_imagpart);
}


/*
 * checks is matrix is real
 */

static int int_spmaxpcolmatrix_isreal (Stack stack, int rhs, int opt, int lhs)
{
  int strict = FALSE;
  NspSpMaxpColMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if ((HMat = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;
  if (rhs==2) 
    {
       if ( GetScalarBool (stack,2,&strict) == FAIL) return RET_BUG;
    }
  if ( nsp_move_boolean(stack,1,nsp_spmaxpcolmatrix_isreal(HMat,strict)) == FAIL)
    return RET_BUG;
  return 1;
}
 
/* norm 1 and inf 
 *
 */

static int int_spmaxpcolmatrix_norm( Stack stack, int rhs, int opt, int lhs)
{
  double norm, p=2.0;
  int id=1, is_vector;
  const char *norm_table[] =       {"1","2","inf","fro","Inf","Fro","M",NULL};
  char norm_lapack_table[] = {'1','2','I'  ,'F'  ,'I'  ,'F'  ,'M'};
  NspSpMaxpColMatrix *A;

  CheckRhs(1,2);
  CheckLhs(0,1);

  if ( (A=GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL ) return RET_BUG;
  is_vector = A->m==1 || A->n==1;
  
  if (rhs == 2)
    {
      if (IsMatObj(stack,2))
	{
	  if ( GetScalarDouble(stack, 2, &p) == FAIL ) return RET_BUG; 
	  if ( is_vector )
	    {
	      if ( !(p >= 1.0) )   /* to detect also nan */ 
		{ 
		  Scierror("%s: second argument must be >= 1 and not equal to %%nan\n",NspFname(stack));
		  return RET_BUG;
		}
	    }
	  else  /* A is a matrix */
	    {
	      if ( !(p==1.0 || p==2.0 || isinf(p)) )
		{ 
		  Scierror("%s: second argument must be 1, 2 or %%inf \n",NspFname(stack));
		  return RET_BUG;
		}
	      if ( isinf(p) ) id = 2; else id = floor(p)-1;
	    }
	}
      else if ( IsSMatObj(stack,2))
	{
	  if ( (id=GetStringInArray(stack,2,norm_table,1)) == -1) return RET_BUG; 
	  if ( is_vector ) /* define p (p is initialised with 2 so corresponding to id == 1 || id == 3 || id == 5)  */
	    {
	      if ( id == 0 ) p = 1.0;
	      else if ( id == 2 || id == 4 || id == 6 ) p = 1.0/(2.0 - p);  /* got an Inf */
	    }
	}
      else
	{
	  Scierror("%s: second argument must be 1,2,%%inf or '1','2','inf','fro','Inf','Fro','M' \n      (or any real >= 1 for a vector)\n",
		   NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( is_vector )
    {
      norm = nsp_spmaxpcolmatrix_vnorm(A, p);
    }
  else
    {
      norm = nsp_spmaxpcolmatrix_norm(A,norm_lapack_table[id]);
    }
  if ( norm < 0 ) return RET_BUG;  /* in some cases a work array must be allocated  */ 
                                   /* and if this fails, nsp_xxxx_norm return -1.0  */

  if ( nsp_move_double(stack,1,norm) == FAIL ) return RET_BUG;
  return Max(lhs,1);
}

/*
 * checks that a sparse matrix is empty (this is defined in object.c but 
 * the implementation in object.c uses the "mn" field which can be 
 * corrupted for "big" sparse, so we define isempty here using "m" and
 * "n" fields)
 */

static int int_spmaxpcolmatrix_isempty (Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;
  if ( nsp_move_boolean(stack,1, HMat->m==0 || HMat->n==0 ) == FAIL)
    return RET_BUG;
  return 1;
}
 
/*
 * checks that a sparse matrix is a scalar (this is defined in object.c but 
 * the implementation in object.c uses the "mn" field which can be 
 * corrupted for "big" sparse, so we define isscalar here using "m" and
 * "n" fields)
 */

static int int_spmaxpcolmatrix_isscalar (Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;
  if ( nsp_move_boolean(stack,1, HMat->m==1 && HMat->n==1 ) == FAIL)
    return RET_BUG;
  return 1;
}

 /**
 * check that a sparse matrix is a vector
 * 
 **/
static int int_spmaxpcolmatrix_isvector(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ((HMat = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;
  if ( nsp_move_boolean(stack,1, HMat->m==1 || HMat->n==1 ) == FAIL)
    return RET_BUG;
  return 1;
}
 
/*
 * computes length or numel of a sparse matrix (because the version
 * defined in object.c uses the mn field and this last could overflow)
 */
static int int_spmaxpcolmatrix_numel (Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;
  if ( nsp_move_double(stack,1,((double) HMat->m)*((double) HMat->n)) == FAIL)
    return RET_BUG;
  return 1;
}
 
/**
 * check that a sparse matrix is a lower or upper triangular
 * 
 **/
static int int_spmaxpcolmatrix_istriangular(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat;
  char *str;
  Boolean rep;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ((HMat = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;
  if ((str=GetString(stack,2)) == NULL) return RET_BUG;
  if ( strcmp(str,"u") == 0 )
    rep = nsp_spmaxpcolmatrix_is_upper_triangular(HMat);
  else if ( strcmp(str,"l") == 0 )
    rep = nsp_spmaxpcolmatrix_is_lower_triangular(HMat);
  else
    { 
      Scierror("%s: second argument must be 'l' or 'u'\n",NspFname(stack));
      return RET_BUG;
    }
  if ( nsp_move_boolean(stack,1,rep) == FAIL ) 
    return RET_BUG;
  return 1;
}
/**
 * check that a sparse matrix is a lower or upper triangular
 * 
 **/
static int int_spmaxpcolmatrix_issymmetric(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *HMat;
  Boolean rep;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ((HMat = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;

  rep = nsp_spmaxpcolmatrix_is_symmetric(HMat);

  if ( nsp_move_boolean(stack,1,rep) == FAIL ) 
    return RET_BUG;
  return 1;
}

/**
 * compute lower and upper bandwidth
 * 
 **/
static int int_spmaxpcolmatrix_lower_upper_bandwidth(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  int kl, ku;
  CheckRhs (1, 1);
  CheckLhs (1, 2);

  if ((A = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;

  if ( nsp_spmaxpcolmatrix_lower_and_upper_bandwidth(A, &kl, &ku) == FAIL )
    { 
      Scierror("%s: is only implemented for square matrix\n",NspFname(stack));
      return RET_BUG;
    }

  if ( nsp_move_double(stack,1,(double) kl) == FAIL ) 
    return RET_BUG;
  if ( nsp_move_double(stack,2,(double) ku) == FAIL ) 
    return RET_BUG;
  return Max(lhs,1);
}

static int int_spmaxpcolmatrix_solve_tri(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMatrix *b, *x=NULLMAT;
  char *str, type;
  int rep;

  CheckRhs (3, 3);
  CheckLhs (1, 1);

  if ((A = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)   return RET_BUG;
  CheckSquare(NspFname(stack),1,A);

  if ((b = GetMat(stack, 2)) == NULLMAT)   return RET_BUG;
  if ( b->m != A->m )
    { 
      Scierror("%s: second argument is incompatible\n",NspFname(stack));
      return RET_BUG;
    }

  if ((str=GetString(stack,3)) == NULL) return RET_BUG;
  if ( strcmp(str,"u") != 0  &&  strcmp(str,"l") != 0 )
    { 
      Scierror("%s: third argument must be 'l' or 'u'\n",NspFname(stack));
      return RET_BUG;
    }

  type = A->rc_type == 'r' && b->rc_type == 'r' ? 'r' : 'c';
  if ( (x = nsp_matrix_create(NVOID, type, b->m, b->n)) == NULLMAT )
    return RET_BUG;

  if ( strcmp(str,"u") == 0 )
    rep = nsp_spmaxpcolmatrix_solve_utri(A, x, b);
  else
    rep = nsp_spmaxpcolmatrix_solve_ltri(A, x, b);

  if ( rep != 0 )
    { 
      Scierror("%s: non inversible matrix, zero diagonal element at line %d\n",NspFname(stack),rep);
      nsp_matrix_destroy(x);
      return RET_BUG;
    }

  MoveObj(stack,1,NSP_OBJECT(x));
  return 1;
}

/* 
 *  B = scale_rows(A,x)  (exists as a method but useful as a function too)
 *
 */
static int int_spmaxpcolmatrix_scale_rows(Stack stack,int rhs,int opt,int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMatrix *x;
  char *op=NULL; char ope='*'; 
  nsp_option opts[] ={{"op",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(2, 2);
  CheckOptRhs(0, 1)
  CheckLhs(1,1);

  if ((A = GetSpMaxpColCopy (stack, 1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((x = GetMat (stack, 2)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ( x->mn != A->m )
    { 
      Scierror("%s: incompatible dimensions between first and second arguments \n",NspFname(stack));
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &op) == FAIL )
    return RET_BUG;

  if ( op != NULL) 
    {
      if ( strcmp(op,"*") == 0 )
	ope = '*';
      else if ( strcmp(op,"/") == 0 )
	ope = '/';
      else
	{
	  Scierror("%s: optional named arg op should be set to '*' or '/'\n",NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( nsp_spmaxpcolmatrix_scale_rows(A, x, ope) == FAIL )
    return RET_BUG;

  NSP_OBJECT(A)->ret_pos = 1; 

  return 1;
}

/* 
 *  B = scale_cols(A,x)  (exists as a method but useful as a function too)
 *
 */
static int int_spmaxpcolmatrix_scale_cols(Stack stack,int rhs,int opt,int lhs)
{
  NspSpMaxpColMatrix *A;
  NspMatrix *x;
  char *op=NULL; char ope='*'; 
  nsp_option opts[] ={{"op",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(2, 2);
  CheckOptRhs(0, 1)
  CheckLhs(1,1);

  if ((A = GetSpMaxpColCopy (stack, 1)) == NULLSPMAXPCOL) return RET_BUG;
  if ((x = GetMat (stack, 2)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ( x->mn != A->n )
    { 
      Scierror("%s: incompatible dimensions between first and second arguments \n",NspFname(stack));
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &op) == FAIL )
    return RET_BUG;

  if ( op != NULL) 
    {
      if ( strcmp(op,"*") == 0 )
	ope = '*';
      else if ( strcmp(op,"/") == 0 )
	ope = '/';
      else
	{
	  Scierror("%s: optional named arg op should be set to '*' or '/'\n",NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( nsp_spmaxpcolmatrix_scale_cols(A, x, ope) == FAIL )
    return RET_BUG;

  NSP_OBJECT(A)->ret_pos = 1; 

  return 1;
}

static int int_spmaxpcolmatrix_kron(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMaxpColMatrix *A, *B, *C;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetSpMaxpCol(stack, 1)) == NULLSPMAXPCOL)
    return RET_BUG;
  if ((B = GetSpMaxpCol(stack, 2)) == NULLSPMAXPCOL)
    return RET_BUG;
  if ((C = nsp_spmaxpcolmatrix_kron(A,B)) == NULLSPMAXPCOL)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) C);
  return 1;
}


/*
 * The Interface for basic numerical sparse matrices operation 
 * we use sp for spmaxpcol 
 */

static OpTab SpMaxpColMatrix_func[]={
  {"spmp_create",int_spmaxpcolmatrix_create},
  {"spmaxpcol_create",int_spmaxpcolmatrix_create},
  {"mp2spmp",int_spmaxpcolmatrix_mp2spmp},
  {"spmp2mp",int_spmaxpcolmatrix_spmp2mp},
  {"dst_spmp_spmp",int_spmaxpcolmatrix_multt},
  {"dst_mp_spmp",int_spmaxpcolmatrix_multt_m_sp},
  {"dst_spmp_mp",int_spmaxpcolmatrix_multt_sp_m},
  {"dsl_spmp_spmp",int_spmaxpcolmatrix_div_el},
  {"dsl_mp_spmp",int_spmaxpcolmatrix_div_el_m_sp},
  {"dsl_spmp_mp",int_spmaxpcolmatrix_div_el_sp_m},
  {"mult_spmp_spmp",int_spmaxpcolmatrix_mult},
  {"mult_spmp_mp",int_spmaxpcolmatrix_mult_sp_m},
  {"pmult_spmp_mp",int_spmaxpcolmatrix_pmult_sp_m},
  {"mult_mp_spmp",int_spmaxpcolmatrix_mult_m_sp},
  {"plus_spmp_spmp",int_spmaxpcolmatrix_plus},
  {"plus_mp_spmp",int_spmaxpcolmatrix_plus_m_sp},
  {"plus_spmp_mp",int_spmaxpcolmatrix_plus_sp_m},
  {"minus_spmp_spmp",int_spmaxpcolmatrix_sub},
  {"minus_spmp_mp",int_spmaxpcolmatrix_sub_sp_m},
  {"minus_mp_spmp",int_spmaxpcolmatrix_sub_m_sp},
  {"minus_spmp",int_spmaxpcolmatrix_minus},
  {"quote_spmp",int_spmaxpcolmatrix_quote},
  {"dprim_spmp", int_spmaxpcolmatrix_dquote},
  {"multt_spmp_spmp",int_spmaxpcolmatrix_multt}, 
  {"redim_spmp",int_spmaxpcolmatrix_redim},
  {"matrix_spmp",int_spmaxpcolmatrix_redim},
  {"reshape_spmp",int_spmaxpcolmatrix_redim},
  {"resize2vect_spmp", int_spmaxpcolmatrix_resize2vect},
  {"concatd_spmp_spmp" ,  int_spmaxpcolmatrix_concatd },
  {"concatr_spmp_spmp" ,  int_spmaxpcolmatrix_concatr },
  {"concatdiag_spmp_spmp" ,  int_spmaxpcolmatrix_concatdiag },
  {"concatd_spmp_mp" ,  int_spmaxpcolmatrix_concatd_sp_m },
  {"concatr_spmp_mp" ,  int_spmaxpcolmatrix_concatr_sp_m },
  {"concatdiag_spmp_mp" ,  int_spmaxpcolmatrix_concatdiag_sp_m },
  {"concatd_mp_spmp" ,  int_spmaxpcolmatrix_concatd_m_sp },
  {"concatr_mp_spmp" ,  int_spmaxpcolmatrix_concatr_m_sp },
  {"concatdiag_mp_spmp" ,  int_spmaxpcolmatrix_concatdiag_m_sp },
  {"deletecols_spmp", int_spmaxpcolmatrix_deletecols},
  {"deleterows_spmp", int_spmaxpcolmatrix_deleterows},
  {"extract_spmp",int_spmaxpcolmatrix_extract},
  {"extractrows_spmp",int_spmaxpcolmatrix_extractrows},
  {"extractcols_spmp",int_spmaxpcolmatrix_extractcols},
  {"diage_spmp" ,  int_spmaxpcolmatrix_diage },
  {"diage_spmp_mp" ,  int_spmaxpcolmatrix_diage },
  /* {"diagset_spmp" ,  int_spmaxpcolmatrix_diagset }, */
  {"diagcre_spmp" ,  int_spmaxpcolmatrix_diagcre },
  {"diagcre_spmp_mp" ,  int_spmaxpcolmatrix_diagcre },
  {"diag_spmp", int_spmaxpcolmatrix_diag},
  {"diag_spmp_mp", int_spmaxpcolmatrix_diag},
  {"sparsemp", int_spmaxpcolmatrix_sparse},  /* maxplus sparse from m */
  {"sparse_mp", int_spmaxpcolmatrix_sparse}, /* maxplus sparse from mp */
  {"spget_spmp", int_spmaxpcolmatrix_get},
  {"spmpfrommtlb",int_spmaxpcolmatrix_from_mtlb},
  {"spmpget_mtlb", int_spmaxpcolmatrix_get_mtlb},
  {"full_spmp",int_spmaxpcolmatrix_spmp2mp},
  {"sum_spmp_s" ,  int_spmaxpcolmatrix_sum },
  {"sum_spmp" ,  int_spmaxpcolmatrix_sum },
  {"setrowscols_spmp",int_spmaxpcolmatrix_setrc},
  {"max_spmp" ,  int_spmaxpcolmatrix_maxi },
  {"max_spmp_s" ,  int_spmaxpcolmatrix_maxi },
  {"min_spmp" ,  int_spmaxpcolmatrix_mini },
  {"min_spmp_s" ,  int_spmaxpcolmatrix_mini },
  {"extractelts_spmp",int_spmaxpcolmatrix_extractelts},
  {"nnz_spmp",int_spmaxpcolmatrix_nnz},
  {"triu_spmp", int_spmaxpcolmatrix_triu},
  {"tril_spmp", int_spmaxpcolmatrix_tril},
  {"spmp_eye", int_spmaxpcolmatrix_eye}, /* to be removed ? */
  {"spmp_ones", int_spmaxpcolmatrix_spones},/* to be removed ? */
  {"spmp_zeros", int_spmaxpcolmatrix_zeros},/* to be removed ? */
  {"spmpeye", int_spmaxpcolmatrix_eye},
  {"spmpones", int_spmaxpcolmatrix_spones},
  {"spmpzeros", int_spmaxpcolmatrix_zeros},
  /* now operations */
  {"abs_spmp",int_spmaxpcolmatrix_abs},
  {"arg_spmp",int_spmaxpcolmatrix_arg},
  {"sin_spmp",int_spmaxpcolmatrix_sin},
  {"sinh_spmp",int_spmaxpcolmatrix_sinh},
  {"asin_spmp",int_spmaxpcolmatrix_asin},
  {"asinh_spmp",int_spmaxpcolmatrix_asinh},
  {"cos_spmp",int_spmaxpcolmatrix_cos},
  {"cosh_spmp",int_spmaxpcolmatrix_cosh},
  {"acos_spmp",int_spmaxpcolmatrix_acos},
  {"acosh_spmp",int_spmaxpcolmatrix_acosh},
  {"atan_spmp",int_spmaxpcolmatrix_atan},
  {"atanh_spmp",int_spmaxpcolmatrix_atanh},
  {"ceil_spmp",int_spmaxpcolmatrix_ceil},
  {"int_spmp",int_spmaxpcolmatrix_int},
  {"floor_spmp",int_spmaxpcolmatrix_floor},
  {"round_spmp",int_spmaxpcolmatrix_round},
  {"sign_spmp",int_spmaxpcolmatrix_sign},
  {"tan_spmp",int_spmaxpcolmatrix_tan},
  {"tanh_spmp",int_spmaxpcolmatrix_tanh},
  {"conj_spmp",int_spmaxpcolmatrix_conj},
  {"find_spmp", int_spmaxpcolmatrix_find},
  {"sqrt_spmp",int_spmaxpcolmatrix_sqrtel},
  {"log_spmp",int_spmaxpcolmatrix_logel},
  {"exp_spmp",int_spmaxpcolmatrix_expel},
  {"spmprand",int_spmaxpcolmatrix_spmprand},  
  {"clean_spmp",int_spmaxpcolmatrix_clean},  
  {"real_spmp", int_spmaxpcolmatrix_real},
  {"imag_spmp", int_spmaxpcolmatrix_imag},
  {"isreal_spmp", int_spmaxpcolmatrix_isreal},
  {"dstd_spmp_spmp", int_spmaxpcolmatrix_kron},	/* operator:  .*. */
  {"kron_spmp_spmp", int_spmaxpcolmatrix_kron},
  {"norm_spmp", int_spmaxpcolmatrix_norm},
  {"isnan_spmp",int_spmaxpcolmatrix_isnan},
  {"isinf_spmp",int_spmaxpcolmatrix_isinf},
  {"isempty_spmp",int_spmaxpcolmatrix_isempty},
  {"isscalar_spmp",int_spmaxpcolmatrix_isscalar},
  {"isvector_spmp",int_spmaxpcolmatrix_isvector},
  {"istriangular_spmp",int_spmaxpcolmatrix_istriangular},
  {"issymmetric_spmp",int_spmaxpcolmatrix_issymmetric},
  {"lower_upper_bandwidths_spmp", int_spmaxpcolmatrix_lower_upper_bandwidth},
  {"solve_tri_spmp",int_spmaxpcolmatrix_solve_tri},
  {"length_spmp",int_spmaxpcolmatrix_numel},
  {"numel_spmp",int_spmaxpcolmatrix_numel},
  {"scale_rows_spmp_mp",int_spmaxpcolmatrix_scale_rows},
  {"scale_cols_spmp_mp",int_spmaxpcolmatrix_scale_cols},
  {(char *) 0, NULL}
};

/*
 * The Interface for basic matrices operation
 */

int SpMaxpColMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SpMaxpColMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void SpMaxpColMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SpMaxpColMatrix_func[i].name;
  *f = SpMaxpColMatrix_func[i].fonc;
}




 



