/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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

#define SpColMatrix_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

/*
 * NspSpColMatrix inherits from NspObject 
 */

int nsp_type_spcolmatrix_id=0;
NspTypeSpColMatrix *nsp_type_spcolmatrix=NULL;
int nsp_type_spcolmatrix_init();

NspTypeSpColMatrix *new_type_spcolmatrix(type_mode mode)
{
  NspTypeSpColMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_spcolmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_spcolmatrix;
    }
  if ((type =  malloc(sizeof(NspTypeSpColMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* spcolmatrix_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = spcolmatrix_get_methods;
  type->new = (new_func *) new_spcolmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for spcolmatrix */ 

  top->pr = (print_func *)nsp_spcolmatrix_print;                  /* printing*/   
  top->dealloc = (dealloc_func *)nsp_spcolmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_spcolmatrix_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_spcolmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_spcolmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_spcolmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_spcolmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) SpColMatIsTrue;*/      /* check if object can be considered as true */  
  /*top->loop =(loop_func *) SpColLoopExtract ; */               /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_spcolmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_spcolmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_spcolmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_spcolmatrix_xdr_save;
  top->load  = (load_func *)nsp_spcolmatrix_xdr_load;

  /* specific methods for spcolmatrix */
  type->init = (init_func *) init_spcolmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_spcolmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_spcolmatrix
       */
      type->id =  nsp_type_spcolmatrix_id = nsp_new_type_id();
      nsp_type_spcolmatrix = type;
      if ( nsp_register_type(nsp_type_spcolmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_spcolmatrix(mode); 
    }
  else 
    {
      type->id = nsp_type_spcolmatrix_id;
      return type;
    }
}
/*
 * initialize SpColmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_spcolmatrix(NspSpColMatrix *o,NspTypeSpColMatrix *type)
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

NspSpColMatrix *new_spcolmatrix() 
{
  NspSpColMatrix *loc; 
  /* type must exists */
  nsp_type_spcolmatrix = new_type_spcolmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspSpColMatrix)))== NULLSPCOL) return loc;
  /* initialize object */
  if ( init_spcolmatrix(loc,nsp_type_spcolmatrix) == FAIL) return NULLSPCOL;
  return loc;
}


/*
 *nsp_spcolmatrix_size: returns filled,hsize,or hsize 
 */

int nsp_spcolmatrix_size(NspSpColMatrix *H, int flag)
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

static char sp_type_name[]="SpColMat";
static char sp_short_type_name[]="sp";

char *nsp_spcolmatrix_type_as_string(void)
{
  return(sp_type_name);
}

char *nsp_spcolmatrix_type_short_string(NspObject *v)
{
  return(sp_short_type_name);
}

int nsp_spcolmatrix_fullcomp(NspSpColMatrix * A,NspSpColMatrix * B,char *op,int *err)
{
  Scierror("SpMatFullComp: to be implemented \n");
  return FALSE;
}

int nsp_spcolmatrix_eq(NspObject *a, NspObject *b)
{
  int j, k;
  NspSpColMatrix *A, *B;
  if ( check_cast(b,nsp_type_spcolmatrix_id) == FALSE) return FALSE ;

  /* quick code by Bruno (I want to test sparse(ij,val,..) function */
  A = (NspSpColMatrix *) a; B = (NspSpColMatrix *) b; 
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

int nsp_spcolmatrix_neq(NspObject *A, NspObject *B)
{
  return ( nsp_spcolmatrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
}


/*
 * Save a Matrix in a file stream 
 */

static int nsp_spcolmatrix_xdr_save(XDR *xdrs, NspSpColMatrix *M)
{
  int rep=FAIL;
  NspMatrix *RC=NULL, *Values=NULL;
  if ( nsp_spcolmatrix_get(M,&RC,&Values) == FAIL) goto fail;
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) goto fail;
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

static NspSpColMatrix *nsp_spcolmatrix_xdr_load(XDR *xdrs)
{
  int m,n;
  NspObject *RC=NULL, *Values=NULL;
  NspSpColMatrix *Loc;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSPCOL;
  if (nsp_xdr_load_i(xdrs, &m) == FAIL)  return NULLSPCOL;
  if (nsp_xdr_load_i(xdrs, &n) == FAIL)  return NULLSPCOL;
  if ( (RC= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSPCOL;
  if ( (Values= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSPCOL;
  if ((Loc = nsp_spcolmatrix_sparse(name,(NspMatrix *)RC,(NspMatrix *)Values,m,n)) == NULLSPCOL) return NULLSPCOL;
  if ( RC != NULL) nsp_object_destroy(&RC);
  if ( Values != NULL) nsp_object_destroy(&Values);
  return Loc;
}


/*
 * A =nsp_spcolmatrix_object(O);
 * checks that O is an object of NspSpColMatrix type. 
 * or a Hobj which points to an object of type SpColMatrix
 * if so, returns a pointer to that NspSpColMatrix and else returns NULL
 */

NspSpColMatrix   *nsp_spcolmatrix_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_spcolmatrix_id) == TRUE) return ((NspSpColMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_spcolmatrix));
  return(NULL);
}


/*
 * IsSpMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  SpColMatrix 
 * or a Hobj which points to an object of type SpColMatrix
 */

int IsSpColMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_spcolmatrix_id);
}

/*
 * IsSpColMat(O)
 * only checks that object is an object of type  SpColMatrix 
 * or a Hobj which points to an object of type SpColMatrix
 */

int IsSpColMat(const NspObject *O)
{
  return nsp_object_type(O , nsp_type_spcolmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a Sp and returns that Sp  
 * or a copy of that Sp if its name 
 * is != NVOID 
 */

NspSpColMatrix *GetSpColCopy(Stack stack, int i)
{
  if (  GetSpCol(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a SpColMatrix and returns that SpColMatrix  
 */

NspSpColMatrix *GetSpCol(Stack stack, int i)
{
  NspSpColMatrix *M;
  if (( M =nsp_spcolmatrix_object(NthObj(i))) == NULLSPCOL )
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a SpColMatrix and returns that SpColMatrix  
 */

NspSpColMatrix *GetRealSpCol(Stack stack, int i)
{
  NspSpColMatrix *M;
  if (( M =nsp_spcolmatrix_object(NthObj(i))) == NULLSPCOL )
    ArgMessage(stack,i);
  if (M->rc_type == 'c')
    {
      Scierror ("\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should not be complex\n", NspFname(stack));
      return NULLSPCOL ;
    }
  return M;
}


/*
 *    spcolmatrix methods
 */


/* 
 *  scale_rows[x]
 *
 *    A <- diag(x)*A
 *
 *    A.scale_rows[x]
 */
static int int_meth_spcolmatrix_scale_rows(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspSpColMatrix *A = (NspSpColMatrix *) self;
  NspMatrix *x;
  CheckLhs(0,0);
  CheckRhs(1,1);

  if ((x = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ( x->mn != A->m )
    { 
      Scierror("%s: the argument should have %d components \n",NspFname(stack),A->m);
      return RET_BUG;
    }

  if ( nsp_spcolmatrix_scale_rows(A, x) == FAIL )
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

static int int_meth_spcolmatrix_scale_cols(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspSpColMatrix *A = (NspSpColMatrix *) self;
  NspMatrix *x;
  CheckLhs(0,0);
  CheckRhs(1,1);

  if ((x = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ( x->mn != A->n )
    { 
      Scierror("%s: the argument should have %d components \n",NspFname(stack),A->n);
      return RET_BUG;
    }

  if ( nsp_spcolmatrix_scale_cols(A, x) == FAIL )
    return RET_BUG;

  return 0;
}

/* 
 * get_nnz 
 */
static int int_meth_spcolmatrix_get_nnz(void *self, Stack stack,int rhs,int opt,int lhs)
{
  CheckLhs(0,1);
  CheckRhs(0,0);
  if ( nsp_move_double(stack,1, nsp_spcolmatrix_nnz((NspSpColMatrix *) self)) == FAIL) return RET_BUG;
  return 1;
}

static int int_meth_spcolmatrix_set_diag(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspSpColMatrix *Diag;
  int k=0;
  CheckRhs (1,2);
  CheckLhs (0,0); 
  if ((Diag = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;
  if ( rhs == 2 )
    {
      if (GetScalarInt (stack,2 , &k) == FAIL)   return RET_BUG;
    }
  if (nsp_spcolmatrix_set_diag ((NspSpColMatrix *) self, Diag, k) != OK)
    return RET_BUG;
  return 0;
}

static NspMethods spcolmatrix_methods[] = {
  { "scale_rows",int_meth_spcolmatrix_scale_rows}, 
  { "scale_cols",int_meth_spcolmatrix_scale_cols}, 
  { "get_nnz", int_meth_spcolmatrix_get_nnz},
  { "set_diag", int_meth_spcolmatrix_set_diag},
  { (char *) 0, NULL}
};

static NspMethods *spcolmatrix_get_methods(void) { return spcolmatrix_methods;};



typedef int (*SpC) (NspSpColMatrix *A,NspSpColMatrix *B);


/*
 * Now the interfaced function for basic sparse operations
 */

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPCOLon failure 
 * The matrix is created with no initial value 
 */

static int int_spcolmatrix_create(Stack stack, int rhs, int opt, int lhs)
{  
  int m1,n1;
  NspSpColMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ((HMat =nsp_spcolmatrix_create(NVOID,'r',m1,n1) ) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1, (NspObject *) HMat);
  return 1;
}

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPCOLon failure 
 * sparse(rc,values [, [m,n]])
 * sparse(m,n) 
 * 
 */

static int int_spcolmatrix_m2sp(Stack stack, int rhs, int opt, int lhs);

static int int_spcolmatrix_sparse(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpColMatrix *A;
  NspMatrix *RC,*Values,*MN;
  if ( rhs == 1) 
    return  int_spcolmatrix_m2sp(stack,rhs,opt,lhs);
  CheckRhs(2,3);
  CheckLhs(1,1);
  if ((RC = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
/*   if ((RC = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG; */
  if ( RC->mn == 1) 
    {
      /* special case sparse(m,n) */
      int m = RC->R[0], n;
      if ( GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
      if ((A =nsp_spcolmatrix_create(NVOID,'r',m,n) ) == NULLSPCOL) return RET_BUG;
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
      if (( A =nsp_spcolmatrix_sparse(NVOID,RC,Values,(int) MN->R[0],(int) MN->R[1])) == NULLSPCOL) 
	return RET_BUG;
    }
  else
    {
      if (( A =nsp_spcolmatrix_sparse(NVOID,RC,Values,-1,-1)) == NULLSPCOL) 
	return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}

static int int_spcolmatrix_sparse_sp(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((A = GetSpColCopy(stack,1)) == NULLSPCOL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos=1;
  return Max(lhs,1);
}

/*                                                                              
 * return [rc,values,[m,n]] from                                                
 * a sparse matrix.                                                             
 * This could be a method                                                       
 */

static int int_spcolmatrix_get(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpColMatrix *A;
  NspMatrix *RC,*Values,*MN=NULL;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ( nsp_spcolmatrix_get(A,&RC,&Values) == FAIL) return RET_BUG;
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
static int int_spcolmatrix_get_mtlb(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *Jc=NULL,*Ir=NULL,*Pr=NULL;
  int nzmax,i;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ( nsp_spcol_set_triplet_from_m(A,TRUE) == FAIL) return RET_BUG;
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
  nsp_spcol_free_triplet(A);
  MoveObj(stack,1,NSP_OBJECT(Jc));
  return Max(lhs,1);
 err:
  nsp_matrix_destroy(Jc);
  nsp_matrix_destroy(Ir);
  nsp_matrix_destroy(Pr);
  if (A->convert == 't') 
    nsp_spcol_free_triplet(A);
  return RET_BUG;
}

/* XXX: remains to check that arguments are 
 * correct 
 *
 */

static int int_spcolmatrix_from_mtlb(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
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
  if ((A =nsp_spcolmatrix_create(NVOID,Pr->rc_type,Mn->R[0],Mn->R[1]) ) == NULLSPCOL) return RET_BUG;
  if ( nsp_spcol_alloc_col_triplet(A,Jc->R[Jc->mn-1])== FAIL) return RET_BUG;
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
  if ( nsp_spcol_update_from_triplet(A) == FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(A));
  return Max(lhs,1);
}




/*
 * Matredim : changes Matrix dimensions
 * but keeps m*n constant
 */

static int int_spcolmatrix_redim(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspSpColMatrix  *A;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if (( A =nsp_spcolmatrix_redim(A,m1,n1)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * Right Concatenation 
 * A= [A,B] 
 */


static int int_spcolmatrix_concat_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpColMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpCol(stack,1))  == NULLSPCOL) return RET_BUG;
  if ( HMat1->m ==0 && HMat1->n == 0) 
    {
      /* return 2 */
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( HMat2->m == 0 && HMat2->n == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetSpColCopy(stack,1))  == NULLSPCOL) return RET_BUG;
      if ( (*F)(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}


static int int_spcolmatrix_concatr(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spcolmatrix_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spcolmatrix_concatdiag(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatdiag);
}

/* [Sp op Full]
 *
 */

static int int_spcolmatrix_concat_sp_m_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpColMatrix *HMat1,*B;
  NspMatrix *HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpCol(stack,1))  == NULLSPCOL) return RET_BUG;
  if ((HMat2 = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( HMat1->m ==0 && HMat1->n == 0) 
    {
      /* return 2 */
      if ((B=nsp_spcolmatrix_from_mat(HMat2)) == NULLSPCOL) return RET_BUG;
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
      if ((HMat1 = GetSpColCopy(stack,1))  == NULLSPCOL) return RET_BUG;
      if ((B=nsp_spcolmatrix_from_mat(HMat2)) == NULLSPCOL) return RET_BUG;
      if ( (*F)(HMat1,B)!= OK) 
	{
	  nsp_spcolmatrix_destroy(B);
	  return RET_BUG;
	}
      nsp_spcolmatrix_destroy(B);
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}

static int int_spcolmatrix_concatr_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_sp_m_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spcolmatrix_concatd_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_sp_m_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spcolmatrix_concatdiag_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_sp_m_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatdiag);
}


/* [Sp op Full]
 *
 */

static int int_spcolmatrix_concat_m_sp_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpColMatrix *HMat2,*B;
  NspMatrix *HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT ) return RET_BUG;
  if ( HMat1->m ==0 && HMat1->n == 0) 
    {
      /* return 2 */
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }

  if ((HMat2 = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( HMat2->m == 0 && HMat2->n == 0) 
    {
      if ((B=nsp_spcolmatrix_from_mat(HMat1)) == NULLSPCOL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(B));
      return 1;
    }
  else
    {
      if ((B=nsp_spcolmatrix_from_mat(HMat1)) == NULLSPCOL) return RET_BUG;
      if ( (*F)(B,HMat2)!= OK) 
	{
	  return RET_BUG;
	}
      MoveObj(stack,1,NSP_OBJECT(B));
    }
  return 1;
}

static int int_spcolmatrix_concatr_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_m_sp_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spcolmatrix_concatd_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_m_sp_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spcolmatrix_concatdiag_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_m_sp_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatdiag);
}



/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *  =======
 */

static int int_spcolmatrix_setrc(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*B=NULLSPCOL,*B1=NULLSPCOL;
  NspMatrix *Bm;
  NspObject *Rows,*Cols=NULL;

  CheckRhs(3,4);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) goto ret_bug;
  if ((Rows =nsp_get_object(stack,2)) == NULLOBJ)  goto ret_bug;
  if ( rhs == 4 )
    {
      if ((Cols =nsp_get_object(stack,3)) == NULLOBJ)  goto ret_bug;
    }
  
  if ( IsMatObj(stack,rhs) )
    {
      if ((Bm = GetMat(stack,rhs)) == NULLMAT ) goto ret_bug;
      if ((B1= B=nsp_spcolmatrix_from_mat(Bm)) == NULLSPCOL) goto ret_bug;
    }
  else if ( IsSpColMatObj(stack,rhs)) 
    {
      if ((B = GetSpCol(stack,rhs)) == NULLSPCOL) goto ret_bug;
    }
  else 
    {
      Scierror("Error: expecting matrix or sparse when setting values in a sparse\n");
      goto ret_bug;
    }

  if ( B == A ) 
    { if ((B = GetSpColCopy(stack,rhs)) == NULLSPCOL) goto ret_bug;}
  if ( rhs == 3 ) 
    { if (nsp_spcolmatrix_set_row( A, Rows,B) == FAIL) goto ret_bug; }
  else 
    { if (nsp_spcolmatrix_set_rowcol( A, Rows,Cols,B) == FAIL )  goto ret_bug;} 
  NSP_OBJECT(A)->ret_pos = 1;
  nsp_spcolmatrix_destroy(B1);
  return 1;
 ret_bug: 
  /* delete if non null; */
  nsp_spcolmatrix_destroy(B1);
  return RET_BUG;
}


/*
 * Res=MatDeletecols(A,Cols)
 *     Cols unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 * =======
 */	


static int int_spcolmatrix_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspObject *Cols;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /*  Note that if Cols is authorized to be a sparse 
   *  then checking that A and Cols are different 
   *  should be checked 
   */
  if ((Cols =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  /*
   */
  if (nsp_spcolmatrix_delete_cols( A, Cols) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=MatDeleterows(A,Rows)
 * WARNING : A must be changed by this routine
 * =======
 */	

static int int_spcolmatrix_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspObject *Rows;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /*  Note that if Cols is authorized to be a sparse 
   *  then checking that A and Cols are different 
   *  should be checked 
   */
  if ((Rows =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  /*
   */
  if (nsp_spcolmatrix_delete_rows( A, Rows) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=nsp_spcolmatrix_extract(Rows,Cols,A)
 * A unchanged, Rows and Cols are unchanged 
 */	

static int int_spcolmatrix_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*Res;
  NspObject *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
#if 0 
  /* Rows id changed by nsp_spcolmatrix_extract */
  if ((Rows = GetRealMatCopy(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;
#else 
  /* Rows is no more changed */
  if ((Rows =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  if ((Cols =nsp_get_object(stack,3)) == NULLOBJ) return RET_BUG;
#endif 
  Res =nsp_spcolmatrix_extract( A, Rows,Cols);
  if ( Res == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * Res=nsp_matrix_extract_elements(Elts,A)
 * A unchanged, Elts
 * WARNING note that on the stack we have Elts,A 
 */	

static int int_spcolmatrix_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*Res;
  NspObject *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((Elts =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  if ((Res =nsp_spcolmatrix_extract_elts( A, Elts)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * columns extraction  Cols A --> A(Cols)				  * 
 */	

static int int_spcolmatrix_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*Res;
  NspObject *Cols;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((Cols =nsp_get_object(stack,2)) == NULLOBJ) return RET_BUG;
  Res =nsp_spcolmatrix_extract_cols( A,Cols,&err);
  /* XXXXX Attention ici il faut un message d''erreur **/
  if ( err == 1) return RET_ENDFOR; 
  if ( Res == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * rows extraction 			
 * Rows , A -> A(Rows,:)
 */	

static int int_spcolmatrix_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*Res;
  NspObject *Rows;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /* Rows is changed by nsp_spcolmatrix_extract_rows */
  if ((Rows =nsp_get_object_copy(stack,2)) == NULLOBJ) return RET_BUG;
  Res =nsp_spcolmatrix_extract_rows( A,Rows,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


/*
 * diag interface 
 */

int int_spcolmatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspSpColMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetSpCol(stack, 1)) == NULLSPCOL)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_spcolmatrix_diag_create (A, k1);
  else
    Res = nsp_spcolmatrix_diag_extract (A, k1);
  if (Res == NULLSPCOL)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Returns the kthe diag of a Matrix 
 */

static int int_spcolmatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpColMatrix *A,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    { if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;}
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  Res =nsp_spcolmatrix_diag_extract( A,k1);
  if ( Res == NULLSPCOL)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 *  Creates a Matrix with kth diag set to Diag 
 */

static int int_spcolmatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpColMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_spcolmatrix_diag_create(Diag,k1)) == NULLSPCOL) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= mx2spmx(A) 
 * A is not changed 
 */

static int int_spcolmatrix_m2sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *Res;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT)  return RET_BUG;
  if (( Res=nsp_spcolmatrix_from_mat(A)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Res= full(A) 
 * A is not changed 
 */

static int int_spcolmatrix_sp2m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetSpCol(stack,1)) == NULLSPCOL)  return RET_BUG;
  if (( Res=nsp_spcolmatrix_to_mat(A)) == NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= full(A) when A is not sparse 
 * do nothing 
 */
#if 0
static int int_spcolmatrix_m2m(Stack stack, int rhs, int opt, int lhs)
{
  /* full(x) when x is already full **/
  NthObj(1)->ret_pos = 1;
  return 1;
}
#endif 


/*
 * Res= A*B 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

/* Generic function for * or .* **/
typedef NspSpColMatrix * (*Sp21) (NspSpColMatrix *A,NspSpColMatrix *B);

static int int_spcolmatrix_mult_gen(Stack stack, int rhs, int opt, int lhs, Sp21 F)
{
  NspSpColMatrix *HMat1,*HMat2,*HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((HMat2 = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( HMat2->m == 1 && HMat2->n == 1) 
    {
      if ( HMat2->D[0]->size == 0) 
	{
	  /* HMat2 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_spcolmatrix_create(NVOID,'r',HMat1->m,HMat1->n)) == NULLSPCOL) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else 
	{
	  /* A * <non-nul-scalar> **/
	  if ((HMat1 = GetSpColCopy(stack,1)) == NULLSPCOL) return RET_BUG;
	  if (nsp_spcolmatrix_mult_scal(HMat1,HMat2) != OK) return RET_BUG;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
    }
  else if ( HMat1->m==1 && HMat1->n == 1 ) 
    {
      if ( HMat1->D[0]->size == 0) 
	{
	  /* HMat1 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_spcolmatrix_create(NVOID,'r',HMat2->m,HMat2->n)) == NULLSPCOL) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else
	{
	  /* since Mat1 is scalar we store the result in Mat2 so we 
	     must copy it **/
	  if ((HMat2 = GetSpColCopy(stack,2)) == NULLSPCOL) return RET_BUG;
	  if (nsp_spcolmatrix_mult_scal(HMat2,HMat1) != OK) return RET_BUG;
	  NSP_OBJECT(HMat2)->ret_pos = 1;
	}
    }
  else 
    {
      if ((HMat3=(*F)(HMat1,HMat2)) == NULLSPCOL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

static int int_spcolmatrix_mult(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_mult_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_mult);
}

/*
 *   Res = A * X , A sparse matrix, X full matrix
 */
static int int_spcolmatrix_mult_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat1;
  NspMatrix *HMat2, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT) return RET_BUG;

  if ( HMat2->m == 1 && HMat2->n == 1 )
    {
      if ((HMat1 = GetSpColCopy(stack,1)) == NULLSPCOL) return RET_BUG;
      if ( nsp_spcolmatrix_mult_scalar(HMat2->R, HMat2->rc_type, HMat1) == FAIL ) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else if ( HMat1->n == HMat2->m )
    {
      if ( (HMat3 = nsp_spcolmatrix_mult_sp_m(HMat1, HMat2, NULLMAT)) == NULLMAT ) return RET_BUG;
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
static int int_spcolmatrix_pmult_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat1;
  NspMatrix *HMat2, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT) return RET_BUG;

  if ( HMat1->m == HMat2->m )
    {
      if ( (HMat3 = nsp_spcolmatrix_pmult_sp_m(HMat1, HMat2, NULLMAT)) == NULLMAT ) return RET_BUG;
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
 *   added by Bruno
 */

static int int_spcolmatrix_mult_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat2;
  NspMatrix *HMat1, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  if ((HMat2 = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;

  if ( HMat1->m == 1 && HMat1->n == 1 )
    {
      if ((HMat2 = GetSpColCopy(stack,2)) == NULLSPCOL) return RET_BUG;
      if ( nsp_spcolmatrix_mult_scalar(HMat1->R, HMat1->rc_type, HMat2) == FAIL ) return RET_BUG;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else if ( HMat1->n == HMat2->m )
    {
      if ( (HMat3 = nsp_spcolmatrix_mult_m_sp(HMat1, HMat2)) == NULLMAT ) return RET_BUG;
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
 * return NULLSPCOLon failure 
 * A is left unchanged 
 */

static int int_spcolmatrix_quote(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((A =nsp_spcolmatrix_transpose(A)) == NULLSPCOL)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * _
 * A'
 */

static int int_spcolmatrix_dquote (Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetSpCol(stack, 1)) == NULLSPCOL)
    return RET_BUG;
  if ((B = nsp_spcolmatrix_transpose (A)) == NULLSPCOL)
    return RET_BUG;
  nsp_spcolmatrix_conj(B);
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}



/*
 * C = A + B with special cases [] and [x] 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

static int int_spcolmatrix_plus(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpColMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((B = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( A->m == 1 && A->n == 1) 
    {
      NspMatrix *C=nsp_spcolmatrix_op_scal(B,A,&flag,'+');
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
      NspMatrix *C=nsp_spcolmatrix_op_scal(A,B,&flag,'+');
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
      NspSpColMatrix *Res =nsp_spcolmatrix_add(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
}

/*
 * {"plus_m_sp",int_spcolmatrix_plus_m_sp},
 *  {"plus_sp_m",int_spcolmatrix_plus_sp_m},
 */

static int int_spcolmatrix_plus_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *B;
  NspMatrix *A,*Bfull=NULL;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((B = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if (( Bfull=nsp_spcolmatrix_to_mat(B)) == NULLMAT) return RET_BUG;

  if ( SameDim(A,Bfull) ) 
    {
      /* compute result in Bfull */
      if (  nsp_mat_dadd(Bfull, A)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Bfull );
      return 1;
    }
  if ( A->mn == 1) 
    {
      if ( nsp_mat_add_scalar_bis(Bfull,A)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Bfull );
      return 1;
    }
  if ( B->m == 1 && B->n == 1) 
    {
      int rep;
      if ((A = GetMatCopy(stack,1)) == NULLMAT) return RET_BUG;
      rep= nsp_mat_add_scalar_bis(A,Bfull);
      nsp_matrix_destroy(Bfull);
      if (rep == FAIL) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
 err:
  nsp_matrix_destroy(Bfull);
  return RET_BUG;
}

static int int_spcolmatrix_plus_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *B,*Afull=NULL;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if (( Afull=nsp_spcolmatrix_to_mat(A)) == NULLMAT) return RET_BUG;

  if ( SameDim(Afull,B) ) 
    {
      /* compute result in Afull */
      if (  nsp_mat_dadd(Afull,B)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Afull );
      return 1;
    }
  if ( B->mn == 1) 
    {
      if ( nsp_mat_add_scalar_bis(Afull,B)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Afull );
      return 1;
    }

  if ( A->m == 1 && A->n == 1) 
    {
      int rep;
      if ((B = GetMatCopy(stack,2)) == NULLMAT) return RET_BUG;
      rep= nsp_mat_add_scalar_bis(B,Afull);
      nsp_matrix_destroy(Afull);
      if (rep == FAIL) return RET_BUG;
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
 err:
  nsp_matrix_destroy(Afull);
  return RET_BUG;
}


/*
 * OHMat3 = OHMat1-OHMat2 
 * return NULLSPCOLon failure 
 */
static int int_spcolmatrix_minus(Stack stack, int rhs, int opt, int lhs);

static int int_spcolmatrix_sub(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpColMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((B = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      NspSpColMatrix *Res =nsp_spcolmatrix_sub(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  if ( A->m == 1 && A->n == 1) 
    {
      NspMatrix *C=nsp_spcolmatrix_op_scal(B,A,&flag,'#'); /* -B + scalar **/
      if ( flag == 1) 
	{
	  /* B -> -B **/
	  stack.first += 1;
	  int_spcolmatrix_minus(stack,1,opt,1);
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
      NspMatrix *C=nsp_spcolmatrix_op_scal(A,B,&flag,'-');
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



static int int_spcolmatrix_sub_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *B;
  NspMatrix *A,*Bfull=NULL;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((B = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if (( Bfull=nsp_spcolmatrix_to_mat(B)) == NULLMAT) return RET_BUG;

  if ( SameDim(A,Bfull) ) 
    {
      /* compute result in Bfull */
      if (  nsp_mat_sub(Bfull, A)== FAIL) goto err; 
      /* change sign */
      nsp_mat_minus(Bfull);
      MoveObj(stack,1,(NspObject *) Bfull );
      return 1;
    }
  if ( A->mn == 1) 
    {
      if ( nsp_scalar_sub_mat_bis(Bfull,A)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Bfull );
      return 1;
    }
  if ( B->m == 1 && B->n == 1)
    {
      int rep;
      if ((A = GetMatCopy(stack,1)) == NULLMAT) return RET_BUG;
      rep= nsp_mat_sub_scalar_bis(A,Bfull);
      nsp_matrix_destroy(Bfull);
      if (rep == FAIL) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
 err:
  nsp_matrix_destroy(Bfull);
  return RET_BUG;
}

/* A - 
 *
 */

static int int_spcolmatrix_sub_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *B,*Afull=NULL;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if (( Afull=nsp_spcolmatrix_to_mat(A)) == NULLMAT) return RET_BUG;
  
  if ( SameDim(Afull,B) ) 
    {
      /* compute result in Bfull */
      if (  nsp_mat_sub(Afull,B)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Afull );
      return 1;
    }
  if ( B->mn == 1) 
    {
      if ( nsp_mat_sub_scalar_bis(Afull,B)== FAIL) goto err;
      MoveObj(stack,1,(NspObject *) Afull );
      return 1;
    }

  if ( A->m == 1 && A->m == 1) 
    {
      int rep;
      if ((B = GetMatCopy(stack,2)) == NULLMAT) return RET_BUG;
      rep=  nsp_scalar_sub_mat_bis(B,Afull);
      nsp_matrix_destroy(Afull);
      if (rep == FAIL) return RET_BUG;
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
 err:
  nsp_matrix_destroy(Afull);
  return RET_BUG;
}

/* As .* Bs 
 */

static int int_spcolmatrix_multt(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_mult_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_multtt);
}

/* A .* Bs 
 *
 */

static int int_spcolmatrix_multt_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *B,*As=NULL;
  NspMatrix *A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1)) == NULLMAT) goto err;
  if ((B = GetSpCol(stack,2)) == NULLSPCOL) goto err;
  if ((As=nsp_spcolmatrix_from_mat(A)) == NULLSPCOL) goto err;
  if ( A->mn == 1) 
    {
      /* A * <non-nul-scalar> **/
      if ((B = GetSpColCopy(stack,2)) == NULLSPCOL) goto err;
      if (nsp_spcolmatrix_mult_scal(B,As) != OK) goto err;
      NSP_OBJECT(B)->ret_pos = 1;
      nsp_spcolmatrix_destroy(As);
      return 1;
    }
  if ( B->m==1 && B->n == 1) 
    {
      if (nsp_spcolmatrix_mult_scal(As,B) != OK) goto err;
      MoveObj(stack,1,NSP_OBJECT(As));
      return 1;
    }
  
  if ( SameDim(A,B) ) 
    {
      NspSpColMatrix *Res;
      if ((Res=nsp_spcolmatrix_multtt(As,B)) == NULLSPCOL) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      nsp_spcolmatrix_destroy(As);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
 err: 
  nsp_spcolmatrix_destroy(As);
  return RET_BUG;
}

/* As .* B 
 *
 */

static int int_spcolmatrix_multt_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A, *Bs=NULL;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) goto err;
  if ((B = GetMat(stack,2)) == NULLMAT) goto err;
  if ((Bs=nsp_spcolmatrix_from_mat(B)) == NULLSPCOL) goto err;

  if ( B->mn == 1) 
    {
      /* A * <non-nul-scalar> **/
      if ((A = GetSpColCopy(stack,1)) == NULLSPCOL) goto err;
      if (nsp_spcolmatrix_mult_scal(A,Bs) != OK) goto err;
      NSP_OBJECT(A)->ret_pos = 1;
      nsp_spcolmatrix_destroy(Bs);
      return 1;
    }

  if ( A->m==1 && A->n == 1) 
    {
      if (nsp_spcolmatrix_mult_scal(Bs,A) != OK) goto err;
      MoveObj(stack,1,NSP_OBJECT(Bs));
      return 1;
    }

  if ( SameDim(A,B) ) 
    {
      NspSpColMatrix *Res;
      if ((Res=nsp_spcolmatrix_multtt(A,Bs)) == NULLSPCOL) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      nsp_spcolmatrix_destroy(Bs);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
 err:
  nsp_spcolmatrix_destroy(Bs);
  return RET_BUG;
}

/*
 * As ./ Bs 
 */

int int_spcolmatrix_div_el(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat1,*HMat2,*HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((HMat2 = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( HMat2->m == 1 && HMat2->n == 1 ) 
    {
      if ( HMat2->D[0]->size == 0 ) 
	{
	  if ((HMat3 = nsp_spcolmatrix_div_zero_tt(HMat1)) == NULLSPCOL) return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else 
	{
	  if ((HMat1 = GetSpColCopy(stack,1)) == NULLSPCOL) return RET_BUG;
	  if (nsp_spcolmatrix_div_scal_tt(HMat1,HMat2) != OK) return RET_BUG;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
    }
  else if ( HMat1->m == 1 && HMat1->n == 1 ) 
    {
      if ((HMat3 =nsp_spcolmatrix_scal_div_tt(HMat1,HMat2))  == NULLSPCOL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  else 
    {
      if ((HMat3 = nsp_spcolmatrix_divel(HMat1,HMat2)) == NULLSPCOL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

/* A ./ As 
 *
 */

static int int_spcolmatrix_div_el_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *B,*As=NULL,*Res=NULL;
  NspMatrix *A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1)) == NULLMAT) goto err;
  if ((B = GetSpCol(stack,2)) == NULLSPCOL) goto err;
  if ((As=nsp_spcolmatrix_from_mat(A)) == NULLSPCOL) goto err;
  if ( A->mn == 1) 
    {
      /* A * <non-nul-scalar> **/
      if ((Res =nsp_spcolmatrix_scal_div_tt(As,B))  == NULLSPCOL) goto err;
      MoveObj(stack,1,(NspObject *) Res);
      nsp_spcolmatrix_destroy(As);
      return 1;
    }
  if ( B->m==1 && B->n == 1) 
    {
      if ( B->D[0]->size == 0 ) 
	{
	  if ((Res = nsp_spcolmatrix_div_zero_tt(As)) == NULLSPCOL) goto err;
	  nsp_spcolmatrix_destroy(As);
	  MoveObj(stack,1,(NspObject *) Res);
	}
      else 
	{
	  if (nsp_spcolmatrix_div_scal_tt(As,B) != OK) goto err;
	  MoveObj(stack,1,(NspObject *) As);
	}
      return 1;
    }
  if ( SameDim(A,B) ) 
    {
      NspSpColMatrix *Res;
      if ((Res=nsp_spcolmatrix_divel(As,B)) == NULLSPCOL) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      nsp_spcolmatrix_destroy(As);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
 err: 
  nsp_spcolmatrix_destroy(As);
  return RET_BUG;
}

/* As ./ A 
 *
 */

static int int_spcolmatrix_div_el_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A, *Bs=NULL,*Res=NULL;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) goto err;
  if ((B = GetMat(stack,2)) == NULLMAT) goto err;
  if ((Bs=nsp_spcolmatrix_from_mat(B)) == NULLSPCOL) goto err;

  if ( B->mn == 1) 
    {
      if ( Bs->D[0]->size == 0 ) 
	{
	  if ((Res = nsp_spcolmatrix_div_zero_tt(A)) == NULLSPCOL) goto err;
	  nsp_spcolmatrix_destroy(Bs);
	  MoveObj(stack,1,(NspObject *) Res);
	}
      else 
	{
	  if ((A = GetSpColCopy(stack,1)) == NULLSPCOL) goto err;
	  if (nsp_spcolmatrix_div_scal_tt(A,Bs) != OK) goto err;
	  nsp_spcolmatrix_destroy(Bs);
	  NSP_OBJECT(A)->ret_pos = 1;
	}
      return 1;
    }

  if ( A->m==1 && A->n == 1) 
    {
      /* A * <non-nul-scalar> **/
      if ((Res =nsp_spcolmatrix_scal_div_tt(A,Bs))  == NULLSPCOL) goto err;
      MoveObj(stack,1,(NspObject *) Res);
      nsp_spcolmatrix_destroy(Bs);
      return 1;
    }

  if ( SameDim(A,B) ) 
    {
      NspSpColMatrix *Res;
      if ((Res=nsp_spcolmatrix_divel(A,Bs)) == NULLSPCOL) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      nsp_spcolmatrix_destroy(Bs);
      return 1;
    }
  Scierror("Error: incompatible dimensions\n");
 err:
  nsp_spcolmatrix_destroy(Bs);
  return RET_BUG;
}



/*
 *nsp_mat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspSpColMatrix *(*SuPro) (NspSpColMatrix *A, int dim);

static int int_spcolmatrix__sum(Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  int dim=0;
  NspSpColMatrix *Res,*HMat; 
  CheckRhs(1,2);
  CheckOptRhs(0, 1)
  CheckLhs(1,1);

  if ((HMat = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;

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

  if ((Res= (*F)(HMat,dim)) == NULLSPCOL) 
    return RET_BUG;

  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_spcolmatrix_isnan(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj=NULLOBJ;
  int dim=-1; /* default value isnan(A,dim='.') */
  NspSpColMatrix *Res,*HMat; 
  nsp_option opts[] ={{"dim",obj,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /* and(A,dim=) */
  if ( get_optional_args(stack, rhs, opt, opts, &Obj) == FAIL )
    return RET_BUG;
  if ( Obj != NULL) 
    {
      if ( GetDimArg(stack, opts[0].position, &dim ) == FAIL ) return RET_BUG;
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(HMat);
    }
  if ((Res= nsp_spcolmatrix_isnan(HMat,dim)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_spcolmatrix_isinf(Stack stack, int rhs, int opt, int lhs)
{
  int dim=-1; /* default value isnan(A,dim='.') */
  NspObject *Obj=NULLOBJ;
  NspSpColMatrix *Res,*HMat; 
  nsp_option opts[] ={{"dim",obj,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /* and(A,dim=) */
  if ( get_optional_args(stack, rhs, opt, opts, &Obj) == FAIL )
    return RET_BUG;
  if ( Obj != NULL) 
    {
      if ( GetDimArg(stack, opts[0].position, &dim ) == FAIL ) return RET_BUG;
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(HMat);
    }
  if ((Res= nsp_spcolmatrix_isinf(HMat,dim)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}



/* added by Bruno : return the number of non zero elements */
static int int_spcolmatrix_nnz(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat; 
  NspMatrix *Res;
  int  nnz;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((Res = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) return FAIL;

  nnz = nsp_spcolmatrix_nnz(HMat);
  Res->R[0] = (double) nnz;
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_spcolmatrix_sum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spcolmatrix__sum(stack,rhs,opt,lhs,nsp_spcolmatrix_sum) );
}


/*
 * nsp_mat_maxi: Maxi(*HMat);
 * A is unchanged 
 * XXXXXX : pas fini 
 */

typedef NspSpColMatrix *(*SpMiMax) (NspSpColMatrix *A,int dim,NspMatrix **Imax,int lhs);

static int int_spcolmatrix__maxi(Stack stack, int rhs, int opt, int lhs, SpMiMax F,int minmax)
{
  int dim=0;
  NspSpColMatrix *A,*M;
  NspMatrix *Imax;
  CheckRhs(1,2);  /* the form max(A1,A2,..) is currently limited to 2 matrices */
  CheckLhs(1,2);

  if ( rhs == 1 || (rhs - opt ) == 1 || ( rhs == 2 && IsSMatObj(stack,2)  ))
    {
      /* max(A), or max(A,str) or max(A,dim=options) idem for min */
      /* XXXXXX : Attention pas fini ici il faut un getrealsp         */ 
      /* en attendant je mets un test */
      if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
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

      if (( M= (*F)(A,dim,&Imax,lhs)) == NULLSPCOL) return RET_BUG;

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
      NspSpColMatrix *Index=NULL;
      if ((A = GetSpColCopy(stack,1))  == NULLSPCOL) return RET_BUG;
      if ((M = GetSpCol(stack,2))  == NULLSPCOL) return RET_BUG;
      Index = nsp_spcolmatrix_maximinitt_g(A,M,index,minmax,&err);
      if (err == TRUE ) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      if ( lhs == 2 ) 
	{
	  MoveObj(stack,2,NSP_OBJECT(Index));
	}
    }
  return Max(lhs,1);
}


static int int_spcolmatrix_maxi(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spcolmatrix__maxi(stack,rhs,opt,lhs,nsp_spcolmatrix_maxi,1));
}

static int int_spcolmatrix_mini(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spcolmatrix__maxi(stack,rhs,opt,lhs,nsp_spcolmatrix_mini,-1));
  return RET_BUG;
}

/* interface for triu 
 *
 */

static int int_spcolmatrix_triu(Stack stack, int rhs, int opt, int lhs)
{
  int k=0;
  NspSpColMatrix *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->m==0 && A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if ( nsp_spcolmatrix_triu(A,k)==FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;

}

/* interface for tril
 *
 */

static int int_spcolmatrix_tril(Stack stack, int rhs, int opt, int lhs)
{
  int k=0;
  NspSpColMatrix *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->m==0 && A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if ( nsp_spcolmatrix_tril(A,k)==FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/* interface for eye,ones,zeros
 */

static int int_spcolmatrix_spones(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A, *Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ((Res= nsp_spcolmatrix_spones(A))== NULLSPCOL) return RET_BUG; 
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}


typedef NspSpColMatrix *(Feoz)(int m,int n);

static int int_spcolmatrix_eoz(Stack stack, int rhs, int opt, int lhs,Feoz *F)
{
  int m,n;
  NspSpColMatrix *A;
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
  if ((A= (*F)(m,n))== NULLSPCOL) return RET_BUG;
  MoveObj(stack,1, NSP_OBJECT(A));
  return 1;
}

static int int_spcolmatrix_eye(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_eoz(stack,rhs,opt,lhs,nsp_spcolmatrix_eye);
}

static int int_spcolmatrix_zeros(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_eoz(stack,rhs,opt,lhs,nsp_spcolmatrix_zeros);
}


static int int_spcolmatrix_clean(Stack stack, int rhs, int opt, int lhs)
{
  double epsa =1.e-10, epsr = 1.e-10;
  NspSpColMatrix *A;
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
  if ((A = GetSpColCopy(stack, 1)) == NULL)
    return RET_BUG;
  nsp_spcolmatrix_clean (A, rhs, epsa, epsr);
  NSP_OBJECT (A)->ret_pos = 1;
  return 1;
}

static int int_spcolmatrix_sprand(Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  char crand='u';
  double sparsity=0.1;
  int m,n;
  NspSpColMatrix *A;
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
  if ((A= nsp_spcolmatrix_rand(m,n,sparsity,crand))== NULLSPCOL) return RET_BUG;
  MoveObj(stack,1, NSP_OBJECT(A));
  return 1;
}

/*
 *  A=op(A) 
 */

typedef int (*M11) (NspSpColMatrix *A);
typedef void (*VM11) (NspSpColMatrix *A);
 
static int int_spcolmatrix__gen11(Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->m==0 || A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if ((*F)(A) == FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

static int int_spcolmatrix__genv11(Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->m==0 || A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  (*F)(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}




/*
 *nsp_spmatrix_minus: A=-(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_minus(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_minus);
}


/*
 * A=Abs(A), absolue value or module of each element 
 */

static int int_spcolmatrix_abs(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_abs);
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

static int int_spcolmatrix_arg(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_arg);
}

/*
 *nsp_spmatrix_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

typedef NspMatrix* (*SpM) (NspSpColMatrix *A);

static int int_spcolmatrix__m_gen11(Stack stack, int rhs, int opt, int lhs, SpM F)
{
  NspMatrix *Loc;
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ((Loc = (*F)(A)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Loc);
  return 1;
}

static int int_spcolmatrix_cos(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_cos);
}

/*
 *nsp_spmatrix_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */
static int int_spcolmatrix_cosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_cosh);
}

/*
 * SpExpl : Exponentiation terme a term 
 * A is changed 
 */

static int int_spcolmatrix_expel(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_expel);
}

/*
 * SpLog : A=LogEl(A) 
 */

static int int_spcolmatrix_logel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->m==0 || A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if (nsp_spcolmatrix_logel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_spmatrix_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_sin(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_sin);
}

/*
 *nsp_spmatrix_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_sinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_sinh);
}

/*
 *nsp_spmatrix_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_sqrtel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->m==0 || A->n == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if (nsp_spcolmatrix_sqrtel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_spmatrix_acos: A=Acos(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_acos(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_acos);
}

/*
 *nsp_spmatrix_acosh: A=Acosh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_acosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_acosh);
}

/*
 *nsp_spmatrix_asin: A=Asin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_asin(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_asin);
}

/*
 *nsp_spmatrix_asinh: A=Asinh(A)
 * A is changed  
 * return 0 if error 
 */


static int int_spcolmatrix_asinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_asinh);
}

/*
 * SpATan : res= arctang(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spcolmatrix_atan(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_atan);
}

/*
 * SpArcTangH : res= atanh(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spcolmatrix_atanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_atanh);
}

/*
 *nsp_spmatrix_ceil: A=Ceil(A)
 * A is changed  
 */

static int int_spcolmatrix_ceil(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_ceil);
}

/*
 *nsp_spmatrix_int: A=Int(A)
 * A is changed  
 */

static int int_spcolmatrix_int(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_int);
}

/*
 *nsp_spmatrix_floor: A=Floor(A)
 * A is changed  
 */
 
static int int_spcolmatrix_floor(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_floor);
}

/*
 *nsp_spmatrix_round: A=Round(A)
 * A is changed  
 */
 
static int int_spcolmatrix_round(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_round);
}

/*
 *nsp_spmatrix_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spcolmatrix_sign(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_sign);
}

/*
 *nsp_spmatrix_tan: A=Tan(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spcolmatrix_tan(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_tan);
}

/*
 *nsp_spmatrix_tanh: A=Tanh(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spcolmatrix_tanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_tanh);
}

/*
 * A=Polar(A,B),  * A is changed 
 */

/*
 *nsp_spmatrix_conj: A=real(A)-i*Imag(A)
 * A is changed  if imaginary not changed if real 
 */

static int int_spcolmatrix_conj(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( (A->m == 0 || A->n == 0) || A->rc_type == 'r' )
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
 nsp_spcolmatrix_conj(A);
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

static int int_spcolmatrix_find(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL)  return RET_BUG;
  if (nsp_spcolmatrix_find(A,Max(lhs,1),&Rr,&Rc) == FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Rr));
  if ( lhs == 2 )
    {
      NthObj(2) = (NspObject *) Rc;
      NthObj(2)->ret_pos = 2;
      return 2;
    }
  return 1;
}

static int int_spcolmatrix_real(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_realpart);
}

static int int_spcolmatrix_imag(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_imagpart);
}


/*
 * checks is matrix is real
 */

static int int_spcolmatrix_isreal (Stack stack, int rhs, int opt, int lhs)
{
  int strict = FALSE;
  NspSpColMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if ((HMat = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;
  if (rhs==2) 
    {
       if ( GetScalarBool (stack,2,&strict) == FAIL) return RET_BUG;
    }
  if ( nsp_move_boolean(stack,1,nsp_spcolmatrix_isreal(HMat,strict)) == FAIL)
    return RET_BUG;
  return 1;
}
 
/* norm 1 and inf 
 *
 */

static int int_spcolmatrix_norm( Stack stack, int rhs, int opt, int lhs)
{
  double norm, p=2.0;
  int id=1, is_vector;
  char *norm_table[] =       {"1","2","inf","fro","Inf","Fro","M",NULL};
  char norm_lapack_table[] = {'1','2','I'  ,'F'  ,'I'  ,'F'  ,'M'};
  NspSpColMatrix *A;

  CheckRhs(1,2);
  CheckLhs(0,1);

  if ( (A=GetSpCol(stack, 1)) == NULLSPCOL ) return RET_BUG;
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
      norm = nsp_spcolmatrix_vnorm(A, p);
    }
  else
    {
      norm = nsp_spcolmatrix_norm(A,norm_lapack_table[id]);
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

static int int_spcolmatrix_isempty (Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;
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

static int int_spcolmatrix_isscalar (Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;
  if ( nsp_move_boolean(stack,1, HMat->m==1 && HMat->n==1 ) == FAIL)
    return RET_BUG;
  return 1;
}

 /**
 * check that a sparse matrix is a vector
 * 
 **/
static int int_spcolmatrix_isvector(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ((HMat = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;
  if ( nsp_move_boolean(stack,1, HMat->m==1 || HMat->n==1 ) == FAIL)
    return RET_BUG;
  return 1;
}
 
/*
 * computes length or numel of a sparse matrix (because the version
 * defined in object.c uses the mn field and this last could overflow)
 */
static int int_spcolmatrix_numel (Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;
  if ( nsp_move_double(stack,1,((double) HMat->m)*((double) HMat->n)) == FAIL)
    return RET_BUG;
  return 1;
}
 
/**
 * check that a sparse matrix is a lower or upper triangular
 * 
 **/
static int int_spcolmatrix_istriangular(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat;
  char *str;
  Boolean rep;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ((HMat = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;
  if ((str=GetString(stack,2)) == NULL) return RET_BUG;
  if ( strcmp(str,"u") == 0 )
    rep = nsp_spcolmatrix_is_upper_triangular(HMat);
  else if ( strcmp(str,"l") == 0 )
    rep = nsp_spcolmatrix_is_lower_triangular(HMat);
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
static int int_spcolmatrix_issymmetric(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat;
  Boolean rep;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ((HMat = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;

  rep = nsp_spcolmatrix_is_symmetric(HMat);

  if ( nsp_move_boolean(stack,1,rep) == FAIL ) 
    return RET_BUG;
  return 1;
}

/**
 * compute lower and upper bandwidth
 * 
 **/
static int int_spcolmatrix_lower_upper_bandwidth(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  int kl, ku;
  CheckRhs (1, 1);
  CheckLhs (1, 2);

  if ((A = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;

  if ( nsp_spcolmatrix_lower_and_upper_bandwidth(A, &kl, &ku) == FAIL )
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

static int int_spcolmatrix_solve_tri(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *b, *x=NULLMAT;
  char *str, type;
  int rep;

  CheckRhs (3, 3);
  CheckLhs (1, 1);

  if ((A = GetSpCol(stack, 1)) == NULLSPCOL)   return RET_BUG;
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
    rep = nsp_spcolmatrix_solve_utri(A, x, b);
  else
    rep = nsp_spcolmatrix_solve_ltri(A, x, b);

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
 * The Interface for basic numerical sparse matrices operation 
 * we use sp for spcol 
 */

static OpTab SpColMatrix_func[]={
  {"sp_create",int_spcolmatrix_create},
  {"spcol_create",int_spcolmatrix_create},
  {"m2sp",int_spcolmatrix_m2sp},
  {"sp2m",int_spcolmatrix_sp2m},
  {"dst_sp_sp",int_spcolmatrix_multt},
  {"dst_m_sp",int_spcolmatrix_multt_m_sp},
  {"dst_sp_m",int_spcolmatrix_multt_sp_m},
  {"dsl_sp_sp",int_spcolmatrix_div_el},
  {"dsl_m_sp",int_spcolmatrix_div_el_m_sp},
  {"dsl_sp_m",int_spcolmatrix_div_el_sp_m},
  {"mult_sp_sp",int_spcolmatrix_mult},
  {"mult_sp_m",int_spcolmatrix_mult_sp_m},
  {"pmult_sp_m",int_spcolmatrix_pmult_sp_m},
  {"mult_m_sp",int_spcolmatrix_mult_m_sp},
  {"plus_sp_sp",int_spcolmatrix_plus},
  {"plus_m_sp",int_spcolmatrix_plus_m_sp},
  {"plus_sp_m",int_spcolmatrix_plus_sp_m},
  {"minus_sp_sp",int_spcolmatrix_sub},
  {"minus_sp_m",int_spcolmatrix_sub_sp_m},
  {"minus_m_sp",int_spcolmatrix_sub_m_sp},
  {"minus_sp",int_spcolmatrix_minus},
  {"quote_sp",int_spcolmatrix_quote},
  {"dprim_sp", int_spcolmatrix_dquote},
  {"multt_sp_sp",int_spcolmatrix_multt}, 
  {"redim_sp",int_spcolmatrix_redim},
  {"matrix_sp",int_spcolmatrix_redim},
  {"concatd_sp_sp" ,  int_spcolmatrix_concatd },
  {"concatr_sp_sp" ,  int_spcolmatrix_concatr },
  {"concatdiag_sp_sp" ,  int_spcolmatrix_concatdiag },
  {"concatd_sp_m" ,  int_spcolmatrix_concatd_sp_m },
  {"concatr_sp_m" ,  int_spcolmatrix_concatr_sp_m },
  {"concatdiag_sp_m" ,  int_spcolmatrix_concatdiag_sp_m },
  {"concatd_m_sp" ,  int_spcolmatrix_concatd_m_sp },
  {"concatr_m_sp" ,  int_spcolmatrix_concatr_m_sp },
  {"concatdiag_m_sp" ,  int_spcolmatrix_concatdiag_m_sp },
  {"deletecols_sp", int_spcolmatrix_deletecols},
  {"deleterows_sp", int_spcolmatrix_deleterows},
  {"extract_sp",int_spcolmatrix_extract},
  {"extractrows_sp",int_spcolmatrix_extractrows},
  {"extractcols_sp",int_spcolmatrix_extractcols},
  {"diage_sp" ,  int_spcolmatrix_diage },
  {"diage_sp_m" ,  int_spcolmatrix_diage },
  /* {"diagset_sp" ,  int_spcolmatrix_diagset }, */
  {"diagcre_sp" ,  int_spcolmatrix_diagcre },
  {"diagcre_sp_m" ,  int_spcolmatrix_diagcre },
  {"diag_sp", int_spcolmatrix_diag},
  {"diag_sp_m", int_spcolmatrix_diag},
  {"sparse", int_spcolmatrix_sparse},
  {"sparse_sp", int_spcolmatrix_sparse_sp},
  {"spget", int_spcolmatrix_get},
  {"spfrommtlb",int_spcolmatrix_from_mtlb},
  {"spget_mtlb", int_spcolmatrix_get_mtlb},
  {"full_sp",int_spcolmatrix_sp2m},
  {"sum_sp_s" ,  int_spcolmatrix_sum },
  {"sum_sp" ,  int_spcolmatrix_sum },
  {"setrowscols_sp",int_spcolmatrix_setrc},
  {"max_sp" ,  int_spcolmatrix_maxi },
  {"max_sp_s" ,  int_spcolmatrix_maxi },
  {"min_sp" ,  int_spcolmatrix_mini },
  {"min_sp_s" ,  int_spcolmatrix_mini },
  {"extractelts_sp",int_spcolmatrix_extractelts},
  {"nnz_sp",int_spcolmatrix_nnz},
  {"triu_sp", int_spcolmatrix_triu},
  {"tril_sp", int_spcolmatrix_tril},
  {"sp_eye", int_spcolmatrix_eye}, /* to be removed ? */
  {"sp_ones", int_spcolmatrix_spones},/* to be removed ? */
  {"sp_zeros", int_spcolmatrix_zeros},/* to be removed ? */
  {"speye", int_spcolmatrix_eye},
  {"spones", int_spcolmatrix_spones},
  {"spzeros", int_spcolmatrix_zeros},
  /* now operations */
  {"abs_sp",int_spcolmatrix_abs},
  {"arg_sp",int_spcolmatrix_arg},
  {"sin_sp",int_spcolmatrix_sin},
  {"sinh_sp",int_spcolmatrix_sinh},
  {"asin_sp",int_spcolmatrix_asin},
  {"asinh_sp",int_spcolmatrix_asinh},
  {"cos_sp",int_spcolmatrix_cos},
  {"cosh_sp",int_spcolmatrix_cosh},
  {"acos_sp",int_spcolmatrix_acos},
  {"acosh_sp",int_spcolmatrix_acosh},
  {"atan_sp",int_spcolmatrix_atan},
  {"atanh_sp",int_spcolmatrix_atanh},
  {"ceil_sp",int_spcolmatrix_ceil},
  {"int_sp",int_spcolmatrix_int},
  {"floor_sp",int_spcolmatrix_floor},
  {"round_sp",int_spcolmatrix_round},
  {"sign_sp",int_spcolmatrix_sign},
  {"tan_sp",int_spcolmatrix_tan},
  {"tanh_sp",int_spcolmatrix_tanh},
  {"conj_sp",int_spcolmatrix_conj},
  {"find_sp", int_spcolmatrix_find},
  {"sqrt_sp",int_spcolmatrix_sqrtel},
  {"log_sp",int_spcolmatrix_logel},
  {"exp_sp",int_spcolmatrix_expel},
  {"sprand",int_spcolmatrix_sprand},  
  {"clean_sp",int_spcolmatrix_clean},  
  {"real_sp", int_spcolmatrix_real},
  {"imag_sp", int_spcolmatrix_imag},
  {"isreal_sp", int_spcolmatrix_isreal},
  {"norm_sp", int_spcolmatrix_norm},
  {"isnan_sp",int_spcolmatrix_isnan},
  {"isinf_sp",int_spcolmatrix_isinf},
  {"isempty_sp",int_spcolmatrix_isempty},
  {"isscalar_sp",int_spcolmatrix_isscalar},
  {"isvector_sp",int_spcolmatrix_isvector},
  {"istriangular_sp",int_spcolmatrix_istriangular},
  {"issymmetric_sp",int_spcolmatrix_issymmetric},
  {"lower_upper_bandwidths_sp", int_spcolmatrix_lower_upper_bandwidth},
  {"solve_tri_sp",int_spcolmatrix_solve_tri},
  {"length_sp",int_spcolmatrix_numel},
  {"numel_sp",int_spcolmatrix_numel},
  {(char *) 0, NULL}
};

/*
 * The Interface for basic matrices operation
 */

int SpColMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SpColMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void SpColMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SpColMatrix_func[i].name;
  *f = SpColMatrix_func[i].fonc;
}




 


