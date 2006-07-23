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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SpMatrix_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

#include "nsp/spmatrix-in.h"
#include "nsp/spmatops-in.h"

/*
 * NspSpMatrix inherits from NspObject 
 */

int nsp_type_spmatrix_id=0;
NspTypeSpMatrix *nsp_type_spmatrix=NULL;
int nsp_type_spmatrix_init();

NspTypeSpMatrix *new_type_spmatrix(type_mode mode)
{
  NspTypeSpMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_spmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_spmatrix;
    }
  if ((type =  malloc(sizeof(NspTypeSpMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* spmatrix_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = NULL; /*spmatrix_get_methods; */
  type->new = (new_func *) new_spmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for spmatrix */ 

  top->pr = (print_func *)nsp_spmatrix_print;                  /* printing*/   
  top->dealloc = (dealloc_func *)nsp_spmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_spmatrix_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_spmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_spmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_spmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_spmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) SpMatIsTrue;*/      /* check if object can be considered as true */  
  /*top->loop =(loop_func *) SpLoopExtract ; */               /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_spmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_spmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_spmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_spmatrix_xdr_save;
  top->load  = (load_func *)nsp_spmatrix_xdr_load;

  /* specific methods for spmatrix */
  type->init = (init_func *) init_spmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_spmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_spmatrix
       */
      type->id =  nsp_type_spmatrix_id = nsp_new_type_id();
      nsp_type_spmatrix = type;
      if ( nsp_register_type(nsp_type_spmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_spmatrix(mode); 
    }
  else 
    {
      type->id = nsp_type_spmatrix_id;
      return type;
    }
}
/*
 * initialize Spmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_spmatrix(NspSpMatrix *o,NspTypeSpMatrix *type)
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

NspSpMatrix *new_spmatrix() 
{
  NspSpMatrix *loc; 
  /* type must exists */
  nsp_type_spmatrix = new_type_spmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspSpMatrix)))== NULLSP) return loc;
  /* initialize object */
  if ( init_spmatrix(loc,nsp_type_spmatrix) == FAIL) return NULLSP;
  return loc;
}


/*
 *nsp_spmatrix_size: returns filled,hsize,or hsize 
 */

int nsp_spmatrix_size(NspSpMatrix *H, int flag)
{
  switch (flag) 
    {
    case 0: return H->mn;
    case 1: return H->m;
    case 2: return H->n;
    }
  return 0;
}

/*
 * MatType 
 */

static char sp_type_name[]="SpMat";
static char sp_short_type_name[]="sp";

char *nsp_spmatrix_type_as_string(void)
{
  return(sp_type_name);
}

char *nsp_spmatrix_type_short_string(void)
{
  return(sp_short_type_name);
}


int nsp_spmatrix_fullcomp(NspSpMatrix * A,NspSpMatrix * B,char *op,int *err)
{
  Scierror("SpMatFullComp: to be implemented \n");
  return FALSE;
}


int nsp_spmatrix_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_spmatrix_id) == FALSE) return FALSE ;
  rep =nsp_spmatrix_fullcomp((NspSpMatrix *) A,(NspSpMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_spmatrix_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_spmatrix_id) == FALSE) return TRUE;
  rep =nsp_spmatrix_fullcomp((NspSpMatrix *) A,(NspSpMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}



/*
 * Save a Matrix in a file stream 
 */

static int nsp_spmatrix_xdr_save(XDR *xdrs, NspSpMatrix *M)
{
  NspMatrix *RC, *Values;
  if ( nsp_spmatrix_get(M,&RC,&Values) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->n) == FAIL) return FAIL;
  if ( NSP_OBJECT(RC)->type->save (xdrs,RC) == FAIL) return FAIL;
  if ( NSP_OBJECT(Values)->type->save (xdrs,Values) == FAIL) return FAIL;
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspSpMatrix *nsp_spmatrix_xdr_load(XDR *xdrs)
{
  int m,n;
  NspObject *RC, *Values;
  NspSpMatrix *Loc;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSP;
  if (nsp_xdr_load_i(xdrs, &m) == FAIL)  return NULLSP;
  if (nsp_xdr_load_i(xdrs, &n) == FAIL)  return NULLSP;
  if ( (RC= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSP;
  if ( (Values= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSP;
  if ((Loc = nsp_spmatrix_sparse(name,(NspMatrix *)RC,(NspMatrix *)Values,m,n)) == NULLSP) return NULLSP;
  return Loc;
}


/*
 * A =nsp_spmatrix_object(O);
 * checks that O is an object of NspSpMatrix type. 
 * or a Hobj which points to an object of type SpMatrix
 * if so, returns a pointer to that NspSpMatrix and else returns NULL
 */

NspSpMatrix   *nsp_spmatrix_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_spmatrix_id) == TRUE) return ((NspSpMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_spmatrix));
  return(NULL);
}


/*
 * IsSpMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  SpMatrix 
 * or a Hobj which points to an object of type SpMatrix
 */

int IsSpMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_spmatrix_id);
}

/*
 * IsSpMat(O)
 * only checks that object is an object of type  SpMatrix 
 * or a Hobj which points to an object of type SpMatrix
 */

int IsSpMat(const NspObject *O)
{
  return nsp_object_type(O , nsp_type_spmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a Sp and returns that Sp  
 * or a copy of that Sp if its name 
 * is != NVOID 
 */

NspSpMatrix *GetSpCopy(Stack stack, int i)
{
  if (  GetSp(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a SpMatrix and returns that SpMatrix  
 */

NspSpMatrix *GetSp(Stack stack, int i)
{
  NspSpMatrix *M;
  if (( M =nsp_spmatrix_object(NthObj(i))) == NULLSP )
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a SpMatrix and returns that SpMatrix  
 */

NspSpMatrix *GetRealSp(Stack stack, int i)
{
  NspSpMatrix *M;
  if (( M =nsp_spmatrix_object(NthObj(i))) == NULLSP )
    ArgMessage(stack,i);
  if (M->rc_type == 'c')
    {
      Scierror ("\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should not be complex\n", NspFname(stack));
      return NULLSP;
    }
  return M;
}


/*
 * Now the interfaced function for basic sparse operations
 */

typedef int (*SpC) (NspSpMatrix *A,NspSpMatrix *B);

/*
 * Creation of a Sparse Matrix 
 * returns NULLSP on failure 
 * The matrix is created with no initial value 
 */

static int int_spmatrix_create(Stack stack, int rhs, int opt, int lhs)
{  
  int m1,n1;
  NspSpMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ((HMat =nsp_spmatrix_create(NVOID,'r',m1,n1) ) == NULLSP) return RET_BUG;
  MoveObj(stack,1, (NspObject *) HMat);
  return 1;
}

/*
 * Creation of a Sparse Matrix 
 * returns NULLSP on failure 
 * sparse(rc,values [, [m,n]])
 */

static int int_spmatrix_sparse(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpMatrix *A;
  NspMatrix *RC,*Values,*MN;
  if ( rhs == 1) 
    return  int_spmatrix_m2sp(stack,rhs,opt,lhs);
  CheckRhs(2,3);
  CheckLhs(1,1);
  if ((RC = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
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
      if (( A =nsp_spmatrix_sparse(NVOID,RC,Values,(int) MN->R[0],(int) MN->R[1])) == NULLSP ) 
	return RET_BUG;
    }
  else
    {
      if (( A =nsp_spmatrix_sparse(NVOID,RC,Values,-1,-1)) == NULLSP ) 
	return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * Creation of a Sparse Matrix 
 * returns NULLSP on failure 
 * sparse(rc,values [, [m,n]])
 */

static int int_spmatrix_get(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpMatrix *A;
  NspMatrix *RC,*Values,*MN=NULL;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ( nsp_spmatrix_get(A,&RC,&Values) == FAIL) return RET_BUG;
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


/*
 * Matredim : changes Matrix dimensions
 * but keeps m*n constant
 */

static int int_spmatrix_redim(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspSpMatrix  *A;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if (( A =nsp_spmatrix_redim(A,m1,n1)) == NULLSP ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * Right Concatenation 
 * A= [A,B] 
 */


static int int_spmatrix_concat_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSp(stack,1))  == NULLSP) return RET_BUG;
  if ( HMat1->mn == 0) 
    {
      /* return 2 */
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSp(stack,2)) == NULLSP) return RET_BUG;
  if ( HMat2->mn == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetSpCopy(stack,1))  == NULLSP) return RET_BUG;
      if ( (*F)(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}


static int int_spmatrix_concatr(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSP on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spmatrix_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSP on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spmatrix_concatdiag(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spmatrix_concatdiag);
}


/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *  =======
 */

static int int_spmatrix_setrc(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A,*B;
  NspMatrix *Rows,*Rows1=NULLMAT,*Cols=NULLMAT,*Cols1=NULLMAT;
  CheckRhs(3,4);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) goto ret_bug;
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Rows is boolean : use find(Rows) **/
      NspBMatrix *BRows ;
      if ((BRows = GetBMat(stack,2)) == NULLBMAT) goto ret_bug;
      if ((Rows = Rows1 = nsp_bmatrix_find(BRows)) == NULLMAT) goto ret_bug;
    }
  else
    {
      if ((Rows = GetRealMat(stack,2)) == NULLMAT) goto ret_bug;
    }
  if ( rhs == 4 )
    {
      /* Cols is boolean : use find(Cols) **/
      if ( IsBMatObj(stack,3)  ) 
	{
	  NspBMatrix *BCols ;
	  if ((BCols = GetBMat(stack,2)) == NULLBMAT) goto ret_bug;
	  if ((Cols = Cols1 = nsp_bmatrix_find(BCols)) == NULLMAT) goto ret_bug;
	}  
      else
	{
	  if ((Cols = GetRealMat(stack,3)) == NULLMAT ) goto ret_bug;
	}
    }
  if ((B = GetSp(stack,rhs)) == NULLSP ) goto ret_bug;
  if ( B == A ) 
    { if ((B = GetSpCopy(stack,rhs)) == NULLSP ) goto ret_bug;}
  if ( rhs == 3 ) 
    { if (nsp_spmatrix_set_row( A, Rows,B) == FAIL) goto ret_bug; }
  else 
    { if (nsp_spmatrix_set_rowcol( A, Rows,Cols,B) == FAIL )  goto ret_bug;} 
  NSP_OBJECT(A)->ret_pos = 1;
  nsp_matrix_destroy(Rows1);
  nsp_matrix_destroy(Cols1);
  return 1;
 ret_bug: 
  /* delete if non null; */
  nsp_matrix_destroy(Rows1);
  nsp_matrix_destroy(Cols1);
  return RET_BUG;
}


/*
 * Res=MatDeletecols(A,Cols)
 *     Cols unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 * =======
 */	

static int int_spmatrix_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  NspMatrix *Cols;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  /* A and Cols can't point to the same object **/
  if ((Cols = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if (nsp_spmatrix_delete_cols( A, Cols) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=MatDeleterows(A,Rows)
 * WARNING : A must be changed by this routine
 * =======
 */	

static int int_spmatrix_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  NspMatrix *Rows;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  /* A and Cols can't point to the same object **/
  if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if (nsp_spmatrix_delete_rows( A, Rows) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=nsp_spmatrix_extract(Rows,Cols,A)
 * A unchanged, Rows and Cols are unchanged 
 */	

static int int_spmatrix_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A,*Res;
  NspMatrix *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;
  Res =nsp_spmatrix_extract( A, Rows,Cols);
  if ( Res == NULLSP) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * Res=nsp_matrix_extract_elements(Elts,A)
 * A unchanged, Elts
 * WARNING note that on the stack we have Elts,A 
 */	

static int int_spmatrix_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A,*Res;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((Elts = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Res =nsp_spmatrix_extract_elts( A, Elts)) == NULLSP) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * columns extraction  Cols A --> A(Cols)				  * 
 */	

static int int_spmatrix_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A,*Res;
  NspMatrix *Cols;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((Cols = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_spmatrix_extract_cols( A,Cols,&err);
  /* XXXXX Attention ici il faut un message d''erreur **/
  if ( err == 1) return RET_ENDFOR; 
  if ( Res == NULLSP) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * rows extraction 			
 * Rows , A -> A(Rows,:)
 */	

static int int_spmatrix_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A,*Res;
  NspMatrix *Rows;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_spmatrix_extract_rows( A,Rows,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLSP) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


/*
 * diag interface 
 */

int int_spmatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspSpMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetSp (stack, 1)) == NULLSP)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_spmatrix_diag_create (A, k1);
  else
    Res = nsp_spmatrix_diag_extract (A, k1);
  if (Res == NULLSP)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Returns the kthe diag of a Matrix 
 */

static int int_spmatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpMatrix *A,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    { if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;}
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  Res =nsp_spmatrix_diag_extract( A,k1);
  if ( Res == NULLSP)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Set the kth Diag of A to Diag 
 *  A is enlarged & comlexified if necessary 
 *  int nsp_matrix_create_diag(A,Diag,k)
 * WARNING : A is not copied we want this routine to change A
 */

static int int_spmatrix_diagset(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspSpMatrix *A,*Diag;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((Diag = GetSp(stack,2)) == NULLSP) return RET_BUG;
  if ( GetScalarInt(stack,3,&k1) == FAIL) return RET_BUG;
  if (nsp_spmatrix_diag_set( A, Diag,k1) != OK) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 *  Creates a Matrix with kth diag set to Diag 
 */

static int int_spmatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_spmatrix_diag_create(Diag,k1)) == NULLSP ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= mx2spmx(A) 
 * A is not changed 
 */

static int int_spmatrix_m2sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *Res;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT)  return RET_BUG;
  if (( Res=nsp_spmatrix_from_mat(A)) == NULLSP) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Res= full(A) 
 * A is not changed 
 */

static int int_spmatrix_sp2m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  NspMatrix *Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetSp(stack,1)) == NULLSP)  return RET_BUG;
  if (( Res=nsp_spmatrix_to_mat(A)) == NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= full(A) when A is not sparse 
 * do nothing 
 */

static int int_spmatrix_m2m(Stack stack, int rhs, int opt, int lhs)
{
  /* full(x) when x is already full **/
  NthObj(1)->ret_pos = 1;
  return 1;
}


/*
 * Res= A*B 
 * return NULLSP on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

/* Generic function for * or .* **/
typedef NspSpMatrix * (*Sp21) (NspSpMatrix *A,NspSpMatrix *B);

static int int_spmatrix_mult_gen(Stack stack, int rhs, int opt, int lhs, Sp21 F)
{
  NspSpMatrix *HMat1,*HMat2,*HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ( HMat1->mn == 0)  
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSp(stack,2)) == NULLSP) return RET_BUG;
  if ( HMat2->mn == 0 )
    {
      /* flag == 1 ==> A op [] returns [] **/
      NSP_OBJECT(HMat2)->ret_pos = 1;
      return 1;
    }
  if ( HMat2->mn == 1) 
    {
      if ( HMat2->D[0]->size == 0) 
	{
	  /* HMat2 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_spmatrix_create(NVOID,'r',HMat1->m,HMat1->n)) == NULLSP) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else 
	{
	  /* A * <non-nul-scalar> **/
	  if ((HMat1 = GetSpCopy(stack,1)) == NULLSP) return RET_BUG;
	  if (nsp_spmatrix_mult_scal(HMat1,HMat2) != OK) return RET_BUG;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
    }
  else if ( HMat1->mn == 1 ) 
    {
      if ( HMat1->D[0]->size == 0) 
	{
	  /* HMat1 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_spmatrix_create(NVOID,'r',HMat2->m,HMat2->n)) == NULLSP) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else
	{
	  /* since Mat1 is scalar we store the result in Mat2 so we 
	     must copy it **/
	  if ((HMat2 = GetSpCopy(stack,2)) == NULLSP) return RET_BUG;
	  if (nsp_spmatrix_mult_scal(HMat2,HMat1) != OK) return RET_BUG;
	  NSP_OBJECT(HMat2)->ret_pos = 1;
	}
    }
  else 
    {
      if ((HMat3=(*F)(HMat1,HMat2)) == NULLSP) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

static int int_spmatrix_mult(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix_mult_gen(stack,rhs,opt,lhs,nsp_spmatrix_mult);
}

/*
 *   Res = A * X , A sparse matrix, X full matrix
 *   A and X are left unchanged
 *   added by Bruno
 */
static int int_spmatrix_multm(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *HMat1;
  NspMatrix *HMat2, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT) return RET_BUG;

  if ( HMat1->mn == 0)  
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else if ( HMat2->mn == 0 )
    {
      NSP_OBJECT(HMat2)->ret_pos = 1;
      return 1;
    }

  if ( HMat1->n != HMat2->m )
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return RET_BUG;
    }

  if ( (HMat3 = nsp_spmatrix_mult_matrix(HMat1, HMat2)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat3);
  return 1;
}

/*
 * Res= A'
 * return NULLSP on failure 
 * A is left unchanged 
 */

static int int_spmatrix_quote(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((A =nsp_spmatrix_transpose(A)) == NULLSP )  return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * _
 * A'
 */

static int int_spmatrix_dquote (Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetSp(stack, 1)) == NULLSP)
    return RET_BUG;
  if ((B = nsp_spmatrix_transpose (A)) == NULLSP)
    return RET_BUG;
 nsp_spmatrix_conj(B);
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}



/*
 * C = A + B with special cases [] and [x] 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

static int int_spmatrix_plus(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((B = GetSp(stack,2)) == NULLSP) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      NspSpMatrix *Res =nsp_spmatrix_add(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  if ( A->mn <= 1) 
    {
      NspMatrix *C=nsp_spmatrix_op_scal(B,A,&flag,'+');
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
  if ( B->mn <= 1) 
    {
      NspMatrix *C=nsp_spmatrix_op_scal(A,B,&flag,'+');
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
  return 1;
}



/*
 * OHMat3 = OHMat1-OHMat2 
 * return NULLSP on failure 
 */

static int int_spmatrix_sub(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((B = GetSp(stack,2)) == NULLSP) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      NspSpMatrix *Res =nsp_spmatrix_sub(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  if ( A->mn <= 1) 
    {
      NspMatrix *C=nsp_spmatrix_op_scal(B,A,&flag,'#'); /* -B + scalar **/
      if ( flag == 1) 
	{
	  /* B -> -B **/
	  stack.first += 1;
	  int_spmatrix_minus(stack,1,opt,1);
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
  if ( B->mn <= 1) 
    {
      NspMatrix *C=nsp_spmatrix_op_scal(A,B,&flag,'-');
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
  return 1;
}

/*
 * OHMat3 = OHMat1 .* OHMat2 
 * return NULLSP on failure 
 */

static int int_spmatrix_multt(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix_mult_gen(stack,rhs,opt,lhs,nsp_spmatrix_multtt);
}

/*
 *nsp_mat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspSpMatrix *(*SuPro) (NspSpMatrix *A,char *);

static int int_spmatrix__sum(Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  char *str;
  NspSpMatrix *Res,*HMat; 
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((HMat = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
    }
  else 
    { str = "F"; }
  if ((Res= (*F)(HMat,str)) == NULLSP ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/* added by Bruno : return the number of non zero elements */
static int int_spmatrix_nnz(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *HMat; 
  NspMatrix *Res;
  int  nnz;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ((Res = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) return FAIL;

  nnz = nsp_spmatrix_nnz(HMat);
  Res->R[0] = (double) nnz;
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_spmatrix_sum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spmatrix__sum(stack,rhs,opt,lhs,nsp_spmatrix_sum) );
}


/*
 *nsp_mat_maxi: Maxi(*HMat);
 * A is unchanged 
 * XXXXXX : pas fini 
 */

typedef NspSpMatrix *(*SpMiMax) (NspSpMatrix *A,char *,NspMatrix **Imax,int lhs);

static int int_spmatrix__maxi(Stack stack, int rhs, int opt, int lhs, SpMiMax F)
{
  char *str;
  NspSpMatrix *A,*M;
  NspMatrix *Imax;
  if ( rhs < 1) 
    { 
      Scierror("Error:\t Rhs must be >= 1 for function %s\n",NspFname(stack));
      return RET_BUG;
    }
  CheckLhs(1,2);
  if ( rhs == 1 || ( rhs == 2 && IsSMatObj(stack,2)  ))
    {
      /* maxi(A) or maxi(A,'c' or 'r' or 'F') where A is a matrix 
       * idem for mini 
       * XXXXXX : Attention pas fini ici il faut un getrealsp 
       */
      if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
      if ( rhs == 2) 
	{
	  if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
	}
      else 
	{ str = "F"; }
      if (( M= (*F)(A,str,&Imax,lhs)) == NULLSP ) return RET_BUG;
      if ( lhs == 2)
	{
	  if ( rhs == 2) MoveObj(stack,2,(NspObject *)Imax);
	}
      MoveObj(stack,1,(NspObject *) M);
    }
  else
    {
      Scierror("Error: XXXXXX a ecrire\n");
      return RET_BUG;
    }
  return Max(lhs,1);
}


static int int_spmatrix_maxi(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spmatrix__maxi(stack,rhs,opt,lhs,nsp_spmatrix_maxi));
}

/* Interface for numeric ops 
 *
 */


/*
 *  A=op(A) 
 */

typedef int (*M11) (NspSpMatrix *A);
typedef void (*VM11) (NspSpMatrix *A);

/* generic function for ones,rand,eyes **/
 
static int int_spmatrix__gen11(Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  (*F)(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

static int int_spmatrix__genv11(Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  (*F)(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 *nsp_spmatrix_minus: A=-(A)
 * A is changed  
 * return 0 if error 
 */

int int_spmatrix_minus(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__gen11(stack,rhs,opt,lhs,nsp_spmatrix_minus);
}


/*
 * A=Abs(A), absolue value or module of each element 
 */

static int int_spmatrix_abs(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__gen11(stack,rhs,opt,lhs,nsp_spmatrix_abs);
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

static int int_spmatrix_arg(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__gen11(stack,rhs,opt,lhs,nsp_spmatrix_arg);
}

/*
 *nsp_spmatrix_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

typedef NspMatrix* (*SpM) (NspSpMatrix *A);

static int int_spmatrix__m_gen11(Stack stack, int rhs, int opt, int lhs, SpM F)
{
  NspMatrix *Loc;
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ((Loc = (*F)(A)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Loc);
  return 1;
}

static int int_spmatrix_cos(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmatrix_cos);
}

/*
 *nsp_spmatrix_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */
static int int_spmatrix_cosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmatrix_cosh);
}

/*
 * SpExpl : Exponentiation terme a term 
 * A is changed 
 */

static int int_spmatrix_expel(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmatrix_expel);
}

/*
 * SpLog : A=LogEl(A) 
 */

static int int_spmatrix_logel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  if (nsp_spmatrix_logel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_spmatrix_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmatrix_sin(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_sin);
}

/*
 *nsp_spmatrix_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmatrix_sinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_sinh);
}

/*
 *nsp_spmatrix_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

static int int_spmatrix_sqrtel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
  if (nsp_spmatrix_sqrtel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_spmatrix_acos: A=Acos(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmatrix_acos(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmatrix_acos);
}

/*
 *nsp_spmatrix_acosh: A=Acosh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmatrix_acosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spmatrix_acosh);
}

/*
 *nsp_spmatrix_asin: A=Asin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spmatrix_asin(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_asin);
}

/*
 *nsp_spmatrix_asinh: A=Asinh(A)
 * A is changed  
 * return 0 if error 
 */


static int int_spmatrix_asinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_asinh);
}

/*
 * SpATan : res= arctang(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spmatrix_atan(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_atan);
}

/*
 * SpArcTangH : res= atanh(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spmatrix_atanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_atanh);
}

/*
 *nsp_spmatrix_ceil: A=Ceil(A)
 * A is changed  
 */

static int int_spmatrix_ceil(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_ceil);
}

/*
 *nsp_spmatrix_int: A=Int(A)
 * A is changed  
 */

static int int_spmatrix_int(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_int);
}

/*
 *nsp_spmatrix_floor: A=Floor(A)
 * A is changed  
 */
 
static int int_spmatrix_floor(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_floor);
}

/*
 *nsp_spmatrix_round: A=Round(A)
 * A is changed  
 */
 
static int int_spmatrix_round(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_round);
}

/*
 *nsp_spmatrix_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spmatrix_sign(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__gen11(stack,rhs,opt,lhs,nsp_spmatrix_sign);
}

/*
 *nsp_spmatrix_tan: A=Tan(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spmatrix_tan(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_tan);
}

/*
 *nsp_spmatrix_tanh: A=Tanh(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spmatrix_tanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmatrix__genv11(stack,rhs,opt,lhs,nsp_spmatrix_tanh);
}

/*
 * A=Polar(A,B),  * A is changed 
 */

/*
 *nsp_spmatrix_conj: A=real(A)-i*Imag(A)
 * A is changed  if imaginary not changed if real 
 */

static int int_spmatrix_conj(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSp(stack,1))== NULLSP) return RET_BUG;
  if ( A->mn == 0 || A->rc_type == 'r' )
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpCopy(stack,1))== NULLSP) return RET_BUG;
 nsp_spmatrix_conj(A);
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

static int int_spmatrix_find(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetSp(stack,1)) == NULLSP)  return RET_BUG;
  if (nsp_spmatrix_find(A,Max(lhs,1),&Rr,&Rc) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Rr);
  if ( lhs == 2 )
    {
      NthObj(2) = (NspObject *) Rc;
      NthObj(2)->ret_pos = 2;
      return 2;
    }
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * The Interface for sparse ops
 */

static OpTab SpMatrix_func[]={
  {"create_sp",int_spmatrix_create},
  {"m2sp",int_spmatrix_m2sp},
  {"sp2m",int_spmatrix_sp2m},
  {"dst_sp_sp",int_spmatrix_multt},
  {"mult_sp_sp",int_spmatrix_mult},
  {"mult_sp_m",int_spmatrix_multm},
  {"plus_sp_sp",int_spmatrix_plus},
  {"minus_sp_sp",int_spmatrix_sub},
  {"minus_sp_m",int_spmatrix_sub},
  {"minus_m_sp",int_spmatrix_sub},
  {"minus_sp",int_spmatrix_minus},
  {"quote_sp",int_spmatrix_quote},
  {"dprim_sp", int_spmatrix_dquote},
  {"multt_sp_sp",int_spmatrix_multt}, 
  {"spredim",int_spmatrix_redim},
  {"concatd_sp_sp" ,  int_spmatrix_concatd },
  {"concatr_sp_sp" ,  int_spmatrix_concatr },
  {"concatdiag_sp_sp" ,  int_spmatrix_concatdiag },
  {"deletecols_sp_m", int_spmatrix_deletecols},
  {"deleterows_sp_m", int_spmatrix_deleterows},
  {"extract_sp",int_spmatrix_extract},
  {"extractrows_sp",int_spmatrix_extractrows},
  {"extractcols_sp",int_spmatrix_extractcols},
  {"diage_sp" ,  int_spmatrix_diage },
  {"diage_sp_m" ,  int_spmatrix_diage },
  {"diagset_sp" ,  int_spmatrix_diagset },
  {"diagcre_sp" ,  int_spmatrix_diagcre },
  {"diagcre_sp_m" ,  int_spmatrix_diagcre },
  {"diag_sp", int_spmatrix_diag},
  {"diag_sp_m", int_spmatrix_diag},
  {"sparse", int_spmatrix_sparse},
  {"spget", int_spmatrix_get},
  {"full_sp",int_spmatrix_sp2m},
  {"full_m",int_spmatrix_m2m},
  {"sum_sp_s" ,  int_spmatrix_sum },
  {"sum_sp" ,  int_spmatrix_sum },
  {"setrowscols_sp",int_spmatrix_setrc},
  {"maxi_sp" ,  int_spmatrix_maxi },
  {"maxi_sp_s" ,  int_spmatrix_maxi },
  {"extractelts_sp",int_spmatrix_extractelts},
  {"nnz_sp",int_spmatrix_nnz},
  /* ops */
  {"abs_sp",int_spmatrix_abs},
  {"arg_sp",int_spmatrix_arg},
  {"sin_sp",int_spmatrix_sin},
  {"sinh_sp",int_spmatrix_sinh},
  {"asin_sp",int_spmatrix_asin},
  {"asinh_sp",int_spmatrix_asinh},
  {"cos_sp",int_spmatrix_cos},
  {"cosh_sp",int_spmatrix_cosh},
  {"acos_sp",int_spmatrix_acos},
  {"acosh_sp",int_spmatrix_acosh},
  {"atan_sp",int_spmatrix_atan},
  {"atanh_sp",int_spmatrix_atanh},
  {"ceil_sp",int_spmatrix_ceil},
  {"int_sp",int_spmatrix_int},
  {"floor_sp",int_spmatrix_floor},
  {"round_sp",int_spmatrix_round},
  {"sign_sp",int_spmatrix_sign},
  {"tan_sp",int_spmatrix_tan},
  {"tanh_sp",int_spmatrix_tanh},
  {"conj_sp",int_spmatrix_conj},
  {"find_sp", int_spmatrix_find},
  {"sqrt_sp",int_spmatrix_sqrtel},
  {"log_sp",int_spmatrix_logel},
  {"exp_sp",int_spmatrix_expel},
  {(char *) 0, NULL}
};

int SpMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SpMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void SpMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SpMatrix_func[i].name;
  *f = SpMatrix_func[i].fonc;
}








