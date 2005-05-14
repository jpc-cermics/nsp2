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

#include "nsp/spmatrix-in.h"

typedef int (*SpC) (NspSpMatrix *A,NspSpMatrix *B);


/*
 * Now the interfaced function for basic sparse operations
 */

/*
 * Creation of a Sparse Matrix 
 * returns NULLSP on failure 
 * The matrix is created with no initial value 
 */

static int int_spcreate(Stack stack, int rhs, int opt, int lhs)
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

static int int_spsparse(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpMatrix *A;
  NspMatrix *RC,*Values,*MN;
  if ( rhs == 1) 
    return  int_spm2sp(stack,rhs,opt,lhs);
  CheckRhs(2,3);
  CheckLhs(1,1);
  if ((RC = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  if (RC->mn != 0 &&  RC->n != 2)
    {
      Scierror("Error: first argument of function %s must have 2 columns\n\n",
	       stack.fname);
      return RET_BUG;
    }
  if ((Values = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( RC->m != Values->mn  ) 
    {
      Scierror("Error: first and second arguments of function %s have incompatible sizes\n",
	       stack.fname);
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

static int int_spget(Stack stack, int rhs, int opt, int lhs)
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

static int int_spredim(Stack stack, int rhs, int opt, int lhs)
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


static int int_spconcat_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
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


static int int_spconcatr(Stack stack, int rhs, int opt, int lhs)
{
  return int_spconcat_gen(stack,rhs,opt,lhs,nsp_spmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSP on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spconcatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_spconcat_gen(stack,rhs,opt,lhs,nsp_spmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSP on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spconcatdiag(Stack stack, int rhs, int opt, int lhs)
{
  return int_spconcat_gen(stack,rhs,opt,lhs,nsp_spmatrix_concatdiag);
}


/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *  =======
 */

static int int_spsetrc(Stack stack, int rhs, int opt, int lhs)
{
  NspSpMatrix *A,*B;
  NspMatrix *Rows,*Cols=NULLMAT;
  CheckRhs(3,4);
  CheckLhs(1,1);
  if ((A = GetSp(stack,1)) == NULLSP) return RET_BUG;
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Rows is boolean : use find(Rows) **/
      NspBMatrix *BRows ;
      if ((BRows = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Rows =nsp_bmatrix_find(BRows)) == NULLMAT) return RET_BUG;
    }
  else
    {
      if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }
  if ( rhs == 4 )
    {
      /* Cols is boolean : use find(Cols) **/
      if ( IsBMatObj(stack,3)  ) 
	{
	  NspBMatrix *BCols ;
	  if ((BCols = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
	  if ((Cols =nsp_bmatrix_find(BCols)) == NULLMAT) return RET_BUG;
	}  
      else
	{
	  if ((Cols = GetRealMat(stack,3)) == NULLMAT ) return RET_BUG;
	}
    }
  if ((B = GetSp(stack,rhs)) == NULLSP ) return RET_BUG;
  if ( B == A ) 
    { if ((B = GetSpCopy(stack,rhs)) == NULLSP ) return RET_BUG;}
  if ( rhs == 3 ) 
    { if (nsp_spmatrix_set_row( A, Rows,B) == FAIL) return RET_BUG; }
  else 
    { if (nsp_spmatrix_set_rowcol( A, Rows,Cols,B) == FAIL )  return RET_BUG;} 
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * Res=MatDeletecols(A,Cols)
 *     Cols unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 * =======
 */	

static int int_spdeletecols(Stack stack, int rhs, int opt, int lhs)
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

static int int_spdeleterows(Stack stack, int rhs, int opt, int lhs)
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

static int int_spextract(Stack stack, int rhs, int opt, int lhs)
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

static int int_spextractelts(Stack stack, int rhs, int opt, int lhs)
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

static int int_spextractcols(Stack stack, int rhs, int opt, int lhs)
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

static int int_spextractrows(Stack stack, int rhs, int opt, int lhs)
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
 * Returns the kthe diag of a Matrix 
 */

static int int_spdiage(Stack stack, int rhs, int opt, int lhs)
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

static int int_spdiagset(Stack stack, int rhs, int opt, int lhs)
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

static int int_spdiagcre(Stack stack, int rhs, int opt, int lhs)
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

static int int_spm2sp(Stack stack, int rhs, int opt, int lhs)
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

static int int_spsp2m(Stack stack, int rhs, int opt, int lhs)
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

static int int_spm2m(Stack stack, int rhs, int opt, int lhs)
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

static int int_spmult_gen(Stack stack, int rhs, int opt, int lhs, Sp21 F)
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

static int int_spmult(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmult_gen(stack,rhs,opt,lhs,nsp_spmatrix_mult);
}

/*
 *   Res = A * X , A sparse matrix, X full matrix
 *   A and X are left unchanged
 *   added by Bruno
 */
static int int_spmultm(Stack stack, int rhs, int opt, int lhs)
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

static int int_spquote(Stack stack, int rhs, int opt, int lhs)
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
 * C = A + B with special cases [] and [x] 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

static int int_spplus(Stack stack, int rhs, int opt, int lhs)
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

static int int_spsub(Stack stack, int rhs, int opt, int lhs)
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
	  int_spminus(stack,1,opt,1);
	  stack.first -= 1;
	  SwapObjs(stack,1,2);
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

static int int_spmultt(Stack stack, int rhs, int opt, int lhs)
{
  return int_spmult_gen(stack,rhs,opt,lhs,nsp_spmatrix_multtt);
}

/*
 *nsp_mat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspSpMatrix *(*SuPro) (NspSpMatrix *A,char *);

static int int_sp_sum(Stack stack, int rhs, int opt, int lhs, SuPro F)
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
static int int_spnnz(Stack stack, int rhs, int opt, int lhs)
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

static int int_spsum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_sp_sum(stack,rhs,opt,lhs,SpSum ) );
}


/*
 *nsp_mat_maxi: Maxi(*HMat);
 * A is unchanged 
 * XXXXXX : pas fini 
 */

typedef NspSpMatrix *(*SpMiMax) (NspSpMatrix *A,char *,NspMatrix **Imax,int lhs);

static int int_sp_maxi(Stack stack, int rhs, int opt, int lhs, SpMiMax F)
{
  char *str;
  NspSpMatrix *A,*M;
  NspMatrix *Imax;
  if ( rhs < 1) 
    { 
      Scierror("Error:\t Rhs must be >= 1 for function %s\n",stack.fname);
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


static int int_spmaxi(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_sp_maxi(stack,rhs,opt,lhs,SpMaxi));
}



/*
 * The Interface for basic matrices operation
 */

static OpTab SpMatrix_func[]={
  {"create_sp",int_spcreate},
  {"m2sp",int_spm2sp},
  {"sp2m",int_spsp2m},
  {"dst_sp_sp",int_spmultt},
  {"mult_sp_sp",int_spmult},
  {"mult_sp_m",int_spmultm},
  {"plus_sp_sp",int_spplus},
  {"minus_sp_sp",int_spsub},
  {"minus_sp",int_spminus},
  {"quote_sp",int_spquote},
  {"multt_sp_sp",int_spmultt}, 
  {"spredim",int_spredim},
  {"concatd_sp_sp" ,  int_spconcatd },
  {"concatr_sp_sp" ,  int_spconcatr },
  {"concatdiag_sp_sp" ,  int_spconcatdiag },
  {"deletecols_sp_m", int_spdeletecols},
  {"deleterows_sp_m", int_spdeleterows},
  {"extract_sp",int_spextract},
  {"extractrows_sp",int_spextractrows},
  {"extractcols_sp",int_spextractcols},
  {"diage_sp" ,  int_spdiage },
  {"diage_sp_m" ,  int_spdiage },
  {"diagset_sp" ,  int_spdiagset },
  {"diagcre_sp" ,  int_spdiagcre },
  {"diagcre_sp_m" ,  int_spdiagcre },
  {"sparse", int_spsparse},
  {"spget", int_spget},
  {"full_sp",int_spsp2m},
  {"full_m",int_spm2m},
  {"sum_sp_s" ,  int_spsum },
  {"sum_sp" ,  int_spsum },
  {"setrowscols_sp",int_spsetrc},
  {"maxi_sp" ,  int_spmaxi },
  {"maxi_sp_s" ,  int_spmaxi },
  {"extractelts_sp",int_spextractelts},
  {"nnz_sp",int_spnnz},
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








