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
  
#include "nsp/object.h"
#include "nsp/pr-output.h"
#include "nsp/blas.h"
#include "nsp/matutil.h" 
#include "nsp/gsort-p.h" 
#include "nsp/cnumeric.h" 

static void nsp_sprowmatrix_print_internal(nsp_num_formats *fmt,NspSpRowMatrix *m, int indent);
/* In file Perm.c **/

extern int C2F(dperm) (double A[],int ind[],int *nv);
extern int C2F(zperm) (doubleC A[],int ind[],int *nv);

typedef void (*BopLeft) (SpRow *,char,int *,SpRow *,char,int);
typedef void (*BopBoth) (SpRow *,char,int *,SpRow *,char,int,SpRow *,char,int);
typedef void (*BopRight) (SpRow *,char,int *,SpRow *,char,int);

static NspSpRowMatrix *BinaryOp (NspSpRowMatrix *,NspSpRowMatrix *,BopLeft,BopBoth,
			      BopRight);

static void PlusLeft (SpRow *,char,int *,SpRow *,char,int);
static void PlusBoth (SpRow *,char,int *,SpRow *,char,int,SpRow *,char,int);
static void PlusRight (SpRow *,char,int *,SpRow *,char,int);

static void MinusLeft (SpRow *,char,int *,SpRow *,char,int);
static void MinusBoth (SpRow *,char,int *,SpRow *,char,int,SpRow *,char,int);
static void MinusRight (SpRow *,char,int *,SpRow *,char,int);

static void MultttLeft (SpRow *,char,int *,SpRow *,char,int);
static void MultttBoth (SpRow *,char,int *,SpRow *,char,int,SpRow *,char,int);
static void MultttRight (SpRow *,char,int *,SpRow *,char,int);

static int nsp_dichotomic_search(int x,const int val[],int imin,int imax);

static int nsp_bi_dichotomic_search(const double x[],int xpmin,int xpmax,const int val[],int imin,int imax,
				    NspMatrix *Work,NspMatrix *Index,int count);



/**
 * nsp_spcolmatrix_cast_to_sprow:
 * @M: a NspMatrix *
 * 
 * Changes the type fields of @M in such a way that 
 * @M becomes a NspSpRowMatrix. Note that 
 * the casted matrix contains the representation of the 
 * transpose of @M.
 * 
 * Returns a #NspMaxpMatrix or %NULLMAXPMAT.
 */

NspSpRowMatrix * nsp_spcolmatrix_cast_to_sprow(NspSpColMatrix *M)
{
  if ( M == NULL) return NULLSPROW;
  M->type = (NspTypeSpColMatrix *) new_type_sprowmatrix(T_BASE);
  NSP_OBJECT(M)->type =(NspTypeObject *) M->type->surtype;
  NSP_OBJECT(M)->basetype =(NspTypeBase *) M->type;
  return (NspSpRowMatrix *) M;
}

/**
 * nsp_sprowmatrix_cast_to_spcol:
 * @M: a NspMatrix *
 * 
 * Changes the type fields of @M in such a way that 
 * @M becomes a #NspSpColMatrix. Note that 
 * the casted matrix contains the representation of the 
 * transpose of @M.
 * 
 * Returns a #NspMaxpMatrix 
 */

NspSpColMatrix * nsp_spowmatrix_cast_to_spcol(NspSpRowMatrix *M)
{
  if ( M == NULL ) return NULLSPCOL;
  M->type = (NspTypeSpRowMatrix *) new_type_spcolmatrix(T_BASE);
  NSP_OBJECT(M)->type =(NspTypeObject *) M->type->surtype;
  NSP_OBJECT(M)->basetype =(NspTypeBase *) M->type;
  return (NspSpColMatrix *) M;
}

/*
 * Creates a Sp Matrix of size mxn with no stored date
 * Attention on peut creer une nx0 matrice XXXXX
 */

NspSpRowMatrix *nsp_sprowmatrix_create(char *name, char type, int m, int n)
{
  NspSpColMatrix *loc; 
  /* create the tranpose in spcol mode */
  if ((loc = nsp_spcolmatrix_create(name,type,n,m))== NULL) return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/*
 * Creation of a Sparse Matrix with specified data
 * Scilab function sparse(rowcols,vals,[m,n])
 * if m and n  have -1 value, sizes are to be computed from 
 * rowscols (RC)
 * 
 */

NspSpRowMatrix *nsp_sprowmatrix_sparse(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n)
{
  NspSpColMatrix *loc=NULL,*loc1=NULL;
  if (( loc = nsp_spcolmatrix_sparse(name,RC,Values,m,n))== NULL) return NULLSPROW;
  loc1= nsp_spcolmatrix_transpose(loc);
  nsp_spcolmatrix_destroy(loc);
  if (loc1 == NULL) return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc1);
}


/*
 * Scilab function [rc,vals,mn]= spget(sp)
 */

int nsp_sprowmatrix_get(NspSpRowMatrix *A, NspMatrix **RC, NspMatrix **Values)
{
  int i;
  if ( nsp_spcolmatrix_get((NspSpColMatrix *)A,RC,Values)== FAIL) return FAIL;
  for ( i = 0 ; i < (*RC)->m ; i++)
    {
      double val = (*RC)->R[i];
      (*RC)->R[i]= (*RC)->R[i+(*RC)->m];
      (*RC)->R[i+(*RC)->m]=val;
    }
  return OK;
}

/*
 * Res =nsp_sprowmatrix_copy(A) 
 * Copy a sparse matrix in Res : res is created 
 */

NspSpRowMatrix *nsp_sprowmatrix_copy(NspSpRowMatrix *A)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_copy((NspSpColMatrix *) A))==NULL) return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/*
 * Resize the ith Row 
 * if Row->size = 0 the Row is created with n potential elements 
 * if Row->size != 0 the Row is resized ( enlarged or reduced ) 
 * according to n.
 * i : number of the Row which is to be resized 
 * WARNING : i must be in [0,Sp->m[ and this is not checked here
 * WARNING : Sp->n is not changed this is to be done elsewhere.
 */

int nsp_sprowmatrix_resize_row(NspSpRowMatrix *Sp, int i, int n)
{
  SpRow *Row;
  int cp = ( Sp->rc_type == 'c') ? 2 : 1;
  Row = Sp->D[i];
  if ( Row->size == 0 ) 
    {
      if ( n <= 0 ) return(OK);
      if ((Row->J =nsp_alloc_int((int) n)) == (int *) 0) return(FAIL);
      /* note that all data are in a union */
      if ((Row->R =nsp_alloc_doubles(n*cp)) == (double *) 0 ) return(FAIL);
      Row->size = n;
      return(OK);
    }
  if ( Row->size == n  ) return(OK);
  
  if ( n <= 0 ) 
    {
      /* empty new size **/
      FREE(Row->J);
      FREE(Row->R);
      Row->size = 0;
      return(OK);
    }
  if ((Row->J =nsp_realloc_int(Row->J, n))  == (int *) 0) return(FAIL);
  if (( Row->R =nsp_realloc_doubles(Row->R, n*cp)) == (double *) 0 ) return(FAIL);
  Row->size = n;
  return(OK);
}

/*
 * Destroy a Sp Matrix 
 */

void nsp_sprowmatrix_row_destroy(SpRow *Row)
{
  if ( Row->size != 0 ) 
    {
      FREE( Row->J);
      FREE( Row->R);
    }
}

void nsp_sprowmatrix_destroy(NspSpRowMatrix *Mat)
{
  nsp_spcolmatrix_destroy((NspSpColMatrix *) Mat);
}

/*
 *  nsp_sprowmatrix_nnz: computes the number of non nul elements
 *  (Number of Non Zero elements) of a  Sparse Matrix
 *  (added by Bruno)
 */
int nsp_sprowmatrix_nnz(const NspSpRowMatrix *HMat)
{
  return nsp_spcolmatrix_nnz((const NspSpColMatrix *) HMat);
}

/*
 *nsp_sprowmatrix_info: display Info on Sparse Matrix
 */

void nsp_sprowmatrix_info(NspSpRowMatrix *Sp, int indent,char *name,int rec_level)
{ 
  int i;
  if ( Sp == NULLSPROW) 
    {
      Sciprintf("Null SpRowMatrix pointer\n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( strcmp(NSP_OBJECT(Sp)->name,NVOID) == 0)
    Sciprintf("SpRowMatrix %c (%dx%d)\n",Sp->rc_type, Sp->m,Sp->n);
  else
    Sciprintf("Spmatrix %s %c (%dx%d)\n",NSP_OBJECT(Sp)->name,Sp->rc_type, Sp->m,Sp->n);
}


/*
 *	 Writes Sp, Sp remains unchanged 
 */

void nsp_sprowmatrix_print(NspSpRowMatrix *Sp, int indent,char *name, int rec_level)
{ 
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Sp)->name; 
  if (user_pref.pr_as_read_syntax)
    {
      const int name_len=128;
      char epname[name_len];
      NspMatrix *RC,*Values;
      if ( nsp_sprowmatrix_get(Sp,&RC,&Values)== FAIL)
	{
	  Sciprintf("Error: failed to print sparse matrix as_read\n");
	  return;
	}
      sprintf(epname,"%s__rc",pname);
      nsp_matrix_print(RC,indent,epname,rec_level);
      sprintf(epname,"%s__val",pname);
      nsp_matrix_print(Values,indent,epname,rec_level);
      nsp_matrix_destroy(RC);
      nsp_matrix_destroy(Values);
      Sciprintf1(indent,"%s=sprow_sparse(%s__rc,%s__val,[%d,%d]);\n",pname,pname,pname,Sp->m,Sp->n);
      Sciprintf1(indent,"clear('%s__rc','%s__val')\n",pname,pname);
    }
  else
    {
      if (Sp->mn==0 ) 
	{
	  Sciprintf1(indent,"%s\t= []\t\tsprow %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	}
      else
	{
	  nsp_num_formats fmt;
	  if ( user_pref.pr_depth  <= rec_level -1 ) 
	    {
	      Sciprintf1(indent,"%s\t= [...]\t\tsprow %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	      return;
	    }
	  nsp_init_pr_format (&fmt);
	  Sciprintf1(indent,"%s\t=\t\tsprow %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	  nsp_sprowmatrix_print_internal(&fmt,Sp,indent+1);
	}
    }
}

/*
 *nsp_sprowmatrix_redim: Changes matrix dimensions to mxn 
 *   the product m*n must be unchanged and 
 *   the stored data is kept considering that data are stored 
 *   columnwise. 
 *   a New Matrix is returned or NULLSPROW on failure
 */

NspSpRowMatrix *nsp_sprowmatrix_redim(NspSpRowMatrix *A, int m, int n)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_redim((NspSpColMatrix *) A,n,m))==NULL) return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/*
 * Add m empty Rows ( Row->size = 0) to a Sparse matrix 
 * if m < Sp->m then Sp is unchanged. 
 */

int nsp_sprowmatrix_enlarge_rows(NspSpRowMatrix *Sp, int m)
{
  return nsp_spcolmatrix_enlarge_cols((NspSpColMatrix *) Sp, m);
}

/*
 *nsp_sprowmatrix_enlarge(A,m,n)
 * changes A to B= [ A , 0; 0,0 ]  
 * in such a way that B (max(A->m,m)xmax(A->n,n));
 * The result is stored in A
 * if m and n are smaller than A->m and A->n Matrix 
 * associated dimensions are not changed 
 */

int nsp_sprowmatrix_enlarge(NspSpRowMatrix *A, int m, int n)
{
  /* special case **/
  if ( n > A->n  ) A->n = n ; /* easy for sparse matrix **/
  if ( m > A->m  ) 
    return nsp_sprowmatrix_enlarge_rows(A,m);
  return OK;
}

/*
 * A = [A, B] 
 * Right concatenation on A, A is changed 
 */

int nsp_sprowmatrix_concatr(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return  nsp_spcolmatrix_concatd((NspSpColMatrix *) A,(NspSpColMatrix *) B);
}

/*
 *  A=[A; B ] 
 *   Down concatenation on A 
 */

int nsp_sprowmatrix_concatd(NspSpRowMatrix *A, NspSpRowMatrix *B)
{ 
  return  nsp_spcolmatrix_concatr((NspSpColMatrix *) A,(NspSpColMatrix *) B);
}


/*
 * Diag Concatenation 
 * A = [A,0;0,B] 
 * A is changed  B is left unchanged 
 */

int nsp_sprowmatrix_concatdiag(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return nsp_spcolmatrix_concatdiag((NspSpColMatrix *) A,(NspSpColMatrix *) B);
}

/*
 * Utility functions 
 *nsp_sprowmatrix_insert_elt(A,i,j,B,rb,cb)
 *nsp_sprowmatrix_delete_elt(A,row,col,amin,amax)
 *nsp_sprowmatrix_get_elt(B,i,j)
 * nsp_sprowmatrix_store(A,r,c,col,B,r1,c1)
 */

/* Utility function **/

void  nsp_sprowmatrix_store(NspSpRowMatrix *A, int r, int c, int col, NspSpRowMatrix *B, int r1, int c1)
{
  A->D[r]->J[c] = col;
  switch ( A->rc_type )
    {
    case 'r' : 
      switch ( B->rc_type ) 
	{
	case 'r' : A->D[r]->R[c] = B->D[r1]->R[c1]; break;
	case 'c' : A->D[r]->R[c] = B->D[r1]->C[c1].r; break;
	}
      break;
    case 'c' :
      switch ( B->rc_type ) 
	{
	case 'r' : A->D[r]->C[c].r = B->D[r1]->R[c1]; A->D[r]->C[c].i = 0.0;
	  break;
	case 'c' : A->D[r]->C[c] = B->D[r1]->C[c1]; break;
	}
      break;
    }
}

/* Insert or change A(i,j) to B(rb,cb) **/

int nsp_sprowmatrix_insert_elt(NspSpRowMatrix *A, int i, int j, NspSpRowMatrix *B, int rb, int cb)
{
  SpRow *Ai = A->D[i];
  int ok = -1,insert=0,k;
  for ( k =0 ; k < Ai->size ; k++ )
    {
      if ( j == Ai->J[k] ) { ok = k; break ;}
      if ( j <  Ai->J[k] ) { insert= k; break ;}
    }
  if ( ok == -1 ) 
    {
      /* we must enlarge the Row **/
      if (nsp_sprowmatrix_resize_row(A,i,Ai->size+1) == FAIL) return FAIL;
      /* Get the new row **/
      Ai= A->D[i];
      /* we insert the new value **/
      if ( insert == 0) 
	{
	  /* insert at the end **/
	  nsp_sprowmatrix_store(A,i,Ai->size-1,j,B,rb,cb);
	}
      else
	{
	  int k1;
	  /* insert at position insert **/
	  /* move right one step **/
	  for ( k1 = Ai->size -1 ; k1 >= insert ; k1--) 
	    {
	      nsp_sprowmatrix_store(A,i,k1+1,Ai->J[k1],A,i,k1);
	    }
	  nsp_sprowmatrix_store(A,i,insert,j,B,rb,cb);
	}
    }
  else
    {
      /* we must change element **/
      nsp_sprowmatrix_store(A,i,ok,j,B,rb,cb);
    }
  return OK;
}

/* 
   Remove element A(i,j) but the associated row is not resized 
   returns -1 if A(i,j) was zero before the call (i.e no change )
   return  k  if A(i,j) is removed and was the k-th non null element 
   of i-th row of A 
   A(i,j) is searched in the i-th row of A but only in the range 
   amin,amax 
   row = i, col=j 
**/


int nsp_sprowmatrix_delete_elt(NspSpRowMatrix *A, int row, int col, int amin, int amax)
{
  int acol,ok1=0,col1,k1;
  /* search if corresponding element exists 
   * FIXME: should make a dichotomic search here !!! 
   */

  for ( acol = amin ; acol < amax ; acol++) 
    {
      if ( col == A->D[row]->J[acol] ) { ok1 = 1; col1 = acol;break;}
      if ( col < A->D[row]->J[acol] )  { break;}
    }
  /* perform deletion if necessary  **/
  if (  ok1 == 1) 
    {
      /* delete A(row,col) **/  
      /* move left one step **/
      for ( k1 = acol+1 ; k1 < amax ; k1++) 
	{
	  nsp_sprowmatrix_store(A,row,k1-1,A->D[row]->J[k1],A,row,k1);
	}
      return acol;
    }
  else
    {
      return -1;
    }
}

/* return k such that B->D[i]->J[k] = j or -1 if such k does not exists **/

int nsp_sprowmatrix_get_elt(NspSpRowMatrix *B, int i, int j)
{
  SpRow *Bi = B->D[i];
  int ok = -1,k;
  for ( k =0 ; k < Bi->size ; k++ )
    {
      if ( j == Bi->J[k] ) { ok = k; break ;}
      if ( j <  Bi->J[k] ) {  break ;}
    }
  return ok;
}


/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked between 
 *  A and B 
 *  XXXXX : Cols doit-il etre croissant ?
 *     ---> A priori non 
 *          Ce qui fait un BUG pour l'instant 
 */

int nsp_sprowmatrix_set_rowcol(NspSpRowMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspSpRowMatrix *B)
{
  int Bnonnul = FALSE;
  char type ='r';
  int rmin,rmax,cmin,cmax,i,k,l,acol;
  /* Check compatibility : B is a scalar or B must have compatible 
     size with Rows and Cols : Note that B=[] is treated elsewhere **/
  if ( B->mn != 1)
    {
      if ( Rows->mn != B->m ||  Cols->mn != B->n )
	{
	  Scierror("Error:\tIncompatible dimensions\n");
	  return(FAIL);
	}
    }
  else
    {
      /* B is a scalar check if it is a null scalar **/
      if ( B->D[0]->size !=0) Bnonnul = TRUE;
    }
  Bounds(Rows,&rmin,&rmax);
  Bounds(Cols,&cmin,&cmax);
  if ( rmin < 1 || cmin < 1 ) 
    {
      Scierror("Error:\tNegative indices are not allowed\n");
      return(FAIL);
    }
  /* Enlarge A if necessary **/
  if ( rmax > A->m ||  cmax > A->n ) 
    if (nsp_sprowmatrix_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  /* Id result complex ? **/
  if ( B->rc_type == 'c' &&   A->rc_type == 'r' )
    { 
      type = 'c';
      if (nsp_sprowmatrix_seti(A,0.00) == FAIL ) return(FAIL);
    }
  /* fill A **/
  for ( i = 0 ; i < Rows->mn ; i++ ) 
    {
      /* The row of A to be changed **/
      int row = ((int) Rows->R[i])-1;
      int Ais = A->D[row]->size ;
      int amin = 0 ;
      int amax = Ais;
      int ib,nel ;
      if ( B->mn == 1)
	{
	  ib = 0;
	  nel = Cols->mn;
	}
      else
	{
	  ib =i;
	  nel = B->D[i]->size ;
	}
      /* The new row will have at most nel + A->D[row]->size non nul elements **/
      if (nsp_sprowmatrix_resize_row(A,row ,nel+Ais) == FAIL) return FAIL;
      for ( k =0 ; k < Cols->mn ; k++ )
	{
	  int ok = -1;
	  int col = ( (int) Cols->R[k])-1;
	  int ok1,col1,k1,kb;
	  kb = ( B->mn == 1) ? 0 : k; /* if B is scalar **/
	  /* search if the kth element of B->D[ib] is non null **/
	  for ( l = 0 ; l < B->D[ib]->size ; l++) 
	    {
	      if ( kb == B->D[ib]->J[l] ) { ok=l ; break;}
	      if ( kb < B->D[ib]->J[l] ) { break;}
	    }
	  if ( ok != -1 ) 
	    {
	      ok1=0;
	      /* we must set the [row,col] element of A to B(ib,ok) **/
	      /* search where to insert **/
	      for ( acol = amin ; acol < amax ; acol++) 
		{
		  if ( col == A->D[row]->J[acol] ) { ok1 = 1; col1 = acol;break;}
		  if ( col < A->D[row]->J[acol] )  { ok1 = 2; col1 = acol; break;}
		}
	      /* perform insertion **/
	      switch ( ok1 ) 
		{
		case 1: /* replace A(row,col) **/  
		  nsp_sprowmatrix_store(A,row,acol, col, B,ib,ok);
		  /*Cols is not supposed to be increasing  amin = acol ;  **/
		  break;
		case 2: /* insert before acol **/
		  /* move right one step **/
		  for ( k1 = amax -1 ; k1 >= acol ; k1--) 
		    {
		      nsp_sprowmatrix_store(A,row,k1+1,A->D[row]->J[k1],A,row,k1);
		    }
		  nsp_sprowmatrix_store(A,row,acol,col,B,ib,ok);
		  /*Cols is not supposed to be increasing amin = acol ; **/
		  amax++ ; break ;
		default : 
		  /* insert at end **/
		  nsp_sprowmatrix_store(A,row,amax,col,B,ib,ok);
		  /* Cols is not supposed to be increasing  amin = amax ; **/
		  amax++;
		}
	    }
	  else 
	    {
	      /* we must set the [row,col] element of A to 0.0 **/
	      ok1 =nsp_sprowmatrix_delete_elt(A,row,col,amin,amax);
	      if ( ok1 != -1 ) 
		{
		  /* Cols is not supposed to be increasing  amin = ok1; **/
		  amax--;
		}
	    }
	}
      /* we resize A(row,:) to its correct size **/
      if (nsp_sprowmatrix_resize_row(A,row ,amax) == FAIL) return FAIL;
    }
  return(OK);
}

/*
 *  A(Inds) = B 
 *  A is changed and enlarged if necessary 
 *  Inds is unchanged 
 *  Size Compatibility is checked 
 * 
 *  Rules : A Matrix or A ==[]  
 *	A(Inds)=B 
 *	B must be row or column 
 *	if A==[] the size of the result depends on b 
 *      A row vector B must be row 
 *      A column vector B must be column 
 *      Inds must be in the range of A indices unless A is row or column or []
 *      Inds and B must have the same size or B must be scalar 
 */

int nsp_sprowmatrix_set_row(NspSpRowMatrix *A, NspMatrix *Inds, NspSpRowMatrix *B)
{
  int i;
  int Bscal=0;
  /* Calling a genric function which check arguments **/
  if (GenericMatSeRo(A,A->m,A->n,A->mn,Inds,
		     B,B->m,B->n,B->mn,(F_Enlarge)nsp_sprowmatrix_enlarge,&Bscal)== FAIL) 
    return FAIL;
  /* */
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_sprowmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if ( Bscal == 0 ) 
    {
      for ( i = 0  ; i < Inds->mn ; i++) 
	{
	  int rb,cb,kb,ia,ra,ca ;
	  rb= i % Inds->m;
	  cb= (i - rb )/Inds->m;
	  kb =nsp_sprowmatrix_get_elt(B,rb,cb);
	  ia = ((int) Inds->R[i])-1;
	  ra = ia % A->m;
	  ca= (ia - ra )/A->m;
	  if ( kb == -1 ) 
	    {
	      /* we must set the [ra,ca] element of A to 0.0 **/
	      int ok1 =nsp_sprowmatrix_delete_elt(A,ra,ca,0,A->D[ra]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_sprowmatrix_resize_row(A,ra,A->D[ra]->size-1) == FAIL) return FAIL;
		}
	    }
	  else
	    {
	      /* must change or insert element in A **/
	      if (nsp_sprowmatrix_insert_elt(A,ra,ca,B,rb,kb)== FAIL) return FAIL;
	    }
	}
    }
  else
    {
      /* Here B is a scalar **/ 
      if ( B->D[0]->size == 0) 
	{
	  /* B is a null scalar sparse matrix **/
	  /* we must remove elements from A **/
	  for ( i = 0  ; i < Inds->mn ; i++) 
	    {
	      int ia,ra,ca,ok1 ;
	      ia = ((int) Inds->R[i])-1;
	      ra = ia % A->m;
	      ca= (ia - ra )/A->m;
	      /* we must set the [ra,ca] element of A to 0.0 **/
	      ok1 =nsp_sprowmatrix_delete_elt(A,ra,ca,0,A->D[ra]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_sprowmatrix_resize_row(A,ra,A->D[ra]->size-1) == FAIL) return FAIL;
		}
	    }
	}
      else 
	{
	  /* B is a non null scalar **/
	  for ( i = 0  ; i < Inds->mn ; i++) 
	    {
	      int ia,ra,ca ;
	      ia = ((int) Inds->R[i])-1;
	      ra = ia % A->m;
	      ca= (ia - ra )/A->m;
	      if (nsp_sprowmatrix_insert_elt(A,ra,ca,B,0,0)== FAIL) return FAIL;
	    }
	}
    }
  return(OK);
}

/*
 *  A(:,Cols) = []
 *  A is changed. 
 *  Cols must be strictly increasing . XXXXXXX
 *  A changer 
 */

int nsp_sprowmatrix_delete_cols(NspSpRowMatrix *A, NspMatrix *Cols)
{
  int cmin,cmax,i,j,k;
  if ( Cols->mn == 0) return(OK);
  Bounds(Cols,&cmin,&cmax);
  if ( cmin < 1 || cmax > A->n ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      return(FAIL);
    }
  for ( i = 0 ; i < A->m ; i++) 
    {
      int ndel;
      SpRow *Ai = A->D[i];
      Ai->iw=0;
      for ( j = 0 ; j < Cols->mn ; j++)
	{
	  int col = ((int) Cols->R[j]) -1;
	  /* test if col is non nul in row i **/
	  /* we use the fact that Cols->R is increasing and Ai->J too 
	     to limit the search at each step. 
	     The elements which are to be deleted are marked with -1 
	  **/
	  for ( k = Ai->iw ; k < Ai->size ; k++) 
	    {
	      if (Ai->J[k] == col) { Ai->J[k]=-1; Ai->iw = k+1 ; break;}
	      if (col < Ai->J[k]) break;
	    }
	}
      ndel =nsp_sprowmatrix_compress_row(A,i);
      if (nsp_sprowmatrix_resize_row(A,i, Ai->size-ndel ) == FAIL) return(FAIL) ;
    }
  A->n -= Cols->mn ;
  /* XXXX : Attention ici si A->n devient egal a zero 
     Il faut detruire les ligne pour renvoyer une matrice vide **/

  return(OK);
}

/* compress ith row of sparse Matrix A **/
/* [1,2,-1,4,5,6,-1,11]--> [1,2,4,5,6,11] **/
/* return the number of deleted elements **/

int nsp_sprowmatrix_compress_row(NspSpRowMatrix *A, int i)
{
  int first = -1,next=-1,ioff=1,j,k;
  SpRow *R = A->D[i];
  for ( j = 0 ; j < R->size ; j++ )
    {
      if ( R->J[j] == -1 ) 
	{ first = j ; break;}
    }
  if (first == -1 ) return 0;
  while (1) 
    {
      /* find next -1 **/
      for ( j = first + 1; j < R->size ; j++ ) 
	{
	  if ( R->J[j] == -1 ) { next = j ; break;}
	}
      if ( next == -1 ) { next = R->size ;} 
      /* move left  **/
      for ( k = first + 1 ; k < next ; k++) 
	{
	  R->J[k-ioff]=R->J[k]-ioff;
	  switch ( A->rc_type ) 
	    {
	    case 'r' : R->R[k-ioff] = R->R[k];break;
	    case 'c' : R->C[k-ioff] = R->C[k];break;
	    }
	}
      if ( next == R->size ) return ioff;
      /* update **/
      first = next;
      next = -1;
      ioff++;
    }
  return ioff;
}


/*
 *  A(Rows,:) = []
 *  A is changed. 
 *  Rows must be increasing XXXXXXXXXXX
 *  A Changer 
 */

int nsp_sprowmatrix_delete_rows(NspSpRowMatrix *A, NspMatrix *Rows)
{
  int rmin,rmax,i,k,ind,next,ioff=1;
  Bounds(Rows,&rmin,&rmax);
  if ( Rows->mn == 0) return(OK);
  if ( rmin < 1 || rmax > A->m ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      return(FAIL);
    }
  /* free the rows **/
  for ( i = 0 ; i < Rows->mn  ; i++)
    {
      ind =  ((int) Rows->R[i])-1; /* row to be deleted **/
      if (nsp_sprowmatrix_resize_row(A,ind,0) == FAIL) return FAIL; /* free the associated row **/
      FREE(A->D[ind]);
    }
  /* Move block data left in A->D  **/
  for ( i = 0 ; i < Rows->mn -1 ; i++)
    {
      ind =  ((int) Rows->R[i])-1; /* row to be deleted **/
      next = ((int) Rows->R[i+1])-1; /* next row to be deleted **/
      for ( k = ind+1; k < next ; k++ ) 
	A->D[k-ioff] = A->D[k];
      ioff++;
    }
  /* Move last block data left **/
  for ( k = ((int) Rows->R[Rows->mn-1]); k < A->m ; k++) 
    A->D[k-ioff] = A->D[k];
  /* free unused memory **/
  A->m = A->m - Rows->mn;
  A->D = REALLOC(A->D, A->m*sizeof( SpRow *));
  if ( A->D == NULL) 
    {
      Scierror("No More Space\n");
      return FAIL;
    }
  return OK;
}


/*
 *  A(elts) = []
 *  A is changed. 
 *  elts must be increasing XXXXXXXXXXX
 *  A Changer et ecrire 
 */

/*
 * Res=nsp_matrix_extract(A,Rows,Cols)
 * A unchanged, Rows and Cols are unchanged 
 */	

static NspSpRowMatrix *SpExtract_G(NspSpRowMatrix *A, NspMatrix *Rows, NspMatrix *Cols, int flag, int *err)
{
  NspMatrix *Work= NULL, *Index = NULL;
  NspSpRowMatrix *Loc;
  int rmin,rmax,cmin,cmax,i,j,Rm;
  if ( A->mn == 0) return nsp_sprowmatrix_create(NVOID,A->rc_type,0,0);
  if (flag == 1) 
    {
      Bounds(Rows,&rmin,&rmax);
      if ( rmin < 1 ||  rmax > A->m  ) 
	{
	  *err=1;
	  Scierror("Error:\tIndices out of bound\n");
	  return(NULLSPROW);
	}
    }
  Index = nsp_mat_sort (Cols,2,"g","i");
  cmin = Cols->R[0]; cmax = Cols->R[Cols->mn-1];
  *err=0;
  if (  cmin < 1 ||  cmax > A->n ) 
    {
      *err=1;
      return(NULLSPROW);
    }
  Rm= (flag == 1) ? Rows->mn : A->m;
  if ( (Loc =nsp_sprowmatrix_create(NVOID,A->rc_type,Rm,Cols->mn))== NULLSPROW) 
    return(NULLSPROW);
  /* used to store elements */
  if ( ( Work = nsp_matrix_create(NVOID,'r',2,Cols->mn)) == NULLMAT) return NULLSPROW;
  
  for ( i = 0 ; i < Loc->m ; i++)
    {
      int count;
      int imin,imax,k;
      SpRow *Ai, *Li;
      int row;
      row = (flag == 1) ? ((int) Rows->R[i])-1 : i ;
      Ai= A->D[row];
      Li= Loc->D[i];
      Li->iw=0;
      if ( Ai->size == 0) continue; /* nothing to do row is empty */
      imin=0; imax= Ai->size-1 ; k = -1;
      Li->iw=0;
      count = nsp_bi_dichotomic_search(Cols->R,0,Cols->mn-1,Ai->J,imin,imax,Work,Index,0);
      /* now we know the row size */
      if (nsp_sprowmatrix_resize_row(Loc,i,count)==FAIL) return NULLSPROW;
      /* Fill columns of i-th row **/
      Li->iw=0;
      for ( j = 0 ; j < Li->size ; j++ )
	{	
	  int k= (int) Work->R[1+2*j];
	  Li->J[j]= (int) Work->R[2*j];
	  switch ( A->rc_type ) 
	    {
	    case 'r' : Li->R[j]= Ai->R[k];break;
	    case 'c' : Li->C[j]= Ai->C[k];break;
	    }
	}
    }
  nsp_matrix_destroy(Work);
  return(Loc);
}




NspSpRowMatrix *nsp_sprowmatrix_extract(NspSpRowMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  NspSpRowMatrix *Sp;
  int err;
  Sp=SpExtract_G(A,Rows,Cols,1,&err);
  if (err==1 ) Scierror("Error:\tIndices out of bound\n");
  return Sp;
}
/*
 * Res=nsp_matrix_extract_elements(A,Elts)
 * A unchanged, Elts
 */


NspSpRowMatrix *nsp_sprowmatrix_extract_elts(NspSpRowMatrix *A, NspMatrix *Elts)
{
  NspSpRowMatrix *Loc;
  int rmin,rmax,i,err,k;
  Bounds(Elts,&rmin,&rmax);
  if ( A->mn == 0) return nsp_sprowmatrix_create(NVOID,A->rc_type,0,0);
  if ( rmin < 1 || rmax > A->mn )
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLSPROW);
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      NspMatrix *Rows ; 
      if ((Rows = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT ) return NULLSPROW;
      Rows->R[0] = 1;
      /* like A(1,Elts) **/
      Loc=  SpExtract_G(A,Rows,Elts,1,&err);
      if (err==1 ) Scierror("Error:\tIndices out of bound\n");
      return Loc;
    }
  else
    {
      if ( (Loc =nsp_sprowmatrix_create(NVOID,A->rc_type,Elts->mn,1))== NULLSPROW) 
	return(NULLSPROW);
      for ( i=0; i < Elts->mn ; i++) 
	{
	  int el = ((int)Elts->R[i]) -1;
	  int row = el % A->m ;   
	  int col = (el-row)/A->m;
	  /* Loc[i,0] = A[row,col] **/
	  int ok = -1;
	  for ( k = 0 ; k < A->D[row]->size ; k++) 
	    {
	      if ( col == A->D[row]->J[k] ) { ok = k ; break;}
	      if ( col <  A->D[row]->J[k] ) { break;}
	    }
	  if ( ok != -1) 
	    {
	      /*  A[row,col] != 0 **/
	      if (nsp_sprowmatrix_resize_row(Loc,i,1)==FAIL) return NULLSPROW;
	      Loc->D[i]->J[0]=0;
	      switch ( A->rc_type ) 
		{
		case 'r' : Loc->D[i]->R[0] =  A->D[row]->R[k]; break;
		case 'c' : Loc->D[i]->C[0] =  A->D[row]->C[k]; break;
		}
	    }
	}
      return(Loc);
    }
}

/*
 * Res=nsp_matrix_extract_columns(A,Cols,err)
 * A unchanged
 * err is used inside for-loops 
 */	

NspSpRowMatrix *nsp_sprowmatrix_extract_cols(NspSpRowMatrix *A, NspMatrix *Cols, int *err)
{
  return SpExtract_G(A,NULLMAT,Cols,0,err);
}

/*
 * A1=MatLoopCol(A1,M,i,rep)
 * Used in for loops 
 */	


/*
 * Res=nsp_matrix_extract_rows(A,Rows,err)
 * A unchanged
 */	

NspSpRowMatrix *nsp_sprowmatrix_extract_rows(NspSpRowMatrix *A, NspMatrix *Rows, int *err)
{
  NspSpRowMatrix *Loc;
  int rmin,rmax,i,j;
  if ( A->mn == 0) return nsp_sprowmatrix_create(NVOID,A->rc_type,0,0);
  Bounds(Rows,&rmin,&rmax);
  *err=0;
  if ( rmin < 1 ||  rmax > A->m  ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLSPROW);
    }
  if ( (Loc =nsp_sprowmatrix_create(NVOID,A->rc_type,Rows->mn,A->n))== NULLSPROW) 
    return(NULLSPROW);
  for ( i = 0 ; i < Loc->m ; i++)
    {
      int row =((int) Rows->R[i])-1; 
      SpRow *Ai= A->D[row];
      SpRow *Li= Loc->D[i];
      if (nsp_sprowmatrix_resize_row(Loc,i,Ai->size)==FAIL) return NULLSPROW;  
      for ( j = 0 ; j < Ai->size ; j++ )
	{	
	  Li->J[j]= Ai->J[j];
	  switch ( A->rc_type ) 
	    {
	    case 'r' : Li->R[j]= Ai->R[j];break;
	    case 'c' : Li->C[j]= Ai->C[j];break;
	    }
	}
    }
  return(Loc);
}

/*
 * Returns the kthe diag of a Sparse NspMatrix as a Sparse 
 * Column 
 * Note : 
 * Note that sparse column vector storage is bigger than 
 * row sparse vector storage and returning a sparse row 
 * could be a better idea 
 */

NspSpRowMatrix *nsp_sprowmatrix_diag_extract(NspSpRowMatrix *A, int k)
{
  NspSpRowMatrix *Loc;
  int j,i;
  int imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc =nsp_sprowmatrix_create(NVOID,A->rc_type,(int) 0 , (int) 0);
      return(Loc);
    }
  if (( Loc =nsp_sprowmatrix_create(NVOID,A->rc_type,imax-imin,(int)1))==NULLSPROW)  return NULLSPROW;
  for ( i = 0 ; i < Loc->m ; i++ ) 
    { 
      int ia= i+imin;
      int ok=-1;
      for ( j = 0 ; j < A->D[ia]->size ; j++) 
	{
	  if ( A->D[ia]->J[j] == ia+k) 
	    {
	      ok=j;break;
	    }
	  if ( A->D[ia]->J[j] > ia+k ) break;
	}
      if ( ok != -1 ) 
	{
	  if (nsp_sprowmatrix_resize_row(Loc,i,1)==FAIL) return NULLSPROW;  
	  Loc->D[i]->J[0] = 0;
	  if ( A->rc_type == 'c' )
	    Loc->D[i]->C[0] = A->D[ia]->C[ok];
	  else 
	    Loc->D[i]->R[0] = A->D[ia]->R[ok];
	}
    }
  return Loc;
}

/*
 * Set the kth Diag of A to Diag 
 *  A is enlarged & comlexified if necessary 
 *  int nsp_sprowmatrix_diag_create(A,Diag,k)
 * Diag is a sparse nxm matrix 
 */

static int  GetDiagVal (NspSpRowMatrix *Diag,int i,double *val,doubleC *cval);

int nsp_sprowmatrix_diag_set(NspSpRowMatrix *A, NspSpRowMatrix *Diag, int k)
{
  int i,l;
  int imin,imax,isize;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  isize = imax-imin ;
  if ( isize > Diag->mn ) 
    {
      Scierror("Error:\tGiven vector is too small\n");
      return(FAIL);
    }
  if ( isize < Diag->mn ) 
    {
      imax = Diag->mn +imin;
      /* enlarge A rows if necessary **/
      if (nsp_sprowmatrix_enlarge_rows(A,imax) == FAIL ) return FAIL;
      A->n = imax+k;
    }
  if ( Diag->rc_type == 'c' && A->rc_type == 'r' ) 
    if (nsp_sprowmatrix_complexify(A) == FAIL ) return(FAIL);
  for ( i = imin ; i < A->m ; i++ ) 
    {
      int rep ;
      double val=0.0;
      doubleC cval={0.0,0.0};
      /* get next element to be inserted */
      rep = GetDiagVal(Diag,i-imin,&val,&cval);
      if ( rep  == OK  ) 
	{
	  int ok = 0,col=-1;
	  /* insert new element in row i **/
	  for ( l = 0 ; l < A->D[i]->size ; l++)
	    {
	      if ( A->D[i]->J[l] == i+k ) 
		{
		  ok = 1 ; col = l  ; break;
		}
	      if ( A->D[i]->J[l] > i+k ) 
		{
		  col = l ; break;
		}
	    }
	  if ( ok != 1) 
	    {
	      /* increment row i by 1 **/
	      if (nsp_sprowmatrix_resize_row(A,i,A->D[i]->size+1)==FAIL) return FAIL;
	      /* Move data right **/
	      if ( col != -1 ) 
		{
		  for ( l = A->D[i]->size -1 ; l >= col ; l--) 
		    {
		      A->D[i]->J[l+1] = A->D[i]->J[l]; 
		      if ( A->rc_type == 'r' ) 
			A->D[i]->R[l+1] = A->D[i]->R[l]; 
		      else 
			A->D[i]->C[l+1] = A->D[i]->C[l]; 
		    }
		}
	      else 
		{
		  /* insertion at the end **/
		  col = A->D[i]->size -1;
		}
	    }
	  /* Now the new row is to be inserted at position col **/
	  A->D[i]->J[col] = i+k;
	  if ( A->rc_type == 'r' ) 
	    {
	      A->D[i]->R[col] = val ;
	    }
	  else
	    {
	      if ( Diag->rc_type == 'r') 
		{
		  A->D[i]->C[col].r  = val ;
		  A->D[i]->C[col].i  = 0.0 ;
		}
	      else 
		A->D[i]->C[col]  = cval;
	    }
	}
    }
  return(OK);
}

/*
 * return the ith element of Diag in ival or Cval + an OK value 
 * if this element is non nul. 
 */

static int  GetDiagVal(NspSpRowMatrix *Diag, int i, double *val, doubleC *cval)
{
  int row, col,k;
  if ( Diag->m == 0) return FAIL;
  row = i % Diag->m ;
  col = (i-row ) / Diag->m ;
  for ( k = 0 ; k < Diag->D[row]->size ; k++) 
    {
      if ( Diag->D[row]->J[k] == col ) 
	{
	  if ( Diag->rc_type == 'r' )
	    *val = Diag->D[row]->R[k] ;
	  else 
	    *cval = Diag->D[row]->C[k] ;
	  return OK ;
	}
      if ( Diag->D[row]->J[k] > col ) return FAIL;
    }
  return FAIL;
}

/*
 *  Creates a Matrix with kth diag set to Diag 
 */

NspSpRowMatrix *nsp_sprowmatrix_diag_create(NspSpRowMatrix *Diag, int k)
{
  int i,k1;
  int imin,imax;
  NspSpRowMatrix *Loc;
  imin = Max(0,-k);
  imax = Diag->mn +imin;
  if ((Loc =nsp_sprowmatrix_create(NVOID,Diag->rc_type,imax,imax+k))  == NULLSPROW) 
    return(NULLSPROW);
  if ( Diag->m == 1 )
    {
      for ( k1=0 ; k1 < Diag->D[0]->size ; k1++) 
	{
	  int j= Diag->D[0]->J[k1];
	  int iloc = (k>0) ? j : j-k;
	  int jloc = (k>0) ? j+k : j;
	  if (nsp_sprowmatrix_resize_row(Loc,iloc,1)==FAIL) return NULLSPROW;
	  Loc->D[iloc]->J[0] = jloc;
	  switch ( Loc->rc_type ) 
	    {
	    case 'r' : Loc->D[iloc]->R[0] = Diag->D[0]->R[k1];break;
	    case 'c' : Loc->D[iloc]->C[0] = Diag->D[0]->C[k1];break;
	    }
	}
    }
  else if ( Diag->n == 1) 
    {
      for ( i=0 ; i < Diag->m ; i++) 
	{
	  if ( Diag->D[i]->size != 0) 
	    {
	      int iloc = (k>0) ? i : i-k;
	      int jloc = (k>0) ? i+k : i;
	      if (nsp_sprowmatrix_resize_row(Loc,iloc,1)==FAIL) return NULLSPROW;
	      Loc->D[iloc]->J[0] = jloc;
	      switch ( Loc->rc_type ) 
		{
		case 'r' : Loc->D[iloc]->R[0] = Diag->D[i]->R[0];break;
		case 'c' : Loc->D[iloc]->C[0] = Diag->D[i]->C[0];break;
		}
	    }
	}
    }
  else
    {
      Scierror("Error: second element should be a sparse vector\n");
      return NULLSPROW;
    }
  return Loc;
}

/*
 * Multiplication of Two Sparse Matrices 
 * ------------------------
 *  multiply sparse matrices by the method of gustafson,acm t.o.m.s. 
 *  vol 4 (1978) p250. 
 *  C coded version : Chancelier 1996
 *  some modifs by Bruno Pincon 2005 to improve efficiency
 */

NspSpRowMatrix *nsp_sprowmatrix_mult(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  NspSpRowMatrix *C = NULLSPROW;
  NspMatrix *x = NULLMAT;
  int *xb = NULL, *pxb = NULL;
  int i, j, k, v, ip, jp, kp, neli, final_neli;
  char type = 'r';
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
  if ( A->n != B->m ) 
    {
      Scierror("SpMult : incompatible arguments\n");
      return(NULLSPROW);
    }

  if ( (C =nsp_sprowmatrix_create(NVOID,type, A->m,B->n)) == NULLSPROW ) return NULLSPROW; 

  /*  x          a one-dimensional array of size ge number of cols of c, 
   *                to contain elements of current row of c, 
   *                in full,i.e. non-sparse form.  */
  if ( (x = nsp_matrix_create(NVOID,type,(int) 1, C->n)) == NULLMAT ) goto err;

  /*  xb         an array of same size as x. xb(j)=i if element in row i, 
   *                column j of c is non-zero. */
  if ( (xb =nsp_alloc_int(C->n)) == (int*) 0) goto err;
  /*  pxb        to store the effective non nul indices of row i...
   *              (act as a pointer to x at the end) */
  if ( (pxb =nsp_alloc_int(C->n)) == (int*) 0) goto err;

  /* initialize the non-zero -element indicator for row i of c. */
  for (v = 0 ; v < B->n ; v++ )  xb[v] = -1;
  /* process the rows of a. */
  for (i = 0 ; i < A->m ; i++) 
    {
      neli = 0;  /* to count the number of a priori non nul elements of row i */
      SpRow *Ai = A->D[i];
      SpRow *Ci = C->D[i];
      /*  process row i of a.  */
      for (jp = 0  ; jp <  Ai->size  ; jp++) 
	{
	  SpRow *Bj;
	  /* j is the current col-index for a */
	  j = Ai->J[jp] ;
	  Bj = B->D[j];
	  /*  We process the row j of  b. For efficiency we separate the real case 
           *  from the 3 complex cases (which are nevertheless treated together)
	   */
	  if ( C->rc_type == 'r' )
	    for (kp = 0 ; kp < Bj->size ; kp++) 
	      {
		/* getting non nul b(j,k) */
		k = Bj->J[kp]; 
		/* check if contribution already exixts to c(i,k) */
		if ( xb[k] != i )
		  { 
		    xb[k] = i; pxb[neli] = k; neli++; x->R[k]=0.00;
		  }
		x->R[k] += Ai->R[jp] * Bj->R[kp];
	      }
	  else 
	    for (kp = 0 ; kp < Bj->size ; kp++) 
	      {
		/* getting non nul b(j,k) */
		k = Bj->J[kp];
		/* check if contribution already exixts to c(i,k) */
		if ( xb[k] != i )
		  { 
		    xb[k] = i; pxb[neli] = k; neli++; x->C[k].r = 0.00; x->C[k].i = 0.00;
		  }
		if ( A->rc_type == 'r') 
		  {
		    x->C[k].i += Ai->R[jp] * Bj->C[kp].i;
		    x->C[k].r += Ai->R[jp] * Bj->C[kp].r;
		  }
		else if ( B->rc_type == 'r' ) 
		  {
		    x->C[k].i += Ai->C[jp].i * Bj->R[kp];
		    x->C[k].r += Ai->C[jp].r * Bj->R[kp];
		  }
		else 
		  {
		    x->C[k].r += Ai->C[jp].r * Bj->C[kp].r -  Ai->C[jp].i* Bj->C[kp].i;
		    x->C[k].i += Ai->C[jp].i * Bj->C[kp].r +  Ai->C[jp].r* Bj->C[kp].i;
		  }
	      }
	}
      /* we have now computed c(i,.) in expanded form and stored it in x,xb */
      /* pxb[0..neli-1] contains all the non nul column indices (so point to */
      /* values to retrieve from x) but their are not in order (and so the sort) */
      /* we store them in C and exclude number which have cancelled during computation */
      
      nsp_qsort_int(pxb,NULL,FALSE,neli,'i');      
      /*  zero=0,c__1=1;
	  C2F(gsort)(pxb,NULL,NULL,&zero,&c__1,&neli,"i","i"); */

      /* but as cancellation is unlikely to occur we allocate first the row */
      /* with the a priori non null element number neli */
      if ( nsp_sprowmatrix_resize_row(C,i,neli) == FAIL ) goto err;
      final_neli = neli;
      ip = 0;
      if ( C->rc_type == 'r' )  /* real case */
	{
	  for (k = 0 ; k < neli ; k++)  
	    { 
	      v = pxb[k];
	      if ( x->R[v] != 0.00 )  /* take care of cancellation */
		{
		  Ci->J[ip] = v; 
		  Ci->R[ip] = x->R[v];
		  ip++;
		}
	      else
		final_neli--;  /* cancellation occurs => final resize */
	    }
	}
      else   /* complex case */
	{
          for (k = 0 ; k < neli ; k++)
            {
	      v = pxb[k];
	      if ( x->C[v].r != 0.00  || x->C[v].i != 0.00 ) /* take care of cancellation */
		{
		  Ci->J[ip] = v;
		  Ci->C[ip].r = x->C[v].r; Ci->C[ip].i = x->C[v].i;
		  ip++;
		}
	      else
		final_neli--; /* cancellation occurs => final resize */
            }
        }
      if ( final_neli < neli )
	if ( nsp_sprowmatrix_resize_row(C,i,final_neli) == FAIL ) goto err;
    }
  FREE(pxb);
  FREE(xb);
  nsp_matrix_destroy(x);
  return C;

 err:
  FREE(pxb);
  FREE(xb);
  nsp_matrix_destroy(x);
  nsp_sprowmatrix_destroy(C);
  return NULLSPROW;
}

/*
 * nsp_sprowmatrix_mult_matrix(A,X) when A is sparse and X full
 * result B is a full matrix. Special cases have been checked
 * in the interface. (added by Bruno)
 */
NspMatrix *nsp_sprowmatrix_mult_matrix(NspSpRowMatrix *A, NspMatrix *X)
{
  int i, j, k, ij, m = A->m, n = X->n, p = X->m;
  char rtype;
  NspMatrix *B;

  rtype = 'c';
  if ( A->rc_type == 'r' && X->rc_type == 'r' )
    rtype = 'r';

  if ( (B = nsp_matrix_create(NVOID,rtype,m,n)) == NULLMAT ) 
    return NULLMAT;

  /* la suite est pas belle ... */
  ij = 0;
  if ( rtype == 'r' )
    {
      for ( j = 0 ; j < n ; j++ )
	{
	  double *Xj = (&X->R[p*j]);
	  for ( i = 0 ; i < m ; i++ )
	    {
	      double temp = 0.0, *Ai = A->D[i]->R; int *Ji = A->D[i]->J;
	      for ( k = 0 ; k < A->D[i]->size ; k++ )
		temp += Ai[k] * Xj[Ji[k]];
	      B->R[ij] = temp;
	      ij++;
	    }
	}
    }
  else if ( A->rc_type == 'r' && X->rc_type == 'c' ) 
    {
      for ( j = 0 ; j < n ; j++ )
	{
	  doubleC *Xj = (&X->C[p*j]);
	  for ( i = 0 ; i < m ; i++ )
	    {
	      doubleC temp; double *Ai = A->D[i]->R; int *Ji = A->D[i]->J;
	      temp.r = 0; temp.i = 0;
	      for ( k = 0 ; k < A->D[i]->size ; k++ )
		{
		  temp.r += Ai[k] * Xj[Ji[k]].r;
		  temp.i += Ai[k] * Xj[Ji[k]].i;
		}
	      B->C[ij].r = temp.r; 
	      B->C[ij].i = temp.i; 
	      ij++;
	    }
	}
    }
  else if ( A->rc_type == 'c' && X->rc_type == 'r' ) 
    {
      for ( j = 0 ; j < n ; j++ )
	{
	  double *Xj = (&X->R[p*j]);
	  for ( i = 0 ; i < m ; i++ )
	    {
	      doubleC temp, *Ai = A->D[i]->C; int *Ji = A->D[i]->J;
	      temp.r = 0; temp.i = 0;
	      for ( k = 0 ; k < A->D[i]->size ; k++ )
		{
		  temp.r += Ai[k].r * Xj[Ji[k]];
		  temp.i += Ai[k].i * Xj[Ji[k]];
		}
	      B->C[ij].r = temp.r; 
	      B->C[ij].i = temp.i; 
	      ij++;
	    }
	}
    }
  else
    {
      for ( j = 0 ; j < n ; j++ )
	{
	  doubleC *Xj = (&X->C[p*j]);
	  for ( i = 0 ; i < m ; i++ )
	    {
	      doubleC temp, *Ai = A->D[i]->C; int *Ji = A->D[i]->J;
	      temp.r = 0; temp.i = 0;
	      for ( k = 0 ; k < A->D[i]->size ; k++ )
		{
		  temp.r += Ai[k].r * Xj[Ji[k]].r  -  Ai[k].i * Xj[Ji[k]].i;
		  temp.i += Ai[k].i * Xj[Ji[k]].r  +  Ai[k].r * Xj[Ji[k]].i;
		}
	      B->C[ij].r = temp.r; 
	      B->C[ij].i = temp.i; 
	      ij++;
	    }
	}
    }

  return B;
}

/*
 * nsp_sprowmatrix_mult_scal(A,B) when B is a scalar sparse 
 * A is changed 
 * the fact that B is scalar is not checked 
 */

int nsp_sprowmatrix_mult_scal_old(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  int i,k ;
  if ( A->rc_type == 'r' &&  B->rc_type == 'c' )  
    {
      if (nsp_sprowmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->R[k] *= B->D[0]->R[0];
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  nsp_prod_c(&A->D[i]->C[k],&B->D[0]->C[0]);
    }
  return(OK);
}




/*
 * Changes A to complex type and 
 * provide storage allocation 
 */

int nsp_sprowmatrix_complexify(NspSpRowMatrix *A)
{
  int inc = 1;
  int i;
  if ( A->rc_type == 'c' ) return(OK);
  for ( i = 0 ; i < A->m ; i++) 
    {
      SpRow *Ai = A->D[i];
      if ( Ai->size != 0) 
	{ 
	  doubleC *cp =nsp_alloc_doubleC((int) Ai->size);
	  if ( cp == (doubleC *) 0) return(FAIL);
	  nsp_dzcopy(&Ai->size,Ai->R,&inc,cp,&inc);
	  FREE(Ai->R);
	  Ai->C = cp;
	}
    }
  A->rc_type = 'c';
  return(OK);
}


/*
 * Set real part of all non nul elements of A to d 
 */

int nsp_sprowmatrix_setr(NspSpRowMatrix *A, double d)
{
  int i,c1=1;
  switch ( A->rc_type ) 
    {
    case 'r' :
      for ( i = 0 ; i < A->m ; i++) 
	{
	  SpRow *Ai = A->D[i];
	  if ( Ai->size != 0) 
	    { 
	      nsp_dset(&Ai->size,&d,Ai->R, &c1);
	    }
	}
      break;
    case 'c' :
      for ( i = 0 ; i < A->m ; i++) 
	{
	  SpRow *Ai = A->D[i];
	  if ( Ai->size != 0) 
	    { 
	      c1=2;
	      nsp_dset(&Ai->size,&d,(double *) Ai->C, &c1);
	    }
	}
    }
  return(OK);
}


/*
 * Set imag part of all non nul elements of A to d 
 * if A is real A is changed to complex 
 */

int nsp_sprowmatrix_seti(NspSpRowMatrix *A, double d)
{
  int i,c1=1;
  switch ( A->rc_type ) 
    {
    case 'r' : if (nsp_sprowmatrix_complexify(A) == FAIL ) return(FAIL); break;
    }
  for ( i = 0 ; i < A->m ; i++) 
    {
      SpRow *Ai = A->D[i];
      if ( Ai->size != 0) 
	{ 
	  c1=2;
	  nsp_dset(&Ai->size,&d,((double *) Ai->C) +1, &c1);
	}
    }
  return(OK);
}



/*
 * Count the number of elements of A which are 
 * non zero 
 * A and val are unchanged 
 */

static int  RowCountNonNull(NspMatrix *A, int i)
{ 
  int count=0,j;
  switch ( A->rc_type ) 
    {
    case 'r' :
      for ( j = 0 ; j < A->n ; j++ )  if (A->R[i+j*(A->m)] != 0.00 ) count++; break;
    case 'c' :
      for ( j = 0 ; j < A->n ; j++ )  
	if (A->C[i+j*A->m].r  != 0.00 ||  A->C[i+j*A->m].i != 0.00)  count++;
      break;
    }
  return(count);
}

#if 0 
static int countnonnull(NspMatrix *A)
{ 
  int count=0,i;
  for ( i = 0 ; i < A->m ; i++ ) count +=  RowCountNonNull(A,(int)i);
  return(count);
}
#endif 

/*
 * Converts a full Matrix to a Sparse one 
 * used for test on sparse matrix 
 * we have two solutions 
 *  [1] use CountNonNull 
 *  [2] SpCreates with 256 elements 
 *        and SpIncrease to increase Storage 
 *        Fix storage at end 
 */

NspSpRowMatrix *nsp_sprowmatrix_from_mat(NspMatrix *A)
{ 
  /* nnul : counts non nul elements in the sparse matrix 
   * count : count non nul elements in row i */
  int i,j;
  NspSpRowMatrix *Sp;
  if (( Sp =nsp_sprowmatrix_create(NVOID,A->rc_type,A->m,A->n))== NULLSPROW) return(NULLSPROW);
  /* first pass to count non null elements on rows */
  for ( i = 0 ; i < A->m ; i++ ) 
    { 
      int count;
      count =  RowCountNonNull(A,(int)i) ;
      if (nsp_sprowmatrix_resize_row(Sp,i,count) == FAIL) return(NULLSPROW) ;
    }
  /* Storing Datas */
  for ( i = 0 ; i < A->m ; i++ ) 
    { 
      SpRow *Ri = Sp->D[i];
      int count = 0;
      switch ( A->rc_type ) 
	{
	case 'r' :
	  for ( j = 0 ; j < A->n ; j++ ) 
	    { 
	      if ( A->R[i+j*A->m] != 0.00  ) 
		{
		  Ri->J[count] = j;
		  Ri->R[count] = A->R[i+j*A->m];
		  count++;
		}
	    }
	  break ;
	case 'c' : 
	  for ( j = 0 ; j < A->n ; j++ ) 
	    { 
	      if ( A->C[i+j*A->m].r != 0.00 || A->C[i+j*A->m].i != 0.00 )
		{
		  Ri->J[count] = j;
		  Ri->C[count].r  = A->C[i+j*A->m].r;
		  Ri->C[count].i  = A->C[i+j*A->m].i;
		  count++;
		}
	    }
	}
    }
  return(Sp) ;
}


/*
 * Sparse to Full conversion 
 * mainly used for testing sparse 
 */

NspMatrix *nsp_sprowmatrix_to_mat(NspSpRowMatrix *Sp)
{ 	
  NspMatrix *A;
  int i,k; 
  A = nsp_matrix_create(NVOID,Sp->rc_type,Sp->m,Sp->n) ;
  if ( A == NULLMAT ) { return(NULLMAT);}
  nsp_mat_set_rval(A,0.0);
  if ( A->rc_type == 'c') if (nsp_mat_set_ival(A,0.00) == FAIL ) return(NULLMAT);
  for ( i = 0 ; i < Sp->m ; i++ ) 
    {
      SpRow *Ri = Sp->D[i] ;
      switch ( A->rc_type ) 
	{
	case 'r' : 
	  for  ( k = 0  ;  k < Ri->size ; k++)
	    { 
	      A->R[i+ Ri->J[k]*A->m] = Ri->R[k]  ;
	    }
	  break ; 
	case 'c' :
	  for  ( k = 0  ;  k < Ri->size ; k++)
	    { 
	      A->C[i+ Ri->J[k]*A->m].r = Ri->C[k].r ;
	      A->C[i+ Ri->J[k]*A->m].i = Ri->C[k].i ;
	    }
	}
    }
  return(A);
}

/*
 * Transpose a sparse Matrix, A is unchanged 
 *  A new matrix is created 
 *  We could add a routine for vectors with a transposition without copy 
 * Warning : For Complex matrices transpose is a' = transp+ conj
 */


NspSpRowMatrix *nsp_sprowmatrix_transpose(const NspSpRowMatrix *A)
{
  int k,i,j;
  NspSpRowMatrix *Loc;
  Loc =nsp_sprowmatrix_create( NVOID,A->rc_type,A->n,A->m);
  if ( Loc == NULLSPROW ) return(NULLSPROW) ; 
  /* Initialisation : a faire a la creation ?? xxx **/
  for (  i  = 0 ;  i  < Loc->m  ;  i++ ) 
    {
      Loc->D[i]->iw = 0;
    } 
  /* Counting elements of transposed matrix  */
  for (  i  = 0 ;  i  < A->m  ;  i++ ) 
    {
      SpRow *Ai = A->D[i];
      for  ( k = 0  ;  k < Ai->size ; k++)
	{ 
	  j = Ai->J[k];
	  (Loc->D[j]->iw)++;
	}
    } 
  /* Space allocation for Rows */
  for (  i  = 0 ;  i  < Loc->m  ;  i++ ) 
    {
      if (nsp_sprowmatrix_resize_row(Loc,i,(int) Loc->D[i]->iw) == FAIL) return(NULLSPROW) ;
      Loc->D[i]->iw = 0 ; /* working storage reinit */
    } 
  /* filling the Tranposed matrix rows by rows  **/
  for (  i = 0 ;  i < A->m  ;  i++) 
    {
      SpRow *Ai = A->D[i];
      for  ( k = 0  ;  k < Ai->size ; k++)
	{ 
	  int jt;
	  j = Ai->J[k];
	  jt = Loc->D[j]->iw ;
	  Loc->D[j]->J[jt]= i;
	  switch ( Loc->rc_type ) 
	    {
	    case 'r' : 
	      Loc->D[j]->R[jt]= A->D[i]->R[k] ;
	      break ;
	    case 'c' :  
	      Loc->D[j]->C[jt].r = A->D[i]->C[k].r  ;
	      Loc->D[j]->C[jt].i = - A->D[i]->C[k].i  ; 
	      break;
	    }

	  (Loc->D[j]->iw)++;
	}
    }
  return(Loc) ;
} 




/*
 * Utilities for term to term operations 
 */
#if 0
double  plus (double x, double y, double xi, double yi, double *ival, char type)
{ 
  if ( type == 'c' ) *ival = xi + yi ;
  return( x+y ) ;
}
#endif 

/*
 * PLus operator A Plus B 
 *   XXXLeft   is used when A(i,j) != 0  and B(i,j) == 0 
 *   XXXRight  is used when A(i,j) == 0  and B(i,j) != 0
 *   XXXBoth   is used when A(i,j) !=    and B(i,j) != 0
 */

void PlusLeft(SpRow *Li, char Ltype, int *count, SpRow *Ai, char Atype, int k1)
{
  if ( Ltype == 'r') 
    Li->R[*count] = Ai->R[k1];
  else 
    { 
      if ( Atype == 'r' )
	{
	  Li->C[*count].r = Ai->R[k1];
	  Li->C[*count].i = 0.00;
	}
      else 
	{
	  Li->C[*count].r = Ai->C[k1].r;
	  Li->C[*count].i= Ai->C[k1].i;
	}
    }
  (*count)++;
}

void PlusBoth(SpRow *Li, char Ltype, int *count, SpRow *Ai, char Atype, int k1, SpRow *Bi, char Btype, int k2)
{
  if ( Ltype == 'r') 
    {
      Li->R[*count] =  Ai->R[k1]+ Bi->R[k2];
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
  else 
    {
      if ( Btype == 'r' )
	{
	  Li->C[*count].r =  Ai->C[k1].r+Bi->R[k2];
	  Li->C[*count].i =  Ai->C[k1].i;
	}
      else 
	{
	  if ( Atype == 'r' ) 
	    {
	      Li->C[*count].r =  Ai->R[k1]+Bi->C[k2].r;
	      Li->C[*count].i =  Bi->C[k2].i;
	    }
	  else 
	    {
	      Li->C[*count].r =   Ai->C[k1].r + Bi->C[k2].r;
	      Li->C[*count].i =   Ai->C[k1].i + Bi->C[k2].i;
	    }
	}
      if ( Li->C[*count].r  != 0.00||  Li->C[*count].i  != 0.00 ) (*count)++; 
    }
}


void PlusRight(SpRow *Li, char Ltype, int *count, SpRow *Bi, char Btype, int k1)
{
  if ( Ltype == 'r') 
    Li->R[*count] = Bi->R[k1];
  else 
    {
      if ( Btype == 'r' )
	{
	  Li->C[*count].r = Bi->R[k1];
	  Li->C[*count].i = 0.00;
	}
      else 
	{
	  Li->C[*count].r = Bi->C[k1].r;
	  Li->C[*count].i= Bi->C[k1].i;
	}
    }
  (*count)++;
}

/*
 * Minus operator 
 */

void MinusLeft(SpRow *Li, char Ltype, int *count, SpRow *Ai, char Atype, int k1)
{
  if ( Ltype == 'r') 
    Li->R[*count] = Ai->R[k1];
  else 
    { 
      if ( Atype == 'r' )
	{
	  Li->C[*count].r =  Ai->R[k1];
	  Li->C[*count].i = 0.00;
	}
      else 
	{
	  Li->C[*count].r =  Ai->C[k1].r;
	  Li->C[*count].i=  Ai->C[k1].i;
	}
    }
  (*count)++;
}

void MinusBoth(SpRow *Li, char Ltype, int *count, SpRow *Ai, char Atype, int k1, SpRow *Bi, char Btype, int k2)
{
  if ( Ltype == 'r') 
    {
      Li->R[*count] =  Ai->R[k1] -  Bi->R[k2];
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
  else 
    {
      if ( Btype == 'r' )
	{
	  Li->C[*count].r =  Ai->C[k1].r - Bi->R[k2];
	  Li->C[*count].i =  Ai->C[k1].i;
	}
      else 
	{
	  if ( Atype == 'r' ) 
	    {
	      Li->C[*count].r =  Ai->R[k1] - Bi->C[k2].r;
	      Li->C[*count].i =  - Bi->C[k2].i;
	    }
	  else 
	    {
	      Li->C[*count].r =   Ai->C[k1].r - Bi->C[k2].r;
	      Li->C[*count].i =   Ai->C[k1].i - Bi->C[k2].i;
	    }
	}
      if ( Li->C[*count].r  != 0.00||  Li->C[*count].i  != 0.00 ) (*count)++; 
    }
}


void MinusRight(SpRow *Li, char Ltype, int *count, SpRow *Bi, char Btype, int k1)
{
  if ( Ltype == 'r') 
    Li->R[*count] = - Bi->R[k1];
  else 
    {
      if ( Btype == 'r' )
	{
	  Li->C[*count].r = - Bi->R[k1];
	  Li->C[*count].i = 0.00;
	}
      else 
	{
	  Li->C[*count].r = - Bi->C[k1].r;
	  Li->C[*count].i= - Bi->C[k1].i;
	}
    }
  (*count)++;
}


/*
 * Multtt ( term to term multiplication ) 
 */

void MultttLeft(SpRow *Li, char Ltype, int *count, SpRow *Ai, char Atype, int k1)
{
}

void MultttBoth(SpRow *Li, char Ltype, int *count, SpRow *Ai, char Atype, int k1, SpRow *Bi, char Btype, int k2)
{
  if ( Ltype == 'r') 
    {
      Li->R[*count] =  Ai->R[k1]*Bi->R[k2];
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
  else 
    {
      if ( Btype == 'r' )
	{
	  Li->C[*count].r =  Ai->C[k1].r*Bi->R[k2];
	  Li->C[*count].i =  Ai->C[k1].i*Bi->R[k2];
	}
      else 
	{
	  if ( Atype == 'r' ) 
	    {
	      Li->C[*count].r =  Ai->R[k1]*Bi->C[k2].r;
	      Li->C[*count].i =  Ai->R[k1]*Bi->C[k2].i;
	    }
	  else 
	    {
	      Li->C[*count].r =   Ai->C[k1].r * Bi->C[k2].r -   Ai->C[k1].i* Bi->C[k2].i;
	      Li->C[*count].i =   Ai->C[k1].i * Bi->C[k2].r +   Ai->C[k1].r* Bi->C[k2].i;;
	    }
	}
      if ( Li->C[*count].r  != 0.00||  Li->C[*count].i  != 0.00 ) (*count)++; 
    }
}

void MultttRight(SpRow *Li, char Ltype, int *count, SpRow *Bi, char Btype, int k1)
{
}

/*
 *nsp_sprowmatrix_add,nsp_sprowmatrix_sub, SpMulttt
 * A+B A-B A.*B
 */

NspSpRowMatrix *nsp_sprowmatrix_add(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return(BinaryOp(A,B,PlusLeft,PlusBoth,PlusRight));
}

NspSpRowMatrix *nsp_sprowmatrix_sub(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return(BinaryOp(A,B,MinusLeft,MinusBoth,MinusRight));
}

NspSpRowMatrix *nsp_sprowmatrix_multtt(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return(BinaryOp(A,B,MultttLeft,MultttBoth,MultttRight));
}

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/*
 * A generic function for performing Res(i,j) = A(i,j) op B(i,j)
 * assuming that 0 op 0 --> 0  (WARNING)
 * For performing a specific operation the function is called 
 * with three operators 
 *    BopLeft for performing  A(i,j) op B(i,j) when 
 *            A(i,j) != 0  and B(i,j) == 0 
 *    BopRight for performing A(i,j) op B(i,j) when 
 *            A(i,j) == 0  and B(i,j) != 0
 *    BopBoth  for performing A(i,j) op B(i,j) when 
 *            A(i,j) !=    and B(i,j) != 0 
 * each Bopxxx functions performs the operation, stores 
 *    the result (if #0) in the sparse matrix Loc 
 *    and increment a counter for counting #0 elements 
 */

static NspSpRowMatrix *BinaryOp(NspSpRowMatrix *A, NspSpRowMatrix *B, BopLeft BinLeft, BopBoth BinBoth, BopRight BinRight)
{ 
  int i,count,k1,k2,k;
  NspSpRowMatrix *Loc;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
      Loc =nsp_sprowmatrix_create(NVOID,type, A->m,A->n);
      if ( Loc == NULLSPROW ) return(NULLSPROW) ; 
      for ( i = 0 ; i < Loc->m ; i++ ) 
	{
	  int iest;
	  SpRow *Ai = A->D[i];
	  SpRow *Bi = B->D[i];
	  SpRow *Li = Loc->D[i];
	  iest= Min( A->n, A->D[i]->size+B->D[i]->size);
	  if (nsp_sprowmatrix_resize_row(Loc,i,(int)iest ) == FAIL) return(NULLSPROW) ;
	  /* We explore the ith line of A and B in increasing order of column 
	     and want to merge the columns found ( in increasing order ) 
	     when a same column number appear in both A and B we call the 
	     2-ary operator op 
	     This is very near to a merge sort of two sorted arrays 
	  **/ 
	  k1 = 0 ; k2 = 0 ; 
	  count = 0;
	  /* merge part **/
	  while ( k1 < Ai->size && k2 <  Bi->size) 
	    { 
	      int j1,j2;
	      j1 = Ai->J[k1] ;
	      j2 = Bi->J[k2] ;
	      if ( j1 < j2 ) 
		{ 
		  Li->J[count] = j1;
		  (*BinLeft)(Li,Loc->rc_type,&count,Ai,A->rc_type,k1);
		  k1++; 
		}
	      else if ( j1 == j2 ) 
		{ 
		  Li->J[count] = j1;
		  (*BinBoth)(Li,Loc->rc_type,&count,Ai,A->rc_type,k1,Bi,B->rc_type,k2);
		  k1++; k2 ++; 
		}
	      else 
		{ 
		  Li->J[count] = j2;
		  (*BinRight)(Li,Loc->rc_type,&count,Bi,B->rc_type,k2);
		  k2++;
		}
	    }
	  /* Keep inserting remaining arguments for A **/
	  for ( k = k1 ; k < Ai->size ; k++) 
	    { 
	      Li->J[count] = Ai->J[k];
	      (*BinLeft)(Li,Loc->rc_type,&count,Ai,A->rc_type,k);
	    }
	  /* Keep inserting remaining arguments for B **/
	  for ( k = k2 ; k < Bi->size ; k++) 
	    { 
	      Li->J[count] = Bi->J[k] ;
	      (*BinRight)(Li,Loc->rc_type,&count,Bi,B->rc_type,k);
	    }
	  /* Count doit maintenant nous donner la taille de la ligne i **/
	  /* Resizer Loc->D[i] : car on peut avoir moins d'el que pr'evus **/
	  if (nsp_sprowmatrix_resize_row(Loc,i,count) == FAIL) return(NULLSPROW) ;
	}
      return(Loc);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      return(NULLSPROW);
    }
}

/*
 * A = A.*B where B is a scalar sparse ( [] or scalar )
 */

int nsp_sprowmatrix_mult_scal(NspSpRowMatrix *A, NspSpRowMatrix *B)
{ 
  int i,k;
  if ( B->mn == 0 || B->D[0]->size == 0)
    {
      /* B is [] or [0] **/
      /* Change A to [] sparse **/
      for ( i=0 ; i < A->m ; i++ ) 
	{
	  if ( A->D[i]->size != 0 ) 
	    {
	      FREE( A->D[i]->J);
	      FREE( A->D[i]->R);
	    }
	  A->D[i]->size =0;
	}
      FREE(A->D);
      A->m = A->n = A->mn = 0;
      return OK;
    }
  if ( B->rc_type == 'c' )
    {
      if (nsp_sprowmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  /* Now B is a non null scalar **/
  if ( A->rc_type == 'r' )
    {
      /* here B is alway real **/
      for ( i = 0 ; i < A->m ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  A->D[i]->R[k] *= B->D[0]->R[0];
    }
  else
    {
      if ( B->rc_type == 'r' ) 
	for ( i = 0 ; i < A->m ; i++ ) 
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      A->D[i]->C[k].r *= B->D[0]->R[0];
	      A->D[i]->C[k].i *= B->D[0]->R[0];
	    }
      else
	for ( i = 0 ; i < A->m ; i++ ) 
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      double loc;
	      loc =   A->D[i]->C[k].r * B->D[0]->C[0].r 
		- A->D[i]->C[k].i* B->D[0]->C[0].i;
	      A->D[i]->C[k].i = A->D[i]->C[k].i * B->D[0]->C[0].r 
		+ A->D[i]->C[k].r* B->D[0]->C[0].i;
	      A->D[i]->C[k].r = loc;
	      
	    }
    }
  return OK;
}

/*
 * A = A op B where B is a scalar sparse ( [] or scalar )
 * op can be + or - 
 * if B==[] or B==[0] 
 *     the result is A  and *flag == 1
 * else a full matrix is returned and *flag == 0 
 * 
 * op can be '+'(A+B) ,'-' (A-B), '#' (-A+B)
 */

NspMatrix *nsp_sprowmatrix_op_scal(NspSpRowMatrix *A, NspSpRowMatrix *B, int *flag, char op)
{ 
  char type = 'r';
  int i,j,k;
  NspMatrix *Loc;
  if ( B->mn == 0 || B->D[0]->size == 0)
    {
      /* B is [] or [0] **/
      /* return A unchanged **/
      *flag = 1;
      return NULLMAT ;
    }
  *flag = 0;
  if ( B->rc_type == 'c' || A->rc_type == 'c' ) type = 'c';
  if ((Loc = nsp_matrix_create(NVOID,type,A->m,A->n))== NULLMAT) 
    return(NULLMAT);
  /* Loc == +-B **/
  switch ( B->rc_type )
    {
    case 'r' : 
      nsp_mat_set_rval(Loc,(op == '-') ? -B->D[0]->R[0] :B->D[0]->R[0]);
      if (Loc->rc_type == 'c' ) 
	{
	  nsp_mat_set_rval(Loc,0.0);
	}
      break;
    case 'c' : 
      nsp_mat_set_rval(Loc,(op == '-') ? -B->D[0]->C[0].r : B->D[0]->C[0].r);
      nsp_mat_set_ival(Loc,(op == '-') ? -B->D[0]->C[0].i : B->D[0]->C[0].i);
      break;
    }
  /* Loc = Loc +A or Loc -A **/
  if ( op != '#' )
    {
      if ( A->rc_type == 'r' )
	{
	  if ( Loc->rc_type == 'r' ) 
	    for ( i = 0 ; i < A->m ; i++ ) 
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  j= A->D[i]->J[k];
		  Loc->R[i+Loc->m*j] += A->D[i]->R[k];
		}
	  else 
	    for ( i = 0 ; i < A->m ; i++ ) 
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  j= A->D[i]->J[k];
		  Loc->C[i+Loc->m*j].r += A->D[i]->R[k];
		}
	}
      else
	{
	  for ( i = 0 ; i < A->m ; i++ ) 
	    for ( k = 0 ; k < A->D[i]->size ; k++) 
	      {
		j= A->D[i]->J[k];
		Loc->C[i+Loc->m*j].r +=   A->D[i]->C[k].r;
		Loc->C[i+Loc->m*j].i +=   A->D[i]->C[k].i;
	      }
	}
    }
  else
    {
      if ( A->rc_type == 'r' )
	{
	  if ( Loc->rc_type == 'r' ) 
	    for ( i = 0 ; i < A->m ; i++ ) 
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  j= A->D[i]->J[k];
		  Loc->R[i+Loc->m*j] -= A->D[i]->R[k];
		}
	  else 
	    for ( i = 0 ; i < A->m ; i++ ) 
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  j= A->D[i]->J[k];
		  Loc->C[i+Loc->m*j].r -= A->D[i]->R[k];
		}
	}
      else
	{
	  for ( i = 0 ; i < A->m ; i++ ) 
	    for ( k = 0 ; k < A->D[i]->size ; k++) 
	      {
		j= A->D[i]->J[k];
		Loc->C[i+Loc->m*j].r -=   A->D[i]->C[k].r;
		Loc->C[i+Loc->m*j].i -=   A->D[i]->C[k].i;
	      }
	}
    }
  return Loc;
}


/**
 * nsp_dichotomic_search:
 * @x: 
 * @val: 
 * @imin: 
 * @imax: 
 * 
 * performs a dichotomic search of x in array @val for indices in [@imin,@imax].
 * @val is supposed to be strictly increasing. 
 * 
 * Return value:  -1 if @x < @val[@imin], -2 if @x > @val[@imax] 
 *  k if @val[k] <= x < @val[k+1] and @imax if  @val[@imax] == x
 **/

static int nsp_dichotomic_search(int x,const int val[],int imin,int imax)
{
  int j, j1, j2;
  if ( x < val[imin]) return -1;
  if ( x > val[imax]) return -2; 
  /* dichotomic search */
  j1 = imin;
  j2 = imax;
  while(j2 - j1 > 1) 
    {
      j = (j1 + j2) / 2;
      if (x  < val[j]) 
	{
	  j2 = j;
	} 
      else 
	{
	  j1 = j;
	}
    }
  /*  here we know that val(j1) <= x <= val(j2)  with j2 = j1 + 1
   *  (in fact we have exactly  val(j1) <= x < val(j2) if j2 < n) 
   */
  if ( x == val[j2])  return j2;
  return j1;
}

/*
 * Utility function for SpExtract 
 * 
 * search x[xpmin,xpmax]-1 in val[imin:imax] 
 * and store information in Work as processing 
 * x and val are increasing 
 * 
 */


static int nsp_bi_dichotomic_search_int(const double x[],int xpmin,int xpmax,int xmin,int xmax,
					const int val[],int imin,int imax,
					NspMatrix *Work,NspMatrix *Index,int count);


static int nsp_bi_dichotomic_search(const double x[],int xpmin,int xpmax,const int val[],int imin,int imax,
				    NspMatrix *Work,NspMatrix *Index,int count)
{
  int xmin = nsp_dichotomic_search(x[xpmin]-1,val,imin,imax);
  int xmax = nsp_dichotomic_search(x[xpmax]-1,val,imin,imax);
  count = nsp_bi_dichotomic_search_int(x,xpmin,xpmax,xmin,xmax,val,imin,imax,Work,Index,0);
  /* last point */
  if ( xmax >= 0 && ((int) x[xpmax])-1 == val[xmax] ) 
    {
      /* Sciprintf("OK pour A(i,%d) -> %d \n",xmax+1,((int) Cols->R[Cols->mn-1]));*/
      Work->R[2*count]= Index->R[xpmax]-1; /* column in final row*/
      Work->R[1+2*count]= xmax; /* position in Ai of the value */
      count++;
    }
  return count;
}

/*
 * search x[xpmin,xpmax]-1 in val[imin:imax] 
 * and store information in Work as processing 
 * Th enumber of stored values is returned in count 
 * xmin >= 0 ==> we know that x[xpmin]-1 is >= val[xmin] 
 * xmin == -1 x[xpmin]-1 is < val[xmin] 
 * xmin == -2 x[xpmin]-1 is > val[xmax] 
 * same rules for for xmax and x[xpmax] 
 * x and val are increasing 
 */

static int nsp_bi_dichotomic_search_int(const double x[],int xpmin,int xpmax,int xmin,int xmax,
					const int val[],int imin,int imax,
					NspMatrix *Work,NspMatrix *Index,int count)
{
  int j,jval;
  /* totally out of range */
  if ( xmax == -1 ) return count;
  if ( xmin == -2 ) return count;
  /* check the whole interval up to failure */
  if ( xmin == xmax ) 
    {
      int k;
      if ( xmin >= 0 ) 
	{
	  /* Sciprintf("Je regarde [%d,%d]\n",xpmin,xpmax); */
	  /* check the whole interval [xpmin,xpmax[ */
	  for ( k = xpmin ; k < xpmax ; k++) 
	    {
	      if (((int) x[k])-1 == val[xmin] ) 
		{
		  /* Sciprintf("OK pour A(i,%d) -> %d \n",xmin+1,((int) x[k])); */
		  Work->R[2*count]= Index->R[k]-1; /* column in final row*/
		  Work->R[1+2*count]= xmin; /* position in Ai of the value */
		  count++;
		}
	      else 
		break;
	    }
	}
      return count; 
    }
  if ( xpmin +1 == xpmax )
    {
      if ( xmin >= 0 && ((int) x[xpmin])-1 == val[xmin] ) 
	{
	  /* Sciprintf("OK pour A(i,%d) -> %d \n",xmin+1,((int) x[xpmin])); */
	  Work->R[2*count]= Index->R[xpmin]-1; /* column in final row*/
	  Work->R[1+2*count]= xmin; /* position in Ai of the value */
	  count++;
	}
      /* xpmax will be treated as the first point of next interval 
       * except if xpmax is the last point which is treated elsewhere
       */
    }
  else 
    {
      int imin1,imax1;
      /* the midle point */
      j = (xpmax+xpmin)/2 ;
      jval = nsp_dichotomic_search(((int)x[j])-1,val,imin,imax);
      /* search in [xpmin,j] */
      imax1= (jval >= 0) ? Min(imax,jval) : imax ;
      count = nsp_bi_dichotomic_search_int(x,xpmin,j,xmin,jval,val,imin,imax1,Work,Index,count);
      imin1= (jval >= 0) ? Max(imin,jval) : imin ;
      count =nsp_bi_dichotomic_search_int(x,j,xpmax,jval,xmax,val,imin1,imax,Work,Index,count);
    }
  return count;
}


/* routines for sparse output 
 */

static char SpInit(const void *M,int *work)
{
  work[0]=  -1;
  work[1]= 0;
  return ( (NspSpRowMatrix *) M)->rc_type;
}

static int SpNext(const void *M, double *r, doubleC *c,int *work)
{
  const NspSpRowMatrix *Sp= M;
  if ( work[0] == -1 ) 
    {
      /* Return first a zero value **/
      switch (Sp->rc_type) 
	{
	case 'r' : *r = 0.00;break;
	case 'c' : c->r = c->i = 0.00;break;
	}
      work[0]++;
      return 1;
    }
  /* Now return the non nul elements **/
  if ( work[0] == Sp->m) return 0;
  /* we still have elements on the current line **/
  if ( work[1] < Sp->D[work[0]]->size )
    {
      switch (Sp->rc_type) 
	{
	case 'r' : *r = Sp->D[work[0]]->R[work[1]];break;
	case 'c' : *c = Sp->D[work[0]]->C[work[1]];break;
	}
      work[1]++;
      return 1;
    }
  else 
    {
      /* find next nonempty row **/
      while (1) 
	{
	  work[0]++;
	  if ( work[0] >= Sp->m) return (0);
	  if ( Sp->D[work[0]]->size != 0) break;
	}
      /* return first non nul element on the row **/
      work[1] =0 ;
      switch (Sp->rc_type) 
	{
	case 'r' : *r = Sp->D[work[0]]->R[work[1]];break;
	case 'c' : *c = Sp->D[work[0]]->C[work[1]];break;
	}
      work[1]++;
      return 1;
    }
  return 1;
}

/* Sparse NspMatrix specific code **/

static int Sp_any_element_is_negative (const void *M)
{
  return gen_any_element_is_negative(M,SpInit,SpNext);
}

/* code for sparse **/

static int Sp_any_element_is_inf_or_nan (const void *M)
{
  return gen_any_element_is_inf_or_nan(M,SpInit,SpNext);
}

/* code for sparse **/

static int Sp_all_elements_are_int_or_inf_or_nan (const void *M)
{
  return gen_all_elements_are_int_or_inf_or_nan (M,SpInit,SpNext);
}

/* code for sparse **/

static void Sp_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax)
{
  return gen_pr_min_max_internal (M,flag,dmin,dmax,SpInit,SpNext);
}

/* Sparse Matrix **/

static void Sp_set_format(nsp_num_formats *fmt,NspSpRowMatrix *M)
{
  gen_set_format(fmt,M,Sp_any_element_is_negative,
		 Sp_any_element_is_inf_or_nan,
		 Sp_pr_min_max_internal,
		 Sp_all_elements_are_int_or_inf_or_nan,
		 SpInit);
}

/* Sparse Matrix with + format : both real and complex cases **/

static void SpM_plus_format(NspSpRowMatrix *Sp, int indent)
{
  int i,j;
  for ( i = 0; i < Sp->m; i++)
    {
      int col=0;
      SpRow *Ri = Sp->D[i];
      nsp_pr_white(indent) ; Sciprintf("| ");
      for ( j = 0; j < Ri->size ; j++)
	{
	  if ( col < Ri->size && j == Ri->J[col] ) 
	    {
	      Sciprintf("+");col++;
	    }
	  else 
	    {
	      Sciprintf(" ");
	    }
	}
      Sciprintf(" |\n");
    }
}

static void SpM_general(nsp_num_formats *fmt,NspSpRowMatrix *Sp, int indent)
{
  int i,j;
  switch ( Sp->rc_type ) 
    {
    case 'r' : 
      for ( i = 0; i < Sp->m; i++)
	{
	  SpRow *Ri = Sp->D[i];
	  for ( j = 0; j < Ri->size ; j++)
	    {
	      nsp_pr_white(indent) ;Sciprintf("(%d,%d) ",i+1,Ri->J[j]+1);
	      nsp_pr_float(fmt, Ri->R[j]);Sciprintf("\n");
	    }
	}
      break;
    case 'c' :
      for ( i = 0; i < Sp->m; i++)
	{
	  SpRow *Ri = Sp->D[i];
	  for ( j = 0; j < Ri->size ; j++)
	    {
	      nsp_pr_white(indent) ; Sciprintf("(%d,%d) ",i+1,Ri->J[j]+1);
	      nsp_pr_complex(fmt, Ri->C[j]);
	      Sciprintf("\n");
	    }
	}
      break;
    }
}

static void nsp_sprowmatrix_print_internal(nsp_num_formats *fmt,NspSpRowMatrix *m, int indent)
{
  if ( m->mn == 0) 
    {
      Sciprintf("[]\n");
    }
  else if (fmt->plus_format && ! user_pref.pr_as_read_syntax )
    {
      SpM_plus_format(m,indent);
    }
  else
    {
      Sp_set_format(fmt,m);
      Sciprintf("\n");
      if (fmt->free_format)
	{
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("free format to be done for sparse [\n");
	  /* XXXXXX xxxx Sciprintf(m); **/
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("]");
	  return;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  Sciprintf("No as read for sparse \n");
	}
      else
	{
	  SpM_general(fmt,m,indent);
	}
    }
}


  
