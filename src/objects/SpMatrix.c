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
  
#include "nsp/object.h"
#include "nsp/pr-output.h"
#include "nsp/blas.h"
#include "nsp/matutil.h" 
#include "nsp/gsort-p.h" 
#include "nsp/cnumeric.h" 

static int nsp_sprowmatrix_print_internal(nsp_num_formats *fmt,NspSpRowMatrix *m, int indent);
/* In file Perm.c **/

extern int C2F(dperm) (double A[],int ind[],int *nv);
extern int C2F(zperm) (doubleC A[],int ind[],int *nv);

static int nsp_dichotomic_search(int x,const int val[],int imin,int imax);
static int nsp_bi_dichotomic_search_i(const int *x,int xpmin,int xpmax,const int *val,int imin,int imax,
				      NspMatrix *Work,NspMatrix *Index,int count);


/**
 * nsp_spcolmatrix_cast_to_sprow:
 * @M: a NspSpColMatrix *
 * 
 * Changes the type fields of @M in such a way that 
 * @M becomes a NspSpRowMatrix. Note that 
 * the casted matrix contains the representation of the 
 * transpose of @M.
 * 
 * Returns a #NspSpRowMatrix or %NULLSPROW.
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

NspSpColMatrix * nsp_sprowmatrix_cast_to_spcol(NspSpRowMatrix *M)
{
  if ( M == NULL ) return NULLSPCOL;
  M->type = (NspTypeSpRowMatrix *) new_type_spcolmatrix(T_BASE);
  NSP_OBJECT(M)->type =(NspTypeObject *) M->type->surtype;
  NSP_OBJECT(M)->basetype =(NspTypeBase *) M->type;
  return (NspSpColMatrix *) M;
}

/**
 * nsp_sprowmatrix_create:
 * @name: 
 * @type: 
 * @m: 
 * @n: 
 * 
 * Creates a #NspSColMatrix of size @mx@n with no stored data
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_create(char *name, char type, int m, int n)
{
  NspSpColMatrix *loc; 
  /* create the tranpose in spcol mode */
  if ((loc = nsp_spcolmatrix_create(name,type,n,m))== NULL) return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/**
 * nsp_sprowmatrix_sparse:
 * @name: 
 * @RC: 
 * @Values: 
 * @m: 
 * @n: 
 * 
 * Creates a #NspSColMatrix of size @mx@n filed with values specified 
 * in @RC ((i,j) values stored in a two column matrix) and @Values 
 * ( A(@RC(k,1),@RC(k,2))= Values(k)).
 * XXXX should be changed in order to cumulate values when specific
 * indices are repeated as in Matlab.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_sparse(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n)
{
  NspSpColMatrix *loc=NULL,*loc1=NULL;
  if (( loc = nsp_spcolmatrix_sparse(name,RC,Values,m,n))== NULL) return NULLSPROW;
  loc1= nsp_spcolmatrix_transpose(loc);
  nsp_spcolmatrix_destroy(loc);
  if (loc1 == NULL) return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc1);
}

/**
 * nsp_sprowmatrix_get:
 * @A: 
 * @RC: 
 * @Values: 
 * 
 * get a @RC,@Values description of the sparse matrix @A
 * 
 * Return value: %OK or %FAIL
 **/

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

/**
 * nsp_sprowmatrix_copy:
 * @A: 
 * 
 * returns a copy of sparse matrix @A.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_copy(NspSpRowMatrix *A)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_copy((NspSpColMatrix *) A))==NULL) return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/* XXX
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
  return nsp_spcolmatrix_resize_col((NspSpColMatrix *) Sp,i,n);
}


/**
 * nsp_sprowmatrix_row_destroy:
 * @Row: 
 * 
 * internal: destroys a #SpCol structure.
 * 
 **/

void nsp_sprowmatrix_row_destroy(SpRow *Row)
{
  if ( Row->size != 0 ) 
    {
      FREE( Row->J);
      FREE( Row->R);
    }
  FREE(Row);
}

/**
 * nsp_sprowmatrix_destroy:
 * @Mat: 
 * 
 * internal: destroys a #SpCol structure.
 * 
 **/

void nsp_sprowmatrix_destroy(NspSpRowMatrix *Mat)
{
  nsp_spcolmatrix_destroy((NspSpColMatrix *) Mat);
}

/**
 * nsp_sprowmatrix_nnz:
 * @HMat: 
 * 
 * computes the number of non nul elements stored in 
 * a sparse Matrix. 
 * 
 * Return value: the number of non nul elements.
 **/

int nsp_sprowmatrix_nnz(const NspSpRowMatrix *HMat)
{
  return nsp_spcolmatrix_nnz((const NspSpColMatrix *) HMat);
}


/**
 * nsp_sprowmatrix_info:
 * @Sp: 
 * @indent: 
 * @name: 
 * @rec_level: 
 * 
 * displays info on Sparse Matrix @Sp.
 **/

int nsp_sprowmatrix_info(NspSpRowMatrix *Sp, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Sp)->name;
  if ( Sp == NULLSPROW) 
    Sciprintf("Null SpMatrix pointer\n");
  else 
    Sciprintf1(indent,"%s\t= [...]\t\tsprow %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
  return TRUE;
}

/**
 * nsp_spcolmatrix_print:
 * @Sp: 
 * @indent: 
 * @name: 
 * @rec_level: 
 * 
 * displays a sparse Matrix.
 **/

int nsp_sprowmatrix_print(NspSpRowMatrix *Sp, int indent,char *name, int rec_level)
{ 
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Sp)->name; 
  if (user_pref.pr_as_read_syntax)
    {
      const int name_len=128;
      char epname[name_len];
      NspMatrix *RC,*Values;
      if ( nsp_sprowmatrix_get(Sp,&RC,&Values)== FAIL)
	{
	  Sciprintf("Error: failed to print sparse matrix as_read\n");
	  return rep;
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
      if (Sp->m ==0 || Sp->n == 0) 
	{
	  Sciprintf1(indent,"%s\t= []\t\tsprow %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	}
      else
	{
	  nsp_num_formats fmt;
	  if ( user_pref.pr_depth  <= rec_level -1 ) 
	    {
	      Sciprintf1(indent,"%s\t= [...]\t\tsprow %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	      return rep;
	    }
	  nsp_init_pr_format (&fmt);
	  Sciprintf1(indent,"%s\t=\t\tsprow %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	  rep =nsp_sprowmatrix_print_internal(&fmt,Sp,indent+1);
	}
    }
  return rep;
}

/**
 * nsp_sprowmatrix_redim:
 * @A: 
 * @m: 
 * @n: 
 * 
 * If the product @mx@n is equal to @A->mw@A->n, 
 * returns a new sparse matrix of size @mx@n. The new 
 * matrix is filled with the values of @A assuming 
 * columnwize order. This operation can be done 
 * without copy on full matrices by here we have to create 
 * a new sparse.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

/* XXXX: Attention c'est faux a cause du codage on obtient un redim de la 
 *       transposée
 */

NspSpRowMatrix *nsp_sprowmatrix_redim(NspSpRowMatrix *A, int m, int n)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_redim((NspSpColMatrix *) A,n,m))==NULL) return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/**
 * nsp_sprowmatrix_enlarge_cols:
 * @Sp: 
 * @n: 
 * 
 * changes the number of columns of Sp to Min(Sp->n,n);
 *
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_enlarge_rows(NspSpRowMatrix *Sp, int m)
{
  return nsp_spcolmatrix_enlarge_cols((NspSpColMatrix *) Sp, m);
}

/**
 * nsp_sprowmatrix_enlarge:
 * @A: 
 * @m: 
 * @n: 
 * 
 * changes @A to [@A,0;0,0]  
 * in such a way that the new size of @A is (max(A->m,m) x max(A->n,n));
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_enlarge(NspSpRowMatrix *A, int m, int n)
{
  /* special case **/
  if ( n > A->n  ) {A->n = n ;/* A->mn = n*A->m;*/} /* easy for sparse matrix **/
  if ( m > A->m  ) 
    return nsp_sprowmatrix_enlarge_rows(A,m);
  return OK;
}

/**
 * nsp_sprowmatrix_concatr:
 * @A: 
 * @B: 
 * 
 * A = [A, B] 
 * Right concatenation on A, A is changed 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_concatr(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return  nsp_spcolmatrix_concatd((NspSpColMatrix *) A,(NspSpColMatrix *) B);
}

/**
 * nsp_sprowmatrix_concatd:
 * @A: 
 * @B: 
 * 
 *  A=[A; B ]  Down concatenation on A 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_concatd(NspSpRowMatrix *A, NspSpRowMatrix *B)
{ 
  return  nsp_spcolmatrix_concatr((NspSpColMatrix *) A,(NspSpColMatrix *) B);
}


/**
 * nsp_sprowmatrix_concatdiag:
 * @A: 
 * @B: 
 * 
 * Diag Concatenation A = [A,0;0,B] 
 * 
 * Return value:  %OK or %FAIL
 **/

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


/**
 * nsp_sprowmatrix_set_rowcol:
 * @A: 
 * @Rows: 
 * @Cols: 
 * @B: 
 * 
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked between 
 *  A and B 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_set_rowcol(NspSpRowMatrix *A, NspObject *Rows, NspObject *Cols, NspSpRowMatrix *B)
{
  return nsp_spcolmatrix_set_rowcol((NspSpColMatrix *) A,Cols,Rows,(NspSpColMatrix *) B);
}

/**
 * nsp_sprowmatrix_set_row:
 * @A: 
 * @Inds: 
 * @B: 
 * 
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
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_set_row(NspSpRowMatrix *A, NspObject *Inds, NspSpRowMatrix *B) 
{
  NspTypeBase *type;
  int i;
  int Bscal=0;
  index_vector index={0};
  index.iwork = matint_iwork1;

  /* check that we can cast Inds */
  
  if (( type = check_implements(Inds,nsp_type_matint_id)) == NULL )
    {
      Scierror("Object do not implements matint interface\n");
      return FAIL;
    }
  
  if ( nsp_get_index_vector_from_object(Inds,&index) == FAIL) goto fail;


  /* Calling a genric function which check arguments **/
  if (GenericMatSeRo(A,A->m,A->n,A->m*A->n,&index,
		     B,B->m,B->n,B->m*B->n,(F_Enlarge)nsp_sprowmatrix_enlarge,&Bscal)== FAIL) 
    goto fail;
  /* */
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_sprowmatrix_complexify(A) == FAIL ) goto fail;
    }
  if ( Bscal == 0 ) 
    {
      for ( i = 0  ; i < index.nval ; i++) 
	{
	  int rb,cb,kb,ia,ra,ca ;
	  rb= i % ((NspSMatrix *)Inds)->m;
	  cb= (i - rb )/((NspSMatrix *)Inds)->m;
	  kb =nsp_sprowmatrix_get_elt(B,rb,cb);
	  ia = index.val[i];
	  ra = ia % A->m;
	  ca= (ia - ra )/A->m;
	  if ( kb == -1 ) 
	    {
	      /* we must set the [ra,ca] element of A to 0.0 **/
	      int ok1 =nsp_sprowmatrix_delete_elt(A,ra,ca,0,A->D[ra]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_sprowmatrix_resize_row(A,ra,A->D[ra]->size-1) == FAIL) goto fail;
		}
	    }
	  else
	    {
	      /* must change or insert element in A **/
	      if (nsp_sprowmatrix_insert_elt(A,ra,ca,B,rb,kb)== FAIL) goto fail;
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
	  for ( i = 0  ; i < index.nval  ; i++) 
	    {
	      int ia,ra,ca,ok1 ;
	      ia = index.val[i];
	      ra = ia % A->m;
	      ca= (ia - ra )/A->m;
	      /* we must set the [ra,ca] element of A to 0.0 **/
	      ok1 =nsp_sprowmatrix_delete_elt(A,ra,ca,0,A->D[ra]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_sprowmatrix_resize_row(A,ra,A->D[ra]->size-1) == FAIL) goto fail;
		}
	    }
	}
      else 
	{
	  /* B is a non null scalar **/
	  for ( i = 0  ; i < index.nval  ; i++) 
	    {
	      int ia,ra,ca ;
	      ia = index.val[i];
	      ra = ia % A->m;
	      ca= (ia - ra )/A->m;
	      if (nsp_sprowmatrix_insert_elt(A,ra,ca,B,0,0)== FAIL) goto fail;
	    }
	}
    }

  nsp_free_index_vector_cache(&index);
  return OK;

 fail:
  nsp_free_index_vector_cache(&index);
  return FAIL;

}


/**
 * nsp_sprowmatrix_compress_row:
 * @A: 
 * @i: 
 * 
 * compress the #SpRow structure of the @i-th column of sparse Matrix @A 
 * by moving elements (eliminating the -1 values). the row indices 
 * stored in A->D[i]->J are also changed during the compression assuming 
 * that each -1 implies that the row indice is decreased by 1.
 * [1,2,-1,4,5,6,-1,11]--> [1,2,3,4,5,9].
 * The #SpRow structure is not reallocated
 * 
 * 
 * Return value: the number of deleted elements in the column @i.
 **/

int nsp_sprowmatrix_compress_row(NspSpRowMatrix *A, int i)
{
  return nsp_spcolmatrix_compress_col((NspSpColMatrix *)A,i);
}

/**
 * nsp_sprowmatrix_delete_rows:
 * @A: 
 * @Rows: 
 * 
 *  A(Rows,:) = [],  A is changed. 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_delete_cols(NspSpRowMatrix *A, NspObject *Cols)
{
  return nsp_spcolmatrix_delete_rows((NspSpColMatrix *) A,Cols);
}

/**
 * nsp_sprowmatrix_delete_cols:
 * @A: 
 * @Cols: 
 * 
 *  A(:,Cols) = []
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_delete_rows(NspSpRowMatrix *A, NspObject *Rows)
{
  return nsp_spcolmatrix_delete_cols((NspSpColMatrix *) A,Rows);
}


/*
 * Res=nsp_matrix_extract(A,Rows,Cols)
 * A unchanged, Rows and Cols are unchanged 
 */	

static NspSpRowMatrix *SpExtract_G(NspSpRowMatrix *A, NspObject *Rows, NspObject *Cols, int flag, int *err)
{
  NspMatrix *Work= NULL, *Index = NULL;
  NspSpRowMatrix *Loc=NULL;
  int i,j,Rm;

  index_vector index_c={0}, index_r={0};
  index_r.iwork = matint_iwork1;
  index_c.iwork = matint_iwork2;

  /* if ( A->m || A->n == 0) return nsp_sprowmatrix_create(NVOID,A->rc_type,A->m,A->n); */
  if (flag == 1) 
    {
      if ( nsp_get_index_vector_from_object(Rows,&index_r) == FAIL) return NULLSPROW;
      if ( index_r.min < 1 ||  index_r.max > A->n  ) 
	{
	  *err=1;
	  Scierror("Error:\tIndices out of bound\n");
	  goto err;
	}
    }
  
  if ( nsp_get_index_vector_from_object(Cols,&index_c) == FAIL) goto err;
  if (( Index = nsp_matrix_create(NVOID,'r',index_c.nval,1)) == NULLMAT ) goto err;
  nsp_qsort_int(index_c.val,Index->I,TRUE,index_c.nval,'i');
  
  *err=0;
  if (  index_c.min < 1 ||  index_c.max > A->n ) 
    {
      *err=1;
      goto err;
    }
  Rm= (flag == 1) ? index_r.nval : A->m;
  if ( (Loc =nsp_sprowmatrix_create(NVOID,A->rc_type,Rm,index_c.nval))== NULLSPROW) 
    goto err;

  /* used to store elements */
  if ( ( Work = nsp_matrix_create(NVOID,'r',2,index_c.nval)) == NULLMAT) return NULLSPROW;
  
  for ( i = 0 ; i < Loc->m ; i++)
    {
      int count;
      int imin,imax,k;
      SpRow *Ai, *Li;
      int row;
      row = (flag == 1) ? index_r.val[i] : i ;
      Ai= A->D[row];
      Li= Loc->D[i];
      Li->iw=0;
      if ( Ai->size == 0) continue; /* nothing to do row is empty */
      imin=0; imax= Ai->size-1 ; k = -1;
      Li->iw=0;
      count = nsp_bi_dichotomic_search_i(index_c.val,0,index_c.nval-1,Ai->J,imin,imax,Work,Index,0);
      /* now we know the row size */
      if (nsp_sprowmatrix_resize_row(Loc,i,count)==FAIL) return NULLSPROW;
      /* Fill columns of i-th row **/
      Li->iw=0;
      for ( j = 0 ; j < Li->size ; j++ )
	{	
	  int k= (int) Work->I[1+2*j];
	  Li->J[j]= (int) Work->I[2*j];
	  switch ( A->rc_type ) 
	    {
	    case 'r' : Li->R[j]= Ai->R[k];break;
	    case 'c' : Li->C[j]= Ai->C[k];break;
	    }
	}
      /* we sort the row in increasing row order, i.e J must be increasing */
      if ( Li->size > 1) 
	{
	  int *xb = Work->I;
	  nsp_qsort_int(Li->J,xb,TRUE,Li->size,'i');
	  if ( A->rc_type == 'r' ) 
	    C2F(dperm)(Li->R,&Li->size,xb);
	  else 
	    C2F(zperm)(Li->C,&Li->size,xb);
	}

    }

  nsp_matrix_destroy(Work);
  nsp_matrix_destroy(Index);
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  return(Loc);
 err:
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  if ( Work  != NULL) nsp_matrix_destroy(Work);
  if ( Index != NULL) nsp_matrix_destroy(Index);
  if ( Loc != NULL) nsp_sprowmatrix_destroy(Loc);
  return NULL;
}

/**
 * nsp_sprowmatrix_extract:
 * @A: 
 * @Rows: 
 * @Cols: 
 * 
 * returns A(Row,Cols) but @Rows is changed in the process.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_extract(NspSpRowMatrix *A, NspObject *Rows, NspObject *Cols)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_extract((NspSpColMatrix *) A,Cols,Rows))==NULL)
    return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/**
 * nsp_sprowmatrix_extract_elts:
 * @A: 
 * @Elts: 
 * 
 * return A(Elts)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_extract_elts(NspSpRowMatrix *A, NspObject *Elts)
{
  NspSpRowMatrix *Loc;
  int i,err=0,k;
  index_vector index={0};
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Elts,&index) == FAIL) return NULLSPROW;
  
  if ( A->m == 0 || A->n == 0) 
    {
      Loc = nsp_sprowmatrix_create(NVOID,A->rc_type,A->m,A->n);
      nsp_free_index_vector_cache(&index);
      return Loc;
    }
  if ( index.min < 1 || index.max > A->m * A->n ) /* possible overflow */
    {
      Scierror("Error:\tIndices out of bound\n");
      goto fail;
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      NspMatrix *Rows ; 
      if ((Rows = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT ) return NULLSPROW;
      Rows->R[0] = 1;
      /* like A(1,Elts) **/
      Loc=  SpExtract_G(A,NSP_OBJECT(Rows),Elts,1,&err);
      if (err==1 ) Scierror("Error:\tIndices out of bound\n");
      nsp_free_index_vector_cache(&index);
      return Loc;
    }
  else
    {
      if ( (Loc =nsp_sprowmatrix_create(NVOID,A->rc_type,index.nval,1))== NULLSPROW) 
	goto fail;
      for ( i=0; i < index.nval ; i++) 
	{
	  int el = index.val[i];
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
	      if (nsp_sprowmatrix_resize_row(Loc,i,1)==FAIL) goto fail;
	      Loc->D[i]->J[0]=0;
	      switch ( A->rc_type ) 
		{
		case 'r' : Loc->D[i]->R[0] =  A->D[row]->R[k]; break;
		case 'c' : Loc->D[i]->C[0] =  A->D[row]->C[k]; break;
		}
	    }
	}
    }
  nsp_free_index_vector_cache(&index);
  return Loc;

 fail:
   nsp_free_index_vector_cache(&index);
   return NULLSPROW;

}


/**
 * nsp_sprowmatrix_extract_rows:
 * @A: 
 * @Rows: 
 * @err: 
 * 
 * returns A(Rows,:) but @Rows is changed 
 * err is used inside for-loops 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_extract_cols(NspSpRowMatrix *A, NspObject *Cols, int *err)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_extract_rows((NspSpColMatrix *) A,Cols,err))==NULL)
    return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/*
 * A1=MatLoopCol(A1,M,i,rep)
 * Used in for loops 
 */	

/**
 * nsp_sprowmatrix_extract_cols:
 * @A: 
 * @Cols: 
 * @err: 
 * 
 * A(:,Cols)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_extract_rows(NspSpRowMatrix *A, NspObject *Rows, int *err)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_extract_cols((NspSpColMatrix *) A,Rows,err))==NULL)
    return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}


/**
 * nsp_sprowmatrix_diag_extract:
 * @A: 
 * @k: 
 * 
 * 
 * Returns the kthe diag of a Sparse NspMatrix as a Sparse Row.
 * 
 * Return value: a new  #NspSpRowMatrix or %NULLSPROW
 **/

NspSpRowMatrix *nsp_sprowmatrix_diag_extract(NspSpRowMatrix *A, int k)
{
  NspSpColMatrix *loc;
  loc = nsp_spcolmatrix_diag_extract((NspSpColMatrix *) A,-k);
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/**
 * nsp_sprowmatrix_set_diag:
 * @A: 
 * @Diag: 
 * @k: 
 * 
 * sets the kth diagonal of A to Diag 
 * A is enlarged and complexified if necessary 
 * 
 * Return value: 
 **/

int nsp_sprowmatrix_set_diag(NspSpRowMatrix *A, NspSpRowMatrix *Diag, int k)
{
  return nsp_spcolmatrix_set_diag((NspSpColMatrix *) A,(NspSpColMatrix *) Diag,-k);
}

/**
 * nsp_sprowmatrix_diag_create:
 * @Diag: 
 * @k: 
 * 
 *  Creates a Matrix with kth diag set to Diag 
 * 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_diag_create(NspSpRowMatrix *Diag, int k)
{
  NspSpColMatrix *loc;
  loc = nsp_spcolmatrix_diag_create((NspSpColMatrix *) Diag,-k);
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/**
 * nsp_sprowmatrix_mult:
 * @A: 
 * @B: 
 * 
 * returns @Ax@B  by the method of gustafson,acm t.o.m.s. 
 * vol 4 (1978) p250. 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_mult(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_mult((NspSpColMatrix *) B,(NspSpColMatrix *) A))==NULL)
    return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/**
 * nsp_sprowmatrix_mult_sp_m:
 * @A: a #NspSpRowMatrix
 * @X:  a #NspMatrix
 * @Res: a #NspMatrix (if NULL space is allocated otherwise Res hold the result)
 * 
 * @Ax@X when @X is a full matrix and returns the result as a full matrix.
 * 
 * Return value: a #NspMatrix or %NULLMAT (in case of failure)
 **/

/* (added by Bruno) */

NspMatrix *nsp_sprowmatrix_mult_sp_m(NspSpRowMatrix *A, NspMatrix *X, NspMatrix *Res)
{
  int i, j, k, ij, m = A->m, n = X->n, p = X->m;
  char rtype;
  NspMatrix *B=NULLMAT;

  rtype = 'c';
  if ( A->rc_type == 'r' && X->rc_type == 'r' )
    rtype = 'r';
  if ( A->n != X->m )
    {
      Scierror("SpMult : incompatible arguments\n");
      return NULLMAT;
    }

  if ( Res == NULL )
    {
      if ( (B = nsp_matrix_create(NVOID,rtype,m,n)) == NULLMAT ) 
	return NULLMAT;
    }
  else
    {
      /* Res should hold the result verify if it is valid... */
      if ( Res->m != A->m || Res->n != X->n || Res->rc_type != rtype )
	{
	  Scierror("SpMult : result badly allocated\n");
	  return NULLMAT;
	}
      B = Res;
    }


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

/**
 * nsp_sprowmatrix_mult_m_sp:
 * @X: 
 * @A: 
 * 
 * @Xx@A when @X is full 
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_sprowmatrix_mult_m_sp(NspMatrix *X,NspSpRowMatrix *A)
{
  NspMatrix *C = NULLMAT;
  int i, j, k, l,size,fact,factA;
  const int inc=1;
  char type = 'r';
  double zero=0.0;
  if ( A->rc_type == 'c' || X->rc_type == 'c' ) type = 'c';
  if ( A->m != X->n ) 
    {
      Scierror("SpMult : incompatible arguments\n");
      return NULLMAT;
    }

  if ( (C =nsp_matrix_create(NVOID,type,X->m,A->n)) == NULLMAT ) return NULLMAT;
  /* initialize to 0.0 */
  fact = (type == 'c') ? 2 : 1;
  size= fact*C->mn;
  nsp_dset(&size,&zero,C->R,&inc);
  factA = ( A->rc_type == 'c') ? 2 : 1;
  /* X*A */
  for ( i = 0 ; i < A->m ; i++) 
    {
      SpRow *Ai= A->D[i];
      for ( k = 0 ;  k < Ai->size ; k++) 
	{
	  void *val = Ai->R + k*factA;
	  j = Ai->J[k];
	  /* element A(i,j) != 0 */
	  for ( l =  0 ; l < X->m ; l++) 
	    {
	      /* C(l,j) += X(l,i)*A(i,j) */
	      if ( C->rc_type == 'r' )
		{
		  C->R[l+C->m*j] += X->R[l+X->m*i]*( *((double *) val));
		}
	      else
		{
		  doubleC *z = &C->C[l+C->m*j];
		  if ( X->rc_type == 'r') 
		    {
		      z->r += X->R[l+X->m*i]*((doubleC *) val)->r;
		      z->i += X->R[l+X->m*i]*((doubleC *) val)->i;
		    }
		  else if ( A->rc_type == 'r' ) 
		    {
		      z->r  += X->C[l+X->m*i].r *( *((double *) val));
		      z->i  += X->C[l+X->m*i].i *( *((double *) val));
		    }
		  else 
		    {
		      z->r += X->C[l+X->m*i].r * ((doubleC *) val)->r - 
			X->C[l+X->m*i].i * ((doubleC *) val)->i;
		      z->i += X->C[l+X->m*i].r*((doubleC *) val)->i + 
			X->C[l+X->m*i].i *((doubleC *) val)->r;
		    }
		}
	    }
	}
    }
  return C;
}



/**
 * nsp_sprowmatrix_complexify:
 * @A: 
 * 
 * Changes A to complex type and 
 * provide storage allocation 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/


int nsp_sprowmatrix_complexify(NspSpRowMatrix *A)
{
  return nsp_spcolmatrix_complexify((NspSpColMatrix *) A);
}

/**
 * nsp_sprowmatrix_setr:
 * @A: 
 * @d: 
 * 
 * Set real part of all non nul elements of A to d 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_setr(NspSpRowMatrix *A, double d)
{
  return nsp_spcolmatrix_setr((NspSpColMatrix *) A,d);
}


/**
 * nsp_sprowmatrix_seti:
 * @A: 
 * @d: 
 * 
 * Set imag part of all non nul elements of A to d 
 * if A is real A is changed to complex 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_seti(NspSpRowMatrix *A, double d)
{
  return nsp_spcolmatrix_seti((NspSpColMatrix *) A,d);
}


/**
 * nsp_sprowmatrix_from_mat:
 * @A: 
 * 
 * from full to sparse.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_from_mat(NspMatrix *A)
{ 
  return nsp_spcolmatrix_cast_to_sprow(nsp_spcolmatrix_from_mat_transpose(A));
}

/**
 * nsp_sprowmatrix_to_mat:
 * @Sp: 
 * 
 * from sparse to full.
 * 
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_sprowmatrix_to_mat(NspSpRowMatrix *Sp)
{ 	
  return nsp_spcolmatrix_to_mat_transpose((NspSpColMatrix *) Sp);
}

/**
 * nsp_sprowmatrix_transpose:
 * @A: 
 * 
 * Transpose a sparse Matrix, A is unchanged and a new matrix is created and 
 * returned. 
 * Warning : For Complex matrices transpose is a' = transp+ conj
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_transpose(const NspSpRowMatrix *A)
{
  NspSpColMatrix *loc;
  if ((loc=nsp_spcolmatrix_transpose((const NspSpColMatrix *) A))==NULL)
    return NULLSPROW;
  return nsp_spcolmatrix_cast_to_sprow(loc);
}




/**
 * nsp_sprowmatrix_add:
 * @A: 
 * @B: 
 * 
 * A+B
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_add(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return nsp_spcolmatrix_cast_to_sprow(nsp_spcolmatrix_add((NspSpColMatrix *)A,
							   (NspSpColMatrix *)B));
}

/**
 * nsp_sprowmatrix_sub:
 * @A: 
 * @B: 
 * 
 * A-B
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_sub(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return nsp_spcolmatrix_cast_to_sprow(nsp_spcolmatrix_sub((NspSpColMatrix *)A,
							   (NspSpColMatrix *)B));
}

/**
 * nsp_sprowmatrix_multtt:
 * @A: 
 * @B: 
 * 
 * A.*B
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_multtt(NspSpRowMatrix *A, NspSpRowMatrix *B)
{
  return nsp_spcolmatrix_cast_to_sprow(nsp_spcolmatrix_multtt((NspSpColMatrix *)A,
							      (NspSpColMatrix *)B));
}

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )


/**
 * nsp_sprowmatrix_mult_scal:
 * @A: 
 * @B: 
 * 
 * A = A.*B where B is a scalar sparse ( [] or scalar )
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_mult_scal(NspSpRowMatrix *A, NspSpRowMatrix *B)
{ 
  int i,k;
  if ( (B->m == 0 && B->n == 0) || B->D[0]->size == 0)
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
      A->m = A->n = 0; /* A->mn = 0; */
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


/**
 * nsp_sprowmatrix_op_scal:
 * @A: 
 * @B: 
 * @flag: 
 * @op: 
 * 
 * A = A op B where B is a scalar sparse ( [] or scalar )
 * op can be + or - 
 * if B==[] or B==[0] 
 *     the result is A  and *flag == 1
 * else a full matrix is returned and *flag == 0 
 * 
 * op can be '+'(A+B) ,'-' (A-B), '#' (-A+B)
 * 
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_sprowmatrix_op_scal(NspSpRowMatrix *A, NspSpRowMatrix *B, int *flag, char op)
{ 
  char type = 'r';
  int i,j,k;
  NspMatrix *Loc;
  if ( ( B->m == 0 && B->n == 0 ) || B->D[0]->size == 0)
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
	  nsp_mat_set_ival(Loc,0.0);
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
 * @x: an integer 
 * @val: an array of int (unchanged)
 * @imin: an integer 
 * @imax: an integer 
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

/**
 * nsp_bi_dichotomic_search_int:
 * @x: 
 * @xpmin: 
 * @xpmax: 
 * @xmin: 
 * @xmax: 
 * @val: 
 * @imin: 
 * @imax: 
 * @Work: 
 * @Index: 
 * @count: 
 * 
 * search x[xpmin,xpmax]-1 in val[imin:imax] 
 * and store information in Work as processing. 
 * The number of stored values is returned in count 
 * xmin >= 0 ==> we know that x[xpmin]-1 is >= val[xmin] 
 * xmin == -1 x[xpmin]-1 is < val[xmin] 
 * xmin == -2 x[xpmin]-1 is > val[xmax] 
 * same rules for for xmax and x[xpmax] 
 * x and val are increasing 
 * 
 * 
 * Return value: 
 **/

static int nsp_bi_dichotomic_search_int_i(const int *x,int xpmin,int xpmax,int xmin,int xmax,
					  const int *val,int imin,int imax,
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
	      if ( (x[k]) == val[xmin] ) 
		{
		  /* Sciprintf("OK pour A(i,%d) -> %d \n",xmin+1,((int) x[k])); */
		  Work->R[2*count]= Index->I[k]-1; /* column in final row*/
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
      if ( xmin >= 0 && ( x[xpmin]) == val[xmin] ) 
	{
	  /* Sciprintf("OK pour A(i,%d) -> %d \n",xmin+1,((int) x[xpmin])); */
	  Work->R[2*count]= Index->I[xpmin]-1; /* column in final row*/
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
      jval = nsp_dichotomic_search((x[j]),val,imin,imax);
      /* search in [xpmin,j] */
      imax1= (jval >= 0) ? Min(imax,jval) : imax ;
      count = nsp_bi_dichotomic_search_int_i(x,xpmin,j,xmin,jval,val,imin,imax1,Work,Index,count);
      imin1= (jval >= 0) ? Max(imin,jval) : imin ;
      count =nsp_bi_dichotomic_search_int_i(x,j,xpmax,jval,xmax,val,imin1,imax,Work,Index,count);
    }
  return count;
}

/**
 * nsp_bi_dichotomic_search:
 * @x: 
 * @xpmin: 
 * @xpmax: 
 * @val: 
 * @imin: 
 * @imax: 
 * @Work: 
 * @Index: 
 * @count: 
 * 
 * 
 * 
 * Return value: 
 **/

static int nsp_bi_dichotomic_search_i(const int *x,int xpmin,int xpmax,const int *val,int imin,int imax,
				      NspMatrix *Work,NspMatrix *Index,int count)
{
  int xmin = nsp_dichotomic_search(x[xpmin],val,imin,imax);
  int xmax = nsp_dichotomic_search(x[xpmax],val,imin,imax);
  count = nsp_bi_dichotomic_search_int_i(x,xpmin,xpmax,xmin,xmax,val,imin,imax,Work,Index,0);
  /* last point */
  if ( xmax >= 0 && ( x[xpmax]) == val[xmax] ) 
    {
      /* Sciprintf("OK pour A(i,%d) -> %d \n",xmax+1,((int) Cols->R[Cols->mn-1]));*/
      Work->R[2*count]= Index->I[xpmax]-1; /* column in final row*/
      Work->R[1+2*count]= xmax; /* position in Ai of the value */
      count++;
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

static int SpM_general(nsp_num_formats *fmt,NspSpRowMatrix *Sp, int indent)
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
  return TRUE;
}

static int nsp_sprowmatrix_print_internal(nsp_num_formats *fmt,NspSpRowMatrix *m, int indent)
{
  int rep = TRUE;
  if ( m->m == 0 || m->n == 0) 
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
	  return rep;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  Sciprintf("No as read for sparse \n");
	}
      else
	{
	  rep = SpM_general(fmt,m,indent);
	}
    }
  return rep;
}


/**
 * nsp_sprowmatrix_clean:
 * @A: 
 * @rhs: 
 * @epsa: 
 * @epsr: 
 * 
 * A = Matclean(a) clean A according to epsa and epsr 
 * epsa is used if rhs >= 1 
 * epsr is used if rhs >= 2
 * A is changed, 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_clean(NspSpRowMatrix *A, int rhs, double epsa, double epsr)
{
  return nsp_spcolmatrix_clean((NspSpColMatrix *)A,rhs,epsa,epsr);
}

/**
 * nsp_sprowmatrix_maximinitt_g:
 * @A: 
 * @B: 
 * @flag: 
 * @minmaxflag: 
 * @err: 
 * 
 * max or min (A,B)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_maximinitt_g(NspSpRowMatrix *A, NspSpRowMatrix *B, int flag, int minmaxflag, int *err)
{
  NspSpColMatrix *loc;
  loc = nsp_spcolmatrix_maximinitt_g((NspSpColMatrix *) A,(NspSpColMatrix *) B,flag,minmaxflag,err);
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/**
 * nsp_sprowmatrix_maxitt:
 * @A: 
 * @B: 
 * @flag: 
 * @err: 
 * 
 *  term to term max A(i;j) = Max(A(i,j),B(i,j) 
 *  Res(i,j) = 1 or 2 or 0 
 *   1 if Max(A(i,j),B(i,j)==A(i,j)
 *   2 if Max(A(i,j),B(i,j)==B(i,j)
 *   0 if A(i,j)=B(i,j)=0
 *  A changed, B unchanged, 
 *  Res Created if flag == 1
 * 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/


NspSpRowMatrix *nsp_sprowmatrix_maxitt(NspSpRowMatrix *A, NspSpRowMatrix *B, int flag, int *err)
{
  return nsp_sprowmatrix_maximinitt_g(A,B,flag,1,err);
}

/**
 * nsp_sprowmatrix_minitt:
 * @A: 
 * @B: 
 * @flag: 
 * @err: 
 * 
 *  term to term min A(i;j) = Min(A(i,j),B(i,j) 
 *  Res(i,j) = 1 or 2 or 0 
 *   1 if Min(A(i,j),B(i,j)==A(i,j)
 *   2 if Min(A(i,j),B(i,j)==B(i,j)
 *   0 if A(i,j)=B(i,j)=0
 *  A changed, B unchanged, 
 *  Res Created if flag == 1
 * 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_minitt(NspSpRowMatrix *A, NspSpRowMatrix *B, int flag, int *err)
{
  return nsp_sprowmatrix_maximinitt_g(A,B,flag,-1,err);
}

/**
 * nsp_sprowmatrix_realpart:
 * @A: 
 * 
 * Return the Real part of Matrix A in A.
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_realpart(NspSpRowMatrix *A)
{
  return nsp_spcolmatrix_realpart((NspSpColMatrix *) A);
}


/**
 * nsp_sprowmatrix_imagpart:
 * @A: 
 * 
 * Return the Imaginary part of Matrix A in A.
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_imagpart(NspSpRowMatrix *A)
{
  return nsp_spcolmatrix_imagpart((NspSpColMatrix *) A);
}


/**
 * nsp_sprowmatrix_isreal:
 * @A: a #NspMatrix 
 * @strict: %TRUE or %FALSE 
 * 
 * checks if @A is a real sparse matrix.
 * @A is a real matrix if @A->rc_type is equal to 'r'
 * or if the imaginary part is only filled with 0.0.
 * If @strict is set to %TRUE then the function returns 
 * %TRUE only if @A->rc_type is equal to 'r'
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_sprowmatrix_isreal(const NspSpRowMatrix *A, int strict)
{
  return nsp_spcolmatrix_isreal((NspSpColMatrix *) A,strict);
}

/*
 *nsp_mat_inv_el: a(i,j)=1/a(i,j) A est changee
 */

/*
 *nsp_mat_kron: produit de Kroeneker
 * A et B sont inchanges 
 */

/*
 *nsp_mat_sort: Index=Sort(A)
 * A is changed, Index created with the indexes 
 * return NULLMAT on error 
 * WARNING : A must be real but the test is not done here 
 * ======
 */

/**
 * nsp_sprowmatrix_sum:
 * @A: 
 * @flag: 
 * 
 * Sum =nsp_mat_sum(A ,B])
 *     A is unchanged 
 * if B= 'c' the sum for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the sum for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the full sum is computed 
 * 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_sum(NspSpRowMatrix *A, char *flag)
{
  NspSpColMatrix *loc=NULL;
  switch ( flag[0]) 
    {
    case 'f':
    case 'F':
      loc = nsp_spcolmatrix_sum((NspSpColMatrix *)A,flag); break;
    case 'r':
    case 'R':
      loc = nsp_spcolmatrix_sum((NspSpColMatrix *)A,"c"); break;
    case 'c':
    case 'C':
      loc = nsp_spcolmatrix_sum((NspSpColMatrix *)A,"r"); break;
    }
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/*
 * Prod =nsp_mat_prod(A ,B])
 *     A is unchanged 
 * if B= 'c' the prod for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the prod for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the full prod is computed 
 */

/*
 *nsp_mat_cum_prod: Cumulative Product of all elements of A
 * A is unchanged 
 */

/*
 *nsp_mat_cum_sum: Cumulative Sum of all elements of A
 * A is unchanged 
 */

/**
 * nsp_sprowmatrix_maxi:
 * @A: 
 * @flag: 
 * @Imax: 
 * @lhs: 
 * 
 * [max,imax]=max(A,'c'|'r'|'g')
 * Max =nsp_mat_maxi(A,B,Imax,lhs)
 *     A is unchanged 
 * if B= 'c' the max for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the max for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the maximum 
 * Imax is created if lhs == 2 
 * Note that Imax is a full matrix XXX not a good idea ? 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_maxi(NspSpRowMatrix *A, char *flag, NspMatrix **Imax, int lhs)
{
  NspSpColMatrix *loc=NULL;
  switch (flag[0]) 
    {
    case 'f':
    case 'F':
      loc = nsp_spcolmatrix_maxi((NspSpColMatrix *)A,flag,Imax,lhs);
      /* Imax is for the transpose */
      if ( lhs == 2)
	{
	  int ival = (*Imax)->R[0]-1;
	  int rb= ival % A->n;
	  int cb= (ival - rb )/A->n;
	  (*Imax)->R[0]= rb +1 + cb*A->m;
	}
      break;
    case 'r':
    case 'R':
      loc = nsp_spcolmatrix_maxi((NspSpColMatrix *)A,"c",Imax,lhs);break;
    case 'c':
    case 'C': 
      loc = nsp_spcolmatrix_maxi((NspSpColMatrix *)A,"r",Imax,lhs);break;
    }
  if ( lhs == 2 )
    {
      int ival = (*Imax)->m;
      (*Imax)->m = (*Imax)->n;
      (*Imax)->n = ival;
    }
  return nsp_spcolmatrix_cast_to_sprow(loc);
}

/**
 * nsp_sprowmatrix_mini:
 * @A: 
 * @flag: 
 * @Imax: 
 * @lhs: 
 * 
 * [max,imax]=max(A,'c'|'r'|'g')
 * Max =nsp_mat_mini(A,B,Imax,lhs)
 *     A is unchanged 
 * if B= 'c' the max for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the max for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the minimum 
 * Imax is created if lhs == 2 
 * Note that Imax is a full matrix XXX not a good idea ? 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_mini(NspSpRowMatrix *A, char *flag, NspMatrix **Imax, int lhs)
{
  NspSpColMatrix *loc=NULL;
  switch (flag[0]) 
    {
    case 'f':
    case 'F':
      loc = nsp_spcolmatrix_mini((NspSpColMatrix *)A,flag,Imax,lhs);
      /* Imax is for the transpose */
      if ( lhs == 2)
	{
	  int ival = (*Imax)->R[0]-1;
	  int rb= ival % A->n;
	  int cb= (ival - rb )/A->n;
	  (*Imax)->R[0]= rb +1 + cb*A->m;
	}
      break;
    case 'r':
    case 'R':
      loc = nsp_spcolmatrix_mini((NspSpColMatrix *)A,"c",Imax,lhs);break;
    case 'c':
    case 'C': 
      loc = nsp_spcolmatrix_mini((NspSpColMatrix *)A,"r",Imax,lhs);break;
    }
  if ( lhs == 2 )
    {
      int ival = (*Imax)->m;
      (*Imax)->m = (*Imax)->n;
      (*Imax)->n = ival;
    }
  return nsp_spcolmatrix_cast_to_sprow(loc);
}




/*
 * Creates a Matrix and initialize it with the 
 * function func 
 * R=func(i,j) or R=func(i,j,&Imag) 
 */


/**
 * nsp_sprowmatrix_triu:
 * @A: 
 * @k: 
 * 
 * A=Triu(A)
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_triu(NspSpRowMatrix *A,int k)
{
  return nsp_spcolmatrix_tril((NspSpColMatrix *) A,-k);
}


/**
 * nsp_sprowmatrix_tril:
 * @A: 
 * @k: 
 * 
 * A=Tril(A)
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_tril(NspSpRowMatrix *A,int k)
{
  return nsp_spcolmatrix_triu((NspSpColMatrix *) A,-k);
}


/**
 * nsp_sprowmatrix_eye:
 * @m: 
 * @n: 
 * 
 * A=Eye(m,n)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_eye(int m, int n)
{
  return nsp_spcolmatrix_cast_to_sprow(nsp_spcolmatrix_eye(n,m));
}


/**
 * nsp_sprowmatrix_ones:
 * @m: 
 * @n: 
 * 
 * A=ones(m,n)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_ones(int m, int n)
{
  return nsp_spcolmatrix_cast_to_sprow(nsp_spcolmatrix_ones(n,m));
}


/**
 * nsp_sprowmatrix_zeros:
 * @m: 
 * @n: 
 * 
 * A=zeros(m,n)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_zeros(int m, int n)
{
  return nsp_sprowmatrix_create(NVOID,'r',m,n);
}

/*
 *nsp_mat_rand: A=rand(m,n)
 * A is changed  a 'ecrire ? 
 */

/*
  A Set of term to term function on Matrices (complex or real)
*/

/*
 *nsp_mat_pow_el(A,B) a(i,i)**b(i,i) 
 * A is changed  since 0.^0 --> 1 the returned matrix is full
 */

/*
 *nsp_mat_pow_scalar(A,B) a(i,i)**b
 * A is changed 
 */

/*
 * MatPowScalarMat(A,B) a(i,j)=b**a(i,j)
 * A is changed 
 */

/*
 *nsp_mat_div_el(A,B) a(i,i)/b(i,i) 
 * A is changed 
 */

/*
 *nsp_mat_div_scalar(A,B) a(i,i)/b
 * A is changed 
 */

/*
 *nsp_mat_bdiv_el(A,B) a(i,j) = a(i,j) \ b(i,j) 
 * A is changed 
 */

/*
 *nsp_mat_bdiv_scalar(A,B) a(i,j)= a(i,j) \ b
 * A is changed 
 */

/*
 * A=nsp_mat_mult_el(A,B) a(i,i).*b(i,i) 
 * A is changed 
 */

/*
 * A=Acos(A), 
 * A is changed 
 */

typedef double (*Func1) (double);
typedef void   (*Func2) (const doubleC *, doubleC *);

static NspMatrix* SpUnary2Full(NspSpRowMatrix *A, Func1 F1, Func2 F2)
{
  double val ;
  doubleC Cval,Czero={0.0,0.0};
  int i,j,k;
  NspMatrix *Loc;
  if ((Loc = nsp_matrix_create(NVOID,A->rc_type,A->m,A->n))== NULLMAT) return(NULLMAT);
  switch ( A->rc_type )
    {
    case 'r' : val = (*F1)(0.00); nsp_mat_set_rval(Loc,val); break;
    case 'c' : (*F2)(&Czero,&Cval);
      nsp_mat_set_rval(Loc,Cval.r);
      nsp_mat_set_ival(Loc,Cval.i);
      break;
    }
  if ( A->rc_type == 'r' )
    {
      for ( i = 0 ; i < A->m ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  {
	    j= A->D[i]->J[k];
	    Loc->R[i+Loc->m*j] = (*F1)( A->D[i]->R[k]);
	  }
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  {
	    j= A->D[i]->J[k];
	    (*F2)(&A->D[i]->C[k],&Loc->C[i+Loc->m*j]);
	  }
    }
  return Loc;
}

/**
 * nsp_sprowmatrix_acos:
 * @A: 
 * 
 * returns cos(A) as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_sprowmatrix_acos(NspSpRowMatrix *A)
{
  return SpUnary2Full(A,acos,nsp_acos_c);
}

/*
 * A=Acosh(A), 
 * A is changed 
 */

/**
 * nsp_sprowmatrix_acosh:
 * @A: 
 * 
 * returns cosh(A) as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_sprowmatrix_acosh(NspSpRowMatrix *A)
{
  return SpUnary2Full(A,acosh,nsp_acosh_c);
}


/*
 * Generic Function for Sparse unary operators 
 * computes A=f1(A) or A=f2(A) assuming fi(0)=0
 */

static void  SpUnary(NspSpRowMatrix *A, Func1 F1, Func2 F2)
{
  int i,k,compress,ndel;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	{
	  compress=0;
	  for ( k=0; k < A->D[i]->size ; k++ ) 
	    {
	      A->D[i]->R[k] = (*F1)(A->D[i]->R[k]);
	      if ( A->D[i]->R[k] == 0.0) 
		{
		  compress=1; A->D[i]->J[k]=-1;
		}
	    }
	  if ( compress == 1) 
	    {
	      ndel =nsp_sprowmatrix_compress_row(A,i);
	      nsp_sprowmatrix_resize_row(A,i,A->D[i]->size-ndel);
	    }
	}
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	{
	  compress=0;
	  for ( k=0; k < A->D[i]->size ; k++ ) 
	    {
	      (*F2)(&A->D[i]->C[k],&A->D[i]->C[k]);
	      if ( A->D[i]->C[k].r == 0.0 &&  A->D[i]->C[k].i == 0.0 )
		{
		  compress=1;
		  A->D[i]->J[k]=-1;
		}
	    }
	  if ( compress == 1) 
	    {
	      ndel =nsp_sprowmatrix_compress_row(A,i);
	      nsp_sprowmatrix_resize_row(A,i,A->D[i]->size-ndel);
	    }
	}
    }
}

/**
 * nsp_sprowmatrix_asin:
 * @A: 
 * A=asin(A)
 * 
 **/


void nsp_sprowmatrix_asin(NspSpRowMatrix *A)
{
  SpUnary(A,asin,nsp_asin_c);
}

/**
 * nsp_sprowmatrix_asinh:
 * @A: 
 * 
 * A=Asinh(A),
 **/

void nsp_sprowmatrix_asinh(NspSpRowMatrix *A)
{
  SpUnary(A,asinh,nsp_asinh_c);
}

/**
 * nsp_sprowmatrix_atan:
 * @A: 
 * 
 * A=Atan(A),
 **/

void nsp_sprowmatrix_atan(NspSpRowMatrix *A)
{
  SpUnary(A,atan,nsp_atan_c);
}


/**
 * nsp_sprowmatrix_atanh:
 * @A: 
 * 
 * A=Atanh(A)
 **/

#ifdef WIN32 
/* XXXX : bug in WIN32 in atanh */
extern double nsp_log1p(double);
static double nsp_atanh(double x) {
  return 0.5 * nsp_log1p( 2 *(x) / (1 - (x)));
}
#else 
static double nsp_atanh(double x) {
  return atanh(x);
}
#endif 

void nsp_sprowmatrix_atanh(NspSpRowMatrix *A)
{
  SpUnary(A,nsp_atanh,nsp_atanh_c);
}


/**
 * nsp_sprowmatrix_ceil:
 * @A: 
 * 
 * A=Ceil(A)
 **/

void nsp_sprowmatrix_ceil(NspSpRowMatrix *A)
{
  SpUnary(A,ceil,nsp_ceil_c);
}



static double R_aint(double x) { return aint(x);} 
/** 
 * nsp_sprowmatrix_int:
 * @A: 
 * 
 * 
 * A=Int(A)
 **/

void nsp_sprowmatrix_int(NspSpRowMatrix *A)
{
  SpUnary(A,R_aint,nsp_aint_c);
}


/**
 * nsp_sprowmatrix_floor:
 * @A: 
 * 
 * A=Floor(A) 
 **/

void nsp_sprowmatrix_floor(NspSpRowMatrix *A)
{
  SpUnary(A,floor,nsp_floor_c);
}



static double R_anint(double x) { return anint(x);} 
/**
 *nsp_sprowmatrix_round: 
 * @A: 
 * 
 * A=Round(A)
 **/

void nsp_sprowmatrix_round(NspSpRowMatrix *A)
{
  SpUnary(A,R_anint,nsp_round_c);
}

/**
 * nsp_sprowmatrix_sign:
 * @A: 
 * 
 * 
 * A=Sign(A)
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_sign(NspSpRowMatrix *A)
{
  return nsp_spcolmatrix_sign((NspSpColMatrix *)A);
}

/**
 * nsp_sprowmatrix_tan:
 * @A: 
 * 
 * A=Tan(A) 
 **/

void nsp_sprowmatrix_tan(NspSpRowMatrix *A)
{
  SpUnary(A,tan,nsp_tan_c);
}

/**
 * nsp_sprowmatrix_tanh:
 * @A: 
 * 
 * A=Tanh(A)
 **/

void nsp_sprowmatrix_tanh(NspSpRowMatrix *A)
{
  SpUnary(A,tanh,nsp_tanh_c);
}

/**
 * nsp_sprowmatrix_abs:
 * @A: 
 * 
 * 
 * A=Abs(A), absolue value or module of each element 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_abs(NspSpRowMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->R[k] = fabs(A->D[i]->R[k]);
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->C[k].r =nsp_abs_c(&A->D[i]->C[k]);
      if (nsp_sprowmatrix_realpart(A) == FAIL) return FAIL;
    }
  return(OK);
}

/**
 * nsp_sprowmatrix_erf:
 * @A: 
 * 
 * 
 * A=Erf(A), Erf function 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_erf(NspSpRowMatrix *A)
{
  return nsp_spcolmatrix_erf((NspSpColMatrix *)A);
}
/**
 * nsp_sprowmatrix_erf:
 * @A: 
 * 
 * 
 * A=Erf(A), Erf function 
 * Return value:  %OK or %FAIL
 **/

/*
 * A=Erfc(A), Erf function 
 */

/*
  int SpErfc(A)
  NspSpRowMatrix *A;
  {
  int i ;
  if ( A->rc_type == 'r') 
  {
  for ( i = 0 ; i < A->mn ; i++) A->R[i]= erfc(A->R[i]);
  }
  else
  {
  Scierror("Erf function argument must be real\n");
  return(FAIL);
  }
  return(OK);
  }
**/


/**
 * nsp_sprowmatrix_arg:
 * @A: 
 * 
 * 
 * A=Arg(A),
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_arg(NspSpRowMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  {
	    if(  A->D[i]->R[k] >= 0.0 ) 
	      A->D[i]->R[k] = 0.0;
	    else if(  A->D[i]->R[k] < 0.0 ) 
	      A->D[i]->R[k] = M_PI;
	  }
      /* should be cleaned */
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->C[k].r =nsp_arg_c(&A->D[i]->C[k]);
      if (nsp_sprowmatrix_realpart(A) == FAIL) return FAIL;
    }
  return(OK);
}

/*
 * A=Polar(A,B),
 * A=A(cos(B)+%i*sin(B);
 */


/**
 * nsp_sprowmatrix_conj:
 * @A: 
 * 
 * A=real(A)-i*Imag(A) A is changed  only if imaginary 
 **/

void nsp_sprowmatrix_conj(NspSpRowMatrix *A)
{
  return nsp_spcolmatrix_conj((NspSpColMatrix *)A);
}


/**
 * nsp_sprowmatrix_cos:
 * @A: 
 * 
 * 
 * Cos(A)
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_sprowmatrix_cos(NspSpRowMatrix *A)
{
  return SpUnary2Full(A,cos,nsp_cos_c);
}

/**
 * nsp_sprowmatrix_cosh:
 * @A: 
 * 
 * 
 * Cosh(A)
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_sprowmatrix_cosh(NspSpRowMatrix *A)
{
  return SpUnary2Full(A,cosh,nsp_cosh_c);
}

/**
 * nsp_sprowmatrix_expel:
 * @A: 
 * 
 * exp(A)
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_sprowmatrix_expel(NspSpRowMatrix *A)
{
  return SpUnary2Full(A,exp,nsp_exp_c);
}


/**
 * nsp_sprowmatrix_logel:
 * @A: 
 * 
 * 
 * log(A)
 * The real case is special since the result can be complex
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_logel(NspSpRowMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r')
    {
      /* Check if really real or imaginary case */
      int itr = 0;
      for ( i=0 ; i < A->m ; i++) 
	for ( k=0 ; k < A->D[i]->size ; k++) 
	  if ( A->D[i]->R[k] < 0.00 ) 
	    {
	      itr = 1; break;
	    }
      if ( itr == 0) 
	{
	  /* real case sqrt(A) is real  */
	  SpUnary(A,log,nsp_log_c);
	  return OK;
	}
      else 
	{
	  /* result is complex  */
	  if (nsp_sprowmatrix_seti(A,0.00) == FAIL ) return FAIL;
	  SpUnary(A,log,nsp_log_c);
	  return OK;
	}
    }
  /* A is complex and sqrt(A) too **/
  SpUnary(A,log,nsp_log_c);
  return OK;
}

/**
 * nsp_sprowmatrix_sin:
 * @A: 
 * 
 * A=sin(A)
 **/

void nsp_sprowmatrix_sin(NspSpRowMatrix *A)
{
  SpUnary(A,sin,nsp_sin_c);
}


/**
 * nsp_sprowmatrix_sinh:
 * @A: 
 * 
 * A=Sinh(A)
 **/

void nsp_sprowmatrix_sinh(NspSpRowMatrix *A)
{
  SpUnary(A,sinh,nsp_sinh_c);
}

/**
 * nsp_sprowmatrix_sqrtel:
 * @A: 
 * 
 * 
 *  A=SqrtEl(A)  term to term square root
 * The real case is special since the result can be complex
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_sqrtel(NspSpRowMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r')
    {
      /* Check if really real or imaginary case */
      int itr = 0;
      for ( i=0 ; i < A->m ; i++) 
	for ( k=0 ; k < A->D[i]->size ; k++) 
	  if ( A->D[i]->R[k] < 0.00 ) 
	    {
	      itr = 1; break;
	    }
      if ( itr == 0) 
	{
	  /* real case sqrt(A) is real  */
	  SpUnary(A,sqrt,nsp_sqrt_c);
	  return OK;
	}
      else 
	{
	  /* result is complex  */
	  if (nsp_sprowmatrix_seti(A,0.00) == FAIL ) return FAIL;
	  SpUnary(A,sqrt,nsp_sqrt_c);
	  return OK;
	}
    }
  /* A is complex and sqrt(A) too **/
  SpUnary(A,sqrt,nsp_sqrt_c);
  return OK;
}

/**
 * nsp_sprowmatrix_minus:
 * @A: 
 * 
 * 
 * A= -A 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_minus(NspSpRowMatrix *A)
{
  return nsp_spcolmatrix_minus((NspSpColMatrix *)A);
}


/*
 * Kronecker product of two Matrices 
 * PK is the result it must be created 
 * before calling this function size (AmxBm,AnxBn)
 * The rule to compute PK is the following 
 * PK[ i + j*B->m + k*(B->m*A->m) + p*(B->m*A->m*B->n)] = a(j,p)*b(i,k)
 * The i-loop leads to dcopy calls 
 */

/*
 *nsp_mat_magic: A=Magic(n)
 */

/*
 *nsp_mat_franck: A=Franck(n)
 */

/*
 *nsp_mat_hilbert: A=Hilbert(n)
 */

/*
 * Comparison operators
 */

/*
 * Operation on Matrices leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j) 
 * with the special case : 
 *      A(i;j)op B(0,0) or A(0,0) op B(i,j) if A or B are of size 1x1
 *      
 * A and B are unchanged : Res is created 
 */

/**
 * nsp_sprowmatrix_find:
 * @A: 
 * @lhs: 
 * @Res1: 
 * @Res2: 
 * 
 * returns in a Matrix the indices for which the 
 * Matrix A has non zero entries 
 * A is left unchanged
 * according to lhs one or two arguments are returned 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprowmatrix_find(NspSpRowMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2)
{
  int k,i,count=0;
  /* first pass for counting **/
  for ( i=0 ; i < A->m ; i++) 
    {
      count += A->D[i]->size ;
    }
  if ( lhs == 1) 
    {
      *Res1 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res1 == NULLMAT) return FAIL;
      count=0;
      for ( i = 0 ; i < A->m ; i++ )
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    (*Res1)->R[count++] = A->D[i]->J[k]*A->m + i + 1;
	}
    }
  else 
    {
      *Res1 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res1 == NULLMAT) return FAIL;
      *Res2 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res2 == NULLMAT) return FAIL;
      count=0;
      for ( i = 0 ; i < A->m ; i++ )
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      (*Res1)->R[count]   = i + 1;
	      (*Res2)->R[count++] = A->D[i]->J[k] + 1;
	    }
	}
    }
  return OK;
}


/**
 * nsp_sprowmatrix_rand:
 * @m: number of rows
 * @n: number of columns
 * @sparsity: percent of non null elements
 * @crand: a character ('n' for normal or 'u' for uniform) 
 * 
 * returns a new #NspSpRowMatrix filled with random values ('n' or 'u') 
 * the percent of non null elements is given by @sparsity.
 * 
 * Return value: a new  #NspRowMatrix or %NULLSPCOL
 **/

NspSpRowMatrix *nsp_sprowmatrix_rand(int m,int n,double sparsity,char crand)
{
  return nsp_spcolmatrix_cast_to_sprow(nsp_spcolmatrix_rand(n,m,sparsity,crand));
}

