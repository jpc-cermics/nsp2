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

#include <nsp/object.h> 
#include <nsp/matint.h> 
#include <nsp/matrix.h> 
#include <nsp/spcolmatrix.h> 
#include <nsp/sprowmatrix.h> 
#include <nsp/spmaxpcolmatrix.h> 
#include <nsp/matrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/cells.h> 
#include <nsp/matint.h> 
#include <nsp/hobj.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 

#include "nsp/pr-output.h"
#include "nsp/blas.h"
#include "nsp/matutil.h" 
#include "nsp/gsort-p.h" 
#include "nsp/cnumeric.h" 
#include "nsp/nsp_lapack.h" /* vector_norm */
#include "../librand/grand.h"

extern int C2F(dperm) (double A[],int ind[],int *nv);
extern int C2F(zperm) (doubleC A[],int ind[],int *nv);
typedef void (*BopLeft) (SpCol *,char,int *,SpCol *,char,int);
typedef void (*BopBoth) (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
typedef void (*BopBothNull) (SpCol *,char,int *);
typedef void (*BopRight) (SpCol *,char,int *,SpCol *,char,int);

static int nsp_spmaxpcolmatrix_print_internal(nsp_num_formats *fmt,NspSpMaxpColMatrix *m, int indent);
static NspSpMaxpColMatrix *BinaryOp (NspSpMaxpColMatrix *,NspSpMaxpColMatrix *,BopLeft,BopBoth,
				 BopRight,int force_real);
static NspSpMaxpColMatrix *BinaryOp_bis(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, BopLeft BinLeft, BopBoth BinBoth, 
				    BopBothNull BinBothNull, BopRight BinRight);
static void PlusLeft (SpCol *,char,int *,SpCol *,char,int);
static void PlusBoth (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
static void PlusRight (SpCol *,char,int *,SpCol *,char,int);

static void MinusLeft (SpCol *,char,int *,SpCol *,char,int);
static void MinusBoth (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
static void MinusRight (SpCol *,char,int *,SpCol *,char,int);

static void MultttLeft (SpCol *,char,int *,SpCol *,char,int);
static void MultttBoth (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
static void MultttRight (SpCol *,char,int *,SpCol *,char,int);

static void DivttLeft (SpCol *,char,int *,SpCol *,char,int);
static void DivttBoth (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
static void DivttRight (SpCol *,char,int *,SpCol *,char,int);

static int nsp_dichotomic_search(int x,const int val[],int imin,int imax);
static int nsp_bi_dichotomic_search_i(const int *x,int xpmin,int xpmax,const int *val,int imin,int imax,
				      NspMatrix *Work,NspMatrix *Index,int count);


static const double mpzero=-1/0.0;

/**
 * nsp_spmaxpcolmatrix_create:
 * @name: name of object 
 * @type: a character to code complex or real matrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a #NspSpMaxpColMatrix of size @mx@n with no stored data
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_create(char *name, char type, int m, int n)
{
  int i;
  NspSpMaxpColMatrix *Sp = new_spmaxpcolmatrix();
  if ( Sp == NULLSPMAXPCOL) 
    {
      Scierror("No more space\n");      return(NULLSPMAXPCOL);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(Sp),name) == NULL)
    return(NULLSPMAXPCOL);
  NSP_OBJECT(Sp)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
    Sp->type = SPMATRIX;
    Sp->ftype = Sp_Type;
  */
  Sp->m=m;
  Sp->n=n;
  /* Sp->mn = Sp->m*Sp->n; */
  Sp->rc_type=type;
  Sp->convert = 'n';
  Sp->triplet.Jc=NULL;
  Sp->triplet.Ir=NULL;
  Sp->triplet.Pr=NULL;
  Sp->triplet.Pi=NULL;
  if ( Sp->n == 0 ) 
    {
      Sp->D = NULL;
      return(Sp);
    }
  Sp->D = ( SpCol **) MALLOC( n*sizeof( SpCol *));
  if ( Sp->D == ( SpCol **) 0) 
    {
      Scierror("No More Space\n");
      return(NULLSPMAXPCOL);
    }
  for ( i = 0  ; i < Sp->n ; i++) 
    {
      Sp->D[i] = ( SpCol *) MALLOC( sizeof( SpCol));
      if ( Sp->D[i] == ( SpCol *) 0) 
	{
	  Scierror("No More Space\n");
	  return(NULLSPMAXPCOL);
	}
      Sp->D[i]->size = 0 ; Sp->D[i]->J = NULL; Sp->D[i]->R = NULL;
    }
  return(Sp);
}


/**
 * nsp_spmaxpcolmatrix_sparse:
 * @name: a string 
 * @RC: a  #NspMatrix
 * @Values: a #NspMatrix 
 * @m: an integer 
 * @n: an integer
 * 
 * Creates a #NspSpMaxpColMatrix of size @mx@n filed with values specified 
 * in @RC ((i,j) values stored in a two column matrix) and @Values 
 * ( A(@RC(k,1),@RC(k,2))= Values(k)).
 * The routine cumulate values when specific indices are repeated as in Matlab
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_sparse(char *name, NspMatrix *RC, NspMatrix *Values, int m, int n)
{
  double *rows, *cols;
  NspSpMaxpColMatrix *Loc = NULLSPMAXPCOL;
  Boolean do_ij_sort = FALSE;
  couple *ij=NULL;
  int *p=NULL, k,kf,first_k,kk,ind,i,imax,j,jmax, colsize, colsize_init;

  if ( (ij = malloc(RC->m*sizeof(couple))) == NULL || (p = malloc(RC->m*sizeof(int))) == NULL )
    {
      Scierror("Error:\tRunning out of memory\n"); goto err;
    }
 
  imax = 0; jmax = 0;
  rows = RC->R; cols = rows + RC->m;

  /* check indices then store in ij for lexical ordering */
  for ( k = 0 ; k < RC->m; k++ )
    {
      i = (int) rows[k];
      j = (int) cols[k];

      if ( i <= 0 || j <= 0  || rows[k] != (double) i  ||  cols[k] != (double) j )
	{
	  Scierror("Error:\t sparse(ij,val,..): invalid indice at row %d of ij\n",k+1);
	  goto err;
	}
      if ( i > imax) imax = i;
      if ( j > jmax) jmax = j;
      ij[k].i = i-1; ij[k].j = j-1;
      if ( k > 0 && !do_ij_sort && ( ij[k].j < ij[k-1].j || (ij[k].j == ij[k-1].j && ij[k].i < ij[k-1].i) ) )
	do_ij_sort = TRUE;
    }

  if ( m == -1 )
    m = imax;
  else if ( m < imax )
    {
      Scierror("Error:\t some row indices are > m=%d\n",m); goto err;
    }

  if ( n == -1 )
    n = jmax;
  else if ( n < jmax )
    {
      Scierror("Error:\t some column indices are > n=%d\n",n); goto err;
    }

  if ( do_ij_sort )
    {
      /* sort ij (a kind of lexical sort) */
      nsp_sqsort_bp_couple(ij, RC->m, p, 'i');
      for ( k = 0 ; k < RC->m ; k++ ) p[k]--;
    }
  else
    for ( k = 0 ; k < RC->m ; k++ ) p[k]=k;

  /* allocate space for Loc */
  if ( (Loc =nsp_spmaxpcolmatrix_create(name,Values->rc_type,m,n))== NULLSPMAXPCOL)
    goto err;

  if ( RC->m == 0 ) return Loc;

  /* now form the sparse matrix, column by column */
  j = ij[0].j;
  first_k = 0; k = 1 ;
  while ( k < RC->m )
    {
      if (ij[k].j != j  ||  k == RC->m-1 ) /* a column is ended, there are 2 cases */
	/*
	 *    1/ elem k is the first of a new column => column is [first_k:k-1]
	 *    2/ elem k is on the same column but is the last one => column [first_k:k]
	 */
	{
	  if ( ij[k].j != j ) kf = k-1;  /* case 1 */
	  else                kf = k;    /* case 2 */
	  colsize = colsize_init = kf - first_k + 1;
	  if ( nsp_spmaxpcolmatrix_resize_col(Loc,j,colsize_init) == FAIL ) goto err;
	  kk = first_k;
	  if ( Values->rc_type == 'r' ) /*** scan of the column - real case ***/
	    {
	      /* 1/ look for the first non zero coef of the column */
	      while ( kk <= kf  &&  Values->R[p[kk]] == 0.0 ) {colsize--; kk++;}
	      if ( colsize > 0 )
		{
		  /* 2/ init with the first non null coef (which can become null after summation in some case...) */
		  Loc->D[j]->R[0] = Values->R[p[kk]]; Loc->D[j]->J[0] = ij[kk].i;
		  ind = 0; kk++;
		  /* 3/ parse the rest of the column */
		  while ( kk <= kf )
		    {
		      if ( ij[kk].i == ij[kk-1].i )  /* same row index => add coef */
			{
			  Loc->D[j]->R[ind] += Values->R[p[kk]];
			  colsize--;
			}
		      else                       /* different row index => a new coef */
			{
			  if ( Loc->D[j]->R[ind] == 0.0 )  /* previous coef have been cancelled */
			    colsize--;
			  else
			    ind++;
			  Loc->D[j]->R[ind] = Values->R[p[kk]]; Loc->D[j]->J[ind] = ij[kk].i;
			}
		      kk++;
		    }
		  /* 4/ the last one coef of the column could have been cancelled too */
		  if ( Loc->D[j]->R[ind] == 0.0 ) colsize--;
		}
	    }
	  else                          /*** scan of the column - complex case ***/
	    {
	      /* 1/ look for the first non zero coef of the column */
	      while ( kk <= kf  &&  Values->C[p[kk]].r == 0.0  &&  Values->C[p[kk]].i == 0.0 ){colsize--; kk++;}
	      if ( colsize > 0 )
		{
		  /* 2/ init with the first non null coef (which can become null after summation in some case...) */
		  Loc->D[j]->C[0] = Values->C[p[kk]]; Loc->D[j]->J[0] = ij[kk].i;
		  ind = 0; kk++;
		  /* 3/ parse the rest of the column */
		  while ( kk <= kf )
		    {
		      if ( ij[kk].i == ij[kk-1].i ) /* same row index => add coef */
			{
			  Loc->D[j]->C[ind].r += Values->C[p[kk]].r; Loc->D[j]->C[ind].i += Values->C[p[kk]].i;
			  colsize--;
			}
		      else                      /* different row index => a new coef */
			{
			  if ( Loc->D[j]->C[ind].r == 0.0 &&  Loc->D[j]->C[ind].i == 0.0 )  /* previous coef have been cancelled */
			    colsize--;
			  else
			    ind++;
			  Loc->D[j]->C[ind] = Values->C[p[kk]]; Loc->D[j]->J[ind] = ij[kk].i;
			}
		      kk++;
		    }
		  /* 4/ the last one coef of the column could have been cancelled too */
		  if ( Loc->D[j]->C[ind].r == 0.0  &&  Loc->D[j]->C[ind].i == 0.0 )
		    colsize--;
		}
	    }
	  
	  if ( colsize < colsize_init ) /* due to zero coef or cancellation we have to resize */
	    if ( nsp_spmaxpcolmatrix_resize_col(Loc,j,colsize) == FAIL )
	      goto err;

	  first_k = kf+1;      /* index of the new column */
	  if ( first_k < RC->m) 
	    j = ij[first_k].j;   /* column number of the new column */
	}
      k++;
    }
  
  /* if the last column is formed of only one element it has not been inserted in the sparse matrix */
  k = RC->m-1;
  if ( first_k == k )
    {
      j = ij[k].j;
      if ( Values->rc_type == 'r' && Values->R[k] != 0.0 )
	{
	  if ( nsp_spmaxpcolmatrix_resize_col(Loc,j,1) == FAIL ) goto err;
	  Loc->D[j]->R[0] = Values->R[p[k]];
	  Loc->D[j]->J[0] = ij[k].i;
	}
      else if ( Values->rc_type == 'c' &&  (Values->C[k].r != 0.0 || Values->C[k].i != 0.0) )
	{
	  if ( nsp_spmaxpcolmatrix_resize_col(Loc,j,1) == FAIL ) goto err;
	  Loc->D[j]->C[0] = Values->C[p[k]];
	  Loc->D[j]->J[0] = ij[k].i;
	}
    }
     
  FREE(ij);
  FREE(p);
  return Loc;

 err:
  FREE(ij);
  FREE(p);
  nsp_spmaxpcolmatrix_destroy(Loc);
  return NULLSPMAXPCOL;
}


/**
 * nsp_spmaxpcolmatrix_get:
 * @A: a #NspSpMaxpColMatrix
 * @RC: 
 * @Values: 
 * 
 * get a @RC,@Values description of the sparse matrix @A
 * 
 * Return value: %OK or %FAIL
 **/
int nsp_spmaxpcolmatrix_get(NspSpMaxpColMatrix *A, NspMatrix **RC, NspMatrix **Values)
{
  int count=0,i,j,iw;
  for ( i = 0 ; i < A->n ; i++) 
    {
      count += A->D[i]->size ;
    }
  if ((*RC = nsp_matrix_create(NVOID,'r',count,2)) == NULLMAT ) return FAIL;
  if ((*Values = nsp_matrix_create(NVOID,A->rc_type,count,1)) == NULLMAT ) return FAIL;
  iw=0;
  for ( i = 0 ; i < A->n ; i++) 
    {
      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	{
	  /* Store (col=i+1,row= A->D[i]->J[j]+1) in RC(iw,:) **/
	  (*RC)->R[iw+(*RC)->m] = i + 1; 
	  (*RC)->R[iw ] = A->D[i]->J[j]+1;
	  /* Store Associated value **/
	  switch ( A->rc_type ) 
	    {
	    case 'r' : (*Values)->R[iw] = A->D[i]->R[j];break;
	    case 'c' : (*Values)->C[iw] = A->D[i]->C[j];break;
	    }
	  iw++;
	}
    }
  return OK;
}

/**
 * nsp_spmaxpcolmatrix_copy:
 * @A: a #NspSpMaxpColMatrix
 * 
 * returns a copy of sparse matrix @A.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_copy(NspSpMaxpColMatrix *A)
{
  int i;
  int inc=1;
  NspSpMaxpColMatrix *Sp;
  Sp =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->m,A->n);
  if ( Sp == NULLSPMAXPCOL ) return(NULLSPMAXPCOL) ; 
  for ( i = 0  ; i < Sp->n ; i++) 
    {
      if (nsp_spmaxpcolmatrix_resize_col(Sp,i,(int)A->D[i]->size) == FAIL) return(NULLSPMAXPCOL);
      nsp_icopy(&A->D[i]->size,A->D[i]->J,&inc,Sp->D[i]->J,&inc);
      if ( A->rc_type == 'r' ) 
	C2F(dcopy)(&A->D[i]->size,A->D[i]->R,&inc,Sp->D[i]->R,&inc);
      else 
	C2F(zcopy)(&A->D[i]->size,A->D[i]->C,&inc,Sp->D[i]->C,&inc);
    }
  return(Sp);
}


/**
 * nsp_spmaxpcolmatrix_resize_col:
 * @Sp: a #NspSpMaxpColMatrix
 * @i: column to be resized 
 * @n: number of non null elements of the column.
 * 
 * reallocates or allocates the #SpCol structure associated to column @i 
 * in such a way that it can contain @n non null elements. In case of 
 * reallocation the stored data is preserved.
 * 
 * WARNING : @i must be in the range [0,Sp->m[ and this is not checked here
 * WARNING : Sp->m is not changed this is to be done elsewhere.
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_resize_col(NspSpMaxpColMatrix *Sp, int i, int n)
{
  SpCol *Col;
  int cp = ( Sp->rc_type == 'c') ? 2 : 1;
  Col = Sp->D[i];
  if ( Col->size == 0 ) 
    {
      if ( n <= 0 ) return(OK);
      if ((Col->J =nsp_alloc_int((int) n)) == (int *) 0) return(FAIL);
      /* note that all data are in a union */
      if ((Col->R =nsp_alloc_doubles(n*cp)) == (double *) 0 ) return(FAIL);
      Col->size = n;
      return(OK);
    }
  if ( Col->size == n  ) return(OK);
  
  if ( n <= 0 ) 
    {
      /* empty new size **/
      FREE(Col->J);
      FREE(Col->R);
      Col->size = 0;
      return(OK);
    }
  if ((Col->J =nsp_realloc_int(Col->J, n))  == (int *) 0) return(FAIL);
  if (( Col->R =nsp_realloc_doubles(Col->R, n*cp)) == (double *) 0 ) return(FAIL);
  Col->size = n;
  return(OK);
}

/**
 * nsp_spmaxpcolmatrix_col_destroy:
 * @Col: a #SpCol object 
 * 
 * internal: destroys a #SpCol structure.
 * 
 **/

void nsp_spmaxpcolmatrix_col_destroy(SpCol *Col)
{
  if ( Col->size != 0 ) 
    {
      FREE( Col->J);
      FREE( Col->R);
    }
  FREE(Col);
}

/**
 * nsp_spmaxpcolmatrix_destroy:
 * @Mat: a #NspSpMaxpColMatrix
 * 
 * destroys a #NspSpMaxpColMatrix.
 * 
 **/

void nsp_spmaxpcolmatrix_destroy(NspSpMaxpColMatrix *Mat)
{
  int i;
  if ( Mat != NULLSPMAXPCOL )
    {
      nsp_spcol_free_triplet((NspSpColMatrix *) Mat);
      nsp_object_destroy_name(NSP_OBJECT(Mat));
      if ( Mat->D != NULL) 
	for ( i = 0  ; i < Mat->n ; i++) 
	  {
	    nsp_spmaxpcolmatrix_col_destroy(Mat->D[i]);
	  }
      FREE(Mat->D);
      FREE(Mat) ;
    }
}

/**
 * nsp_spmaxpcolmatrix_nnz:
 * @HMat: a #NspSpMaxpColMatrix
 * 
 * computes the number of non nul elements stored in 
 * a sparse Matrix.
 * 
 * Return value: the number of non nul elements.
 **/
int nsp_spmaxpcolmatrix_nnz(const NspSpMaxpColMatrix *HMat)
{
  int i, nnz=0;
  for ( i = 0 ; i < HMat->n ; i++ )
    nnz += HMat->D[i]->size;
  return nnz;
}


/**
 * nsp_spmaxpcolmatrix_info:
 * @Sp: a #NspSpMaxpColMatrix
 * @indent: an integer 
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * Display info on the sparse matrix @Sp using the default 
 * Sciprintf() function. 
 * @indent is the given indentation for printing.
 **/

int nsp_spmaxpcolmatrix_info(NspSpMaxpColMatrix *Sp, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Sp)->name;
  if ( Sp == NULLSPMAXPCOL) 
    {
      Sciprintf("Null SpMatrix pointer\n");
      return TRUE;
    }
  Sciprintf1(indent,"%s\t= [...]\t\tspmaxpcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
  return TRUE;
}


/**
 * nsp_spmaxpcolmatrix_print:
 * @Sp: 
 * @indent: 
 * @name: 
 * @rec_level: 
 * 
 * displays a sparse Matrix.
 **/

int nsp_spmaxpcolmatrix_print(NspSpMaxpColMatrix *Sp, int indent,char *name, int rec_level)
{ 
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Sp)->name; 
  if (user_pref.pr_as_read_syntax)
    {
      const int name_len=128;
      char epname[name_len];
      NspMatrix *RC,*Values;
      if ( nsp_spmaxpcolmatrix_get(Sp,&RC,&Values)== FAIL)
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
      Sciprintf1(indent,"%s=sparse(%s__rc,%s__val,[%d,%d]);\n",pname,pname,pname,Sp->m,Sp->n);
      Sciprintf1(indent,"clear('%s__rc','%s__val')\n",pname,pname);
    }
  else
    {
      if (Sp->m==0 || Sp->n==0 ) 
	{
	  Sciprintf1(indent,"%s\t= []\t\tspmaxpcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	}
      else
	{
	  nsp_num_formats fmt;
	  if ( user_pref.pr_depth  <= rec_level -1 ) 
	    {
	      Sciprintf1(indent,"%s\t= [...]\t\tspmaxpcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	      return rep;
	    }
	  nsp_init_pr_format (&fmt);
	  Sciprintf1(indent,"%s\t=\t\tspmaxpcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	  rep = nsp_spmaxpcolmatrix_print_internal(&fmt,Sp,indent+1);
	}
    }
  return rep;
}

/**
 * nsp_spmaxpcolmatrix_redim:
 * @A: a #NspSpMaxpColMatrix
 * @m: an integer 
 * @n: an integer
 * @inplace: should be TRUE if the operation should be done 
 *           "in place" (in this case the returned value is *A) 
 * If the product @mx@n is equal to @A->m x @A->n, 
 * returns a new sparse matrix of size @mx@n. The new 
 * matrix is filled with the values of @A assuming 
 * columnwize order. 
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_redim(NspSpMaxpColMatrix *A, int m, int n, Boolean inplace)
{
  int i,k;
  NspSpMaxpColMatrix *Loc;
  double mn = ((double) A->m)*((double) A->n);

  if ( m == -1 ) 
    {
      if ( n > 0 )
	{
	  double mm = floor( mn / (double) n);
	  if ( mm > INT_MAX )
	    {
	      Scierror("Error:\tCannot change size to (%gx%d) because of integer overflow\n",mm,n);
	      return NULLSPMAXPCOL;
	    }
	  m = (int) mm;
	}
      else
	m = 0;
    }
  else if ( n == -1 )
    {
      if ( m > 0 )
	{
	  double nn = floor( mn / (double) m);
	  if ( nn > INT_MAX )
	    {
	      Scierror("Error:\tCannot change size to (%dx%g) because of integer overflow\n",m,nn);
	      return NULLSPMAXPCOL;
	    }
	  n = (int) nn;
	}
      else
	n = 0;
    }

  if ( mn !=  ((double)m)*((double)n) )
    {
      Scierror("Error:\tCannot change size to (%dx%d) since matrix has %g elements\n",m, n, mn);
      return NULLSPMAXPCOL;
    }

  if ((Loc =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,m,n))== NULLSPMAXPCOL ) 
    return NULLSPMAXPCOL;
  /* initialize iw */
  for ( i= 0 ; i < Loc->n ; i++) 
    Loc->D[i]->iw= 0;
  /* counting elements in each new col of Loc */
  for ( i = 0 ; i < A->n ; i++ ) 
    {
      SpCol *Ri = A->D[i];
      for  ( k = 0 ;  k < Ri->size ; k++)
	{ 
	  /* ind is the one dimensional indice */
	  int ind= Ri->J[k]+i*A->m,i1;
	  /* the new column indice */
	  i1= ind / m;
	  Loc->D[i1]->iw++;
	}
    }
  /* Enlarge columns of Loc using computed sizes */
  for ( i= 0 ; i < Loc->n ; i++) 
    {
      if (nsp_spmaxpcolmatrix_resize_col(Loc,i, Loc->D[i]->iw)== FAIL) return NULLSPMAXPCOL;
    }
  /* Fill the columns  of the new matrix 
   */
  for ( i= 0 ; i < Loc->n ; i++) 
    Loc->D[i]->iw= 0;
  for ( i = 0 ; i < A->n ; i++ ) 
    {
      SpCol *Ri = A->D[i];
      for  ( k = 0 ;  k < Ri->size ; k++)
	{ 
	  int ind= Ri->J[k]+i*A->m,i1,j1;
	  /* the new row */
	  j1= ind % m;
	  /* the new column indice */
	  i1= ind / m;
	  Loc->D[i1]->J[Loc->D[i1]->iw] = j1;
	  switch (Loc->rc_type ) 
	    {
	    case 'r' : Loc->D[i1]->R[Loc->D[i1]->iw]= Ri->R[k];break;
	    case 'c' : Loc->D[i1]->C[Loc->D[i1]->iw]= Ri->C[k];break;
	    }
	  Loc->D[i1]->iw++;
	}
    }

  if ( inplace )
    {
      /* destroy A sub-objects */
      for ( k = 0 ; k < A->n ; k++ )
	nsp_spmaxpcolmatrix_col_destroy(A->D[k]);
      free(A->D);
      A->m = Loc->m; A->n = Loc->n;
      A->D = Loc->D;
      free(Loc);
      return A;
    }
  else
    return Loc;
}

/**
 * nsp_spmaxpcolmatrix_enlarge_cols:
 * @Sp: a #NspSpMaxpColMatrix
 * @n: an integer 
 * 
 * changes the number of columns of @Sp to Min(@Sp->n,@n);
 *
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_enlarge_cols(NspSpMaxpColMatrix *Sp, int n)
{
  int i;
  if ( Sp->n >= n ) return(OK);
  Sp->D = ( SpCol **) REALLOC(Sp->D, n*sizeof( SpCol *));
  if ( Sp->D == ( SpCol **) 0) 
    {
      Scierror("No More Space\n");
      return(FAIL);
    }
  for ( i = Sp->n ; i < n ; i++) 
    {
      Sp->D[i] = ( SpCol *) MALLOC( sizeof( SpCol));
      if ( Sp->D[i] == ( SpCol *) 0) 
	{
	  Scierror("No More Space\n");
	  return(FAIL);
	}
      Sp->D[i]->size = 0 ;
    }
  Sp->n = n;
  /* Sp->mn = Sp->n*Sp->m; */
  return(OK);
}


/**
 * nsp_spmaxpcolmatrix_enlarge:
 * @A: a #NspSpMaxpColMatrix
 * @m: an integer 
 * @n: an integer
 * 
 * changes @A to [@A,0;0,0]  
 * in such a way that the new size of @A is (max(A->m,m) x max(A->n,n));
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_enlarge(NspSpMaxpColMatrix *A, int m, int n)
{
  /* special case **/
  if ( m > A->m  ) {A->m = m ;/* A->mn=m*A->n;*/} /* easy for sparse matrix **/
  if ( n > A->n  ) 
    return nsp_spmaxpcolmatrix_enlarge_cols(A,n);
  return OK;
}


/**
 * nsp_spmaxpcolmatrix_concatr:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * A = [A, B] 
 * Right concatenation on A, A is changed 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_concatr(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{ 
  int Am,inc=1;
  int i;
  if ( A->m != B->m ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(FAIL);
    }
  Am = A->n;
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_spmaxpcolmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if (nsp_spmaxpcolmatrix_enlarge_cols(A,A->n+B->n) == FAIL ) { return(FAIL) ; } ; 
  for ( i = Am ; i < A->n ; i++) 
    { 
      SpCol *Ai = A->D[i];
      SpCol *Bi = B->D[i-Am];
      if (nsp_spmaxpcolmatrix_resize_col(A,i,(int) Bi->size) == FAIL) return(FAIL) ;
      nsp_icopy(&Bi->size,Bi->J,&inc,Ai->J,&inc);
      if ( B->rc_type == 'r' ) 
	{
	  if ( A->rc_type == 'r') 
	    C2F(dcopy)(&Bi->size,Bi->R,&inc,Ai->R,&inc);
	  else 
	    nsp_dzcopy(&Bi->size,Bi->R,&inc,Ai->C,&inc);
	}
      else 
	{
	  /* in that case A is complex due to complexify */
	  C2F(zcopy)(&Bi->size,Bi->C,&inc,Ai->C,&inc);
	}
    }
  return(OK);
}


/**
 * nsp_spmaxpcolmatrix_concatd:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 *  A=[A; B ]  Down concatenation on A 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_concatd(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  int i;
  if ( A->rc_type == 'r' &&  B->rc_type == 'c' )  
    {
      if (nsp_spmaxpcolmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if ( A->n != B->n ) 
    {
      Scierror("Sparse Concat : incompatible size  \n");
      return(FAIL);
    }
  /* We enlarge the cols of A to store the non-null values comming from B*/
  for ( i = 0 ; i < A->n ; i++) 
    { 
      if (nsp_spmaxpcolmatrix_resize_col(A,i,((int) (A->D[i]->size+B->D[i]->size))) == FAIL)return(FAIL);
    } 

  for ( i = 0 ; i < A->n ;  i++ ) 
    { 
      int inc = 1;
      SpCol *Ai = A->D[i];
      SpCol *Bi = B->D[i];
      /* SpresizeRow has changed the Ai->size */ 
      int ioffset = Ai->size-Bi->size;
      nsp_icopy(&Bi->size,Bi->J,&inc,Ai->J+ioffset,&inc);
      /* must add A->m to the inserted column values **/
      nsp_iadd(&Bi->size,&A->m,Ai->J+ioffset,&inc);
      if ( B->rc_type == 'r' ) 
	{
	  if ( A->rc_type == 'c' ) 
	    nsp_dzcopy(&Bi->size,Bi->R,&inc,Ai->C+ioffset,&inc);
	  else 
	    C2F(dcopy)(&Bi->size,Bi->R,&inc,Ai->R+ioffset,&inc);
	}
      else 
	{
	  /* in that case A is complex due to complexify */
	  C2F(zcopy)(&Bi->size,Bi->C,&inc,Ai->C+ioffset,&inc);
	}
    }
  A->m += B->m;
  /* A->mn = A->m*A->n; */
  return(OK);
}


/**
 * nsp_spmaxpcolmatrix_concatdiag:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * Diag Concatenation A = [A,0;0,B] 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_concatdiag(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  int i,j;
  int Am = A->m;
  int An = A->n;
  int Bm = B->m;
  /* first [A,B] */
  int n1 = Max(A->m,B->m);
  A->m = n1;
  B->m = n1;
  if (nsp_spmaxpcolmatrix_concatr( A,B) == FAIL) 
    {
      B->m = Bm;
      return FAIL;
    }
  /* push last cols **/
  for ( i = An ; i < A->n ; i++) 
    { 
      SpCol *Ai = A->D[i];
      for ( j = 0 ; j < Ai->size ; j++) 
	Ai->J[j] += Am;
    }
  /* restore proper row dimensions **/
  A->m = Am + B->m ;
  /* A->mn = A->m*A->n; */
  B->m = Bm;
  return(OK);
  
}

/* 
 * WARNING: take care of the fact that r and c refers to 
 * column and row in next functions !!
 */

/**
 * nsp_spmaxpcolmatrix_store:
 * @A: a #NspSpMaxpColMatrix
 * @r: 
 * @c: 
 * @col: 
 * @B: a #NspSpMaxpColMatrix
 * @r1: 
 * @c1: 
 * 
 * 
 * utility 
 **/

void  nsp_spmaxpcolmatrix_store(NspSpMaxpColMatrix *A, int r, int c, int col, NspSpMaxpColMatrix *B, int r1, int c1)
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


/**
 * nsp_spmaxpcolmatrix_insert_elt:
 * @A: a #NspSpMaxpColMatrix
 * @i: an integer 
 * @j: an integer 
 * @B: a #NspSpMaxpColMatrix
 * @rb: an intege
 * @cb: an intege
 * 
 * Insert or change A(i,j) to B(rb,cb)
 * 
 * Returns: 
 **/

int nsp_spmaxpcolmatrix_insert_elt(NspSpMaxpColMatrix *A, int i, int j, NspSpMaxpColMatrix *B, int rb, int cb)
{
  SpCol *Ai = A->D[i];
  int ok = -1,insert=0,k;
  for ( k =0 ; k < Ai->size ; k++ )
    {
      if ( j == Ai->J[k] ) { ok = k; break ;}
      if ( j <  Ai->J[k] ) { insert= k; break ;}
    }
  if ( ok == -1 ) 
    {
      /* we must enlarge the Row **/
      if (nsp_spmaxpcolmatrix_resize_col(A,i,Ai->size+1) == FAIL) return FAIL;
      /* Get the new row **/
      Ai= A->D[i];
      /* we insert the new value **/
      if ( insert == 0) 
	{
	  /* insert at the end **/
	  nsp_spmaxpcolmatrix_store(A,i,Ai->size-1,j,B,rb,cb);
	}
      else
	{
	  int k1;
	  /* insert at position insert **/
	  /* move right one step **/
	  for ( k1 = Ai->size -2 ; k1 >= insert ; k1--) 
	    {
	      nsp_spmaxpcolmatrix_store(A,i,k1+1,Ai->J[k1],A,i,k1);
	    }
	  nsp_spmaxpcolmatrix_store(A,i,insert,j,B,rb,cb);
	}
    }
  else
    {
      nsp_spmaxpcolmatrix_store(A,i,ok,j,B,rb,cb);
    }
  return OK;
}

/**
 * nsp_spmaxpcolmatrix_delete_elt:
 * @A: a #NspSpMaxpColMatrix
 * @row: an integer 
 * @col: an integer 
 * @amin: an integer 
 * @amax: an integer 
 * 
 *  Remove element A(i,j) where i=@row and j=@col but the associated row of the matrix 
 *  is not resized. The function returns -1 if @A(i,j) was zero before the call (i.e no change )
 *  return  k  if @A(i,j) is removed and was the k-th non null element of i-th row of @A 
 *  @A(i,j) is searched in the i-th row of A but only in the range [@amin,@amax].
 * 
 * Return value: the row indice of the deleted element or -1 if the element was already a 
 * null value.
 **/

int nsp_spmaxpcolmatrix_delete_elt(NspSpMaxpColMatrix *A, int row, int col, int amin, int amax)
{
  int acol,ok1=0,k1;
  /* search if corresponding element exists 
   * FIXME: should make a dichotomic search here !!! 
   */

  for ( acol = amin ; acol < amax ; acol++) 
    {
      if ( col == A->D[row]->J[acol] ) { ok1 = 1; /* col1 = acol;*/ break;}
      if ( col < A->D[row]->J[acol] )  { break;}
    }
  /* perform deletion if necessary  **/
  if (  ok1 == 1) 
    {
      /* delete A(row,col) **/  
      /* move left one step **/
      for ( k1 = acol+1 ; k1 < amax ; k1++) 
	{
	  nsp_spmaxpcolmatrix_store(A,row,k1-1,A->D[row]->J[k1],A,row,k1);
	}
      return acol;
    }
  else
    {
      return -1;
    }
}

/**
 * nsp_spmaxpcolmatrix_get_elt:
 * @B: a #NspSpMaxpColMatrix
 * @i: an integer 
 * @j: an integer 
 * 
 * return k such that B->D[i]->J[k] = j or -1 if such k does not exists 
 * 
 * 
 * Return value: the row indice in the B->D[i]->J array of the searched element 
 * or -1 if (i,j) is not stored in B->D[i]->J.
 **/


int nsp_spmaxpcolmatrix_get_elt(NspSpMaxpColMatrix *B, int i, int j)
{
  int ok = -1,k;
  SpCol *Bi;
  if ( i >= B->n && j == 0) 
    {
      /* B is a vector we want the (i,j) elt in a vector */
      j=i;
      i=0;
    }
  Bi = B->D[i];
  for ( k =0 ; k < Bi->size ; k++ )
    {
      if ( j == Bi->J[k] ) { ok = k; break ;}
      if ( j <  Bi->J[k] ) {  break ;}
    }
  return ok;
}



/**
 * nsp_spmaxpcolmatrix_set_rowcol:
 * @A: a #NspSpMaxpColMatrix
 * @Rows: a #NspMatrix 
 * @Cols: a #NspMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked between 
 *  A and B 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_set_rowcol(NspSpMaxpColMatrix *A, NspObject *Rows, NspObject *Cols, NspSpMaxpColMatrix *B)
{
  /* int Bnonnul = FALSE; */
  /* char type ='r';*/
  int i,k,l,acol;

  index_vector index_c={0}, index_r={0};
  index_r.iwork = matint_iwork1;
  index_c.iwork = matint_iwork2;

  if ( nsp_get_index_vector_from_object(Cols,&index_c) == FAIL) goto fail;
  if ( nsp_get_index_vector_from_object(Rows,&index_r) == FAIL) goto fail;
  
  /* Check compatibility : B is a scalar or B must have compatible 
   * size with Rows and Cols : Note that B=[] is treated elsewhere 
   */
  
  if ( B->m != 1 || B->n != 1)
    {
      if ( index_r.nval != B->m || index_c.nval != B->n )
	{
	  Scierror("Error:\tIncompatible dimensions\n");
	  goto fail;
	}
    }
  else
    {
      /* B is a scalar check if it is a null scalar **/
      /* if ( B->D[0]->size !=0) Bnonnul = TRUE; */
    }
  
  if ( index_r.min < 1 || index_c.min < 1 ) 
    {
      Scierror("Error:\tNegative indices are not allowed\n");
      goto fail;
    }
  /* Enlarge A if necessary */
  if ( index_r.max > A->m ||  index_c.max > A->n ) 
    if (nsp_spmaxpcolmatrix_enlarge(A,index_r.max,index_c.max) == FAIL) goto fail;
  /* Id result complex ? */
  if ( B->rc_type == 'c' &&   A->rc_type == 'r' )
    { 
      /* type = 'c'; */
      if (nsp_spmaxpcolmatrix_seti(A,0.00) == FAIL ) goto fail;
    }
  /* fill A */
  for ( i = 0 ; i < index_c.nval ; i++ ) 
    {
      /* The col of A to be changed **/
      int col = index_c.val[i];
      int Ais = A->D[col]->size ;
      int amin = 0 ;
      int amax = Ais;
      int ib,nel ;
      if ( B->m == 1 && B->n == 1)
	{
	  ib = 0;
	  nel = index_r.nval ;
	}
      else
	{
	  ib =i;
	  nel = B->D[i]->size ;
	}
      /* The new col will have at most nel + Ais non nul elements **/
      if (nsp_spmaxpcolmatrix_resize_col(A,col ,nel+Ais) == FAIL) goto fail;
      for ( k =0 ; k < index_r.nval ; k++ )
	{
	  int ok = -1;
	  int row = index_r.val[k];
	  int ok1,k1,kb;
	  kb = ( B->m == 1 && B->n == 1) ? 0 : k; /* if B is scalar **/
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
		  if ( row == A->D[col]->J[acol] ) { ok1 = 1; /* col1 = acol;*/ break;}
		  if ( row < A->D[col]->J[acol] )  { ok1 = 2; /* col1 = acol;*/ break;}
		}
	      /* perform insertion **/
	      switch ( ok1 ) 
		{
		case 1: /* replace A(row,col) **/  
		  nsp_spmaxpcolmatrix_store(A,col,acol,row, B,ib,ok);
		  /*Cols is not supposed to be increasing  amin = acol; */
		  break;
		case 2: /* insert before acol **/
		  /* move right one step */
		  for ( k1 = amax -1 ; k1 >= acol ; k1--) 
		    {
		      nsp_spmaxpcolmatrix_store(A,col,k1+1,A->D[col]->J[k1],A,col,k1);
		    }
		  nsp_spmaxpcolmatrix_store(A,col,acol,row,B,ib,ok);
		  /*Cols is not supposed to be increasing amin = acol ; */
		  amax++ ; break ;
		default : 
		  /* insert at end **/
		  nsp_spmaxpcolmatrix_store(A,col,amax,row,B,ib,ok);
		  /* Cols is not supposed to be increasing  amin = amax ; **/
		  amax++;
		}
	    }
	  else 
	    {
	      /* we must set the [row,col] element of A to 0.0 **/
	      ok1 =nsp_spmaxpcolmatrix_delete_elt(A,col,row,amin,amax);
	      if ( ok1 != -1 ) 
		{
		  /* Cols is not supposed to be increasing  amin = ok1; **/
		  amax--;
		}
	    }
	}
      /* we resize A(row,:) to its correct size **/
      if (nsp_spmaxpcolmatrix_resize_col(A,col ,amax) == FAIL) goto fail;
    }
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  return(OK);

 fail: 
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  return FAIL;

}

/**
 * nsp_spmaxpcol_test_in_place_assign_OK:
 * @A: a #NspSpMaxpColMatrix
 * @jA: an int 
 * @index_r: an #index_vector
 * @q: an array of int
 *
 *  Test if the indices in index_r->val (which have been sorted previously so are in increasing order)
 *  are all in the array Col->J (Col is the jA th column of the matrix A). If yes return TRUE which 
 *  means that an in place insertion can be done. Moreover the corresponding indices of Col->J 
 *  (indices k such that Col->J[k] = index_r->val[i] are recorded in the array q (its purpose is to 
 *  do fastly the in place insertion when possible). 
 *  This is an utility function for sparse assignment (A(i,j) = B). 
 *
 * Return value:  %TRUE or %FALSE
 **/

Boolean nsp_spmaxpcolmatrix_test_in_place_assign_OK(NspSpMaxpColMatrix *A, int jA, index_vector *index_r, int *q)
{
  SpCol *Col = A->D[jA];
  int j, k = nsp_spmaxpcolmatrix_locate(Col, index_r->val[0]);

  if ( k == - 1 )
    return FALSE;
  else
    {
      q[0] = k;
      for ( j = 1 ; j < index_r->nval ; j++ )
	{
	  /* get k such that Col->J[k] <= index_r->val[j] */
	  while ( k < Col->size && index_r->val[j] > Col->J[k] )
	    k++;
	  if ( k >= Col->size || index_r->val[j] != Col->J[k] )
	    return FALSE;
	  else
	    q[j] = k;
	}
      return TRUE;
    }
}

/**
 * void nsp_spmaxpcolmatrix_clean_zeros:
 * @Col: a #SpMaxpCol
 * @type: type (real/complex) of SpCol Col
 *
 *  suppress all 0 elements (move all non zeros elements accordingly) 
 *  and free the corresponding memory 
 **/

void nsp_spmaxpcolmatrix_clean_zeros(SpCol *Col, char type)
{
  int k, kp=0, it= type=='r' ? 1 : 2;
  if ( type == 'r' )
    {
      for ( k = 0 ; k < Col->size ; k++ )
	if ( Col->R[k] != 0.0 )
	  {
	    if ( kp != k )
	      {
		Col->R[kp] = Col->R[k];
		Col->J[kp] = Col->J[k];
	      }
	    kp++;
	  }
    }
  else
    {
      for ( k = 0 ; k < Col->size ; k++ )
	if ( Col->C[k].r != 0.0 &&  Col->C[k].i != 0.0 )
	  {
	    if ( kp != k )
	      {
		Col->C[kp] = Col->C[k]; 
		Col->J[kp] = Col->J[k];
	      }
	    kp++;
	  }
    }

  if ( kp > 0 )
    {
      Col->J = REALLOC(Col->J, kp*sizeof(int));
      Col->R = REALLOC(Col->R, it*kp*sizeof(double));
    }
  else
    {
      if ( Col->size != 0) 
	{
	  FREE(Col->J); FREE(Col->R);
	}
    }
  Col->size = kp;
}
 
/**
 * nsp_spmaxpcol_in_place_assign:
 * @A: a #NspSpMaxpColMatrix
 * @jA: an int (column number)
 * @index_r: an #index_vector
 * @q: an array of int of size index_r->nval such that Col->J[q[k]] = index_r[k]
 * @p: an array of int of size index_r->nval such that index_r[k] corresponds to the element B(p[k],j)
 *     (this comes from a previous increasing sort onto the array index_r)
 * @B: a #NspMatrix
 * @jB: an int the column of B which is involved here.
 *
 * 
 * Return value:  %TRUE or %FALSE
 **/
	
void nsp_spmaxpcolmatrix_in_place_assign(NspSpMaxpColMatrix *A, int jA, index_vector *index_r, int *q, int *p, NspMatrix *B, int jB)
{
  SpCol *Col = A->D[jA];
  int k;  
  Boolean do_clean = FALSE;

  if ( B->m == 1 && B->n == 1 )
    {
      if ( A->rc_type == 'c' )
	{
	  doubleC Bval;
	  if ( B->rc_type == 'c' )
	    {
	      Bval = B->C[0]; if ( Bval.r == 0.0 && Bval.i == 0.0 ) do_clean = TRUE;
	    }
	  else
	    {
	      Bval.r = B->R[0]; Bval.i = 0.0; if ( Bval.r == 0.0 ) do_clean = TRUE;
	    }
	  for ( k = 0 ; k < index_r->nval ; k++ )
	    Col->C[q[k]] = Bval;
	}
      else
	{
	  double Bval = B->R[0];
	  if ( Bval == 0.0 ) do_clean = TRUE;
	  for ( k = 0 ; k < index_r->nval ; k++ )
	    Col->R[q[k]] = Bval;
	}
    }
  else
    {
      int base = jB*B->m;
      if ( A->rc_type == 'r' )
	{
	  double Bval;
	  for ( k = 0 ; k < index_r->nval ; k++ )
	    {
	      Bval =  B->R[base + p[k]];
	      if ( Bval == 0.0 ) do_clean = TRUE;
	      Col->R[q[k]] = Bval;
	    }
	}
      else if ( B->rc_type == 'r' )
	{
	  double Bval;
	  for ( k = 0 ; k < index_r->nval ; k++ )
	    {
	      Bval =  B->R[base + p[k]];
	      if ( Bval == 0.0 ) do_clean = TRUE;
	      Col->C[q[k]].r = Bval;  Col->C[q[k]].i = 0.0;
	    }
	}
      else
	{
	  doubleC Bval;
	  for ( k = 0 ; k < index_r->nval ; k++ )
	    {
	      Bval =  B->C[base + p[k]];
	      if ( Bval.r == 0.0 && Bval.i == 0.0 ) do_clean = TRUE;
	      Col->C[q[k]] = Bval;
	    }
	}
    }

  if ( do_clean )
    nsp_spmaxpcolmatrix_clean_zeros(Col, A->rc_type);
}

typedef enum { real_scalar, cmplx_scalar, real_to_real, real_to_cmplx, cmplx_to_cmplx } assign_type;
typedef struct _nsp_sparse_assign nsp_sparse_assign;

struct _nsp_sparse_assign {
  Boolean first_call;
  assign_type assign; 
  double Bval;
  doubleC BvalC;
  int base;
};

static void assign_val(SpCol *ColNew, int kCn, char type,  NspMatrix *B, int iB, int jB, 
		       Boolean *do_clean, nsp_sparse_assign *asv )
{
  if (asv->first_call )
    {
      asv->assign= real_scalar;
      asv->Bval=0.0;
      asv->BvalC.r =       asv->BvalC.i = 0;
      asv->base=0;
      if ( B->m == 1 && B->n == 1 ) /* scalar case */
	{
	  if (type == 'r')
	    {
	      asv->Bval = B->R[0]; asv->assign = real_scalar; 
	      if ( asv->Bval == 0.0 ) *do_clean = TRUE;
	    }
	  else if ( B->rc_type == 'c' )
	    {
	      asv->BvalC = B->C[0]; asv->assign = cmplx_scalar; 
	      if ( asv->BvalC.r == 0.0 && asv->BvalC.i == 0.0 ) *do_clean = TRUE;
	    } 
	  else
	    {
	      asv->BvalC.r = B->R[0]; asv->BvalC.i = 0.0; asv->assign = cmplx_scalar; 
	      if ( asv->BvalC.r == 0.0 ) *do_clean = TRUE;
	    }
	}
      else   
	{
	  asv->base = jB*B->m;  /* init base adr of first element of the jB th column of B */
	  asv->assign = ( type == 'r' ) ?  real_to_real: 
	    (( B->rc_type == 'r' ) ?  real_to_cmplx: cmplx_to_cmplx);
	}
      asv->first_call = FALSE;
    }
  
  switch ( asv->assign )
    {
    case (real_scalar):
      ColNew->R[kCn] = asv->Bval; break;
    case (cmplx_scalar):
      ColNew->C[kCn] = asv->BvalC; break;
    case (real_to_real):
      ColNew->R[kCn] = B->R[asv->base + iB];
      if ( ColNew->R[kCn] == 0.0 ) *do_clean = TRUE; 
      break;
    case ( real_to_cmplx ):
      ColNew->C[kCn].r = B->R[asv->base + iB]; ColNew->C[kCn].i = 0.0;
      if ( ColNew->C[kCn].r == 0.0 ) *do_clean = TRUE;
      break;
    case (cmplx_to_cmplx):
      ColNew->C[kCn] = B->C[asv->base + iB];
      if ( ColNew->C[kCn].r == 0.0  &&  ColNew->C[kCn].i == 0.0 ) *do_clean = TRUE;
      break;
    }
}

/**
 * nsp_spmaxpcolmatrix_assign_by_merge:
 * @A: a #NspSpMaxpColMatrix
 * @jA: an int (column number) 
 * @index_r: an #index_vector
 * @p: an array of int of size index_r->nval such that index_r[k] corresponds to the element B(p[k],j)
 *     (this comes from a previous increasing sort onto the array index_r)
 * @B: a #NspMatrix
 * @jB: an int the column of B which is involved here.
 * @type: the type (real or complex) of Col
 *
 * 
 * Return value:  %OK or %FAIL
 **/
	
int nsp_spmaxpcolmatrix_assign_by_merge(NspSpMaxpColMatrix *A, int jA, index_vector *index_r, int *p, NspMatrix *B, int jB)
{
  SpCol *Col=A->D[jA], *ColNew=NULL;
  int kC=0, kCn=0, kB=0, size_max = index_r->nval + Col->size, ib = A->rc_type == 'r' ? 1 : 2;
  Boolean do_clean = FALSE;
  nsp_sparse_assign asv ={TRUE, real_scalar,0.0,{0,0},0};
  
  if ( (ColNew = (SpCol *) MALLOC( sizeof(SpCol))) == NULL )
    return FAIL;
  ColNew->J = NULL; ColNew->R = NULL; 
  if ( (ColNew->J = (int *) MALLOC( size_max*sizeof(int))) == NULL )
    goto fail;
  if ( (ColNew->R = (double *) MALLOC( ib*size_max*sizeof(double))) == NULL )
    goto fail;
  ColNew->size = size_max;

  while ( kC < Col->size && kB < index_r->nval )
    {
      if ( Col->J[kC] < index_r->val[kB] )
	{
	  ColNew->J[kCn] = Col->J[kC];
	  if ( A->rc_type == 'r' ) 
	    ColNew->R[kCn] = Col->R[kC];
	  else 
	    ColNew->C[kCn] = Col->C[kC];
	  kC++; 
	}
      else
	{
	  ColNew->J[kCn] = index_r->val[kB];
	  assign_val(ColNew, kCn, A->rc_type, B, p[kB], jB, &do_clean, &asv);
	  if ( Col->J[kC] == index_r->val[kB] ) kC++;
	  kB++;
	} 
      kCn++;
    }

  while ( kC < Col->size )
    {
      ColNew->J[kCn] = Col->J[kC];
      if ( A->rc_type == 'r' ) 
	ColNew->R[kCn] = Col->R[kC];
      else 
	ColNew->C[kCn] = Col->C[kC];
      kCn++; kC++;
    }

  while ( kB < index_r->nval )
    {
      ColNew->J[kCn] = index_r->val[kB];
      assign_val(ColNew, kCn, A->rc_type, B, p[kB], jB, &do_clean, &asv);
      kB++; kCn++;
    }

  if ( Col->size != 0) 
    {
      FREE(Col->R); FREE(Col->J); FREE(Col);
    }

  A->D[jA] = ColNew;

  if ( do_clean )   /* some zeros have been inserted */
    {
      ColNew->size = kCn;   /* avoid to test unused unitialized last elements (at pos kCn,...,size_max-1) */
      nsp_spmaxpcolmatrix_clean_zeros(ColNew, A->rc_type);
    }
  else if ( kCn < size_max )  /* free unused memery */
    nsp_spmaxpcolmatrix_resize_col(A, jA, kCn);

  return OK;

 fail:
  nsp_spmaxpcolmatrix_col_destroy(ColNew);
  return FAIL;
}

/**
 * nsp_spmaxpcolmatrix_set_rowcol_from_full:
 * @A: a #NspSpMaxpColMatrix
 * @Rows: a #NspMatrix 
 * @Cols: a #NspMatrix
 * @B: a #NspMatrix
 * 
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked between 
 *  A and B 
 *
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_set_rowcol_from_full(NspSpMaxpColMatrix *A, NspObject *Rows, NspObject *Cols, NspMatrix *B)
{
  int *p=NULL,*q=NULL, j,k;
  index_vector index_c={0}, index_r={0};
  index_r.iwork = matint_iwork1;
  index_c.iwork = matint_iwork2;

  if ( nsp_get_index_vector_from_object(Cols,&index_c) == FAIL ) goto fail;
  if ( nsp_get_index_vector_from_object(Rows,&index_r) == FAIL ) goto fail;
  if ( (p = nsp_alloc_work_int(index_r.nval)) == NULL ) goto fail;
  if ( (q = nsp_alloc_work_int(index_r.nval)) == NULL ) goto fail;

  /* Check compatibility : B is a scalar or B must have compatible 
   * size with Rows and Cols : Note that B=[] is treated elsewhere 
   */
  if ( B->m != 1 || B->n != 1)
    {
      if ( index_r.nval != B->m || index_c.nval != B->n )
	{
	  Scierror("Error:\tIncompatible dimensions\n");
	  goto fail;
	}
    }
  
  if ( index_r.min < 1 || index_c.min < 1 ) 
    {
      Scierror("Error:\tNegative indices are not allowed\n");
      goto fail;
    }

  /* Enlarge A if necessary */
  if ( index_r.max > A->m || index_c.max > A->n ) 
    if (nsp_spmaxpcolmatrix_enlarge(A, index_r.max, index_c.max) == FAIL) 
      goto fail;

  /* Is result complex ? */
  if ( B->rc_type == 'c' && A->rc_type == 'r' )
    if ( nsp_spmaxpcolmatrix_seti(A,0.00) == FAIL ) 
      goto fail;

  /* sort index_r.val */
  if ( index_r.flag ) /* index vector is of the form [k,k+1,k+2,....] so already sorted */
    for ( k = 0 ; k < index_r.nval ; k++ )  /* just set p to identity permutation */ 
      p[k] = k;
  else                /* call sorting routine */
    {
      nsp_sqsort_bp_int(index_r.val, index_r.nval, p, 'i');
      for ( k = 0 ; k < index_r.nval ; k++ ) p[k]--; /* sort routine provides 1-based indices so get 0-based */
    }

  /* make assignment column by column */
  for ( j = 0 ; j < index_c.nval ; j++ ) 
    {
      int jA = index_c.val[j];
      if ( nsp_spmaxpcolmatrix_test_in_place_assign_OK(A, jA, &index_r, q) )
	nsp_spmaxpcolmatrix_in_place_assign(A, jA, &index_r, q, p, B, j);
      else
	if ( nsp_spmaxpcolmatrix_assign_by_merge(A, jA, &index_r, p, B, j) == FAIL ) goto fail;
    }

  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  FREE(p); FREE(q);
  return OK;

 fail: 
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  FREE(p); FREE(q);
  return FAIL;

}

/**
 * nsp_spmaxpcolmatrix_set_row:
 * @A: a #NspSpMaxpColMatrix
 * @Inds: a #NspMatrix 
 * @B: a #NspSpMaxpColMatrix
 * 
 * performs A(Inds) = B. @A is changed and enlarged if necessary 
 * @Inds is unchanged.  Size Compatibility is checked with the following
 * rules: 
 * if A is a Matrix or A ==[] then @B must be row or column 
 * if A==[] the size of the result depends on @B.
 * If A is a row vector then B must be row 
 * If A is a column vector then B must be column 
 * @Inds must be in the range of @A indices unless @A is row or column or []
 * @Inds and @B must have the same size or @B must be scalar 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_set_row(NspSpMaxpColMatrix *A, NspObject *Inds, NspSpMaxpColMatrix *B)
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
		     B,B->m,B->n,B->m*B->n,(F_Enlarge)nsp_spmaxpcolmatrix_enlarge,&Bscal)== FAIL) 
    goto fail;
  /* */
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_spmaxpcolmatrix_complexify(A) == FAIL ) goto fail;
    }
  if ( Bscal == 0 ) 
    {
      for ( i = 0  ; i < index.nval ; i++) 
	{
	  int rb,cb,kb,ia,ra,ca ;
	  rb= i % ((NspSMatrix *) Inds)->m;
	  cb= (i - rb )/((NspSMatrix *) Inds)->m;
	  kb =nsp_spmaxpcolmatrix_get_elt(B,cb,rb);
	  ia = index.val[i];
	  ra = ia % A->m;
	  ca= (ia - ra )/A->m;
	  if ( kb == -1 ) 
	    {
	      /* we must set the [ra,ca] element of A to 0.0 **/
	      int ok1 =nsp_spmaxpcolmatrix_delete_elt(A,ca,ra,0,A->D[ca]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_spmaxpcolmatrix_resize_col(A,ca,A->D[ca]->size-1) == FAIL) goto fail;
		}
	    }
	  else
	    {
	      /* must change or insert element in A */
	      /* take care of B column */
	      if ( cb >= B->n ) cb=0;
	      if (nsp_spmaxpcolmatrix_insert_elt(A,ca,ra,B,cb,kb)== FAIL) goto fail;
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
	      ok1 =nsp_spmaxpcolmatrix_delete_elt(A,ca,ra,0,A->D[ca]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_spmaxpcolmatrix_resize_col(A,ca,A->D[ca]->size-1) == FAIL) goto fail;
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
	      if (nsp_spmaxpcolmatrix_insert_elt(A,ca,ra,B,0,0)== FAIL) goto fail;
	    }
	}
    }

  nsp_free_index_vector_cache(&index);
  return OK;

 fail:
  nsp_free_index_vector_cache(&index);
  return FAIL;

}

/*generic function which check sizes and enlarge A if necessary 
 * should be moved in Sparse where it is still used. 
 */

int MaxpGenericMatSeRo(void *A, int Am, int An, int Amn,   index_vector *index,
		   void *B, int Bm, int Bn, int Bmn, F_Enlarge F, int *Bscal)
{
  if ( index->min < 1 ) 
    {
      Scierror("Error:\tNegative indices are not allowed\n");
      return FAIL;
    }
  if ( Bm != 1 && Bn != 1) 
    {
      Scierror("Error:\tA(ind)=B, B must be a vector");
      return FAIL;
    }
  if ( Am == 1  && Bm != 1) 
    {
      Scierror("Error:\tA(ind)=B, B must be row when A is a row\n");
      return FAIL;
    } 
  if ( An == 1 && Bn != 1 )
    {
      Scierror("Error:\tA(ind)=B, B must be column when A is a column\n");
      return FAIL;
    }
  /* Enlarging A **/
  if ( index->max > Amn ) 
    {
      if ( Amn == 0) 
	{
	  if ( Bn != 1) 
	    { if ( (*F)(A,1,index->max) == FAIL) return(FAIL);}
	  else
	    { if ( (*F)(A,index->max,1) == FAIL) return(FAIL);}
	}
      else if ( Am == 1) 
	{
	  if ( An == 1) 
	    {
	      if ( Bn != 1) 
		{if ( (*F)(A,1,index->max) == FAIL) return(FAIL);}
	      else
		{if ( (*F)(A,index->max,1) == FAIL) return(FAIL);}
	    } 
	  else
	    {
	      if ( (*F)(A,Am,index->max) == FAIL) return(FAIL);
	    }
	}
      else
	{
	  if ( An == 1)
	    {
	      if ( (*F)(A,index->max,An) == FAIL) return(FAIL);
	    }
	  else
	    {
	      Scierror("Error:\tA(ind)=B, ind must be inside A range when A is not a vector\n");
	      return(FAIL);
	    }  
	}
    }
  if ( Bmn == 1) 
    {
      *Bscal=1;
    }
  else
    {
      if ( Bmn != index->nval  ) 
	{
	  Scierror("Error:\tA(ind)=B, ind and B have incompatible sizes\n");
	  return FAIL;
	}
      *Bscal=0;
    }
  return OK;
}



/**
 * nsp_spmaxpcolmatrix_delete_rows:
 * @A: a #NspSpMaxpColMatrix
 * @Rows: a #NspObject
 * 
 *  A(Rows,:) = [],  A is changed. 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_delete_rows(NspSpMaxpColMatrix *A, NspObject *Rows)
{
  NspMatrix *Row;
  int i,j,k,count_deleted = 0;
  index_vector index={0};
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Rows,&index) == FAIL) return FAIL;

  if ( index.nval == 0) return(OK);
  if ( index.min < 1 || index.max > A->m ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      goto end_fail;
    }
  /* working array */
  if ((Row = nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT ) 
    goto end_fail;
  for ( i= 0 ; i < Row->mn ; i++) Row->I[i]=0;
  /* count deleted rows */
  for ( i= 0 ; i < index.nval ; i++)
    {
      int row = index.val[i];
      if ( Row->I[row]==0)
	{
	  count_deleted++ ;
	  Row->I[row]=1;
	}
    }
  for ( i = 0 ; i < A->n ; i++) 
    {
      int row=-1,rowp=-1,last;
      int ndel=0;
      SpCol *Ai = A->D[i];
      Ai->iw=0;
      for ( j = 0 ; j < Row->mn ; j++)
	{
	  if ( Row->I[j]== 0) continue;
	  rowp=row;
	  row=j;
	  /* test if row is non nul in column i 
	   * we use the fact that rows are explored in increasing order
	   * and Ai->J is increasing too to limit the search at each step. 
	   * The elements which are to be deleted are marked with -1 
	   */
	  last = Ai->size;
	  for ( k = Ai->iw ; k < Ai->size ; k++) 
	    {
	      int Air = Ai->J[k];
	      if (Air == row) { Ai->J[k]=-1; last = k+1 ; break;}
	      else if ( row < Air ) { last = k;break;}
	      else if ( rowp < Air ) { Ai->J[k] -= ndel;}
	    }
	  Ai->iw = last;
	  ndel++;
	}
      /* last rows have to be changed */
      for ( k = Ai->iw; k < Ai->size ; k++)
	Ai->J[k] -= ndel;
      /* eliminate -1 values and resize */
      ndel =nsp_spmaxpcolmatrix_compress_col_simple(A,i);
      if (nsp_spmaxpcolmatrix_resize_col(A,i, Ai->size-ndel ) == FAIL) 
	{
	  nsp_matrix_destroy(Row);
	  goto end_fail;
	}
      for ( k = 0 ; k < Ai->size ; k++) 
	{
	  if ( Ai->J[k] > A->m-  count_deleted)
	    Sciprintf("Bug row value out of bounds\n");
	}
    }
  A->m -= count_deleted;
  /* XXXX : Attention ici si A->n devient egal a zero 
   * Il faut detruire les ligne pour renvoyer une matrice vide 
   */
  nsp_matrix_destroy(Row);
  nsp_free_index_vector_cache(&index);
  return(OK);

 end_fail: 
  nsp_free_index_vector_cache(&index);
  return FAIL;
}


/**
 * nsp_spmaxpcolmatrix_compress_col:
 * @A: a #NspSpMaxpColMatrix
 * @i: an integer 
 * 
 * compress the #SpCol structure of the @i-th column of sparse Matrix @A 
 * by moving elements (eliminating the -1 values). the row indices 
 * stored in A->D[i]->J are also changed during the compression assuming 
 * that each -1 implies that the row indice is decreased by 1.
 * [1,2,-1,4,5,6,-1,11]--> [1,2,3,4,5,9].
 * The #SpCol structure is not reallocated
 * 
 * 
 * Return value: the number of deleted elements in the column @i.
 **/

int nsp_spmaxpcolmatrix_compress_col(NspSpMaxpColMatrix *A, int i)
{
  int first = -1,next=-1,ioff=1,j,k;
  SpCol *R = A->D[i];
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

/**
 * nsp_spmaxpcolmatrix_compress_col_simple:
 * @A: a #NspSpMaxpColMatrix
 * @i: an integer
 * 
 * 
 * compress the #SpCol structure of the @i-th column of sparse Matrix @A 
 * by moving elements (eliminating the -1 values). the row indices 
 * stored in A->D[i]->J are not changed here. 
 * [1,2,-1,4,5,6,-1,11]--> [1,2,4,5,6,11].
 * The #SpCol structure is not reallocated
 * 
 * Return value: the number of deleted elements in the column @i.
 **/

int nsp_spmaxpcolmatrix_compress_col_simple(NspSpMaxpColMatrix *A, int i)
{
  int ndel = 0;
  /* Move block data left in A->D 
   * column to be removed are marked with iw == -1 
   */
  SpCol *Ai = A->D[i];
  int free_pos=-1;
  for ( i = 0 ; i < Ai->size ; i++)
    {
      if (  Ai->J[i] != -1 )
	{
	  if ( free_pos != -1 ) 
	    {
	      Ai->J[free_pos] = Ai->J[i];
	      switch ( A->rc_type ) 
		{
		case 'r' : Ai->R[free_pos] = Ai->R[i];break;
		case 'c' : Ai->C[free_pos] = Ai->C[i];break;
		}
	      free_pos++;
	    }
	}
      else 
	{
	  if ( free_pos == -1) free_pos=i;
	  ndel++;
	}
    }
  return ndel;
}


/**
 * nsp_spmaxpcolmatrix_delete_cols:
 * @A: a #NspSpMaxpColMatrix
 * @Cols: a #NspObject
 * 
 *  A(:,Cols) = []
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_delete_cols(NspSpMaxpColMatrix *A, NspObject *Cols)
{
  int i,ind,free_pos,count_deleted=0;
  index_vector index={0};
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Cols,&index) == FAIL) return FAIL;

  if ( index.nval == 0) goto end_ok;
  if ( index.min < 1 || index.max > A->n ) 
    {
      Scierror("Error:\tIndices out of bounds in sparse column deletion\n");
      goto end_fail;
    }
  for ( i= 0 ; i < A->n ; i++) 
    A->D[i]->iw= 0;
  /* resize to zero the column which are to be deleted 
   * and mark them 
   */
  for ( i = 0 ; i < index.nval  ; i++)
    {
      ind =  index.val[i]; /* column to be deleted, val is 0-based */
      if (nsp_spmaxpcolmatrix_resize_col(A,ind,0) == FAIL) 
	goto end_fail; /* free_pos the associated data */
      if ( A->D[ind]->iw==0) 
	{
	  count_deleted++;
	  A->D[ind]->iw= -1;
	}
      /* FREE(A->D[ind]); performed latter */
    }
  /* Move block data left in A->D 
   * column to be removed are marked with iw == -1 
   */
  free_pos=-1;
  for ( i = 0 ; i < A->n ; i++)
    {
      if (  A->D[i]->iw != -1 )
	{
	  if ( free_pos != -1 ) 
	    {
	      A->D[free_pos] = A->D[i];
	      free_pos++;
	    }
	}
      else 
	{
	  if ( free_pos == -1) free_pos=i;
	  FREE(A->D[i]);
	}
    }
  /* resize and  free unused memory */
  A->n -= count_deleted;
  if ( A->n == 0 ) 
    {
      FREE(A->D);
      A->D=NULL;
    }
  else
    {
      A->D = REALLOC(A->D, A->n*sizeof( SpCol *));
      if ( A->D == NULL) 
	{
	  Scierror("Error: no more space available\n");
	  goto end_fail;
	}
    }

 end_ok : 
  nsp_free_index_vector_cache(&index);
  return OK;

 end_fail: 
  nsp_free_index_vector_cache(&index);
  return FAIL;

}


/* 
 * A(elts) = []: XXXXX To be done 
 * 
 */


/**
 * SpExtract_G:
 * @A: a #NspSpMaxpColMatrix
 * @Rows: a #NspMatrix 
 * @Cols: a #NspMatrix 
 * @flag: an integer 
 * @err: an in pointer 
 * 
 * Extract elements A(Row,Cols). 
 * Warning: Rows is changed (sorted).
 * Cols can be NULL if flag == 0 (this should be changed with a NULL check on Cols XXXXX
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

static NspSpMaxpColMatrix *SpExtract_G(NspSpMaxpColMatrix *A, NspObject *Rows, NspObject *Cols, int flag, int *err)
{
  NspMatrix *Work= NULL, *Index = NULL;
  NspSpMaxpColMatrix *Loc=NULL;
  int i,j,Cn;
  
  index_vector index_c={0}, index_r={0};
  index_r.iwork = matint_iwork1;
  index_c.iwork = matint_iwork2;

  /* if ( A->mn == 0) return nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,0,0); */
  if (flag == 1) 
    {
      if ( nsp_get_index_vector_from_object(Cols,&index_c) == FAIL) return NULLSPMAXPCOL;
      if ( index_c.min < 1 ||  index_c.max > A->n  ) 
	{
	  *err=1;
	  Scierror("Error:\tColumn indices are out of bound\n");
	  goto err;
	}
    }
  if ( nsp_get_index_vector_from_object(Rows,&index_r) == FAIL) goto err;
  
  /* */
  
  if (( Index = nsp_matrix_create(NVOID,'r',index_r.nval,1)) == NULLMAT ) goto err;
  nsp_qsort_int(index_r.val,Index->I,TRUE,index_r.nval,'i');
  
  if ( index_r.nval == 0 ) 
    {
      int cols = (Cols == NULL) ? A->n : index_c.nval;
      Loc= nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,0,cols);
      if ( Loc ==  NULLSPMAXPCOL) goto err;
      nsp_matrix_destroy(Index);
      nsp_free_index_vector_cache(&index_r);
      nsp_free_index_vector_cache(&index_c);
      return Loc;
    }
  *err=0;
  if (  index_r.min < 1 ||  index_r.max > A->m ) 
    {
      *err=1;
      goto err;
    }
  Cn= (flag == 1) ? index_c.nval  : A->n;
  if ( (Loc =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,index_r.nval,Cn))== NULLSPMAXPCOL) 
    goto err;
  /* used to store elements */
  if ( ( Work = nsp_matrix_create(NVOID,'r',2,index_r.nval)) == NULLMAT) 
    goto err;
  
  for ( i = 0 ; i < Loc->n ; i++)
    {
      int count;
      int imin,imax;
      SpCol *Ai, *Li;
      int col;
      col = (flag == 1) ? index_c.val[i] : i ;
      Ai= A->D[col];
      Li= Loc->D[i];
      Li->iw=0;
      if ( Ai->size == 0) continue; /* nothing to do row is empty */
      imin=0; imax= Ai->size-1 ;
      Li->iw=0;
      count = nsp_bi_dichotomic_search_i(index_r.val,0,index_r.nval-1,Ai->J,imin,imax,Work,Index,0);
      /* now we know the column size */
      if (nsp_spmaxpcolmatrix_resize_col(Loc,i,count)==FAIL) goto err;
      /* Fills the rows of i-th column 
       * the first line of Rows gives the lines indice to fill in Li 
       * the second line gives the associated lines to use in Ai
       */
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
      /* we sort the column in increasing row order, i.e J must be increasing */
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
  if ( Loc != NULL) nsp_spmaxpcolmatrix_destroy(Loc);
  return NULL;
}



/**
 * nsp_spmaxpcolmatrix_extract:
 * @A: a #NspSpMaxpColMatrix
 * @Rows: a #NspMatrix 
 * @Cols: a #NspMatrix 
 * 
 * returns A(Row,Cols) but @Rows is changed in the process.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_extract(NspSpMaxpColMatrix *A, NspObject *Rows, NspObject *Cols)
{
  NspSpMaxpColMatrix *Sp;
  int err;
  Sp=SpExtract_G(A,Rows,Cols,1,&err);
  if (err==1 ) Scierror("Error:\tIndices out of bound\n");
  return Sp;
}


/**
 * nsp_spmaxpcolmatrix_extract_elts:
 * @A: a #NspSpMaxpColMatrix
 * @Elts: a #NspMatrix 
 * 
 * return A(Elts)
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_extract_elts(NspSpMaxpColMatrix *A, NspObject *Elts)
{
  NspSpMaxpColMatrix *Loc;
  int i,err,k;
  index_vector index={0};
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Elts,&index) == FAIL) return NULLSPMAXPCOL;
  if ( A->m ==0 || A->n == 0) 
    {
      Loc = nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->m,A->n);
      nsp_free_index_vector_cache(&index);
      return Loc;
    }

  if ( index.min < 1 || index.max > A->m*A->n ) /* possible overflow */
    {
      Scierror("Error:\tIndices out of bound\n");
      goto fail;
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      NspMatrix *Rows ; 
      if ((Rows = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT ) goto fail;
      Rows->R[0] = 1;
      /* like A(1,Elts) */
      Loc=  SpExtract_G(A,NSP_OBJECT(Rows),Elts,1,&err);
      if (err==1 ) Scierror("Error:\tIndices out of bound\n");
      nsp_free_index_vector_cache(&index);
      return Loc;
    }
  else
    {
      int n= (index.nval ==0 )? 0: 1;
      if ( (Loc =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,index.nval,n))== NULLSPMAXPCOL) 
	goto fail;
      A->D[0]->iw=0;
      for ( i=0; i < index.nval ; i++) 
	{
	  int el = index.val[i];
	  int row = el % A->m ;   
	  int col = (el-row)/A->m;
	  /* Loc[i,0] = A[row,col] **/
	  int ok = -1;
	  for ( k = 0 ; k < A->D[col]->size ; k++) 
	    {
	      if ( row == A->D[col]->J[k] ) { ok = k ; break;}
	      if ( row <  A->D[col]->J[k] ) { break;}
	    }
	  if ( ok != -1) 
	    {
	      /*  A[row,col] is non null */
	      int j = A->D[0]->iw;
	      A->D[0]->iw++;
	      if (nsp_spmaxpcolmatrix_resize_col(Loc,0,A->D[0]->iw)==FAIL) goto fail;
	      Loc->D[0]->J[j]=i;
	      switch ( A->rc_type ) 
		{
		case 'r' : Loc->D[0]->R[j] =  A->D[col]->R[k]; break;
		case 'c' : Loc->D[0]->C[j] =  A->D[col]->C[k]; break;
		}
	    }
	}
    }
  
  nsp_free_index_vector_cache(&index);
  return Loc;

 fail:
   nsp_free_index_vector_cache(&index);
   return NULLSPMAXPCOL;
}

/**
 * nsp_spmaxpcolmatrix_extract_rows:
 * @A: a #NspSpMaxpColMatrix
 * @Rows: a #NspObject
 * @err: an int pointer
 * 
 * returns A(Rows,:) but @Rows is changed 
 * err is used inside for-loops 
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_extract_rows(NspSpMaxpColMatrix *A, NspObject *Rows, int *err)
{
  return SpExtract_G(A,Rows,NULLOBJ,0,err);
}

/*
 * A1=MatLoopCol(A1,M,i,rep)
 * Used in for loops XXXX to be done 
 */	

/**
 * nsp_spmaxpcolmatrix_extract_cols:
 * @A: a #NspSpMaxpColMatrix
 * @Cols: a #NspMatrix 
 * @err: an int pointer
 * 
 * A(:,Cols)
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_extract_cols(NspSpMaxpColMatrix *A, NspObject *Cols, int *err)
{
  NspSpMaxpColMatrix *Loc;
  int i,j;
  index_vector index={0};
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Cols,&index) == FAIL) 
    return NULLSPMAXPCOL;
  
  *err=0;
  if ( index.nval == 0) 
    {
      Loc= nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->m,0);
      if ( Loc == NULLSPMAXPCOL ) goto fail;
      return Loc;
    }
  if ( index.min < 1 ||  index.max > A->n  ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      goto fail;
    }
  if ( (Loc =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->m,index.nval))== NULLSPMAXPCOL) 
    goto fail;

  for ( i = 0 ; i < Loc->n ; i++)
    {
      int col= index.val[i];
      SpCol *Ai= A->D[col];
      SpCol *Li= Loc->D[i];
      if (nsp_spmaxpcolmatrix_resize_col(Loc,i,Ai->size)==FAIL) goto fail;
      switch ( A->rc_type ) 
	{
	case 'r' : 
	  for ( j = 0 ; j < Ai->size ; j++ )
	    {	
	      Li->J[j]= Ai->J[j];
	      Li->R[j]= Ai->R[j];
	    }
	  break;
	case 'c': 
	  for ( j = 0 ; j < Ai->size ; j++ )
	    {	
	      Li->J[j]= Ai->J[j];
	      Li->C[j]= Ai->C[j];
	    }
	  break;
	}
    }
  return Loc;
 fail: 
  nsp_free_index_vector_cache(&index);
  return NULLSPMAXPCOL;
  
}


/**
 * nsp_spmaxpcolmatrix_diag_extract:
 * @A: a #NspSpMaxpColMatrix
 * @k: an integer
 * 
 * 
 * Returns the kthe diag of a Sparse NspMatrix as a Sparse  Column.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_diag_extract(NspSpMaxpColMatrix *A, int k)
{
  NspSpMaxpColMatrix *Loc;
  int j,i,rmin,/* rmax,*/ cmin,/* cmax,*/ itmax,count=0;
  rmin = Max(0,-k);
  cmin = Max(0,k);
  itmax = Min(A->m-rmin,A->n -cmin );
  /* rmax = rmin + itmax; */
  /* cmax = cmin + itmax; */
  if ( itmax <= 0 ) 
    {
      Loc =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,(int) 0 , (int) 0);
      return(Loc);
    }
  if (( Loc =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,itmax,(int)1))==NULLSPMAXPCOL)  return NULLSPMAXPCOL;
  if (nsp_spmaxpcolmatrix_resize_col(Loc,0,itmax)==FAIL) return NULLSPMAXPCOL;  
  /* accumulate values */
  for ( i = 0 ; i < itmax ; i++)
    {
      int ir = rmin+i;
      int ic = cmin+i;
      int ok=-1;
      for ( j = 0 ; j < A->D[ic]->size ; j++) 
	{
	  if ( A->D[ic]->J[j] == ir) 
	    {
	      ok=j;break;
	    }
	  else if ( A->D[ic]->J[j] > ir ) break;
	}
      if ( ok != -1 ) 
	{
	  Loc->D[0]->J[count] = ir -rmin ;
	  if ( A->rc_type == 'c' )
	    Loc->D[0]->C[count] = A->D[ic]->C[ok];
	  else 
	    Loc->D[0]->R[count] = A->D[ic]->R[ok];
	  count++;
	}
    }
  if (nsp_spmaxpcolmatrix_resize_col(Loc,0, count ) == FAIL) 
    { 
      nsp_spmaxpcolmatrix_destroy(Loc);
      return NULL;
    }
  return Loc;
}


static int  GetDiagVal (NspSpMaxpColMatrix *Diag,int i,double *val,doubleC *cval);

/**
 * nsp_spmaxpcolmatrix_set_diag:
 * @A: a #NspSpMaxpColMatrix
 * @ODiag: a #NspObject
 * @k: an integer 
 * 
 * sets the @k-th diagonal of @A to @Diag 
 * @A is complexified if necessary 
 *  
 * diagonal elements can be specified by a sparse or a full matrix (and also by a simple scalar)
 * Return value: %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_set_diag(NspSpMaxpColMatrix *A, NspObject *ODiag, int k)
{
  int i,l, ii;
  int rmin,cmin,dsize, mn;
  NspSpMaxpColMatrix *Diag = (NspSpMaxpColMatrix*) ODiag;
  NspMatrix *FDiag = (NspMatrix *) ODiag; 
  Boolean diag_is_sparse = IsSpMaxpColMat(ODiag) || IsSpMat(ODiag), diag_is_scalar;
  char diag_rc_type;

  rmin = Max(0,-k);
  cmin = Max(0,k);
  if ( diag_is_sparse )
    {
      diag_rc_type = Diag->rc_type;
      mn = Diag->m*Diag->n;
    }
  else
    {
      diag_rc_type = FDiag->rc_type;
      mn = FDiag->mn;
    }

  dsize = Min(A->m-rmin,A->n -cmin );
  diag_is_scalar = mn == 1;
  if ( dsize <= 0 || (!diag_is_scalar  &&  dsize != mn) ) 
    {
      Scierror("Error:\tdiagonal number and/or vector size not compatible with given matrix\n");
      return(FAIL);
    }

  if ( diag_rc_type == 'c' && A->rc_type == 'r' ) 
    if ( nsp_spmaxpcolmatrix_complexify(A) == FAIL ) return(FAIL);

  if ( ! diag_is_sparse )
    {
      /* be sure that diag is in proper mode */
      FDiag = Mat2double(FDiag);
    }

  for ( i = 0, ii = 0 ; i < dsize ; i++ ) 
    {
      int rep ;
      double val=0.0;
      doubleC cval={0.0,0.0};

      /* get next element to be inserted */
      if ( diag_is_sparse )
	rep = GetDiagVal(Diag,ii,&val,&cval);
      else
	{
	  if ( diag_rc_type == 'r' ) 
	    { 
	      val = FDiag->R[ii]; 
	      rep = val != 0.0 ? OK : FAIL;
	    }
	  else
	    { 
	      cval = FDiag->C[ii]; 
	      rep = cval.r != 0.0 || cval.i != 0.0 ? OK : FAIL; 
	    }
	}
      if ( ! diag_is_scalar ) ii++;

      if ( rep  == OK  )    /* element to be inserted is not null */
	{
	  SpCol *Ai= A->D[cmin+i];
	  int ok = 0,row=-1;
	  /* element is to be inserted at position (rmin+i,cmin+i) */
	  /* check if there is already a non zero element present */
	  for ( l = 0 ; l < Ai->size ; l++)
	    {
	      if ( Ai->J[l] == rmin+i ) 
		{
		  ok = 1 ; row = l  ; break;
		}
	      if ( Ai->J[l] > rmin+i ) 
		{
		  row = l ; break;
		}
	    }
	  if ( ok != 1) 
	    {
	      /* increment row i by 1 **/
	      if (nsp_spmaxpcolmatrix_resize_col(A,cmin+i,Ai->size+1)==FAIL) 
		return FAIL;
	      /* Move data right */
	      if ( row != -1 ) 
		{
		  memmove(&Ai->J[row+1], &Ai->J[row], (Ai->size-1-row)*sizeof(int));
		  if ( A->rc_type == 'r') 
		    memmove(&Ai->R[row+1], &Ai->R[row], (Ai->size-1-row)*sizeof(double));
		  else
		    memmove(&Ai->C[row+1], &Ai->C[row], (Ai->size-1-row)*sizeof(doubleC));
		}
	      else 
		{
		  /* insertion at the end */
		  row = Ai->size -1;
		}
	    }
	  /* Now the new row is to be inserted at position col **/
	  Ai->J[row] = rmin+i;
	  if ( A->rc_type == 'r' ) 
	    {
	      Ai->R[row] = val ;
	    }
	  else
	    {
	      if ( diag_rc_type == 'r') 
		{
		  Ai->C[row].r  = val ; Ai->C[row].i  = 0.0 ;
		}
	      else 
		Ai->C[row]  = cval;
	    }
	}
      else                      /*  element to insert is a zero  */ 
	{
	  SpCol *Ai= A->D[cmin+i];
	  int ok = 0,tag=-1;
	  /* element is to be inserted at position (rmin+i,cmin+i) */
	  /* check if there is already a non zero element present  */
	  for ( l = 0 ; l < Ai->size ; l++)
	    {
	      if ( Ai->J[l] == rmin+i ) 
		{
		  ok = 1 ; tag = l  ; break;
		}
	      if ( Ai->J[l] > rmin+i ) 
		{
		  break;
		}
	    }
	  if ( ok == 1) 
	    {
	      /*  such an element was present and we want to set   */ 
	      /*  it to zero thus we must supress it               */ 
	      memmove(&Ai->J[tag], &Ai->J[tag+1], (Ai->size-1-tag)*sizeof(int));
	      if ( A->rc_type == 'r') 
		memmove(&Ai->R[tag], &Ai->R[tag+1], (Ai->size-1-tag)*sizeof(double));
	      else
		memmove(&Ai->C[tag], &Ai->C[tag+1], (Ai->size-1-tag)*sizeof(doubleC));

	      if ( nsp_spmaxpcolmatrix_resize_col(A,cmin+i,Ai->size-1) == FAIL ) 
		return FAIL;
	    }
	}
    }
  return(OK);
}


/**
 * GetDiagVal:
 * @Diag: a #NspSpMaxpColMatrix
 * @i: an integer 
 * @val: a double pointer 
 * @cval: a #doubleC pointer 
 * 
 * return the ith element of Diag in val or cval + an OK value 
 * if this element is non nul. 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

static int  GetDiagVal(NspSpMaxpColMatrix *Diag, int i, double *val, doubleC *cval)
{
  int row, col,k;
  if ( Diag->m == 0) return FAIL;
  row = i % Diag->m ;
  col = (i-row ) / Diag->m ;
  for ( k = 0 ; k < Diag->D[col]->size ; k++) 
    {
      if ( Diag->D[col]->J[k] == row ) 
	{
	  if ( Diag->rc_type == 'r' )
	    *val = Diag->D[col]->R[k] ;
	  else 
	    *cval = Diag->D[col]->C[k] ;
	  return OK ;
	}
      if ( Diag->D[col]->J[k] > row ) return FAIL;
    }
  return FAIL;
}

/**
 * nsp_spmaxpcolmatrix_diag_create:
 * @Diag: a #NspSpMaxpColMatrix 
 * @k: an integer 
 * 
 *  Creates a Matrix with kth diag set to Diag 
 * 
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_diag_create(NspSpMaxpColMatrix *Diag, int k)
{
  int i,k1;
  int rmin,cmin,rmax,cmax,n;
  NspSpMaxpColMatrix *Loc;
  rmin = Max(0,-k);
  cmin = Max(0,k);
  rmax = Diag->m*Diag->n + Max(0,-k);
  cmax = Diag->m*Diag->n + Max(0,k);
  /* make a square matrix */
  n = Max(cmax,rmax);
  if ((Loc =nsp_spmaxpcolmatrix_create(NVOID,Diag->rc_type,n,n))  == NULLSPMAXPCOL) 
    return(NULLSPMAXPCOL);
  if ( Diag->n == 1 )
    {
      for ( k1=0 ; k1 < Diag->D[0]->size ; k1++) 
	{
	  int j= Diag->D[0]->J[k1];
	  int row = rmin+j;
	  int col = cmin+j;
	  if (nsp_spmaxpcolmatrix_resize_col(Loc,col,1)==FAIL) return NULLSPMAXPCOL;
	  Loc->D[col]->J[0] = row;
	  switch ( Loc->rc_type ) 
	    {
	    case 'r' : Loc->D[col]->R[0] = Diag->D[0]->R[k1];break;
	    case 'c' : Loc->D[col]->C[0] = Diag->D[0]->C[k1];break;
	    }
	}
    }
  else if ( Diag->m == 1) 
    {
      for ( i=0 ; i < Diag->n ; i++) 
	{
	  if ( Diag->D[i]->size != 0) 
	    {
	      int row = rmin+i;
	      int col = cmin+i;
	      if (nsp_spmaxpcolmatrix_resize_col(Loc,col,1)==FAIL) return NULLSPMAXPCOL;
	      Loc->D[col]->J[0] = row;
	      switch ( Loc->rc_type ) 
		{
		case 'r' : Loc->D[col]->R[0] = Diag->D[i]->R[0];break;
		case 'c' : Loc->D[col]->C[0] = Diag->D[i]->C[0];break;
		}
	    }
	}
    }
  else
    {
      Scierror("Error: second element should be a sparse vector\n");
      return NULLSPMAXPCOL;
    }
  return Loc;
}


/**
 * nsp_spmaxpcolmatrix_mult:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * returns @Ax@B  by the method of gustafson,acm t.o.m.s. 
 * vol 4 (1978) p250. 
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_mult(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  NspSpMaxpColMatrix *C = NULLSPMAXPCOL;
  NspMatrix *x = NULLMAT;
  int *xb = NULL, *pxb = NULL;
  int i, j, k, v, ip, jp, kp, neli, final_neli;
  char type = 'r';
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
  if ( A->n != B->m ) 
    {
      Scierror("SpMult : incompatible arguments\n");
      return(NULLSPMAXPCOL);
    }

  if ( (C =nsp_spmaxpcolmatrix_create(NVOID,type, A->m,B->n)) == NULLSPMAXPCOL ) return NULLSPMAXPCOL; 
  
  if ( A->m == 0 || B->n ==0 ) return C;

  /* XXXX A unique alloc here should be better 
   * x of size 2*C->m xb = (int *) (x+C->m) ; pxb = xb+C->m
   * assuming strlen(double)=2*strlen(int);
   */ 

  /*  x is a one-dimensional array of size ge the number of rows of c, 
   *  used to store elements of current row of c in full (non-sparse) form.
   */
  if ( (x = nsp_matrix_create(NVOID,type,(int) 1, C->m)) == NULLMAT ) goto err;
  /*  xb is an array of same size as x. xb(j)=i if c(j,i) != 0.
   */
  if ( (xb =nsp_alloc_int(C->m)) == NULL) goto err;
  /*  pxb is used to store the effective non nul indices of row i of C 
   *  (act as a pointer to x at the end).
   */
  if ( (pxb =nsp_alloc_int(C->m)) == NULL) goto err;

  /* initialize the non-zero-element indicator for row i of c. */
  for (v = 0 ; v < C->m ; v++ )  xb[v] = -1;

  /* process the columns of B */
  for (i = 0 ; i < B->n ; i++) 
    {
      neli = 0;  /* to count the number of a priori non nul elements of column i of C */
      SpCol *Bi = B->D[i];
      SpCol *Ci = C->D[i];
      /*  process column i of B */
      for (jp = 0  ; jp <  Bi->size  ; jp++) 
	{
	  SpCol *Aj;
	  /* j is the current row-index for B i.e B(j,i) */
	  j = Bi->J[jp] ;
	  /* column j of A */
	  Aj = A->D[j];
	  /*  We process the column j of  A. For efficiency we separate the real case 
           *  from the 3 complex cases (which are nevertheless treated together)
	   */
	  if ( C->rc_type == 'r' )
	    for (kp = 0 ; kp < Aj->size ; kp++) 
	      {
		/* getting non nul A(k,j) */
		k = Aj->J[kp]; 
		/* check if contribution already exixts to C(k,i) */
		if ( xb[k] != i )
		  { 
		    xb[k] = i; pxb[neli] = k; neli++; x->R[k]=0.00;
		  }
		/* A(k,j)*B(j,i) */
		x->R[k] += Aj->R[kp] * Bi->R[jp];
	      }
	  else 
	    for (kp = 0 ; kp < Aj->size ; kp++) 
	      {
		/* getting non nul b(j,k) */
		k = Aj->J[kp];
		/* check if contribution already exixts to c(i,k) */
		if ( xb[k] != i )
		  { 
		    xb[k] = i; pxb[neli] = k; neli++; x->C[k].r = 0.00; x->C[k].i = 0.00;
		  }
		if ( A->rc_type == 'r') 
		  {
		    x->C[k].i += Aj->R[kp] * Bi->C[jp].i;
		    x->C[k].r += Aj->R[kp] * Bi->C[jp].r;
		  }
		else if ( B->rc_type == 'r' ) 
		  {
		    x->C[k].i += Aj->C[kp].i * Bi->R[jp];
		    x->C[k].r += Aj->C[kp].r * Bi->R[jp];
		  }
		else 
		  {
		    x->C[k].r += Aj->C[kp].r * Bi->C[jp].r -  Aj->C[kp].i* Bi->C[jp].i;
		    x->C[k].i += Aj->C[kp].i * Bi->C[jp].r +  Aj->C[kp].r* Bi->C[jp].i;
		  }
	      }
	}
      /* we have now computed c(.,i) in expanded form and stored it in x,xb 
       * pxb[0..neli-1] contains all the non nul row indices (so point to
       * values to retrieve from x) but their are not in order (and so the sort) 
       * we store them in C and exclude number which have cancelled during computation 
       */
      
      nsp_qsort_int(pxb,NULL,FALSE,neli,'i');
      /* nsp_qsort_bp_int(pxb, neli, NULL, FALSE, 'i'); */

      /* but as cancellation is unlikely to occur we allocate first the column
       * with the a priori non null element number neli 
       */
      if ( nsp_spmaxpcolmatrix_resize_col(C,i,neli) == FAIL ) goto err;
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
		  Ci->C[ip] = x->C[v];
		  ip++;
		}
	      else
		final_neli--; /* cancellation occurs => final resize */
            }
        }
      if ( final_neli < neli )
	if ( nsp_spmaxpcolmatrix_resize_col(C,i,final_neli) == FAIL ) goto err;
    }
  FREE(pxb);
  FREE(xb);
  nsp_matrix_destroy(x);
  return C;

 err:
  FREE(pxb);
  FREE(xb);
  nsp_matrix_destroy(x);
  nsp_spmaxpcolmatrix_destroy(C);
  return NULLSPMAXPCOL;
}


/**
 * nsp_spmaxpcolmatrix_mult_sp_m:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspMatrix
 * @Res: a #NspMatrix (if NULL space is allocated otherwise Res hold the result)
 *                     
 * @Ax@B when @B is a full matrix and returns the result as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLMAT (in case of failure)
 **/

NspMatrix *nsp_spmaxpcolmatrix_mult_sp_m(NspSpMaxpColMatrix *A, NspMatrix *B, NspMatrix *Res)
{
  NspMatrix *C = NULLMAT;
  int j, jp, kp,size;
  const int inc=1;
  char type = 'r';
  double zero=0.0;
  SpCol *Ajp;
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
  if ( A->n != B->m )
    {
      Scierror("SpMult : incompatible arguments\n");
      return NULLMAT;
    }

  if ( Res == NULL )
    {
      if ( (C =nsp_matrix_create(NVOID,type,A->m,B->n)) == NULLMAT ) return NULLMAT;
    }
  else  
    {
      /* Res should hold the result verify if it is valid... */
      if ( Res->m != A->m || Res->n != B->n || Res->rc_type != type )
	{
	  Scierror("SpMult : result badly allocated\n");
	  return NULLMAT;
	}
      C = Res;
    }

  /* initialize to 0.0 */
  size=(type == 'c') ? 2*C->mn : C->mn;
  nsp_dset(&size,&zero,C->R,&inc);

  if ( A->rc_type == 'r' )
    {
      if ( B->rc_type == 'r' )      /* A real, B real */
	{
	  double *Bj, *Cj, coef;
	  for ( j = 0, Bj=B->R, Cj=C->R ; j < B->n ; j++, Bj+=B->m, Cj+=C->m )
	    {
	      for (jp = 0 ; jp <  A->n  ; jp++)
		{
		  Ajp = A->D[jp]; coef = Bj[jp];
		  for (kp = 0 ; kp < Ajp->size ; kp++)
		    Cj[Ajp->J[kp]] += Ajp->R[kp]*coef;
		}
	    }
	}
      else                          /* A real, B complex */
	{
	  doubleC *Bj, *Cj, coef;
	  for ( j = 0, Bj=B->C, Cj=C->C ; j < B->n ; j++, Bj+=B->m, Cj+=C->m )
	    {
	      for (jp = 0 ; jp <  A->n  ; jp++)
		{
		  Ajp = A->D[jp]; coef = Bj[jp];
		  for (kp = 0 ; kp < Ajp->size ; kp++)
		    {
		      Cj[Ajp->J[kp]].r += Ajp->R[kp]*coef.r;
		      Cj[Ajp->J[kp]].i += Ajp->R[kp]*coef.i;
		    }
		}
	    }
	}
    }
  else
    {
      if ( B->rc_type == 'r' )      /* A complex, B real */
	{
	  double *Bj, coef; doubleC *Cj;
	  for ( j = 0, Bj=B->R, Cj=C->C ; j < B->n ; j++, Bj+=B->m, Cj+=C->m )
	    {
	      for (jp = 0 ; jp <  A->n  ; jp++)
		{
		  Ajp = A->D[jp]; coef = Bj[jp];
		  for (kp = 0 ; kp < Ajp->size ; kp++)
		    {
		      Cj[Ajp->J[kp]].r += Ajp->C[kp].r*coef;
		      Cj[Ajp->J[kp]].i += Ajp->C[kp].i*coef;
		    }
		}
	    }
	}
      else                          /* A complex, B complex */
	{
	  doubleC *Bj, *Cj, coef;
	  for ( j = 0, Bj=B->C, Cj=C->C ; j < B->n ; j++, Bj+=B->m, Cj+=C->m )
	    {
	      for (jp = 0 ; jp <  A->n  ; jp++)
		{
		  Ajp = A->D[jp]; coef = Bj[jp];
		  for (kp = 0 ; kp < Ajp->size ; kp++)
		    {
		      Cj[Ajp->J[kp]].r += Ajp->C[kp].r*coef.r - Ajp->C[kp].i*coef.i;
		      Cj[Ajp->J[kp]].i += Ajp->C[kp].r*coef.i + Ajp->C[kp].i*coef.r;
		    }
		}
	    }
	}
    }
  return C;
}
/**
 * nsp_spmaxpcolmatrix_pmult_sp_m:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspMatrix
 * @Res: a #NspMatrix (if NULL space is allocated otherwise Res hold the result)
 *                     
 * @A' * @B when @B is a full matrix and returns the result as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLMAT (in case of failure)
 **/

extern NspSpRowMatrix *nsp_spcolmatrix_cast_to_sprow(NspSpColMatrix *M);
extern NspSpColMatrix *nsp_sprowmatrix_cast_to_spcol(NspSpRowMatrix *M);

NspMatrix *nsp_spmaxpcolmatrix_pmult_sp_m(NspSpMaxpColMatrix *A, NspMatrix *B, NspMatrix *Res)
{
  NspMatrix *C = NULLMAT;
  NspSpRowMatrix *AA;
  int k;
  char type='r';

  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';

  /* A' has dimensions A->n x A->m so compatibility condition is: */
  if ( A->m != B->m )
    {
      Scierror("SptMult : incompatible arguments\n");
      return NULLMAT;
    }

  if ( Res == NULL )
    {
      if ( (C =nsp_matrix_create(NVOID,type,A->n,B->n)) == NULLMAT ) return NULLMAT;
    }
  else  
    {
      /* Res should hold the result verify if it is valid... */
      if ( Res->m != A->n || Res->n != B->n || Res->rc_type != type )
	{
	  Scierror("SptMult : result badly allocated\n");
	  return NULLMAT;
	}
      C = Res;
    }

  AA = nsp_spcolmatrix_cast_to_sprow((NspSpColMatrix *) A);

  if ( B->rc_type == 'c' )  /* B should be conjugated */
    {
      for ( k = 0 ; k < B->mn ; k++ )
	B->C[k].i = - B->C[k].i;
    }

  nsp_sprowmatrix_mult_sp_m(AA, B, C);

  if ( C->rc_type == 'c' )
    {
      for ( k = 0 ; k < B->mn ; k++ )
	C->C[k].i = - C->C[k].i;
    }

  if ( B->rc_type == 'c' )   /* return to initial state for B */
    {
      for ( k = 0 ; k < B->mn ; k++ )
	B->C[k].i = - B->C[k].i;
    }
  
  nsp_sprowmatrix_cast_to_spcol(AA);

  return C;
}

/**
 * nsp_spmaxpcolmatrix_mult_m_sp:
 * @X: a #NspMatrix
 * @A: a #NspSpMaxpColMatrix
 * 
 * @Xx@A when @X is full 
 * 
 * Return value: a new  #NspMatrix or %NULLMAT
 **/

NspMatrix *nsp_spmaxpcolmatrix_mult_m_sp(NspMatrix *X,NspSpMaxpColMatrix *A)
{
  int i, j, k;
  char rtype = 'c';
  NspMatrix *B;

  if ( A->rc_type == 'r' && X->rc_type == 'r' ) rtype = 'r';

  if ( (B = nsp_matrix_create(NVOID,rtype,X->m,A->n)) == NULLMAT ) 
    return NULLMAT;

  /* la suite est pas belle ... */

  if ( rtype == 'r' )
    {
      double *Bij = B->R;
      for ( j = 0 ; j < B->n ; j++ )
	{
	  SpCol *Aj = A->D[j];
	  for ( i = 0 ; i < B->m ; i++ )
	    {
	      double temp = 0.0;
	      for ( k = 0 ; k < Aj->size ; k++ )
		temp += X->R[i+Aj->J[k]*X->m]* Aj->R[k];
	      *Bij = temp;
	      Bij++;
	    }
	}
    }
  else if ( A->rc_type == 'r' && X->rc_type == 'c' ) 
    {
      doubleC *Bij = B->C;
      for ( j = 0 ; j < B->n ; j++ )
	{
	  SpCol *Aj = A->D[j];
	  for ( i = 0 ; i < B->m ; i++ )
	    {
	      doubleC temp = {0.0, 0.0};
	      for ( k = 0 ; k < Aj->size ; k++ )
		{
		  doubleC Z= X->C[i+Aj->J[k]*X->m];
		  temp.r += Z.r* Aj->R[k];
		  temp.i += Z.i* Aj->R[k];
		}
	      *Bij = temp;
	      Bij++;
	    }
	}
    }
  else if ( A->rc_type == 'c' && X->rc_type == 'r' ) 
    {
      doubleC *Bij = B->C;
      for ( j = 0 ; j < B->n ; j++ )
	{
	  SpCol *Aj = A->D[j];
	  for ( i = 0 ; i < B->m ; i++ )
	    {
	      doubleC temp = {0.0, 0.0};
	      for ( k = 0 ; k < Aj->size ; k++ )
		{
		  double Z= X->R[i+Aj->J[k]*X->m];
		  doubleC W= Aj->C[k];
		  temp.r += Z* W.r;
		  temp.i += Z* W.i;
		}
	      *Bij = temp;
	      Bij++;
	    }
	}
    }
  else
    {
      doubleC *Bij = B->C;
      for ( j = 0 ; j < B->n ; j++ )
	{
	  SpCol *Aj = A->D[j];
	  for ( i = 0 ; i < B->m ; i++ )
	    {
	      doubleC temp = {0.0, 0.0};
	      for ( k = 0 ; k < Aj->size ; k++ )
		{
		  doubleC Z= X->C[i+Aj->J[k]*X->m];
		  doubleC W= Aj->C[k];
		  temp.r += Z.r* W.r - Z.i* W.i;
		  temp.i += Z.r* W.i + Z.i* W.r;
		}
	      *Bij = temp;
	      Bij++;
	    }
	}
    }
  return B;
}


/**
 * nsp_spmaxpcolmatrix_mult_scalar:
 * @val: pointer to a double or a double C
 * @val_type: 'r' if val point to a double and 'c' if val point to a complex
 * @A: a NspSpMaxpColMatrix
 * 
 * Do the operation A <- scalar * A  (that is A is modified in place)
 *
 * Return value: %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_mult_scalar(double *val, char val_type, NspSpMaxpColMatrix *A)
{
  int i, k;
  double scal=0.0;
  doubleC scalc; 
  Boolean zero_flag, was_complexified = FALSE;

  if ( val_type == 'r' )
    {
      scal = *val;
      zero_flag = scal == 0.0 ? TRUE : FALSE;
    }
  else
    {
      scalc.r = val[0]; scalc.i = val[1];
      zero_flag = (scalc.r == 0.0 && scalc.i == 0.0) ? TRUE : FALSE;
    }

  if ( zero_flag )
    {
      for ( i=0 ; i < A->n ; i++ ) 
	{
	  if ( A->D[i]->size != 0 ) 
	    {
	      FREE( A->D[i]->J);
	      FREE( A->D[i]->R);
	      A->D[i]->size =0;
	    }
	}
      return OK;
    }

  if ( val_type == 'c' && A->rc_type == 'r' )
    {
       if ( nsp_spmaxpcolmatrix_complexify(A) == FAIL )
	return FAIL;
      was_complexified = TRUE;
   }

  if ( A->rc_type == 'r' )       /* scalar and matrix real */
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->R[k] *= scal;
    }
  else if ( val_type == 'r' )    /* scalar real but matrix cmplx */
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  {
	    A->D[i]->C[k].r *= scal; A->D[i]->C[k].i *= scal;
	  }
    }
  else if ( was_complexified )   /* scalar is cmplx but matrix initially real */
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  {
	    A->D[i]->C[k].i = A->D[i]->C[k].r * scalc.i;
	    A->D[i]->C[k].r *= scalc.r;
	  }
    }
  else                           /* scalar and matrix both cmplx */
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0 ; k < A->D[i]->size ; k++ ) 
	  nsp_prod_c(&A->D[i]->C[k],&scalc);
    }

  return OK;
}

/**
 * nsp_spmaxpcolmatrix_mult_scal_old:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * nsp_spmaxpcolmatrix_mult_scal(A,B) when B is a scalar sparse 
 * A is changed 
 * the fact that B is scalar is not checked 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_mult_scal_old(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  int i,k ;
  if ( A->rc_type == 'r' &&  B->rc_type == 'c' )  
    {
      if (nsp_spmaxpcolmatrix_complexify(A) == FAIL ) return(FAIL);
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



/**
 * nsp_spmaxpcolmatrix_complexify:
 * @A: a #NspSpMaxpColMatrix
 * 
 * Changes A to complex type and 
 * provide storage allocation 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_complexify(NspSpMaxpColMatrix *A)
{
  int inc = 1;
  int i;
  if ( A->rc_type == 'c' ) return(OK);
  for ( i = 0 ; i < A->n ; i++) 
    {
      SpCol *Ai = A->D[i];
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

/**
 * nsp_spmaxpcolmatrix_setr:
 * @A: a #NspSpMaxpColMatrix
 * @d: a double 
 * 
 * Set real part of all non nul elements of A to d 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_setr(NspSpMaxpColMatrix *A, double d)
{
  int i,c1=1;
  switch ( A->rc_type ) 
    {
    case 'r' :
      for ( i = 0 ; i < A->n ; i++) 
	{
	  SpCol *Ai = A->D[i];
	  if ( Ai->size != 0) 
	    { 
	      nsp_dset(&Ai->size,&d,Ai->R, &c1);
	    }
	}
      break;
    case 'c' :
      for ( i = 0 ; i < A->n ; i++) 
	{
	  SpCol *Ai = A->D[i];
	  if ( Ai->size != 0) 
	    { 
	      c1=2;
	      nsp_dset(&Ai->size,&d,(double *) Ai->C, &c1);
	    }
	}
    }
  return(OK);
}


/**
 * nsp_spmaxpcolmatrix_seti:
 * @A: a #NspSpMaxpColMatrix
 * @d: a double 
 * 
 * Set imag part of all non nul elements of A to d 
 * if A is real A is changed to complex 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_seti(NspSpMaxpColMatrix *A, double d)
{
  int i,c1=1;
  switch ( A->rc_type ) 
    {
    case 'r' : if (nsp_spmaxpcolmatrix_complexify(A) == FAIL ) return(FAIL); break;
    }
  for ( i = 0 ; i < A->n ; i++) 
    {
      SpCol *Ai = A->D[i];
      if ( Ai->size != 0) 
	{ 
	  c1=2;
	  nsp_dset(&Ai->size,&d,((double *) Ai->C) +1, &c1);
	}
    }
  return(OK);
}


/**
 * RowMaxpCountNonNull:
 * @A: a #NspMatrix
 * @i: an integer 
 * 
 * 
 * Return value: the number of non null elements of @i-th row of matrix @A.
 **/

static int  RowMaxpCountNonNull(NspMatrix *A, int i)
{ 
  int count=0,j;
  switch ( A->rc_type ) 
    {
    case 'r' :
      for ( j = 0 ; j < A->n ; j++ )
	if (A->R[i+j*(A->m)] != mpzero ) count++;
      break;
    case 'c' :
      for ( j = 0 ; j < A->n ; j++ )  
	if (A->C[i+j*A->m].r  != mpzero ||  A->C[i+j*A->m].i != mpzero ) count++;
      break;
    }
  return(count);
}

/**
 * ColMaxpCountNonNull:
 * @A: a #NspMatrix
 * @j: an integer 
 * 
 * 
 * 
 * Return value: the number of non null elements of @i-th column of matrix @A.
 **/

static int  ColMaxpCountNonNull(NspMatrix *A, int j)
{ 
  int count=0,i;
  switch ( A->rc_type ) 
    {
    case 'r' :
      for ( i = 0 ; i < A->m ; i++ )
	if (A->R[i+j*(A->m)] != mpzero ) count++;
      break;
    case 'c' :
      for ( i = 0 ; i < A->m ; i++ )  
	if (A->C[i+j*A->m].r  != mpzero ||  A->C[i+j*A->m].i != mpzero )  count++;
      break;
    }
  return(count);
}

/**
 * nsp_spmaxpcolmatrix_from_mat:
 * @A:  a #NspMatrix
 * 
 * from full to sparse.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_from_mat(NspMatrix *A)
{ 
  /* nnul : counts non nul elements in the sparse matrix 
   * count : count non nul elements in row i 
   */
  int i,j;
  NspSpMaxpColMatrix *Sp;
  if (( Sp =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->m,A->n))== NULLSPMAXPCOL) return(NULLSPMAXPCOL);
  /* first pass to count non null elements on rows */
  for ( i = 0 ; i < A->n ; i++ ) 
    { 
      int count;
      count =  ColMaxpCountNonNull(A,(int)i) ;
      if (nsp_spmaxpcolmatrix_resize_col(Sp,i,count) == FAIL) return(NULLSPMAXPCOL) ;
    }
  /* Storing Datas */
  for ( i = 0 ; i < A->n ; i++ ) 
    { 
      SpCol *Ri = Sp->D[i];
      int count = 0;
      switch ( A->rc_type ) 
	{
	case 'r' :
	  for ( j = 0 ; j < A->m ; j++ ) 
	    { 
	      if ( A->R[j+i*A->m] != mpzero  ) 
		{
		  Ri->J[count] = j;
		  Ri->R[count] = A->R[j+i*A->m];
		  count++;
		}
	    }
	  break ;
	case 'c' : 
	  for ( j = 0 ; j < A->m ; j++ ) 
	    { 
	      if ( A->C[j+i*A->m].r != mpzero || A->C[j+i*A->m].i != mpzero )
		{
		  Ri->J[count] = j;
		  Ri->C[count].r  = A->C[j+i*A->m].r;
		  Ri->C[count].i  = A->C[j+i*A->m].i;
		  count++;
		}
	    }
	}
    }
  return(Sp) ;
}

/**
 * nsp_spmaxpcolmatrix_from_mat_transpose:
 * @A:  a #NspMatrix
 * 
 * creates a sparse matrix filled with the transpose of @A
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_from_mat_transpose(NspMatrix *A)
{ 
  /* nnul : counts non nul elements in the sparse matrix 
   * count : count non nul elements in row i 
   */
  int i,j;
  NspSpMaxpColMatrix *Sp;
  if (( Sp =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->n,A->m))== NULLSPMAXPCOL) return(NULLSPMAXPCOL);
  /* first pass to count non null elements */
  for ( i = 0 ; i < A->m ; i++ ) 
    { 
      int count;
      count =  RowMaxpCountNonNull(A,(int)i) ;
      if (nsp_spmaxpcolmatrix_resize_col(Sp,i,count) == FAIL) return(NULLSPMAXPCOL) ;
    }
  /* Storing Datas */
  for ( i = 0 ; i < A->m ; i++ ) 
    { 
      SpCol *Ri = Sp->D[i];
      int count = 0;
      switch ( A->rc_type ) 
	{
	case 'r' :
	  for ( j = 0 ; j < A->n ; j++ ) 
	    { 
	      if ( A->R[i+j*A->m] != mpzero  ) 
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
	      if ( A->C[i+j*A->m].r != mpzero || A->C[i+j*A->m].i != mpzero )
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

/**
 * nsp_spmaxpcolmatrix_to_mat:
 * @Sp: a #NspSpMaxpColMatrix
 * 
 * from sparse to full.
 * 
 * 
 * Return value: a new  #NspMpMatrix or %NULLMAXPMAT
 **/

NspMaxpMatrix *nsp_spmaxpcolmatrix_to_mpmat(NspSpMaxpColMatrix *Sp)
{ 	
  double *wR;
  doubleC *wC;
  NspMaxpMatrix *A;
  int i,k; 
  A = nsp_mpmatrix_create(NVOID,Sp->rc_type,Sp->m,Sp->n) ;
  if ( A == NULLMAXPMAT ) { return(NULLMAXPMAT);}
  nsp_mat_set_rval((NspMatrix *) A,mpzero );
  if ( A->rc_type == 'c') if (nsp_mat_set_ival((NspMatrix *) A,0.00) == FAIL ) return(NULLMAXPMAT);
  /* walk on columns */
  for ( i = 0 ; i < Sp->n ; i++ ) 
    {
      SpCol *Ri = Sp->D[i] ;
      int iof = i*A->m;
      switch ( A->rc_type ) 
	{
	case 'r' : 
	  wR = A->R+iof;
	  for  ( k = 0  ;  k < Ri->size ; k++)
	    { 
	      if ( Ri->J[k] > A->m || Ri->J[k] < 0) 
		{
		  Sciprintf("wrong column in sparse\n" );
		}
	      wR[Ri->J[k]] = Ri->R[k]  ;
	    }
	  break ; 
	case 'c' :
	  wC = A->C+iof;
	  for  ( k = 0  ;  k < Ri->size ; k++)
	    { 
	      wC[Ri->J[k]].r = Ri->C[k].r ;
	      wC[Ri->J[k]].i = Ri->C[k].i ;
	    }
	}
    }
  return(A);
}

/**
 * nsp_spmaxpcolmatrix_to_mat_transpose:
 * @Sp: a #NspSpMaxpColMatrix
 * 
 * from sparse to full. returns a full matrix 
 * filled from the transpose of @Sp.
 * 
 * 
 * Return value: a new  #NspMatrix or %NULLMAT
 **/

NspMatrix *nsp_spmaxpcolmatrix_to_mat_transpose(NspSpMaxpColMatrix *Sp)
{ 	
  double *wR;
  doubleC *wC;
  NspMatrix *A;
  int i,k; 
  A = nsp_matrix_create(NVOID,Sp->rc_type,Sp->n,Sp->m) ;
  if ( A == NULLMAT ) { return(NULLMAT);}
  nsp_mat_set_rval(A,0.0);
  if ( A->rc_type == 'c') if (nsp_mat_set_ival(A,0.00) == FAIL ) return(NULLMAT);
  /* walk on columns */
  for ( i = 0 ; i < Sp->n ; i++ ) 
    {
      SpCol *Ri = Sp->D[i] ;
      int iof = i;
      switch ( A->rc_type ) 
	{
	case 'r' : 
	  wR = A->R+iof;
	  for  ( k = 0  ;  k < Ri->size ; k++)
	    { 
	      if ( Ri->J[k] > A->n || Ri->J[k] < 0) 
		{
		  Sciprintf("wrong column in sparse\n" );
		}
	      *(wR+A->m*Ri->J[k]) = Ri->R[k]  ;
	    }
	  break ; 
	case 'c' :
	  wC = A->C+iof;
	  for  ( k = 0  ;  k < Ri->size ; k++)
	    { 
	      *(wC+A->m*Ri->J[k])= Ri->C[k];
	    }
	}
    }
  return(A);
}

/**
 * nsp_spmaxpcolmatrix_transpose:
 * @A: a #NspSpMaxpColMatrix
 * 
 * Transpose a sparse Matrix, A is unchanged and a new matrix is created and 
 * returned. 
 * Warning : For Complex matrices transpose is a' = transp+ conj
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_transpose(const NspSpMaxpColMatrix *A)
{
  int k,i,j;
  NspSpMaxpColMatrix *Loc;
  Loc =nsp_spmaxpcolmatrix_create( NVOID,A->rc_type,A->n,A->m);
  if ( Loc == NULLSPMAXPCOL ) return(NULLSPMAXPCOL) ; 
  /* Initialisation : a faire a la creation ?? xxx **/
  for (  i  = 0 ;  i  < Loc->n  ;  i++ ) 
    {
      Loc->D[i]->iw = 0;
    } 
  /* Counting elements of transposed matrix  */
  for (  i  = 0 ;  i  < A->n  ;  i++ ) 
    {
      SpCol *Ai = A->D[i];
      for  ( k = 0  ;  k < Ai->size ; k++)
	{ 
	  j = Ai->J[k];
	  (Loc->D[j]->iw)++;
	}
    } 
  /* Space allocation for Rows */
  for (  i  = 0 ;  i  < Loc->n  ;  i++ ) 
    {
      if (nsp_spmaxpcolmatrix_resize_col(Loc,i,(int) Loc->D[i]->iw) == FAIL) return NULLSPMAXPCOL;
      Loc->D[i]->iw = 0 ; /* working storage reinit */
    } 

  /* filling the Tranposed matrix rows by rows  */
  if ( Loc->rc_type == 'r' )
    {  
      for (  i = 0 ;  i < A->n  ;  i++) 
	{
	  SpCol *Ai = A->D[i];
	  for  ( k = 0  ;  k < Ai->size ; k++)
	    { 
	      int jt;
	      j = Ai->J[k];
	      jt = Loc->D[j]->iw ;
	      Loc->D[j]->J[jt]= i;
	      Loc->D[j]->R[jt]= A->D[i]->R[k] ;
	      (Loc->D[j]->iw)++;
	    }
	}
    }
  else
    {
      for (  i = 0 ;  i < A->n  ;  i++) 
	{
	  SpCol *Ai = A->D[i];
	  for  ( k = 0  ;  k < Ai->size ; k++)
	    { 
	      int jt;
	      j = Ai->J[k];
	      jt = Loc->D[j]->iw ;
	      Loc->D[j]->J[jt]= i;
	      Loc->D[j]->C[jt].r = A->D[i]->C[k].r  ;
	      Loc->D[j]->C[jt].i = - A->D[i]->C[k].i  ; 
	      (Loc->D[j]->iw)++;
	    }
	}
    }
  return Loc;
} 

/*
 * Utilities for term to term operations 
 */

#if 0
static double plus(double x, double y, double xi, double yi, double *ival, char type)
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

static void PlusLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
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

static void PlusBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
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


static void PlusRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
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

static void MinusLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
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

static void MinusBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
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


static void MinusRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
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

static void MultttLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
{
}

static void MultttBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
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

static void MultttRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
{
}

/*
 * Divtt ( term to term multiplication ) 
 */

static void DivttLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
{
  /*  A(i,j) != 0  and B(i,j) == 0 */
  if ( Ltype == 'r') 
    Li->R[*count] = Ai->R[k1]/0.0;
  else 
    { 
      if ( Atype == 'r' )
	{
	  Li->C[*count].r =  Ai->R[k1]/0.0;
	  Li->C[*count].i = 0.00/0.0;
	}
      else 
	{
	  Li->C[*count].r =  Ai->C[k1].r/0.0;
	  Li->C[*count].i =  Ai->C[k1].i/0.0;
	}
    }
  (*count)++;

}

static void DivttBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
{
  if ( Ltype == 'r') 
    {
      Li->R[*count] =  Ai->R[k1]/Bi->R[k2];
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
  else 
    {
      if ( Btype == 'r' )
	{
	  Li->C[*count].r =  Ai->C[k1].r/Bi->R[k2];
	  Li->C[*count].i =  Ai->C[k1].i/Bi->R[k2];
	}
      else 
	{
	  if ( Atype == 'r' ) 
	    {
	      nsp_div_dc(Ai->R[k1],&Bi->C[k2],&Li->C[*count]);
	    }
	  else 
	    {
	      nsp_div_cc(&Ai->C[k1],&Bi->C[k2],&Li->C[*count]);
	    }
	}
      if ( Li->C[*count].r  != 0.00||  Li->C[*count].i  != 0.00 ) (*count)++; 
    }
}

static void DivttBothNull(SpCol *Li, char Ltype, int *count)
{
  double d= 0;
  if ( Ltype == 'r') 
    {
      Li->R[*count] =  d/d;
    }
  else 
    {
      Li->C[*count].r =       Li->C[*count].i = d/d;
    }
  (*count)++; 
}

static void DivttRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
{
  /* DivttRight  is used when A(i,j) == 0  and B(i,j) != 0 */
  if ( Ltype == 'r') 
    {
      Li->R[*count] = 0.0/ Bi->R[k1];
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
  else 
    {
      if ( Btype == 'r' )
	{
	  Li->C[*count].r = Li->C[*count].i = 0/ Bi->R[k1];
	  if ( Li->C[*count].r != 0.00 ) (*count)++; 
	}
      else 
	{
	  nsp_div_dc(0.0,&Bi->C[k1],&Li->C[*count]);
	  if ( Li->C[*count].r != 0.00 ||  Li->C[*count].i != 0.00  ) (*count)++; 
	}
    }
}



/*
 * and 
 */

static void AndLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
{
  (*count)++;
}

static void AndBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
{
  if ( Atype == 'r') 
    {
      if ( Btype == 'r' )
	Li->R[*count] =  (Ai->R[k1] != 0 ) && (Bi->R[k2] != 0) ;
      else
	Li->R[*count] =  (Ai->R[k1] != 0 ) && ( Bi->C[k2].i != 0 ||  Bi->C[k2].r != 0) ;
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
  else 
    {
      if ( Btype == 'r' )
	Li->R[*count] =  ( Ai->C[k1].i != 0 ||  Ai->C[k1].i != 0) && (Bi->R[k2] != 0);
      else 
	Li->R[*count] =  ( Ai->C[k1].i != 0 ||  Ai->C[k1].i != 0) && ( Bi->C[k2].i != 0 ||  Bi->C[k2].r != 0);
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
}


static void AndRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
{
  (*count)++;
}


/*
 * or 
 */

static void OrRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
{
  if ( Btype == 'r') 
    Li->R[*count] = (Bi->R[k1] != 0 );
  else 
    Li->R[*count] = ( Bi->C[k1].i != 0 ||  Bi->C[k1].i != 0);
   if ( Li->R[*count] != 0.00 ) (*count)++;
}

static void OrLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
{
  if ( Atype == 'r') 
    Li->R[*count] = (Ai->R[k1] != 0 );
  else 
    Li->R[*count] = ( Ai->C[k1].i != 0 ||  Ai->C[k1].i != 0);
   if ( Li->R[*count] != 0.00 ) (*count)++;
}

static void OrBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
{
  if ( Atype == 'r') 
    {
      if ( Btype == 'r' )
	Li->R[*count] =  (Ai->R[k1] != 0 ) || (Bi->R[k2] != 0) ;
      else
	Li->R[*count] =  (Ai->R[k1] != 0 ) || ( Bi->C[k2].i != 0 ||  Bi->C[k2].r != 0) ;
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
  else 
    {
      if ( Btype == 'r' )
	Li->R[*count] =  ( Ai->C[k1].i != 0 ||  Ai->C[k2].i != 0) || (Bi->R[k2] != 0);
      else 
	Li->R[*count] =  ( Ai->C[k1].i != 0 ||  Ai->C[k2].i != 0) || ( Bi->C[k2].i != 0 ||  Bi->C[k2].r != 0);
      if ( Li->R[*count] != 0.00 ) (*count)++; 
    }
}



/**
 * nsp_spmaxpcolmatrix_add:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * A+B
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_add(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  return(BinaryOp(A,B,PlusLeft,PlusBoth,PlusRight,FALSE));
}

/**
 * nsp_spmaxpcolmatrix_sub:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * A-B
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/
NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_sub(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  return(BinaryOp(A,B,MinusLeft,MinusBoth,MinusRight,FALSE));
}

/**
 * nsp_spmaxpcolmatrix_multtt:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * A.*B
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_multtt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  return(BinaryOp(A,B,MultttLeft,MultttBoth,MultttRight,FALSE));
}

/**
 * nsp_spmaxpcolmatrix_and:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * A.*B
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_and(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  return(BinaryOp(A,B,AndLeft,AndBoth,AndRight,TRUE));
}

/**
 * nsp_spmaxpcolmatrix_or:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * A.*B
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_or(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  return(BinaryOp(A,B,OrLeft,OrBoth,OrRight,TRUE));
}





/**
 * nsp_spmaxpcolmatrix_divel:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * A ./ B 
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

/* XXXXX : BinaryOp is not valid here because common zero will give 
 * 0 and should give Nan 
 */

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_divel(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  return(BinaryOp_bis(A,B,DivttLeft,DivttBoth,DivttBothNull,DivttRight));
}

/* A ./ sparse(0) 
 * scalar 0
 */ 

NspSpMaxpColMatrix * nsp_spmaxpcolmatrix_div_zero_tt(NspSpMaxpColMatrix *A)
{
  double zero = 0;
  int i,j;
  NspSpMaxpColMatrix *Loc;
  if (( Loc=nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
  for ( i = 0 ; i < Loc->n ; i++ )  
    {
      if (nsp_spmaxpcolmatrix_resize_col(Loc,i,A->m)== FAIL) return NULLSPMAXPCOL;
      for ( j = 0 ; j < Loc->D[i]->size ; j++ ) 
	{
	  Loc->D[i]->J[j]=j;
	  if ( Loc->rc_type == 'r' ) 
	    Loc->D[i]->R[j]= 0/ zero;
	  else 
	    Loc->D[i]->C[j].r = Loc->D[i]->C[j].i = 0/ zero;
	}
      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	{
	  int k = A->D[i]->J[j];
	  if ( Loc->rc_type == 'r' ) 
	    Loc->D[i]->R[j] = A->D[i]->R[k]/ zero;
	  else 
	    {
	      Loc->D[i]->C[j].r = A->D[i]->C[k].r / zero;
	      Loc->D[i]->C[j].i = A->D[i]->C[k].i / zero;
	    }
	}
    }
  return Loc;
}

/* A ./ sparse(x) x!= 0 scalar 
 *
 */ 

int nsp_spmaxpcolmatrix_div_scal_tt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  int i,k,ndel;
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_spmaxpcolmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  for (  i  = 0 ;  i  < A->n  ;  i++ ) 
    {
      int compress=0;
      doubleC C;
      SpCol *Ai = A->D[i];
      for  ( k = 0  ;  k < Ai->size ; k++)
	{ 
	  if ( A->rc_type == 'r' ) 
	    {
	      if ( B->rc_type == 'r' )
		{
		  Ai->R[k] /= B->D[0]->R[0];
		  if ( Ai->R[k]== 0) 
		    {
		      Ai->J[k]= -1; compress=1;
		    }
		}
	      else 
		{
		  Scierror("Error: we should never get there ! \n");
		  return FAIL;    
		}
	    }
	  else 
	    {
	      if ( B->rc_type == 'r' )
		{
		  Ai->C[k].r /= B->D[0]->R[0];
		  Ai->C[k].i /= B->D[0]->R[0];
		  if ( Ai->C[k].i == 0 && Ai->C[k].r==0 )
		    {
		      compress=1;Ai->J[k]= -1;
		    }
		} 
	      else 
		{
		  nsp_div_cc(&Ai->C[k],&B->D[0]->C[0],&C);
		  Ai->C[k]= C;
		  if ( Ai->C[k].i == 0 && Ai->C[k].r==0 ) 
		    {
		      Ai->J[k]= -1;
		      compress=1;
		    }
		}
	    }
	}
      /* Note that 0 can appear as result to division for example 6/%inf 
       */
      if ( compress == 1 ) 
	{
	  ndel =nsp_spmaxpcolmatrix_compress_col_simple(A,i);
	  if (nsp_spmaxpcolmatrix_resize_col(A,i, Ai->size-ndel ) == FAIL) return(FAIL) ;
	}
    }
  return OK ;
}

/* A ./ B and A is 1x1 scalar and != 0 
 *
 */ 

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_scal_div_tt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  NspSpMaxpColMatrix *Loc;
  int i,k,ndel;
  doubleC czero={0,0};
  double zero=0;
  char t = 'r';
  double Ar = 0;
  doubleC Ac = {0,0};
  if ( A->D[0]->size == 1 ) 
    {
      if ( A->rc_type == 'r' ) 
	Ar = A->D[0]->R[0];
      else 
	Ac = A->D[0]->C[0];
    }

  if ( A->rc_type == 'c' || B->rc_type == 'c' ) t= 'c';
  if (( Loc=nsp_spmaxpcolmatrix_create(NVOID,t,B->m,B->n)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
  for (  i  = 0 ;  i  < Loc->n  ;  i++ ) 
    {
      int compress=0;
      doubleC C;
      SpCol *Bi = B->D[i];
      SpCol *Li;
      if (nsp_spmaxpcolmatrix_resize_col(Loc,i,B->m)== FAIL) return NULLSPMAXPCOL;
      Li = Loc->D[i];
      if (  A->rc_type == 'r' ) 
	{
	  for  ( k = 0  ;  k < Li->size ; k++)
	    {
	      if ( t == 'r' )
		Li->R[k] = Ar / zero;
	      else 
		{
		  nsp_div_dc(Ar ,&czero,&Li->C[k]);
		}
	      Li->J[k] = k;
	    }
	}
      else 
	{
	  for  ( k = 0  ;  k < Li->size ; k++)
	    {
	      nsp_div_cc(&Ac,&czero,&Li->C[k]);
	      Li->J[k] = k;
	    }
	}
      for  ( k = 0  ;  k < Bi->size ; k++)
	{ 
	  int j = Bi->J[k];
	  if ( A->rc_type == 'r' ) 
	    {
	      if ( B->rc_type == 'r' )
		{
		  Li->R[j] = Ar / Bi->R[j];
		  if ( Li->R[j]== 0) 
		    {
		      Li->J[j]= -1; compress=1;
		    }
		}
	      else 
		{
		  nsp_div_dc(Ar ,&Bi->C[j],&Li->C[j]);
		  if ( Li->C[j].r == 0 && Li->C[j].i == 0) 
		    {
		      Li->J[j]= -1; compress=1;
		    }
		}
	    }
	  else 
	    {
	      if ( B->rc_type == 'r' )
		{
		  Li->C[j].r = Ac.r / Bi->R[j];
		  Li->C[j].i = Ac.i / Bi->R[j];
		  if ( Li->C[j].i == 0 && Li->C[j].r==0 )
		    {
		      compress=1;Li->J[j]= -1;
		    }
		} 
	      else 
		{
		  nsp_div_cc(&Ac,&Bi->C[j],&C);
		  Li->C[j]= C;
		  if ( Li->C[j].i == 0 && Li->C[j].r==0 ) 
		    {
		      Li->J[j]= -1;
		      compress=1;
		    }
		}
	    }
	}
      /* Note that 0 can appear as result to division for example 6/%inf 
       */
      if ( compress == 1 ) 
	{
	  ndel =nsp_spmaxpcolmatrix_compress_col_simple(Loc,i);
	  if (nsp_spmaxpcolmatrix_resize_col(Loc,i, Li->size-ndel ) == FAIL) return NULL;
	}
    }
  return Loc ;
}



#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/**
 * BinaryOp:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * @BinLeft: a function 
 * @BinBoth: a function 
 * @BinRight: a function 
 * @force_real: an integer to force the result not to be complex.
 * 
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
 * 
 * This function implicitly use the fact that 0 op 0 returns 0
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

static NspSpMaxpColMatrix *BinaryOp(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, BopLeft BinLeft, BopBoth BinBoth, BopRight BinRight,
				int force_real)
{ 
  int i,count,k1,k2,k;
  NspSpMaxpColMatrix *Loc;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
      if ( force_real ) type ='r';
      Loc =nsp_spmaxpcolmatrix_create(NVOID,type, A->m,A->n);
      if ( Loc == NULLSPMAXPCOL ) return(NULLSPMAXPCOL) ; 
      for ( i = 0 ; i < Loc->n ; i++ ) 
	{
	  int iest;
	  SpCol *Ai = A->D[i];
	  SpCol *Bi = B->D[i];
	  SpCol *Li = Loc->D[i];
	  iest= Min( A->m, A->D[i]->size+B->D[i]->size);
	  if (nsp_spmaxpcolmatrix_resize_col(Loc,i,(int)iest ) == FAIL) return(NULLSPMAXPCOL) ;
	  /* We explore the ith column of A and B in increasing order of row 
	   *  and want to merge the rows found ( in increasing order ) 
	   *  when a same row number appear in both A and B we call the 
	   *  2-ary operator op 
	   *  This is very near to a merge sort of two sorted arrays 
	   */ 
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
	  if (nsp_spmaxpcolmatrix_resize_col(Loc,i,count) == FAIL) return(NULLSPMAXPCOL) ;
	}
      return(Loc);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      return(NULLSPMAXPCOL);
    }
}

/**
 * BinaryOp_bis:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * @BinLeft: a function 
 * @BinBoth: a function 
 * @BinBothNull: a function 
 * @BinRight: a function 
 * 
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
 *    BopBothNull for performing 0 op 0 
 * each Bopxxx functions performs the operation, stores 
 *    the result (if #0) in the sparse matrix Loc 
 *    and increment a counter for counting #0 elements 
 * 
 * This function implicitly use the fact that 0 op 0 returns 0
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

static NspSpMaxpColMatrix *BinaryOp_bis(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, BopLeft BinLeft, BopBoth BinBoth, 
				    BopBothNull BinBothNull, BopRight BinRight)
{ 
  int i,count,k1,k2,k,j3;
  NspSpMaxpColMatrix *Loc;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
      Loc =nsp_spmaxpcolmatrix_create(NVOID,type, A->m,A->n);
      if ( Loc == NULLSPMAXPCOL ) return(NULLSPMAXPCOL) ; 
      for ( i = 0 ; i < Loc->n ; i++ ) 
	{
	  int iest,lx;
	  SpCol *Ai = A->D[i];
	  SpCol *Bi = B->D[i];
	  SpCol *Li = Loc->D[i];
	  /* since here 0 op 0 can return non zero values */
	  iest=  A->m; 
	  if (nsp_spmaxpcolmatrix_resize_col(Loc,i,(int)iest ) == FAIL) return(NULLSPMAXPCOL) ;
	  /* We explore the ith column of A and B in increasing order of row 
	   *  and want to merge the rows found ( in increasing order ) 
	   *  when a same row number appear in both A and B we call the 
	   *  2-ary operator op 
	   *  This is very near to a merge sort of two sorted arrays 
	   */ 
	  k1 = 0 ; k2 = 0 ; lx=0;
	  count = 0;
	  /* merge part */
	  while ( k1 < Ai->size && k2 <  Bi->size) 
	    { 
	      int j1,j2;
	      j1 = Ai->J[k1] ;
	      j2 = Bi->J[k2] ;
	      for ( j3 = lx ; j3 < Min(j1,j2) ; j3++)
		{
		  Li->J[count] = j3;
		  (*BinBothNull)(Li,Loc->rc_type,&count);
		}
	      lx = Min(j1,j2)+1;
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
	  /* Keep inserting remaining arguments for A */
	  for ( k = k1 ; k < Ai->size ; k++) 
	    { 
	      for ( j3 = lx ; j3 < Ai->J[k] ; j3++)
		{
		  Li->J[count] = j3;
		  (*BinBothNull)(Li,Loc->rc_type,&count);
		}
	      lx = Ai->J[k] +1;
	      Li->J[count] = Ai->J[k];
	      (*BinLeft)(Li,Loc->rc_type,&count,Ai,A->rc_type,k);
	    }
	  /* Keep inserting remaining arguments for B */
	  for ( k = k2 ; k < Bi->size ; k++) 
	    { 
	      for ( j3 = lx ; j3 < Bi->J[k] ; j3++)
		{
		  Li->J[count] = j3;
		  (*BinBothNull)(Li,Loc->rc_type,&count);
		}
	      lx = Bi->J[k] +1;
	      Li->J[count] = Bi->J[k] ;
	      (*BinRight)(Li,Loc->rc_type,&count,Bi,B->rc_type,k);
	    }
	  /* keep inserting the last 0 op 0 */
	  for ( j3 = lx ; j3 < A->m ; j3++)
	    {
	      Li->J[count] = j3;
	      (*BinBothNull)(Li,Loc->rc_type,&count);
	    }
	  /* Count doit maintenant nous donner la taille de la ligne i */
	  /* Resizer Loc->D[i] : car on peut avoir moins d'el que pr'evus */
	  if (nsp_spmaxpcolmatrix_resize_col(Loc,i,count) == FAIL) return(NULLSPMAXPCOL) ;
	}
      return(Loc);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      return(NULLSPMAXPCOL);
    }
}

/**
 * nsp_spmaxpcolmatrix_mult_scal:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * 
 * A = A.*B where B is a scalar sparse ( [] or scalar )
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_mult_scal(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{ 
  int i,k;
  if ( (B->m == 0 || B->n == 0) || B->D[0]->size == 0)
    {
      /* B is [] or [0] **/
      /* Change A to [] sparse **/
      for ( i=0 ; i < A->n ; i++ ) 
	{
	  if ( A->D[i]->size != 0 ) 
	    {
	      FREE( A->D[i]->J);
	      FREE( A->D[i]->R);
	    }
	  A->D[i]->size =0;
	}
      FREE(A->D);
      A->m = A->n = 0 ; /* A->mn = 0; */
      return OK;
    }
  if ( B->rc_type == 'c' )
    {
      if (nsp_spmaxpcolmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  /* Now B is a non null scalar **/
  if ( A->rc_type == 'r' )
    {
      /* here B is alway real **/
      for ( i = 0 ; i < A->n ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  A->D[i]->R[k] *= B->D[0]->R[0];
    }
  else
    {
      if ( B->rc_type == 'r' ) 
	for ( i = 0 ; i < A->n ; i++ ) 
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      A->D[i]->C[k].r *= B->D[0]->R[0];
	      A->D[i]->C[k].i *= B->D[0]->R[0];
	    }
      else
	for ( i = 0 ; i < A->n ; i++ ) 
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
 * nsp_spmaxpcolmatrix_op_scal:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * @flag: int pointer 
 * @op: a char
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
 * Return value: a new  #NspMatrix or %NULLMAT
 **/

NspMatrix *nsp_spmaxpcolmatrix_op_scal(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, int *flag, char op)
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
	    for ( i = 0 ; i < A->n ; i++ ) 
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  j= A->D[i]->J[k];
		  Loc->R[i*Loc->m+j] += A->D[i]->R[k];
		}
	  else 
	    for ( i = 0 ; i < A->n ; i++ ) 
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  j= A->D[i]->J[k];
		  Loc->C[i*Loc->m+j].r += A->D[i]->R[k];
		}
	}
      else
	{
	  for ( i = 0 ; i < A->n ; i++ ) 
	    for ( k = 0 ; k < A->D[i]->size ; k++) 
	      {
		j= A->D[i]->J[k];
		Loc->C[i*Loc->m+j].r +=   A->D[i]->C[k].r;
		Loc->C[i*Loc->m+j].i +=   A->D[i]->C[k].i;
	      }
	}
    }
  else
    {
      if ( A->rc_type == 'r' )
	{
	  if ( Loc->rc_type == 'r' ) 
	    for ( i = 0 ; i < A->n ; i++ ) 
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  j= A->D[i]->J[k];
		  Loc->R[i*Loc->m+j] -= A->D[i]->R[k];
		}
	  else 
	    for ( i = 0 ; i < A->n ; i++ ) 
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  j= A->D[i]->J[k];
		  Loc->C[i*Loc->m+j].r -= A->D[i]->R[k];
		}
	}
      else
	{
	  for ( i = 0 ; i < A->n ; i++ ) 
	    for ( k = 0 ; k < A->D[i]->size ; k++) 
	      {
		j= A->D[i]->J[k];
		Loc->C[i*Loc->m+j].r -=   A->D[i]->C[k].r;
		Loc->C[i*Loc->m+j].i -=   A->D[i]->C[k].i;
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
  return ( (NspSpMaxpColMatrix *) M)->rc_type;
}

static int SpNext(const void *M, double *r, doubleC *c,int *work)
{
  const NspSpMaxpColMatrix *Sp= M;
  if ( work[0] == -1 ) 
    {
      /* Return first a zero value **/
      switch (Sp->rc_type) 
	{
	case 'r' : *r = 0.00;break;
	case 'c' : c->r = c->i = 0.00;break;
	}
      work[0]++;
      /* maybe we could decide not to use the value 0 since this 
       * function is just to decide a format and zeros are not displayed
       * 
       */
      return 1;
    }
  /* Now return the non nul elements **/
  if ( work[0] == Sp->n ) return 0;
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
      /* find next nonempty column */
      while (1) 
	{
	  work[0]++;
	  if ( work[0] >= Sp->n) return (0);
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

static void Sp_set_format(nsp_num_formats *fmt,NspSpMaxpColMatrix *M)
{
  gen_set_format(fmt,M,Sp_any_element_is_negative,
		 Sp_any_element_is_inf_or_nan,
		 Sp_pr_min_max_internal,
		 Sp_all_elements_are_int_or_inf_or_nan,
		 SpInit);
}

/* Sparse Matrix with + format : both real and complex cases **/

static void SpM_plus_format(NspSpMaxpColMatrix *Sp, int indent)
{
  int i,j;
  for ( i = 0; i < Sp->m; i++)
    {
      int col=0;
      SpCol *Ri = Sp->D[i];
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

static int SpM_general(nsp_num_formats *fmt,NspSpMaxpColMatrix *Sp, int indent)
{
  int i,j,imore,p_rows=0;
  int max_width ,winrows;
  sci_get_screen_size(&winrows,&max_width);
  switch ( Sp->rc_type ) 
    {
    case 'r' : 
      for ( i = 0; i < Sp->n; i++)
	{
	  SpCol *Ri = Sp->D[i];
	  for ( j = 0; j < Ri->size ; j++)
	    {
	      nsp_pr_white(indent) ;Sciprintf("(%d,%d) ",Ri->J[j]+1,i+1);
	      nsp_pr_float(fmt, Ri->R[j], FALSE );Sciprintf("\n");
	      p_rows++;
	      if ( p_rows >= winrows ) 
		{
		  scimore(&imore);
		  if ( imore == 1) return FALSE;
		  p_rows=0;
		}
	    }
	}
      break;
    case 'c' :
      for ( i = 0; i < Sp->n; i++)
	{
	  SpCol *Ri = Sp->D[i];
	  for ( j = 0; j < Ri->size ; j++)
	    {
	      nsp_pr_white(indent) ; Sciprintf("(%d,%d) ",Ri->J[j]+1,i+1);
	      nsp_pr_complex(fmt, Ri->C[j], FALSE );
	      Sciprintf("\n");
	      p_rows++;
	      if ( p_rows >= winrows ) 
		{
		  scimore(&imore);
		  if ( imore == 1) return FALSE;
		  p_rows=0;
		}
	    }
	}
      break;
    }
  return TRUE;
}

static int nsp_spmaxpcolmatrix_print_internal(nsp_num_formats *fmt,NspSpMaxpColMatrix *m, int indent)
{
  int rep = TRUE;
  if ( m->m ==0 || m->n == 0) 
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
	  rep =SpM_general(fmt,m,indent);
	}
    }
  return rep;
}

/* Operations XXX
 *
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/**
 * nsp_spmaxpcolmatrix_clean:
 * @A: a #NspSpMaxpColMatrix
 * @rhs: an integer 
 * @epsa: absolute precision 
 * @epsr: relative precision 
 * 
 * A = Matclean(a) clean A according to epsa and epsr 
 * epsa is used if rhs >= 2 
 * epsr is used if rhs >= 3
 * A is changed, 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_clean(NspSpMaxpColMatrix *A, int rhs, double epsa, double epsr)
{
  int j,i,n;
  double d_epsa=( rhs >= 2 ) ? epsa: DBL_EPSILON;
  double d_epsr=( rhs >= 3 ) ? epsr: DBL_EPSILON;
  double norm,eps;
  int inc=1;
  if ( A->rc_type == 'r') 
    {
      norm=0.0;
      for ( i = 0 ; i < A->n ; i++) 
	if ( A->D[i]->size != 0) 
	  norm += C2F(dasum)(&A->D[i]->size,A->D[i]->R,&inc);
    }
  else
    {
      norm=0.0;
      for ( i = 0 ; i < A->n ; i++) 
	if ( A->D[i]->size != 0) 
	  norm +=nsp_zasum(&A->D[i]->size,A->D[i]->C,&inc);
    }
  eps= Max(d_epsa,d_epsr*norm);
  for ( i = 0 ; i < A->n  ; i++ ) 
    {
      n =0;
      for ( j = 0 ; j < A->D[i]->size ; j++) 
	{
	  switch ( A->rc_type ) 
	    {
	    case 'r' :
	      if ( Abs(A->D[i]->R[j])   < eps) A->D[i]->J[j] = -1;
	      n=1; break ;
	    case 'c' : 
	      /* using complex absolute value  */
	      if (nsp_abs_c(&A->D[i]->C[j]) < eps) A->D[i]->J[j] = -1;
	      n=1; break ;
	      /* 
	       * if ( Abs(A->D[i]->C[j].r) < eps ) A->D[i]->C[j].r = 0.0;
	       * if ( Abs(A->D[i]->C[j].i) < eps ) A->D[i]->C[j].i = 0.0;
	       * if ( A->D[i]->C[j].r == 0.0 && A->D[i]->C[j].i == 0.0) 
	       *  { A->D[i]->J[j] = -1;n=1;break;}
	       */
	    }
	}
      /* remove null elements and resize rows **/
      if ( n != 0 ) 
	{
	  int ndel =nsp_spmaxpcolmatrix_compress_col_simple(A,i);
	  if (nsp_spmaxpcolmatrix_resize_col(A,i, A->D[i]->size-ndel ) == FAIL) return(FAIL) ;
	}
    }
  return OK;
}


/**
 * nsp_spmaxpcolmatrix_maximinitt_g:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * @flag: an integer 
 * @minmaxflag: an integer 
 * @err: an int pointer 
 * 
 * max or min (A,B)
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *
nsp_spmaxpcolmatrix_maximinitt_g(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, int flag, int minmaxflag, int *err)
{
  /* Same philosophy as in BinaryOp **/
  int i,count,icount,k1,k2,k;
  NspSpMaxpColMatrix *Loc,*ILoc;
  NspSpMaxpColMatrix *Indi=NULL;
  char type = 'r';
  *err=FALSE;
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) 
	{
	  Scierror("Error: Arguments must be real matrices\n");
	  goto err;
	}
      /* storing indices **/
      if ( flag == 1) 
	{
	  if (( Indi = nsp_spmaxpcolmatrix_create(NVOID,'r',A->m,A->n)) == NULLSPMAXPCOL ) 
	    goto err;
	}
      /* Buffer */
      if ((Loc =nsp_spmaxpcolmatrix_create(NVOID,type,A->m,1)) == NULLSPMAXPCOL ) goto err;
      if (nsp_spmaxpcolmatrix_resize_col(Loc,0,A->m ) == FAIL) goto err;
      /* Buffer for indices */
      if ((ILoc =nsp_spmaxpcolmatrix_create(NVOID,type,A->m,1)) == NULLSPMAXPCOL ) goto err;
      if (nsp_spmaxpcolmatrix_resize_col(ILoc,0,A->m ) == FAIL) goto err;
      
      for ( i = 0 ; i < A->n ; i++ ) 
	{
	  SpCol *Ai = A->D[i];
	  SpCol *Bi = B->D[i];
	  /* We explore the ith column of A and B in increasing order of rows
	   *  and want to merge the rows found ( in increasing order ) 
	   *  when a same row number appear in both A and B we call the 
	   *  2-ary operator op 
	   *  This is very near to a merge sort of two sorted arrays 
	   */ 
	  k1 = 0 ; k2 = 0 ; 
	  count = 0 ;
	  icount= 0;
	  /* merge part **/
	  while ( k1 < Ai->size && k2 <  Bi->size) 
	    { 
	      int j1,j2;
	      j1 = Ai->J[k1] ;
	      j2 = Bi->J[k2] ;
	      if ( j1 < j2 ) 
		{ 
		  /* A != 0 and B == 0 **/
		  if ( minmaxflag*Ai->R[k1] > 0 ) 
		    {
		      Loc->D[0]->J[count] = j1;
		      Loc->D[0]->R[count] = Ai->R[k1];
		      count++;
		      if ( flag == 1) 
			{
			  ILoc->D[0]->J[icount] = j1;
			  ILoc->D[0]->R[icount] = 1;
			  icount++;
			}
		    }
		  else 
		    {
		      if ( flag == 1) 
			{
			  ILoc->D[0]->J[icount] = j1;
			  ILoc->D[0]->R[icount] = 2;
			  icount++;
			}
		    }
		  k1++; 
		}
	      else if ( j1 == j2 ) 
		{ 
		  /* A != 0 and B != 0 **/
		  Loc->D[0]->J[count] = j1;
		  if (  minmaxflag*Ai->R[k1] >=   minmaxflag*Bi->R[k2] )
		    {
		      Loc->D[0]->R[count]=  Ai->R[k1];
		      if ( flag == 1) 
			{
			  ILoc->D[0]->J[icount] = j1;
			  ILoc->D[0]->R[icount] = 1;
			}
		    }
		  else
		    {
		      Loc->D[0]->R[count]=  Bi->R[k2];
		      if ( flag == 1) 
			{
			  ILoc->D[0]->J[icount] = j1;
			  ILoc->D[0]->R[icount] = 2;
			}
		    }
		  count++;
		  icount++;
		  k1++; k2 ++; 
		}
	      else 
		{ 
		  /* A == 0 and B != 0 **/
		  if (minmaxflag* Bi->R[k2] > 0 ) 
		    {
		      Loc->D[0]->J[count] = j2;
		      Loc->D[0]->R[count] = Bi->R[k2];
		      count++;
		      if ( flag == 1) 
			{
			  ILoc->D[0]->J[icount] = j2;
			  ILoc->D[0]->R[icount] = 2;
			  icount++;
			}
		    }
		  else
		    {
		      if ( flag == 1) 
			{
			  ILoc->D[0]->J[icount] = j2;
			  ILoc->D[0]->R[icount] = 1;
			  icount++;
			}
		    }
		  k2++;
		}
	    }
	  /* Keep inserting remaining arguments for A **/
	  for ( k = k1 ; k < Ai->size ; k++) 
	    { 
	      if ( minmaxflag*Ai->R[k] > 0 ) 
		{
		  Loc->D[0]->J[count] = Ai->J[k];
		  Loc->D[0]->R[count] = Ai->R[k];
		  count++;
		  if ( flag == 1) 
		    {
		      ILoc->D[0]->J[icount] = Ai->J[k];
		      ILoc->D[0]->R[icount] = 1;
		      icount++;
		    }
		  
		}
	      else
		{
		  if ( flag == 1) 
		    {
		      ILoc->D[0]->J[icount] = Ai->J[k];
		      ILoc->D[0]->R[icount] = 2;
		      icount++;
		    }
		}
	    }
	  /* Keep inserting remaining arguments for B **/
	  for ( k = k2 ; k < Bi->size ; k++) 
	    { 
	      if ( minmaxflag* Bi->R[k] > 0 ) 
		{
		  Loc->D[0]->J[count] = Bi->J[k];
		  Loc->D[0]->R[count] = Bi->R[k];
		  count++;
		  if ( flag == 1) 
		    {
		      ILoc->D[0]->J[icount] = Bi->J[k];
		      ILoc->D[0]->R[icount] = 2;
		      icount++;
		    }
		}
	      else
		{
		  if ( flag == 1) 
		    {
		      ILoc->D[0]->J[icount] = Bi->J[k];
		      ILoc->D[0]->R[icount] = 1;
		      icount++;
		    }
		}
	    }
	  /* count is not set to the proper ith row dimension  */
	  /* we resize A(i,:) and store Loc  **/
	  if (nsp_spmaxpcolmatrix_resize_col(A,i,count)  == FAIL)
	    goto err;
	  /* use icopy and dcopy XXXX */
	  for ( k =0 ; k < A->D[i]->size ; k++) 
	    {
	      A->D[i]->J[k] = Loc->D[0]->J[k];
	      A->D[i]->R[k] = Loc->D[0]->R[k];
	    }
	  /* idem for max */
	  if ( flag == 1)
	    {
	      if (nsp_spmaxpcolmatrix_resize_col(Indi,i,icount)  == FAIL) goto err;
	      /* use icopy and dcopy XXXX */
	      for ( k =0 ; k < Indi->D[i]->size ; k++) 
		{
		  Indi->D[i]->J[k] = ILoc->D[0]->J[k];
		  Indi->D[i]->R[k] = ILoc->D[0]->R[k];
		}
	    }
	}
      nsp_spmaxpcolmatrix_destroy(Loc);
      nsp_spmaxpcolmatrix_destroy(ILoc);
      return(Indi);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      goto err;
    }
 err:
  *err=TRUE;
  return NULLSPMAXPCOL;
}


/**
 * nsp_spmaxpcolmatrix_maxitt:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
 * @flag: an integer 
 * @err: an int pointer 
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
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_maxitt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, int flag, int *err)
{
  return nsp_spmaxpcolmatrix_maximinitt_g(A,B,flag,1,err);
}


/**
 * nsp_spmaxpcolmatrix_minitt:
 * @A: a #NspSpMaxpColMatrix
 * @B: a #NspSpMaxpColMatrix
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
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_minitt(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B, int flag, int *err)
{
  return nsp_spmaxpcolmatrix_maximinitt_g(A,B,flag,-1,err);
}

/**
 * nsp_spmaxpcol_resize:
 * @col: 
 * @n: 
 * @rc_type: 
 * 
 * allocate or reallocate a sparse column structure
 * 
 * Return value:  %OK or %FAIL
 **/
      
static int nsp_spmaxpcol_resize(SpCol **col, int n, char rc_type)
{
  int cp = ( rc_type == 'c') ? 2 : 1;
  if ( *col == NULL ) 
    {
      if ((*col= MALLOC(sizeof(SpCol))) == NULL) return FAIL; 
      (*col)->size=0;
    }

  if ( (*col)->size == 0 ) 
    {
      if ( n <= 0 ) return(OK);
      if (((*col)->J =nsp_alloc_int((int) n)) == (int *) 0) return(FAIL);
      /* note that all data are in a union */
      if (((*col)->R =nsp_alloc_doubles(n*cp)) == (double *) 0 ) return(FAIL);
      (*col)->size = n;
      return(OK);
    }
  if ( (*col)->size == n  ) return(OK);
  if ( n <= 0 ) 
    {
      /* empty new size **/
      FREE((*col)->J);
      FREE((*col)->R);
      (*col)->size = 0;
      return(OK);
    }
  if (((*col)->J =nsp_realloc_int((*col)->J, n))  == (int *) 0) return(FAIL);
  if (( (*col)->R =nsp_realloc_doubles((*col)->R, n*cp)) == (double *) 0 ) return(FAIL);
  (*col)->size = n;
  return(OK);
}

/**
 * nsp_spmaxpcolmatrix_realpart:
 * @A: a #NspSpMaxpColMatrix
 * 
 * Return the Real part of Matrix A in A.
 * 
 * Return value:  %OK or %FAIL
 **/




int nsp_spmaxpcolmatrix_realpart(NspSpMaxpColMatrix *A)
{
  SpCol *col=NULL;
  int i,k;
  if ( A->rc_type == 'r' )  return(OK);
  for ( i=0 ; i < A->n ; i++)
    {
      col = NULL;
      /* count non null elements in column i */
      int count =0; 
      for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	{
	  if ( A->D[i]->C[k].r != 0.0) count++;
	}
      if ( nsp_spmaxpcol_resize(&col,count,'r')== FAIL) return FAIL;
      count =0;
      for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	{
	  if ( A->D[i]->C[k].r != 0.0) 
	    { 
	      col->R[count] = A->D[i]->C[k].r; 
	      col->J[count] = A->D[i]->J[k]; 
	      count++;
	    }
	}
      nsp_spmaxpcolmatrix_col_destroy(A->D[i]);
      A->D[i]= col;
    }
  A->rc_type = 'r';
  return(OK);
}

/**
 * nsp_spmaxpcolmatrix_imagpart:
 * @A: a #NspSpMaxpColMatrix
 * 
 * Return the Imaginary part of Matrix A in A.
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_imagpart(NspSpMaxpColMatrix *A)
{
  SpCol *col=NULL;
  int i,k;
  if ( A->rc_type == 'r')
    {
      for ( i=0 ; i < A->n ; i++ ) 
	{
	  if ( A->D[i]->size != 0 ) 
	    {
	      FREE( A->D[i]->J);
	      FREE( A->D[i]->R);
	      /* FREE( A->D[i]->C); */
	    }
	  A->D[i]->size =0;
	}
    }
  else
    {
      for ( i=0 ; i < A->n ; i++)
	{
	  int count =0; 
	  col=NULL;
	  for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	    {
	      if ( A->D[i]->C[k].i != 0.0) count++;
	    }
	  if ( nsp_spmaxpcol_resize(&col,count,'r')== FAIL) return FAIL;
	  count =0;
	  for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	    {
	      if ( A->D[i]->C[k].i != 0.0) 
		{ 
		  col->R[count] = A->D[i]->C[k].i; 
		  col->J[count] = A->D[i]->J[k]; 
		  count++;
		}
	    }
	  nsp_spmaxpcolmatrix_col_destroy(A->D[i]);
	  A->D[i]= col;
	}
      A->rc_type = 'r';
    }
  return OK;
}


/**
 * nsp_spmaxpcolmatrix_isreal:
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

int nsp_spmaxpcolmatrix_isreal(const NspSpMaxpColMatrix *A, int strict)
{
  int i,k;
  if ( A->rc_type == 'r') return TRUE;
  if ( strict == TRUE ) return FALSE;

  for ( i=0 ; i < A->n ; i++)
    {
      for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	{
	  if ( A->D[i]->C[k].i != 0.0) return FALSE;
	}
    }
  return TRUE;
}

/*
 *nsp_mat_inv_el: a(i,j)=1/a(i,j) A est changee
 */

/*
 * nsp_spmaxpcolmatrix_kron: Kronecker product of 2 sparse matrices
 * A and B are not changed
 */
NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_kron(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *B)
{
  NspSpMaxpColMatrix *C = NULLSPMAXPCOL;
  char type = 'r';
  int mC, nC, jA, jB, kA, kB, k;

  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';

  mC = A->m*B->m; 
  nC = A->n*B->n;
  if (   ((double) mC) !=  ((double)A->m) * ((double)B->m)
      || ((double) nC) !=  ((double)A->n) * ((double)B->n) )
    {
      Scierror("Error: matrix got from kronecker product is too large \n");
      return NULLSPMAXPCOL;
    }

  if ( (C =nsp_spmaxpcolmatrix_create(NVOID,type, mC, nC)) == NULLSPMAXPCOL ) 
    return NULLSPMAXPCOL; 
  
  if ( C->m == 0 || C->n ==0 ) return C;

  for ( jA = 0 ; jA < A->n ; jA++ )
    {
      SpCol *Aj = A->D[jA];
      for ( jB = 0 ; jB < B->n ; jB++ )
	{
	  // fill column jA*B->n + jB of C
	  SpCol *Bj = B->D[jB];  
	  SpCol *Cj = C->D[jA*B->n + jB];
	  if ( nsp_spmaxpcolmatrix_resize_col(C,jA*B->n + jB, Aj->size*Bj->size) == FAIL ) 
	    goto err;	  

	  for (kA = 0, k = 0  ; kA <  Aj->size  ; kA++) 
	    {
	      int iA = B->m*Aj->J[kA];
	      if ( A->rc_type == 'r' )
		{
		  double valA = Aj->R[kA];
		  if ( B->rc_type == 'r' )
		    for (kB = 0  ; kB <  Bj->size  ; kB++, k++)
		      { 
			Cj->R[k] = valA*Bj->R[kB]; 
			Cj->J[k] = iA + Bj->J[kB];
		      }
		  else  /* B is complex */
		    for (kB = 0  ; kB <  Bj->size  ; kB++, k++)
		      { 
			Cj->C[k].r = valA*Bj->C[kB].r; Cj->C[k].i = valA*Bj->C[kB].i;
			Cj->J[k] = iA + Bj->J[kB];
		      }
		}
 	      else  /* A is complex */
		{
		  doubleC valA = Aj->C[kA];
		  if ( B->rc_type == 'r' )
		    for (kB = 0  ; kB <  Bj->size  ; kB++, k++)
		      { 
			Cj->C[k].r = valA.r*Bj->R[kB]; Cj->C[k].i = valA.i*Bj->R[kB];
			Cj->J[k] = iA + Bj->J[kB];
		      }
		  else  /*  both A and B complex */
		    for (kB = 0  ; kB <  Bj->size  ; kB++, k++)
		      { 
			Cj->C[k].r = valA.r*Bj->C[kB].r - valA.i*Bj->C[kB].i; 
			Cj->C[k].i = valA.i*Bj->C[kB].r + valA.r*Bj->C[kB].i;
			Cj->J[k] = iA + Bj->J[kB];
		      }
		}
	    }
	}
    }
  return C;

 err:
  nsp_spmaxpcolmatrix_destroy(C);
  return NULLSPMAXPCOL;
}


/*
 *nsp_mat_sort: Index=Sort(A)
 * A is changed, Index created with the indexes 
 * return NULLMAT on error 
 * WARNING : A must be real but the test is not done here 
 * ======
 */

/**
 * nsp_spmaxpcolmatrix_sum:
 * @A: a #NspSpMaxpColMatrix
 * @dim: 
 * 
 * Sum = nsp_spmaxpcolmatrix_sum(A ,dim)
 *     A is unchanged 
 * if dim= 2 the sum for the column indices is computed 
 *       and a column vector is returned. 
 * if dim= 1 the sum for the row indices is computed 
 *       and a Row vector is returned.
 * if dim= 0 the full sum is computed 
 * 
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_sum(NspSpMaxpColMatrix *A, int dim)
{
  double S;
  doubleC SC,C;
  NspSpMaxpColMatrix *Sum=NULL;
  int i,k,count;
  int inc=1;

  if ( A->m ==0 || A->n == 0) 
    {
      if ( dim == 0 )
	{
	  if ((Sum = nsp_spmaxpcolmatrix_create(NVOID,'r',1,1)) == NULLSPMAXPCOL) 
	    return NULLSPMAXPCOL;
	  if (nsp_spmaxpcolmatrix_resize_col(Sum,0,1)== FAIL) return NULLSPMAXPCOL;
	  Sum->D[0]->J[0] = 0;
	  Sum->D[0]->R[0] = 0.0;
	  return Sum;
	}
      else 
	return nsp_spmaxpcolmatrix_create(NVOID,'r',0,0);
    }

  switch (dim)
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);


    case 0:   /* sum of all elements */

      if ((Sum =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,1,1)) == NULLSPMAXPCOL) 
	return NULLSPMAXPCOL;
      if ( A->rc_type == 'r' ) 
	{
	  S=0;
	  for ( i= 0 ; i < A->n ; i++ ) 
	    S +=nsp_dsum(&A->D[i]->size,A->D[i]->R,&inc);
	  if ( S != 0) 
	    {
	      if (nsp_spmaxpcolmatrix_resize_col(Sum,0,1) == FAIL) return NULLSPMAXPCOL;
	      Sum->D[0]->R[0] = S;
	      Sum->D[0]->J[0] = 0;
	    }
	}
      else
	{
	  SC.r = SC.i = 0.0;
	  for ( i= 0 ; i < A->n ; i++ ) 
	    { 
	      nsp_zsum(&C,&A->D[i]->size,A->D[i]->C,&inc); 
	      SC.r += C.r;SC.i += C.i;
	    }
	  if ( SC.r  != 0.0 ||  SC.i != 0.0) 
	    {
	      if (nsp_spmaxpcolmatrix_resize_col(Sum,0,1) == FAIL) return NULLSPMAXPCOL;
	      Sum->D[0]->C[0] = SC;
	      Sum->D[0]->J[0] = 0;
	    }
	}
      break;

    case 2:   /* sum of the rows */

      if ((Sum =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->m,1)) == NULLSPMAXPCOL) return NULLSPMAXPCOL;
      if (nsp_spmaxpcolmatrix_resize_col(Sum,0,A->m)== FAIL) return NULLSPMAXPCOL;
      for ( k=0 ; k < Sum->D[0]->size ; k++) 
	{
	  Sum->D[0]->J[k]=k;
	  if ( A->rc_type == 'r' ) 
	    Sum->D[0]->R[k]=0.0;
	  else
	    Sum->D[0]->C[k].r = Sum->D[0]->C[k].i = 0.0;
	}
      for ( i = 0 ; i < A->n ; i++) 
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      switch ( A->rc_type ) 
		{
		case 'r' :  Sum->D[0]->R[A->D[i]->J[k]] += A->D[i]->R[k];break;
		case 'c' :  Sum->D[0]->C[A->D[i]->J[k]].r += A->D[i]->C[k].r ;
		  Sum->D[0]->C[A->D[i]->J[k]].i += A->D[i]->C[k].i ;break;
		}
	    }
	}
      count =0;
      switch ( A->rc_type ) 
	{
	case 'r' : 
	  for ( k=0 ; k < Sum->D[0]->size ; k++) 
	    {
	      if ( Sum->D[0]->R[k] == 0.0 ) { count=1; Sum->D[0]->J[k]=-1;}
	    }
	  break;
	case 'c' : 
	  for ( k=0 ; k < Sum->D[0]->size ; k++) 
	    {
	      if ( Sum->D[0]->C[k].r == 0.0 && Sum->D[0]->C[k].i == 0.0 ) 
		{ count=1; Sum->D[0]->J[k]=-1;}
	    }
	  break;
	}
      if ( count != 0 ) 
	{
	  int ndel =nsp_spmaxpcolmatrix_compress_col(Sum,0);
	  if (nsp_spmaxpcolmatrix_resize_col(Sum,0,A->m-ndel ) == FAIL) return NULLSPMAXPCOL;
	}
      break;


    case 1:    /* sum of the cols */
  
      if ((Sum =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,1,A->n)) == NULLSPMAXPCOL) return NULLSPMAXPCOL;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      double S;
	      S =nsp_dsum(&A->D[i]->size,A->D[i]->R,&inc); 
	      if ( S != 0.0 ) 
		{
		  if (nsp_spmaxpcolmatrix_resize_col(Sum,i,1)== FAIL) return NULLSPMAXPCOL;
		  Sum->D[i]->R[0] = S;
		  Sum->D[i]->J[0] = 0;
		}
	    }
	  break ;
	case 'c' :  
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      nsp_zsum(&C,&A->D[i]->size,A->D[i]->C,&inc); 
	      if ( C.r  != 0.0 || C.i != 0.0 ) 
		{
		  if (nsp_spmaxpcolmatrix_resize_col(Sum,i,1)== FAIL) return NULLSPMAXPCOL;
		  Sum->D[i]->C[0] = C;
		  Sum->D[i]->J[0] = 0;
		}
	    }
	  break;
	}
      break;
    }
  return Sum;
}

/* * Prod =nsp_mat_prod(A ,B])
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

typedef int (*SpMaMi1) (NspSpMaxpColMatrix *A,NspSpMaxpColMatrix *M);
typedef int (*SpMaMi2) (NspSpMaxpColMatrix *A,int j,NspSpMaxpColMatrix *M);
typedef int (*SpMaMi3) (NspSpMaxpColMatrix *A,int j,NspSpMaxpColMatrix *M,int *count);

/**
 * utility 
 **/

static NspSpMaxpColMatrix *SpMaxpColMaxiMini(NspSpMaxpColMatrix *A, int dim, NspMatrix **Imax, int lhs, SpMaMi1 F1, SpMaMi2 F2, SpMaMi3 F3)
{
  NspSpMaxpColMatrix *M=NULL;
  int j;
  int /* inc=1, */ imax,count;
  if ( A->m == 0 || A->n == 0 ) 
    {
      if ( lhs == 2) *Imax = nsp_matrix_create(NVOID,'r',0,0);
      return nsp_spmaxpcolmatrix_create(NVOID,'r',0,0);
    }
  switch (dim) 
    {

    case 0: 
      if ((M =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,1,1)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
      imax = (*F1)(A,M);
      /* Check if M was properly resized **/
      if ( imax == 0 )  return NULLSPMAXPCOL;
      
      if ( lhs == 2 ) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT)
	    return NULLSPMAXPCOL; 
	  (*Imax)->R[0] = imax;
	}
      break;

    case 1:
      if ((M =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,1,A->n)) == NULLSPMAXPCOL)
	return NULLSPMAXPCOL;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,A->n)) == NULLMAT) 
	    return NULLSPMAXPCOL;
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      (*Imax)->R[j]=(*F2)(A,j,M);
	    }
	}
      else 
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    (*F2)(A,j,M);
	  }
      break ;

    case 2:
      if ((M =nsp_spmaxpcolmatrix_create(NVOID,A->rc_type,A->m,1)) == NULLSPMAXPCOL) 
	return NULLSPMAXPCOL;
      if (nsp_spmaxpcolmatrix_resize_col(M,0,A->m) == FAIL) return NULLSPMAXPCOL;
      count =0;
      /* inc = A->m; */
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT) 
	    return NULLSPMAXPCOL; 
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      int imax =  (*F3)(A,j,M,&count);
	      (*Imax)->R[j] = imax;
	    }
	}
      else
	for ( j= 0 ; j < A->m ; j++) 
	  {
	    (*F3)(A,j,M,&count);
	  }
      if (nsp_spmaxpcolmatrix_resize_col(M,0,count) == FAIL) return NULLSPMAXPCOL;
      break;
    }

  return M;
}


/**
 * SpMaxpColMaxi1:
 * @A: a #NspSpMaxpColMatrix
 * @M: a #NspSpMaxpColMatrix
 * 
 * M(1) = Maxi(A) max of all the elements 
 * 
 * 
 * Return value: the indice of the element of Matrix @A which realize the maximum 
 * (the indice is given as a global indice of a mxn Matrix).
 **/

static int SpMaxpColMaxi1(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *M)
{
  int imax = 0,i,k;
  double amax= -DBL_MAX; imax=-1;
  /* find the max  */
  for ( i = 0 ; i < A->n ; i++ ) 
    {
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->R[k] > amax ) 
	    {
	      amax = A->D[i]->R[k];
	      imax = (A->D[i]->J[k]+1)+A->m*i;
	    }
	}
    }
  if ( amax > 0.0 )
    {
      if (nsp_spmaxpcolmatrix_resize_col(M,0,1) == FAIL) return 0;
      M->D[0]->J[0]=0;
      M->D[0]->R[0]= amax;
    }
  else 
    {
      int kmax=0;
      /* find a zero */
      for ( i = 0 ; i < A->n ; i++ ) 
	{
	  kmax=0;
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      if ( kmax < A->D[i]->J[k] ) 
		{
		  break;
		}
	      kmax++;
	    }
	  if ( kmax < A->m ) break;
	}
      if ( kmax == A->m) 
	{
	  /* The max was really negative */
	  if (nsp_spmaxpcolmatrix_resize_col(M,0,1) == FAIL) return 0;
	  M->D[0]->J[0]=0;
	  M->D[0]->R[0]= amax;
	}
      else
	{
	  /* the max is zero */
	  imax = (kmax+1)+A->m*i;
	}
    }
  return imax;
}

/**
 * SpMaxpColMaxi3:
 * @A: a #NspSpMaxpColMatrix
 * @j: 
 * @M: a #NspSpMaxpColMatrix
 * @count: 
 * 
 * utility : M(j)=Max A(j,:) : max of row j  
 *
 * Return value: the column indice which realize the max of row @j.
 **/

static int SpMaxpColMaxi3(NspSpMaxpColMatrix *A, int j, NspSpMaxpColMatrix *M, int *count)
{
  int imax = 1,i,k;
  double amax= -DBL_MAX;
  /* find a first value */
  for ( i = 0 ; i < A->n ; i++ ) 
    {
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->J[k] == j ) 
	    { amax = A->D[i]->R[k] ; imax = i+1; break;}
	  if ( j < A->D[i]->J[k] ) break;
	}
      if ( amax != 0.0 ) break;
    }
  /* find the max */
  for ( i = 0 ; i < A->n ; i++ ) 
    {
      int ok=-1;
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->J[k] == j ) { ok=k;break;}
	  if ( j < A->D[i]->J[k] ) break;
	}
      if ( ok != -1 )
	{
	  if ( A->D[i]->R[ok] > amax ) 
	    {
	      amax = A->D[i]->R[ok];
	      imax = i+1;
	    }
	}
      else 
	{
	  /* max is realized by a zero */
	  if ( 0 > amax )
	    {
	      amax = 0;
	      imax = i+1;
	    }
	}
    }
  if ( amax != 0.0 )
    {
      M->D[0]->J[*count]= j;
      M->D[0]->R[*count]= amax; (*count)++;
    }
  return imax;
}


/**
 * SpMaxpColMaxi2:
 * @A: a #NspSpMaxpColMatrix
 * @j: 
 * @M: a #NspSpMaxpColMatrix
 *
 * utility: M(j)=Max A(:,j) find the max of column j 
 * 
 * 
 * Return value: the row indice which realize the max of column @j.
 **/

static int SpMaxpColMaxi2(NspSpMaxpColMatrix *A, int j, NspSpMaxpColMatrix *M)
{
  int imax = 0,k;
  double amax= -DBL_MAX; imax=-1;
  /* find the max */
  for ( k = 0 ; k < A->D[j]->size ; k++) 
    {
      if ( A->D[j]->R[k]> amax ) 
	{
	  amax = A->D[j]->R[k];
	  imax = A->D[j]->J[k]+1;
	}
    }
  if ( amax > 0.0 )
    {
      if (nsp_spmaxpcolmatrix_resize_col(M,j,1) == FAIL) return 0;
      M->D[j]->J[0]= 0;
      M->D[j]->R[0]= amax;
    }
  else 
    {
      int kmax=0;
      for ( k = 0 ; k < A->D[j]->size ; k++) 
	{
	  if ( kmax < A->D[j]->J[k] ) 
	    {
	      break;
	    }
	  kmax++;
	}
      if ( kmax == A->m) 
	{
	  /* The max was really negative */
	  if (nsp_spmaxpcolmatrix_resize_col(M,j,1) == FAIL) return 0;
	  M->D[j]->J[0]=0;
	  M->D[j]->R[0]= amax;
	}
      else
	{
	  /* the max is zero */
	  imax = (kmax+1);
	}
    }
  return imax;
}

/**
 * nsp_spmaxpcolmatrix_maxi:
 * @A: a #NspSpMaxpColMatrix
 * @dim: an integer among  0, 1 or 2
 * @Imax: a #NspMatrix 
 * @lhs: an integer 
 * 
 * [max,imax]=max(A,dim=dim)
 * Max =nsp_spmaxpcolmatrix_maxi(A,dim,Imax,lhs)
 *     A is unchanged 
 * if dim=2 the max along the 2 dimension is computed 
 *       and a column vector is returned. 
 * if dim=1 the max along the first dimension is computed 
 *       and a Row vector is returned.
 * if dim=0 the maximum 
 * Imax is created if lhs == 2 
 * Note that Imax is a full matrix. 
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_maxi(NspSpMaxpColMatrix *A, int dim, NspMatrix **Imax, int lhs)
{
  return SpMaxpColMaxiMini(A,dim,Imax,lhs,SpMaxpColMaxi1,SpMaxpColMaxi2,SpMaxpColMaxi3);
}



/**
 * SpMaxpColMini1:
 * @A: a #NspSpMaxpColMatrix
 * @M: a #NspSpMaxpColMatrix
 * 
 * M(1) = Mini(A) min of all the elements 
 * 
 * Return value: the indice of the element of Matrix @A which realize the minimum
 * (the indice is given as a global indice of a mxn Matrix).
 **/

static int SpMaxpColMini1(NspSpMaxpColMatrix *A, NspSpMaxpColMatrix *M)
{
  int imin = 0,i,k;
  double amin= DBL_MAX; imin=-1;
  /* find the min  */
  for ( i = 0 ; i < A->n ; i++ ) 
    {
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->R[k] < amin ) 
	    {
	      amin = A->D[i]->R[k];
	      imin = (A->D[i]->J[k]+1)+A->m*i;
	    }
	}
    }
  if ( amin < 0.0 )
    {
      if (nsp_spmaxpcolmatrix_resize_col(M,0,1) == FAIL) return 0;
      M->D[0]->J[0]=0;
      M->D[0]->R[0]= amin;
    }
  else 
    {
      int kmin=0;
      /* find a sparse zero  */
      for ( i = 0 ; i < A->n ; i++ ) 
	{
	  kmin=0;
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      if ( kmin < A->D[i]->J[k] ) 
		{
		  break;
		}
	      kmin++;
	    }
	  if ( kmin < A->m ) break;
	}
      if ( kmin == A->m) 
	{
	  /* The min was really positive */
	  if (nsp_spmaxpcolmatrix_resize_col(M,0,1) == FAIL) return 0;
	  M->D[0]->J[0]=0;
	  M->D[0]->R[0]= amin;
	}
      else
	{
	  /* the min is zero */
	  imin = (kmin+1)+A->m*i;
	}
    }
  return imin;
}



/**
 * SpMaxpColMini3:
 * @A: a #NspSpMaxpColMatrix
 * @j: 
 * @M: a #NspSpMaxpColMatrix
 * @count: 
 * 
 * utility : M(j)=Min A(j,:) : min of row j  
 *
 * Return value: the column indice which realize the max of row @j.
 **/

static int SpMaxpColMini3(NspSpMaxpColMatrix *A, int j, NspSpMaxpColMatrix *M, int *count)
{
  int imin = 1,i,k;
  double amin= DBL_MAX;
  /* find a first value */
  for ( i = 0 ; i < A->n ; i++ ) 
    {
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->J[k] == j ) 
	    { amin = A->D[i]->R[k] ; imin = i+1; break;}
	  if ( j < A->D[i]->J[k] ) break;
	}
      if ( amin != 0.0 ) break;
    }
  /* find the min */
  for ( i = 0 ; i < A->n ; i++ ) 
    {
      int ok=-1;
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->J[k] == j ) { ok=k;break;}
	  if ( j < A->D[i]->J[k] ) break;
	}
      if ( ok != -1 )
	{
	  if ( A->D[i]->R[ok] < amin ) 
	    {
	      amin = A->D[i]->R[ok];
	      imin = i+1;
	    }
	}
      else 
	{
	  /* min is realized by a zero */
	  if ( 0 < amin )
	    {
	      amin = 0;
	      imin = i+1;
	    }
	}
    }
  if ( amin != 0.0 )
    {
      M->D[0]->J[*count]= j;
      M->D[0]->R[*count]= amin; (*count)++;
    }
  return imin;
}


/**
 * SpMaxpColMini2:
 * @A: a #NspSpMaxpColMatrix
 * @j: 
 * @M: a #NspSpMaxpColMatrix
 *
 * utility: M(j)=Min A(:,j) find the min of column j 
 * 
 * 
 * Return value: the row indice which realize the min of column @j.
 **/

static int SpMaxpColMini2(NspSpMaxpColMatrix *A, int j, NspSpMaxpColMatrix *M)
{
  int imin = 0,k;
  double amin= DBL_MAX; imin=-1;
  /* find the min */
  for ( k = 0 ; k < A->D[j]->size ; k++) 
    {
      if ( A->D[j]->R[k] < amin ) 
	{
	  amin = A->D[j]->R[k];
	  imin = A->D[j]->J[k]+1;
	}
    }
  if ( amin < 0.0 )
    {
      if (nsp_spmaxpcolmatrix_resize_col(M,j,1) == FAIL) return 0;
      M->D[j]->J[0]= 0;
      M->D[j]->R[0]= amin;
    }
  else 
    {
      int kmin=0;
      for ( k = 0 ; k < A->D[j]->size ; k++) 
	{
	  if ( kmin < A->D[j]->J[k] ) 
	    {
	      break;
	    }
	  kmin++;
	}
      if ( kmin == A->m) 
	{
	  /* The min was really negative */
	  if (nsp_spmaxpcolmatrix_resize_col(M,j,1) == FAIL) return 0;
	  M->D[j]->J[0]=0;
	  M->D[j]->R[0]= amin;
	}
      else
	{
	  /* the min is zero */
	  imin = (kmin+1);
	}
    }
  return imin;
}

/**
 * nsp_spmaxpcolmatrix_mini:
 * @A: a #NspSpMaxpColMatrix
 * @dim: an integer among  0, 1 or 2
 * @Imax: a #NspMatrix 
 * @lhs: an integer 
 * 
 * [min,imin]=min(A,dim=dim)
 * Min =nsp_spmaxpcolmatrix_maxi(A,dim,Imin,lhs)
 *     A is unchanged 
 * if dim=2 the min along the 2 dimension is computed 
 *       and a column vector is returned. 
 * if dim=1 the min along the first dimension is computed 
 *       and a Row vector is returned.
 * if dim=0 the minimum
 *
 * Imin is created if lhs == 2 
 * Note that Imin is a full matrix. 
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_mini(NspSpMaxpColMatrix *A, int dim, NspMatrix **Imin, int lhs)
{
  return SpMaxpColMaxiMini(A,dim,Imin,lhs,SpMaxpColMini1,SpMaxpColMini2,SpMaxpColMini3);
}



/*
 * Creates a Matrix and initialize it with the 
 * function func 
 * R=func(i,j) or R=func(i,j,&Imag) 
 */

/**
 * nsp_spmaxpcolmatrix_triu:
 * @A: a #NspSpMaxpColMatrix
 * @k: an integer 
 * 
 * A=Triu(A)
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_triu(NspSpMaxpColMatrix *A,int k)
{
  int i,j;
  for ( i = 0 ; i < A->n ; i++)
    {
      int resize=FALSE;
      /* maximum row indice to keep for column i */
      int maxrow= i-k;
      for ( j=0; j < A->D[i]->size ; j++ ) 
	{
	  if ( A->D[i]->J[j] > maxrow )
	    { 
	      resize=TRUE;break;
	    }
	}
      if ( resize == TRUE )
	{
	  if (nsp_spmaxpcolmatrix_resize_col(A,i,j) == FAIL) return FAIL;
	}
    }
  return OK;
}


/**
 * nsp_spmaxpcolmatrix_tril:
 * @A: a #NspSpMaxpColMatrix
 * @k: 
 * 
 * A=Tril(A)
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_tril(NspSpMaxpColMatrix *A,int k)
{
  int ndel;
  int i,j;
  for ( i = 0 ; i < A->n ; i++)
    {
      /* int resize=FALSE; */
      /* maximum row indice to keep for column i */
      int minrow= i-k;
      for ( j=0; j < A->D[i]->size ; j++ ) 
	{
	  if ( A->D[i]->J[j] >= minrow )
	    { 
	      /* resize=TRUE;*/ break;
	    }
	  else 
	    {
	      A->D[i]->J[j] = -1;
	    }
	}
      ndel =nsp_spmaxpcolmatrix_compress_col_simple(A,i);
      if (nsp_spmaxpcolmatrix_resize_col(A,i, A->D[i]->size-ndel ) == FAIL) return(FAIL) ;
    }
  return OK;
}



/**
 * nsp_spmaxpcolmatrix_eye:
 * @m: an integer 
 * @n: an integer 
 * 
 * return eye(m,n) coded as #NspSpMaxpColMatrix.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_eye(int m, int n)
{
  NspSpMaxpColMatrix *Loc;
  int i;
  if (( Loc=nsp_spmaxpcolmatrix_create(NVOID,'r',m,n)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
  for ( i = 0 ; i < Min(Loc->m,Loc->n) ; i++ ) 
    {
      if (nsp_spmaxpcolmatrix_resize_col(Loc,i,1)== FAIL) return NULLSPMAXPCOL;
      Loc->D[i]->J[0]= i;
      Loc->D[i]->R[0]= 1.0;
    }
  return(Loc);
}


/**
 * nsp_spmaxpcolmatrix_spones:
 * @A: a #NspSpMaxpColMatrix
 * 
 * generates a matrix with the same sparsity structure as A, 
 * but with ones in the nonzero positions
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_spones(NspSpMaxpColMatrix *A)
{
  int i,j;
  NspSpMaxpColMatrix *Loc;
  if ( A->rc_type == 'r') 
    {
      if ((Loc= nsp_spmaxpcolmatrix_copy(A)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
      for ( i = 0 ; i < Loc->n ; i++ ) 
	{
	  for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	    {
	      Loc->D[i]->R[j]= 1.0;
	    }
	}
      return(Loc);
    }
  else 
    {
      if ((Loc= nsp_spmaxpcolmatrix_copy(A)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
      Loc->rc_type = 'r';
      for ( i = 0 ; i < Loc->n ; i++ ) 
	{
	  int size = Loc->D[i]->size ;
	  if ( size != 0) 
	    {
	      /* switch from complex to real */
	      if ((Loc->D[i]->R =nsp_realloc_doubles(Loc->D[i]->R,size))
		  == NULL) return NULL;
	    }
	  for ( j = 0 ; j < size ; j++ ) 
	    {
	      Loc->D[i]->R[j]= 1.0;
	    }
	}
      return(Loc);
    }
}

/**
 * nsp_spmaxpcolmatrix_ones:
 * @m: an integer 
 * @n: an integer 
 * 
 * ones(m,n) as a sparse Matrix.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/


NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_ones(int m, int n)
{
  NspSpMaxpColMatrix *Loc;
  int i,k;
  if (( Loc=nsp_spmaxpcolmatrix_create(NVOID,'r',m,n)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
  for ( i = 0 ; i < Loc->n ; i++ ) 
    {
      if (nsp_spmaxpcolmatrix_resize_col(Loc,i,Loc->m)== FAIL) return NULLSPMAXPCOL;
      for ( k = 0 ; k < Loc->m ; k++) 
	{
	  Loc->D[i]->J[k]= k;
	  Loc->D[i]->R[k]= 1.0;
	}
    }
  return(Loc);
}


/**
 * nsp_spmaxpcolmatrix_zeros:
 * @m: 
 * @n: 
 * 
 * A=zeros(m,n)
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_zeros(int m, int n)
{
  NspSpMaxpColMatrix *Loc;
  if (( Loc=nsp_spmaxpcolmatrix_create(NVOID,'r',m,n)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
  return(Loc);
}

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


typedef double (*Func1) (double);
typedef void   (*Func2) (const doubleC *, doubleC *);

static NspMatrix* SpMaxpColUnary2Full(NspSpMaxpColMatrix *A, Func1 F1, Func2 F2)
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
      for ( i = 0 ; i < A->n ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  {
	    j= A->D[i]->J[k];
	    Loc->R[j+i*Loc->m] = (*F1)( A->D[i]->R[k]);
	  }
    }
  else
    {
      for ( i = 0 ; i < A->n ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  {
	    j= A->D[i]->J[k];
	    (*F2)(&A->D[i]->C[k],&Loc->C[j+i*Loc->m]);
	  }
    }
  return Loc;
}

/**
 * nsp_spmaxpcolmatrix_acos:
 * @A: a #NspSpMaxpColMatrix
 * 
 * returns cos(A) as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLMAT
 **/

NspMatrix *nsp_spmaxpcolmatrix_acos(NspSpMaxpColMatrix *A)
{
  return SpMaxpColUnary2Full(A,acos,nsp_acos_c);
}


/**
 * nsp_spmaxpcolmatrix_acosh:
 * @A: a #NspSpMaxpColMatrix
 * 
 * returns cosh(A) as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLMAT
 **/

NspMatrix *nsp_spmaxpcolmatrix_acosh(NspSpMaxpColMatrix *A)
{
  return SpMaxpColUnary2Full(A,acosh,nsp_acosh_c);
}

/*
 * Generic Function for Sparse unary operators 
 * computes A=f1(A) or A=f2(A) assuming fi(0)=0
 */

static void  SpMaxpColUnary(NspSpMaxpColMatrix *A, Func1 F1, Func2 F2)
{
  int i,k,compress,ndel;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->n ; i++)
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
	      ndel =nsp_spmaxpcolmatrix_compress_col(A,i);
	      nsp_spmaxpcolmatrix_resize_col(A,i,A->D[i]->size-ndel);
	    }
	}
    }
  else
    {
      for ( i = 0 ; i < A->n ; i++)
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
	      ndel =nsp_spmaxpcolmatrix_compress_col(A,i);
	      nsp_spmaxpcolmatrix_resize_col(A,i,A->D[i]->size-ndel);
	    }
	}
    }
}


/**
 * nsp_spmaxpcolmatrix_asin:
 * @A: a #NspSpMaxpColMatrix
 *
 * A=asin(A)
 * 
 **/

void nsp_spmaxpcolmatrix_asin(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,asin,nsp_asin_c);
}



/**
 * nsp_spmaxpcolmatrix_asinh:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=Asinh(A),
 **/

void nsp_spmaxpcolmatrix_asinh(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,asinh,nsp_asinh_c);
}


/**
 * nsp_spmaxpcolmatrix_atan:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=Atan(A),
 **/
void nsp_spmaxpcolmatrix_atan(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,atan,nsp_atan_c);
}


/**
 * nsp_spmaxpcolmatrix_atanh:
 * @A: a #NspSpMaxpColMatrix
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

void nsp_spmaxpcolmatrix_atanh(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,nsp_atanh,nsp_atanh_c);
}

/**
 * nsp_spmaxpcolmatrix_ceil:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=Ceil(A)
 **/

void nsp_spmaxpcolmatrix_ceil(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,ceil,nsp_ceil_c);
}

static double R_aint(double x) { return aint(x);} 

/** 
 * nsp_spmaxpcolmatrix_int:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * A=Int(A)
 **/

void nsp_spmaxpcolmatrix_int(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,R_aint,nsp_aint_c);
}


/**
 * nsp_spmaxpcolmatrix_floor:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=Floor(A) 
 **/

void nsp_spmaxpcolmatrix_floor(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,floor,nsp_floor_c);
}

static double R_anint(double x) { return anint(x);} 

/**
 *nsp_spmaxpcolmatrix_round: 
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=Round(A)
 **/

void nsp_spmaxpcolmatrix_round(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,R_anint,nsp_round_c);
}


/**
 * nsp_spmaxpcolmatrix_sign:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * A=Sign(A)
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_sign(NspSpMaxpColMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  {
	    if (A->D[i]->R[k] > 0.0) 
	      A->D[i]->R[k] = 1.00;
	    else if (A->D[i]->R[k] < 0.0) 
	      A->D[i]->R[k] = -1.00;
	  }
    }
  else
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  nsp_signum_c(&A->D[i]->C[k],&A->D[i]->C[k]);
    }
  return(OK);
}


/**
 * nsp_spmaxpcolmatrix_tan:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=Tan(A) 
 **/

void nsp_spmaxpcolmatrix_tan(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,tan,nsp_tan_c);
}

/**
 * nsp_spmaxpcolmatrix_tanh:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=Tanh(A)
 **/

void nsp_spmaxpcolmatrix_tanh(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,tanh,nsp_tanh_c);
}


/**
 * nsp_spmaxpcolmatrix_abs:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * A=Abs(A), absolue value or module of each element 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_abs(NspSpMaxpColMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->R[k] = fabs(A->D[i]->R[k]);
    }
  else
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->C[k].r =nsp_abs_c(&A->D[i]->C[k]);
      if (nsp_spmaxpcolmatrix_realpart(A) == FAIL) return FAIL;
    }
  return(OK);
}


/**
 * nsp_spmaxpcolmatrix_erf:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * A=Erf(A), Erf function 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_erf(NspSpMaxpColMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->R[k] = erf(A->D[i]->R[k]);
    }
  else
    {
      Scierror("Error:\t erf function argument must be real\n");
      return(FAIL);
    }
  return(OK);
}

/*
 * A=Erfc(A), Erf function 
 */

/*
  int SpMaxpColErfc(A)
  NspSpMaxpColMatrix *A;
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
 * nsp_spmaxpcolmatrix_arg:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * A=Arg(A),
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_arg(NspSpMaxpColMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  {
	    if(  A->D[i]->R[k] >= 0.0 ) 
	      A->D[i]->R[k] = 0.0;
	    else if(  A->D[i]->R[k] < 0.0 ) 
	      A->D[i]->R[k] = M_PI;
	  }
      /* XXX  we should remove the zeroes */
    }
  else
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->C[k].r =nsp_arg_c(&A->D[i]->C[k]);
      if (nsp_spmaxpcolmatrix_realpart(A) == FAIL) return FAIL;
    }
  return(OK);
}

/*
 * A=Polar(A,B),
 * A=A(cos(B)+%i*sin(B);
 */

/**
 * nsp_spmaxpcolmatrix_conj:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=real(A)-i*Imag(A) A is changed  only if imaginary 
 **/

void nsp_spmaxpcolmatrix_conj(NspSpMaxpColMatrix *A)
{
  int i,k;
  switch ( A->rc_type ) 
    {
    case 'r' : break;
    case 'c' : 
      for ( i = 0 ; i < A->n ; i++)
	{
	  for ( k= 0 ; k < A->D[i]->size; k++ ) 
	    A->D[i]->C[k].i = - A->D[i]->C[k].i;
	}
      break;
    }
}

/**
 * nsp_spmaxpcolmatrix_cos:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * Cos(A)
 * Return value: a new  #NspMatrix or %NULLMAT
 **/
NspMatrix *nsp_spmaxpcolmatrix_cos(NspSpMaxpColMatrix *A)
{
  return SpMaxpColUnary2Full(A,cos,nsp_cos_c);
}


/**
 * nsp_spmaxpcolmatrix_cosh:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * Cosh(A)
 * Return value: a new  #NspMatrix or %NULLMAT
 **/

NspMatrix *nsp_spmaxpcolmatrix_cosh(NspSpMaxpColMatrix *A)
{
  return SpMaxpColUnary2Full(A,cosh,nsp_cosh_c);
}

/**
 * nsp_spmaxpcolmatrix_expel:
 * @A: a #NspSpMaxpColMatrix
 * 
 * exp(A)
 * Return value: a new  #NspMatrix or %NULLMAT
 **/

NspMatrix *nsp_spmaxpcolmatrix_expel(NspSpMaxpColMatrix *A)
{
  return SpMaxpColUnary2Full(A,exp,nsp_exp_c);
}

/**
 * nsp_spmaxpcolmatrix_logel:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * log(A)
 * The real case is special since the result can be complex
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_logel(NspSpMaxpColMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r')
    {
      /* Check if really real or imaginary case */
      int itr = 0;
      for ( i=0 ; i < A->n ; i++) 
	for ( k=0 ; k < A->D[i]->size ; k++) 
	  if ( A->D[i]->R[k] < 0.00 ) 
	    {
	      itr = 1; break;
	    }
      if ( itr == 0) 
	{
	  /* real case sqrt(A) is real  */
	  SpMaxpColUnary(A,log,nsp_log_c);
	  return OK;
	}
      else 
	{
	  /* result is complex  */
	  if (nsp_spmaxpcolmatrix_seti(A,0.00) == FAIL ) return FAIL;
	  SpMaxpColUnary(A,log,nsp_log_c);
	  return OK;
	}
    }
  /* A is complex and sqrt(A) too **/
  SpMaxpColUnary(A,log,nsp_log_c);
  return OK;
}

/**
 * nsp_spmaxpcolmatrix_sin:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=sin(A)
 **/

void nsp_spmaxpcolmatrix_sin(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,sin,nsp_sin_c);
}



/**
 * nsp_spmaxpcolmatrix_sinh:
 * @A: a #NspSpMaxpColMatrix
 * 
 * A=Sinh(A)
 **/

void nsp_spmaxpcolmatrix_sinh(NspSpMaxpColMatrix *A)
{
  SpMaxpColUnary(A,sinh,nsp_sinh_c);
}


/**
 * nsp_spmaxpcolmatrix_sqrtel:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 *  A=SqrtEl(A)  term to term square root
 * The real case is special since the result can be complex
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_sqrtel(NspSpMaxpColMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r')
    {
      /* Check if really real or imaginary case */
      int itr = 0;
      for ( i=0 ; i < A->n ; i++) 
	for ( k=0 ; k < A->D[i]->size ; k++) 
	  if ( A->D[i]->R[k] < 0.00 ) 
	    {
	      itr = 1; break;
	    }
      if ( itr == 0) 
	{
	  /* real case sqrt(A) is real  */
	  SpMaxpColUnary(A,sqrt,nsp_sqrt_c);
	  return OK;
	}
      else 
	{
	  /* result is complex  */
	  if (nsp_spmaxpcolmatrix_seti(A,0.00) == FAIL ) return FAIL;
	  SpMaxpColUnary(A,sqrt,nsp_sqrt_c);
	  return OK;
	}
    }
  /* A is complex and sqrt(A) too **/
  SpMaxpColUnary(A,sqrt,nsp_sqrt_c);
  return OK;
}

/**
 * nsp_spmaxpcolmatrix_minus:
 * @A: a #NspSpMaxpColMatrix
 * 
 * 
 * A= -A 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_minus(NspSpMaxpColMatrix *A)
{
  int i,k ;
  if ( A->rc_type  == 'r') 
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k = 0 ; k < A->D[i]->size ; k++)
	  {
	    A->D[i]->R[k] = - A->D[i]->R[k];
	  }
    }
  else
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k = 0 ; k < A->D[i]->size ; k++)
	  {
	    A->D[i]->C[k].r = - A->D[i]->C[k].r;
	    A->D[i]->C[k].i = - A->D[i]->C[k].i;
	  }
    }
  return(OK);
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
 * nsp_spmaxpcolmatrix_find:
 * @A: a #NspSpMaxpColMatrix
 * @lhs: an integer 
 * @Res1: a #NspMatrix pointer 
 * @Res2: a #NspMatrix pointer 
 * @V: a #NspMatrix pointer 
 * 
 * returns in one or two Matrices the indices for which the 
 * Matrix @A has non zero entries and eventually return the entries.
 * @A is left unchanged and according to lhs one or two or three 
 * values are returned 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spmaxpcolmatrix_find(NspSpMaxpColMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2,
			 NspMatrix **V)
{
  int k,i,count=0;
  /* first pass for counting **/
  for ( i=0 ; i < A->n ; i++) 
    {
      count += A->D[i]->size ;
    }
  if ( lhs == 1) 
    {
      *Res1 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res1 == NULLMAT) return FAIL;
      count=0;
      for ( i = 0 ; i < A->n ; i++ )
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    (*Res1)->R[count++] = A->D[i]->J[k]+A->m* i + 1;
	}
    }
  else 
    {
      *Res1 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res1 == NULLMAT) return FAIL;
      *Res2 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res2 == NULLMAT) return FAIL;
      if ( lhs == 3 ) 
	{
	  *V= nsp_matrix_create(NVOID,A->rc_type,(int) 1,(int) count);
	  if ( *V == NULLMAT) return FAIL;
	}
      count=0;
      if ( lhs == 2 )
	{
	  for ( i = 0 ; i < A->n ; i++ )
	    {
	      for ( k = 0 ; k < A->D[i]->size ; k++) 
		{
		  (*Res2)->R[count]   = i + 1;
		  (*Res1)->R[count++] = A->D[i]->J[k] + 1;
		}
	    }
	}
      else 
	{
	  if ( A->rc_type == 'r' ) 
	    for ( i = 0 ; i < A->n ; i++ )
	      {
		for ( k = 0 ; k < A->D[i]->size ; k++) 
		  {
		    (*V)->R[count]   = A->D[i]->R[k];
		    (*Res2)->R[count]   = i + 1;
		    (*Res1)->R[count++] = A->D[i]->J[k] + 1;
		  }
	      }
	  else
	    for ( i = 0 ; i < A->n ; i++ )
	      {
		for ( k = 0 ; k < A->D[i]->size ; k++) 
		  {
		    (*V)->C[count]   = A->D[i]->C[k];
		    (*Res2)->R[count]   = i + 1;
		    (*Res1)->R[count++] = A->D[i]->J[k] + 1;
		  }
	      }
	  
	}
    }
  return OK;
}


/**
 * nsp_spmaxpcolmatrix_rand_one:
 * @m: number of rows
 * @n: number of columns
 * @sparsity: percent of non null elements
 * @crand: a character ('n' for normal or 'u' for uniform) 
 * 
 * returns a new #NspSpMaxpColMatrix filled with random values ('n' or 'u') 
 * the percent of non null elements is given by @sparsity.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_rand_one(int m,int n,double sparsity,char crand)
{
  double moy=0, std=1.0, mcol=m*sparsity;
  int mres, k, i, *icol=NULL;
  NspSpMaxpColMatrix *A=NULLSPMAXPCOL;

  if ((A =nsp_spmaxpcolmatrix_create(NVOID,'r',m,n))== NULLSPMAXPCOL ) 
    return NULLSPMAXPCOL;

  if ( (icol=nsp_alloc_work_int(m)) == NULL )
    goto err;

  for ( i=0 ; i < A->n ; i++) 
    {
      double u=rand_ranf();
      mres = (int) ( (u >=0.5) ? ceil(mcol) : floor(mcol));
      /* make a permutation of (0,1,..,m-1) */
      nsp_rand_prm(icol, m, 0);
      /* sort the mres first elements */
      nsp_qsort_int(icol,NULL,FALSE,mres,'i');
      /* resize column i */
      if ( nsp_spmaxpcolmatrix_resize_col(A,i,mres) == FAIL )
	goto err;

      for ( k = 0 ; k < A->D[i]->size ; k++) 
	A->D[i]->J[k] = icol[k];
      if ( crand == 'n' ) 
	for (k = 0 ; k < A->D[i]->size ; k++)
	  A->D[i]->R[k]= nsp_rand_nor(moy,std);
      else 
	for (k = 0 ; k < A->D[i]->size ; k++)
	  A->D[i]->R[k]= rand_ranf();
    }
  FREE(icol);
  return A;

 err:
  FREE(icol);
  nsp_spmaxpcolmatrix_destroy(A);
  return NULLSPMAXPCOL;
}


/* 
 * randomly select requested number of non-nul elements for each column 
 * in order to get a total of ntot non-nul elements.
 */

static int *nsp_sprand_column_deviates(int ntot,int ncol,int colsize)
{
  int *Icol;
  int count=0;

  if ( (Icol = nsp_alloc_work_int(ncol)) == NULL ) return NULL;
  memset(Icol, 0, ncol*sizeof(int));
  while ( count != ntot )
    {
      int val = floor(ncol* rand_ranf());
      if ( Icol[val] < colsize )
	{
	  Icol[val]++;
	  count++;
	}
    }
  return Icol;
}

/**
 * nsp_spmaxpcolmatrix_rand:
 * @m: number of rows
 * @n: number of columns
 * @sparsity: percent of non null elements
 * @crand: a character ('n' for normal or 'u' for uniform) 
 * 
 * returns a new #NspSpMaxpColMatrix filled with random values ('n' or 'u') 
 * the percent of non null elements is given by @sparsity.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_rand(int m,int n,double sparsity,char crand)
{
  double moy=0.0, std=1.0;
  int *nb_elts_col = NULL, *icol = NULL, *tcol = NULL;
  NspSpMaxpColMatrix *A=NULLSPMAXPCOL;
  int nnz=(int)(m*(n*Min(Max(sparsity,0.0),1.0))), k, j;

  if ( (A =nsp_spmaxpcolmatrix_create(NVOID,'r',m,n)) == NULLSPMAXPCOL )
    return NULLSPMAXPCOL;

  /* used to detect row already selected */
  if ( (icol =nsp_alloc_work_int(2*m)) == NULL ) goto err;
  tcol = icol + m;
  memset(tcol, 0, m*sizeof(int));  /* out of the loop */

  /* number of non-nul elements for each column  */
  if ( (nb_elts_col =nsp_sprand_column_deviates(nnz,A->n,A->m)) == NULL )
    goto err;

  for ( j=0 ; j < A->n ; j++)
    {
      int count = 0;
      int nnzj = nb_elts_col[j]; /* number of non-nul elements in column j */
      /* random selection of row indices */
      while ( count != nnzj )
	{
	  int val = floor(A->m* rand_ranf());
	  if ( tcol[val] != 1)
	    {
	      tcol[val] = 1;
	      icol[count++] = val;
	    }
	}
      /* reset tcol to 0 (but only on the previous flagged cases) */
      for ( k = 0 ; k < nnzj ; k++ )
	tcol[icol[k]] = 0;

      /* sort icol[0:nnzj-1] */
      nsp_qsort_int(icol, NULL, FALSE, nnzj, 'i');

      /* resize column j */
      nsp_spmaxpcolmatrix_resize_col(A, j, nnzj);

      /* fill the column j (with row indices and random values) */
      for ( k = 0 ; k < nnzj ; k++ )
	A->D[j]->J[k] = icol[k];
      if ( crand == 'n' )
	for ( k = 0 ; k < nnzj ; k++ )
	  A->D[j]->R[k] = nsp_rand_nor(moy,std);
      else
	for ( k = 0 ; k < nnzj ; k++ )
	  A->D[j]->R[k] = rand_ranf();
    }
   
  FREE(icol);
  FREE(nb_elts_col);
  return A;

 err:
  FREE(icol);
  FREE(nb_elts_col);
  nsp_spmaxpcolmatrix_destroy(A);
  return NULLSPMAXPCOL;
}


/*
 **/

static double nsp_spmaxpcolmatrix_norm1(NspSpMaxpColMatrix *A)
{
  int i,j;
  double norm=0.0;
  switch ( A->rc_type ) 
    {
    case 'r': 
      for ( i = 0 ; i < A->n ; i++) 
	{
	  double vnorm=0.0;
	  for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	    {
	      vnorm += Abs(A->D[i]->R[j]);
	    }
	  norm = Max(norm,vnorm);
	}
      break;
    case 'c': 
      for ( i = 0 ; i < A->n ; i++) 
	{
	  double vnorm=0.0;
	  for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	    {
	      vnorm += nsp_abs_c(&A->D[i]->C[j]);
	    }
	  norm = Max(norm,vnorm);
	}
    }
  return norm;
}

static double nsp_spmaxpcolmatrix_fro(NspSpMaxpColMatrix *A)
{
  int j,k;
  double norm=0.0, coef, r, scale=1;
  switch ( A->rc_type ) 
    {
    case 'r': 
      for ( j = 0 ; j < A->n ; j++) 
	{
	  for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	    {
	      coef = fabs(A->D[j]->R[k]);
	      if ( coef <= scale )
		{
		  r = coef/scale; norm += r*r;
		}
	      else
		{
		  r = scale/coef; norm = 1 + norm*(r*r);
		  scale = coef;
		}
	    }
	}
      norm = scale*sqrt(norm);
      break;
    case 'c': 
      for ( j = 0 ; j < A->n ; j++) 
	{
	  for ( k = 0 ; k < 2*A->D[j]->size ; k++ ) 
	    {
	      coef = fabs(A->D[j]->R[k]);
	      if ( coef <= scale )
		{
		  r = coef/scale; norm += r*r;
		}
	      else
		{
		  r = scale/coef; norm = 1 + norm*(r*r);
		  scale = coef;
		}
	    }
	}
      norm = scale*sqrt(norm);
      break;
    }
  return norm;
}

/* return a < 0 norm when there's an allocation problem 
 **/

static double nsp_spmaxpcolmatrix_norminf(NspSpMaxpColMatrix *A)
{
  double norm;
  int i,j;
  NspMatrix *N=NULL;
  if ((N = nsp_mat_zeros(A->m,1)) == NULLMAT )
    return -1;
  switch ( A->rc_type ) 
    {
    case 'r': 
      for ( i = 0 ; i < A->n ; i++) 
	{
	  for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	    {
	      int row = A->D[i]->J[j];
	      N->R[row] +=  Abs(A->D[i]->R[j]);
	    }
	}
      break;
    case 'c': 
      for ( i = 0 ; i < A->n ; i++) 
	{
	  for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	    {
	      int row = A->D[i]->J[j];
	      N->R[row] += nsp_abs_c(&A->D[i]->C[j]);
	    }
	}
    }
  nsp_array_maxi(N->mn,N->R,1,&norm);
  nsp_matrix_destroy(N);
  return norm;
}

/**
 * nsp_spmaxpcolmatrix_norm:
 * @A: a real or complex #NspSpMaxpColMatrix
 * @c: character defining the kind of matrix norm to compute:
 *
 *        @c='1' for 1-norm ||A||_1 = max of ||Ax||_1 for all x such that ||x||_1 = 1
 *        @c='2' for 2-norm ||A||_2 = max of ||Ax||_2 for all x such that ||x||_2 = 1
 *        @c='I' for Inf-norm ||A||_I = max of ||Ax||_inf for all x such that ||x||_inf = 1
 *        @c='F' for Frobenius norm ||A||_F = sqrt( sum_{i,j} A(i,j)^2 )
 *        @c='M' for  ||A||_F = max_{i,j} |A(i,j)|  (which is not exactly a matrix-norm)
 *
 * @A is not modified.
 * 
 * Return value: the matrix norm or -1 in case of failure (alloc problem or problem in
 *               svd (2-norm))
 **/

double nsp_spmaxpcolmatrix_norm(NspSpMaxpColMatrix *A, char c)
{
  switch (c) {
  case '1':
    return nsp_spmaxpcolmatrix_norm1(A);
  case '2': 
    Scierror("Error: norm(.,2) not implemented for sparse matrices\n");
    return -1;
  case 'I':
    return nsp_spmaxpcolmatrix_norminf(A);
  case 'F':
    return nsp_spmaxpcolmatrix_fro(A);
  case 'M': 
    Scierror("Error: M norm not implemented for sparse matrices\n");
    return -1;
  }
  Scierror("Error: %c norm not implemented for sparse matrices\n",c);
  return -1;
}

/**
 * nsp_spmaxpcolmatrix_vnorm:
 * @A: a real or complex sparse vector in a #bNspSpMaxpColMatrix
 * @p: a #double which must be >= 1 
 * 
 * Computes the p-norm of the vector @A : 
 *
 *     ( sum_k |A_k|^p )^(1/p)
 * 
 * @p must be +Inf to compute the infinite norm ( max_k |A_k| )
 * @A is not modified
 * 
 * Return value: the p-norm of the vector @A or -1.0 in case of alloc
 *               problem (which may happen only for intzvnorm)
 **/

double nsp_spmaxpcolmatrix_vnorm(NspSpMaxpColMatrix *A, double p)
{
  int i,j;
  double norm;
  NspMatrix *V=NULL;
  int nnz = nsp_spmaxpcolmatrix_nnz(A), count=0;
  if ((V=nsp_matrix_create(NVOID,A->rc_type,nnz,1)) == NULLMAT)
    return -1;
  if ( A->n == 1 ) 
    {
      if ( A->m == 0) return 0;
      memcpy(V->R,A->D[0]->R,nnz*sizeof(double)*((A->rc_type=='r') ? 1: 2));
      norm = nsp_vector_norm(V, p);
      nsp_matrix_destroy(V);
    }
  else 
    {
      if ( A->n == 0) return 0;
      switch ( A->rc_type ) 
	{
	case 'r': 
	  for ( i = 0 ; i < A->n ; i++) 
	    for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	      V->R[count++]= A->D[i]->R[j];
	  break;
	case 'c' :
	  for ( i = 0 ; i < A->n ; i++) 
	    for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	      V->C[count++]= A->D[i]->C[j];
	  break;
	}
      norm = nsp_vector_norm(V, p);
      nsp_matrix_destroy(V);
    }
  return norm;
}


/**
 * nsp_spmaxpcolmatrix_isnan:
 * @A: a #NspSpMaxpColMatrix
 * @flag: an integer 
 * 
 * if @flag is -1 then a new #NspSpMaxpColMatrix is returned 
 * containing for each entries of the matrix @A the value of isinf(A(i;j)).
 * 
 * if @flag = 2 a sparse column vector is returned. The i-th row of the column vector 
 * contains a non null entry if the coresponding row of @A contains a nan.
 * if @flag = 1 a sparse row vector is returned. The i-th column of the row vector 
 * contains a non null entry if the coresponding column of @A contains a nan.
 * if @flag = 0. A 1x1 sparse is returned containing a non null entry is @A contains a nan.
 * 
 * The returned type should be changed to boolean sparse when thi stype will be implemented.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

typedef int (*Fis)(double) ;
static NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_isnan_gen(NspSpMaxpColMatrix *A,int flag, Fis F);

static int isnan_f(double r)
{
  /* isnan is a macro */
  return isnan(r);
}

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_isnan(NspSpMaxpColMatrix *A,int flag)
{
  return nsp_spmaxpcolmatrix_isnan_gen(A,flag,isnan_f);
}


static NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_isnan_gen(NspSpMaxpColMatrix *A,int flag, Fis F)
{
  int S=0;
  NspSpMaxpColMatrix *Res=NULL;
  int i,k,count,j;
  int c =  flag;
  if ( A->m == 0 || A->n == 0  )
    {
      switch (c ) 
	{
	case 0: 
	  return nsp_spmaxpcolmatrix_create(NVOID,'r',0,0);
	case 2:
	  return nsp_spmaxpcolmatrix_create(NVOID,'r',A->m,(A->n == 0) ? 0: 1);
	case 1: 
	  return nsp_spmaxpcolmatrix_create(NVOID,'r',(A->m == 0) ? 0: 1,A->n);
	case -1: 
	  return nsp_spmaxpcolmatrix_create(NVOID,'r',A->m,A->n);
	default: 
	  Scierror("Error: unknown dim flag %d\n", flag);
	  return NULLSPMAXPCOL;
	}
    }
  switch (c) 
    {
    case 0:
      /* return a 1x1 result */
      if ((Res =nsp_spmaxpcolmatrix_create(NVOID,'r',1,1)) == NULLSPMAXPCOL) return(NULLSPMAXPCOL);
      switch ( A->rc_type) 
	{
	case 'r' : 
	  S=0;
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
		if ( F(A->D[i]->R[j]) ) {  S=1; break;}
	      if ( S== 1) break;
	    }
	  break;
	case 'c' :  
	  S = 0; 
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
		if ( F(A->D[i]->C[j].r) ||  F(A->D[i]->C[j].i) ) {  S=1; break;}
	      if ( S== 1) break;
	    }
	  break;
	}
      if ( S != 0) 
	{
	  if (nsp_spmaxpcolmatrix_resize_col(Res,0,1)== FAIL) return NULLSPMAXPCOL;
	  Res->D[0]->R[0] = 1;
	  Res->D[0]->J[0] = 0;
	}
      break;
    case 2:
      /* return a A->mx1 result */
      if ((Res =nsp_spmaxpcolmatrix_create(NVOID,'r',A->m,1)) == NULLSPMAXPCOL) return NULLSPMAXPCOL;
      if (nsp_spmaxpcolmatrix_resize_col(Res,0,A->m)== FAIL) return NULLSPMAXPCOL;
      for ( k=0 ; k < Res->D[0]->size ; k++) 
	{
	  Res->D[0]->J[k]=k;
	  Res->D[0]->R[k]=0.0;
	}
      for ( i = 0 ; i < A->n ; i++) 
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      switch ( A->rc_type ) 
		{
		case 'r' : if ( F( A->D[i]->R[k]))  Res->D[0]->R[A->D[i]->J[k]] =1; break;
		case 'c' : if ( F( A->D[i]->C[k].r) ||F( A->D[i]->C[k].i)) 
		    Res->D[0]->R[A->D[i]->J[k]] = 1;
		  break;
		}
	    }
	}
      count =0;
      for ( k=0 ; k < Res->D[0]->size ; k++) 
	{
	  if ( Res->D[0]->R[k] == 0.0 ) { count=1; Res->D[0]->J[k]=-1;}
	}
      if ( count != 0 ) 
	{
	  int ndel =nsp_spmaxpcolmatrix_compress_col(Res,0);
	  if (nsp_spmaxpcolmatrix_resize_col(Res,0,Res->D[0]->size-ndel ) == FAIL) return NULLSPMAXPCOL;
	}
      break;
    case 1: 
      /* return a 1x A->n result */
      if ((Res =nsp_spmaxpcolmatrix_create(NVOID,'r',1,A->n)) == NULLSPMAXPCOL) return NULLSPMAXPCOL;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      int S=0;
	      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
		if ( F(A->D[i]->R[j]) ) {  S=1; break;}
	      if ( S != 0.0 ) 
		{
		  if (nsp_spmaxpcolmatrix_resize_col(Res,i,1)== FAIL) return NULLSPMAXPCOL;
		  Res->D[i]->R[0] = 1;
		  Res->D[i]->J[0] = 0;
		}
	    }
	  break ;
	case 'c' :  
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      int S=0;
	      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
		if ( F(A->D[i]->C[j].r) ||  F(A->D[i]->C[j].i) ) {  S=1; break;}
	      if ( S  != 0.0 ) 
		{
		  if (nsp_spmaxpcolmatrix_resize_col(Res,i,1)== FAIL) return NULLSPMAXPCOL;
		  Res->D[i]->R[0] = 1;
		  Res->D[i]->J[0] = 0;
		}
	    }
	  break;
	}
      break;
    case -1: 
      if ((Res =nsp_spmaxpcolmatrix_create(NVOID,'r',A->m,A->n)) == NULLSPMAXPCOL) return NULLSPMAXPCOL;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      int k=0;
	      if (nsp_spmaxpcolmatrix_resize_col(Res,i,A->D[i]->size)== FAIL) return NULLSPMAXPCOL;
	      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
		if ( F(A->D[i]->R[j]) ) 
		  {
		    Res->D[i]->J[k]=A->D[i]->J[j];
		    Res->D[i]->R[k]=1;
		    k++;
		  }
	      if (nsp_spmaxpcolmatrix_resize_col(Res,i,k)== FAIL) return NULLSPMAXPCOL;
	    }
	  break ;
	case 'c' :  
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      int k=0;
	      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
		if ( F(A->D[i]->C[j].r) ||  F(A->D[i]->C[j].i) )
		  {
		    Res->D[i]->J[k]=A->D[i]->J[j];
		    Res->D[i]->R[k]=1;
		    k++;
		  }
	      if (nsp_spmaxpcolmatrix_resize_col(Res,i,k)== FAIL) return NULLSPMAXPCOL;
	    }
	  break;
	}
      break;
    default: 
      Scierror("Error: unknown flag %d \n", flag);
      return NULLSPMAXPCOL;
    }
  return Res;
}

/**
 * nsp_spmaxpcolmatrix_isinf:
 * @A: a #NspSpMaxpColMatrix
 * @flag: an integer. 
 * 
 * if @flag is -1 then a new #NspSpMaxpColMatrix is returned 
 * containing for each entries of the matrix @A the value of isinf(A(i;j)).
 * 
 * if @flag = 2 a sparse column vector is returned. The i-th row of the column vector 
 * contains a non null entry if the coresponding row of @A contains a nan.
 * if @flag = 1 a sparse row vector is returned. The i-th column of the row vector 
 * contains a non null entry if the coresponding column of @A contains a nan.
 * if @flag=0. A 1x1 sparse is returned containing a non null entry is @A contains a nan.
 * 
 * The returned type should be changed to boolean sparse when thi stype will be implemented.
 * 
 * Return value: a new  #NspSpMaxpColMatrix or %NULLSPMAXPCOL
 **/

static int isinf_f(double r)
{
  /* isinf can be a macro */
  return isinf(r);
}

NspSpMaxpColMatrix *nsp_spmaxpcolmatrix_isinf(NspSpMaxpColMatrix *A,int flag)
{
  return nsp_spmaxpcolmatrix_isnan_gen(A,flag,isinf_f);
}


/**
 * nsp_spmaxpcolmatrix_is_lower_triangular:
 * @A: a #NspSpMaxpColMatrix
 * 
 * the code should work even if zero elements are stored
 *
 * Return value: %TRUE or %FALSE
 **/

Boolean nsp_spmaxpcolmatrix_is_lower_triangular(const NspSpMaxpColMatrix *A)
{
  int i, j, k;

  if ( A->m != A->n )
    return FALSE;

  switch ( A->rc_type ) 
    {
    case 'r': 
      for ( j = 1 ; j < A->n ; j++) 
	{
	  for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	    {
	      i = A->D[j]->J[k];
	      if ( i >= j ) 
		break;
	      else if ( A->D[j]->R[k] != 0.0 )
		return FALSE;
	    }
	}
      break;
    case 'c': 
      for ( j = 1 ; j < A->n ; j++) 
	{
	  for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	    {
	      i = A->D[j]->J[k];
	      if ( i >= j ) 
		break;
	      else if ( A->D[j]->C[k].r != 0.0 || A->D[j]->C[k].i != 0.0 )
		return FALSE;
	    }
	}
      break;
    }
  return TRUE;
}

/**
 * nsp_spmaxpcolmatrix_is_upper_triangular:
 * @A: a #NspSpMaxpColMatrix
 * 
 * the code should work even if zero elements are stored
 *
 * Return value: %TRUE or %FALSE
 **/

Boolean nsp_spmaxpcolmatrix_is_upper_triangular(NspSpMaxpColMatrix *A)
{
  int i, j, k;

  if ( A->m != A->n )
    return FALSE;

  switch ( A->rc_type ) 
    {
    case 'r': 
      for ( j = 0 ; j < A->n-1 ; j++) 
	{
	  for ( k = A->D[j]->size-1 ; k >= 0  ; k-- ) 
	    {
	      i = A->D[j]->J[k];
	      if ( i <= j ) 
		break;
	      else if ( A->D[j]->R[k] != 0.0 )
		return FALSE;
	    }
	}
      break;
    case 'c': 
      for ( j = 0 ; j < A->n-1 ; j++) 
	{
	  for ( k = A->D[j]->size-1 ; k >= 0  ; k-- ) 
	    {
	      i = A->D[j]->J[k];
	      if ( i <= j ) 
		break;
	      else if ( A->D[j]->C[k].r != 0.0 || A->D[j]->C[k].i != 0.0 )
		return FALSE;
	    }
	}
      break;
    }
  return TRUE;
}

int nsp_spmaxpcolmatrix_locate(SpCol *Col,int j)
{
  int k, n = Col->size, LIM = 16;

  if ( n == 0  ||  j < Col->J[0]  ||  Col->J[n-1] < j )
    return -1;

  if ( n < LIM )
    {
      for ( k = 0 ; k < n ; k++ )
	{
	  if ( j == Col->J[k] )
	    return k;
	  else if ( j < Col->J[k] )
	    return -1;
	}
    }
  else  /* dicho search */
    {
      int k1 = 0, k2 = n-1, km;
      while ( k2 - k1 > 1 )
	{
	  km = (k1+k2)/2;
	  if ( j < Col->J[km] )
	    k2 = km;
	  else
	    k1 = km;
	}
      if ( j == Col->J[k1] )
	return k1;
      else if ( j == Col->J[k2] )
	return k2;
    }
  return -1;
}

/**
 * nsp_spmaxpcolmatrix_is_symmetric:
 * @A: a #NspSpMaxpColMatrix
 * 
 * the code should work even if zero elements are stored
 *
 * Return value: %TRUE or %FALSE
 **/

Boolean nsp_spmaxpcolmatrix_is_symmetric(NspSpMaxpColMatrix *A)
{
  int i, j, k, kp;

  if ( A->m != A->n )
    return FALSE;

  if ( A->rc_type == 'r' ) 
    {
      double val_ij, val_ji;
      for ( j = 0 ; j < A->n-1 ; j++ ) 
	{
	  for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	    {
	      val_ij = A->D[j]->R[k];
	      i = A->D[j]->J[k];
	      if ( i != j  &&  val_ij != 0.0 )
		{
		  kp = nsp_spmaxpcolmatrix_locate(A->D[i],j);
		  if ( kp < 0 )
		    return FALSE;
		  val_ji = A->D[i]->R[kp];
		  if ( val_ij != val_ji )
		    return FALSE;
		}
	    }
	}
    }
  else
    {
      doubleC val_ij, val_ji;
      for ( j = 0 ; j < A->n-1 ; j++ ) 
	{
	  for ( k = A->D[j]->size-1 ; k >= 0 ; k-- ) 
	    {
	      val_ij = A->D[j]->C[k];
	      i = A->D[j]->J[k];
	      if ( i != j  &&  (val_ij.r != 0.0 || val_ij.i != 0.0) )
		{
		  kp = nsp_spmaxpcolmatrix_locate(A->D[i],j);
		  if ( kp < 0 )
		    return FALSE;
		  val_ji = A->D[i]->C[kp];
		  if ( val_ij.r != val_ji.r  ||  val_ij.i != -val_ji.i ) 
		    return FALSE;
		}
	    }
	}
    }
  return TRUE;
}

/**
 * nsp_spmaxpcolmatrix_lower_and_upper_bandwidth:
 * @A: a real or complex sparse matrix
 * @Kl: (output) the lower bandwidth
 * @Ku: (output) the upper bandwidth
 *
 * computes the lower and upper bandwith of @A
 * don't take into account stored zeros.
 * 
 * Return value: %FAIL (if the matrix is not square) or %OK 
 **/

int nsp_spmaxpcolmatrix_lower_and_upper_bandwidth(NspSpMaxpColMatrix *A, int *Kl, int *Ku)
{
  int j, k, kl=0, ku=0;

  if ( A->m != A->n )
    return FAIL;

  for ( j = 0 ; j < A->n ; j++ ) 
    {
      k = A->D[j]->size;
      if ( k != 0 )
	{
	  kl = Max( kl, A->D[j]->J[k-1] - j );
	  ku = Max( ku, j - A->D[j]->J[0]);
	}
    }
  *Kl = kl; *Ku = ku;
  return OK;
}

/**
 * nsp_spmaxpcolmatrix_to_lapack_band_format:
 * @A: a real or complex sparse matrix (a priori with small bandwith kl and ku)
 * @kl: the lower bandwidth of @A
 * @ku: the upper bandwidth of @A
 * @enlarge: true if the banded lapack format is enlarged at top with kl rows (useful
 *           for lapack band solvers)
 *
 * Return value: a #NspMatrix: the lapack banded storage of @A (kl and ku should have 
 *               been computed by #nsp_spmaxpcolmatrix_lower_and_upper_bandwidth)
 **/

NspMatrix *nsp_spmaxpcolmatrix_to_lapack_band_format(NspSpMaxpColMatrix *A, int kl, int ku, Boolean enlarge)
{
  int i, j, k, stride = enlarge ? kl : 0;
  NspMatrix *Af;

  if ( (Af = nsp_matrix_create(NVOID,A->rc_type, stride+kl+1+ku, A->n)) == NULLMAT ) 
    return NULLMAT;

  if ( Af->rc_type == 'r' )
    {
      /* fill Af with zeros */
      for ( k = 0 ; k < Af->mn ; k++ )
	Af->R[k] = 0.0;
      /* fill Af with non zeros values of A */
      for ( j = 0 ; j < A->n ; j++ ) 
	for ( k = 0; k < A->D[j]->size ; k++ )
	  {
	    i = stride + A->D[j]->J[k]; 
	    Af->R[i + Af->m*j] = A->D[j]->R[k];
	  }
    }
  else
    {
      /* fill Af with zeros */
      for ( k = 0 ; k < Af->mn ; k++ )
	{
	  Af->C[k].r = 0.0; Af->C[k].i = 0.0;
	}
      /* fill Af with non zeros values of A */
      for ( j = 0 ; j < A->n ; j++ ) 
	for ( k = 0; k < A->D[j]->size ; k++ )
	  {
	    i = stride + A->D[j]->J[k]; 
	    Af->C[i + Af->m*j] = A->D[j]->C[k];
	  }
    }

  return Af;
}

/**
 * nsp_spmaxpcolmatrix_solve_utri
 * @U: a #NspSpMaxpColMatrix
 * @b: a #NspMatrix
 * @x: a #NspMatrix
 *
 * solve U x = b when @U is upper triangular
 * the code works only with the upper triangular
 * part of @U (so that @U need not to be upper 
 * triangular)
 * x must be preallocated with the good size
 * and good rc_type ('r' or 'c')
 *
 * Return value: 0 means OK and i > 0 means that U(i,i) = 0
 *              -1 means a dim problem or a bad type for x
 *               
 **/

int nsp_spmaxpcolmatrix_solve_utri(NspSpMaxpColMatrix *U, NspMatrix *x, NspMatrix *b)
{
  int i, j, jj, k, kk;
  char type;
  Boolean found;

  /* check dims */
  if ( U->m != U->n  ||  b->m != U->m  ||  x->m != U->m  ||  x->n != b->n )
    return -1;

  /* check rc_type */
  type = (U->rc_type == 'r' &&  b->rc_type == 'r') ? 'r' : 'c';
  if ( x->rc_type != type )
    return -1;

  /* init x with b */
  if ( x->rc_type == 'r' )
    for ( i = 0 ; i < x->mn ; i++ ) x->R[i] = b->R[i];
  else if ( b->rc_type == 'c' )
    for ( i = 0 ; i < x->mn ; i++ ) x->C[i] = b->C[i];
  else  /* b real and x complex (because U is complex) */
    for ( i = 0 ; i < x->mn ; i++ ) 
      { x->C[i].r = b->R[i]; x->C[i].i = 0.0; }

  /* solve now (column triangular algorithm because U is a SpMaxpColMat) */
  for ( j = U->m-1 ; j >= 0 ; j-- ) 
    {
      /* look for row index i = j (because the upper form is not mandatory) */
      for ( k = U->D[j]->size-1, found = FALSE; k >= 0 ; k-- )  
	{
	  i = U->D[j]->J[k];
	  if ( i == j ) { found = TRUE; break; }
	  else if ( i < j ) return j+1;
	}
      if ( ! found ) return j+1;

      if ( U->rc_type == 'r' )
	{
	  double piv;
	  piv = U->D[j]->R[k]; 
	  /* U(j,j) is stored but verify if it is not zero */
	  if ( piv == 0.0 )return j+1;

	  if ( x->rc_type == 'r' )
	    {
	      for ( jj = 0 ; jj < x->n ; jj++ )
		{
		  double *xjj = x->R + jj*x->m;
		  xjj[j] /= piv;
		  for ( kk = k-1 ; kk >= 0 ; kk-- )
		    {
		      i = U->D[j]->J[kk];
		      xjj[i] -= U->D[j]->R[kk]*xjj[j];
		    }
		}
	    }
	  else  /* U real but x complex (because b is complex) */
	    {
	      for ( jj = 0 ; jj < x->n ; jj++ )
		{
		  doubleC *xjj = x->C + jj*x->m;
		  xjj[j].r /= piv; xjj[j].i /= piv;
		  for ( kk = k-1 ; kk >= 0 ; kk-- )
		    {
		      i = U->D[j]->J[kk];
		      xjj[i].r -= U->D[j]->R[kk]*xjj[j].r;
		      xjj[i].i -= U->D[j]->R[kk]*xjj[j].i;
		    }
		}
	      
	    }
	}
      else  /* U is complex and so x too */
	{
	  doubleC piv;
	  piv = U->D[j]->C[k];
	  /* U(j,j) is stored but verify if it is not zero */
	  if ( piv.r == 0.0  &&  piv.i == 0.0 ) return j+1;

	  for ( jj = 0 ; jj < x->n ; jj++ )
	    {
	      doubleC *xjj = x->C + jj*x->m;
	      nsp_div_cc(&xjj[j],&piv,&xjj[j]);
	      for ( kk = k-1 ; kk >= 0 ; kk-- )
		{
		  i = U->D[j]->J[kk];
		  xjj[i].r -= U->D[j]->C[kk].r*xjj[j].r - U->D[j]->C[kk].i*xjj[j].i;
		  xjj[i].i -= U->D[j]->C[kk].r*xjj[j].i + U->D[j]->C[kk].i*xjj[j].r;
		}
	    }
	}
    }
  return 0;
}

/**
 * nsp_spmaxpcolmatrix_solve_ltri
 * @L: a #NspSpMaxpColMatrix
 * @b: a #NspMatrix
 * @x: a #NspMatrix
 *
 * solve L x = b when @L is lower triangular
 * the code works only with the lower triangular
 * part of @L (so that @L need not to be upper 
 * triangular)
 * x must be preallocated with the good size
 * and good rc_type ('r' or 'c')
 *
 * Return value: 0 means OK and i > 0 means that L(i,i) = 0
 *              -1 means a dim problem or a bad type for x
 *               
 **/

int nsp_spmaxpcolmatrix_solve_ltri(NspSpMaxpColMatrix *L, NspMatrix *x, NspMatrix *b)
{
  int i, j, jj, k, kk;
  char type;
  Boolean found;

  /* check dims */
  if ( L->m != L->n  ||  b->m != L->m  ||  x->m != L->m  ||  x->n != b->n )
    return -1;

  /* check rc_type */
  type = (L->rc_type == 'r' &&  b->rc_type == 'r') ? 'r' : 'c';
  if ( x->rc_type != type )
    return -1;

  /* init x with b */
  if ( x->rc_type == 'r' )
    for ( i = 0 ; i < x->mn ; i++ ) x->R[i] = b->R[i];
  else if ( b->rc_type == 'c' )
    for ( i = 0 ; i < x->mn ; i++ ) x->C[i] = b->C[i];
  else  /* b real and x complex (because L is complex) */
    for ( i = 0 ; i < x->mn ; i++ ) 
      { x->C[i].r = b->R[i]; x->C[i].i = 0.0; }

  /* solve now (column triangular algorithm because L is a SpMaxpColMat) */
  for ( j = 0 ; j < L->m ; j++ ) 
    {
      /* look for row index i = j (because the lower form is not mandatory) */
      for ( k = 0, found = FALSE ; k < L->D[j]->size ; k++ )  
	{
	  i = L->D[j]->J[k];
	  if ( i == j ) { found = TRUE; break; }
	  else if ( i > j ) return j+1;
	}
      if ( ! found ) return j+1;

      if ( L->rc_type == 'r' )
	{
	  double piv;
	  piv = L->D[j]->R[k]; 
	  /* L(j,j) is stored but verify if it is not zero */
	  if ( piv == 0.0 )return j+1;

	  if ( x->rc_type == 'r' )
	    {
	      for ( jj = 0 ; jj < x->n ; jj++ )
		{
		  double *xjj = x->R + jj*x->m;
		  xjj[j] /= piv;
		  for ( kk = k+1 ; kk < L->D[j]->size ; kk++ )
		    {
		      i = L->D[j]->J[kk];
		      xjj[i] -= L->D[j]->R[kk]*xjj[j];
		    }
		}
	    }
	  else  /* L real but x complex (because b is complex) */
	    {
	      for ( jj = 0 ; jj < x->n ; jj++ )
		{
		  doubleC *xjj = x->C + jj*x->m;
		  xjj[j].r /= piv; xjj[j].i /= piv;
		  for ( kk = k+1 ; kk < L->D[j]->size ; kk++ )
		    {
		      i = L->D[j]->J[kk];
		      xjj[i].r -= L->D[j]->R[kk]*xjj[j].r;
		      xjj[i].i -= L->D[j]->R[kk]*xjj[j].i;
		    }
		}
	      
	    }
	}
      else  /* L is complex and so x too */
	{
	  doubleC piv;
	  piv = L->D[j]->C[k];
	  /* L(j,j) is stored but verify if it is not zero */
	  if ( piv.r == 0.0  &&  piv.i == 0.0 ) return j+1;

	  for ( jj = 0 ; jj < x->n ; jj++ )
	    {
	      doubleC *xjj = x->C + jj*x->m;
	      nsp_div_cc(&xjj[j],&piv,&xjj[j]);
	      for ( kk = k+1 ; kk < L->D[j]->size ; kk++ )
		{
		  i = L->D[j]->J[kk];
		  xjj[i].r -= L->D[j]->C[kk].r*xjj[j].r - L->D[j]->C[kk].i*xjj[j].i;
		  xjj[i].i -= L->D[j]->C[kk].r*xjj[j].i + L->D[j]->C[kk].i*xjj[j].r;
		}
	    }
	}
    }
  return 0;
}

/**
 * nsp_spmaxpcolmatrix_scale_rows:
 * @A: a #NspSpMaxpColMatrix of size m x n
 * @x: a #NspMatrix must be a vector of size m (1 x m or m x 1)
 * @op: a char should be '*' to multiply and '/' to divide
 *
 *  for (i from 0 to m-1)  
 *      multiply or divide row i of A by x[i]
 * 
 * Return value: %FAIL or %OK
 **/

int nsp_spmaxpcolmatrix_scale_rows(NspSpMaxpColMatrix *A, NspMatrix *x, char op)
{
  int i,j, k;
  Boolean do_clean;
  
  if ( A->rc_type == 'r' ) 
    {
      if ( x->rc_type == 'r') 
	{
	  for ( j = 0 ; j < A->n ; j++)
	    {
	      do_clean = FALSE;
	      for ( k = 0 ; k < A->D[j]->size ; k++ )
		{
		  i = A->D[j]->J[k];
		  if ( op == '*' )
		    A->D[j]->R[k] *= x->R[i];
		  else
		    A->D[j]->R[k] /= x->R[i];
		  if ( A->D[j]->R[k] == 0.0 )
		    do_clean = TRUE;
		}
	      if ( do_clean )
		nsp_spmaxpcolmatrix_clean_zeros(A->D[j], A->rc_type);
	    }
	}
      else 
	{
	  if ( nsp_spmaxpcolmatrix_complexify(A) == FAIL ) 
	    return FAIL;
	  for ( j = 0 ; j < A->n ; j++)
	    {
	      do_clean = FALSE;
	      for ( k = 0 ; k < A->D[j]->size ; k++ ) 
		{
		  i = A->D[j]->J[k];
		  if ( op == '*' )
		    {
		      A->D[j]->C[k].i = A->D[j]->C[k].r * x->C[i].i;
		      A->D[j]->C[k].r *= x->C[i].r;
		    }
		  else
		    {
		      nsp_div_dc(A->D[j]->C[k].r, &x->C[i], &A->D[j]->C[k]);
		    }
		  if ( A->D[j]->C[k].r == 0.0 && A->D[j]->C[k].i == 0.0 )
		    do_clean = TRUE;
		}
	      if ( do_clean )
		nsp_spmaxpcolmatrix_clean_zeros(A->D[j], A->rc_type);
	    }
	}
    }
  else
    {
      if ( x->rc_type == 'r') 
	for ( j = 0 ; j < A->n ; j++)
	  {
	    do_clean = FALSE;
	    for ( k = 0 ; k <  A->D[j]->size ; k++ )
	      {
		i = A->D[j]->J[k];
		if ( op == '*' )
		  {
		    A->D[j]->C[k].r *= x->R[i];
		    A->D[j]->C[k].i *= x->R[i];
		  }
		else
		  {
		    A->D[j]->C[k].r /= x->R[i];
		    A->D[j]->C[k].i /= x->R[i];
		  }
		if ( A->D[j]->C[k].r == 0.0 && A->D[j]->C[k].i == 0.0 )
		  do_clean = TRUE;
	      }
	    if ( do_clean )
	      nsp_spmaxpcolmatrix_clean_zeros(A->D[j], A->rc_type);
	  }
      else 
	for ( j = 0 ; j < A->n ; j++)
	  {
	    do_clean = FALSE;
	    for ( k = 0 ; k <  A->D[j]->size ; k++ )
	      {
		i = A->D[j]->J[k];
		if ( op == '*' )
		  nsp_prod_c(&A->D[j]->C[k],&x->C[i]);
		else
		  nsp_div_cc(&A->D[j]->C[k],&x->C[i],&A->D[j]->C[k]);
		if ( A->D[j]->C[k].r == 0.0 && A->D[j]->C[k].i == 0.0 )
		  do_clean = TRUE;
	      }
	    if ( do_clean )
	      nsp_spmaxpcolmatrix_clean_zeros(A->D[j], A->rc_type);
	  }
    }
  return OK;
}

/**
 * nsp_spmaxpcolmatrix_scale_cols:
 * @A: a #NspSpMaxpColMatrix of size m x n
 * @x: a #NspMatrix must be a vector of size n (1 x n or n x 1)
 * @op: a char should be '*' to multiply and '/' to divide
 * 
 *  for (j from 0 to n-1)  
 *      multiply or divide column j of A by x[j]
 * 
 * Return value: %FAIL or %OK
 **/

int nsp_spmaxpcolmatrix_scale_cols(NspSpMaxpColMatrix *A, NspMatrix *x, char op)
{
  int j, k;
  Boolean do_clean;

  if(A->rc_type == 'r' ) 
    {
      if ( x->rc_type == 'r') 
	{
	  for ( j = 0 ; j < A->n ; j++)
	    {
	      do_clean = FALSE;
	      for ( k = 0 ; k < A->D[j]->size ; k++ ) 
		{
		  if ( op == '*' )
		    A->D[j]->R[k] *= x->R[j];
		  else
		    A->D[j]->R[k] /= x->R[j];
		  if ( A->D[j]->R[k] == 0.0 )
		    do_clean = TRUE;
		}
	      if ( do_clean )
		nsp_spmaxpcolmatrix_clean_zeros(A->D[j], A->rc_type);
	    }
	}
      else 
	{
	  if ( nsp_spmaxpcolmatrix_complexify(A) == FAIL ) 
	    return FAIL;
	  for ( j = 0 ; j < A->n ; j++)
	    {
	      do_clean = FALSE;
	      for ( k = 0 ; k < A->D[j]->size ; k++ ) 
		{
		  if ( op == '*' )
		    {
		      A->D[j]->C[k].i = A->D[j]->C[k].r * x->C[j].i;
		      A->D[j]->C[k].r *= x->C[j].r;
		    }
		  else
		    {
		      nsp_div_dc(A->D[j]->C[k].r, &x->C[j], &A->D[j]->C[k]);
		    }
		  if ( A->D[j]->C[k].r == 0.0 && A->D[j]->C[k].i == 0.0 )
		    do_clean = TRUE;
		}
	      if ( do_clean )
		nsp_spmaxpcolmatrix_clean_zeros(A->D[j], A->rc_type);
	    }
 	}
    }
  else
    {
      if ( x->rc_type == 'r') 
	for ( j = 0 ; j < A->n ; j++)
	  {
	    do_clean = FALSE;
	    for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	      {
		if ( op == '*' )
		  {
		    A->D[j]->C[k].r *= x->R[j];
		    A->D[j]->C[k].i *= x->R[j];
		  }
		else
		  {
		    A->D[j]->C[k].r /= x->R[j];
		    A->D[j]->C[k].i /= x->R[j];
		  }
		if ( A->D[j]->C[k].r == 0.0 && A->D[j]->C[k].i == 0.0 )
		  do_clean = TRUE;
	      }
	    if ( do_clean )
	      nsp_spmaxpcolmatrix_clean_zeros(A->D[j], A->rc_type);
	  }
      else 
	for ( j = 0 ; j < A->n ; j++)
	  {
	    do_clean = FALSE;
	    for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	      {
		if ( op == '*' )
		  nsp_prod_c(&A->D[j]->C[k],&x->C[j]);
		else
		  nsp_div_cc(&A->D[j]->C[k],&x->C[j],&A->D[j]->C[k]);
		if ( A->D[j]->C[k].r == 0.0 && A->D[j]->C[k].i == 0.0 )
		  do_clean = TRUE;
	      }
	    if ( do_clean )
	      nsp_spmaxpcolmatrix_clean_zeros(A->D[j], A->rc_type);
	  }
    }
  return OK;
}
