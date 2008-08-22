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
#include "nsp/nsp_lapack.h" /* vector_norm */
#include "../librand/grand.h"

static int nsp_spcolmatrix_print_internal(nsp_num_formats *fmt,NspSpColMatrix *m, int indent);
/* In file Perm.c **/

extern int C2F(dperm) (double A[],int ind[],int *nv);
extern int C2F(zperm) (doubleC A[],int ind[],int *nv);

typedef void (*BopLeft) (SpCol *,char,int *,SpCol *,char,int);
typedef void (*BopBoth) (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
typedef void (*BopBothNull) (SpCol *,char,int *);
typedef void (*BopRight) (SpCol *,char,int *,SpCol *,char,int);

static NspSpColMatrix *BinaryOp (NspSpColMatrix *,NspSpColMatrix *,BopLeft,BopBoth,
				 BopRight,int force_real);
static NspSpColMatrix *BinaryOp_bis(NspSpColMatrix *A, NspSpColMatrix *B, BopLeft BinLeft, BopBoth BinBoth, 
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

/**
 * nsp_spcolmatrix_create:
 * @name: name of object 
 * @type: a character to code complex or real matrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a #NspSColMatrix of size @mx@n with no stored data
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_create(char *name, char type, int m, int n)
{
  int i;
  NspSpColMatrix *Sp = new_spcolmatrix();
  if ( Sp == NULLSPCOL) 
    {
      Scierror("No more space\n");      return(NULLSPCOL);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(Sp),name) == NULL)
    return(NULLSPCOL);
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
      return(NULLSPCOL);
    }
  for ( i = 0  ; i < Sp->n ; i++) 
    {
      Sp->D[i] = ( SpCol *) MALLOC( sizeof( SpCol));
      if ( Sp->D[i] == ( SpCol *) 0) 
	{
	  Scierror("No More Space\n");
	  return(NULLSPCOL);
	}
      Sp->D[i]->size = 0 ;
    }
  return(Sp);
}


/**
 * nsp_spcolmatrix_sparse:
 * @name: a string 
 * @RC: a  #NspMatrix 
 * @Values: a #NspMatrix 
 * @m: an integer 
 * @n: an integer
 * 
 * Creates a #NspSColMatrix of size @mx@n filed with values specified 
 * in @RC ((i,j) values stored in a two column matrix) and @Values 
 * ( A(@RC(k,1),@RC(k,2))= Values(k)).
 * Rewritten by Bruno Pincon  to cumulate values when specific
 * indices are repeated as in Matlab (fix also a bug and test
 * if indices are integer)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_sparse(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n)
{
  double *rows, *cols;
  NspSpColMatrix *Loc = NULLSPCOL;
  int *jc_ir=NULL, *ir, *p=NULL;
  int k,kf,first_k,kk,ind,i,imax,j,jmax, colsize, colsize_init;

  if ( (jc_ir=nsp_alloc_work_int(RC->mn)) == NULL || (p=nsp_alloc_work_int(RC->mn)) == NULL ) 
    goto err;
 
  imax = 0; jmax = 0;
  rows = RC->R; cols = rows + RC->m;
  ir = jc_ir + RC->m;

  /* check indices then swap cols and rows stored in jc_ir for lexical ordering */
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
      jc_ir[k] = j-1; ir[k] = i-1;
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

  /* sort jc_ir lexical row increasing */
  nsp_qsort_gen_lexirow_int(jc_ir, p, 1, RC->m, 2, 'i');
  for ( k = 0 ; k < RC->m ; k++ ) p[k]--;

  /* allocate space for Loc **/
  if ( (Loc =nsp_spcolmatrix_create(name,Values->rc_type,m,n))== NULLSPCOL)
    goto err;

  if ( RC->m == 0 ) return Loc;

  /* now form the sparse matrix, column by column */
  j = jc_ir[0];
  first_k = 0; k = 1 ;
  while ( k < RC->m )
    {
      if (jc_ir[k] != j  ||  k == RC->m-1 ) /* a column is ended, there are 2 cases */
	/*
	 *    1/ elem k is the first of a new column => column is [first_k:k-1]
	 *    2/ elem k is on the same column but is the last one => column [first_k:k] 
	 */
	{
	  if ( jc_ir[k] != j ) kf = k-1;  /* case 1 */
	  else                 kf = k;    /* case 2 */
	  colsize = colsize_init = kf - first_k + 1;
	  if ( nsp_spcolmatrix_resize_col(Loc,j,colsize_init) == FAIL ) goto err;
	  kk = first_k;
	  if ( Values->rc_type == 'r' ) /*** scan of the column - real case ***/
	    {
	      /* 1/ look for the first non zero coef of the column */
	      while ( kk <= kf  &&  Values->R[p[kk]] == 0.0 ) {colsize--; kk++;}
	      if ( colsize > 0 )
		{
		  /* 2/ init with the first non null coef (which can become null after summation in some case...) */
		  Loc->D[j]->R[0] = Values->R[p[kk]]; Loc->D[j]->J[0] = ir[kk];
		  ind = 0; kk++;
		  /* 3/ parse the rest of the column */
		  while ( kk <= kf ) 
		    {
		      if ( ir[kk] == ir[kk-1] )  /* same row index => add coef */
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
			  Loc->D[j]->R[ind] = Values->R[p[kk]]; Loc->D[j]->J[ind] = ir[kk];
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
		  Loc->D[j]->C[0] = Values->C[p[kk]]; Loc->D[j]->J[0] = ir[kk];
		  ind = 0; kk++;
		  /* 3/ parse the rest of the column */
		  while ( kk <= kf ) 
		    {
		      if ( ir[kk] == ir[kk-1] ) /* same row index => add coef */
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
			  Loc->D[j]->C[ind] = Values->C[p[kk]]; Loc->D[j]->J[ind] = ir[kk];
			}
		      kk++;
		    }
		  /* 4/ the last one coef of the column could have been cancelled too */
		  if ( Loc->D[j]->C[ind].r == 0.0  &&  Loc->D[j]->C[ind].i == 0.0 )
		    colsize--;
		}
	    }
	  
	  if ( colsize < colsize_init ) /* due to zero coef or cancellation we have to resize */
	    if ( nsp_spcolmatrix_resize_col(Loc,j,colsize) == FAIL ) 
	      goto err;

	  first_k = kf+1;      /* index of the new column */
	  j = jc_ir[first_k];  /* column number of the new column */
	}
      k++;
    }

  /* if the last column is formed of only one element it has not been inserted in the sparse matrix */
  k = RC->m-1;
  if ( first_k == k )
    {
      j = jc_ir[k];
      if ( Values->rc_type == 'r' && Values->R[k] != 0.0 )
	{
	  if ( nsp_spcolmatrix_resize_col(Loc,j,1) == FAIL ) goto err;
	  Loc->D[j]->R[0] = Values->R[p[k]];
	  Loc->D[j]->J[0] = ir[k];
	}
      else if ( Values->rc_type == 'c' &&  (Values->C[k].r != 0.0 || Values->C[k].i != 0.0) )
	{
	  if ( nsp_spcolmatrix_resize_col(Loc,j,1) == FAIL ) goto err;
	  Loc->D[j]->C[0] = Values->C[p[k]];
	  Loc->D[j]->J[0] = ir[k];
	}
    }
     
  FREE(jc_ir);
  FREE(p);
  return Loc;

 err:
  FREE(jc_ir);
  FREE(p);
  nsp_spcolmatrix_destroy(Loc);
  return NULLSPCOL;
}


/**
 * nsp_spcolmatrix_get:
 * @A: a #NspSpColMatrix
 * @RC: 
 * @Values: 
 * 
 * get a @RC,@Values description of the sparse matrix @A
 * 
 * Return value: %OK or %FAIL
 **/
int nsp_spcolmatrix_get(NspSpColMatrix *A, NspMatrix **RC, NspMatrix **Values)
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
 * nsp_spcolmatrix_copy:
 * @A: a #NspSpColMatrix
 * 
 * returns a copy of sparse matrix @A.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_copy(NspSpColMatrix *A)
{
  int i;
  int inc=1;
  NspSpColMatrix *Sp;
  Sp =nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,A->n);
  if ( Sp == NULLSPCOL ) return(NULLSPCOL) ; 
  for ( i = 0  ; i < Sp->n ; i++) 
    {
      if (nsp_spcolmatrix_resize_col(Sp,i,(int)A->D[i]->size) == FAIL) return(NULLSPCOL);
      nsp_icopy(&A->D[i]->size,A->D[i]->J,&inc,Sp->D[i]->J,&inc);
      if ( A->rc_type == 'r' ) 
	C2F(dcopy)(&A->D[i]->size,A->D[i]->R,&inc,Sp->D[i]->R,&inc);
      else 
	C2F(zcopy)(&A->D[i]->size,A->D[i]->C,&inc,Sp->D[i]->C,&inc);
    }
  return(Sp);
}


/**
 * nsp_spcolmatrix_resize_col:
 * @Sp: a #NspSpColMatrix
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

int nsp_spcolmatrix_resize_col(NspSpColMatrix *Sp, int i, int n)
{
  SpCol *Row;
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

/**
 * nsp_spcolmatrix_col_destroy:
 * @Col: a #SpCol object 
 * 
 * internal: destroys a #SpCol structure.
 * 
 **/

void nsp_spcolmatrix_col_destroy(SpCol *Col)
{
  if ( Col->size != 0 ) 
    {
      FREE( Col->J);
      FREE( Col->R);
    }
  FREE(Col);
}

/**
 * nsp_spcolmatrix_destroy:
 * @Mat: a #NspSpColMatrix
 * 
 * destroys a #NspSpColMatrix.
 * 
 **/

void nsp_spcolmatrix_destroy(NspSpColMatrix *Mat)
{
  int i;
  if ( Mat != NULLSPCOL )
    {
      nsp_object_destroy_name(NSP_OBJECT(Mat));
      if ( Mat->D != NULL) 
	for ( i = 0  ; i < Mat->n ; i++) 
	  {
	    nsp_spcolmatrix_col_destroy(Mat->D[i]);
	  }
      FREE(Mat->D);
      FREE(Mat) ;
    }
}

/**
 * nsp_spcolmatrix_nnz:
 * @HMat: a #NspSpColMatrix
 * 
 * computes the number of non nul elements stored in 
 * a sparse Matrix. 
 * 
 * Return value: the number of non nul elements.
 **/
/*  (added by Bruno) */

int nsp_spcolmatrix_nnz(const NspSpColMatrix *HMat)
{
  int i, nnz=0;
  for ( i = 0 ; i < HMat->n ; i++ )
    nnz += HMat->D[i]->size;
  return nnz;
}


/**
 * nsp_spcolmatrix_info:
 * @Sp: a #NspSpColMatrix
 * @indent: an integer 
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * Display info on the sparse matrix @Sp using the default 
 * Sciprintf() function. 
 * @indent is the given indentation for printing.
 **/

int nsp_spcolmatrix_info(NspSpColMatrix *Sp, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Sp)->name;
  if ( Sp == NULLSPCOL) 
    {
      Sciprintf("Null SpMatrix pointer\n");
      return TRUE;
    }
  Sciprintf1(indent,"%s\t= [...]\t\tspcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
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

int nsp_spcolmatrix_print(NspSpColMatrix *Sp, int indent,char *name, int rec_level)
{ 
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Sp)->name; 
  if (user_pref.pr_as_read_syntax)
    {
      const int name_len=128;
      char epname[name_len];
      NspMatrix *RC,*Values;
      if ( nsp_spcolmatrix_get(Sp,&RC,&Values)== FAIL)
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
	  Sciprintf1(indent,"%s\t= []\t\tspcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	}
      else
	{
	  nsp_num_formats fmt;
	  if ( user_pref.pr_depth  <= rec_level -1 ) 
	    {
	      Sciprintf1(indent,"%s\t= [...]\t\tspcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	      return rep;
	    }
	  nsp_init_pr_format (&fmt);
	  Sciprintf1(indent,"%s\t=\t\tspcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	  rep = nsp_spcolmatrix_print_internal(&fmt,Sp,indent+1);
	}
    }
  return rep;
}

/**
 * nsp_spcolmatrix_redim:
 * @A: a #NspSpColMatrix
 * @m: an integer 
 * @n: an integer
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

NspSpColMatrix *nsp_spcolmatrix_redim(NspSpColMatrix *A, int m, int n)
{
  int *xb;
  int i,k;
  NspSpColMatrix *Loc;
  if ( A->m*A->n !=  m*n )  /* possible overflow */
    {
      Scierror("Error:\tCannot change size to (%dx%d) since matrix has %d elements\n",m,n,A->m*A->n);
      return(NULLSPCOL);
    }
  if ((Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,m,n))== NULLSPCOL ) 
    return NULLSPCOL;
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
      if (nsp_spcolmatrix_resize_col(Loc,i, Loc->D[i]->iw)== FAIL) return NULLSPCOL;
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
  
  /* we sort each col in increasing column order **/
  if ( (xb =nsp_alloc_int(Loc->m)) == (int*) 0) return(NULLSPCOL);
  for (i = 0 ; i < Loc->n ; ++i) 
    {
      if (Loc->D[i]->size > 1) 
	{
	  nsp_qsort_int(Loc->D[i]->J,xb,TRUE,Loc->D[i]->size,'i');
	  if ( Loc->rc_type == 'r' ) 
	    C2F(dperm)(Loc->D[i]->R,&Loc->D[i]->size,xb);
	  else 
	    C2F(zperm)(Loc->D[i]->C,&Loc->D[i]->size,xb);
	}
    }
  FREE(xb);
  return Loc;
}

/*
 */

/**
 * nsp_spcolmatrix_enlarge_cols:
 * @Sp: a #NspSpColMatrix
 * @n: an integer 
 * 
 * changes the number of columns of @Sp to Min(@Sp->n,@n);
 *
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_enlarge_cols(NspSpColMatrix *Sp, int n)
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
 * nsp_spcolmatrix_enlarge:
 * @A: a #NspSpColMatrix
 * @m: an integer 
 * @n: an integer
 * 
 * changes @A to [@A,0;0,0]  
 * in such a way that the new size of @A is (max(A->m,m) x max(A->n,n));
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_enlarge(NspSpColMatrix *A, int m, int n)
{
  /* special case **/
  if ( m > A->m  ) {A->m = m ;/* A->mn=m*A->n;*/} /* easy for sparse matrix **/
  if ( n > A->n  ) 
    return nsp_spcolmatrix_enlarge_cols(A,n);
  return OK;
}


/**
 * nsp_spcolmatrix_concatr:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * A = [A, B] 
 * Right concatenation on A, A is changed 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_concatr(NspSpColMatrix *A, NspSpColMatrix *B)
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
      if (nsp_spcolmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if (nsp_spcolmatrix_enlarge_cols(A,A->n+B->n) == FAIL ) { return(FAIL) ; } ; 
  for ( i = Am ; i < A->n ; i++) 
    { 
      SpCol *Ai = A->D[i];
      SpCol *Bi = B->D[i-Am];
      if (nsp_spcolmatrix_resize_col(A,i,(int) Bi->size) == FAIL) return(FAIL) ;
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
 * nsp_spcolmatrix_concatd:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 *  A=[A; B ]  Down concatenation on A 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_concatd(NspSpColMatrix *A, NspSpColMatrix *B)
{
  int i;
  if ( A->rc_type == 'r' &&  B->rc_type == 'c' )  
    {
      if (nsp_spcolmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if ( A->n != B->n ) 
    {
      Scierror("Sparse Concat : incompatible size  \n");
      return(FAIL);
    }
  /* We enlarge the cols of A to store the non-null values comming from B*/
  for ( i = 0 ; i < A->n ; i++) 
    { 
      if (nsp_spcolmatrix_resize_col(A,i,((int) (A->D[i]->size+B->D[i]->size))) == FAIL)return(FAIL);
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
 * nsp_spcolmatrix_concatdiag:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * Diag Concatenation A = [A,0;0,B] 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_concatdiag(NspSpColMatrix *A, NspSpColMatrix *B)
{
  int i,j;
  int Am = A->m;
  int An = A->n;
  int Bm = B->m;
  /* first [A,B] */
  int n1 = Max(A->m,B->m);
  A->m = n1;
  B->m = n1;
  if (nsp_spcolmatrix_concatr( A,B) == FAIL) 
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
 * nsp_spcolmatrix_store:
 * @A: a #NspSpColMatrix
 * @r: 
 * @c: 
 * @col: 
 * @B: a #NspSpColMatrix
 * @r1: 
 * @c1: 
 * 
 * 
 * utility 
 **/

void  nsp_spcolmatrix_store(NspSpColMatrix *A, int r, int c, int col, NspSpColMatrix *B, int r1, int c1)
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
 * nsp_spcolmatrix_insert_elt:
 * @A: a #NspSpColMatrix
 * @i: an integer 
 * @j: an integer 
 * @B: a #NspSpColMatrix
 * @rb: an intege
 * @cb: an intege
 * 
 * Insert or change A(i,j) to B(rb,cb)
 * 
 * Returns: 
 **/

int nsp_spcolmatrix_insert_elt(NspSpColMatrix *A, int i, int j, NspSpColMatrix *B, int rb, int cb)
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
      if (nsp_spcolmatrix_resize_col(A,i,Ai->size+1) == FAIL) return FAIL;
      /* Get the new row **/
      Ai= A->D[i];
      /* we insert the new value **/
      if ( insert == 0) 
	{
	  /* insert at the end **/
	  nsp_spcolmatrix_store(A,i,Ai->size-1,j,B,rb,cb);
	}
      else
	{
	  int k1;
	  /* insert at position insert **/
	  /* move right one step **/
	  for ( k1 = Ai->size -2 ; k1 >= insert ; k1--) 
	    {
	      nsp_spcolmatrix_store(A,i,k1+1,Ai->J[k1],A,i,k1);
	    }
	  nsp_spcolmatrix_store(A,i,insert,j,B,rb,cb);
	}
    }
  else
    {
      nsp_spcolmatrix_store(A,i,ok,j,B,rb,cb);
    }
  return OK;
}

/**
 * nsp_spcolmatrix_delete_elt:
 * @A: a #NspSpColMatrix
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

int nsp_spcolmatrix_delete_elt(NspSpColMatrix *A, int row, int col, int amin, int amax)
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
	  nsp_spcolmatrix_store(A,row,k1-1,A->D[row]->J[k1],A,row,k1);
	}
      return acol;
    }
  else
    {
      return -1;
    }
}

/**
 * nsp_spcolmatrix_get_elt:
 * @B: a #NspSpColMatrix
 * @i: an integer 
 * @j: an integer 
 * 
 * return k such that B->D[i]->J[k] = j or -1 if such k does not exists 
 * 
 * 
 * Return value: the row indice in the B->D[i]->J array of the searched element 
 * or -1 if (i,j) is not stored in B->D[i]->J.
 **/


int nsp_spcolmatrix_get_elt(NspSpColMatrix *B, int i, int j)
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
 * nsp_spcolmatrix_set_rowcol:
 * @A: a #NspSpColMatrix
 * @Rows: a #NspMatrix 
 * @Cols: a #NspMatrix
 * @B: a #NspSpColMatrix
 * 
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked between 
 *  A and B 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_set_rowcol(NspSpColMatrix *A, NspObject *Rows, NspObject *Cols, NspSpColMatrix *B)
{
  int Bnonnul = FALSE;
  char type ='r';
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
      if ( B->D[0]->size !=0) Bnonnul = TRUE;
    }
  
  if ( index_r.min < 1 || index_c.min < 1 ) 
    {
      Scierror("Error:\tNegative indices are not allowed\n");
      goto fail;
    }
  /* Enlarge A if necessary */
  if ( index_r.max > A->m ||  index_c.max > A->n ) 
    if (nsp_spcolmatrix_enlarge(A,index_r.max,index_c.max) == FAIL) goto fail;
  /* Id result complex ? */
  if ( B->rc_type == 'c' &&   A->rc_type == 'r' )
    { 
      type = 'c';
      if (nsp_spcolmatrix_seti(A,0.00) == FAIL ) goto fail;
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
      if (nsp_spcolmatrix_resize_col(A,col ,nel+Ais) == FAIL) goto fail;
      for ( k =0 ; k < index_r.nval ; k++ )
	{
	  int ok = -1;
	  int row = index_r.val[k];
	  int ok1,col1,k1,kb;
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
		  if ( row == A->D[col]->J[acol] ) { ok1 = 1; col1 = acol;break;}
		  if ( row < A->D[col]->J[acol] )  { ok1 = 2; col1 = acol; break;}
		}
	      /* perform insertion **/
	      switch ( ok1 ) 
		{
		case 1: /* replace A(row,col) **/  
		  nsp_spcolmatrix_store(A,col,acol,row, B,ib,ok);
		  /*Cols is not supposed to be increasing  amin = acol; */
		  break;
		case 2: /* insert before acol **/
		  /* move right one step */
		  for ( k1 = amax -1 ; k1 >= acol ; k1--) 
		    {
		      nsp_spcolmatrix_store(A,col,k1+1,A->D[col]->J[k1],A,col,k1);
		    }
		  nsp_spcolmatrix_store(A,col,acol,row,B,ib,ok);
		  /*Cols is not supposed to be increasing amin = acol ; */
		  amax++ ; break ;
		default : 
		  /* insert at end **/
		  nsp_spcolmatrix_store(A,col,amax,row,B,ib,ok);
		  /* Cols is not supposed to be increasing  amin = amax ; **/
		  amax++;
		}
	    }
	  else 
	    {
	      /* we must set the [row,col] element of A to 0.0 **/
	      ok1 =nsp_spcolmatrix_delete_elt(A,col,row,amin,amax);
	      if ( ok1 != -1 ) 
		{
		  /* Cols is not supposed to be increasing  amin = ok1; **/
		  amax--;
		}
	    }
	}
      /* we resize A(row,:) to its correct size **/
      if (nsp_spcolmatrix_resize_col(A,col ,amax) == FAIL) goto fail;
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
 * nsp_spcolmatrix_set_row:
 * @A: a #NspSpColMatrix
 * @Inds: a #NspMatrix 
 * @B: a #NspSpColMatrix
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

int nsp_spcolmatrix_set_row(NspSpColMatrix *A, NspObject *Inds, NspSpColMatrix *B)
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
		     B,B->m,B->n,B->m*B->n,(F_Enlarge)nsp_spcolmatrix_enlarge,&Bscal)== FAIL) 
    goto fail;
  /* */
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_spcolmatrix_complexify(A) == FAIL ) goto fail;
    }
  if ( Bscal == 0 ) 
    {
      for ( i = 0  ; i < index.nval ; i++) 
	{
	  int rb,cb,kb,ia,ra,ca ;
	  rb= i % ((NspSMatrix *) Inds)->m;
	  cb= (i - rb )/((NspSMatrix *) Inds)->m;
	  kb =nsp_spcolmatrix_get_elt(B,cb,rb);
	  ia = index.val[i];
	  ra = ia % A->m;
	  ca= (ia - ra )/A->m;
	  if ( kb == -1 ) 
	    {
	      /* we must set the [ra,ca] element of A to 0.0 **/
	      int ok1 =nsp_spcolmatrix_delete_elt(A,ca,ra,0,A->D[ca]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_spcolmatrix_resize_col(A,ca,A->D[ca]->size-1) == FAIL) goto fail;
		}
	    }
	  else
	    {
	      /* must change or insert element in A */
	      /* take care of B column */
	      if ( cb >= B->n ) cb=0;
	      if (nsp_spcolmatrix_insert_elt(A,ca,ra,B,cb,kb)== FAIL) goto fail;
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
	      ok1 =nsp_spcolmatrix_delete_elt(A,ca,ra,0,A->D[ca]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_spcolmatrix_resize_col(A,ca,A->D[ca]->size-1) == FAIL) goto fail;
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
	      if (nsp_spcolmatrix_insert_elt(A,ca,ra,B,0,0)== FAIL) goto fail;
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

int GenericMatSeRo(void *A, int Am, int An, int Amn,   index_vector *index,
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
 * nsp_spcolmatrix_delete_rows:
 * @A: a #NspSpColMatrix
 * @Rows: a #NspObject
 * 
 *  A(Rows,:) = [],  A is changed. 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_delete_rows(NspSpColMatrix *A, NspObject *Rows)
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
      ndel =nsp_spcolmatrix_compress_col_simple(A,i);
      if (nsp_spcolmatrix_resize_col(A,i, Ai->size-ndel ) == FAIL) 
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
 * nsp_spcolmatrix_compress_col:
 * @A: a #NspSpColMatrix
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

int nsp_spcolmatrix_compress_col(NspSpColMatrix *A, int i)
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
 * nsp_spcolmatrix_compress_col_simple:
 * @A: a #NspSpColMatrix
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

int nsp_spcolmatrix_compress_col_simple(NspSpColMatrix *A, int i)
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
 * nsp_spcolmatrix_delete_cols:
 * @A: a #NspSpColMatrix
 * @Cols: a #NspObject
 * 
 *  A(:,Cols) = []
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_delete_cols(NspSpColMatrix *A, NspObject *Cols)
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
      if (nsp_spcolmatrix_resize_col(A,ind,0) == FAIL) 
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
 * @A: a #NspSpColMatrix
 * @Rows: a #NspMatrix 
 * @Cols: a #NspMatrix 
 * @flag: an integer 
 * @err: an in pointer 
 * 
 * Extract elements A(Row,Cols). 
 * Warning: Rows is changed (sorted).
 * Cols can be NULL if flag == 0 (this should be changed with a NULL check on Cols XXXXX
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

static NspSpColMatrix *SpExtract_G(NspSpColMatrix *A, NspObject *Rows, NspObject *Cols, int flag, int *err)
{
  NspMatrix *Work= NULL, *Index = NULL;
  NspSpColMatrix *Loc=NULL;
  int i,j,Cn;
  
  index_vector index_c={0}, index_r={0};
  index_r.iwork = matint_iwork1;
  index_c.iwork = matint_iwork2;

  /* if ( A->mn == 0) return nsp_spcolmatrix_create(NVOID,A->rc_type,0,0); */
  if (flag == 1) 
    {
      if ( nsp_get_index_vector_from_object(Cols,&index_c) == FAIL) return NULLSPCOL;
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
      Loc= nsp_spcolmatrix_create(NVOID,A->rc_type,0,cols);
      if ( Loc ==  NULLSPCOL) goto err;
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
  if ( (Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,index_r.nval,Cn))== NULLSPCOL) 
    goto err;
  /* used to store elements */
  if ( ( Work = nsp_matrix_create(NVOID,'r',2,index_r.nval)) == NULLMAT) 
    goto err;
  
  for ( i = 0 ; i < Loc->n ; i++)
    {
      int count;
      int imin,imax,k;
      SpCol *Ai, *Li;
      int col;
      col = (flag == 1) ? index_c.val[i] : i ;
      Ai= A->D[col];
      Li= Loc->D[i];
      Li->iw=0;
      if ( Ai->size == 0) continue; /* nothing to do row is empty */
      imin=0; imax= Ai->size-1 ; k = -1;
      Li->iw=0;
      count = nsp_bi_dichotomic_search_i(index_r.val,0,index_r.nval-1,Ai->J,imin,imax,Work,Index,0);
      /* now we know the column size */
      if (nsp_spcolmatrix_resize_col(Loc,i,count)==FAIL) goto err;
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
  if ( Loc != NULL) nsp_spcolmatrix_destroy(Loc);
  return NULL;
}



/**
 * nsp_spcolmatrix_extract:
 * @A: a #NspSpColMatrix
 * @Rows: a #NspMatrix 
 * @Cols: a #NspMatrix 
 * 
 * returns A(Row,Cols) but @Rows is changed in the process.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_extract(NspSpColMatrix *A, NspObject *Rows, NspObject *Cols)
{
  NspSpColMatrix *Sp;
  int err;
  Sp=SpExtract_G(A,Rows,Cols,1,&err);
  if (err==1 ) Scierror("Error:\tIndices out of bound\n");
  return Sp;
}


/**
 * nsp_spcolmatrix_extract_elts:
 * @A: a #NspSpColMatrix
 * @Elts: a #NspMatrix 
 * 
 * return A(Elts)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_extract_elts(NspSpColMatrix *A, NspObject *Elts)
{
  NspSpColMatrix *Loc;
  int i,err,k;
  index_vector index={0};
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Elts,&index) == FAIL) return NULLSPCOL;
  if ( A->m ==0 || A->n == 0) 
    {
      Loc = nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,A->n);
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
      if ( (Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,index.nval,n))== NULLSPCOL) 
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
	      if (nsp_spcolmatrix_resize_col(Loc,0,A->D[0]->iw)==FAIL) goto fail;
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
   return NULLSPCOL;
}

/**
 * nsp_spcolmatrix_extract_rows:
 * @A: a #NspSpColMatrix
 * @Rows: a #NspObject
 * @err: an int pointer
 * 
 * returns A(Rows,:) but @Rows is changed 
 * err is used inside for-loops 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_extract_rows(NspSpColMatrix *A, NspObject *Rows, int *err)
{
  return SpExtract_G(A,Rows,NULLOBJ,0,err);
}

/*
 * A1=MatLoopCol(A1,M,i,rep)
 * Used in for loops XXXX to be done 
 */	

/**
 * nsp_spcolmatrix_extract_cols:
 * @A: a #NspSpColMatrix
 * @Cols: a #NspMatrix 
 * @err: an int pointer
 * 
 * A(:,Cols)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_extract_cols(NspSpColMatrix *A, NspObject *Cols, int *err)
{
  NspSpColMatrix *Loc;
  int i,j;
  index_vector index={0};
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Cols,&index) == FAIL) 
    return NULLSPCOL;
  
  *err=0;
  if ( index.nval == 0) 
    {
      Loc= nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,0);
      if ( Loc == NULLSPCOL ) goto fail;
      return Loc;
    }
  if ( index.min < 1 ||  index.max > A->n  ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      goto fail;
    }
  if ( (Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,index.nval))== NULLSPCOL) 
    goto fail;

  for ( i = 0 ; i < Loc->n ; i++)
    {
      int col= index.val[i];
      SpCol *Ai= A->D[col];
      SpCol *Li= Loc->D[i];
      if (nsp_spcolmatrix_resize_col(Loc,i,Ai->size)==FAIL) goto fail;
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
  return NULLSPCOL;
  
}


/**
 * nsp_spcolmatrix_diag_extract:
 * @A: a #NspSpColMatrix
 * @k: an integer
 * 
 * 
 * Returns the kthe diag of a Sparse NspMatrix as a Sparse  Column.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_diag_extract(NspSpColMatrix *A, int k)
{
  NspSpColMatrix *Loc;
  int j,i,rmin,rmax, cmin,cmax,itmax,count=0;
  rmin = Max(0,-k);
  cmin = Max(0,k);
  itmax = Min(A->m-rmin,A->n -cmin );
  rmax = rmin + itmax;
  cmax = cmin + itmax;
  if ( itmax <= 0 ) 
    {
      Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,(int) 0 , (int) 0);
      return(Loc);
    }
  if (( Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,itmax,(int)1))==NULLSPCOL)  return NULLSPCOL;
  if (nsp_spcolmatrix_resize_col(Loc,0,itmax)==FAIL) return NULLSPCOL;  
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
  if (nsp_spcolmatrix_resize_col(Loc,0, count ) == FAIL) 
    { 
      nsp_spcolmatrix_destroy(Loc);
      return NULL;
    }
  return Loc;
}


static int  GetDiagVal (NspSpColMatrix *Diag,int i,double *val,doubleC *cval);

/**
 * nsp_spcolmatrix_set_diag:
 * @A: a #NspSpColMatrix
 * @Diag: a #NspSpColMatrix
 * @k: an integer 
 * 
 * sets the @k-th diagonal of @A to @Diag 
 * @A is enlarged and complexified if necessary 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_spcolmatrix_set_diag(NspSpColMatrix *A, NspSpColMatrix *Diag, int k)
{
  int i,l;
  int rmin,cmin,rmax,cmax,itmax;
  /*ZZZ */
  rmin = Max(0,-k);
  cmin = Max(0,k);
  rmax = Diag->m*Diag->n + Max(0,-k);
  cmax = Diag->m*Diag->n + Max(0,k);
  itmax = Min(A->m-rmin,A->n -cmin );
  if ( itmax != Diag->m*Diag->n ) 
    {
      Scierror("Error: the given diag vector should be of size %d\n",itmax);
      return(FAIL);
    }
  if ( Diag->rc_type == 'c' && A->rc_type == 'r' ) 
    if (nsp_spcolmatrix_complexify(A) == FAIL ) return(FAIL);

  for ( i = 0 ; i < itmax ; i++ ) 
    {
      int rep ;
      double val=0.0;
      doubleC cval={0.0,0.0};
      /* get next element to be inserted */
      rep = GetDiagVal(Diag,i,&val,&cval);
      if ( rep  == OK  ) 
	{
	  /* element in Diag is not null */
	  SpCol *Ai= A->D[cmin+i];
	  int ok = 0,row=-1;
	  /* element is to be inserted at position (rmin+i,cmin+i) */
	  /* chek is already present */
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
	      if (nsp_spcolmatrix_resize_col(A,cmin+i,Ai->size+1)==FAIL) 
		return FAIL;
	      /* Move data right */
	      if ( row != -1 ) 
		{
		  for ( l = Ai->size -2 ; l >= row ; l--) 
		    {
		      Ai->J[l+1] = Ai->J[l]; 
		      if ( A->rc_type == 'r' ) 
			Ai->R[l+1] = Ai->R[l]; 
		      else 
			Ai->C[l+1] = Ai->C[l]; 
		    }
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
	      if ( Diag->rc_type == 'r') 
		{
		  Ai->C[row].r  = val ;
		  Ai->C[row].i  = 0.0 ;
		}
	      else 
		Ai->C[row]  = cval;
	    }
	}
      else 
	{
	  /* element in Diag is null 
	   */
	  SpCol *Ai= A->D[cmin+i];
	  int ok = 0,tag=-1;
	  /* element is to be inserted at position (rmin+i,cmin+i) */
	  /* chek is already present */
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
	      /* element was present and we want to set it to zero 
	       * thus we must supress it 
	       */
	      /* XXXXX faire des memmove */
	      for ( l = tag  ; l < Ai->size -1 ; l++)
		{
		  Ai->J[l]= Ai->J[l+1];
		  if ( A->rc_type == 'r') 
		    Ai->R[l] = Ai->R[l+1];
		  else 
		    Ai->C[l] = Ai->C[l+1];
		}
	      if (nsp_spcolmatrix_resize_col(A,cmin+i,Ai->size-1)==FAIL) 
		return FAIL;
	    }
	}
    }
  return(OK);
}


/**
 * GetDiagVal:
 * @Diag: a #NspSpColMatrix
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

static int  GetDiagVal(NspSpColMatrix *Diag, int i, double *val, doubleC *cval)
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
 * nsp_spcolmatrix_diag_create:
 * @Diag: a #NspSColMatrix 
 * @k: an integer 
 * 
 *  Creates a Matrix with kth diag set to Diag 
 * 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_diag_create(NspSpColMatrix *Diag, int k)
{
  int i,k1;
  int rmin,cmin,rmax,cmax,n;
  NspSpColMatrix *Loc;
  rmin = Max(0,-k);
  cmin = Max(0,k);
  rmax = Diag->m*Diag->n + Max(0,-k);
  cmax = Diag->m*Diag->n + Max(0,k);
  /* make a square matrix */
  n = Max(cmax,rmax);
  if ((Loc =nsp_spcolmatrix_create(NVOID,Diag->rc_type,n,n))  == NULLSPCOL) 
    return(NULLSPCOL);
  if ( Diag->n == 1 )
    {
      for ( k1=0 ; k1 < Diag->D[0]->size ; k1++) 
	{
	  int j= Diag->D[0]->J[k1];
	  int row = rmin+j;
	  int col = cmin+j;
	  if (nsp_spcolmatrix_resize_col(Loc,col,1)==FAIL) return NULLSPCOL;
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
	      if (nsp_spcolmatrix_resize_col(Loc,col,1)==FAIL) return NULLSPCOL;
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
      return NULLSPCOL;
    }
  return Loc;
}


/**
 * nsp_spcolmatrix_mult:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * returns @Ax@B  by the method of gustafson,acm t.o.m.s. 
 * vol 4 (1978) p250. 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

/*  some modifs by Bruno Pincon 2005 to improve efficiency */

NspSpColMatrix *nsp_spcolmatrix_mult(NspSpColMatrix *A, NspSpColMatrix *B)
{
  NspSpColMatrix *C = NULLSPCOL;
  NspMatrix *x = NULLMAT;
  int *xb = NULL, *pxb = NULL;
  int i, j, k, v, ip, jp, kp, neli, final_neli;
  char type = 'r';
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
  if ( A->n != B->m ) 
    {
      Scierror("SpMult : incompatible arguments\n");
      return(NULLSPCOL);
    }

  if ( (C =nsp_spcolmatrix_create(NVOID,type, A->m,B->n)) == NULLSPCOL ) return NULLSPCOL; 
  
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
      if ( nsp_spcolmatrix_resize_col(C,i,neli) == FAIL ) goto err;
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
	if ( nsp_spcolmatrix_resize_col(C,i,final_neli) == FAIL ) goto err;
    }
  FREE(pxb);
  FREE(xb);
  nsp_matrix_destroy(x);
  return C;

 err:
  FREE(pxb);
  FREE(xb);
  nsp_matrix_destroy(x);
  nsp_spcolmatrix_destroy(C);
  return NULLSPCOL;
}


/**
 * nsp_spcolmatrix_mult_sp_m:
 * @A: a #NspSpColMatrix
 * @B: a #NspMatrix
 * @Res: a #NspMatrix (if NULL space is allocated otherwise Res hold the result)
 *                     
 * @Ax@B when @B is a full matrix and returns the result as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLMAT (in case of failure)
 **/

NspMatrix *nsp_spcolmatrix_mult_sp_m(NspSpColMatrix *A, NspMatrix *B, NspMatrix *Res)
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
 * nsp_spcolmatrix_pmult_sp_m:
 * @A: a #NspSpColMatrix
 * @B: a #NspMatrix
 * @Res: a #NspMatrix (if NULL space is allocated otherwise Res hold the result)
 *                     
 * @A' * @B when @B is a full matrix and returns the result as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLMAT (in case of failure)
 **/
extern NspSpRowMatrix *nsp_spcolmatrix_cast_to_sprow(NspSpColMatrix *M);
extern NspSpColMatrix *nsp_sprowmatrix_cast_to_spcol(NspSpRowMatrix *M);

NspMatrix *nsp_spcolmatrix_pmult_sp_m(NspSpColMatrix *A, NspMatrix *B, NspMatrix *Res)
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

  AA = nsp_spcolmatrix_cast_to_sprow(A);

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
 * nsp_spcolmatrix_mult_m_sp:
 * @X: a #NspMatrix
 * @A: a #NspSpColMatrix
 * 
 * @Xx@A when @X is full 
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

/* (added by Bruno) */

NspMatrix *nsp_spcolmatrix_mult_m_sp(NspMatrix *X,NspSpColMatrix *A)
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
 * nsp_spcolmatrix_mult_scalar:
 * @val: pointer to a double or a double C
 * @val_type: 'r' if val point to a double and 'c' if val point to a complex
 * @A: a NspSpColMatrix
 * 
 * Do the operation A <- scalar * A  (that is A is modified in place)
 *
 * Return value: %OK or %FAIL
 **/

int nsp_spcolmatrix_mult_scalar(double *val, char val_type, NspSpColMatrix *A)
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
       if ( nsp_spcolmatrix_complexify(A) == FAIL )
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
 * nsp_spcolmatrix_mult_scal_old:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * nsp_spcolmatrix_mult_scal(A,B) when B is a scalar sparse 
 * A is changed 
 * the fact that B is scalar is not checked 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_mult_scal_old(NspSpColMatrix *A, NspSpColMatrix *B)
{
  int i,k ;
  if ( A->rc_type == 'r' &&  B->rc_type == 'c' )  
    {
      if (nsp_spcolmatrix_complexify(A) == FAIL ) return(FAIL);
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
 * nsp_spcolmatrix_complexify:
 * @A: a #NspSpColMatrix
 * 
 * Changes A to complex type and 
 * provide storage allocation 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_complexify(NspSpColMatrix *A)
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
 * nsp_spcolmatrix_setr:
 * @A: a #NspSpColMatrix
 * @d: a double 
 * 
 * Set real part of all non nul elements of A to d 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_setr(NspSpColMatrix *A, double d)
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
 * nsp_spcolmatrix_seti:
 * @A: a #NspSpColMatrix
 * @d: a double 
 * 
 * Set imag part of all non nul elements of A to d 
 * if A is real A is changed to complex 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_seti(NspSpColMatrix *A, double d)
{
  int i,c1=1;
  switch ( A->rc_type ) 
    {
    case 'r' : if (nsp_spcolmatrix_complexify(A) == FAIL ) return(FAIL); break;
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
 * RowCountNonNull:
 * @A: a #NspMatrix
 * @i: an integer 
 * 
 * 
 * Return value: the number of non null elements of @i-th row of matrix @A.
 **/

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

/**
 * ColCountNonNull:
 * @A: a #NspMatrix
 * @j: an integer 
 * 
 * 
 * 
 * Return value: the number of non null elements of @i-th column of matrix @A.
 **/

static int  ColCountNonNull(NspMatrix *A, int j)
{ 
  int count=0,i;
  switch ( A->rc_type ) 
    {
    case 'r' :
      for ( i = 0 ; i < A->m ; i++ )  if (A->R[i+j*(A->m)] != 0.00 ) count++; break;
    case 'c' :
      for ( i = 0 ; i < A->m ; i++ )  
	if (A->C[i+j*A->m].r  != 0.00 ||  A->C[i+j*A->m].i != 0.00)  count++;
      break;
    }
  return(count);
}

#if 1 
/**
 * countnonnull:
 * @A:  a #NspMatrix
 * 
 * 
 * 
 * Return value: the number of non null elements of matrix @A.
 **/

int countnonnull(NspMatrix *A)
{ 
  int count=0,i;
  for ( i = 0 ; i < A->m ; i++ ) count +=  RowCountNonNull(A,(int)i);
  return(count);
}
#endif 


/**
 * nsp_spcolmatrix_from_mat:
 * @A:  a #NspMatrix
 * 
 * from full to sparse.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_from_mat(NspMatrix *A)
{ 
  /* nnul : counts non nul elements in the sparse matrix 
   * count : count non nul elements in row i 
   */
  int i,j;
  NspSpColMatrix *Sp;
  if (( Sp =nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,A->n))== NULLSPCOL) return(NULLSPCOL);
  /* first pass to count non null elements on rows */
  for ( i = 0 ; i < A->n ; i++ ) 
    { 
      int count;
      count =  ColCountNonNull(A,(int)i) ;
      if (nsp_spcolmatrix_resize_col(Sp,i,count) == FAIL) return(NULLSPCOL) ;
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
	      if ( A->R[j+i*A->m] != 0.00  ) 
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
	      if ( A->C[j+i*A->m].r != 0.00 || A->C[j+i*A->m].i != 0.00 )
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
 * nsp_spcolmatrix_from_mat_transpose:
 * @A:  a #NspMatrix
 * 
 * creates a sparse matrix filled with the transpose of @A
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_from_mat_transpose(NspMatrix *A)
{ 
  /* nnul : counts non nul elements in the sparse matrix 
   * count : count non nul elements in row i 
   */
  int i,j;
  NspSpColMatrix *Sp;
  if (( Sp =nsp_spcolmatrix_create(NVOID,A->rc_type,A->n,A->m))== NULLSPCOL) return(NULLSPCOL);
  /* first pass to count non null elements */
  for ( i = 0 ; i < A->m ; i++ ) 
    { 
      int count;
      count =  RowCountNonNull(A,(int)i) ;
      if (nsp_spcolmatrix_resize_col(Sp,i,count) == FAIL) return(NULLSPCOL) ;
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

/**
 * nsp_spcolmatrix_to_mat:
 * @Sp: a #NspSpColMatrix
 * 
 * from sparse to full.
 * 
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_spcolmatrix_to_mat(NspSpColMatrix *Sp)
{ 	
  double *wR;
  doubleC *wC;
  NspMatrix *A;
  int i,k; 
  A = nsp_matrix_create(NVOID,Sp->rc_type,Sp->m,Sp->n) ;
  if ( A == NULLMAT ) { return(NULLMAT);}
  nsp_mat_set_rval(A,0.0);
  if ( A->rc_type == 'c') if (nsp_mat_set_ival(A,0.00) == FAIL ) return(NULLMAT);
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
 * nsp_spcolmatrix_to_mat_transpose:
 * @Sp: a #NspSpColMatrix
 * 
 * from sparse to full. returns a full matrix 
 * filled from the transpose of @Sp.
 * 
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_spcolmatrix_to_mat_transpose(NspSpColMatrix *Sp)
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
 * nsp_spcolmatrix_transpose:
 * @A: a #NspSpColMatrix
 * 
 * Transpose a sparse Matrix, A is unchanged and a new matrix is created and 
 * returned. 
 * Warning : For Complex matrices transpose is a' = transp+ conj
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_transpose(const NspSpColMatrix *A)
{
  int k,i,j;
  NspSpColMatrix *Loc;
  Loc =nsp_spcolmatrix_create( NVOID,A->rc_type,A->n,A->m);
  if ( Loc == NULLSPCOL ) return(NULLSPCOL) ; 
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
      if (nsp_spcolmatrix_resize_col(Loc,i,(int) Loc->D[i]->iw) == FAIL) return NULLSPCOL;
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
 * nsp_spcolmatrix_add:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * A+B
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_add(NspSpColMatrix *A, NspSpColMatrix *B)
{
  return(BinaryOp(A,B,PlusLeft,PlusBoth,PlusRight,FALSE));
}

/**
 * nsp_spcolmatrix_sub:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * A-B
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/
NspSpColMatrix *nsp_spcolmatrix_sub(NspSpColMatrix *A, NspSpColMatrix *B)
{
  return(BinaryOp(A,B,MinusLeft,MinusBoth,MinusRight,FALSE));
}

/**
 * nsp_spcolmatrix_multtt:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * A.*B
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_multtt(NspSpColMatrix *A, NspSpColMatrix *B)
{
  return(BinaryOp(A,B,MultttLeft,MultttBoth,MultttRight,FALSE));
}

/**
 * nsp_spcolmatrix_and:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * A.*B
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_and(NspSpColMatrix *A, NspSpColMatrix *B)
{
  return(BinaryOp(A,B,AndLeft,AndBoth,AndRight,TRUE));
}

/**
 * nsp_spcolmatrix_or:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * A.*B
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_or(NspSpColMatrix *A, NspSpColMatrix *B)
{
  return(BinaryOp(A,B,OrLeft,OrBoth,OrRight,TRUE));
}





/**
 * nsp_spcolmatrix_divel:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * A ./ B 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

/* XXXXX : BinaryOp is not valid here because common zero will give 
 * 0 and should give Nan 
 */

NspSpColMatrix *nsp_spcolmatrix_divel(NspSpColMatrix *A, NspSpColMatrix *B)
{
  return(BinaryOp_bis(A,B,DivttLeft,DivttBoth,DivttBothNull,DivttRight));
}

/* A ./ sparse(0) 
 * scalar 0
 */ 

NspSpColMatrix * nsp_spcolmatrix_div_zero_tt(NspSpColMatrix *A)
{
  double zero = 0;
  int i,j;
  NspSpColMatrix *Loc;
  if (( Loc=nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLSPCOL) return(NULLSPCOL);
  for ( i = 0 ; i < Loc->n ; i++ )  
    {
      if (nsp_spcolmatrix_resize_col(Loc,i,A->m)== FAIL) return NULLSPCOL;
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

int nsp_spcolmatrix_div_scal_tt(NspSpColMatrix *A, NspSpColMatrix *B)
{
  int i,k,ndel;
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_spcolmatrix_complexify(A) == FAIL ) return(FAIL);
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
	  ndel =nsp_spcolmatrix_compress_col_simple(A,i);
	  if (nsp_spcolmatrix_resize_col(A,i, Ai->size-ndel ) == FAIL) return(FAIL) ;
	}
    }
  return OK ;
}

/* A ./ B and A is 1x1 scalar and != 0 
 *
 */ 

NspSpColMatrix *nsp_spcolmatrix_scal_div_tt(NspSpColMatrix *A, NspSpColMatrix *B)
{
  NspSpColMatrix *Loc;
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
  if (( Loc=nsp_spcolmatrix_create(NVOID,t,B->m,B->n)) == NULLSPCOL) return(NULLSPCOL);
  for (  i  = 0 ;  i  < Loc->n  ;  i++ ) 
    {
      int compress=0;
      doubleC C;
      SpCol *Bi = B->D[i];
      SpCol *Li;
      if (nsp_spcolmatrix_resize_col(Loc,i,B->m)== FAIL) return NULLSPCOL;
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
	  ndel =nsp_spcolmatrix_compress_col_simple(Loc,i);
	  if (nsp_spcolmatrix_resize_col(Loc,i, Li->size-ndel ) == FAIL) return NULL;
	}
    }
  return Loc ;
}



#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/**
 * BinaryOp:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
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
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

static NspSpColMatrix *BinaryOp(NspSpColMatrix *A, NspSpColMatrix *B, BopLeft BinLeft, BopBoth BinBoth, BopRight BinRight,
				int force_real)
{ 
  int i,count,k1,k2,k;
  NspSpColMatrix *Loc;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
      if ( force_real ) type ='r';
      Loc =nsp_spcolmatrix_create(NVOID,type, A->m,A->n);
      if ( Loc == NULLSPCOL ) return(NULLSPCOL) ; 
      for ( i = 0 ; i < Loc->n ; i++ ) 
	{
	  int iest;
	  SpCol *Ai = A->D[i];
	  SpCol *Bi = B->D[i];
	  SpCol *Li = Loc->D[i];
	  iest= Min( A->m, A->D[i]->size+B->D[i]->size);
	  if (nsp_spcolmatrix_resize_col(Loc,i,(int)iest ) == FAIL) return(NULLSPCOL) ;
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
	  if (nsp_spcolmatrix_resize_col(Loc,i,count) == FAIL) return(NULLSPCOL) ;
	}
      return(Loc);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      return(NULLSPCOL);
    }
}

/**
 * BinaryOp_bis:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
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
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

static NspSpColMatrix *BinaryOp_bis(NspSpColMatrix *A, NspSpColMatrix *B, BopLeft BinLeft, BopBoth BinBoth, 
				    BopBothNull BinBothNull, BopRight BinRight)
{ 
  int i,count,k1,k2,k,j3;
  NspSpColMatrix *Loc;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
      Loc =nsp_spcolmatrix_create(NVOID,type, A->m,A->n);
      if ( Loc == NULLSPCOL ) return(NULLSPCOL) ; 
      for ( i = 0 ; i < Loc->n ; i++ ) 
	{
	  int iest,lx;
	  SpCol *Ai = A->D[i];
	  SpCol *Bi = B->D[i];
	  SpCol *Li = Loc->D[i];
	  /* since here 0 op 0 can return non zero values */
	  iest=  A->m; 
	  if (nsp_spcolmatrix_resize_col(Loc,i,(int)iest ) == FAIL) return(NULLSPCOL) ;
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
	  if (nsp_spcolmatrix_resize_col(Loc,i,count) == FAIL) return(NULLSPCOL) ;
	}
      return(Loc);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      return(NULLSPCOL);
    }
}

/**
 * nsp_spcolmatrix_mult_scal:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * 
 * A = A.*B where B is a scalar sparse ( [] or scalar )
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_mult_scal(NspSpColMatrix *A, NspSpColMatrix *B)
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
      if (nsp_spcolmatrix_complexify(A) == FAIL ) return(FAIL);
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
 * nsp_spcolmatrix_op_scal:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
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
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_spcolmatrix_op_scal(NspSpColMatrix *A, NspSpColMatrix *B, int *flag, char op)
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
  return ( (NspSpColMatrix *) M)->rc_type;
}

static int SpNext(const void *M, double *r, doubleC *c,int *work)
{
  const NspSpColMatrix *Sp= M;
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

static void Sp_set_format(nsp_num_formats *fmt,NspSpColMatrix *M)
{
  gen_set_format(fmt,M,Sp_any_element_is_negative,
		 Sp_any_element_is_inf_or_nan,
		 Sp_pr_min_max_internal,
		 Sp_all_elements_are_int_or_inf_or_nan,
		 SpInit);
}

/* Sparse Matrix with + format : both real and complex cases **/

static void SpM_plus_format(NspSpColMatrix *Sp, int indent)
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

static int SpM_general(nsp_num_formats *fmt,NspSpColMatrix *Sp, int indent)
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
	      nsp_pr_float(fmt, Ri->R[j]);Sciprintf("\n");
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
	      nsp_pr_complex(fmt, Ri->C[j]);
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

static int nsp_spcolmatrix_print_internal(nsp_num_formats *fmt,NspSpColMatrix *m, int indent)
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
 * nsp_spcolmatrix_clean:
 * @A: a #NspSpColMatrix
 * @rhs: an integer 
 * @epsa: absolute precision 
 * @epsr: relative precision 
 * 
 * A = Matclean(a) clean A according to epsa and epsr 
 * epsa is used if rhs >= 1 
 * epsr is used if rhs >= 2
 * A is changed, 
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_clean(NspSpColMatrix *A, int rhs, double epsa, double epsr)
{
  int j,i,n;
  double d_epsr=DBL_EPSILON;
  double d_epsa=DBL_EPSILON;
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
  if ( rhs >= 1 ) d_epsa = epsa;
  if ( rhs >= 2 ) d_epsr = epsr;
  eps= Max(epsa,epsr*norm);
  for ( i = 0 ; i < A->n  ; i++ ) 
    {
      n =0;
      for ( j = 0 ; j < A->D[i]->size ; j++) 
	{
	  switch ( A->rc_type ) 
	    {
	    case 'r' : if ( Abs(A->D[i]->R[j])   < eps) A->D[i]->J[j] = -1;n=1; break ;
	    case 'c' : 
	      /* using complex absolute value  */
	      if (nsp_abs_c(&A->D[i]->C[j]) < eps) A->D[i]->J[j] = -1;n=1; break ;
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
	  int ndel =nsp_spcolmatrix_compress_col_simple(A,i);
	  if (nsp_spcolmatrix_resize_col(A,i, A->D[i]->size-ndel ) == FAIL) return(FAIL) ;
	}
    }
  return OK;
}


/**
 * nsp_spcolmatrix_maximinitt_g:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
 * @flag: an integer 
 * @minmaxflag: an integer 
 * @err: an int pointer 
 * 
 * max or min (A,B)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *
nsp_spcolmatrix_maximinitt_g(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int minmaxflag, int *err)
{
  /* Same philosophy as in BinaryOp **/
  int i,count,icount,k1,k2,k;
  NspSpColMatrix *Loc,*ILoc;
  NspSpColMatrix *Indi=NULL;
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
	  if (( Indi = nsp_spcolmatrix_create(NVOID,'r',A->m,A->n)) == NULLSPCOL ) 
	    goto err;
	}
      /* Buffer */
      if ((Loc =nsp_spcolmatrix_create(NVOID,type,A->m,1)) == NULLSPCOL ) goto err;
      if (nsp_spcolmatrix_resize_col(Loc,0,A->m ) == FAIL) goto err;
      /* Buffer for indices */
      if ((ILoc =nsp_spcolmatrix_create(NVOID,type,A->m,1)) == NULLSPCOL ) goto err;
      if (nsp_spcolmatrix_resize_col(ILoc,0,A->m ) == FAIL) goto err;
      
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
	    }
	  /* count is not set to the proper ith row dimension  */
	  /* we resize A(i,:) and store Loc  **/
	  if (nsp_spcolmatrix_resize_col(A,i,count)  == FAIL)
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
	      if (nsp_spcolmatrix_resize_col(Indi,i,icount)  == FAIL) goto err;
	      /* use icopy and dcopy XXXX */
	      for ( k =0 ; k < Indi->D[i]->size ; k++) 
		{
		  Indi->D[i]->J[k] = ILoc->D[0]->J[k];
		  Indi->D[i]->R[k] = ILoc->D[0]->R[k];
		}
	    }
	}
      nsp_spcolmatrix_destroy(Loc);
      nsp_spcolmatrix_destroy(ILoc);
      return(Indi);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      goto err;
    }
 err:
  *err=TRUE;
  return NULLSPCOL;
}


/**
 * nsp_spcolmatrix_maxitt:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
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
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_maxitt(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int *err)
{
  return nsp_spcolmatrix_maximinitt_g(A,B,flag,1,err);
}


/**
 * nsp_spcolmatrix_minitt:
 * @A: a #NspSpColMatrix
 * @B: a #NspSpColMatrix
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

NspSpColMatrix *nsp_spcolmatrix_minitt(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int *err)
{
  return nsp_spcolmatrix_maximinitt_g(A,B,flag,-1,err);
}

/**
 * nsp_spcol_resize:
 * @col: 
 * @n: 
 * @rc_type: 
 * 
 * allocate or reallocate a sparse column structure
 * 
 * Return value:  %OK or %FAIL
 **/
      
static int nsp_spcol_resize(SpCol **col, int n, char rc_type)
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
 * nsp_spcolmatrix_realpart:
 * @A: a #NspSpColMatrix
 * 
 * Return the Real part of Matrix A in A.
 * 
 * Return value:  %OK or %FAIL
 **/




int nsp_spcolmatrix_realpart(NspSpColMatrix *A)
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
      if ( nsp_spcol_resize(&col,count,'r')== FAIL) return FAIL;
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
      nsp_spcolmatrix_col_destroy(A->D[i]);
      A->D[i]= col;
    }
  A->rc_type = 'r';
  return(OK);
}

/**
 * nsp_spcolmatrix_imagpart:
 * @A: a #NspSpColMatrix
 * 
 * Return the Imaginary part of Matrix A in A.
 * 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_imagpart(NspSpColMatrix *A)
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
	  if ( nsp_spcol_resize(&col,count,'r')== FAIL) return FAIL;
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
	  nsp_spcolmatrix_col_destroy(A->D[i]);
	  A->D[i]= col;
	}
      A->rc_type = 'r';
    }
  return OK;
}


/**
 * nsp_spcolmatrix_isreal:
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

int nsp_spcolmatrix_isreal(const NspSpColMatrix *A, int strict)
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
 * nsp_spcolmatrix_sum:
 * @A: a #NspSpColMatrix
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

NspSpColMatrix *nsp_spcolmatrix_sum(NspSpColMatrix *A, char *flag)
{
  double S;
  doubleC SC,C;
  NspSpColMatrix *Sum=NULL;
  int i,k,count;
  int inc=1;
  if ( A->m ==0 || A->n == 0) 
    {
      if ( flag[0] == 'F' || flag[0]=='f' )
	{
	  if ((Sum =nsp_spcolmatrix_create(NVOID,'r',1,1)) == NULLSPCOL) return NULLSPCOL;
	  if (nsp_spcolmatrix_resize_col(Sum,0,1)== FAIL) return NULLSPCOL;
	  Sum->D[0]->J[0] =0;
	  Sum->D[0]->R[0] =0.00;
	  return Sum;
	}
      else 
	return nsp_spcolmatrix_create(NVOID,'r',0,0);
    }
  switch (flag[0]) 
    {
    case 'f': 
    case 'F':
  
      if ((Sum =nsp_spcolmatrix_create(NVOID,A->rc_type,1,1)) == NULLSPCOL) return(NULLSPCOL);
      switch ( A->rc_type) 
	{
	case 'r' : 
	  S=0;
	  for ( i= 0 ; i < A->n ; i++ ) 
	    S +=nsp_dsum(&A->D[i]->size,A->D[i]->R,&inc);
	  if ( S != 0) 
	    {
	      if (nsp_spcolmatrix_resize_col(Sum,0,1)== FAIL) return NULLSPCOL;
	      Sum->D[0]->R[0] = S;
	      Sum->D[0]->J[0] = 0;
	    }
	  break;
	case 'c' :  
	  SC.r = SC.i = 0.0;
	  for ( i= 0 ; i < A->n ; i++ ) 
	    { 
	      nsp_zsum(&C,&A->D[i]->size,A->D[i]->C,&inc); 
	      SC.r += C.r;SC.i += C.i;
	    }
	  if ( SC.r  != 0.0 ||  SC.i != 0.0) 
	    {
	      if (nsp_spcolmatrix_resize_col(Sum,0,1)== FAIL) return NULLSPCOL;
	      Sum->D[0]->C[0] = SC;
	      Sum->D[0]->J[0] = 0;
	    }
	  break;
	}
      break;
    case 'c':
    case 'C':
      
      if ((Sum =nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,1)) == NULLSPCOL) return NULLSPCOL;
      if (nsp_spcolmatrix_resize_col(Sum,0,A->m)== FAIL) return NULLSPCOL;
      for ( k=0 ; k < Sum->D[0]->size ; k++) 
	{
	  Sum->D[0]->J[k]=k;
	  switch ( A->rc_type ) 
	    {
	    case 'r' :  Sum->D[0]->R[k]=0.0;break;
	    case 'c' :  Sum->D[0]->C[k].r = Sum->D[0]->C[k].i =0.0;break;
	    }
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
	  int ndel =nsp_spcolmatrix_compress_col(Sum,0);
	  if (nsp_spcolmatrix_resize_col(Sum,0,A->m-ndel ) == FAIL) return NULLSPCOL;
	}
      break;
    case 'r':
    case 'R':
  
      if ((Sum =nsp_spcolmatrix_create(NVOID,A->rc_type,1,A->n)) == NULLSPCOL) return NULLSPCOL;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      double S;
	      S =nsp_dsum(&A->D[i]->size,A->D[i]->R,&inc); 
	      if ( S != 0.0 ) 
		{
		  if (nsp_spcolmatrix_resize_col(Sum,i,1)== FAIL) return NULLSPCOL;
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
		  if (nsp_spcolmatrix_resize_col(Sum,i,1)== FAIL) return NULLSPCOL;
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

typedef int (*SpMaMi1) (NspSpColMatrix *A,NspSpColMatrix *M);
typedef int (*SpMaMi2) (NspSpColMatrix *A,int j,NspSpColMatrix *M);
typedef int (*SpMaMi3) (NspSpColMatrix *A,int j,NspSpColMatrix *M,int *count);

/**
 * utility 
 **/

static NspSpColMatrix *SpColMaxiMini(NspSpColMatrix *A, char *flag, NspMatrix **Imax, int lhs, SpMaMi1 F1, SpMaMi2 F2, SpMaMi3 F3)
{
  NspSpColMatrix *M=NULL;
  int j;
  int inc=1,imax,count;
  if ( A->m == 0 || A->n == 0 ) 
    {
      if ( lhs == 2) *Imax = nsp_matrix_create(NVOID,'r',0,0);
      return nsp_spcolmatrix_create(NVOID,'r',0,0);
    }
  switch (flag[0]) 
    {
    case 'f': 
    case 'F':
      if ((M =nsp_spcolmatrix_create(NVOID,A->rc_type,1,1)) == NULLSPCOL) return(NULLSPCOL);
      imax = (*F1)(A,M);
      /* Check if M was properly resized **/
      if ( imax == 0 )  return NULLSPCOL;
      
      if ( lhs == 2 ) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT)
	    return NULLSPCOL; 
	  (*Imax)->R[0] = imax;
	}
      break;
    case 'r':
    case 'R':
      if ((M =nsp_spcolmatrix_create(NVOID,A->rc_type,1,A->n)) == NULLSPCOL)
	return NULLSPCOL;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,A->n)) == NULLMAT) 
	    return NULLSPCOL;
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
    case 'c':
    case 'C':
      if ((M =nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,1)) == NULLSPCOL) 
	return NULLSPCOL;
      if (nsp_spcolmatrix_resize_col(M,0,A->m) == FAIL) return NULLSPCOL;
      count =0;
      inc = A->m;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT) 
	    return NULLSPCOL; 
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
      if (nsp_spcolmatrix_resize_col(M,0,count) == FAIL) return NULLSPCOL;
      break;
    }
  return M;
}


/**
 * SpColMaxi1:
 * @A: a #NspSpColMatrix
 * @M: a #NspSpColMatrix
 * 
 * M(1) = Maxi(A) max of all the elements 
 * 
 * 
 * Return value: the indice of the element of Matrix @A which realize the maximum 
 * (the indice is given as a global indice of a mxn Matrix).
 **/

static int SpColMaxi1(NspSpColMatrix *A, NspSpColMatrix *M)
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
      if (nsp_spcolmatrix_resize_col(M,0,1) == FAIL) return 0;
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
	  if (nsp_spcolmatrix_resize_col(M,0,1) == FAIL) return 0;
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
 * SpColMaxi3:
 * @A: a #NspSpColMatrix
 * @j: 
 * @M: a #NspSpColMatrix
 * @count: 
 * 
 * utility : M(j)=Max A(j,:) : max of row j  
 *
 * Return value: the column indice which realize the max of row @j.
 **/

static int SpColMaxi3(NspSpColMatrix *A, int j, NspSpColMatrix *M, int *count)
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
 * SpColMaxi2:
 * @A: a #NspSpColMatrix
 * @j: 
 * @M: a #NspSpColMatrix
 *
 * utility: M(j)=Max A(:,j) find the max of column j 
 * 
 * 
 * Return value: the row indice which realize the max of column @j.
 **/

static int SpColMaxi2(NspSpColMatrix *A, int j, NspSpColMatrix *M)
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
      if (nsp_spcolmatrix_resize_col(M,j,1) == FAIL) return 0;
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
	  if (nsp_spcolmatrix_resize_col(M,j,1) == FAIL) return 0;
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
 * nsp_spcolmatrix_maxi:
 * @A: a #NspSpColMatrix
 * @flag: a string among  "c", "r" and "g"
 * @Imax: a #NspMatrix 
 * @lhs: an integer 
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
 * Note that Imax is a full matrix. 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_maxi(NspSpColMatrix *A, char *flag, NspMatrix **Imax, int lhs)
{
  return SpColMaxiMini(A,flag,Imax,lhs,SpColMaxi1,SpColMaxi2,SpColMaxi3);
}



/**
 * SpColMini1:
 * @A: a #NspSpColMatrix
 * @M: a #NspSpColMatrix
 * 
 * M(1) = Mini(A) min of all the elements 
 * 
 * Return value: the indice of the element of Matrix @A which realize the minimum
 * (the indice is given as a global indice of a mxn Matrix).
 **/

static int SpColMini1(NspSpColMatrix *A, NspSpColMatrix *M)
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
      if (nsp_spcolmatrix_resize_col(M,0,1) == FAIL) return 0;
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
	  if (nsp_spcolmatrix_resize_col(M,0,1) == FAIL) return 0;
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
 * SpColMini3:
 * @A: a #NspSpColMatrix
 * @j: 
 * @M: a #NspSpColMatrix
 * @count: 
 * 
 * utility : M(j)=Min A(j,:) : min of row j  
 *
 * Return value: the column indice which realize the max of row @j.
 **/

static int SpColMini3(NspSpColMatrix *A, int j, NspSpColMatrix *M, int *count)
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
 * SpColMini2:
 * @A: a #NspSpColMatrix
 * @j: 
 * @M: a #NspSpColMatrix
 *
 * utility: M(j)=Min A(:,j) find the min of column j 
 * 
 * 
 * Return value: the row indice which realize the min of column @j.
 **/

static int SpColMini2(NspSpColMatrix *A, int j, NspSpColMatrix *M)
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
      if (nsp_spcolmatrix_resize_col(M,j,1) == FAIL) return 0;
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
	  if (nsp_spcolmatrix_resize_col(M,j,1) == FAIL) return 0;
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
 * nsp_spcolmatrix_mini:
 * @A: a #NspSpColMatrix
 * @flag: a string among  "c", "r" and "g"
 * @Imax: a #NspMatrix 
 * @lhs: an integer 
 * 
 * [max,imax]=max(A,'c'|'r'|'g')
 * Min =nsp_mat_mini(A,B,Imax,lhs)
 *     A is unchanged 
 * if B= 'c' the max for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the max for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the minimum 
 * Imax is created if lhs == 2 
 * Note that Imax is a full matrix. 
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_mini(NspSpColMatrix *A, char *flag, NspMatrix **Imax, int lhs)
{
  return SpColMaxiMini(A,flag,Imax,lhs,SpColMini1,SpColMini2,SpColMini3);
}



/*
 * Creates a Matrix and initialize it with the 
 * function func 
 * R=func(i,j) or R=func(i,j,&Imag) 
 */

/**
 * nsp_spcolmatrix_triu:
 * @A: a #NspSpColMatrix
 * @k: an integer 
 * 
 * A=Triu(A)
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_triu(NspSpColMatrix *A,int k)
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
	  if (nsp_spcolmatrix_resize_col(A,i,j) == FAIL) return FAIL;
	}
    }
  return OK;
}


/**
 * nsp_spcolmatrix_tril:
 * @A: a #NspSpColMatrix
 * @k: 
 * 
 * A=Tril(A)
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_tril(NspSpColMatrix *A,int k)
{
  int ndel;
  int i,j;
  for ( i = 0 ; i < A->n ; i++)
    {
      int resize=FALSE;
      /* maximum row indice to keep for column i */
      int minrow= i-k;
      for ( j=0; j < A->D[i]->size ; j++ ) 
	{
	  if ( A->D[i]->J[j] >= minrow )
	    { 
	      resize=TRUE;break;
	    }
	  else 
	    {
	      A->D[i]->J[j] = -1;
	    }
	}
      ndel =nsp_spcolmatrix_compress_col_simple(A,i);
      if (nsp_spcolmatrix_resize_col(A,i, A->D[i]->size-ndel ) == FAIL) return(FAIL) ;
    }
  return OK;
}



/**
 * nsp_spcolmatrix_eye:
 * @m: an integer 
 * @n: an integer 
 * 
 * return eye(m,n) coded as #NspSColMatrix.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_eye(int m, int n)
{
  NspSpColMatrix *Loc;
  int i;
  if (( Loc=nsp_spcolmatrix_create(NVOID,'r',m,n)) == NULLSPCOL) return(NULLSPCOL);
  for ( i = 0 ; i < Min(Loc->m,Loc->n) ; i++ ) 
    {
      if (nsp_spcolmatrix_resize_col(Loc,i,1)== FAIL) return NULLSPCOL;
      Loc->D[i]->J[0]= i;
      Loc->D[i]->R[0]= 1.0;
    }
  return(Loc);
}


/**
 * nsp_spcolmatrix_spones:
 * @A: a #NspSpColMatrix
 * 
 * generates a matrix with the same sparsity structure as A, 
 * but with ones in the nonzero positions
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_spones(NspSpColMatrix *A)
{
  int i,j;
  NspSpColMatrix *Loc;
  if ( A->rc_type == 'r') 
    {
      if ((Loc= nsp_spcolmatrix_copy(A)) == NULLSPCOL) return(NULLSPCOL);
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
      if ((Loc= nsp_spcolmatrix_copy(A)) == NULLSPCOL) return(NULLSPCOL);
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
 * nsp_spcolmatrix_ones:
 * @m: an integer 
 * @n: an integer 
 * 
 * ones(m,n) as a sparse Matrix.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/


NspSpColMatrix *nsp_spcolmatrix_ones(int m, int n)
{
  NspSpColMatrix *Loc;
  int i,k;
  if (( Loc=nsp_spcolmatrix_create(NVOID,'r',m,n)) == NULLSPCOL) return(NULLSPCOL);
  for ( i = 0 ; i < Loc->n ; i++ ) 
    {
      if (nsp_spcolmatrix_resize_col(Loc,i,Loc->m)== FAIL) return NULLSPCOL;
      for ( k = 0 ; k < Loc->m ; k++) 
	{
	  Loc->D[i]->J[k]= k;
	  Loc->D[i]->R[k]= 1.0;
	}
    }
  return(Loc);
}


/**
 * nsp_spcolmatrix_zeros:
 * @m: 
 * @n: 
 * 
 * A=zeros(m,n)
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_zeros(int m, int n)
{
  NspSpColMatrix *Loc;
  if (( Loc=nsp_spcolmatrix_create(NVOID,'r',m,n)) == NULLSPCOL) return(NULLSPCOL);
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

static NspMatrix* SpColUnary2Full(NspSpColMatrix *A, Func1 F1, Func2 F2)
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
 * nsp_spcolmatrix_acos:
 * @A: a #NspSpColMatrix
 * 
 * returns cos(A) as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_spcolmatrix_acos(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,acos,nsp_acos_c);
}


/**
 * nsp_spcolmatrix_acosh:
 * @A: a #NspSpColMatrix
 * 
 * returns cosh(A) as a full matrix.
 * 
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_spcolmatrix_acosh(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,acosh,nsp_acosh_c);
}

/*
 * Generic Function for Sparse unary operators 
 * computes A=f1(A) or A=f2(A) assuming fi(0)=0
 */

static void  SpColUnary(NspSpColMatrix *A, Func1 F1, Func2 F2)
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
	      ndel =nsp_spcolmatrix_compress_col(A,i);
	      nsp_spcolmatrix_resize_col(A,i,A->D[i]->size-ndel);
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
	      ndel =nsp_spcolmatrix_compress_col(A,i);
	      nsp_spcolmatrix_resize_col(A,i,A->D[i]->size-ndel);
	    }
	}
    }
}


/**
 * nsp_spcolmatrix_asin:
 * @A: a #NspSpColMatrix
 *
 * A=asin(A)
 * 
 **/

void nsp_spcolmatrix_asin(NspSpColMatrix *A)
{
  SpColUnary(A,asin,nsp_asin_c);
}



/**
 * nsp_spcolmatrix_asinh:
 * @A: a #NspSpColMatrix
 * 
 * A=Asinh(A),
 **/

void nsp_spcolmatrix_asinh(NspSpColMatrix *A)
{
  SpColUnary(A,asinh,nsp_asinh_c);
}


/**
 * nsp_spcolmatrix_atan:
 * @A: a #NspSpColMatrix
 * 
 * A=Atan(A),
 **/
void nsp_spcolmatrix_atan(NspSpColMatrix *A)
{
  SpColUnary(A,atan,nsp_atan_c);
}


/**
 * nsp_spcolmatrix_atanh:
 * @A: a #NspSpColMatrix
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

void nsp_spcolmatrix_atanh(NspSpColMatrix *A)
{
  SpColUnary(A,nsp_atanh,nsp_atanh_c);
}

/**
 * nsp_spcolmatrix_ceil:
 * @A: a #NspSpColMatrix
 * 
 * A=Ceil(A)
 **/

void nsp_spcolmatrix_ceil(NspSpColMatrix *A)
{
  SpColUnary(A,ceil,nsp_ceil_c);
}

static double R_aint(double x) { return aint(x);} 

/** 
 * nsp_spcolmatrix_int:
 * @A: a #NspSpColMatrix
 * 
 * 
 * A=Int(A)
 **/

void nsp_spcolmatrix_int(NspSpColMatrix *A)
{
  SpColUnary(A,R_aint,nsp_aint_c);
}


/**
 * nsp_spcolmatrix_floor:
 * @A: a #NspSpColMatrix
 * 
 * A=Floor(A) 
 **/

void nsp_spcolmatrix_floor(NspSpColMatrix *A)
{
  SpColUnary(A,floor,nsp_floor_c);
}

static double R_anint(double x) { return anint(x);} 

/**
 *nsp_spcolmatrix_round: 
 * @A: a #NspSpColMatrix
 * 
 * A=Round(A)
 **/

void nsp_spcolmatrix_round(NspSpColMatrix *A)
{
  SpColUnary(A,R_anint,nsp_round_c);
}


/**
 * nsp_spcolmatrix_sign:
 * @A: a #NspSpColMatrix
 * 
 * 
 * A=Sign(A)
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_sign(NspSpColMatrix *A)
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
 * nsp_spcolmatrix_tan:
 * @A: a #NspSpColMatrix
 * 
 * A=Tan(A) 
 **/

void nsp_spcolmatrix_tan(NspSpColMatrix *A)
{
  SpColUnary(A,tan,nsp_tan_c);
}

/**
 * nsp_spcolmatrix_tanh:
 * @A: a #NspSpColMatrix
 * 
 * A=Tanh(A)
 **/

void nsp_spcolmatrix_tanh(NspSpColMatrix *A)
{
  SpColUnary(A,tanh,nsp_tanh_c);
}


/**
 * nsp_spcolmatrix_abs:
 * @A: a #NspSpColMatrix
 * 
 * 
 * A=Abs(A), absolue value or module of each element 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_abs(NspSpColMatrix *A)
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
      if (nsp_spcolmatrix_realpart(A) == FAIL) return FAIL;
    }
  return(OK);
}


/**
 * nsp_spcolmatrix_erf:
 * @A: a #NspSpColMatrix
 * 
 * 
 * A=Erf(A), Erf function 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_erf(NspSpColMatrix *A)
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
  int SpColErfc(A)
  NspSpColMatrix *A;
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
 * nsp_spcolmatrix_arg:
 * @A: a #NspSpColMatrix
 * 
 * 
 * A=Arg(A),
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_arg(NspSpColMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      /* change a to  [] sparse **/
      for ( i=0 ; i < A->n ; i++ ) 
	{
	  if ( A->D[i]->size != 0 ) 
	    {
	      FREE( A->D[i]->J);
	      FREE( A->D[i]->R);
	      FREE( A->D[i]->C);
	    }
	  A->D[i]->size =0;
	}
    }
  else
    {
      for ( i = 0 ; i < A->n ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->C[k].r =nsp_arg_c(&A->D[i]->C[k]);
      if (nsp_spcolmatrix_realpart(A) == FAIL) return FAIL;
    }
  return(OK);
}

/*
 * A=Polar(A,B),
 * A=A(cos(B)+%i*sin(B);
 */

/**
 * nsp_spcolmatrix_conj:
 * @A: a #NspSpColMatrix
 * 
 * A=real(A)-i*Imag(A) A is changed  only if imaginary 
 **/

void nsp_spcolmatrix_conj(NspSpColMatrix *A)
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
 * nsp_spcolmatrix_cos:
 * @A: a #NspSpColMatrix
 * 
 * 
 * Cos(A)
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/
NspMatrix *nsp_spcolmatrix_cos(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,cos,nsp_cos_c);
}


/**
 * nsp_spcolmatrix_cosh:
 * @A: a #NspSpColMatrix
 * 
 * 
 * Cosh(A)
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_spcolmatrix_cosh(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,cosh,nsp_cosh_c);
}

/**
 * nsp_spcolmatrix_expel:
 * @A: a #NspSpColMatrix
 * 
 * exp(A)
 * Return value: a new  #NspMatrix or %NULLSPCOL
 **/

NspMatrix *nsp_spcolmatrix_expel(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,exp,nsp_exp_c);
}

/**
 * nsp_spcolmatrix_logel:
 * @A: a #NspSpColMatrix
 * 
 * 
 * log(A)
 * The real case is special since the result can be complex
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_logel(NspSpColMatrix *A)
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
	  SpColUnary(A,log,nsp_log_c);
	  return OK;
	}
      else 
	{
	  /* result is complex  */
	  if (nsp_spcolmatrix_seti(A,0.00) == FAIL ) return FAIL;
	  SpColUnary(A,log,nsp_log_c);
	  return OK;
	}
    }
  /* A is complex and sqrt(A) too **/
  SpColUnary(A,log,nsp_log_c);
  return OK;
}

/**
 * nsp_spcolmatrix_sin:
 * @A: a #NspSpColMatrix
 * 
 * A=sin(A)
 **/

void nsp_spcolmatrix_sin(NspSpColMatrix *A)
{
  SpColUnary(A,sin,nsp_sin_c);
}



/**
 * nsp_spcolmatrix_sinh:
 * @A: a #NspSpColMatrix
 * 
 * A=Sinh(A)
 **/

void nsp_spcolmatrix_sinh(NspSpColMatrix *A)
{
  SpColUnary(A,sinh,nsp_sinh_c);
}


/**
 * nsp_spcolmatrix_sqrtel:
 * @A: a #NspSpColMatrix
 * 
 * 
 *  A=SqrtEl(A)  term to term square root
 * The real case is special since the result can be complex
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_sqrtel(NspSpColMatrix *A)
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
	  SpColUnary(A,sqrt,nsp_sqrt_c);
	  return OK;
	}
      else 
	{
	  /* result is complex  */
	  if (nsp_spcolmatrix_seti(A,0.00) == FAIL ) return FAIL;
	  SpColUnary(A,sqrt,nsp_sqrt_c);
	  return OK;
	}
    }
  /* A is complex and sqrt(A) too **/
  SpColUnary(A,sqrt,nsp_sqrt_c);
  return OK;
}

/**
 * nsp_spcolmatrix_minus:
 * @A: a #NspSpColMatrix
 * 
 * 
 * A= -A 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_minus(NspSpColMatrix *A)
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
 * nsp_spcolmatrix_find:
 * @A: a #NspSpColMatrix
 * @lhs: an integer 
 * @Res1: a #NspMatrix pointer 
 * @Res2: a #NspMatrix pointer 
 * 
 * returns in a Matrix the indices for which the 
 * Matrix A has non zero entries 
 * A is left unchanged
 * according to lhs one or two arguments are returned 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcolmatrix_find(NspSpColMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2)
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
      count=0;
      for ( i = 0 ; i < A->n ; i++ )
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      (*Res2)->R[count]   = i + 1;
	      (*Res1)->R[count++] = A->D[i]->J[k] + 1;
	    }
	}
    }
  return OK;
}


/**
 * nsp_spcolmatrix_rand_one:
 * @m: number of rows
 * @n: number of columns
 * @sparsity: percent of non null elements
 * @crand: a character ('n' for normal or 'u' for uniform) 
 * 
 * returns a new #NspSpColMatrix filled with random values ('n' or 'u') 
 * the percent of non null elements is given by @sparsity.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_rand_one(int m,int n,double sparsity,char crand)
{
  double moy=0.0,std=1.0, mcol=m*sparsity;
  int mres;
  NspMatrix *icol=NULL;
  NspSpColMatrix *A=NULL;
  int k,i;
  if ((A =nsp_spcolmatrix_create(NVOID,'r',m,n))== NULLSPCOL ) 
    return NULLSPCOL;
  if ((icol=nsp_matrix_create(NVOID,'r',m,1)) == NULLMAT)
    {
      nsp_spcolmatrix_destroy(A);
      return NULLSPCOL;
    }
  for (i=0; i < icol->m ; i++) icol->R[i]=i;
  for ( i=0 ; i < A->n ; i++) 
    {
      double u=rand_ranf();
      mres =(int) ( (u >=0.5) ? ceil(mcol) : floor(mcol));
      /* XXX should use integers here */
      /* permute the icol vector */
      rand_genprm(icol->R,icol->m);
      /* sort the mres first elements */
      nsp_qsort_double(icol->R,NULL,FALSE,mres,'i');
      /* resize column i */
      nsp_spcolmatrix_resize_col(A,i,mres);
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  A->D[i]->J[k] = icol->R[k];
	}
      if ( crand == 'n' ) 
	for (k = 0 ; k < A->D[i]->size ; k++)
	  A->D[i]->R[k]= rand_gennor(moy,std);
      else 
	for (k = 0 ; k < A->D[i]->size ; k++)
	  A->D[i]->R[k]= rand_ranf();
    }
  nsp_matrix_destroy(icol);
  return A;
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
 * nsp_spcolmatrix_rand:
 * @m: number of rows
 * @n: number of columns
 * @sparsity: percent of non null elements
 * @crand: a character ('n' for normal or 'u' for uniform) 
 * 
 * returns a new #NspSpColMatrix filled with random values ('n' or 'u') 
 * the percent of non null elements is given by @sparsity.
 * 
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

NspSpColMatrix *nsp_spcolmatrix_rand(int m,int n,double sparsity,char crand)
{
  double moy=0.0, std=1.0;
  int *nb_elts_col = NULL, *icol = NULL, *tcol = NULL;
  NspSpColMatrix *A=NULLSPCOL;
  int nnz=(int)(m*(n*Min(Max(sparsity,0.0),1.0))), k, j;

  if ( (A =nsp_spcolmatrix_create(NVOID,'r',m,n)) == NULLSPCOL )
    return NULLSPCOL;

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
      nsp_spcolmatrix_resize_col(A, j, nnzj);

      /* fill the column j (with row indices and random values) */
      for ( k = 0 ; k < nnzj ; k++ )
	A->D[j]->J[k] = icol[k];
      if ( crand == 'n' )
	for ( k = 0 ; k < nnzj ; k++ )
	  A->D[j]->R[k] = rand_gennor(moy,std);
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
  nsp_spcolmatrix_destroy(A);
  return NULLSPCOL;
}


/*
 **/

static double nsp_spcolmatrix_norm1(NspSpColMatrix *A)
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

/* added by Bruno using lapack way of scaling */
static double nsp_spcolmatrix_fro(NspSpColMatrix *A)
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

static double nsp_spcolmatrix_norminf(NspSpColMatrix *A)
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
 * nsp_spcolmatrix_norm:
 * @A: a real or complex #NspSpColMatrix
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

double nsp_spcolmatrix_norm(NspSpColMatrix *A, char c)
{
  switch (c) {
  case '1':
    return nsp_spcolmatrix_norm1(A);
  case '2': 
    Scierror("Error: norm(.,2) not implemented for sparse matrices\n");
    return -1;
  case 'I':
    return nsp_spcolmatrix_norminf(A);
  case 'F':
    return nsp_spcolmatrix_fro(A);
  case 'M': 
    Scierror("Error: M norm not implemented for sparse matrices\n");
    return -1;
  }
  Scierror("Error: %c norm not implemented for sparse matrices\n",c);
  return -1;
}

/**
 * nsp_spcolmatrix_vnorm:
 * @A: a real or complex sparse vector in a #bNspSpColMatrix
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

double nsp_spcolmatrix_vnorm(NspSpColMatrix *A, double p)
{
  int i,j;
  double norm;
  NspMatrix *V=NULL;
  int nnz = nsp_spcolmatrix_nnz(A), count=0;
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
 * nsp_spcolmatrix_isnan:
 * @A: a #NspSpColMatrix
 * @flag: an integer 
 * 
 * if @flag is -1 then a new #NspSpColMatrix is returned 
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
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

typedef int (*Fis)(double) ;
static NspSpColMatrix *nsp_spcolmatrix_isnan_gen(NspSpColMatrix *A,int flag, Fis F);

static int isnan_f(double r)
{
  /* isnan is a macro */
  return isnan(r);
}

NspSpColMatrix *nsp_spcolmatrix_isnan(NspSpColMatrix *A,int flag)
{
  return nsp_spcolmatrix_isnan_gen(A,flag,isnan_f);
}


static NspSpColMatrix *nsp_spcolmatrix_isnan_gen(NspSpColMatrix *A,int flag, Fis F)
{
  int S=0;
  NspSpColMatrix *Res=NULL;
  int i,k,count,j;
  int c =  flag;
  if ( A->m == 0 || A->n == 0  )
    {
      switch (c ) 
	{
	case 0: 
	  return nsp_spcolmatrix_create(NVOID,'r',0,0);
	case 2:
	  return nsp_spcolmatrix_create(NVOID,'r',A->m,(A->n == 0) ? 0: 1);
	case 1: 
	  return nsp_spcolmatrix_create(NVOID,'r',(A->m == 0) ? 0: 1,A->n);
	case -1: 
	  return nsp_spcolmatrix_create(NVOID,'r',A->m,A->n);
	default: 
	  Scierror("Error: unknown dim flag %d\n", flag);
	  return NULLSPCOL;
	}
    }
  switch (c) 
    {
    case 0:
      /* return a 1x1 result */
      if ((Res =nsp_spcolmatrix_create(NVOID,'r',1,1)) == NULLSPCOL) return(NULLSPCOL);
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
	  if (nsp_spcolmatrix_resize_col(Res,0,1)== FAIL) return NULLSPCOL;
	  Res->D[0]->R[0] = 1;
	  Res->D[0]->J[0] = 0;
	}
      break;
    case 2:
      /* return a A->mx1 result */
      if ((Res =nsp_spcolmatrix_create(NVOID,'r',A->m,1)) == NULLSPCOL) return NULLSPCOL;
      if (nsp_spcolmatrix_resize_col(Res,0,A->m)== FAIL) return NULLSPCOL;
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
		    Res->D[0]->R[A->D[i]->J[k]] = 1;break;
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
	  int ndel =nsp_spcolmatrix_compress_col(Res,0);
	  if (nsp_spcolmatrix_resize_col(Res,0,Res->D[0]->size-ndel ) == FAIL) return NULLSPCOL;
	}
      break;
    case 1: 
      /* return a 1x A->n result */
      if ((Res =nsp_spcolmatrix_create(NVOID,'r',1,A->n)) == NULLSPCOL) return NULLSPCOL;
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
		  if (nsp_spcolmatrix_resize_col(Res,i,1)== FAIL) return NULLSPCOL;
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
		  if (nsp_spcolmatrix_resize_col(Res,i,1)== FAIL) return NULLSPCOL;
		  Res->D[i]->R[0] = 1;
		  Res->D[i]->J[0] = 0;
		}
	    }
	  break;
	}
      break;
    case -1: 
      if ((Res =nsp_spcolmatrix_create(NVOID,'r',A->m,A->n)) == NULLSPCOL) return NULLSPCOL;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( i = 0 ; i < A->n ; i++) 
	    {
	      int k=0;
	      if (nsp_spcolmatrix_resize_col(Res,i,A->D[i]->size)== FAIL) return NULLSPCOL;
	      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
		if ( F(A->D[i]->R[j]) ) 
		  {
		    Res->D[i]->J[k]=A->D[i]->J[j];
		    Res->D[i]->R[k]=1;
		    k++;
		  }
	      if (nsp_spcolmatrix_resize_col(Res,i,k)== FAIL) return NULLSPCOL;
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
	      if (nsp_spcolmatrix_resize_col(Res,i,k)== FAIL) return NULLSPCOL;
	    }
	  break;
	}
      break;
    default: 
      Scierror("Error: unknown flag %d \n", flag);
      return NULLSPCOL;
    }
  return Res;
}

/**
 * nsp_spcolmatrix_isinf:
 * @A: a #NspSpColMatrix
 * @flag: an integer. 
 * 
 * if @flag is -1 then a new #NspSpColMatrix is returned 
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
 * Return value: a new  #NspSColMatrix or %NULLSPCOL
 **/

static int isinf_f(double r)
{
  /* isinf can be a macro */
  return isinf(r);
}

NspSpColMatrix *nsp_spcolmatrix_isinf(NspSpColMatrix *A,int flag)
{
  return nsp_spcolmatrix_isnan_gen(A,flag,isinf_f);
}


/**
 * nsp_spcolmatrix_is_lower_triangular:
 * @A: a #NspSpColMatrix
 * 
 * the code should work even if zero elements are stored
 *
 * Return value: %TRUE or %FALSE
 **/
/* added by Bruno */
Boolean nsp_spcolmatrix_is_lower_triangular(NspSpColMatrix *A)
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
 * nsp_spcolmatrix_is_upper_triangular:
 * @A: a #NspSpColMatrix
 * 
 * the code should work even if zero elements are stored
 *
 * Return value: %TRUE or %FALSE
 **/
/* added by Bruno */
Boolean nsp_spcolmatrix_is_upper_triangular(NspSpColMatrix *A)
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


/* added by Bruno */
static int nsp_spcolmatrix_locate(SpCol *Col,int j)
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
 * nsp_spcolmatrix_is_symmetric:
 * @A: a #NspSpColMatrix
 * 
 * the code should work even if zero elements are stored
 *
 * Return value: %TRUE or %FALSE
 **/
/* added by Bruno */
Boolean nsp_spcolmatrix_is_symmetric(NspSpColMatrix *A)
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
		  kp = nsp_spcolmatrix_locate(A->D[i],j);
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
		  kp = nsp_spcolmatrix_locate(A->D[i],j);
		  if ( k < 0 )
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
 * nsp_spcolmatrix_lower_and_upper_bandwidth:
 * @A: a real or complex sparse matrix
 * @Kl: (output) the lower bandwidth
 * @Ku: (output) the upper bandwidth
 *
 * computes the lower and upper bandwith of @A
 * don't take into account stored zeros.
 * 
 * Return value: %FAIL (if the matrix is not square) or %OK 
 **/
/* added by Bruno */
int nsp_spcolmatrix_lower_and_upper_bandwidth(NspSpColMatrix *A, int *Kl, int *Ku)
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
 * nsp_spcolmatrix_solve_utri
 * @U: a #NspSpColMatrix
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
/* added by Bruno */
int nsp_spcolmatrix_solve_utri(NspSpColMatrix *U, NspMatrix *x, NspMatrix *b)
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

  /* solve now (column triangular algorithm because U is a SpColMat) */
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
 * nsp_spcolmatrix_solve_ltri
 * @L: a #NspSpColMatrix
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
/* added by Bruno */
int nsp_spcolmatrix_solve_ltri(NspSpColMatrix *L, NspMatrix *x, NspMatrix *b)
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

  /* solve now (column triangular algorithm because L is a SpColMat) */
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
 * nsp_spcolmatrix_scale_rows:
 * @A: a #NspSpColMatrix of size m x n
 * @x: a #NspMatrix must be a vector of size m (1 x m or m x 1)
 * 
 *  for (i from 0 to m-1)  
 *      multiplie row i of A by x[i]
 * 
 * Return value: %FAIL or %OK
 **/

int nsp_spcolmatrix_scale_rows(NspSpColMatrix *A, NspMatrix *x)
{
  int i,j, k;
  
  if(A->rc_type == 'r' ) 
    {
      if ( x->rc_type == 'r') 
	{
	  for ( j = 0 ; j < A->n ; j++)
	    for ( k = 0 ; k < A->D[j]->size ; k++ )
	      {
		i = A->D[j]->J[k];
		A->D[j]->R[k] *= x->R[i];
	      }
	}
      else 
	{
	  if ( nsp_spcolmatrix_complexify(A) == FAIL ) 
	    return FAIL;
	  for ( j = 0 ; j < A->n ; j++)
	    for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	      {
		i = A->D[j]->J[k];
		A->D[j]->C[k].i = A->D[j]->C[k].r * x->C[i].i;
		A->D[j]->C[k].r *= x->C[i].r;
	      }
	}
    }
  else
    {
      if ( x->rc_type == 'r') 
	for ( j = 0 ; j < A->n ; j++)
	  for ( k = 0 ; k <  A->D[j]->size ; k++ )
	    {
	      i = A->D[j]->J[k];
	      A->D[j]->C[k].r *= x->R[i];
	      A->D[j]->C[k].i *= x->R[i];
	    }
      else 
	for ( j = 0 ; j < A->n ; j++)
	  for ( k = 0 ; k <  A->D[j]->size ; k++ )
	    {
	      i = A->D[j]->J[k];
	      nsp_prod_c(&A->D[j]->C[k],&x->C[i]);
	    }
    }
  return OK;
}

/**
 * nsp_spcolmatrix_scale_cols:
 * @A: a #NspSpColMatrix of size m x n
 * @x: a #NspMatrix must be a vector of size n (1 x n or n x 1)
 * 
 *  for (j from 0 to n-1)  
 *      multiplie column j of A by x[j]
 * 
 * Return value: %FAIL or %OK
 **/

int nsp_spcolmatrix_scale_cols(NspSpColMatrix *A, NspMatrix *x)
{
  int j, k;
  
  if(A->rc_type == 'r' ) 
    {
      if ( x->rc_type == 'r') 
	{
	  for ( j = 0 ; j < A->n ; j++)
	    for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	      A->D[j]->R[k] *= x->R[j];
	}
      else 
	{
	  if ( nsp_spcolmatrix_complexify(A) == FAIL ) 
	    return FAIL;
	  for ( j = 0 ; j < A->n ; j++)
	    for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	      {
		A->D[j]->C[k].i = A->D[j]->C[k].r * x->C[j].i;
		A->D[j]->C[k].r *= x->C[j].r;
	      }
	}
    }
  else
    {
      if ( x->rc_type == 'r') 
	for ( j = 0 ; j < A->n ; j++)
	  for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	    {
	      A->D[j]->C[k].r *= x->R[j];
	      A->D[j]->C[k].i *= x->R[j];
	    }
      else 
	for ( j = 0 ; j < A->n ; j++)
	  for ( k = 0 ; k < A->D[j]->size ; k++ ) 
	    nsp_prod_c(&A->D[j]->C[k],&x->C[j]);
    }
  return OK;
}
