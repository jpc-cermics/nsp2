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

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/gsort-p.h"

/*
 * General sorting routines 
 * xI is the transmitted table to sort ( if table is int ) 
 * xD is the transmitted table to sort ( if table is double ) 
 * ind is the int table to store the permutation 
 *     (which is to be initialized and changed )
 * iflag == if 1 ind is to be computed if 0 ind is ignored 
 * m,n : matrix size 
 * type : the operation ( see the interface ) 
 * iord : "i" or "d" : increasind or decreasing sort 
 */

/**
 * nsp_matrix_sort:
 * @A: 
 * @Ind: 
 * @ind_flag: 
 * @dir: direction 'i' for increasing 'd' for decreasing 
 * 
 * global sort of the elements of a matrix. If flag is %TRUE 
 * Index is computed and returned 
 * 
 **/

int nsp_matrix_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir, nsp_sort type)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  switch (type)
    {
    case sort_gb: 
      /* qsort Bruno */
      nsp_qsort_bp_double(A->R,A->mn,index,ind_flag,dir);break;
    case sort_gs:
      /* stable quick sort */
      if ( dir == 'i' )
	nsp_qsort_stable_incr_double(A->R,index,ind_flag,0,A->mn,dir);
      else 
	nsp_qsort_stable_decr_double(A->R,index,ind_flag,0,A->mn,dir);
      break;
    case sort_gm:
      /* merge sort */
      if ( nsp_mergesort_double(A->R,index,ind_flag,0,A->mn,dir)==FAIL) return FAIL;
      break;
    case sort_gd :
      /* non stable qsort */
      nsp_qsort_double(A->R,index,ind_flag,A->mn,dir);break;      
    default: 
      /* non stable qsort specializd for double (faster than the generic one) */
      nsp_qsort_double(A->R,index,ind_flag,A->mn,dir);break;
      /* for testing the int case 
       * A = Mat2int(A);
       * A->convert = 'i';
       * nsp_qsort_int(A->I,index,ind_flag,A->mn,dir);break;      
       */
    }
  return OK;
}

int nsp_matrix_column_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return FAIL ;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  nsp_qsort_gen_col_sort_double(A->R,index,ind_flag,A->m,A->n,dir);
  return OK;
}

int nsp_matrix_row_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  nsp_qsort_gen_row_sort_double(A->R,index,ind_flag,A->m,A->n,dir);
  return OK;
}


int nsp_matrix_lexical_column_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir,char mode)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',1,A->n) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  if ( mode == 'i') 
    {
      A = Mat2int(A);
      nsp_qsort_gen_lexicol_int(A->I,index,ind_flag,A->m,A->n,dir);
    }
  else 
    {
      nsp_qsort_gen_lexicol_double(A->R,index,ind_flag,A->m,A->n,dir);
    }
  return OK;
}

int nsp_matrix_lexical_row_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir,char mode)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,1) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  if ( mode == 'i') 
    {
      A = Mat2int(A);
      nsp_qsort_gen_lexirow_int(A->I,index,ind_flag,A->m,A->n,dir);
    }
  else 
    {
      nsp_qsort_gen_lexirow_double(A->R,index,ind_flag,A->m,A->n,dir);
    }
  return OK;
}


/**
 * nsp_smatrix_sort:
 * @A: 
 * @Ind: 
 * @ind_flag: 
 * @dir: direction 'i' for increasing 'd' for decreasing 
 * 
 * global sort of the elements of a smatrix. If flag is %TRUE 
 * Index is computed and returned 
 * 
 **/

int nsp_smatrix_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  /* use the specialized version */
  nsp_qsort_nsp_string(A->S,index,ind_flag,A->mn,dir);
  return OK;
}

int nsp_smatrix_column_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return FAIL ;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  nsp_qsort_gen_col_sort_nsp_string(A->S,index,ind_flag,A->m,A->n,dir);
  return OK;
}

int nsp_smatrix_row_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  nsp_qsort_gen_row_sort_nsp_string(A->S,index,ind_flag,A->m,A->n,dir);
  return OK;
}


int nsp_smatrix_lexical_column_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',1,A->n) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  nsp_qsort_gen_lexicol_nsp_string(A->S,index,ind_flag,A->m,A->n,dir);
  return OK;
}

int nsp_smatrix_lexical_row_sort(NspSMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,1) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  nsp_qsort_gen_lexirow_nsp_string(A->S,index,ind_flag,A->m,A->n,dir);
  return OK;
}

/* to be removed FIXME */

int nsp_gsort(int *xI, double *xD, int *ind, int *iflag, int *m, int *n,nsp_const_string type,nsp_const_string iord)
{
  /* int i; */
  switch ( type[0])
    {
    case 'r' :  nsp_qsort_gen_col_sort_double(xD,ind,*iflag,*m,*n,iord[0]);break;
    case 'c' :  nsp_qsort_gen_row_sort_double(xD,ind,*iflag,*m,*n,iord[0]);break;
    case 'l' :  
      if ( type[1] == 'r' ) 
	nsp_qsort_gen_lexirow_int(xI,ind,*iflag,*m,*n,iord[0]);
      else
	nsp_qsort_gen_lexicol_int(xI,ind,*iflag,*m,*n,iord[0]);
      break;
    case 'i' : nsp_qsort_int(xI,ind,*iflag,*m*(*n),iord[0]);break;
    case 'g' : 
    default :  
      nsp_qsort_double(xD,ind,*iflag,*m*(*n),iord[0]);break;
    }
  return(0);
}

