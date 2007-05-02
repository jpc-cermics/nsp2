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
 * Use to allocated, store and convert sparse data with Matab 
 * storage convention in a nsp coded sparse matrix. 
 *
 * Note that sparse data structure in the Matlab style is called triplet 
 * which stands for matlab internal storage convention (Jc,Ir,Pr) and NOT for the 
 * triplet convention (I,J,Val).
 */
  
#include "nsp/object.h"

static int nsp_sprow_fill_zi_triplet(const NspSpRowMatrix *M);
static void nsp_sprow_free_triplet( NspSpRowMatrix *M);
static int nsp_sprow_set_triplet_from_m_internal( NspSpRowMatrix *M,int flag);
static int nsp_sprow_update_from_triplet_internal( NspSpRowMatrix *M);
static int nsp_sprow_alloc_triplet( NspSpRowMatrix *M,int nzmax);

/**
 * nsp_sprow_set_triplet_from_m:
 * @M: a sparse row coded matrix (#NspSpRowMatrix)
 * @flag: %TRUE or %FALSE
 * 
 * This function is used to allocate triplet data. If @flag 
 * is %TRUE the triplet data is also initialized using sparse matrix data. 
 * 
 * Return value: %OK or %FAIL
 **/


int nsp_sprow_set_triplet_from_m( NspSpRowMatrix *M,int flag)
{
  NspSpRowMatrix *Mt = nsp_sprowmatrix_transpose(M);
  if ( Mt == NULL) return FAIL;
  if ( nsp_sprow_set_triplet_from_m_internal(Mt,flag) == OK) 
    {
      M->convert = 't';
      M->triplet = Mt->triplet;
      Mt->convert = 'v';
    }
  nsp_sprowmatrix_destroy(Mt);
  return OK;
}

/**
 * nsp_sprow_update_from_triplet:
 * @M: a sparse row coded matrix (#NspSpRowMatrix)
 * 
 * Update the nsp sparse data with the triplet data. 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprow_update_from_triplet( NspSpRowMatrix *M)
{
  int i,nn;
  NspSpRowMatrix *Mt;
  /* partial free */
  for ( i = 0  ; i < M->m ; i++) 
    {
      nsp_sprowmatrix_row_destroy(M->D[i]);
    }
  FREE(M->D);
  M->m= M->n = 0;
  /* switch triplet */
  nn =   M->triplet.m ;
  M->triplet.m = M->triplet.n;
  M->triplet.n = nn;
  if ( nsp_sprow_update_from_triplet_internal(M) == FAIL) return FAIL;
  /* now M is filled with its traspose */
  Mt = nsp_sprowmatrix_transpose(M);
  /* fill with Mt */
  M->m=Mt->m;
  M->n=Mt->n;
  M->mn = M->m*M->n;
  M->rc_type= Mt->rc_type;
  M->convert = Mt->convert;
  M->triplet = Mt->triplet;
  M->D = Mt->D ;
  /* free Mt */
  Mt->D= NULL; Mt->m = Mt->n = Mt->mn = 0;
  nsp_sprowmatrix_destroy(Mt);
  return OK;
}

/**
 * nsp_sprow_alloc_col_triplet:
 * @M: a sparse row coded matrix (#NspSpRowMatrix)
 * @nzmax: initial max number of non nul elements.
 * 
 * Allocate internal triplet data. This is needed in mexlib 
 * until default representation is switched. from row to column.
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprow_alloc_col_triplet(NspSpRowMatrix *M,int nzmax)
{
  if ( M->convert == 't' ) return OK;
  if ((M->triplet.Jc = malloc(sizeof(int)*(M->n+1)))== NULL) return FAIL;
  if ((M->triplet.Ir = malloc(sizeof(int)*(nzmax)))== NULL) return FAIL;
  if ((M->triplet.Pr = malloc(sizeof(double)*(nzmax))) == NULL)
      return FAIL;
  M->triplet.Pi=NULL;
  if ( M->rc_type=='c' ) 
    {
      if ((M->triplet.Pi = malloc(sizeof(double)*(nzmax))) == NULL)
	return FAIL;
    }
  M->triplet.Aisize = nzmax;
  M->triplet.m= M->m;
  M->triplet.n= M->n;
  M->convert = 't';
  return OK;
}

/**
 * nsp_sprow_realloc_col_triplet:
 * @M: a sparse row coded matrix (#NspSpRowMatrix)
 * @nzmax: max number of non nul elements.
 * 
 * reallocates internal triplet data.
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_sprow_realloc_col_triplet( NspSpRowMatrix *M,int nzmax)
{
  if ( M->convert != 't' ) 
    {
      return nsp_sprow_alloc_col_triplet(M,nzmax);
    }
  else
    {
      /* no need to realloc Jc */
      if ((M->triplet.Ir = realloc(M->triplet.Ir,sizeof(int)*(nzmax)))== NULL) return FAIL;
      if ((M->triplet.Pr = realloc(M->triplet.Pr,sizeof(double)*(nzmax))) == NULL)
	return FAIL;
      if ( M->rc_type=='c' )
	{
	  if ((M->triplet.Pi = realloc(M->triplet.Pi,sizeof(double)*(nzmax))) == NULL)
	    return FAIL;
	}
      M->triplet.Aisize = nzmax;
      M->triplet.m= M->m;
      M->triplet.n= M->n;
    }
  return OK;
}


/**
 * nsp_sprow_set_triplet_from_m_internal:
 * @M: a sparse row coded matrix (#NspSpRowMatrix)
 * @flag: %TRUE or %FALSE
 * 
 * This function is used to allocate triplet data. If @flag 
 * is %TRUE the triplet data is also initialized using sparse matrix data. 
 * 
 * Return value: %OK or %FAIL
 **/

static int nsp_sprow_set_triplet_from_m_internal( NspSpRowMatrix *M,int flag)
{
  int nnz = nsp_sprowmatrix_nnz(M);
  if ( M->convert == 't' ) return OK;
  if ( nsp_sprow_alloc_triplet(M,nnz) == FAIL) return FAIL;
  if ( flag == TRUE )
    {
      if ( nsp_sprow_fill_zi_triplet(M)== FAIL) 
	{
	  nsp_sprow_free_triplet(M);
	  return FAIL;
	}
    }
  return OK;
}


/**
 * nsp_sprow_update_from_triplet_internal:
 * @M: a sparse row coded matrix (#NspSpRowMatrix)
 * 
 * Update the nsp sparse data with the triplet data. 
 * 
 * Return value:  %OK or %FAIL
 **/

static int nsp_sprow_update_from_triplet_internal( NspSpRowMatrix *M)
{
  int i;
  /* use triplet to back change M */
  if ( M->convert != 't' ) return OK;
  if ( M->m < M->triplet.m ) 
    {
      if ( nsp_sprowmatrix_enlarge_rows(M,M->triplet.m-M->m)== FAIL) return FAIL;
    }
  else if ( M->triplet.m < M->m ) 
    {
      /* delete extra lines */
      for ( i = M->triplet.m ; i < M->m ; i++)
	{
	  nsp_sprowmatrix_row_destroy(M->D[i]);
	  M->D[i]= NULL;
	}
    }
  M->m = M->triplet.m;
  M->n = M->triplet.n;
  M->mn = M->m*M->n;
  /* resize each row */
  for ( i = 0 ; i < M->m ; i++) 
    {
      int row_size = M->triplet.Jc[i+1]-M->triplet.Jc[i];
      if (nsp_sprowmatrix_resize_row(M,i,row_size) == FAIL) return FAIL;
    }
  if ( M->rc_type == 'r')
    {
      for ( i = 0 ; i < M->m ; i++)
	{
	  int start = M->triplet.Jc[i],j;
	  SpRow *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      D->J[j]= M->triplet.Ir[start+j];
	      D->R[j]= M->triplet.Pr[start+j];
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < M->m ; i++)
	{
	  int start = M->triplet.Jc[i],j;
	  SpRow *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      D->J[j]= M->triplet.Ir[start+j];
	      D->C[j].r= M->triplet.Pr[start+j];
	      D->C[j].i= M->triplet.Pi[start+j];
	    }
	}
    }
  nsp_sprow_free_triplet(M);
  return OK;
}



/**
 * nsp_sprow_fill_zi_triplet:
 * @M:  a sparse row coded matrix (#NspSpRowMatrix)
 * 
 * from internal coding to the (Jc,Ir,Pr) coding using int and double.
 * Note that the matrix @M is row coded and thus the triplet is also row coded.
 * Here, we assume that the triplet (Jc,Ir,Az) has already been allocated !
 * 
 * Return value:  %OK or %FAIL
 **/

static int nsp_sprow_fill_zi_triplet(const NspSpRowMatrix *M)
{
  int i, *loc;
  /* fill the array Jc */
  if ( M->convert != 't' ) return FAIL;
  loc = M->triplet.Jc; loc[0]=0;
  for ( i = 1 ; i <= M->m ; i++)
    {
      loc[i]= loc[i-1] + M->D[i-1]->size;
    }
  /* fills the array Ir and Pr */
  if ( M->rc_type == 'r')
    {
      for ( i = 0 ; i < M->m ; i++)
	{
	  int start = M->triplet.Jc[i],j;
	  SpRow *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      M->triplet.Ir[start+j]= D->J[j];
	      M->triplet.Pr[start+j]= D->R[j];
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < M->m ; i++)
	{
	  int start = M->triplet.Jc[i],j;
	  SpRow *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      M->triplet.Ir[start+j]=  D->J[j];
	      /* 
		 M->triplet.Pr[2*(start+j)]= D->C[j].r;
		 M->triplet.Pr[2*(start+j)+1]= D->C[j].i;
	      */
	      M->triplet.Pr[start+j]= D->C[j].r;
	      M->triplet.Pi[start+j]= D->C[j].i;
	    }
	}
    }
  return OK;
}

static void nsp_sprow_free_triplet(NspSpRowMatrix *M)
{
  M->convert = 'v';
  FREE(M->triplet.Jc);
  FREE(M->triplet.Ir);
  FREE(M->triplet.Pr);
  FREE(M->triplet.Pi);
}

/**
 * nsp_sprow_alloc_triplet:
 * @M:  a sparse row coded matrix (#NspSpRowMatrix)
 * @nzmax: max number of non nul elements.
 * 
 * This function is used to allocate triplet data.
 * 
 * Return value:  %OK or %FAIL
 **/

static int nsp_sprow_alloc_triplet(NspSpRowMatrix *M,int nzmax)
{
  if ( M->convert == 't' ) return OK;
  if ((M->triplet.Jc = malloc(sizeof(int)*(M->m+1)))== NULL) return FAIL;
  if ((M->triplet.Ir = malloc(sizeof(int)*(nzmax)))== NULL) return FAIL;
  if ((M->triplet.Pr = malloc(sizeof(double)*(nzmax)*(M->rc_type=='c' ? 2: 1))) == NULL)
      return FAIL;
  M->triplet.Pi = NULL;
  if ( M->rc_type=='c' )
    {
      if ((M->triplet.Pi = malloc(sizeof(double)*(nzmax))) == NULL)
	return FAIL;
    }
  M->triplet.Aisize = nzmax;
  M->triplet.m= M->m;
  M->triplet.n= M->n;
  M->convert = 't';
  return OK;
}

/* spcol versions 
 */


static int nsp_spcol_fill_zi_triplet(const NspSpColMatrix *M);
static void nsp_spcol_free_triplet( NspSpColMatrix *M);
static int nsp_spcol_set_triplet_from_m_internal( NspSpColMatrix *M,int flag);
static int nsp_spcol_update_from_triplet_internal( NspSpColMatrix *M);

/**
 * nsp_spcol_set_triplet_from_m:
 * @M: a sparse column coded matrix (#NspSpColMatrix)
 * @flag: %TRUE or %FALSE
 * 
 * This function is used to allocate triplet data 
 * to internal representation of M. If @flag 
 * is %TRUE the triplet data is also initialized using sparse matrix data. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_spcol_set_triplet_from_m( NspSpColMatrix *M,int flag)
{
  if ( nsp_spcol_set_triplet_from_m_internal(M,flag) == FAIL) 
    return FAIL;
  M->convert = 't';
  return OK;
}

/**
 * nsp_spcol_update_from_triplet:
 * @M: a sparse column coded matrix (#NspSpColMatrix)
 * 
 * updates @M data from its triplet internal representation.
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcol_update_from_triplet( NspSpColMatrix *M)
{
  int i;
  /* partial free */
  for ( i = 0  ; i < M->n ; i++) 
    {
      nsp_spcolmatrix_col_destroy(M->D[i]);
      M->D[i]=NULL;
    }
  FREE(M->D);
  M->m= M->n = M->mn = 0;
  if ( nsp_spcol_update_from_triplet_internal(M) == FAIL) return FAIL;
  return OK;
}

/**
 * nsp_spcol_alloc_col_triplet:
 * @M: a sparse column coded matrix (#NspSpColMatrix)
 * @nzmax: initial max number of non nul elements. 
 * 
 * Allocate internal triplet data.
 *
 * Return value:  %OK or %FAIL
 **/

int nsp_spcol_alloc_col_triplet(NspSpColMatrix *M,int nzmax)
{
  if ( M->convert == 't' ) return OK;
  if ((M->triplet.Jc = malloc(sizeof(int)*(M->n+1)))== NULL) return FAIL;
  if ((M->triplet.Ir = malloc(sizeof(int)*(nzmax)))== NULL) return FAIL;
  if ((M->triplet.Pr = malloc(sizeof(double)*(nzmax))) == NULL)
      return FAIL;
  M->triplet.Pi =NULL;
  if (M->rc_type=='c' ) 
    {
      if ((M->triplet.Pi = malloc(sizeof(double)*(nzmax))) == NULL)
	return FAIL;
    }
  M->triplet.Aisize = nzmax;
  M->triplet.m= M->m;
  M->triplet.n= M->n;
  M->convert = 't';
  return OK;
}

/**
 * nsp_spcol_realloc_col_triplet:
 * @M: a sparse column coded matrix (#NspSpColMatrix)
 * @nzmax: initial max number of non nul elements. 
 * 
 * reallocates internal triplet data.
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_spcol_realloc_col_triplet( NspSpColMatrix *M,int nzmax)
{
  int cp = (M->rc_type=='c' ? 2: 1);
  if ( M->convert != 't' ) 
    {
      return nsp_spcol_alloc_col_triplet(M,nzmax);
    }
  else
    {
      /* no need to realloc Jc */
      if ((M->triplet.Ir = realloc(M->triplet.Ir,sizeof(int)*(nzmax)))== NULL) return FAIL;
      if ((M->triplet.Pr = realloc(M->triplet.Pr,sizeof(double)*(nzmax)*cp)) == NULL)
	return FAIL;
      if (M->rc_type=='c')
	{
	  if ((M->triplet.Pi = realloc(M->triplet.Pi,sizeof(double)*(nzmax)*cp)) == NULL)
	    return FAIL;
	}
      M->triplet.Aisize = nzmax;
      M->triplet.m= M->m;
      M->triplet.n= M->n;
    }
  return OK;
}


/**
 * nsp_spcol_set_triplet_from_m_internal:
 * @M: a sparse column coded matrix (#NspSpColMatrix)
 * @flag: 
 * 
 * This function is used to allocate triplet data. If @flag 
 * is %TRUE the triplet data is also initialized using sparse matrix data. 
 * 
 * Return value:  %OK or %FAIL
 **/

static int nsp_spcol_set_triplet_from_m_internal( NspSpColMatrix *M,int flag)
{
  int nnz = nsp_spcolmatrix_nnz(M);
  if ( M->convert == 't' ) return OK;
  if ( nsp_spcol_alloc_col_triplet(M,nnz) == FAIL) return FAIL;
  if ( flag == TRUE )
    {
      if ( nsp_spcol_fill_zi_triplet(M)== FAIL) 
	{
	  nsp_spcol_free_triplet(M);
	  return FAIL;
	}
    }
  return OK;
}


/**
 * nsp_spcol_update_from_triplet_internal:
 * @M: a sparse column coded matrix (#NspSpColMatrix)
 * 
 * updates @M data from its triplet internal representation.
 * 
 * Return value:  %OK or %FAIL
 **/

static int nsp_spcol_update_from_triplet_internal( NspSpColMatrix *M)
{
  int i;
  /* use triplet to back change M */
  if ( M->convert != 't' ) return OK;
  if ( M->n < M->triplet.n ) 
    {
      if ( nsp_spcolmatrix_enlarge_cols(M,M->triplet.n-M->n)== FAIL) return FAIL;
    }
  else if ( M->triplet.n < M->n ) 
    {
      /* delete extra columns */
      for ( i = M->triplet.n ; i < M->n ; i++)
	{
	  nsp_spcolmatrix_col_destroy(M->D[i]);
	  M->D[i]= NULL;
	}
    }
  M->m = M->triplet.m;
  M->n = M->triplet.n;
  M->mn = M->m*M->n;
  /* resize each column */
  for ( i = 0 ; i < M->n ; i++) 
    {
      int col_size = M->triplet.Jc[i+1]-M->triplet.Jc[i];
      if (nsp_spcolmatrix_resize_col(M,i,col_size) == FAIL) return FAIL;
    }
  if ( M->rc_type == 'r')
    {
      for ( i = 0 ; i < M->n ; i++)
	{
	  int start = M->triplet.Jc[i],j;
	  SpCol *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      D->J[j]= M->triplet.Ir[start+j];
	      D->R[j]= M->triplet.Pr[start+j];
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < M->n ; i++)
	{
	  int start = M->triplet.Jc[i],j;
	  SpCol *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      D->J[j]= M->triplet.Ir[start+j];
	      D->C[j].r= M->triplet.Pr[start+j];
	      D->C[j].i= M->triplet.Pi[start+j];
	    }
	}
    }
  nsp_spcol_free_triplet(M);
  return OK;
}

/**
 * nsp_spcol_fill_zi_triplet:
 * @M: a sparse column coded matrix (#NspSpColMatrix)
 * 
 * from internal coding to the Jc,Ir,Az coding using int and double.
 * Here, we assume that the triplet (Jc,Ir,Az) has already been allocated !
 * 
 * Return value:  %OK or %FAIL
 **/

static int nsp_spcol_fill_zi_triplet(const NspSpColMatrix *M)
{
  int i, *loc;
  /* fill the array Jc */
  if ( M->convert != 't' ) return FAIL;
  loc = M->triplet.Jc; loc[0]=0;
  for ( i = 1 ; i <= M->n ; i++)
    {
      loc[i]= loc[i-1] + M->D[i-1]->size;
    }
  /* fills the array Ir and Pr */
  if ( M->rc_type == 'r')
    {
      for ( i = 0 ; i < M->n ; i++)
	{
	  int start = M->triplet.Jc[i],j;
	  SpCol *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      M->triplet.Ir[start+j]= D->J[j];
	      M->triplet.Pr[start+j]= D->R[j];
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < M->n ; i++)
	{
	  int start = M->triplet.Jc[i],j;
	  SpCol *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      M->triplet.Ir[start+j]=  D->J[j];
	      M->triplet.Pr[start+j]=  D->C[j].r;
	      M->triplet.Pi[start+j]= D->C[j].i;
	    }
	}
    }
  return OK;
}

/**
 * nsp_spcol_free_triplet:
 * @M: a sparse column coded matrix (#NspSpColMatrix)
 * 
 * free the triplet storage. 
 *
 **/

static void nsp_spcol_free_triplet(NspSpColMatrix *M)
{
  M->convert = 'v';
  FREE(M->triplet.Jc);
  FREE(M->triplet.Ir);
  FREE(M->triplet.Pr);
  FREE(M->triplet.Pi);
}

