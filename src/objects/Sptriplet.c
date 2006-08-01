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
 */
  
#include "nsp/object.h"

static int nsp_sprow_fill_zi_triplet(const NspSpRowMatrix *M);
static void nsp_sprow_free_triplet( NspSpRowMatrix *M);
static int nsp_sprow_set_triplet_from_m_internal( NspSpRowMatrix *M,int flag);
static int nsp_sprow_update_from_triplet_internal( NspSpRowMatrix *M);
static int nsp_sprow_alloc_triplet( NspSpRowMatrix *M,int nzmax);

/* tricky version to get a column triplet */

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

/* tricky version to update from  a column triplet */

int nsp_sprow_update_from_triplet( NspSpRowMatrix *M)
{
  int i,nn;
  NspSpRowMatrix *Mt;
  /* partial free */
  for ( i = 0  ; i < M->m ; i++) 
    {
      nsp_sprowmatrix_row_destroy(M->D[i]);
      FREE(M->D[i]);
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

/* needed in mexlib until representation is switched */

int nsp_sprow_alloc_col_triplet(NspSpRowMatrix *M,int nzmax)
{
  if ( M->convert == 't' ) return OK;
  if ((M->triplet.Ap = malloc(sizeof(int)*(M->n+1)))== NULL) return FAIL;
  if ((M->triplet.Ai = malloc(sizeof(int)*(nzmax)))== NULL) return FAIL;
  if ((M->triplet.Ax = malloc(sizeof(double)*(nzmax)*(M->rc_type=='c' ? 2: 1))) == NULL)
      return FAIL;
  M->triplet.Aisize = nzmax;
  M->triplet.m= M->m;
  M->triplet.n= M->n;
  M->convert = 't';
  return OK;
}

int nsp_sprow_realloc_col_triplet( NspSpRowMatrix *M,int nzmax)
{
  int cp = (M->rc_type=='c' ? 2: 1);
  if ( M->convert != 't' ) 
    {
      return nsp_sprow_alloc_col_triplet(M,nzmax);
    }
  else
    {
      /* no need to realloc Ap */
      if ((M->triplet.Ai = realloc(M->triplet.Ai,sizeof(int)*(nzmax)))== NULL) return FAIL;
      if ((M->triplet.Ax = realloc(M->triplet.Ax,sizeof(double)*(nzmax)*cp)) == NULL)
	return FAIL;
      M->triplet.Aisize = nzmax;
      M->triplet.m= M->m;
      M->triplet.n= M->n;
    }
  return OK;
}


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
	  FREE(M->D[i]);
	  M->D[i]= NULL;
	}
    }
  M->m = M->triplet.m;
  M->n = M->triplet.n;
  /* resize each row */
  for ( i = 0 ; i < M->m ; i++) 
    {
      int row_size = M->triplet.Ap[i+1]-M->triplet.Ap[i];
      if (nsp_sprowmatrix_resize_row(M,i,row_size) == FAIL) return FAIL;
    }
  if ( M->rc_type == 'r')
    {
      for ( i = 0 ; i < M->m ; i++)
	{
	  int start = M->triplet.Ap[i],j;
	  SpRow *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      D->J[j]= M->triplet.Ai[start+j];
	      D->R[j]= M->triplet.Ax[start+j];
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < M->m ; i++)
	{
	  int start = M->triplet.Ap[i],j;
	  SpRow *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      D->J[j]= M->triplet.Ai[start+j];
	      /* 
		 D->C[j].r= M->triplet.Ax[2*(start+j)];
		 D->C[j].i= M->triplet.Ax[2*(start+j)+1];
	      */
	      D->C[j].r= M->triplet.Ax[start+j];
	      D->C[j].i= M->triplet.Ax[start+j+M->triplet.Aisize];
	    }
	}
    }
  nsp_sprow_free_triplet(M);
  return OK;
}


/* from internal coding to the Ap,Ai,Az coding using int and double.
 * Note that the matrix M is row coded and thus the triplet is also 
 * row coded 
 * here the triplet has been allocated !
 */

static int nsp_sprow_fill_zi_triplet(const NspSpRowMatrix *M)
{
  int i, *loc;
  /* fill the array Ap */
  if ( M->convert != 't' ) return FAIL;
  loc = M->triplet.Ap; loc[0]=0;
  for ( i = 1 ; i <= M->m ; i++)
    {
      loc[i]= loc[i-1] + M->D[i-1]->size;
    }
  /* fills the array Ai and Ax */
  if ( M->rc_type == 'r')
    {
      for ( i = 0 ; i < M->m ; i++)
	{
	  int start = M->triplet.Ap[i],j;
	  SpRow *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      M->triplet.Ai[start+j]= D->J[j];
	      M->triplet.Ax[start+j]= D->R[j];
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < M->m ; i++)
	{
	  int start = M->triplet.Ap[i],j;
	  SpRow *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      M->triplet.Ai[start+j]=  D->J[j];
	      /* 
		 M->triplet.Ax[2*(start+j)]= D->C[j].r;
		 M->triplet.Ax[2*(start+j)+1]= D->C[j].i;
	      */
	      M->triplet.Ax[start+j]= D->C[j].r;
	      M->triplet.Ax[start+j+M->triplet.Aisize]= D->C[j].i;
	    }
	}
    }
  return OK;
}

static void nsp_sprow_free_triplet(NspSpRowMatrix *M)
{
  M->convert = 'v';
  FREE(M->triplet.Ap);
  FREE(M->triplet.Ai);
  FREE(M->triplet.Ax);
}

static int nsp_sprow_alloc_triplet(NspSpRowMatrix *M,int nzmax)
{
  if ( M->convert == 't' ) return OK;
  if ((M->triplet.Ap = malloc(sizeof(int)*(M->m+1)))== NULL) return FAIL;
  if ((M->triplet.Ai = malloc(sizeof(int)*(nzmax)))== NULL) return FAIL;
  if ((M->triplet.Ax = malloc(sizeof(double)*(nzmax)*(M->rc_type=='c' ? 2: 1))) == NULL)
      return FAIL;
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

/* associate a column triplet to internal representation of M */

int nsp_spcol_set_triplet_from_m( NspSpColMatrix *M,int flag)
{
  if ( nsp_spcol_set_triplet_from_m_internal(M,flag) == FAIL) 
    return FAIL;
  M->convert = 't';
  return OK;
}

/* update M data from its triplet internal representation  */

int nsp_spcol_update_from_triplet( NspSpColMatrix *M)
{
  int i;
  /* partial free */
  for ( i = 0  ; i < M->n ; i++) 
    {
      nsp_spcolmatrix_col_destroy(M->D[i]);
      FREE(M->D[i]);
    }
  FREE(M->D);
  M->m= M->n = 0;
  if ( nsp_spcol_update_from_triplet_internal(M) == FAIL) return FAIL;
  return OK;
}

/* needed in mexlib until representation is switched */

int nsp_spcol_alloc_col_triplet(NspSpColMatrix *M,int nzmax)
{
  if ( M->convert == 't' ) return OK;
  if ((M->triplet.Ap = malloc(sizeof(int)*(M->n+1)))== NULL) return FAIL;
  if ((M->triplet.Ai = malloc(sizeof(int)*(nzmax)))== NULL) return FAIL;
  if ((M->triplet.Ax = malloc(sizeof(double)*(nzmax)*(M->rc_type=='c' ? 2: 1))) == NULL)
      return FAIL;
  M->triplet.Aisize = nzmax;
  M->triplet.m= M->m;
  M->triplet.n= M->n;
  M->convert = 't';
  return OK;
}

int nsp_spcol_realloc_col_triplet( NspSpColMatrix *M,int nzmax)
{
  int cp = (M->rc_type=='c' ? 2: 1);
  if ( M->convert != 't' ) 
    {
      return nsp_spcol_alloc_col_triplet(M,nzmax);
    }
  else
    {
      /* no need to realloc Ap */
      if ((M->triplet.Ai = realloc(M->triplet.Ai,sizeof(int)*(nzmax)))== NULL) return FAIL;
      if ((M->triplet.Ax = realloc(M->triplet.Ax,sizeof(double)*(nzmax)*cp)) == NULL)
	return FAIL;
      M->triplet.Aisize = nzmax;
      M->triplet.m= M->m;
      M->triplet.n= M->n;
    }
  return OK;
}


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
	  FREE(M->D[i]);
	  M->D[i]= NULL;
	}
    }
  M->m = M->triplet.m;
  M->n = M->triplet.n;
  /* resize each column */
  for ( i = 0 ; i < M->n ; i++) 
    {
      int col_size = M->triplet.Ap[i+1]-M->triplet.Ap[i];
      if (nsp_spcolmatrix_resize_col(M,i,col_size) == FAIL) return FAIL;
    }
  if ( M->rc_type == 'r')
    {
      for ( i = 0 ; i < M->n ; i++)
	{
	  int start = M->triplet.Ap[i],j;
	  SpCol *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      D->J[j]= M->triplet.Ai[start+j];
	      D->R[j]= M->triplet.Ax[start+j];
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < M->n ; i++)
	{
	  int start = M->triplet.Ap[i],j;
	  SpCol *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      D->J[j]= M->triplet.Ai[start+j];
	      D->C[j].r= M->triplet.Ax[start+j];
	      D->C[j].i= M->triplet.Ax[start+j+M->triplet.Aisize];
	    }
	}
    }
  nsp_spcol_free_triplet(M);
  return OK;
}

/* from internal coding to the Ap,Ai,Az coding using int and double.
 * here the triplet has been allocated !
 */

static int nsp_spcol_fill_zi_triplet(const NspSpColMatrix *M)
{
  int i, *loc;
  /* fill the array Ap */
  if ( M->convert != 't' ) return FAIL;
  loc = M->triplet.Ap; loc[0]=0;
  for ( i = 1 ; i <= M->n ; i++)
    {
      loc[i]= loc[i-1] + M->D[i-1]->size;
    }
  /* fills the array Ai and Ax */
  if ( M->rc_type == 'r')
    {
      for ( i = 0 ; i < M->n ; i++)
	{
	  int start = M->triplet.Ap[i],j;
	  SpCol *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      M->triplet.Ai[start+j]= D->J[j];
	      M->triplet.Ax[start+j]= D->R[j];
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < M->n ; i++)
	{
	  int start = M->triplet.Ap[i],j;
	  SpCol *D = M->D[i];
	  for (j= 0 ; j < D->size ; j++) 
	    {
	      M->triplet.Ai[start+j]=  D->J[j];
	      M->triplet.Ax[start+j]=  D->C[j].r;
	      M->triplet.Ax[start+j+M->triplet.Aisize]= D->C[j].i;
	    }
	}
    }
  return OK;
}

static void nsp_spcol_free_triplet(NspSpColMatrix *M)
{
  M->convert = 'v';
  FREE(M->triplet.Ap);
  FREE(M->triplet.Ai);
  FREE(M->triplet.Ax);
}

