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
  
#include "nsp/object.h"

static int nsp_sparse_fill_zi_triplet(NspSpMatrix *M);
static void nsp_sparse_free_triplet(NspSpMatrix *M);
static int nsp_sparse_set_triplet_from_m_internal(NspSpMatrix *M,int flag);
static int nsp_sparse_update_from_triplet_internal(NspSpMatrix *M);
static int nsp_sparse_alloc_triplet(NspSpMatrix *M,int nzmax);

/* tricky version to get a column triplet */

int nsp_sparse_set_triplet_from_m(NspSpMatrix *M,int flag)
{
  NspSpMatrix *Mt = nsp_spmatrix_transpose(M);
  if ( Mt == NULL) return FAIL;
  if ( nsp_sparse_set_triplet_from_m_internal(Mt,flag) == OK) 
    {
      M->convert = 't';
      M->triplet = Mt->triplet;
      Mt->convert = 'v';
    }
  nsp_spmatrix_destroy(Mt);
  return OK;
}

/* tricky version to update from  a column triplet */

int nsp_sparse_update_from_triplet(NspSpMatrix *M)
{
  int i,nn;
  NspSpMatrix *Mt;
  /* partial free */
  for ( i = 0  ; i < M->m ; i++) 
    {
      SpRowDestroy(M->D[i]);
      FREE(M->D[i]);
    }
  FREE(M->D);
  M->m= M->n = 0;
  /* switch triplet */
  nn =   M->triplet.m ;
  M->triplet.m = M->triplet.n;
  M->triplet.n = nn;
  if ( nsp_sparse_update_from_triplet_internal(M) == FAIL) return FAIL;
  /* now M is filled with its traspose */
  Mt = nsp_spmatrix_transpose(M);
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
  nsp_spmatrix_destroy(Mt);
  return OK;
}

/* needed in mexlib until representation is switched */

int nsp_sparse_alloc_col_triplet(NspSpMatrix *M,int nzmax)
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

int nsp_sparse_realloc_col_triplet(NspSpMatrix *M,int nzmax)
{
  int cp = (M->rc_type=='c' ? 2: 1);
  if ( M->convert != 't' ) 
    {
      return nsp_sparse_alloc_col_triplet(M,nzmax);
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


static int nsp_sparse_set_triplet_from_m_internal(NspSpMatrix *M,int flag)
{
  int nnz = nsp_spmatrix_nnz(M);
  if ( M->convert == 't' ) return OK;
  if ( nsp_sparse_alloc_triplet(M,nnz) == FAIL) return FAIL;
  if ( flag == TRUE )
    {
      if ( nsp_sparse_fill_zi_triplet(M)== FAIL) 
	{
	  nsp_sparse_free_triplet(M);
	  return FAIL;
	}
    }
  return OK;
}


static int nsp_sparse_update_from_triplet_internal(NspSpMatrix *M)
{
  int i;
  /* use triplet to back change M */
  if ( M->convert != 't' ) return OK;
  if ( M->m < M->triplet.m ) 
    {
      if ( nsp_spmatrix_enlarge_rows(M,M->triplet.m-M->m)== FAIL) return FAIL;
    }
  else if ( M->triplet.m < M->m ) 
    {
      /* delete extra lines */
      for ( i = M->triplet.m ; i < M->m ; i++)
	{
	  SpRowDestroy(M->D[i]);
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
      if (nsp_spmatrix_resize_row(M,i,row_size) == FAIL) return FAIL;
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
	      D->C[j].r= M->triplet.Ax[2*(start+j)];
	      D->C[j].i= M->triplet.Ax[2*(start+j)+1];
	    }
	}
    }
  nsp_sparse_free_triplet(M);
  return OK;
}


/* from internal coding to the Ap,Ai,Az coding using int and double.
 * Note that the matrix M is row coded and thus the triplet is also 
 * row coded 
 * here the triplet has been allocated !
 */

static int nsp_sparse_fill_zi_triplet(NspSpMatrix *M)
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
	      M->triplet.Ax[2*(start+j)]= D->C[j].r;
	      M->triplet.Ax[2*(start+j)+1]= D->C[j].i;
	    }
	}
    }
  return OK;
}

static void nsp_sparse_free_triplet(NspSpMatrix *M)
{
  M->convert = 'v';
  FREE(M->triplet.Ap);
  FREE(M->triplet.Ai);
  FREE(M->triplet.Ax);
}

static int nsp_sparse_alloc_triplet(NspSpMatrix *M,int nzmax)
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

