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

static void nsp_spcolmatrix_print_internal(nsp_num_formats *fmt,NspSpColMatrix *m, int indent);
/* In file Perm.c **/

extern int C2F(dperm) (double A[],int ind[],int *nv);
extern int C2F(zperm) (doubleC A[],int ind[],int *nv);

typedef void (*BopLeft) (SpCol *,char,int *,SpCol *,char,int);
typedef void (*BopBoth) (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
typedef void (*BopRight) (SpCol *,char,int *,SpCol *,char,int);

static NspSpColMatrix *BinaryOp (NspSpColMatrix *,NspSpColMatrix *,BopLeft,BopBoth,
			      BopRight);

static void PlusLeft (SpCol *,char,int *,SpCol *,char,int);
static void PlusBoth (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
static void PlusRight (SpCol *,char,int *,SpCol *,char,int);

static void MinusLeft (SpCol *,char,int *,SpCol *,char,int);
static void MinusBoth (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
static void MinusRight (SpCol *,char,int *,SpCol *,char,int);

static void MultttLeft (SpCol *,char,int *,SpCol *,char,int);
static void MultttBoth (SpCol *,char,int *,SpCol *,char,int,SpCol *,char,int);
static void MultttRight (SpCol *,char,int *,SpCol *,char,int);

static int nsp_dichotomic_search(int x,const int val[],int imin,int imax);

static int nsp_bi_dichotomic_search(const double x[],int xpmin,int xpmax,const int val[],int imin,int imax,
				    NspMatrix *Work,NspMatrix *Index,int count);

/* XXXXXXXXXXXXXXX OK 
 * Creates a Sp Matrix of size mxn with no stored date
 * Attention on peut creer une nx0 matrice XXXXX
 */

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
  Sp->mn = Sp->m*Sp->n;
  Sp->rc_type=type;
  Sp->convert = 'n';
  Sp->triplet.Ai=NULL;
  Sp->triplet.Ap=NULL;
  Sp->triplet.Ax=NULL;
  if ( Sp->mn == 0 ) 
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


/*
 * Creation of a Sparse Matrix with specified data
 * Scilab function sparse(rowcols,vals,[m,n])
 * if m and n  have -1 value, sizes are to be computed from 
 * rowscols (RC)
 * Take care that RC is changed 
 * XXXXXXXXXXXXXXX OK
 */

NspSpColMatrix *nsp_spcolmatrix_sparse(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n)
{
  double *rows;
  NspSpColMatrix *Loc;
  NspMatrix *Index;
  int imax,i,maxrow;
  /* max col indice **/
  imax = 0;
  rows = RC->R+RC->m;
  /* swap cols and row in RC for lexical ordering */
  for ( i = 0 ; i < RC->m; i++ ) 
    {
      int icol = (int) rows[i];
      rows[i] = RC->R[i];
      RC->R[i] = icol;
    }
  /* now cols are in RC->R and rows in rows */
  for ( i = 0 ; i < RC->m; i++ ) 
    {
      int icol= (int) RC->R[i];
      if ( icol <= 0 ) 
	{
	  Scierror("Error:\t negative indice in sparse fisrt argument i=%d ind=%d\n",
		   i,icol);
	  return NULLSPCOL;
	}
      if ( icol > imax) imax = icol;
    }
  if ( n != -1 &&  n < imax ) 
    {
      Scierror("Error:\t some given indices for cols are > n=%d\n",n);
      return NULLSPCOL;
    }
  /* sort RC lexical row increasing */
  Index = nsp_mat_sort (RC,2,"lr","i");
  if ( n == -1 ) n = imax;
  /* allocate space for Loc with proper col size **/
  if ((Loc =nsp_spcolmatrix_create(name,Values->rc_type,1,n))== NULLSPCOL ) return NULLSPCOL;
  /* Counting non nul arguments of each column and store it in Loc **/
  for ( i = 0 ; i < Loc->n ; i++) Loc->D[i]->iw=0;
  switch ( Values->rc_type )
    {
    case 'r' :
      for ( i = 0 ; i < RC->m ; i++)  
	{
	  if (Values->R[i] != 0.0 ) Loc->D[((int) RC->R[i])-1]->iw++;
	} 
      break;
    case 'c' : 
      for ( i = 0 ; i < RC->m ; i++)  
	{
	  if (Values->C[i].r != 0.0 || Values->C[i].i ) Loc->D[((int) RC->R[i])-1]->iw++;
	} 
      break;
    }
  /* Resizing each row */
  for ( i = 0 ; i < Loc->n ; i++) 
    {
      if (nsp_spcolmatrix_resize_col(Loc,i,Loc->D[i]->iw) == FAIL) return(NULLSPCOL);
    }
  /* Check columns **/
  maxrow =0;
  for ( i = 0 ; i < RC->m ; i++) 
    {
      if ( ((int) rows[i]) > maxrow ) maxrow =((int) rows[i]);
    }
  
  if ( m != -1 && m < maxrow ) 
    {
      Scierror("Error:\t some given indices for rows (%d) are > m=%d\n",maxrow,m);
      return NULLSPCOL;
    }
  /* fix column size and total size  **/
  if ( m == -1 ) m = maxrow;
  Loc->m = m;
  Loc->mn = n*m;
  /* fill each col with Values 
   */
  for ( i = 0 ; i < Loc->n ; i++) Loc->D[i]->iw=0;
  for ( i = 0 ; i < RC->m ; i++ ) 
    {
      int iloc=((int) RC->R[i])-1;
      int count = Loc->D[iloc]->iw;
      int id =((int) Index->R[i])-1;
      switch ( Values->rc_type )
	{
	case 'r' : 
	  if ( Values->R[id] != 0.0 ) 
	    {
	      Loc->D[iloc]->R[count] = Values->R[id];
	      Loc->D[iloc]->J[count] = ((int) rows[i])-1;
	      if ( Loc->D[iloc]->J[count] >= 0 && Loc->D[iloc]->J[count] < Loc->m )
		{ 
		  if ( count != 0 && ( Loc->D[iloc]->J[count] == Loc->D[iloc]->J[count-1])) 
		    {
		      /* Sciprintf("Warning (%d,%d) is duplicated \n",iloc+1,Loc->D[iloc]->J[count]+1);*/
		      Loc->D[iloc]->R[count-1]+=  Loc->D[iloc]->R[count];
		    }
		  else
		    Loc->D[iloc]->iw++;
		}
	      else
		{
		  Scierror("Warning (%d,%d) is out of range, ignored\n",Loc->D[iloc]->J[count]+1,iloc+1);
		  return NULLSPCOL;
		}
	    }
	  break;
	case 'c' : 
	  if ( Values->C[i].r != 0.0 || Values->C[id].i != 0.0) 
	    {
	      Loc->D[iloc]->C[count] = Values->C[id];
	      Loc->D[iloc]->J[count] =((int) rows[i])-1;
	      if ( Loc->D[iloc]->J[count] >= 0 && Loc->D[iloc]->J[count] < Loc->m )
		{ 
		  if ( count != 0 && ( Loc->D[iloc]->J[count] == Loc->D[iloc]->J[count-1])) 
		    {
		      /* Sciprintf("Warning (%d,%d) is duplicated \n",iloc+1,Loc->D[iloc]->J[count]+1);*/
		      Loc->D[iloc]->C[count-1].r +=  Loc->D[iloc]->C[count].r;
		      Loc->D[iloc]->C[count-1].i +=  Loc->D[iloc]->C[count].i;
		    }
		  else 
		    Loc->D[iloc]->iw++;
		}
	      else
		{
		  Scierror("Warning (%d,%d) is out of range, ignored\n",Loc->D[iloc]->J[count]+1,iloc+1);
		  return NULLSPCOL;
		}
	    }
	  break;
	}
    }
  /* Resizing each col (due to duplicate values) */
  for ( i = 0 ; i < Loc->n ; i++) 
    {
      if ( Loc->D[i]->iw != Loc->D[i]->size ) 
	{
	  if (nsp_spcolmatrix_resize_col(Loc,i,Loc->D[i]->iw) == FAIL) return(NULLSPCOL);
	}
    }
  /* no need to sort here */
  /*   if ( (xb =nsp_alloc_int(Loc->n)) == (int*) 0) return(NULLSPCOL); */
  /*   for (i = 0 ; i < Loc->m ; ++i)  */
  /*     { */
  /*       if (Loc->D[i]->size > 1)  */
  /* 	{ */
  /* 	  int c__1 =1; */
  /*      nsp_qsort_int(Loc->D[i]->J,xb,TRUE,Loc->D[i]->size,'i');*/
  /* 	  C2F(gsort)(Loc->D[i]->J,NULL,xb,&c__1,&c__1,&Loc->D[i]->size,"i","i"); */
  /* 	  if ( Loc->rc_type == 'r' )  */
  /* 	    C2F(dperm)(Loc->D[i]->R,&Loc->D[i]->size,xb); */
  /* 	  else  */
  /* 	    C2F(zperm)(Loc->D[i]->C,&Loc->D[i]->size,xb); */
  /* 	} */
  /*     } */
  /*   FREE(xb); */
  nsp_matrix_destroy(Index);
  return Loc;
}


/*
 * Scilab function [rc,vals,mn]= spget(sp)
 * XXXXXXXXXXX: OK updated 
 *
 */

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

/*
 * Res =nsp_spcolmatrix_copy(A) 
 * Copy a sparse matrix in Res : res is created 
 * XXXXXXXXXXX: OK updated 
 */

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

/* XXXXXXXXXXX: OK updated : Note that name is wrong since it resize a col
 * Resize the ith Row 
 * if Row->size = 0 the Row is created with n potential elements 
 * if Row->size != 0 the Row is resized ( enlarged or reduced ) 
 * according to n.
 * i : number of the Row which is to be resized 
 * WARNING : i must be in [0,Sp->m[ and this is not checked here
 * WARNING : Sp->n is not changed this is to be done elsewhere.
 */

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

/*
 * Destroy a Sp Matrix 
 * XXXXXXXXXXX: OK updated 
 */

void nsp_spcolmatrix_col_destroy(SpCol *Row)
{
  if ( Row->size != 0 ) 
    {
      FREE( Row->J);
      FREE( Row->R);
    }
}

/* XXXXXXXXXXX: OK updated  */

void nsp_spcolmatrix_destroy(NspSpColMatrix *Mat)
{
  int i;
  if ( Mat != NULLSPCOL )
    {
      nsp_object_destroy_name(NSP_OBJECT(Mat));
      for ( i = 0  ; i < Mat->n ; i++) 
	{
	nsp_spcolmatrix_col_destroy(Mat->D[i]);
	  FREE(Mat->D[i]);
	}
      FREE(Mat->D);
      FREE(Mat) ;
    }
}

/*
 *  nsp_spcolmatrix_nnz: computes the number of non nul elements
 *  (Number of Non Zero elements) of a  Sparse Matrix
 *  (added by Bruno)
 * XXXXXXXXXXX: OK updated 
 */

int nsp_spcolmatrix_nnz(const NspSpColMatrix *HMat)
{
  int i, nnz=0;
  for ( i = 0 ; i < HMat->n ; i++ )
    nnz += HMat->D[i]->size;
  return nnz;
}

/*
 *nsp_spcolmatrix_info: display Info on Sparse Matrix
 */

void nsp_spcolmatrix_info(NspSpColMatrix *Sp, int indent,char *name,int rec_level)
{ 
  int i;
  if ( Sp == NULLSPCOL) 
    {
      Sciprintf("Null SpMatrix pointer\n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( strcmp(NSP_OBJECT(Sp)->name,NVOID) == 0)
    Sciprintf("SpMatrix %c (%dx%d)\n",Sp->rc_type, Sp->m,Sp->n);
  else
    Sciprintf("Spmatrix %s %c (%dx%d)\n",NSP_OBJECT(Sp)->name,Sp->rc_type, Sp->m,Sp->n);
}


/*
 *	 Writes Sp, Sp remains unchanged 
 */

void nsp_spcolmatrix_print(NspSpColMatrix *Sp, int indent,char *name, int rec_level)
{ 
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Sp)->name; 
  if (user_pref.pr_as_read_syntax)
    {
      const int name_len=128;
      char epname[name_len];
      NspMatrix *RC,*Values;
      if ( nsp_spcolmatrix_get(Sp,&RC,&Values)== FAIL)
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
      Sciprintf1(indent,"%s=sparse(%s__rc,%s__val,[%d,%d]);\n",pname,pname,pname,Sp->m,Sp->n);
      Sciprintf1(indent,"clear('%s__rc','%s__val')\n",pname,pname);
    }
  else
    {
      if (Sp->mn==0 ) 
	{
	  Sciprintf1(indent,"%s\t= []\t\tspcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	}
      else
	{
	  nsp_num_formats fmt;
	  if ( user_pref.pr_depth  <= rec_level -1 ) 
	    {
	      Sciprintf1(indent,"%s\t= [...]\t\tspcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	      return;
	    }
	  nsp_init_pr_format (&fmt);
	  Sciprintf1(indent,"%s\t=\t\tspcol %c (%dx%d)\n",pname,Sp->rc_type,Sp->m,Sp->n);
	  nsp_spcolmatrix_print_internal(&fmt,Sp,indent+1);
	}
    }
}

/*
 *nsp_spcolmatrix_redim: Changes matrix dimensions to mxn 
 *   the product m*n must be unchanged and 
 *   the stored data is kept considering that data are stored 
 *   columnwise. 
 *   a New Matrix is returned or NULLSPCOL on failure
 * XXXXXXXXXXXX ok 
 * 
 */

NspSpColMatrix *nsp_spcolmatrix_redim(NspSpColMatrix *A, int m, int n)
{
  int *xb;
  int i,k;
  NspSpColMatrix *Loc;
  if ( A->mn !=  m*n ) 
    {
      Scierror("Error:\tCannot change size to (%dx%d) since matrix has %d elements\n",m,n,A->mn);
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
 * Add m empty Cols ( Row->size = 0) to a Sparse matrix 
 * if n < Sp->n then Sp is unchanged. 
 * XXXXXXXXXXX: OK updated Note that it enlarges the cols 
 */

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
  return(OK);
}


/*
 *nsp_spcolmatrix_enlarge(A,m,n)
 * changes A to B= [ A , 0; 0,0 ]  
 * in such a way that B (max(A->m,m)xmax(A->n,n));
 * The result is stored in A
 * if m and n are smaller than A->m and A->n Matrix 
 * associated dimensions are not changed 
 * XXXXXXXXXXX: OK updated 
 */

int nsp_spcolmatrix_enlarge(NspSpColMatrix *A, int m, int n)
{
  /* special case **/
  if ( m > A->m  ) A->m = m ; /* easy for sparse matrix **/
  if ( n > A->n  ) 
    return nsp_spcolmatrix_enlarge_cols(A,n);
  return OK;
}

/*
 * A = [A, B] 
 * Right concatenation on A, A is changed 
 * XXXXXXX Ok;
 */

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

/*
 *  A=[A; B ] 
 *   Down concatenation on A 
 *  XXXXX OK
 */

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
  return(OK);
}


/*
 * Diag Concatenation 
 * A = [A,0;0,B] 
 * A is changed  B is left unchanged 
 * XXXXXXXX OK 
 */

int nsp_spcolmatrix_concatdiag(NspSpColMatrix *A, NspSpColMatrix *B)
{
  int i,j;
  int Am = A->m;
  int An = A->n;
  /* first [A,B] */
  int n1 = Max(A->m,B->m);
  A->m = n1;
  B->m = n1;
  if (nsp_spcolmatrix_concatr( A,B) == FAIL) return FAIL;
  /* push last cols **/
  for ( i = An ; i < A->n ; i++) 
    { 
      SpCol *Ai = A->D[i];
      for ( j = 0 ; j < Ai->size ; j++) 
	Ai->J[j] += Am;
    }
  /* restore proper row dimensions **/
  A->m = Am + B->m ;
  return(OK);
}


/*
 * Utility functions 
 * nsp_spcolmatrix_insert_elt(A,i,j,B,rb,cb)
 * nsp_spcolmatrix_delete_elt(A,row,col,amin,amax)
 * nsp_spcolmatrix_get_elt(B,i,j)
 * nsp_spcolmatrix_store(A,r,c,col,B,r1,c1)
 * XXXXXXXXXX OK
 * Attention toutefois aux notations trompeuses 
 * r et c sont a lire col et row !!!
 */

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

/* Insert or change A(i,j) to B(rb,cb) **/

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
      /* we must change element **/
      nsp_spcolmatrix_store(A,i,ok,j,B,rb,cb);
    }
  return OK;
}

/* 
 *  Remove element A(i,j) but the associated row is not resized 
 *  returns -1 if A(i,j) was zero before the call (i.e no change )
 *  return  k  if A(i,j) is removed and was the k-th non null element 
 *  of i-th row of A 
 *  A(i,j) is searched in the i-th row of A but only in the range 
 *  amin,amax 
 *  row = i, col=j 
 */

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

/* return k such that B->D[i]->J[k] = j or -1 if such k does not exists **/

int nsp_spcolmatrix_get_elt(NspSpColMatrix *B, int i, int j)
{
  SpCol *Bi = B->D[i];
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
 *  XXXXX : Rows doit-il etre croissant ?
 *  XXXXXXXXXXX  OK
 */

int nsp_spcolmatrix_set_rowcol(NspSpColMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspSpColMatrix *B)
{
  int Bnonnul = FALSE;
  char type ='r';
  int rmin,rmax,cmin,cmax,i,k,l,acol;
  /* Check compatibility : B is a scalar or B must have compatible 
   * size with Rows and Cols : Note that B=[] is treated elsewhere 
   */
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
  /* Enlarge A if necessary */
  if ( rmax > A->m ||  cmax > A->n ) 
    if (nsp_spcolmatrix_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  /* Id result complex ? */
  if ( B->rc_type == 'c' &&   A->rc_type == 'r' )
    { 
      type = 'c';
      if (nsp_spcolmatrix_seti(A,0.00) == FAIL ) return(FAIL);
    }
  /* fill A */
  for ( i = 0 ; i < Cols->mn ; i++ ) 
    {
      /* The col of A to be changed **/
      int col = ((int) Cols->R[i])-1;
      int Ais = A->D[col]->size ;
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
      /* The new col will have at most nel + A->D[col]->size non nul elements **/
      if (nsp_spcolmatrix_resize_col(A,col ,nel+Ais) == FAIL) return FAIL;
      for ( k =0 ; k < Rows->mn ; k++ )
	{
	  int ok = -1;
	  int row = ( (int) Rows->R[k])-1;
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
      if (nsp_spcolmatrix_resize_col(A,col ,amax) == FAIL) return FAIL;
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
 * XXXXXXXXXXXX OK
 */

int nsp_spcolmatrix_set_row(NspSpColMatrix *A, NspMatrix *Inds, NspSpColMatrix *B)
{
  int i;
  int Bscal=0;
  /* Calling a genric function which check arguments **/
  if (GenericMatSeRo(A,A->m,A->n,A->mn,Inds,
		     B,B->m,B->n,B->mn,(F_Enlarge)nsp_spcolmatrix_enlarge,&Bscal)== FAIL) 
    return FAIL;
  /* */
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_spcolmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if ( Bscal == 0 ) 
    {
      for ( i = 0  ; i < Inds->mn ; i++) 
	{
	  int rb,cb,kb,ia,ra,ca ;
	  rb= i % Inds->m;
	  cb= (i - rb )/Inds->m;
	  kb =nsp_spcolmatrix_get_elt(B,cb,rb);
	  ia = ((int) Inds->R[i])-1;
	  ra = ia % A->m;
	  ca= (ia - ra )/A->m;
	  if ( kb == -1 ) 
	    {
	      /* we must set the [ra,ca] element of A to 0.0 **/
	      int ok1 =nsp_spcolmatrix_delete_elt(A,ca,ra,0,A->D[ca]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_spcolmatrix_resize_col(A,ca,A->D[ca]->size-1) == FAIL) return FAIL;
		}
	    }
	  else
	    {
	      /* must change or insert element in A **/
	      if (nsp_spcolmatrix_insert_elt(A,ca,ra,B,cb,kb)== FAIL) return FAIL;
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
	      ok1 =nsp_spcolmatrix_delete_elt(A,ca,ra,0,A->D[ca]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_spcolmatrix_resize_col(A,ca,A->D[ca]->size-1) == FAIL) return FAIL;
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
	      if (nsp_spcolmatrix_insert_elt(A,ca,ra,B,0,0)== FAIL) return FAIL;
	    }
	}
    }
  return(OK);
}

/*
 *  A(Rows,:) = [],  A is changed. 
 *  XXXXXXXXXXXXX OK.
 */

int nsp_spcolmatrix_delete_rows(NspSpColMatrix *A, NspMatrix *Rows)
{
  NspMatrix *Row;
  int cmin,cmax,i,j,k,count_deleted = 0;
  if ( Rows->mn == 0) return(OK);
  Bounds(Rows,&cmin,&cmax);
  if ( cmin < 1 || cmax > A->m ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      return(FAIL);
    }
  /* working array */
  if ((Row = nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT ) return FAIL;
  for ( i= 0 ; i < Row->mn ; i++) Row->I[i]=0;
  /* count deleted rows */
  for ( i= 0 ; i < Rows->mn ; i++)
    {
      int row = ((int) Rows->R[i]) -1;
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
	  return FAIL;
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
  return(OK);
}

/* compress ith col of sparse Matrix A 
 * [1,2,-1,4,5,6,-1,11]--> [1,2,4,5,6,11] 
 * return the number of deleted elements 
 * XXXXXXXXXXXXX OK
 */

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

/* do not changes values just move values in arrays */

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


/*
 *  A(:,Cols) = []
 *  A is changed. 
 *  Cols is not supposed to be increasing
 *  XXXXXXXX OK 
 */

int nsp_spcolmatrix_delete_cols(NspSpColMatrix *A, NspMatrix *Cols)
{
  int rmin,rmax,i,ind,free_pos,count_deleted=0;
  Bounds(Cols,&rmin,&rmax);
  if ( Cols->mn == 0) return(OK);
  if ( rmin < 1 || rmax > A->n ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      return(FAIL);
    }
  for ( i= 0 ; i < A->n ; i++) 
    A->D[i]->iw= 0;
  /* resize to zero the column which are to be deleted 
   * and mark them 
   */
  for ( i = 0 ; i < Cols->mn  ; i++)
    {
      ind =  ((int) Cols->R[i])-1; /* column to be deleted */
      if (nsp_spcolmatrix_resize_col(A,ind,0) == FAIL) return FAIL; /* free_pos the associated data */
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
  A->D = REALLOC(A->D, A->n*sizeof( SpCol *));
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
 * flag == 0 when cols is NULL XXX to be changed with a NULL check 
 * XXXXXXXXXX OK 
 * XXXXXXXX Pourquoi Rows doit il etre trié ? 
 */	

static NspSpColMatrix *SpExtract_G(NspSpColMatrix *A, NspMatrix *Rows, NspMatrix *Cols, int flag, int *err)
{
  NspMatrix *Work= NULL, *Index = NULL;
  NspSpColMatrix *Loc;
  int rmin,rmax,cmin,cmax,i,j,Cn;
  if ( A->mn == 0) return nsp_spcolmatrix_create(NVOID,A->rc_type,0,0);
  if (flag == 1) 
    {
      Bounds(Cols,&cmin,&cmax);
      if ( cmin < 1 ||  cmax > A->n  ) 
	{
	  *err=1;
	  Scierror("Error:\tColumn indices are out of bound\n");
	  return(NULLSPCOL);
	}
    }
  Index = nsp_mat_sort (Rows,2,"g","i");
  rmin = Rows->R[0]; rmax = Rows->R[Rows->mn-1];
  *err=0;
  if (  rmin < 1 ||  rmax > A->n ) 
    {
      *err=1;
      return(NULLSPCOL);
    }
  Cn= (flag == 1) ? Cols->mn : A->n;
  if ( (Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,Rows->mn,Cn))== NULLSPCOL) 
    return(NULLSPCOL);
  /* used to store elements */
  if ( ( Work = nsp_matrix_create(NVOID,'r',2,Rows->mn)) == NULLMAT) return NULLSPCOL;
  
  for ( i = 0 ; i < Loc->n ; i++)
    {
      int count;
      int imin,imax,k;
      SpCol *Ai, *Li;
      int col;
      col = (flag == 1) ? ((int) Cols->R[i])-1 : i ;
      Ai= A->D[col];
      Li= Loc->D[i];
      Li->iw=0;
      if ( Ai->size == 0) continue; /* nothing to do row is empty */
      imin=0; imax= Ai->size-1 ; k = -1;
      Li->iw=0;
      count = nsp_bi_dichotomic_search(Rows->R,0,Rows->mn-1,Ai->J,imin,imax,Work,Index,0);
      /* now we know the column size */
      if (nsp_spcolmatrix_resize_col(Loc,i,count)==FAIL) return NULLSPCOL;
      /* Fills the rows of i-th column */
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


NspSpColMatrix *nsp_spcolmatrix_extract(NspSpColMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  NspSpColMatrix *Sp;
  int err;
  Sp=SpExtract_G(A,Rows,Cols,1,&err);
  if (err==1 ) Scierror("Error:\tIndices out of bound\n");
  return Sp;
}

/*
 * Res=nsp_spcolmatrix_extract_elements(A,Elts)
 * A unchanged, Elts
 * XXXXXXXXXX OK 
 */

NspSpColMatrix *nsp_spcolmatrix_extract_elts(NspSpColMatrix *A, NspMatrix *Elts)
{
  NspSpColMatrix *Loc;
  int rmin,rmax,i,err,k;
  Bounds(Elts,&rmin,&rmax);
  if ( A->mn == 0) return nsp_spcolmatrix_create(NVOID,A->rc_type,0,0);
  if ( rmin < 1 || rmax > A->mn )
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLSPCOL);
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      NspMatrix *Rows ; 
      if ((Rows = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT ) return NULLSPCOL;
      Rows->R[0] = 1;
      /* like A(1,Elts) */
      Loc=  SpExtract_G(A,Rows,Elts,1,&err);
      if (err==1 ) Scierror("Error:\tIndices out of bound\n");
      return Loc;
    }
  else
    {
      if ( (Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,Elts->mn,1))== NULLSPCOL) 
	return NULLSPCOL;
      A->D[0]->iw=0;
      for ( i=0; i < Elts->mn ; i++) 
	{
	  int el = ((int)Elts->R[i]) -1;
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
	      if (nsp_spcolmatrix_resize_col(Loc,0,A->D[0]->iw)==FAIL) return NULLSPCOL;
	      Loc->D[0]->J[j]=i;
	      switch ( A->rc_type ) 
		{
		case 'r' : Loc->D[0]->R[j] =  A->D[col]->R[k]; break;
		case 'c' : Loc->D[0]->C[j] =  A->D[col]->C[k]; break;
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
 * XXXXXXXXXXX OK
 */	

NspSpColMatrix *nsp_spcolmatrix_extract_rows(NspSpColMatrix *A, NspMatrix *Rows, int *err)
{
  return SpExtract_G(A,Rows,NULLMAT,0,err);
}

/*
 * A1=MatLoopCol(A1,M,i,rep)
 * Used in for loops 
 */	


/*
 * Res=nsp_matrix_extract_cols(A,Cols,err)
 * A unchanged
 * XXXXXXXXX OK
 */	

NspSpColMatrix *nsp_spcolmatrix_extract_cols(NspSpColMatrix *A, NspMatrix *Cols, int *err)
{
  NspSpColMatrix *Loc;
  int rmin,rmax,i,j;
  if ( A->mn == 0) return nsp_spcolmatrix_create(NVOID,A->rc_type,0,0);
  Bounds(Cols,&rmin,&rmax);
  *err=0;
  if ( rmin < 1 ||  rmax > A->n  ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLSPCOL);
    }
  if ( (Loc =nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,Cols->mn))== NULLSPCOL) 
    return(NULLSPCOL);
  for ( i = 0 ; i < Loc->n ; i++)
    {
      int col=((int) Cols->R[i])-1; 
      SpCol *Ai= A->D[col];
      SpCol *Li= Loc->D[i];
      if (nsp_spcolmatrix_resize_col(Loc,i,Ai->size)==FAIL) return NULLSPCOL; 
      /* XXXX : faire des memcpy */
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
 * XXXXXXXXXXXX OK 
 */

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

/*
 * Set the kth Diag of A to Diag 
 * A is enlarged & comlexified if necessary 
 * int nsp_spcolmatrix_diag_create(A,Diag,k)
 * Diag is a sparse nxm matrix 
 */

static int  GetDiagVal (NspSpColMatrix *Diag,int i,double *val,doubleC *cval);

int nsp_spcolmatrix_diag_set(NspSpColMatrix *A, NspSpColMatrix *Diag, int k)
{
  int i,l;
  int rmin,cmin,rmax,cmax,itmax;
  /*ZZZ */
  rmin = Max(0,-k);
  cmin = Max(0,k);
  rmax = Diag->mn + Max(0,-k);
  cmax = Diag->mn + Max(0,k);
  itmax = Min(A->m-rmin,A->n -cmin );
  if ( itmax != Diag->mn ) 
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

/*
 * return the ith element of Diag in val or cval + an OK value 
 * if this element is non nul. 
 * XXXXXXXXXXXX OK 
 */

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

/*
 *  Creates a Matrix with kth diag set to Diag 
 */

NspSpColMatrix *nsp_spcolmatrix_diag_create(NspSpColMatrix *Diag, int k)
{
  int i,k1;
  int rmin,cmin,rmax,cmax,n;
  NspSpColMatrix *Loc;
  rmin = Max(0,-k);
  cmin = Max(0,k);
  rmax = Diag->mn + Max(0,-k);
  cmax = Diag->mn + Max(0,k);
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


/*
 * Multiplication of Two Sparse Matrices 
 * ------------------------
 *  multiply sparse matrices by the method of gustafson,acm t.o.m.s. 
 *  vol 4 (1978) p250. 
 *  C coded version : Chancelier 1996
 *  some modifs by Bruno Pincon 2005 to improve efficiency
 */

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
  for (v = 0 ; v < B->n ; v++ )  xb[v] = -1;

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

/* m= sp*m multiplication 
 * XXX change the name 
 * 
 */

NspMatrix *nsp_spcolmatrix_mult_matrix(NspSpColMatrix *A, NspMatrix *B)
{
  NspMatrix *C = NULLMAT;
  int i, j, k, jp, kp, neli,size;
  const int inc=1;
  char type = 'r';
  double zero=0.0;
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
  if ( A->n != B->m ) 
    {
      Scierror("SpMult : incompatible arguments\n");
      return NULLMAT;
    }

  if ( (C =nsp_matrix_create(NVOID,type,A->m,B->n)) == NULLMAT ) return NULLMAT;
  /* initialize to 0.0 */
  size=(type == 'c') ? 2*C->mn : C->mn;
  nsp_dset(&size,&zero,C->R,&inc);
  /* process the columns of B */
  for (i = 0 ; i < B->n ; i++) 
    {
      neli = 0;  /* to count the number of a priori non nul elements of column i of C */
      double *BRi = &B->R[i*B->m];
      double *CRi = &C->R[i*C->m];
      doubleC *BCi = &B->C[i*B->m];
      doubleC *CCi = &C->C[i*C->m];
      /*  process column i of B */
      for (jp = 0  ; jp <  B->m  ; jp++) 
	{
	  SpCol *Aj;
	  /* j is the current row-index for B i.e B(j,i) */
	  j = jp;
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
		/* A(k,j)*B(j,i) */
		CRi[k] += Aj->R[kp] * BRi[jp];
	      }
	  else 
	    for (kp = 0 ; kp < Aj->size ; kp++) 
	      {
		/* getting non nul b(j,k) */
		k = Aj->J[kp];
		if ( A->rc_type == 'r') 
		  {
		    CCi[k].i += Aj->R[kp] * BCi[jp].i;
		    CCi[k].r += Aj->R[kp] * BCi[jp].r;
		  }
		else if ( B->rc_type == 'r' ) 
		  {
		    CCi[k].i += Aj->C[kp].i * BRi[jp];
		    CCi[k].r += Aj->C[kp].r * BRi[jp];
		  }
		else 
		  {
		    CCi[k].r += Aj->C[kp].r * BCi[jp].r -  Aj->C[kp].i* BCi[jp].i;
		    CCi[k].i += Aj->C[kp].i * BCi[jp].r +  Aj->C[kp].r* BCi[jp].i;
		  }
	      }
	}
    }
  return C;
}

/*
 * nsp_spcolmatrix_mult_matrix(X,A) when A is sparse and X full
 * result B is a full matrix. Special cases have been checked
 * in the interface. (added by Bruno)
 *
 */

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

/*
 * nsp_spcolmatrix_mult_scal(A,B) when B is a scalar sparse 
 * A is changed 
 * the fact that B is scalar is not checked 
 */

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




/*
 * Changes A to complex type and 
 * provide storage allocation 
 * XXXXXXXXXX OK 
 */

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

/*
 * Set real part of all non nul elements of A to d 
 * XXXXXXX OK
 */

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


/*
 * Set imag part of all non nul elements of A to d 
 * if A is real A is changed to complex 
 * XXXXXXXXXXXXX OK
 */

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



/*
 * Count the number of elements of A which are 
 * non zero 
 * A and val are unchanged 
 * XXXXXXXXXXXX OK
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
int countnonnull(NspMatrix *A)
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
 * XXXXXXX OK
 */

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


/*
 * Sparse to Full conversion 
 * mainly used for testing sparse 
 * XXXXXXX OK
 */

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

/*
 * Transpose a sparse Matrix, A is unchanged 
 * A new matrix is created 
 * We could add a routine for vectors with a transposition without copy 
 * Warning : For Complex matrices transpose is a' = transp+ conj
 * XXXXXXXXXX OK 
 */

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
      if (nsp_spcolmatrix_resize_col(Loc,i,(int) Loc->D[i]->iw) == FAIL) return(NULLSPCOL) ;
      Loc->D[i]->iw = 0 ; /* working storage reinit */
    } 
  /* filling the Tranposed matrix rows by rows  */
  for (  i = 0 ;  i < A->n  ;  i++) 
    {
      SpCol *Ai = A->D[i];
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

void PlusLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
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

void PlusBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
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


void PlusRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
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

void MinusLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
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

void MinusBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
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


void MinusRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
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

void MultttLeft(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1)
{
}

void MultttBoth(SpCol *Li, char Ltype, int *count, SpCol *Ai, char Atype, int k1, SpCol *Bi, char Btype, int k2)
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

void MultttRight(SpCol *Li, char Ltype, int *count, SpCol *Bi, char Btype, int k1)
{
}

/*
 *nsp_spcolmatrix_add,nsp_spcolmatrix_sub, SpMulttt
 * A+B A-B A.*B
 */

NspSpColMatrix *nsp_spcolmatrix_add(NspSpColMatrix *A, NspSpColMatrix *B)
{
  return(BinaryOp(A,B,PlusLeft,PlusBoth,PlusRight));
}

NspSpColMatrix *nsp_spcolmatrix_sub(NspSpColMatrix *A, NspSpColMatrix *B)
{
  return(BinaryOp(A,B,MinusLeft,MinusBoth,MinusRight));
}

NspSpColMatrix *nsp_spcolmatrix_multtt(NspSpColMatrix *A, NspSpColMatrix *B)
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
 * XXXXXXXXXXXXX OK
 */

static NspSpColMatrix *BinaryOp(NspSpColMatrix *A, NspSpColMatrix *B, BopLeft BinLeft, BopBoth BinBoth, BopRight BinRight)
{ 
  int i,count,k1,k2,k;
  NspSpColMatrix *Loc;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
      Loc =nsp_spcolmatrix_create(NVOID,type, A->m,A->n);
      if ( Loc == NULLSPCOL ) return(NULLSPCOL) ; 
      for ( i = 0 ; i < Loc->n ; i++ ) 
	{
	  int iest;
	  SpCol *Ai = A->D[i];
	  SpCol *Bi = B->D[i];
	  SpCol *Li = Loc->D[i];
	  iest= Min( A->n, A->D[i]->size+B->D[i]->size);
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

/*
 * A = A.*B where B is a scalar sparse ( [] or scalar )
 * XXXXXXXX OK
 */

int nsp_spcolmatrix_mult_scal(NspSpColMatrix *A, NspSpColMatrix *B)
{ 
  int i,k;
  if ( B->mn == 0 || B->D[0]->size == 0)
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
      A->m = A->n = A->mn = 0;
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

/*
 * A = A op B where B is a scalar sparse ( [] or scalar )
 * op can be + or - 
 * if B==[] or B==[0] 
 *     the result is A  and *flag == 1
 * else a full matrix is returned and *flag == 0 
 * 
 * op can be '+'(A+B) ,'-' (A-B), '#' (-A+B)
 * XXXXXXXXXXXXXX OK 
 */

NspMatrix *nsp_spcolmatrix_op_scal(NspSpColMatrix *A, NspSpColMatrix *B, int *flag, char op)
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
  return ( (NspSpColMatrix *) M)->rc_type;
}

/* XXXXXXXXXXX OK */

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

static void SpM_general(nsp_num_formats *fmt,NspSpColMatrix *Sp, int indent)
{
  int i,j;
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
	    }
	}
      break;
    }
}

static void nsp_spcolmatrix_print_internal(nsp_num_formats *fmt,NspSpColMatrix *m, int indent)
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

/* Operations XXX
 *
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/*
 * A = Matclean(a) clean A according to epsa and epsr 
 * epsa is used if rhs >= 1 
 * epsr is used if rhs >= 2
 * A is changed, 
 * XXXXXXXXXX OK 
 */

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
	    case 'c' : if (nsp_abs_c(&A->D[i]->C[j]) < eps) A->D[i]->J[j] = -1;n=1; break ;
	    }
	}
      /* remove null elements and resize rows **/
      if ( n != 0 ) 
	{
	  int ndel =nsp_spcolmatrix_compress_col(A,i);
	  if (nsp_spcolmatrix_resize_col(A,i, A->D[i]->size-ndel ) == FAIL) return(FAIL) ;
	}
    }
  return OK;
}

/*
 *  Res = Maxi(A,B) 
 *  term to term max  A(i;j) = Max(A(i,j),B(i,j)
 *  Res(i,j) = 1 or 2  
 *  A changed, B unchanged, 
 *  Res Created if flag == 1
 *  XXXXXXXX OK but untested to be added in table of functions 
 */

/*  minmaxflag = 1 for max -1 for min  */

NspMatrix *nsp_spcolmatrix_maximinitt_g(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int minmaxflag, int *err)
{
  /* Same philosophy as in BinaryOp **/
  int i,count,k1,k2,k;
  NspSpColMatrix *Loc;
  NspMatrix *Indi=NULL;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) 
	{
	  Scierror("Error: Arguments must be real matrices\n");
	  return NULLMAT;
	}
      /* storing indices **/
      if ( flag == 1) 
	{
	  if (( Indi = nsp_matrix_create(NVOID,'r',A->m,A->n)) == NULLMAT ) 
	    return NULLMAT;
	  nsp_mat_set_rval(Indi,1.0);
	}
      /* Buffer **/
      if ((Loc =nsp_spcolmatrix_create(NVOID,type,1,A->n)) == NULLSPCOL ) return(NULLMAT);
      if (nsp_spcolmatrix_resize_col(Loc,1,A->n ) == FAIL) return(NULLMAT) ;
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
		    }
		  else 
		    {
		      if ( flag == 1) Indi->R[i+Indi->m*j1]=2;
		    }
		  k1++; 
		}
	      else if ( j1 == j2 ) 
		{ 
		  /* A != 0 and B != 0 **/
		  Loc->D[0]->J[count] = j1;
		  if (  minmaxflag*Ai->R[k1] >   minmaxflag*Bi->R[k2] )
		    {
		      Loc->D[0]->R[count]=  Ai->R[k1];
		    }
		  else
		    {
		      Loc->D[0]->R[count]=  Bi->R[k2];
		      if ( flag == 1) Indi->R[i+Indi->m*j1]=2;
		    }
		  count++;
		  k1++; k2 ++; 

		}
	      else 
		{ 
		  /* A == 0 and B != 0 **/
		  if (minmaxflag* Bi->R[k2] > 0 ) 
		    {
		      Loc->D[0]->J[count] = j2;
		      Loc->D[0]->R[count] = Bi->R[k2];
		      if ( flag == 1) Indi->R[i+Indi->m*j2]=2;
		      count++;
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
		}
	    }
	  /* Keep inserting remaining arguments for B **/
	  for ( k = k2 ; k < Bi->size ; k++) 
	    { 
	      if ( minmaxflag* Bi->R[k] > 0 ) 
		{
		  Loc->D[0]->J[count] = Bi->J[k];
		  Loc->D[0]->R[count] = Bi->R[k];
		  if ( flag == 1) Indi->R[i+Indi->m*k]=2;
		  count++;
		}
	    }
	  /* count is not set to the proper ith row dimension  **/
	  /* we resize A(i,:) and store Loc  **/
	  if (nsp_spcolmatrix_resize_col(A,i,count)  == FAIL) return(NULLMAT) ;
	  /* use icopy and dcopy XXXX **/
	  for ( k =0 ; k < A->D[i]->size ; k++) 
	    {
	      A->D[i]->J[k] = Loc->D[0]->J[k];
	      A->D[i]->R[k] = Loc->D[0]->R[k];
	    }
						  
	}
      return(Indi);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      return(NULLMAT);
    }
}


NspMatrix *nsp_spcolmatrix_maxitt(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int *err)
{
  return nsp_spcolmatrix_maximinitt_g(A,B,flag,1,err);
}

/*
 *  A(i,j) = Maxi(A(i,j),B(i,j)) 
 *  Ind(i,j) set to j if B(i,j) realize the max and flag ==1 
 *  B unchanged A,B are changed 
 */


/*
 *  Res = Mini(A,B) 
 *  term to term max  A(i;j) = Max(A(i,j),B(i,j)
 *  Res(i,j) = 1 or 2  
 *  A changed, B unchanged, 
 *  Res Created if flag == 1
 */

NspMatrix *nsp_spcolmatrix_minitt(NspSpColMatrix *A, NspSpColMatrix *B, int flag, int *err)
{
  return nsp_spcolmatrix_maximinitt_g(A,B,flag,-1,err);
}


/*
 *  A(i,j) = Mini(A(i,j),B(i,j)) 
 *  Ind(i,j) set to j if B(i,j) realize the max and flag ==1 
 *  B unchanged A,B are changed 
 */

/*
 * Return the Real part of Matrix A 
 * In a Real Matrix A
 * A is changed if a is not real 
 * A = real(A) 
 * XXXXXXXXX OK 
 */

int nsp_spcolmatrix_realpart(NspSpColMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r' )  return(OK);
  for ( i=0 ; i < A->n ; i++)
    {
      int count =0; 
      for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	{
	  if ( A->D[i]->C[k].r != 0.0) count++;
	}
      if ( count != 0) 
	{
	  if ((A->D[i]->R =nsp_alloc_doubles((int) count)) == (double *) 0 ) 
	    {
	      Scierror("Error:\tRunning out of memory\n");
	      return(FAIL);
	    }
	  count =0;
	  for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	    {
	      if ( A->D[i]->C[k].r != 0.0) 
		{ 
		  A->D[i]->R[count] =A->D[i]->C[k].r; count++;
		}
	    }
	  FREE ( A->D[i]->C ) ;
	}
    }
  A->rc_type = 'r';
  return(OK);
}

/*
 * Return the Imaginary part of Matrix A 
 * In a Real Matrix A. A= Imag(A) 
 * A is changed  
 * XXXXXXXXXX Ok 
 */

int nsp_spcolmatrix_imagpart(NspSpColMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r')
    {
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
      for ( i=0 ; i < A->n ; i++)
	{
	  int count =0; 
	  for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	    {
	      if ( A->D[i]->C[k].i != 0.0) count++;
	    }
	  if ( count != 0) 
	    {
	      if ((A->D[i]->R =nsp_alloc_doubles((int) count)) == (double *) 0 ) 
		{
		  Scierror("Error:\tRunning out of memory\n");
		  return(FAIL);
		}
	      count =0;
	      for ( k = 0 ; k < A->D[i]->size ; k++ ) 
		{
		  if ( A->D[i]->C[k].i != 0.0) 
		    { 
		      A->D[i]->R[count] =A->D[i]->C[k].i; count++;
		    }
		}
	      FREE ( A->D[i]->C ) ;
	    }
	}
      A->rc_type = 'r';
    }
  return OK;
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

/*
 * Sum =nsp_mat_sum(A ,B])
 *     A is unchanged 
 * if B= 'c' the sum for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the sum for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the full sum is computed 
 * XXXXXXXXXXXXXXXX OK 
 */

NspSpColMatrix *nsp_spcolmatrix_sum(NspSpColMatrix *A, char *flag)
{
  double S;
  doubleC SC,C;
  NspSpColMatrix *Sum=NULL;
  int i,k,count;
  int inc=1;
  if ( A->mn == 0) 
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
	  if (nsp_spcolmatrix_resize_col(Sum,0,A->D[0]->size-ndel ) == FAIL) return NULLSPCOL;
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

/*
 * Max =nsp_mat_maxi(A,B,Imax,lhs)
 *     A is unchanged 
 * if B= 'c' the max for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the max for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the maximum 
 * Imax is created if lhs == 2 
 *    Note that Imax is a full matrix;
 */

typedef int (*SpMaMi1) (NspSpColMatrix *A,NspSpColMatrix *M);
typedef int (*SpMaMi2) (NspSpColMatrix *A,int j,NspSpColMatrix *M,int *count);
typedef int (*SpMaMi3) (NspSpColMatrix *A,int j,NspSpColMatrix *M);

static NspSpColMatrix *SpColMaxiMini(NspSpColMatrix *A, char *flag, NspMatrix **Imax, int lhs, SpMaMi1 F1, SpMaMi2 F2, SpMaMi3 F3)
{
  NspSpColMatrix *M=NULL;
  int j;
  int inc=1,imax,count;
  if ( A->mn == 0 ) 
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
      if (nsp_spcolmatrix_resize_col(M,0,A->n) == FAIL) return NULLSPCOL;
      count =0;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,A->n)) == NULLMAT) 
	    return NULLSPCOL;
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      (*Imax)->R[j]=(*F2)(A,j,M,&count); 
	    }
	}
      else 
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    (*F2)(A,j,M,&count); 
	  }
      if (nsp_spcolmatrix_resize_col(M,0,count) == FAIL) return NULLSPCOL;
      break ;
    case 'c':
    case 'C':
      if ((M =nsp_spcolmatrix_create(NVOID,A->rc_type,A->m,1)) == NULLSPCOL) 
	return NULLSPCOL;
      inc = A->m;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT) 
	    return NULLSPCOL; 
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      int imax =  (*F3)(A,j,M);
	      if ( imax == 0) return NULLSPCOL;
	      (*Imax)->R[j] = imax;
	    }
	}
      else
	for ( j= 0 ; j < A->m ; j++) 
	  {
	    int imax =  (*F3)(A,j,M);
	    if ( imax == 0) return NULLSPCOL;
	  }
      break;
    }
  return M;
}

/*M(1) = Maxi(A) **/

static int SpColMaxi1(NspSpColMatrix *A, NspSpColMatrix *M)
{
  int imax = 0,i,k;
  double amax=0.0; imax=1;
  /* find a first value **/
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      if ( A->D[i]->size !=0 ) 
	{ amax = A->D[i]->R[0];imax = A->D[i]->J[0]+1; break;}
    }
  /* find the max  **/
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->R[k] > amax ) 
	    {
	      amax = A->D[i]->R[k];
	      imax =  A->D[i]->J[k]+1;
	    }
	}
    }
  if ( amax != 0.0 )
    {
      if (nsp_spcolmatrix_resize_col(M,0,1) == FAIL) return 0;
      M->D[0]->J[0]=0;
      M->D[0]->R[0]= amax;
    }
  return imax;
}

/*M(j)=Max A(:,j) **/

static int SpColMaxi2(NspSpColMatrix *A, int j, NspSpColMatrix *M, int *count)
{
  int imax = 0,i,k;
  double amax=0.0; imax=1;
  /* find a first value **/
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->J[k] == j ) 
	    { amax = A->D[i]->R[k] ; imax = i+1; break;}
	  if ( j < A->D[i]->J[k] ) break;
	}
      if ( amax != 0.0 ) break;
    }
  /* find the max **/
  for ( i = 0 ; i < A->m ; i++ ) 
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
	      amax = A->D[i]->R[k];
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

/*M(j)=Max A(j,:) **/

static int SpColMaxi3(NspSpColMatrix *A, int j, NspSpColMatrix *M)
{
  int imax = 0,k;
  double amax=0.0; imax=1;
  /* find a first value **/
  if ( A->D[j]->size != 0 ) { amax = A->D[j]->R[0] ; imax = A->D[j]->J[0]+1;}
  /* find the max **/
  for ( k = 0 ; k < A->D[j]->size ; k++) 
    {
      if ( A->D[j]->R[k]> amax ) 
	{
	  amax = A->D[j]->R[k];
	  imax = A->D[j]->J[k]+1;
	}
    }
  if ( amax != 0.0 )
    {
      if (nsp_spcolmatrix_resize_col(M,j,1) == FAIL) return 0;
      M->D[j]->J[0]= 0;
      M->D[j]->R[0]= amax;
    }
  return imax;
}


NspSpColMatrix *nsp_spcolmatrix_maxi(NspSpColMatrix *A, char *flag, NspMatrix **Imax, int lhs)
{
  return SpColMaxiMini(A,flag,Imax,lhs,SpColMaxi1,SpColMaxi2,SpColMaxi3);
}


/*
 *nsp_mat_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */

/*
 * Creates a Matrix and initialize it with the 
 * function func 
 * R=func(i,j) or R=func(i,j,&Imag) 
 */

/*
 * nsp_mat_triu: A=Triu(A)
 * A is changed 
 */

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


/*
 *nsp_mat_tril: A=Tril(A)
 * A is changed  
 */

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



/*
 *nsp_mat_eye: A=Eye(m,n)
 */

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

/*
 *nsp_mat_ones: A=ones(m,n)
 * A is changed  
 */

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

/*
 *nsp_mat_ones: A=zeros(m,n)
 * A is changed  
 */

NspSpColMatrix *nsp_spcolmatrix_zeros(int m, int n)
{
  NspSpColMatrix *Loc;
  if (( Loc=nsp_spcolmatrix_create(NVOID,'r',m,n)) == NULLSPCOL) return(NULLSPCOL);
  return(Loc);
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
      for ( i = 0 ; i < A->m ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  {
	    j= A->D[i]->J[k];
	    (*F2)(&A->D[i]->C[k],&Loc->C[j+i*Loc->m]);
	  }
    }
  return Loc;
}

NspMatrix *nsp_spcolmatrix_acos(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,acos,nsp_acos_c);
}

/*
 * A=Acosh(A), 
 * A is changed 
 */

NspMatrix *nsp_spcolmatrix_acosh(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,acosh,nsp_acosh_c);
}

/*
 * Generic Function for Sparse unary operators 
 * computes A=f1(A) or A=f2(A) assuming fi(0)=0
 * XXXXXXXXXXXXX OK 
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


/*
 * A=Asin(A), 
 * A is changed 
 */


void nsp_spcolmatrix_asin(NspSpColMatrix *A)
{
  SpColUnary(A,asin,nsp_asin_c);
}

/*
 * A=Asinh(A),
 * A is changed 
 */

void nsp_spcolmatrix_asinh(NspSpColMatrix *A)
{
  SpColUnary(A,asinh,nsp_asinh_c);
}

/*
 * A=Atan(A),  * A is changed 
 */

void nsp_spcolmatrix_atan(NspSpColMatrix *A)
{
  SpColUnary(A,atan,nsp_atan_c);
}

/*
 * A=Atanh(A),
 * A is changed 
 */

void nsp_spcolmatrix_atanh(NspSpColMatrix *A)
{
  SpColUnary(A,atanh,nsp_atanh_c);
}


/*
 *nsp_mat_ceil: A=Ceil(A)
 * A is changed  
 */

void nsp_spcolmatrix_ceil(NspSpColMatrix *A)
{
  SpColUnary(A,ceil,nsp_ceil_c);
}

/*
 *nsp_mat_int: A=Int(A)
 * A is changed  
 */

static double R_aint(double x) { return aint(x);} 

void nsp_spcolmatrix_int(NspSpColMatrix *A)
{
  SpColUnary(A,R_aint,nsp_aint_c);
}

/*
 *nsp_mat_floor: A=Floor(A)
 * A is changed  
 */

void nsp_spcolmatrix_floor(NspSpColMatrix *A)
{
  SpColUnary(A,floor,nsp_floor_c);
}

/*
 *nsp_mat_round: A=Round(A)
 * A is changed  
 */

static double R_anint(double x) { return anint(x);} 

void nsp_spcolmatrix_round(NspSpColMatrix *A)
{
  SpColUnary(A,R_anint,nsp_round_c);
}

/*
 *nsp_mat_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 * XXXXXXXXXXXX OK 
 */

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

/*
 * A=Tan(A), absolue value or module of each element 
 * A is changed 
 */

void nsp_spcolmatrix_tan(NspSpColMatrix *A)
{
  SpColUnary(A,tan,nsp_tan_c);
}

/*
 * A=Tanh(A), absolue value or module of each element 
 * A is changed 
 */

void nsp_spcolmatrix_tanh(NspSpColMatrix *A)
{
  SpColUnary(A,tanh,nsp_tanh_c);
}

/*
 * A=Abs(A), absolue value or module of each element 
 * A is changed 
 * XXXXXXXXXXXX OK 
 */

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

/*
 * A=Erf(A), Erf function 
 * XXXXXXXXXXXX OK 
 */

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

/*
 * A=Arg(A),
 * Argument or Phase 
 * XXXXXXXXXXXX OK 
 */

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

/*
 *nsp_mat_conj: A=real(A)-i*Imag(A)
 * A is changed  only if imaginary 
 * XXXXXXXXXXXX OK 
 */

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

/*
 *nsp_mat_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */


NspMatrix *nsp_spcolmatrix_cos(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,cos,nsp_cos_c);
}

/*
 *nsp_mat_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */

NspMatrix *nsp_spcolmatrix_cosh(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,cosh,nsp_cosh_c);
}

/*
 * MatExpl : Exponentiation term to term 
 * A is changed 
 */

NspMatrix *nsp_spcolmatrix_expel(NspSpColMatrix *A)
{
  return SpColUnary2Full(A,exp,nsp_exp_c);
}

/*
 *nsp_mat_logel: A=LogEl(A)  log term to term 
 * A is changed  
 * The real case is special since the result can be complex
 */

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

/*
 *nsp_mat_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

void nsp_spcolmatrix_sin(NspSpColMatrix *A)
{
  SpColUnary(A,sin,nsp_sin_c);
}


/*
 *nsp_mat_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

void nsp_spcolmatrix_sinh(NspSpColMatrix *A)
{
  SpColUnary(A,sinh,nsp_sinh_c);
}

/*
 *nsp_mat_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 * The real case is special since the result can be complex
 */

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

/*
 *nsp_mat_minus(A),  A= -A 
 * A is changed 
 */

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

/*
 * returns in a Matrix the indices for which the 
 * Matrix A has non zero entries 
 * A is left unchanged
 * according to lhs one or two arguments are returned 
 * XXXXXXXXXXX OK 
 */

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


