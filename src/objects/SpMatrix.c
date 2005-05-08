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

/* In file Perm.c **/

extern int C2F(dperm) (double A[],int ind[],int *nv);
extern int C2F(zperm) (doubleC A[],int ind[],int *nv);

typedef void (*BopLeft) (SpRow *,char,int *,SpRow *,char,int);
typedef void (*BopBoth) (SpRow *,char,int *,SpRow *,char,int,SpRow *,char,int);
typedef void (*BopRight) (SpRow *,char,int *,SpRow *,char,int);

static NspSpMatrix *BinaryOp (NspSpMatrix *,NspSpMatrix *,BopLeft,BopBoth,
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

/*
 * Creates a Sp Matrix of size mxn with no stored date
 * Attention on peut creer une nx0 matrice XXXXX
 */

NspSpMatrix *nsp_spmatrix_create(char *name, char type, int m, int n)
{
  int i;
  NspSpMatrix *Sp = new_spmatrix();
  if ( Sp == NULLSP) 
    {
      Scierror("No more space\n");      return(NULLSP);
    }
  if ( ( NSP_OBJECT(Sp)->name = NewString(name)) == NULLSTRING) return(NULLSP);
  NSP_OBJECT(Sp)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
    Sp->type = SPMATRIX;
    Sp->ftype = Sp_Type;
  */
  Sp->m=m;
  Sp->n=n;
  Sp->mn = Sp->m*Sp->n;
  Sp->rc_type=type;
  if ( Sp->mn == 0 ) 
    {
      Sp->D = NULL;
      return(Sp);
    }
  Sp->D = ( SpRow **) MALLOC( m*sizeof( SpRow *));
  if ( Sp->D == ( SpRow **) 0) 
    {
      Scierror("No More Space\n");
      return(NULLSP);
    }
  for ( i = 0  ; i < Sp->m ; i++) 
    {
      Sp->D[i] = ( SpRow *) MALLOC( sizeof( SpRow));
      if ( Sp->D[i] == ( SpRow *) 0) 
	{
	  Scierror("No More Space\n");
	  return(NULLSP);
	}
      Sp->D[i]->size = 0 ;
    }
  Sp->m = m;
  return(Sp);
}

/*
 * Creation of a Sparse Matrix with specified data
 * Scilab function sparse(rowcols,vals,[m,n])
 * if m and n  have -1 value, sizes are to be computed from 
 * rowscols (RC)
 * 
 */

NspSpMatrix *nsp_spmatrix_sparse_old(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n)
{
  NspSpMatrix *Loc;
  int imax,*xb,i,maxcol;
  int *Iloc;
  Iloc = (int *) RC->R;
  nsp_double2int(&RC->mn,RC->R,Iloc);
  /* max row indice **/
  imax = 0;
  for ( i = 0 ; i < RC->m; i++ ) 
    {
      if ( Iloc[i] <= 0 ) 
	{
	  Scierror("Error:\t negative indice in sparse fisrt argument i=%d ind=%d\n",
		   i,Iloc[i]);
	  return NULLSP;
	}
      if ( Iloc[i] > imax) 
	imax = Iloc[i] ;
    }
  if (m!= -1 &&  m < imax ) 
    {
      Scierror("Error:\t some given indices for rows are > m=%d\n",m);
      return NULLSP;
    }
  if ( m == -1 ) m = imax;
  /* allocate space for Loc with proper row size **/
  if ((Loc =nsp_spmatrix_create(name,Values->rc_type,m,1))== NULLSP ) return NULLSP;
  /* Counting non nul arguments of each line and store it in Loc **/
  for ( i = 0 ; i < Loc->m ; i++) Loc->D[i]->iw=0;
  switch ( Values->rc_type )
    {
    case 'r' :
      for ( i = 0 ; i < RC->m ; i++)  
	{
	  if (Values->R[i] != 0.0 ) Loc->D[Iloc[i]-1]->iw++;
	} 
      break;
    case 'c' : 
      for ( i = 0 ; i < RC->m ; i++)  
	{
	  if (Values->C[i].r != 0.0 || Values->C[i].i ) Loc->D[Iloc[i]-1]->iw++;
	} 
      break;
    }
  /* Resizing each row */
  for ( i = 0 ; i < Loc->m ; i++) 
    {
      if (nsp_spmatrix_resize_row(Loc,i,Loc->D[i]->iw) == FAIL) return(NULLSP);
    }
  /* Check columns **/
  maxcol =0;
  for ( i = 0 ; i < RC->m ; i++) 
    {
      if ( Iloc[i+RC->m] > maxcol ) maxcol =Iloc[i+RC->m];
    }
  
  if ( n != -1 && n < maxcol ) 
    {
      Scierror("Error:\t some given indices for columns (%d) are > n=%d\n",maxcol,n);
      return NULLSP;
    }
  /* fix column size and total size  **/
  if ( n == -1 ) n = maxcol;
  Loc->n = n;
  Loc->mn = n*m;
  /* fill each row with Values **/
  for ( i = 0 ; i < Loc->m ; i++) Loc->D[i]->iw=0;
  for ( i = 0 ; i < RC->m ; i++ ) 
    {
      int count = Loc->D[Iloc[i]-1]->iw;
      switch ( Values->rc_type )
	{
	case 'r' : 
	  if ( Values->R[i] != 0.0 ) 
	    {
	      Loc->D[Iloc[i]-1]->R[count] = Values->R[i];
	      Loc->D[Iloc[i]-1]->J[count] = Iloc[i+RC->m]-1;
	      Loc->D[Iloc[i]-1]->iw++;
	    }
	  break;
	case 'c' : 
	  if ( Values->C[i].r != 0.0 || Values->C[i].i != 0.0) 
	    {
	      Loc->D[Iloc[i]-1]->C[count] = Values->C[i];
	      Loc->D[Iloc[i]-1]->J[count] = Iloc[i+RC->m]-1;
	      Loc->D[Iloc[i]-1]->iw++;
	    }
	  break;
	}
    }
  /* we sort each row in increasing column order **/
  if ( (xb =nsp_alloc_int(Loc->n)) == (int*) 0) return(NULLSP);
  for (i = 0 ; i < Loc->m ; ++i) 
    {
      if (Loc->D[i]->size > 1) 
	{
	  int c__1 =1;
	  C2F(gsort)(Loc->D[i]->J,NULL,xb,&c__1,&c__1,&Loc->D[i]->size,"i","i");
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
 * Creation of a Sparse Matrix with specified data
 * Scilab function sparse(rowcols,vals,[m,n])
 * if m and n  have -1 value, sizes are to be computed from 
 * rowscols (RC)
 * 
 */

NspSpMatrix *nsp_spmatrix_sparse(char *name,NspMatrix *RC, NspMatrix *Values, int m, int n)
{
  NspSpMatrix *Loc;
  NspMatrix *Index;
  int imax,i,maxcol;
  /* max row indice **/
  imax = 0;
  for ( i = 0 ; i < RC->m; i++ ) 
    {
      if ( ((int) RC->R[i]) <= 0 ) 
	{
	  Scierror("Error:\t negative indice in sparse fisrt argument i=%d ind=%d\n",
		   i,((int) RC->R[i]));
	  return NULLSP;
	}
      if (((int) RC->R[i])  > imax) imax = (int) RC->R[i];
    }
  if (m!= -1 &&  m < imax ) 
    {
      Scierror("Error:\t some given indices for rows are > m=%d\n",m);
      return NULLSP;
    }
  /* sort RC lexical row increasing */
  Index = nsp_mat_sort (RC,2,"lr","i");
  if ( m == -1 ) m = imax;
  /* allocate space for Loc with proper row size **/
  if ((Loc =nsp_spmatrix_create(name,Values->rc_type,m,1))== NULLSP ) return NULLSP;
  /* Counting non nul arguments of each line and store it in Loc **/
  for ( i = 0 ; i < Loc->m ; i++) Loc->D[i]->iw=0;
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
  for ( i = 0 ; i < Loc->m ; i++) 
    {
      if (nsp_spmatrix_resize_row(Loc,i,Loc->D[i]->iw) == FAIL) return(NULLSP);
    }
  /* Check columns **/
  maxcol =0;
  for ( i = 0 ; i < RC->m ; i++) 
    {
      if ( ((int) RC->R[i+RC->m]) > maxcol ) maxcol =((int) RC->R[i+RC->m]);
    }
  
  if ( n != -1 && n < maxcol ) 
    {
      Scierror("Error:\t some given indices for columns (%d) are > n=%d\n",maxcol,n);
      return NULLSP;
    }
  /* fix column size and total size  **/
  if ( n == -1 ) n = maxcol;
  Loc->n = n;
  Loc->mn = n*m;
  /* fill each row with Values 
   */
  for ( i = 0 ; i < Loc->m ; i++) Loc->D[i]->iw=0;
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
	      Loc->D[iloc]->J[count] = ((int) RC->R[i+RC->m])-1;
	      if ( count != 0 && ( Loc->D[iloc]->J[count] == Loc->D[iloc]->J[count-1])) 
		{
		  Sciprintf("Warning (%d,%d) is duplicated \n",iloc+1,Loc->D[iloc]->J[count]+1);
		  Loc->D[iloc]->R[count-1]+=  Loc->D[iloc]->R[count];
		}
	      else
		Loc->D[iloc]->iw++;
	    }
	  break;
	case 'c' : 
	  if ( Values->C[i].r != 0.0 || Values->C[id].i != 0.0) 
	    {
	      Loc->D[iloc]->C[count] = Values->C[id];
	      Loc->D[iloc]->J[count] =((int) RC->R[i+RC->m])-1;
	      if ( count != 0 && ( Loc->D[iloc]->J[count] == Loc->D[iloc]->J[count-1])) 
		{
		  Sciprintf("Warning (%d,%d) is duplicated \n",iloc+1,Loc->D[iloc]->J[count]+1);
		  Loc->D[iloc]->C[count-1].r +=  Loc->D[iloc]->C[count].r;
		  Loc->D[iloc]->C[count-1].i +=  Loc->D[iloc]->C[count].i;
		}
	      else 
		Loc->D[iloc]->iw++;
	    }
	  break;
	}
    }
  /* Resizing each row (due to duplicate values) */
  for ( i = 0 ; i < Loc->m ; i++) 
    {
      if ( Loc->D[i]->iw != Loc->D[i]->size ) 
	{
	  if (nsp_spmatrix_resize_row(Loc,i,Loc->D[i]->iw) == FAIL) return(NULLSP);
	}
    }
  /* no need to sort here */
  /*   if ( (xb =nsp_alloc_int(Loc->n)) == (int*) 0) return(NULLSP); */
  /*   for (i = 0 ; i < Loc->m ; ++i)  */
  /*     { */
  /*       if (Loc->D[i]->size > 1)  */
  /* 	{ */
  /* 	  int c__1 =1; */
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
 */

int nsp_spmatrix_get(NspSpMatrix *A, NspMatrix **RC, NspMatrix **Values)
{
  int count=0,i,j,iw;
  for ( i = 0 ; i < A->m ; i++) 
    {
      count += A->D[i]->size ;
    }
  if ((*RC = nsp_matrix_create(NVOID,'r',count,2)) == NULLMAT ) return FAIL;
  if ((*Values = nsp_matrix_create(NVOID,A->rc_type,count,1)) == NULLMAT ) return FAIL;
  iw=0;
  for ( i = 0 ; i < A->m ; i++) 
    {
      for ( j = 0 ; j < A->D[i]->size ; j++ ) 
	{
	  /* Store (row=i+1,col= A->D[i]->J[j]+1) in RC(iw,:) **/
	  (*RC)->R[iw] = i + 1; 
	  (*RC)->R[iw+(*RC)->m ] = A->D[i]->J[j]+1;
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
 * Res =nsp_spmatrix_copy(A) 
 * Copy a sparse matrix in Res : res is created 
 */

NspSpMatrix *nsp_spmatrix_copy(NspSpMatrix *A)
{
  int i;
  int inc=1;
  NspSpMatrix *Sp;
  Sp =nsp_spmatrix_create(NVOID,A->rc_type,A->m,A->n);
  if ( Sp == NULLSP ) return(NULLSP) ; 
  for ( i = 0  ; i < Sp->m ; i++) 
    {
      if (nsp_spmatrix_resize_row(Sp,i,(int)A->D[i]->size) == FAIL) return(NULLSP);
 nsp_icopy(&A->D[i]->size,A->D[i]->J,&inc,Sp->D[i]->J,&inc);
      if ( A->rc_type == 'r' ) 
	C2F(dcopy)(&A->D[i]->size,A->D[i]->R,&inc,Sp->D[i]->R,&inc);
      else 
	C2F(zcopy)(&A->D[i]->size,A->D[i]->C,&inc,Sp->D[i]->C,&inc);
    }
  return(Sp);
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

int nsp_spmatrix_resize_row(NspSpMatrix *Sp, int i, int n)
{
  SpRow *Row;
  Row = Sp->D[i];
  if ( Row->size == 0 ) 
    {
      if ( n <= 0 ) return(OK);
      if ((Row->J =nsp_alloc_int((int) n)) == (int *) 0) return(FAIL);
      switch ( Sp->rc_type ) 
	{
	case 'r' : 
	  if ( (Row->R =nsp_alloc_doubles((int) n)) == (double *) 0 ) return(FAIL);
	  Row->C = (doubleC *) 0;
	  break;
	case 'c' : 
	  if ( (Row->C =nsp_alloc_doubleC((int) n)) == (doubleC *) 0 ) return(FAIL);
	  Row->R = (double *) 0; 
	  break;
	}
      Row->size = n;
      return(OK);
    }
  if ( Row->size == n  ) return(OK);
  
  if ( n <= 0 ) 
    {
      /* empty new size **/
      FREE(Row->J);
      switch ( Sp->rc_type ) 
	{
	case 'r' : 
	  FREE(Row->R);
	  break;
	case 'c' : 
	  FREE(Row->C);
	}
      Row->size = 0;
      return(OK);
    }
  if ((Row->J =nsp_realloc_int(Row->J,(int) n))  == (int *) 0) return(FAIL);
  switch ( Sp->rc_type ) 
    {
    case 'r' : 
      if (( Row->R =nsp_realloc_doubles(Row->R,(int) n)) == (double *) 0 ) return(FAIL);
      Row->C = (doubleC *) 0;
      break;
    case 'c' : 
      if (( Row->C =nsp_realloc_doubleC(Row->C, (int) n)) == (doubleC *) 0 ) return(FAIL);
      Row->R = (double *) 0; 
      break;
    }
  Row->size = n;
  return(OK);
}

/*
 * Destroy a Sp Matrix 
 */

void SpRowDestroy(SpRow *Row)
{
  if ( Row->size != 0 ) 
    {
      FREE( Row->J);
      FREE( Row->R);
      FREE( Row->C);
    }
}

void nsp_spmatrix_destroy(NspSpMatrix *Mat)
{
  int i;
  FREE(NSP_OBJECT(Mat)->name);
  for ( i = 0  ; i < Mat->m ; i++) 
    {
      SpRowDestroy(Mat->D[i]);
      FREE(Mat->D[i]);
    }
  FREE(Mat->D);
  FREE(Mat) ;
}

/*
 *nsp_spmatrix_info: display Info on Sparse Matrix
 */

void nsp_spmatrix_info(NspSpMatrix *Sp, int indent)
{ 
  int i;
  if ( Sp == NULLSP) 
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

void nsp_spmatrix_print(NspSpMatrix *Sp, int indent)
{ 
  int j;
  for ( j=0 ; j < indent ; j++) Sciprintf(" ");
  if (Sp->mn==0 ) 
    {
      Sciprintf("%s\t= []\t\t %c (%dx%d) sparse\n",NSP_OBJECT(Sp)->name,Sp->rc_type,Sp->m,Sp->n);
    }
  else
    {
      Sciprintf("%s\t=\t\t %c (%dx%d) sparse\n",NSP_OBJECT(Sp)->name,Sp->rc_type,Sp->m,Sp->n);
      nsp_print_internalSpM (Sp,indent);
    }

  /* 
      int i,k;
  static char formatr[] = "(%d,%d) %f\n";
  static char formati[] = "(%d,%d) %f + %f i\n";
  switch ( Sp->rc_type ) 
    {
    case 'r' :
      for ( i = 0 ; i < Sp->m ; i++ ) 
	{
	  SpRow *Ri = Sp->D[i];
	  for  ( k = 0 ;  k < Ri->size ; k++)
	    { 
	      Sciprintf(formatr ,i+1,Ri->J[k]+1,Ri->R[k]);
	    }
	}
      break;
    case 'c' :
      for ( i = 0 ; i < Sp->m ; i++ ) 
	{
	  SpRow *Ri = Sp->D[i];
	  for  ( k = 0 ;  k < Ri->size ; k++)
	    { 
	      Sciprintf(formati ,i+1,Ri->J[k]+1,Ri->C[k].r,Ri->C[k].i);
	    }
	}
      break;
    }
  **/

}

/*
 *nsp_spmatrix_redim: Changes matrix dimensions to mxn 
 *   the product m*n must be unchanged and 
 *   the stored data is kept considering that data are stored 
 *   columnwise. 
 *   a New Matrix is returned or NULLSP on failure
 */

NspSpMatrix *nsp_spmatrix_redim(NspSpMatrix *A, int m, int n)
{
  int *xb;
  int i,k;
  NspSpMatrix *Loc;
  if ( A->mn !=  m*n ) 
    {
      Scierror("Error:\tCannot change size to (%dx%d) since matrix has %d elements\n",m,n,A->mn);
      return(NULLSP);
    }
  if ((Loc =nsp_spmatrix_create(NVOID,A->rc_type,m,n))== NULLSP ) 
    return NULLSP;
  /* initialize iw **/
  for ( i= 0 ; i < Loc->m ; i++) 
    Loc->D[i]->iw= 0;
  /* counting elements **/
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      SpRow *Ri = A->D[i];
      for  ( k = 0 ;  k < Ri->size ; k++)
	{ 
	  int ind= Ri->J[k]*A->m+i,i1;
	  i1= ind % m;
	  Loc->D[i1]->iw++;
	}
    }
  /* Enlarge Loc Rows **/
  for ( i= 0 ; i < Loc->m ; i++) 
    {
      if (nsp_spmatrix_resize_row(Loc,i, Loc->D[i]->iw)== FAIL) return NULLSP;
    }
  /* Fill the rows of the new matrix **/
  /* re-initialize iw **/
  for ( i= 0 ; i < Loc->m ; i++) 
    Loc->D[i]->iw= 0;
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      SpRow *Ri = A->D[i];
      for  ( k = 0 ;  k < Ri->size ; k++)
	{ 
	  int ind= Ri->J[k]*A->m+i,i1,j1;
	  i1= ind % m;
	  j1= (ind-i1)/m;
	  Loc->D[i1]->J[Loc->D[i1]->iw] = j1;
	  switch (Loc->rc_type ) 
	    {
	    case 'r' : Loc->D[i1]->R[Loc->D[i1]->iw]= Ri->R[k];break;
	    case 'c' : Loc->D[i1]->C[Loc->D[i1]->iw]= Ri->C[k];break;
	    }
	  Loc->D[i1]->iw++;
	}
    }

  /* we sort each row in increasing column order **/
  if ( (xb =nsp_alloc_int(Loc->n)) == (int*) 0) return(NULLSP);
  for (i = 0 ; i < Loc->m ; ++i) 
    {
      if (Loc->D[i]->size > 1) 
	{
	  int c__1 =1;
	  C2F(gsort)(Loc->D[i]->J,NULL,xb,&c__1,&c__1,&Loc->D[i]->size,"i","i");
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
 * Add m empty Rows ( Row->size = 0) to a Sparse matrix 
 * if m < Sp->m then Sp is unchanged. 
 */

int nsp_spmatrix_enlarge_rows(NspSpMatrix *Sp, int m)
{
  int i;
  if ( Sp->m >= m ) return(OK);
  Sp->D = ( SpRow **) REALLOC(Sp->D, m*sizeof( SpRow *));
  if ( Sp->D == ( SpRow **) 0) 
    {
      Scierror("No More Space\n");
      return(FAIL);
    }
  for ( i = Sp->m ; i < m ; i++) 
    {
      Sp->D[i] = ( SpRow *) MALLOC( sizeof( SpRow));
      if ( Sp->D[i] == ( SpRow *) 0) 
	{
	  Scierror("No More Space\n");
	  return(FAIL);
	}
      Sp->D[i]->size = 0 ;
    }
  Sp->m = m;
  return(OK);
}


/*
 *nsp_spmatrix_enlarge(A,m,n)
 * changes A to B= [ A , 0; 0,0 ]  
 * in such a way that B (max(A->m,m)xmax(A->n,n));
 * The result is stored in A
 * if m and n are smaller than A->m and A->n Matrix 
 * associated dimensions are not changed 
 */

int nsp_spmatrix_enlarge(NspSpMatrix *A, int m, int n)
{
  /* special case **/
  if ( n > A->n  ) A->n = n ; /* easy for sparse matrix **/
  if ( m > A->m  ) 
    return nsp_spmatrix_enlarge_rows(A,m);
  return OK;
}

/*
 * A = [A, B] 
 * Right concatenation on A, A is changed 
 */

int nsp_spmatrix_concatr(NspSpMatrix *A, NspSpMatrix *B)
{
  int i;
  if ( A->rc_type == 'r' &&  B->rc_type == 'c' )  
    {
      if (nsp_spmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if ( A->m != B->m ) 
    {
      Scierror("Sparse Concat : incompatible size  \n");
      return(FAIL);
    }
  /* We enlarge the rows of A to store the non-null values comming from B*/
  for ( i = 0 ; i < A->m ; i++) 
    { 
      if (nsp_spmatrix_resize_row(A,i,((int) (A->D[i]->size+B->D[i]->size))) == FAIL)return(FAIL);
    } 

  for ( i = 0 ; i < A->m ;  i++ ) 
    { 
      int inc = 1;
      SpRow *Ai = A->D[i];
      SpRow *Bi = B->D[i];
      /* SpresizeRow has changed the Ai->size */ 
      int ioffset = Ai->size-Bi->size;
 nsp_icopy(&Bi->size,Bi->J,&inc,Ai->J+ioffset,&inc);
      /* must add A->n to the inserted column values **/
 nsp_iadd(&Bi->size,&A->n,Ai->J+ioffset,&inc);
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
  A->n += B->n;
  return(OK);
}

/*
 *  A=[A; B ] 
 *   Down concatenation on A 
 */

int nsp_spmatrix_concatd(NspSpMatrix *A, NspSpMatrix *B)
{ 
  int Am,inc=1;
  int i;
  if ( A->n != B->n ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(FAIL);
    }
  Am = A->m;
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_spmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if (nsp_spmatrix_enlarge_rows(A,A->m+B->m) == FAIL ) { return(FAIL) ; } ; 
  for ( i = Am ; i < A->m ; i++) 
    { 
      SpRow *Ai = A->D[i];
      SpRow *Bi = B->D[i-Am];
      if (nsp_spmatrix_resize_row(A,i,(int) Bi->size) == FAIL) return(FAIL) ;
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
 * Diag Concatenation 
 * A = [A,0;0,B] 
 * A is changed  B is left unchanged 
 */

int nsp_spmatrix_concatdiag(NspSpMatrix *A, NspSpMatrix *B)
{
  int i,j;
  int Am = A->m;
  int An = A->n;
  /* first [A;B]  **/
  int n1 = Max(A->n,B->n);
  A->n = n1;
  B->n = n1;
  if (nsp_spmatrix_concatd( A,B) == FAIL) return FAIL;
  /* push last rows **/
  for ( i = Am ; i < A->m ; i++) 
    { 
      SpRow *Ai = A->D[i];
      for ( j = 0 ; j < Ai->size ; j++) 
	Ai->J[j]+= An;
    }
  /* restore proper columns dimensions **/
  A->n = An + B->n ;
  return(OK);
}


/*
 * Utility functions 
 *nsp_spmatrix_insert_elt(A,i,j,B,rb,cb)
 *nsp_spmatrix_delete_elt(A,row,col,amin,amax)
 *nsp_spmatrix_get_elt(B,i,j)
 * nsp_spmatrix_store(A,r,c,col,B,r1,c1)
 */


/* Utility function **/

void  nsp_spmatrix_store(NspSpMatrix *A, int r, int c, int col, NspSpMatrix *B, int r1, int c1)
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

int nsp_spmatrix_insert_elt(NspSpMatrix *A, int i, int j, NspSpMatrix *B, int rb, int cb)
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
      if (nsp_spmatrix_resize_row(A,i,Ai->size+1) == FAIL) return FAIL;
      /* Get the new row **/
      Ai= A->D[i];
      /* we insert the new value **/
      if ( insert == 0) 
	{
	  /* insert at the end **/
	 nsp_spmatrix_store(A,i,Ai->size-1,j,B,rb,cb);
	}
      else
	{
	  int k1;
	  /* insert at position insert **/
	  /* move right one step **/
	  for ( k1 = Ai->size -1 ; k1 >= insert ; k1--) 
	    {
	 nsp_spmatrix_store(A,i,k1+1,Ai->J[k1],A,i,k1);
	    }
	 nsp_spmatrix_store(A,i,insert,j,B,rb,cb);
	}
    }
  else
    {
      /* we must change element **/
  nsp_spmatrix_store(A,i,ok,j,B,rb,cb);
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


int nsp_spmatrix_delete_elt(NspSpMatrix *A, int row, int col, int amin, int amax)
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
	 nsp_spmatrix_store(A,row,k1-1,A->D[row]->J[k1],A,row,k1);
	}
      return acol;
    }
  else
    {
      return -1;
    }
}

/* return k such that B->D[i]->J[k] = j or -1 if such k does not exists **/

int nsp_spmatrix_get_elt(NspSpMatrix *B, int i, int j)
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

int nsp_spmatrix_set_rowcol(NspSpMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspSpMatrix *B)
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
    if (nsp_spmatrix_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  /* Id result complex ? **/
  if ( B->rc_type == 'c' &&   A->rc_type == 'r' )
    { 
      type = 'c';
      if (nsp_spmatrix_seti(A,0.00) == FAIL ) return(FAIL);
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
      if (nsp_spmatrix_resize_row(A,row ,nel+Ais) == FAIL) return FAIL;
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
		 nsp_spmatrix_store(A,row,acol, col, B,ib,ok);
		  /*Cols is not supposed to be increasing  amin = acol ;  **/
		  break;
		case 2: /* insert before acol **/
		  /* move right one step **/
		  for ( k1 = amax -1 ; k1 >= acol ; k1--) 
		    {
		 nsp_spmatrix_store(A,row,k1+1,A->D[row]->J[k1],A,row,k1);
		    }
		 nsp_spmatrix_store(A,row,acol,col,B,ib,ok);
		  /*Cols is not supposed to be increasing amin = acol ; **/
		  amax++ ; break ;
		default : 
		  /* insert at end **/
		 nsp_spmatrix_store(A,row,amax,col,B,ib,ok);
		  /* Cols is not supposed to be increasing  amin = amax ; **/
		  amax++;
		}
	    }
	  else 
	    {
	      /* we must set the [row,col] element of A to 0.0 **/
	      ok1 =nsp_spmatrix_delete_elt(A,row,col,amin,amax);
	      if ( ok1 != -1 ) 
		{
		  /* Cols is not supposed to be increasing  amin = ok1; **/
		  amax--;
		}
	    }
	}
      /* we resize A(row,:) to its correct size **/
      if (nsp_spmatrix_resize_row(A,row ,amax) == FAIL) return FAIL;
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

int nsp_spmatrix_set_row(NspSpMatrix *A, NspMatrix *Inds, NspSpMatrix *B)
{
  int i;
  int Bscal=0;
  /* Calling a genric function which check arguments **/
  if (GenericMatSeRo(A,A->m,A->n,A->mn,Inds,
		     B,B->m,B->n,B->mn,(F_Enlarge)nsp_spmatrix_enlarge,&Bscal)== FAIL) 
    return FAIL;
  /* */
  if ( A->rc_type == 'r' && B->rc_type == 'c' ) 
    {
      if (nsp_spmatrix_complexify(A) == FAIL ) return(FAIL);
    }
  if ( Bscal == 0 ) 
    {
      for ( i = 0  ; i < Inds->mn ; i++) 
	{
	  int rb,cb,kb,ia,ra,ca ;
	  rb= i % Inds->m;
	  cb= (i - rb )/Inds->m;
	  kb =nsp_spmatrix_get_elt(B,rb,cb);
	  ia = ((int) Inds->R[i])-1;
	  ra = ia % A->m;
	  ca= (ia - ra )/A->m;
	  if ( kb == -1 ) 
	    {
	      /* we must set the [ra,ca] element of A to 0.0 **/
	      int ok1 =nsp_spmatrix_delete_elt(A,ra,ca,0,A->D[ra]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_spmatrix_resize_row(A,ra,A->D[ra]->size-1) == FAIL) return FAIL;
		}
	    }
	  else
	    {
	      /* must change or insert element in A **/
	      if (nsp_spmatrix_insert_elt(A,ra,ca,B,rb,kb)== FAIL) return FAIL;
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
	      ok1 =nsp_spmatrix_delete_elt(A,ra,ca,0,A->D[ra]->size);
	      if ( ok1 != -1 ) 
		{
		  if (nsp_spmatrix_resize_row(A,ra,A->D[ra]->size-1) == FAIL) return FAIL;
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
	      if (nsp_spmatrix_insert_elt(A,ra,ca,B,0,0)== FAIL) return FAIL;
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

int nsp_spmatrix_delete_cols(NspSpMatrix *A, NspMatrix *Cols)
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
      ndel =nsp_spmatrix_compress_row(A,i);
      if (nsp_spmatrix_resize_row(A,i, Ai->size-ndel ) == FAIL) return(FAIL) ;
    }
  A->n -= Cols->mn ;
  /* XXXX : Attention ici si A->n devient egal a zero 
    Il faut detruire les ligne pour renvoyer une matrice vide **/

  return(OK);
}

/* compress ith row of sparse Matrix A **/
/* [1,2,-1,4,5,6,-1,11]--> [1,2,4,5,6,11] **/
/* return the number of deleted elements **/

int nsp_spmatrix_compress_row(NspSpMatrix *A, int i)
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

int nsp_spmatrix_delete_rows(NspSpMatrix *A, NspMatrix *Rows)
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
      if (nsp_spmatrix_resize_row(A,ind,0) == FAIL) return FAIL; /* free the associated row **/
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

static NspSpMatrix *SpExtract_G(NspSpMatrix *A, NspMatrix *Rows, NspMatrix *Cols, int flag, int *err)
{
  NspMatrix *Work= NULL, *Index = NULL;
  NspSpMatrix *Loc;
  int rmin,rmax,cmin,cmax,i,j,Rm;
  if ( A->mn == 0) return nsp_spmatrix_create(NVOID,A->rc_type,0,0);
  if (flag == 1) 
    {
      Bounds(Rows,&rmin,&rmax);
      if ( rmin < 1 ||  rmax > A->m  ) 
	{
	  *err=1;
	  Scierror("Error:\tIndices out of bound\n");
	  return(NULLSP);
	}
    }
  Index = nsp_mat_sort (Cols,2,"g","i");
  cmin = Cols->R[0]; cmax = Cols->R[Cols->mn-1];
  *err=0;
  if (  cmin < 1 ||  cmax > A->n ) 
    {
      *err=1;
      return(NULLSP);
    }
  Rm= (flag == 1) ? Rows->mn : A->m;
  if ( (Loc =nsp_spmatrix_create(NVOID,A->rc_type,Rm,Cols->mn))== NULLSP) 
    return(NULLSP);
  /* used to store elements */
  if ( ( Work = nsp_matrix_create(NVOID,'r',2,Cols->mn)) == NULLMAT) return NULLSP;
  
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
      if (nsp_spmatrix_resize_row(Loc,i,count)==FAIL) return NULLSP;
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




NspSpMatrix *nsp_spmatrix_extract(NspSpMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  NspSpMatrix *Sp;
  int err;
  Sp=SpExtract_G(A,Rows,Cols,1,&err);
  if (err==1 ) Scierror("Error:\tIndices out of bound\n");
  return Sp;
}
/*
 * Res=nsp_matrix_extract_elements(A,Elts)
 * A unchanged, Elts
 */


NspSpMatrix *nsp_spmatrix_extract_elts(NspSpMatrix *A, NspMatrix *Elts)
{
  NspSpMatrix *Loc;
  int rmin,rmax,i,err,k;
  Bounds(Elts,&rmin,&rmax);
  if ( A->mn == 0) return nsp_spmatrix_create(NVOID,A->rc_type,0,0);
  if ( rmin < 1 || rmax > A->mn )
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLSP);
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      NspMatrix *Rows ; 
      if ((Rows = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT ) return NULLSP;
      Rows->R[0] = 1;
      /* like A(1,Elts) **/
      Loc=  SpExtract_G(A,Rows,Elts,1,&err);
      if (err==1 ) Scierror("Error:\tIndices out of bound\n");
      return Loc;
    }
  else
    {
      if ( (Loc =nsp_spmatrix_create(NVOID,A->rc_type,Elts->mn,1))== NULLSP) 
	return(NULLSP);
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
	      if (nsp_spmatrix_resize_row(Loc,i,1)==FAIL) return NULLSP;
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

NspSpMatrix *nsp_spmatrix_extract_cols(NspSpMatrix *A, NspMatrix *Cols, int *err)
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

NspSpMatrix *nsp_spmatrix_extract_rows(NspSpMatrix *A, NspMatrix *Rows, int *err)
{
  NspSpMatrix *Loc;
  int rmin,rmax,i,j;
  if ( A->mn == 0) return nsp_spmatrix_create(NVOID,A->rc_type,0,0);
  Bounds(Rows,&rmin,&rmax);
  *err=0;
  if ( rmin < 1 ||  rmax > A->m  ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLSP);
    }
  if ( (Loc =nsp_spmatrix_create(NVOID,A->rc_type,Rows->mn,A->n))== NULLSP) 
    return(NULLSP);
  for ( i = 0 ; i < Loc->m ; i++)
    {
      int row =((int) Rows->R[i])-1; 
      SpRow *Ai= A->D[row];
      SpRow *Li= Loc->D[i];
      if (nsp_spmatrix_resize_row(Loc,i,Ai->size)==FAIL) return NULLSP;  
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

NspSpMatrix *nsp_spmatrix_diag_extract(NspSpMatrix *A, int k)
{
  NspSpMatrix *Loc;
  int j,i;
  int imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc =nsp_spmatrix_create(NVOID,A->rc_type,(int) 0 , (int) 0);
      return(Loc);
    }
  if (( Loc =nsp_spmatrix_create(NVOID,A->rc_type,imax-imin,(int)1))==NULLSP)  return NULLSP;
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
	  if (nsp_spmatrix_resize_row(Loc,i,1)==FAIL) return NULLSP;  
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
 *  int nsp_spmatrix_diag_create(A,Diag,k)
 * Diag is a sparse nxm matrix 
 */

static int  GetDiagVal (NspSpMatrix *Diag,int i,double *val,doubleC *cval);

int nsp_spmatrix_diag_set(NspSpMatrix *A, NspSpMatrix *Diag, int k)
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
      if (nsp_spmatrix_enlarge_rows(A,imax) == FAIL ) return FAIL;
      A->n = imax+k;
    }
  if ( Diag->rc_type == 'c' && A->rc_type == 'r' ) 
      if (nsp_spmatrix_complexify(A) == FAIL ) return(FAIL);
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
	      if (nsp_spmatrix_resize_row(A,i,A->D[i]->size+1)==FAIL) return FAIL;
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

static int  GetDiagVal(NspSpMatrix *Diag, int i, double *val, doubleC *cval)
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

NspSpMatrix *nsp_spmatrix_diag_create(NspSpMatrix *Diag, int k)
{
  int i,k1;
  int imin,imax;
  NspSpMatrix *Loc;
  imin = Max(0,-k);
  imax = Diag->mn +imin;
  if ((Loc =nsp_spmatrix_create(NVOID,Diag->rc_type,imax,imax+k))  == NULLSP) 
    return(NULLSP);
  if ( Diag->m == 1 )
    {
      for ( k1=0 ; k1 < Diag->D[0]->size ; k1++) 
	{
	  int j= Diag->D[0]->J[k1];
	  int iloc = (k>0) ? j : j-k;
	  int jloc = (k>0) ? j+k : j;
	  if (nsp_spmatrix_resize_row(Loc,iloc,1)==FAIL) return NULLSP;
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
	      if (nsp_spmatrix_resize_row(Loc,iloc,1)==FAIL) return NULLSP;
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
      return NULLSP;
    }
  return Loc;
}

/*
 * Multiplication of Two Sparse Matrices 
 * ------------------------
 *  multiply sparse matrices by the method of gustafson,acm t.o.m.s. 
 *  vol 4 (1978) p250. 
 *  C coded version : Chancelier 1996
 */

NspSpMatrix *nsp_spmatrix_mult(NspSpMatrix *A, NspSpMatrix *B)
{
  int cdisize;
  NspSpMatrix *C;
  NspMatrix *x;
  int *xb;
  static int   i, j, k, v, ip, jp, kp;
  char type = 'r';
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
  if ( A->n != B->m ) 
    {
      Scierror("SpMult : incompatible arguments\n");
      return(NULLSP);
    }
  C =nsp_spmatrix_create(NVOID,type, A->m,B->n);
  if ( C == NULLSP ) return(NULLSP) ; 
  /*  x          a one-dimensional array of size ge number of cols of c, 
   *                to contain elements of current row of c, 
   *                in full,i.e. non-sparse form.  */
  x = nsp_matrix_create(NVOID,type,(int) 1, C->n );
  if ( x == NULLMAT) return(NULLSP);
  /*  xb         an array of same size as x. xb(j)=i if element in row i, 
   *                column j of c is non-zero. */
  if ( (xb =nsp_alloc_int(C->n)) == (int*) 0) return(NULLSP);
  ip = 0; /* Counts non null elements of C */
  /* initialize the non-zero -element indicator for row i of c. */
  for (v = 0 ; v < B->n ; v++ )  xb[v] = -1;
  /* process the rows of a. */
  for (i = 0 ; i < A->m ; i++) 
    {
      SpRow *Ai = A->D[i];
      /*  process row i of a.  */
      for (jp = 0  ; jp <  Ai->size  ; jp++) 
	{
	  SpRow *Bj;
	  /* j is the current col-index for a */
	  j = Ai->J[jp] ;
	  Bj= B->D[j];
	  /* We process the row j of  b. */
	  for (kp = 0 ; kp < Bj->size ; kp++) 
	    {
	      /* getting non nul b(j,k) */
	      k = Bj->J[kp];
	      /* check if contribution already exixts to c(i,k) */
	      if ( xb[k] != i )
		{ 
		  xb[k] = i ; 
		  if ( C->rc_type == 'r' )
		    {
		      x->R[k]=0.00;
		    }
		  else 
		    {
		      x->C[k].r = 0.00; x->C[k].i = 0.00;
		    }
		}
	      if ( C->rc_type == 'r' ) 
		{
		  x->R[k] += Ai->R[jp] * Bj->R[kp];
		}
	      else 
		{
		  if ( A->rc_type == 'r') 
		    {
		      x->C[k].i += Ai->R[jp] * Bj->C[kp].i;
		      x->C[k].r += Ai->R[jp] * Bj->C[kp].r;
		    }
		  else 
		    {
		      if ( B->rc_type == 'r' ) 
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
	    }
	}
      /* we have now computed c(i,.) in expanded form in stored in x,xb */
      /* we store them in C and exclude number which have cancelled during 
	 computation */
      cdisize= C->D[i]->size;
      if ( C->rc_type == 'r') 
	{
	  for (v = 0 ; v < B->n ; v++)  
	    { 
	      if ( xb[v] == i && x->R[v] != 0.00 )  cdisize++;
	    }
	}
      else 
	{
	  for (v = 0 ; v < B->n ; v++)  
	    { 
	      if ( xb[v] == i && ( x->C[v].r != 0.00  || x->C[v].i != 0.00))  cdisize++;
	    }
	}
      nsp_spmatrix_resize_row(C,i,cdisize);
      ip = 0; 
      if ( C->rc_type == 'r' ) 
	{
	  for (v = 0 ; v < B->n ; v++)  
	    { 
	      if ( xb[v] == i && x->R[v] != 0.00 ) 
		{
		  C->D[i]->J[ip] = v;
		  C->D[i]->R[ip] = x->R[v];
		  ip++;
		}
	    }
	}
      else
	{
          for (v = 0 ; v < B->n ; v++)
            {
              if ( xb[v] == i &&  ( x->C[v].r != 0.00  || x->C[v].i != 0.00))
                {
                  C->D[i]->J[ip] = v;
                  C->D[i]->C[ip].r = x->C[v].r;
                  C->D[i]->C[ip].i = x->C[v].i;
                  ip++;
                }
            }
        }

    }
  /*  sort result */ 
  for (i = 0 ; i < C->m ; ++i) 
    {
      if (C->D[i]->size > 1) 
	{
	  int c__1 =1;
	  /* C2F(isort1)(C->D[i]->J,&C->D[i]->size,xb,&c__1); **/
	  C2F(gsort)(C->D[i]->J,NULL,xb,&c__1,&c__1,&C->D[i]->size,"i","i");
	  if ( C->rc_type == 'r' ) 
	    C2F(dperm)(C->D[i]->R,&C->D[i]->size,xb);
	  else 
	    C2F(zperm)(C->D[i]->C,&C->D[i]->size,xb);
	}
    }
  FREE(xb);
  nsp_matrix_destroy(x);
  return(C);
}


/*
 * nsp_spmatrix_mult_scal(A,B) when B is a scalar sparse 
 * A is changed 
 * the fact that B is scalar is not checked 
 */

int nsp_spmatrix_mult_scal_old(NspSpMatrix *A, NspSpMatrix *B)
{
  int i,k ;
  if ( A->rc_type == 'r' &&  B->rc_type == 'c' )  
    {
      if (nsp_spmatrix_complexify(A) == FAIL ) return(FAIL);
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

int nsp_spmatrix_complexify(NspSpMatrix *A)
{
  int inc = 1;
  int i;
  if ( A->rc_type == 'c' ) return(OK);
  for ( i = 0 ; i < A->m ; i++) 
    {
      SpRow *Ai = A->D[i];
      if ( Ai->size != 0) 
	{ 
	  Ai->C =nsp_alloc_doubleC((int) Ai->size);
	  if ( Ai->C == (doubleC *) 0) return(FAIL);
	nsp_dzcopy(&Ai->size,Ai->R,&inc,Ai->C,&inc);
	  FREE(Ai->R);
	}
    }
  A->rc_type = 'c';
  return(OK);
}


/*
 * Set real part of all non nul elements of A to d 
 */

int nsp_spmatrix_setr(NspSpMatrix *A, double d)
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

int nsp_spmatrix_seti(NspSpMatrix *A, double d)
{
  int i,c1=1;
  switch ( A->rc_type ) 
    {
    case 'r' : if (nsp_spmatrix_complexify(A) == FAIL ) return(FAIL); break;
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

int  RowCountNonNull(NspMatrix *A, int i)
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

int  CountNonNull(NspMatrix *A)
{ 
  int count=0,i;
  for ( i = 0 ; i < A->m ; i++ ) count +=  RowCountNonNull(A,(int)i);
  return(count);
}

/*
 * Converts a full Matrix to a Sparse one 
 * used for test on sparse matrix 
 * we have two solutions 
 *  [1] use CountNonNull 
 *  [2] SpCreates with 256 elements 
 *        and SpIncrease to increase Storage 
 *        Fix storage at end 
 */

NspSpMatrix *nsp_spmatrix_from_mat(NspMatrix *A)
{ 
  /* nnul : counts non nul elements in the sparse matrix 
   * count : count non nul elements in row i */
  int i,j;
  NspSpMatrix *Sp;
  if (( Sp =nsp_spmatrix_create(NVOID,A->rc_type,A->m,A->n))== NULLSP) return(NULLSP);
  /* first pass to count non null elements on rows */
  for ( i = 0 ; i < A->m ; i++ ) 
    { 
      int count;
      count =  RowCountNonNull(A,(int)i) ;
      if (nsp_spmatrix_resize_row(Sp,i,count) == FAIL) return(NULLSP) ;
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

NspMatrix *nsp_spmatrix_to_mat(NspSpMatrix *Sp)
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


NspSpMatrix *nsp_spmatrix_transpose(NspSpMatrix *A)
{
  int k,i,j;
  NspSpMatrix *Loc;
  Loc =nsp_spmatrix_create( NVOID,A->rc_type,A->n,A->m);
  if ( Loc == NULLSP ) return(NULLSP) ; 
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
      if (nsp_spmatrix_resize_row(Loc,i,(int) Loc->D[i]->iw) == FAIL) return(NULLSP) ;
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

double  plus (double x, double y, double xi, double yi, double *ival, char type)
{ 
  if ( type == 'c' ) *ival = xi + yi ;
  return( x+y ) ;
}

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
 *nsp_spmatrix_add,nsp_spmatrix_sub, SpMulttt
 * A+B A-B A.*B
 */

NspSpMatrix *nsp_spmatrix_add(NspSpMatrix *A, NspSpMatrix *B)
{
  return(BinaryOp(A,B,PlusLeft,PlusBoth,PlusRight));
}

NspSpMatrix *nsp_spmatrix_sub(NspSpMatrix *A, NspSpMatrix *B)
{
  return(BinaryOp(A,B,MinusLeft,MinusBoth,MinusRight));
}

NspSpMatrix *nsp_spmatrix_multtt(NspSpMatrix *A, NspSpMatrix *B)
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

static NspSpMatrix *BinaryOp(NspSpMatrix *A, NspSpMatrix *B, BopLeft BinLeft, BopBoth BinBoth, BopRight BinRight)
{ 
  int i,count,k1,k2,k;
  NspSpMatrix *Loc;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
      Loc =nsp_spmatrix_create(NVOID,type, A->m,A->n);
      if ( Loc == NULLSP ) return(NULLSP) ; 
      for ( i = 0 ; i < Loc->m ; i++ ) 
	{
	  int iest;
	  SpRow *Ai = A->D[i];
	  SpRow *Bi = B->D[i];
	  SpRow *Li = Loc->D[i];
	  iest= Min( A->n, A->D[i]->size+B->D[i]->size);
	  if (nsp_spmatrix_resize_row(Loc,i,(int)iest ) == FAIL) return(NULLSP) ;
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
	  if (nsp_spmatrix_resize_row(Loc,i,count) == FAIL) return(NULLSP) ;
	}
      return(Loc);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      return(NULLSP);
    }
}

/*
 * A = A.*B where B is a scalar sparse ( [] or scalar )
 */

int nsp_spmatrix_mult_scal(NspSpMatrix *A, NspSpMatrix *B)
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
	      FREE( A->D[i]->C);
	    }
	  A->D[i]->size =0;
	}
      FREE(A->D);
      A->m = A->n = A->mn = 0;
      return OK;
    }
  if ( B->rc_type == 'c' )
    {
      if (nsp_spmatrix_complexify(A) == FAIL ) return(FAIL);
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

NspMatrix *nsp_spmatrix_op_scal(NspSpMatrix *A, NspSpMatrix *B, int *flag, char op)
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



  
