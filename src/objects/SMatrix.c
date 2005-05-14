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
#include <ctype.h> /* tolower toupper */

#include "nsp/object.h"
#include "nsp/interf.h" /* for ret_endfor */
#include "nsp/pr-output.h" 
#include "nsp/matutil.h"
#include "nsp/gsort-p.h"
#include "nsp/matint.h"

/*
 * Creation of a NspSMatrix all the elements
 *	 are created with "." value or with str value according 
 *       to flag 
 * HINT : 
 * The array for storing the Strings is of size (m*n)+1 
 * and the last element is set to (char *) 0 
 * This can be used to detect the last element in XXX->S
 */

NspSMatrix* nsp_smatrix_create(nsp_const_string name, int m, int n,nsp_const_string str, int flag)
{
  int i;
  NspSMatrix *Loc = new_smatrix();
  nsp_const_string init; 
  nsp_const_string def="";
  if ( Loc == NULLSMAT) 
    { 
      Scierror("Error:\tRunning out of memory\n");
      return(NULLSMAT);
    }
 new_nsp_string(name);
  if ( ( NSP_OBJECT(Loc)->name =new_nsp_string(name)) == NULLSTRING) return(NULLSMAT);
  NSP_OBJECT(Loc)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /* Loc->otype = SMATRIX;
     Loc->ftype = SMatrix_Type;
  */
  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  if ( Loc->mn == 0 ) 
    {
      /* empty string Matrix */
      Loc->S = (nsp_string *) 0;
      Loc->m = Loc->n = 0;
      return(Loc);
    }
  if ((Loc->S = (nsp_string *) MALLOC((Loc->mn+1)* sizeof(nsp_string))) == (nsp_string *) 0 )
    { 
      Scierror("SMatCreate : Error no more space\n");
      return(NULLSMAT);
    }
  init = ( flag == 0) ? def : str ;
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ((Loc->S[i] =nsp_basic_to_string(init)) == NULLSTRING )  return(NULLSMAT);
    }
  /* Last element set to Null pointer **/
  Loc->S[Loc->mn]=(nsp_string) 0;
  return(Loc);
}

/*
 * Creation of a NspSMatrix all the elements ( if m*n != 0) 
 * are initialized as string of size strl (unfilled) 
 * Note that if strl is < 0 elements are not initialized 
 */

NspSMatrix*nsp_smatrix_create_with_length(nsp_const_string name, int m, int n, int strl)
{
  int i;
  NspSMatrix *Loc = new_smatrix();
  if ( Loc == NULLSMAT) 
    { 
      Scierror("Error:\tRunning out of memory\n");
      return(NULLSMAT);
    }
  if ( ( NSP_OBJECT(Loc)->name =new_nsp_string(name)) == NULLSTRING) return(NULLSMAT);
  NSP_OBJECT(Loc)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  Loc->m = Max(m,0);
  Loc->n = Max(n,0);
  Loc->mn=m*n;
  if ( Loc->mn == 0 ) 
    {
      Loc->m = Loc->n = 0;
      /* empty string Matrix */
      Loc->S = (nsp_string *) 0;
      return(Loc);
    }
  if ((Loc->S = (nsp_string *) MALLOC((Loc->mn+1)* sizeof(nsp_string))) == (nsp_string *) 0 )
    { 
      Scierror("Error:\tRunning out of memory\n");
      return(NULLSMAT);
    }
  if ( strl >= 0) 
    for ( i = 0 ; i < Loc->mn ; i++ ) 
      {
	if ((Loc->S[i] =new_nsp_string_n(strl)) == (nsp_string) 0 )  return(NULLSMAT);
      }
  /* Last element set to Null pointer **/
  Loc->S[Loc->mn]=(nsp_string) 0;
  return(Loc);
}

/*
 * Res =nsp_smatrix_create_from_table(T) 
 * T string table ended with (char *)0.
 */

NspSMatrix*nsp_smatrix_create_from_table(char **T)
{
  NspSMatrix *Loc;
  int i=0,count=0;
  while ( T[count] != NULL) count++;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_smatrix_create_with_length(NVOID,count,1,-1) ) == NULLSMAT) return(NULLSMAT);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < count ; i++ )
    {
      if ((Loc->S[ i] =nsp_string_copy(T[i])) == (nsp_string) 0) return(NULLSMAT);
    }
  return(Loc);
}

/*
 * Res =nsp_smatrix_create_from_array(n,T) 
 */

NspSMatrix* nsp_smatrix_create_from_array(nsp_const_string name,int n,const char **T)
{
  NspSMatrix *Loc;
  int i=0;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_smatrix_create_with_length(name,n,1,-1) ) == NULLSMAT) return(NULLSMAT);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < n ; i++ )
    {
      if ((Loc->S[i] =nsp_string_copy(T[i])) == (nsp_string) 0) return(NULLSMAT);
    }
  return(Loc);
}

/*
 * Res =nsp_smatrix_create_from_struct(T,size) 
 * T array of objects of size size  ended with NULL .
 * each object is supposed to start with a char* field.
 */

NspSMatrix*nsp_smatrix_create_from_struct(nsp_const_string name,const void *T,unsigned int size)
{
  NspSMatrix *Loc;
  char **entry;
  int i=0,count=0;
  for (entry = (char **) T, count = 0; *entry != NULL; entry = ((char **) (((char *) entry)+ size)), count++) {};
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_smatrix_create_with_length(name,count,1,-1) ) == NULLSMAT) return(NULLSMAT);
  /* allocate elements and store copies of A elements **/
  for (entry = (char **) T, i = 0 ; i < count ;entry = ((char **) (((char *) entry)+ size)), i++ )
    {
      if ((Loc->S[ i] =nsp_string_copy(*entry)) == (nsp_string) 0) return(NULLSMAT);
    }
  return(Loc);
}

/*
 * Res =nsp_smatrix_copy(A) 
 * Creates a Copy of NspSMatrix A : A is not checked 
 */

NspSMatrix*nsp_smatrix_copy(const NspSMatrix *A)
{
  int i;
  NspSMatrix *Loc;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_smatrix_create_with_length(NVOID,A->m,A->n,-1) ) == NULLSMAT) return(NULLSMAT);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ((Loc->S[ i] =nsp_string_copy(A->S[i])) == (nsp_string) 0) return(NULLSMAT);
    }
  return(Loc);
}


/*
 *nsp_smatrix_resize: Changes NspSMatrix dimensions
 * Warning : when m*n > A->mn this routine only enlarges the array 
 * of the NspSMatrix storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspSMatrix is changed 
 * return FAIL on failure 
 * should also work with A==[] XXXXX 
 */

int nsp_smatrix_resize(NspSMatrix *A, int m, int n)
{
  int i;
  if ( A->mn == m*n ) 
    {
      /* easy case : nothing to allocate **/
      if ( A->mn == 0) 
	{
	  A->m = A->n = 0;
	}
      else 
	{
	  A->m=m;
	  A->n=n;
	}
      return(OK);
    };
  if ( m*n < 0) return FAIL;
  if ( m*n < A->mn )
    {
      /* Clear before Realloc **/
      for ( i = m*n ; i < A->mn ; i++ )
	nsp_string_destroy(&(A->S[i]));
    }
  if ( m*n == 0 ) 
    {
        A->m =  A->n = A->mn= 0;
	FREE(A->S);
	return OK;
    }
  
  if ( A->mn == 0 ) 
    A->S = (nsp_string *)  MALLOC ((m*n+1) * sizeof(nsp_string));
  else 
    A->S = (nsp_string *)  REALLOC (A->S, (m*n+1) * sizeof(nsp_string));
  if ( A->S == (nsp_string *) 0) return(FAIL);

  /* Initialize new area **/
  A->S[(m*n)] = (nsp_string)0;
  for ( i = A->mn ; i < m*n ; i++ )
    {
      if ((A->S[i] =nsp_string_copy(".")) == ( nsp_string) 0 )  return(FAIL);
    }
  A->m =m ;
  A->n =n;
  A->mn=m*n ;
  if ( A->mn == 0) A->m = A->n = 0;
  return(OK);
}


/*
 * Delete the NspSMatrix A
 */

void nsp_smatrix_destroy(NspSMatrix *A)
{
  int i;
  if ( A == NULLSMAT) return;
  FREE(NSP_OBJECT(A)->name);
  if ( A-> mn != 0 ) 
    {
      for ( i = 0 ; i < A->mn ; i++ ) 
	{
	nsp_string_destroy(&(A->S[i]));
	}
      FREE(A->S);
    }
  FREE(A);
}



/*
 *nsp_smatrix_info: display Info on Matrix Mat 
 */

void nsp_smatrix_info(const NspSMatrix *Mat, int indent)
{
  int i;
  if ( Mat == NULLSMAT) 
    {
      Sciprintf("Null Pointer NspSMatrix \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) == 0) 
    Sciprintf("SMatrix (%dx%d) \n",Mat->m,Mat->n);
  else
    Sciprintf("SMatrix %s(%dx%d) \n",NSP_OBJECT(Mat)->name,Mat->m,Mat->n);
}

/*
 *nsp_smatrix_print: writes Mat Objet 
 */

void nsp_smatrix_print(const NspSMatrix *Mat, int indent)
{
  int i;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
	{
	  Sciprintf("%s=%s",NSP_OBJECT(Mat)->name,(Mat->mn==0 ) ? " m2s([])\n" : "" );
	}
    }
  else 
    {
      Sciprintf("%s\t=%s\t\ts (%dx%d)\n",NSP_OBJECT(Mat)->name,
		(Mat->mn==0 ) ? " []" : "",Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    nsp_print_internalSM (Mat,indent);
}


/*
 *nsp_smatrix_redim: Changes matrix dimensions
 * m*n must be unchanged 
 * The NspSMatrix is changed (m,n are changed ) 
 * return 0 on failure 
 */

int nsp_smatrix_redim(NspSMatrix *A, int m, int n)
{
  if ( A->mn ==  m*n ) 
    {
      A->m =m ;
      A->n =n;
      return(OK);
    }
  else 
    {
      Scierror("Error:\tCannot change size to (%dx%d) since matrix has %d elements\n"
	       ,m,n,A->mn);
      Scierror("SMatRedim : can't redim");
      return(FAIL);
    }
}



/*
 *nsp_smatrix_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for "." strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 * The result is stored in A 
 */

int nsp_smatrix_enlarge(NspSMatrix *A, int m, int n)
{
  if ( A->mn == 0) return nsp_smatrix_resize(A,m,n);
  if ( n > A->n  )
    if ( nsp_smatrix_add_columns(A,n- A->n) == FAIL) return(FAIL);
  if ( m > A->m  )  
    if ( nsp_smatrix_add_rows(A, m - A->m) == FAIL) return(FAIL);
  return(OK);
}

#define SameDim(SMAT1,SMAT2) ( SMAT1->m == SMAT2->m && SMAT1->n == SMAT2->n  )

/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 */

int nsp_smatrix_concat_right(NspSMatrix *A,const NspSMatrix *B)
{
  int Asize;
  Asize=A->mn;
  if ( A->m != B->m ) 
    {
      Scierror("Error:\tincompatible dimensions\n");
      return(FAIL);
    }
  if ( nsp_smatrix_resize(A,A->m,A->n+B->n) == FAIL) return(FAIL);
  if ( Scopy(B->mn,B->S,A->S+Asize) == FAIL) return(FAIL);
  return(OK);
}

int Scopy(int n, nsp_string *s1, nsp_string *s2)
{
  int i;
  for ( i = n-1 ; i >= 0 ; i--) 
    {
 nsp_string_destroy(&(s2[i] ));
      if ((s2[ i] =nsp_string_copy(s1[i])) == (nsp_string) 0)  return(FAIL);
    }
  return(OK);
}

/*
 *nsp_smatrix_add_columns: add n cols of zero to NspSMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

int nsp_smatrix_add_columns(NspSMatrix *A, int n)
{
  if (n == 0) return OK;
  else if ( n < 0) 
    {      
      Scierror("Error: Negative indice (%d) in MatAddCols\n",n);
      return FAIL;
    }
  if ( nsp_smatrix_resize(A,A->m,A->n+n) == FAIL) return(FAIL);
  /*  normalemeny inutile car Resize le fait deja 
     int Asize;
     Asize=A->mn;
     ns= (A->m)*n;
     if ( Sset(ns,".",A->S+Asize) == FAIL) return(FAIL);**/
  return(OK);
}

int Sset(int n, nsp_string s1, nsp_string *s2)
{
  int i;
  for ( i = 0 ; i < n ; i++) 
    {
 nsp_string_destroy(&(s2[i]));
      if ((s2[ i] =nsp_string_copy(s1)) == (nsp_string) 0)  return(FAIL);
    }
  return(OK);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

NspSMatrix*nsp_smatrix_concat_down(const NspSMatrix *A,const NspSMatrix *B)
{
  NspSMatrix *Loc;
  int j;
  if ( A->n != B->n ) 
    {
      Scierror("Error: [.;.] incompatible dimensions\n");
      return(NULLSMAT);
    }
  if ((Loc =nsp_smatrix_create(NVOID,A->m+B->m,A->n,"v",(int) 0)) == NULLSMAT) 
    return(NULLSMAT);
  for ( j = 0 ; j < A->n ; j++ ) 
    {
      if ( Scopy(A->m,A->S+j*A->m,Loc->S+j*(Loc->m)) == FAIL) 
	return(NULLSMAT);
      if ( Scopy(B->m,B->S+j*B->m,Loc->S+j*(Loc->m)+A->m) == FAIL)
	return(NULLSMAT);
    }
  return(Loc) ;
}

/*
 * Diag Concatenation
 * Res = [A,0;0,B]
 * return NULLBMAT on failure ( No more space )
 * A and B are left unchanged
 * XXXXXX : A Faire ????
 */

/*
 * Add Rows : Add m rows of zero to a NspSMatrix A 
 * A = [A;ones(m,n)]
 * return NULLSMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int nsp_smatrix_add_rows(NspSMatrix *A, int m)
{
  int Am;
  int j;
  if ( m == 0) return OK;
  else if ( m < 0) 
    {      
      Scierror("Error: Negative indice (%d) in SMatAddRows\n",m);
      return FAIL;
    }
  Am= A->m;
  if ( nsp_smatrix_resize(A,A->m+m,A->n)== FAIL) return(FAIL);
  for ( j = A->n-1  ; j >= 1 ; j-- ) 
    {
      if (  Scopy(Am,A->S+j*Am,A->S+j*(A->m)) == FAIL) 
	return(FAIL);
    }
  for ( j = A->n-2  ; j >= 0 ; j-- ) 
    {
      if (  Sset (m,".",A->S+j*(A->m)+Am) == FAIL)
	return(FAIL);
    }
  return(OK);
}

/*
*  A(Rows,Cols) = B 
*  A is changed and enlarged if necessary 
*  Rows and Cols are unchanged 
*  Size Compatibility is checked 
*/

int nsp_smatrix_set_submatrix(NspSMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspSMatrix *B)
{
  int rmin,rmax,cmin,cmax,i,j;
  if ( B->mn != 1)
    {
      if ( Rows->mn != B->m ||  Cols->mn != B->n )
	{
	  Scierror("Set incompatible indices ");
	  return(FAIL);
	}
    }
  Bounds(Rows,&rmin,&rmax);
  Bounds(Cols,&cmin,&cmax);
  if ( rmin < 1 || cmin < 1 ) 
    {
      Scierror("Error:\tNegative indices are not allowed\n");
      return(FAIL);
    }
  if ( rmax > A->m ||  cmax > A->n )
    if ( nsp_smatrix_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  if ( B->mn != 1) 
    for ( i = 0 ; i < Rows->mn ; i++)
      for ( j = 0 ; j < Cols->mn ; j++ )
	{
	nsp_string_destroy(&((A->S[((int) Rows->R[i])-1+ (((int) Cols->R[j])-1)*A->m])));
	  if (( A->S[((int) Rows->R[i])-1+ (((int)Cols->R[j])-1)*A->m] 
		=nsp_string_copy(B->S[i+B->m*j]))
	      == (nsp_string) 0)  return(FAIL);
	}
  else
    for ( i = 0 ; i < Rows->mn ; i++)
      for ( j = 0 ; j < Cols->mn ; j++ )
	{
	nsp_string_destroy(&((A->S[((int) Rows->R[i])-1+ (((int) Cols->R[j])-1)*A->m])));
	  if (( A->S[((int) Rows->R[i])-1+ (((int)Cols->R[j])-1)*A->m] 
		=nsp_string_copy(B->S[0]))
	      == (nsp_string) 0)  return(FAIL);
	}
  return(OK);
}


/*
 *  A(Rows) = B
 *  A is changed and enlarged if necessary
 *  Size Compatibility is checked
 */

int nsp_smatrix_set_rows(NspSMatrix *A, NspMatrix *Rows, NspSMatrix *B)
{
  int i,Bscal=0;
  if (GenericMatSeRo(A,A->m,A->n,A->mn,Rows,B,B->m,B->n,B->mn,
		     (F_Enlarge) nsp_smatrix_enlarge,&Bscal)== FAIL) 
    return FAIL;
  if ( Bscal == 0) 
    for ( i = 0 ; i < Rows->mn ; i++)
      {
	nsp_string_destroy(&((A->S[((int) Rows->R[i])-1])));
	if (( A->S[((int) Rows->R[i])-1] =nsp_string_copy(B->S[i]))
	    == (nsp_string) 0)  return(FAIL);
      }
  else
    for ( i = 0 ; i < Rows->mn ; i++)
      {
	nsp_string_destroy(&((A->S[((int) Rows->R[i])-1])));
	if (( A->S[((int) Rows->R[i])-1] =nsp_string_copy(B->S[0]))
	    == (nsp_string) 0)  return(FAIL);
      }
  return(OK);
}

/*
 *  A(:,Cols) = []
 *  A is changed.
 */

int nsp_smatrix_delete_columns(NspSMatrix *A, NspMatrix *Cols)
{
  int i, j, k, ij, *flag, new_A_n, count;

  if ( Cols->mn == 0) return OK;

  if ( (flag = nsp_complement_for_deletions(A->n, Cols, &count)) == NULL )
    return FAIL;

  new_A_n = A->n - count;

  k = 0;
  ij = 0;
  for ( j = 0 ; j < A->n  ; j++)
    if ( flag[j] )
      {
	if ( k < ij )
	  for ( i = 0 ; i < A->m ; i++ )
	    {
	      A->S[k] = A->S[ij];
	      A->S[ij] = NULLSTRING;
	      k++; ij++; 
	    }
	else
	  {
	    k += A->m ; j += A->m;
	  }
      }
    else
      for ( i = 0 ; i < A->m ; i++ )
	{
	nsp_string_destroy(&A->S[ij]);  /* DATADestroy: must NULLIFY the pointer */
	  ij++;
	}
  
  FREE(flag);

  if ( nsp_smatrix_resize(A, A->m, new_A_n) == FAIL ) 
    return FAIL;

  return OK;
}


/*
 *  A(Rows,:) = []
 *  A is changed.
 */

int nsp_smatrix_delete_rows(NspSMatrix *A, NspMatrix *Rows)
{
  int i, j, k, ij, *flag, new_A_m, count;

  if ( Rows->mn == 0) return OK;

  if ( (flag = nsp_complement_for_deletions(A->m, Rows, &count)) == NULL )
    return FAIL;

  new_A_m = A->m - count;

  k = 0;
  ij = 0;
  for ( j = 0 ; j < A->n  ; j++)
    for ( i = 0 ; i < A->m ; i++ )
      {
	if ( flag[i] )
	  {
	    if ( k < ij )
	      {
		A->S[k] = A->S[ij];
		A->S[ij] = NULLSTRING;
	      }
	    k++; 
	  }
	else
	nsp_string_destroy(&A->S[ij]);
	ij++;
      }

  FREE(flag);

  if ( nsp_smatrix_resize(A, new_A_m, A->n) == FAIL ) 
    return FAIL;

  return OK;
}

/*
 *  A(elts) = []
 *  A is changed.
 *  modified by Bruno (same modifs than in nsp_matrix_delete_elements).
 *  The algorithm uses now the function nsp_complement_for_deletions). 
 *  Indices from Elts do not need to be in increasing order.
 *  
 */

int nsp_smatrix_delete_elements(NspSMatrix *A, NspMatrix *Elts)
{
  int i,k,*flag, new_A_mn, count;

  if ( Elts->mn == 0) return OK;
  
  if ( (flag = nsp_complement_for_deletions(A->mn, Elts, &count)) == NULL )
    return FAIL;

  new_A_mn = A->mn - count;

  k = 0;
  for ( i = 0 ; i < A->mn ; i++ )
    if ( flag[i] )
      {
	if ( k < i )
	  {
	    A->S[k] = A->S[i];
	    A->S[i] =  NULLSTRING;
	  }
	k++;
      }
    else
      nsp_string_destroy(&A->S[i]);
  
  FREE(flag);

  if ( A->m == 1)
    {
      if ( nsp_smatrix_resize(A,1,new_A_mn) == FAIL ) return FAIL;
    }
  else
    {
      if ( nsp_smatrix_resize(A,new_A_mn,1) == FAIL) return FAIL;
    }
  return OK;
}

/* this one is more generic and could be used for cells, */

int nsp_smatrix_delete_elements1(NspSMatrix *A, NspMatrix *Elts)
{
  NspTypeBase *type;
  int i,k,*flag, new_A_mn, count;

  if ( Elts->mn == 0) return OK;
  
  if ( (flag = nsp_complement_for_deletions(A->mn, Elts, &count)) == NULL )
    return FAIL;

  if (( type = check_implements(A,nsp_type_matint_id)) == NULL )
    {
      Scierror("Object do not implements matint interface\n");
      return FAIL;
    }
    
  new_A_mn = A->mn - count;

  k = 0;
  for ( i = 0 ; i < A->mn ; i++ )
    if ( flag[i] )
      {
	if ( k < i )
	  {
	    A->S[k] = A->S[i];
	    A->S[i] =  NULLSTRING;
	  }
	k++;
      }
    else
      {
	MAT_INT(type)->free_elt((void **) &A->S[i]);
      }
  
  FREE(flag);

  if ( A->m == 1)
    {
      if ( MAT_INT(type)->resize(A,1,new_A_mn) == FAIL ) return FAIL;
    }
  else
    {
      if ( MAT_INT(type)->resize(A,new_A_mn,1) == FAIL) return FAIL;
    }
  return OK;
}


int nsp_smatrix_delete_elements3(NspSMatrix *A, NspMatrix *Elts)
{
  const int kout=-2;
  int i,*flag, new_A_mn, count,ks,kf,k;

  if ( Elts->mn == 0) return OK;
  
  if ( (flag = nsp_complement_for_deletions(A->mn, Elts, &count)) == NULL )
    return FAIL;
  new_A_mn = A->mn - count;

  for ( i = 0 ; i < A->mn ; i++ ) 
    {
      if ( flag[i]==0 ) nsp_string_destroy(&(A->S[i]));
    }

  ks=kf=kout;k=0;
  for ( i = 0 ; i < A->mn ; i++ ) 
    {
      if ( flag[i] == 0 ) 
	{
	  if ( kf == i -1 )
	    {
	      /* a starting zero zone after a non null zone */
	      if ( k!=ks) memmove(A->S+k,A->S+ks,(kf-ks+1)*(sizeof(double)));
	      k+= (kf-ks+1);
	      ks=kout;
	    }
	}
      else 
	{
	  /* accumulate elements in a != 0 zone */
	  if ( ks == kout ) 
	    ks=kf=i;
	  else 
	    kf++;
	}
    }
  if ( flag[A->mn-1] != 0)  memmove(A->S+k,A->S+ks,(kf-ks+1)*(sizeof(void *)));
  free(flag);

  if ( A->m == 1)
    {
      if ( nsp_smatrix_resize(A,1,new_A_mn) == FAIL ) return FAIL;
    }
  else
    {
      if ( nsp_smatrix_resize(A,new_A_mn,1) == FAIL ) return FAIL;
    }
  return OK;
}

/*
 * Res=nsp_smatrix_extract(A,Rows,Cols)
 * A, Rows and Cols are unchanged 
 */	

NspSMatrix*nsp_smatrix_extract(NspSMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  NspSMatrix *Loc;
  int rmin,rmax,cmin,cmax,i,j;
  if ( A->mn == 0) return nsp_smatrix_create(NVOID,0,0,"v",0);
  Bounds(Rows,&rmin,&rmax);
  Bounds(Cols,&cmin,&cmax);
  if ( rmin < 1 || cmin < 1 || rmax > A->m || cmax > A->n ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLSMAT);
    }
  if ((Loc =nsp_smatrix_create_with_length(NVOID,Rows->mn,Cols->mn, -1))== NULLSMAT) 
      return(NULLSMAT);
  for ( i = 0 ; i < Rows->mn ; i++)
    for ( j = 0 ; j < Cols->mn ; j++ )
      {
	if ((Loc->S[i+Loc->m*j] = 
	nsp_string_copy(A->S[((int) Rows->R[i])-1+(((int) Cols->R[j])-1)*A->m]))
	    == (nsp_string) 0 ) return(NULLSMAT);
      }
   return(Loc);
}



/*
 * Res=nsp_smatrix_extract_elements(A,Elts)
 * A unchanged, Elts
 */	

NspSMatrix*nsp_smatrix_extract_elements(NspSMatrix *A, NspMatrix *Elts, int *err)
{
  NspSMatrix *Loc;
  int rmin,rmax,i;
  Bounds(Elts,&rmin,&rmax);
  *err=0;
  if ( A->mn == 0) return nsp_smatrix_create(NVOID,0,0,"v",0);
  if ( rmin < 1 || rmax > A->mn )
    {
      *err=1;
      return(NULLSMAT);
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      if ((Loc =nsp_smatrix_create_with_length(NVOID,1,Elts->mn,-1)) ==NULLSMAT) return(NULLSMAT);
    }
  else
    {
      if ( (Loc =nsp_smatrix_create_with_length(NVOID,Elts->mn,1,-1)) == NULLSMAT) return(NULLSMAT);
    }
  for ( i = 0 ; i < Elts->mn ; i++)
    { 
      if ((Loc->S[i] =nsp_string_copy(A->S[((int) Elts->R[i])-1]))== (nsp_string) 0)  return(NULLSMAT);
    }
  return(Loc);
}

/*
 * Res=nsp_smatrix_extract_columns(A,Cols,err)
 * A unchanged
 */

NspSMatrix*nsp_smatrix_extract_columns(NspSMatrix *A, NspMatrix *Cols, int *err)
{
  NspSMatrix *Loc;
  int j,cmin,cmax;
  *err=0;
  if ( A->mn == 0) return nsp_smatrix_create(NVOID,0,0,"v",0);
  Bounds(Cols,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->n )
    {
      *err=1;
      return(NULLSMAT);
    }
  if ((Loc =nsp_smatrix_create_with_length(NVOID,A->m,Cols->mn,-1))==NULLSMAT)  return(NULLSMAT);
  for ( j = 0 ; j < Cols->mn ; j++ )
    {
      int ind=(((int) Cols->R[j])-1)*A->m, i, ind1=Loc->m*j;
      for ( i = A->m -1 ; i >= 0 ; i--) 
	{
	  if (( Loc->S[ind1+i] =nsp_string_copy( A->S[ind+i])) == (nsp_string) 0)  return NULLSMAT;
	}
    }
  return(Loc);
}

/*
 * A1=SMatLoopCol(A1,M,i,rep)
 * Used in for loops
 */

NspSMatrix*SMatLoopCol(char *str, NspSMatrix *Col, NspSMatrix *A, int icol, int *rep)
{
  register int iof;
  NspSMatrix *Loc;
  if ( icol > A->n )
    {
      *rep = RET_ENDFOR;
      return(NULLSMAT);
    }
  *rep =0;
  if ( Col == NULLSMAT)
    Loc =nsp_smatrix_create(str,A->m,1,"v",(int)0);
  else
    Loc = Col;
  if ( Loc == NULLSMAT) return(NULLSMAT);
  iof = (icol-1)*A->m;
  if ( Scopy(A->m,A->S + iof ,Loc->S) == FAIL) return NULLSMAT;
  return(Loc);
}

/*
 * Res=nsp_smatrix_extract_rows(A,Rows,err)
 * A unchanged
 */

NspSMatrix*nsp_smatrix_extract_rows(NspSMatrix *A, NspMatrix *Rows, int *err)
{
  NspSMatrix *Loc;
  int i,j,cmin,cmax;
  *err=0;
  if ( A->mn == 0) return nsp_smatrix_create(NVOID,0,0,"v",0);
  Bounds(Rows,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->m )
    {
      *err=1;
      return(NULLSMAT);
    }
  if ((Loc =nsp_smatrix_create_with_length(NVOID,Rows->mn,A->n,-1)) == NULLSMAT )   return(NULLSMAT);
  for ( i = 0 ; i < Rows->mn ; i++)
    for ( j = 0 ; j < A->n ; j++ )
      {
	if (( Loc->S[i+ j*Loc->m]=nsp_string_copy(A->S[(((int) Rows->R[i])-1)+ j*A->m]))  == (nsp_string) 0)  return NULLSMAT;
      }
  return(Loc);
}
 
/*
 * Res=new_nsp_string(str) 
 * Creates a copy of str in Res or (char*)0 if no more memory 
 */

nsp_string new_nsp_string(nsp_const_string str)
{
  nsp_string loc;
  if (( loc =new_nsp_string_n(strlen(str))) == (nsp_string) 0) return(loc);
  strcpy(loc,str);
  return((char*) loc);
}

nsp_string nsp_basic_to_string(nsp_const_string str)
{
  nsp_string loc;
  if (( loc =new_nsp_string_n(strlen(str))) == (nsp_string) 0) return(loc);
  strcpy((char*) loc,(char*) str);
  return(loc);
}

nsp_string nsp_string_copy(nsp_const_string str)
{
  nsp_string loc;
  int n= strlen((char*) str);
  if (( loc =new_nsp_string_n(n)) == (nsp_string) 0) return(loc);
  /* memcpy((void*) loc,(void*) str, (n+1)*sizeof(char)); **/
  strcpy((char*) loc,(char*) str); 
  return(loc);
}

void nsp_string_destroy(nsp_string *str)
{
  FREE(*str);
  *str = NULLSTRING;
}

/*
 * Res=new_nsp_string_n(n) 
 * Creates a string of size (n+1) : i.e to put n chars + '\0'
 * returns the string or (char*) 0
 */

nsp_string new_nsp_string_n(int n)
{
  nsp_string loc;
  if ((loc = (nsp_string) MALLOC((n+1)*sizeof(char))) == (nsp_string) 0) 
    { 
      Scierror("NewString : Error no more space\n");
      return(NULLSTRING);
    }
  return(loc);
}


/*
 * int nsp_string_resize(Hstr,n) 
 * resize the string Hstr to size (n+1)
 * returns OK or FAIL 
 * Hstr is not Checked (MUST BE != NULLSTRING )
 */

int nsp_string_resize(nsp_string *Hstr, unsigned int n)
{
  nsp_string loc;
  if ( ( loc = (nsp_string) REALLOC( *Hstr, (n+1)* sizeof(char))) == NULLSTRING) 
    { 
      Scierror("Error:\tSring resize, no more memory\n");
      return(FAIL);
    }
  *Hstr = loc;
  return(OK);
}



/*
 * int = SMatConcatTT
 * Term to term concatenation 
 * A(i;j) = "A(i;j)strB(i;j)" : A is changed  B unchanged 
 * str unchanged : is used if flag == 1
 * Need to be improved to accept: 
 *    A(i,j)+B(1,1) and A(1,1)+B(i,j)
 * XXXXXXX
 */

int nsp_smatrix_concat_strings(NspSMatrix *A, NspSMatrix *B,nsp_const_string str, int flag)
{
  int i, str_l = (flag == 1) ? strlen(str) : 0;
  if ( A->mn != B->mn ) 
    {
      Scierror("Error:\tIncompatible dimensions for concatenation\n");
      return(FAIL);
    }
  for ( i = 0 ; i < A->mn ; i++) 
    {	  
      if (nsp_string_resize(&(A->S[i]),(unsigned) strlen(A->S[i])+strlen(B->S[i])+str_l)
	  == FAIL )  return(FAIL);
      if (flag == 1) strcat( A->S[i],str);
      strcat(A->S[i],B->S[i]);
    }
  return(OK);
}

/* here B->mn = 1 and  A = A + B */

int nsp_smatrix_concat_string_right(NspSMatrix *A, NspSMatrix *B,nsp_const_string str, int flag)
{
  char **S = A->S;
  char *strB = B->S[0];
  int str_l = (flag == 1) ? strlen(str) : 0;
  while ( *S != NULL) 
    {	  
      if (nsp_string_resize(S,(unsigned) strlen(*S)+str_l+ strlen(strB)) == FAIL )  return(FAIL);
      if (flag == 1) strcat(*S,str);
      strcat(*S,strB);
      S++;
    }
  return OK ;
}

int nsp_smatrix_concat_string_left(NspSMatrix *A, NspSMatrix *B,nsp_const_string str, int flag)
{
  char **S = A->S;
  char *strB = B->S[0];
  int i, str_l = (flag == 1) ? strlen(str) : 0;
  while ( *S != NULL) 
    {	  
      int ofset=str_l+ strlen(strB);
      if (nsp_string_resize(S,(unsigned) strlen(*S)+ofset) == FAIL )  return(FAIL);
      for ( i = strlen(*S) ; i >=0  ; i--) 
	(*S)[i+ofset]=(*S)[i];
      for ( i = 0 ; i < strlen(strB) ; i++) 
	(*S)[i]= strB[i];
      for ( i = 0 ; i < str_l ; i++) (*S)[strlen(strB)+i]=str[i];
      S++;
    }
  return OK ;
}


/*
 * Res =nsp_smatrix_strcmp(A,B) 
 *  Res[i;j] = strcmp(A[i;j],B[i;j]) 
 */

NspMatrix *nsp_smatrix_strcmp(NspSMatrix *A, NspSMatrix *B)
{
  int i;
   NspMatrix *Loc;
  if ( A->mn != B->mn ) 
    {
      Scierror("Concattt for strin incompatible dimensions ");
      return(NULLMAT);
    }
  if ( ( Loc = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return(NULLMAT);
  for ( i = 0 ; i < A->mn ; i++) 
    {
      Loc->R[i]= strcmp(A->S[i],B->S[i]);
    }
  return(Loc);
}

/*
 * Res=nsp_smatrix_column_concat(A) 
 * returns a mx1 matrix such that Res(i) = "A(i,1)str A(i,2)str  ...A(i,n)"
 * white spaces are inserted 
 * str is used if flag =1 
 */

NspSMatrix*nsp_smatrix_column_concat_padded(NspSMatrix *A,nsp_const_string str, int flag)
{
  int i,j,lentot=0;
  NspSMatrix *Loc;
  NspMatrix *Lw;
  if ((Loc=nsp_smatrix_create(NVOID,A->m,1,"v",0))  == NULLSMAT) return(NULLSMAT);
  if ((Lw= nsp_matrix_create(NVOID,'r',A->n,1)) == NULLMAT) return(NULLSMAT);
  /* evaluation of sizes */
  for ( j = 0 ; j < A->n ; j++ ) 
    {
      int len = 0;
      for ( i = 0 ; i < A->m ; i++ )
	{
	  int l;
	  l = strlen(A->S[i+j*A->m]);
	  if ( l > len ) len = l;
	}
      Lw->R[j]= len;
      lentot +=len ;
    }
  if ( flag == 1) lentot += (A->n -1)*strlen(str);
  /* New matrix creation */
  for ( i = 0 ; i < Loc->m ; i++ )
    {
      if (nsp_string_resize(&(Loc->S[i]),(unsigned)lentot) == FAIL) return(NULLSMAT);
      for ( j = 0 ; j < A->n -1 ; j++ ) 
	{
	  int len = (int) anint(Lw->R[j]);
	  sprintf(Loc->S[i],"%*s",len,A->S[i+j*A->m]);
	  if ( flag == 1) 
	    strcat(Loc->S[i],str);
	}
      strcat( Loc->S[i],A->S[i+ (A->n-1)*A->n]);
    }
  return(Loc);
}

/*
 * Catenate the columns of A whitout white space padding 
 * and using str char as separtor ( if flag == 1) 
 */

NspSMatrix*nsp_smatrix_column_concat(NspSMatrix *A,nsp_const_string str, int flag)
{
  int i,j,*Iloc;
  NspSMatrix *Loc;
  NspMatrix *Lw;
  if ((Loc=nsp_smatrix_create(NVOID,A->m,1,"v",0))  == NULLSMAT) return(NULLSMAT);
  if ((Lw = nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT) return(NULLSMAT);
  Iloc = (int *) Lw->R;
  /* evaluation of sizes */
  for ( i = 0 ; i < A->m ; i++ )
    {
      int len = 0;
      for ( j = 0 ; j < A->n ; j++ ) 
	len += strlen(A->S[i+j*A->m]);
      if ( flag == 1) len += (A->n -1)*strlen(str);
      Iloc[i]= len;
    }
  /* New matrix creation */
  for ( i = 0 ; i < Loc->m ; i++ )
    {
      if (nsp_string_resize(&(Loc->S[i]),(unsigned) Iloc[i] ) == FAIL) return(NULLSMAT);
      strcpy(Loc->S[i],A->S[i]);
      for ( j = 1 ; j < A->n ; j++ ) 
	{
	  if ( flag == 1) strcat(Loc->S[i],str);
	  strcat(Loc->S[i],A->S[i+j*A->m]);
	}
    }
  nsp_matrix_destroy(Lw);
  return(Loc);
}

/*
 * Catenate the rows of A whitout white space padding 
 * and using str char as separtor ( if flag == 1) 
 */

NspSMatrix*nsp_smatrix_row_concat(NspSMatrix *A,nsp_const_string str, int flag)
{
  int i,j,*Iloc;
  NspSMatrix *Loc;
  NspMatrix *Lw;
  if ((Loc=nsp_smatrix_create(NVOID,1,A->n,"v",0))  == NULLSMAT) return(NULLSMAT);
  if ((Lw = nsp_matrix_create(NVOID,'r',1,A->n)) == NULLMAT) return(NULLSMAT);
  Iloc = (int *) Lw->R;
  /* evaluation of sizes */
  for ( j = 0 ; j < A->n ; j++ ) 
    {
      int len = 0;
      for ( i = 0 ; i < A->m ; i++ )
	{
	  len += strlen(A->S[i+j*A->m]);
	  if ( flag == 1) len += (A->m -1)*strlen(str);
	}
      Iloc[j]= len;
    }
  /* New matrix creation */
  for ( j = 0 ; j < Loc->n ; j++ )
    {
      if (nsp_string_resize(&(Loc->S[j]),(unsigned) Iloc[j] ) == FAIL) return(NULLSMAT);
      strcpy(Loc->S[j],A->S[j*A->m]);
      for ( i = 1 ; i < A->m ; i++ )
	{
	  if ( flag == 1) strcat(Loc->S[j],str);
	  strcat(Loc->S[j],A->S[i+j*A->m]);
	}
    }
  nsp_matrix_destroy(Lw);
  return(Loc);
}

/*
 * Res=nsp_smatrix_elts_concat(A,....) 
 * catenate all the elements row by row 
 * rstr is the row-separator and cflag the col-separator 
 */

nsp_string nsp_smatrix_elts_concat(NspSMatrix *A,nsp_const_string rstr, int rflag,
				   nsp_const_string cstr, int cflag)
{
  int i,j,lentot=0;
  nsp_string Loc;
  /* evaluation of sizes */
  for ( j = 0 ; j < A->mn ; j++ )  lentot += strlen(A->S[j]);
  if ( rflag == 1) lentot += (A->m -1)*strlen(rstr);
  if ( cflag == 1) lentot += (A->m*(A->n -1))*strlen(cstr);
  /* New String */
  if ((Loc =new_nsp_string_n(lentot)) == (nsp_string) 0 )  return(NULL);
  strcpy(Loc,"");
  for ( i = 0 ; i < A->m ; i++ )
    {
      for ( j = 0 ; j < A->n ; j++ )
	{
	  strcat(Loc,A->S[i+(A->m)*j]);
	  if ( cflag == 1 && j != A->n-1 ) strcat(Loc,cstr);
	}
      if ( rflag == 1 && i != A->m-1 ) strcat(Loc,rstr);
    }
  return(Loc);
}

/*
 * Res= Part(A,Ind) 
 * part function of Scilab A is  unchanged 
 * Ind unchanged 
 */


NspSMatrix*nsp_smatrix_part(NspSMatrix *A, NspMatrix *Ind)
{
  int i,k;
  int ind;
  NspSMatrix *Loc;
  if ((Loc =nsp_smatrix_create_with_length(NVOID,A->m,A->n,Ind->mn)) == NULLSMAT)
    return(Loc);
  for ( i = 0 ; i < A->mn ; i ++) 
    {
      for ( k =0; k < Ind->mn ; k++ ) 
	{
	  ind = ((int) Ind->R[k])-1;
	  if ( ind < 0 || ind >= (int) strlen(A->S[i]))
	    Loc->S[i][k]=' ';
	  else 
	    Loc->S[i][k] = A->S[i][ind];
	}
      Loc->S[i][Ind->mn]='\0';
    }
  return(Loc);
}




/*
 * Res= length(A) 
 * return a matrix which contains the length of the strings 
 * contained in A  A unchanged 
 * for Poly length menas degre of each polynom 
 */

NspMatrix *nsp_smatrix_elts_length(NspSMatrix *A)
{
  int i;
  NspMatrix *Loc;
  Loc = nsp_matrix_create(NVOID,'r',A->m,A->n);
  if ( Loc == NULLMAT) { return(Loc);}
  for ( i = 0 ; i < A->mn ; i++) 
    {
      Loc->R[i] = strlen(A->S[i]) ;
    }
  return(Loc);
}


/*
 *  Res=nsp_matrix_to_smatrix(A) 
 *      A s not changed 
 *  str is a format which is used if flag == 1 
 *  should be replaced by a null test on str. 
 *  FIXME: the default format should be the format 
 *    used for printing which is computed from A 
 */

NspSMatrix*nsp_matrix_to_smatrix(NspMatrix *A,nsp_const_string str, int flag)
{
  char buf[1024],formati[256];
  nsp_const_string def="%f", defi ="%f +%fi", format;
  int i;
  NspSMatrix *Loc;
  if (flag == 1 )
    {
      if ( A->rc_type == 'r') 
	format = str;
      else 
	sprintf(formati,"%s +%si",str,str);
    }
  else 
    {
      if  ( A->rc_type == 'r') 
	format = def;
      else 
	strcpy(formati,defi);
    }
  if ((Loc =nsp_smatrix_create_with_length(NVOID,A->m,A->n,-1)) == NULLSMAT) 
    return(NULLSMAT);
  for ( i = 0 ; i < Loc->mn ; i++ )
    {
      if ( A->rc_type == 'r') 
	sprintf(buf,format,A->R[i]);
      else 
	sprintf(buf,formati,A->C[i].r,A->C[i].i);
      if ((Loc->S[i] =nsp_basic_to_string(buf)) == (nsp_string) 0)  return(NULLSMAT);
    }
  return(Loc);
}

/*
 * A=tolower(A) 
 */

void nsp_smatrix_tolower(NspSMatrix *A)
{
  int i;
  unsigned int j;
  for ( i = 0 ; i < A->mn ; i++ ) 
      if ( A->S[i] != (nsp_string) 0 ) 
	for ( j = 0 ; j < strlen(A->S[i]) ; j++ ) A->S[i][j]= tolower(A->S[i][j]);
}


/*
 * A=toupper(A) 
 */

void nsp_smatrix_toupper(NspSMatrix *A)
{
  int i;
  unsigned int j;
  for ( i = 0 ; i < A->mn ; i++ ) 
    if ( A->S[i] != (nsp_string) 0) 
      for ( j = 0 ; j < strlen(A->S[i]) ; j++ )  A->S[i][j]= toupper(A->S[i][j]);
}



/*
 * capitalize(A)
 */

void nsp_smatrix_capitalize(NspSMatrix *A)
{
  int i;
  unsigned int j;
  for ( i = 0 ; i < A->mn ; i++ ) 
    if ( A->S[i] != (nsp_string) 0) 
      {
	A->S[i][0]= toupper(A->S[i][0]);
	for ( j = 1 ; j < strlen(A->S[i]) ; j++ ) A->S[i][j]= tolower(A->S[i][j]);
      }
}

/*
 * Res=nsp_smatrix_strstr(A,Str)
 * Returns in Res[i]  the index of Str in A[i] 
 * or 0 if Str is not in A[i]
 */


NspMatrix *nsp_smatrix_strstr(NspSMatrix *A,nsp_const_string Str)
{
  NspMatrix *Loc;
  int i;
  if ((Loc =  nsp_matrix_create(NVOID,'r',A->m,A->n)) == NULLMAT ) return NULLMAT;
  for ( i = 0 ; i < A->mn ; i++ ) 
    {
      char *s;
      s = strstr(A->S[i],Str) ;
      if ( s == NULL) 
	Loc->R[i] = 0;
      else 
	Loc->R[i]= s - A->S[i]+1;
    }
  return Loc;
}

/*
 * Res=nsp_smatrix_strindex(Str, Motif)
 * Returns in Res  the indices of Motif in Str
 * or[] if Motif is not in Str.
 * Contributed by Bruno (to get 
 */

NspMatrix *nsp_smatrix_strindex(nsp_const_string Str,nsp_const_string Motif)
{
  NspMatrix *Loc;
  int k, count, maxcount, lstr = strlen(Str), lmotif = strlen(Motif);
  char *s;

  if (lmotif == 0 || lmotif > lstr) 
    {
      if ((Loc =  nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT ) 
	return NULLMAT;
      else
	return Loc;
    }

  /* usual case lmotif > 0 and lmotif <= lstr */
  maxcount = lstr/lmotif;
  if ((Loc =  nsp_matrix_create(NVOID,'r',1,maxcount)) == NULLMAT ) 
    return NULLMAT;

  k = 0; count = 0;
  while ( lmotif <= lstr-k )
    {
      s = strstr(&(Str[k]),Motif);
      if ( s == NULL) 
	break;
      else
	{
	  k = s - Str;
	  Loc->R[count] = k+1;
	  count++; 
	  k += lmotif;
	}
    }

  /* resize result, will take care of count == 0  */
  if ( nsp_matrix_resize(Loc, 1, count) == FAIL )
    return NULLMAT;

  return Loc;
}

/*
 * Ascii2SMat : Create a string with 
 *    entries of A which are considered as ascii codes 
 */

NspSMatrix*nsp_ascii_to_smatrix(NspMatrix *A)
{
  int i;
  NspSMatrix *Loc;
  if ( ( Loc =nsp_smatrix_create_with_length(NVOID,1,1,A->mn)) == NULLSMAT) return(NULLSMAT);
  for ( i = 0 ; i < A->mn ; i++ )
    Loc->S[0][i]= (int) A->R[i];
  Loc->S[0][A->mn] = '\0';
  return Loc;
}

/*
 * SMat2Ascii : Create a Row vector with ascii 
 *    Codes of string A 
 *    A is supposed to be a string check is not done 
 */

NspMatrix *nsp_string_to_ascii(nsp_const_string S)
{
  unsigned int i;
  NspMatrix *Loc;
  if ((Loc = nsp_matrix_create(NVOID,'r',1,strlen(S))) == NULLMAT) return(NULLMAT);
  for ( i = 0 ; i < strlen(S) ; i++ )
    Loc->R[i] = (int) S[i];
  return Loc;
}

/*
 * SMatSort : Index=Sort(A)
 * A is changed, Index created with the indexes 
 * return NULLMAT on error 
 * WARNING : A must be real but the test is not done here 
 * ======
 */

NspMatrix *nsp_smatrix_sort(NspSMatrix *A,int flag,nsp_const_string str1,nsp_const_string str2)
{
  int iflag=0,inc=-1,*iloc=NULL,Locm=A->m,Locn=A->n;
  NspMatrix *Loc=NULL;
  if ( flag == 2 )
    {
      if ( str1[0] == 'l' ) 
	{
	  if ( str1[1] == 'r' ) Locn=Min(Locn,1);
	  else Locm=Min(Locm,1);
	}
      if ((Loc= nsp_matrix_create(NVOID,'r',Locm,Locn))  == NULLMAT) return(NULLMAT);
      iflag = 1;
      iloc =(int *) Loc->R;  
    }
  C2F(gsorts)(A->S,iloc,&iflag,&A->m,&A->n,str1,str2);
  if ( flag == 2) 
    {
      /* Loc contains ints, NOTE inc = -1 **/
      nsp_int2double(&Loc->mn,iloc,&inc,Loc->R,&inc);
    }
  return Loc;
}

/*
 * SMatSplit : M=Split(S,chars) 
 * S is a String.  * chars a set of splitting chars 
 * M is a Vector which results from the splitting of S
 */

NspSMatrix*nsp_smatrix_split(nsp_const_string string,nsp_const_string splitChars)
{
  register nsp_const_string p, p2;
  nsp_const_string elementStart;
  int  splitCharLen, stringLen, i, j;
  NspSMatrix *A;
  stringLen = strlen(string);
  splitCharLen = strlen(splitChars);
  if (splitCharLen == 0) 
    {
      /* split to stringLen chars :
       *  split('foo','') --> ['f','o','o']
       */
      if ((A=nsp_smatrix_create(NVOID,1,stringLen,".",0))== NULLSMAT) 
	return NULLSMAT;
      for (i = 0 ;  i < stringLen;  i++) A->S[i][0] = string[i];
    }
  else 
    {
      /* split with split characters 
       * ex: split('foo pooumou',' u') --> ['foo','poo','mou']
       */
      int col=0;
      if ((A=nsp_smatrix_create(NVOID,0,0,".",0))== NULLSMAT) 
	return NULLSMAT;
      for (i = 0, p = elementStart = string;  i < stringLen;  i++, p++) {
	for (j = 0, p2 = splitChars;  j < splitCharLen;  j++, p2++) {
	  if (*p2 == *p) {
	    col++;
	    if ( nsp_smatrix_resize(A,1,col) == FAIL) return(NULLSMAT);
	    if (nsp_string_resize(&A->S[col-1],(p-elementStart))== FAIL) 
	      return(NULLSMAT);
	    strncpy( A->S[col-1],elementStart,(p-elementStart));
	    A->S[col-1][p-elementStart]='\0';
	    elementStart = p+1;
	    break;
	  }
	}
      }
      if (p != string) {
	int remainingChars = stringLen - (elementStart-string);
	col++;
	if ( nsp_smatrix_resize(A,1,col) == FAIL) return(NULLSMAT);
	if (nsp_string_resize(&A->S[col-1], remainingChars)== FAIL) 
	  return(NULLSMAT);
	strncpy( A->S[col-1],elementStart, remainingChars);
	A->S[col-1][remainingChars]='\0';
      }
    }
  return A;
}

/*
 * Add string str at the end of column string vector A 
 */

int nsp_row_smatrix_append_string(NspSMatrix *A,nsp_const_string str)
{
  if ( nsp_smatrix_resize(A,A->m+1,1) == FAIL) return FAIL;
  if (nsp_string_resize(&A->S[A->m-1],strlen(str)) ==  FAIL) return FAIL;
  strcpy( A->S[A->m-1],str);
  return OK;
}


/*
 * Comparison operators
 */

/* Operations **/

static int Lt(char * a, char * b) {  return( strcmp(a,b) < 0);}
static int Le(char * a, char * b) {  return( strcmp(a,b) <= 0);}
static int Eq(char * a, char * b) {  return( strcmp(a,b) == 0);}
static int NEq(char * a, char * b) {  return(strcmp(a,b) != 0);}
static int Gt(char * a, char * b) {  return( strcmp(a,b) > 0);}
static int Ge(char * a, char * b) {  return( strcmp(a,b) >= 0);}

typedef int (SMat_CompOp) (char *,char *);

/* typedef int (SMat_CompOp) (char *,char *); */

typedef struct cpt {
  char *name;
  SMat_CompOp *fonc,*foncop;
} SMatCompTab;

/* Warning : sorted tab **/ 

static SMatCompTab comptab[] = {
  {"<",Lt   ,Ge  },
  {"<=",Le  ,Gt  },
  {"<>",NEq ,Eq  },
  {"==",Eq  ,NEq },
  {">",Gt   ,Le  },
  {">=",Ge  ,Lt  },
  {(char *) NULL , 0,0}
}; 

static int SMatSearchComp(char *op, SMat_CompOp (**comp_op))
{
  int i=0;
  while ( comptab[i].name != (char *) NULL)
    {
      int j;
      j = strcmp(op,comptab[i].name);
      if ( j == 0 )
	{
	  *comp_op = comptab[i].foncop;
	  return(OK);
	}
      else
	{ 
	  if ( j <= 0)
	    {
	      Sciprintf("\nUnknow comp operator <%s>\n",op);
	      return(FAIL);
	    }
	  else i++;
	}
    }
  Sciprintf("\n Unknow comp operator <%s>\n",op);
  return(FAIL);
}


/*
 * Operation on String Matrices leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j) 
 * with the special case : 
 *      A(i;j)op B(0,0) or A(0,0) op B(i,j) if A or B are of size 1x1
 *      
 * A and B are unchanged : Res is created 
 */

NspBMatrix  *SMatCompOp(NspSMatrix *A, NspSMatrix *B, char *op)
{
  SMat_CompOp *comp_op;
  int i;
  NspBMatrix *Loc ;
  if ( SMatSearchComp(op,&comp_op) == FAIL) return(NULLBMAT);
  if ( A->mn != B->mn)
    {
      if ( B->mn == 1 && A->mn != 0  ) 
	{
	  /* Special case B is a constant, Loc created with true */
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) { return(NULLBMAT);   }
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*comp_op)(A->S[i],B->S[0]) ) Loc->B[i] = FALSE;
	  return(Loc);
	}
      if ( A->mn == 1 && B->mn != 0) 
	{
	  /* Special case A is a constant */
	  Loc =nsp_bmatrix_create(NVOID,B->m,B->n);
	  if ( Loc == NULLBMAT)     { return(NULLBMAT);  }
	  for ( i = 0 ; i < B->mn ; i++ )  
	    if ( (*comp_op)(A->S[0],B->S[i]) ) Loc->B[i] = FALSE;
	  return(Loc);
	}
      /* Incompatible dimensions **/
      if ( strcmp(op,"==") == 0) 
	{
	  if ((Loc =nsp_bmatrix_create(NVOID,1,1))== NULLBMAT)return(NULLBMAT);
	  Loc->B[0] = FALSE;
	  return Loc;
	}
      else if ( strcmp(op,"<>") == 0) 
	{
	  if ((Loc =nsp_bmatrix_create(NVOID,1,1))== NULLBMAT)return(NULLBMAT);
	  Loc->B[0] = TRUE ;
	  return Loc;
	}
      else 
	{
	  Scierror("Error:\tIncompatible dimensions\n");
	  return( NULLBMAT);
	}
    }
  else 
    {
      /* A and B are of same dimensions */
      if ( A->mn == 0) 
	{
	  Loc =nsp_bmatrix_create(NVOID,1,1);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  if ( (*comp_op)("","")) Loc->B[0] = FALSE;
	}
      else
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*comp_op)(A->S[i],B->S[i])) Loc->B[i] = FALSE;
	}
    }
  return(Loc);
}

/* 
 * Deuxieme chose a faire :  la meme chose mais Globale
 *  Le resultat est un booleen qui vaut True si 
 *  A(i;i) <= B(i,j) pour tout les (i,j) 
 */ 

int SMatFullComp(NspSMatrix *A, NspSMatrix *B, char *op,int *err)
{
  SMat_CompOp *comp_op;
  int i, rep = TRUE;
  *err=0;
  if ( SMatSearchComp(op,&comp_op) == FAIL) return FALSE;
  if ( A->mn != B->mn)
    {
      if ( B->mn == 1 ) 
	{
	  for ( i = 0 ; i < A->mn ; i++ )  
	    {
	      if ( (*comp_op)(A->S[i],B->S[0]) ) 
		{ 
		  rep = FALSE;
		  break;
		}
	    }
	  return rep;
	}
      if ( A->mn == 1) 
	{
	  for ( i = 0 ; i < B->mn ; i++ )  
	    {
	      if ( (*comp_op)(A->S[i],B->S[0]) ) 
		{ 
		  rep = FALSE;
		  break;
		}
	    }
	  return rep ;
	}
      /* Scierror("Error:\tIncompatible dimensions\n");*/
      *err=1;
      return FALSE ;
    }
  else 
    {
      for ( i = 0 ; i < A->mn ; i++ )  
	{
	  if ( (*comp_op)(A->S[i],B->S[i]) ) 
	    {
	      rep =  FALSE;
	      break ;
	    }
	}
    }
  return rep;
}



/*
 * Res =nsp_smatrix_transpose(A) 
 * Transpose A 
 */

NspSMatrix*nsp_smatrix_transpose(const NspSMatrix *A)
{
  int i,j;
  NspSMatrix *Loc;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_smatrix_create_with_length(NVOID,A->n,A->m,-1) ) == NULLSMAT) return(NULLSMAT);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < Loc->m ; i++ )
    for ( j = 0 ; j < Loc->n ; j++ )
      {
	if ((Loc->S[i+(Loc->m)*j] =nsp_string_copy(A->S[j+(A->m)*i])) == (nsp_string) 0) return(NULLSMAT);
      }
  return(Loc);
}

/*
 * Res =nsp_smatrix_subst(A,search,replace) 
 */

NspSMatrix *nsp_smatrix_subst(const NspSMatrix *A,const char *needle,const char *replace)
{
  /* modified by Bruno (30 april 05) for a gain in speed */
  int i, more, len_needle = strlen(needle), len_replace = strlen(replace);
  NspSMatrix *Loc;
  more = len_replace - len_needle;
  /* initial mxn matrix with unallocated elements **/
  if ( ( Loc =nsp_smatrix_create_with_length(NVOID,A->m,A->n,-1) ) == NULLSMAT) 
    return(NULLSMAT);
  for ( i = 0 ; i < A->mn ; i++)
    {
      char *str_to_build, *p_str_to_build, *p_str, *str = A->S[i];
      int k, kb, len_str_to_build, len_str = strlen(str);

      if ( more > 0 && len_needle > 0 )
	len_str_to_build = len_str+(len_str/len_needle)*more;
      else
	len_str_to_build = len_str;
      
      if (  (str_to_build =new_nsp_string_n(len_str_to_build)) == NULLSTRING )
	{
	  nsp_smatrix_destroy(Loc);
	  return(NULLSMAT);
	}
      p_str_to_build = str_to_build;

      k = 0;
      if ( len_needle > 0 )
	while ( (p_str = strstr(&(str[k]),needle)) != NULL )
	  {
	    kb = p_str - str;
	    p_str_to_build = strncpy(p_str_to_build, &(str[k]), kb-k) + kb-k;
	    p_str_to_build = strncpy(p_str_to_build, replace, len_replace) + len_replace;
	    k = kb + len_needle;
	  };
      p_str_to_build = strncpy(p_str_to_build, &(str[k]), len_str-k) + len_str-k;
      *p_str_to_build = '\0';
      if (  p_str_to_build - str_to_build <  len_str_to_build )
	{
	  len_str_to_build = p_str_to_build - str_to_build;
	  if (nsp_string_resize(&str_to_build, len_str_to_build) == FAIL )
	    {
	      nsp_smatrix_destroy(Loc);
	      return(NULLSMAT);
	    }
	}
      Loc->S[i] = str_to_build;
    }
  return Loc;
}


/*
 * Res =nsp_smatrix_strip_blanks(A,search,replace) 
 */

int nsp_smatrix_strip_blanks(NspSMatrix *A)
{
  int i,j;
  for ( i = 0 ; i < A->mn ; i++) 
    {
      char *loc = A->S[i], *loc1 = loc;
      int ln = strlen(loc);
      for ( j = ln-1 ; j >=0 ; j--) 
	{
	  if ( loc[j]== ' ') 
	    loc[j]='\0';
	  else 
	    break;
	}
      for ( j = 0 ;j < ln ; j++) 
	{
	  if ( loc[j]== ' ') 
	    {
	      loc1 = loc + j + 1;
	    }
	  else 
	    break;
	}
      if ( loc1 != loc ) 
	{
	  nsp_string s; 
	  if ((s =nsp_string_copy(loc1)) == (nsp_string) 0) return FAIL;
	nsp_string_destroy(&(A->S[i]));
	  A->S[i]= s;
	}
    }
  return OK;
}



