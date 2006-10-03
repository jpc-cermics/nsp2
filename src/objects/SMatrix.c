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

static int nsp_smatrix_print_internal(nsp_num_formats *fmt,const NspSMatrix *m, int indent);

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
  NSP_OBJECT(Loc)->name = NULL;
  if ( NSP_OBJECT(Loc)->type->set_name(NSP_OBJECT(Loc),name) == NULL)
    return(NULLSMAT);
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

NspSMatrix *nsp_smatrix_clone(const char *name, NspSMatrix *A, int m, int n)
{
  return nsp_smatrix_create_with_length(name, m, n, -1);
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
  NSP_OBJECT(Loc)->name = NULL;
  if ( NSP_OBJECT(Loc)->type->set_name(NSP_OBJECT(Loc),name) == NULL)
    return(NULLSMAT);
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
    {
      for ( i = 0 ; i < Loc->mn ; i++ ) 
	{
	  if ((Loc->S[i] =new_nsp_string_n(strl)) == (nsp_string) 0 )  return(NULLSMAT);
	}
    }
  else
    {
      for ( i = 0 ; i < Loc->mn ; i++ ) Loc->S[i] = (nsp_string) 0;
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


/**
 * nsp_smatrix_elt_size:
 * @M: a #NspSMatrix 
 * 
 * size of string matrix elements.
 * 
 * Return value: size of @M elements.
 **/

unsigned int  nsp_smatrix_elt_size(NspMatrix *M)
{
  return sizeof(nsp_string);
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
  NSP_OBJECT(A)->type->set_name(NSP_OBJECT(A),NVOID);
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

void nsp_smatrix_info(const NspSMatrix *Mat, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;

  switch ( Mat->mn )
    {
    case 0: 
      Sciprintf1(indent,"%s\t= []\t\ts (%dx%d)\n",pname,Mat->m,Mat->n);
      break;
    case 1 :
      Sciprintf1(indent,"%s\t= [%s]\t\ts (%dx%d)\n",pname,Mat->S[0],Mat->m,Mat->n);
      break;
    default :
      Sciprintf1(indent,"%s\t= [...]\t\ts (%dx%d)\n",pname,Mat->m,Mat->n);
      break;
    }
}

/*
 *nsp_smatrix_print: writes Mat Objet 
 */

void nsp_smatrix_print(const NspSMatrix *Mat, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=%s",pname,(Mat->mn==0 ) ? " m2s([])\n" : "" );
	}
      else 
	{
	  Sciprintf1(indent,"%s",(Mat->mn==0 ) ? " m2s([])\n" : "" );
	}
    }
  else 
    {
      if ( user_pref.pr_depth  <= rec_level -1 ) 
	{
	  nsp_smatrix_info(Mat,indent,pname,rec_level);
	  return;
	}
      Sciprintf1(indent,"%s\t=%s\t\ts (%dx%d)\n",pname,
		 (Mat->mn==0 ) ? " []" : "",Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      nsp_smatrix_print_internal(&fmt,Mat,indent);
    }
}

/**
 * nsp_smatrix_latex_print:
 * @SMat: a #NspSMatrix
 * 
 * print the #NspSMatrix @SMat using the default Sciprintf() function and LaTeX 
 * syntax. 
 */

void nsp_smatrix_latex_print(NspSMatrix *SMat)
{
  int i,j;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf("{%s = \\left(\\begin{array}{",NSP_OBJECT(SMat)->name );
  for (i=0; i <  SMat->n;i++) Sciprintf("c");
  Sciprintf("}\n");
  for (j=0;j < SMat->m;j++)
    {
      for (i=0;i < SMat->n - 1;i++)
	{ 
	  Sciprintf("{\\texttt \"%s\"}\t& ",SMat->S[i+j*SMat->m]);
	}
      Sciprintf("{\\texttt \"%s\"}\t\\\\\n",SMat->S[SMat->m-1+j*SMat->m]);
    }
  Sciprintf("\\end{array}\\right)}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}

/**
 * nsp_smatrix_latex_tab_print:
 * @SMat: a #NspSMatrix
 * 
 * print the #NspSMatrix @A using the default Sciprintf() function and LaTeX tabular
 * syntax. 
 */

void nsp_smatrix_latex_tab_print(NspSMatrix *SMat)
{
  int i,j;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf("\\begin{tabular}{|l|");
  for (i=0; i < SMat->n ;i++) Sciprintf("c|");
  Sciprintf("}\\hline\n %s &\t",NSP_OBJECT(SMat)->name);
  for (i=1; i < SMat->n ;i++) Sciprintf("$C_{%d}$\t&",i);
  Sciprintf("$C_{%d}$\\\\ \\hline\n",SMat->n);
  for (j=0;j < SMat->m ;j++)
    {
      Sciprintf("$L_{%d}$\t&",j+1);
      for (i=0;i < SMat->n-1 ;i++)
	{
	  Sciprintf("{\\texttt \"%s\"}\t& ",SMat->S[i+j*SMat->m]);
	}
      Sciprintf("{\\texttt \"%s\"}\t\\\\ \\hline\n",SMat->S[SMat->m-1+j*SMat->m]);
    }
  Sciprintf("\\end{tabular}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
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
  if ((Loc = nsp_smatrix_create_with_length(NVOID,A->m+B->m,A->n,-1)) == NULLSMAT) 
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
 * Down Concatenation 
 * A = [A;B] 
 * return NULLSMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int Smove(int n, nsp_string *s1, nsp_string *s2);
static int Scopy1(int n, nsp_string *s1, nsp_string *s2);

/* take care that A != B 
 * if flag is true, then strings from matrix B can be used 
 * without copy and B matrix is destroyed.
 */

int nsp_smatrix_concat_down1(NspSMatrix *A,NspSMatrix *B,int flag)
{
  int j,m=A->m,n= A->n;
  if ( A->n != B->n && A->mn != 0 ) 
    {
      Scierror("Error: [.;.] incompatible dimensions\n");
      return FAIL;
    }
  if ( A == B )
    {
      Scierror("Error: first and second arguments points to the same matrix\n");
      return FAIL;
    }
  if ( A->mn == 0)
    {
      A->S = (nsp_string *)  MALLOC ((B->mn+1)* sizeof(nsp_string));
      A->n = B->n;
      A->m = B->m;
      A->mn = A->m*A->n;
    }
  else
    {
      A->S = (nsp_string *)  REALLOC (A->S, ((A->m+B->m)*A->n+1)* sizeof(nsp_string));
      A->m += B->m;
      A->mn = A->m*A->n;
    }
  if ( A->S == (nsp_string *) 0) 
    {
      Scierror("Error: allocation failure\n");
      return FAIL;
    }
  for ( j = n -1 ; j >0  ; j-- ) 
    {
      if ( Smove(m,A->S+j*m,A->S+j*(A->m)) == FAIL) 
	return FAIL;
    }
  if ( flag == TRUE )
    {
      for ( j = B->n -1 ; j >=0  ; j-- ) 
	{
	  if ( Smove(B->m,B->S+j*B->m,A->S+j*(A->m)+m) == FAIL)
	    return FAIL;
	}
      for ( j = 0 ; j < B->mn  ; j++) B->S[j]= NULL;
      nsp_smatrix_destroy(B);
    }
  else 
    {
      for ( j = B->n -1 ; j >=0  ; j-- ) 
	{
	  if ( Scopy1(B->m,B->S+j*B->m,A->S+j*(A->m)+m) == FAIL)
	    return FAIL;
	}
    }
  A->S[A->mn]=(nsp_string) 0;
  return OK;
}

static int Smove(int n, nsp_string *s1, nsp_string *s2)
{
  int i;
  for ( i = n-1 ; i >= 0 ; i--) 
    {
      s2[i]=s1[i];
    }
  return(OK);
}

static int Scopy1(int n, nsp_string *s1, nsp_string *s2)
{
  int i;
  for ( i = n-1 ; i >= 0 ; i--) 
    {
      if ((s2[ i] =nsp_string_copy(s1[i])) == (nsp_string) 0)  return(FAIL);
    }
  return(OK);
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

int nsp_smatrix_set_submatrix_obsolete(NspSMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspSMatrix *B)
{
  return nsp_matint_set_submatrix1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols),NSP_OBJECT(B));
}


/*
 *  A(Rows) = B
 *  A is changed and enlarged if necessary
 *  Size Compatibility is checked
 */

int nsp_smatrix_set_rows_obsolete(NspSMatrix *A, NspMatrix *Rows, NspSMatrix *B)
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
 * Res=nsp_smatrix_extract(A,Rows,Cols)
 * A, Rows and Cols are unchanged 
 */	

NspSMatrix*nsp_smatrix_extract_obsolete(NspSMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
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

NspSMatrix*nsp_smatrix_extract_elements_obsolete(NspSMatrix *A, NspMatrix *Elts, int *err)
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

NspSMatrix*nsp_smatrix_extract_columns_obsolete(NspSMatrix *A, NspMatrix *Cols, int *err)
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

NspSMatrix*nsp_smatrix_extract_rows_obsolete(NspSMatrix *A, NspMatrix *Rows, int *err)
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

/**
 * nsp_new_string:
 * @bytes: string to be copied.
 * @length: -1 or the number of bytes to use for copy.
 * 
 * return a new string filled with bytes from @bytes. 
 * If @length is < 0 @bytes must be null terminated, If @length is 
 * positive then @length characters from @length are used.
 * 
 * Return value: a #nsp_string or NULL
 **/

nsp_string nsp_new_string(nsp_const_string bytes,int length)
{
  if ( length >= 0) 
    {
      nsp_string str = new_nsp_string_n(length) ;
      memcpy(str,bytes, length*sizeof(char));
      str[length]='\0';
      return str;
    }
  else
    {
      return new_nsp_string(bytes);
    }
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
      Scierror("Error:\tString resize, no more memory\n");
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
 * 
 * Catenate the rows of A whitout white space padding 
 * and using str char as separtor ( if flag == 1) 
 * 
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


/**
 * nsp_smatrix_elts_concat:
 * @A: a #NspSMatrix
 * @rstr: a string, row separator (used if @rflag=1)
 * @rflag: tell if we add the row separator  
 * @cstr: a string, column separator (used if @cflag=1)
 * @cflag: tell if we add the column separator
 * 
 * catenate all the elements row by row 
 * rstr is the row-separator and cflag the col-separator 
 * improved in speed by bruno 
 * Return value: a string or NULL
 **/

nsp_string nsp_smatrix_elts_concat(const NspSMatrix *A,nsp_const_string rstr, int rflag,
				   nsp_const_string cstr, int cflag)
{
  int i,j,k,lentot=0, len, lsc=0, lsr=0;
  nsp_string Loc;
  char *p;
  /* evaluation of sizes */
  for ( j = 0 ; j < A->mn ; j++ ) lentot += strlen(A->S[j]);
  if ( rflag == 1)
    {
      lsr = strlen(rstr);
      lentot += (A->m -1)*lsr;
    }
  if ( cflag == 1)
    {
      lsc = strlen(cstr);
      lentot += (A->m*(A->n -1))*lsc;
    }

  /* New String */
  if ( (Loc = new_nsp_string_n(lentot)) != NULLSTRING )
    {
      p = Loc;
      for ( i = 0 ; i < A->m ; i++ )
	{
	  for ( j = 0 ; j < A->n ; j++ )
	    {
	      k = i+(A->m)*j;
	      len = strlen(A->S[k]);
	      strncpy(p, A->S[k], len); p += len;
	      if ( cflag == 1 && j != A->n-1 ) { strncpy(p,cstr,lsc); p += lsc; }
	    }
	  if ( rflag == 1 && i != A->m-1 )  { strncpy(p,rstr,lsr); p += lsr; }
	}
      *p='\0';
    }
  return Loc;
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

/* XXXXX */
extern void nsp_matrix_set_format(nsp_num_formats *fmt,NspMatrix *M) ;


NspSMatrix*nsp_matrix_to_smatrix(NspMatrix *A,nsp_const_string str, int flag)
{
  char buf[1024],formati[256];
  nsp_const_string def="%f", format=def;
  /* nsp_const_string  defi ="%f +%fi", */
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
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      nsp_matrix_set_format(&fmt,A) ;
      if  ( A->rc_type == 'r') 
        format = fmt.curr_real_fmt;
      else 
	sprintf(formati,"%s + %si",fmt.curr_real_fmt,fmt.curr_imag_fmt);
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

/**
 * nsp_smatrix_strindex:
 * @Str: a string 
 * @Motif: a string used as pattern 
 * 
 * returns the position of pattern @Motif in string @Str 
 * or an empty matrix [] if @Motif is not in @Str.
 * 
 * Return value: %NULLMAT or a new #NspMatrix
 **/

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


/**
 * nsp_ascii_to_smatrix:
 * @A: a #NspMatrix
 * 
 * creates a string using entries of matrix @A 
 * which are considered as ascii codes. 
 * 
 * Return value: %NULLSMAT or a new #NspSMatrix
 **/

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


/**
 * nsp_string_to_ascii:
 * @S: a string 
 * 
 * creates a row vector with ascii codes of characters from string @A 
 * 
 * Return value: %NULLMAT or a new #NspMatrix
 **/

NspMatrix *nsp_string_to_ascii(nsp_const_string S)
{
  unsigned int i;
  NspMatrix *Loc;
  if ((Loc = nsp_matrix_create(NVOID,'r',1,strlen(S))) == NULLMAT) return(NULLMAT);
  for ( i = 0 ; i < strlen(S) ; i++ )
    Loc->R[i] = (int) S[i];
  return Loc;
}


/**
 * nsp_smatrix_split:
 * @string: the string to be splitted in words
 * @splitChars: a string with the split characters
 * @msep: an int 
 * 
 * Return value: a #NspSMatrix of size 1 x nb_words with the words resulting from the splitting
 **/

/* modified by bruno to get 2 differents behavior*/


NspSMatrix*nsp_smatrix_split_string(nsp_const_string string,nsp_const_string splitChars, int msep)
{
  char *s;
  register nsp_const_string p;
  nsp_const_string elementStart;
  int stringLen, i;
  NspSMatrix *A;
  stringLen = strlen(string);
  if ( strlen(splitChars) == 0 )
    {
      /* split to stringLen chars :
       *  split('foo','') --> ['f','o','o']
       */
      if ( (A=nsp_smatrix_create(NVOID,1,stringLen,".",1)) == NULLSMAT )
	return NULLSMAT;
      for (i = 0 ; i < stringLen ; i++) A->S[i][0] = string[i];
    }
  else
    {
      /* split with split characters
       * ex: split('foo poouumou',' u') --> ['foo','poo','','mo', ''] if msep=0
       *                                --> ['foo','poo','mo'] if msep=1
       */
      int nb_words=0;
      int nb_chars;
      Boolean in_a_word=FALSE;

      if ( (A=nsp_smatrix_create(NVOID,0,0,".",0)) == NULLSMAT )
	return NULLSMAT;

      if ( msep == 0 )
	{
	  elementStart = string;
	  for (i = 0, p = string;  i < stringLen;  i++, p++)
	    {
	      if ( strchr(splitChars,*p) != NULL ) /* *p is a split character */
		{
		  nb_words++;
		  if ( nsp_smatrix_resize(A,1,nb_words) == FAIL) goto err;
		  nb_chars = p - elementStart;
		  if ( (s  = new_nsp_string_n(nb_chars)) == NULLSTRING ) goto err;
		  nsp_string_destroy(&A->S[nb_words-1]);
		  A->S[nb_words-1]= s;
		  strncpy( A->S[nb_words-1],elementStart, nb_chars);
		  A->S[nb_words-1][p-elementStart]='\0';
		  elementStart = p+1;
		}
	    }
	  nb_words++;
	  if ( nsp_smatrix_resize(A,1,nb_words) == FAIL ) goto err;
	  nb_chars = p - elementStart;
	  if ( (s  = new_nsp_string_n(nb_chars)) == NULLSTRING ) goto err;
	  nsp_string_destroy(&A->S[nb_words-1]);
	  A->S[nb_words-1]= s;
	  strncpy( A->S[nb_words-1],elementStart, nb_chars);
	  A->S[nb_words-1][p-elementStart]='\0';
	}
      else  /* merge separators (don't create empty string between 2 separators) */
	{
	  elementStart = string;
	  for (i = 0, p = string;  i < stringLen;  i++, p++)
	    {
	      if ( strchr(splitChars,*p) != NULL ) /* *p is a split character */
		{
		  if ( in_a_word )  /* so this ends the word */
		    {
		      nb_words++;
		      if ( nsp_smatrix_resize(A,1,nb_words) == FAIL) goto err;
		      nb_chars = p - elementStart;
		      if ( (s  = new_nsp_string_n(nb_chars)) == NULLSTRING ) goto err;
		      nsp_string_destroy(&A->S[nb_words-1]);
		      A->S[nb_words-1]= s;
		      strncpy( A->S[nb_words-1],elementStart, nb_chars);
		      A->S[nb_words-1][p-elementStart]='\0';
		    }
		  in_a_word = FALSE;
		}
	      else
		if ( !in_a_word ) { elementStart = p; in_a_word = TRUE; }
	    }
	  if ( in_a_word )  /* the last word is ended by the end of the string */
	    {
	      nb_words++;
	      if ( nsp_smatrix_resize(A,1,nb_words) == FAIL ) goto err;
	      nb_chars = p - elementStart;
	      if ( (s  = new_nsp_string_n(nb_chars)) == NULLSTRING ) goto err;
	      nsp_string_destroy(&A->S[nb_words-1]);
	      A->S[nb_words-1]= s;
	      strncpy( A->S[nb_words-1],elementStart, nb_chars);
	      A->S[nb_words-1][p-elementStart]='\0';
	    }
	}
    }
  return A;
 err:
  nsp_smatrix_destroy(A);
  return NULLSMAT;
}


NspSMatrix* nsp_smatrix_split(NspSMatrix *Src,nsp_const_string splitChars, int msep)
{
  NspSMatrix *M=NULLSMAT,*loc;
  int i;
  if ((M=nsp_smatrix_split_string(Src->S[0],splitChars,msep))==NULLSMAT) return NULLSMAT;

  for ( i= 1 ; i < Src->mn ; i++) 
    {
      if ((loc=nsp_smatrix_split_string(Src->S[i],splitChars,msep))==NULLSMAT) goto err;
      if ( loc->n != M->n )
	{
	  Scierror("Error: string %d contains too many tokens %d (expecting %d)\n",
		   i,loc->n,M->n);
	  goto err;
	}
      /* we use loc without copy */
      if ( nsp_smatrix_concat_down1(M,loc,TRUE)== FAIL) goto err;
    }
  return M;
 err:
  nsp_smatrix_destroy(M);
  return NULLSMAT;
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


/* set of function used to print string matrices 
 *
 */

static void SMij_string_as_read(const nsp_num_formats *fmt,const void *m, int i, int j)
{
  const NspSMatrix *M=m;
  char *c= M->S[i+(M->m)*j];
  Sciprintf("\"");
  while ( *c != '\0') 
    {
      switch (*c) 
	{
	case '\'' :
	case '\"' : 
	  Sciprintf("%s","''");break;
	case '\n' :
	  Sciprintf("%s","\\n");break;
	default: 
	  Sciprintf("%c",*c);
	}
      c++;
    }
  Sciprintf("\"");
}

static int nsp_smatrix_print_internal(nsp_num_formats *fmt,const NspSMatrix *m, int indent)
{
  
  int *Iloc;
  int inc,column_width=2,total_width;
  int p_rows=0,col;
  int max_width ,winrows,offset;
  int i,j;
  int nr = m->m;
  int nc = m->n;
  if (nr == 0 || nc == 0) nsp_print_empty_matrix ( nr, nc);
  sci_get_screen_size(&winrows,&max_width);
  /* Allocate a table to store each column width **/
  if ((Iloc =nsp_alloc_int(m->n)) == (int*) 0) return(FAIL);
  /* set Iloc[j] to the max len of column j **/
  for ( j=0 ; j < m->n ; j++ )
    {
      Iloc[j]=strlen(m->S[j*m->m]);
      for ( i = 1 ; i < m->m ; i++) 
	{
	  if ( Iloc[j] < (int) strlen(m->S[i+j*m->m]) ) Iloc[j]= strlen(m->S[i+j*m->m]);
	}
    }
  Sciprintf("\n");
  /* compute the necessary width **/
  total_width=0;
  for ( j=0 ; j < m->n ; j++) 
    {
      column_width = Iloc[j] + 2;
      total_width +=  column_width;
    }
  inc = nc;
  offset =  indent + 4; /* 4 = " |...| " */
  if (total_width > max_width && user_pref.split_long_rows)
    {
      inc = (max_width -offset) / column_width;
      if (inc == 0) inc++;
    }

  if ( user_pref.pr_as_read_syntax )
    {
      nsp_gen_matrix_as_read_syntax(fmt,m,nr,nc,inc,indent,SMij_string_as_read);
      return 0;
    }
  col=0;
  while ( col < nc )
    {
      int lim,num_cols,t_width;
      inc=0;
      t_width = 0;
      for ( j= col ; j < m->n ; j++) 
	{
	  t_width +=  Iloc[j]+2;
	  if ( t_width < max_width) inc++;
	  else break;
	}
      if (inc == 0)	inc++;
      lim = col + inc < nc ? col + inc : nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  if (col != 0)  Sciprintf("\n");
	  num_cols = lim - col;
	  if (num_cols == 1)
	    Sciprintf(" Column %d :\n\n",col+1);
	  else if (num_cols == 2)
	    Sciprintf(" Columns %d and %d:\n\n",col+1,lim);
	  else
	    Sciprintf(" Columns %d through %d:\n\n",col+1,lim);
	}
      for ( i = 0; i < nr; i++)
	{
	  int imore;
	  p_rows++;
	  if ( p_rows >= winrows ) 
	    {
	      scimore(&imore);
	      if ( imore == 1) return(OK);
	      p_rows=0;
	    }
	  nsp_pr_white(indent);
	  /*
	    if (  lim - col == 1 &&  t_width > max_width ) 
	    {
	    Sciprintf("Must try to cut the column \n");
	    }
	    else 
	  */
	  {
	    for ( j = col; j < lim; j++)
	      {
		Sciprintf("  ");
		Sciprintf("%s",m->S[i+(m->m)*j]);
		nsp_pr_white(Iloc[j]-strlen(m->S[i+(m->m)*j]));
	      }
	    Sciprintf("\n");
	  }
	}
      col += inc;
    }
  FREE(Iloc);
  return(OK);
}

/* SMatrix to Matrix 
 * with strtod
 */

NspMatrix *nsp_smatrix_strtod(const NspSMatrix *S)
{
  NspMatrix *M;
  char *next = NULL;
  int i; 
  if ((M = nsp_matrix_create(NVOID,'r',S->m,S->n)) == NULLMAT)  return NULLMAT;
  for ( i= 0 ; i < S->mn ; i++)
    {
      M->R[i]= strtod(S->S[i],&next);
      /* 
      if ( next == S->S[i])
	Sciprintf("Warning the string %s cannot be converted to double\n",S->S[i]);
      */
    }
  return M;
}


