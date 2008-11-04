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
#include <ctype.h> /* tolower toupper */

#include "nsp/object.h"
#include "nsp/interf.h" /* for ret_endfor */
#include "nsp/pr-output.h" 
#include "nsp/matutil.h"
#include "nsp/gsort-p.h"
#include "nsp/matint.h"

static int nsp_smatrix_print_internal(nsp_num_formats *fmt,const NspSMatrix *m, int indent);


/**
 * nsp_smatrix_create:
 * @name: name to give to #NspSMatrix
 * @m: number of rows 
 * @n: number of columns 
 * @str: default value for entries 
 * @flag: 1 or 0.
 * 
 * creates a #NspSMatrix, all the entries are set to empty string or 
 * to @str if @flag is not zero. 
 * Note that the array for storing the strings is of size (m*n)+1 
 * and the last element is set to a null char pointers. This can be 
 * used to detect the last element in the string array.
 * 
 * Return value: a new #NspSMatrix or %NULLSMAT 
 **/

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

/**
 * nsp_smatrix_clone:
 * @name: name to give to #NspSMatrix
 * @A: a #NspSMatrix
 * @m: number of rows 
 * @n: number of columns
 * @init: %TRUE or %FALSE
 * 
 * creates a #NspSMatrix of size @m x @n. If @init is %TRUE the 
 * entries are set to empty string else the entries are unallocated.
 * 
 * Return value: a new #NspSMatrix or %NULLSMAT 
 **/

NspSMatrix *nsp_smatrix_clone(const char *name, NspSMatrix *A, int m, int n, int init)
{
  if ( init == TRUE ) 
    return nsp_smatrix_create(name, m, n, NULL,0);
  else
    return nsp_smatrix_create_with_length(name, m, n, -1);
}

/**
 * nsp_smatrix_create_with_length:
 * @name: object name 
 * @m: number of rows 
 * @n: number of columns 
 * @strl: an integer 
 * 
 * creates a  new #NspSMatrix of size @m time @n. 
 * If @strl is strictly negative the string are not allocated. 
 * If @strl is positive strings are allocated nut not initialized.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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

/**
 * nsp_smatrix_create_from_table:
 * @T:  a %NULL terminated array of strings.
 * 
 * creates a  new #NspSMatrix using string copied from a given array @T.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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


/**
 * nsp_smatrix_create_from_array:
 * @name: object name 
 * @n: length of @T
 * @T: an array of strings.
 * 
 * creates a  new #NspSMatrix using string copied from a given array @T.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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


/**
 * nsp_smatrix_create_from_struct:
 * @name: object name 
 * @T: array of objects (struct) 
 * @size: size of each struct.
 *  
 * creates a  new #NspSMatrix using string copied from a given array. 
 * @T array of objects of size @size with last element set to %NULL.
 * Each struct stored in @T is supposed to start with a char pointer field.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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

/**
 * nsp_smatrix_copy:
 * @A: a #NspSMatrix 
 * 
 * returns a copy of @A with name set to %NVOID.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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

/**
 * nsp_smatrix_resize:
 * @A: a #NspSMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * resizes @A to @m x @n. When @m * @n > @A->mn, the function only 
 * enlarges the array used for string storage to a @m x @n length.
 * The previously stored strings are not moved and occupy the first cells 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smatrix_resize(NspSMatrix *A, int m, int n)
{
  int i;
  if ( A->mn == m*n ) 
    {
      A->m=m;
      A->n=n;
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


/**
 * nsp_smatrix_destroy:
 * @A: a #NspSMatrix 
 * 
 * free object @A.
 **/

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


/**
 * nsp_smatrix_info:
 * @Mat:  #NspSMatrix
 * @indent: indentation value 
 * @name:  NULL or new name to use for printing 
 * @rec_level: depth counter 
 * 
 * display Info on Matrix @Mat 
 *
 * Return value: %TRUE or %FALSE
 **/

int nsp_smatrix_info(const NspSMatrix *Mat, int indent,const char *name, int rec_level)
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
  return TRUE;
}


/**
 * nsp_smatrix_print:
 * @Mat:   #NspSMatrix
 * @indent: indentation value 
 * @name: NULL or new name to use for printing 
 * @rec_level: depth counter 
 * 
 * prints the contents of @Mat.
 *
 * Return value: %TRUE or %FALSE
 **/

int nsp_smatrix_print(const NspSMatrix *Mat, int indent,const char *name, int rec_level)
{
  int rep = TRUE;
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
	  return rep;
	}
      Sciprintf1(indent,"%s\t=%s\t\ts (%dx%d)\n",pname,
		 (Mat->mn==0 ) ? " []" : "",Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      rep = nsp_smatrix_print_internal(&fmt,Mat,indent);
    }
  return rep;
}

/**
 * nsp_smatrix_latex_print:
 * @SMat: a #NspSMatrix
 * 
 * print the #NspSMatrix @SMat using the default Sciprintf() function and LaTeX 
 * syntax. 
 *
 * Return value: %TRUE or %FALSE
 */

int nsp_smatrix_latex_print(NspSMatrix *SMat)
{
  int i,j;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf("{%s = \\left(\\begin{array}{",NSP_OBJECT(SMat)->name );
  for (i=0; i <  SMat->n;i++) Sciprintf("c");
  Sciprintf("}\n");

  for (i=0; i < SMat->m; i++)
    {
      for (j=0; j < SMat->n - 1; j++)
	{ 
	  Sciprintf("{\\texttt \"%s\"}\t& ",SMat->S[i+j*SMat->m]);
	}
      Sciprintf("{\\texttt \"%s\"}\t",SMat->S[i+(SMat->n-1)*SMat->m]);
      if ( i != SMat->m -1 ) 
	Sciprintf("\\\\\n");
      else 
	Sciprintf("\n");
    }
  Sciprintf("\\end{array}\\right)}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}

/**
 * nsp_smatrix_latex_tab_print:
 * @SMat: a #NspSMatrix
 * 
 * print the #NspSMatrix @A using the default Sciprintf() function and LaTeX tabular
 * syntax. 
 */

int nsp_smatrix_latex_tab_print(NspSMatrix *SMat)
{
  int i,j;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf("\\begin{tabular}{|l|");
  for (i=0; i < SMat->n ;i++) Sciprintf("c|");
  Sciprintf("}\\hline\n %s &\t",NSP_OBJECT(SMat)->name);
  for (i=1; i < SMat->n ;i++) Sciprintf("$C_{%d}$\t&",i);
  Sciprintf("$C_{%d}$\\\\ \\hline\n",SMat->n);
  for (i=0; i < SMat->m; i++)
    {
      Sciprintf("$L_{%d}$\t&",i+1);
      for (j=0; j < SMat->n - 1; j++)
	{ 
	  Sciprintf("{\\texttt \"%s\"}\t& ",SMat->S[i+j*SMat->m]);
	}
      Sciprintf("{\\texttt \"%s\"}\t\\\\ \\hline\n",SMat->S[i+(SMat->n-1)*SMat->m]);
    }
  Sciprintf("\\end{tabular}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}



/**
 * nsp_smatrix_enlarge:
 * @A: a #NspSMatrix 
 * @m: minimal number of rows expected 
 * @n:  minimal number of columns expected
 * 
 * 
 * changes @A to [@A , 0; 0,0 ]  where 0 stands for "." strings
 * in such a way that the size of the result is (max(A->m,m) x max(A->n,n));
 * The result is stored in A 
 * 
 * Return value: %OK or %FAIL.
 **/

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


/**
 * nsp_smatrix_concat_right:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix 
 * 
 * set @A to  [@A,@B] 
 * 
 * Return value: %OK or %FAIL.
 **/

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

/**
 * nsp_smatrix_add_columns:
 * @A: a #NspSMatrix 
 * @n: number of columns to be added
 * 
 * add @n  columns to @A.
 * 
 * Return value: %OK or %FAIL.
 **/

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


/**
 * nsp_smatrix_concat_down:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix 
 * 
 * returns [A;B] 
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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

/**
 * nsp_smatrix_concat_down1:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix 
 * @flag: %TRUE or %FALSE
 * 
 * take care that A != B 
 * if flag is true, then strings from matrix B can be used 
 * without copy and B matrix is destroyed.
 * 
 * Return value: %OK or %FAIL
 **/

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

/**
 * nsp_smatrix_add_rows:
 * @A: a #NspSMatrix 
 * @m: number of rows to be added
 * 
 * Add m rows to matrix @A.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_smatrix_add_rows(NspSMatrix *A, int m)
{
  int Am;
  int i, j, k, kb;
  nsp_string temp;
  if ( m == 0) return OK;
  else if ( m < 0) 
    {      
      Scierror("Error: Negative indice (%d) in SMatAddRows\n",m);
      return FAIL;
    }
  Am= A->m;
  if ( nsp_smatrix_resize(A,A->m+m,A->n)== FAIL) return FAIL;

  for ( j = A->n ; j > 1 ; j-- )
    {
      k = j*A->m - m - 1;
      kb = j*Am - 1;
      for ( i = 0 ; i < Am ; i++ )
	{
	  temp = A->S[k]; A->S[k] = A->S[kb]; A->S[kb] = temp;
	  k--; kb--;
	}
    }

  return OK;
}

/**
 * nsp_smatrix_set_submatrix:
 * @A: a #NspSMatrix 
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 * @B: a #NspSMatrix 
 * 
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Rows and Cols are unchanged 
 *  Size Compatibility is checked 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_smatrix_set_submatrix(NspSMatrix *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspSMatrix *B)
{
  return nsp_matint_set_submatrix1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols),NSP_OBJECT(B));
}

/**
 * nsp_smatrix_set_rows:
 * @A: a #NspSMatrix 
 * @Rows: a #NspMatrix
 * @B: a #NspSMatrix 
 * 
 *  A(Rows) = B
 *  A is changed and enlarged if necessary
 *  Size Compatibility is checked
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_smatrix_set_rows(NspSMatrix *A, NspMatrix *Rows, NspSMatrix *B)
{
  return nsp_matint_set_elts1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(B));
}


/**
 * nsp_smatrix_extract:
 * @A: a #NspSMatrix 
 * @Rows: a #NspMatrix 
 * @Cols: a #NspMatrix 
 * 
 * extracts @A(@Rows,@Cols)
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

NspSMatrix*nsp_smatrix_extract(NspSMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  return (NspSMatrix*)nsp_matint_extract1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols));
}



/**
 * nsp_smatrix_extract_elements:
 * @A: a #NspSMatrix 
 * @Elts: a #NspMatrix 
 * @err: int pointer 
 * 
 * extracts @A(@Elts), @err is unused.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

NspSMatrix*nsp_smatrix_extract_elements(NspSMatrix *A, NspMatrix *Elts, int *err)
{
  *err=0;
  return (NspSMatrix *) nsp_matint_extract_elements1(NSP_OBJECT(A),NSP_OBJECT(Elts));
}


/**
 * nsp_smatrix_extract_columns:
 * @A: a #NspSMatrix 
 * @Cols: a #NspMatrix 
 * @err: int pointer 
 * 
 * extracts @A(:,@Cols), @err is unused.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

NspSMatrix*nsp_smatrix_extract_columns(NspSMatrix *A, NspMatrix *Cols, int *err)
{
  *err=0;
  return (NspSMatrix *) nsp_matint_extract_columns1(NSP_OBJECT(A),NSP_OBJECT(Cols));
}

/**
 * nsp_smatrix_extract_rows:
 * @A: a #NspSMatrix 
 * @Rows: a #NspMatrix 
 * @err: a int pointer for error.
 * 
 * returns a sub matrix of matrix @A by selecting rows from @Row
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

NspSMatrix*nsp_smatrix_extract_rows(NspSMatrix *A, NspMatrix *Rows, int *err)
{
  *err=0;
  return (NspSMatrix *) nsp_matint_extract_rows1(NSP_OBJECT(A),NSP_OBJECT(Rows));
}


/**
 * SMatLoopCol:
 * @str: 
 * @Col: 
 * @A: a #NspSMatrix 
 * @icol: 
 * @rep: 
 * 
 * 
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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



/**
 * nsp_smatrix_extract_diag:
 * @A: a #NspSMatrix
 * @k: an integer 
 *
 * Extract the @k-th diagonal of matrix @A and returns 
 * its value as a column vector. 
 * 
 * returns a #NspSMatrix or %NULLSMAT 
 */

NspSMatrix  *nsp_smatrix_extract_diag(NspSMatrix *A, int k)
{
  NspSMatrix *Loc;
  int j,i;
  int imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc =nsp_smatrix_create(NVOID,(int) 0 , (int) 0,"",0);
      return(Loc);
    }
  if (( Loc =nsp_smatrix_create_with_length(NVOID,imax-imin,1,-1)) == NULLSMAT)
    return(NULLSMAT);
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    if ((Loc->S[j++] = nsp_string_copy(A->S[i+(i+k)*A->m])) == (nsp_string) 0)
      return(NULLSMAT);
  return(Loc);
}

/**
 * nsp_smatrix_set_diag:
 * @A: a #NspSMatrix
 * @Diag: a #NspSMatrix
 * @k: an integer 
 *
 * sets the @k-th diagonal of matrix @A with values from @Diag. 
 * 
 * returns %OK or %FAIL.
 */

int nsp_smatrix_set_diag(NspSMatrix *A, NspSMatrix *Diag, int k)
{
  int i,j;
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
      if (nsp_smatrix_enlarge(A,imax,imax+k) == FAIL) return(FAIL);
    }
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    {
      nsp_string_destroy(&(A->S[i+(i+k)*A->m]));
      if ((A->S[i+(i+k)*A->m] = nsp_string_copy(Diag->S[j++])) == (nsp_string) 0)
	return FAIL;
    }
  return OK;
}

/**
 * nsp_smatrix_create_diag:
 * @Diag: a #NspSMatrix
 * @k: an integer 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns a #NspSMatrix or %NULLSMAT 
 */

NspSMatrix  *nsp_smatrix_create_diag(NspSMatrix *Diag, int k)
{
  int i,j;
  int imin,imax;
  NspSMatrix *Loc;
  imin = Max(0,-k);
  imax = Diag->mn +imin;
  if (( Loc =nsp_smatrix_create(NVOID,imax,imax+k,"",1)) == NULLSMAT) 
    return(NULLSMAT);
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    {
      nsp_string_destroy(&Loc->S[i+(i+k)*Loc->m]);
      if ((Loc->S[i+(i+k)*Loc->m] =nsp_string_copy( Diag->S[j++])) == (nsp_string) 0)
	return(NULLSMAT);
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

/**
 * new_nsp_string:
 * @str: a #nsp_const_string
 * 
 * returns a copy of @str.
 * 
 * Return value: a new #nsp_string or %NULL
 **/

nsp_string new_nsp_string(nsp_const_string str)
{
  nsp_string loc;
  if (( loc =new_nsp_string_n(strlen(str))) == (nsp_string) 0) return(loc);
  strcpy(loc,str);
  return((char*) loc);
}

/**
 * nsp_basic_to_string:
 * @str: a #nsp_const_string
 * 
 * 
 * Return value: a new #nsp_string or %NULL
 **/

nsp_string nsp_basic_to_string(nsp_const_string str)
{
  nsp_string loc;
  if (( loc =new_nsp_string_n(strlen(str))) == (nsp_string) 0) return(loc);
  strcpy((char*) loc,(char*) str);
  return(loc);
}

/**
 * nsp_string_copy:
 * @str: a #nsp_const_string
 * 
 * returns a copy of given string 
 * 
 * Return value: a new #nsp_string or %NULL
 **/

nsp_string nsp_string_copy(nsp_const_string str)
{
  nsp_string loc;
  int n= strlen((char*) str);
  if (( loc =new_nsp_string_n(n)) == (nsp_string) 0) return(loc);
  /* memcpy((void*) loc,(void*) str, (n+1)*sizeof(char)); **/
  strcpy((char*) loc,(char*) str); 
  return(loc);
}

/**
 * nsp_string_destroy:
 * @str: a string 
 * 
 * free storage associated to @str.
 * 
 **/

void nsp_string_destroy(nsp_string *str)
{
  FREE(*str);
  *str = NULLSTRING;
}

/**
 * new_nsp_string_n:
 * @n: length of string 
 * 
 * allocated a new #nsp_string of size @n (can contain @n+1 characters).
 * 
 * Return value: a new #nsp_string or %NULL
 **/

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


/**
 * nsp_string_resize:
 * @Hstr: 
 * @n: 
 * 
 * resize the string @Hstr to size (@n+1)
 *
 * Return value: %OK or %FAIL 
 **/

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

/**
 * nsp_string_protect:
 * @str: a nsp_const_string 
 * 
 * returns a new #nsp_string in which special characters 
 * are protected by '\'. 
 * 
 *
 * Return value: a new nsp_string or %NULL
 **/

nsp_string nsp_string_protect(nsp_const_string str)
{
  int count = 0;
  nsp_string loc, str2; 
  nsp_const_string str1 = str;
  while ( *str1 != '\0') 
    {
      switch (*str1) 
	{
	case '\'' :
	case '\"' : 
	case '\\' :
	case '\a' :
	case '\b' :
	case '\f' :
	case '\n' :
	case '\r' :
	case '\t' :
	case '\v' :
	  count++;
	default: 
	  if ( ! isprint(*str1)) count++; 
	}
      str1++;
    }
  if ( (loc = new_nsp_string_n(strlen(str)+count)) == NULLSTRING ) 
    return NULL;
  str1= str; 
  str2= loc; 
  while ( *str1 != '\0') 
    {
      switch (*str1) 
	{
	case '\'' :  *str2++ = '\''; *str2++ = *str1++; break;
	case '\"' :  *str2++ = '\"'; *str2++ = *str1++; break;
	case '\\' :  *str2++ = '\\'; *str2++ = *str1++;break;
	case '\a' :  *str2++ = '\\'; *str2++ = 'a'; str1++; break;
	case '\b' :  *str2++ = '\\'; *str2++ = 'b'; str1++; break;
	case '\f' :  *str2++ = '\\'; *str2++ = 'f'; str1++; break;
	case '\n' :  *str2++ = '\\'; *str2++ = 'n'; str1++; break;
	case '\r' :  *str2++ = '\\'; *str2++ = 'r'; str1++; break;
	case '\t' :  *str2++ = '\\'; *str2++ = 't'; str1++; break;
	case '\v' :  *str2++ = '\\'; *str2++ = 'v'; str1++; break;
	default: 
	  if ( ! isprint(*str1))  *str2++ = '\\'; 
	  *str2++ = *str1++;
	}
    }
  *str2++='\0';
  return loc;
}


/**
 * nsp_smatrix_concat_strings:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix 
 * @str: a string 
 * @flag: 0 or 1 
 * 
 * term to term concatenation each @A(i;j) entry is set to @A(i;j)@str@B(i;j)
 * @str is used if @flag is equal to one.
 * Note that @A and @B must have the same dimensions.
 * 
 * Return value: %OK or %FAIL
 **/

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

/**
 * nsp_smatrix_concat_string_right:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix of size 1x1
 * @str: a string 
 * @flag: 0 or 1 
 * 
 * updates @A entries to @A + @str + @B if @flag==1  or to @A+ @B if @flag==0.
 * 
 * Return value: %OK or %FAIL
 **/

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

/**
 * nsp_smatrix_concat_string_left:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix of size 1x1
 * @str:  a string 
 * @flag: 0 or 1 
 * 
 * updates @A entries to @B + @str + @A if @flag==1  or to @B + @A if @flag==0.
 * 
 * Return value: %OK or %FAIL
 **/

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


/**
 * nsp_smatrix_strcmp:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix 
 * 
 * returns a matrix filled with strcmp(@A(i,j),@B(i,j))
 * Note that @A and @B must have the same dimensions.
 * 
 * Return value:  a new #NspMatrix or %NULLMAT 
 **/

NspMatrix *nsp_smatrix_strcmp(NspSMatrix *A, NspSMatrix *B)
{
  int i;
  NspMatrix *Loc;
  if ( A->mn != B->mn ) 
    {
      if ( A->mn == 1 ) 
	{
	  if ( ( Loc = nsp_matrix_create(NVOID,'r',B->m,B->n) ) == NULLMAT ) return(NULLMAT);
	  for ( i = 0 ; i < B->mn ; i++) 
	    {
	      Loc->R[i]= strcmp(A->S[0],B->S[i]);
	    }
	  return(Loc);
	}
      else if ( B->mn == 1) 
	{
	  if ( ( Loc = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return(NULLMAT);
	  for ( i = 0 ; i < A->mn ; i++) 
	    {
	      Loc->R[i]= strcmp(A->S[i],B->S[0]);
	    }
	  return(Loc);
	}
      else 
	{
	  Scierror("Error: incompatible dimensions for strcmp\n");
	  return(NULLMAT);
	}
    }
  if ( ( Loc = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return(NULLMAT);
  for ( i = 0 ; i < A->mn ; i++) 
    {
      Loc->R[i]= strcmp(A->S[i],B->S[i]);
    }
  return(Loc);
}


/**
 * nsp_smatrix_column_concat_padded:
 * @A: a #NspSMatrix 
 * @str: a constant string 
 * @flag: an integer
 * 
 * returns a mx1 matrix such that Res(i) = "A(i,1)str A(i,2)str  ...A(i,n)"
 * white spaces are inserted and @str separator is used if @flag =1 
 * 
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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


/**
 * nsp_smatrix_column_concat:
 * @A: a #NspSMatrix 
 * @str: a string 
 * @flag: 0 or 1 
 * 
 * catenates the columns of @A whitout white space padding 
 * and using @str string as separtor ( if flag == 1) 
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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

/**
 * nsp_smatrix_row_concat:
 * @A: a #NspSMatrix 
 * @str: a string 
 * @flag: 0 or 1 
 * 
 * catenates the rows of @A whitout white space padding
 * and using @str string as separtor ( if @flag == 1) 
 *
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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


/**
 * nsp_smatrix_part:
 * @A: a #NspSMatrix 
 * @Ind: a #NspMatrix 
 * 
 * returns a new #NspSMatrix obtained by extracting from @A entries 
 * the characters given by indices given in @Ind. When an indice is out 
 * of range a blank character is inserted.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

NspSMatrix*nsp_smatrix_part(NspSMatrix *A, NspMatrix *Ind)
{
  int i,k;
  int ind;
  NspSMatrix *Loc;
  if ((Loc =nsp_smatrix_create_with_length(NVOID,A->m,A->n,Ind->mn)) == NULLSMAT)
    return(Loc);
  for ( i = 0 ; i < A->mn ; i ++) 
    {
      int strlen_i =  (int) strlen(A->S[i]);
      for ( k =0; k < Ind->mn ; k++ ) 
	{
	  ind = ((int) Ind->R[k])-1;
	  if ( ind < 0 || ind >= strlen_i )
	    Loc->S[i][k]=' ';
	  else 
	    Loc->S[i][k] = A->S[i][ind];
	}
      Loc->S[i][Ind->mn]='\0';
    }
  return(Loc);
}

/**
 * nsp_smatrix_elts_length:
 * @A: a #NspSMatrix 
 * 
 * return a numeric matrix #NspMatrix which contains the length of the strings 
 * contained in string matrix @A.
 * 
 * Return value:  a new #NspMatrix or %NULLMAT 
 **/

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


extern void nsp_matrix_set_format(nsp_num_formats *fmt,NspMatrix *M) ;

/**
 * nsp_matrix_to_smatrix:
 * @A: a #NspSMatrix 
 * @str: format to be used for printing
 * @flag: 0 or 1 
 * 
 * converts a #NspMatrix @A to a string matrix using nsp default format 
 * if @flag is equal to 0 and the given format if flag is equal to one.
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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

/**
 * nsp_smatrix_tolower:
 * @A: a #NspSMatrix 
 * 
 * applies tolower() to each entry of string matrix @A.
 **/

void nsp_smatrix_tolower(NspSMatrix *A)
{
  int i;
  unsigned int j;
  for ( i = 0 ; i < A->mn ; i++ ) 
    if ( A->S[i] != (nsp_string) 0 ) 
      for ( j = 0 ; j < strlen(A->S[i]) ; j++ ) A->S[i][j]= tolower(A->S[i][j]);
}

/**
 * nsp_smatrix_toupper:
 * @A: a #NspSMatrix 
 * 
 * applies toupper() to each entry of string matrix @A.
 **/

void nsp_smatrix_toupper(NspSMatrix *A)
{
  int i;
  unsigned int j;
  for ( i = 0 ; i < A->mn ; i++ ) 
    if ( A->S[i] != (nsp_string) 0) 
      for ( j = 0 ; j < strlen(A->S[i]) ; j++ )  A->S[i][j]= toupper(A->S[i][j]);
}


/**
 * nsp_smatrix_capitalize:
 * @A: a #NspSMatrix 
 * 
 * capitalize entries of string matrix @A
 **/

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


/**
 * nsp_smatrix_strstr:
 * @A: a #NspSMatrix 
 * @Str: string to be searched 
 * 
 * Returns in a #NspMatrix of same dimension as @A, the index of @Str in each 
 * @A entry or 0 if @Str is not a substring of the entry.
 * 
 * Return value:  a new #NspMatrix or %NULLMAT 
 **/

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
 * nsp_smatrix_split_string:
 * @string: the string to be splitted in words
 * @splitChars: a string with the split characters
 * @msep: an integer
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


/**
 * nsp_smatrix_split:
 * @Src: a String matrix to be splited 
 * @splitChars: a string with the split characters
 * @msep: an integer
 * 
 * uses nsp_smatrix_split_string() to split each string of string matrix @Src. 
 * If each string of the matrix is splited in the same number of tokens then 
 * the result is returned in a string matrix where each line contains a splited 
 * string of an element of @Src (@Src in column order mode).
 *
 * Return value: A new #NspSMatrix or %NULLSMAT
 **/

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

/**
 * nsp_row_smatrix_append_string:
 * @A: a #NspSMatrix 
 * @str: string to be added
 * 
 * enlarges a row string matrix by adding a new string at the end.
 * 
 * Return value:  %OK or %FAIL
 **/

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


/**
 * SMatCompOp:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix 
 * @op:  a string which gives the logical operation which is to be performed.
 * 
 * returns a boolean matrix filled with @A(i,j) @op @B(i,j). 
 * Note that @A and @B must have the same dimensions with the 
 * convention that 1x1 matrices can match any dimensions. 
 * 
 * Return value:  a new #NspBMatrix or %NULLBMAT 
 **/

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

/**
 * SMatFullComp:
 * @A: a #NspSMatrix 
 * @B: a #NspSMatrix 
 * @op: a string which gives the logical operation which is to be performed.
 * @err: set to 1 is dimensions are incompatible else to 0.
 * 
 * returns %TRUE if @A(i,j) @op @B(i,j) is %TRUE for all entries. 
 * Note that @A and @B must have the same dimensions with the 
 * convention that 1x1 matrices can match any dimensions. 
 * 
 * Return value: %TRUE or %FALSE.
 **/

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


/**
 * nsp_smatrix_transpose:
 * @A: a #NspSMatrix 
 * 
 * Transpose A 
 * 
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/

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

/**
 * nsp_smatrix_subst:
 * @A: a #NspSMatrix 
 * @needle: 
 * @replace: 
 * 
 * 
 * 
 * Return value:  a new #NspSMatrix or %NULLSMAT 
 **/
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


/**
 * nsp_smatrix_strip_blanks:
 * @A: a #NspSMatrix 
 * 
 * strips blanck characters at the begining and end of each entry of @A.
 * 
 * Return value: %OK or %FAIL
 **/

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

/**
 * nsp_print_string_as_read:
 * @str: string to be printed 
 * 
 * prints a string using Sciprintf() in such a way that the 
 * string can be re-parsed by nsp.
 **/

void nsp_print_string_as_read(const char *str)
{
  Sciprintf("\"");
  while ( *str != '\0') 
    {
      switch (*str) 
	{
	case '\'' :
	case '\"' : 
	  Sciprintf("%s","''");break;
	case '\\' :
	  Sciprintf("%s","\\\\");break;
	case '\a' :
	  Sciprintf("%s","\\a"); break;
	case '\b' :
	  Sciprintf("%s","\\b"); break;
	case '\f' :
	  Sciprintf("%s","\\f"); break;
	case '\n' :
	  Sciprintf("%s","\\n");break;
	case '\r' :
	  Sciprintf("%s","\\r"); break;
	case '\t' :
	  Sciprintf("%s","\\t"); break;
	case '\v' :
	  Sciprintf("%s","\\v"); break;
	default: 
	  if (isprint(*str)) 
	    Sciprintf("%c",*str);
	  else 
	    Sciprintf("\\%o",*str);
	}
      str++;
    }
  Sciprintf("\"");
}


static void SMij_string_as_read(const nsp_num_formats *fmt,const void *m, int i, int j)
{
  const NspSMatrix *M=m;
  nsp_print_string_as_read(M->S[i+(M->m)*j]);
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
  if ((Iloc =nsp_alloc_int(m->n)) == (int*) 0) return TRUE;
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
      return TRUE;
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
	      if ( imore == 1) return TRUE;
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
  return TRUE;
}



/**
 * nsp_smatrix_strtod:
 * @S: a #NspSMatrix
 * 
 * converts a #NspSMatrix to a #NspMatrix using strtod().
 * 
 * Return value:  a new #NspMatrix or %NULLMAT 
 **/

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


/**
 * nsp_smatrix_unique:
 * @x: (input-output) #NspSMatrix
 * @Ind: (output) if NOT NULL a real #NspMatrix (see after)
 * @Occ: (output) if NOT NULL a real #NspMatrix (see after)
 * @first_ind: (input) used in case Ind is not NULL true if Ind[i]
 *             should be the first index of x[i] in the original @x
 *
 * Removes from @x multiple occurences of identical entries. Thus after 
 * calling this function @x will contain strictly different values sorted 
 * in increasing order. If @Ind is non null, it will be set to 
 * a new NspMatrix filled with the indice in the original array of each 
 * output value in @x. Thus @Ind[i] will contain the original indice of 
 * @x[i] in the input array @x. Note that indices in @Ind start at 1 !
 * If @Occ is non null, then @Occ[i] will contain the number of occurences 
 * in the input matrix @x of the output value @x[i]. 
 *
 * Return value: %OK or %FAIL
 *
 **/
int nsp_smatrix_unique(NspSMatrix *x, NspMatrix **Ind, NspMatrix **Occ, Boolean first_ind)
{
  int i0, i, i_old, *index;
  NspMatrix *ind=NULLMAT, *occ=NULLMAT;
  nsp_string val;

  if ( Ind == NULL )
    {
      if ( x->mn <= 1 ) return OK;
      nsp_qsort_nsp_string(x->S, NULL, 0, x->mn, 'i');
      i0 = 0; val = x->S[0];
      for ( i = 1 ; i < x->mn ; i++ )
	if ( strcmp(x->S[i],val) != 0 )
	  {
	    val = x->S[i];
	    i0++;
	    if ( i > i0 )
	      {
		nsp_string_destroy(&(x->S[i0]));
		x->S[i0] = val; x->S[i] = NULLSTRING;
	      }
	  }
      if ( x->m == 1 )
	nsp_smatrix_resize(x, 1, i0+1);
      else
	nsp_smatrix_resize(x, i0+1, 1);
      return OK;
    }

  else
    {
      if ( (ind = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	return FAIL;
      index = (int *) ind->R;

      if ( Occ != NULL )
	if ( (occ = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	  {
	    nsp_matrix_destroy(ind); return FAIL;
	  }
      
      if ( x->mn > 0 )
	{
	  if ( first_ind )
	    nsp_sqsort_bp_nsp_string( x->S, x->mn, index, 'i');
	  else
	    nsp_qsort_nsp_string( x->S, index, 1, x->mn, 'i');
      
	  i0 = 0; i_old = 0; val = x->S[0];
	  for ( i = 1 ; i < x->mn ; i++ )
	    {
	      if ( strcmp(x->S[i], val) != 0 )
		{
		  if (Occ != NULL) { occ->R[i0] = i - i_old; i_old = i; }
		  i0++;
		  val = x->S[i];
		  if ( i > i0 )
		    {
		      nsp_string_destroy(&(x->S[i0]));
		      x->S[i0] = val; x->S[i] = NULLSTRING;
		    }
		  index[i0] = index[i];
		}
	    }
	  if (Occ != NULL) occ->R[i0] = x->mn - i_old;
	  
	  if ( x->m == 1 )
	    {
	      nsp_smatrix_resize(x, 1, i0+1);
	      nsp_matrix_resize(ind, 1, i0+1);
	      if ( Occ != NULL ) nsp_matrix_resize(occ, 1, i0+1);
	    }
	  else
	    {
	      nsp_smatrix_resize(x, i0+1, 1);
	      nsp_matrix_resize(ind, i0+1, 1);
	      if ( Occ != NULL ) nsp_matrix_resize(occ, i0+1, 1);
	    }
	  ind->convert = 'i';
	  ind = Mat2double(ind);
	}

      *Ind = ind;
      if ( Occ != NULL ) *Occ = occ;
      return OK;
    }
}

/**
 * nsp_smatrix_has:
 * @A: (input) #NspSMatrix
 * @x: (input) #NspSMatrix
 * @lhs: (input) dim parameter: for lhs=2 ind must be computed, for lhs=3, ind and ind2 must be computed
 * @ind: (optional output) 
 * @ind2: (optional output) 
 *
 * looks for each component of @x if it is in @A returning the value in a boolean matrix of same size as @x. 
 * Additional indices are returned is requested i.e a one dimensional indice is returned in @ind if lhs=2 and 
 * two indices are returned in @ind (row indice) and @ind2 (column indice) if lhs=3. 
 * 
 * Return value: a NspBMatrix
 **/

NspBMatrix *nsp_smatrix_has(NspSMatrix *A, NspSMatrix *x, int lhs, NspMatrix **ind, NspMatrix **ind2)
{
  NspBMatrix *B=NULLBMAT;
  NspMatrix *Ind=NULLMAT, *Ind2=NULLMAT;
  int i, k;

  if ( (B = nsp_bmatrix_create(NVOID,x->m,x->n)) == NULLBMAT )
    return NULLBMAT;

  for ( k = 0 ; k < x->mn ; k++ ) B->B[k] = FALSE;

  if ( lhs >= 2 )
    {
      if ( (Ind = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	goto err;
      for ( k = 0 ; k < x->mn ; k++ ) Ind->R[k] = 0.0;
      
      if ( lhs == 3 )
	{
	  if ( (Ind2 = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	    goto err;
	  for ( k = 0 ; k < x->mn ; k++ ) Ind2->R[k] = 0.0;
	}
    }

  for ( k = 0 ; k < x->mn ; k++ )
    {
      for ( i = 0 ; i < A->mn ; i++ )
	if ( strcmp(A->S[i],x->S[k]) == 0 )
	  {
	    B->B[k] = TRUE;
	    if ( lhs == 2 )
	      Ind->R[k] = i+1;
	    else if ( lhs == 3 )
	      {
		Ind->R[k] = (i % A->m) + 1; Ind2->R[k] = i / A->m + 1;
	      }
	    break;
	  }
    }

  if ( lhs >= 2 )
    {
      *ind = Ind;
      if ( lhs == 3 )
	*ind2 = Ind2;
    }
  return B;

 err:
  nsp_bmatrix_destroy(B);
  nsp_matrix_destroy(Ind);
  nsp_matrix_destroy(Ind2);
  return NULLBMAT;
}
