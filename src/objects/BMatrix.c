/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/interf.h" /* for ret_endfor */
#include <nsp/matutil.h> /* icopy iset */


static int nsp_bmatrix_print_internal (nsp_num_formats *fmt,NspBMatrix *cm, int indent);

/**
 * nsp_bmatrix_create:
 * @name: matrix name 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a new boolean matrix filled with %TRUE values.
 * 
 * Return value: a #NspMatrix or %NULLMAT in case of allocation failure.
 */
  
NspBMatrix  *nsp_bmatrix_create(const char *name, int m, int n)
{
  int i;
  NspBMatrix *Loc= new_bmatrix();

  if ( Loc == NULLBMAT) 
    { 
      Scierror("BMatCreate : Error no more space ");
      return(NULLBMAT);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(Loc),name) == NULL)
    return(NULLBMAT);
  NSP_OBJECT(Loc)->ret_pos = -1 ; /* XXXX must be added to all data types */ 

  Loc->m =m;
  Loc->n = n;
  Loc->mn=m*n;
  if ( (  Loc->B = (Boolean *) MALLOC( Loc->mn* sizeof(Boolean)))
       == (Boolean *) 0 )
    { 
      Scierror("BMatCreate : Error no more space ");
      return(NULLBMAT);
    }
  /* we could use a iset nsp_iset(&ns,&d,A->B+Asize,&inc); **/
  for ( i = 0 ; i < Loc->mn ; i++ )   Loc->B[ i] = TRUE ;
  return(Loc);
}

/**
 * nsp_bmatrix_clone:
 * @name: matrix name 
 * @A: a #NspBMatrix
 * @m: number of rows 
 * @n: number of columns 
 * @init: unsused init flag.
 * 
 * Creates a new boolean matrix with with unspecified values. @A is not 
 * used for #NspBMatrix.
 * 
 * Return value: a #NspBMatrix or %NULLMAT in case of allocation failure.
 */

NspBMatrix *nsp_bmatrix_clone(const char *name, NspBMatrix *A, int m, int n, int init)
{
  return nsp_bmatrix_create(name, m, n);
}


/**
 * nsp_bmatrix_copy:
 * @A: a #NspBMatrix 
 *
 * copies #NspBMatrix @A and returns the copy or %NULLBMAT.
 * 
 * Return value: a #NspMatrix or %NULLBMAT.
 */

NspBMatrix  *nsp_bmatrix_copy(NspBMatrix *A)
{
  int i;
  NspBMatrix *Loc;
  if ( ( Loc =nsp_bmatrix_create(NVOID,A->m,A->n) ) == NULLBMAT) 
    return(NULLBMAT);
  for ( i = 0 ; i < Loc->mn ; i++ ) Loc->B[i] = A->B[i];
  return(Loc);
}


/**
 * nsp_bmatrix_elt_size:
 * @M: a #NspMatrix 
 * 
 * size of matrix elements.
 * 
 * Return value: size of elements of matrix @M. 
 **/

unsigned int  nsp_bmatrix_elt_size(NspMatrix *M)
{
  return sizeof(int);
}


/**
 * nsp_bmatrix_resize:
 * @A: a #NspBMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspBMatrix @A dimensions are changed to be @m x @n. 
 * This routine only enlarges or shrink (using realloc()) 
 * the data array of @A to size mxn. The previous data are not moved and 
 * occupy the first array cells. Note that @A can be 
 * and empty matrix when calling this routine ( malloc() is used in that 
 * case ). 
 *
 * Return value: %OK or %FAIL. When %OK is returned @A is changed. 
 */


int nsp_bmatrix_resize(NspBMatrix *A, int m, int n) 
{
  if ( A->mn == m*n ) 
    {
      A->m=m;
      A->n=n;
      return(OK);
    };
  if ( m*n < 0) return FAIL;
  if ( m*n == 0 ) 
    {
      A->m =  A->n = A->mn= 0;
      FREE(A->B);
      return OK;
    }
  A->m =m ;  A->n =n;   A->mn=m*n ;
  /* Realloc will realloc or alloc depending on A->B **/
  A->B=nsp_realloc_int(A->B,A->mn);
  if ( A->B == (int *) 0) return(FAIL);
  return(OK);
}

/**
 * nsp_bmatrix_destroy:
 * @BMat: a #NspBMatrix 
 * 
 * frees the #NspMatrix @BMat. 
 */

void nsp_bmatrix_destroy(NspBMatrix *BMat)
{
  if ( BMat != NULLBMAT)
    {
      FREE(BMat->B);
      nsp_object_destroy_name(NSP_OBJECT(BMat));
      FREE(BMat) ;
    };
}

/**
 * nsp_bmatrix_info:
 * @BMat: a #NspBMatrix
 * @indent: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 *
 * Displays info on the #NspBMatrix @A using the default Sciprintf() function. 
 * @indent is the given indentation for printing. If the #NspBMatrix has a name 
 * then this name is displayed but it can be replaced by the value of the argument 
 * @name is such an argument is non null. 
 * 
 * Return value: %TRUE or %FALSE
 */

int nsp_bmatrix_info(NspBMatrix *BMat, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(BMat)->name;
  if ( BMat->m >=1 &&  BMat->mn >= 2 ) 
    {
      Sciprintf1(indent,"%s\t= [...]\t\tb (%dx%d)\n",pname,BMat->m,BMat->n);
    }
  else
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      Sciprintf1(indent,"%s\t= [ %s]\t\tb (%dx%d)\n",pname,
		 (BMat->mn != 0) ? ((BMat->B[0]==FALSE) ? "F " : "T " ) : "",
		 BMat->m,BMat->n);
    }
  return TRUE;

}


/**
 * nsp_bmatrix_print:
 * @BMat: a #NspBMatrix
 * @indent: an int 
 * @name: %NULL or name to be used.
 * @rec_level: deph level of the print.
 *
 * Print the #NspMBatrix @A using the default nsp output function. 
 * @indent is the given indentation for printing. If the #NspBMatrix has a name 
 * then this name is displayed but it can be replaced by the value of the argument 
 * @name is such an argument is non null. 
 * 
 * Return value: %TRUE or %FALSE
 */

int nsp_bmatrix_print(NspBMatrix *BMat, int indent,const char *name, int rec_level)
{
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(BMat)->name;

  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=%s",pname,(BMat->mn==0 ) ? " m2b([])\n" : "" );
	}
      else 
	{
	  Sciprintf1(indent,"%s",(BMat->mn==0 ) ? " m2b([])\n" : "" );
	}
    }
  else 
    {
      if ( user_pref.pr_depth  <= rec_level -1 ) 
	{
	  nsp_bmatrix_info(BMat,indent,pname,rec_level);
	  return rep;
	}
      Sciprintf1(indent,"%s\t=%s\t\t b (%dx%d)\n",pname,
		(BMat->mn==0 ) ? " []" : "",BMat->m,BMat->n);
    }
  if ( BMat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      rep =nsp_bmatrix_print_internal (&fmt,BMat,indent);
    }
  return rep;
}


/**
 * nsp_bmatrix_latex_print:
 * @BMat: a #NspBMatrix
 * 
 * print the #NspBMatrix @A using the default Sciprintf() function and LaTeX 
 * syntax. 
 *
 * Return value: %TRUE or %FALSE
 */

int nsp_bmatrix_latex_print(NspBMatrix *BMat)
{
  int i,j;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf("{%s = \\left(\\begin{array}{",NSP_OBJECT(BMat)->name );
  for (i=0; i <  BMat->n;i++) Sciprintf("c");
  Sciprintf("}\n");
  for (i=0; i < BMat->m; i++)
    {
      for (j=0; j < BMat->n - 1; j++)
	{ 
	  Sciprintf("%d\t& ",BMat->B[i+j*BMat->m]);
	}
      Sciprintf("%d\t",BMat->B[i+(BMat->n-1)*BMat->m]);
      if ( i != BMat->m -1 ) 
	Sciprintf("\\\\\n");
      else 
	Sciprintf("\n");
    }

  Sciprintf("\\end{array}\\right)}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}

/**
 * nsp_bmatrix_latex_tab_print:
 * @BMat: a #NspBMatrix
 * 
 * print the #NspBMatrix @A using the default Sciprintf() function and LaTeX tabular
 * syntax. 
 *
 * Return value: %TRUE or %FALSE
 */


int nsp_bmatrix_latex_tab_print(NspBMatrix *BMat)
{
  int i,j;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf("\\begin{tabular}{|l|");
  for (i=0; i < BMat->n ;i++) Sciprintf("c|");
  Sciprintf("}\\hline\n %s &\t",NSP_OBJECT(BMat)->name);
  for (i=1; i < BMat->n ;i++) Sciprintf("$C_{%d}$\t&",i);
  Sciprintf("$C_{%d}$\\\\ \\hline\n",BMat->n);
  for (i=0; i < BMat->m; i++)
    {
      Sciprintf("$L_{%d}$\t&",i+1);
      for (j=0; j < BMat->n - 1; j++)
	{ 
	  Sciprintf("$%d$\t& ",BMat->B[i+j*BMat->m]);
	}
      Sciprintf("$%d$\t\\\\ \\hline\n",BMat->B[i+(BMat->n-1)*BMat->m]);
    }
  Sciprintf("\\end{tabular}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}


/**
 * nsp_bmatrix_enlarge:
 * @A: a #NspBMatrix
 * @m: number of rows 
 * @n: number of columns
 * 
 * Changes the #NspBMatrix @A size to max(A->m,m) x max(A->n,n) adding 
 * rows and columns of zero to @A. 
 * A = [ A ,ones(A->m,Max(n-A->n,0); 
 *      ones(Max(m-A->m,0), Max(A->n,n)) ]
 * 
 * returns: %OK or %FAIL.
 */


int nsp_bmatrix_enlarge(NspBMatrix *A, int m, int n)
{
  if ( A->mn == 0)
    {
      int i;
      if (nsp_bmatrix_resize(A,m,n) == FAIL) return FAIL;
      for ( i = 0 ; i < A->mn ; i++ ) A->B[ i] = FALSE; /*TRUE ; a voir */
    }
  if ( n > A->n  )
    if (nsp_bmatrix_add_columns(A,n- A->n) == FAIL) return(FAIL);
  if ( m > A->m  )  
    if (nsp_bmatrix_add_rows(A, m - A->m) == FAIL) return(FAIL);
  return(OK);
}

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/**
 * nsp_bmatrix_concat_right:
 * @A: a #NspBMatrix
 * @B: a #NspBMatrix
 * 
 * Changes the #NspBMatrix @A to [@A , @B ] ; 
 * 
 * returns: %OK or %FAIL.
 */


int nsp_bmatrix_concat_right(NspBMatrix *A, NspBMatrix *B)
{
  int inc = 1;
  int Asize;
  Asize=A->mn;
  if ( A->m != B->m ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(FAIL);
    }
  if (nsp_bmatrix_resize(A,A->m,A->n+B->n) == FAIL) return(FAIL);
  nsp_icopy(&B->mn,B->B,&inc,A->B+Asize,&inc);
  return(OK);
}

/**
 * nsp_bmatrix_add_columns:
 * @A: a #NspBMatrix
 * @n: number of columns 
 * 
 * Adds @n columns to the #NspBMatrix. Note that 
 * if @A is an empy matrix on entry it remains empty.
 * 
 * returns: %OK or %FAIL.
 */

int nsp_bmatrix_add_columns(NspBMatrix *A, int n)
{
  int d=FALSE; /* TRUE; */
  int inc = 1,ns;
  int Asize;
  if (n == 0) return OK;
  else if ( n < 0) 
    {      
      Scierror("Error: Negative indice (%d) in BMatAddCols\n",n);
      return FAIL;
    }
  Asize=A->mn;
  ns= (A->m)*n;
  if (nsp_bmatrix_resize(A,A->m,A->n+n) == FAIL) return(FAIL);
  nsp_iset(&ns,&d,A->B+Asize,&inc);
  return(OK);
}

/**
 * nsp_bmatrix_concat_down:
 * @A: a #NspBMatrix
 * @B: a #NspBMatrix
 * 
 * returns a new #NspBMatrix equals to  [@A ; @B ] ; 
 * 
 * returns: a #NspBMatrix or %NULLMAT.
 */

NspBMatrix *nsp_bmatrix_concat_down(NspBMatrix *A, NspBMatrix *B)
{
  NspBMatrix *Loc;
  int inc = 1;
  int j;
  if ( A->n != B->n ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(NULLBMAT);
    }
  if ( ( Loc =nsp_bmatrix_create(NVOID,A->m+B->m,A->n)) == NULLBMAT) 
    return(NULLBMAT);
  for ( j = 0 ; j < A->n ; j++ ) 
    {
      nsp_icopy(&A->m,A->B+j*A->m,&inc,Loc->B+j*(Loc->m),&inc);
      nsp_icopy(&B->m,B->B+j*B->m,&inc,Loc->B+j*(Loc->m)+A->m,&inc);
    }
  return(Loc) ;
}

/**
 * nsp_bmatrix_concat_diag:
 * @A: a #NspBMatrix
 * @B: a #NspBMatrix
 * 
 * returns a new #NspBMatrix equals to  [@A,0 ;0, @B ] ; 
 * 
 * returns: a #NspBMatrix or %NULLMAT.
 */


NspBMatrix *nsp_bmatrix_concat_diag(NspBMatrix *A, NspBMatrix *B)
{
  NspBMatrix *Loc;
  int d=FALSE; /* TRUE; */
  int inc = 1;
  int j;
  if ( ( Loc =nsp_bmatrix_create(NVOID,A->m+B->m,A->n+B->n)) == NULLBMAT) 
    return(NULLBMAT);
  for ( j = 0 ; j < A->n ; j++ ) 
    {
      nsp_icopy(&A->m,A->B+j*A->m,&inc,Loc->B+j*(Loc->m),&inc);
      nsp_iset(&B->m,&d,Loc->B+j*(Loc->m)+A->m,&inc);
    }
  for ( j = 0 ; j < B->n ; j++ ) 
    {
      nsp_icopy(&B->m,B->B+j*A->m,&inc,Loc->B+(j+A->n)*(Loc->m)+A->m,&inc);
      nsp_iset(&A->m,&d,Loc->B+(j+A->n)*(Loc->m),&inc);
    }

  return(Loc) ;
}

/**
 * nsp_bmatrix_add_rows:
 * @A: a #NspBMatrix
 * @m: number of rows
 * 
 * Adds @n rows to the #NspBMatrix. Note that 
 * if @A is an empy matrix on entry it remains empty.
 * 
 * returns: %OK or %FAIL.
 */

int nsp_bmatrix_add_rows(NspBMatrix *A, int m)
{
  int d=FALSE; /* TRUE; */
  int inc = -1,Am;
  int j;
  if ( m == 0) return OK;
  else if ( m < 0) 
    {      
      Scierror("Error: Negative indice (%d) in BMatAddRows\n",m);
      return FAIL;
    }
  Am= A->m;
  if (nsp_bmatrix_resize(A,A->m+m,A->n)== FAIL) return(FAIL);
  for ( j = A->n-1  ; j >= 0 ; j-- ) 
    {
      if ( j != 0) nsp_icopy(&Am,A->B+j*Am,&inc,A->B+j*(A->m),&inc);
      nsp_iset(&m,&d,A->B+j*(A->m)+Am,&inc);
    }
  return(OK);
}

/**
 * nsp_bmatrix_set_submatrix:
 * @A: a #NspBMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 * @B: a #NspBMatrix
 * 
 * Performe  A(Rows,Cols) = B. A is changed and enlarged if necessary and 
 * size compatibility is checked i.e B must be scalar or  
 * we must have size(B)==[size(Rows),size(Cols)]. 
 * 
 * returns: %OK or %FAIL.
 */

int nsp_bmatrix_set_submatrix(NspBMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspBMatrix *B)
{
  return nsp_matint_set_submatrix1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols),NSP_OBJECT(B));
}


/** 
 * nsp_bmatrix_set_rows:
 * @A: a #NspBMatrix
 * @Rows: a #NspMatrix
 * @B: a #NspBMatrix
 * 
 * Performs  A(Inds) = B. A is changed and enlarged if necessary 
 * Inds is unchanged 
 * Size Compatibility is checked 
 * Rules : A Matrix or A ==[]  
 *	A(Inds)=B 
 *	B must be row or column 
 *	if A==[] the size of the result depends on b 
 * 
 *      A row vector B must be row 
 *      A column vector B must be column 
 *      Inds must be in the range of A indices unless A is row or column or []
 *      Inds and B must have the same size or B must be scalar 
 * 
 * returns: %OK or %FAIL.
 */

int nsp_bmatrix_set_rows(NspBMatrix *A, NspMatrix *Rows, NspBMatrix *B)
{
  return nsp_matint_set_elts1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(B));
}

/**
 * nsp_bmatrix_extract:
 * @A: a #NspBMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(Rows,Cols) and returns the new #NspBMatrix 
 * 
 * returns: a #NspBMatrix or %NULLMAT 
 */

NspBMatrix  *nsp_bmatrix_extract(NspBMatrix *A, NspMatrix *Rows, NspMatrix *Cols)
{
  return (NspBMatrix*)nsp_matint_extract1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols));
}

/**
 * nsp_bmatrix_extract_elements:
 * @A: a #NspBMatrix
 * @Elts: a #NspMatrix
 *
 * Compute A(Elts) and returns the new #NspBMatrix 
 * 
 * returns: a #NspBMatrix or %NULLMAT 
 */

NspBMatrix  *nsp_bmatrix_extract_elements(NspBMatrix *A, NspMatrix *Elts)
{
  return (NspBMatrix *) nsp_matint_extract_elements1(NSP_OBJECT(A),NSP_OBJECT(Elts));
}

/**
 * nsp_bmatrix_extract_columns:
 * @A: a #NspBMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(:,Cols) and returns the new #NspBMatrix 
 * 
 * returns: a #NspBMatrix or %NULLMAT 
 */

NspBMatrix  *nsp_bmatrix_extract_columns(NspBMatrix *A, NspMatrix *Cols)
{
  return (NspBMatrix *) nsp_matint_extract_columns1(NSP_OBJECT(A),NSP_OBJECT(Cols));
}

/**
 * nsp_bmatrix_extract_rows:
 * @A: a #NspBMatrix
 * @Rows: a #NspMatrix
 *
 * Compute A(Rows,:) and returns the new #NspBMatrix 
 * 
 * returns: a #NspBMatrix or %NULLMAT 
 */

NspBMatrix  *nsp_bmatrix_extract_rows(NspBMatrix *A, NspMatrix *Rows)
{
  return (NspBMatrix *) nsp_matint_extract_rows1(NSP_OBJECT(A),NSP_OBJECT(Rows));
}

/*
 * A1=BMatLoopCol(A1,M,i,rep)
 * Used in for loops 
 */

NspBMatrix  *BMatLoopCol(char *str, NspBMatrix *Col, NspBMatrix *A, int icol, int *rep)
{
  int i;
  register int iof;
  NspBMatrix *Loc;
  if ( icol > A->n )
    {
      *rep = RET_ENDFOR;
      return(NULLBMAT);
    }
  *rep =0;
  if ( Col == NULLBMAT) 
    Loc =nsp_bmatrix_create(str,A->m,1);
  else 
    Loc = Col;
  if ( Loc == NULLBMAT) return(NULLBMAT);
  iof = (icol-1)*A->m;
  /* Coulb be a icopy XXXXXX **/
  for ( i = 0 ; i < A->m ; i++)
    {
      Loc->B[i]= A->B[i+iof] ;
    }
  return(Loc);
}
/**
 * nsp_bmatrix_extract_diag:
 * @A: a #NspBMatrix
 * @k: an integer 
 *
 * Extract the @k-th diagonal of matrix @A and returns 
 * its value as a column vector. 
 * 
 * returns: a #NspBMatrix or %NULLMAT 
 */

NspBMatrix  *nsp_bmatrix_extract_diag(NspBMatrix *A, int k)
{
  NspBMatrix *Loc;
  int j,i;
  int imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc =nsp_bmatrix_create(NVOID,(int) 0 , (int) 0);
      return(Loc);
    }
  if (( Loc =nsp_bmatrix_create(NVOID,imax-imin,(int)1)) == NULLBMAT)
    return(NULLBMAT);
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    Loc->B[j++] = A->B[i+(i+k)*A->m];
  return(Loc);
}

/**
 * nsp_bmatrix_set_diag:
 * @A: a #NspBMatrix
 * @Diag: a #NspMatrix
 * @k: an integer 
 *
 * sets the @k-th diagonal of matrix @A with values from @Diag. 
 * 
 * returns: %OK or %FAIL.
 */

int nsp_bmatrix_set_diag(NspBMatrix *A, NspBMatrix *Diag, int k)
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
      if (nsp_bmatrix_enlarge(A,imax,imax+k) == FAIL) return(FAIL);
    }
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    A->B[i+(i+k)*A->m] = Diag->B[j++] ;

  return(OK);
}

/**
 * nsp_bmatrix_create_diag:
 * @Diag: a #NspBMatrix
 * @k: an integer 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns: a #NspBMatrix or %NULLMAT 
 */


NspBMatrix  *nsp_bmatrix_create_diag(NspBMatrix *Diag, int k)
{
  int i,j;
  NspBMatrix *Loc;
  int imin = Max(0,-k);
  int imax = Diag->mn +imin;
  int nd = Diag->mn+Abs(k);
  if (( Loc =nsp_bmatrix_create(NVOID,nd,nd)) == NULLBMAT) 
    return(NULLBMAT);
  j=0;
  for ( i = imin ; i < imax ; i++ ) 
    Loc->B[i+(i+k)*Loc->m] = Diag->B[j++] ;
  return(Loc);
}

/**
 * nsp_bmatrix_transpose: 
 * @A: a #NspBMatrix
 *
 * return the transpose of A
 * 
 * returns: a #NspBMatrix or %NULLMAT 
 */

NspBMatrix  *nsp_bmatrix_transpose(NspBMatrix *A)
{
  int i,j;
  NspBMatrix *Loc;
  if (( Loc =nsp_bmatrix_create(NVOID,A->n,A->m)) == NULLBMAT) return(NULLBMAT);
  for ( i = 0  ; i < A->m ; i++) 
    for ( j = 0 ; j < A->n ; j++) 
      Loc->B[j+Loc->m*i ] = A->B[i+A->m*j];
  return Loc;
}

/**
 * nsp_matrix_to_bmatrix:
 * @M: a #NspMatrix
 * 
 * #NspMatrix to #NspBMatrix conversion 
 * 
 * Return value: a #NspBMatrix or %NULLBMAT.
 */

NspBMatrix  *nsp_matrix_to_bmatrix(NspMatrix *M)
{
  int i;
  NspBMatrix *Loc;
  if (( Loc =nsp_bmatrix_create(NVOID,M->m,M->n)) == NULLBMAT) 
    return(NULLBMAT);
  if ( M->rc_type == 'r') 
    for ( i = 0 ; i < M->mn ; i++ ) 
      Loc->B[i] = ( M->R[i] != 0.0) ? TRUE : FALSE ;
  else
    for ( i = 0 ; i < M->mn ; i++ ) 
      Loc->B[i] = ( M->C[i].r != 0.0 || M->C[i].i != 0.0) ? TRUE : FALSE;
  return(Loc);
}


/**
 * nsp_bmatrix_to_matrix:
 * @M: a #NspBMatrix
 * 
 * #NspBMatrix to #NspMatrix conversion 
 * Return value: a #NspMatrix or %NULLMAT.
 */

NspMatrix *nsp_bmatrix_to_matrix(NspBMatrix *M)
{
  int i;
  NspMatrix *Loc;
  if (( Loc = nsp_matrix_create(NVOID,'r',M->m,M->n)) == NULLMAT) 
    return(NULLMAT);
  for ( i = 0 ; i < M->mn ; i++ ) 
    Loc->R[i] = M->B[i] ;
  return(Loc);
}

/**
 * nsp_bmatrix_and
 * @A: a #NspBMatrix
 * @B: a #NspBMatrix
 *  
 * term to term logical and  A = A & B. If %OK is returned @A is changed. 
 * Return value: %OK or %FAIL. 
 */

int nsp_bmatrix_and(NspBMatrix *A,const  NspBMatrix *B)
{
  int i;
  if ( A->mn != B->mn ) 
    {
      Scierror("BUG Not Same Size ");
      return(FAIL);
    }
  for ( i = 0 ; i < A->mn ; i++) 
    {
      A->B[i]   &= B->B[i];
    }
  return(OK);
}

/**
 * nsp_bmatrix_scalar_and
 * @A: a #NspBMatrix. 
 * @B: a #NspBMatrix of size 1x1
 *  
 * Logical and operator between a boolean matrix @A and a boolean scalar @B. The result 
 * is stored in @A.
 * Return value: %OK.
 */

int nsp_bmatrix_scalar_and(NspBMatrix *A,const  NspBMatrix *B)
{
  int i;
  for ( i = 0 ; i < A->mn ; i++) 
    {
      A->B[i]   &= B->B[0];
    }
  return(OK);
}


/**
 * nsp_bmatrix_or
 * @A: a #NspBMatrix
 * @B: a #NspBMatrix
 *  
 * term to term logical or A = A | B. If %OK is returned @A is changed. 
 * Return value: %OK or %FAIL. 
 */

int nsp_bmatrix_or(NspBMatrix *A,const  NspBMatrix *B)
{
  int i;
  if ( A->mn != B->mn ) 
    {
      Scierror("BUG Not Same Size ");
      return(FAIL);
    }
  for ( i = 0 ; i < A->mn ; i++) 
    {
      A->B[i]   |= B->B[i];
      
    }
  return(OK);
}


/**
 * nsp_bmatrix_scalar_or
 * @A: a #NspBMatrix. 
 * @B: a #NspBMatrix of size 1x1
 *  
 * Logical or operator between a boolean matrix @A and a boolean scalar @B. The result 
 * is stored in @A.
 * Return value: %OK.
 */

int nsp_bmatrix_scalar_or(NspBMatrix *A,const  NspBMatrix *B)
{
  int i;
  for ( i = 0 ; i < A->mn ; i++) 
    {
      A->B[i]   |= B->B[0];
    }
  return(OK);
}


/**
 * nsp_bmatrix_not
 * @A: a #NspBMatrix. 
 *  
 * Logical not operator applied on  a boolean matrix @A. 
 * Return value: %OK.
 */


int nsp_bmatrix_not(NspBMatrix *A)
{
  int i;
  for ( i = 0 ; i < A->mn ; i++) 
    {
      A->B[i]   = (A->B[i] == 1) ? 0 : 1;
    }
  return(OK);
}

/**
 * nsp_bmatrix_count_true:
 * @A: a #NspBMatrix. 
 * 
 * counts the number of %TRUE in #NspBMatrix @A.
 * returns: this number in a #NspMatrix.
 **/

NspMatrix *nsp_bmatrix_count_true(const NspBMatrix *A)
{
  int i;
  NspMatrix *Loc = nsp_matrix_create(NVOID,'r',(int) 1,(int) 1);
  if ( Loc == NULLMAT) return(NULLMAT);
  Loc->R[0] = 0;
  for ( i=0; i < A->mn ; i++ ) 
    {
      if ( A->B[i]) Loc->R[0]++;
    }
  return(Loc);
}


/**
 * nsp_bmatrix_find:
 * @A: a #NspBMatrix. 
 * 
 * returns in a #NspMatrix the indices for which the 
 * #NspBMatrix @A is true considering @A as o one dimensional array.
 * 
 * Return value:  a new #NspMatrix or %NULLMAT
 */

NspMatrix *nsp_bmatrix_find(const NspBMatrix *A)
{
  NspMatrix *Res;
  int i,count=0, nrow = ( A->mn == 0) ? 0: 1;
  /* first pass for counting **/
  for ( i=0 ; i < A->mn ; i++)
    {
      if ( A->B[i] ) count++;
    }
  Res = nsp_matrix_create(NVOID,'r', nrow, count);
  if ( Res == NULLMAT) return NULLMAT;
  count=0;
  for ( i = 0 ; i < A->mn ; i++ )
    {
      if ( A->B[i] ) Res->R[count++] = i+1;
    }
  return Res;
}


/**
 * nsp_bmatrix_find_2:
 * @A: a #NspBMatrix. 
 * @lhs: a flag 
 * @Res1:  a #NspMatrix. 
 * @Res2: a #NspMatrix. 
 * 
 * returns in one or two #NspMatrix objects the indices for which the 
 * #NspBMatrix @A is true. if @lhs is equal to one, the @Res1 will 
 * contains the indices in @A considering that @A is a one dimensional array.
 * If @lhs is equal to two then @Res1 will contian the rows indices and @Res2 
 * will contain the columns indices.
 *
 * Return value: %OK or %FAIL. %FAIL is returned in case of malloc() failure.
 */

int nsp_bmatrix_find_2(const NspBMatrix *A, int lhs, NspObject **Res1, NspObject **Res2, char ind_type)
{
  int j,i,count=0, k;
  int nrow = ( A->mn == 0) ? 0: 1;
  int *indr=NULL, *indc=NULL;

  /* first pass for counting */
  for ( i=0 ; i < A->mn ; i++) if ( A->B[i] ) count++;
  /* special rule for scalars */
  if ( A->mn == 1 && count ==0) nrow =0;

  if ( lhs == 1)
    {
      if ( (*Res1 = nsp_alloc_mat_or_imat(nrow, count, ind_type, &indr)) == NULLOBJ )
	return FAIL;
      count=0;
      for ( i = 0 ; i < A->mn ; i++ )
	if ( A->B[i] ) indr[count++] = i+1;
      return OK;
    }
  else
    {
      if ( (*Res1 = nsp_alloc_mat_or_imat(nrow, count, ind_type, &indr)) == NULLOBJ )
	return FAIL;
      if ( (*Res2 = nsp_alloc_mat_or_imat(nrow, count, ind_type, &indc)) == NULLOBJ )
	{
	  nsp_object_destroy(Res1);
	  return FAIL;
	}
      count=0;
      for ( j = 0, k = 0 ; j < A->n ; j++ )
	for ( i = 0 ; i < A->m ; i++, k++ )
	  if ( A->B[k] )
	    {
	      indr[count] = i+1;
	      indc[count++] = j+1;
	    }
    }
  return OK;
}


/*
 * Comparison operators
 */

/* Operations **/

static int Eq(int a, int b) {  return(a==b);}
static int NEq(int a, int b) {  return(a!=b);}

typedef int (CompOp) (int,int);

typedef struct cpt {
  char *name;
  CompOp *fonc,*foncop;
} CompTab;

/* Warning : sorted tab **/ 

static CompTab comptab[] = {
  {"<>",NEq ,Eq},
  {"==",Eq  ,NEq},
  {(char *) NULL,0,0}
};

static int SearchBComp(char *op, CompOp (**realop))
{
  int i=0;
  while ( comptab[i].name != (char *) NULL)
    {
      int j;
      j = strcmp(op,comptab[i].name);
      if ( j == 0 )
	{
	  *realop = comptab[i].foncop;
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
 * nsp_bmatrix_compare:
 * @A: a #NspBMatrix 
 * @B: a #NspBMatrix 
 * @op: can be "<>" or "==" 
 *
 * term to term comparison between the two matrices @A and @B. 
 * Performs A(i,j) == B(i,j) or A(i,j) <> B(i,j) according to @op value.
 * Note that, if @A or @B is a 1x1 matrix its size is promoted to the size of the 
 * other argument when applying @op operator.
 *
 * returns: a #NspBMatrix. 
 */

NspBMatrix  *nsp_bmatrix_compare(const NspBMatrix *A,const  NspBMatrix *B, char *op)
{
  CompOp *realop;
  int i;
  NspBMatrix *Loc ;
  if ( SearchBComp(op,&realop) == FAIL) return(NULLBMAT);
  if ( A->mn != B->mn)
    {
      if ( B->mn == 1 && A->mn != 0  ) 
	{
	  /* Special case B is a constant, Loc created with true */
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) { return(NULLBMAT);   }
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*realop)(A->B[i],B->B[0]) ) Loc->B[i] = FALSE;
	  return(Loc);
	}
      if ( A->mn == 1 && B->mn != 0 ) 
	{
	  /* Special case A is a constant */
	  Loc =nsp_bmatrix_create(NVOID,B->m,B->n);
	  if ( Loc == NULLBMAT)     { return(NULLBMAT);  }
	  for ( i = 0 ; i < B->mn ; i++ )  
	    if ( (*realop)(A->B[0],B->B[i]) ) Loc->B[i] = FALSE;
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
	  if ( (*realop)(1,1)) Loc->B[0] = FALSE;
	}
      else
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*realop)(A->B[i],B->B[i])) Loc->B[i] = FALSE;
	}
    }
  return(Loc);
}

/**
 * nsp_bmatrix_full_compare
 * @A: a #NspBMatrix 
 * @B: a #NspBMatrix 
 * @op: can be "<>" or "==" 
 * @err: pointer to an integer
 * 
 * %TRUE is returned if @A(i,j) op @B(i,j) is %TRUE for all indices. 
 * Note that, if @A or @B is a 1x1 matrix its size is promoted to the size of the 
 * other argument when applying @op operator. In all the other cases %FALSE is returned. 
 * 
 * Return value: %TRUE or %FALSE.
 */ 

int nsp_bmatrix_full_compare(const NspBMatrix *A,const NspBMatrix *B, char *op,int *err)
{
  CompOp *realop;
  int i,rep=TRUE;
  *err=0;
  if ( SearchBComp(op,&realop) == FAIL) return FALSE;
  /* Loc = true */ 
  if ( A->mn != B->mn)
    {
      if ( B->mn == 1 ) 
	{
	  for ( i = 0 ; i < A->mn ; i++ )  
	    {
	      if ( (*realop)(A->B[i],B->B[0]) ) 
		{ 
		  rep =  FALSE;
		  break;
		}
	    }
	  return rep ;
	}
      if ( A->mn == 1) 
	{
	  for ( i = 0 ; i < B->mn ; i++ )  
	    {
	      if ( (*realop)(A->B[i],B->B[0]) ) 
		{ 
		  rep  = FALSE;
		  break;
		}
	    }
	  return rep  ;
	}
      /*       Scierror("Error:\tIncompatible dimensions\n"); */
      *err=1;
      return FALSE; 
    }
  else 
    {
      for ( i = 0 ; i < A->mn ; i++ )  
	{
	  if ( (*realop)(A->B[i],B->B[i]) ) 
	    {
	      rep =  FALSE;
	      break ;
	    }
	}
    }
  return rep;
}


/*
 * routines for output of boolean matrices 
 */

static void BMij_plus_format(const void *m, int i, int j)
{
  const NspBMatrix *M=m;
  if (M->B[i+(M->m)*j] == FALSE ) 
    Sciprintf(" ");
  else
    Sciprintf("+");
}

static void BMij_as_read(const nsp_num_formats *fmt,const void *m, int i, int j)
{
  const NspBMatrix *M=m;
  Sciprintf(" ");
  Sciprintf("%%%c",M->B[i+(M->m)*j]==FALSE ? 'f' :'t');
}

static void BMij(const nsp_num_formats *fmt,const void *m, int i, int j)
{
  const NspBMatrix *M=m;
  Sciprintf(" ");
  Sciprintf("%c",M->B[i+(M->m)*j]==FALSE ? 'F' :'T');
}



static int nsp_bmatrix_print_internal (nsp_num_formats *fmt,NspBMatrix *cm, int indent)
{
  int rep = TRUE;
  int nr = cm->m;
  int nc = cm->n;
  if (fmt->plus_format && ! user_pref.pr_as_read_syntax)
    {
      nsp_matrix_plus_format(cm,nr,nc,BMij_plus_format,indent);
    }
  else
    {
      int column_width,total_width,inc  ;
      int max_width ,winrows ;
      column_width = 2;
      total_width = nc * column_width;
      sci_get_screen_size(&winrows,&max_width);
      if (user_pref.pr_as_read_syntax)	max_width -= 4;
      Sciprintf("\n");
      inc = nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  inc = max_width / column_width;
	  if (inc == 0)
	    inc++;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  nsp_gen_matrix_as_read_syntax(fmt,cm,nr,nc,inc,indent,BMij_as_read);
	}
      else
	{
	  rep = nsp_matrix_general(fmt,cm,nr,nc,inc,total_width,max_width,winrows,
				   indent,BMij);
	}
    }
  return rep;
}

