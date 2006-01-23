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
#include "nsp/interf.h"    /* for ret_endfor **/
#include "nsp/cnumeric.h" 
#include "nsp/mpmatrix.h" /* FIXME: to be added in object */
/* extern functions from other scilab sections **/

#include "nsp/blas.h"
#include "nsp/matutil.h"


/* XXX */
extern void nsp_real_matrix_print_internal(nsp_num_formats *fmt,NspMatrix *m, int indent);
extern void nsp_complex_matrix_print_internal (nsp_num_formats *fmt,NspMatrix *cm, int indent);


/*
 * Max Plus matrices 
 * It is always possible to cast a NspMaxpMatrix * to a NspMatrix * 
 * when passing it to a function which accept a NspMatrix * as argument.
 * It is also possible to obtain a NspMaxpMatrix Object from a NspMatrix 
 * and conversely with  nsp_matrix_cast_to_mpmatrix and 
 * nsp_mpmatrix_cast_to_matrix.
 * 
 */

/**
 * nsp_mpmatrix_create:
 * @name: matrix name 
 * @type: real ('r') or complex('c') 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a new max plus matrix with unspecified values, 
 * 
 * Return value: a #NspMaxpMatrix or %NULLMAT.
 **/
NspMaxpMatrix * nsp_mpmatrix_create(const char *name, char type, int m, int n)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_create(name,type,m,n));
}

/**
 * nsp_mp_matrix_from_m: 
 * @name: matrix name 
 * @M: a NspMatrix *
 * 
 * DEPRECATED:
 *
 * Creates a new max plus matrix using data from a given 
 * argument matrix @M. Data is not copied but moved from 
 * given argument to the new matrix. the @M matrix size 
 * is then set to 0x0 and data is set to null to prevent 
 * sharing data. This function is usefull when using a 
 * function returning a NspMatrix where a NspMaxpMatrix is 
 * requested. 
 * 
 * Returns a #NspMaxpMatrix or %NULLMAT.
 */

NspMaxpMatrix * nsp_mp_matrix_from_m(const char *name,NspMatrix *M)
{
  NspMaxpMatrix *Mat = new_mpmatrix();
  
  if ( Mat == NULLMAXPMAT) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAXPMAT);
    }
  /* shared by all objects */
  if ((NSP_OBJECT(Mat)->name =new_nsp_string(name))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAXPMAT);
    }
  NSP_OBJECT(Mat)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  Mat->m=M->m;
  Mat->n=M->n;
  Mat->rc_type = M->rc_type;
  Mat->mn=M->mn;
  Mat->convert = 'd'; 
  if ( Mat->mn == 0 ) 
    {
      Mat->m = Mat->n=0;
      Mat->R = (double *) 0; 
      Mat->C = (doubleC *) 0;
      return(Mat);
    }
  switch (  Mat->rc_type ) 
    {
    case 'r' :
      Mat->C = (doubleC *) 0;
      Mat->R = M->R; 
      break;
    case 'c' : 
      Mat->R = (double *) 0; 
      Mat->C = M->C;
    }
  M->R =NULL; M->C = NULL;
  M->m = M->n = M->mn = 0;
  return(Mat);
}


/**
 * nsp_matrix_cast_to_mpmatrix:
 * @M: a NspMatrix *
 * 
 * Changes the type fields of @M in such a way that 
 * @M becomes a #NspMaxpMatrix. 
 * 
 * Returns a #NspMaxpMatrix or %NULLMAXPMAT.
 */

NspMaxpMatrix * nsp_matrix_cast_to_mpmatrix(NspMatrix *M)
{
  if ( M == NULLMAT) return NULLMAXPMAT;
  M->type = (NspTypeMatrix *) new_type_mpmatrix(T_BASE);
  NSP_OBJECT(M)->type =(NspTypeObject *) M->type->surtype;
  NSP_OBJECT(M)->basetype =(NspTypeBase *) M->type;
  return (NspMaxpMatrix *) M;
}



/**
 * nsp_mpmatrix_cast_to_matrix
 * @M: a NspMatrix *
 * 
 * Changes the type fields of @M in such a way that 
 * @M becomes a #NspMaxpMatrix. 
 * 
 * Returns a #NspMaxpMatrix 
 */

NspMatrix * nsp_mpmatrix_cast_to_matrix(NspMaxpMatrix *M)
{
  if ( M == NULLMAXPMAT ) return NULLMAT;
  M->type = (NspTypeMaxpMatrix *) new_type_matrix(T_BASE);
  NSP_OBJECT(M)->type =(NspTypeObject *) M->type->surtype;
  NSP_OBJECT(M)->basetype =(NspTypeBase *) M->type;
  return (NspMatrix *) M;
}



/**
 * nsp_mpmatrix_create_impl:
 * @first: first value as a double 
 * @step: step 
 * @last:  last value as a double
 *
 * creates a row matrix with values from @first to @last with step @step. 
 * Returns a #NspMaxpMatrix or %NULLMAXPMAT.
 */


NspMaxpMatrix *nsp_mpmatrix_create_impl(double first, double step, double last)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_create_impl(first,step,last));
}

/**
 * nsp_mpmatrix_create_from_doubles:
 * @name: matrix name 
 * @m: number of rows 
 * @n: number of columns 
 * @...: values given as double 
 * 
 * creates a new scalar matrix of size @m x @n filled with given @m x @n values. 
 * returns the matrix or %NULLMAXPMAT
 */

NspMaxpMatrix *nsp_mpmatrix_create_from_doubles(const char *name,int m,int n,...)
{
  int i;
  NspMaxpMatrix *Loc;
  if (( Loc = nsp_mpmatrix_create(name,'r',m,n)) == NULLMAXPMAT) return NULLMAXPMAT;
  va_list ap;
  va_start(ap,n);
  for ( i= 0 ; i < m*n ; i++) 
    Loc->R[i] = (double) va_arg(ap,double);
  va_end(ap);
  return(Loc);
}

/**
 * nsp_mpmatrix_copy:
 * @A: a #NspMaxpMatrix 
 *
 * copies #NspMaxpMatrix @A and returns the copy or %NULLMAXPMAT.
 * Returns a #NspMaxpMatrix or %NULLMAXPMAT.
 */

NspMaxpMatrix *nsp_mpmatrix_copy(const NspMaxpMatrix *A)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_copy((const NspMatrix *) A));
}

/**
 * nsp_mpmatrix_fill_with:
 * @A: a #NspMaxpMatrix 
 * @B: a #NspMaxpMatrix 
 * 
 * The #NspMaxpMatrix @A size and type (real or complex) are changed 
 * to be the same as size and type of @B. Then @A is filled 
 * with @B data. 
 *
 * Returns %OK or %FAIL. When %OK is returned @A is changed. 
 */

int nsp_mpmatrix_fill_with(NspMaxpMatrix *A,const NspMaxpMatrix *B)
{
  int rep =  nsp_matrix_fill_with((NspMatrix *)A,(const NspMatrix *)B);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/**
 * nsp_mpmatrix_resize:
 * @A: a #NspMaxpMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspMaxpMatrix @A dimensions are changed to be @m x @n. 
 * This routine only enlarges or shrink (using realloc()) 
 * the data array of @A to size mxn. The previous data are not moved and 
 * occupy the first array cells. Note that @A can be 
 * and empty matrix when calling this routine ( malloc() is used in that 
 * case ). 
 *
 * returns %OK or %FAIL. When %OK is returned @A is changed. 
 */

int nsp_mpmatrix_resize(NspMaxpMatrix *A, int m, int n)
{
  return nsp_matrix_resize((NspMatrix *) A,m,n);
}


/**
 * nsp_mpmatrix_scalar_to_mn:
 * @A: a #NspMaxpMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspMaxpMatrix @A of dimension 1x1 is changed to a matrix of size @m x @n 
 * filled with the @A scalar value i.e A= A(1,1)*ones(m;n). 
 * Note that the size of @A is not checked on entry it sould be 1x1.
 *
 * returns %OK or %FAIL. When %OK is returned @A is changed. 
 */

int nsp_mpmatrix_scalar_to_mn(NspMaxpMatrix *A, int m, int n)
{
  int rep =  nsp_matrix_scalar_to_mn((NspMatrix *)A,m,n);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/**
 * nsp_mpmatrix_destroy:
 * @Mat: a #NspMaxpMatrix 
 * 
 * free the #NspMaxpMatrix @Mat. 
 */

void nsp_mpmatrix_destroy(NspMaxpMatrix *Mat)
{
  if ( Mat == NULLMAXPMAT ) return ; 
  FREE(Mat->C) ;
  FREE(Mat->R);
  FREE(NSP_OBJECT(Mat)->name);
  FREE(Mat) ;
}

/**
 * nsp_mpmatrix_info:
 * @Mat: a #NspMaxpMatrix
 * @indent: an int 
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * Display info on the #NspMaxpMatrix @A 
 *
 */

void nsp_mpmatrix_info(const NspMaxpMatrix *Mat, int indent,char *name,int rec_level)
{
  int i;
  if ( Mat == NULLMAXPMAT) 
    {
      Sciprintf("Null Pointer Matrix \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) == 0) 
    Sciprintf("MaxpMatrix (%d,%d) type %c\n",Mat->m,Mat->n,Mat->rc_type);
  else
    Sciprintf("MaxpMatrix %s(%d,%d) type %c\n",NSP_OBJECT(Mat)->name,Mat->m,Mat->n,Mat->rc_type);
}

/**
 * nsp_mpmatrix_print:
 * @Mat: a #NspMaxpMatrix
 * @indent: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 *
 * Print the #NspMaxpMatrix @A using the default nsp output function. 
 * @indent is the given indentation for printing.
 */


void nsp_mpmatrix_print( NspMaxpMatrix *Mat, int indent,char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  int i;
  Mat = (NspMaxpMatrix *) Mat2double((NspMatrix *)Mat);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf("%s=%s",pname,(Mat->mn==0 ) ? " m2mp([])\n" : "" );
	}
      else 
	{
	  Sciprintf("%s",(Mat->mn==0 ) ? " m2mp([])\n" : "" );
	}
    }
  else 
    {
      Sciprintf("%s\t=%s\t\t mp %c(%dx%d) \n",pname,
		(Mat->mn==0 ) ? " []" : "",Mat->rc_type,Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      if ( Mat->rc_type == 'r') 
	nsp_real_matrix_print_internal(&fmt,(NspMatrix *)Mat,indent);
      else 
	nsp_complex_matrix_print_internal(&fmt,(NspMatrix *)Mat,indent);
    }
}

/**
 * nsp_mpmatrix_latex_print:
 * @Mat: a #NspMaxpMatrix
 * 
 * print the #NspMaxpMatrix @A using the default Sciprintf() function and LaTeX 
 * syntax. 
 */

void nsp_mpmatrix_latex_print(const NspMaxpMatrix *Mat)
{
  int i,j;
  if ( Mat->rc_type == 'r' ) 
    {
      Sciprintf("{%s = \\left(\\begin{array}{",NSP_OBJECT(Mat)->name );
      for (i=0; i <  Mat->n;i++) Sciprintf("c");
      Sciprintf("}\n");
      for (j=0;j < Mat->m;j++)
	{
	  for (i=0;i < Mat->m - 1;i++)
	    { 
	      Sciprintf("%g\t& ",Mat->R[i+j*Mat->m]);
	    }
	  Sciprintf("%g\t\\\\\n",Mat->R[Mat->m-1+j*Mat->m]);
	}
      Sciprintf("\\end{array}\\right)}\n");
    }
}

/**
 * nsp_mpmatrix_latex_tab_print:
 * @Mat: a #NspMaxpMatrix
 * 
 * print the #NspMaxpMatrix @A using the default Sciprintf() function and LaTeX tabular
 * syntax. 
 */


void nsp_mpmatrix_latex_tab_print(const NspMaxpMatrix *Mat)
{
  int i,j;
  if ( Mat->rc_type == 'r' ) 
    {
      Sciprintf("\\begin{tabular}{|l|");
      for (i=0; i < Mat->n ;i++) Sciprintf("c|");
      Sciprintf("}\\hline\n %s &\t",NSP_OBJECT(Mat)->name);
      for (i=0; i < Mat->n ;i++) Sciprintf("$C_{%d}$\t&",i);
      Sciprintf("$C_{%d}$\\\\ \\hline\n",Mat->n);
      for (j=0;j < Mat->m ;j++)
	{
	  Sciprintf("$L_{%d}$\t&",j);
	  for (i=0;i < Mat->n-1 ;i++)
	    {
	      Sciprintf("%g\t& ",Mat->R[i+j*Mat->m]);
	    }
	  Sciprintf("%g\t\\\\ \\hline\n",Mat->R[Mat->m-1+j*Mat->m]);
	}
      Sciprintf("\\end{tabular}\n");
    }
}


/**
 * nsp_mpmatrix_redim:
 * @A: a #NspMaxpMatrix
 * @m: number of rows 
 * @n: number of columns
 * 
 * Checks that the #NspMaxpMatrix @A of size m' x n' satisfy m'*n' = @m * @n and reshapes 
 * @A to size m x @n.
 *
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_redim(NspMaxpMatrix *A, int m, int n)
{
  return nsp_matrix_redim((NspMatrix *)A,m,n);
}


/**
 * nsp_mpmatrix_enlarge:
 * @A: a #NspMaxpMatrix
 * @m: number of rows 
 * @n: number of columns
 * 
 * Changes the #NspMaxpMatrix @A size to max(A->m,m) x max(A->n,n) adding 
 * rows and columns of zero to @A. 
 * A = [ A ,ones(A->m,Max(n-A->n,0); 
 *      ones(Max(m-A->m,0), Max(A->n,n)) ]
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_enlarge(NspMaxpMatrix *A, int m, int n)
{
  int rep = nsp_matrix_enlarge((NspMatrix *)A,m,n);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/**
 * nsp_mpmatrix_concat_right:
 * @A: a #NspMaxpMatrix
 * @B: a #NspMaxpMatrix
 * 
 * Changes the #NspMaxpMatrix @A to [@A , @B ] ; 
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_concat_right(NspMaxpMatrix *A,const NspMaxpMatrix *B)
{
  int rep=nsp_matrix_concat_right((NspMatrix *)A,(const NspMatrix *)B);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/**
 * nsp_mpmatrix_add_columns:
 * @A: a #NspMaxpMatrix
 * @n: number of columns 
 * 
 * Adds @n columns to the #NspMaxpMatrix. Note that 
 * if @A is an empy matrix on entry it remains empty.
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_add_columns(NspMaxpMatrix *A, int n)
{
  int rep = nsp_matrix_add_columns((NspMatrix *)A,n);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/**
 * nsp_mpmatrix_concat_down:
 * @A: a #NspMaxpMatrix
 * @B: a #NspMaxpMatrix
 * 
 * returns a new #NspMaxpMatrix equals to  [@A ; @B ] ; 
 * @A and @B must have compatible dimensions. 
 * 
 * returns a #NspMaxpMatrix or %NULLMAXPMAT.
 */

NspMaxpMatrix* nsp_mpmatrix_concat_down(const NspMaxpMatrix *A,const NspMaxpMatrix *B)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_concat_down((const NspMatrix *)A,(const NspMatrix *)B));
}

/**
 * nsp_mpmatrix_concat_diag:
 * @A: a #NspMaxpMatrix
 * @B: a #NspMaxpMatrix
 * 
 * returns a new #NspMaxpMatrix equals to  [@A,0 ;0, @B ] ; 
 * 
 * returns a #NspMaxpMatrix or %NULLMAXPMAT.
 */

NspMaxpMatrix*nsp_mpmatrix_concat_diag(const NspMaxpMatrix *A,const NspMaxpMatrix *B)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_concat_diag((const NspMatrix *)A,(const NspMatrix *)B));
}


/**
 * nsp_mpmatrix_add_rows:
 * @A: a #NspMaxpMatrix
 * @m: number of rows
 * 
 * Adds @n rows to the #NspMaxpMatrix. Note that 
 * if @A is an empy matrix on entry it remains empty.
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_add_rows(NspMaxpMatrix *A, int m)
{
  int rep = nsp_matrix_add_rows((NspMatrix *)A,m);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/**
 * nsp_mpmatrix_set_submatrix:
 * @A: a #NspMaxpMatrix
 * @Rows: a #NspMaxpMatrix
 * @Cols: a #NspMaxpMatrix
 * @B: a #NspMaxpMatrix
 * 
 * Performe  A(Rows,Cols) = B. A is changed and enlarged if necessary and 
 * size compatibility is checked i.e B must be scalar or  
 * we must have size(B)==[size(Rows),size(Cols)]. 
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_set_submatrix(NspMaxpMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspMaxpMatrix *B)
{
  int rep = nsp_matrix_set_submatrix((NspMatrix *)A,Rows,Cols,(NspMatrix *)B);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/** 
 * nsp_mpmatrix_set_rows:
 * @A: a #NspMaxpMatrix
 * @Rows: a #NspMaxpMatrix
 * @B: a #NspMaxpMatrix
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
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_set_rows(NspMaxpMatrix *A, NspMatrix *Rows, NspMaxpMatrix *B)
{
  int rep = nsp_matrix_set_rows((NspMatrix *)A,Rows,(NspMatrix *)B);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/**
 * nsp_mpmatrix_is_increasing;
 * @A: a #NspMaxpMatrix
 * 
 * Checks if the #NspMaxpMatrix @A contains values in increasing order. 
 *
 * returns %OK or %FAIL.
 */

int mpmat_is_increasing(const NspMaxpMatrix *A)
{
  return mat_is_increasing((const NspMatrix *) A);
}


/**
 * nsp_mpmatrix_delete_columns:
 * @A: a #NspMaxpMatrix
 * @Cols: a #NspMaxpMatrix
 *
 * Performs A(:,Cols) = []. 
 *
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_delete_columns(NspMaxpMatrix *A, NspMatrix *Cols)
{
  int rep = nsp_smatrix_delete_columns((NspSMatrix *)A,Cols);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *) A);
  return rep;
}

/**
 * nsp_mpmatrix_delete_rows:
 * @A: a #NspMaxpMatrix
 * @Rows: a #NspMatrix
 *
 * Performs A(Rows,:)  = []. 
 *
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_delete_rows(NspMaxpMatrix *A, NspMatrix *Rows)
{
  return nsp_smatrix_delete_rows((NspSMatrix *) A,Rows);
}

/**
 * nsp_mpmatrix_delete_elements:
 * @A: a #NspMaxpMatrix
 * @Elts: a #NspMatrix
 *
 * Performs A(Elts) = []. 
 *
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_delete_elements(NspMaxpMatrix *A, NspMatrix *Elts)
{
  return nsp_smatrix_delete_elements((NspSMatrix *)A,Elts);
}

/**
 * nsp_mpmatrix_extract:
 * @A: a #NspMaxpMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(Rows,Cols) and returns the new #MspMatrix 
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_extract(const NspMaxpMatrix *A,const  NspMatrix *Rows, const NspMatrix *Cols)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_extract((const NspMatrix *)A,Rows,Cols));
}

/**
 * nsp_mpmatrix_extract_elements:
 * @A: a #NspMaxpMatrix
 * @Elts: a #NspMatrix
 *
 * Compute A(Elts) and returns the new #MspMatrix 
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_extract_elements(const NspMaxpMatrix *A,const NspMatrix *Elts)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_extract_elements((const NspMatrix *)A,Elts));
}


/**
 * nsp_mpmatrix_extract_columns:
 * @A: a #NspMaxpMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(:,Cols) and returns the new #MspMatrix 
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_extract_columns(const NspMaxpMatrix *A,const NspMatrix *Cols)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_extract_columns((const NspMatrix *)A,Cols));
}


/**
 * nsp_mpmatrix_extract_rows:
 * @A: a #NspMaxpMatrix
 * @Rows: a #NspMatrix
 *
 * Compute A(Rows,:) and returns the new #MspMatrix 
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_extract_rows(const NspMaxpMatrix *A,const NspMatrix *Rows)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_extract_rows((const NspMatrix *)A,Rows));
}



/**
 * MatLoopCol: 
 * @str: 
 * @Col: 
 * @A: 
 * @icol: 
 * @rep: 
 * @Returns: 

 * A1=MatLoopCol(A1,M,i,rep)
 * Used in for loops with matrices 
 * If Col is null it is created and filled with icol column of A 
 * If Col is non-null it is filled with icol column of A 
 * if icol is > A->n rep is set to RET_ENDFOR
 */

NspMaxpMatrix *MpMatLoopCol(char *str, NspMaxpMatrix *Col, NspMaxpMatrix *A, int icol, int *rep)
{
  int i;
  register int iof;
  NspMaxpMatrix *Loc;
  if ( icol > A->n )
    {
      *rep = RET_ENDFOR;
      return(NULLMAXPMAT);
    }
  *rep =0;
  if ( Col == NULLMAXPMAT) 
    Loc = nsp_mpmatrix_create(str,A->rc_type,A->m,1);
  else 
    Loc = Col;
  if ( Loc == NULLMAXPMAT) return(NULLMAXPMAT);
  iof = (icol-1)*A->m;
  if ( A->rc_type == 'c' )
    {
      for ( i = 0 ; i < A->m ; i++)
	{
	  Loc->C[i].r=A->C[i+iof].r ;
	  Loc->C[i].i=A->C[i+iof].i ;
	}
    }
  else 
    {
      for ( i = 0 ; i < A->m ; i++)
	Loc->R[i]=A->R[i+iof] ;
    }
  return(Loc);
}

/**
 * nsp_mpmatrix_extract_diag:
 * @A: a #NspMaxpMatrix
 * @k: an int 
 *
 * Extract the @k-th diagonal of matrix @A and returns 
 * its value as a column vector. 
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_extract_diag(const NspMaxpMatrix *A, int k)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_extract_diag((const NspMatrix *)A, k));
}

/**
 * nsp_mpmatrix_set_diag:
 * @A: a #NspMaxpMatrix
 * @Diag: a #NspMaxpMatrix
 * @k: an int 
 *
 * sets the @k-th diagonal of matrix @A with values from @Diag. 
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_set_diag(NspMaxpMatrix *A, NspMaxpMatrix *Diag, int k)
{
  int rep= nsp_matrix_set_diag((NspMatrix *)A,(NspMatrix *)Diag, k);
  nsp_matrix_cast_to_mpmatrix((NspMatrix *)A);
  return rep;
}

/**
 * nsp_mpmatrix_create_diag:
 * @Diag: a #NspMaxpMatrix
 * @k: an int 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_create_diag(const NspMaxpMatrix *Diag, int k)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_create_diag((const NspMatrix *)Diag, k));
}

/**
 * nsp_mpmatrix_transpose: 
 * @A: a #NspMaxpMatrix
 *
 * return the transpose of A
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_transpose(const NspMaxpMatrix *A)
{
  return nsp_matrix_cast_to_mpmatrix(nsp_matrix_transpose((const NspMatrix *)A));
}




