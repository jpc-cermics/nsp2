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


/*
 * Max Plus matrices 
 * It is always possible to cast a NspMaxpMatrix * to a NspMatrix * 
 * when passing it to a function which accept a NspMatrix * as argument.
 * It is also possible to create a NspMatrix Object from a NspMatrix 
 * without copy with nsp_mp_matrix_from_m. Note that in this last case 
 * the given NspMatrix is changed to NULL.
 */

/**
 * nsp_mpmatrix_create:
 * @name: matrix name 
 * @type: real ('r') or complex('c') 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a new max plus matrix with unspecified values, returns %NULLMAT on failure. 
 * Returns a #NspMaxpMatrix or %NULLMAT.
 */

NspMaxpMatrix * nsp_mpmatrix_create(const char *name, char type, integer m, integer n)
{
  NspMaxpMatrix *Mat = new_mpmatrix();
  
  if ( Mat == NULLMAXPMAT) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAXPMAT);
    }
  /* shared by all objects */
  if ((NSP_OBJECT(Mat)->name = NewString(name))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAXPMAT);
    }
  NSP_OBJECT(Mat)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /* specific for Matrix */
  Mat->m=m;
  Mat->n=n;
  Mat->rc_type = type;
  Mat->mn=m*n;
  Mat->convert = 'd'; 
  if ( Mat->mn == 0 ) 
    {
      Mat->m = Mat->n=0;
      Mat->R = (double *) 0; 
      Mat->I = (doubleC *) 0;
      return(Mat);
    }
  switch ( type ) 
    {
    case 'r' :
      Mat->I = (doubleC *) 0;
      Mat->R =nsp_alloc_doubles(Mat->mn);
      if ( Mat->R == (double *) 0 ) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return(NULLMAXPMAT);
	}
      break;
    case 'i' : 
      Mat->R = (double *) 0; 
      Mat->I =nsp_alloc_doubleC(Mat->mn);
      if (  Mat->I == (doubleC *) 0) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return(NULLMAXPMAT);
	}
    }
#ifdef OCAML 
  Mat->proxy = NULL; 
#endif
  return(Mat);
}

/**
 * nsp_mp_matrix_from_m: 
 * @name: matrix name 
 * @M: a NspMatrix *
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
  if ((NSP_OBJECT(Mat)->name = NewString(name))== NULLSTRING) 
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
      Mat->I = (doubleC *) 0;
      return(Mat);
    }
  switch (  Mat->rc_type ) 
    {
    case 'r' :
      Mat->I = (doubleC *) 0;
      Mat->R = M->R; 
      break;
    case 'i' : 
      Mat->R = (double *) 0; 
      Mat->I = M->I;
    }
  M->R =NULL; M->I = NULL;
  M->m = M->n = M->mn = 0;
  return(Mat);
}



/*
 * used when data is transmited by caml 
 */

#ifdef OCAML 
NspMaxpMatrix *MatCreateFromData(char *name, char type, integer m, integer n,
				 struct caml_bigarray *b)
{
  struct caml_bigarray_proxy * proxy;
  NspMaxpMatrix *Mat = new_mpmatrix();
  if ( Mat == NULLMAXPMAT) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAXPMAT);
    }

  /* shared by all objects */
  if ((NSP_OBJECT(Mat)->name = NewString(name))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAXPMAT);
    }
  NSP_OBJECT(Mat)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
  Mat->otype =  MATRIX; 
  Mat->ftype = &Matrix_Type;
  */
  /* specific for Matrix */
  Mat->m=m;
  Mat->n=n;
  Mat->mn=m*n;
  Mat->type=type;
  Mat->convert = 'd'; 
  if ( Mat->mn == 0 ) 
    {
      Mat->m = Mat->n=0;
      Mat->R = (double *) 0; 
      Mat->I = (doubleC *) 0;
      return(Mat);
    }
  switch ( type ) 
    {
    case 'r' :
      Mat->I = (doubleC *) 0;
      Mat->R = b->data;
      break;
    case 'i' : 
      Mat->R = (double *) 0; 
      Mat->I = b->data; 
    }
  /* dealing with proxy */ 
  /* Nothing to do for un-managed arrays */
  Mat->flags = b->flags ;
  if ((b->flags & BIGARRAY_MANAGED_MASK) == BIGARRAY_EXTERNAL) 
    return Mat;
  if ( b->proxy != NULL) {
    /* If b is already a proxy for a larger array, increment refcount of
       proxy */
    Mat->proxy = b->proxy;
    ++ b->proxy->refcount;
  } else {
    /* Otherwise, create proxy and attach it to both b1 and b2 */
    proxy = stat_alloc(sizeof(struct caml_bigarray_proxy));
    proxy->refcount = 2;      /* original array + sub array */
    proxy->data = b->data;
    proxy->size =
      b->flags & BIGARRAY_MAPPED_FILE ? bigarray_byte_size(b) : 0;
    Mat->proxy = proxy;
    b->proxy = proxy;
  }
  return(Mat);
}
#endif 



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
  int i;
  NspMaxpMatrix *Loc;
  double vals = first;
  int count=0;
  if ( (first < last && step < 0 ) 
       || (first >  last && step > 0 ) 
       || step == 0.00)
    {
      Loc = nsp_mpmatrix_create(NVOID,'r',(integer) 0,(integer) 0);
      return(Loc);
    }
  /* counting **/
  if ( step > 0 ) 
    {
      while ( vals <= last ) { vals += step ; count++;}
      if ( vals -last <  Max(Abs(first),Abs(last))*EPSILON*10) count++;
    }
  else if ( step < 0) 
    {
      while ( vals >= last ) { vals += step ; count++;}
      if ( last - vals <  Max(Abs(first),Abs(last))*EPSILON*10) count++;
    }
  else { 
    Scierror("Error:\t step is 0 in an implicit vector specification\n");
    return NULLMAXPMAT;
  }
  Loc = nsp_mpmatrix_create(NVOID,'r',(integer) 1,(integer) count);
  if ( Loc == NULLMAXPMAT) return(NULLMAXPMAT);
  for ( i=0 ; i < count; i++) 
    {
      Loc->R[i] = first + ((double) i)*step;
    }
  return(Loc);
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

NspMaxpMatrix *nsp_mpmatrix_create_from_doubles(const char *name,integer m,integer n,...)
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
  NspMaxpMatrix *Mat;
  if ((Mat = nsp_mpmatrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAXPMAT) { return(NULLMAXPMAT);}
  switch ( Mat->rc_type ) 
    {
    case 'r' :
      /* C2F(dcopy)(&(Mat->mn),A->R,&inc,Mat->R,&inc); */
      memcpy(Mat->R,A->R, (Mat->mn)*sizeof(double));
      break;
    case 'i' :
      /* C2F(zcopy)(&(Mat->mn),A->I,&inc,Mat->I,&inc); */
      memcpy(Mat->I,A->I, (Mat->mn)*sizeof(doubleC));
      break;
    }
  return(Mat);
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
  switch ( B->rc_type ) 
    {
    case 'r' : 
      if (A->rc_type == 'i') 
	{ 
	  if (nsp_mat_get_real((NspMatrix *)A) == FAIL) return FAIL; 
	}
      break; 
    case 'i' : 
      if (A->rc_type == 'r') 
	{ 
	  if (nsp_mat_complexify((NspMatrix *)A,0.0) == FAIL) return FAIL; 
	}
      break;
    }
  if ( nsp_mpmatrix_resize((NspMaxpMatrix *)A, B->m, B->n) == FAIL) return FAIL;
  switch ( A->rc_type ) 
    {
    case 'r' :
      /* C2F(dcopy)(&(A->mn),B->R,&inc,A->R,&inc); */
      memcpy(A->R,B->R, (A->mn)*sizeof(double));
      break;
    case 'i' :
      /* C2F(zcopy)(&(A->mn),B->I,&inc,A->I,&inc); */
      memcpy(A->I,B->I, (A->mn)*sizeof(doubleC));
      break;
    }
  return OK;
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

int nsp_mpmatrix_resize(NspMaxpMatrix *A, integer m, integer n)
{

  if ( A->mn == m*n ) /* no need to resize data just m and n */
    {
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
  if ( m*n == 0 ) /* need to free space */
    {
        A->m =  A->n = A->mn= 0;
	if ( A->rc_type == 'r' ) {FREE(A->R);} else  {FREE(A->I);}
	return OK;
    }

  A->m =m ;  A->n =n;   A->mn=m*n ;
  /* need to realloc */
#ifdef OCAML 
  if ( A->proxy != NULL ) 
    failwith("shared matrix cannot be resized \n");
#endif
  switch ( A->rc_type ) 
    {
    case 'r' : 
      A->R=nsp_realloc_doubles(A->R,A->mn);
      if ( A->R == (double *) 0) return(FAIL);
      break ; 
    case 'i' : 
      A->I =nsp_realloc_doubleC(A->I,A->mn);
      if ( A->I == (doubleC *) 0) return(FAIL);
      break;
    }
  return(OK);
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

int nsp_mpmatrix_scalar_to_mn(NspMaxpMatrix *A, integer m, integer n)
{
  int i;
  doubleC x;
  if ( nsp_mpmatrix_resize(A,m,n) == FAIL) return FAIL; 
  switch ( A->rc_type ) 
    {
    case 'r' : nsp_mat_set_rval((NspMatrix *)A,A->R[0]);break; 
    case 'i' : 
      x= A->I[0]; 
      for ( i=1; i < A->mn ; i++) 
	{
	  A->I[i]= x;
	}
      break;
    }
  return OK;
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
#ifdef OCAML 
  if ( Mat->proxy == NULL ) 
    {
      FREE(Mat->I) ;
      FREE(Mat->R);
      FREE(NSP_OBJECT(Mat)->name);
      FREE(Mat) ;
    }
  else 
    {
      /* matrix is shared with a caml bigarray */
      switch (Mat->flags & BIGARRAY_MANAGED_MASK ) {
      case BIGARRAY_MANAGED : 
	if (-- Mat->proxy->refcount == 0) {
	  free(Mat->proxy->data);
	  stat_free(Mat->proxy);
	}
      case BIGARRAY_MAPPED_FILE:
	if (-- Mat->proxy->refcount == 0) {
	  bigarray_unmap_file(Mat->proxy->data, Mat->proxy->size);
	  stat_free(Mat->proxy);
	}
      }
      FREE(NSP_OBJECT(Mat)->name);
      FREE(Mat) ;
    }
#else 
  FREE(Mat->I) ;
  FREE(Mat->R);
  FREE(NSP_OBJECT(Mat)->name);
  FREE(Mat) ;
#endif 

}

/**
 * nsp_mpmatrix_info:
 * @Mat: a #NspMaxpMatrix
 * @indent: an integer 
 * 
 * Display info on the #NspMaxpMatrix @A using the default Sciprintf() function. 
 * @indent is the given indentation for printing.
 */

void nsp_mpmatrix_info(const NspMaxpMatrix *Mat, int indent)
{
  int i;
  if ( Mat == NULLMAXPMAT) 
    {
      Sciprintf("Null Pointer Matrix \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) == 0) 
    Sciprintf("Matrix (%d,%d) type %c\n",Mat->m,Mat->n,Mat->rc_type);
  else
    Sciprintf("Matrix %s(%d,%d) type %c\n",NSP_OBJECT(Mat)->name,Mat->m,Mat->n,Mat->rc_type);
}


/**
 * nsp_mpmatrix_print:
 * @Mat: a #NspMaxpMatrix
 * @indent: an integer 
 * @header: a flag, if %FALSE then data are preinted but not header information
 *
 * Print the #NspMaxpMatrix @A using the default nsp output function. 
 * @indent is the given indentation for printing.
 */


void nsp_mpmatrix_print( NspMaxpMatrix *Mat, int indent,int header )
{
  int i;
  Mat = (NspMaxpMatrix *) Mat2double((NspMatrix *)Mat);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
	{
	  Sciprintf("%s=%s",NSP_OBJECT(Mat)->name,(Mat->mn==0 ) ? " m2mp([])\n" : "" );
	}
    }
  else 
    {
      Sciprintf("%s\t=%s\t\t mp %c(%dx%d) \n",NSP_OBJECT(Mat)->name,
		(Mat->mn==0 ) ? " []" : "",Mat->rc_type,Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      if ( Mat->rc_type == 'r') 
	nsp_print_internalM ((NspMatrix *)Mat,indent);
      else 
	nsp_print_internalCM ((NspMatrix *)Mat,indent);
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

int nsp_mpmatrix_redim(NspMaxpMatrix *A, integer m, integer n)
{
  if ( A->mn ==  m*n ) 
    {
      A->m =m ;
      A->n =n;
      return(OK);
    }
  else 
    {
      Scierror("Error:\tCannot change size to (%dx%d) since matrix has %d elements\n",m,n,A->mn);
      return(FAIL);
    }
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

int nsp_mpmatrix_enlarge(NspMaxpMatrix *A, integer m, integer n)
{
  if ( A->mn == 0) 
    {
      /* special case A=[] **/
      if ( nsp_mpmatrix_resize(A,m,n) == FAIL) return(FAIL);
      nsp_mat_set_rval((NspMatrix *)A,0.00);
      return OK;
    }
  if ( n > A->n  )
    if ( nsp_mpmatrix_add_columns(A,n- A->n) == FAIL) return(FAIL);
  if ( m > A->m  )  
    if ( nsp_mpmatrix_add_rows(A, m - A->m) == FAIL) return(FAIL);
  return(OK);
}

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )


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
  integer inc = 1;
  integer Asize;
  Asize=A->mn;
  if ( A->m != B->m ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(FAIL);
    }
  if ( nsp_mpmatrix_resize(A,A->m,A->n+B->n) == FAIL) return(FAIL);
  if ( A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r' )
	{
	  /* C2F(dcopy) (&B->mn,B->R,&inc,A->R+Asize,&inc); */
	  memcpy (A->R+Asize,B->R,sizeof(double)*B->mn);
	}
      else 
	{
	  if (nsp_mat_complexify((NspMatrix *)A,0.00) == FAIL ) return(FAIL);
	  /* C2F(zcopy)(&B->mn,B->I,&inc,A->I+Asize,&inc);*/
	  memcpy (A->I+Asize,B->I,sizeof(doubleC)*B->mn);
	}
    }
  else 
    {
      if ( B->rc_type == 'r' )
	nsp_dzcopy(&B->mn,B->R,&inc,A->I+Asize,&inc);
      else
	/* C2F(zcopy)(&B->mn,B->I,&inc,A->I+Asize,&inc); */
	memcpy(A->I+Asize,B->I,sizeof(doubleC)*B->mn);
    }
  return(OK);
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

int nsp_mpmatrix_add_columns(NspMaxpMatrix *A, integer n)
{
  double d=0.0;
  integer inc = 1,ns;
  integer Asize;
  if (n == 0) return OK;
  else if ( n < 0) 
    {      
      Scierror("Error: Negative indice (%d) in MatAddCols\n",n);
      return FAIL;
    }

  Asize=A->mn;
  ns= (A->m)*n;
  if ( nsp_mpmatrix_resize(A,A->m,A->n+n) == FAIL) return(FAIL);
  switch ( A->rc_type )
    {
    case 'r' : nsp_dset(&ns,&d,A->R+Asize,&inc);break;
    case 'i' : nsp_csetd(&ns,&d,A->I+Asize,&inc);break;
    }
  return(OK);
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
  char type = 'r';
  NspMaxpMatrix *Loc;
  int inc = 1,j,cols;
  if ( A->n != B->n &&  A->mn != 0 && B->mn != 0 ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(NULLMAXPMAT);
    }
  cols=Max(A->n,B->n);
  if ( A->rc_type == 'i' || B->rc_type == 'i' ) type = 'i';
  if (( Loc = nsp_mpmatrix_create(NVOID,type,A->m+B->m,cols)) == NULLMAXPMAT)
    return(NULLMAXPMAT);
  if ( Loc->rc_type == 'r' ) 
    {
      for ( j = 0 ; j < A->n ; j++ ) 
	memcpy(Loc->R+j*(Loc->m),A->R+j*A->m,A->m*sizeof(double));
      for ( j = 0 ; j < B->n ; j++ ) 
	memcpy(Loc->R+j*(Loc->m)+A->m,B->R+j*B->m,B->m*sizeof(double));
    }
  else 
    {
      switch ( A->rc_type ) 
 	{
 	case 'r' :
 	  for ( j = 0 ; j < A->n ; j++ ) 
	    nsp_dzcopy(&A->m,A->R+j*A->m,&inc,Loc->I+j*(Loc->m),&inc);
 	  break;
 	case 'i' :
 	  for ( j = 0 ; j < A->n ; j++ ) 
	    /*	C2F(zcopy) (&A->m,A->I+j*A->m,&inc,Loc->I+j*(Loc->m),&inc); **/
	    memcpy(Loc->I+j*(Loc->m),A->I+j*A->m,A->m*sizeof(doubleC));
 	  break;
 	}
      switch ( B->rc_type ) 
 	{
 	case 'r' :
 	  for ( j = 0 ; j < B->n ; j++ ) 
	    nsp_dzcopy(&B->m,B->R+j*B->m,&inc,Loc->I+j*(Loc->m)+A->m,&inc);
 	  break;
 	case 'i' :
 	  for ( j = 0 ; j < B->n ; j++ ) 
	    /*   C2F(zcopy) (&B->m,B->I+j*B->m,&inc,Loc->I+j*(Loc->m)+A->m,&inc);
	    */
	    memcpy(Loc->I+j*(Loc->m)+A->m,B->I+j*B->m,B->m*sizeof(doubleC));
 	  break;
 	}
    }
  return(Loc) ;
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
  char type = 'r';
  NspMaxpMatrix *Loc;
  double d=0.0;
  integer inc = 1,j=0;
  if ( A->rc_type == 'i' || B->rc_type == 'i' ) type = 'i';
  if ((Loc = nsp_mpmatrix_create(NVOID,type,A->m+B->m,A->n+B->n))==NULLMAXPMAT )
    return NULLMAXPMAT;
   
  if ( Loc->rc_type == 'r' ) 
    {
      for ( j = 0 ; j < A->n ; j++ ) 
	{
	  /* C2F(dcopy) (&A->m,A->R+j*A->m,&inc,Loc->R+j*(Loc->m),&inc); */
	  memcpy(Loc->R+j*(Loc->m),A->R+j*A->m,A->m*sizeof(double));
	nsp_dset(&B->m,&d,Loc->R+j*(Loc->m)+A->m,&inc);
	}
    }
  else 
    {
      if ( A->rc_type == 'i') 
	{
	  /* C2F(zcopy) (&A->m,A->I+j*A->m,&inc,Loc->I+j*(Loc->m),&inc); */
	  memcpy(Loc->I+j*(Loc->m),A->I+j*A->m,A->m*sizeof(doubleC));
	nsp_csetd(&B->m,&d,Loc->I+j*(Loc->m)+A->m,&inc);
	}
      else 
	{
	nsp_dzcopy(&A->m,A->R+j*A->m,&inc,Loc->I+j*(Loc->m),&inc);
	nsp_csetd(&B->m,&d,Loc->I+j*(Loc->m)+A->m,&inc);
	}
    }
  if ( Loc->rc_type == 'r' ) 
    {
      for ( j = 0 ; j < B->n ; j++ ) 
	{
	  /* 
	  C2F(dcopy) (&B->m,B->R+j*A->m,&inc,Loc->R+(j+A->n)*(Loc->m)+A->m,&inc);
	  */
	  memcpy(Loc->R+(j+A->n)*(Loc->m)+A->m,B->R+j*A->m,B->m*sizeof(double));
	nsp_dset(&A->m,&d,Loc->R+(j+A->n)*(Loc->m),&inc);
	}
    }
  else 
    {
      if ( B->rc_type == 'i') 
	{
	  /* 
	  C2F(zcopy) (&B->m,B->I+j*A->m,&inc,Loc->I+(j+A->n)*(Loc->m)+A->m,&inc);
	  */
	  memcpy(Loc->I+(j+A->n)*(Loc->m)+A->m,B->I+j*A->m,B->m*sizeof(doubleC));
	nsp_csetd(&A->m,&d,Loc->I+(j+A->n)*(Loc->m),&inc);
	}
      else 
	{
	nsp_dzcopy(&B->m,B->R+j*A->m,&inc,Loc->I+(j+A->n)*(Loc->m)+A->m,&inc);
	nsp_csetd(&A->m,&d,Loc->I+(j+A->n)*(Loc->m),&inc);
	}
    }
  return(Loc) ;
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

int nsp_mpmatrix_add_rows(NspMaxpMatrix *A, integer m)
{
  double d=0.0;
  integer inc = -1,Am;
  int j;
  if ( m == 0) return OK;
  else if ( m < 0) 
    {      
      Scierror("Error: Negative indice (%d) in MatAddRows\n",m);
      return FAIL;
    }
  Am= A->m;
  if ( nsp_mpmatrix_resize(A,A->m+m,A->n)== FAIL) return(FAIL);
  if (A->rc_type == 'i' ) 
    {
      for ( j = A->n-1  ; j >= 0 ; j-- ) 
 	{
 	  C2F(zcopy) (&A->m,A->I+j*Am,&inc,A->I+j*(A->m),&inc);
 	nsp_csetd(&m,&d,A->I+j*(A->m)+Am,&inc);
 	}
    }
  else 
    {
      for ( j = A->n-1  ; j >= 0 ; j-- ) 
 	{
 	  C2F(dcopy) (&A->m,A->R+j*Am,&inc,A->R+j*(A->m),&inc);
 	nsp_dset(&m,&d,A->R+j*(A->m)+Am,&inc);
 	}
    }

  return(OK);
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
  integer rmin,rmax,cmin,cmax,i,j;
  /* Check compatibility : B is a scalar or B must have compatible 
      size with Rows and Cols **/
  if ( B->mn != 1)
    {
      if ( Rows->mn != B->m ||  Cols->mn != B->n )
	{
	  Scierror("Error:\tIncompatible dimensions\n");
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
    if ( nsp_mpmatrix_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  if ( B->rc_type == 'i' )
    {
      if (  A->rc_type == 'r' )
 	{ 
 	  if (nsp_mat_complexify((NspMatrix *)A,0.00) == FAIL ) return(FAIL);
 	} 
      if ( B->mn != 1) 
	for ( i = 0 ; i < Rows->mn ; i++)
	  for ( j = 0 ; j < Cols->mn ; j++ )
	    { 
	      A->I[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m].r =B->I[i+B->m*j].r;
	      A->I[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m].i =B->I[i+B->m*j].i;
	    }
      else
	for ( i = 0 ; i < Rows->mn ; i++)
	  for ( j = 0 ; j < Cols->mn ; j++ )
	    { 
	      A->I[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m].r =B->I[0].r;
	      A->I[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m].i =B->I[0].i;
	    }
    }
  else 
    {
      if (  A->rc_type == 'i' )
 	{ 
	  if ( B->mn != 1) 
	    for ( i = 0 ; i < Rows->mn ; i++)
	      for ( j = 0 ; j < Cols->mn ; j++ )
		{	
		  A->I[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m].r =B->R[i+B->m*j];
		  A->I[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m].i = 0.00;
		}
	  else
	    for ( i = 0 ; i < Rows->mn ; i++)
	      for ( j = 0 ; j < Cols->mn ; j++ )
		{	
		  A->I[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m].r =B->R[0];
		  A->I[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m].i = 0.00;
		}
	}
      else
 	{ 
	  if ( B->mn != 1) 
	    for ( i = 0 ; i < Rows->mn ; i++)
	      for ( j = 0 ; j < Cols->mn ; j++ )
		{	
		  A->R[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m] =B->R[i+B->m*j];
		}
	  else
	    for ( i = 0 ; i < Rows->mn ; i++)
	      for ( j = 0 ; j < Cols->mn ; j++ )
		{	
		  A->R[((int) Rows->R[i]) -1 +(((int) Cols->R[j])-1)*A->m] =B->R[0];
		}
	}
    }
  return(OK);
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
  int i;
  int Bscal=0;
  if (Rows->mn == 0) return OK;
  if (GenericMatSeRo(A,A->m,A->n,A->mn,Rows,
		     B,B->m,B->n,B->mn,(F_Enlarge)nsp_mpmatrix_enlarge,&Bscal)== FAIL) 
    return FAIL;

  if ( B->rc_type == 'i' )
    {
      if (  A->rc_type == 'r' )
 	{ 
 	  if (nsp_mat_complexify((NspMatrix *)A,0.00) == FAIL ) return(FAIL);
 	} 
      if ( Bscal == 0 ) 
	for ( i = 0 ; i < Rows->mn ; i++)
	  {	
	    A->I[((int) Rows->R[i]) -1].r =B->I[i].r;
	    A->I[((int) Rows->R[i]) -1].i =B->I[i].i;
	  }
      else
	for ( i = 0 ; i < Rows->mn ; i++)
	  {	
	    A->I[((int) Rows->R[i]) -1].r =B->I[0].r;
	    A->I[((int) Rows->R[i]) -1].i =B->I[0].i;
	  }
    }
  else 
    {
      if (  A->rc_type == 'i' )
 	{ 
	  if ( Bscal == 0) 
	    for ( i = 0 ; i < Rows->mn ; i++)
	      {	
		A->I[((int) Rows->R[i]) -1].r =B->R[i];
		A->I[((int) Rows->R[i]) -1].i = 0.00;
	      }
	  else
	    for ( i = 0 ; i < Rows->mn ; i++)
	      {	
		A->I[((int) Rows->R[i]) -1].r =B->R[0];
		A->I[((int) Rows->R[i]) -1].i = 0.00;
	      }
	}
      else
 	{ 
	  if ( Bscal == 0) 
	    for ( i = 0 ; i < Rows->mn ; i++)
	      {	
		A->R[((int) Rows->R[i]) -1] =B->R[i];
	      }
	  else
	    for ( i = 0 ; i < Rows->mn ; i++)
	      {	
		A->R[((int) Rows->R[i]) -1] =B->R[0];
	      }
	}
    }
  return(OK);
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
  int max,i;
  if (A->rc_type != 'r'  ) return FAIL;
  if (A->mn == 0) return OK;
  max = (int) A->R[0];
  for (i=1 ; i < A->mn ; i++) 
    {
      if ( max > (int) A->R[i]) 
	return FAIL;
      else 
	{
	  max = (int) A->R[i];
	}
    }
  return OK;
}


/**
 * nsp_mpmatrix_delete_columns:
 * @A: a #NspMaxpMatrix
 * @Cols: a #NspMaxpMatrix
 *
 * Performs A(:,Cols) = []. 
 * Important Note: @Cols must be real and increasing 
 * and this is not checked here
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_delete_columns(NspMaxpMatrix *A, NspMatrix *Cols)
{
  integer ioff=0,cmin,cmax,i,col,last,nn;
  if ( Cols->mn == 0) return(OK);
  /* Bounds(Cols,&cmin,&cmax); */
  cmin = (int) Cols->R[0]; cmax = (int) Cols->R[Cols->mn-1];
  if ( cmin < 1 || cmax > A->n ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      return(FAIL);
    }
  for ( i = 0 ; i < Cols->mn ; i++)
    {
      /* mv [Cols[i],Cols[i+1] back ioff columns **/
      col= ((int) Cols->R[i]);
      last = (i == Cols->mn -1 ) ? A->n -1 : ((int) Cols->R[i+1]) -1;
      nn= (last-col+1)*A->m;
      if ( nn != 0) 
	{
	  ioff++;
	  if ( A->rc_type == 'r') 
	    /* C2F(dcopy)(&nn,A->R+ (col)*A->m,&un,A->R+(col-ioff)*A->m,&un); */
	    memcpy(A->R+(col-ioff)*A->m,A->R+ (col)*A->m,nn*sizeof(double));
	  else 
	    /*C2F(zcopy)(&nn,A->I+ (col)*A->m,&un,A->I+(col-ioff)*A->m,&un);*/
	    memcpy(A->I+(col-ioff)*A->m,A->I+ (col)*A->m,nn*sizeof(doubleC));
	}
    }
  if ( nsp_mpmatrix_resize(A,A->m,A->n-ioff)== FAIL) return(FAIL);
  return(OK);
}


/**
 * nsp_mpmatrix_delete_rows:
 * @A: a #NspMaxpMatrix
 * @Rows: a #NspMatrix
 *
 * Performs A(Rows,:)  = []. 
 * Important Note: @Rows  must be real and increasing 
 * and this is not checked here
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_delete_rows(NspMaxpMatrix *A, NspMatrix *Rows)
{
  integer rmin,rmax,i,j,ind,last,nn,ioff=0;
  if ( Rows->mn == 0) return(OK);
  /* Bounds(Rows,&rmin,&rmax); */
  rmin= (int) Rows->R[0]; rmax= (int) Rows->R[Rows->mn-1];
  if ( rmin < 1 || rmax > A->m ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      return(FAIL);
    }
  for ( j = 0 ; j < A->n  ; j++)
    for ( i = 0 ; i < Rows->mn ; i++)
      {
	ind =  ((int) Rows->R[i])+ j*A->m;
	last = (i < Rows->mn -1) ? ((int) Rows->R[i+1]) +j*A->m : ((int) Rows->R[i])-1+(j+1)*A->m ;
	last = ( last < A->mn ) ? last : A->mn;
	nn= (last-ind);
	if ( nn != 0) 
	  {
	    ioff++;
	    if ( A->rc_type == 'r') 
	      /*C2F(dcopy)(&nn,A->R +ind,&un,A->R +ind -ioff,&un); */
	      memcpy(A->R +ind -ioff,A->R +ind,nn*sizeof(double));
	    else 
	      /* C2F(zcopy)(&nn,A->I +ind,&un,A->I +ind -ioff,&un); */
	      memcpy(A->I +ind -ioff,A->I +ind,nn*sizeof(doubleC));
	  }
      }
  if ( nsp_mpmatrix_resize(A,A->m -Rows->mn,A->n)== FAIL) return(FAIL);
  return(OK);
}



/**
 * nsp_mpmatrix_delete_elements:
 * @A: a #NspMaxpMatrix
 * @Elts: a #NspMatrix
 *
 * Performs A(Elts) = []. 
 * Important Note: @Elts must be real and increasing 
 * and this is not checked here
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_delete_elements(NspMaxpMatrix *A, NspMatrix *Elts)
{
  integer rmin,rmax,i,ind,last,nn,ioff=0;
  if ( Elts->mn == 0) return(OK);
  /* Bounds(Elts,&rmin,&rmax); **/
  rmin = (int) Elts->R[0];rmax = (int) Elts->R[Elts->mn-1];
  if ( rmin < 1 || rmax > A->mn ) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      return(FAIL);
    }
  for ( i = 0 ; i < Elts->mn ; i++)
    {
      ind =  ((int) Elts->R[i]);
      last = (i < Elts->mn -1) ? ((int) Elts->R[i+1])-1 : A->mn ;
      nn= (last-ind);
      if ( nn != 0 ) 
	{
	  ioff++;
	  if ( A->rc_type == 'r') 
	    /* C2F(dcopy)(&nn,A->R +ind,&un,A->R +ind -ioff,&un); */
	    memcpy(A->R +ind -ioff,A->R +ind,nn*sizeof(double));
	  else 
	    /* C2F(zcopy)(&nn,A->I +ind,&un,A->I +ind -ioff,&un); */
	    memcpy(A->I +ind -ioff,A->I +ind,nn*sizeof(doubleC));
	}
    }
  if ( A->m == 1) 
    {
      if ( nsp_mpmatrix_resize(A,A->m,A->n -Elts->mn)== FAIL) return(FAIL);
    }
  else 
    {
       if ( nsp_mpmatrix_resize(A,A->mn-Elts->mn,1)== FAIL) return(FAIL);
    } 
  return(OK);
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
  NspMaxpMatrix *Loc;
  integer rmin,rmax,cmin,cmax,i,j;
  Bounds(Rows,&rmin,&rmax);
  Bounds(Cols,&cmin,&cmax);
  if ( A->mn == 0) return nsp_mpmatrix_create(NVOID,A->rc_type,0,0);
  if ( rmin < 1 || cmin < 1 || rmax > A->m || cmax > A->n ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLMAXPMAT);
    }
  if ( (Loc = nsp_mpmatrix_create(NVOID,A->rc_type,Rows->mn,Cols->mn))== NULLMAXPMAT) 
    return(NULLMAXPMAT);
  if ( A->rc_type == 'i' )
    {
      for ( i = 0 ; i < Rows->mn ; i++)
 	for ( j = 0 ; j < Cols->mn ; j++ )
 	  {	
	    register int ind;
	    ind= ((int) Rows->R[i])-1+(((int) Cols->R[j])-1)*A->m;
 	    Loc->I[i+Loc->m*j].r=A->I[ind].r ;
 	    Loc->I[i+Loc->m*j].i=A->I[ind].i ;
 	  }
    }
  else 
    {
      for ( i = 0 ; i < Rows->mn ; i++)
 	for ( j = 0 ; j < Cols->mn ; j++ )
 	  { 
 	    Loc->R[i+Loc->m*j]=A->R[((int) Rows->R[i])-1+(((int) Cols->R[j])-1)*A->m] ;
 	  }
    }
  return(Loc);
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
  NspMaxpMatrix *Loc;
  integer rmin,rmax,i;
  Bounds(Elts,&rmin,&rmax);
  if ( A->mn == 0) return nsp_mpmatrix_create(NVOID,A->rc_type,0,0);
  if ( rmin < 1 || rmax > A->mn )
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLMAXPMAT);
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      if ( (Loc = nsp_mpmatrix_create(NVOID,A->rc_type,1,Elts->mn))== NULLMAXPMAT) 
	return(NULLMAXPMAT);
    }
  else
    {
      if ( (Loc = nsp_mpmatrix_create(NVOID,A->rc_type,Elts->mn,1))== NULLMAXPMAT) 
	return(NULLMAXPMAT);
    }
  if ( A->rc_type == 'i' )
    {
      for ( i = 0 ; i < Elts->mn ; i++)
	{	
	  Loc->I[i].r=A->I[((int) Elts->R[i])-1].r ;
	  Loc->I[i].i=A->I[((int) Elts->R[i])-1].i ;
	}
    }
  else 
    {
      for ( i = 0 ; i < Elts->mn ; i++)
	{ 
	  Loc->R[i]=A->R[((int) Elts->R[i])-1];
	}
    }
  return(Loc);
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
  NspMaxpMatrix *Loc;
  integer j,cmin,cmax;
  if ( A->mn == 0) return nsp_mpmatrix_create(NVOID,A->rc_type,0,0);
  Bounds(Cols,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->n ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLMAXPMAT);
    }
  if ((Loc = nsp_mpmatrix_create(NVOID,A->rc_type,A->m,Cols->mn)) == NULLMAXPMAT) 
    return(NULLMAXPMAT);
  if ( A->rc_type == 'i' )
    {
      for ( j = 0 ; j < Cols->mn ; j++ )
	memcpy( Loc->I+ Loc->m*j, A->I + (((int) Cols->R[j])-1)*A->m,A->m*sizeof(doubleC));
    }
  else 
    {
      for ( j = 0 ; j < Cols->mn ; j++ )
	memcpy( Loc->R+ Loc->m*j, A->R + (((int) Cols->R[j])-1)*A->m,A->m*sizeof(double));
    }
  return(Loc);
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
  NspMaxpMatrix *Loc;
  integer i,j,cmin,cmax;
  if ( A->mn == 0) return nsp_mpmatrix_create(NVOID,A->rc_type,0,0);
  Bounds(Rows,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->m ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLMAXPMAT);
    }
  Loc = nsp_mpmatrix_create(NVOID,A->rc_type,Rows->mn,A->n);
  if ( Loc == NULLMAXPMAT) 
      return(NULLMAXPMAT);
  if ( A->rc_type == 'i' )
    {
      for ( i = 0 ; i < Rows->mn ; i++)
	{
	  for ( j = 0 ; j < A->n ; j++ )
	    {	
	      Loc->I[i+Loc->m*j].r=A->I[(((int) Rows->R[i])-1)+ j*A->m].r ;
	      Loc->I[i+Loc->m*j].i=A->I[(((int) Rows->R[i])-1)+ j*A->m].i ;
	    }
	}
    }
  else 
    {
      for ( i = 0 ; i < Rows->mn ; i++)
 	for ( j = 0 ; j < A->n ; j++ )
 	  {	
 	    Loc->R[i+Loc->m*j]=A->R[(((int) Rows->R[i])-1)+ j*A->m] ;
 	  }
    }
  return(Loc);
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
  if ( A->rc_type == 'i' )
    {
      for ( i = 0 ; i < A->m ; i++)
	{
	  Loc->I[i].r=A->I[i+iof].r ;
	  Loc->I[i].i=A->I[i+iof].i ;
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
 * @k: an integer 
 *
 * Extract the @k-th diagonal of matrix @A and returns 
 * its value as a column vector. 
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_extract_diag(const NspMaxpMatrix *A, integer k)
{
  NspMaxpMatrix *Loc;
  int j,i;
  integer imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc = nsp_mpmatrix_create(NVOID,A->rc_type,(integer) 0 , (integer) 0);
      return(Loc);
    }
  Loc = nsp_mpmatrix_create(NVOID,A->rc_type,imax-imin,(integer)1);
  if ( Loc == NULLMAXPMAT)  return(NULLMAXPMAT);
  j=0;
  if ( A->rc_type == 'i') 
    {
      j=0;
      for ( i = imin ; i < imax ; i++ ) 
	{ 
          Loc->I[j].r = A->I[i+(i+k)*A->m].r;
          Loc->I[j++].i = A->I[i+(i+k)*A->m].i;
	}
    }
  else 
    {
      j=0;
      for ( i = imin ; i < imax ; i++ ) 
	{ 
          Loc->R[j++] = A->R[i+(i+k)*A->m];
	}
    }
  return(Loc);
}


/**
 * nsp_mpmatrix_set_diag:
 * @A: a #NspMaxpMatrix
 * @Diag: a #NspMaxpMatrix
 * @k: an integer 
 *
 * sets the @k-th diagonal of matrix @A with values from @Diag. 
 * 
 * returns %OK or %FAIL.
 */

int nsp_mpmatrix_set_diag(NspMaxpMatrix *A, NspMaxpMatrix *Diag, integer k)
{
  int i,j;
  integer imin,imax,isize;
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
      if ( nsp_mpmatrix_enlarge(A,imax,imax+k) == FAIL) return(FAIL);
    }
  j=0;

  if ( Diag->rc_type == 'r' ) 
    {
      if ( A->rc_type == 'r' ) 
	{
	  for ( i = imin ; i < A->m ; i++ ) 
	    A->R[i+(i+k)*A->m] = Diag->R[j++] ;
	}
      else 
	{
	  for ( i = imin ; i < A->m ; i++ ) 
	    {
	      A->I[i+(i+k)*A->m].r = Diag->R[j++] ;
	      A->I[i+(i+k)*A->m].i = 0.00;
	    }
	}
    }
  else 
    {
      if ( A->rc_type == 'r' ) 
	if (nsp_mat_complexify((NspMatrix *)A,0.00) == FAIL ) return(FAIL);
      for ( i = imin ; i < A->m ; i++ ) 
	{
	  A->I[i+(i+k)*A->m].r = Diag->I[j].r ;
	  A->I[i+(i+k)*A->m].i = Diag->I[j++].i ;
	}
    }
  return(OK);
}


/**
 * nsp_mpmatrix_create_diag:
 * @Diag: a #NspMaxpMatrix
 * @k: an integer 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns a #MspMatrix or %NULLMAXPMAT 
 */

NspMaxpMatrix *nsp_mpmatrix_create_diag(const NspMaxpMatrix *Diag, integer k)
{
  int i,j;
  integer imin,imax,nd;
  integer inc=1;
  double d=0.00;
  NspMaxpMatrix *Loc;
  imin = Max(0,-k);
  imax = Diag->mn +imin;
  nd = Diag->mn+Abs(k);
  if (( Loc = nsp_mpmatrix_create(NVOID,Diag->rc_type,nd,nd)) == NULLMAXPMAT)
    return(NULLMAXPMAT);
  switch (Loc->rc_type ) 
    {
    case 'r' : nsp_mat_set_rval((NspMatrix *) Loc,0.00);break;
    case 'i': nsp_csetd(&Loc->mn,&d,Loc->I,&inc);break;
    }
  if ( Loc->rc_type == 'i') 
    {
      j=0;
      for ( i = imin ; i < imax ; i++ ) 
	{
	  Loc->I[i+(i+k)*Loc->m].i= Diag->I[j].i;
	  Loc->I[i+(i+k)*Loc->m].r= Diag->I[j++].r;
	}
    }
  else 
    {
      j=0;
      for ( i = imin ; i < imax ; i++ ) 
	{
	  Loc->R[i+(i+k)*Loc->m]= Diag->R[j++];
	}
    }
  return(Loc);
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
  int i,j;
  NspMaxpMatrix *Loc;
  if (( Loc = nsp_mpmatrix_create(NVOID,A->rc_type,A->n,A->m)) == NULLMAXPMAT) return(NULLMAXPMAT);
  switch (Loc->rc_type ) 
    {
    case 'r' : 
      for ( i = 0  ; i < A->m ; i++) 
	for ( j = 0 ; j < A->n ; j++) 
	  Loc->R[j+Loc->m*i ] = A->R[i+A->m*j];
      break;
    case 'i' :
      for ( i = 0  ; i < A->m ; i++) 
	for ( j = 0 ; j < A->n ; j++) 
	  {
	    Loc->I[j+Loc->m*i ].r = A->I[i+A->m*j].r;
	    Loc->I[j+Loc->m*i ].i = -A->I[i+A->m*j].i;
	  }
      break;
    }
  return Loc;
}







