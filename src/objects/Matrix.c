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

/* extern functions from other scilab sections **/

#include "nsp/blas.h"
#include "nsp/matutil.h"


/*
 * matrix-Title:
 * This is the title of the matrix section 
 */

/**
 * nsp_matrix_create:
 * @name: matrix name 
 * @type: real ('r') or complex('c') 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a new matrix with unspecified values, returns %NULLMAT on failure. 
 * Returns a #NspMatrix or %NULLMAT.
 */

NspMatrix * nsp_matrix_create(const char *name, char type, int m, int n)
{
  NspMatrix *Mat = new_matrix();
  
  if ( Mat == NULLMAT) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAT);
    }
  /* shared by all objects */
  if ((NSP_OBJECT(Mat)->name = NewString(name))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAT);
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
	  return(NULLMAT);
	}
      break;
    case 'i' : 
      Mat->R = (double *) 0; 
      Mat->I =nsp_alloc_doubleC(Mat->mn);
      if (  Mat->I == (doubleC *) 0) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return(NULLMAT);
	}
    }
#ifdef OCAML 
  Mat->proxy = NULL; 
#endif
  return(Mat);
}

/*
 * used when data is transmited by caml 
 */

#ifdef OCAML 
NspMatrix *MatCreateFromData(char *name, char type, int m, int n,
			     struct caml_bigarray *b)
{
  struct caml_bigarray_proxy * proxy;
  NspMatrix *Mat = new_matrix();
  if ( Mat == NULLMAT) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAT);
    }

  /* shared by all objects */
  if ((NSP_OBJECT(Mat)->name = NewString(name))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMAT);
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
 * nsp_matrix_create_impl:
 * @first: first value as a double 
 * @step: step 
 * @last:  last value as a double
 *
 * creates a row matrix with values from @first to @last with step @step. 
 * Returns a #NspMatrix or %NULLMAT.
 */


NspMatrix *nsp_matrix_create_impl(double first, double step, double last)
{
  int i;
  NspMatrix *Loc;
  double vals = first;
  int count=0;
  if ( (first < last && step < 0 ) 
       || (first >  last && step > 0 ) 
       || step == 0.00)
    {
      Loc = nsp_matrix_create(NVOID,'r',(int) 0,(int) 0);
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
    return NULLMAT;
  }
  Loc = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
  if ( Loc == NULLMAT) return(NULLMAT);
  for ( i=0 ; i < count; i++) 
    {
      Loc->R[i] = first + ((double) i)*step;
    }
  return(Loc);
}

/**
 * nsp_matrix_create_linspace:
 * @first: first values (size @r)
 * @last:  last values  (size @r)
 * @r : number of elements in @first and @last 
 * @n : number of points 
 *
 * creates a (@r,@n) matrix with n values in each row from @first[i] to @last[i] 
 * regularly spaced 
 *  
 * Returns a #NspMatrix or %NULLMAT.
 */


NspMatrix *nsp_matrix_create_linspace(const double first[],const double last[],int r,int n)
{
  int i,j;
  NspMatrix *Loc;
  if ( r == 0 || n == 0) 
    {
      Loc = nsp_matrix_create(NVOID,'r',(int) 0,(int) 0);
      return(Loc);
    }
  if ((Loc = nsp_matrix_create(NVOID,'r',r,n))== NULLMAT) return NULLMAT;
  for ( i=0 ; i < r ; i++) 
    {
      for ( j=0 ; j < n ; j++) 
	{
	  if ( n == 0 ) 
	    Loc->R[i+j*Loc->m]=  last[i]; /* just as in Scilab ! */
	  else
	    Loc->R[i+j*Loc->m]= (last[i]-first[i])*((double)j)/(n-1) + first[i];
	}
    }
  return(Loc);
}

/**
 * nsp_matrix_create_logspace:
 * @first: first values (size @r)
 * @last:  last values  (size @r)
 * @r : number of elements in @first and @last 
 * @n : number of points 
 *
 * creates a (@r,@n) matrix with n values in each row from @first[i] to @last[i] 
 * logarithmically spaced 
 * 
 * Returns a #NspMatrix or %NULLMAT.
 */

NspMatrix *nsp_matrix_create_logspace(const double first[],const double last[],int r,int n)
{
  int i,j;
  NspMatrix *Loc;
  if ( r == 0 || n == 0) 
    {
      Loc = nsp_matrix_create(NVOID,'r',(int) 0,(int) 0);
      return(Loc);
    }
  if ((Loc = nsp_matrix_create(NVOID,'r',r,n))== NULLMAT) return NULLMAT;
  for ( i=0 ; i < r ; i++) 
    {
      for ( j=0 ; j < n ; j++) 
	{
	  if ( n == 0 ) 
	    Loc->R[i+j*Loc->m]=  pow(10.0,last[i]); /* just as in Scilab ! */
	  else
	    Loc->R[i+j*Loc->m]= pow(10.0,(last[i]-first[i])*((double)j)/(n-1) + first[i]);
	}
    }
  return(Loc);
}



/**
 * nsp_matrix_create_from_doubles:
 * @name: matrix name 
 * @m: number of rows 
 * @n: number of columns 
 * @...: values given as double 
 * 
 * creates a new scalar matrix of size @m x @n filled with given @m x @n values. 
 * returns the matrix or %NULLMAT
 */

NspMatrix *nsp_matrix_create_from_doubles(const char *name,int m,int n,...)
{
  int i;
  NspMatrix *Loc;
  if (( Loc = nsp_matrix_create(name,'r',m,n)) == NULLMAT) return NULLMAT;
  va_list ap;
  va_start(ap,n);
  for ( i= 0 ; i < m*n ; i++) 
    Loc->R[i] = (double) va_arg(ap,double);
  va_end(ap);
  return(Loc);
}


/**
 * nsp_matrix_create_from_array:
 * @name: matrix name 
 * @m: number of rows 
 * @n: number of columns 
 * @valr: real values. 
 * @valc: imaginary values or NULL. 
 * 
 * creates a new scalar matrix of size @m x @n filled with given @m x @n values. 
 * returns the matrix or %NULLMAT
 */

NspMatrix *nsp_matrix_create_from_array(const char *name,int m,int n,const double valr[],const double valc[])
{
  int i;
  NspMatrix *Loc;
  if ( valc == NULL) 
    {
      if (( Loc = nsp_matrix_create(name,'r',m,n)) == NULLMAT) return NULLMAT;
      for ( i= 0; i < Loc->mn; i++) Loc->R[i]=valr[i];
    }
  else 
    {
      if (( Loc = nsp_matrix_create(name,'i',m,n)) == NULLMAT) return NULLMAT;
      for ( i= 0; i < Loc->mn; i++) { Loc->I[i].r =valr[i];Loc->I[i].i =valc[i];}
    }
  return Loc;
}


/**
 * nsp_matrix_copy:
 * @A: a #NspMatrix 
 *
 * copies #NspMatrix @A and returns the copy or %NULLMAT.
 * Returns a #NspMatrix or %NULLMAT.
 */

NspMatrix *nsp_matrix_copy(const NspMatrix *A)
{
  NspMatrix *Mat;
  if ((Mat = nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) { return(NULLMAT);}
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
 * nsp_matrix_fill_with:
 * @A: a #NspMatrix 
 * @B: a #NspMatrix 
 * 
 * The #NspMatrix @A size ans type (real or complex) are changed 
 * to be the same as size and type of @B. Then @A is filled 
 * with @B data. 
 *
 * Returns %OK or %FAIL. When %OK is returned @A is changed. 
 */

int nsp_matrix_fill_with(NspMatrix *A,const NspMatrix *B)
{
  switch ( B->rc_type ) 
    {
    case 'r' : 
      if (A->rc_type == 'i') 
	{ 
	  if (nsp_mat_get_real(A) == FAIL) return FAIL; 
	}
      break; 
    case 'i' : 
      if (A->rc_type == 'r') 
	{ 
	  if (nsp_mat_complexify(A,0.0) == FAIL) return FAIL; 
	}
      break;
    }
  if ( nsp_matrix_resize(A, B->m, B->n) == FAIL) return FAIL;
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
 * nsp_matrix_resize:
 * @A: a #NspMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspMatrix @A dimensions are changed to be @m x @n. 
 * This routine only enlarges or shrink (using realloc()) 
 * the data array of @A to size mxn. The previous data are not moved and 
 * occupy the first array cells. Note that @A can be 
 * and empty matrix when calling this routine ( malloc() is used in that 
 * case ). 
 *
 * returns %OK or %FAIL. When %OK is returned @A is changed. 
 */

int nsp_matrix_resize(NspMatrix *A, int m, int n)
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
 * nsp_matrix_scalar_to_mn:
 * @A: a #NspMatrix 
 * @m: number of rows 
 * @n: number of columns 
 * 
 * The #NspMatrix @A of dimension 1x1 is changed to a matrix of size @m x @n 
 * filled with the @A scalar value i.e A= A(1,1)*ones(m;n). 
 * Note that the size of @A is not checked on entry it sould be 1x1.
 *
 * returns %OK or %FAIL. When %OK is returned @A is changed. 
 */

int nsp_matrix_scalar_to_mn(NspMatrix *A, int m, int n)
{
  int i;
  doubleC x;
  if ( nsp_matrix_resize(A,m,n) == FAIL) return FAIL; 
  switch ( A->rc_type ) 
    {
    case 'r' : nsp_mat_set_rval(A,A->R[0]);break; 
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
 *nsp_matrix_destroy:
 * @Mat: a #NspMatrix 
 * 
 * free the #NspMatrix @Mat. 
 */

void nsp_matrix_destroy(NspMatrix *Mat)
{
  if ( Mat == NULLMAT ) return ; 
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
 * nsp_matrix_info:
 * @Mat: a #NspMatrix
 * @indent: an int 
 * 
 * Display info on the #NspMatrix @A using the default Sciprintf() function. 
 * @indent is the given indentation for printing.
 */

void nsp_matrix_info(const NspMatrix *Mat, int indent)
{
  int i;
  if ( Mat == NULLMAT) 
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
 * nsp_matrix_print:
 * @Mat: a #NspMatrix
 * @indent: an int 
 * @header: a flag, if %FALSE then data are printed but not header information
 *
 * Print the #NspMatrix @A using the default nsp output function. 
 * @indent is the given indentation for printing.
 */

void nsp_matrix_print( NspMatrix *Mat, int indent,int header )
{
  int i;
  Mat = Mat2double(Mat); /* we should write a nsp_print_internal when Mat is int  XXXXX */
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
	{
	  Sciprintf("%s=%s",NSP_OBJECT(Mat)->name,(Mat->mn==0 ) ? " []\n" : "" );
	}
    }
  else 
    {
      Sciprintf("%s\t=%s\t\t%c (%dx%d)\n",NSP_OBJECT(Mat)->name,
		(Mat->mn==0 ) ? " []" : "",Mat->rc_type,Mat->m,Mat->n);
    }
  if ( Mat->mn != 0) 
    {
      if ( Mat->rc_type == 'r') 
	nsp_print_internalM (Mat,indent);
      else 
	nsp_print_internalCM (Mat,indent);
    }
}


/**
 * nsp_matrix_latex_print:
 * @Mat: a #NspMatrix
 * 
 * print the #NspMatrix @A using the default Sciprintf() function and LaTeX 
 * syntax. 
 */

void nsp_matrix_latex_print(const NspMatrix *Mat)
{
  int i,j;
  if ( Mat->rc_type == 'r' ) 
    {
      if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
      if ( strcmp(NSP_OBJECT(Mat)->name,NVOID) != 0) 
	Sciprintf("{%s = \\left(\\begin{array}{",NSP_OBJECT(Mat)->name );
      else 
	Sciprintf("{\\left(\\begin{array}{");
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
      if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
    }

  else 
    {
      Sciprintf("Fixme : to be done\n");
    }
}

/**
 * nsp_matrix_latex_tab_print:
 * @Mat: a #NspMatrix
 * 
 * print the #NspMatrix @A using the default Sciprintf() function and LaTeX tabular
 * syntax. 
 */

void nsp_matrix_latex_tab_print(const NspMatrix *Mat)
{
  int i,j;
  if ( Mat->rc_type == 'r' ) 
    {
      if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:");
      Sciprintf("\\begin{tabular}{|l|");
      for (i=0; i < Mat->n ;i++) Sciprintf("c|");
      Sciprintf("}\\hline\n %s &\t",NSP_OBJECT(Mat)->name);
      for (i=0; i < Mat->n -1 ;i++) Sciprintf("$C_{%d}$\t&",i+1);
      Sciprintf("$C_{%d}$\\\\ \\hline\n",Mat->n);
      for (j=0;j < Mat->m ;j++)
	{
	  Sciprintf("$L_{%d}$\t&",j+1);
	  for (i=0;i < Mat->n-1 ;i++)
	    {
	      Sciprintf("$%g$\t& ",Mat->R[i+j*Mat->m]);
	    }
	  Sciprintf("$%g$\t\\\\ \\hline\n",Mat->R[Mat->m-1+j*Mat->m]);
	}
      Sciprintf("\\end{tabular}\n");
      if ( nsp_from_texmacs() == TRUE ) Sciprintf("\005");
    }
  else 
    {
      Sciprintf("Fixme : to be done\n");
    }
}


/**
 * nsp_matrix_redim:
 * @A: a #NspMatrix
 * @m: number of rows 
 * @n: number of columns
 * 
 * Checks that the #NspMatrix @A of size m' x n' satisfy m'*n' = @m * @n and reshapes 
 * @A to size m x @n.
 *
 * returns %OK or %FAIL.
 */

int nsp_matrix_redim(NspMatrix *A, int m, int n)
{
  
  if ( m == -1 ) m = (n== 0) ? 0 : A->mn/n;
  if ( n == -1 ) n = (m== 0) ? 0 : A->mn/m;
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
 * nsp_matrix_enlarge:
 * @A: a #NspMatrix
 * @m: number of rows 
 * @n: number of columns
 * 
 * Changes the #NspMatrix @A size to max(A->m,m) x max(A->n,n) adding 
 * rows and columns of zero to @A. 
 * A = [ A ,ones(A->m,Max(n-A->n,0); 
 *      ones(Max(m-A->m,0), Max(A->n,n)) ]
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_enlarge(NspMatrix *A, int m, int n)
{
  if ( A->mn == 0) 
    {
      /* special case A=[] **/
      if ( nsp_matrix_resize(A,m,n) == FAIL) return(FAIL);
 nsp_mat_set_rval(A,0.00);
      return OK;
    }
  if ( n > A->n  )
    if ( nsp_matrix_add_columns(A,n- A->n) == FAIL) return(FAIL);
  if ( m > A->m  )  
    if ( nsp_matrix_add_rows(A, m - A->m) == FAIL) return(FAIL);
  return(OK);
}

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )


/**
 * nsp_matrix_concat_right:
 * @A: a #NspMatrix
 * @B: a #NspMatrix
 * 
 * Changes the #NspMatrix @A to [@A , @B ] ; 
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_concat_right(NspMatrix *A,const NspMatrix *B)
{
  int inc = 1;
  int Asize;
  Asize=A->mn;
  if ( A->m != B->m ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(FAIL);
    }
  if ( nsp_matrix_resize(A,A->m,A->n+B->n) == FAIL) return(FAIL);
  if ( A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r' )
	{
	  /* C2F(dcopy) (&B->mn,B->R,&inc,A->R+Asize,&inc); */
	  memcpy (A->R+Asize,B->R,sizeof(double)*B->mn);
	}
      else 
	{
	  if (nsp_mat_complexify(A,0.00) == FAIL ) return(FAIL);
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
 * nsp_matrix_add_columns:
 * @A: a #NspMatrix
 * @n: number of columns 
 * 
 * Adds @n columns to the #NspMatrix. Note that 
 * if @A is an empy matrix on entry it remains empty.
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_add_columns(NspMatrix *A, int n)
{
  double d=0.0;
  int inc = 1,ns;
  int Asize;
  if (n == 0) return OK;
  else if ( n < 0) 
    {      
      Scierror("Error: Negative indice (%d) in MatAddCols\n",n);
      return FAIL;
    }

  Asize=A->mn;
  ns= (A->m)*n;
  if ( nsp_matrix_resize(A,A->m,A->n+n) == FAIL) return(FAIL);
  switch ( A->rc_type )
    {
    case 'r' : nsp_dset(&ns,&d,A->R+Asize,&inc);break;
    case 'i' : nsp_csetd(&ns,&d,A->I+Asize,&inc);break;
    }
  return(OK);
}


/**
 * nsp_matrix_concat_down:
 * @A: a #NspMatrix
 * @B: a #NspMatrix
 * 
 * returns a new #NspMatrix equals to  [@A ; @B ] ; 
 * 
 * returns a #NspMatrix or %NULLMAT.
 */

NspMatrix* nsp_matrix_concat_down(const NspMatrix *A,const NspMatrix *B)
{
  char type = 'r';
  NspMatrix *Loc;
  int inc = 1;
  int j;
  if ( A->n != B->n ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(NULLMAT);
    }
  if ( A->rc_type == 'i' || B->rc_type == 'i' ) type = 'i';
  if (( Loc = nsp_matrix_create(NVOID,type,A->m+B->m,A->n)) == NULLMAT)  return(NULLMAT);
  if ( Loc->rc_type == 'r' ) 
    {
      for ( j = 0 ; j < A->n ; j++ ) 
 	{
	  /*
 	  C2F(dcopy) (&A->m,A->R+j*A->m,&inc,Loc->R+j*(Loc->m),&inc);
 	  C2F(dcopy) (&B->m,B->R+j*B->m,&inc,Loc->R+j*(Loc->m)+A->m,&inc);
	  */
	  memcpy(Loc->R+j*(Loc->m),A->R+j*A->m,A->m*sizeof(double));
	  memcpy(Loc->R+j*(Loc->m)+A->m,B->R+j*B->m,B->m*sizeof(double));
 	}
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
	    /*
		C2F(zcopy) (&A->m,A->I+j*A->m,&inc,Loc->I+j*(Loc->m),&inc); **/
	    memcpy(Loc->I+j*(Loc->m),A->I+j*A->m,A->m*sizeof(doubleC));
 	  break;
 	}
      switch ( B->rc_type ) 
 	{
 	case 'r' :
 	  for ( j = 0 ; j < A->n ; j++ ) 
 	nsp_dzcopy(&B->m,B->R+j*B->m,&inc,Loc->I+j*(Loc->m)+A->m,&inc);
 	  break;
 	case 'i' :
 	  for ( j = 0 ; j < A->n ; j++ ) 
	    /* 
 	    C2F(zcopy) (&B->m,B->I+j*B->m,&inc,Loc->I+j*(Loc->m)+A->m,&inc);
	    */
	    memcpy(Loc->I+j*(Loc->m)+A->m,B->I+j*B->m,B->m*sizeof(doubleC));
 	  break;
 	}
    }
  return(Loc) ;
}

/**
 * nsp_matrix_concat_diag:
 * @A: a #NspMatrix
 * @B: a #NspMatrix
 * 
 * returns a new #NspMatrix equals to  [@A,0 ;0, @B ] ; 
 * 
 * returns a #NspMatrix or %NULLMAT.
 */

NspMatrix*nsp_matrix_concat_diag(const NspMatrix *A,const NspMatrix *B)
{
  char type = 'r';
  NspMatrix *Loc;
  double d=0.0;
  int inc = 1,j=0;
  if ( A->rc_type == 'i' || B->rc_type == 'i' ) type = 'i';
  if ((Loc = nsp_matrix_create(NVOID,type,A->m+B->m,A->n+B->n))==NULLMAT )
    return NULLMAT;
   
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
 * nsp_matrix_add_rows:
 * @A: a #NspMatrix
 * @m: number of rows
 * 
 * Adds @n rows to the #NspMatrix. Note that 
 * if @A is an empy matrix on entry it remains empty.
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_add_rows(NspMatrix *A, int m)
{
  double d=0.0;
  int inc = -1,Am;
  int j;
  if ( m == 0) return OK;
  else if ( m < 0) 
    {      
      Scierror("Error: Negative indice (%d) in MatAddRows\n",m);
      return FAIL;
    }
  Am= A->m;
  if ( nsp_matrix_resize(A,A->m+m,A->n)== FAIL) return(FAIL);
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
 * nsp_matrix_set_submatrix:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 * @B: a #NspMatrix
 * 
 * Performe  A(Rows,Cols) = B. A is changed and enlarged if necessary and 
 * size compatibility is checked i.e B must be scalar or  
 * we must have size(B)==[size(Rows),size(Cols)]. 
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_set_submatrix(NspMatrix *A, NspMatrix *Rows, NspMatrix *Cols, NspMatrix *B)
{
  int rmin,rmax,cmin,cmax,i,j;
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
    if ( nsp_matrix_enlarge(A,rmax,cmax) == FAIL) return(FAIL);
  if ( B->rc_type == 'i' )
    {
      if (  A->rc_type == 'r' )
 	{ 
 	  if (nsp_mat_complexify(A,0.00) == FAIL ) return(FAIL);
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
 * nsp_matrix_set_rows:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 * @B: a #NspMatrix
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

/*generic function which check sizes and enlarge A if necessary */

int GenericMatSeRo(void *A, int Am, int An, int Amn, NspMatrix *Rows, void *B, int Bm, int Bn, int Bmn, F_Enlarge F, int *Bscal)
{
  int rmin,rmax;
  Bounds(Rows,&rmin,&rmax);
  if ( rmin < 1 ) 
    {
      Scierror("Error:\tNegative indices are not allowed\n");
      return(FAIL);
    }
  if ( Bm != 1 && Bn != 1) 
    {
      Scierror("Error:\tA(x)=B, B must be a vector");
      return(FAIL);
    }
  if ( Am == 1  && Bm != 1) 
    {
      Scierror("Error:\tA(x)=B, B must be row when A is a row\n");
      return(FAIL);
    } 
  if ( An == 1 && Bn != 1 )
    {
      Scierror("Error:\tA(x)=B, B must be column when A is a column\n");
      return(FAIL);
    }
  /* Enlarging A **/
  if ( rmax > Amn ) 
    {
      if ( Amn == 0) 
	{
	  if ( Bn != 1) 
	    { if ( (*F)(A,1,rmax) == FAIL) return(FAIL);}
	  else
	    { if ( (*F)(A,rmax,1) == FAIL) return(FAIL);}
	}
      else if ( Am == 1) 
	{
	  if ( An == 1) 
	    {
	      if ( Bn != 1) 
		{if ( (*F)(A,1,rmax) == FAIL) return(FAIL);}
	      else
		{if ( (*F)(A,rmax,1) == FAIL) return(FAIL);}
	    } 
	  else
	    {
	      if ( (*F)(A,Am,rmax) == FAIL) return(FAIL);
	    }
	}
      else
	{
	  if ( An == 1)
	    {
	      if ( (*F)(A,rmax,An) == FAIL) return(FAIL);
	    }
	  else
	    {
	      Scierror("Error:\tA(x)=B, x must be inside A range when A is not a vector\n");
	      return(FAIL);
	    }  
	}
    }
  if ( Bmn == 1) 
    {
      *Bscal=1;
    }
  else
    {
      if ( Bmn != Rows->mn ) 
	{
	  Scierror("Error:\tA(x)=B, x and B have incompatible sizes\n");
	  return FAIL;
	}
      *Bscal=0;
    }
  return OK;
}


int nsp_matrix_set_rows(NspMatrix *A, NspMatrix *Rows, NspMatrix *B)
{
  int i;
  int Bscal=0;
  if (Rows->mn == 0) return OK;
  if (GenericMatSeRo(A,A->m,A->n,A->mn,Rows,
		     B,B->m,B->n,B->mn,(F_Enlarge)nsp_matrix_enlarge,&Bscal)== FAIL) 
    return FAIL;

  if ( B->rc_type == 'i' )
    {
      if (  A->rc_type == 'r' )
 	{ 
 	  if (nsp_mat_complexify(A,0.00) == FAIL ) return(FAIL);
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
 * nsp_matrix_is_increasing;
 * @A: a #NspMatrix
 * 
 * Checks if the #NspMatrix @A contains values in increasing order. 
 *
 * returns %OK or %FAIL.
 */

int mat_is_increasing(const NspMatrix *A)
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
 * nsp_matrix_delete_columns:
 * @A: a #NspMatrix
 * @Cols: a #NspMatrix
 *
 * Performs A(:,Cols) = []. 
 * Important Note: @Cols must be real and increasing 
 * and this is not checked here
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_delete_columns(NspMatrix *A, NspMatrix *Cols)
{
  int ioff=0,cmin,cmax,i,col,last,nn;
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
  if ( nsp_matrix_resize(A,A->m,A->n-ioff)== FAIL) return(FAIL);
  return(OK);
}


/**
 * nsp_matrix_delete_rows:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 *
 * Performs A(Rows,:)  = []. 
 * Important Note: @Rows  must be real and increasing 
 * and this is not checked here
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_delete_rows(NspMatrix *A, NspMatrix *Rows)
{
  int rmin,rmax,i,j,ind,last,nn,ioff=0;
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
  if ( nsp_matrix_resize(A,A->m -Rows->mn,A->n)== FAIL) return(FAIL);
  return(OK);
}



/**
 * nsp_matrix_delete_elements:
 * @A: a #NspMatrix
 * @Elts: a #NspMatrix
 *
 * Performs A(Elts) = []. 
 * Important Note: @Elts must be real and increasing 
 * and this is not checked here
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_delete_elements(NspMatrix *A, NspMatrix *Elts)
{
  int rmin,rmax,i,ind,last,nn,ioff=0;
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
      if ( nsp_matrix_resize(A,A->m,A->n -Elts->mn)== FAIL) return(FAIL);
    }
  else 
    {
       if ( nsp_matrix_resize(A,A->mn-Elts->mn,1)== FAIL) return(FAIL);
    } 
  return(OK);
}

/**
 * nsp_matrix_extract:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(Rows,Cols) and returns the new #MspMatrix 
 * 
 * returns a #MspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract(const NspMatrix *A,const  NspMatrix *Rows, const NspMatrix *Cols)
{
  NspMatrix *Loc;
  int rmin,rmax,cmin,cmax,i,j;
  Bounds(Rows,&rmin,&rmax);
  Bounds(Cols,&cmin,&cmax);
  if ( A->mn == 0) return nsp_matrix_create(NVOID,A->rc_type,0,0);
  if ( rmin < 1 || cmin < 1 || rmax > A->m || cmax > A->n ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLMAT);
    }
  if ( (Loc = nsp_matrix_create(NVOID,A->rc_type,Rows->mn,Cols->mn))== NULLMAT) 
    return(NULLMAT);
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
 * nsp_matrix_extract_elements:
 * @A: a #NspMatrix
 * @Elts: a #NspMatrix
 *
 * Compute A(Elts) and returns the new #MspMatrix 
 * 
 * returns a #MspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract_elements(const NspMatrix *A,const NspMatrix *Elts)
{
  NspMatrix *Loc;
  int rmin,rmax,i;
  Bounds(Elts,&rmin,&rmax);
  if ( A->mn == 0) return nsp_matrix_create(NVOID,A->rc_type,0,0);
  if ( rmin < 1 || rmax > A->mn )
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLMAT);
    }
  if ( A->m == 1 && A->n > 1 ) 
    {
      if ( (Loc = nsp_matrix_create(NVOID,A->rc_type,1,Elts->mn))== NULLMAT) 
	return(NULLMAT);
    }
  else
    {
      if ( (Loc = nsp_matrix_create(NVOID,A->rc_type,Elts->mn,1))== NULLMAT) 
	return(NULLMAT);
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
 * nsp_matrix_extract_columns:
 * @A: a #NspMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(:,Cols) and returns the new #MspMatrix 
 * 
 * returns a #MspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract_columns(const NspMatrix *A,const NspMatrix *Cols)
{
  NspMatrix *Loc;
  int j,cmin,cmax;
  if ( A->mn == 0) return nsp_matrix_create(NVOID,A->rc_type,0,0);
  Bounds(Cols,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->n ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLMAT);
    }
  if ((Loc = nsp_matrix_create(NVOID,A->rc_type,A->m,Cols->mn)) == NULLMAT) 
    return(NULLMAT);
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
 * nsp_matrix_extract_rows:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 *
 * Compute A(Rows,:) and returns the new #MspMatrix 
 * 
 * returns a #MspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract_rows(const NspMatrix *A,const NspMatrix *Rows)
{
  NspMatrix *Loc;
  int i,j,cmin,cmax;
  if ( A->mn == 0) return nsp_matrix_create(NVOID,A->rc_type,0,0);
  Bounds(Rows,&cmin,&cmax);
  if ( cmin < 1 || cmax  > A->m ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return(NULLMAT);
    }
  Loc = nsp_matrix_create(NVOID,A->rc_type,Rows->mn,A->n);
  if ( Loc == NULLMAT) 
      return(NULLMAT);
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

NspMatrix *MatLoopCol(char *str, NspMatrix *Col, NspMatrix *A, int icol, int *rep)
{
  int i;
  register int iof;
  NspMatrix *Loc;
  if ( icol > A->n )
    {
      *rep = RET_ENDFOR;
      return(NULLMAT);
    }
  *rep =0;
  if ( Col == NULLMAT) 
    Loc = nsp_matrix_create(str,A->rc_type,A->m,1);
  else 
    Loc = Col;
  if ( Loc == NULLMAT) return(NULLMAT);
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
 * nsp_matrix_extract_diag:
 * @A: a #NspMatrix
 * @k: an int 
 *
 * Extract the @k-th diagonal of matrix @A and returns 
 * its value as a column vector. 
 * 
 * returns a #MspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract_diag(const NspMatrix *A, int k)
{
  NspMatrix *Loc;
  int j,i;
  int imin,imax;
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  if ( imin > imax ) 
    {
      Loc = nsp_matrix_create(NVOID,A->rc_type,(int) 0 , (int) 0);
      return(Loc);
    }
  Loc = nsp_matrix_create(NVOID,A->rc_type,imax-imin,(int)1);
  if ( Loc == NULLMAT)  return(NULLMAT);
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
 * nsp_matrix_set_diag:
 * @A: a #NspMatrix
 * @Diag: a #NspMatrix
 * @k: an int 
 *
 * sets the @k-th diagonal of matrix @A with values from @Diag. 
 * 
 * returns %OK or %FAIL.
 */

int nsp_matrix_set_diag(NspMatrix *A, NspMatrix *Diag, int k)
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
      if ( nsp_matrix_enlarge(A,imax,imax+k) == FAIL) return(FAIL);
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
	if (nsp_mat_complexify(A,0.00) == FAIL ) return(FAIL);
      for ( i = imin ; i < A->m ; i++ ) 
	{
	  A->I[i+(i+k)*A->m].r = Diag->I[j].r ;
	  A->I[i+(i+k)*A->m].i = Diag->I[j++].i ;
	}
    }
  return(OK);
}


/**
 * nsp_matrix_create_diag:
 * @Diag: a #NspMatrix
 * @k: an int 
 *
 * Creates a square marix with its @k-th diagonal filled with @Diag.
 * 
 * returns a #MspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_create_diag(const NspMatrix *Diag, int k)
{
  int i,j;
  int imin,imax,nd;
  int inc=1;
  double d=0.00;
  NspMatrix *Loc;
  imin = Max(0,-k);
  imax = Diag->mn +imin;
  nd = Diag->mn+Abs(k);
  if (( Loc = nsp_matrix_create(NVOID,Diag->rc_type,nd,nd)) == NULLMAT)
    return(NULLMAT);
  switch (Loc->rc_type ) 
    {
    case 'r' : nsp_mat_set_rval(Loc,0.00);break;
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
 * nsp_matrix_transpose: 
 * @A: a #NspMatrix
 *
 * return the transpose of A
 * 
 * returns a #MspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_transpose(const NspMatrix *A)
{
  int i,j;
  NspMatrix *Loc;
  if (( Loc = nsp_matrix_create(NVOID,A->rc_type,A->n,A->m)) == NULLMAT) return(NULLMAT);
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







