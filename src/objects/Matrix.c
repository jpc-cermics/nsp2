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
#include "nsp/blas.h"
#include "nsp/matutil.h"

/* should be moved in matint */

#define WORK_SIZE 100
int iwork1[WORK_SIZE];
int iwork2[WORK_SIZE];

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
  if ( nsp_object_set_initial_name(NSP_OBJECT(Mat),name) == NULL)
    return(NULLMAT);
  
  NSP_OBJECT(Mat)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /* specific for Matrix */
  Mat->m=m;
  Mat->n=n;
  Mat->rc_type = type;
  Mat->mn=m*n;
  Mat->convert = 'd'; 
  if ( Mat->mn == 0 ) 
    {
#ifdef MTLB_MODE
#else
      Mat->m = Mat->n=0;
#endif
      Mat->R = (double *) 0; 
      Mat->C = (doubleC *) 0;
      return(Mat);
    }
  switch ( type ) 
    {
    case 'r' :
      Mat->C = (doubleC *) 0;
      Mat->R =nsp_alloc_doubles(Mat->mn);
      if ( Mat->R == (double *) 0 ) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return(NULLMAT);
	}
      break;
    case 'c' : 
      Mat->R = (double *) 0; 
      Mat->C =nsp_alloc_doubleC(Mat->mn);
      if (  Mat->C == (doubleC *) 0) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return(NULLMAT);
	}
    }
  return(Mat);
}

/**
 * nsp_matrix_clone:
 * @name: matrix name 
 * @A: a #NspMatrix
 * @m: number of rows 
 * @n: number of columns 
 * 
 * Creates a new matrix with same rc_type than A with unspecified values, returns %NULLMAT on failure. 
 * Returns a #NspMatrix or %NULLMAT.
 */
NspMatrix *nsp_matrix_clone(const char *name, NspMatrix *A, int m, int n,int init)
{
  NspMatrix *loc =  nsp_matrix_create(name, A->rc_type, m, n);
  /* if ( loc != NULL) loc->convert = A->convert ; */
  if ( init == TRUE ) 
    {
      double d=0.0;
      int inc=1;
      switch ( loc->rc_type )
	{
	case 'r' : nsp_dset(&loc->mn,&d,loc->R,&inc);break;
	case 'c' : nsp_csetd(&loc->mn,&d,loc->C,&inc);break;
	}
    }
  return loc;
}

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
      Loc = nsp_matrix_create(NVOID,'r',(int) 1,(int) 0);
      return(Loc);
    }
  /* counting **/
  if ( step > 0 )
    {
      /*       while ( vals <= last ) { vals += step ; count++;} */
      count = 1 + (int) floor((last-first)/step);
      vals = first + count*step;
      if ( vals -last <  Max(Abs(first),Abs(last))*DBL_EPSILON*10) count++;
    }
  else if ( step < 0)
    {
      /*       while ( vals >= last ) { vals += step ; count++;} */
      count = 1 + (int) floor((last-first)/step);
      vals = first + count*step;
      if ( last - vals <  Max(Abs(first),Abs(last))*DBL_EPSILON*10) count++;
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

NspMatrix *nsp_matrix_create_int_impl(int first, int step, int last)
{
  NspMatrix *Loc;
  int count=0;
  if ( (first < last  &&  step < 0) || (first > last && step > 0) || step == 0 )
    {
      Loc = nsp_matrix_create(NVOID,'r', 1, 0);
      return Loc;
    }

  /* counting **/
  if ( step == 1 )
    count = last - first + 1;
  else if ( step == -1 )
    count = first - last + 1;
  else if ( step > 0 )
    count = 1 + (last-first)/step;
  else
    count = 1 + (first-last)/(-step);

  if ( (Loc = nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT )
    return NULLMAT;
  Loc->m=1;
  Loc->n=Loc->mn=count;
  Loc->impl[0]=first;
  Loc->impl[1]=step;
  Loc->convert = 'u';
  return Loc;
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
      if (( Loc = nsp_matrix_create(name,'c',m,n)) == NULLMAT) return NULLMAT;
      for ( i= 0; i < Loc->mn; i++) { Loc->C[i].r =valr[i];Loc->C[i].i =valc[i];}
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
  if ( A->convert == 'u' )
    {
      if ((Mat = nsp_matrix_create(NVOID,A->rc_type,0,0)) == NULLMAT)
	return(NULLMAT);
      Mat->m = A->m;
      Mat->n = A->n;
      Mat->mn = A->mn;
      Mat->impl[0]= A->impl[0];
      Mat->impl[1]= A->impl[1];
      Mat->convert=A->convert;
    }
  else 
    {
      if ((Mat = nsp_matrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLMAT) { return(NULLMAT);}
      switch ( Mat->rc_type ) 
	{
	case 'r' :
	  /* C2F(dcopy)(&(Mat->mn),A->R,&inc,Mat->R,&inc); */
	  memcpy(Mat->R,A->R, (Mat->mn)*sizeof(double));
	  break;
	case 'c' :
	  /* C2F(zcopy)(&(Mat->mn),A->C,&inc,Mat->C,&inc); */
	  memcpy(Mat->C,A->C, (Mat->mn)*sizeof(doubleC));
	  break;
	}
      Mat->convert=A->convert;
    }
  return(Mat);
}


/**
 * nsp_matrix_elt_size:
 * @M: a #NspMatrix 
 * 
 * size of matrix elements.
 * 
 * Return value: size of @M elements.
 **/

unsigned int  nsp_matrix_elt_size(NspMatrix *M)
{
  return M->rc_type == 'r' ? 
    ((M->convert == 'i' ) ? sizeof(int) 
     : ((M->convert == 'f') ? sizeof(float) : sizeof(double)))
    : 2*sizeof(double);
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
 **/

int nsp_matrix_fill_with(NspMatrix *A,const NspMatrix *B)
{
  switch ( B->rc_type ) 
    {
    case 'r' : 
      if (A->rc_type == 'c') 
	{ 
	  if (nsp_mat_get_real(A) == FAIL) return FAIL; 
	}
      break; 
    case 'c' : 
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
    case 'c' :
      /* C2F(zcopy)(&(A->mn),B->C,&inc,A->C,&inc); */
      memcpy(A->C,B->C, (A->mn)*sizeof(doubleC));
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
      A->m=m;
      A->n=n;
      return(OK);
    }
  if ( m*n < 0) return FAIL;
  if ( m*n == 0 ) /* need to free space */
    {
      A->m =  A->n = A->mn= 0;
      if ( A->rc_type == 'r' ) {FREE(A->R);} else  {FREE(A->C);}
      return OK;
    }

  A->m =m ;  A->n =n;   A->mn=m*n ;
  /* need to realloc */
  switch ( A->rc_type ) 
    {
    case 'r' : 
      A->R=nsp_realloc_doubles(A->R,A->mn);
      if ( A->R == (double *) 0) return(FAIL);
      break ; 
    case 'c' : 
      A->C =nsp_realloc_doubleC(A->C,A->mn);
      if ( A->C == (doubleC *) 0) return(FAIL);
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
    case 'c' : 
      x= A->C[0]; 
      for ( i=1; i < A->mn ; i++) 
	{
	  A->C[i]= x;
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
  FREE(Mat->C) ; /* C,R,I are stored in an union */
  /* free name */
  nsp_object_destroy_name(NSP_OBJECT(Mat));
  FREE(Mat) ;

}

/**
 * nsp_matrix_info:
 * @Mat: a #NspMatrix
 * @indent: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * Display info on the #NspMatrix @A using the default Sciprintf() function. 
 * @indent is the given indentation for printing.
 *
 * Return value: %TRUE or %FALSE
 */

int nsp_matrix_info(NspMatrix *Mat, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  Mat = Mat2double(Mat); /* be sure that mat is back converted to double */

  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( Mat->m >=1 &&  Mat->mn >= 2 ) 
    {
      Sciprintf("%s\t= [...]\t\t%c (%dx%d)\n",pname,Mat->rc_type,Mat->m,Mat->n);
    }
  else 
    {
      /* for scalar we directly give the value */
      nsp_num_formats fmt;
      Sciprintf("%s\t= [ ",pname);
      nsp_init_pr_format (&fmt);
      nsp_matrix_set_format(&fmt,Mat);
      if ( Mat->mn != 0 )
	( Mat->rc_type == 'r') ? nsp_pr_float (&fmt,Mat->R[0]) : nsp_pr_complex (&fmt, Mat->C[0]);
      Sciprintf(" ]\t\t%c (%dx%d)\n",Mat->rc_type,Mat->m,Mat->n);
    }
  return TRUE;
}

/**
 * nsp_matrix_print:
 * @Mat: a #NspMatrix
 * @indent: an int 
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 *
 * Print the #NspMatrix @A using the default nsp output function. 
 * @indent is the given indentation for printing.
 *
 * Return value: %TRUE or %FALSE
 */

static void nsp_matrix_print_as_read_with_slice( NspMatrix *Mat, int indent,const char *name, int rec_level,
						 int slice);

int nsp_matrix_print( NspMatrix *Mat, int indent,const char *name, int rec_level)
{
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  int slice=10000;
  Mat = Mat2double(Mat); /* be sure that mat is back converted to double */

  if (user_pref.pr_as_read_syntax)
    {
      if ( Mat->mn > slice ) 
	{
	  nsp_matrix_print_as_read_with_slice(Mat,indent,pname,rec_level,slice);
	  return rep;
	}
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=%s",pname,(Mat->mn==0 ) ? " []\n" : "" );
	}
      else 
	{
	  Sciprintf1(indent,"%s",(Mat->mn==0 ) ? " []\n" : "" );
	}
    }
  else 
    {
      if ( user_pref.pr_depth  <= rec_level -1 ) 
	{
	  nsp_matrix_info(Mat,indent,pname,rec_level);
	  return rep;
	}
      Sciprintf1(indent,"%s\t=%s\t\t%c (%dx%d)\n",pname,
		(Mat->mn==0 ) ? " []" : "",Mat->rc_type,Mat->m,Mat->n);
    }
  /* now print the values */
  if ( Mat->mn != 0) 
    {
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      if ( Mat->rc_type == 'r') 
	rep = nsp_real_matrix_print_internal (&fmt,Mat,indent);
      else 
	rep = nsp_complex_matrix_print_internal (&fmt,Mat,indent);
    }
  return rep;
}

/* used when matrix is large */

static void nsp_matrix_print_as_read_with_slice( NspMatrix *Mat, int indent,const char *name, int rec_level,
						 int slice)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  int i,init=0;
  nsp_num_formats fmt;
  nsp_init_pr_format (&fmt);
  nsp_matrix_set_format(&fmt,Mat);
  Sciprintf1(indent+1,"x___=[];\n");
  while (1)
    {
      int last = Min(init+slice,Mat->mn );
      Sciprintf1(indent+1,"xx___=[");
      
      if ( Mat->rc_type == 'r') 
	for ( i=init; i < last ; i++) 
	  {
	    Sciprintf1(indent+1,"");
	    nsp_pr_float(&fmt,Mat->R[i]);
	    if ( i != last-1) Sciprintf(";\n");
	  }
      else
	for ( i=init; i < last; i++) 
	  {
	    Sciprintf1(indent+1,"");
	    nsp_pr_complex (&fmt,Mat->C[i]);
	    if ( i != last-1) Sciprintf(";\n");
	  }
      Sciprintf1(indent+1,"];\n");
      Sciprintf1(indent+1,"x___=[x___;xx___];\n");
      init = init+slice;
      if ( init >= Mat->mn) break;
    }
  if ( strcmp(pname,NVOID) != 0) 
    {
      Sciprintf1(indent+1,"%s=matrix(x___,%d,%d);\n",pname,Mat->m,Mat->n);
    }
}


/**
 * nsp_matrix_latex_print:
 * @Mat: a #NspMatrix
 * 
 * print the #NspMatrix @A using the default Sciprintf() function and LaTeX 
 * syntax. 
 *
 * Return value: %TRUE or %FALSE
 */

int nsp_matrix_latex_print(const NspMatrix *Mat)
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
      for (i=0; i < Mat->m; i++)
	{
	  for (j=0; j < Mat->n - 1; j++)
	    { 
	      Sciprintf("%g\t& ",Mat->R[i+j*Mat->m]);
	    }
	  Sciprintf("%g\t",Mat->R[i+(Mat->n-1)*Mat->m]);
	  if ( i != Mat->m -1 ) 
	    Sciprintf("\\\\\n");
	  else 
	    Sciprintf("\n");
	}
      Sciprintf("\\end{array}\\right)}\n");
      if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
    }
  else 
    {
      Sciprintf("Fixme : to be done\n");
    }
  return TRUE;
}

/**
 * nsp_matrix_latex_tab_print:
 * @Mat: a #NspMatrix
 * 
 * print the #NspMatrix @A using the default Sciprintf() function and LaTeX tabular
 * syntax. 
 */

int nsp_matrix_latex_tab_print(const NspMatrix *Mat)
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

      for (i=0; i < Mat->m; i++)
	{
	  Sciprintf("$L_{%d}$\t&",i+1);
	  for (j=0; j < Mat->n - 1; j++)
	    { 
	      Sciprintf("$%g$\t& ",Mat->R[i+j*Mat->m]);
	    }
	  Sciprintf("$%g$\t\\\\ \\hline\n",Mat->R[i+(Mat->n-1)*Mat->m]);
	}
      Sciprintf("\\end{tabular}\n");
      if ( nsp_from_texmacs() == TRUE ) Sciprintf("\005");
    }
  else 
    {
      Sciprintf("Fixme : to be done\n");
    }
  return TRUE;
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
  if ( A->m != B->m && A->mn !=0 ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return(FAIL);
    }
  if ( nsp_matrix_resize(A,B->m,A->n+B->n) == FAIL) return(FAIL);
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
	  /* C2F(zcopy)(&B->mn,B->C,&inc,A->C+Asize,&inc);*/
	  memcpy (A->C+Asize,B->C,sizeof(doubleC)*B->mn);
	}
    }
  else 
    {
      if ( B->rc_type == 'r' )
	nsp_dzcopy(&B->mn,B->R,&inc,A->C+Asize,&inc);
      else
	/* C2F(zcopy)(&B->mn,B->C,&inc,A->C+Asize,&inc); */
	memcpy(A->C+Asize,B->C,sizeof(doubleC)*B->mn);
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
    case 'c' : nsp_csetd(&ns,&d,A->C+Asize,&inc);break;
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
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
  if (( Loc = nsp_matrix_create(NVOID,type,A->m+B->m,A->n)) == NULLMAT)  return(NULLMAT);
  if ( Loc->rc_type == 'r' ) 
    {
      for ( j = 0 ; j < A->n ; j++ ) 
 	{
	  /*
	   * C2F(dcopy) (&A->m,A->R+j*A->m,&inc,Loc->R+j*(Loc->m),&inc);
	   * C2F(dcopy) (&B->m,B->R+j*B->m,&inc,Loc->R+j*(Loc->m)+A->m,&inc);
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
	    nsp_dzcopy(&A->m,A->R+j*A->m,&inc,Loc->C+j*(Loc->m),&inc);
 	  break;
 	case 'c' :
 	  for ( j = 0 ; j < A->n ; j++ ) 
	    /*
	      C2F(zcopy) (&A->m,A->C+j*A->m,&inc,Loc->C+j*(Loc->m),&inc); **/
	    memcpy(Loc->C+j*(Loc->m),A->C+j*A->m,A->m*sizeof(doubleC));
 	  break;
 	}
      switch ( B->rc_type ) 
 	{
 	case 'r' :
 	  for ( j = 0 ; j < A->n ; j++ ) 
	    nsp_dzcopy(&B->m,B->R+j*B->m,&inc,Loc->C+j*(Loc->m)+A->m,&inc);
 	  break;
 	case 'c' :
 	  for ( j = 0 ; j < A->n ; j++ ) 
	    /* 
	       C2F(zcopy) (&B->m,B->C+j*B->m,&inc,Loc->C+j*(Loc->m)+A->m,&inc);
	    */
	    memcpy(Loc->C+j*(Loc->m)+A->m,B->C+j*B->m,B->m*sizeof(doubleC));
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
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) type = 'c';
  if ((Loc = nsp_matrix_create(NVOID,type,A->m+B->m,A->n+B->n))==NULLMAT )
    return NULLMAT;
   
  if ( Loc->rc_type == 'r' ) 
    {
      for ( j = 0 ; j < A->n ; j++ ) 
	{
	  memcpy(Loc->R+j*(Loc->m),A->R+j*A->m,A->m*sizeof(double));
	  nsp_dset(&B->m,&d,Loc->R+j*(Loc->m)+A->m,&inc);
	}
    }
  else 
    {
      if ( A->rc_type == 'c') 
	{
	  for ( j = 0 ; j < A->n ; j++ ) 
	    {
	      memcpy(Loc->C+j*(Loc->m),A->C+j*A->m,A->m*sizeof(doubleC));
	      nsp_csetd(&B->m,&d,Loc->C+j*(Loc->m)+A->m,&inc);
	    }
	}
      else 
	{
	  for ( j = 0 ; j < A->n ; j++ ) 
	    {
	      nsp_dzcopy(&A->m,A->R+j*A->m,&inc,Loc->C+j*(Loc->m),&inc);
	      nsp_csetd(&B->m,&d,Loc->C+j*(Loc->m)+A->m,&inc);
	    }
	}
    }
  if ( Loc->rc_type == 'r' ) 
    {
      for ( j = 0 ; j < B->n ; j++ ) 
	{
	  memcpy(Loc->R+(j+A->n)*(Loc->m)+A->m,B->R+j*A->m,B->m*sizeof(double));
	  nsp_dset(&A->m,&d,Loc->R+(j+A->n)*(Loc->m),&inc);
	}
    }
  else 
    {
      if ( B->rc_type == 'c') 
	{
	  for ( j = 0 ; j < B->n ; j++ ) 
	    {
	      memcpy(Loc->C+(j+A->n)*(Loc->m)+A->m,B->C+j*A->m,B->m*sizeof(doubleC));
	      nsp_csetd(&A->m,&d,Loc->C+(j+A->n)*(Loc->m),&inc);
	    }
	}
      else 
	{
	  for ( j = 0 ; j < B->n ; j++ ) 
	    {
	      nsp_dzcopy(&B->m,B->R+j*A->m,&inc,Loc->C+(j+A->n)*(Loc->m)+A->m,&inc);
	      nsp_csetd(&A->m,&d,Loc->C+(j+A->n)*(Loc->m),&inc);
	    }
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
  if (A->rc_type == 'c' ) 
    {
      for ( j = A->n-1  ; j >= 0 ; j-- ) 
 	{
	  if ( j!= 0) memmove(A->C+j*(A->m),A->C+j*Am,Am*sizeof(doubleC));
	  nsp_csetd(&m,&d,A->C+j*(A->m)+Am,&inc);
 	}
    }
  else 
    {
      for ( j = A->n-1  ; j >= 0 ; j-- ) 
 	{
	  if ( j != 0) memmove(A->R+j*(A->m),A->R+j*Am,Am*sizeof(double));
	  nsp_dset(&m,&d,A->R+j*(A->m)+Am,&inc);
 	}
    }

  return OK;
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
  return nsp_matint_set_submatrix1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols),NSP_OBJECT(B));
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

int nsp_matrix_set_rows(NspMatrix *A, NspMatrix *Rows, NspMatrix *B)
{
  return nsp_matint_set_elts1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(B));
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
 * nsp_matrix_extract:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(Rows,Cols) and returns the new #NspMatrix 
 * 
 * returns a #NspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract(const NspMatrix *A, const NspMatrix *Rows, const NspMatrix *Cols)
{
  return (NspMatrix*) nsp_matint_extract1(NSP_OBJECT(A),NSP_OBJECT(Rows),NSP_OBJECT(Cols));
}

/**
 * nsp_matrix_extract_elements:
 * @A: a #NspMatrix
 * @Elts: a #NspMatrix
 *
 * Compute A(Elts) and returns the new #NspMatrix 
 * 
 * returns a #NspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract_elements(const NspMatrix *A,const NspMatrix *Elts)
{
  return (NspMatrix *) nsp_matint_extract_elements1(NSP_OBJECT(A),NSP_OBJECT(Elts));
}

/**
 * nsp_matrix_extract_columns:
 * @A: a #NspMatrix
 * @Cols: a #NspMatrix
 *
 * Compute A(:,Cols) and returns the new #NspMatrix 
 * 
 * returns a #NspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract_columns(const NspMatrix *A,const NspMatrix *Cols)
{
  return (NspMatrix *) nsp_matint_extract_columns1(NSP_OBJECT(A),NSP_OBJECT(Cols));
}

/**
 * nsp_matrix_extract_rows:
 * @A: a #NspMatrix
 * @Rows: a #NspMatrix
 *
 * Compute A(Rows,:) and returns the new #NspMatrix 
 * 
 * returns a #NspMatrix or %NULLMAT 
 */

NspMatrix *nsp_matrix_extract_rows(const NspMatrix *A,const NspMatrix *Rows)
{
  return (NspMatrix *) nsp_matint_extract_rows1(NSP_OBJECT(A),NSP_OBJECT(Rows));
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
  NspMatrix *Loc;
  if ( icol > A->n )
    {
      *rep = RET_ENDFOR;
      return NULLMAT;
    }
  *rep =0;
  if ( Col == NULLMAT ) 
    Loc = nsp_matrix_create(str,A->rc_type,A->m,1);
  else 
    Loc = Col;
  if ( Loc == NULLMAT) return NULLMAT;

  if ( A->rc_type == 'c' )
    {
      memcpy(Loc->C,A->C+(icol-1)*A->m ,A->m*sizeof(doubleC));
    }
  else
    {
      switch ( A->convert ) 
	{
	case 'u': 
	  Loc->R[0] = A->impl[0]+(icol-1)*A->impl[1];
	  Loc->convert = 'd';
	  break;
	case 'd' : 
	  memcpy(Loc->R,A->R+(icol-1)*A->m ,A->m*sizeof(double));
	  Loc->convert = A->convert;
	  break;
	case 'i': 
	  memcpy(Loc->I,A->I+(icol-1)*A->m ,A->m*sizeof(int));
	  Loc->convert = A->convert;
	  break;
	case 'f': 
	  memcpy(Loc->F,A->F+(icol-1)*A->m ,A->m*sizeof(float));
	  Loc->convert = A->convert;
	  break;
	}
    }
  return Loc;
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
  imax = Min(A->m,A->n -k);
  if ( imin >= imax ) 
    {
      Loc = nsp_matrix_create(NVOID,A->rc_type,0,0);
      return Loc;
    }

  if ( (Loc = nsp_matrix_create(NVOID,A->rc_type,imax-imin,1)) == NULLMAT )
    return NULLMAT;

  if ( A->rc_type == 'c') 
    for ( i = imin, j = 0 ; i < imax ; i++, j++ ) 
      Loc->C[j] = A->C[i+(i+k)*A->m];
  else 
    for ( i = imin, j = 0 ; i < imax ; i++, j++ ) 
      Loc->R[j] = A->R[i+(i+k)*A->m];

  return Loc;
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
	  for ( i = imin ; i < imax ; i++ ) 
	    A->R[i+(i+k)*A->m] = Diag->R[j++] ;
	}
      else 
	{
	  for ( i = imin ; i < imax ; i++ ) 
	    {
	      A->C[i+(i+k)*A->m].r = Diag->R[j++] ;
	      A->C[i+(i+k)*A->m].i = 0.00;
	    }
	}
    }
  else 
    {
      if ( A->rc_type == 'r' ) 
	if (nsp_mat_complexify(A,0.00) == FAIL ) return(FAIL);
      for ( i = imin ; i < imax ; i++ ) 
	{
	  A->C[i+(i+k)*A->m].r = Diag->C[j].r ;
	  A->C[i+(i+k)*A->m].i = Diag->C[j++].i ;
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
    case 'c': nsp_csetd(&Loc->mn,&d,Loc->C,&inc);break;
    }
  if ( Loc->rc_type == 'c') 
    {
      j=0;
      for ( i = imin ; i < imax ; i++ ) 
	{
	  Loc->C[i+(i+k)*Loc->m].i= Diag->C[j].i;
	  Loc->C[i+(i+k)*Loc->m].r= Diag->C[j++].r;
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

  if ( (Loc = nsp_matrix_create(NVOID,A->rc_type,A->n,A->m)) == NULLMAT ) 
    return NULLMAT;

  if ( A->n == 1  ||  A->m == 1 )   /* vector case */
    {
      if ( Loc->rc_type == 'r' )
	{
	  if (A->mn <= 100) 
	    {
	      for ( i = 0 ; i < A->mn ; i++ ) Loc->R[i]= A->R[i];
	    }
	  else 
	    {
	      memcpy(Loc->R, A->R, A->mn*sizeof(double));
	    }
	}
      else
	for ( i = 0 ; i < A->mn ; i++ )
	  {
	    Loc->C[i].r = A->C[i].r;
	    Loc->C[i].i =-A->C[i].i;
	  }
    }
  
  else                             /* matrix case */
    {
      if (Loc->rc_type == 'r') 
	for ( i = 0  ; i < A->m ; i++) 
	  for ( j = 0 ; j < A->n ; j++) 
	    Loc->R[j+Loc->m*i ] = A->R[i+A->m*j];
      else
	for ( i = 0  ; i < A->m ; i++) 
	  for ( j = 0 ; j < A->n ; j++)
	    { 
	      Loc->C[j+Loc->m*i].r = A->C[i+A->m*j].r;
	      Loc->C[j+Loc->m*i].i =-A->C[i+A->m*j].i;
	    }
    }

  return Loc;
}



/**
 * nsp_print_array_double:
 * @indent: an int 
 * @name: %NULL or name to be used. 
 * @val: array of doubles 
 * @size: size of array 
 * @rec_level: deph level of the print.
 *
 * print a double array with the Nsp matrix like 
 * printing mode 
 * 
 * Returns: %TRUE or %FALSE 
 **/
int nsp_print_array_double(int indent, char *name, double *val, int size, int rec_level)
{
  int rep;
  /* int nsp_matrix_print( NspMatrix *Mat, int indent,const char *name, int rec_level) */
  if ( user_pref.pr_depth  <= rec_level -1 ) 
    {
      Sciprintf1(indent,"%s\t=[...]\t\t double[%d]\n",name,size);
      return TRUE;
    }
  Sciprintf1(indent,"%s\t= %s\t\t double[%d]\n",name,(size==0) ? "[]": "", size);
  /* now print the values */
  if ( size != 0) 
    {
      NspMatrix *M= nsp_matrix_create(NVOID,'r',0,0);
      nsp_num_formats fmt;
      nsp_init_pr_format (&fmt);
      M->R = val;
      M->m = 1;
      M->n = size;
      M->mn = size;
      rep = nsp_real_matrix_print_internal (&fmt,M,indent);
      M->R=NULL; M->m=M->n=M->mn =0;
      nsp_matrix_destroy(M);
    }
  return rep;
}

/**
 * nsp_print_latex_array_double:
 * @indent: an int 
 * @name: %NULL or name to be used. 
 * @val: array of doubles 
 * @size: size of array 
 * @rec_level: deph level of the print.
 * 
 * print a double array with the Nsp matrix like 
 * latex printing mode 
 * 
 * Returns: %TRUE or %FALSE 
 **/

int nsp_print_latex_array_double(int indent, char *name, double *val, int size, int rec_level)
{
  int i, j;

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( strcmp(name,NVOID) != 0) 
    Sciprintf("{%s = \\left(\\begin{array}{",name);
  else 
    Sciprintf("{\\left(\\begin{array}{");
  for (i=0; i <  size ;i++) Sciprintf("c");
  Sciprintf("}\n");
  for (j=0; j < size - 1; j++)
    { 
      Sciprintf("%g\t& ",val[j]);
    }
  Sciprintf("\n");
  Sciprintf("\\end{array}\\right)}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}

/* A set of routines used for displaying matrices 
 */

static char nsp_matrix_iter_init(const void *M,int *work)
{
  *work = 0;
  return ( (NspMatrix *) M)->rc_type;
}

static int nsp_matrix_iter_next(const void *M, double *r, doubleC *c,int *work)
{
  if ( *work  == ((NspMatrix *) M)->mn ) return 0;
  switch (((NspMatrix *) M)->rc_type) 
    {
    case 'r' : *r = ((NspMatrix *) M)->R[(*work)++];break;
    case 'c' : *c = ((NspMatrix *) M)->C[(*work)++];break;
    }
  return 1;
}

int nsp_matrix_any_element_is_negative (const void *M)
{
  return gen_any_element_is_negative(M,nsp_matrix_iter_init,nsp_matrix_iter_next);
}

int nsp_matrix_any_element_is_inf_or_nan (const void *M)
{
  return gen_any_element_is_inf_or_nan(M,nsp_matrix_iter_init,nsp_matrix_iter_next);
}

int nsp_matrix_all_elements_are_int_or_inf_or_nan (const void *M)
{
  return gen_all_elements_are_int_or_inf_or_nan (M,nsp_matrix_iter_init,nsp_matrix_iter_next);
}

void nsp_matrix_pr_min_max_internal (const void *M, char flag, double *dmin, double *dmax)
{
  return gen_pr_min_max_internal (M,flag,dmin,dmax,nsp_matrix_iter_init,nsp_matrix_iter_next);
}

/* Matrix case */

void nsp_matrix_set_format(nsp_num_formats *fmt,NspMatrix *M)
{
  gen_set_format(fmt,M,nsp_matrix_any_element_is_negative,
		 nsp_matrix_any_element_is_inf_or_nan,
		 nsp_matrix_pr_min_max_internal,
		 nsp_matrix_all_elements_are_int_or_inf_or_nan,
		 nsp_matrix_iter_init);
}

/*
 * Printing Scilab Matrix 
 *    nsp_print_internalM
 *    the first function are generic functions
 *    used for other Scilab types 
 */

static void nsp_real_matrix_elt_plus_format(const void *m, int i, int j)
{
  const NspMatrix *M=m;
  if (M->R[i+(M->m)*j] == 0.0)
    Sciprintf(" ");
  else
    Sciprintf("+");
}

void nsp_matrix_plus_format(const void *m, int nr, int nc, Mijplus F, int indent)
{
  int i,j;
  for ( i = 0; i < nr; i++)
    {
      for ( j = 0; j < nc; j++)
	{
	  if (j == 0) { nsp_pr_white(indent) ; Sciprintf("| ");}
	  (*F)(m,i,j);
	}
      Sciprintf(" |\n");
    }
}

static void Mij_float(const nsp_num_formats *fmt,const void *m, int i, int j)
{
  const NspMatrix *M=m;
  Sciprintf("  ");
  nsp_pr_float (fmt,M->R[i+(M->m)*j]);
}

/* print matrix elements 
 * return value is TRUE if the print was fully done and FALSE if print 
 * was canceled after answering no to scimore.
 */

int nsp_matrix_general(const nsp_num_formats *fmt,void *m, int nr, int nc, int inc, int total_width, int max_width, int winrows, int indent, Mijfloat F)
{
  int i,j;
  int p_rows=0;
  int col;
  for ( col = 0; col < nc; col += inc)
    {
      int lim = col + inc < nc ? col + inc : nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  int num_cols;
	  if (col != 0)  Sciprintf("\n");
	  nsp_pr_white(indent);
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
	      if ( imore == 1) return FALSE;
	      p_rows=0;
	    }
	  nsp_pr_white(indent);Sciprintf(" |");
	  for ( j = col; j < lim; j++)
	    {
	      (*F)(fmt,m,i,j);
	    }
	  Sciprintf(" |\n");
	}
    }
  return TRUE;
}

int nsp_real_matrix_print_internal(nsp_num_formats *fmt,NspMatrix *m, int indent)
{
  int rep = TRUE;
  int nr = m->m;
  int nc = m->n;
  if ( m->mn == 0) 
    {
      nsp_pr_white (indent); Sciprintf("[]\n");
    }
  else if (fmt->plus_format && ! user_pref.pr_as_read_syntax )
    {
      nsp_matrix_plus_format(m,nr,nc,nsp_real_matrix_elt_plus_format,indent);
    }
  else
    {
      int inc,column_width,total_width;
      int max_width ,winrows, offset;
      sci_get_screen_size(&winrows,&max_width);
      nsp_matrix_set_format(fmt,m);
      /* Sciprintf("prec= %d,Format [%s]\n",
	 user_pref.output_precision,
	 curr_real_fmt); */
      Sciprintf("\n");
      column_width = fmt->curr_real_fw + 2;
      offset =  indent + 4; /* 4 = " |...| " */
      total_width = nc * column_width + offset;
      if (user_pref.pr_as_read_syntax) max_width -= 4;
      if (fmt->free_format)
	{
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("[\n");
	  /* XXXXXX xxxx Sciprintf(m); **/
	  if (user_pref.pr_as_read_syntax)   Sciprintf("];");
	  return rep ;
	}
      inc = nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  inc = (max_width -offset) / column_width;
	  if (inc == 0) inc++;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  nsp_gen_matrix_as_read_syntax(fmt,m,nr,nc,inc,indent,Mij_float);
	}
      else
	{
	  rep= nsp_matrix_general(fmt,m,nr,nc,inc,total_width,max_width,winrows,
				  indent,Mij_float);
	}
    }
  return rep;
}

/*
 * Print any complex Matrix 
 */

static void nsp_complex_matrix_elt_plus_format(const void *m, int i, int j)
{
  const NspMatrix *M=m;
  if (M->C[i+(M->m)*j].r == 0.0 && M->C[i+(M->m)*j].i == 0)
    Sciprintf(" ");
  else
    Sciprintf("+");
}


static void CMij_float(const nsp_num_formats *fmt,const void *m, int i, int j)
{
  const NspMatrix *M=m;
  Sciprintf("  ");
  nsp_pr_complex (fmt, M->C[i+(M->m)*j]);
}

int nsp_complex_matrix_print_internal (nsp_num_formats *fmt,NspMatrix *cm, int indent)
{
  int rep = TRUE;
  int nr = cm->m;
  int nc = cm->n;

  if (cm->mn == 0) 
    {
      Sciprintf("[]\n");
    }
  else if (fmt->plus_format && ! user_pref.pr_as_read_syntax)
    {
      nsp_matrix_plus_format(cm,nr,nc,nsp_complex_matrix_elt_plus_format,indent);
    }
  else
    {
      int column_width,total_width,inc  ;
      int max_width ,winrows ;
      nsp_matrix_set_format(fmt,cm); 
      Sciprintf("\n");
      column_width = fmt->curr_real_fw + fmt->curr_imag_fw;
      column_width += fmt->bank_format ? 2 : 7;
      total_width = nc * column_width;

      sci_get_screen_size(&winrows,&max_width);
      if (user_pref.pr_as_read_syntax)
	max_width -= 4;

      if (fmt->free_format)
	{
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("[\n");
	  /* Sciprintf(cm); xxx **/
	  if (user_pref.pr_as_read_syntax)
	    Sciprintf("];");

	  return rep;
	}

      inc = nc;
      if (total_width > max_width && user_pref.split_long_rows)
	{
	  inc = max_width / column_width;
	  if (inc == 0)
	    inc++;
	}
      if (user_pref.pr_as_read_syntax)
	{
	  nsp_gen_matrix_as_read_syntax(fmt,cm,nr,nc,inc,indent,CMij_float);
	}
      else
	{
	  rep= nsp_matrix_general(fmt,cm,nr,nc,inc,total_width,max_width,winrows,
				  indent,CMij_float);
	}
    }
  return rep;
}











