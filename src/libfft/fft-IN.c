/* 
 * Nsp interface for fft
 * Copyright (C) 2005-2009  Bruno Pincon
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

#include <strings.h>
#include "nsp/interf.h"
#include "nsp/matrix.h"

#ifdef WITH_FFTW3
#include <fftw3.h>
static int hermitian_redundancy_1d_test(NspMatrix *x, int dim);
#else
/* headers for fftpack */
void C2F(zffti)(int *, double *);
void C2F(zfftf)(int *, doubleC *, double *);
void C2F(zfftb)(int *, doubleC *, double *);

/* a routine used only when fftpack is used */
static void transpose_cmplx_mat(NspMatrix *x, NspMatrix *y)
{
  /*        T
   *   y = x    
   */
  int i, j;
  for ( i = 0  ; i < x->m ; i++) 
    for ( j = 0 ; j < x->n ; j++) 
      {
	/* y(j,i) = x(i,j) */
	y->C[j+y->m*i ].r = x->C[i+x->m*j].r;
	y->C[j+y->m*i ].i = x->C[i+x->m*j].i;
      }
}
#endif



#ifdef WITH_FFTW3

NspMatrix *nsp_fft(NspMatrix *x)
{
  NspMatrix *y;
  int i ;
  static fftw_plan forward_plan=NULL, forward_r2c_plan=NULL;
  static int last_forward_n=-1, last_forward_r2c_n=-1;

  if ( (y = nsp_matrix_create(NVOID,'c',x->m,x->n)) == NULLMAT )
    return NULL;
  
  if ( x->rc_type == 'r' )
    {
      if ( x->mn != last_forward_r2c_n )
	{  
	  fftw_destroy_plan(forward_r2c_plan);
	  forward_r2c_plan = fftw_plan_dft_r2c_1d(x->mn, x->R, (fftw_complex *)y->C, 
						  FFTW_ESTIMATE | FFTW_UNALIGNED );
	  last_forward_r2c_n = x->mn;
	}
      fftw_execute_dft_r2c(forward_r2c_plan, x->R , (fftw_complex *)y->C);
      /* complete the vector by hermitian symetry */
      for ( i = 1 ; i < (1+x->mn)/2 ; i++ )
	{
	  y->C[x->mn-i].r = y->C[i].r;
	  y->C[x->mn-i].i =-y->C[i].i;
	}
    }
  else
    {
      if ( x->mn != last_forward_n )
	{  
	  fftw_destroy_plan(forward_plan);
	  forward_plan = fftw_plan_dft_1d(x->mn, (fftw_complex *)x->C, (fftw_complex *)y->C, 
					  -1, FFTW_ESTIMATE | FFTW_UNALIGNED );
	  last_forward_n = x->mn;
	}
      fftw_execute_dft(forward_plan, (fftw_complex *)x->C , (fftw_complex *)y->C);
    }
  return y;
}


NspMatrix *nsp_ifft( NspMatrix *x)
{ 
  NspMatrix *xx=NULLMAT, *y;
  double invn=0;
  int k, dim_flag=0, have_hermitian_redundancy=0;
  static fftw_plan backward_plan=NULL, backward_c2r_plan=NULL;
  static int last_backward_n=-1, last_backward_c2r_n=-1;

  if ( x->rc_type == 'r' )
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT )
	return NULL;
      for ( k = 0 ; k < x->mn ; k++ )
	{
	  xx->C[k].r = x->R[k];
	  xx->C[k].i = 0.0;
	}
      x = xx;
    }

  /* test if x have the hermitian redundancy (if yes the backward transform leads to a 
   * pure real vector (dim_flag=0) or to pure real vectors (dim_flag=1 or 2) 
   */
  have_hermitian_redundancy = hermitian_redundancy_1d_test(x, dim_flag);

  invn = 1.0 / x->mn;

  if ( have_hermitian_redundancy )
    { 
      if ( (y = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	{
	  nsp_matrix_destroy(xx); return NULL;
	}

      if ( x->mn != last_backward_c2r_n )
	{  
	  fftw_destroy_plan(backward_c2r_plan);
	  backward_c2r_plan = fftw_plan_dft_c2r_1d(x->mn, (fftw_complex *)x->C, y->R, 
						   FFTW_ESTIMATE | FFTW_UNALIGNED | FFTW_PRESERVE_INPUT);
	  last_backward_c2r_n = x->mn;
	} 
      fftw_execute_dft_c2r(backward_c2r_plan, (fftw_complex *)x->C , y->R); 
    }
  else
    {
      if ( (y = nsp_matrix_create(NVOID,'c',x->m,x->n)) == NULLMAT )
	{
	  nsp_matrix_destroy(xx); return NULL;
	}

      if ( x->mn != last_backward_n )
	{  
	  fftw_destroy_plan(backward_plan);
	  backward_plan = fftw_plan_dft_1d(x->mn, (fftw_complex *)x->C, (fftw_complex *)y->C, 
					   1, FFTW_ESTIMATE | FFTW_UNALIGNED);
	  last_backward_n = x->mn;
	}
      fftw_execute_dft(backward_plan, (fftw_complex *)x->C , (fftw_complex *)y->C);
    }

  /* apply normalisation */
  if ( have_hermitian_redundancy )
    for (k = 0 ; k < y->mn ; k++) y->R[k] *= invn;
  else
    for (k = 0 ; k < y->mn ; k++) { y->C[k].r *= invn; y->C[k].i *= invn; }

  return y;
}


int int_nsp_fft_deprecated( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = fft(x, forward_backward_flag, row_col_flag)
   *  using the fftw3  lib
   */
  NspMatrix *x;
  double invn=0;
  int s, k, dim_flag=0;
  static fftw_plan forward_plan=NULL, backward_plan=NULL, multi_plan;
  static int last_forward_n=-1, last_backward_n=-1;

  CheckRhs(2,3);
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if (GetScalarInt(stack, 2, &s) == FAIL)
    return RET_BUG;

  if ( s != -1  &&  s != 1 )
    {
      Scierror("%s: second arg must be -1 (forward transform) or 1 (backward transform)\n",NspFname(stack));
      return RET_BUG;
    }

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ( rhs == 3 )
    {
      if ( GetDimArg(stack, 3, &dim_flag) == FAIL )
	return RET_BUG;
      if ( dim_flag == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim_flag == -2 )  /* matlab compatibility flag */
	dim_flag = GiveMatlabDimFlag(x);
    }

  if ((x = GetMatCopy(stack,1)) == NULLMAT) 
    return RET_BUG;

  if ( x->rc_type == 'r' )
    if ( nsp_mat_complexify(x,0.0) == FAIL ) 
      return RET_BUG;

  if ( dim_flag == 0 )
    {
      if ( s == -1 ) /* forward fft */
	{
	  if ( x->mn != last_forward_n )
	    {  
	      fftw_destroy_plan(forward_plan);
	      forward_plan = fftw_plan_dft_1d(x->mn, (fftw_complex *)x->C, (fftw_complex *)x->C, 
					      s, FFTW_ESTIMATE | FFTW_UNALIGNED );
	      last_forward_n = x->mn;
	    }
	  fftw_execute_dft(forward_plan, (fftw_complex *)x->C , (fftw_complex *)x->C);
	}
      else           /* backward fft */
	{
	  if ( x->mn != last_backward_n )
	    {  
	      fftw_destroy_plan(backward_plan);
	      backward_plan = fftw_plan_dft_1d(x->mn, (fftw_complex *)x->C, (fftw_complex *)x->C, 
					       s, FFTW_ESTIMATE | FFTW_UNALIGNED);
	      last_backward_n = x->mn;
	    }
	  fftw_execute_dft(backward_plan, (fftw_complex *)x->C , (fftw_complex *)x->C);
	  invn = 1.0 / x->mn;
	}
    }
  else    /* fft of the rows or the columns */
    {
      if ( dim_flag == 1 )      /* fft of the columns */
	{
	  if ( s == 1) invn = 1.0 / x->m;
	  multi_plan = fftw_plan_many_dft(1, &(x->m), x->n,  
					  (fftw_complex *)x->C, &(x->mn), 1, x->m,
					  (fftw_complex *)x->C, &(x->mn), 1, x->m,
					  s, FFTW_ESTIMATE | FFTW_UNALIGNED );
	}
      else if ( dim_flag == 2 )      /* fft of the rows */
	{
	  if ( s == 1) invn = 1.0 / x->n;
	  multi_plan = fftw_plan_many_dft(1, &(x->n), x->m,  
					  (fftw_complex *)x->C, &(x->mn), x->m, 1,
					  (fftw_complex *)x->C, &(x->mn), x->m, 1,
					  s, FFTW_ESTIMATE | FFTW_UNALIGNED );
	}
      else
	{
	  Scierror("%s: Invalid dim flag '%d' (must be 0, 1 or 2)\n", NspFname(stack), dim_flag);
	  return RET_BUG;
	}

      fftw_execute(multi_plan);
      fftw_destroy_plan(multi_plan);
    }

  if ( s == 1 ) /* backward fft => apply normalisation */
    for (k = 0 ; k < x->mn ; k++) { x->C[k].r *= invn; x->C[k].i *= invn; }
  
  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}



int int_nsp_fftnew( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = fft(x, dimflag)
   *                  = fft(x, dim=dimflag)
   *  using the fftw3  lib
   */
  NspMatrix *x, *y;
  int i, j, dim_flag=0;
  static fftw_plan forward_plan=NULL, forward_r2c_plan=NULL;
  static int last_forward_n=-1, last_forward_r2c_n=-1;
  fftw_plan multi_plan;

  CheckRhs(1,2);
  CheckOptRhs(0, 1)
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if (rhs == 2)
    {
      if ( opt == 0 )
	{
	  if ( GetDimArg(stack, 2, &dim_flag) == FAIL )
	    return RET_BUG;
	}
      else /* opt == 1 */
	{
	  nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &dim_flag) == FAIL )
	    return RET_BUG;
 	}
 
     if ( dim_flag == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim_flag == -2 )  /* matlab compatibility flag */
	dim_flag = GiveMatlabDimFlag(x);
    }

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ( (y = nsp_matrix_create(NVOID,'c',x->m,x->n)) == NULLMAT )
    return RET_BUG;

  if ( dim_flag == 0 )
    {
      if ( x->rc_type == 'r' )
	{
	  if ( x->mn != last_forward_r2c_n )
	    {  
	      fftw_destroy_plan(forward_r2c_plan);
	      forward_r2c_plan = fftw_plan_dft_r2c_1d(x->mn, x->R, (fftw_complex *)y->C, 
						      FFTW_ESTIMATE | FFTW_UNALIGNED );
	      last_forward_r2c_n = x->mn;
	    }
	  fftw_execute_dft_r2c(forward_r2c_plan, x->R , (fftw_complex *)y->C);
	  /* complete the vector by hermitian symetry */
	  for ( i = 1 ; i < (1+x->mn)/2 ; i++ )
	    {
	      y->C[x->mn-i].r = y->C[i].r;
	      y->C[x->mn-i].i =-y->C[i].i;
	    }
	}
      else
	{
	  if ( x->mn != last_forward_n )
	    {  
	      fftw_destroy_plan(forward_plan);
	      forward_plan = fftw_plan_dft_1d(x->mn, (fftw_complex *)x->C, (fftw_complex *)y->C, 
					      -1, FFTW_ESTIMATE | FFTW_UNALIGNED );
	      last_forward_n = x->mn;
	    }
	  fftw_execute_dft(forward_plan, (fftw_complex *)x->C , (fftw_complex *)y->C);
	}
    }
  else    /* fft of the rows or the columns */
    {
      if ( dim_flag == 1 )      /* fft of the columns */
	{
	  if ( x->rc_type == 'r' )
	    {
	      multi_plan = fftw_plan_many_dft_r2c(1, &(x->m), x->n, 
						  x->R, &(x->mn), 
						  1, x->m,
						  (fftw_complex *)y->C, &(y->mn), 
						  1, x->m,
						  FFTW_ESTIMATE | FFTW_UNALIGNED );
	      fftw_execute(multi_plan);
	      /* complete the matrix by hermitian symetry */
	      for ( j = 0 ; j < y->n ; j++ )
		{
		  int stride = j*x->m; 
		  for ( i = 1 ; i < (1+y->m)/2 ; i++ )
		    {
		      y->C[stride+x->m-i].r = y->C[stride+i].r;
		      y->C[stride+x->m-i].i =-y->C[stride+i].i;
		    }
		}
	    }
	  else
	    {
	      multi_plan = fftw_plan_many_dft(1, &(x->m), x->n,  
					      (fftw_complex *)x->C, &(x->mn), 1, x->m,
					      (fftw_complex *)y->C, &(y->mn), 1, y->m,
					      -1, FFTW_ESTIMATE | FFTW_UNALIGNED );
	      fftw_execute(multi_plan);
	    }
	}
      else if ( dim_flag == 2 )      /* fft of the rows */
	{
	  if ( x->rc_type == 'r' )
	    {
	      multi_plan = fftw_plan_many_dft_r2c(1, &(x->n), x->m,  
						  x->R, &(x->mn), x->m, 1,
						  (fftw_complex *)y->C, &(y->mn), x->m, 1,
						  FFTW_ESTIMATE | FFTW_UNALIGNED );
	      fftw_execute(multi_plan);
	      /* complete the matrix by hermitian symetry */
	      for ( j = 1 ; j < (1+y->n)/2 ; j++ )
		{
		  int stride1 = j*y->m, stride2 = (y->n-j)*y->m; 
		  for ( i = 0 ; i < y->m ; i++ )
		    {
		      y->C[stride2+i].r = y->C[stride1+i].r;
		      y->C[stride2+i].i =-y->C[stride1+i].i;
		    }
		}
	    }
	  else
	    {
	      multi_plan = fftw_plan_many_dft(1, &(x->n), x->m,  
					      (fftw_complex *)x->C, &(x->mn), x->m, 1,
					      (fftw_complex *)y->C, &(y->mn), x->m, 1,
					      -1, FFTW_ESTIMATE | FFTW_UNALIGNED );
	      fftw_execute(multi_plan);
	    }
	}
      else    /* normaly we shouldn't branch here */
	{
	  Scierror("%s: Invalid dim flag '%d' (must be 0, 1 or 2)\n", NspFname(stack), dim_flag);
	  nsp_matrix_destroy(y);
	  return RET_BUG;
	}

      fftw_destroy_plan(multi_plan);
    }

  MoveObj (stack, 1, (NspObject *) y);
  return 1;
}


static int hermitian_redundancy_1d_test(NspMatrix *x, int dim)
{
  int flag = 0;
  double delta, tol;

  if ( dim == 0 )
    {
      int k;
      tol = 10*sqrt(x->mn)*DBL_EPSILON;
      flag = fabs(x->C[0].i)  <= tol*fabs(x->C[0].r);
      for ( k = 1 ; k <= x->mn/2 && flag ; k++ )
	{
	  delta = fabs(x->C[x->mn-k].r - x->C[k].r) + fabs(x->C[x->mn-k].i + x->C[k].i);
	  flag = delta <= tol*( fabs(x->C[k].r) + fabs(x->C[k].i) );
	}
    }

  else if ( dim == 1 )
    {
      int i, j;
      doubleC *X;
      tol = 10*sqrt(x->m)*DBL_EPSILON;
      flag = 1;
      for ( j = 0, X = x->C ; j < x->n  && flag ; j++, X+=x->m )
	{
	  flag =  fabs(X[0].i) <= tol*fabs(X[0].r);
	  for ( i = 1 ; i <= x->m/2 && flag ; i++ )
	    {
	      delta = fabs(X[x->m-i].r - X[i].r) + fabs(X[x->m-i].i + X[i].i);
	      flag = delta <= tol*(fabs(X[i].r) + fabs(X[i].i));
	    }
	}
    }

  else if ( dim == 2 )
    {
      int i, j, stride1, stride2;
      tol = 10*sqrt(x->n)*DBL_EPSILON;
      flag = 1;
      // first column must be real
      for ( i = 0 ; i < x->m  && flag ; i++ )
	flag = fabs(x->C[i].i)  <= tol*fabs(x->C[i].r);

      for ( j = 1, stride1 = x->m, stride2 = x->m*(x->n-1) ; j <= x->n/2  && flag ; j++, stride1+=x->m, stride2-=x->m)
	for ( i = 0 ; i < x->m && flag ; i++ )
	  {
	    delta = fabs(x->C[stride2+i].r - x->C[stride1+i].r) + fabs(x->C[stride2+i].i + x->C[stride1+i].i); 
	    flag = delta <= tol*(fabs(x->C[stride1+i].r) + fabs( x->C[stride1+i].i));
	  }
    }

  return flag;
}

int int_nsp_ifftnew( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = ifft(x, dimflag)
   *                  = ifft(x, dim=dimflag)
   *  using the fftw3  lib
   */
  NspMatrix *x, *xx=NULLMAT, *y;
  double invn=0;
  int k, dim_flag=0, have_hermitian_redundancy=0;
  static fftw_plan backward_plan=NULL, backward_c2r_plan=NULL;
  static int last_backward_n=-1, last_backward_c2r_n=-1;
  fftw_plan multi_plan;

  CheckRhs(1,2);
  CheckOptRhs(0, 1)
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if (rhs == 2)
    {
      if ( opt == 0 )
	{
	  if ( GetDimArg(stack, 2, &dim_flag) == FAIL )
	    return RET_BUG;
	}
      else /* opt == 1 */
	{
	  nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &dim_flag) == FAIL )
	    return RET_BUG;
 	}
 
     if ( dim_flag == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim_flag == -2 )  /* matlab compatibility flag */
	dim_flag = GiveMatlabDimFlag(x);
    }

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ( x->rc_type == 'r' )
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT )
	return RET_BUG;
      for ( k = 0 ; k < x->mn ; k++ )
	{
	  xx->C[k].r = x->R[k];
	  xx->C[k].i = 0.0;
	}
      x = xx;
    }

  /* test if x have the hermitian redundancy (if yes the backward transform leads to a 
   * pure real vector (dim_flag=0) or to pure real vectors (dim_flag=1 or 2) 
   */
  have_hermitian_redundancy = hermitian_redundancy_1d_test(x, dim_flag);

  if ( dim_flag == 0 )
    {
      invn = 1.0 / x->mn;

      if ( have_hermitian_redundancy )
	{ 
	  if ( (y = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	    {
	      nsp_matrix_destroy(xx); return RET_BUG;
	    }

	  if ( x->mn != last_backward_c2r_n )
	    {  
	      fftw_destroy_plan(backward_c2r_plan);
	      backward_c2r_plan = fftw_plan_dft_c2r_1d(x->mn, (fftw_complex *)x->C, y->R, 
						       FFTW_ESTIMATE | FFTW_UNALIGNED | FFTW_PRESERVE_INPUT);
	      last_backward_c2r_n = x->mn;
	    } 
	  fftw_execute_dft_c2r(backward_c2r_plan, (fftw_complex *)x->C , y->R); 
	}
      else
	{
	  if ( (y = nsp_matrix_create(NVOID,'c',x->m,x->n)) == NULLMAT )
	    {
	      nsp_matrix_destroy(xx); return RET_BUG;
	    }

	  if ( x->mn != last_backward_n )
	    {  
	      fftw_destroy_plan(backward_plan);
	      backward_plan = fftw_plan_dft_1d(x->mn, (fftw_complex *)x->C, (fftw_complex *)y->C, 
					       1, FFTW_ESTIMATE | FFTW_UNALIGNED);
	      last_backward_n = x->mn;
	    }
	  fftw_execute_dft(backward_plan, (fftw_complex *)x->C , (fftw_complex *)y->C);
	}
    }
  else    /* ifft of the rows or the columns */
    {
      if ( dim_flag == 1 )        /* ifft of the columns */
	{
	  invn = 1.0 / x->m;

	  if ( have_hermitian_redundancy )
	    { 
	      if ( (y = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
		{
		  nsp_matrix_destroy(xx); return RET_BUG;
		}
	      multi_plan = fftw_plan_many_dft_c2r(1, &(x->m), x->n,  
						  (fftw_complex *)x->C, &(x->mn), 1, x->m,
						  y->R, &(y->mn), 1, y->m,
						  FFTW_ESTIMATE | FFTW_UNALIGNED | FFTW_PRESERVE_INPUT);
	    }
	  else
	    {
	      if ( (y = nsp_matrix_create(NVOID,'c',x->m,x->n)) == NULLMAT )
		{
		  nsp_matrix_destroy(xx); return RET_BUG;
		}
	      multi_plan = fftw_plan_many_dft(1, &(x->m), x->n,  
					      (fftw_complex *)x->C, &(x->mn), 1, x->m,
					      (fftw_complex *)y->C, &(y->mn), 1, y->m,
					      1, FFTW_ESTIMATE | FFTW_UNALIGNED );
	    }
	}
      else if ( dim_flag == 2 )   /* ifft of the rows */
	{
	  invn = 1.0 / x->n;

	  if ( have_hermitian_redundancy )
	    { 
	      if ( (y = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
		{
		  nsp_matrix_destroy(xx); return RET_BUG;
		}
	      multi_plan = fftw_plan_many_dft_c2r(1, &(x->n), x->m,  
						  (fftw_complex *)x->C, &(x->mn), x->m, 1,
						  y->R, &(y->mn), x->m, 1,
						  FFTW_ESTIMATE | FFTW_UNALIGNED | FFTW_PRESERVE_INPUT);
	    }
	  else
	    {
	      if ( (y = nsp_matrix_create(NVOID,'c',x->m,x->n)) == NULLMAT )
		{
		  nsp_matrix_destroy(xx); return RET_BUG;
		}
	      multi_plan = fftw_plan_many_dft(1, &(x->n), x->m,  
					      (fftw_complex *)x->C, &(x->mn), x->m, 1,
					      (fftw_complex *)y->C, &(y->mn), x->m, 1,
					      1, FFTW_ESTIMATE | FFTW_UNALIGNED );
	    }
	}
      else                        /* normaly we shouldn't branch here */
	{
	  Scierror("%s: Invalid dim flag '%d' (must be 0, 1 or 2)\n", NspFname(stack), dim_flag);
	  nsp_matrix_destroy(xx);
	  return RET_BUG;
	}
      
      fftw_execute(multi_plan);
      fftw_destroy_plan(multi_plan);
    }

  nsp_matrix_destroy(xx);

  /* apply normalisation */
  if ( have_hermitian_redundancy )
    for (k = 0 ; k < y->mn ; k++) y->R[k] *= invn;
  else
    for (k = 0 ; k < y->mn ; k++) { y->C[k].r *= invn; y->C[k].i *= invn; }
  
  MoveObj (stack, 1, (NspObject *) y);
  return 1;
}


int int_nsp_fft2_deprecated( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  interface for y = fft2(x, flag)
   *  using the fftw3 lib
   */
  NspMatrix *x;
  double invn;
  int s, k;
  fftw_plan p;

  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if (GetScalarInt(stack, 2, &s) == FAIL)
    return RET_BUG;

  if ( s != -1  &&  s != 1 )
    {
      Scierror("%s: second arg must be -1 (forward transform) or 1 (backward transform)",NspFname(stack));
      return RET_BUG;
    }

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ((x = GetMatCopy(stack,1)) == NULLMAT) 
    return RET_BUG;

  if ( x->rc_type == 'r' )
    if (nsp_mat_complexify(x,0.00) == FAIL ) 
      return RET_BUG;

  p = fftw_plan_dft_2d(x->n, x->m, (fftw_complex *)x->C, (fftw_complex *)x->C, s, FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);

  if (s == 1)     /* apply normalisation */
    {
      invn = 1.0 / x->mn;
      for (k = 0 ; k < x->mn ; k++) { x->C[k].r *= invn; x->C[k].i *= invn; }
    }

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}


int int_nsp_fft2new( Stack stack, int rhs, int opt, int lhs)
{
  /*  interface for y = fft2new(x)
   *  using the fftw3 lib
   */
  NspMatrix *x, *y;
  fftw_plan p;

  CheckRhs(1,1);
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT)
    return RET_BUG;

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ( ( y = nsp_matrix_create(NVOID, 'c', x->m, x->n) ) == NULLMAT )
    return RET_BUG;

  if ( x->rc_type == 'r' )
    {
      if ( (p = fftw_plan_dft_r2c_2d(x->n, x->m, x->R, (fftw_complex *)y->C, FFTW_ESTIMATE)) == NULL )
	goto err;
    }
  else
    {
      if( (p = fftw_plan_dft_2d(x->n, x->m, (fftw_complex *)x->C, (fftw_complex *)y->C, -1, FFTW_ESTIMATE)) == NULL )
	goto err;
    }

  fftw_execute(p);
  fftw_destroy_plan(p);

  if ( x->rc_type == 'r' )  /* complete y by applying hermitian redundancy  */
    {
      /* but we must move some blocs first... */
      int i, j, stride1, stride2, sizebloc = x->m/2 + 1;
      int sztblc =  sizebloc*sizeof(doubleC);
      doubleC *from, *to;
      for ( j = x->n-1, from = &(y->C[sizebloc*(x->n-1)]), to = &(y->C[x->m*(x->n-1)])  ; j > 1 ; j--, from-=sizebloc, to-=x->m )
	memcpy(to, from, sztblc);
      memmove(to, from, sztblc);

      /* now complete by hermitian redundancy:   
       *      y[m-i,0] = conj(y[i, 0])   for i = 1..(x->m-1)/2 
       * and  y[m-i,j] = conj(y[i, n-j]) for j = 1..y->n  and i = 1..(x->m-1)/2 
       */
      for ( i = 1 ; i < (x->m+1)/2 ; i++ )
	{
	  y->C[x->m - i].r = y->C[i].r;
	  y->C[x->m - i].i =-y->C[i].i;
	}
      for ( j = 1, stride1 = 2*x->m, stride2 = x->mn-x->m ; j < x->n ; j++, stride1+=x->m, stride2-=x->m )
	for ( i = 1 ; i < (x->m+1)/2 ; i++ )
	  {
	    y->C[stride1 - i].r = y->C[stride2 + i].r;
	    y->C[stride1 - i].i =-y->C[stride2 + i].i;
	  }
    }

  MoveObj (stack, 1, (NspObject *) y);
  return 1;

 err:
  nsp_matrix_destroy(y);
  Scierror ("Error:\t fftw plan not initialised in %s\n", NspFname(stack));
  return RET_BUG;
}

static int hermitian_redundancy_2d_test(NspMatrix *x)
{
  int i, j, stride1, stride2, flag, m = x->m, n = x->n;
  double delta, tol = 10*Max(m,n)*DBL_EPSILON;  /* FIXME: to be set more precisely */

  flag = 1;

  /* x[0,0] should be real */
  flag = fabs(x->C[0].i) <=  tol*fabs(x->C[0].r); 
  
  /* x[m-i,0] == conj(x[i, 0]) ? for  i = 1..m/2 */  
  for ( i = 1 ; i <= m/2 && flag ; i++ )
    {
      delta = fabs( x->C[m - i].r - x->C[i].r ) + fabs( x->C[m - i].i + x->C[i].i );
      flag =  delta <= tol*( fabs(x->C[i].r) + fabs(x->C[i].i) );
    }

  /* x[0,n-j] == conj(x[0, j]) ? for j = 1..n/2 */  
  for ( j = 1 ; j <= n/2 && flag ; j++ )
    {
      delta = fabs(x->C[m*(n-j)].r - x->C[m*j].r) + fabs(x->C[m*(n-j)].i + x->C[m*j].i);
      flag =  delta <= tol*( fabs(x->C[m*j].r) + fabs(x->C[m*j].i) );
    }

  /* x[m-i,j] == conj(x[i, j])  for j=1..n-1 and i=1..m/2 */
  for ( j = 1, stride1 = 2*m, stride2 = m*(n-1) ; j < n ; j++, stride1+=m, stride2-=m )
    for ( i = 1 ; i <= m/2  && flag ; i++ )
      {
	delta = fabs(x->C[stride1-i].r - x->C[stride2+i].r) + fabs(x->C[stride1-i].i + x->C[stride2+i].i);
	flag = delta <= tol*( fabs(x->C[stride2 + i].r) + fabs(x->C[stride2 + i].i) );
      }

  return flag;
}

int int_nsp_ifft2new( Stack stack, int rhs, int opt, int lhs)
{
  /*  interface for y = ifft2new(x)
   *  using the fftw3 lib
   */
  NspMatrix *x, *xx=NULLMAT, *y=NULLMAT;
  doubleC *halfx=NULL;
  fftw_plan p;
  int k;
  double invn;

  CheckRhs(1,1);
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT)
    return RET_BUG;

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ( x->rc_type == 'r' )
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT )
	return RET_BUG;
      for ( k = 0 ; k < x->mn ; k++ )
	{
	  xx->C[k].r = x->R[k]; xx->C[k].i = 0.0;
	}
      x = xx;
    }

  if ( hermitian_redundancy_2d_test(x) )
    {
      int j, sizebloc = x->m/2 + 1, sztblc =  sizebloc*sizeof(doubleC);
      doubleC *from, *to;
      
      /* allocate halfx then copy the non redundant part of x in halfx  */
      if ( ( halfx = malloc(x->n *sztblc) ) == NULL )
	{
	  Scierror ("Error:\t running out memory in %s\n", NspFname(stack));
 	  goto err;
	}
      for ( j = 0, from = x->C, to = halfx ; j < x->n ; j++, from+=x->m, to+=sizebloc )
	memcpy(to, from, sztblc);

      if ( ( y = nsp_matrix_create(NVOID, 'r', x->m, x->n) ) == NULLMAT ) goto err;

      /* compute the c2r plan */
      if ( (p = fftw_plan_dft_c2r_2d(x->n, x->m, (fftw_complex *)halfx, y->R, FFTW_ESTIMATE)) == NULL ) 
	{
	  Scierror ("Error:\t fftw plan not initialised in %s\n", NspFname(stack));
	  goto err;
	}
    }
  else
    {
      if ( ( y = nsp_matrix_create(NVOID, 'c', x->m, x->n) ) == NULLMAT ) goto err;

      if ( (p = fftw_plan_dft_2d(x->n, x->m, (fftw_complex *)x->C, (fftw_complex *)y->C, 1, FFTW_ESTIMATE)) == NULL ) 
	{
	  Scierror ("Error:\t fftw plan not initialised in %s\n", NspFname(stack));
	  goto err;
	}
    }

  fftw_execute(p);
  fftw_destroy_plan(p);

  /* apply normalisation */
  invn = 1.0 / y->mn;
  if ( y->rc_type == 'r' )
    {
      for ( k = 0 ; k < y->mn ; k++ ) y->R[k] *= invn;
      free(halfx);
    }
  else
    for ( k = 0 ; k < y->mn ; k++ ) { y->C[k].r *= invn; y->C[k].i *= invn; }

  nsp_matrix_destroy(xx);
  MoveObj (stack, 1, (NspObject *) y);
  return 1;

 err:
  free(halfx);
  nsp_matrix_destroy(xx);
  nsp_matrix_destroy(y);
  return RET_BUG;
}

#else


NspMatrix *nsp_fftnew(Nspmatrix *x)
{ 
  /*  using the fftpack lib   */
  NspMatrix *xx, *y;
  int k, n, dim_flag=0;
  static double *wsave = NULL;
  static int last_n=-1;

  n = x->mn;
  
  if ( x->rc_type == 'c' )
    {
      if ( (xx = nsp_matrix_copy(x)) == NULLMAT ) 
	return RET_BUG;
    }
  else
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT ) 
	return RET_BUG;
      for ( k = 0 ; k < x->mn ; k++ ) { xx->C[k].r = x->R[k]; xx->C[k].i = 0.0; }
    }

  if ( n != last_n )
    {
      free(wsave);
      if ( (wsave = malloc((4*n+15)*sizeof(double))) == NULL )
	goto err;
      C2F(zffti)(&n, wsave);
      last_n = n;
    }

  C2F(zfftf)(&n, xx->C, wsave);
  return xx;
 err:
  nsp_matrix_destroy(xx);
  return NULL;
}


NspMatrix *int_nsp_ifftnew(NspMatrix *x)
{ 
  /*  using the fftpack lib  */
  NspMatrix *xx, *y;
  int k, n; 
  static double *wsave = NULL, invn;
  static int last_n=-1;

  n = x->mn;
  
  if ( x->rc_type == 'c' )
    {
      if ( (xx = nsp_matrix_copy(x)) == NULLMAT ) 
	return RET_BUG;
    }
  else
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT ) 
	return RET_BUG;
      for ( k = 0 ; k < x->mn ; k++ ) { xx->C[k].r = x->R[k]; xx->C[k].i = 0.0; }
    }

  if ( n != last_n )
    {
      free(wsave);
      if ( (wsave = malloc((4*n+15)*sizeof(double))) == NULL )
	goto err;
      C2F(zffti)(&n, wsave);
      last_n = n;
      invn = 1.0 / (double) n;
    }

  C2F(zfftb)(&n, xx->C, wsave);
  for (k = 0 ; k < x->mn ; k++) { xx->C[k].r *= invn; xx->C[k].i *= invn; }
  return xx;
 err:
  nsp_matrix_destroy(xx);
  return NULL;
}



int int_nsp_fft_deprecated( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = fft(x, forward_backward_flag, row_col_flag)
   *  using the fftpack lib
   */
  enum {ALL, BY_ROW, BY_COL};
  NspMatrix *x, *y;
  int s, k, dim_flag=0;
  static double *wsave = NULL, invn;
  static int last_n=-1, n, flag_orient;

  CheckRhs(2,3);
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if (GetScalarInt(stack, 2, &s) == FAIL)
    return RET_BUG;

  if ( s != -1  &&  s != 1 )
    {
      Scierror("%s: second arg must be -1 (forward transform) or 1 (backward transform)\n",NspFname(stack));
      return RET_BUG;
    }

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ( rhs == 3 )
    {
      if ( GetDimArg(stack, 3, &dim_flag) == FAIL )
	return RET_BUG;
      if ( dim_flag == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim_flag == -2 )  /* matlab compatibility flag */
	dim_flag = GiveMatlabDimFlag(x);
    }

  if ((x = GetMatCopy(stack,1)) == NULLMAT) 
    return RET_BUG;

  if ( x->rc_type == 'r' )
    if (nsp_mat_complexify(x,0.00) == FAIL ) 
      return RET_BUG;

  if ( dim_flag == 0 )
    {
      n = x->mn;
      flag_orient = ALL;
    }
  else if ( dim_flag == 1 )
    {
      n = x->m;
      flag_orient = BY_COL;
    }
  else if ( dim_flag == 2 )
    {
      n = x->n;
      flag_orient = BY_ROW;
    }
  else
    {
      Scierror("%s: Invalid dim flag '%d' (must be 0, 1 or 2)\n", NspFname(stack), dim_flag);
      return RET_BUG;
    }

  
  if ( n != last_n )
    {
      free(wsave);
      if ( (wsave = malloc((4*n+15)*sizeof(double))) == NULL )
	return RET_BUG;
      C2F(zffti)(&n, wsave);
      last_n = n;
      invn = 1.0 / (double) n;
    }

  
  if ( flag_orient == ALL || (flag_orient == BY_ROW && x->m == 1) )
    {  
      if ( s == -1 )
	C2F(zfftf)(&n, x->C, wsave);
      else
	C2F(zfftb)(&n, x->C, wsave);
    }
  else if ( flag_orient == BY_COL )
    {
      if ( s == -1 )
	for ( k = 0 ; k < x->n ; k++ )
	  C2F(zfftf)(&n, &(x->C[k*x->m]), wsave);
      else
	for ( k = 0 ; k < x->n ; k++ )
	  C2F(zfftb)(&n, &(x->C[k*x->m]), wsave);
    }
  else if ( flag_orient == BY_ROW )
    {
      if ( (y = nsp_matrix_create(NVOID, 'c', x->n, x->m)) == NULLMAT )
	return RET_BUG;
      transpose_cmplx_mat(x, y);
      if ( s == -1 )
	for ( k = 0 ; k < y->n ; k++ )
	  C2F(zfftf)(&n, &(y->C[k*y->m]), wsave);
      else
	for ( k = 0 ; k < y->n ; k++ )
	  C2F(zfftb)(&n, &(y->C[k*y->m]), wsave);
      transpose_cmplx_mat(y, x);
      nsp_matrix_destroy(y);
    }

  if ( s == 1 ) /* backward fft => apply normalisation */
    for (k = 0 ; k < x->mn ; k++) { x->C[k].r *= invn; x->C[k].i *= invn; }

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}


int int_nsp_fftnew( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = fftnew(x row_col_flag)
   *  using the fftpack lib
   */
  NspMatrix *x, *xx, *y;
  int k, n, dim_flag=0;
  static double *wsave = NULL;
  static int last_n=-1;

  CheckRhs(1,2);
  CheckOptRhs(0, 1)
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if (rhs == 2)
    {
      if ( opt == 0 )
	{
	  if ( GetDimArg(stack, 2, &dim_flag) == FAIL )
	    return RET_BUG;
	}
      else /* opt == 1 */
	{
	  nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &dim_flag) == FAIL )
	    return RET_BUG;
 	}
 
     if ( dim_flag == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim_flag == -2 )  /* matlab compatibility flag */
	dim_flag = GiveMatlabDimFlag(x);
    }

  if ( dim_flag == 0 )
    n = x->mn;
  else if ( dim_flag == 1 )
    n = x->m;
  else if ( dim_flag == 2 )
    n = x->n;
  else
    {
      Scierror ("Error:\t bad value for dim flag in %s\n", NspFname(stack));
      return RET_BUG;
    }

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }


  if ( x->rc_type == 'c' )
    {
      if ( (xx = nsp_matrix_copy(x)) == NULLMAT ) 
	return RET_BUG;
    }
  else
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT ) 
	return RET_BUG;
      for ( k = 0 ; k < x->mn ; k++ ) { xx->C[k].r = x->R[k]; xx->C[k].i = 0.0; }
    }

  if ( n != last_n )
    {
      free(wsave);
      if ( (wsave = malloc((4*n+15)*sizeof(double))) == NULL )
	goto err;
      C2F(zffti)(&n, wsave);
      last_n = n;
    }

  if ( dim_flag == 0 )
    C2F(zfftf)(&n, xx->C, wsave);
  else if ( dim_flag == 1 )
    for ( k = 0 ; k < xx->n ; k++ )
      C2F(zfftf)(&n, &(xx->C[k*x->m]), wsave);
  else /* dim_flag == 2 */
    {
      if ( (y = nsp_matrix_create(NVOID, 'c', xx->n, xx->m)) == NULLMAT )
	goto err;
      transpose_cmplx_mat(xx, y);
      for ( k = 0 ; k < y->n ; k++ )
	C2F(zfftf)(&n, &(y->C[k*y->m]), wsave);
      transpose_cmplx_mat(y, xx);
      nsp_matrix_destroy(y);
    }

  MoveObj (stack, 1, (NspObject *) xx);
  return 1;

 err:
  nsp_matrix_destroy(xx);
  return RET_BUG;
}


int int_nsp_ifftnew( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = ifftnew(x, dim_flag)
   *  using the fftpack lib
   */
  NspMatrix *x, *xx, *y;
  int k, n, dim_flag=0;
  static double *wsave = NULL, invn;
  static int last_n=-1;

  CheckRhs(1,2);
  CheckOptRhs(0, 1)
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if (rhs == 2)
    {
      if ( opt == 0 )
	{
	  if ( GetDimArg(stack, 2, &dim_flag) == FAIL )
	    return RET_BUG;
	}
      else /* opt == 1 */
	{
	  nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &dim_flag) == FAIL )
	    return RET_BUG;
 	}
 
     if ( dim_flag == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim_flag == -2 )  /* matlab compatibility flag */
	dim_flag = GiveMatlabDimFlag(x);
    }

  if ( dim_flag == 0 )
    n = x->mn;
  else if ( dim_flag == 1 )
    n = x->m;
  else if ( dim_flag == 2 )
    n = x->n;
  else
    {
      Scierror ("Error:\t bad value for dim flag in %s\n", NspFname(stack));
      return RET_BUG;
    }

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }


  if ( x->rc_type == 'c' )
    {
      if ( (xx = nsp_matrix_copy(x)) == NULLMAT ) 
	return RET_BUG;
    }
  else
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT ) 
	return RET_BUG;
      for ( k = 0 ; k < x->mn ; k++ ) { xx->C[k].r = x->R[k]; xx->C[k].i = 0.0; }
    }

  if ( n != last_n )
    {
      free(wsave);
      if ( (wsave = malloc((4*n+15)*sizeof(double))) == NULL )
	goto err;
      C2F(zffti)(&n, wsave);
      last_n = n;
      invn = 1.0 / (double) n;
    }

  if ( dim_flag == 0 )
    C2F(zfftb)(&n, xx->C, wsave);
  else if ( dim_flag == 1 )
    for ( k = 0 ; k < xx->n ; k++ )
      C2F(zfftb)(&n, &(xx->C[k*x->m]), wsave);
  else /* dim_flag == 2 */
    {
      if ( (y = nsp_matrix_create(NVOID, 'c', xx->n, xx->m)) == NULLMAT )
	goto err;
      transpose_cmplx_mat(xx, y);
      for ( k = 0 ; k < y->n ; k++ )
	C2F(zfftb)(&n, &(y->C[k*y->m]), wsave);
      transpose_cmplx_mat(y, xx);
      nsp_matrix_destroy(y);
    }

  for (k = 0 ; k < x->mn ; k++) { xx->C[k].r *= invn; xx->C[k].i *= invn; }

  MoveObj (stack, 1, (NspObject *) xx);
  return 1;

 err:
  nsp_matrix_destroy(xx);
  return RET_BUG;
}

int int_nsp_fft2_deprecated( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = fft2_bis(x, forward_backward_flag)
   *  using the fftpack lib
   */
  NspMatrix *x, *y;
  int s, k;
  double *wsave;
  double invn;

  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if (GetScalarInt(stack, 2, &s) == FAIL)
    return RET_BUG;

  if ( s != -1  &&  s != 1 )
    {
      Scierror("%s: second arg must be -1 (forward transform) or 1 (backward transform)\n",NspFname(stack));
      return RET_BUG;
    }

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ((x = GetMatCopy(stack,1)) == NULLMAT) 
    return RET_BUG;

  if ( x->rc_type == 'r' )
    if (nsp_mat_set_ival(x,0.00) == FAIL ) 
      return RET_BUG;

  if ( (wsave = malloc((4*x->m+15)*sizeof(double))) == NULL )
    return RET_BUG;
  C2F(zffti)(&(x->m), wsave);

  if ( s == -1 )
    for ( k = 0 ; k < x->n ; k++ )
      C2F(zfftf)(&(x->m), &(x->C[k*x->m]), wsave);
  else
    for ( k = 0 ; k < x->n ; k++ )
      C2F(zfftb)(&(x->m), &(x->C[k*x->m]), wsave);

  if ( x->n != x->m )
    {
      free(wsave);
      if ( (wsave = malloc((4*x->n+15)*sizeof(double))) == NULL )
	return RET_BUG;
      C2F(zffti)(&(x->n), wsave);
    }

  if ( (y = nsp_matrix_create(NVOID, 'c', x->n, x->m)) == NULLMAT )
    {
      free(wsave);
      return RET_BUG;
    }
  transpose_cmplx_mat(x, y);

  if ( s == -1 )
    for ( k = 0 ; k < y->n ; k++ )
      C2F(zfftf)(&(y->m), &(y->C[k*y->m]), wsave);
  else
    for ( k = 0 ; k < y->n ; k++ )
      C2F(zfftb)(&(y->m), &(y->C[k*y->m]), wsave);
  transpose_cmplx_mat(y, x);
  free(wsave);
  nsp_matrix_destroy(y);

  if (s == 1)     /* apply normalisation */
    {
      invn = 1.0 / x->mn;
      for (k = 0 ; k < x->mn ; k++) { x->C[k].r *= invn; x->C[k].i *= invn; }
    }

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}


int int_nsp_fft2new( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = fft2new(x)
   *  using the fftpack lib
   */
  NspMatrix *x, *xx, *y=NULLMAT;
  int k;
  double *wsave=NULL;

  CheckRhs(1,1);
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ( x->rc_type == 'c' )
    {
      if ( (xx = nsp_matrix_copy(x)) == NULLMAT ) 
	return RET_BUG;
    }
  else
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT ) 
	return RET_BUG;
      for ( k = 0 ; k < x->mn ; k++ ) { xx->C[k].r = x->R[k]; xx->C[k].i = 0.0; }
    }


  if ( (wsave = malloc((4*x->m+15)*sizeof(double))) == NULL )
    goto err;
  C2F(zffti)(&(xx->m), wsave);

  for ( k = 0 ; k < x->n ; k++ )
    C2F(zfftf)(&(xx->m), &(xx->C[k*x->m]), wsave);

  if ( xx->n != xx->m )
    {
      free(wsave);
      if ( (wsave = malloc((4*xx->n+15)*sizeof(double))) == NULL )
	goto err;
      C2F(zffti)(&(xx->n), wsave);
    }

  if ( (y = nsp_matrix_create(NVOID, 'c', xx->n, xx->m)) == NULLMAT )
    goto err;

  transpose_cmplx_mat(xx, y);
  for ( k = 0 ; k < y->n ; k++ )
    C2F(zfftf)(&(y->m), &(y->C[k*y->m]), wsave);
  transpose_cmplx_mat(y, xx);

  free(wsave);
  nsp_matrix_destroy(y);

  MoveObj (stack, 1, (NspObject *) xx);
  return 1;

 err:
  free(wsave);
  nsp_matrix_destroy(xx);
  return RET_BUG;
}


int int_nsp_ifft2new( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = ifft2new(x)
   *  using the fftpack lib
   */
  NspMatrix *x, *xx, *y=NULLMAT;
  int k;
  double *wsave=NULL;
  double invn;

  CheckRhs(1,1);
  CheckLhs(1,1);

  if ((x = GetMat(stack,1)) == NULLMAT) 
    return RET_BUG;

  if ( x->mn == 0 )
    {
      NSP_OBJECT (x)->ret_pos = 1;
      return 1;
    }

  if ( x->rc_type == 'c' )
    {
      if ( (xx = nsp_matrix_copy(x)) == NULLMAT ) 
	return RET_BUG;
    }
  else
    {
      if ( (xx = nsp_matrix_create(NVOID, 'c', x->m, x->n)) == NULLMAT ) 
	return RET_BUG;
      for ( k = 0 ; k < x->mn ; k++ ) { xx->C[k].r = x->R[k]; xx->C[k].i = 0.0; }
    }


  if ( (wsave = malloc((4*x->m+15)*sizeof(double))) == NULL )
    goto err;
  C2F(zffti)(&(xx->m), wsave);

  for ( k = 0 ; k < x->n ; k++ )
    C2F(zfftb)(&(xx->m), &(xx->C[k*x->m]), wsave);

  if ( xx->n != xx->m )
    {
      free(wsave);
      if ( (wsave = malloc((4*xx->n+15)*sizeof(double))) == NULL )
	goto err;
      C2F(zffti)(&(xx->n), wsave);
    }

  if ( (y = nsp_matrix_create(NVOID, 'c', xx->n, xx->m)) == NULLMAT )
    goto err;

  transpose_cmplx_mat(xx, y);
  for ( k = 0 ; k < y->n ; k++ )
    C2F(zfftb)(&(y->m), &(y->C[k*y->m]), wsave);
  transpose_cmplx_mat(y, xx);

  free(wsave);
  nsp_matrix_destroy(y);

  invn = 1.0 / xx->mn;
  for (k = 0 ; k < xx->mn ; k++) { xx->C[k].r *= invn; xx->C[k].i *= invn; }

  MoveObj (stack, 1, (NspObject *) xx);
  return 1;

 err:
  free(wsave);
  nsp_matrix_destroy(xx);
  return RET_BUG;
}


#endif

static NspMatrix *nsp_mat_shift(NspMatrix *x, int dim_flag, char orient)
{
  NspMatrix *y;
  int j, im, jm;
  Boolean is_vector;

  if ( (y = nsp_matrix_create(NVOID, x->rc_type, x->m, x->n)) == NULLMAT )
    return NULLMAT;

  is_vector =  x->m == 1 || x->n == 1;

  if ( dim_flag == 0 )
    {
      if ( is_vector )
	{
	  im = orient == 's' ? (int) ceil( x->mn/2.0 ) : (int) floor( x->mn/2.0 );
	  if ( y->rc_type == 'r' )
	    {
	      memcpy(&y->R[x->mn-im], x->R, im*sizeof(double));
	      memcpy(y->R, &x->R[im], (x->mn-im)*sizeof(double));
	    }
	  else
	    {
	      memcpy(&y->C[x->mn-im], x->C, im*sizeof(doubleC));
	      memcpy(y->C, &x->C[im], (x->mn-im)*sizeof(doubleC));
	    }
	}
      else
	{
	  im = orient == 's' ? (int) ceil( x->m/2.0 ) : (int) floor( x->m/2.0 );
	  jm = orient == 's' ? (int) ceil( x->n/2.0 ) : (int) floor( x->n/2.0 );
	  char *from, *to, *qx1, *qy1, *qx2, *qy2, *qx3, *qy3, *qx4, *qy4;
	  int elt_size, stride, b12rsize, b34rsize;
	  if ( y->rc_type == 'r' )
	    {
	      elt_size = sizeof(double); qx1 = (char *) x->R; qy1 = (char *) y->R;
	    }
	  else
	    {
	      elt_size = sizeof(doubleC); qx1 = (char *) x->C; qy1 = (char *) y->C;
	    }
	  stride = elt_size*x->m; b12rsize = elt_size*im; b34rsize = elt_size*(x->m-im);
	  qx4 = qx1 + b12rsize; qx2 = qx1 + jm*stride; qx3 = qx2 + b12rsize;  
	  qy4 = qy1 + b34rsize; qy2 = qy1 + (x->n-jm)*stride; qy3 = qy2 + b34rsize;  
	  /* first x quadrant -> third y quadrant */
	  for ( j = 0, from = qx1, to = qy3 ; j < jm ; j++, from+=stride, to+=stride )
	    memcpy(to, from, b12rsize);
	  /* third x quadrant -> first y quadrant */
	  for ( j = 0, from = qx3, to = qy1 ; j < x->n-jm ; j++, from+=stride, to+=stride )
	    memcpy(to, from, b34rsize);
	  /* second x quadrant -> fourth y quadrant */
	  for ( j = 0, from = qx2, to = qy4 ; j < x->n-jm ; j++, from+=stride, to+=stride )
	    memcpy(to, from, b12rsize);
	  /* fourth x quadrant -> second y quadrant */
	  for ( j = 0, from = qx4, to = qy2 ; j < jm ; j++, from+=stride, to+=stride )
	    memcpy(to, from, b34rsize);
	}
    }
  else if ( dim_flag == 1 )
    {
      im = orient == 's' ? (int) ceil( x->m/2.0 ) : (int) floor( x->m/2.0 );
      char *from, *to, *qx1, *qy1, *qx2, *qy2;
      int elt_size, stride, b12rsize, b34rsize;
      if ( y->rc_type == 'r' )
	{
	  elt_size = sizeof(double); qx1 = (char *) x->R; qy1 = (char *) y->R;
	}
      else
	{
	  elt_size = sizeof(doubleC); qx1 = (char *) x->C; qy1 = (char *) y->C;
	}
      stride = elt_size*x->m; b12rsize = elt_size*im; b34rsize = elt_size*(x->m-im);
      qx2 = qx1 + b12rsize; qy2 = qy1 + b34rsize;
      /* first im rows of x -> last im rows of y */
      for ( j = 0, from = qx1, to = qy2 ; j < x->n ; j++, from+=stride, to+=stride )
	memcpy(to, from, b12rsize);
      /* last (x->m-im) rows of x -> first (x->m-im) rows of y */
      for ( j = 0, from = qx2, to = qy1 ; j < x->n ; j++, from+=stride, to+=stride )
	memcpy(to, from, b34rsize);
    }
  else  /* dim_flag == 2 */
    {
      jm = orient == 's' ? (int) ceil( x->n/2.0 ) : (int) floor( x->n/2.0 );
      if ( y->rc_type == 'r' )
	{
	  memcpy(&y->R[(x->n-jm)*x->m], x->R, x->m*jm*sizeof(double));
	  memcpy(y->R, &x->R[jm*x->m], x->m*(x->n-jm)*sizeof(double));
	}
      else
	{
	  memcpy(&y->C[(x->n-jm)*x->m], x->C, x->m*jm*sizeof(doubleC));
	  memcpy(y->C, &x->C[jm*x->m], x->m*(x->n-jm)*sizeof(doubleC));
	}
     }
      
  return y;
}


static NspMatrix *nsp_mat_fliplr(NspMatrix *x)
{
  NspMatrix *y;
  int j, colsize;
  char *to, *from;

  if ( (y = nsp_matrix_create(NVOID, x->rc_type, x->m, x->n)) == NULLMAT )
    return NULLMAT;

  if ( x->rc_type == 'r' )
    {
      from = (char *) &(x->R[(x->n-1)*x->m]); to =  (char *) y->R; colsize = x->m*sizeof(double);
    }
  else
    {
      from = (char *) &(x->C[(x->n-1)*x->m]); to =  (char *) y->C; colsize = x->m*sizeof(doubleC);
    }

  for ( j = 0; j < x->n ; j++ , to += colsize, from -= colsize )
    memcpy(to, from, colsize);

  return y;
}

static NspMatrix *nsp_mat_flipud(NspMatrix *x)
{
  NspMatrix *y;
  int i, j, k=0, stride;

  if ( (y = nsp_matrix_create(NVOID, x->rc_type, x->m, x->n)) == NULLMAT )
    return NULLMAT;

  if ( x->rc_type == 'r' )
    {
      for ( j = 0, stride = x->m-1 ; j < x->n ; j++, stride += x->m )
	for ( i = 0 ; i < x->m ; i++, k++ )
	  y->R[stride - i] = x->R[k];
    }
  else
    {
      for ( j = 0, stride = x->m-1 ; j < x->n ; j++, stride += x->m )
	for ( i = 0 ; i < x->m ; i++, k++ )
	  y->C[stride - i] = x->C[k];
    }

  return y;
}

static NspMatrix *nsp_mat_rot(NspMatrix *x, int nbrot)
{
  NspMatrix *y = NULLMAT;
  int i, j, k, stride;

  while (nbrot < 0) 
    nbrot+=4;
  nbrot = nbrot % 4;

  switch ( nbrot )
    {
    case 0:
      y = nsp_matrix_copy(x);
      break;

    case 1:
      if ( (y = nsp_matrix_create(NVOID, x->rc_type, x->n, x->m)) == NULLMAT )
	return NULLMAT;

      if ( x->rc_type == 'r' )
	{
	  for ( j = 0, k=0 ; j < x->n ; j++ )
	    for ( i = 0, stride = x->n-1-j ; i < x->m ; i++, stride+=x->n, k++ )
	      y->R[stride] = x->R[k];
	}
      else
	{
	  for ( j = 0, k=0 ; j < x->n ; j++ )
	    for ( i = 0, stride = x->n-1-j ; i < x->m ; i++, stride+=x->n, k++ )
	      y->C[stride] = x->C[k];
	}
      break;

    case 2:
      if ( (y = nsp_matrix_create(NVOID, x->rc_type, x->m, x->n)) == NULLMAT )
	return NULLMAT;

      if ( x->rc_type == 'r' )
	{
	  for ( i = 0, k = x->mn-1 ; i < x->mn ; i++, k-- )
	    y->R[k] = x->R[i];
	}
      else
	{
	  for ( i = 0, k = x->mn-1 ; i < x->mn ; i++, k-- )
	    y->C[k] = x->C[i];
	}
      break;

    case 3:
      if ( (y = nsp_matrix_create(NVOID, x->rc_type, x->n, x->m)) == NULLMAT )
	return NULLMAT;

      if ( x->rc_type == 'r' )
	{
	  for ( j = 0, k=0 ; j < x->n ; j++ )
	    for ( i = 0, stride = j + (x->m-1)*x->n ; i < x->m ; i++, stride-=x->n, k++ )
	      y->R[stride] = x->R[k];
	}
      else
	{
	  for ( j = 0, k=0 ; j < x->n ; j++ )
	    for ( i = 0, stride = j + (x->m-1)*x->n ; i < x->m ; i++, stride-=x->n, k++ )
	      y->C[stride] = x->C[k];
	}
      break;
    }

  return y;
}

static int int_nsp_fftshift(Stack stack, int rhs, int opt, int lhs)
{
  int dim_flag=0;
  NspMatrix *x, *y;
  CheckRhs (1, 2);
  CheckLhs (1, 1);

  if ((x = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;

  if (rhs == 2)
    {
      if ( GetDimArg(stack, 2, &dim_flag) == FAIL )
	return RET_BUG;
      if ( dim_flag == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim_flag == -2 )  /* matlab compatibility flag */
	dim_flag = GiveMatlabDimFlag(x);
    }

  if ( dim_flag > 2 )
    {
      Scierror("%s: Invalid dim flag '%d' (must be 0, 1 or 2)\n", NspFname(stack), dim_flag);
      return RET_BUG;
    }
  
  if ( ( y = nsp_mat_shift(x, dim_flag, 's') ) == NULLMAT )
    return RET_BUG;

  MoveObj (stack, 1, (NspObject *) y);
  return 1;
}

static int int_nsp_ifftshift(Stack stack, int rhs, int opt, int lhs)
{
  int dim_flag=0;
  NspMatrix *x, *y;
  CheckRhs (1, 2);
  CheckLhs (1, 1);

  if ((x = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;

  if (rhs == 2)
    {
      if ( GetDimArg(stack, 2, &dim_flag) == FAIL )
	return RET_BUG;
      if ( dim_flag == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim_flag == -2 )  /* matlab compatibility flag */
	dim_flag = GiveMatlabDimFlag(x);
    }

  if ( ( y = nsp_mat_shift(x, dim_flag, 'i') ) == NULLMAT )
    return RET_BUG;

  if ( dim_flag > 2 )
    {
      Scierror("%s: Invalid dim flag '%d' (must be 0, 1 or 2)\n", NspFname(stack), dim_flag);
      return RET_BUG;
    }

  MoveObj (stack, 1, (NspObject *) y);
  return 1;
}

static int int_nsp_fliplr(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *y;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetMat (stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( (y = nsp_mat_fliplr(x)) == NULLMAT )
    return RET_BUG;

  MoveObj (stack, 1, (NspObject *) y);
  return 1;
}

static int int_nsp_flipud(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *y;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetMat (stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( (y = nsp_mat_flipud(x)) == NULLMAT )
    return RET_BUG;

  MoveObj (stack, 1, (NspObject *) y);
  return 1;
}

static int int_nsp_rot(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *y;
  int nbrot = 1;
  CheckRhs (1, 2);
  CheckLhs (1, 1);

  if ( (x = GetMat (stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( rhs == 2 )
    if (GetScalarInt(stack, 2, &nbrot) == FAIL)
      return RET_BUG;

  if ( (y = nsp_mat_rot(x,nbrot)) == NULLMAT )
    return RET_BUG;

  MoveObj (stack, 1, (NspObject *) y);
  return 1;
}

static OpTab Fft_func[]={
    {"fft_m", int_nsp_fftnew},
    {"fft_deprecated_m_m", int_nsp_fft_deprecated},
    {"fftnew_m", int_nsp_fftnew},
    {"ifftnew_m", int_nsp_ifftnew},
    {"ifft_m", int_nsp_ifftnew},
    {"fft2_m", int_nsp_fft2new},
    {"fft2_deprecated_m_m", int_nsp_fft2_deprecated},
    {"fft2new_m", int_nsp_fft2new},
    {"ifft2new_m", int_nsp_ifft2new},
    {"ifft2_m", int_nsp_ifft2new},
    {"fftshift_m", int_nsp_fftshift},
    {"ifftshift_m", int_nsp_ifftshift},
    {"fliplr_m", int_nsp_fliplr},
    {"flipud_m", int_nsp_flipud},
    {"rot90_m", int_nsp_rot},
    {(char *) 0, NULL}
};

int Fft_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Fft_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Fft_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Fft_func[i].name;
  *f = Fft_func[i].fonc;
}
