/* 
 * Nsp interface for fft
 * Copyright (C) 2005  Bruno Pincon
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

int int_nsp_fft( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = fft(x, forward_backward_flag, row_col_flag)
   *  using the fftw3  lib
   */
  NspMatrix *x;
  double invn=0;
  int s, k;
  char *str;
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
      if ((str = GetString(stack,3)) == (char*)0)
	return RET_BUG;
    }
  else
    str = "f";


  if ((x = GetMatCopy(stack,1)) == NULLMAT) 
    return RET_BUG;

  if ( x->rc_type == 'r' )
    if (nsp_mat_set_ival(x,0.00) == FAIL ) 
      return RET_BUG;

  if ( str[0] == '*' || str[0] == 'f'  || str[0] == 'F' )
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
      else  /* backward fft */
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
  else   /* fft of the rows or the columns */
    {
      if ( str[0] == 'c' || str[0] == 'C' )  /* fft of the columns */
	{
	  if ( s == 1) invn = 1.0 / x->m;
	  multi_plan = fftw_plan_many_dft(1, &(x->m), x->n,  
					 (fftw_complex *)x->C, &(x->mn), 1, x->m,
					 (fftw_complex *)x->C, &(x->mn), 1, x->m,
					 s, FFTW_ESTIMATE | FFTW_UNALIGNED );
	}
      else if ( str[0] == 'r' || str[0] == 'R' ) /* fft of the rows */
	{
	  if ( s == 1) invn = 1.0 / x->n;
	  multi_plan = fftw_plan_many_dft(1, &(x->n), x->m,  
					 (fftw_complex *)x->C, &(x->mn), x->m, 1,
					 (fftw_complex *)x->C, &(x->mn), x->m, 1,
					 s, FFTW_ESTIMATE | FFTW_UNALIGNED );
	}
      else
	{
	  Scierror("%s: bad third arg\n",NspFname(stack));
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


int int_nsp_fft2( Stack stack, int rhs, int opt, int lhs)
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
    if (nsp_mat_set_ival(x,0.00) == FAIL ) 
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

#else


int int_nsp_fft( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  
   *  interface for y = fft(x, forward_backward_flag, row_col_flag)
   *  using the fftpack lib
   */
  enum {ALL, BY_ROW, BY_COL};
  NspMatrix *x, *y;
  int s, k;
  char *str;
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
      if ((str = GetString(stack,3)) == (char*)0)
	return RET_BUG;
    }
  else
    str = "f";


  if ((x = GetMatCopy(stack,1)) == NULLMAT) 
    return RET_BUG;

  if ( x->rc_type == 'r' )
    if (nsp_mat_set_ival(x,0.00) == FAIL ) 
      return RET_BUG;

  if ( str[0] == '*' || str[0] == 'f'  || str[0] == 'F' )
    {
      n = x->mn;
      flag_orient = ALL;
    }
  else if ( str[0] == 'c' ||  str[0] == 'C' )
    {
      n = x->m;
      flag_orient = BY_COL;
    }
  else if ( str[0] == 'r' ||  str[0] == 'R' )
    {
      n = x->n;
      flag_orient = BY_ROW;
    }
  else
    {
      Scierror("%s: bad third arg\n",NspFname(stack));
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

int int_nsp_fft2( Stack stack, int rhs, int opt, int lhs)
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

#endif


static OpTab Fft_func[]={
    {"fft", int_nsp_fft},
    {"fft2", int_nsp_fft2},
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
