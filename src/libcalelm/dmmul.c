/* Nsp
 * Copyright (C) 2007-2009 Ramine Nikoukhah (Inria) 
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
 *
 * utilities copyrighted GPL in this version by Ramine Nikoukhah
 * translated to C and simplified by Jean-Philippe Chancelier 
 * 
 *--------------------------------------------------------------------------*/

#include "nsp/machine.h"
#include "nsp/object.h"
#include "nsp/blas.h"

/**
 * nsp_calpack_dmmul:
 * @a: array of double 
 * @na: int pointer
 * @b:  array of double 
 * @nb: int pointer
 * @c:  array of double 
 * @nc: int pointer
 * @l: int pointer 
 * @m: int pointer
 * @n: int pointer
 * 
 * computes the matrix product @c = @a*@b, where @c is (@l,@n), @a is 
 * (@l,@m) and @b is (@m,@n) by calling dgemm().
 * 
 * Returns: 0 
 **/

int nsp_calpack_dmmul (double *a, int *na, double *b, int *nb, double *c,
		       int *nc, int *l, int *m, int *n)
{
  double c_b4 = 1.0, c_b5 = 0.0;
  C2F (dgemm) ("n", "n", l, n, m, &c_b4, a, na, b, nb, &c_b5, c, nc, 1L, 1L);
  return 0;
}

/**
 * nsp_calpack_dmmul1:
 * @a: array of double 
 * @na: int pointer
 * @b:  array of double 
 * @nb: int pointer
 * @c:  array of double 
 * @nc: int pointer
 * @l: int pointer 
 * @m: int pointer
 * @n: int pointer
 * 
 * computes the matrix product @c =@c +  @a*@b, where @c is (@l,@n), @a is 
 * (@l,@m) and @b is (@m,@n) by calling dgemm().
 * 
 * Returns: 0 
 **/

static int c__1 = 1;

int nsp_calpack_dmmul1 (double *a, int *na, double *b, int *nb, double *c__,
			int *nc, int *l, int *m, int *n)
{
  int i__1, i__2;
  static int i__, j, ib, ic;

  --c__;
  --b;
  --a;
  
  /* Function Body */
  ib = 1;
  ic = 0;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j) {
    i__2 = *l;
    for (i__ = 1; i__ <= i__2; ++i__) {
      /* L20: */
      c__[ic + i__] += C2F(ddot)(m, &a[i__], na, &b[ib], &c__1);
    }
    ic += *nc;
    ib += *nb;
    /* L30: */
  }
  return 0;
}

/*
 * computes the matrix product C = A * B where the 
 * matrices are complex with the scilab storage 
 *           C   =   A   *   B 
 *         (l,n)   (l,m) * (m,n) 
 */

int nsp_calpack_wmmul (double *ar, double *ai, int *na, double *br,
		       double *bi, int *nb, double *cr, double *ci, int *nc,
		       int *l, int *m, int *n)
{
  double c_b4 = 1.0, c_b5 = 0., c_b8 = -1.;
  /*    Cr <-  1*Ar*Br + 0*Cr */
  C2F (dgemm) ("n", "n", l, n, m, &c_b4, ar, na, br, nb, &c_b5, cr, nc, 1L, 1L);
  /*    Cr <- -1*Ai*Bi + 1*Cr  */
  C2F (dgemm) ("n", "n", l, n, m, &c_b8, ai, na, bi, nb, &c_b4, cr, nc, 1L, 1L);
  /*    Ci <-  1*Ar*Bi + 0*Ci  */
  C2F (dgemm) ("n", "n", l, n, m, &c_b4, ar, na, bi, nb, &c_b5, ci, nc, 1L, 1L);
  /*    Ci <-  1*Ai*Br + 1*Ci  */
  C2F (dgemm) ("n", "n", l, n, m, &c_b4, ai, na, br, nb, &c_b4, ci, nc, 1L, 1L);
  return 0;
}

/*
 *     forms the dot product of two vectors. 
 *     uses unrolled loops for increments equal to one. 
 *     jack dongarra, linpack, 3/11/78. 
 *     modified 12/3/93, array(1) declarations changed to array(*) 
 */
 
double ddot(int *n, double *dx, int *incx, double *dy, int *incy)
{
  int i1;
  double ret_val;
  
  static int i, m;
  static double dtemp;
  static int ix, iy, mp1;
  
  --dy;
  --dx;

  ret_val = 0.;
  dtemp = 0.;
  if (*n <= 0) {
    return ret_val;
  }
  if (*incx == 1 && *incy == 1) {
    goto L20;
  }
  
  /*        code for unequal increments or equal increments */
  /*          not equal to 1 */
  
  ix = 1;
  iy = 1;
  if (*incx < 0) {
    ix = (-(*n) + 1) * *incx + 1;
  }
  if (*incy < 0) {
    iy = (-(*n) + 1) * *incy + 1;
  }
  i1 = *n;
  for (i = 1; i <= i1; ++i) {
    dtemp += dx[ix] * dy[iy];
    ix += *incx;
    iy += *incy;
    /* L10: */
  }
  ret_val = dtemp;
  return ret_val;
  
  /*        code for both increments equal to 1 */
  /*        clean-up loop */
  
 L20:
  m = *n % 5;
  if (m == 0) {
    goto L40;
  }
  i1 = m;
  for (i = 1; i <= i1; ++i)
    {
      dtemp += dx[i] * dy[i];
    }
  if (*n < 5)
    {
      goto L60;
    }
 L40:
  mp1 = m + 1;
  i1 = *n;
  for (i = mp1; i <= i1; i += 5)
    {
      dtemp = dtemp + dx[i] * dy[i] + dx[i + 1] * dy[i+1] +
	dx[i+2] * dy[i+2] + dx[i+3] * dy[i+3] + dx[i+4] * dy[i+4];
  }
 L60:
  ret_val = dtemp;
  return ret_val;
}


