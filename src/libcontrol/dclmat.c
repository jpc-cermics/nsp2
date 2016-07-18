/* dclmat.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

static int c__1 = 1;

/* 
 *     computes a matrix polynomial representated in a chebyshev 
 *     base by the clenshaw method. 
 * 
 *    subroutine dclmat(ia, n, a, b, ib,w , c, ndng) 
 * 
 *    int ia,n,ib,ndng 
 *    double precision a,b,w,c 
 *    dimension a(ia,n), b(ib,n), c(*), w(*) 
 * 
 *     ia: the leading dimension of array a. 
 *     n: the order of the matrices a,b. 
 *     a: the  array that contains the n*n matrix a 
 *     b: the  array that contains the n*n matrix 
 *        pol(a). 
 *     ib:the leading dimension of array b. 
 *     w : work-space array of size n+n 
 *     c:  vectors which contains the coefficients 
 *     of the polynome. 
 *     ndng: the polynomial order. 
 */

int
nsp_ctrlpack_dclmat (int *ia, int *n, double *a, double *b, int *ib,
		     double *w, double *c__, int *ndng)
{
  /* Initialized data */

  static double zero = 0.;
  static double two = 2.;
  static double half = .5;

  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;

  /* Local variables */
  int ndng1,/* ndng2,*/ i__, j;
  int i1;
  double w1,/* rc,*/ wd;
  int im1;

  /* Parameter adjustments */
  a_dim1 = *ia;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *ib;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  --w;
  --c__;

  /* Function Body */
  /* 
   */
  ndng1 = *ndng + 2;
  /* ndng2 = *ndng - 1; */
  /* rc = c__[ndng1 - 1]; */
  wd = c__[1];
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  w[*n + i__] = zero;
	  w[i__] = zero;
	  /* L10: */
	}
      i__2 = *ndng;
      for (i1 = 1; i1 <= i__2; ++i1)
	{
	  im1 = ndng1 - i1;
	  nsp_calpack_dmmul (&a[a_offset], ia, &w[1], n, &b[j * b_dim1 + 1],
			     ib, n, n, &c__1);
	  i__3 = *n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      w1 = two * b[i__ + j * b_dim1] - w[*n + i__];
	      w[*n + i__] = w[i__];
	      w[i__] = w1;
	      /* L20: */
	    }
	  w[j] += c__[im1];
	  /* L30: */
	}
      nsp_calpack_dmmul (&a[a_offset], ia, &w[1], n, &b[j * b_dim1 + 1], ib,
			 n, n, &c__1);
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  w[i__] = two * b[i__ + j * b_dim1] - w[*n + i__];
	  /* L40: */
	}
      w[j] += wd;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  b[i__ + j * b_dim1] = (w[i__] - w[*n + i__]) * half;
	  /* L50: */
	}
      b[j + j * b_dim1] += half * wd;
      /* L60: */
    }
  return 0;
}				/* dclmat_ */
