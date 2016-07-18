/* wclmat.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static int c__1 = 1;

/*/MEMBR ADD NAME=WCLMAT,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_wclmat (int *ia, int *n, double *ar, double *ai, double *br,
		     double *bi, int *ib, double *w, double *c__, int *ndng)
{
  /* Initialized data */

  static double zero = 0.;
  static double two = 2.;
  static double half = .5;

  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, br_dim1, br_offset, bi_dim1,
    bi_offset, i__1, i__2, i__3;

  /* Local variables */
  int ndng1,/* ndng2*/ i__, j, i1;
  int n4;
  double w1,/* rc,*/ wd;
  int k1i, k2i, im1, k1r, k2r;

  /* 
   *%purpose 
   *     computes a complex matrix polynomial representated in a 
   *     chebychev base by the clenshaw method. 
   * 
   *%calling sequence 
   * 
   *    subroutine wclmat(ia, n, ar, ai, br, bi, ib, w, c, ndng) 
   * 
   *    int ia,n,ib,ndng 
   *    double precision ar,ai,br,bi,w,c 
   *    dimension ar(ia,n),ai(ia,n),br(ib,n),bi(ib,n),c(*),w(*) 
   * 
   *     ia: the leading dimension of array a. 
   *     n: the order of the matrices a,b. 
   *     ar,ai : the  array that contains the n*n matrix a 
   *     br,bi : the  array that contains the n*n matrix 
   *        pol(a). 
   *     ib:the leading dimension of array b. 
   *     w : work-space array of size 4*n 
   *     c:  vectors which contains the coefficients 
   *     of the polynome. 
   *     ndng: the polynomial order. 
   * 
   *%auxiliary routines 
   *    wmmul (blas.extension) 
   *% 
   * 
   *internal variables 
   * 
   */
  /* Parameter adjustments */
  ai_dim1 = *ia;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *ia;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  bi_dim1 = *ib;
  bi_offset = bi_dim1 + 1;
  bi -= bi_offset;
  br_dim1 = *ib;
  br_offset = br_dim1 + 1;
  br -= br_offset;
  --w;
  --c__;

  /* Function Body */
  /* 
   */
  k1r = 1;
  k1i = k1r + *n;
  k2r = k1i + *n;
  k2i = k2r + *n;
  n4 = *n << 2;
  ndng1 = *ndng + 2;
  /* ndng2 = *ndng - 1; */
  /* rc = c__[ndng1 - 1]; */
  wd = c__[1];
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = n4;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  w[i__] = zero;
	  /* L10: */
	}
      i__2 = *ndng;
      for (i1 = 1; i1 <= i__2; ++i1)
	{
	  im1 = ndng1 - i1;
	  nsp_calpack_wmmul (&ar[ar_offset], &ai[ai_offset], ia, &w[k1r],
			     &w[k1i], n, &br[j * br_dim1 + 1],
			     &bi[j * bi_dim1 + 1], ib, n, n, &c__1);
	  i__3 = *n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      w1 = two * br[i__ + j * br_dim1] - w[k2r - 1 + i__];
	      w[k2r - 1 + i__] = w[k1r - 1 + i__];
	      w[k1r - 1 + i__] = w1;
	      w1 = two * bi[i__ + j * bi_dim1] - w[k2i - 1 + i__];
	      w[k2i - 1 + i__] = w[k1i - 1 + i__];
	      w[k1i - 1 + i__] = w1;
	      /* L20: */
	    }
	  w[j] += c__[im1];
	  /* L30: */
	}
      nsp_calpack_wmmul (&ar[ar_offset], &ai[ai_offset], ia, &w[k1r],
			 &w[k1i], n, &br[j * br_dim1 + 1],
			 &bi[j * bi_dim1 + 1], ib, n, n, &c__1);
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  w[k1r - 1 + i__] = two * br[i__ + j * br_dim1] - w[k2r - 1 + i__];
	  w[k1i - 1 + i__] = two * bi[i__ + j * bi_dim1] - w[k2i - 1 + i__];
	  /* L40: */
	}
      w[j] += wd;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  br[i__ + j * br_dim1] =
	    (w[k1r - 1 + i__] - w[k2r - 1 + i__]) * half;
	  bi[i__ + j * bi_dim1] =
	    (w[k1i - 1 + i__] - w[k2i - 1 + i__]) * half;
	  /* L50: */
	}
      br[j + j * br_dim1] += half * wd;
      /* L60: */
    }
  return 0;
}				/* wclmat_ */
