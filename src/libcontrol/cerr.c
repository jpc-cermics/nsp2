/* cerr.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/*
 *    cerr evaluate the error introduced by pade 
 *    approximant and normalise the  matrix a accordingly 
 *
 *    subroutine cerr(a,w,ia,n,ndng,m,maxc) 
 *    a        : array containing the matrix a 
 *    w        : work space array of size 2*n*n + n 
 *    ia       : leading dimension of array a 
 *    n        : size of matrix a 
 *    ndng     : degree of pade approximant 
 *    m        :  the factor of normalisation is 2**(-m) 
 *    maxc     : maximum admissible for m 
 */

static double c_b6 = 0.;
static int c__1 = 1;
static double c_b8 = 1.;

int nsp_ctrlpack_cerr (double *a, double *w, int *ia, int *n, int *ndng,
		       int *m, int *maxc)
{
  double zero = 0.;
  double one = 1.;
  double two = 2.;

  int a_dim1, a_offset, i__1, i__2, i__3;
  double d__1;

  int itab[15];
  double norm, norm1;
  int i__, j, k, l;
  double alpha;
  int i1, k1, n2;
  int ke, mm, mt, kw;

  /* Parameter adjustments */
  --w;
  a_dim1 = *ia;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  /* 
   * 
   */
  norm = 0.;
  n2 = *n * *n;
  k1 = 1;
  ke = k1 + n2;
  kw = ke + n2;
  k = *ndng << 1;
  nsp_calpack_dmmul (&a[a_offset], ia, &a[a_offset], ia, &w[ke], n, n, n, n);
  nsp_calpack_gdcp2i (&k, itab, &mt);
  if (!itab[0])
    {
      goto L30;
    }
  norm = zero;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      alpha = zero;
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  alpha += (d__1 = a[i__ + j * a_dim1], Abs (d__1));
	  /* L10: */
	}
      if (alpha > norm)
	{
	  norm = alpha;
	}
      /* L20: */
    }
  nsp_calpack_dmcopy (&a[a_offset], ia, &w[k1], n, n, n);
  goto L40;
 L30:
  nsp_dset (&n2, &c_b6, &w[k1], &c__1);
  i__1 = *n + 1;
  nsp_dset (n, &c_b8, &w[k1], &i__1);
 L40:
  if (mt == 1)
    {
      goto L110;
    }
  i__1 = mt;
  for (i1 = 2; i1 <= i__1; ++i1)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  l = 0;
	  i__3 = *n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      w[kw - 1 + i__] =
		C2F (ddot) (n, &w[k1 - 1 + j], n, &w[ke + l], &c__1);
	      l += *n;
	      /* L50: */
	    }
	  C2F (dcopy) (n, &w[kw], &c__1, &w[k1 - 1 + j], n);
	  /* L70: */
	}
      if (!itab[i1 - 1])
	{
	  goto L100;
	}
      norm1 = zero;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  alpha = zero;
	  l = i__ - 1;
	  i__3 = *n;
	  for (j = 1; j <= i__3; ++j)
	    {
	      alpha += (d__1 = w[k1 + l], Abs (d__1));
	      l += *n;
	      /* L80: */
	    }
	  if (alpha > norm1)
	    {
	      norm1 = alpha;
	    }
	  /* L90: */
	}
      norm *= norm1;
    L100:
      ;
    }
 L110:
  norm /= (double) (k + 1);
  i__1 = *ndng;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /*Computing 2nd power 
       */
      i__2 = k - i__ + 1;
      norm /= (double) (i__2 * i__2);
      /* L120: */
    }
  norm *= 8.;
  mm = 0;
 L130:
  if (norm + one <= one)
    {
      goto L140;
    }
  ++mm;
  alpha = pow_di (two, mm);
  norm /= alpha;
  if (mm + *m > *maxc)
    {
      goto L140;
    }
  goto L130;
 L140:
  alpha = pow_di (two, mm);
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  a[i__ + j * a_dim1] /= alpha;
	  /* L150: */
	}
      /* L160: */
    }
  *m += mm;
  return 0;
}				/* cerr_ */
