/* wgesl.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static int c__1 = 1;

int
nsp_ctrlpack_wgesl (double *ar, double *ai, int *lda, int *n, int *ipvt,
		    double *br, double *bi, int *job)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;
  double d__1;

  /* Local variables */
  int k, l;
  int kb;
  double ti, tr;
  int nm1;

  /*    Copyright INRIA 
   *!purpose 
   * 
   *    wgesl solves the double-complex system 
   *    a * x = b  or  ctrans(a) * x = b 
   *    using the factors computed by wgeco or wgefa. 
   * 
   *!calling sequence 
   * 
   *     subroutine wgesl(ar,ai,lda,n,ipvt,br,bi,job) 
   *    on entry 
   * 
   *       a       double-complex(lda, n) 
   *               the output from wgeco or wgefa. 
   * 
   *       lda     int 
   *               the leading dimension of the array  a . 
   * 
   *       n       int 
   *               the order of the matrix  a . 
   * 
   *       ipvt    int(n) 
   *               the pivot vector from wgeco or wgefa. 
   * 
   *       b       double-complex(n) 
   *               the right hand side vector. 
   * 
   *       job     int 
   *               = 0         to solve  a*x = b , 
   *               = nonzero   to solve  ctrans(a)*x = b  where 
   *                           ctrans(a)  is the conjugate transpose. 
   * 
   *    on return 
   * 
   *       b       the solution vector  x . 
   * 
   *    error condition 
   * 
   *       a division by zero will occur if the input factor contains a 
   *       zero on the diagonal.  technically this indicates singularity 
   *       but it is often caused by improper arguments or improper 
   *       setting of lda .  it will not occur if the subroutines are 
   *       called correctly and if wgeco has set rcond .gt. 0.0 
   *       or wgefa has set info .eq. 0 . 
   * 
   *    to compute  inverse(a) * c  where  c  is a matrix 
   *    with  p  columns 
   *          call wgeco(a,lda,n,ipvt,rcond,z) 
   *          if (rcond is too small) go to ... 
   *          do 10 j = 1, p 
   *             call wgesl(a,lda,n,ipvt,c(1,j),0) 
   *       10 continue 
   * 
   *!originator 
   *    linpack. this version dated 07/01/79 . 
   *    cleve moler, university of new mexico, argonne national lab. 
   * 
   *!auxiliary routines 
   * 
   *    blas waxpy,wdotc 
   * 
   *! 
   *    internal variables 
   * 
   * 
   */
  /* Parameter adjustments */
  ai_dim1 = *lda;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *lda;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  --ipvt;
  --br;
  --bi;

  /* Function Body */
  nm1 = *n - 1;
  if (*job != 0)
    {
      goto L50;
    }
  /* 
   *       job = 0 , solve  a * x = b 
   *       first solve  l*y = b 
   * 
   */
  if (nm1 < 1)
    {
      goto L30;
    }
  i__1 = nm1;
  for (k = 1; k <= i__1; ++k)
    {
      l = ipvt[k];
      tr = br[l];
      ti = bi[l];
      if (l == k)
	{
	  goto L10;
	}
      br[l] = br[k];
      bi[l] = bi[k];
      br[k] = tr;
      bi[k] = ti;
    L10:
      i__2 = *n - k;
      nsp_calpack_waxpy (&i__2, &tr, &ti, &ar[k + 1 + k * ar_dim1],
			 &ai[k + 1 + k * ai_dim1], &c__1, &br[k + 1],
			 &bi[k + 1], &c__1);
      /* L20: */
    }
 L30:
  /* 
   *       now solve  u*x = y 
   * 
   */
  i__1 = *n;
  for (kb = 1; kb <= i__1; ++kb)
    {
      k = *n + 1 - kb;
      nsp_calpack_wdiv (&br[k], &bi[k], &ar[k + k * ar_dim1],
			&ai[k + k * ai_dim1], &br[k], &bi[k]);
      tr = -br[k];
      ti = -bi[k];
      i__2 = k - 1;
      nsp_calpack_waxpy (&i__2, &tr, &ti, &ar[k * ar_dim1 + 1],
			 &ai[k * ai_dim1 + 1], &c__1, &br[1], &bi[1], &c__1);
      /* L40: */
    }
  goto L100;
 L50:
  /* 
   *       job = nonzero, solve  ctrans(a) * x = b 
   *       first solve  ctrans(u)*y = b 
   * 
   */
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      i__2 = k - 1;
      tr =
	br[k] - nsp_calpack_wdotcr (&i__2, &ar[k * ar_dim1 + 1],
				    &ai[k * ai_dim1 + 1], &c__1, &br[1],
				    &bi[1], &c__1);
      i__2 = k - 1;
      ti =
	bi[k] - nsp_calpack_wdotci (&i__2, &ar[k * ar_dim1 + 1],
				    &ai[k * ai_dim1 + 1], &c__1, &br[1],
				    &bi[1], &c__1);
      d__1 = -ai[k + k * ai_dim1];
      nsp_calpack_wdiv (&tr, &ti, &ar[k + k * ar_dim1], &d__1, &br[k],
			&bi[k]);
      /* L60: */
    }
  /* 
   *       now solve ctrans(l)*x = y 
   * 
   */
  if (nm1 < 1)
    {
      goto L90;
    }
  i__1 = nm1;
  for (kb = 1; kb <= i__1; ++kb)
    {
      k = *n - kb;
      i__2 = *n - k;
      br[k] +=
	nsp_calpack_wdotcr (&i__2, &ar[k + 1 + k * ar_dim1],
			    &ai[k + 1 + k * ai_dim1], &c__1, &br[k + 1],
			    &bi[k + 1], &c__1);
      i__2 = *n - k;
      bi[k] +=
	nsp_calpack_wdotci (&i__2, &ar[k + 1 + k * ar_dim1],
			    &ai[k + 1 + k * ai_dim1], &c__1, &br[k + 1],
			    &bi[k + 1], &c__1);
      l = ipvt[k];
      if (l == k)
	{
	  goto L70;
	}
      tr = br[l];
      ti = bi[l];
      br[l] = br[k];
      bi[l] = bi[k];
      br[k] = tr;
      bi[k] = ti;
    L70:
      /* L80: */
      ;
    }
 L90:
 L100:
  return 0;
}				/* wgesl_ */
