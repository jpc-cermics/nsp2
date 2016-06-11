/* wshrsl.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;

/*/MEMBR ADD NAME=WSHRSL,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_wshrsl (double *ar, double *ai, double *br, double *bi,
		     double *cr, double *ci, int *m, int *n, int *na, int *nb,
		     int *nc, double *eps, double *rmax, int *fail)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, br_dim1, br_offset, bi_dim1,
    bi_offset, cr_dim1, cr_offset, ci_dim1, ci_offset, i__1;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int i__, k, l;
  double t, ti, tr;
  int km1, lm1;

  /* 
   *!purpose 
   *  wshrsl is a fortran iv subroutine to solve the complex matrix 
   *  equation ax + xb = c, where a is in lower triangular form 
   *  and b is in upper triangular form, 
   * 
   *!calling sequence 
   * 
   *     subroutine wshrsl(ar,ai,br,bi,cr,ci,m,n,na,nb,nc,eps,rmax,fail) 
   *  ar,ai  a doubly subscripted array containg the matrix a in 
   *         lower triangular form 
   * 
   *  br,bi  a doubly subscripted array containing tbe matrix br,bi 
   *         in upper triangular form 
   * 
   *  cr,ci  a doubly subscripted array containing the matrix c. 
   * 
   *  m      the order of the matrix a 
   * 
   *  n      the order of the matrix b 
   * 
   *  na     the first dimension of the array a 
   * 
   *  nb     the first dimension of the array b 
   * 
   *  nc     the first dimension of the array c 
   * 
   *  eps    tolerance on a(k,k)+b(l,l) 
   *         if |a(k,k)+b(l,l)|<eps algorithm suppose that |a(k,k)+b(l,l)|=eps 
   * 
   *  rmax   maximum allowed size of any element of the transformation 
   * 
   *  fail   indicates if wshrsl failed 
   * 
   *!auxiliary routines 
   *    ddot (blas) 
   *    abs sqrt (fortran) 
   *!originator 
   *    Steer Serge  I.N.R.I.A from shrslv (Bartels and Steward) 
   *! 
   * 
   *internal variables 
   * 
   * 
   */
  /* Parameter adjustments */
  ai_dim1 = *na;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *na;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  bi_dim1 = *nb;
  bi_offset = bi_dim1 + 1;
  bi -= bi_offset;
  br_dim1 = *nb;
  br_offset = br_dim1 + 1;
  br -= br_offset;
  ci_dim1 = *nc;
  ci_offset = ci_dim1 + 1;
  ci -= ci_offset;
  cr_dim1 = *nc;
  cr_offset = cr_dim1 + 1;
  cr -= cr_offset;

  /* Function Body */
  *fail = TRUE;
  /* 
   */
  l = 1;
 L10:
  lm1 = l - 1;
  if (l == 1)
    {
      goto L30;
    }
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      cr[i__ + l * cr_dim1] =
	cr[i__ + l * cr_dim1] - C2F (ddot) (&lm1, &cr[i__ + cr_dim1],
					    nc, &br[l * br_dim1 + 1],
					    &c__1) +
	C2F (ddot) (&lm1, &ci[i__ + ci_dim1], nc, &bi[l * bi_dim1 + 1],
		    &c__1);
      ci[i__ + l * ci_dim1] =
	ci[i__ + l * ci_dim1] - C2F (ddot) (&lm1, &cr[i__ + cr_dim1],
					    nc, &bi[l * bi_dim1 + 1],
					    &c__1) -
	C2F (ddot) (&lm1, &ci[i__ + ci_dim1], nc, &br[l * br_dim1 + 1],
		    &c__1);
      /* L20: */
    }
  /* 
   */
 L30:
  k = 1;
 L40:
  km1 = k - 1;
  if (k == 1)
    {
      goto L50;
    }
  cr[k + l * cr_dim1] =
    cr[k + l * cr_dim1] - C2F (ddot) (&km1, &ar[k + ar_dim1], na,
				      &cr[l * cr_dim1 + 1],
				      &c__1) + C2F (ddot) (&km1,
							   &ai[k
							       +
							       ai_dim1],
							   na,
							   &ci[l
							       *
							       ci_dim1
							       + 1], &c__1);
  ci[k + l * ci_dim1] =
    ci[k + l * ci_dim1] - C2F (ddot) (&km1, &ar[k + ar_dim1], na,
				      &ci[l * ci_dim1 + 1],
				      &c__1) - C2F (ddot) (&km1,
							   &ai[k
							       +
							       ai_dim1],
							   na,
							   &cr[l
							       *
							       cr_dim1
							       + 1], &c__1);
  /* 
   */
 L50:
  tr = ar[k + k * ar_dim1] + br[l + l * br_dim1];
  ti = ai[k + k * ai_dim1] + bi[l + l * bi_dim1];
  t = tr * tr + ti * ti;
  if (t < *eps * *eps)
    {
      tr = 1. / *eps;
    }
  else
    {
      tr /= t;
      ti /= t;
    }
  /* 
   */
  t = cr[k + l * cr_dim1] * tr + ci[k + l * ci_dim1] * ti;
  ci[k + l * ci_dim1] = -cr[k + l * cr_dim1] * ti + ci[k + l * ci_dim1] * tr;
  cr[k + l * cr_dim1] = t;
  t = sqrt (t * t + ci[k + l * ci_dim1] * ci[k + l * ci_dim1]);
  if (t >= *rmax)
    {
      return 0;
    }
  ++k;
  if (k <= *m)
    {
      goto L40;
    }
  ++l;
  if (l <= *n)
    {
      goto L10;
    }
  *fail = FALSE;
  return 0;
}				/* wshrsl_ */
