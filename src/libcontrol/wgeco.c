/* wgeco.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static int c__1 = 1;

int
nsp_ctrlpack_wgeco (double *ar, double *ai, int *lda, int *n, int *ipvt,
		    double *rcond, double *zr, double *zi)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;
  double d__1, d__2, d__3, d__4;

  /* Local variables */
  int info;
  double wkmi;
  double wkmr;
  int j, k, l;
  double s;
  double anorm;
  double ynorm;
  int kb;
  double ti, sm, tr;
  int kp1;
  double eki, ekr, wki, wkr;

  /*    Copyright INRIA 
   *!purpose 
   * 
   *    wgeco factors a double-complex matrix by gaussian elimination 
   *    and estimates the condition of the matrix. 
   * 
   *    if  rcond  is not needed, wgefa is slightly faster. 
   *    to solve  a*x = b , follow wgeco by wgesl. 
   *    to compute  inverse(a)*c , follow wgeco by wgesl. 
   *    to compute  determinant(a) , follow wgeco by wgedi. 
   *    to compute  inverse(a) , follow wgeco by wgedi. 
   * 
   *!calling sequence 
   * 
   *     subroutine wgeco(ar,ai,lda,n,ipvt,rcond,zr,zi) 
   *    on entry 
   * 
   *       a       double-complex(lda, n) 
   *               the matrix to be factored. 
   * 
   *       lda     int 
   *               the leading dimension of the array  a . 
   * 
   *       n       int 
   *               the order of the matrix  a . 
   * 
   *    on return 
   * 
   *       a       an upper triangular matrix and the multipliers 
   *               which were used to obtain it. 
   *               the factorization can be written  a = l*u  where 
   *               l  is a product of permutation and unit lower 
   *               triangular matrices and  u  is upper triangular. 
   * 
   *       ipvt    int(n) 
   *               an int vector of pivot indices. 
   * 
   *       rcond   double precision 
   *               an estimate of the reciprocal condition of  a . 
   *               for the system  a*x = b , relative perturbations 
   *               in  a  and  b  of size  epsilon  may cause 
   *               relative perturbations in  x  of size  epsilon/rcond . 
   *               if  rcond  is so small that the int expression 
   *                          1.0 + rcond .eq. 1.0 
   *               is true, then  a  may be singular to working 
   *               precision.  in particular,  rcond  is zero  if 
   *               exact singularity is detected or the estimate 
   *               underflows. 
   * 
   *       z       double-complex(n) 
   *               a work vector whose contents are usually unimportant. 
   *               if  a  is close to a singular matrix, then  z  is 
   *               an approximate null vector in the sense that 
   *               norm(a*z) = rcond*norm(a)*norm(z) . 
   * 
   *!originator 
   *    linpack. this version dated 07/01/79 . 
   *    cleve moler, university of new mexico, argonne national lab. 
   * 
   *!auxiliary routines 
   * 
   *    linpack wgefa 
   *    blas waxpy,wdotc,wasum 
   *    fortran abs,max 
   * 
   *! 
   *    internal variables 
   * 
   * 
   * 
   *    compute 1-norm of a 
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
  --zr;
  --zi;

  /* Function Body */
  anorm = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      /*Computing MAX 
       */
      d__1 = anorm, d__2 =
	nsp_calpack_wasum (n, &ar[j * ar_dim1 + 1], &ai[j * ai_dim1 + 1],
			   &c__1);
      anorm = Max (d__1, d__2);
      /* L10: */
    }
  /* 
   *    factor 
   * 
   */
  nsp_ctrlpack_wgefa (&ar[ar_offset], &ai[ai_offset], lda, n, &ipvt[1],
		      &info);
  /* 
   *    rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) . 
   *    estimate = norm(z)/norm(y) where  a*z = y  and  ctrans(a)*y = e . 
   *    ctrans(a)  is the conjugate transpose of a . 
   *    the components of  e  are chosen to cause maximum local 
   *    growth in the elements of w  where  ctrans(u)*w = e . 
   *    the vectors are frequently rescaled to avoid overflow. 
   * 
   *    solve ctrans(u)*w = e 
   * 
   */
  ekr = 1.;
  eki = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      zr[j] = 0.;
      zi[j] = 0.;
      /* L20: */
    }
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      d__1 = -zr[k];
      d__2 = -zi[k];
      nsp_calpack_wsign (&ekr, &eki, &d__1, &d__2, &ekr, &eki);
      d__1 = ekr - zr[k];
      d__2 = eki - zi[k];
      if (Abs (d__1) + Abs (d__2) <=
	  (d__3 = ar[k + k * ar_dim1], Abs (d__3)) + (d__4 =
						      ai[k + k * ai_dim1],
						      Abs (d__4)))
	{
	  goto L40;
	}
      d__3 = ekr - zr[k];
      d__4 = eki - zi[k];
      s =
	((d__1 = ar[k + k * ar_dim1], Abs (d__1)) + (d__2 =
						     ai[k + k * ai_dim1],
						     Abs (d__2))) /
	(Abs (d__3) + Abs (d__4));
      nsp_calpack_wrscal (n, &s, &zr[1], &zi[1], &c__1);
      ekr = s * ekr;
      eki = s * eki;
    L40:
      wkr = ekr - zr[k];
      wki = eki - zi[k];
      wkmr = -ekr - zr[k];
      wkmi = -eki - zi[k];
      s = Abs (wkr) + Abs (wki);
      sm = Abs (wkmr) + Abs (wkmi);
      if ((d__1 = ar[k + k * ar_dim1], Abs (d__1)) + (d__2 =
						      ai[k + k * ai_dim1],
						      Abs (d__2)) == 0.)
	{
	  goto L50;
	}
      d__1 = -ai[k + k * ai_dim1];
      nsp_calpack_wdiv (&wkr, &wki, &ar[k + k * ar_dim1], &d__1, &wkr, &wki);
      d__1 = -ai[k + k * ai_dim1];
      nsp_calpack_wdiv (&wkmr, &wkmi, &ar[k + k * ar_dim1], &d__1, &wkmr,
			&wkmi);
      goto L60;
    L50:
      wkr = 1.;
      wki = 0.;
      wkmr = 1.;
      wkmi = 0.;
    L60:
      kp1 = k + 1;
      if (kp1 > *n)
	{
	  goto L100;
	}
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  d__1 = -ai[k + j * ai_dim1];
	  nsp_calpack_wmul (&wkmr, &wkmi, &ar[k + j * ar_dim1], &d__1, &tr,
			    &ti);
	  d__1 = zr[j] + tr;
	  d__2 = zi[j] + ti;
	  sm += Abs (d__1) + Abs (d__2);
	  d__1 = -ai[k + j * ai_dim1];
	  nsp_calpack_waxpy (&c__1, &wkr, &wki, &ar[k + j * ar_dim1], &d__1,
			     &c__1, &zr[j], &zi[j], &c__1);
	  s += (d__1 = zr[j], Abs (d__1)) + (d__2 = zi[j], Abs (d__2));
	  /* L70: */
	}
      if (s >= sm)
	{
	  goto L90;
	}
      tr = wkmr - wkr;
      ti = wkmi - wki;
      wkr = wkmr;
      wki = wkmi;
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  d__1 = -ai[k + j * ai_dim1];
	  nsp_calpack_waxpy (&c__1, &tr, &ti, &ar[k + j * ar_dim1], &d__1,
			     &c__1, &zr[j], &zi[j], &c__1);
	  /* L80: */
	}
    L90:
    L100:
      zr[k] = wkr;
      zi[k] = wki;
      /* L110: */
    }
  s = 1. / nsp_calpack_wasum (n, &zr[1], &zi[1], &c__1);
  nsp_calpack_wrscal (n, &s, &zr[1], &zi[1], &c__1);
  /* 
   *    solve ctrans(l)*y = w 
   * 
   */
  i__1 = *n;
  for (kb = 1; kb <= i__1; ++kb)
    {
      k = *n + 1 - kb;
      if (k >= *n)
	{
	  goto L120;
	}
      i__2 = *n - k;
      zr[k] +=
	nsp_calpack_wdotcr (&i__2, &ar[k + 1 + k * ar_dim1],
			    &ai[k + 1 + k * ai_dim1], &c__1, &zr[k + 1],
			    &zi[k + 1], &c__1);
      i__2 = *n - k;
      zi[k] +=
	nsp_calpack_wdotci (&i__2, &ar[k + 1 + k * ar_dim1],
			    &ai[k + 1 + k * ai_dim1], &c__1, &zr[k + 1],
			    &zi[k + 1], &c__1);
    L120:
      if ((d__1 = zr[k], Abs (d__1)) + (d__2 = zi[k], Abs (d__2)) <= 1.)
	{
	  goto L130;
	}
      s = 1. / ((d__1 = zr[k], Abs (d__1)) + (d__2 = zi[k], Abs (d__2)));
      nsp_calpack_wrscal (n, &s, &zr[1], &zi[1], &c__1);
    L130:
      l = ipvt[k];
      tr = zr[l];
      ti = zi[l];
      zr[l] = zr[k];
      zi[l] = zi[k];
      zr[k] = tr;
      zi[k] = ti;
      /* L140: */
    }
  s = 1. / nsp_calpack_wasum (n, &zr[1], &zi[1], &c__1);
  nsp_calpack_wrscal (n, &s, &zr[1], &zi[1], &c__1);
  /* 
   */
  ynorm = 1.;
  /* 
   *    solve l*v = y 
   * 
   */
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      l = ipvt[k];
      tr = zr[l];
      ti = zi[l];
      zr[l] = zr[k];
      zi[l] = zi[k];
      zr[k] = tr;
      zi[k] = ti;
      if (k < *n)
	{
	  i__2 = *n - k;
	  nsp_calpack_waxpy (&i__2, &tr, &ti, &ar[k + 1 + k * ar_dim1],
			     &ai[k + 1 + k * ai_dim1], &c__1, &zr[k + 1],
			     &zi[k + 1], &c__1);
	}
      if ((d__1 = zr[k], Abs (d__1)) + (d__2 = zi[k], Abs (d__2)) <= 1.)
	{
	  goto L150;
	}
      s = 1. / ((d__1 = zr[k], Abs (d__1)) + (d__2 = zi[k], Abs (d__2)));
      nsp_calpack_wrscal (n, &s, &zr[1], &zi[1], &c__1);
      ynorm = s * ynorm;
    L150:
      /* L160: */
      ;
    }
  s = 1. / nsp_calpack_wasum (n, &zr[1], &zi[1], &c__1);
  nsp_calpack_wrscal (n, &s, &zr[1], &zi[1], &c__1);
  ynorm = s * ynorm;
  /* 
   *    solve  u*z = v 
   * 
   */
  i__1 = *n;
  for (kb = 1; kb <= i__1; ++kb)
    {
      k = *n + 1 - kb;
      if ((d__1 = zr[k], Abs (d__1)) + (d__2 = zi[k], Abs (d__2)) <= (d__3 =
								      ar[k +
									 k *
									 ar_dim1],
								      Abs
								      (d__3))
	  + (d__4 = ai[k + k * ai_dim1], Abs (d__4)))
	{
	  goto L170;
	}
      s =
	((d__1 = ar[k + k * ar_dim1], Abs (d__1)) + (d__2 =
						     ai[k + k * ai_dim1],
						     Abs (d__2))) / ((d__3 =
								      zr[k],
								      Abs
								      (d__3))
								     + (d__4 =
									zi[k],
									Abs
									(d__4)));
      nsp_calpack_wrscal (n, &s, &zr[1], &zi[1], &c__1);
      ynorm = s * ynorm;
    L170:
      if ((d__1 = ar[k + k * ar_dim1], Abs (d__1)) + (d__2 =
						      ai[k + k * ai_dim1],
						      Abs (d__2)) == 0.)
	{
	  goto L180;
	}
      nsp_calpack_wdiv (&zr[k], &zi[k], &ar[k + k * ar_dim1],
			&ai[k + k * ai_dim1], &zr[k], &zi[k]);
    L180:
      if ((d__1 = ar[k + k * ar_dim1], Abs (d__1)) + (d__2 =
						      ai[k + k * ai_dim1],
						      Abs (d__2)) != 0.)
	{
	  goto L190;
	}
      zr[k] = 1.;
      zi[k] = 0.;
    L190:
      tr = -zr[k];
      ti = -zi[k];
      i__2 = k - 1;
      nsp_calpack_waxpy (&i__2, &tr, &ti, &ar[k * ar_dim1 + 1],
			 &ai[k * ai_dim1 + 1], &c__1, &zr[1], &zi[1], &c__1);
      /* L200: */
    }
  /*    make znorm = 1.0 
   */
  s = 1. / nsp_calpack_wasum (n, &zr[1], &zi[1], &c__1);
  nsp_calpack_wrscal (n, &s, &zr[1], &zi[1], &c__1);
  ynorm = s * ynorm;
  /* 
   */
  if (anorm != 0.)
    {
      *rcond = ynorm / anorm;
    }
  if (anorm == 0.)
    {
      *rcond = 0.;
    }
  return 0;
}				/* wgeco_ */
