/* dgeco.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *    dgeco factors a double precision matrix by gaussian elimination 
 *    and estimates the condition of the matrix. 
 * 
 *    if  rcond  is not needed, dgefa is slightly faster. 
 *    to solve  a*x = b , follow dgeco by dgesl. 
 *    to compute  inverse(a)*c , follow dgeco by dgesl. 
 *    to compute  determinant(a) , follow dgeco by dgedi. 
 *    to compute  inverse(a) , follow dgeco by dgedi. 
 * 
 *     subroutine dgeco(a,lda,n,ipvt,rcond,z) 
 *    on entry 
 *       a       double precision(lda, n) 
 *               the matrix to be factored. 
 *       lda     int 
 *               the leading dimension of the array  a . 
 *       n       int 
 *               the order of the matrix  a . 
 *    on return 
 *       a       an upper triangular matrix and the multipliers 
 *               which were used to obtain it. 
 *               the factorization can be written  a = l*u  where 
 *               l  is a product of permutation and unit lower 
 *               triangular matrices and  u  is upper triangular. 
 *       ipvt    int(n) 
 *               an int vector of pivot indices. 
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
 *       z       double precision(n) 
 *               a work vector whose contents are usually unimportant. 
 *               if  a  is close to a singular matrix, then  z  is 
 *               an approximate null vector in the sense that 
 *               norm(a*z) = rcond*norm(a)*norm(z) . 
 * 
 *    linpack. this version dated 08/14/78 . 
 *    cleve moler, university of new mexico, argonne national lab. 
 */

static int c__1 = 1;

int
nsp_ctrlpack_dgeco (double *a, int *lda, int *n, int *ipvt, double *rcond,
		    double *z__)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;
  double d__1, d__2;

  /* Builtin functions */
  double d_sign (double *, double *);

  /* Local variables */
  int info;
  int j, k, l;
  double s, t;
  double anorm;
  double ynorm;
  int kb;
  double ek, sm, wk;
  int kp1;
  double wkm;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;
  --z__;

  /* Function Body */
  anorm = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      /*Computing MAX 
       */
      d__1 = anorm, d__2 = C2F (dasum) (n, &a[j * a_dim1 + 1], &c__1);
      anorm = Max (d__1, d__2);
      /* L10: */
    }
  /* 
   *    factor 
   * 
   */
  nsp_ctrlpack_dgefa (&a[a_offset], lda, n, &ipvt[1], &info);
  /* 
   *    rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) . 
   *    estimate = norm(z)/norm(y) where  a*z = y  and  trans(a)*y = e . 
   *    trans(a)  is the transpose of a .  the components of  e  are 
   *    chosen to cause maximum local growth in the elements of w  where 
   *    trans(u)*w = e .  the vectors are frequently rescaled to avoid 
   *    overflow. 
   * 
   *    solve trans(u)*w = e 
   * 
   */
  ek = 1.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      z__[j] = 0.;
      /* L20: */
    }
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      if (z__[k] != 0.)
	{
	  d__1 = -z__[k];
	  ek = d_sign (&ek, &d__1);
	}
      if ((d__1 = ek - z__[k], Abs (d__1)) <= (d__2 =
					       a[k + k * a_dim1], Abs (d__2)))
	{
	  goto L30;
	}
      s = (d__1 = a[k + k * a_dim1], Abs (d__1)) / (d__2 =
						    ek - z__[k], Abs (d__2));
      C2F (dscal) (n, &s, &z__[1], &c__1);
      ek = s * ek;
    L30:
      wk = ek - z__[k];
      wkm = -ek - z__[k];
      s = Abs (wk);
      sm = Abs (wkm);
      if (a[k + k * a_dim1] == 0.)
	{
	  goto L40;
	}
      wk /= a[k + k * a_dim1];
      wkm /= a[k + k * a_dim1];
      goto L50;
    L40:
      wk = 1.;
      wkm = 1.;
    L50:
      kp1 = k + 1;
      if (kp1 > *n)
	{
	  goto L90;
	}
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  sm += (d__1 = z__[j] + wkm * a[k + j * a_dim1], Abs (d__1));
	  z__[j] += wk * a[k + j * a_dim1];
	  s += (d__1 = z__[j], Abs (d__1));
	  /* L60: */
	}
      if (s >= sm)
	{
	  goto L80;
	}
      t = wkm - wk;
      wk = wkm;
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  z__[j] += t * a[k + j * a_dim1];
	  /* L70: */
	}
    L80:
    L90:
      z__[k] = wk;
      /* L100: */
    }
  s = 1. / C2F (dasum) (n, &z__[1], &c__1);
  C2F (dscal) (n, &s, &z__[1], &c__1);
  /* 
   *    solve trans(l)*y = w 
   * 
   */
  i__1 = *n;
  for (kb = 1; kb <= i__1; ++kb)
    {
      k = *n + 1 - kb;
      if (k < *n)
	{
	  i__2 = *n - k;
	  z__[k] +=
	    C2F (ddot) (&i__2, &a[k + 1 + k * a_dim1], &c__1,
			&z__[k + 1], &c__1);
	}
      if ((d__1 = z__[k], Abs (d__1)) <= 1.)
	{
	  goto L110;
	}
      s = 1. / (d__1 = z__[k], Abs (d__1));
      C2F (dscal) (n, &s, &z__[1], &c__1);
    L110:
      l = ipvt[k];
      t = z__[l];
      z__[l] = z__[k];
      z__[k] = t;
      /* L120: */
    }
  s = 1. / C2F (dasum) (n, &z__[1], &c__1);
  C2F (dscal) (n, &s, &z__[1], &c__1);
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
      t = z__[l];
      z__[l] = z__[k];
      z__[k] = t;
      if (k < *n)
	{
	  i__2 = *n - k;
	  C2F (daxpy) (&i__2, &t, &a[k + 1 + k * a_dim1], &c__1,
		       &z__[k + 1], &c__1);
	}
      if ((d__1 = z__[k], Abs (d__1)) <= 1.)
	{
	  goto L130;
	}
      s = 1. / (d__1 = z__[k], Abs (d__1));
      C2F (dscal) (n, &s, &z__[1], &c__1);
      ynorm = s * ynorm;
    L130:
      /* L140: */
      ;
    }
  s = 1. / C2F (dasum) (n, &z__[1], &c__1);
  C2F (dscal) (n, &s, &z__[1], &c__1);
  ynorm = s * ynorm;
  /* 
   *    solve  u*z = v 
   * 
   */
  i__1 = *n;
  for (kb = 1; kb <= i__1; ++kb)
    {
      k = *n + 1 - kb;
      if ((d__1 = z__[k], Abs (d__1)) <= (d__2 =
					  a[k + k * a_dim1], Abs (d__2)))
	{
	  goto L150;
	}
      s = (d__1 = a[k + k * a_dim1], Abs (d__1)) / (d__2 =
						    z__[k], Abs (d__2));
      C2F (dscal) (n, &s, &z__[1], &c__1);
      ynorm = s * ynorm;
    L150:
      if (a[k + k * a_dim1] != 0.)
	{
	  z__[k] /= a[k + k * a_dim1];
	}
      if (a[k + k * a_dim1] == 0.)
	{
	  z__[k] = 1.;
	}
      t = -z__[k];
      i__2 = k - 1;
      C2F (daxpy) (&i__2, &t, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
      /* L160: */
    }
  /*    make znorm = 1.0 
   */
  s = 1. / C2F (dasum) (n, &z__[1], &c__1);
  C2F (dscal) (n, &s, &z__[1], &c__1);
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
}				/* dgeco_ */
