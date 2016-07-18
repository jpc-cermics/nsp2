/* coef.f -- translated by f2c (version 19961017). */

#include "ctrlpack.h"

/*
 *    coef compute the lengh,and the coefficients of the 
 *    exponential pade approximant 
 *
 *    subroutine coef(ierr) 
 *    common /dcoeff/ b,n 
 * 
 *    double precision b(41) 
 *    int n,ierr 
 *    ierr error indicator : if ierr.ne.0 n is too large 
 *         machine precision can't be achieved 
 * 
 *    b    array containing pade coefficients 
 * 
 *    n    lengh of pade approximation 
 * 
 *     j.roche  - laboratoire d'automatique de grenoble 
 */

pade_data pade_dcoeff;

#define dcoeff_1 pade_dcoeff

int nsp_ctrlpack_coef (int *ierr)
{
  static double zero = 0.;
  static double one = 1.;
  static double two = 2.;
  static double cnst = .55693;
  static double half = .5;

  
  int i__1;
  double d__1;

  
  double a[41];
  int i__, j, k, m[21];
  double b1, b2, b3;
  int j1, n1, n2, id, ie, ir, im1, ip1;

  /* 
   */
  *ierr = 0;
  /* 
   *  determination of the pade approximants type 
   * 
   */
  dcoeff_1.n = 1;
  b1 = exp (one);
  b3 = 6.;
  b2 = b1 / (b3 * (cnst - one));
  b2 = Abs (b2);
 L10:
  if (b2 + one <= one)
    {
      goto L20;
    }
  ++dcoeff_1.n;
  b3 *= (double) dcoeff_1.n * 4. + two;
  d__1 = (double) dcoeff_1.n * cnst - one;
  b2 = b1 / (b3 * nsp_pow_di (d__1, dcoeff_1.n));
  goto L10;
 L20:
  if (dcoeff_1.n > 40)
    {
      *ierr = dcoeff_1.n;
    }
  dcoeff_1.n = Min (dcoeff_1.n, 40);
  /* 
   *  compute the coefficients of pade approximants 
   * 
   */
  n1 = dcoeff_1.n + 1;
  n2 = (dcoeff_1.n + 2) / 2;
  a[0] = one;
  a[1] = half;
  i__1 = dcoeff_1.n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      im1 = i__ - 1;
      ip1 = i__ + 1;
      a[ip1 - 1] =
	a[i__ - 1] * (double) (dcoeff_1.n -
			       im1) / (double) (i__ * ((dcoeff_1.n << 1) -
						       im1));
      /* L30: */
    }
  /* 
   *  compute the coefficients of pade approximants in chebychef system 
   * 
   */
  i__1 = n2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      m[i__ - 1] = 0;
      /* L40: */
    }
  i__1 = n1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      dcoeff_1.b[i__ - 1] = zero;
      /* L50: */
    }
  m[0] = 1;
  dcoeff_1.b[0] = a[0];
  dcoeff_1.b[1] = a[1];
  i__ = 0;
  b3 = one;
 L60:
  ++i__;
  b3 *= half;
  ir = i__ % 2;
  id = (i__ + 3) / 2;
  ie = id;
  if (ir == 0)
    {
      goto L70;
    }
  else
    {
      goto L80;
    }
 L70:
  m[id - 1] += m[id - 1];
 L80:
  m[id - 1] += m[id - 2];
  --id;
  if (id - 1 == 0)
    {
      goto L90;
    }
  goto L80;
 L90:
  j = i__ + 2;
  j1 = j;
  i__1 = ie;
  for (k = 1; k <= i__1; ++k)
    {
      b2 = (double) m[k - 1];
      b1 = a[j1 - 1] * b2 * b3;
      dcoeff_1.b[j - 1] += b1;
      j += -2;
      /* L100: */
    }
  if (n1 - i__ != 2)
    {
      goto L60;
    }
  return 0;
}				/* coef_ */
