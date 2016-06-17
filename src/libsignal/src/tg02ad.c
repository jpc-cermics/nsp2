/* tg02ad.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/* Common Block Declarations */

struct
{
  int k;
} tg02bd_;

#define tg02bd_1 tg02bd_

/* Table of constant values */

static double c_b10 = 2.;

/*/MEMBR ADD NAME=TG02AD,SSI=0 
 */
/* Subroutine */ int
signal_tg02_ad (int *ix, int *n, double *u, double *s, double *d__, double *x,
		double *v)
{
  /* Initialized data */

  static int iflg = 0;
  static int ieps = -30;

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2, d__3;


  /* Local variables */
  double gama, a, b, c__, h__;
  int i__, j;
  double t, theta, d0, d1, c3, s0, s1, hr, phi, hrr;

  /*!but 
   *     tg02ad - a routine to compute function values and 1st, 2nd 
   *     and 3rd derivative values of a cubic spline.  the spline 
   *     is defined by its values and 1st derivative values at the 
   *     knots. 
   * 
   *     the values of the spline and its derivatives are set to be 
   *     zero if x should fall outside the knot range plus or minus 
   *     a rounding error. 
   *!calling sequence 
   *     ix    set by the user to specify the method of estimating 
   *           the knot interval containing x.  if ix<0 the 
   *           initial guess is calculated on the assumption that the 
   *           knots are equally spaced.  if ix>=0 it assumes that 
   *           the current x is very near the x of the previous 
   *           entry and that it is the same spline. 
   *     n,u(i),s(i),d(i) i=1,n; define the spline and are set by 
   *           the user to the number of knots, the knots, the spline 
   *           values at the knots and the 1st derivative values 
   *           at the knots.  the knots u(i) i=1,n must be ordered. 
   *     x     the point at which the spline value is required. 
   *     v(i) i=1,4; set by the routine to the values of the spline 
   *           and its first three derivatives at the point x. 
   *! 
   * 
   * 
   *     k     returned set to the knot interval that 
   *           contained x, i.e. u(k)<=x<=u(k+1). 
   * 
   *     allowable rounding error on points at extreams 
   *     of knot range is 2**ieps*max(|u(1)|,|u(n)|). 
   */
  /* Parameter adjustments */
  --v;
  --d__;
  --s;
  --u;

  /* Function Body */
  /* 
   *     test whether point in range. 
   */
  j = 0;
  tg02bd_1.k = 0;
  if (*x < u[1])
    {
      goto L990;
    }
  if (*x > u[*n])
    {
      goto L991;
    }
  /* 
   *     jump if knot interval requires random search. 
   */
  if (*ix < 0 || iflg == 0)
    {
      goto L12;
    }
  /*     test if knot interval same as last time. 
   */
  if (*x > u[j + 1])
    {
      goto L1;
    }
  if (*x >= u[j])
    {
      goto L18;
    }
  goto L2;
  /*         loop till interval found. 
   */
 L1:
  ++j;
 L11:
  if (*x > u[j + 1])
    {
      goto L1;
    }
  goto L7;
  /* 
   *         estimate knot interval by assuming equally spaced knots. 
   */
 L12:
  j = (int) ((d__1 = *x - u[1], Abs (d__1)) / (u[*n] - u[1]) * (*n - 1) + 1);
  /*         ensure case x=u(n) gives j=n-1. 
   *Computing MIN 
   */
  i__1 = j, i__2 = *n - 1;
  j = Min (i__1, i__2);
  /*         indicate that knot interval inside range has been used. 
   */
  iflg = 1;
  /*         search for knot interval containing x. 
   */
  if (*x >= u[j])
    {
      goto L11;
    }
 L2:
  --j;
  if (*x < u[j])
    {
      goto L2;
    }
  /* 
   *         calculate spline parameters for jth interval. 
   * 
   *     knot interval 
   */
 L7:
  tg02bd_1.k = j;
  h__ = u[j + 1] - u[j];
  hr = 1. / h__;
  hrr = (hr + hr) * hr;
  /* 
   */
  s0 = s[j];
  s1 = s[j + 1];
  d0 = d__[j];
  d1 = d__[j + 1];
  a = s1 - s0;
  b = a - h__ * d1;
  a -= h__ * d0;
  c__ = a + b;
  c3 = c__ * 3.;
  /*     transform the variable 
   */
 L18:
  theta = (*x - u[j]) * hr;
  phi = 1. - theta;
  t = theta * phi;
  gama = theta * b - phi * a;
  /*     spline value 
   */
  v[1] = theta * s1 + phi * s0 + t * gama;
  /*     1st derivative value 
   */
  v[2] = theta * d1 + phi * d0 + t * c3 * hr;
  /*     2nd derivative value 
   */
  v[3] = (c__ * (phi - theta) - gama) * hrr;
  /*     3rd derivative value 
   */
  v[4] = -c3 * hrr * hr;
  return 0;
  /*         test if x within rounding error of u(1). 
   */
 L990:
  /*Computing MAX 
   */
  d__2 = Abs (u[1]), d__3 = (d__1 = u[*n], Abs (d__1));
  if (*x <= u[1] - nsp_pow_di (c_b10, ieps) * Max (d__2, d__3))
    {
      goto L99;
    }
  j = 1;
  goto L7;
  /*         test if x within rounding error of u(n). 
   */
 L991:
  /*Computing MAX 
   */
  d__2 = Abs (u[1]), d__3 = (d__1 = u[*n], Abs (d__1));
  if (*x >= u[*n] + nsp_pow_di (c_b10, ieps) * Max (d__2, d__3))
    {
      goto L995;
    }
  j = *n - 1;
  goto L7;
 L995:
  tg02bd_1.k = *n;
 L99:
  iflg = 0;
  /*         values set to zero for points outside the range 
   */
  /* L5: */
  for (i__ = 1; i__ <= 4; ++i__)
    {
      /* L6: */
      v[i__] = 0.;
    }
  return 0;
}				/* tg02ad_ */
