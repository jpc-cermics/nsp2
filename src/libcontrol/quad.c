/* quad.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

int
nsp_ctrlpack_quad (double *a, double *b1, double *c__, double *sr, double *si,
		   double *lr, double *li)
{
  /* System generated locals */
  double d__1;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  double b, d__, e;

  /*calculate the zeros of the quadratic a*z**2+b1*z+c. 
   *the quadratic formula, modified to avoid 
   *overflow, is used to find the larger zero if the 
   *zeros are real and both zeros are complex 
   *the smaller real zero is found directly from the 
   *product of the zero c/a. 
   */
  if (*a != 0.)
    {
      goto L20;
    }
  *sr = 0.;
  if (*b1 != 0.)
    {
      *sr = -(*c__) / *b1;
    }
  *lr = 0.;
 L10:
  *si = 0.;
  *li = 0.;
  return 0;
 L20:
  if (*c__ != 0.)
    {
      goto L30;
    }
  *sr = 0.;
  *lr = -(*b1) / *a;
  goto L10;
  /*compute discriminant avoiding overflow 
   */
 L30:
  b = *b1 / 2.;
  if (Abs (b) < Abs (*c__))
    {
      goto L40;
    }
  e = 1. - *a / b * (*c__ / b);
  d__ = sqrt ((Abs (e))) * Abs (b);
  goto L50;
 L40:
  e = *a;
  if (*c__ < 0.)
    {
      e = -(*a);
    }
  e = b * (b / Abs (*c__)) - e;
  d__ = sqrt ((Abs (e))) * sqrt ((Abs (*c__)));
 L50:
  if (e < 0.)
    {
      goto L60;
    }
  /*real zeros 
   */
  if (b >= 0.)
    {
      d__ = -d__;
    }
  *lr = (-b + d__) / *a;
  *sr = 0.;
  if (*lr != 0.)
    {
      *sr = *c__ / *lr / *a;
    }
  goto L10;
  /*complex conjugate zeros 
   */
 L60:
  *sr = -b / *a;
  *lr = *sr;
  *si = (d__1 = d__ / *a, Abs (d__1));
  *li = -(*si);
  return 0;
}				/* quad_ */
