/* wtan.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/* Table of constant values */

static double c_b3 = 1.;

int nsp_calpack_wtan (double *xr, double *xi, double *yr, double *yi)
{
  /* Initialized data */

  static int first = TRUE;

  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double d__;
  double si, sr;
  static double lim;

  /* 
   *    PURPOSE 
   *       wtan compute the tangent of a complex number 
   *       y = yr + i yi = tan(x), x = xr + i xi 
   * 
   *    CALLING LIST / PARAMETERS 
   *       subroutine wtan(xr,xi,yr,yi) 
   *       double precision xr,xi,yr,yi 
   * 
   *       xr,xi: real and imaginary parts of the complex number 
   *       yr,yi: real and imaginary parts of the result 
   *              yr,yi may have the same memory cases than xr et xi 
   * 
   *    ALGORITHM 
   *       based on the formula : 
   * 
   *                        0.5 sin(2 xr) +  i 0.5 sinh(2 xi) 
   *       tan(xr + i xi) = --------------------------------- 
   *                            cos(xr)^2  + sinh(xi)^2 
   * 
   *       noting  d = cos(xr)^2 + sinh(xi)^2, we have : 
   * 
   *               yr = 0.5 * sin(2 * xr) / d       (1) 
   * 
   *               yi = 0.5 * sinh(2 * xi) / d      (2) 
   * 
   *       to avoid spurious overflows in computing yi with 
   *       formula (2) (which results in NaN for yi) 
   *       we use also the following formula : 
   * 
   *               yi = sign(xi)   when |xi| > LIM  (3) 
   * 
   *       Explanations for (3) : 
   * 
   *       we have d = sinh(xi)^2 ( 1 + (cos(xr)/sinh(xi))^2 ), 
   *       so when : 
   * 
   *          (cos(xr)/sinh(xi))^2 < epsm   ( epsm = max relative error 
   *                                         for coding a real in a f.p. 
   *                                         number set F(b,p,emin,emax) 
   *                                            epsm = 0.5 b^(1-p) ) 
   *       which is forced  when : 
   * 
   *           1/sinh(xi)^2 < epsm   (4) 
   *       <=> |xi| > asinh(1/sqrt(epsm)) (= 19.06... in ieee 754 double) 
   * 
   *       sinh(xi)^2 is a good approximation for d (relative to the f.p. 
   *       arithmetic used) and then yr may be approximate with : 
   * 
   *        yr = cosh(xi)/sinh(xi) 
   *           = sign(xi) (1 + exp(-2 |xi|))/(1 - exp(-2|xi|)) 
   *           = sign(xi) (1 + 2 u + 2 u^2 + 2 u^3 + ...) 
   * 
   *       with u = exp(-2 |xi|)). Now when : 
   * 
   *           2 exp(-2|xi|) < epsm  (2) 
   *       <=> |xi| > 0.5 * log(2/epsm) (= 18.71... in ieee 754 double) 
   * 
   *       sign(xi)  is a good approximation for yr. 
   * 
   *       Constraint (1) is stronger than (2) and we take finaly 
   * 
   *       LIM = 1 + log(2/sqrt(epsm)) 
   * 
   *       (log(2/sqrt(epsm)) being very near asinh(1/sqrt(epsm)) 
   * 
   *    AUTHOR 
   *       Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> 
   * 
   *    PARAMETER 
   *    LOCAL VAR 
   *    EXTERNAL 
   *    STATIC VAR 
   */
  if (first)
    {
      /*       epsm is gotten with dlamch('e') 
       */
      lim = log (2. / sqrt (C2F (dlamch) ("e", 1L))) + 1.;
      first = FALSE;
    }
  /*    (0) avoid memory pb ... 
   */
  sr = *xr;
  si = *xi;
  /*    (1) go on .... 
   *Computing 2nd power 
   */
  d__1 = cos (sr);
  /*Computing 2nd power 
   */
  d__2 = sinh (si);
  d__ = d__1 * d__1 + d__2 * d__2;
  *yr = sin (sr * 2.) * .5 / d__;
  if (Abs (si) < lim)
    {
      *yi = sinh (si * 2.) * .5 / d__;
    }
  else
    {
      *yi = d_sign (&c_b3, &si);
    }
  return 0;
}				/* wtan_ */
