/* MA02FD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Subroutine */ int
nsp_slicot_ma02fd (double *x1, double *x2, double *c__, double *s, int *info)
{
  /* System generated locals */
  double d__1;

  /* Builtin functions */
  double sqrt (double), d_sign (double *, double *);

  /* 
   *    SLICOT RELEASE 5.0. 
   * 
   *    Copyright (c) 2002-2009 NICONET e.V. 
   * 
   *    This program is free software: you can redistribute it and/or 
   *    modify it under the terms of the GNU General Public License as 
   *    published by the Free Software Foundation, either version 2 of 
   *    the License, or (at your option) any later version. 
   * 
   *    This program is distributed in the hope that it will be useful, 
   *    but WITHOUT ANY WARRANTY; without even the implied warranty of 
   *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
   *    GNU General Public License for more details. 
   * 
   *    You should have received a copy of the GNU General Public License 
   *    along with this program.  If not, see 
   *    <http://www.gnu.org/licenses/>. 
   * 
   *    PURPOSE 
   * 
   *    To compute the coefficients c and s (c^2 + s^2 = 1) for a modified 
   *    hyperbolic plane rotation, such that, 
   * 
   *        y1 := 1/c * x1 - s/c * x2 = sqrt(x1^2 - x2^2), 
   *        y2 :=  -s * y1 +  c  * x2 = 0, 
   * 
   *    given two real numbers x1 and x2, satisfying either x1 = x2 = 0, 
   *    or Abs(x2) < Abs(x1). 
   * 
   *    ARGUMENTS 
   * 
   *    Input/Output Parameters 
   * 
   *    X1      (input/output) DOUBLE PRECISION 
   *            On entry, the real number x1. 
   *            On exit, the real number y1. 
   * 
   *    X2      (input) DOUBLE PRECISION 
   *            The real number x2. 
   *            The values x1 and x2 should satisfy either x1 = x2 = 0, or 
   *            Abs(x2) < Abs(x1). 
   * 
   *    C       (output) DOUBLE PRECISION 
   *            The cosines c of the modified hyperbolic plane rotation. 
   * 
   *    S       (output) DOUBLE PRECISION 
   *            The sines s of the modified hyperbolic plane rotation. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  succesful exit; 
   *            = 1:  if Abs(x2) >= Abs(x1) and either x1 <> 0 or x2 <> 0. 
   * 
   *    CONTRIBUTOR 
   * 
   *    D. Kressner, Technical Univ. Chemnitz, Germany, June 2000. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, June 2000. 
   * 
   *    KEYWORDS 
   * 
   *    Orthogonal transformation, plane rotation. 
   * 
   *    ***************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Intrinsic Functions .. 
   *    .. Executable Statements .. 
   * 
   */
  if ((*x1 != 0. || *x2 != 0.) && Abs (*x2) >= Abs (*x1))
    {
      *info = 1;
    }
  else
    {
      *info = 0;
      if (*x1 == 0.)
	{
	  *s = 0.;
	  *c__ = 1.;
	}
      else
	{
	  *s = *x2 / *x1;
	  /* 
	   *          No overflows could appear in the next statement; underflows 
	   *          are possible if X2 is tiny and X1 is huge, but then 
	   *             Abs(C) = ONE - delta, 
	   *          where delta is much less than machine precision. 
	   * 
	   */
	  d__1 = sqrt (1. - *s) * sqrt (*s + 1.);
	  *c__ = d_sign (&d__1, x1);
	  *x1 = *c__ * *x1;
	}
    }
  /* 
   */
  return 0;
  /**** Last line of MA02FD *** 
   */
}				/* nsp_slicot_ma02fd */
