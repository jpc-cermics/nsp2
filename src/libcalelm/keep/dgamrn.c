/* dgamrn.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__4 = 4;
static int c__14 = 14;
static int c__5 = 5;

/*DECK DGAMRN 
 */
double nsp_calpack_dgamrn (double *x)
{
  /* Initialized data */

  static double gr[12] =
    { 1., -.015625, .0025634765625, -.0012798309326171875,
    .00134351104497909546, -.00243289663922041655, .00675423753364157164,
    -.0266369606131178216,
    .141527455519564332, -.974384543032201613, 8.43686251229783675,
    -89.7258321640552515
  };

  /* System generated locals */
  int i__1;
  double ret_val, d__1;

  /* Local variables */
  double xinc, xmin, xdmy;
  int i__, k;
  double s;
  int mx, nx;
  double xm, xp, fln, rln, tol, trm, xsq;
  int i1m11;

  /****BEGIN PROLOGUE  DGAMRN 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to DBSKIN 
   ****LIBRARY   SLATEC 
   ****TYPE      DOUBLE PRECISION (GAMRN-S, DGAMRN-D) 
   ****AUTHOR  Amos, D. E., (SNLA) 
   ****DESCRIPTION 
   * 
   *    Abstract   * A Double Precision Routine * 
   *        DGAMRN computes the GAMMA function ratio GAMMA(X)/GAMMA(X+0.5) 
   *        for real X.gt.0. If X.ge.XMIN, an asymptotic expansion is 
   *        evaluated. If X.lt.XMIN, an int is added to X to form a 
   *        new value of X.ge.XMIN and the asymptotic expansion is eval- 
   *        uated for this new value of X. Successive application of the 
   *        recurrence relation 
   * 
   *                     W(X)=W(X+1)*(1+0.5/X) 
   * 
   *        reduces the argument to its original value. XMIN and comp- 
   *        utational tolerances are computed as a function of the number 
   *        of digits carried in a word by calls to I1MACH and D1MACH. 
   *        However, the computational accuracy is limited to the max- 
   *        imum of unit roundoff (=D1MACH(4)) and 1.0D-18 since critical 
   *        constants are given to only 18 digits. 
   * 
   *        Input      X is Double Precision 
   *          X      - Argument, X.gt.0.0D0 
   * 
   *        Output      DGAMRN is DOUBLE PRECISION 
   *          DGAMRN  - Ratio  GAMMA(X)/GAMMA(X+0.5) 
   * 
   ****SEE ALSO  DBSKIN 
   ****REFERENCES  Y. L. Luke, The Special Functions and Their 
   *                Approximations, Vol. 1, Math In Sci. And 
   *                Eng. Series 53, Academic Press, New York, 1969, 
   *                pp. 34-35. 
   ****ROUTINES CALLED  D1MACH, I1MACH 
   ****REVISION HISTORY  (YYMMDD) 
   *  820601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900328  Added TYPE section.  (WRB) 
   *  910722  Updated AUTHOR section.  (ALS) 
   *  920520  Added REFERENCES section.  (WRB) 
   ****END PROLOGUE  DGAMRN 
   * 
   */
  /* 
   ****FIRST EXECUTABLE STATEMENT  DGAMRN 
   */
  nx = (int) (*x);
  /*Computing MAX 
   */
  d__1 = nsp_calpack_d1mach (&c__4);
  tol = Max (d__1, 1e-18);
  i1m11 = nsp_calpack_i1mach (&c__14);
  rln = nsp_calpack_d1mach (&c__5) * i1m11;
  fln = Min (rln, 20.);
  fln = Max (fln, 3.);
  fln += -3.;
  xm = fln * (fln * .01723 + .2366) + 2.;
  mx = (int) xm + 1;
  xmin = (double) mx;
  xdmy = *x - .25;
  xinc = 0.;
  if (*x >= xmin)
    {
      goto L10;
    }
  xinc = xmin - nx;
  xdmy += xinc;
L10:
  s = 1.;
  if (xdmy * tol > 1.)
    {
      goto L30;
    }
  xsq = 1. / (xdmy * xdmy);
  xp = xsq;
  for (k = 2; k <= 12; ++k)
    {
      trm = gr[k - 1] * xp;
      if (Abs (trm) < tol)
	{
	  goto L30;
	}
      s += trm;
      xp *= xsq;
      /* L20: */
    }
L30:
  s /= sqrt (xdmy);
  if (xinc != 0.)
    {
      goto L40;
    }
  ret_val = s;
  return ret_val;
L40:
  nx = (int) xinc;
  xp = 0.;
  i__1 = nx;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s *= .5 / (*x + xp) + 1.;
      xp += 1.;
      /* L50: */
    }
  ret_val = s;
  return ret_val;
}				/* dgamrn_ */
#endif
