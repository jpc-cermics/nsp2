/* dbkisr.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__4 = 4;
static int c__3 = 3;
static int c__2 = 2;
static int c__1 = 1;

/*DECK DBKISR 
 */
int nsp_calpack_dbkisr (double *x, int *n, double *sum, int *ierr)
{
  /* Initialized data */

  static double c__[2] = { 1.57079632679489662, 1. };

  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  double atol;
  int i__, k, k1;
  double ak, bk, fk, fn;
  int kk, np;
  double hx, pr;
  int kkn;
  double pol, tkp, tol, xln, hxs, trm;

  /****BEGIN PROLOGUE  DBKISR 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to DBSKIN 
   ****LIBRARY   SLATEC 
   ****TYPE      DOUBLE PRECISION (BKISR-S, DBKISR-D) 
   ****AUTHOR  Amos, D. E., (SNLA) 
   ****DESCRIPTION 
   * 
   *    DBKISR computes repeated integrals of the K0 Bessel function 
   *    by the series for N=0,1, and 2. 
   * 
   ****SEE ALSO  DBSKIN 
   ****ROUTINES CALLED  D1MACH, DPSIXN 
   ****REVISION HISTORY  (YYMMDD) 
   *  820601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900328  Added TYPE section.  (WRB) 
   *  910722  Updated AUTHOR section.  (ALS) 
   ****END PROLOGUE  DBKISR 
   * 
   */
  /****FIRST EXECUTABLE STATEMENT  DBKISR 
   */
  *ierr = 0;
  /*Computing MAX 
   */
  d__1 = nsp_calpack_d1mach (&c__4);
  tol = Max (d__1, 1e-18);
  if (*x < tol)
    {
      goto L50;
    }
  pr = 1.;
  pol = 0.;
  if (*n == 0)
    {
      goto L20;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      pol = -pol * *x + c__[i__ - 1];
      pr = pr * *x / i__;
      /* L10: */
    }
L20:
  hx = *x * .5;
  hxs = hx * hx;
  xln = log (hx);
  np = *n + 1;
  tkp = 3.;
  fk = 2.;
  fn = (double) (*n);
  bk = 4.;
  ak = 2. / ((fn + 1.) * (fn + 2.));
  i__1 = *n + 3;
  *sum =
    ak * (nsp_calpack_dpsixn (&i__1) - nsp_calpack_dpsixn (&c__3) +
	  nsp_calpack_dpsixn (&c__2) - xln);
  atol = *sum * tol * .75;
  for (k = 2; k <= 20; ++k)
    {
      ak =
	ak * (hxs / bk) * ((tkp + 1.) / (tkp + fn + 1.)) * (tkp / (tkp + fn));
      k1 = k + 1;
      kk = k1 + k;
      kkn = kk + *n;
      trm =
	(nsp_calpack_dpsixn (&k1) + nsp_calpack_dpsixn (&kkn) -
	 nsp_calpack_dpsixn (&kk) - xln) * ak;
      *sum += trm;
      if (Abs (trm) <= atol)
	{
	  goto L40;
	}
      tkp += 2.;
      bk += tkp;
      fk += 1.;
      /* L30: */
    }
  goto L80;
L40:
  *sum = (*sum * hxs + nsp_calpack_dpsixn (&np) - xln) * pr;
  if (*n == 1)
    {
      *sum = -(*sum);
    }
  *sum = pol + *sum;
  return 0;
  /*----------------------------------------------------------------------- 
   *    SMALL X CASE, X.LT.WORD TOLERANCE 
   *----------------------------------------------------------------------- 
   */
L50:
  if (*n > 0)
    {
      goto L60;
    }
  hx = *x * .5;
  *sum = nsp_calpack_dpsixn (&c__1) - log (hx);
  return 0;
L60:
  *sum = c__[*n - 1];
  return 0;
L80:
  *ierr = 2;
  return 0;
}				/* dbkisr_ */

#endif
