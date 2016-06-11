/* zbuni.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__2 = 2;
static int c__1 = 1;

/*DECK ZBUNI 
 */
int
nsp_calpack_zbuni (double *zr, double *zi, double *fnu, int *kode, int *n,
		   double *yr, double *yi, int *nz, int *nui, int *nlast,
		   double *fnul, double *tol, double *elim, double *alim)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double dfnu;
  double fnui;
  int i__, k, iflag;
  double ascle, csclr, cscrr;
  int iform;
  double ax, ay;
  int nl, nw;
  double c1i, c1m, c1r, s1i, s2i, s1r, s2r, cyi[2], gnu, raz, cyr[2], sti,
    bry[3], rzi, str, rzr;

  /****BEGIN PROLOGUE  ZBUNI 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to ZBESI and ZBESK 
   ****LIBRARY   SLATEC 
   ****TYPE      ALL (CBUNI-A, ZBUNI-A) 
   ****AUTHOR  Amos, D. E., (SNL) 
   ****DESCRIPTION 
   * 
   *    ZBUNI COMPUTES THE I BESSEL FUNCTION FOR LARGE ABS(Z).GT. 
   *    FNUL AND FNU+N-1.LT.FNUL. THE ORDER IS INCREASED FROM 
   *    FNU+N-1 GREATER THAN FNUL BY ADDING NUI AND COMPUTING 
   *    ACCORDING TO THE UNIFORM ASYMPTOTIC EXPANSION FOR I(FNU,Z) 
   *    ON IFORM=1 AND THE EXPANSION FOR J(FNU,Z) ON IFORM=2 
   * 
   ****SEE ALSO  ZBESI, ZBESK 
   ****ROUTINES CALLED  D1MACH, ZABS, ZUNI1, ZUNI2 
   ****REVISION HISTORY  (YYMMDD) 
   *  830501  DATE WRITTEN 
   *  910415  Prologue converted to Version 4.0 format.  (BAB) 
   ****END PROLOGUE  ZBUNI 
   *    COMPLEX CSCL,CSCR,CY,RZ,ST,S1,S2,Y,Z 
   ****FIRST EXECUTABLE STATEMENT  ZBUNI 
   */
  /* Parameter adjustments */
  --yi;
  --yr;

  /* Function Body */
  *nz = 0;
  ax = Abs (*zr) * 1.7321;
  ay = Abs (*zi);
  iform = 1;
  if (ay > ax)
    {
      iform = 2;
    }
  if (*nui == 0)
    {
      goto L60;
    }
  fnui = (double) (*nui);
  dfnu = *fnu + (*n - 1);
  gnu = dfnu + fnui;
  if (iform == 2)
    {
      goto L10;
    }
  /*----------------------------------------------------------------------- 
   *    ASYMPTOTIC EXPANSION FOR I(FNU,Z) FOR LARGE FNU APPLIED IN 
   *    -PI/3.LE.ARG(Z).LE.PI/3 
   *----------------------------------------------------------------------- 
   */
  nsp_calpack_zuni1 (zr, zi, &gnu, kode, &c__2, cyr, cyi, &nw, nlast, fnul,
		     tol, elim, alim);
  goto L20;
L10:
  /*----------------------------------------------------------------------- 
   *    ASYMPTOTIC EXPANSION FOR J(FNU,Z*EXP(M*HPI)) FOR LARGE FNU 
   *    APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I 
   *    AND HPI=PI/2 
   *----------------------------------------------------------------------- 
   */
  nsp_calpack_zuni2 (zr, zi, &gnu, kode, &c__2, cyr, cyi, &nw, nlast, fnul,
		     tol, elim, alim);
L20:
  if (nw < 0)
    {
      goto L50;
    }
  if (nw != 0)
    {
      goto L90;
    }
  str = nsp_calpack_zabs (cyr, cyi);
  /*---------------------------------------------------------------------- 
   *    SCALE BACKWARD RECURRENCE, BRY(3) IS DEFINED BUT NEVER USED 
   *---------------------------------------------------------------------- 
   */
  bry[0] = nsp_calpack_d1mach (&c__1) * 1e3 / *tol;
  bry[1] = 1. / bry[0];
  bry[2] = bry[1];
  iflag = 2;
  ascle = bry[1];
  csclr = 1.;
  if (str > bry[0])
    {
      goto L21;
    }
  iflag = 1;
  ascle = bry[0];
  csclr = 1. / *tol;
  goto L25;
L21:
  if (str < bry[1])
    {
      goto L25;
    }
  iflag = 3;
  ascle = bry[2];
  csclr = *tol;
L25:
  cscrr = 1. / csclr;
  s1r = cyr[1] * csclr;
  s1i = cyi[1] * csclr;
  s2r = cyr[0] * csclr;
  s2i = cyi[0] * csclr;
  raz = 1. / nsp_calpack_zabs (zr, zi);
  str = *zr * raz;
  sti = -(*zi) * raz;
  rzr = (str + str) * raz;
  rzi = (sti + sti) * raz;
  i__1 = *nui;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      str = s2r;
      sti = s2i;
      s2r = (dfnu + fnui) * (rzr * str - rzi * sti) + s1r;
      s2i = (dfnu + fnui) * (rzr * sti + rzi * str) + s1i;
      s1r = str;
      s1i = sti;
      fnui += -1.;
      if (iflag >= 3)
	{
	  goto L30;
	}
      str = s2r * cscrr;
      sti = s2i * cscrr;
      c1r = Abs (str);
      c1i = Abs (sti);
      c1m = Max (c1r, c1i);
      if (c1m <= ascle)
	{
	  goto L30;
	}
      ++iflag;
      ascle = bry[iflag - 1];
      s1r *= cscrr;
      s1i *= cscrr;
      s2r = str;
      s2i = sti;
      csclr *= *tol;
      cscrr = 1. / csclr;
      s1r *= csclr;
      s1i *= csclr;
      s2r *= csclr;
      s2i *= csclr;
    L30:
      ;
    }
  yr[*n] = s2r * cscrr;
  yi[*n] = s2i * cscrr;
  if (*n == 1)
    {
      return 0;
    }
  nl = *n - 1;
  fnui = (double) nl;
  k = nl;
  i__1 = nl;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      str = s2r;
      sti = s2i;
      s2r = (*fnu + fnui) * (rzr * str - rzi * sti) + s1r;
      s2i = (*fnu + fnui) * (rzr * sti + rzi * str) + s1i;
      s1r = str;
      s1i = sti;
      str = s2r * cscrr;
      sti = s2i * cscrr;
      yr[k] = str;
      yi[k] = sti;
      fnui += -1.;
      --k;
      if (iflag >= 3)
	{
	  goto L40;
	}
      c1r = Abs (str);
      c1i = Abs (sti);
      c1m = Max (c1r, c1i);
      if (c1m <= ascle)
	{
	  goto L40;
	}
      ++iflag;
      ascle = bry[iflag - 1];
      s1r *= cscrr;
      s1i *= cscrr;
      s2r = str;
      s2i = sti;
      csclr *= *tol;
      cscrr = 1. / csclr;
      s1r *= csclr;
      s1i *= csclr;
      s2r *= csclr;
      s2i *= csclr;
    L40:
      ;
    }
  return 0;
L50:
  *nz = -1;
  if (nw == -2)
    {
      *nz = -2;
    }
  return 0;
L60:
  if (iform == 2)
    {
      goto L70;
    }
  /*----------------------------------------------------------------------- 
   *    ASYMPTOTIC EXPANSION FOR I(FNU,Z) FOR LARGE FNU APPLIED IN 
   *    -PI/3.LE.ARG(Z).LE.PI/3 
   *----------------------------------------------------------------------- 
   */
  nsp_calpack_zuni1 (zr, zi, fnu, kode, n, &yr[1], &yi[1], &nw, nlast, fnul,
		     tol, elim, alim);
  goto L80;
L70:
  /*----------------------------------------------------------------------- 
   *    ASYMPTOTIC EXPANSION FOR J(FNU,Z*EXP(M*HPI)) FOR LARGE FNU 
   *    APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I 
   *    AND HPI=PI/2 
   *----------------------------------------------------------------------- 
   */
  nsp_calpack_zuni2 (zr, zi, fnu, kode, n, &yr[1], &yi[1], &nw, nlast, fnul,
		     tol, elim, alim);
L80:
  if (nw < 0)
    {
      goto L50;
    }
  *nz = nw;
  return 0;
L90:
  *nlast = *n;
  return 0;
}				/* zbuni_ */
#endif
