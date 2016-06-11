/* zbinu.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__2 = 2;

/*DECK ZBINU 
 */
int
nsp_calpack_zbinu (double *zr, double *zi, double *fnu, int *kode, int *n,
		   double *cyr, double *cyi, int *nz, double *rl,
		   double *fnul, double *tol, double *elim, double *alim)
{
  /* Initialized data */

  static double zeror = 0.;
  static double zeroi = 0.;

  /* System generated locals */
  int i__1;

  /* Local variables */
  double dfnu;
  int i__, nlast;
  double az;
  int nn, nw;
  double cwi[2], cwr[2];
  int nui, inw;

  /****BEGIN PROLOGUE  ZBINU 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to ZAIRY, ZBESH, ZBESI, ZBESJ, ZBESK and ZBIRY 
   ****LIBRARY   SLATEC 
   ****TYPE      ALL (CBINU-A, ZBINU-A) 
   ****AUTHOR  Amos, D. E., (SNL) 
   ****DESCRIPTION 
   * 
   *    ZBINU COMPUTES THE I FUNCTION IN THE RIGHT HALF Z PLANE 
   * 
   ****SEE ALSO  ZAIRY, ZBESH, ZBESI, ZBESJ, ZBESK, ZBIRY 
   ****ROUTINES CALLED  ZABS, ZASYI, ZBUNI, ZMLRI, ZSERI, ZUOIK, ZWRSK 
   ****REVISION HISTORY  (YYMMDD) 
   *  830501  DATE WRITTEN 
   *  910415  Prologue converted to Version 4.0 format.  (BAB) 
   ****END PROLOGUE  ZBINU 
   */
  /* Parameter adjustments */
  --cyi;
  --cyr;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  ZBINU 
   */
  *nz = 0;
  az = nsp_calpack_zabs (zr, zi);
  nn = *n;
  dfnu = *fnu + (*n - 1);
  if (az <= 2.)
    {
      goto L10;
    }
  if (az * az * .25 > dfnu + 1.)
    {
      goto L20;
    }
L10:
  /*----------------------------------------------------------------------- 
   *    POWER SERIES 
   *----------------------------------------------------------------------- 
   */
  nsp_calpack_zseri (zr, zi, fnu, kode, &nn, &cyr[1], &cyi[1], &nw, tol,
		     elim, alim);
  inw = Abs (nw);
  *nz += inw;
  nn -= inw;
  if (nn == 0)
    {
      return 0;
    }
  if (nw >= 0)
    {
      goto L120;
    }
  dfnu = *fnu + (nn - 1);
L20:
  if (az < *rl)
    {
      goto L40;
    }
  if (dfnu <= 1.)
    {
      goto L30;
    }
  if (az + az < dfnu * dfnu)
    {
      goto L50;
    }
  /*----------------------------------------------------------------------- 
   *    ASYMPTOTIC EXPANSION FOR LARGE Z 
   *----------------------------------------------------------------------- 
   */
L30:
  nsp_calpack_zasyi (zr, zi, fnu, kode, &nn, &cyr[1], &cyi[1], &nw, rl, tol,
		     elim, alim);
  if (nw < 0)
    {
      goto L130;
    }
  goto L120;
L40:
  if (dfnu <= 1.)
    {
      goto L70;
    }
L50:
  /*----------------------------------------------------------------------- 
   *    OVERFLOW AND UNDERFLOW TEST ON I SEQUENCE FOR MILLER ALGORITHM 
   *----------------------------------------------------------------------- 
   */
  nsp_calpack_zuoik (zr, zi, fnu, kode, &c__1, &nn, &cyr[1], &cyi[1], &nw,
		     tol, elim, alim);
  if (nw < 0)
    {
      goto L130;
    }
  *nz += nw;
  nn -= nw;
  if (nn == 0)
    {
      return 0;
    }
  dfnu = *fnu + (nn - 1);
  if (dfnu > *fnul)
    {
      goto L110;
    }
  if (az > *fnul)
    {
      goto L110;
    }
L60:
  if (az > *rl)
    {
      goto L80;
    }
L70:
  /*----------------------------------------------------------------------- 
   *    MILLER ALGORITHM NORMALIZED BY THE SERIES 
   *----------------------------------------------------------------------- 
   */
  nsp_calpack_zmlri (zr, zi, fnu, kode, &nn, &cyr[1], &cyi[1], &nw, tol);
  if (nw < 0)
    {
      goto L130;
    }
  goto L120;
L80:
  /*----------------------------------------------------------------------- 
   *    MILLER ALGORITHM NORMALIZED BY THE WRONSKIAN 
   *----------------------------------------------------------------------- 
   *----------------------------------------------------------------------- 
   *    OVERFLOW TEST ON K FUNCTIONS USED IN WRONSKIAN 
   *----------------------------------------------------------------------- 
   */
  nsp_calpack_zuoik (zr, zi, fnu, kode, &c__2, &c__2, cwr, cwi, &nw, tol,
		     elim, alim);
  if (nw >= 0)
    {
      goto L100;
    }
  *nz = nn;
  i__1 = nn;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      cyr[i__] = zeror;
      cyi[i__] = zeroi;
      /* L90: */
    }
  return 0;
L100:
  if (nw > 0)
    {
      goto L130;
    }
  nsp_calpack_zwrsk (zr, zi, fnu, kode, &nn, &cyr[1], &cyi[1], &nw, cwr, cwi,
		     tol, elim, alim);
  if (nw < 0)
    {
      goto L130;
    }
  goto L120;
L110:
  /*----------------------------------------------------------------------- 
   *    INCREMENT FNU+NN-1 UP TO FNUL, COMPUTE AND RECUR BACKWARD 
   *----------------------------------------------------------------------- 
   */
  nui = (int) (*fnul - dfnu + 1);
  nui = Max (nui, 0);
  nsp_calpack_zbuni (zr, zi, fnu, kode, &nn, &cyr[1], &cyi[1], &nw, &nui,
		     &nlast, fnul, tol, elim, alim);
  if (nw < 0)
    {
      goto L130;
    }
  *nz += nw;
  if (nlast == 0)
    {
      goto L120;
    }
  nn = nlast;
  goto L60;
L120:
  return 0;
L130:
  *nz = -1;
  if (nw == -2)
    {
      *nz = -2;
    }
  return 0;
}				/* zbinu_ */
#endif
