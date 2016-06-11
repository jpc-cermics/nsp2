/* zasyi.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;

/*DECK ZASYI 
 */
int
nsp_calpack_zasyi (double *zr, double *zi, double *fnu, int *kode, int *n,
		   double *yr, double *yi, int *nz, double *rl, double *tol,
		   double *elim, double *alim)
{
  /* Initialized data */

  static double pi = 3.14159265358979324;
  static double rtpi = .159154943091895336;
  static double zeror = 0.;
  static double zeroi = 0.;
  static double coner = 1.;
  static double conei = 0.;

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;


  /* Local variables */
  double dfnu, atol;
  int i__, j, k, m;
  double s;
  int koded;
  double aa, bb;
  int ib;
  double ak, bk;
  int il, jl;
  double az;
  int nn;
  double p1i, s2i, p1r, s2r, cki, dki, fdn, arg, aez, arm, ckr, dkr, czi, ezi,
    sgn;
  int inu;
  double raz, czr, ezr, sqk, sti, rzi, tzi, str, rzr, tzr, ak1i, ak1r, cs1i,
    cs2i, cs1r, cs2r, dnu2, rtr1;

  /****BEGIN PROLOGUE  ZASYI 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to ZBESI and ZBESK 
   ****LIBRARY   SLATEC 
   ****TYPE      ALL (CASYI-A, ZASYI-A) 
   ****AUTHOR  Amos, D. E., (SNL) 
   ****DESCRIPTION 
   * 
   *    ZASYI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY 
   *    MEANS OF THE ASYMPTOTIC EXPANSION FOR LARGE ABS(Z) IN THE 
   *    REGION ABS(Z).GT.MAX(RL,FNU*FNU/2). NZ=0 IS A NORMAL RETURN. 
   *    NZ.LT.0 INDICATES AN OVERFLOW ON KODE=1. 
   * 
   ****SEE ALSO  ZBESI, ZBESK 
   ****ROUTINES CALLED  D1MACH, ZABS, ZDIV, ZEXP, ZMLT, ZSQRT 
   ****REVISION HISTORY  (YYMMDD) 
   *  830501  DATE WRITTEN 
   *  910415  Prologue converted to Version 4.0 format.  (BAB) 
   *  930122  Added ZEXP and ZSQRT to EXTERNAL statement.  (RWC) 
   ****END PROLOGUE  ZASYI 
   *    COMPLEX AK1,CK,CONE,CS1,CS2,CZ,CZERO,DK,EZ,P1,RZ,S2,Y,Z 
   */
  /* Parameter adjustments */
  --yi;
  --yr;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  ZASYI 
   */
  *nz = 0;
  az = nsp_calpack_zabs (zr, zi);
  arm = nsp_calpack_d1mach (&c__1) * 1e3;
  rtr1 = sqrt (arm);
  il = Min (2, *n);
  dfnu = *fnu + (*n - il);
  /*----------------------------------------------------------------------- 
   *    OVERFLOW TEST 
   *----------------------------------------------------------------------- 
   */
  raz = 1. / az;
  str = *zr * raz;
  sti = -(*zi) * raz;
  ak1r = rtpi * str * raz;
  ak1i = rtpi * sti * raz;
  nsp_calpack_zsqrt (&ak1r, &ak1i, &ak1r, &ak1i);
  czr = *zr;
  czi = *zi;
  if (*kode != 2)
    {
      goto L10;
    }
  czr = zeror;
  czi = *zi;
L10:
  if (Abs (czr) > *elim)
    {
      goto L100;
    }
  dnu2 = dfnu + dfnu;
  koded = 1;
  if (Abs (czr) > *alim && *n > 2)
    {
      goto L20;
    }
  koded = 0;
  nsp_calpack_zexp (&czr, &czi, &str, &sti);
  nsp_calpack_zmlt (&ak1r, &ak1i, &str, &sti, &ak1r, &ak1i);
L20:
  fdn = 0.;
  if (dnu2 > rtr1)
    {
      fdn = dnu2 * dnu2;
    }
  ezr = *zr * 8.;
  ezi = *zi * 8.;
  /*----------------------------------------------------------------------- 
   *    WHEN Z IS IMAGINARY, THE ERROR TEST MUST BE MADE RELATIVE TO THE 
   *    FIRST RECIPROCAL POWER SINCE THIS IS THE LEADING TERM OF THE 
   *    EXPANSION FOR THE IMAGINARY PART. 
   *----------------------------------------------------------------------- 
   */
  aez = az * 8.;
  s = *tol / aez;
  jl = (int) (*rl + *rl + 2);
  p1r = zeror;
  p1i = zeroi;
  if (*zi == 0.)
    {
      goto L30;
    }
  /*----------------------------------------------------------------------- 
   *    CALCULATE EXP(PI*(0.5+FNU+N-IL)*I) TO MINIMIZE LOSSES OF 
   *    SIGNIFICANCE WHEN FNU OR N IS LARGE 
   *----------------------------------------------------------------------- 
   */
  inu = (int) (*fnu);
  arg = (*fnu - inu) * pi;
  inu = inu + *n - il;
  ak = -sin (arg);
  bk = cos (arg);
  if (*zi < 0.)
    {
      bk = -bk;
    }
  p1r = ak;
  p1i = bk;
  if (inu % 2 == 0)
    {
      goto L30;
    }
  p1r = -p1r;
  p1i = -p1i;
L30:
  i__1 = il;
  for (k = 1; k <= i__1; ++k)
    {
      sqk = fdn - 1.;
      atol = s * Abs (sqk);
      sgn = 1.;
      cs1r = coner;
      cs1i = conei;
      cs2r = coner;
      cs2i = conei;
      ckr = coner;
      cki = conei;
      ak = 0.;
      aa = 1.;
      bb = aez;
      dkr = ezr;
      dki = ezi;
      i__2 = jl;
      for (j = 1; j <= i__2; ++j)
	{
	  nsp_calpack_zdiv (&ckr, &cki, &dkr, &dki, &str, &sti);
	  ckr = str * sqk;
	  cki = sti * sqk;
	  cs2r += ckr;
	  cs2i += cki;
	  sgn = -sgn;
	  cs1r += ckr * sgn;
	  cs1i += cki * sgn;
	  dkr += ezr;
	  dki += ezi;
	  aa = aa * Abs (sqk) / bb;
	  bb += aez;
	  ak += 8.;
	  sqk -= ak;
	  if (aa <= atol)
	    {
	      goto L50;
	    }
	  /* L40: */
	}
      goto L110;
    L50:
      s2r = cs1r;
      s2i = cs1i;
      if (*zr + *zr >= *elim)
	{
	  goto L60;
	}
      tzr = *zr + *zr;
      tzi = *zi + *zi;
      d__1 = -tzr;
      d__2 = -tzi;
      nsp_calpack_zexp (&d__1, &d__2, &str, &sti);
      nsp_calpack_zmlt (&str, &sti, &p1r, &p1i, &str, &sti);
      nsp_calpack_zmlt (&str, &sti, &cs2r, &cs2i, &str, &sti);
      s2r += str;
      s2i += sti;
    L60:
      fdn = fdn + dfnu * 8. + 4.;
      p1r = -p1r;
      p1i = -p1i;
      m = *n - il + k;
      yr[m] = s2r * ak1r - s2i * ak1i;
      yi[m] = s2r * ak1i + s2i * ak1r;
      /* L70: */
    }
  if (*n <= 2)
    {
      return 0;
    }
  nn = *n;
  k = nn - 2;
  ak = (double) k;
  str = *zr * raz;
  sti = -(*zi) * raz;
  rzr = (str + str) * raz;
  rzi = (sti + sti) * raz;
  ib = 3;
  i__1 = nn;
  for (i__ = ib; i__ <= i__1; ++i__)
    {
      yr[k] = (ak + *fnu) * (rzr * yr[k + 1] - rzi * yi[k + 1]) + yr[k + 2];
      yi[k] = (ak + *fnu) * (rzr * yi[k + 1] + rzi * yr[k + 1]) + yi[k + 2];
      ak += -1.;
      --k;
      /* L80: */
    }
  if (koded == 0)
    {
      return 0;
    }
  nsp_calpack_zexp (&czr, &czi, &ckr, &cki);
  i__1 = nn;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      str = yr[i__] * ckr - yi[i__] * cki;
      yi[i__] = yr[i__] * cki + yi[i__] * ckr;
      yr[i__] = str;
      /* L90: */
    }
  return 0;
L100:
  *nz = -1;
  return 0;
L110:
  *nz = -2;
  return 0;
}				/* zasyi_ */
#endif
