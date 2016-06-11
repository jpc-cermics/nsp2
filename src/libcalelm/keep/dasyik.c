/* dasyik.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__3 = 3;

/*DECK DASYIK 
 */
int
nsp_calpack_dasyik (double *x, double *fnu, int *kode, double *flgik,
		    double *ra, double *arg, int *in, double *y)
{
  /* Initialized data */

  static double con[2] = { .398942280401432678, 1.25331413731550025 };
  static double c__[65] =
    { -.208333333333333, .125, .334201388888889, -.401041666666667, .0703125,
    -1.02581259645062, 1.84646267361111, -.8912109375, .0732421875,
    4.66958442342625,
    -11.207002616223, 8.78912353515625, -2.3640869140625, .112152099609375,
    -28.2120725582002,
    84.6362176746007, -91.81824154324, 42.5349987453885, -7.36879435947963,
    .227108001708984,
    212.570130039217, -765.252468141182, 1059.990452528, -699.579627376133,
    218.190511744212,
    -26.4914304869516, .572501420974731, -1919.45766231841, 8061.72218173731,
    -13586.5500064341,
    11655.3933368645, -5305.6469786134, 1200.90291321635, -108.090919788395,
    1.72772750258446,
    20204.2913309661, -96980.5983886375, 192547.001232532, -203400.177280416,
    122200.464983017,
    -41192.6549688976, 7109.51430248936, -493.915304773088, 6.07404200127348,
    -242919.187900551,
    1311763.61466298, -2998015.91853811, 3763271.2976564, -2813563.22658653,
    1268365.27332162,
    -331645.172484564, 45218.7689813627, -2499.83048181121, 24.3805296995561,
    3284469.85307204,
    -19706819.1184322, 50952602.4926646, -74105148.2115327, 66344512.274729,
    -37567176.6607634,
    13288767.1664218, -2785618.12808645, 308186.404612662, -13886.089753717,
    110.017140269247
  };

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;
  
  /* Local variables */
  double coef;
  int j, k, l;
  double t, z__;
  double s1, s2, t2, ak, ap, fn;
  int kk, jn;
  double gln, tol, etx;

  /****BEGIN PROLOGUE  DASYIK 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to DBESI and DBESK 
   ****LIBRARY   SLATEC 
   ****TYPE      DOUBLE PRECISION (ASYIK-S, DASYIK-D) 
   ****AUTHOR  Amos, D. E., (SNLA) 
   ****DESCRIPTION 
   * 
   *                   DASYIK computes Bessel functions I and K 
   *                 for arguments X.GT.0.0 and orders FNU.GE.35 
   *                 on FLGIK = 1 and FLGIK = -1 respectively. 
   * 
   *                                   INPUT 
   * 
   *     X    - Argument, X.GT.0.0D0 
   *     FNU  - Order of first Bessel function 
   *     KODE - A parameter to indicate the scaling option 
   *            KODE=1 returns Y(I)=        I/SUB(FNU+I-1)/(X), I=1,IN 
   *                   or      Y(I)=        K/SUB(FNU+I-1)/(X), I=1,IN 
   *                   on FLGIK = 1.0D0 or FLGIK = -1.0D0 
   *            KODE=2 returns Y(I)=EXP(-X)*I/SUB(FNU+I-1)/(X), I=1,IN 
   *                   or      Y(I)=EXP( X)*K/SUB(FNU+I-1)/(X), I=1,IN 
   *                   on FLGIK = 1.0D0 or FLGIK = -1.0D0 
   *    FLGIK - Selection parameter for I or K FUNCTION 
   *            FLGIK =  1.0D0 gives the I function 
   *            FLGIK = -1.0D0 gives the K function 
   *       RA - SQRT(1.+Z*Z), Z=X/FNU 
   *      ARG - Argument of the leading exponential 
   *       IN - Number of functions desired, IN=1 or 2 
   * 
   *                                   OUTPUT 
   * 
   *        Y - A vector whose first IN components contain the sequence 
   * 
   *    Abstract  **** A double precision routine **** 
   *        DASYIK implements the uniform asymptotic expansion of 
   *        the I and K Bessel functions for FNU.GE.35 and real 
   *        X.GT.0.0D0. The forms are identical except for a change 
   *        in sign of some of the terms. This change in sign is 
   *        accomplished by means of the FLAG FLGIK = 1 or -1. 
   * 
   ****SEE ALSO  DBESI, DBESK 
   ****ROUTINES CALLED  D1MACH 
   ****REVISION HISTORY  (YYMMDD) 
   *  750101  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900328  Added TYPE section.  (WRB) 
   *  910408  Updated the AUTHOR section.  (WRB) 
   ****END PROLOGUE  DASYIK 
   * 
   */
  /* Parameter adjustments */
  --y;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DASYIK 
   */
  tol = nsp_calpack_d1mach (&c__3);
  tol = Max (tol, 1e-15);
  fn = *fnu;
  z__ = (3. - *flgik) / 2.;
  kk = (int) z__;
  i__1 = *in;
  for (jn = 1; jn <= i__1; ++jn)
    {
      if (jn == 1)
	{
	  goto L10;
	}
      fn -= *flgik;
      z__ = *x / fn;
      *ra = sqrt (z__ * z__ + 1.);
      gln = log ((*ra + 1.) / z__);
      etx = (double) (*kode - 1);
      t = *ra * (1. - etx) + etx / (z__ + *ra);
      *arg = fn * (t - gln) * *flgik;
    L10:
      coef = exp (*arg);
      t = 1. / *ra;
      t2 = t * t;
      t /= fn;
      t = d_sign (&t, flgik);
      s2 = 1.;
      ap = 1.;
      l = 0;
      for (k = 2; k <= 11; ++k)
	{
	  ++l;
	  s1 = c__[l - 1];
	  i__2 = k;
	  for (j = 2; j <= i__2; ++j)
	    {
	      ++l;
	      s1 = s1 * t2 + c__[l - 1];
	      /* L20: */
	    }
	  ap *= t;
	  ak = ap * s1;
	  s2 += ak;
	  /*Computing MAX 
	   */
	  d__1 = Abs (ak), d__2 = Abs (ap);
	  if (Max (d__1, d__2) < tol)
	    {
	      goto L40;
	    }
	  /* L30: */
	}
    L40:
      t = Abs (t);
      y[jn] = s2 * coef * sqrt (t) * con[kk - 1];
      /* L50: */
    }
  return 0;
}				/* dasyik_ */
#endif
