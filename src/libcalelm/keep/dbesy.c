/* dbesy.f -- translated by f2c (version 19961017).
 *
 *
 */

#if 0 
#include "calpack.h"

/* Table of constant values */

static int c__15 = 15;
static int c__5 = 5;
static int c__1 = 1;

/*DECK DBESY 
 */
int nsp_calpack_dbesy (double *x, double *fnu, int *n, double *y, int *ierr)
{
  /* Initialized data */

  static int nulim[2] = { 70, 100 };

  /* System generated locals */
  int i__1;

  /* Local variables */
  double elim;
  int iflw;
  double xlim;
  int i__, j;
  double s, w[2], flgjy;
  double s1, s2;
  int nb;
  double cn;
  int nd;
  double fn;
  int nn;
  double tm, wk[7];
  double w2n, ran;
  int nud;
  double dnu, azn, trx, xxn;

  /****BEGIN PROLOGUE  DBESY 
   ****PURPOSE  Implement forward recursion on the three term recursion 
   *           relation for a sequence of non-negative order Bessel 
   *           functions Y/SUB(FNU+I-1)/(X), I=1,...,N for real, positive 
   *           X and non-negative orders FNU. 
   ****LIBRARY   SLATEC 
   ****CATEGORY  C10A3 
   ****TYPE      DOUBLE PRECISION (BESY-S, DBESY-D) 
   ****KEYWORDS  SPECIAL FUNCTIONS, Y BESSEL FUNCTION 
   ****AUTHOR  Amos, D. E., (SNLA) 
   ****DESCRIPTION 
   * 
   *    Abstract  **** a double precision routine **** 
   *        DBESY implements forward recursion on the three term 
   *        recursion relation for a sequence of non-negative order Bessel 
   *        functions Y/sub(FNU+I-1)/(X), I=1,N for real X .GT. 0.0D0 and 
   *        non-negative orders FNU.  If FNU .LT. NULIM, orders FNU and 
   *        FNU+1 are obtained from DBSYNU which computes by a power 
   *        series for X .LE. 2, the K Bessel function of an imaginary 
   *        argument for 2 .LT. X .LE. 20 and the asymptotic expansion for 
   *        X .GT. 20. 
   * 
   *        If FNU .GE. NULIM, the uniform asymptotic expansion is coded 
   *        in DASYJY for orders FNU and FNU+1 to start the recursion. 
   *        NULIM is 70 or 100 depending on whether N=1 or N .GE. 2.  An 
   *        overflow test is made on the leading term of the asymptotic 
   *        expansion before any extensive computation is done. 
   * 
   *        The maximum number of significant digits obtainable 
   *        is the smaller of 14 and the number of digits carried in 
   *        double precision arithmetic. 
   * 
   *    Description of Arguments 
   * 
   *        Input 
   *          X      - X .GT. 0.0D0 
   *          FNU    - order of the initial Y function, FNU .GE. 0.0D0 
   *          N      - number of members in the sequence, N .GE. 1 
   * 
   *        Output 
   *          Y      - a vector whose first N components contain values 
   *                   for the sequence Y(I)=Y/sub(FNU+I-1)/(X), I=1,N. 
   * 
   *    Error Conditions 
   *        Improper input arguments - a fatal error 
   *        Overflow - a fatal error 
   * 
   ****REFERENCES  F. W. J. Olver, Tables of Bessel Functions of Moderate 
   *                or Large Orders, NPL Mathematical Tables 6, Her 
   *                Majesty's Stationery Office, London, 1962. 
   *              N. M. Temme, On the numerical evaluation of the modified 
   *                Bessel function of the third kind, Journal of 
   *                Computational Physics 19, (1975), pp. 324-337. 
   *              N. M. Temme, On the numerical evaluation of the ordinary 
   *                Bessel function of the second kind, Journal of 
   *                Computational Physics 21, (1976), pp. 343-350. 
   ****ROUTINES CALLED  D1MACH, DASYJY, DBESY0, DBESY1, DBSYNU, DYAIRY, 
   *                   XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  800501  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  890911  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  920501  Reformatted the REFERENCES section.  (WRB) 
   ****END PROLOGUE  DBESY 
   * 
   */
  /* Parameter adjustments */
  --y;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DBESY 
   */
  *ierr = 0;
  nn = -nsp_calpack_i1mach (&c__15);
  elim = (nn * nsp_calpack_d1mach (&c__5) - 3.) * 2.303;
  xlim = nsp_calpack_d1mach (&c__1) * 1e3;
  if (*fnu < 0.)
    {
      goto L140;
    }
  if (*x <= 0.)
    {
      goto L150;
    }
  if (*x < xlim)
    {
      goto L170;
    }
  if (*n < 1)
    {
      goto L160;
    }
  /* 
   *    ND IS A DUMMY VARIABLE FOR N 
   * 
   */
  nd = *n;
  nud = (int) (*fnu);
  dnu = *fnu - nud;
  nn = Min (2, nd);
  fn = *fnu + *n - 1;
  if (fn < 2.)
    {
      goto L100;
    }
  /* 
   *    OVERFLOW TEST  (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION) 
   *    FOR THE LAST ORDER, FNU+N-1.GE.NULIM 
   * 
   */
  xxn = *x / fn;
  w2n = 1. - xxn * xxn;
  if (w2n <= 0.)
    {
      goto L10;
    }
  ran = sqrt (w2n);
  azn = log ((ran + 1.) / xxn) - ran;
  cn = fn * azn;
  if (cn > elim)
    {
      goto L170;
    }
L10:
  if (nud < nulim[nn - 1])
    {
      goto L20;
    }
  /* 
   *    ASYMPTOTIC EXPANSION FOR ORDERS FNU AND FNU+1.GE.NULIM 
   * 
   */
  flgjy = -1.;
  nsp_calpack_dasyjy ((U_fp) dyairy_, x, fnu, &flgjy, &nn, &y[1], wk, &iflw);
  if (iflw != 0)
    {
      goto L170;
    }
  if (nn == 1)
    {
      return 0;
    }
  trx = 2. / *x;
  tm = (*fnu + *fnu + 2.) / *x;
  goto L80;
  /* 
   */
L20:
  if (dnu != 0.)
    {
      goto L30;
    }
  s1 = nsp_calpack_dbesy0 (x);
  if (nud == 0 && nd == 1)
    {
      goto L70;
    }
  s2 = nsp_calpack_dbesy1 (x);
  goto L40;
L30:
  nb = 2;
  if (nud == 0 && nd == 1)
    {
      nb = 1;
    }
  nsp_calpack_dbsynu (x, &dnu, &nb, w);
  s1 = w[0];
  if (nb == 1)
    {
      goto L70;
    }
  s2 = w[1];
L40:
  trx = 2. / *x;
  tm = (dnu + dnu + 2.) / *x;
  /*    FORWARD RECUR FROM DNU TO FNU+1 TO GET Y(1) AND Y(2) 
   */
  if (nd == 1)
    {
      --nud;
    }
  if (nud > 0)
    {
      goto L50;
    }
  if (nd > 1)
    {
      goto L70;
    }
  s1 = s2;
  goto L70;
L50:
  i__1 = nud;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s = s2;
      s2 = tm * s2 - s1;
      s1 = s;
      tm += trx;
      /* L60: */
    }
  if (nd == 1)
    {
      s1 = s2;
    }
L70:
  y[1] = s1;
  if (nd == 1)
    {
      return 0;
    }
  y[2] = s2;
L80:
  if (nd == 2)
    {
      return 0;
    }
  /*    FORWARD RECUR FROM FNU+2 TO FNU+N-1 
   */
  i__1 = nd;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      y[i__] = tm * y[i__ - 1] - y[i__ - 2];
      tm += trx;
      /* L90: */
    }
  return 0;
  /* 
   */
L100:
  /*    OVERFLOW TEST 
   */
  if (fn <= 1.)
    {
      goto L110;
    }
  if (-fn * (log (*x) - .693) > elim)
    {
      goto L170;
    }
L110:
  if (dnu == 0.)
    {
      goto L120;
    }
  nsp_calpack_dbsynu (x, fnu, &nd, &y[1]);
  return 0;
L120:
  j = nud;
  if (j == 1)
    {
      goto L130;
    }
  ++j;
  y[j] = nsp_calpack_dbesy0 (x);
  if (nd == 1)
    {
      return 0;
    }
  ++j;
L130:
  y[j] = nsp_calpack_dbesy1 (x);
  if (nd == 1)
    {
      return 0;
    }
  trx = 2. / *x;
  tm = trx;
  goto L80;
  /* 
   * 
   * 
   */
L140:
  *ierr = 1;
  /*     CALL XERMSG ('SLATEC', 'DBESY', 'ORDER, FNU, LESS THAN ZERO', 2, 
   *    +   1) 
   */
  return 0;
L150:
  *ierr = 1;
  /*     CALL XERMSG ('SLATEC', 'DBESY', 'X LESS THAN OR EQUAL TO ZERO', 
   *    +   2, 1) 
   */
  return 0;
L160:
  *ierr = 1;
  /*     CALL XERMSG ('SLATEC', 'DBESY', 'N LESS THAN ONE', 2, 1) 
   */
  return 0;
L170:
  *ierr = 2;
  /*     CALL XERMSG ('SLATEC', 'DBESY', 
   *    +   'OVERFLOW, FNU OR N TOO LARGE OR X TOO SMALL', 6, 1) 
   */
  return 0;
}				/* dbesy_ */
#endif
